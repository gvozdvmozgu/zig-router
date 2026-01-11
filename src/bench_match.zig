const std = @import("std");
const lib = @import("lib.zig");

const Config = struct {
    routes: usize = 900,
    paths: usize = 9000,
    iters: usize = 200,
    seed: u64 = 0x5a17_0b1a_d00d_f00d,
};

fn printUsage() void {
    std.debug.print(
        "Usage: zig run src/bench_match.zig -OReleaseFast -- [--routes=N] [--paths=N] [--iters=N] [--seed=N]\n",
        .{},
    );
}

fn parseArgs() !Config {
    var cfg = Config{};
    var args = std.process.args();
    _ = args.next();
    while (args.next()) |arg| {
        if (std.mem.startsWith(u8, arg, "--routes=")) {
            cfg.routes = try std.fmt.parseInt(usize, arg["--routes=".len..], 10);
        } else if (std.mem.startsWith(u8, arg, "--paths=")) {
            cfg.paths = try std.fmt.parseInt(usize, arg["--paths=".len..], 10);
        } else if (std.mem.startsWith(u8, arg, "--iters=")) {
            cfg.iters = try std.fmt.parseInt(usize, arg["--iters=".len..], 10);
        } else if (std.mem.startsWith(u8, arg, "--seed=")) {
            cfg.seed = try std.fmt.parseInt(u64, arg["--seed=".len..], 10);
        } else if (std.mem.eql(u8, arg, "--help")) {
            printUsage();
            std.process.exit(0);
        } else {
            printUsage();
            std.process.exit(1);
        }
    }
    return cfg;
}

fn insertRoute(
    router: *lib.Router(usize),
    allocator: std.mem.Allocator,
    route: []const u8,
    value: usize,
) !void {
    switch (try router.insert(route, value)) {
        .ok => {},
        .err => |err_val| {
            var err = err_val;
            defer err.deinit(allocator);
            std.debug.print("insert failed for {s}\n", .{route});
            return error.InsertFailed;
        },
    }
}

const MatchStats = struct {
    hits: u64,
    misses: u64,
    sink: usize,
};

fn runMatches(
    router: *const lib.Router(usize),
    paths: []const []u8,
    iters: usize,
) !MatchStats {
    var hits: u64 = 0;
    var misses: u64 = 0;
    var sink: usize = 0;
    var iter: usize = 0;
    while (iter < iters) : (iter += 1) {
        for (paths) |path| {
            if (router.match(path)) |match_val| {
                var match = match_val;
                sink +%= match.value.*;
                sink +%= match.params.len();
                hits += 1;
                match.deinit();
            } else |err| switch (err) {
                error.NotFound => misses += 1,
                else => return err,
            }
        }
    }
    return .{ .hits = hits, .misses = misses, .sink = sink };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const cfg = try parseArgs();
    if (cfg.routes < 3 or cfg.paths == 0 or cfg.iters == 0) {
        printUsage();
        std.process.exit(1);
    }

    const static_count = cfg.routes / 3;
    const param_count = cfg.routes / 3;
    const catchall_count = cfg.routes - static_count - param_count;

    var router = lib.Router(usize).init(allocator);
    defer router.deinit();

    var value: usize = 0;
    var i: usize = 0;
    while (i < static_count) : (i += 1) {
        const route = try std.fmt.allocPrint(allocator, "/s/{d}", .{i});
        defer allocator.free(route);
        try insertRoute(&router, allocator, route, value);
        value += 1;
    }

    i = 0;
    while (i < param_count) : (i += 1) {
        const route = try std.fmt.allocPrint(allocator, "/p/{d}/{{id}}", .{i});
        defer allocator.free(route);
        try insertRoute(&router, allocator, route, value);
        value += 1;
    }

    i = 0;
    while (i < catchall_count) : (i += 1) {
        const route = try std.fmt.allocPrint(allocator, "/c/{d}/{{*path}}", .{i});
        defer allocator.free(route);
        try insertRoute(&router, allocator, route, value);
        value += 1;
    }

    var paths = std.ArrayListUnmanaged([]u8){};
    defer {
        for (paths.items) |path| allocator.free(path);
        paths.deinit(allocator);
    }

    var prng = std.Random.DefaultPrng.init(cfg.seed);
    const rand = prng.random();

    i = 0;
    while (i < cfg.paths) : (i += 1) {
        const kind = rand.uintLessThan(u8, 4);
        const path = switch (kind) {
            0 => blk: {
                const idx = rand.uintLessThan(usize, static_count);
                break :blk try std.fmt.allocPrint(allocator, "/s/{d}", .{idx});
            },
            1 => blk: {
                const idx = rand.uintLessThan(usize, param_count);
                const id = rand.uintLessThan(u32, 1_000_000);
                break :blk try std.fmt.allocPrint(allocator, "/p/{d}/{d}", .{ idx, id });
            },
            2 => blk: {
                const idx = rand.uintLessThan(usize, catchall_count);
                const a = rand.uintLessThan(u32, 1_000_000);
                const b = rand.uintLessThan(u32, 1_000_000);
                break :blk try std.fmt.allocPrint(allocator, "/c/{d}/{d}/{d}", .{ idx, a, b });
            },
            else => blk: {
                const idx = rand.uintLessThan(usize, cfg.routes);
                break :blk try std.fmt.allocPrint(allocator, "/miss/{d}", .{idx});
            },
        };
        try paths.append(allocator, path);
    }

    _ = try runMatches(&router, paths.items, 1);

    var timer = try std.time.Timer.start();
    const stats = try runMatches(&router, paths.items, cfg.iters);
    const elapsed_ns = timer.read();
    std.mem.doNotOptimizeAway(stats.sink);

    const total = stats.hits + stats.misses;
    if (total == 0) {
        std.debug.print("no matches executed\n", .{});
        return;
    }

    const ns_per_op = @as(u128, elapsed_ns) / total;
    const ops_per_sec = @as(u128, total) * std.time.ns_per_s / elapsed_ns;

    std.debug.print(
        "routes={d} paths={d} iters={d} hits={d} misses={d}\n",
        .{ cfg.routes, cfg.paths, cfg.iters, stats.hits, stats.misses },
    );
    std.debug.print(
        "elapsed_ns={d} ns/op={d} ops/s={d}\n",
        .{ elapsed_ns, ns_per_op, ops_per_sec },
    );
}
