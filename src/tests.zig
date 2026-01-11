const std = @import("std");
const lib = @import("lib.zig");
const testing = std.testing;
const Allocator = std.mem.Allocator;

const ExpectedInsert = union(enum) {
    ok,
    conflict: []const u8,
    invalid_param_segment,
    invalid_param,
    too_many_params,
    invalid_catch_all,
};

const InsertCase = struct {
    route: []const u8,
    expected: ExpectedInsert,
};

fn expectInsertResult(
    allocator: Allocator,
    got: lib.InsertResult,
    expected: ExpectedInsert,
) !void {
    switch (got) {
        .ok => switch (expected) {
            .ok => {},
            else => try testing.expect(false),
        },
        .err => |err_val| {
            var err = err_val;
            defer err.deinit(allocator);
            switch (expected) {
                .ok => try testing.expect(false),
                .conflict => |expected_route| switch (err) {
                    .Conflict => |route| try testing.expect(std.mem.eql(u8, route.slice(), expected_route)),
                    else => try testing.expect(false),
                },
                .invalid_param_segment => switch (err) {
                    .InvalidParamSegment => {},
                    else => try testing.expect(false),
                },
                .invalid_param => switch (err) {
                    .InvalidParam => {},
                    else => try testing.expect(false),
                },
                .too_many_params => switch (err) {
                    .TooManyParams => {},
                    else => try testing.expect(false),
                },
                .invalid_catch_all => switch (err) {
                    .InvalidCatchAll => {},
                    else => try testing.expect(false),
                },
            }
        },
    }
}

fn expectInsertOk(allocator: Allocator, got: lib.InsertResult) !void {
    switch (got) {
        .ok => {},
        .err => |err_val| {
            var err = err_val;
            defer err.deinit(allocator);
            try testing.expect(false);
        },
    }
}

fn runInsertTest(cases: []const InsertCase) !void {
    const allocator = testing.allocator;
    var router = lib.Router([]const u8).init(allocator);
    defer router.deinit();

    for (cases) |case| {
        const got = try router.insert(case.route, case.route);
        try expectInsertResult(allocator, got, case.expected);
    }
}

const ParamPair = struct {
    key: []const u8,
    value: []const u8,
};

const Segment = union(enum) {
    Static: []const u8,
    Param: struct { name: []const u8, suffix: []const u8 },
    CatchAll: []const u8,
};

const Pattern = struct {
    route: []const u8,
    segments: []Segment,

    fn deinit(self: *Pattern, allocator: Allocator) void {
        for (self.segments) |segment| {
            switch (segment) {
                .Static => |value| allocator.free(value),
                .Param => |param| {
                    allocator.free(param.name);
                    allocator.free(param.suffix);
                },
                .CatchAll => |value| allocator.free(value),
            }
        }
        allocator.free(self.segments);
        allocator.free(self.route);
        self.* = undefined;
    }
};

fn randRangeInclusive(rand: std.Random, min: usize, max: usize) usize {
    std.debug.assert(min <= max);
    return min + rand.uintLessThan(usize, max - min + 1);
}

fn randomAlphaString(allocator: Allocator, rand: std.Random, min: usize, max: usize) ![]u8 {
    const len = randRangeInclusive(rand, min, max);
    const buf = try allocator.alloc(u8, len);
    for (buf) |*ch| {
        ch.* = 'a' + rand.uintLessThan(u8, 26);
    }
    return buf;
}

fn buildRouteString(allocator: Allocator, segments: []const Segment) ![]u8 {
    var buf = std.ArrayListUnmanaged(u8){};
    errdefer buf.deinit(allocator);

    try buf.append(allocator, '/');
    for (segments, 0..) |segment, idx| {
        if (idx > 0) try buf.append(allocator, '/');
        switch (segment) {
            .Static => |value| try buf.appendSlice(allocator, value),
            .Param => |param| {
                try buf.append(allocator, '{');
                try buf.appendSlice(allocator, param.name);
                try buf.append(allocator, '}');
                try buf.appendSlice(allocator, param.suffix);
            },
            .CatchAll => |name| {
                try buf.appendSlice(allocator, "{*");
                try buf.appendSlice(allocator, name);
                try buf.append(allocator, '}');
            },
        }
    }

    return buf.toOwnedSlice(allocator);
}

fn generatePattern(
    allocator: Allocator,
    rand: std.Random,
    id: usize,
) !Pattern {
    const max_segments = 4;
    const segment_count = randRangeInclusive(rand, 1, max_segments);
    var segments = std.ArrayListUnmanaged(Segment){};
    errdefer {
        for (segments.items) |segment| {
            switch (segment) {
                .Static => |value| allocator.free(value),
                .Param => |param| {
                    allocator.free(param.name);
                    allocator.free(param.suffix);
                },
                .CatchAll => |value| allocator.free(value),
            }
        }
        segments.deinit(allocator);
    }

    var idx: usize = 0;
    while (idx < segment_count) : (idx += 1) {
        const is_last = idx + 1 == segment_count;
        const kind = rand.uintLessThan(u8, 10);
        if (is_last and kind == 0) {
            const name = try std.fmt.allocPrint(allocator, "c{d}", .{id});
            try segments.append(allocator, .{ .CatchAll = name });
            break;
        }

        if (kind < 6) {
            const value = try randomAlphaString(allocator, rand, 1, 5);
            try segments.append(allocator, .{ .Static = value });
            continue;
        }

        const name = try std.fmt.allocPrint(allocator, "p{d}_{d}", .{ id, idx });
        const suffix = try randomAlphaString(allocator, rand, 0, 3);
        try segments.append(allocator, .{ .Param = .{ .name = name, .suffix = suffix } });
    }

    const owned_segments = try segments.toOwnedSlice(allocator);
    const route = try buildRouteString(allocator, owned_segments);
    return .{ .route = route, .segments = owned_segments };
}

const ExpectedPath = struct {
    path: []u8,
    params: []ParamPair,

    fn deinit(self: *ExpectedPath, allocator: Allocator) void {
        for (self.params) |param| {
            allocator.free(param.value);
        }
        allocator.free(self.params);
        allocator.free(self.path);
        self.* = undefined;
    }
};

fn buildPathFromPattern(
    allocator: Allocator,
    rand: std.Random,
    pattern: *const Pattern,
) !ExpectedPath {
    var buf = std.ArrayListUnmanaged(u8){};
    errdefer buf.deinit(allocator);

    var pairs = std.ArrayListUnmanaged(ParamPair){};
    errdefer pairs.deinit(allocator);

    try buf.append(allocator, '/');
    for (pattern.segments, 0..) |segment, idx| {
        if (idx > 0) try buf.append(allocator, '/');
        switch (segment) {
            .Static => |value| try buf.appendSlice(allocator, value),
            .Param => |param| {
                const value = try randomAlphaString(allocator, rand, 1, 6);
                try pairs.append(allocator, .{ .key = param.name, .value = value });
                try buf.appendSlice(allocator, value);
                try buf.appendSlice(allocator, param.suffix);
            },
            .CatchAll => |name| {
                var catchall_value_buf = std.ArrayListUnmanaged(u8){};
                errdefer catchall_value_buf.deinit(allocator);
                const tail_segment_count = randRangeInclusive(rand, 1, 3);
                var tail_index: usize = 0;
                while (tail_index < tail_segment_count) : (tail_index += 1) {
                    if (tail_index > 0) {
                        try buf.append(allocator, '/');
                        try catchall_value_buf.append(allocator, '/');
                    }
                    const segment_bytes = try randomAlphaString(allocator, rand, 1, 6);
                    defer allocator.free(segment_bytes);
                    try buf.appendSlice(allocator, segment_bytes);
                    try catchall_value_buf.appendSlice(allocator, segment_bytes);
                }
                const value = try catchall_value_buf.toOwnedSlice(allocator);
                try pairs.append(allocator, .{ .key = name, .value = value });
                break;
            },
        }
    }

    const path = try buf.toOwnedSlice(allocator);
    const owned_params = try pairs.toOwnedSlice(allocator);
    return .{ .path = path, .params = owned_params };
}

fn randomPath(allocator: Allocator, rand: std.Random) ![]u8 {
    var buf = std.ArrayListUnmanaged(u8){};
    errdefer buf.deinit(allocator);

    try buf.append(allocator, '/');
    const segment_count = randRangeInclusive(rand, 1, 4);
    var idx: usize = 0;
    while (idx < segment_count) : (idx += 1) {
        if (idx > 0) try buf.append(allocator, '/');
        const segment_bytes = try randomAlphaString(allocator, rand, 1, 6);
        defer allocator.free(segment_bytes);
        try buf.appendSlice(allocator, segment_bytes);
    }

    return buf.toOwnedSlice(allocator);
}

fn nextPathSegment(path: []const u8, path_index: *usize) ?[]const u8 {
    if (path_index.* >= path.len) return null;
    if (path[path_index.*] == '/') return null;
    const start = path_index.*;
    const slash_opt = std.mem.indexOfScalarPos(u8, path, path_index.*, '/');
    const end = slash_opt orelse path.len;
    path_index.* = if (slash_opt) |pos| pos + 1 else path.len;
    return path[start..end];
}

fn patternMatches(pattern: *const Pattern, path: []const u8) bool {
    if (path.len == 0 or path[0] != '/') return false;
    var path_index: usize = 1;

    for (pattern.segments) |segment| {
        switch (segment) {
            .Static => |value| {
                const seg = nextPathSegment(path, &path_index) orelse return false;
                if (!std.mem.eql(u8, seg, value)) return false;
            },
            .Param => |param| {
                const seg = nextPathSegment(path, &path_index) orelse return false;
                if (seg.len == 0 or seg.len <= param.suffix.len) return false;
                if (param.suffix.len > 0 and !std.mem.endsWith(u8, seg, param.suffix)) return false;
            },
            .CatchAll => {
                if (path_index >= path.len) return false;
                return true;
            },
        }
    }

    return path_index == path.len;
}

fn patternIsAllStatic(pattern: *const Pattern) bool {
    for (pattern.segments) |segment| {
        if (segment != .Static) return false;
    }
    return true;
}

fn patternParams(
    allocator: Allocator,
    pattern: *const Pattern,
    path: []const u8,
) !?[]ParamPair {
    if (path.len == 0 or path[0] != '/') return null;
    var path_index: usize = 1;
    var pairs = std.ArrayListUnmanaged(ParamPair){};
    errdefer pairs.deinit(allocator);

    for (pattern.segments) |segment| {
        switch (segment) {
            .Static => |value| {
                const seg = nextPathSegment(path, &path_index) orelse {
                    pairs.deinit(allocator);
                    return null;
                };
                if (!std.mem.eql(u8, seg, value)) {
                    pairs.deinit(allocator);
                    return null;
                }
            },
            .Param => |param| {
                const seg = nextPathSegment(path, &path_index) orelse {
                    pairs.deinit(allocator);
                    return null;
                };
                if (seg.len == 0 or seg.len <= param.suffix.len) {
                    pairs.deinit(allocator);
                    return null;
                }
                if (param.suffix.len > 0 and !std.mem.endsWith(u8, seg, param.suffix)) {
                    pairs.deinit(allocator);
                    return null;
                }
                const value = seg[0 .. seg.len - param.suffix.len];
                try pairs.append(allocator, .{ .key = param.name, .value = value });
            },
            .CatchAll => |name| {
                if (path_index >= path.len) {
                    pairs.deinit(allocator);
                    return null;
                }
                const value = path[path_index..];
                try pairs.append(allocator, .{ .key = name, .value = value });
                const owned = try pairs.toOwnedSlice(allocator);
                return owned;
            },
        }
    }

    if (path_index != path.len) {
        pairs.deinit(allocator);
        return null;
    }

    const owned = try pairs.toOwnedSlice(allocator);
    return owned;
}

const ExpectedParams = union(enum) {
    ok: []const ParamPair,
    err,
};

const MatchCase = struct {
    path: []const u8,
    route: []const u8,
    params: ExpectedParams,
};

fn params(pairs: []const ParamPair) ExpectedParams {
    return .{ .ok = pairs };
}

fn expectParams(actual: *const lib.Params, expected: []const ParamPair) !void {
    var iter = actual.iter();
    var idx: usize = 0;
    while (iter.next()) |kv| {
        if (idx >= expected.len) {
            try testing.expect(false);
            return;
        }
        const exp = expected[idx];
        try testing.expect(std.mem.eql(u8, kv.key, exp.key));
        try testing.expect(std.mem.eql(u8, kv.value, exp.value));
        idx += 1;
    }
    try testing.expectEqual(expected.len, idx);
}

fn expectParam(params_actual: *const lib.Params, key: []const u8, expected: ?[]const u8) !void {
    const got = params_actual.get(key);
    if (expected) |exp| {
        const value = got orelse {
            try testing.expect(false);
            return;
        };
        try testing.expect(std.mem.eql(u8, value, exp));
    } else {
        try testing.expect(got == null);
    }
}

fn runMatchTest(routes: []const []const u8, cases: []const MatchCase) !void {
    const allocator = testing.allocator;
    var router = lib.Router([]u8).init(allocator);
    defer router.deinit();

    var values = std.ArrayListUnmanaged([]u8){};
    defer {
        for (values.items) |buf| allocator.free(buf);
        values.deinit(allocator);
    }

    for (routes) |route| {
        const buf = try allocator.alloc(u8, route.len);
        @memcpy(buf, route);
        try values.append(allocator, buf);
        const insert_err = try router.insert(route, buf);
        try expectInsertOk(allocator, insert_err);
    }

    for (cases) |case| {
        if (router.match(case.path)) |match_val| {
            var match = match_val;
            defer match.deinit();
            switch (case.params) {
                .err => try testing.expect(false),
                .ok => |expected_pairs| {
                    try testing.expect(std.mem.eql(u8, match.value.*, case.route));
                    try expectParams(&match.params, expected_pairs);

                    var mut_match = try router.matchMut(case.path);
                    defer mut_match.deinit();
                    if (mut_match.value.*.len > 0) {
                        const original = mut_match.value.*[0];
                        mut_match.value.*[0] = 'Z';

                        var check = try router.match(case.path);
                        defer check.deinit();
                        try testing.expect(check.value.*.len > 0);
                        try testing.expectEqual(@as(u8, 'Z'), check.value.*[0]);

                        var restore = try router.matchMut(case.path);
                        restore.value.*[0] = original;
                        restore.deinit();
                    }
                },
            }
        } else |err| {
            switch (case.params) {
                .err => try testing.expect(err == error.NotFound),
                .ok => try testing.expect(false),
            }
        }
    }
}

const Operation = enum {
    insert,
    remove,
};

const RemoveCase = struct {
    op: Operation,
    route: []const u8,
    expected: ?[]const u8,
};

fn expectOptionalSlice(expected: ?[]const u8, got: ?[]const u8) !void {
    if (expected) |exp| {
        const value = got orelse {
            try testing.expect(false);
            return;
        };
        try testing.expect(std.mem.eql(u8, value, exp));
    } else {
        try testing.expect(got == null);
    }
}

fn runRemoveTest(
    routes: []const []const u8,
    ops: []const RemoveCase,
    remaining: []const []const u8,
) !void {
    const allocator = testing.allocator;
    var router = lib.Router([]const u8).init(allocator);
    defer router.deinit();

    for (routes) |route| {
        const insert_err = try router.insert(route, route);
        try expectInsertOk(allocator, insert_err);
    }

    for (ops) |op| {
        switch (op.op) {
            .insert => {
                const insert_err = try router.insert(op.route, op.route);
                try expectInsertOk(allocator, insert_err);
            },
            .remove => {
                const got = try router.remove(op.route);
                try expectOptionalSlice(op.expected, got);
            },
        }
    }

    for (remaining) |route| {
        var match = try router.match(route);
        match.deinit();
    }
}

test "missing leading slash suffix" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "/{foo}", .expected = .ok },
        .{ .route = "/{foo}suffix", .expected = .ok },
    });
    try runInsertTest(&[_]InsertCase{
        .{ .route = "{foo}", .expected = .ok },
        .{ .route = "{foo}suffix", .expected = .ok },
    });
}

test "param suffix and non-suffix nodes" {
    const allocator = testing.allocator;
    var router = lib.Router([]const u8).init(allocator);
    defer router.deinit();

    try expectInsertOk(allocator, try router.insert("/with/{id}suffix", "suffix"));
    try expectInsertOk(allocator, try router.insert("/plain/{id}", "plain"));

    var match_suffix = try router.match("/with/123suffix");
    defer match_suffix.deinit();
    try testing.expect(std.mem.eql(u8, match_suffix.value.*, "suffix"));
    try expectParam(&match_suffix.params, "id", "123");

    var match_plain = try router.match("/plain/456");
    defer match_plain.deinit();
    try testing.expect(std.mem.eql(u8, match_plain.value.*, "plain"));
    try expectParam(&match_plain.params, "id", "456");
}

test "missing leading slash conflict" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "{foo}/", .expected = .ok },
        .{ .route = "foo/", .expected = .ok },
    });
    try runInsertTest(&[_]InsertCase{
        .{ .route = "foo/", .expected = .ok },
        .{ .route = "{foo}/", .expected = .ok },
    });
}

test "wildcard conflict" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "/cmd/{tool}/{sub}", .expected = .ok },
        .{ .route = "/cmd/vet", .expected = .ok },
        .{ .route = "/foo/bar", .expected = .ok },
        .{ .route = "/foo/{name}", .expected = .ok },
        .{ .route = "/foo/{names}", .expected = .{ .conflict = "/foo/{name}" } },
        .{ .route = "/cmd/{*path}", .expected = .{ .conflict = "/cmd/{tool}/{sub}" } },
        .{ .route = "/cmd/{xxx}/names", .expected = .ok },
        .{ .route = "/cmd/{tool}/{xxx}/foo", .expected = .ok },
        .{ .route = "/src/{*filepath}", .expected = .ok },
        .{ .route = "/src/{file}", .expected = .{ .conflict = "/src/{*filepath}" } },
        .{ .route = "/src/static.json", .expected = .ok },
        .{ .route = "/src/$filepathx", .expected = .ok },
        .{ .route = "/src/", .expected = .ok },
        .{ .route = "/src/foo/bar", .expected = .ok },
        .{ .route = "/src1/", .expected = .ok },
        .{ .route = "/src1/{*filepath}", .expected = .ok },
        .{ .route = "/src2{*filepath}", .expected = .ok },
        .{ .route = "/src2/{*filepath}", .expected = .ok },
        .{ .route = "/src2/", .expected = .ok },
        .{ .route = "/src2", .expected = .ok },
        .{ .route = "/src3", .expected = .ok },
        .{ .route = "/src3/{*filepath}", .expected = .ok },
        .{ .route = "/search/{query}", .expected = .ok },
        .{ .route = "/search/valid", .expected = .ok },
        .{ .route = "/user_{name}", .expected = .ok },
        .{ .route = "/user_x", .expected = .ok },
        .{ .route = "/user_{bar}", .expected = .{ .conflict = "/user_{name}" } },
        .{ .route = "/id{id}", .expected = .ok },
        .{ .route = "/id/{id}", .expected = .ok },
        .{ .route = "/x/{id}", .expected = .ok },
        .{ .route = "/x/{id}/", .expected = .ok },
        .{ .route = "/x/{id}y", .expected = .ok },
        .{ .route = "/x/{id}y/", .expected = .ok },
        .{ .route = "/x/{id}y", .expected = .{ .conflict = "/x/{id}y" } },
        .{ .route = "/x/x{id}", .expected = .{ .conflict = "/x/{id}y/" } },
        .{ .route = "/x/x{id}y", .expected = .{ .conflict = "/x/{id}y/" } },
        .{ .route = "/y/{id}", .expected = .ok },
        .{ .route = "/y/{id}/", .expected = .ok },
        .{ .route = "/y/y{id}", .expected = .ok },
        .{ .route = "/y/y{id}/", .expected = .ok },
        .{ .route = "/y/{id}y", .expected = .{ .conflict = "/y/y{id}/" } },
        .{ .route = "/y/{id}y/", .expected = .{ .conflict = "/y/y{id}/" } },
        .{ .route = "/y/x{id}y", .expected = .{ .conflict = "/y/y{id}/" } },
        .{ .route = "/z/x{id}y", .expected = .ok },
        .{ .route = "/z/{id}", .expected = .ok },
        .{ .route = "/z/{id}y", .expected = .{ .conflict = "/z/x{id}y" } },
        .{ .route = "/z/x{id}", .expected = .{ .conflict = "/z/x{id}y" } },
        .{ .route = "/z/y{id}", .expected = .{ .conflict = "/z/x{id}y" } },
        .{ .route = "/z/x{id}z", .expected = .{ .conflict = "/z/x{id}y" } },
        .{ .route = "/z/z{id}y", .expected = .{ .conflict = "/z/x{id}y" } },
        .{ .route = "/bar/{id}", .expected = .ok },
        .{ .route = "/bar/x{id}y", .expected = .ok },
    });
}

test "prefix suffix conflict" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "/x1/{a}suffix", .expected = .ok },
        .{ .route = "/x1/prefix{a}", .expected = .{ .conflict = "/x1/{a}suffix" } },
        .{ .route = "/x1/prefix{a}suffix", .expected = .{ .conflict = "/x1/{a}suffix" } },
        .{ .route = "/x1/suffix{a}prefix", .expected = .{ .conflict = "/x1/{a}suffix" } },
        .{ .route = "/x1", .expected = .ok },
        .{ .route = "/x1/", .expected = .ok },
        .{ .route = "/x1/{a}", .expected = .ok },
        .{ .route = "/x1/{a}/", .expected = .ok },
        .{ .route = "/x1/{a}suffix/", .expected = .ok },
        .{ .route = "/x2/{a}suffix", .expected = .ok },
        .{ .route = "/x2/{a}", .expected = .ok },
        .{ .route = "/x2/prefix{a}", .expected = .{ .conflict = "/x2/{a}suffix" } },
        .{ .route = "/x2/prefix{a}suff", .expected = .{ .conflict = "/x2/{a}suffix" } },
        .{ .route = "/x2/prefix{a}suffix", .expected = .{ .conflict = "/x2/{a}suffix" } },
        .{ .route = "/x2/prefix{a}suffixy", .expected = .{ .conflict = "/x2/{a}suffix" } },
        .{ .route = "/x2", .expected = .ok },
        .{ .route = "/x2/", .expected = .ok },
        .{ .route = "/x2/{a}suffix/", .expected = .ok },
        .{ .route = "/x3/prefix{a}", .expected = .ok },
        .{ .route = "/x3/{a}suffix", .expected = .{ .conflict = "/x3/prefix{a}" } },
        .{ .route = "/x3/prefix{a}suffix", .expected = .{ .conflict = "/x3/prefix{a}" } },
        .{ .route = "/x3/prefix{a}/", .expected = .ok },
        .{ .route = "/x3/{a}", .expected = .ok },
        .{ .route = "/x3/{a}/", .expected = .ok },
        .{ .route = "/x4/prefix{a}", .expected = .ok },
        .{ .route = "/x4/{a}", .expected = .ok },
        .{ .route = "/x4/{a}suffix", .expected = .{ .conflict = "/x4/prefix{a}" } },
        .{ .route = "/x4/suffix{a}p", .expected = .{ .conflict = "/x4/prefix{a}" } },
        .{ .route = "/x4/suffix{a}prefix", .expected = .{ .conflict = "/x4/prefix{a}" } },
        .{ .route = "/x4/prefix{a}/", .expected = .ok },
        .{ .route = "/x4/{a}/", .expected = .ok },
        .{ .route = "/x5/prefix1{a}", .expected = .ok },
        .{ .route = "/x5/prefix2{a}", .expected = .ok },
        .{ .route = "/x5/{a}suffix", .expected = .{ .conflict = "/x5/prefix1{a}" } },
        .{ .route = "/x5/prefix{a}suffix", .expected = .{ .conflict = "/x5/prefix1{a}" } },
        .{ .route = "/x5/prefix1{a}suffix", .expected = .{ .conflict = "/x5/prefix1{a}" } },
        .{ .route = "/x5/prefix2{a}suffix", .expected = .{ .conflict = "/x5/prefix2{a}" } },
        .{ .route = "/x5/prefix3{a}suffix", .expected = .{ .conflict = "/x5/prefix1{a}" } },
        .{ .route = "/x5/prefix1{a}/", .expected = .ok },
        .{ .route = "/x5/prefix2{a}/", .expected = .ok },
        .{ .route = "/x5/prefix3{a}/", .expected = .ok },
        .{ .route = "/x5/{a}", .expected = .ok },
        .{ .route = "/x5/{a}/", .expected = .ok },
        .{ .route = "/x6/prefix1{a}", .expected = .ok },
        .{ .route = "/x6/prefix2{a}", .expected = .ok },
        .{ .route = "/x6/{a}", .expected = .ok },
        .{ .route = "/x6/{a}suffix", .expected = .{ .conflict = "/x6/prefix1{a}" } },
        .{ .route = "/x6/prefix{a}suffix", .expected = .{ .conflict = "/x6/prefix1{a}" } },
        .{ .route = "/x6/prefix1{a}suffix", .expected = .{ .conflict = "/x6/prefix1{a}" } },
        .{ .route = "/x6/prefix2{a}suffix", .expected = .{ .conflict = "/x6/prefix2{a}" } },
        .{ .route = "/x6/prefix3{a}suffix", .expected = .{ .conflict = "/x6/prefix1{a}" } },
        .{ .route = "/x6/prefix1{a}/", .expected = .ok },
        .{ .route = "/x6/prefix2{a}/", .expected = .ok },
        .{ .route = "/x6/prefix3{a}/", .expected = .ok },
        .{ .route = "/x6/{a}/", .expected = .ok },
        .{ .route = "/x7/prefix{a}suffix", .expected = .ok },
        .{ .route = "/x7/{a}suff", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/{a}suffix", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/{a}suffixy", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/{a}prefix", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/suffix{a}prefix", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/prefix{a}", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/another{a}", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/suffix{a}", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/prefix{a}/", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/prefix{a}suff", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/prefix{a}suffix", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/prefix{a}suffixy", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/prefix1{a}", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/prefix{a}/", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/{a}suffix/", .expected = .{ .conflict = "/x7/prefix{a}suffix" } },
        .{ .route = "/x7/prefix{a}suffix/", .expected = .ok },
        .{ .route = "/x7/{a}", .expected = .ok },
        .{ .route = "/x7/{a}/", .expected = .ok },
        .{ .route = "/x8/prefix{a}suffix", .expected = .ok },
        .{ .route = "/x8/{a}", .expected = .ok },
        .{ .route = "/x8/{a}suff", .expected = .{ .conflict = "/x8/prefix{a}suffix" } },
        .{ .route = "/x8/{a}suffix", .expected = .{ .conflict = "/x8/prefix{a}suffix" } },
        .{ .route = "/x8/{a}suffixy", .expected = .{ .conflict = "/x8/prefix{a}suffix" } },
        .{ .route = "/x8/prefix{a}", .expected = .{ .conflict = "/x8/prefix{a}suffix" } },
        .{ .route = "/x8/prefix{a}/", .expected = .{ .conflict = "/x8/prefix{a}suffix" } },
        .{ .route = "/x8/prefix{a}suff", .expected = .{ .conflict = "/x8/prefix{a}suffix" } },
        .{ .route = "/x8/prefix{a}suffix", .expected = .{ .conflict = "/x8/prefix{a}suffix" } },
        .{ .route = "/x8/prefix{a}suffixy", .expected = .{ .conflict = "/x8/prefix{a}suffix" } },
        .{ .route = "/x8/prefix1{a}", .expected = .{ .conflict = "/x8/prefix{a}suffix" } },
        .{ .route = "/x8/prefix{a}/", .expected = .{ .conflict = "/x8/prefix{a}suffix" } },
        .{ .route = "/x8/{a}suffix/", .expected = .{ .conflict = "/x8/prefix{a}suffix" } },
        .{ .route = "/x8/prefix{a}suffix/", .expected = .ok },
        .{ .route = "/x8/{a}/", .expected = .ok },
        .{ .route = "/x9/prefix{a}", .expected = .ok },
        .{ .route = "/x9/{a}suffix", .expected = .{ .conflict = "/x9/prefix{a}" } },
        .{ .route = "/x9/prefix{a}suffix", .expected = .{ .conflict = "/x9/prefix{a}" } },
        .{ .route = "/x9/prefixabc{a}suffix", .expected = .{ .conflict = "/x9/prefix{a}" } },
        .{ .route = "/x9/pre{a}suffix", .expected = .{ .conflict = "/x9/prefix{a}" } },
        .{ .route = "/x10/{a}", .expected = .ok },
        .{ .route = "/x10/prefix{a}", .expected = .ok },
        .{ .route = "/x10/{a}suffix", .expected = .{ .conflict = "/x10/prefix{a}" } },
        .{ .route = "/x10/prefix{a}suffix", .expected = .{ .conflict = "/x10/prefix{a}" } },
        .{ .route = "/x10/prefixabc{a}suffix", .expected = .{ .conflict = "/x10/prefix{a}" } },
        .{ .route = "/x10/pre{a}suffix", .expected = .{ .conflict = "/x10/prefix{a}" } },
        .{ .route = "/x11/{a}", .expected = .ok },
        .{ .route = "/x11/{a}suffix", .expected = .ok },
        .{ .route = "/x11/prx11fix{a}", .expected = .{ .conflict = "/x11/{a}suffix" } },
        .{ .route = "/x11/prx11fix{a}suff", .expected = .{ .conflict = "/x11/{a}suffix" } },
        .{ .route = "/x11/prx11fix{a}suffix", .expected = .{ .conflict = "/x11/{a}suffix" } },
        .{ .route = "/x11/prx11fix{a}suffixabc", .expected = .{ .conflict = "/x11/{a}suffix" } },
        .{ .route = "/x12/prefix{a}suffix", .expected = .ok },
        .{ .route = "/x12/pre{a}", .expected = .{ .conflict = "/x12/prefix{a}suffix" } },
        .{ .route = "/x12/prefix{a}", .expected = .{ .conflict = "/x12/prefix{a}suffix" } },
        .{ .route = "/x12/prefixabc{a}", .expected = .{ .conflict = "/x12/prefix{a}suffix" } },
        .{ .route = "/x12/pre{a}suffix", .expected = .{ .conflict = "/x12/prefix{a}suffix" } },
        .{ .route = "/x12/prefix{a}suffix", .expected = .{ .conflict = "/x12/prefix{a}suffix" } },
        .{ .route = "/x12/prefixabc{a}suffix", .expected = .{ .conflict = "/x12/prefix{a}suffix" } },
        .{ .route = "/x12/prefix{a}suff", .expected = .{ .conflict = "/x12/prefix{a}suffix" } },
        .{ .route = "/x12/prefix{a}suffix", .expected = .{ .conflict = "/x12/prefix{a}suffix" } },
        .{ .route = "/x12/prefix{a}suffixabc", .expected = .{ .conflict = "/x12/prefix{a}suffix" } },
        .{ .route = "/x12/{a}suff", .expected = .{ .conflict = "/x12/prefix{a}suffix" } },
        .{ .route = "/x12/{a}suffix", .expected = .{ .conflict = "/x12/prefix{a}suffix" } },
        .{ .route = "/x12/{a}suffixabc", .expected = .{ .conflict = "/x12/prefix{a}suffix" } },
        .{ .route = "/x13/{a}", .expected = .ok },
        .{ .route = "/x13/prefix{a}suffix", .expected = .ok },
        .{ .route = "/x13/pre{a}", .expected = .{ .conflict = "/x13/prefix{a}suffix" } },
        .{ .route = "/x13/prefix{a}", .expected = .{ .conflict = "/x13/prefix{a}suffix" } },
        .{ .route = "/x13/prefixabc{a}", .expected = .{ .conflict = "/x13/prefix{a}suffix" } },
        .{ .route = "/x13/pre{a}suffix", .expected = .{ .conflict = "/x13/prefix{a}suffix" } },
        .{ .route = "/x13/prefix{a}suffix", .expected = .{ .conflict = "/x13/prefix{a}suffix" } },
        .{ .route = "/x13/prefixabc{a}suffix", .expected = .{ .conflict = "/x13/prefix{a}suffix" } },
        .{ .route = "/x13/prefix{a}suff", .expected = .{ .conflict = "/x13/prefix{a}suffix" } },
        .{ .route = "/x13/prefix{a}suffix", .expected = .{ .conflict = "/x13/prefix{a}suffix" } },
        .{ .route = "/x13/prefix{a}suffixabc", .expected = .{ .conflict = "/x13/prefix{a}suffix" } },
        .{ .route = "/x13/{a}suff", .expected = .{ .conflict = "/x13/prefix{a}suffix" } },
        .{ .route = "/x13/{a}suffix", .expected = .{ .conflict = "/x13/prefix{a}suffix" } },
        .{ .route = "/x13/{a}suffixabc", .expected = .{ .conflict = "/x13/prefix{a}suffix" } },
        .{ .route = "/x15/{*rest}", .expected = .ok },
        .{ .route = "/x15/{a}suffix", .expected = .{ .conflict = "/x15/{*rest}" } },
        .{ .route = "/x15/{a}suffix", .expected = .{ .conflict = "/x15/{*rest}" } },
        .{ .route = "/x15/prefix{a}", .expected = .ok },
        .{ .route = "/x16/{*rest}", .expected = .ok },
        .{ .route = "/x16/prefix{a}suffix", .expected = .ok },
        .{ .route = "/x17/prefix{a}/z", .expected = .ok },
        .{ .route = "/x18/prefix{a}/z", .expected = .ok },
    });
}

test "invalid catchall" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "/non-leading-{*catchall}", .expected = .ok },
        .{ .route = "/foo/bar{*catchall}", .expected = .ok },
        .{ .route = "/src/{*filepath}x", .expected = .invalid_catch_all },
        .{ .route = "/src/{*filepath}/x", .expected = .invalid_catch_all },
        .{ .route = "/src2/", .expected = .ok },
        .{ .route = "/src2/{*filepath}/x", .expected = .invalid_catch_all },
    });
}

test "catchall root conflict" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "/", .expected = .ok },
        .{ .route = "/{*filepath}", .expected = .ok },
    });
}

test "child conflict" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "/cmd/vet", .expected = .ok },
        .{ .route = "/cmd/{tool}", .expected = .ok },
        .{ .route = "/cmd/{tool}/{sub}", .expected = .ok },
        .{ .route = "/cmd/{tool}/misc", .expected = .ok },
        .{ .route = "/cmd/{tool}/{bad}", .expected = .{ .conflict = "/cmd/{tool}/{sub}" } },
        .{ .route = "/src/AUTHORS", .expected = .ok },
        .{ .route = "/src/{*filepath}", .expected = .ok },
        .{ .route = "/user_x", .expected = .ok },
        .{ .route = "/user_{name}", .expected = .ok },
        .{ .route = "/id/{id}", .expected = .ok },
        .{ .route = "/id{id}", .expected = .ok },
        .{ .route = "/{id}", .expected = .ok },
        .{ .route = "/{*filepath}", .expected = .{ .conflict = "/{id}" } },
    });
}

test "duplicates" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "/", .expected = .ok },
        .{ .route = "/", .expected = .{ .conflict = "/" } },
        .{ .route = "/doc/", .expected = .ok },
        .{ .route = "/doc/", .expected = .{ .conflict = "/doc/" } },
        .{ .route = "/src/{*filepath}", .expected = .ok },
        .{ .route = "/src/{*filepath}", .expected = .{ .conflict = "/src/{*filepath}" } },
        .{ .route = "/search/{query}", .expected = .ok },
        .{ .route = "/search/{query}", .expected = .{ .conflict = "/search/{query}" } },
        .{ .route = "/user_{name}", .expected = .ok },
        .{ .route = "/user_{name}", .expected = .{ .conflict = "/user_{name}" } },
    });
}

test "unnamed param" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "/{}", .expected = .invalid_param },
        .{ .route = "/user{}/", .expected = .invalid_param },
        .{ .route = "/cmd/{}/", .expected = .invalid_param },
        .{ .route = "/src/{*}", .expected = .invalid_param },
    });
}

test "double params" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "/{foo}{bar}", .expected = .invalid_param_segment },
        .{ .route = "/{foo}{bar}/", .expected = .invalid_param_segment },
        .{ .route = "/{foo}{{*bar}/", .expected = .invalid_param },
    });
}

test "too many params" {
    const allocator = testing.allocator;
    var router = lib.Router([]const u8).init(allocator);
    defer router.deinit();

    var buf = std.ArrayListUnmanaged(u8){};
    defer buf.deinit(allocator);

    const max_params: usize = @as(usize, 'z' - 'a' + 1);
    try buf.append(allocator, '/');
    var i: usize = 0;
    while (i < max_params + 1) : (i += 1) {
        if (i > 0) try buf.append(allocator, '/');
        try buf.append(allocator, '{');
        var name_buf: [8]u8 = undefined;
        const name = try std.fmt.bufPrint(&name_buf, "p{d}", .{i});
        try buf.appendSlice(allocator, name);
        try buf.append(allocator, '}');
    }

    const got = try router.insert(buf.items, buf.items);
    try expectInsertResult(allocator, got, .too_many_params);
}

test "invalid insert preserves existing routes" {
    const allocator = testing.allocator;
    var router = lib.Router([]const u8).init(allocator);
    defer router.deinit();

    try expectInsertOk(allocator, try router.insert("/foo/{id}", "ok"));
    const got = try router.insert("/{a}{b}", "bad");
    try expectInsertResult(allocator, got, .invalid_param_segment);

    var match = try router.match("/foo/123");
    defer match.deinit();
    try testing.expect(std.mem.eql(u8, match.value.*, "ok"));
    try expectParam(&match.params, "id", "123");
}

test "invalid catchall preserves existing routes" {
    const allocator = testing.allocator;
    var router = lib.Router([]const u8).init(allocator);
    defer router.deinit();

    try expectInsertOk(allocator, try router.insert("/foo/{id}", "ok"));
    const got = try router.insert("/{*a}x", "bad");
    try expectInsertResult(allocator, got, .invalid_catch_all);

    var match = try router.match("/foo/123");
    defer match.deinit();
    try testing.expect(std.mem.eql(u8, match.value.*, "ok"));
    try expectParam(&match.params, "id", "123");
}

test "normalized conflict" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "/x/{foo}/bar", .expected = .ok },
        .{ .route = "/x/{bar}/bar", .expected = .{ .conflict = "/x/{foo}/bar" } },
        .{ .route = "/{y}/bar/baz", .expected = .ok },
        .{ .route = "/{y}/baz/baz", .expected = .ok },
        .{ .route = "/{z}/bar/bat", .expected = .ok },
        .{ .route = "/{z}/bar/baz", .expected = .{ .conflict = "/{y}/bar/baz" } },
    });
}

test "more conflicts" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "/con{tact}", .expected = .ok },
        .{ .route = "/who/are/{*you}", .expected = .ok },
        .{ .route = "/who/foo/hello", .expected = .ok },
        .{ .route = "/whose/{users}/{name}", .expected = .ok },
        .{ .route = "/who/are/foo", .expected = .ok },
        .{ .route = "/who/are/foo/bar", .expected = .ok },
        .{ .route = "/con{nection}", .expected = .{ .conflict = "/con{tact}" } },
        .{ .route = "/whose/{users}/{user}", .expected = .{ .conflict = "/whose/{users}/{name}" } },
    });
}

test "catchall static overlap" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "/bar", .expected = .ok },
        .{ .route = "/bar/", .expected = .ok },
        .{ .route = "/bar/{*foo}", .expected = .ok },
    });

    try runInsertTest(&[_]InsertCase{
        .{ .route = "/foo", .expected = .ok },
        .{ .route = "/{*bar}", .expected = .ok },
        .{ .route = "/bar", .expected = .ok },
        .{ .route = "/baz", .expected = .ok },
        .{ .route = "/baz/{split}", .expected = .ok },
        .{ .route = "/", .expected = .ok },
        .{ .route = "/{*bar}", .expected = .{ .conflict = "/{*bar}" } },
        .{ .route = "/{*zzz}", .expected = .{ .conflict = "/{*bar}" } },
        .{ .route = "/{xxx}", .expected = .{ .conflict = "/{*bar}" } },
    });

    try runInsertTest(&[_]InsertCase{
        .{ .route = "/{*bar}", .expected = .ok },
        .{ .route = "/bar", .expected = .ok },
        .{ .route = "/bar/x", .expected = .ok },
        .{ .route = "/bar_{x}", .expected = .ok },
        .{ .route = "/bar_{x}", .expected = .{ .conflict = "/bar_{x}" } },
        .{ .route = "/bar_{x}/y", .expected = .ok },
        .{ .route = "/bar/{x}", .expected = .ok },
    });
}

test "duplicate conflict" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "/hey", .expected = .ok },
        .{ .route = "/hey/users", .expected = .ok },
        .{ .route = "/hey/user", .expected = .ok },
        .{ .route = "/hey/user", .expected = .{ .conflict = "/hey/user" } },
    });
}

test "invalid param" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "{", .expected = .invalid_param },
        .{ .route = "}", .expected = .invalid_param },
        .{ .route = "x{y", .expected = .invalid_param },
        .{ .route = "x}", .expected = .invalid_param },
    });
}

test "escaped param" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "{{", .expected = .ok },
        .{ .route = "}}", .expected = .ok },
        .{ .route = "xx}}", .expected = .ok },
        .{ .route = "}}yy", .expected = .ok },
        .{ .route = "}}yy{{}}", .expected = .ok },
        .{ .route = "}}yy{{}}{{}}y{{", .expected = .ok },
        .{ .route = "}}yy{{}}{{}}y{{", .expected = .{ .conflict = "}yy{}{}y{" } },
        .{ .route = "/{{yy", .expected = .ok },
        .{ .route = "/{yy}", .expected = .ok },
        .{ .route = "/{id}{{", .expected = .ok },
        .{ .route = "/foo", .expected = .ok },
        .{ .route = "/foo/{{", .expected = .ok },
        .{ .route = "/foo/{{/{x}", .expected = .ok },
        .{ .route = "/foo/{ba{{r}", .expected = .ok },
        .{ .route = "/bar/{ba}}r}", .expected = .ok },
        .{ .route = "/xxx/{x{{}}y}", .expected = .ok },
    });
}

test "bare catchall" {
    try runInsertTest(&[_]InsertCase{
        .{ .route = "{*foo}", .expected = .ok },
        .{ .route = "foo/{*bar}", .expected = .ok },
    });
}

test "partial overlap" {
    const allocator = testing.allocator;
    {
        var router = lib.Router([]const u8).init(allocator);
        defer router.deinit();
        try expectInsertOk(allocator, try router.insert("/foo_bar", "Welcome!"));
        try expectInsertOk(allocator, try router.insert("/foo/bar", "Welcome!"));
        try testing.expectError(error.NotFound, router.match("/foo/"));
    }
    {
        var router = lib.Router([]const u8).init(allocator);
        defer router.deinit();
        try expectInsertOk(allocator, try router.insert("/foo", "Welcome!"));
        try expectInsertOk(allocator, try router.insert("/foo/bar", "Welcome!"));
        try testing.expectError(error.NotFound, router.match("/foo/"));
    }
}

test "wildcard overlap" {
    const allocator = testing.allocator;
    {
        var router = lib.Router([]const u8).init(allocator);
        defer router.deinit();
        try expectInsertOk(allocator, try router.insert("/path/foo", "foo"));
        try expectInsertOk(allocator, try router.insert("/path/{*rest}", "wildcard"));

        {
            var match = try router.match("/path/foo");
            defer match.deinit();
            try testing.expect(std.mem.eql(u8, match.value.*, "foo"));
        }

        {
            var match = try router.match("/path/bar");
            defer match.deinit();
            try testing.expect(std.mem.eql(u8, match.value.*, "wildcard"));
        }

        {
            var match = try router.match("/path/foo/");
            defer match.deinit();
            try testing.expect(std.mem.eql(u8, match.value.*, "wildcard"));
        }
    }

    {
        var router = lib.Router([]const u8).init(allocator);
        defer router.deinit();
        try expectInsertOk(allocator, try router.insert("/path/foo/{arg}", "foo"));
        try expectInsertOk(allocator, try router.insert("/path/{*rest}", "wildcard"));

        {
            var match = try router.match("/path/foo/myarg");
            defer match.deinit();
            try testing.expect(std.mem.eql(u8, match.value.*, "foo"));
        }

        {
            var match = try router.match("/path/foo/myarg/");
            defer match.deinit();
            try testing.expect(std.mem.eql(u8, match.value.*, "wildcard"));
        }

        {
            var match = try router.match("/path/foo/myarg/bar/baz");
            defer match.deinit();
            try testing.expect(std.mem.eql(u8, match.value.*, "wildcard"));
        }
    }
}

test "overlapping param backtracking" {
    const allocator = testing.allocator;
    var matcher = lib.Router([]const u8).init(allocator);
    defer matcher.deinit();

    try expectInsertOk(allocator, try matcher.insert("/{object}/{id}", "object with id"));
    try expectInsertOk(
        allocator,
        try matcher.insert("/secret/{id}/path", "secret with id and path"),
    );

    {
        var matched = try matcher.match("/secret/978/path");
        defer matched.deinit();
        try expectParam(&matched.params, "id", "978");
    }

    {
        var matched = try matcher.match("/something/978");
        defer matched.deinit();
        try expectParam(&matched.params, "id", "978");
        try expectParam(&matched.params, "object", "something");
    }

    {
        var matched = try matcher.match("/secret/978");
        defer matched.deinit();
        try expectParam(&matched.params, "id", "978");
    }
}

test "empty route" {
    const routes = [_][]const u8{ "", "/foo" };
    const cases = [_]MatchCase{
        .{ .path = "", .route = "", .params = params(&[_]ParamPair{}) },
        .{ .path = "/foo", .route = "/foo", .params = params(&[_]ParamPair{}) },
    };
    try runMatchTest(&routes, &cases);
}

test "bare catchall match" {
    const routes = [_][]const u8{ "{*foo}", "foo/{*bar}" };
    const cases = [_]MatchCase{
        .{ .path = "x/y", .route = "{*foo}", .params = params(&[_]ParamPair{.{ .key = "foo", .value = "x/y" }}) },
        .{ .path = "/x/y", .route = "{*foo}", .params = params(&[_]ParamPair{.{ .key = "foo", .value = "/x/y" }}) },
        .{ .path = "/foo/x/y", .route = "{*foo}", .params = params(&[_]ParamPair{.{ .key = "foo", .value = "/foo/x/y" }}) },
        .{ .path = "foo/x/y", .route = "foo/{*bar}", .params = params(&[_]ParamPair{.{ .key = "bar", .value = "x/y" }}) },
    };
    try runMatchTest(&routes, &cases);
}

test "param suffix flag issue" {
    const routes = [_][]const u8{ "/foo/{foo}suffix", "/foo/{foo}/bar" };
    const cases = [_]MatchCase{
        .{
            .path = "/foo/barsuffix",
            .route = "/foo/{foo}suffix",
            .params = params(&[_]ParamPair{.{ .key = "foo", .value = "bar" }}),
        },
    };
    try runMatchTest(&routes, &cases);
}

test "normalized match" {
    const routes = [_][]const u8{
        "/x/{foo}/bar",
        "/x/{bar}/baz",
        "/{foo}/{baz}/bax",
        "/{foo}/{bar}/baz",
        "/{fod}/{baz}/{bax}/foo",
        "/{fod}/baz/bax/foo",
        "/{foo}/baz/bax",
        "/{bar}/{bay}/bay",
        "/s",
        "/s/s",
        "/s/s/s",
        "/s/s/s/s",
        "/s/s/{s}/x",
        "/s/s/{y}/d",
    };
    const cases = [_]MatchCase{
        .{ .path = "/x/foo/bar", .route = "/x/{foo}/bar", .params = params(&[_]ParamPair{.{ .key = "foo", .value = "foo" }}) },
        .{ .path = "/x/foo/baz", .route = "/x/{bar}/baz", .params = params(&[_]ParamPair{.{ .key = "bar", .value = "foo" }}) },
        .{
            .path = "/y/foo/baz",
            .route = "/{foo}/{bar}/baz",
            .params = params(&[_]ParamPair{ .{ .key = "foo", .value = "y" }, .{ .key = "bar", .value = "foo" } }),
        },
        .{
            .path = "/y/foo/bax",
            .route = "/{foo}/{baz}/bax",
            .params = params(&[_]ParamPair{ .{ .key = "foo", .value = "y" }, .{ .key = "baz", .value = "foo" } }),
        },
        .{
            .path = "/y/baz/baz",
            .route = "/{foo}/{bar}/baz",
            .params = params(&[_]ParamPair{ .{ .key = "foo", .value = "y" }, .{ .key = "bar", .value = "baz" } }),
        },
        .{ .path = "/y/baz/bax/foo", .route = "/{fod}/baz/bax/foo", .params = params(&[_]ParamPair{.{ .key = "fod", .value = "y" }}) },
        .{
            .path = "/y/baz/b/foo",
            .route = "/{fod}/{baz}/{bax}/foo",
            .params = params(&[_]ParamPair{ .{ .key = "fod", .value = "y" }, .{ .key = "baz", .value = "baz" }, .{ .key = "bax", .value = "b" } }),
        },
        .{ .path = "/y/baz/bax", .route = "/{foo}/baz/bax", .params = params(&[_]ParamPair{.{ .key = "foo", .value = "y" }}) },
        .{
            .path = "/z/bar/bay",
            .route = "/{bar}/{bay}/bay",
            .params = params(&[_]ParamPair{ .{ .key = "bar", .value = "z" }, .{ .key = "bay", .value = "bar" } }),
        },
        .{ .path = "/s", .route = "/s", .params = params(&[_]ParamPair{}) },
        .{ .path = "/s/s", .route = "/s/s", .params = params(&[_]ParamPair{}) },
        .{ .path = "/s/s/s", .route = "/s/s/s", .params = params(&[_]ParamPair{}) },
        .{ .path = "/s/s/s/s", .route = "/s/s/s/s", .params = params(&[_]ParamPair{}) },
        .{ .path = "/s/s/s/x", .route = "/s/s/{s}/x", .params = params(&[_]ParamPair{.{ .key = "s", .value = "s" }}) },
        .{ .path = "/s/s/s/d", .route = "/s/s/{y}/d", .params = params(&[_]ParamPair{.{ .key = "y", .value = "s" }}) },
    };
    try runMatchTest(&routes, &cases);
}

test "blog" {
    const routes = [_][]const u8{
        "/{page}",
        "/posts/{year}/{month}/{post}",
        "/posts/{year}/{month}/index",
        "/posts/{year}/top",
        "/static/{*path}",
        "/favicon.ico",
    };
    const cases = [_]MatchCase{
        .{ .path = "/about", .route = "/{page}", .params = params(&[_]ParamPair{.{ .key = "page", .value = "about" }}) },
        .{
            .path = "/posts/2021/01/rust",
            .route = "/posts/{year}/{month}/{post}",
            .params = params(&[_]ParamPair{
                .{ .key = "year", .value = "2021" },
                .{ .key = "month", .value = "01" },
                .{ .key = "post", .value = "rust" },
            }),
        },
        .{
            .path = "/posts/2021/01/index",
            .route = "/posts/{year}/{month}/index",
            .params = params(&[_]ParamPair{ .{ .key = "year", .value = "2021" }, .{ .key = "month", .value = "01" } }),
        },
        .{
            .path = "/posts/2021/top",
            .route = "/posts/{year}/top",
            .params = params(&[_]ParamPair{.{ .key = "year", .value = "2021" }}),
        },
        .{
            .path = "/static/foo.png",
            .route = "/static/{*path}",
            .params = params(&[_]ParamPair{.{ .key = "path", .value = "foo.png" }}),
        },
        .{ .path = "/favicon.ico", .route = "/favicon.ico", .params = params(&[_]ParamPair{}) },
    };
    try runMatchTest(&routes, &cases);
}

test "double overlap" {
    const routes = [_][]const u8{
        "/{object}/{id}",
        "/secret/{id}/path",
        "/secret/978",
        "/other/{object}/{id}/",
        "/other/an_object/{id}",
        "/other/static/path",
        "/other/long/static/path/",
    };
    const cases = [_]MatchCase{
        .{
            .path = "/secret/978/path",
            .route = "/secret/{id}/path",
            .params = params(&[_]ParamPair{.{ .key = "id", .value = "978" }}),
        },
        .{
            .path = "/some_object/978",
            .route = "/{object}/{id}",
            .params = params(&[_]ParamPair{ .{ .key = "object", .value = "some_object" }, .{ .key = "id", .value = "978" } }),
        },
        .{ .path = "/secret/978", .route = "/secret/978", .params = params(&[_]ParamPair{}) },
        .{ .path = "/super_secret/978/", .route = "", .params = .err },
        .{
            .path = "/other/object/1/",
            .route = "/other/{object}/{id}/",
            .params = params(&[_]ParamPair{ .{ .key = "object", .value = "object" }, .{ .key = "id", .value = "1" } }),
        },
        .{ .path = "/other/object/1/2", .route = "/other/{object}/{id}", .params = .err },
        .{
            .path = "/other/an_object/1",
            .route = "/other/an_object/{id}",
            .params = params(&[_]ParamPair{.{ .key = "id", .value = "1" }}),
        },
        .{ .path = "/other/static/path", .route = "/other/static/path", .params = params(&[_]ParamPair{}) },
        .{ .path = "/other/long/static/path/", .route = "/other/long/static/path/", .params = params(&[_]ParamPair{}) },
    };
    try runMatchTest(&routes, &cases);
}

test "catchall off by one" {
    const routes = [_][]const u8{ "/foo/{*catchall}", "/bar", "/bar/", "/bar/{*catchall}" };
    const cases = [_]MatchCase{
        .{ .path = "/foo", .route = "", .params = .err },
        .{ .path = "/foo/", .route = "", .params = .err },
        .{
            .path = "/foo/x",
            .route = "/foo/{*catchall}",
            .params = params(&[_]ParamPair{.{ .key = "catchall", .value = "x" }}),
        },
        .{ .path = "/bar", .route = "/bar", .params = params(&[_]ParamPair{}) },
        .{ .path = "/bar/", .route = "/bar/", .params = params(&[_]ParamPair{}) },
        .{
            .path = "/bar/x",
            .route = "/bar/{*catchall}",
            .params = params(&[_]ParamPair{.{ .key = "catchall", .value = "x" }}),
        },
    };
    try runMatchTest(&routes, &cases);
}

test "overlap" {
    const routes = [_][]const u8{
        "/foo",
        "/bar",
        "/{*bar}",
        "/baz",
        "/baz/",
        "/baz/x",
        "/baz/{xxx}",
        "/",
        "/xxx/{*x}",
        "/xxx/",
    };
    const cases = [_]MatchCase{
        .{ .path = "/foo", .route = "/foo", .params = params(&[_]ParamPair{}) },
        .{ .path = "/bar", .route = "/bar", .params = params(&[_]ParamPair{}) },
        .{ .path = "/baz", .route = "/baz", .params = params(&[_]ParamPair{}) },
        .{ .path = "/baz/", .route = "/baz/", .params = params(&[_]ParamPair{}) },
        .{ .path = "/baz/x", .route = "/baz/x", .params = params(&[_]ParamPair{}) },
        .{ .path = "/???", .route = "/{*bar}", .params = params(&[_]ParamPair{.{ .key = "bar", .value = "???" }}) },
        .{ .path = "/", .route = "/", .params = params(&[_]ParamPair{}) },
        .{ .path = "", .route = "", .params = .err },
        .{ .path = "/xxx/y", .route = "/xxx/{*x}", .params = params(&[_]ParamPair{.{ .key = "x", .value = "y" }}) },
        .{ .path = "/xxx/", .route = "/xxx/", .params = params(&[_]ParamPair{}) },
        .{ .path = "/xxx", .route = "/{*bar}", .params = params(&[_]ParamPair{.{ .key = "bar", .value = "xxx" }}) },
    };
    try runMatchTest(&routes, &cases);
}

test "missing trailing slash param" {
    const routes = [_][]const u8{ "/foo/{object}/{id}", "/foo/bar/baz", "/foo/secret/978/" };
    const cases = [_]MatchCase{
        .{ .path = "/foo/secret/978/", .route = "/foo/secret/978/", .params = params(&[_]ParamPair{}) },
        .{
            .path = "/foo/secret/978",
            .route = "/foo/{object}/{id}",
            .params = params(&[_]ParamPair{ .{ .key = "object", .value = "secret" }, .{ .key = "id", .value = "978" } }),
        },
    };
    try runMatchTest(&routes, &cases);
}

test "extra trailing slash param" {
    const routes = [_][]const u8{ "/foo/{object}/{id}", "/foo/bar/baz", "/foo/secret/978" };
    const cases = [_]MatchCase{
        .{ .path = "/foo/secret/978/", .route = "", .params = .err },
        .{ .path = "/foo/secret/978", .route = "/foo/secret/978", .params = params(&[_]ParamPair{}) },
    };
    try runMatchTest(&routes, &cases);
}

test "missing trailing slash catch all" {
    const routes = [_][]const u8{ "/foo/{*bar}", "/foo/bar/baz", "/foo/secret/978/" };
    const cases = [_]MatchCase{
        .{
            .path = "/foo/secret/978",
            .route = "/foo/{*bar}",
            .params = params(&[_]ParamPair{.{ .key = "bar", .value = "secret/978" }}),
        },
        .{ .path = "/foo/secret/978/", .route = "/foo/secret/978/", .params = params(&[_]ParamPair{}) },
    };
    try runMatchTest(&routes, &cases);
}

test "extra trailing slash catch all" {
    const routes = [_][]const u8{ "/foo/{*bar}", "/foo/bar/baz", "/foo/secret/978" };
    const cases = [_]MatchCase{
        .{
            .path = "/foo/secret/978/",
            .route = "/foo/{*bar}",
            .params = params(&[_]ParamPair{.{ .key = "bar", .value = "secret/978/" }}),
        },
        .{ .path = "/foo/secret/978", .route = "/foo/secret/978", .params = params(&[_]ParamPair{}) },
    };
    try runMatchTest(&routes, &cases);
}

test "double overlap trailing slash" {
    const routes = [_][]const u8{
        "/{object}/{id}",
        "/secret/{id}/path",
        "/secret/978/",
        "/other/{object}/{id}/",
        "/other/an_object/{id}",
        "/other/static/path",
        "/other/long/static/path/",
    };
    const cases = [_]MatchCase{
        .{ .path = "/secret/978/path/", .route = "", .params = .err },
        .{ .path = "/object/id/", .route = "", .params = .err },
        .{ .path = "/object/id/path", .route = "", .params = .err },
        .{ .path = "/other/object/1", .route = "", .params = .err },
        .{ .path = "/other/object/1/2", .route = "", .params = .err },
        .{
            .path = "/other/an_object/1/",
            .route = "/other/{object}/{id}/",
            .params = params(&[_]ParamPair{ .{ .key = "object", .value = "an_object" }, .{ .key = "id", .value = "1" } }),
        },
        .{
            .path = "/other/static/path/",
            .route = "/other/{object}/{id}/",
            .params = params(&[_]ParamPair{ .{ .key = "object", .value = "static" }, .{ .key = "id", .value = "path" } }),
        },
        .{ .path = "/other/long/static/path", .route = "", .params = .err },
        .{ .path = "/other/object/static/path", .route = "", .params = .err },
    };
    try runMatchTest(&routes, &cases);
}

test "trailing slash overlap" {
    const routes = [_][]const u8{ "/foo/{x}/baz/", "/foo/{x}/baz", "/foo/bar/bar" };
    const cases = [_]MatchCase{
        .{ .path = "/foo/x/baz/", .route = "/foo/{x}/baz/", .params = params(&[_]ParamPair{.{ .key = "x", .value = "x" }}) },
        .{ .path = "/foo/x/baz", .route = "/foo/{x}/baz", .params = params(&[_]ParamPair{.{ .key = "x", .value = "x" }}) },
        .{ .path = "/foo/bar/bar", .route = "/foo/bar/bar", .params = params(&[_]ParamPair{}) },
    };
    try runMatchTest(&routes, &cases);
}

test "trailing slash" {
    const routes = [_][]const u8{
        "/hi",
        "/b/",
        "/search/{query}",
        "/cmd/{tool}/",
        "/src/{*filepath}",
        "/x",
        "/x/y",
        "/y/",
        "/y/z",
        "/0/{id}",
        "/0/{id}/1",
        "/1/{id}/",
        "/1/{id}/2",
        "/aa",
        "/a/",
        "/admin",
        "/admin/static",
        "/admin/{category}",
        "/admin/{category}/{page}",
        "/doc",
        "/doc/rust_faq.html",
        "/doc/rust1.26.html",
        "/no/a",
        "/no/b",
        "/no/a/b/{*other}",
        "/api/{page}/{name}",
        "/api/hello/{name}/bar/",
        "/api/bar/{name}",
        "/api/baz/foo",
        "/api/baz/foo/bar",
        "/foo/{p}",
    };
    const cases = [_]MatchCase{
        .{ .path = "/hi/", .route = "", .params = .err },
        .{ .path = "/b", .route = "", .params = .err },
        .{ .path = "/search/rustacean/", .route = "", .params = .err },
        .{ .path = "/cmd/vet", .route = "", .params = .err },
        .{ .path = "/src", .route = "", .params = .err },
        .{ .path = "/src/", .route = "", .params = .err },
        .{ .path = "/x/", .route = "", .params = .err },
        .{ .path = "/y", .route = "", .params = .err },
        .{ .path = "/0/rust/", .route = "", .params = .err },
        .{ .path = "/1/rust", .route = "", .params = .err },
        .{ .path = "/a", .route = "", .params = .err },
        .{ .path = "/admin/", .route = "", .params = .err },
        .{ .path = "/doc/", .route = "", .params = .err },
        .{ .path = "/admin/static/", .route = "", .params = .err },
        .{ .path = "/admin/cfg/", .route = "", .params = .err },
        .{ .path = "/admin/cfg/users/", .route = "", .params = .err },
        .{ .path = "/api/hello/x/bar", .route = "", .params = .err },
        .{ .path = "/api/baz/foo/", .route = "", .params = .err },
        .{ .path = "/api/baz/bax/", .route = "", .params = .err },
        .{ .path = "/api/bar/huh/", .route = "", .params = .err },
        .{ .path = "/api/baz/foo/bar/", .route = "", .params = .err },
        .{ .path = "/api/world/abc/", .route = "", .params = .err },
        .{ .path = "/foo/pp/", .route = "", .params = .err },
        .{ .path = "/", .route = "", .params = .err },
        .{ .path = "/no", .route = "", .params = .err },
        .{ .path = "/no/", .route = "", .params = .err },
        .{ .path = "/no/a/b", .route = "", .params = .err },
        .{ .path = "/no/a/b/", .route = "", .params = .err },
        .{ .path = "/_", .route = "", .params = .err },
        .{ .path = "/_/", .route = "", .params = .err },
        .{ .path = "/api", .route = "", .params = .err },
        .{ .path = "/api/", .route = "", .params = .err },
        .{ .path = "/api/hello/x/foo", .route = "", .params = .err },
        .{ .path = "/api/baz/foo/bad", .route = "", .params = .err },
        .{ .path = "/foo/p/p", .route = "", .params = .err },
    };
    try runMatchTest(&routes, &cases);
}

test "backtracking trailing slash" {
    const routes = [_][]const u8{ "/a/{b}/{c}", "/a/b/{c}/d/" };
    const cases = [_]MatchCase{
        .{ .path = "/a/b/c/d", .route = "", .params = .err },
    };
    try runMatchTest(&routes, &cases);
}

test "root trailing slash" {
    const routes = [_][]const u8{ "/foo", "/bar", "/{baz}" };
    const cases = [_]MatchCase{
        .{ .path = "/", .route = "", .params = .err },
    };
    try runMatchTest(&routes, &cases);
}

test "catchall overlap" {
    const routes = [_][]const u8{ "/yyy/{*x}", "/yyy{*x}" };
    const cases = [_]MatchCase{
        .{ .path = "/yyy/y", .route = "/yyy/{*x}", .params = params(&[_]ParamPair{.{ .key = "x", .value = "y" }}) },
        .{ .path = "/yyy/", .route = "/yyy{*x}", .params = params(&[_]ParamPair{.{ .key = "x", .value = "/" }}) },
    };
    try runMatchTest(&routes, &cases);
}

test "escaped match" {
    const routes = [_][]const u8{
        "/",
        "/{{",
        "/}}",
        "/{{x",
        "/}}y{{",
        "/xy{{",
        "/{{/xyz",
        "/{ba{{r}",
        "/{ba{{r}/",
        "/{ba{{r}/x",
        "/baz/{xxx}",
        "/baz/{xxx}/xy{{",
        "/baz/{xxx}/}}xy{{{{",
        "/{{/{x}",
        "/xxx/",
        "/xxx/{x}}{{}}}}{{}}{{{{}}y}",
    };
    const cases = [_]MatchCase{
        .{ .path = "/", .route = "/", .params = params(&[_]ParamPair{}) },
        .{ .path = "/{", .route = "/{{", .params = params(&[_]ParamPair{}) },
        .{ .path = "/}", .route = "/}}", .params = params(&[_]ParamPair{}) },
        .{ .path = "/{x", .route = "/{{x", .params = params(&[_]ParamPair{}) },
        .{ .path = "/}y{", .route = "/}}y{{", .params = params(&[_]ParamPair{}) },
        .{ .path = "/xy{", .route = "/xy{{", .params = params(&[_]ParamPair{}) },
        .{ .path = "/{/xyz", .route = "/{{/xyz", .params = params(&[_]ParamPair{}) },
        .{ .path = "/foo", .route = "/{ba{{r}", .params = params(&[_]ParamPair{.{ .key = "ba{r", .value = "foo" }}) },
        .{ .path = "/{{", .route = "/{ba{{r}", .params = params(&[_]ParamPair{.{ .key = "ba{r", .value = "{{" }}) },
        .{ .path = "/{{}}/", .route = "/{ba{{r}/", .params = params(&[_]ParamPair{.{ .key = "ba{r", .value = "{{}}" }}) },
        .{ .path = "/{{}}{{/x", .route = "/{ba{{r}/x", .params = params(&[_]ParamPair{.{ .key = "ba{r", .value = "{{}}{{" }}) },
        .{ .path = "/baz/x", .route = "/baz/{xxx}", .params = params(&[_]ParamPair{.{ .key = "xxx", .value = "x" }}) },
        .{ .path = "/baz/x/xy{", .route = "/baz/{xxx}/xy{{", .params = params(&[_]ParamPair{.{ .key = "xxx", .value = "x" }}) },
        .{ .path = "/baz/x/xy{{", .route = "", .params = .err },
        .{ .path = "/baz/x/}xy{{", .route = "/baz/{xxx}/}}xy{{{{", .params = params(&[_]ParamPair{.{ .key = "xxx", .value = "x" }}) },
        .{ .path = "/{/{{", .route = "/{{/{x}", .params = params(&[_]ParamPair{.{ .key = "x", .value = "{{" }}) },
        .{ .path = "/xxx", .route = "/{ba{{r}", .params = params(&[_]ParamPair{.{ .key = "ba{r", .value = "xxx" }}) },
        .{ .path = "/xxx/", .route = "/xxx/", .params = params(&[_]ParamPair{}) },
        .{
            .path = "/xxx/foo",
            .route = "/xxx/{x}}{{}}}}{{}}{{{{}}y}",
            .params = params(&[_]ParamPair{.{ .key = "x}{}}{}{{}y", .value = "foo" }}),
        },
    };
    try runMatchTest(&routes, &cases);
}

test "empty param" {
    const routes = [_][]const u8{
        "/y/{foo}",
        "/x/{foo}/z",
        "/z/{*foo}",
        "/a/x{foo}",
        "/b/{foo}x",
    };
    const cases = [_]MatchCase{
        .{ .path = "/y/", .route = "", .params = .err },
        .{ .path = "/x//z", .route = "", .params = .err },
        .{ .path = "/z/", .route = "", .params = .err },
        .{ .path = "/a/x", .route = "", .params = .err },
        .{ .path = "/b/x", .route = "", .params = .err },
    };
    try runMatchTest(&routes, &cases);
}

test "wildcard suffix" {
    const routes = [_][]const u8{ "/", "/{foo}x", "/foox", "/{foo}x/bar", "/{foo}x/bar/baz" };
    const cases = [_]MatchCase{
        .{ .path = "/", .route = "/", .params = params(&[_]ParamPair{}) },
        .{ .path = "/foox", .route = "/foox", .params = params(&[_]ParamPair{}) },
        .{ .path = "/barx", .route = "/{foo}x", .params = params(&[_]ParamPair{.{ .key = "foo", .value = "bar" }}) },
        .{ .path = "/mx", .route = "/{foo}x", .params = params(&[_]ParamPair{.{ .key = "foo", .value = "m" }}) },
        .{ .path = "/mx/", .route = "", .params = .err },
        .{ .path = "/mxm", .route = "", .params = .err },
        .{ .path = "/mx/bar", .route = "/{foo}x/bar", .params = params(&[_]ParamPair{.{ .key = "foo", .value = "m" }}) },
        .{ .path = "/mxm/bar", .route = "", .params = .err },
        .{ .path = "/x", .route = "", .params = .err },
        .{ .path = "/xfoo", .route = "", .params = .err },
        .{ .path = "/xfoox", .route = "/{foo}x", .params = params(&[_]ParamPair{.{ .key = "foo", .value = "xfoo" }}) },
        .{ .path = "/xfoox/bar", .route = "/{foo}x/bar", .params = params(&[_]ParamPair{.{ .key = "foo", .value = "xfoo" }}) },
        .{ .path = "/xfoox/bar/baz", .route = "/{foo}x/bar/baz", .params = params(&[_]ParamPair{.{ .key = "foo", .value = "xfoo" }}) },
    };
    try runMatchTest(&routes, &cases);
}

test "mixed wildcard suffix" {
    const routes = [_][]const u8{
        "/",
        "/{f}o/b",
        "/{f}oo/b",
        "/{f}ooo/b",
        "/{f}oooo/b",
        "/foo/b",
        "/foo/{b}",
        "/foo/{b}one",
        "/foo/{b}one/",
        "/foo/{b}two",
        "/foo/{b}/one",
        "/foo/{b}one/one",
        "/foo/{b}two/one",
        "/foo/{b}one/one/",
        "/bar/{b}one",
        "/bar/{b}",
        "/bar/{b}/baz",
        "/bar/{b}one/baz",
        "/baz/{b}/bar",
        "/baz/{b}one/bar",
    };
    const cases = [_]MatchCase{
        .{ .path = "/", .route = "/", .params = params(&[_]ParamPair{}) },
        .{ .path = "/o/b", .route = "", .params = .err },
        .{ .path = "/fo/b", .route = "/{f}o/b", .params = params(&[_]ParamPair{.{ .key = "f", .value = "f" }}) },
        .{ .path = "/foo/b", .route = "/foo/b", .params = params(&[_]ParamPair{}) },
        .{ .path = "/fooo/b", .route = "/{f}ooo/b", .params = params(&[_]ParamPair{.{ .key = "f", .value = "f" }}) },
        .{ .path = "/foooo/b", .route = "/{f}oooo/b", .params = params(&[_]ParamPair{.{ .key = "f", .value = "f" }}) },
        .{ .path = "/foo/b/", .route = "", .params = .err },
        .{ .path = "/foooo/b/", .route = "", .params = .err },
        .{ .path = "/foo/bb", .route = "/foo/{b}", .params = params(&[_]ParamPair{.{ .key = "b", .value = "bb" }}) },
        .{ .path = "/foo/bone", .route = "/foo/{b}one", .params = params(&[_]ParamPair{.{ .key = "b", .value = "b" }}) },
        .{ .path = "/foo/bone/", .route = "/foo/{b}one/", .params = params(&[_]ParamPair{.{ .key = "b", .value = "b" }}) },
        .{ .path = "/foo/btwo", .route = "/foo/{b}two", .params = params(&[_]ParamPair{.{ .key = "b", .value = "b" }}) },
        .{ .path = "/foo/btwo/", .route = "", .params = .err },
        .{ .path = "/foo/b/one", .route = "/foo/{b}/one", .params = params(&[_]ParamPair{.{ .key = "b", .value = "b" }}) },
        .{ .path = "/foo/bone/one", .route = "/foo/{b}one/one", .params = params(&[_]ParamPair{.{ .key = "b", .value = "b" }}) },
        .{ .path = "/foo/bone/one/", .route = "/foo/{b}one/one/", .params = params(&[_]ParamPair{.{ .key = "b", .value = "b" }}) },
        .{ .path = "/foo/btwo/one", .route = "/foo/{b}two/one", .params = params(&[_]ParamPair{.{ .key = "b", .value = "b" }}) },
        .{ .path = "/bar/b", .route = "/bar/{b}", .params = params(&[_]ParamPair{.{ .key = "b", .value = "b" }}) },
        .{ .path = "/bar/b/baz", .route = "/bar/{b}/baz", .params = params(&[_]ParamPair{.{ .key = "b", .value = "b" }}) },
        .{ .path = "/bar/bone", .route = "/bar/{b}one", .params = params(&[_]ParamPair{.{ .key = "b", .value = "b" }}) },
        .{ .path = "/bar/bone/baz", .route = "/bar/{b}one/baz", .params = params(&[_]ParamPair{.{ .key = "b", .value = "b" }}) },
        .{ .path = "/baz/b/bar", .route = "/baz/{b}/bar", .params = params(&[_]ParamPair{.{ .key = "b", .value = "b" }}) },
        .{ .path = "/baz/bone/bar", .route = "/baz/{b}one/bar", .params = params(&[_]ParamPair{.{ .key = "b", .value = "b" }}) },
    };
    try runMatchTest(&routes, &cases);
}

test "basic" {
    const routes = [_][]const u8{
        "/hi",
        "/contact",
        "/co",
        "/c",
        "/a",
        "/ab",
        "/doc/",
        "/doc/rust_faq.html",
        "/doc/rust1.26.html",
        "/ʯ",
        "/β",
        "/sd!here",
        "/sd$here",
        "/sd&here",
        "/sd'here",
        "/sd(here",
        "/sd)here",
        "/sd+here",
        "/sd,here",
        "/sd;here",
        "/sd=here",
    };
    const cases = [_]MatchCase{
        .{ .path = "/a", .route = "/a", .params = params(&[_]ParamPair{}) },
        .{ .path = "", .route = "", .params = .err },
        .{ .path = "/hi", .route = "/hi", .params = params(&[_]ParamPair{}) },
        .{ .path = "/contact", .route = "/contact", .params = params(&[_]ParamPair{}) },
        .{ .path = "/co", .route = "/co", .params = params(&[_]ParamPair{}) },
        .{ .path = "", .route = "", .params = .err },
        .{ .path = "", .route = "", .params = .err },
        .{ .path = "", .route = "", .params = .err },
        .{ .path = "/ab", .route = "/ab", .params = params(&[_]ParamPair{}) },
        .{ .path = "/ʯ", .route = "/ʯ", .params = params(&[_]ParamPair{}) },
        .{ .path = "/β", .route = "/β", .params = params(&[_]ParamPair{}) },
        .{ .path = "/sd!here", .route = "/sd!here", .params = params(&[_]ParamPair{}) },
        .{ .path = "/sd$here", .route = "/sd$here", .params = params(&[_]ParamPair{}) },
        .{ .path = "/sd&here", .route = "/sd&here", .params = params(&[_]ParamPair{}) },
        .{ .path = "/sd'here", .route = "/sd'here", .params = params(&[_]ParamPair{}) },
        .{ .path = "/sd(here", .route = "/sd(here", .params = params(&[_]ParamPair{}) },
        .{ .path = "/sd)here", .route = "/sd)here", .params = params(&[_]ParamPair{}) },
        .{ .path = "/sd+here", .route = "/sd+here", .params = params(&[_]ParamPair{}) },
        .{ .path = "/sd,here", .route = "/sd,here", .params = params(&[_]ParamPair{}) },
        .{ .path = "/sd;here", .route = "/sd;here", .params = params(&[_]ParamPair{}) },
        .{ .path = "/sd=here", .route = "/sd=here", .params = params(&[_]ParamPair{}) },
    };
    try runMatchTest(&routes, &cases);
}

test "wildcard" {
    const routes = [_][]const u8{
        "/",
        "/cmd/{tool}/",
        "/cmd/{tool2}/{sub}",
        "/cmd/whoami",
        "/cmd/whoami/root",
        "/cmd/whoami/root/",
        "/src",
        "/src/",
        "/src/{*filepath}",
        "/search/",
        "/search/{query}",
        "/search/actix-web",
        "/search/google",
        "/user_{name}",
        "/user_{name}/about",
        "/files/{dir}/{*filepath}",
        "/doc/",
        "/doc/rust_faq.html",
        "/doc/rust1.26.html",
        "/info/{user}/public",
        "/info/{user}/project/{project}",
        "/info/{user}/project/rustlang",
        "/aa/{*xx}",
        "/ab/{*xx}",
        "/ab/hello{*xx}",
        "/{cc}",
        "/c1/{dd}/e",
        "/c1/{dd}/e1",
        "/{cc}/cc",
        "/{cc}/{dd}/ee",
        "/{cc}/{dd}/{ee}/ff",
        "/{cc}/{dd}/{ee}/{ff}/gg",
        "/{cc}/{dd}/{ee}/{ff}/{gg}/hh",
        "/get/test/abc/",
        "/get/{param}/abc/",
        "/something/{paramname}/thirdthing",
        "/something/secondthing/test",
        "/get/abc",
        "/get/{param}",
        "/get/abc/123abc",
        "/get/abc/{param}",
        "/get/abc/123abc/xxx8",
        "/get/abc/123abc/{param}",
        "/get/abc/123abc/xxx8/1234",
        "/get/abc/123abc/xxx8/{param}",
        "/get/abc/123abc/xxx8/1234/ffas",
        "/get/abc/123abc/xxx8/1234/{param}",
        "/get/abc/123abc/xxx8/1234/kkdd/12c",
        "/get/abc/123abc/xxx8/1234/kkdd/{param}",
        "/get/abc/{param}/test",
        "/get/abc/123abd/{param}",
        "/get/abc/123abddd/{param}",
        "/get/abc/123/{param}",
        "/get/abc/123abg/{param}",
        "/get/abc/123abf/{param}",
        "/get/abc/123abfff/{param}",
    };
    const cases = [_]MatchCase{
        .{ .path = "/", .route = "/", .params = params(&[_]ParamPair{}) },
        .{ .path = "/cmd/test", .route = "", .params = .err },
        .{
            .path = "/cmd/test/",
            .route = "/cmd/{tool}/",
            .params = params(&[_]ParamPair{.{ .key = "tool", .value = "test" }}),
        },
        .{
            .path = "/cmd/test/3",
            .route = "/cmd/{tool2}/{sub}",
            .params = params(&[_]ParamPair{
                .{ .key = "tool2", .value = "test" },
                .{ .key = "sub", .value = "3" },
            }),
        },
        .{ .path = "/cmd/who", .route = "", .params = .err },
        .{
            .path = "/cmd/who/",
            .route = "/cmd/{tool}/",
            .params = params(&[_]ParamPair{.{ .key = "tool", .value = "who" }}),
        },
        .{ .path = "/cmd/whoami", .route = "/cmd/whoami", .params = params(&[_]ParamPair{}) },
        .{
            .path = "/cmd/whoami/",
            .route = "/cmd/{tool}/",
            .params = params(&[_]ParamPair{.{ .key = "tool", .value = "whoami" }}),
        },
        .{
            .path = "/cmd/whoami/r",
            .route = "/cmd/{tool2}/{sub}",
            .params = params(&[_]ParamPair{
                .{ .key = "tool2", .value = "whoami" },
                .{ .key = "sub", .value = "r" },
            }),
        },
        .{ .path = "/cmd/whoami/r/", .route = "", .params = .err },
        .{
            .path = "/cmd/whoami/root",
            .route = "/cmd/whoami/root",
            .params = params(&[_]ParamPair{}),
        },
        .{
            .path = "/cmd/whoami/root/",
            .route = "/cmd/whoami/root/",
            .params = params(&[_]ParamPair{}),
        },
        .{ .path = "/src", .route = "/src", .params = params(&[_]ParamPair{}) },
        .{ .path = "/src/", .route = "/src/", .params = params(&[_]ParamPair{}) },
        .{
            .path = "/src/some/file.png",
            .route = "/src/{*filepath}",
            .params = params(&[_]ParamPair{.{ .key = "filepath", .value = "some/file.png" }}),
        },
        .{ .path = "/search/", .route = "/search/", .params = params(&[_]ParamPair{}) },
        .{
            .path = "/search/actix",
            .route = "/search/{query}",
            .params = params(&[_]ParamPair{.{ .key = "query", .value = "actix" }}),
        },
        .{
            .path = "/search/actix-web",
            .route = "/search/actix-web",
            .params = params(&[_]ParamPair{}),
        },
        .{
            .path = "/search/someth!ng+in+ünìcodé",
            .route = "/search/{query}",
            .params = params(&[_]ParamPair{
                .{ .key = "query", .value = "someth!ng+in+ünìcodé" },
            }),
        },
        .{ .path = "/search/someth!ng+in+ünìcodé/", .route = "", .params = .err },
        .{
            .path = "/user_rustacean",
            .route = "/user_{name}",
            .params = params(&[_]ParamPair{.{ .key = "name", .value = "rustacean" }}),
        },
        .{
            .path = "/user_rustacean/about",
            .route = "/user_{name}/about",
            .params = params(&[_]ParamPair{.{ .key = "name", .value = "rustacean" }}),
        },
        .{
            .path = "/files/js/inc/framework.js",
            .route = "/files/{dir}/{*filepath}",
            .params = params(&[_]ParamPair{
                .{ .key = "dir", .value = "js" },
                .{ .key = "filepath", .value = "inc/framework.js" },
            }),
        },
        .{
            .path = "/info/gordon/public",
            .route = "/info/{user}/public",
            .params = params(&[_]ParamPair{.{ .key = "user", .value = "gordon" }}),
        },
        .{
            .path = "/info/gordon/project/rust",
            .route = "/info/{user}/project/{project}",
            .params = params(&[_]ParamPair{
                .{ .key = "user", .value = "gordon" },
                .{ .key = "project", .value = "rust" },
            }),
        },
        .{
            .path = "/info/gordon/project/rustlang",
            .route = "/info/{user}/project/rustlang",
            .params = params(&[_]ParamPair{.{ .key = "user", .value = "gordon" }}),
        },
        .{ .path = "/aa/", .route = "", .params = .err },
        .{
            .path = "/aa/aa",
            .route = "/aa/{*xx}",
            .params = params(&[_]ParamPair{.{ .key = "xx", .value = "aa" }}),
        },
        .{
            .path = "/ab/ab",
            .route = "/ab/{*xx}",
            .params = params(&[_]ParamPair{.{ .key = "xx", .value = "ab" }}),
        },
        .{
            .path = "/ab/hello-world",
            .route = "/ab/hello{*xx}",
            .params = params(&[_]ParamPair{.{ .key = "xx", .value = "-world" }}),
        },
        .{
            .path = "/a",
            .route = "/{cc}",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "a" }}),
        },
        .{
            .path = "/all",
            .route = "/{cc}",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "all" }}),
        },
        .{
            .path = "/d",
            .route = "/{cc}",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "d" }}),
        },
        .{
            .path = "/ad",
            .route = "/{cc}",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "ad" }}),
        },
        .{
            .path = "/dd",
            .route = "/{cc}",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "dd" }}),
        },
        .{
            .path = "/dddaa",
            .route = "/{cc}",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "dddaa" }}),
        },
        .{
            .path = "/aa",
            .route = "/{cc}",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "aa" }}),
        },
        .{
            .path = "/aaa",
            .route = "/{cc}",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "aaa" }}),
        },
        .{
            .path = "/aaa/cc",
            .route = "/{cc}/cc",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "aaa" }}),
        },
        .{
            .path = "/ab",
            .route = "/{cc}",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "ab" }}),
        },
        .{
            .path = "/abb",
            .route = "/{cc}",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "abb" }}),
        },
        .{
            .path = "/abb/cc",
            .route = "/{cc}/cc",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "abb" }}),
        },
        .{
            .path = "/allxxxx",
            .route = "/{cc}",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "allxxxx" }}),
        },
        .{
            .path = "/alldd",
            .route = "/{cc}",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "alldd" }}),
        },
        .{
            .path = "/all/cc",
            .route = "/{cc}/cc",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "all" }}),
        },
        .{
            .path = "/a/cc",
            .route = "/{cc}/cc",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "a" }}),
        },
        .{
            .path = "/c1/d/e",
            .route = "/c1/{dd}/e",
            .params = params(&[_]ParamPair{.{ .key = "dd", .value = "d" }}),
        },
        .{
            .path = "/c1/d/e1",
            .route = "/c1/{dd}/e1",
            .params = params(&[_]ParamPair{.{ .key = "dd", .value = "d" }}),
        },
        .{
            .path = "/c1/d/ee",
            .route = "/{cc}/{dd}/ee",
            .params = params(&[_]ParamPair{
                .{ .key = "cc", .value = "c1" },
                .{ .key = "dd", .value = "d" },
            }),
        },
        .{
            .path = "/cc/cc",
            .route = "/{cc}/cc",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "cc" }}),
        },
        .{
            .path = "/ccc/cc",
            .route = "/{cc}/cc",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "ccc" }}),
        },
        .{
            .path = "/deedwjfs/cc",
            .route = "/{cc}/cc",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "deedwjfs" }}),
        },
        .{
            .path = "/acllcc/cc",
            .route = "/{cc}/cc",
            .params = params(&[_]ParamPair{.{ .key = "cc", .value = "acllcc" }}),
        },
        .{
            .path = "/get/test/abc/",
            .route = "/get/test/abc/",
            .params = params(&[_]ParamPair{}),
        },
        .{
            .path = "/get/te/abc/",
            .route = "/get/{param}/abc/",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "te" }}),
        },
        .{
            .path = "/get/testaa/abc/",
            .route = "/get/{param}/abc/",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "testaa" }}),
        },
        .{
            .path = "/get/xx/abc/",
            .route = "/get/{param}/abc/",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "xx" }}),
        },
        .{
            .path = "/get/tt/abc/",
            .route = "/get/{param}/abc/",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "tt" }}),
        },
        .{
            .path = "/get/a/abc/",
            .route = "/get/{param}/abc/",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "a" }}),
        },
        .{
            .path = "/get/t/abc/",
            .route = "/get/{param}/abc/",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "t" }}),
        },
        .{
            .path = "/get/aa/abc/",
            .route = "/get/{param}/abc/",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "aa" }}),
        },
        .{
            .path = "/get/abas/abc/",
            .route = "/get/{param}/abc/",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "abas" }}),
        },
        .{
            .path = "/something/secondthing/test",
            .route = "/something/secondthing/test",
            .params = params(&[_]ParamPair{}),
        },
        .{
            .path = "/something/abcdad/thirdthing",
            .route = "/something/{paramname}/thirdthing",
            .params = params(&[_]ParamPair{.{ .key = "paramname", .value = "abcdad" }}),
        },
        .{
            .path = "/something/secondthingaaaa/thirdthing",
            .route = "/something/{paramname}/thirdthing",
            .params = params(&[_]ParamPair{
                .{ .key = "paramname", .value = "secondthingaaaa" },
            }),
        },
        .{
            .path = "/something/se/thirdthing",
            .route = "/something/{paramname}/thirdthing",
            .params = params(&[_]ParamPair{.{ .key = "paramname", .value = "se" }}),
        },
        .{
            .path = "/something/s/thirdthing",
            .route = "/something/{paramname}/thirdthing",
            .params = params(&[_]ParamPair{.{ .key = "paramname", .value = "s" }}),
        },
        .{
            .path = "/c/d/ee",
            .route = "/{cc}/{dd}/ee",
            .params = params(&[_]ParamPair{
                .{ .key = "cc", .value = "c" },
                .{ .key = "dd", .value = "d" },
            }),
        },
        .{
            .path = "/c/d/e/ff",
            .route = "/{cc}/{dd}/{ee}/ff",
            .params = params(&[_]ParamPair{
                .{ .key = "cc", .value = "c" },
                .{ .key = "dd", .value = "d" },
                .{ .key = "ee", .value = "e" },
            }),
        },
        .{
            .path = "/c/d/e/f/gg",
            .route = "/{cc}/{dd}/{ee}/{ff}/gg",
            .params = params(&[_]ParamPair{
                .{ .key = "cc", .value = "c" },
                .{ .key = "dd", .value = "d" },
                .{ .key = "ee", .value = "e" },
                .{ .key = "ff", .value = "f" },
            }),
        },
        .{
            .path = "/c/d/e/f/g/hh",
            .route = "/{cc}/{dd}/{ee}/{ff}/{gg}/hh",
            .params = params(&[_]ParamPair{
                .{ .key = "cc", .value = "c" },
                .{ .key = "dd", .value = "d" },
                .{ .key = "ee", .value = "e" },
                .{ .key = "ff", .value = "f" },
                .{ .key = "gg", .value = "g" },
            }),
        },
        .{
            .path = "/cc/dd/ee/ff/gg/hh",
            .route = "/{cc}/{dd}/{ee}/{ff}/{gg}/hh",
            .params = params(&[_]ParamPair{
                .{ .key = "cc", .value = "cc" },
                .{ .key = "dd", .value = "dd" },
                .{ .key = "ee", .value = "ee" },
                .{ .key = "ff", .value = "ff" },
                .{ .key = "gg", .value = "gg" },
            }),
        },
        .{ .path = "/get/abc", .route = "/get/abc", .params = params(&[_]ParamPair{}) },
        .{
            .path = "/get/a",
            .route = "/get/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "a" }}),
        },
        .{
            .path = "/get/abz",
            .route = "/get/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "abz" }}),
        },
        .{
            .path = "/get/12a",
            .route = "/get/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "12a" }}),
        },
        .{
            .path = "/get/abcd",
            .route = "/get/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "abcd" }}),
        },
        .{
            .path = "/get/abc/123abc",
            .route = "/get/abc/123abc",
            .params = params(&[_]ParamPair{}),
        },
        .{
            .path = "/get/abc/12",
            .route = "/get/abc/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "12" }}),
        },
        .{
            .path = "/get/abc/123ab",
            .route = "/get/abc/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "123ab" }}),
        },
        .{
            .path = "/get/abc/xyz",
            .route = "/get/abc/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "xyz" }}),
        },
        .{
            .path = "/get/abc/123abcddxx",
            .route = "/get/abc/{param}",
            .params = params(&[_]ParamPair{
                .{ .key = "param", .value = "123abcddxx" },
            }),
        },
        .{
            .path = "/get/abc/123abc/xxx8",
            .route = "/get/abc/123abc/xxx8",
            .params = params(&[_]ParamPair{}),
        },
        .{
            .path = "/get/abc/123abc/x",
            .route = "/get/abc/123abc/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "x" }}),
        },
        .{
            .path = "/get/abc/123abc/xxx",
            .route = "/get/abc/123abc/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "xxx" }}),
        },
        .{
            .path = "/get/abc/123abc/abc",
            .route = "/get/abc/123abc/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "abc" }}),
        },
        .{
            .path = "/get/abc/123abc/xxx8xxas",
            .route = "/get/abc/123abc/{param}",
            .params = params(&[_]ParamPair{
                .{ .key = "param", .value = "xxx8xxas" },
            }),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1234",
            .route = "/get/abc/123abc/xxx8/1234",
            .params = params(&[_]ParamPair{}),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1",
            .route = "/get/abc/123abc/xxx8/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "1" }}),
        },
        .{
            .path = "/get/abc/123abc/xxx8/123",
            .route = "/get/abc/123abc/xxx8/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "123" }}),
        },
        .{
            .path = "/get/abc/123abc/xxx8/78k",
            .route = "/get/abc/123abc/xxx8/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "78k" }}),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1234xxxd",
            .route = "/get/abc/123abc/xxx8/{param}",
            .params = params(&[_]ParamPair{
                .{ .key = "param", .value = "1234xxxd" },
            }),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1234/ffas",
            .route = "/get/abc/123abc/xxx8/1234/ffas",
            .params = params(&[_]ParamPair{}),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1234/f",
            .route = "/get/abc/123abc/xxx8/1234/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "f" }}),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1234/ffa",
            .route = "/get/abc/123abc/xxx8/1234/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "ffa" }}),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1234/kka",
            .route = "/get/abc/123abc/xxx8/1234/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "kka" }}),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1234/ffas321",
            .route = "/get/abc/123abc/xxx8/1234/{param}",
            .params = params(&[_]ParamPair{
                .{ .key = "param", .value = "ffas321" },
            }),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1234/kkdd/12c",
            .route = "/get/abc/123abc/xxx8/1234/kkdd/12c",
            .params = params(&[_]ParamPair{}),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1234/kkdd/1",
            .route = "/get/abc/123abc/xxx8/1234/kkdd/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "1" }}),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1234/kkdd/12",
            .route = "/get/abc/123abc/xxx8/1234/kkdd/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "12" }}),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1234/kkdd/12b",
            .route = "/get/abc/123abc/xxx8/1234/kkdd/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "12b" }}),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1234/kkdd/34",
            .route = "/get/abc/123abc/xxx8/1234/kkdd/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "34" }}),
        },
        .{
            .path = "/get/abc/123abc/xxx8/1234/kkdd/12c2e3",
            .route = "/get/abc/123abc/xxx8/1234/kkdd/{param}",
            .params = params(&[_]ParamPair{
                .{ .key = "param", .value = "12c2e3" },
            }),
        },
        .{
            .path = "/get/abc/12/test",
            .route = "/get/abc/{param}/test",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "12" }}),
        },
        .{
            .path = "/get/abc/123abdd/test",
            .route = "/get/abc/{param}/test",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "123abdd" }}),
        },
        .{
            .path = "/get/abc/123abdddf/test",
            .route = "/get/abc/{param}/test",
            .params = params(&[_]ParamPair{
                .{ .key = "param", .value = "123abdddf" },
            }),
        },
        .{
            .path = "/get/abc/123ab/test",
            .route = "/get/abc/{param}/test",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "123ab" }}),
        },
        .{
            .path = "/get/abc/123abgg/test",
            .route = "/get/abc/{param}/test",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "123abgg" }}),
        },
        .{
            .path = "/get/abc/123abff/test",
            .route = "/get/abc/{param}/test",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "123abff" }}),
        },
        .{
            .path = "/get/abc/123abffff/test",
            .route = "/get/abc/{param}/test",
            .params = params(&[_]ParamPair{
                .{ .key = "param", .value = "123abffff" },
            }),
        },
        .{
            .path = "/get/abc/123abd/test",
            .route = "/get/abc/123abd/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "test" }}),
        },
        .{
            .path = "/get/abc/123abddd/test",
            .route = "/get/abc/123abddd/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "test" }}),
        },
        .{
            .path = "/get/abc/123/test22",
            .route = "/get/abc/123/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "test22" }}),
        },
        .{
            .path = "/get/abc/123abg/test",
            .route = "/get/abc/123abg/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "test" }}),
        },
        .{
            .path = "/get/abc/123abf/testss",
            .route = "/get/abc/123abf/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "testss" }}),
        },
        .{
            .path = "/get/abc/123abfff/te",
            .route = "/get/abc/123abfff/{param}",
            .params = params(&[_]ParamPair{.{ .key = "param", .value = "te" }}),
        },
    };
    try runMatchTest(&routes, &cases);
}

test "remove normalized" {
    const routes = [_][]const u8{
        "/x/{foo}/bar",
        "/x/{bar}/baz",
        "/{foo}/{baz}/bax",
        "/{foo}/{bar}/baz",
        "/{fod}/{baz}/{bax}/foo",
        "/{fod}/baz/bax/foo",
        "/{foo}/baz/bax",
        "/{bar}/{bay}/bay",
        "/s",
        "/s/s",
        "/s/s/s",
        "/s/s/s/s",
        "/s/s/{s}/x",
        "/s/s/{y}/d",
    };
    const ops = [_]RemoveCase{
        .{ .op = .remove, .route = "/x/{foo}/bar", .expected = "/x/{foo}/bar" },
        .{ .op = .remove, .route = "/x/{bar}/baz", .expected = "/x/{bar}/baz" },
        .{ .op = .remove, .route = "/{foo}/{baz}/bax", .expected = "/{foo}/{baz}/bax" },
        .{ .op = .remove, .route = "/{foo}/{bar}/baz", .expected = "/{foo}/{bar}/baz" },
        .{
            .op = .remove,
            .route = "/{fod}/{baz}/{bax}/foo",
            .expected = "/{fod}/{baz}/{bax}/foo",
        },
        .{ .op = .remove, .route = "/{fod}/baz/bax/foo", .expected = "/{fod}/baz/bax/foo" },
        .{ .op = .remove, .route = "/{foo}/baz/bax", .expected = "/{foo}/baz/bax" },
        .{ .op = .remove, .route = "/{bar}/{bay}/bay", .expected = "/{bar}/{bay}/bay" },
        .{ .op = .remove, .route = "/s", .expected = "/s" },
        .{ .op = .remove, .route = "/s/s", .expected = "/s/s" },
        .{ .op = .remove, .route = "/s/s/s", .expected = "/s/s/s" },
        .{ .op = .remove, .route = "/s/s/s/s", .expected = "/s/s/s/s" },
        .{ .op = .remove, .route = "/s/s/{s}/x", .expected = "/s/s/{s}/x" },
        .{ .op = .remove, .route = "/s/s/{y}/d", .expected = "/s/s/{y}/d" },
    };
    const remaining = [_][]const u8{};
    try runRemoveTest(&routes, &ops, &remaining);
}

test "remove basic" {
    const routes = [_][]const u8{ "/home", "/home/{id}" };
    const ops = [_]RemoveCase{
        .{ .op = .remove, .route = "/home", .expected = "/home" },
        .{ .op = .remove, .route = "/home", .expected = null },
        .{ .op = .remove, .route = "/home/{id}", .expected = "/home/{id}" },
        .{ .op = .remove, .route = "/home/{id}", .expected = null },
    };
    const remaining = [_][]const u8{};
    try runRemoveTest(&routes, &ops, &remaining);
}

test "remove blog" {
    const routes = [_][]const u8{
        "/{page}",
        "/posts/{year}/{month}/{post}",
        "/posts/{year}/{month}/index",
        "/posts/{year}/top",
        "/static/{*path}",
        "/favicon.ico",
    };
    const ops = [_]RemoveCase{
        .{ .op = .remove, .route = "/{page}", .expected = "/{page}" },
        .{
            .op = .remove,
            .route = "/posts/{year}/{month}/{post}",
            .expected = "/posts/{year}/{month}/{post}",
        },
        .{
            .op = .remove,
            .route = "/posts/{year}/{month}/index",
            .expected = "/posts/{year}/{month}/index",
        },
        .{ .op = .remove, .route = "/posts/{year}/top", .expected = "/posts/{year}/top" },
        .{ .op = .remove, .route = "/static/{*path}", .expected = "/static/{*path}" },
        .{ .op = .remove, .route = "/favicon.ico", .expected = "/favicon.ico" },
    };
    const remaining = [_][]const u8{};
    try runRemoveTest(&routes, &ops, &remaining);
}

test "remove catchall" {
    const routes = [_][]const u8{
        "/foo/{*catchall}",
        "/bar",
        "/bar/",
        "/bar/{*catchall}",
    };
    const ops = [_]RemoveCase{
        .{ .op = .remove, .route = "/foo/{catchall}", .expected = null },
        .{ .op = .remove, .route = "/foo/{*catchall}", .expected = "/foo/{*catchall}" },
        .{ .op = .remove, .route = "/bar/", .expected = "/bar/" },
        .{ .op = .insert, .route = "/foo/*catchall", .expected = "/foo/*catchall" },
        .{ .op = .remove, .route = "/bar/{*catchall}", .expected = "/bar/{*catchall}" },
    };
    const remaining = [_][]const u8{ "/bar", "/foo/*catchall" };
    try runRemoveTest(&routes, &ops, &remaining);
}

test "remove overlapping routes" {
    const routes = [_][]const u8{
        "/home",
        "/home/{id}",
        "/users",
        "/users/{id}",
        "/users/{id}/posts",
        "/users/{id}/posts/{post_id}",
        "/articles",
        "/articles/{category}",
        "/articles/{category}/{id}",
    };
    const ops = [_]RemoveCase{
        .{ .op = .remove, .route = "/home", .expected = "/home" },
        .{ .op = .insert, .route = "/home", .expected = "/home" },
        .{ .op = .remove, .route = "/home/{id}", .expected = "/home/{id}" },
        .{ .op = .insert, .route = "/home/{id}", .expected = "/home/{id}" },
        .{ .op = .remove, .route = "/users", .expected = "/users" },
        .{ .op = .insert, .route = "/users", .expected = "/users" },
        .{ .op = .remove, .route = "/users/{id}", .expected = "/users/{id}" },
        .{ .op = .insert, .route = "/users/{id}", .expected = "/users/{id}" },
        .{ .op = .remove, .route = "/users/{id}/posts", .expected = "/users/{id}/posts" },
        .{ .op = .insert, .route = "/users/{id}/posts", .expected = "/users/{id}/posts" },
        .{
            .op = .remove,
            .route = "/users/{id}/posts/{post_id}",
            .expected = "/users/{id}/posts/{post_id}",
        },
        .{
            .op = .insert,
            .route = "/users/{id}/posts/{post_id}",
            .expected = "/users/{id}/posts/{post_id}",
        },
        .{ .op = .remove, .route = "/articles", .expected = "/articles" },
        .{ .op = .insert, .route = "/articles", .expected = "/articles" },
        .{ .op = .remove, .route = "/articles/{category}", .expected = "/articles/{category}" },
        .{ .op = .insert, .route = "/articles/{category}", .expected = "/articles/{category}" },
        .{
            .op = .remove,
            .route = "/articles/{category}/{id}",
            .expected = "/articles/{category}/{id}",
        },
        .{
            .op = .insert,
            .route = "/articles/{category}/{id}",
            .expected = "/articles/{category}/{id}",
        },
    };
    const remaining = [_][]const u8{
        "/home",
        "/home/{id}",
        "/users",
        "/users/{id}",
        "/users/{id}/posts",
        "/users/{id}/posts/{post_id}",
        "/articles",
        "/articles/{category}",
        "/articles/{category}/{id}",
    };
    try runRemoveTest(&routes, &ops, &remaining);
}

test "remove trailing slash" {
    const routes = [_][]const u8{ "/{home}/", "/foo" };
    const ops = [_]RemoveCase{
        .{ .op = .remove, .route = "/", .expected = null },
        .{ .op = .remove, .route = "/{home}", .expected = null },
        .{ .op = .remove, .route = "/foo/", .expected = null },
        .{ .op = .remove, .route = "/foo", .expected = "/foo" },
        .{ .op = .remove, .route = "/{home}", .expected = null },
        .{ .op = .remove, .route = "/{home}/", .expected = "/{home}/" },
    };
    const remaining = [_][]const u8{};
    try runRemoveTest(&routes, &ops, &remaining);
}

test "remove root" {
    const routes = [_][]const u8{"/"};
    const ops = [_]RemoveCase{
        .{ .op = .remove, .route = "/", .expected = "/" },
    };
    const remaining = [_][]const u8{};
    try runRemoveTest(&routes, &ops, &remaining);
}

test "remove escaped params" {
    const routes = [_][]const u8{
        "/foo/{id}",
        "/foo/{id}/bar",
        "/bar/{user}/{id}",
        "/bar/{user}/{id}/baz",
        "/baz/{product}/{user}/{id}",
    };
    const ops = [_]RemoveCase{
        .{ .op = .remove, .route = "/foo/{a}", .expected = null },
        .{ .op = .remove, .route = "/foo/{a}/bar", .expected = null },
        .{ .op = .remove, .route = "/bar/{a}/{b}", .expected = null },
        .{ .op = .remove, .route = "/bar/{a}/{b}/baz", .expected = null },
        .{ .op = .remove, .route = "/baz/{a}/{b}/{c}", .expected = null },
    };
    const remaining = [_][]const u8{
        "/foo/{id}",
        "/foo/{id}/bar",
        "/bar/{user}/{id}",
        "/bar/{user}/{id}/baz",
        "/baz/{product}/{user}/{id}",
    };
    try runRemoveTest(&routes, &ops, &remaining);
}

test "remove escaped literal" {
    const allocator = testing.allocator;
    var router = lib.Router([]const u8).init(allocator);
    defer router.deinit();

    try expectInsertOk(allocator, try router.insert("/foo/{a}", "/foo/{a}"));
    try expectInsertOk(allocator, try router.insert("/foo/{{a}}", "/foo/{{a}}"));

    const removed = try router.remove("/foo/{a}");
    try expectOptionalSlice("/foo/{a}", removed);

    var match = try router.match("/foo/{a}");
    defer match.deinit();
    try testing.expect(std.mem.eql(u8, match.value.*, "/foo/{{a}}"));
}

test "remove wildcard suffix" {
    const routes = [_][]const u8{
        "/foo/{id}",
        "/foo/{id}/bar",
        "/foo/{id}bar",
        "/foo/{id}bar/baz",
        "/foo/{id}bar/baz/bax",
        "/bar/x{id}y",
        "/bar/x{id}y/",
        "/baz/x{id}y",
        "/baz/x{id}y/",
    };
    const ops = [_]RemoveCase{
        .{ .op = .remove, .route = "/foo/{id}", .expected = "/foo/{id}" },
        .{ .op = .remove, .route = "/foo/{id}bar", .expected = "/foo/{id}bar" },
        .{ .op = .remove, .route = "/foo/{id}bar/baz", .expected = "/foo/{id}bar/baz" },
        .{ .op = .insert, .route = "/foo/{id}bax", .expected = "/foo/{id}bax" },
        .{ .op = .insert, .route = "/foo/{id}bax/baz", .expected = "/foo/{id}bax/baz" },
        .{ .op = .remove, .route = "/foo/{id}bax/baz", .expected = "/foo/{id}bax/baz" },
        .{ .op = .remove, .route = "/bar/x{id}y", .expected = "/bar/x{id}y" },
        .{ .op = .remove, .route = "/baz/x{id}y/", .expected = "/baz/x{id}y/" },
    };
    const remaining = [_][]const u8{
        "/foo/{id}/bar",
        "/foo/{id}bar/baz/bax",
        "/foo/{id}bax",
        "/bar/x{id}y/",
        "/baz/x{id}y",
    };
    try runRemoveTest(&routes, &ops, &remaining);
}

test "merge ok" {
    const allocator = testing.allocator;
    var root = lib.Router([]const u8).init(allocator);
    defer root.deinit();
    try expectInsertOk(allocator, try root.insert("/foo", "foo"));
    try expectInsertOk(allocator, try root.insert("/bar/{id}", "bar"));

    var child = lib.Router([]const u8).init(allocator);
    defer child.deinit();
    try expectInsertOk(allocator, try child.insert("/baz", "baz"));
    try expectInsertOk(allocator, try child.insert("/xyz/{id}", "xyz"));

    switch (try root.mergeFrom(&child)) {
        .ok => {},
        .err => |err_val| {
            var err = err_val;
            defer err.deinit(allocator);
            try testing.expect(false);
        },
    }

    var match_foo = try root.match("/foo");
    defer match_foo.deinit();
    try testing.expect(std.mem.eql(u8, match_foo.value.*, "foo"));

    var match_bar = try root.match("/bar/1");
    defer match_bar.deinit();
    try testing.expect(std.mem.eql(u8, match_bar.value.*, "bar"));

    var match_baz = try root.match("/baz");
    defer match_baz.deinit();
    try testing.expect(std.mem.eql(u8, match_baz.value.*, "baz"));

    var match_xyz = try root.match("/xyz/2");
    defer match_xyz.deinit();
    try testing.expect(std.mem.eql(u8, match_xyz.value.*, "xyz"));
}

test "merge conflict" {
    const allocator = testing.allocator;
    var root = lib.Router([]const u8).init(allocator);
    defer root.deinit();
    try expectInsertOk(allocator, try root.insert("/foo", "foo"));
    try expectInsertOk(allocator, try root.insert("/bar", "bar"));

    var child = lib.Router([]const u8).init(allocator);
    defer child.deinit();
    try expectInsertOk(allocator, try child.insert("/foo", "changed"));
    try expectInsertOk(allocator, try child.insert("/bar", "changed"));
    try expectInsertOk(allocator, try child.insert("/baz", "baz"));

    switch (try root.mergeFrom(&child)) {
        .ok => try testing.expect(false),
        .err => |err_val| {
            var err = err_val;
            defer err.deinit(allocator);

            try testing.expectEqual(@as(usize, 2), err.errors.items.len);
            switch (err.errors.items[0]) {
                .Conflict => |route| try testing.expect(std.mem.eql(u8, route.slice(), "/foo")),
                else => try testing.expect(false),
            }
            switch (err.errors.items[1]) {
                .Conflict => |route| try testing.expect(std.mem.eql(u8, route.slice(), "/bar")),
                else => try testing.expect(false),
            }
        },
    }

    var match_foo = try root.match("/foo");
    defer match_foo.deinit();
    try testing.expect(std.mem.eql(u8, match_foo.value.*, "foo"));

    var match_bar = try root.match("/bar");
    defer match_bar.deinit();
    try testing.expect(std.mem.eql(u8, match_bar.value.*, "bar"));

    var match_baz = try root.match("/baz");
    defer match_baz.deinit();
    try testing.expect(std.mem.eql(u8, match_baz.value.*, "baz"));
}

test "merge nested" {
    const allocator = testing.allocator;
    var root = lib.Router([]const u8).init(allocator);
    defer root.deinit();
    try expectInsertOk(allocator, try root.insert("/foo", "foo"));

    var child = lib.Router([]const u8).init(allocator);
    defer child.deinit();
    try expectInsertOk(allocator, try child.insert("/foo/bar", "bar"));

    switch (try root.mergeFrom(&child)) {
        .ok => {},
        .err => |err_val| {
            var err = err_val;
            defer err.deinit(allocator);
            try testing.expect(false);
        },
    }

    var match_foo = try root.match("/foo");
    defer match_foo.deinit();
    try testing.expect(std.mem.eql(u8, match_foo.value.*, "foo"));

    var match_bar = try root.match("/foo/bar");
    defer match_bar.deinit();
    try testing.expect(std.mem.eql(u8, match_bar.value.*, "bar"));
}

test "fuzz router match vs naive" {
    const allocator = testing.allocator;
    var router = lib.Router([]const u8).init(allocator);
    defer router.deinit();

    var patterns = std.ArrayListUnmanaged(Pattern){};
    defer {
        for (patterns.items) |*pattern| {
            pattern.deinit(allocator);
        }
        patterns.deinit(allocator);
    }

    var prng = std.Random.DefaultPrng.init(0x8d0b_4ac5_1f2a_3901);
    const rand = prng.random();

    var route_index: usize = 0;
    while (route_index < 80) : (route_index += 1) {
        var pattern = try generatePattern(allocator, rand, route_index);
        switch (try router.insert(pattern.route, pattern.route)) {
            .ok => try patterns.append(allocator, pattern),
            .err => |err_val| {
                var err = err_val;
                defer err.deinit(allocator);
                pattern.deinit(allocator);
            },
        }
    }

    var path_index: usize = 0;
    while (path_index < 200) : (path_index += 1) {
        const use_pattern_path = patterns.items.len > 0 and rand.uintLessThan(u8, 10) < 7;
        if (use_pattern_path) {
            const pattern_index = rand.uintLessThan(usize, patterns.items.len);
            var expected_path = try buildPathFromPattern(allocator, rand, &patterns.items[pattern_index]);
            defer expected_path.deinit(allocator);

            if (router.match(expected_path.path)) |match_result| {
                var match_value = match_result;
                defer match_value.deinit();

                const matched_route = match_value.value.*;
                var matched_pattern: ?*const Pattern = null;
                for (patterns.items) |*pattern| {
                    if (std.mem.eql(u8, pattern.route, matched_route)) {
                        matched_pattern = pattern;
                        break;
                    }
                }
                try testing.expect(matched_pattern != null);

                const param_pairs_opt = try patternParams(allocator, matched_pattern.?, expected_path.path);
                try testing.expect(param_pairs_opt != null);
                const param_pairs = param_pairs_opt.?;
                defer allocator.free(param_pairs);

                try expectParams(&match_value.params, param_pairs);
            } else |err| switch (err) {
                error.NotFound => try testing.expect(false),
                else => return err,
            }
        } else {
            const path = try randomPath(allocator, rand);
            defer allocator.free(path);

            if (router.match(path)) |match_result| {
                var match_value = match_result;
                defer match_value.deinit();

                const matched_route = match_value.value.*;
                var matched_pattern: ?*const Pattern = null;
                for (patterns.items) |*pattern| {
                    if (std.mem.eql(u8, pattern.route, matched_route)) {
                        matched_pattern = pattern;
                        break;
                    }
                }
                try testing.expect(matched_pattern != null);

                const param_pairs_opt = try patternParams(allocator, matched_pattern.?, path);
                try testing.expect(param_pairs_opt != null);
                const param_pairs = param_pairs_opt.?;
                defer allocator.free(param_pairs);

                try expectParams(&match_value.params, param_pairs);
            } else |err| switch (err) {
                error.NotFound => {
                    var any_static_match = false;
                    for (patterns.items) |*pattern| {
                        if (patternIsAllStatic(pattern) and std.mem.eql(u8, pattern.route, path)) {
                            any_static_match = true;
                            break;
                        }
                    }
                    try testing.expect(!any_static_match);
                },
                else => return err,
            }
        }
    }
}
