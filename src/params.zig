const std = @import("std");
const errors = @import("errors.zig");
const route = @import("route.zig");

const Allocator = std.mem.Allocator;
const ParamError = errors.ParamError;
const UnescapedRoute = route.UnescapedRoute;

pub const ParamRemapping = struct {
    const Self = @This();
    const Name = struct {
        start: usize,
        len: usize,
    };

    buffer: std.ArrayListUnmanaged(u8) = .{},
    names: std.ArrayListUnmanaged(Name) = .{},

    fn deinit(self: *Self, allocator: Allocator) void {
        self.buffer.deinit(allocator);
        self.names.deinit(allocator);
        self.* = undefined;
    }

    fn append(self: *Self, allocator: Allocator, name: []const u8) Allocator.Error!void {
        const start = self.buffer.items.len;
        try self.buffer.appendSlice(allocator, name);
        errdefer self.buffer.shrinkRetainingCapacity(start);
        try self.names.append(allocator, .{ .start = start, .len = name.len });
    }

    fn len(self: *const Self) usize {
        return self.names.items.len;
    }

    fn slice(self: *const Self, index: usize) []const u8 {
        std.debug.assert(index < self.names.items.len);
        const entry = self.names.items[index];
        std.debug.assert(entry.start + entry.len <= self.buffer.items.len);
        return self.buffer.items[entry.start .. entry.start + entry.len];
    }
};

pub fn remappingDeinit(remapping: *ParamRemapping, allocator: Allocator) void {
    remapping.deinit(allocator);
}

pub fn remappingEqual(a: *const ParamRemapping, b: *const ParamRemapping) bool {
    const len = a.len();
    if (len != b.len()) return false;
    var i: usize = 0;
    while (i < len) : (i += 1) {
        if (!std.mem.eql(u8, a.slice(i), b.slice(i))) return false;
    }
    return true;
}

// Normalization uses single-letter placeholders: {a}..{z}.
const MAX_PARAMS: usize = @as(usize, 'z' - 'a' + 1);

pub fn normalizeParams(
    allocator: Allocator,
    route_value: *UnescapedRoute,
) (Allocator.Error || ParamError)!ParamRemapping {
    var start: usize = 0;
    var original: ParamRemapping = .{};
    errdefer remappingDeinit(&original, allocator);

    var count: usize = 0;

    while (true) {
        const wildcard_opt = try route.findWildcard(route_value.asRef().sliceOff(start));
        if (wildcard_opt == null) return original;
        var wildcard = wildcard_opt.?;
        wildcard.start += start;
        wildcard.end += start;

        if (wildcard.len() < 2) return error.InvalidParam;

        if (route_value.inner.items[wildcard.start + 1] == '*') {
            start = wildcard.end;
            continue;
        }

        const name = route_value.inner.items[wildcard.start + 1 .. wildcard.end - 1];
        try original.append(allocator, name);

        if (count >= MAX_PARAMS) return error.TooManyParams;
        const replacement = [_]u8{ '{', @intCast('a' + count), '}' };
        try route_value.splice(allocator, wildcard, &replacement);

        count += 1;

        start = wildcard.start + 3;
    }
}

pub fn denormalizeParams(
    allocator: Allocator,
    route_value: *UnescapedRoute,
    params: *const ParamRemapping,
) Allocator.Error!void {
    var start: usize = 0;
    var i: usize = 0;

    while (true) {
        const wildcard_opt = route.findWildcard(route_value.asRef().sliceOff(start)) catch unreachable;
        if (wildcard_opt == null) return;
        var wildcard = wildcard_opt.?;
        wildcard.start += start;
        wildcard.end += start;

        if (i >= params.len()) return;
        const param = params.slice(i);

        var buf = try allocator.alloc(u8, param.len + 2);
        errdefer allocator.free(buf);
        buf[0] = '{';
        @memcpy(buf[1 .. 1 + param.len], param);
        buf[buf.len - 1] = '}';
        try route_value.splice(allocator, wildcard, buf);
        allocator.free(buf);

        i += 1;
        start = wildcard.start + param.len + 2;
    }
}

pub const Param = struct {
    key: []const u8,
    value: []const u8,

    pub const empty = Param{ .key = "", .value = "" };
};

const SMALL: usize = 3;

comptime {
    std.debug.assert(SMALL <= std.math.maxInt(u32));
}

const SmallParams = struct {
    items: [SMALL]Param = [_]Param{Param.empty} ** SMALL,
    count_u32: u32 = 0,

    fn count(self: *const SmallParams) usize {
        return @intCast(self.count_u32);
    }

    fn countAs(self: *const SmallParams, comptime Int: type) Int {
        comptime std.debug.assert(SMALL <= std.math.maxInt(Int));
        return @intCast(self.count_u32);
    }

    fn slice(self: *const SmallParams) []const Param {
        std.debug.assert(self.count_u32 <= @as(u32, SMALL));
        return self.items[0..self.count_u32];
    }

    fn sliceMut(self: *SmallParams) []Param {
        std.debug.assert(self.count_u32 <= @as(u32, SMALL));
        return self.items[0..self.count_u32];
    }

    fn isFull(self: *const SmallParams) bool {
        return self.count_u32 == @as(u32, SMALL);
    }

    fn appendAssumeCapacity(self: *SmallParams, param: Param) void {
        std.debug.assert(!self.isFull());
        const index: usize = @intCast(self.count_u32);
        self.items[index] = param;
        self.count_u32 += 1;
    }

    fn truncate(self: *SmallParams, n: usize) void {
        std.debug.assert(n <= self.count());
        self.count_u32 = @intCast(n);
    }
};

const ParamsKind = union(enum) {
    Small: SmallParams,
    Large: std.ArrayListUnmanaged(Param),
};

pub const Params = struct {
    allocator: Allocator,
    kind: ParamsKind,

    pub fn init(allocator: Allocator) Params {
        return .{
            .allocator = allocator,
            .kind = .{ .Small = .{} },
        };
    }

    pub fn deinit(self: *Params) void {
        switch (self.kind) {
            .Large => |*list| list.deinit(self.allocator),
            else => {},
        }
        self.* = undefined;
    }

    pub fn len(self: *const Params) usize {
        return switch (self.kind) {
            .Small => |small| small.count(),
            .Large => |list| list.items.len,
        };
    }

    pub fn isEmpty(self: *const Params) bool {
        return self.len() == 0;
    }

    pub fn get(self: *const Params, key: []const u8) ?[]const u8 {
        switch (self.kind) {
            .Small => |small| {
                for (small.slice()) |param| {
                    if (std.mem.eql(u8, param.key, key)) return param.value;
                }
            },
            .Large => |list| {
                for (list.items) |param| {
                    if (std.mem.eql(u8, param.key, key)) return param.value;
                }
            },
        }

        return null;
    }

    pub fn iter(self: *const Params) ParamsIter {
        const items = switch (self.kind) {
            .Small => |*small| small.slice(),
            .Large => |list| list.items,
        };
        return .{ .items = items, .index = 0 };
    }

    pub fn truncate(self: *Params, n: usize) void {
        switch (self.kind) {
            .Small => |*small| {
                if (n <= small.count()) {
                    small.truncate(n);
                }
            },
            .Large => |*list| {
                if (n <= list.items.len) {
                    list.shrinkRetainingCapacity(n);
                }
            },
        }
    }

    pub fn push(self: *Params, key: []const u8, value: []const u8) Allocator.Error!void {
        const param = Param{ .key = key, .value = value };

        switch (self.kind) {
            .Small => |*small| {
                if (small.isFull()) {
                    var list = std.ArrayListUnmanaged(Param){};
                    errdefer list.deinit(self.allocator);
                    try list.appendSlice(self.allocator, small.slice());
                    try list.append(self.allocator, param);
                    self.kind = .{ .Large = list };
                } else {
                    small.appendAssumeCapacity(param);
                }
            },
            .Large => |*list| {
                try list.append(self.allocator, param);
            },
        }
    }

    pub fn applyRemapping(self: *Params, remapping: *const ParamRemapping) void {
        switch (self.kind) {
            .Small => |*small| {
                const count = @min(small.count(), remapping.len());
                const items = small.sliceMut();
                var i: usize = 0;
                while (i < count) : (i += 1) {
                    items[i].key = remapping.slice(i);
                }
            },
            .Large => |*list| {
                const count = @min(list.items.len, remapping.len());
                var i: usize = 0;
                while (i < count) : (i += 1) {
                    list.items[i].key = remapping.slice(i);
                }
            },
        }
    }
};

pub const ParamKV = struct {
    key: []const u8,
    value: []const u8,
};

pub const ParamsIter = struct {
    items: []const Param,
    index: usize,

    pub fn next(self: *ParamsIter) ?ParamKV {
        if (self.index >= self.items.len) return null;
        const param = self.items[self.index];
        self.index += 1;
        return .{ .key = param.key, .value = param.value };
    }

    pub fn len(self: *const ParamsIter) usize {
        std.debug.assert(self.index <= self.items.len);
        return self.items.len - self.index;
    }
};

test "SmallParams basics" {
    var small: SmallParams = .{};
    try std.testing.expectEqual(@as(usize, 0), small.count());
    try std.testing.expect(!small.isFull());

    const param = Param{ .key = "key", .value = "value" };
    var i: usize = 0;
    while (i < SMALL) : (i += 1) {
        small.appendAssumeCapacity(param);
    }

    try std.testing.expectEqual(@as(usize, SMALL), small.count());
    try std.testing.expect(small.isFull());
    try std.testing.expectEqual(@as(u8, SMALL), small.countAs(u8));

    small.truncate(1);
    try std.testing.expectEqual(@as(usize, 1), small.count());
    try std.testing.expectEqual(@as(usize, 1), small.slice().len);
}
