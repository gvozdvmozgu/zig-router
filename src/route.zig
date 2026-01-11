const std = @import("std");
const errors = @import("errors.zig");

const Allocator = std.mem.Allocator;
const ParamError = errors.ParamError;

pub const Range = struct {
    start: usize,
    end: usize,

    pub fn len(self: Range) usize {
        return self.end - self.start;
    }
};

fn containsUsize(list: []const usize, value: usize) bool {
    for (list) |item| {
        if (item == value) return true;
    }
    return false;
}

fn assertEscapedInvariant(escaped: []const usize, len: usize) void {
    if (escaped.len == 0) return;
    var prev = escaped[0];
    std.debug.assert(prev < len);
    var i: usize = 1;
    while (i < escaped.len) : (i += 1) {
        const esc = escaped[i];
        std.debug.assert(esc < len);
        std.debug.assert(esc > prev);
        prev = esc;
    }
}

pub fn segmentTerminator(bytes: []const u8) usize {
    if (std.mem.indexOfScalar(u8, bytes, '/')) |idx| {
        return idx + 1;
    }
    return bytes.len;
}

pub fn isEmptyOrSlash(bytes: []const u8) bool {
    return bytes.len == 0 or (bytes.len == 1 and bytes[0] == '/');
}

pub const UnescapedRoute = struct {
    inner: std.ArrayListUnmanaged(u8) = .{},
    escaped: std.ArrayListUnmanaged(usize) = .{},

    pub fn init(allocator: Allocator, bytes: []const u8) Allocator.Error!UnescapedRoute {
        var route = UnescapedRoute{};
        try route.inner.appendSlice(allocator, bytes);

        var i: usize = 0;
        while (i < route.inner.items.len) : (i += 1) {
            const c = route.inner.items[i];
            if ((c == '{' and i + 1 < route.inner.items.len and route.inner.items[i + 1] == '{') or
                (c == '}' and i + 1 < route.inner.items.len and route.inner.items[i + 1] == '}'))
            {
                _ = route.inner.orderedRemove(i);
                try route.escaped.append(allocator, i);
            }
        }

        return route;
    }

    pub fn deinit(self: *UnescapedRoute, allocator: Allocator) void {
        self.inner.deinit(allocator);
        self.escaped.deinit(allocator);
        self.* = undefined;
    }

    pub fn clone(self: *const UnescapedRoute, allocator: Allocator) Allocator.Error!UnescapedRoute {
        var route = UnescapedRoute{};
        try route.inner.appendSlice(allocator, self.inner.items);
        try route.escaped.appendSlice(allocator, self.escaped.items);
        return route;
    }

    pub fn toOwnedUnescaped(self: *const UnescapedRoute, allocator: Allocator) Allocator.Error![]u8 {
        const out = try allocator.alloc(u8, self.inner.items.len);
        @memcpy(out, self.inner.items);
        return out;
    }

    pub fn len(self: *const UnescapedRoute) usize {
        return self.inner.items.len;
    }

    pub fn isEscaped(self: *const UnescapedRoute, idx: usize) bool {
        return containsUsize(self.escaped.items, idx);
    }

    pub fn splice(
        self: *UnescapedRoute,
        allocator: Allocator,
        range: Range,
        replace: []const u8,
    ) Allocator.Error!void {
        std.debug.assert(range.start <= range.end);
        std.debug.assert(range.end <= self.inner.items.len);
        assertEscapedInvariant(self.escaped.items, self.inner.items.len);

        const old_len = self.inner.items.len;
        const offset: isize = @as(isize, @intCast(replace.len)) -
            @as(isize, @intCast(range.len()));
        const new_len = @as(isize, @intCast(old_len)) + offset;
        std.debug.assert(new_len >= 0);

        var idx: usize = 0;
        while (idx < self.escaped.items.len) {
            const esc = self.escaped.items[idx];
            if (esc >= range.start and esc < range.end) {
                _ = self.escaped.orderedRemove(idx);
            } else {
                idx += 1;
            }
        }

        if (offset != 0) {
            for (self.escaped.items) |*esc| {
                if (esc.* >= range.end) {
                    const updated = @as(isize, @intCast(esc.*)) + offset;
                    std.debug.assert(updated >= 0);
                    std.debug.assert(updated < new_len);
                    esc.* = @intCast(updated);
                }
            }
        }

        try self.inner.replaceRange(allocator, range.start, range.len(), replace);
        assertEscapedInvariant(self.escaped.items, self.inner.items.len);
    }

    pub fn append(
        self: *UnescapedRoute,
        allocator: Allocator,
        other: *const UnescapedRoute,
    ) Allocator.Error!void {
        const offset = self.inner.items.len;
        try self.escaped.ensureUnusedCapacity(allocator, other.escaped.items.len);
        for (other.escaped.items) |esc| {
            self.escaped.appendAssumeCapacity(offset + esc);
        }
        try self.inner.appendSlice(allocator, other.inner.items);
    }

    pub fn truncate(self: *UnescapedRoute, to: usize) void {
        std.debug.assert(to <= self.inner.items.len);
        assertEscapedInvariant(self.escaped.items, self.inner.items.len);
        var idx: usize = 0;
        while (idx < self.escaped.items.len) {
            if (self.escaped.items[idx] >= to) {
                _ = self.escaped.orderedRemove(idx);
            } else {
                idx += 1;
            }
        }
        self.inner.shrinkRetainingCapacity(to);
        assertEscapedInvariant(self.escaped.items, self.inner.items.len);
    }

    pub fn asRef(self: *const UnescapedRoute) UnescapedRef {
        return .{
            .inner = self.inner.items,
            .escaped = self.escaped.items,
            .offset = 0,
        };
    }

    pub fn unescaped(self: *const UnescapedRoute) []const u8 {
        return self.inner.items;
    }
};

pub const UnescapedRef = struct {
    inner: []const u8,
    escaped: []const usize,
    offset: isize,

    pub fn toOwned(self: UnescapedRef, allocator: Allocator) Allocator.Error!UnescapedRoute {
        var route = UnescapedRoute{};
        try route.inner.appendSlice(allocator, self.inner);

        const len_isize = @as(isize, @intCast(self.inner.len));
        for (self.escaped) |esc| {
            const adjusted = @as(isize, @intCast(esc)) + self.offset;
            if (adjusted >= 0 and adjusted < len_isize) {
                try route.escaped.append(allocator, @intCast(adjusted));
            }
        }

        return route;
    }

    pub fn isEscaped(self: UnescapedRef, idx: usize) bool {
        const adjusted = @as(isize, @intCast(idx)) - self.offset;
        if (adjusted < 0) return false;
        return containsUsize(self.escaped, @intCast(adjusted));
    }

    pub fn sliceOff(self: UnescapedRef, start: usize) UnescapedRef {
        return .{
            .inner = self.inner[start..],
            .escaped = self.escaped,
            .offset = self.offset - @as(isize, @intCast(start)),
        };
    }

    pub fn sliceUntil(self: UnescapedRef, end: usize) UnescapedRef {
        return .{
            .inner = self.inner[0..end],
            .escaped = self.escaped,
            .offset = self.offset,
        };
    }

    pub fn unescaped(self: UnescapedRef) []const u8 {
        return self.inner;
    }
};

pub fn findWildcard(path: UnescapedRef) ParamError!?Range {
    var start: usize = 0;
    while (start < path.inner.len) : (start += 1) {
        const c = path.inner[start];
        if (c == '}' and !path.isEscaped(start)) return error.InvalidParam;

        if (c != '{' or path.isEscaped(start)) continue;

        if (start + 1 < path.inner.len and path.inner[start + 1] == '}') {
            return error.InvalidParam;
        }

        var i: usize = start + 2;
        while (i < path.inner.len) : (i += 1) {
            const ch = path.inner[i];
            switch (ch) {
                '}' => {
                    if (path.isEscaped(i)) {
                        // continue
                    } else {
                        if (i > 0 and path.inner[i - 1] == '*') {
                            return error.InvalidParam;
                        }
                        return Range{ .start = start, .end = i + 1 };
                    }
                },
                '*', '/' => return error.InvalidParam,
                else => {},
            }
        }

        return error.InvalidParam;
    }

    return null;
}
