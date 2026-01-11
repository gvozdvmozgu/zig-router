const std = @import("std");

const Allocator = std.mem.Allocator;

pub const MatchError = error{NotFound};

pub const ParamError = error{ InvalidParam, TooManyParams };

/// Owns an allocated string. Call deinit with the allocator that created it.
pub const OwnedStr = struct {
    bytes: []u8,

    pub fn deinit(self: *OwnedStr, allocator: Allocator) void {
        allocator.free(self.bytes);
    }

    pub fn slice(self: *const OwnedStr) []const u8 {
        return self.bytes;
    }
};

pub const InsertError = union(enum) {
    Conflict: OwnedStr,
    InvalidParamSegment,
    InvalidParam,
    TooManyParams,
    InvalidCatchAll,

    pub fn deinit(self: *InsertError, allocator: Allocator) void {
        switch (self.*) {
            .Conflict => |*route| route.deinit(allocator),
            else => {},
        }
    }
};

pub const InsertResult = union(enum) {
    ok,
    err: InsertError,
};

pub const MergeError = struct {
    errors: std.ArrayListUnmanaged(InsertError) = .{},

    pub fn deinit(self: *MergeError, allocator: Allocator) void {
        for (self.errors.items) |*err| {
            err.deinit(allocator);
        }
        self.errors.deinit(allocator);
    }
};

pub const MergeResult = union(enum) {
    ok,
    err: MergeError,
};
