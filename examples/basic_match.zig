const std = @import("std");
const lib = @import("router");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var router = lib.Router([]const u8).init(gpa.allocator());
    defer router.deinit();

    _ = try router.insert("/users/{id}", "user");
    _ = try router.insert("/assets/{*path}", "asset");

    var match = try router.match("/users/42");
    defer match.deinit();

    const user_id = match.params.get("id").?;
    std.debug.print("id={s} value={s}\n", .{ user_id, match.value.* });
}
