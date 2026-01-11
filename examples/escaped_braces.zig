const std = @import("std");
const lib = @import("router");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var router = lib.Router([]const u8).init(gpa.allocator());
    defer router.deinit();

    _ = try router.insert("/literal/{{braces}}", "ok");

    var match = try router.match("/literal/{braces}");
    defer match.deinit();

    std.debug.print("value={s}\n", .{match.value.*});
}
