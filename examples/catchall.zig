const std = @import("std");
const lib = @import("router");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var router = lib.Router([]const u8).init(gpa.allocator());
    defer router.deinit();

    _ = try router.insert("/static/{*path}", "static");

    var match = try router.match("/static/css/app.css");
    defer match.deinit();

    const path = match.params.get("path").?;
    std.debug.print("path={s}\n", .{path});
}
