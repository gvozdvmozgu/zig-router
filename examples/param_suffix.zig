const std = @import("std");
const lib = @import("router");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var router = lib.Router([]const u8).init(gpa.allocator());
    defer router.deinit();

    _ = try router.insert("/files/{name}.txt", "file");

    var match = try router.match("/files/report.txt");
    defer match.deinit();

    const name = match.params.get("name").?;
    std.debug.print("name={s}\n", .{name});
}
