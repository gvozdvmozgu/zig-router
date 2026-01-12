const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const router_module = b.addModule("router", .{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lib = b.addLibrary(.{
        .name = "router",
        .linkage = .static,
        .root_module = router_module,
    });

    const bench_match = b.addExecutable(.{
        .name = "bench-match",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/bench_match.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const bench_match_run = b.addRunArtifact(bench_match);
    if (b.args) |args| {
        bench_match_run.addArgs(args);
    }

    const bench_match_step = b.step("bench-match", "Run match throughput benchmark");
    bench_match_step.dependOn(&bench_match_run.step);

    const example_basic = b.addExecutable(.{
        .name = "example-basic",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/basic_match.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_basic.root_module.addImport("router", router_module);
    const example_basic_run = b.addRunArtifact(example_basic);
    if (b.args) |args| {
        example_basic_run.addArgs(args);
    }
    const example_basic_step = b.step("example:basic", "Run basic match example");
    example_basic_step.dependOn(&example_basic_run.step);

    const example_param_suffix = b.addExecutable(.{
        .name = "example-param-suffix",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/param_suffix.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_param_suffix.root_module.addImport("router", router_module);
    const example_param_suffix_run = b.addRunArtifact(example_param_suffix);
    if (b.args) |args| {
        example_param_suffix_run.addArgs(args);
    }
    const example_param_suffix_step = b.step("example:param-suffix", "Run param suffix example");
    example_param_suffix_step.dependOn(&example_param_suffix_run.step);

    const example_catchall = b.addExecutable(.{
        .name = "example-catchall",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/catchall.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_catchall.root_module.addImport("router", router_module);
    const example_catchall_run = b.addRunArtifact(example_catchall);
    if (b.args) |args| {
        example_catchall_run.addArgs(args);
    }
    const example_catchall_step = b.step("example:catchall", "Run catchall example");
    example_catchall_step.dependOn(&example_catchall_run.step);

    const example_escaped_braces = b.addExecutable(.{
        .name = "example-escaped-braces",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/escaped_braces.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_escaped_braces.root_module.addImport("router", router_module);
    const example_escaped_braces_run = b.addRunArtifact(example_escaped_braces);
    if (b.args) |args| {
        example_escaped_braces_run.addArgs(args);
    }
    const example_escaped_braces_step = b.step("example:escaped-braces", "Run escaped braces example");
    example_escaped_braces_step.dependOn(&example_escaped_braces_run.step);

    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/all_tests.zig"),
            .target = target,
            .optimize = optimize,
        }),
        .filters = b.args orelse &.{},
    });
    const test_run = b.addRunArtifact(tests);
    if (b.args != null) {
        test_run.has_side_effects = true;
    }

    const test_step = b.step("test", "Run tests (and fmt without a filter)");
    const test_unit_step = b.step("test:unit", "Run unit tests");
    const test_fmt_step = b.step("test:fmt", "Check formatting");

    test_step.dependOn(&test_run.step);
    test_unit_step.dependOn(&test_run.step);

    const fmt = b.addFmt(.{ .paths = &.{"."}, .check = true });
    test_fmt_step.dependOn(&fmt.step);
    if (b.args == null) {
        test_step.dependOn(&fmt.step);
    }

    b.installArtifact(lib);
}
