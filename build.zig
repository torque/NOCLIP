const std = @import("std");

pub fn build(b: *std.Build) void {
    const target: std.Build.ResolvedTarget = b.standardTargetOptions(.{});
    const optimize: std.builtin.Mode = b.standardOptimizeOption(.{});



    const test_step = b.step("test", "Run unit tests");
    const tests = b.addTest(.{
        .name = "tests",
        .root_source_file = b.path("source/parser.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_main_tests = b.addRunArtifact(tests);
    test_step.dependOn(&b.addInstallArtifact(tests, .{}).step);
    test_step.dependOn(&run_main_tests.step);
}

fn demo(
    b: *std.Build,
    noclip: *std.Build.Module,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.Mode,
) void {
    const demo_step = b.step("demo", "Build and install CLI demo program");

    const exe = b.addExecutable(.{
        .name = "noclip-demo",
        .root_source_file = b.path("demo/demo.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("noclip", noclip);
    const install_demo = b.addInstallArtifact(exe, .{});

    demo_step.dependOn(&install_demo.step);
}
