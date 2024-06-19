const std = @import("std");

pub fn build(b: *std.Build) void {
    const target: std.Build.ResolvedTarget = b.standardTargetOptions(.{});
    const optimize: std.builtin.Mode = b.standardOptimizeOption(.{});

    const noclip = b.addModule("noclip", .{
        .root_source_file = b.path("source/noclip.zig"),
    });

    demo(b, noclip, target, optimize);

    const test_step = b.step("test", "Run unit tests");
    const tests = b.addTest(.{
        .name = "tests",
        .root_source_file = b.path("source/noclip.zig"),
        .target = target,
        .optimize = optimize,
    });

    test_step.dependOn(&tests.step);
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
