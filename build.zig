const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    demo(b, target, optimize);

    const tests = b.step("test", "Run unit tests");
    const lib_tests = b.addTest(.{
        .name = "tests",
        .root_source_file = .{ .path = "source/noclip.zig" },
        .target = target,
        .optimize = optimize,
    });

    tests.dependOn(&lib_tests.step);
}

fn demo(b: *std.build.Builder, target: anytype, optimize: anytype) void {
    const demo_step = b.step("demo", "Build and install CLI demo program");
    const noclip = b.createModule(.{ .source_file = .{ .path = "source/noclip.zig" } });

    const exe = b.addExecutable(.{
        .name = "noclip-demo",
        .root_source_file = .{ .path = "demo/demo.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.addModule("noclip", noclip);
    const install_demo = b.addInstallArtifact(exe);

    demo_step.dependOn(&install_demo.step);
}
