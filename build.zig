const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const demo = b.step("demo", "noclip demo");
    const tests = b.step("test", "Run unit tests");

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addSharedLibrary(.{
        .name = "noclip",
        .root_source_file = .{ .path = "source/noclip.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.install();

    const demo_exe = b.addExecutable(.{
        .name = "noclip-demo",
        .root_source_file = .{ .path = "source/doodle.zig" },
    });
    const install_demo = b.addInstallArtifact(demo_exe);
    demo.dependOn(&install_demo.step);

    const lib_tests = b.addTest(.{
        .name = "tests",
        .root_source_file = .{ .path = "source/noclip.zig" },
        .target = target,
        .optimize = optimize,
    });

    tests.dependOn(&lib_tests.step);
}
