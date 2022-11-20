const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const demo = b.step("demo", "noclip demo");
    const tests = b.step("test", "Run unit tests");

    // b.use_stage1 = false;
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addSharedLibrary("noclip", "source/noclip.zig", .unversioned);

    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const demo_exe = b.addExecutable("noclip-demo", "demo/demo.zig");
    demo_exe.addPackagePath("noclip", "source/noclip.zig");
    const install_demo = b.addInstallArtifact(demo_exe);
    demo.dependOn(&install_demo.step);

    const lib_tests = b.addTest("source/noclip.zig");
    lib_tests.setTarget(target);
    lib_tests.setBuildMode(mode);

    tests.dependOn(&lib_tests.step);
}
