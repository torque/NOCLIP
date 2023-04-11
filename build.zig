const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    demo(b, target, optimize);
    tokenator(b, target, optimize);
    zed(b, target, optimize);

    const test_step = b.step("test", "Run unit tests");
    const tests = b.addTest(.{
        .name = "tests",
        .root_source_file = .{ .path = "source/noclip.zig" },
        .target = target,
        .optimize = optimize,
    });

    test_step.dependOn(&tests.step);
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

fn tokenator(b: *std.build.Builder, target: anytype, optimize: anytype) void {
    const tok_step = b.step("tokenator", "Build documentation tokenizer");
    const noclip = b.createModule(.{ .source_file = .{ .path = "source/noclip.zig" } });

    const exe = b.addExecutable(.{
        .name = "tokenator",
        .root_source_file = .{ .path = "documentation/tokenator.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.addModule("noclip", noclip);
    const install_tok = b.addInstallArtifact(exe);

    tok_step.dependOn(&install_tok.step);
}

fn zed(b: *std.build.Builder, target: anytype, optimize: anytype) void {
    const tok_step = b.step("zed", "Build documentation generator");
    const noclip = b.createModule(.{ .source_file = .{ .path = "source/noclip.zig" } });

    const exe = b.addExecutable(.{
        .name = "zed",
        .root_source_file = .{ .path = "documentation/zed.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.addModule("noclip", noclip);
    const install_tok = b.addInstallArtifact(exe);

    tok_step.dependOn(&install_tok.step);

    const test_step = b.step("run-zed-tests", "Test documentation generator");
    const tests = b.addTest(.{
        .name = "test-zed",
        .root_source_file = .{ .path = "documentation/zed.zig" },
        .target = target,
        .optimize = optimize,
    });

    const runcmd = tests.run();
    test_step.dependOn(&runcmd.step);
}
