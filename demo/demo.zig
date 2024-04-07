const std = @import("std");
const noclip = @import("noclip");

const CommandBuilder = noclip.CommandBuilder;

const Choice = enum { first, second };

const cli = cmd: {
    var cmd = CommandBuilder(*u32){
        .description =
        \\The definitive noclip demonstration utility.
        \\
        \\This command demonstrates the functionality of the noclip library. cool!
        \\
        \\>     // implementing factorial recursively is a silly thing to do
        \\>     pub fn fact(n: u64) u64 {
        \\>         if (n == 0) return 1;
        \\>         return n*fact(n - 1);
        \\>     }
        \\
        \\Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
        \\incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
        \\nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
        \\Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
        \\eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident,
        \\sunt in culpa qui officia deserunt mollit anim id est laborum.
        ,
    };
    cmd.addOption(.{ .OutputType = struct { u8, u8 } }, .{
        .name = "test",
        .short_tag = "-t",
        .long_tag = "--test",
        .env_var = "NOCLIP_TEST",
        .description = "multi-value test option",
        .nice_type_name = "int> <int",
    });
    cmd.addOption(.{ .OutputType = Choice }, .{
        .name = "choice",
        .short_tag = "-c",
        .long_tag = "--choice",
        .default = .second,
        .env_var = "NOCLIP_CHOICE",
        .description = "enum choice option",
        .nice_type_name = "choice",
    });
    cmd.stringOption(.{
        .name = "string",
        .short_tag = "-s",
        .long_tag = "--string",
        .env_var = "NOCLIP_STRING",
        .description = "A string value option",
    });
    cmd.addOption(.{ .OutputType = u32 }, .{
        .name = "default",
        .short_tag = "-d",
        .long_tag = "--default",
        .env_var = "NOCLIP_DEFAULT",
        .default = 100,
        .description = "default value integer option",
        .nice_type_name = "uint",
    });
    cmd.addOption(.{ .OutputType = u8, .multi = true }, .{
        .name = "multi",
        .short_tag = "-m",
        .long_tag = "--multi",
        .description = "multiple specification test option",
    });
    cmd.addFlag(.{}, .{
        .name = "flag",
        .truthy = .{ .short_tag = "-f", .long_tag = "--flag" },
        .falsy = .{ .short_tag = "-F", .long_tag = "--no-flag" },
        .env_var = "NOCLIP_FLAG",
        .description = "boolean flag",
    });
    cmd.addFlag(.{ .multi = true }, .{
        .name = "multiflag",
        .truthy = .{ .short_tag = "-M" },
        .description = "multiple specification test flag ",
    });
    cmd.addOption(.{ .OutputType = u8 }, .{
        .name = "env",
        .env_var = "NOCLIP_ENVIRON",
        .description = "environment variable only option",
    });

    break :cmd cmd;
};

const subcommand = cmd: {
    var cmd = CommandBuilder([]const u8){
        .description =
        \\Demonstrate subcommand functionality
        \\
        \\This command demonstrates how subcommands work.
        ,
    };
    cmd.simpleFlag(.{
        .name = "flag",
        .truthy = .{ .short_tag = "-f", .long_tag = "--flag" },
        .falsy = .{ .long_tag = "--no-flag" },
        .env_var = "NOCLIP_SUBFLAG",
    });
    cmd.stringArgument(.{ .name = "argument" });
    cmd.stringArgument(.{
        .name = "arg",
        .description = "This is an argument that doesn't really do anything, but it's very important.",
    });
    break :cmd cmd;
};

fn subHandler(context: []const u8, result: subcommand.Output()) !void {
    std.debug.print("subcommand: {s}\n", .{result.argument});
    std.debug.print("context: {s}\n", .{context});
}

fn cliHandler(context: *u32, result: cli.Output()) !void {
    std.debug.print("context: {d}\n", .{context.*});
    std.debug.print("callback is working {s}\n", .{result.string orelse "null"});
    std.debug.print("callback is working {any}\n", .{result.choice});
    std.debug.print("callback is working {d}\n", .{result.default});
    context.* += 1;
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const base = try noclip.commandGroup(allocator, .{ .description = "base group" });
    defer base.deinitTree();

    var context: u32 = 2;
    const sc: []const u8 = "whassup";

    try base.addSubcommand("main", try cli.createInterface(allocator, cliHandler, &context));
    try base.addSubcommand("other", try subcommand.createInterface(allocator, subHandler, &sc));

    const group = try noclip.commandGroup(allocator, .{ .description = "final level of a deeply nested subcommand" });
    const subcon = try noclip.commandGroup(allocator, .{ .description = "third level of a deeply nested subcommand" });
    const nested = try noclip.commandGroup(allocator, .{ .description = "second level of a deeply nested subcommand" });
    const deeply = try noclip.commandGroup(allocator, .{ .description = "start of a deeply nested subcommand" });
    try base.addSubcommand("deeply", deeply);
    try deeply.addSubcommand("nested", nested);
    try nested.addSubcommand("subcommand", subcon);
    try subcon.addSubcommand("group", group);
    try group.addSubcommand("run", try cli.createInterface(allocator, cliHandler, &context));

    base.execute() catch |err| {
        std.io.getStdErr().writeAll(base.getParseError()) catch {};
        return err;
    };

    return 0;
}
