const std = @import("std");
const noclip = @import("noclip");

const CommandBuilder = noclip.CommandBuilder;

const Choice = enum { first, second };

const cli = cmd: {
    var cmd = CommandBuilder(u32).init();
    cmd.add_option(.{ .OutputType = struct { u8, u8 } }, .{
        .name = "test",
        .short_tag = "-t",
        .long_tag = "--test",
        .env_var = "NOCLIP_TEST",
    });
    cmd.add_option(.{ .OutputType = Choice }, .{
        .name = "choice",
        .short_tag = "-c",
        .long_tag = "--choice",
        .env_var = "NOCLIP_CHOICE",
    });
    cmd.add_option(.{ .OutputType = u32 }, .{
        .name = "default",
        .short_tag = "-d",
        .long_tag = "--default",
        .env_var = "NOCLIP_DEFAULT",
        .default = 100,
    });
    cmd.add_option(.{ .OutputType = u8, .multi = true }, .{
        .name = "multi",
        .short_tag = "-m",
        .long_tag = "--multi",
        .env_var = "NOCLIP_MULTI",
    });
    cmd.add_flag(.{}, .{
        .name = "flag",
        .truthy = .{ .short_tag = "-f", .long_tag = "--flag" },
        .falsy = .{ .long_tag = "--no-flag" },
        .env_var = "NOCLIP_FLAG",
    });
    cmd.add_flag(.{ .multi = true }, .{
        .name = "multiflag",
        .truthy = .{ .short_tag = "-M" },
        .env_var = "NOCLIP_MULTIFLAG",
    });
    cmd.add_argument(.{ .OutputType = []const u8 }, .{
        .name = "arg",
    });

    break :cmd cmd;
};

const subcommand = cmd: {
    var cmd = CommandBuilder(void).init();
    cmd.add_flag(.{}, .{
        .name = "flag",
        .truthy = .{ .short_tag = "-f", .long_tag = "--flag" },
        .falsy = .{ .long_tag = "--no-flag" },
        .env_var = "NOCLIP_SUBFLAG",
    });
    cmd.add_argument(.{ .OutputType = []const u8 }, .{ .name = "argument" });
    break :cmd cmd;
};

fn sub_handler(_: *void, result: subcommand.Output()) !void {
    std.debug.print("subcommand: {s}\n", .{result.argument});
}

fn cli_handler(context: *u32, result: cli.Output()) !void {
    _ = context;

    std.debug.print("callback is working {any}\n", .{result.choice});
    std.debug.print("callback is working {d}\n", .{result.default});
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = cli.bind(cli_handler, allocator);
    var context: u32 = 2;

    var subcon = subcommand.bind(sub_handler, allocator);
    try parser.add_subcommand("verb", subcon.interface());

    const iface = parser.interface(&context);
    try iface.execute();
}
