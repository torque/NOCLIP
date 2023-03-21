const std = @import("std");
const noclip = @import("noclip");

const context: []const u8 = "hello friend";
const ContextType = @TypeOf(context);

const subcommand = blk: {
    var cmd = noclip.Command(ContextType, .{ .name = "verb", .help = "this a sub command" });
    cmd.add(cmd.defaultHelpFlag);
    cmd.add(cmd.StringOption{ .name = "meta", .short = "-m" });
    cmd.add(cmd.StringArgument{ .name = "argument" });
    cmd.add(cmd.Argument(u32){ .name = "another", .default = 0 });
    break :blk cmd;
};

const command = blk: {
    var cmd = noclip.Command(ContextType, .{
        .name = "main",
        .help =
        \\This is the main CLI entry point for the noclip demo
        \\
        \\This program demonstrates the major features of noclip both in terms of API
        \\usage (in its source code) and argument parsing (in its execution).
        ,
    });
    cmd.add(cmd.defaultHelpFlag);
    cmd.add(cmd.Flag{ .name = "flag", .truthy = .{ .short = "-f", .long = "--flag" }, .falsy = .{ .long = "--no-flag" } });
    cmd.add(cmd.StringOption{
        .name = "input",
        .short = "-i",
        .long = "--input",
        .help = "some generic input",
        .default = "in",
        .envVar = "OPTS_INPUT",
    });
    cmd.add(cmd.StringOption{
        .name = "output",
        .long = "--output",
        .default = "waoh",
        .help = "name of the output",
    });
    cmd.add(cmd.Option(i32){
        .name = "number",
        .short = "-n",
        .long = "--number",
        .help = "a number",
        .default = 0,
    });

    cmd.add(subcommand.Parser(subCallback));
    break :blk cmd;
};

fn printHandler(ctx: ContextType, input: []const u8) ![]const u8 {
    std.debug.print("ctx: {s}\n", .{ctx});
    return input;
}

pub fn subCallback(_: ContextType, result: subcommand.CommandResult()) !void {
    std.debug.print(
        \\subcommand: {{
        \\    .meta = {s}
        \\    .argument = {s}
        \\    .another = {d}
        \\}}
        \\
    ,
        .{ result.meta, result.argument, result.another },
    );
}

pub fn mainCommand(_: ContextType, result: command.CommandResult()) !void {
    // std.debug.print("{any}", .{result});
    std.debug.print(
        \\arguments: {{
        \\    .flag = {any}
        \\    .input = {s}
        \\    .output = {s}
        \\    .number = {d}
        \\}}
        \\
    ,
        .{ result.flag, result.input, result.output, result.number },
    );
}

pub fn main() !void {
    var parser = command.Parser(mainCommand);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    var argit = try std.process.argsWithAllocator(allocator);

    try parser.execute(allocator, std.process.ArgIterator, &argit, context);
}
