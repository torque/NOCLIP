const std = @import("std");
const noclip = @import("noclip");

const context: []const u8 = "hello friend";
const ContextType = @TypeOf(context);

const helpFlag = noclip.HelpFlag(.{ .UserContext = ContextType });

const subData: noclip.CommandData = .{ .name = "subcommand", .help = "this a sub command" };
const subFlag: noclip.StringOption(ContextType) = .{ .name = "meta", .short = "-m" };
const subArg: noclip.StringArg(ContextType) = .{ .name = "sub" };
const subSpec = .{ helpFlag, subFlag, subArg };
const subCommand: noclip.CommandParser(subData, subSpec, ContextType, subCallback) = .{};

fn wrecker(zontext: ContextType, input: []const u8) ![]const u8 {
    std.debug.print("ctx: {s}\n", .{zontext});
    return input;
}

const cdata: noclip.CommandData = .{ .name = "main", .help = "main CLI entry point" };
const flagCheck: noclip.FlagOption(ContextType) = .{
    .name = "flag",
    .truthy = .{ .short = "-f", .long = "--flag" },
    .falsy = .{ .long = "--no-flag" },
};
const inputOption: noclip.StringOption(ContextType) = .{
    .name = "input",
    .short = "-i",
    .long = "--input",
    .handler = wrecker,
    .envVar = "OPTS_INPUT",
};
const outputOption: noclip.StringOption(ContextType) = .{ .name = "output", .long = "--output", .default = "waoh" };
const numberOption: noclip.ValuedOption(.{ .Output = i32, .UserContext = ContextType }) = .{ .name = "number", .short = "-n", .long = "--number" };
const argCheck: noclip.StringArg(ContextType) = .{ .name = "argument" };
const argAgain: noclip.StringArg(ContextType) = .{ .name = "another", .default = "nope" };

const mainSpec = .{
    helpFlag,
    flagCheck,
    inputOption,
    outputOption,
    numberOption,
    argCheck,
    argAgain,
    subCommand,
};

pub fn subCallback(_: ContextType, result: noclip.CommandResult(subSpec, ContextType)) !void {
    std.debug.print("subcommand {any}!!!\n", .{result});
}

pub fn mainCommand(_: ContextType, result: noclip.CommandResult(mainSpec, ContextType)) !void {
    std.debug.print(
        \\arguments: {{
        \\    .flag = {any}
        \\    .input = {s}
        \\    .output = {s}
        \\    .number = {d}
        \\    .argument = {s}
        \\    .another = {s}
        \\}}
        \\
    ,
        .{
            result.flag,
            result.input,
            result.output,
            result.number,
            result.argument,
            result.another,
        },
    );
}

pub fn main() !void {
    var command: noclip.CommandParser(cdata, mainSpec, ContextType, mainCommand) = .{};

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    var argit = try std.process.argsWithAllocator(allocator);
    _ = argit.next();

    try command.execute(allocator, std.process.ArgIterator, &argit, context);
}
