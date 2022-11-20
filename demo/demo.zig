const std = @import("std");
const noclip = @import("noclip");

const subData: noclip.CommandData = .{ .name = "subcommand", .help = "this a sub command\n" };

const subFlag = noclip.StringOption{
    .name = "meta",
    .short = "-m",
    .handler = noclip.passthrough,
};
const subArg = noclip.StringArg{
    .name = "sub",
    .handler = noclip.passthrough,
};
const subSpec = .{ subFlag, subArg };
const subCommand = noclip.Command(subData, subSpec, void, subCallback);

const cdata: noclip.CommandData = .{
    .name = "main",
    .help = "main CLI entry point\n",
};
const flagCheck = noclip.FlagOption{
    .name = "flag",
    .default = .{ .value = false },
    .truthy = .{ .short = "-f", .long = "--flag" },
    .falsy = .{ .long = "--no-flag" },
};
const inputOption = noclip.StringOption{
    .name = "input",
    .short = "-i",
    .long = "--input",
    .envVar = "OPTS_INPUT",
    .handler = noclip.passthrough,
};
const outputOption = noclip.StringOption{
    .name = "output",
    .long = "--output",
    .default = .{ .value = "waoh" },
    .handler = noclip.passthrough,
};
const numberOption = noclip.ValuedOption(i32){
    .name = "number",
    .short = "-n",
    .long = "--number",
    .handler = noclip.intHandler(i32),
};
const argCheck = noclip.StringArg{
    .name = "argument",
    .handler = noclip.passthrough,
};
const argAgain = noclip.StringArg{
    .name = "another",
    .handler = noclip.passthrough,
};

const mainSpec = .{
    noclip.defaultHelpFlag,
    flagCheck,
    inputOption,
    outputOption,
    numberOption,
    argCheck,
    argAgain,
    subCommand,
};

pub fn subCallback(_: void, result: noclip.CommandResult(subSpec)) !void {
    std.debug.print("subcommand {}!!!\n", .{result});
}

pub fn mainCommand(_: void, result: noclip.CommandResult(mainSpec)) !void {
    std.debug.print(
        \\arguments: {{
        \\    .flag = {}
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
    const command = noclip.Command(cdata, mainSpec, void, mainCommand);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    var argit = try std.process.argsWithAllocator(allocator);
    _ = argit.next();

    try command.execute(allocator, std.process.ArgIterator, &argit, {});
}
