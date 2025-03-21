const std = @import("std");

const NoclipError = @import("./errors.zig").NoclipError;
const ncmeta = @import("./meta.zig");
const FixedCount = @import("./parameters.zig").FixedCount;
const parser = @import("./parser.zig");

const AlignablePair = struct {
    left: []const u8,
    right: []const u8,
};

const OptionDescription = struct {
    pairs: []AlignablePair,
    just: usize,
};

pub fn StructuredPrinter(comptime Writer: type) type {
    return struct {
        wrap_width: usize = 100,
        writer: Writer,

        pub fn printPair(self: *@This(), pair: AlignablePair, leading_indent: u8, tabstop: usize) !void {
            try self.writer.writeByteNTimes(' ', leading_indent);
            const left = std.mem.trim(u8, pair.left, " \n");
            try self.writer.writeAll(left);

            const offset: usize = leading_indent + left.len;
            // TODO: lol return a real error
            if (offset > tabstop) return NoclipError.UnexpectedFailure;

            try self.writer.writeByteNTimes(' ', tabstop - offset);
            try self.printRewrap(std.mem.trim(u8, pair.right, " \n"), tabstop);
            try self.writer.writeByte('\n');
        }

        pub fn printPairBrief(self: *@This(), pair: AlignablePair, leading_indent: u8, tabstop: usize) !void {
            const brief = ncmeta.partition(u8, pair.right, &[_][]const u8{"\n\n"})[0];
            const simulacrum: AlignablePair = .{
                .left = pair.left,
                .right = brief,
            };

            try self.printPair(simulacrum, leading_indent, tabstop);
        }

        pub fn printWrapped(self: *@This(), text: []const u8, leading_indent: usize) !void {
            try self.writer.writeByteNTimes(' ', leading_indent);
            try self.printRewrap(std.mem.trim(u8, text, "\n"), leading_indent);
        }

        fn printRewrap(self: *@This(), text: []const u8, indent: usize) !void {
            // TODO: lol return a real error
            if (indent >= self.wrap_width) return NoclipError.UnexpectedFailure;

            if (text.len == 0) return;

            // this assumes output stream has already had the first line properly
            // indented.
            var splitter = std.mem.splitScalar(u8, text, '\n');

            var location: usize = indent;
            while (splitter.next()) |line| {
                if (line.len == 0) {
                    // we have a trailing line that needs to be cleaned up
                    if (location > indent)
                        _ = try self.clearLine(indent);

                    location = try self.clearLine(indent);
                    continue;
                }

                if (line[0] == '>') maybe: {
                    if (line.len > 1) {
                        if (line[1] == ' ') {
                            try self.writer.writeAll(line[2..]);
                        } else break :maybe;
                    }
                    location = try self.clearLine(indent);
                    continue;
                }

                var choppee = line;
                var need_forced_break = false;
                choppa: while (choppee.len > 0) {
                    const breakoff = self.wrap_width - location;

                    if (breakoff >= choppee.len) {
                        if (location > indent)
                            try self.writer.writeByte(' ');

                        try self.writer.writeAll(choppee);
                        location += choppee.len;
                        break;
                    }

                    var split = breakoff;
                    while (choppee[split] != ' ') : (split -= 1) {
                        if (split == 0) {
                            // we have encountered a word that is too long to break,
                            // so force breaking it
                            if (need_forced_break) {
                                split = breakoff;
                                break;
                            }
                            if (location != indent)
                                location = try self.clearLine(indent);

                            need_forced_break = true;
                            continue :choppa;
                        }
                    }
                    if (location > indent)
                        try self.writer.writeByte(' ');

                    try self.writer.writeAll(choppee[0..split]);
                    location = try self.clearLine(indent);
                    choppee = choppee[split + 1 ..];
                }
            }
        }

        fn clearLine(self: *@This(), indent: usize) !usize {
            try self.writer.writeByte('\n');
            try self.writer.writeByteNTimes(' ', indent);
            return indent;
        }
    };
}

pub fn HelpBuilder(comptime command: anytype) type {
    const help_info = optInfo(command.generate());

    return struct {
        writebuffer: std.ArrayList(u8),

        pub fn init(allocator: std.mem.Allocator) @This() {
            return @This(){
                .writebuffer = std.ArrayList(u8).init(allocator),
            };
        }

        pub fn buildMessage(
            self: *@This(),
            name: []const u8,
            subcommands: parser.CommandMap,
        ) ![]const u8 {
            const writer = self.writebuffer.writer();
            try writer.print(
                "Usage: {s}{s}{s}{s}\n\n",
                .{
                    name,
                    self.optionBrief(),
                    try self.argsBrief(),
                    self.subcommandsBrief(subcommands),
                },
            );

            var printer = StructuredPrinter(@TypeOf(writer)){ .writer = writer };
            try printer.printWrapped(command.description, 2);
            try writer.writeAll("\n\n");

            const arguments = try self.describeArguments();
            const options = try self.describeOptions();
            const env_vars = try self.describeEnv();
            const subcs = try self.describeSubcommands(subcommands);
            const max_just = @max(arguments.just, @max(options.just, @max(env_vars.just, subcs.just)));

            if (arguments.pairs.len > 0) {
                try writer.writeAll("Arguments:\n");
                for (arguments.pairs) |pair|
                    try printer.printPair(pair, 2, max_just + 4);

                try writer.writeAll("\n");
            }

            if (options.pairs.len > 0) {
                try writer.writeAll("Options:\n");
                for (options.pairs) |pair|
                    try printer.printPair(pair, 2, max_just + 4);

                try writer.writeAll("\n");
            }

            if (env_vars.pairs.len > 0) {
                try writer.writeAll("Environment variables:\n");
                for (env_vars.pairs) |pair|
                    try printer.printPair(pair, 2, max_just + 4);

                try writer.writeAll("\n");
            }

            if (subcs.pairs.len > 0) {
                try writer.writeAll("Subcommands:\n");
                for (subcs.pairs) |pair|
                    try printer.printPairBrief(pair, 2, max_just + 4);

                try writer.writeAll("\n");
            }

            return self.writebuffer.toOwnedSlice();
        }

        fn optionBrief(_: @This()) []const u8 {
            return if (comptime help_info.options.len > 0)
                " [options...]"
            else
                "";
        }

        fn argsBrief(self: @This()) ![]const u8 {
            var buf = std.ArrayList(u8).init(self.writebuffer.allocator);
            defer buf.deinit();

            const writer = buf.writer();

            for (comptime help_info.arguments) |arg| {
                try writer.writeAll(" ");
                if (!arg.required) try writer.writeAll("[");
                try writer.writeByte('<');
                try writer.writeAll(arg.name);
                if (arg.multi)
                    try writer.print(" [{s} ...]", .{arg.name});
                try writer.writeByte('>');
                if (!arg.required) try writer.writeAll("]");
            }

            return buf.toOwnedSlice();
        }

        fn subcommandsBrief(_: @This(), subcommands: parser.CommandMap) []const u8 {
            return if (subcommands.count() > 0)
                " <subcommand ...>"
            else
                "";
        }

        fn describeArguments(self: @This()) !OptionDescription {
            var pairs = std.ArrayList(AlignablePair).init(self.writebuffer.allocator);
            defer pairs.deinit();

            var just: usize = 0;
            inline for (comptime help_info.arguments) |arg| {
                const pair: AlignablePair = .{
                    .left = arg.name,
                    .right = arg.description,
                };
                if (pair.left.len > just) just = pair.left.len;
                try pairs.append(pair);
            }

            return .{
                .pairs = try pairs.toOwnedSlice(),
                .just = just,
            };
        }

        fn describeOptions(self: @This()) !OptionDescription {
            var pairs = std.ArrayList(AlignablePair).init(self.writebuffer.allocator);
            defer pairs.deinit();

            var just: usize = 0;
            inline for (help_info.options) |opt| {
                const pair = try self.describeOption(opt);
                if (pair.left.len > just) just = pair.left.len;
                try pairs.append(pair);
            }

            return .{
                .pairs = try pairs.toOwnedSlice(),
                .just = just,
            };
        }

        fn describeOption(self: @This(), comptime opt: OptHelp) !AlignablePair {
            var buffer = std.ArrayList(u8).init(self.writebuffer.allocator);
            defer buffer.deinit();
            const writer = buffer.writer();

            if (comptime opt.short_truthy) |tag| {
                if (buffer.items.len > 0) try writer.writeAll(", ");
                try writer.writeAll(tag);
            }
            if (comptime opt.long_truthy) |tag| {
                if (buffer.items.len > 0) try writer.writeAll(", ");
                try writer.writeAll(tag);
            }

            var falsy_seen = false;
            if (comptime opt.short_falsy) |tag| {
                if (buffer.items.len > 0)
                    try writer.writeAll(" / ")
                else
                    try writer.writeAll("/ ");
                try writer.writeAll(tag);
                falsy_seen = true;
            }
            if (comptime opt.long_falsy) |tag| {
                if (falsy_seen)
                    try writer.writeAll(", ")
                else if (buffer.items.len > 0)
                    try writer.writeAll(" / ");

                try writer.writeAll(tag);
            }
            if (opt.value_count > 0) {
                try writer.print(" <{s}>", .{opt.type_name});
            }

            const left = try buffer.toOwnedSlice();

            if (comptime opt.required) {
                try writer.writeAll("[required]");
            }

            if (comptime opt.description.len > 0) {
                if (buffer.items.len > 0) try writer.writeAll(" ");
                try writer.writeAll(opt.description);
            }

            if (comptime opt.env_var) |env| {
                if (buffer.items.len > 0) try writer.writeAll(" ");
                try writer.print("(env: {s})", .{env});
            }

            if (comptime opt.default) |def| {
                if (buffer.items.len > 0) try writer.writeAll(" ");
                try writer.print("(default: {s})", .{def});
            }

            const right = try buffer.toOwnedSlice();

            return .{ .left = left, .right = right };
        }

        fn describeEnv(self: @This()) !OptionDescription {
            var pairs = std.ArrayList(AlignablePair).init(self.writebuffer.allocator);
            defer pairs.deinit();

            var just: usize = 0;
            for (comptime help_info.env_vars) |env| {
                if (env.description.len == 0) continue;

                const pair: AlignablePair = .{
                    .left = env.env_var,
                    .right = env.description,
                };
                if (pair.left.len > just) just = pair.left.len;
                try pairs.append(pair);
            }

            return .{
                .pairs = try pairs.toOwnedSlice(),
                .just = just,
            };
        }

        fn describeSubcommands(self: @This(), subcommands: parser.CommandMap) !OptionDescription {
            var pairs = std.ArrayList(AlignablePair).init(self.writebuffer.allocator);
            defer pairs.deinit();

            var just: usize = 0;
            for (subcommands.keys()) |key| {
                const pair: AlignablePair = .{
                    .left = key,
                    .right = subcommands.get(key).?.describe(),
                };
                if (pair.left.len > just) just = pair.left.len;
                try pairs.append(pair);
            }

            return .{
                .pairs = try pairs.toOwnedSlice(),
                .just = just,
            };
        }
    };
}

const CommandHelp = struct {
    options: []const OptHelp,
    arguments: []const ArgHelp,
    env_vars: []const EnvHelp,
};

const OptHelp = struct {
    short_truthy: ?[]const u8 = null,
    long_truthy: ?[]const u8 = null,
    short_falsy: ?[]const u8 = null,
    long_falsy: ?[]const u8 = null,
    env_var: ?[]const u8 = null,
    description: []const u8 = "",
    type_name: []const u8 = "",
    extra: []const u8 = "",
    default: ?[]const u8 = null,
    // this is the pivot
    value_count: FixedCount = 0,
    required: bool = false,
    multi: bool = false,
};

const EnvHelp = struct {
    env_var: []const u8 = "",
    description: []const u8 = "",
    default: ?[]const u8 = null,
};

const ArgHelp = struct {
    name: []const u8 = "",
    description: []const u8 = "",
    type_name: []const u8 = "",
    multi: bool = false,
    required: bool = true,
};

pub fn optInfo(comptime command: anytype) CommandHelp {
    // TODO: this could be runtime and it would be slightly simpler.
    comptime {
        var options: []const OptHelp = &[_]OptHelp{};
        var env_vars: []const EnvHelp = &[_]EnvHelp{};
        var arguments: []const ArgHelp = &[_]ArgHelp{};

        var last_name: []const u8 = "";
        var last_option: OptHelp = .{};

        paramloop: for (command) |param| {
            const PType = @TypeOf(param);
            if (PType.param_type == .Ordinal) {
                arguments = arguments ++ &[_]ArgHelp{.{
                    .name = param.name,
                    .description = param.description,
                    .type_name = param.nice_type_name,
                    .multi = PType.multi,
                    .required = param.required,
                }};

                continue :paramloop;
            }

            if (!std.mem.eql(u8, last_name, param.name)) {
                if (last_name.len > 0) {
                    if (envOnly(last_option)) {
                        env_vars = env_vars ++ &[_]EnvHelp{.{
                            .env_var = last_option.env_var,
                            .description = last_option.description,
                            .default = last_option.default,
                        }};
                    } else {
                        options = options ++ &[_]OptHelp{last_option};
                    }
                }
                last_name = param.name;
                last_option = .{};
            }

            if (PType.is_flag) {
                switch (param.flag_bias) {
                    .truthy => {
                        last_option.short_truthy = param.short_tag;
                        last_option.long_truthy = param.long_tag;
                    },
                    .falsy => {
                        last_option.short_falsy = param.short_tag;
                        last_option.long_falsy = param.long_tag;
                    },
                    .unbiased => last_option.env_var = param.env_var,
                }
            } else {
                last_option.short_truthy = param.short_tag;
                last_option.long_truthy = param.long_tag;
                last_option.env_var = param.env_var;
                last_option.value_count = PType.value_count.fixed;
            }
            last_option.type_name = param.nice_type_name;
            last_option.description = param.description;
            last_option.required = param.required;
            last_option.multi = PType.multi;

            if (param.default) |def| {
                var buf = ncmeta.ComptimeSliceBuffer{};
                const writer = buf.writer();
                // TODO: this is only acceptable for some types. It behaves poorly on
                // enum-based choice types because it prints the whole type name rather
                // than just the tag name. Roll our own eventually.
                blk: {
                    switch (@typeInfo(@TypeOf(def))) {
                        .pointer => |info| if (info.size == .Slice and info.child == u8) {
                            writer.print("{s}", .{def}) catch @compileError("no");
                            break :blk;
                        },
                        else => {},
                    }
                    writer.print("{any}", .{def}) catch @compileError("whoah");
                }

                last_option.default = buf.buffer;
            }
        }

        if (last_name.len > 0) {
            if (envOnly(last_option)) {
                env_vars = env_vars ++ &[_]EnvHelp{.{
                    .env_var = last_option.env_var.?,
                    .description = last_option.description,
                    .default = last_option.default,
                }};
            } else {
                options = options ++ &[_]OptHelp{last_option};
            }
        }

        return .{
            .options = options,
            .arguments = arguments,
            .env_vars = env_vars,
        };
    }
}

inline fn envOnly(option: OptHelp) bool {
    return option.short_truthy == null and
        option.long_truthy == null and
        option.short_falsy == null and
        option.long_falsy == null;
}
