const std = @import("std");

const errors = @import("./errors.zig");
const help = @import("./help.zig");
const ncmeta = @import("./meta.zig");

const ParseError = errors.ParseError;
const NoclipError = errors.NoclipError;

pub const ParserInterface = struct {
    const Vtable = struct {
        execute: *const fn (parser: *anyopaque, context: *anyopaque) anyerror!void,
        parse: *const fn (parser: *anyopaque, context: *anyopaque, name: []const u8, args: [][:0]u8, env: std.process.EnvMap) anyerror!void,
        finish: *const fn (parser: *anyopaque, context: *anyopaque) anyerror!void,
        getChild: *const fn (parser: *anyopaque, name: []const u8) ?ParserInterface,
        describe: *const fn () []const u8,
        deinit: *const fn (parser: *anyopaque) void,
        deinitTree: *const fn (parser: *anyopaque) void,
    };

    parser: *anyopaque,
    context: *anyopaque,
    methods: *const Vtable,

    fn create(comptime ParserType: type, parser: *anyopaque, context: *anyopaque) @This() {
        return .{
            .parser = parser,
            .context = context,
            .methods = &.{
                .execute = ParserType._wrapExecute,
                .parse = ParserType._wrapParse,
                .finish = ParserType._wrapFinish,
                .getChild = ParserType._wrapGetChild,
                .describe = ParserType._wrapDescribe,
                .deinit = ParserType._wrapDeinit,
                .deinitTree = ParserType._wrapDeinitTree,
            },
        };
    }

    pub fn execute(self: @This()) anyerror!void {
        return try self.methods.execute(self.parser, self.context);
    }

    pub fn parse(self: @This(), name: []const u8, args: [][:0]u8, env: std.process.EnvMap) anyerror!void {
        return try self.methods.parse(self.parser, self.context, name, args, env);
    }

    pub fn finish(self: @This()) anyerror!void {
        return try self.methods.finish(self.parser, self.context);
    }

    pub fn getChild(self: @This(), name: []const u8) ?ParserInterface {
        return self.methods.getChild(self.parser, name);
    }

    pub fn describe(self: @This()) []const u8 {
        return self.methods.describe();
    }

    pub fn deinit(self: @This()) void {
        self.methods.deinit(self.parser);
    }

    pub fn deinitTree(self: @This()) void {
        self.methods.deinitTree(self.parser);
    }
};

fn InterfaceWrappers(comptime ParserType: type) type {
    return struct {
        inline fn castInterfaceParser(parser: *anyopaque) *ParserType {
            return @ptrCast(@alignCast(parser));
        }

        fn _wrapExecute(parser: *anyopaque, ctx: *anyopaque) anyerror!void {
            const self = castInterfaceParser(parser);

            const context = self.castContext(ctx);
            return try self.execute(context);
        }

        fn _wrapParse(
            parser: *anyopaque,
            ctx: *anyopaque,
            name: []const u8,
            args: [][:0]u8,
            env: std.process.EnvMap,
        ) anyerror!void {
            const self = castInterfaceParser(parser);
            const context = self.castContext(ctx);
            return try self.subparse(context, name, args, env);
        }

        fn _wrapFinish(parser: *anyopaque, ctx: *anyopaque) anyerror!void {
            const self = castInterfaceParser(parser);
            const context = self.castContext(ctx);
            return try self.finish(context);
        }

        fn _wrapGetChild(parser: *anyopaque, name: []const u8) ?ParserInterface {
            const self = castInterfaceParser(parser);
            return self.getChild(name);
        }

        fn _wrapDeinit(parser: *anyopaque) void {
            const self = castInterfaceParser(parser);
            self.deinit();
        }

        fn _wrapDeinitTree(parser: *anyopaque) void {
            const self = castInterfaceParser(parser);
            self.deinitTree();
        }

        fn _wrapDescribe() []const u8 {
            return ParserType.command_description;
        }
    };
}

fn InterfaceGen(comptime ParserType: type, comptime UserContext: type) type {
    const CtxInfo = @typeInfo(UserContext);

    return if (CtxInfo == .Void) struct {
        pub fn interface(self: *ParserType) ParserInterface {
            return ParserInterface.create(ParserType, self, @constCast(&void{}));
        }

        fn castContext(_: ParserType, _: *anyopaque) void {
            return void{};
        }
    } else if (CtxInfo == .Pointer and CtxInfo.Pointer.size != .Slice) struct {
        pub fn interface(self: *ParserType, context: UserContext) ParserInterface {
            return ParserInterface.create(ParserType, self, @constCast(context));
        }

        fn castContext(_: ParserType, ctx: *anyopaque) UserContext {
            return @ptrCast(@alignCast(ctx));
        }
    } else struct {
        pub fn interface(self: *ParserType, context: *const UserContext) ParserInterface {
            return ParserInterface.create(ParserType, self, @ptrCast(@constCast(context)));
        }

        fn castContext(_: ParserType, ctx: *anyopaque) UserContext {
            return @as(*const UserContext, @ptrCast(@alignCast(ctx))).*;
        }
    };
}

pub const CommandMap = std.hash_map.StringHashMap(ParserInterface);

// the parser is generated by the bind method of the CommandBuilder, so we can
// be extremely type-sloppy here, which simplifies the signature.
pub fn Parser(comptime command: anytype, comptime callback: anytype) type {
    const UserContext = @TypeOf(command).UserContextType;
    const parameters = command.generate();
    const Intermediate = command.Intermediate();
    const Output = command.Output();

    return struct {
        const command_description = command.description;

        intermediate: Intermediate = .{},
        output: Output = undefined,
        consumed_args: u32 = 0,
        progname: ?[]const u8 = null,
        has_global_tags: bool = false,
        arena: *std.heap.ArenaAllocator,
        allocator: std.mem.Allocator,
        subcommands: CommandMap,
        subcommand: ?ParserInterface = null,
        help_builder: help.HelpBuilder(command),

        pub fn addSubcommand(self: *@This(), verb: []const u8, parser: ParserInterface) !void {
            try self.subcommands.put(verb, parser);
        }

        // This is a slightly annoying hack to work around the fact that there's no way to
        // provide a method signature conditionally.
        pub usingnamespace InterfaceGen(@This(), UserContext);
        pub usingnamespace InterfaceWrappers(@This());

        pub fn subparse(self: *@This(), context: UserContext, name: []const u8, args: [][:0]u8, env: std.process.EnvMap) anyerror!void {
            const sliceto = try self.parse(name, args);
            try self.readEnvironment(env);
            try self.convertEager(context);

            if (self.subcommand) |verb| {
                const verbname = try std.mem.join(
                    self.allocator,
                    " ",
                    &[_][]const u8{ name, args[sliceto - 1] },
                );
                try verb.parse(verbname, args[sliceto..], env);
            } else if (self.subcommands.count() > 0 and command.subcommand_required) {
                const stderr = std.io.getStdErr().writer();
                try stderr.writeAll("A subcommand is required.\n\n");

                self.printHelp(name);
            }
        }

        pub fn finish(self: *@This(), context: UserContext) anyerror!void {
            try self.convert(context);
            try callback(context, self.output);
            if (self.subcommand) |verb| try verb.finish();
        }

        pub fn deinit(self: @This()) void {
            self.arena.deinit();
            self.arena.child_allocator.destroy(self.arena);
        }

        pub fn deinitTree(self: @This()) void {
            var iterator = self.subcommands.valueIterator();
            while (iterator.next()) |subcommand| {
                subcommand.deinitTree();
            }
            self.deinit();
        }

        pub fn getChild(self: @This(), name: []const u8) ?ParserInterface {
            return self.subcommands.get(name);
        }

        pub fn execute(self: *@This(), context: UserContext) anyerror!void {
            const args = try std.process.argsAlloc(self.allocator);
            var env = try std.process.getEnvMap(self.allocator);

            if (args.len < 1) return ParseError.EmptyArgs;

            self.progname = std.fs.path.basename(args[0]);

            try self.subparse(context, self.progname.?, args[1..], env);
            try self.finish(context);
        }

        fn printValue(self: @This(), value: anytype, comptime indent: []const u8) void {
            if (comptime @hasField(@TypeOf(value), "items")) {
                std.debug.print("{s}[\n", .{indent});
                for (value.items) |item| {
                    self.printValue(item, indent ++ "   ");
                }
                std.debug.print("{s}]\n", .{indent});
            } else {
                std.debug.print("{s}{s}\n", .{ indent, value });
            }
        }

        pub fn parse(
            self: *@This(),
            name: []const u8,
            args: [][:0]u8,
        ) anyerror!usize {
            // run pre-parse pass if we have any global parameters
            // try self.preparse()

            var forced_ordinal = false;
            var argit = ncmeta.SliceIterator(@TypeOf(args)).wrap(args);

            // there are a LOT of different parsing strategies that can be adopted to
            // handle "incorrect" command lines. For example, a --long-style named
            // argument could be parsed as an ordered argument if it doesn't match any
            // of the specified tag names. However, if the user has not passed `--`
            // then it's more likely the erroneous flag is a typo or some other
            // erroneous input and should be treated as such. Similarly, handling the
            // pair `--long-style --some-value`. if long_style takes one value,
            // should --some-value be treated as the value, or should we assume the
            // user forgot the value and is specifying a second tag? Getting too clever
            // with context (e.g. checking if --some-value is a known tag name)
            // probably also violates the principle of least astonishment, as if it
            // doesn't match, it could very likely be a typo or other erroneous input.
            // In this case we have an out, sort of, as --long-style=--some-value is
            // unambiguous in purpose. However, this approach misses for short flags,
            // unless we also support a -l=--some-value syntax, which I don't like and
            // don't think is a common convention. In this case, I think it is
            // reasonable to consume the value without getting fancy,
            // e.g. -l --some-value produces 'long_style: "--some-value"'. Odds are, if
            // the command line was specified incorrectly, the error will cascade
            // through somewhere.

            // another consideration is how to deal with mixed --named and positional
            // arguments. Theoretically, fixed quantity positional arguments can be
            // unambiguously interspersed with named arguments, but that feels sloppy.
            // If a positional argument needs to start with --, we have the -- argument
            // to force positional parsing.

            argloop: while (argit.next()) |arg| {
                if (!forced_ordinal and std.mem.eql(u8, arg, "--")) {
                    forced_ordinal = true;
                    continue :argloop;
                }

                if (!forced_ordinal and arg.len > 1 and arg[0] == '-') {
                    if (arg.len > 2 and arg[1] == '-') {
                        try self.parseLongTag(name, arg, &argit);
                        continue :argloop;
                    } else if (arg.len > 1) {
                        for (arg[1..], 1..) |short, idx| {
                            try self.parseShortTag(name, short, arg.len - idx - 1, &argit);
                        }
                        continue :argloop;
                    }

                    // if we've fallen through to here then we will be parsing ordinals
                    // exclusively from here on out.
                    forced_ordinal = true;
                }

                if (try self.parseOrdinals(arg, &argit)) |verb| {
                    self.subcommand = verb;
                    // TODO: return slice of remaining or offset index
                    return argit.index;
                }
            }

            return 0;
        }

        fn parseLongTag(
            self: *@This(),
            name: []const u8,
            arg: []const u8,
            argit: *ncmeta.SliceIterator([][:0]u8),
        ) ParseError!void {
            if (comptime command.help_flag.long_tag) |long|
                if (std.mem.eql(u8, arg, long))
                    self.printHelp(name);

            inline for (comptime parameters) |param| {
                const PType = @TypeOf(param);
                // removing the comptime here causes the compiler to die
                comptime if (PType.param_type != .Nominal or param.long_tag == null) continue;
                const tag = param.long_tag.?;

                if (std.mem.startsWith(u8, arg, tag)) match: {
                    if (arg.len == tag.len) {
                        try self.applyParamValues(param, argit, false);
                    } else if (arg[tag.len] == '=') {
                        try self.applyFusedValues(param, arg[tag.len + 1 ..]);
                    } else break :match;

                    return;
                }
            }

            return ParseError.UnknownLongTagParameter;
        }

        fn parseShortTag(
            self: *@This(),
            name: []const u8,
            arg: u8,
            remaining: usize,
            argit: *ncmeta.SliceIterator([][:0]u8),
        ) ParseError!void {
            if (comptime command.help_flag.short_tag) |short|
                if (arg == short[1])
                    self.printHelp(name);

            inline for (comptime parameters) |param| {
                const PType = @TypeOf(param);
                // removing the comptime here causes the compiler to die
                comptime if (PType.param_type != .Nominal or param.short_tag == null) continue;
                const tag = param.short_tag.?;

                if (arg == tag[1]) {
                    if (comptime !PType.is_flag)
                        if (remaining > 0)
                            return ParseError.FusedShortTagValueMissing;

                    try self.applyParamValues(param, argit, false);
                    return;
                }
            }

            return ParseError.UnknownShortTagParameter;
        }

        fn parseOrdinals(
            self: *@This(),
            arg: []const u8,
            argit: *ncmeta.SliceIterator([][:0]u8),
        ) ParseError!?ParserInterface {
            comptime var arg_index: u32 = 0;
            inline for (comptime parameters) |param| {
                comptime if (@TypeOf(param).param_type != .Ordinal) continue;

                if (self.consumed_args == arg_index) {
                    argit.rewind();
                    if (comptime @TypeOf(param).G.multi) {
                        while (argit.peek()) |_| try self.applyParamValues(param, argit, false);
                    } else {
                        try self.applyParamValues(param, argit, false);
                    }
                    self.consumed_args += 1;
                    return null;
                }

                arg_index += 1;
            }

            return self.subcommands.get(arg) orelse ParseError.ExtraValue;
        }

        fn pushIntermediateValue(
            self: *@This(),
            comptime param: anytype,
            // @TypeOf(param).G.IntermediateValue() should work but appears to trigger a
            //  compiler bug: expected pointer, found 'u1'
            value: param.IntermediateValue(),
        ) ParseError!void {
            const gen = @TypeOf(param).G;
            if (comptime gen.multi) {
                if (@field(self.intermediate, param.name) == null) {
                    @field(self.intermediate, param.name) = gen.IntermediateType().init(self.allocator);
                }
                @field(self.intermediate, param.name).?.append(value) catch return ParseError.UnexpectedFailure;
            } else if (comptime @TypeOf(param).G.nonscalar()) {
                if (@field(self.intermediate, param.name)) |list| list.deinit();
                @field(self.intermediate, param.name) = value;
            } else {
                @field(self.intermediate, param.name) = value;
            }
        }

        fn applyParamValues(
            self: *@This(),
            comptime param: anytype,
            argit: anytype,
            bounded: bool,
        ) ParseError!void {
            switch (comptime @TypeOf(param).G.value_count) {
                .flag => try self.pushIntermediateValue(param, comptime param.flag_bias.string()),
                .count => @field(self.intermediate, param.name) += 1,
                .fixed => |count| switch (count) {
                    0 => return ParseError.ExtraValue,
                    1 => try self.pushIntermediateValue(param, argit.next() orelse return ParseError.MissingValue),
                    else => |total| {
                        var list = std.ArrayList([]const u8).initCapacity(self.allocator, total) catch
                            return ParseError.UnexpectedFailure;

                        var consumed: u32 = 0;
                        while (consumed < total) : (consumed += 1) {
                            const next = argit.next() orelse return ParseError.MissingValue;
                            list.append(next) catch return ParseError.UnexpectedFailure;
                        }
                        if (bounded and argit.next() != null) return ParseError.ExtraValue;

                        try self.pushIntermediateValue(param, list);
                    },
                },
            }
        }

        fn applyFusedValues(
            self: *@This(),
            comptime param: anytype,
            value: []const u8,
        ) ParseError!void {
            var iter = std.mem.split(u8, value, ",");
            return try self.applyParamValues(param, &iter, true);
        }

        fn readEnvironment(self: *@This(), env: std.process.EnvMap) !void {
            inline for (comptime parameters) |param| {
                if (comptime param.env_var) |env_var| blk: {
                    if (@field(self.intermediate, param.name) != null) break :blk;
                    const val = env.get(env_var) orelse break :blk;
                    if (comptime @TypeOf(param).G.value_count == .flag) {
                        try self.pushIntermediateValue(param, val);
                    } else {
                        try self.applyFusedValues(param, val);
                    }
                }
            }
        }

        fn convertEager(self: *@This(), context: UserContext) NoclipError!void {
            inline for (comptime parameters) |param| {
                if (comptime param.eager) {
                    try self.convertParam(param, context);
                }
            }
        }

        fn convert(self: *@This(), context: UserContext) NoclipError!void {
            inline for (comptime parameters) |param| {
                if (comptime !param.eager) {
                    try self.convertParam(param, context);
                }
            }
        }

        fn convertParam(self: *@This(), comptime param: anytype, context: UserContext) NoclipError!void {
            if (@field(self.intermediate, param.name)) |intermediate| {
                var buffer = std.ArrayList(u8).init(self.allocator);
                const writer = buffer.writer();

                if (comptime @TypeOf(param).has_output) {
                    @field(self.output, param.name) = param.converter(context, intermediate, writer) catch |err| {
                        const stderr = std.io.getStdErr().writer();
                        stderr.print("Error parsing option \"{s}\": {s}\n", .{ param.name, buffer.items }) catch {};
                        return err;
                    };
                } else {
                    param.converter(context, intermediate, writer) catch |err| {
                        const stderr = std.io.getStdErr().writer();
                        stderr.print("Error parsing option \"{s}\": {s}\n", .{ param.name, buffer.items }) catch {};
                        return err;
                    };
                }
            } else {
                if (comptime param.required) {
                    return ParseError.RequiredParameterMissing;
                } else if (comptime @TypeOf(param).has_output) {
                    if (comptime param.default) |def| {
                        // this has to be explicitly set because even though we set it as
                        // the field default, it gets clobbered because self.output is
                        // initialized as undefined.
                        @field(self.output, param.name) = def;
                    } else {
                        @field(self.output, param.name) = null;
                        return;
                    }
                }
            }
        }

        fn printHelp(self: *@This(), name: []const u8) noreturn {
            defer std.process.exit(0);

            const stderr = std.io.getStdErr().writer();
            if (self.help_builder.buildMessage(name, self.subcommands)) |message|
                stderr.writeAll(message) catch return
            else |_|
                stderr.writeAll("There was a problem generating the help.") catch return;
        }
    };
}
