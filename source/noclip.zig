// Copyright (c) 2022 torque <torque@users.noreply.github.com>

// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.

// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
// REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
// INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
// LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
// OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
// PERFORMANCE OF THIS SOFTWARE.

const std = @import("std");
const StructField = std.builtin.Type.StructField;

const Brand = enum {
    Option,
    Flag,
    Argument,
    Command,
};

pub fn noopHandleGen(comptime ResultType: type) *const fn (buf: []const u8) anyerror!ResultType {
    return struct {
        pub fn handler(input: []const u8) anyerror!ResultType {
            return input;
        }
    }.handler;
}

pub const noopHandler = noopHandleGen([]const u8);
pub const passthrough = noopHandler;
const noOptHandler = noopHandleGen(?[]const u8);

pub fn intHandler(comptime intType: type) *const fn (buf: []const u8) std.fmt.ParseIntError!intType {
    return struct {
        pub fn handler(buf: []const u8) std.fmt.ParseIntError!intType {
            return try std.fmt.parseInt(intType, buf, 0);
        }
    }.handler;
}

pub fn intRadixHandler(comptime intType: type, radix: u8) *const fn (buf: []const u8) std.fmt.ParseIntError!intType {
    return struct {
        pub fn handler(buf: []const u8) std.fmt.ParseIntError!intType {
            return try std.fmt.parseInt(intType, buf, radix);
        }
    }.handler;
}

pub const OptionError = error{
    BadShortOption,
    BadLongOption,
    UnknownOption,
    MissingOption,
    MissingArgument,
    ExtraArguments,
};

pub const ArgCountCategory = enum {
    None,
    Some,
    Many,
};

pub const ArgCount = union(ArgCountCategory) {
    None: void,
    Some: u32,
    Many: void,
};

pub fn ValuedOption(comptime resultType: type) type {
    return struct {
        pub fn brand(_: @This()) Brand {
            return .Option;
        }

        name: []const u8,
        // this fake optional is a workaround for a bug in the stage1 compiler
        // (it doesn't handle nested optionals in struct fields correctly) and
        // should be replaced with proper optionals as soon as stage2 is
        // functional.
        default: union(enum) { none: void, value: resultType } = .none,
        // this is a combination conversion/validation callback.
        // Should we try to pass a user context? Zig's bound functions
        // don't seem to coerce nicely to this type, probably because
        // they're no longer just a pointer. Any nontrivial type may need an
        // allocator context passed.
        handler: *const fn (input: []const u8) anyerror!resultType,
        short: ?*const [2]u8 = null,
        long: ?[]const u8 = null,
        help: ?[]const u8 = null,

        envVar: ?[]const u8 = null,
        hideResult: bool = false,
        eager: bool = false,

        args: ArgCount = .{ .Some = 1 },

        pub fn ResultType(comptime _: @This()) type {
            return resultType;
        }

        pub fn required(self: @This()) bool {
            return self.default == .none;
        }
    };
}

pub const StringOption = ValuedOption([]const u8);

// this could be ValuedOption(bool) except it allows truthy/falsy flag variants
// and it doesn't want to parse a value. It could be lowered into a pair of
// ValuedOption(bool) though, if consuming a value became optional.

const ShortLong = struct {
    short: ?*const [2]u8 = null,
    long: ?[]const u8 = null,
};

pub const FlagOption = struct {
    pub fn brand(_: @This()) Brand {
        return .Flag;
    }

    name: []const u8,
    default: union(enum) { none: void, value: bool } = .{ .value = false },
    truthy: ShortLong = .{},
    falsy: ShortLong = .{},
    help: ?[]const u8 = null,
    // should envVar be split into truthy/falsy the way the args are? otherwise
    // we probably need to peek the value of the environmental variable to see
    // if it is truthy or falsy. Honestly, looking at the value is probably
    // required to avoid violating the principle of least astonishment because
    // otherwise you can get `MY_VAR=false` causing `true` to be emitted, which
    // looks and feels bad. But then we need to establish a truthiness baseline.
    // case insensitive true/false is easy. What about yes/no? 0/1 (or nonzero).
    // How about empty strings? I'd base on how it reads, and `MY_VAR= prog`
    // reads falsy to me.
    envVar: ?[]const u8 = null,
    hideResult: bool = false,
    eager: ?*const fn (cmd: CommandData) anyerror!void = null,

    pub fn ResultType(comptime _: @This()) type {
        return bool;
    }

    pub fn required(self: @This()) bool {
        return self.default == .none;
    }
};

pub fn produceHelp(cmd: CommandData) !void {
    std.debug.print("{s}", .{cmd.help});
    std.process.exit(0);
}

// I haven't really figured out a way not to special case the help flag.
// Everything else assumes that it can be handled in a vacuum without worrying
// about intermediates (and must be so, as we don't have a deterministic order
// for assembling the result. We could make the parse order deterministic, but
// I suspect it would require increasing the parser complexity a fair amount).
// Flag types are created on the fly, so we can only actually hand pre-composed
// help text to whatever callback this provides.
const HelpFlagArgs = struct {
    name: []const u8 = "help",
    short: ?*const [2]u8 = "-h",
    long: ?[]const u8 = "--help",
    help: []const u8 = "print this help message",
};

// this doesn't work in situ,
pub fn HelpFlag(comptime args: HelpFlagArgs) FlagOption {
    return FlagOption{
        .name = args.name,
        .truthy = .{ .short = args.short, .long = args.long },
        .help = args.help,
        .hideResult = true,
        .eager = produceHelp,
    };
}

// but this does, which is kind of silly.
pub const defaultHelpFlag = HelpFlag(.{});

pub fn Argument(comptime resultType: type) type {
    return struct {
        pub fn brand(_: @This()) Brand {
            return .Argument;
        }

        name: []const u8,
        default: union(enum) { none: void, value: resultType } = .none,
        handler: *const fn (input: []const u8) anyerror!resultType,
        help: ?[]const u8 = null,
        hideResult: bool = false,
        // allow loading arguments from environmental variables? I don't think
        // it's possible to come up with sane semantics for this.

        pub fn ResultType(comptime _: @This()) type {
            return resultType;
        }

        pub fn required(self: @This()) bool {
            return self.default == .none;
        }
    };
}

pub const StringArg = Argument([]const u8);

pub const CommandData = struct {
    name: []const u8,
    help: []const u8 = "",
    // cheesy way to allow deferred initialization of the subcommands
    subcommands: ?std.ArrayList(*CommandData) = null,
};

/// spec is a tuple of ValuedOption, FlagOption, and Argument
pub fn Command(
    comptime commandData: CommandData,
    comptime spec: anytype,
    comptime UdType: type,
    comptime callback: *const fn (userdata: UdType, res: CommandResult(spec)) anyerror!void,
) type {
    comptime var argCount = 0;
    comptime var requiredOptions = 0;
    comptime for (spec) |param| {
        switch (param.brand()) {
            .Argument => argCount += 1,
            .Option, .Flag => if (param.required()) {
                requiredOptions += 1;
            },
            .Command => continue,
        }
    };

    const ResultType = CommandResult(spec);
    const RequiredType = RequiredTracker(spec);

    const ParseState = enum { Mixed, ForcedArgs };

    return struct {
        pub fn brand() Brand {
            return .Command;
        }
        // copy happens at comptime
        pub var data: CommandData = commandData;

        /// parse command line arguments from an iterator
        pub fn execute(alloc: std.mem.Allocator, comptime argit_type: type, argit: *argit_type, userdata: UdType) !void {

            // we could precompute some tuples that would simplify some of the later logic:
            // tuple of eager Options/Flags
            // tuple of non-eager Options/Flags
            // tuple of Arguments
            // tuple of Commands

            var result: ResultType = createCommandresult();
            var required: RequiredType = .{};
            var parseState: ParseState = .Mixed;

            try extractEnvVars(alloc, &result, &required);

            var seenArgs: u32 = 0;
            argloop: while (argit.next()) |arg| {
                if (parseState == .Mixed and arg.len > 1 and arg[0] == '-') {
                    if (std.mem.eql(u8, "--", arg)) {
                        // TODO: the way this works, -- only forces argument
                        // parsing until a subcommand is found. This seems
                        // reasonable to me, but it may be unexpected that
                        // `command -a -- subcommand -b` parses b as an option
                        // flag. We could propagate the forced args flag to
                        // subcommands, but I'm not sure that would be better.
                        //
                        // Another option is to stop parsing altogether when --
                        // is hit, but that means that subcommands cannot be
                        // invoked at the same time as forced arguments, which
                        // seems worse somehow, as it affects macroscopic CLI
                        // behavior.
                        parseState = .ForcedArgs;
                        continue :argloop;
                    }

                    if (arg[1] == '-') {
                        // we have a long flag or option
                        specloop: inline for (spec) |param| {
                            switch (comptime param.brand()) {
                                .Option => {
                                    // have to force lower the handler to runtime
                                    var handler = param.handler;
                                    if (param.long) |flag| {
                                        if (std.mem.eql(u8, flag, arg)) {
                                            if (comptime param.required()) {
                                                @field(required, param.name) = true;
                                            }

                                            const val = argit.next() orelse return OptionError.MissingArgument;
                                            if (param.hideResult == false) {
                                                @field(result, param.name) = try handler(val);
                                            }
                                            continue :argloop;
                                        }
                                    }
                                },
                                .Flag => {
                                    inline for (.{ .{ param.truthy.long, true }, .{ param.falsy.long, false } }) |variant| {
                                        if (variant[0]) |flag| {
                                            if (std.mem.eql(u8, flag, arg)) {
                                                if (param.eager) |handler| {
                                                    try handler(data);
                                                }

                                                if (comptime param.required()) {
                                                    @field(required, param.name) = true;
                                                }

                                                if (param.hideResult == false) {
                                                    @field(result, param.name) = variant[1];
                                                }
                                                continue :argloop;
                                            }
                                        }
                                    }
                                },
                                .Argument, .Command => continue :specloop,
                            }
                        }

                        // nothing matched
                        return OptionError.UnknownOption;
                    } else {
                        // we have a short flag, which may be multiple fused flags
                        shortloop: for (arg[1..]) |shorty, idx| {
                            specloop: inline for (spec) |param| {
                                switch (comptime param.brand()) {
                                    .Option => {
                                        var handler = param.handler;
                                        if (param.short) |flag| {
                                            if (flag[1] == shorty) {
                                                if (comptime param.required()) {
                                                    @field(required, param.name) = true;
                                                }

                                                const val = if (arg.len > (idx + 2))
                                                    arg[(idx + 2)..]
                                                else
                                                    argit.next() orelse return OptionError.MissingArgument;

                                                if (param.hideResult == false) {
                                                    @field(result, param.name) = try handler(val);
                                                }
                                                continue :argloop;
                                            }
                                        }
                                    },
                                    .Flag => {
                                        inline for (.{ .{ param.truthy.short, true }, .{ param.falsy.short, false } }) |variant| {
                                            if (variant[0]) |flag| {
                                                if (flag[1] == shorty) {
                                                    if (param.eager) |handler| {
                                                        try handler(data);
                                                    }

                                                    if (comptime param.required()) {
                                                        @field(required, param.name) = true;
                                                    }

                                                    if (param.hideResult == false) {
                                                        @field(result, param.name) = variant[1];
                                                    }
                                                    continue :shortloop;
                                                }
                                            }
                                        }
                                    },
                                    .Argument, .Command => continue :specloop,
                                }
                            }
                            // nothing matched
                            return OptionError.UnknownOption;
                        }
                    }
                } else {
                    // we have a subcommand or an Argument. Arguments are parsed first, exclusively.
                    defer seenArgs += 1;
                    comptime var idx = 0;
                    inline for (spec) |param| {
                        switch (comptime param.brand()) {
                            .Argument => {
                                if (seenArgs == idx) {
                                    var handler = param.handler;
                                    @field(result, param.name) = try handler(arg);
                                    continue :argloop;
                                }
                                idx += 1;
                            },
                            .Command => {
                                if (seenArgs == argCount and std.mem.eql(u8, param.data.name, arg)) {
                                    // we're calling a subcommand
                                    try checkErrors(seenArgs, required);
                                    try callback(userdata, result);
                                    return param.execute(alloc, argit_type, argit, userdata);
                                }
                            },
                            else => continue,
                        }
                    }
                }
            }
            try checkErrors(seenArgs, required);
            try callback(userdata, result);
        }

        inline fn checkErrors(seenArgs: u32, required: RequiredType) OptionError!void {
            if (seenArgs < argCount) {
                return OptionError.MissingArgument;
            } else if (seenArgs > argCount) {
                return OptionError.ExtraArguments;
            }

            inline for (@typeInfo(@TypeOf(required)).Struct.fields) |field| {
                if (@field(required, field.name) == false) {
                    return OptionError.MissingOption;
                }
            }
        }

        fn attachSubcommands(alloc: std.mem.Allocator) !void {
            if (data.subcommands == null) {
                data.subcommands = std.ArrayList(*CommandData).init(alloc);
            }

            inline for (spec) |param| {
                switch (comptime param.brand()) {
                    .Command => {
                        try data.subcommands.append(&param);
                    },
                    else => continue,
                }
            }
        }

        fn scryTruthiness(alloc: std.mem.Allocator, input: []const u8) !bool {
            // empty string is falsy.
            if (input.len == 0) return false;

            if (input.len <= 5) {
                const comp = try std.ascii.allocLowerString(alloc, input);
                defer alloc.free(comp);

                inline for ([_][]const u8{ "false", "no", "0" }) |candidate| {
                    if (std.mem.eql(u8, comp, candidate)) {
                        return false;
                    }
                }
            }
            // TODO: actually try float conversion on input string? This seems
            // really silly to me, in the context of the shell, but for example
            // MY_VAR=0 evaluates to false but MY_VAR=0.0 evaluates to true. And
            // if we accept multiple representations of zero, a whole can of
            // worms gets opened. Should 0x0 be falsy? 0o0? That's a lot of
            // goofy edge cases.

            // any nonempty value is considered to be truthy.
            return true;
        }

        fn extractEnvVars(alloc: std.mem.Allocator, result: *ResultType, required: *RequiredType) !void {
            var env: std.process.EnvMap = try std.process.getEnvMap(alloc);
            defer env.deinit();

            inline for (spec) |param| {
                switch (comptime param.brand()) {
                    .Option => {
                        if (param.envVar) |want| {
                            if (env.get(want)) |value| {
                                if (comptime param.required()) {
                                    @field(required, param.name) = true;
                                }

                                var handler = param.handler;
                                @field(result, param.name) = try handler(value);
                            }
                        }
                    },
                    .Flag => {
                        if (param.envVar) |want| {
                            if (env.get(want)) |value| {
                                if (comptime param.required()) {
                                    @field(required, param.name) = true;
                                }

                                @field(result, param.name) = try scryTruthiness(alloc, value);
                            }
                        }
                    },
                    .Argument, .Command => continue,
                }
            }
        }

        inline fn createCommandresult() ResultType {
            var result: ResultType = undefined;
            inline for (spec) |param| {
                switch (comptime param.brand()) {
                    .Command => continue,
                    else => if (param.hideResult == false) {
                        @field(result, param.name) = switch (param.default) {
                            .none => continue,
                            .value => |val| val,
                        };
                    },
                }
            }
            return result;
        }
    };
}

pub fn CommandResult(comptime spec: anytype) type {
    comptime {
        // not sure how to do this without iterating twice, so let's iterate
        // twice
        var outsize = 0;
        for (spec) |param| {
            switch (param.brand()) {
                .Command => continue,
                else => if (param.hideResult == false) {
                    outsize += 1;
                },
            }
        }

        var fields: [outsize]StructField = undefined;

        var idx = 0;
        for (spec) |param| {
            switch (param.brand()) {
                .Command => continue,
                else => if (param.hideResult == true) continue,
            }

            const fieldType = param.ResultType();

            fields[idx] = .{
                .name = param.name,
                .field_type = fieldType,
                .default_value = switch (param.default) {
                    .none => null,
                    .value => |val| @ptrCast(?*const anyopaque, &val),
                },
                .is_comptime = false,
                .alignment = @alignOf(fieldType),
            };

            idx += 1;
        }

        return @Type(.{ .Struct = .{
            .layout = .Auto,
            .fields = &fields,
            .decls = &.{},
            .is_tuple = false,
        } });
    }
}

fn RequiredTracker(comptime spec: anytype) type {
    comptime {
        // not sure how to do this without iterating twice, so let's iterate
        // twice
        var outsize = 0;
        for (spec) |param| {
            switch (param.brand()) {
                .Argument, .Command => continue,
                else => {
                    if (param.required()) outsize += 1;
                },
            }
        }

        var fields: [outsize]StructField = undefined;

        var idx = 0;
        for (spec) |param| {
            switch (param.brand()) {
                .Argument, .Command => continue,
                else => if (param.required()) {
                    fields[idx] = .{
                        .name = param.name,
                        .field_type = bool,
                        .default_value = &false,
                        .is_comptime = false,
                        .alignment = @alignOf(bool),
                    };

                    idx += 1;
                },
            }
        }

        return @Type(.{ .Struct = .{
            .layout = .Auto,
            .fields = &fields,
            .decls = &.{},
            .is_tuple = false,
        } });
    }
}
