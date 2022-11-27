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
pub const meta = @import("./meta.zig");
pub const params = @import("./params.zig");
pub const Command = @import("./bakery.zig").Command;

pub const OptionError = error{
    BadShortOption,
    BadLongOption,
    UnknownOption,
    MissingOption,
    MissingArgument,
    ExtraArguments,
};

/// spec is a tuple of Option, Flag, and Argument
pub fn CommandParser(
    comptime commandData: params.CommandData,
    comptime spec: anytype,
    comptime UserContext: type,
    comptime callback: *const fn (UserContext, CommandResult(spec, UserContext)) anyerror!void,
) type {
    comptime var argCount = 0;
    comptime for (spec) |param| {
        switch (@TypeOf(param).brand) {
            .Argument => argCount += 1,
            .Option, .Flag, .Command => continue,
        }
    };

    const ResultType = CommandResult(spec, UserContext);
    const RequiredType = RequiredTracker(spec);

    const ParseState = enum { Mixed, ForcedArgs };

    return struct {
        pub const brand: params.Brand = .Command;
        pub const ContextType = UserContext;
        // this should be copied at compile time
        var data: params.CommandData = commandData;

        /// parse command line arguments from an iterator
        pub fn execute(self: @This(), alloc: std.mem.Allocator, comptime argit_type: type, argit: *argit_type, context: UserContext) !void {
            try self.attachSubcommands(alloc);

            var result: ResultType = createCommandresult();
            var required: RequiredType = .{};
            var parseState: ParseState = .Mixed;

            try extractEnvVars(alloc, &result, &required, context);

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
                            switch (@TypeOf(param).brand) {
                                .Option => {
                                    // have to force lower the handler to runtime
                                    // var handler = param.handler.?;
                                    if (param.long) |flag| {
                                        if (std.mem.eql(u8, flag, arg)) {
                                            if (comptime param.required()) {
                                                @field(required, param.name) = true;
                                            }

                                            const val = argit.next() orelse return OptionError.MissingArgument;
                                            if (param.hideResult == false) {
                                                @field(result, param.name) = try param.handler.?(context, val);
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
                                                    try handler(context, data);
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
                                switch (@TypeOf(param).brand) {
                                    .Option => {
                                        // var handler = param.handler.?;
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
                                                    @field(result, param.name) = try param.handler.?(context, val);
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
                                                        try handler(context, data);
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
                        switch (@TypeOf(param).brand) {
                            .Command => {
                                if (std.mem.eql(u8, @TypeOf(param).data.name, arg)) {
                                    // we're calling a subcommand
                                    try checkErrors(seenArgs, required);
                                    try callback(context, result);
                                    return param.execute(alloc, argit_type, argit, context);
                                }
                            },
                            .Argument => {
                                if (seenArgs == idx) {
                                    if (comptime param.required()) {
                                        @field(required, param.name) = true;
                                    }
                                    // var handler = param.handler;
                                    @field(result, param.name) = try param.handler.?(context, arg);
                                    continue :argloop;
                                }
                                idx += 1;
                            },
                            else => continue,
                        }
                    }
                }
            }
            try checkErrors(seenArgs, required);
            try callback(context, result);
        }

        pub fn OutType() type {
            return CommandResult(spec, UserContext);
        }

        inline fn checkErrors(seenArgs: u32, required: RequiredType) OptionError!void {
            if (seenArgs < argCount) {
                return OptionError.MissingArgument;
            } else if (seenArgs > argCount) {
                return OptionError.ExtraArguments;
            }

            describeError(required);

            inline for (@typeInfo(@TypeOf(required)).Struct.fields) |field| {
                if (@field(required, field.name) == false) {
                    return OptionError.MissingOption;
                }
            }
        }

        pub fn describeError(required: RequiredType) void {
            inline for (@typeInfo(@TypeOf(required)).Struct.fields) |field| {
                if (@field(required, field.name) == false) {
                    std.debug.print("missing {s}\n", .{field.name});
                }
            }
        }

        fn attachSubcommands(_: @This(), alloc: std.mem.Allocator) !void {
            if (data.subcommands == null) {
                data.subcommands = std.ArrayList(*params.CommandData).init(alloc);
            }

            inline for (spec) |param| {
                switch (@TypeOf(param).brand) {
                    .Command => {
                        try data.subcommands.?.append(&@TypeOf(param).data);
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

        fn extractEnvVars(
            alloc: std.mem.Allocator,
            result: *ResultType,
            required: *RequiredType,
            context: UserContext,
        ) !void {
            var env: std.process.EnvMap = try std.process.getEnvMap(alloc);
            defer env.deinit();

            inline for (spec) |param| {
                const ParamType = @TypeOf(param);
                switch (ParamType.brand) {
                    .Option => {
                        if (param.envVar) |want| {
                            if (env.get(want)) |value| {
                                if (param.required()) {
                                    @field(required, param.name) = true;
                                }

                                @field(result, param.name) = try param.handler.?(context, value);
                            }
                        }
                    },
                    .Flag => {
                        if (param.envVar) |want| {
                            if (env.get(want)) |value| {
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
                switch (@TypeOf(param).brand) {
                    .Command => continue,
                    else => if (param.hideResult == false) {
                        @field(result, param.name) = param.default orelse continue;
                    },
                }
            }
            return result;
        }
    };
}

pub fn CommandResult(comptime spec: anytype, comptime UserContext: type) type {
    comptime {
        // not sure how to do this without iterating twice, so let's iterate
        // twice
        var outsize = 0;
        for (spec) |param| {
            const ParamType = @TypeOf(param);
            if (ParamType.ContextType != UserContext) {
                @compileError("param \"" ++ param.name ++ "\" has wrong context type (wanted: " ++ @typeName(UserContext) ++ ", got: " ++ @typeName(ParamType.ContextType) ++ ")");
            }
            switch (ParamType.brand) {
                .Argument, .Option => {
                    if (param.handler == null) {
                        @compileError("param \"" ++ param.name ++ "\" does not have a handler");
                    }
                },
                else => {},
            }

            switch (ParamType.brand) {
                .Command => continue,
                else => {
                    if (param.hideResult == false) {
                        outsize += 1;
                    }
                },
            }
        }

        var fields: [outsize]StructField = undefined;

        var idx = 0;
        for (spec) |param| {
            const ParamType = @TypeOf(param);
            switch (ParamType.brand) {
                .Command => continue,
                else => if (param.hideResult == true) continue,
            }

            const FieldType = ParamType.ResultType;

            fields[idx] = .{
                .name = param.name,
                .field_type = FieldType,
                .default_value = @ptrCast(?*const anyopaque, &param.default),
                .is_comptime = false,
                .alignment = @alignOf(FieldType),
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
            const ParamType = @TypeOf(param);
            switch (ParamType.brand) {
                // flags are always optional, and commands don't map into the
                // output type.
                .Flag, .Command => continue,
                .Argument, .Option => if (param.required()) {
                    // if mayBeOptional is false, then the argument/option is
                    // required. Otherwise, we have to check if a default has
                    // been provided.
                    outsize += 1;
                },
            }
        }

        var fields: [outsize]StructField = undefined;

        var idx = 0;
        for (spec) |param| {
            const ParamType = @TypeOf(param);
            switch (ParamType.brand) {
                .Flag, .Command => continue,
                .Argument, .Option => if (param.required()) {
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

test {
    _ = meta;
}
