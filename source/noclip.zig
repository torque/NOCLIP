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
    const param_count: struct {
        opts: comptime_int,
        args: comptime_int,
        subs: comptime_int,
    } = comptime comp: {
        var optc = 0;
        var argc = 0;
        var subc = 0;
        for (spec) |param| {
            switch (@TypeOf(param).brand) {
                .Argument => argc += 1,
                .Option, .Flag => optc += 1,
                .Command => subc += 1,
            }
        }
        break :comp .{ .opts = optc, .args = argc, .subs = subc };
    };

    const ResultType = CommandResult(spec, UserContext);
    const RequiredType = RequiredTracker(spec);

    const ParseState = enum { Mixed, ForcedArgs };

    return struct {
        pub const brand: params.Brand = .Command;
        pub const ContextType = UserContext;
        // this should be copied at compile time
        var data: params.CommandData = commandData;

        pub fn execute(self: @This(), alloc: std.mem.Allocator, comptime argit_type: type, argit: *argit_type, context: UserContext) !void {
            return try self.internal_execute(alloc, argit_type, argit, context, null);
        }

        fn internal_execute(self: @This(), alloc: std.mem.Allocator, comptime argit_type: type, argit: *argit_type, context: UserContext, prog: ?[]const u8) !void {
            try self.attachSubcommands(alloc);

            var result: ResultType = createCommandresult();
            var required: RequiredType = .{};
            var parseState: ParseState = .Mixed;

            try extractEnvVars(alloc, &result, &required, context);

            // TODO: this does not even slightly work with subcommands
            const progName = prog orelse std.fs.path.basename(argit.next() orelse @panic("base, name?"));

            // TODO: only do this if the help flag has been passed. Alternatively, try
            // to assemble this at comptime?
            var helpDescription: params.CommandData = .{ .name = data.name };
            try buildHelpDescription(progName, &helpDescription, alloc);
            defer alloc.free(helpDescription.help);

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
                                    if (param.long) |flag| {
                                        if (std.mem.startsWith(u8, arg, flag) and (flag.len == arg.len or arg[flag.len] == '=')) {
                                            const val = if (flag.len == arg.len)
                                                argit.next() orelse return OptionError.MissingArgument
                                            else
                                                arg[flag.len + 1 ..];

                                            if (comptime param.required()) {
                                                @field(required, param.name) = true;
                                            }

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
                                                    try handler(context, helpDescription);
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
                        shortloop: for (arg[1..], 0..) |shorty, idx| {
                            specloop: inline for (spec) |param| {
                                switch (@TypeOf(param).brand) {
                                    .Option => {
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
                                                        try handler(context, helpDescription);
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
                                const name = @TypeOf(param).data.name;
                                if (std.mem.eql(u8, name, arg)) {
                                    // we're calling a subcommand
                                    try checkErrors(seenArgs, required);
                                    try callback(context, result);

                                    const combined = try std.mem.join(alloc, " ", &[_][]const u8{ progName, name });
                                    defer alloc.free(combined);
                                    return param.internal_execute(alloc, argit_type, argit, context, combined);
                                }
                            },
                            .Argument => {
                                if (seenArgs == idx) {
                                    if (comptime param.required()) {
                                        @field(required, param.name) = true;
                                    }
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

        fn buildHelpDescription(
            progName: []const u8,
            inData: *params.CommandData,
            alloc: std.mem.Allocator,
        ) !void {
            var seen: u32 = 0;
            var maxlen: usize = 0;

            var argnames: [param_count.args][]const u8 = undefined;
            var args: [param_count.args]ParamRow = undefined;
            inline for (spec) |param| {
                switch (@TypeOf(param).brand) {
                    .Argument => {
                        argnames[seen] = param.name;
                        args[seen] = try describeArgument(param, alloc);
                        maxlen = @max(args[seen].flags.len, maxlen);
                        seen += 1;
                    },
                    else => continue,
                }
            }

            seen = 0;
            var rows: [param_count.opts]ParamRow = undefined;
            inline for (spec) |param| {
                const describer = switch (@TypeOf(param).brand) {
                    .Option => describeOption,
                    .Flag => describeFlag,
                    else => continue,
                };
                rows[seen] = try describer(param, alloc);
                maxlen = @max(rows[seen].flags.len, maxlen);
                seen += 1;
            }

            seen = 0;
            var subs: [param_count.subs]ParamRow = undefined;
            inline for (spec) |param| {
                switch (@TypeOf(param).brand) {
                    .Command => {
                        subs[seen] = try describeSubcommand(param, alloc);
                        maxlen = @max(subs[seen].flags.len, maxlen);
                        seen += 1;
                    },
                    else => continue,
                }
            }

            var buffer = std.ArrayList(u8).init(alloc);
            const writer = buffer.writer();

            for (argnames) |name| {
                try std.fmt.format(writer, " <{s}>", .{name});
            }

            const short_args = try buffer.toOwnedSlice();
            defer alloc.free(short_args);

            try std.fmt.format(
                writer,
                "Usage: {s}{s}{s}{s}\n\n",
                .{
                    progName,
                    if (param_count.opts > 0) " [options]" else "",
                    if (param_count.args > 0) short_args else "",
                    if (param_count.subs > 0) " [subcommand] ..." else "",
                },
            );

            try writer.writeAll(data.help);

            if (param_count.args > 0) {
                try writer.writeAll("\n\nArguments:\n");

                for (args) |arg| {
                    defer arg.deinit(alloc);
                    try std.fmt.format(
                        writer,
                        "  {[0]s: <[1]}{[2]s}\n",
                        .{ arg.flags, maxlen + 2, arg.description },
                    );
                }
            }

            if (param_count.opts > 0) {
                try writer.writeAll("\nOptions:\n");

                for (rows) |row| {
                    defer row.deinit(alloc);
                    try std.fmt.format(
                        writer,
                        "  {[0]s: <[1]}{[2]s}\n",
                        .{ row.flags, maxlen + 2, row.description },
                    );
                }
            }

            if (param_count.subs > 0) {
                try writer.writeAll("\nSubcommands:\n");
                // try std.fmt.format(writer, "\nSubcommands {d}:\n", .{param_count.subs});
                for (subs) |sub| {
                    defer sub.deinit(alloc);
                    try std.fmt.format(
                        writer,
                        "  {[0]s: <[1]}{[2]s}\n",
                        .{ sub.flags, maxlen + 2, sub.description },
                    );
                }
            }

            inData.help = try buffer.toOwnedSlice();
        }

        const ParamRow = struct {
            flags: []const u8,
            description: []const u8,

            pub fn deinit(self: @This(), alloc: std.mem.Allocator) void {
                alloc.free(self.flags);
                alloc.free(self.description);
            }
        };

        fn describeArgument(comptime param: anytype, alloc: std.mem.Allocator) !ParamRow {
            var buffer = std.ArrayList(u8).init(alloc);
            const writer = buffer.writer();

            try writer.writeAll(param.name);
            try std.fmt.format(writer, " ({s})", .{param.type_name()});

            const flags = try buffer.toOwnedSlice();

            if (param.help) |help| {
                try writer.writeAll(help);
            }
            if (param.required()) {
                try writer.writeAll(" [required]");
            }
            const description = try buffer.toOwnedSlice();

            return ParamRow{ .flags = flags, .description = description };
        }

        fn describeOption(comptime param: anytype, alloc: std.mem.Allocator) !ParamRow {
            var buffer = std.ArrayList(u8).init(alloc);
            const writer = buffer.writer();

            if (param.envVar) |varName| {
                try std.fmt.format(writer, "{s}", .{varName});
            }
            if (param.short) |short| {
                if (buffer.items.len > 0) {
                    try writer.writeAll(", ");
                }
                try writer.writeAll(short);
            }
            if (param.long) |long| {
                if (buffer.items.len > 0) {
                    try writer.writeAll(", ");
                }
                try writer.writeAll(long);
            }
            try std.fmt.format(writer, " ({s})", .{param.type_name()});

            const flags = try buffer.toOwnedSlice();

            if (param.help) |help| {
                try writer.writeAll(help);
            }
            if (param.required()) {
                try writer.writeAll(" [required]");
            }
            const description = try buffer.toOwnedSlice();

            return ParamRow{ .flags = flags, .description = description };
        }

        fn describeFlag(comptime param: anytype, alloc: std.mem.Allocator) !ParamRow {
            var buffer = std.ArrayList(u8).init(alloc);
            const writer = buffer.writer();

            var truthy_seen: bool = false;
            var falsy_seen: bool = false;

            if (param.truthy.short) |short| {
                try writer.writeAll(short);
                truthy_seen = true;
            }
            if (param.truthy.long) |long| {
                if (truthy_seen) {
                    try writer.writeAll(", ");
                }
                try writer.writeAll(long);
                truthy_seen = true;
            }

            if (param.falsy.short) |short| {
                if (truthy_seen) {
                    try writer.writeAll("/");
                }
                try writer.writeAll(short);
                falsy_seen = true;
            }
            if (param.falsy.long) |long| {
                if (falsy_seen) {
                    try writer.writeAll(", ");
                } else if (truthy_seen) {
                    try writer.writeAll("/");
                }
                try writer.writeAll(long);
                falsy_seen = true;
            }

            if (param.envVar) |varName| {
                try std.fmt.format(writer, " ({s})", .{varName});
            }

            const flags = try buffer.toOwnedSlice();

            if (param.help) |help| {
                try writer.writeAll(help);
            }
            if (param.required()) {
                try writer.writeAll(" [required]");
            }
            const description = try buffer.toOwnedSlice();

            return ParamRow{ .flags = flags, .description = description };
        }

        fn describeSubcommand(comptime param: anytype, alloc: std.mem.Allocator) !ParamRow {
            var buffer = std.ArrayList(u8).init(alloc);
            const writer = buffer.writer();

            const paramdata = @TypeOf(param).data;

            try writer.writeAll(paramdata.name);

            const flags = try buffer.toOwnedSlice();

            try writer.writeAll(paramdata.help);
            const description = try buffer.toOwnedSlice();

            return ParamRow{ .flags = flags, .description = description };
        }

        pub fn OutType() type {
            return CommandResult(spec, UserContext);
        }

        inline fn checkErrors(seenArgs: u32, required: RequiredType) OptionError!void {
            if (seenArgs < param_count.args) {
                return OptionError.MissingArgument;
            } else if (seenArgs > param_count.args) {
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

        fn scryTruthiness(input: []const u8) bool {
            // empty string is falsy.
            if (input.len == 0) return false;

            if (input.len <= 5) {
                var lowerBuf: [5]u8 = undefined;
                const comp = std.ascii.lowerString(&lowerBuf, input);

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
                                if (comptime param.required()) {
                                    @field(required, param.name) = true;
                                }

                                @field(result, param.name) = try param.handler.?(context, value);
                            }
                        }
                    },
                    .Flag => {
                        if (param.envVar) |want| {
                            if (env.get(want)) |value| {
                                @field(result, param.name) = scryTruthiness(value);
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
                .type = FieldType,
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
                        .type = bool,
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
