const std = @import("std");

const ncmeta = @import("./meta.zig");
const parser = @import("./parser.zig");
const FixedCount = @import("./parameters.zig").FixedCount;

// error.OutOfMemory

const AlignablePair = struct {
    left: []const u8,
    right: []const u8,
};

const OptionDescription = struct {
    pairs: []AlignablePair,
    just: usize,
};

pub fn HelpBuilder(comptime command: anytype) type {
    const help_info = opt_info(command.generate());

    return struct {
        writebuffer: std.ArrayList(u8),

        pub fn init(allocator: std.mem.Allocator) @This() {
            return @This(){
                .writebuffer = std.ArrayList(u8).init(allocator),
            };
        }

        pub fn build_message(
            self: *@This(),
            name: []const u8,
            subcommands: std.hash_map.StringHashMap(parser.ParserInterface),
        ) ![]const u8 {
            try self.describe_command(name, subcommands);

            return self.writebuffer.toOwnedSlice();
        }

        fn describe_command(
            self: *@This(),
            name: []const u8,
            subcommands: std.hash_map.StringHashMap(parser.ParserInterface),
        ) !void {
            const writer = self.writebuffer.writer();
            try writer.print(
                "Usage: {s}{s}{s}{s}\n\n",
                .{
                    name,
                    try self.option_brief(),
                    if (help_info.arguments.len > 0) " <arguments>" else "",
                    if (subcommands.count() > 0) " [<subcommand> ...]" else "",
                },
            );

            try writer.writeAll(std.mem.trim(u8, command.description, " \n"));
            try writer.writeAll("\n\n");

            const options = try self.describe_options();
            if (options.pairs.len > 0) {
                try writer.writeAll("Options:\n");
            }

            for (options.pairs) |pair| {
                try writer.print(
                    "  {[0]s: <[1]}{[2]s}\n",
                    .{ pair.left, options.just + 3, pair.right },
                );
            }
        }

        fn option_brief(self: @This()) ![]const u8 {
            if (comptime help_info.options.len > 1) {
                return " [options...]";
            } else if (comptime help_info.options.len == 1) {
                return " [option]";
            } else if (comptime help_info.options.len == 0) {
                return "";
            } else {
                var buffer = std.ArrayList(u8).init(self.writebuffer.allocator);
                const writer = buffer.writer();

                for (comptime help_info.options) |opt| {
                    try writer.writeAll(" [");

                    var written = false;
                    if (opt.short_truthy) |tag| {
                        try writer.writeAll(tag);
                        written = true;
                    }
                    if (opt.long_truthy) |tag| {
                        if (written) try writer.writeAll(" | ");
                        try writer.writeAll(tag);
                        written = true;
                    }
                    if (opt.short_falsy) |tag| {
                        if (written) try writer.writeAll(" | ");
                        try writer.writeAll(tag);
                        written = true;
                    }
                    if (opt.long_falsy) |tag| {
                        if (written) try writer.writeAll(" | ");
                        try writer.writeAll(tag);
                    }

                    try writer.writeAll("]");
                }

                return buffer.toOwnedSlice();
            }
        }

        fn describe_options(self: @This()) !OptionDescription {
            var pairs = std.ArrayList(AlignablePair).init(self.writebuffer.allocator);

            var just: usize = 0;
            for (comptime help_info.options) |opt| {
                const pair = try self.describe_option(opt);
                if (pair.left.len > just) just = pair.left.len;
                try pairs.append(pair);
            }

            return .{
                .pairs = try pairs.toOwnedSlice(),
                .just = just,
            };
        }

        fn describe_option(self: @This(), opt: OptHelp) !AlignablePair {
            var buffer = std.ArrayList(u8).init(self.writebuffer.allocator);
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

            if (comptime opt.description.len > 0) {
                try writer.writeAll(opt.description);
            }

            if (comptime opt.default) |def| {
                if (buffer.items.len > 0) try writer.writeAll(" ");
                try writer.print("(default: {s})", .{def});
            }

            if (comptime opt.required) {
                if (buffer.items.len > 0) try writer.writeAll(" ");
                try writer.writeAll("[required]");
            }
            const right = try buffer.toOwnedSlice();

            return .{ .left = left, .right = right };
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
    type_name: []const u8 = "",
    multi: bool = false,
    required: bool = true,
};

pub fn opt_info(comptime command: anytype) CommandHelp {
    comptime {
        var options: []const OptHelp = &[_]OptHelp{};
        var env_vars: []const EnvHelp = &[_]EnvHelp{};
        var arguments: []const ArgHelp = &[_]ArgHelp{};

        var last_name: []const u8 = "";
        var last_option: OptHelp = .{};

        paramloop: for (command) |param| {
            const PType = @TypeOf(param);
            if (PType.param_type != .Nominal) continue :paramloop;

            if (!std.mem.eql(u8, last_name, param.name)) {
                if (last_name.len > 0) {
                    if (last_option.short_truthy != null or
                        last_option.long_truthy != null or
                        last_option.short_falsy != null or
                        last_option.long_falsy != null)
                    {
                        options = options ++ &[_]OptHelp{last_option};
                    } else {
                        env_vars = env_vars ++ &[_]EnvHelp{.{
                            .env_var = last_option.env_var,
                            .description = last_option.description,
                            .default = last_option.default,
                        }};
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
                writer.print("{any}", .{def}) catch @compileError("whoah");
                last_option.default = buf.buffer;
            }
        }

        if (last_name.len > 0) {
            if (last_option.short_truthy != null or
                last_option.long_truthy != null or
                last_option.short_falsy != null or
                last_option.long_falsy != null)
            {
                options = options ++ &[_]OptHelp{last_option};
            } else {
                env_vars = env_vars ++ &[_]EnvHelp{.{
                    .env_var = last_option.env_var,
                    .description = last_option.description,
                    .default = last_option.default,
                }};
            }
        }

        return .{
            .options = options,
            .arguments = arguments,
            .env_vars = env_vars,
        };
    }
}
