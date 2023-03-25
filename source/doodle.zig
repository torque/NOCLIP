const std = @import("std");
const StructField = std.builtin.Type.StructField;

const converters = @import("./converters.zig");
const ncmeta = @import("./meta.zig");

const ConverterSignature = converters.ConverterSignature;

const ParameterType = enum {
    Nominal,
    Ordinal,
    Executable,
};

const Errors = error{
    BadConfiguration,
    MissingTag,
    ArgumentWithTags,
    ArgumentWithEnvVar,
    MissingDefaultConverter,
};

const ParseError = error{
    ValueMissing,
    FusedShortTagValueMissing,
    UnknownLongTagParameter,
    UnknownShortTagParameter,
};

const FlagBias = enum {
    falsy,
    truthy,
    unbiased,

    pub fn string(self: @This()) []const u8 {
        return switch (self) {
            .truthy => "true",
            else => @compileLog(self),
        };
    }
};

const OptionResult = union(enum) {
    Value: type,
    flag: FlagBias,
};

pub const ParameterGenerics = struct {
    ContextType: type = void,
    result: OptionResult = .{ .Value = []const u8 },
    param_type: ParameterType,

    pub fn no_context(comptime self: @This()) bool {
        return self.ContextType == void;
    }

    pub fn is_flag(comptime self: @This()) bool {
        return self.result == .flag;
    }

    pub fn clone(comptime self: @This(), comptime NewResult: type) @This() {
        return @This(){
            .ContextType = self.ContextType,
            .result = .{ .Value = NewResult },
        };
    }

    pub fn ResultType(comptime self: @This()) type {
        return switch (self.result) {
            .Value => |res| res,
            .flag => bool,
        };
    }
};

const ValuedGenericsBasis = struct { ContextType: type = void, Result: type };
const FlagGenericsBasis = struct { ContextType: type = void, flag_bias: FlagBias = .truthy };

fn tag_generics(comptime basis: ValuedGenericsBasis) ParameterGenerics {
    return ParameterGenerics{
        .ContextType = basis.ContextType,
        .result = .{ .Value = basis.Result },
        .param_type = .Nominal,
    };
}

fn flag_generics(comptime basis: FlagGenericsBasis) ParameterGenerics {
    return ParameterGenerics{
        .ContextType = basis.ContextType,
        .result = .{ .flag = basis.flag_bias },
        .param_type = .Nominal,
    };
}

fn arg_generics(comptime basis: ValuedGenericsBasis) ParameterGenerics {
    return ParameterGenerics{
        .ContextType = basis.ContextType,
        .result = .{ .Value = basis.Result },
        .param_type = .Ordinal,
    };
}

fn OptionConfig(comptime generics: ParameterGenerics) type {
    return struct {
        name: []const u8,

        short_tag: ?[]const u8 = null,
        long_tag: ?[]const u8 = null,
        env_var: ?[]const u8 = null,

        default: ?generics.ResultType() = null,
        converter: ?ConverterSignature(generics) = null,
        arg_count: u32 = if (generics.is_flag()) 0 else 1,
        eager: bool = false,
        required: bool = generics.param_type == .Ordinal,
        exposed: bool = true,
        secret: bool = false,
        nice_type_name: []const u8 = @typeName(generics.ResultType()),
    };
}

fn OptionType(comptime generics: ParameterGenerics) type {
    return struct {
        pub const gen = generics;
        pub const param_type: ParameterType = generics.param_type;
        pub const is_flag: bool = generics.is_flag();
        pub const flag_bias: FlagBias = if (generics.is_flag()) generics.result.flag else .unbiased;

        name: []const u8,
        short_tag: ?[]const u8,
        long_tag: ?[]const u8,
        env_var: ?[]const u8,

        default: ?generics.ResultType(),
        converter: ConverterSignature(generics),
        description: []const u8 = "", // description for output in help text
        arg_count: u32,
        eager: bool,
        required: bool,
        exposed: bool, // do not expose the resulting value in the output type. the handler must have side effects for this option to do anything
        secret: bool, // do not print help for this parameter
        nice_type_name: ?[]const u8 = null, // friendly type name (string better than []const u8)
    };
}

fn check_short(comptime short_tag: ?[]const u8) void {
    if (short_tag) |short| {
        if (short.len != 2 or short[0] != '-') @compileError("bad short tag");
    }
}

fn check_long(comptime long_tag: ?[]const u8) void {
    if (long_tag) |long| {
        if (long.len < 3 or long[0] != '-' or long[1] != '-') @compileError("bad long tag");
    }
}

fn make_option(comptime generics: ParameterGenerics, comptime opts: OptionConfig(generics)) OptionType(generics) {
    if (opts.short_tag == null and opts.long_tag == null and opts.env_var == null) {
        @compileError(
            "option " ++
                opts.name ++
                " must have at least one of a short tag, a long tag, or an environment variable",
        );
    }

    check_short(opts.short_tag);
    check_long(opts.long_tag);

    // perform the logic to create the default converter here? Could be done
    // when creating the OptionConfig instead. Need to do it here because there
    // may be an error. That's the essential distinction between the OptionType
    // and the OptionConfig, is the OptionConfig is just unvalidated parameters,
    // whereas the OptionType is an instance of an object that has been
    // validated.
    const converter = opts.converter orelse converters.default_converter(generics) orelse {
        @compileLog(opts);
        @compileError("implement me");
    };

    return OptionType(generics){
        .name = opts.name,
        .short_tag = opts.short_tag,
        .long_tag = opts.long_tag,
        .env_var = opts.env_var,
        .default = opts.default,
        .converter = converter,
        .arg_count = opts.arg_count,
        .eager = opts.eager,
        .required = opts.required,
        .exposed = opts.exposed,
        .secret = opts.secret,
    };
}

fn make_argument(comptime generics: ParameterGenerics, comptime opts: OptionConfig(generics)) OptionType(generics) {
    // TODO: it would technically be possible to support specification of
    // ordered arguments through environmental variables, but it doesn't really
    // make a lot of sense. The algorithm would consume the env var greedily
    if (opts.short_tag != null or opts.long_tag != null or opts.env_var != null) {
        @compileLog(opts);
        @compileError("argument " ++ opts.name ++ " must not have a long or short tag or an env var");
    }

    const converter = opts.converter orelse converters.default_converter(generics) orelse {
        @compileLog(opts);
        @compileError("implement me");
    };

    return OptionType(generics){
        .name = opts.name,
        .short_tag = opts.short_tag,
        .long_tag = opts.long_tag,
        .env_var = opts.env_var,
        .default = opts.default,
        .converter = converter,
        .arg_count = opts.arg_count,
        .eager = opts.eager,
        .required = opts.required,
        .exposed = opts.exposed,
        .secret = opts.secret,
    };
}

const ShortLongPair = struct {
    short_tag: ?[]const u8 = null,
    long_tag: ?[]const u8 = null,
};

fn FlagBuilderArgs(comptime ContextType: type) type {
    return struct {
        name: []const u8,
        truthy: ?ShortLongPair = null,
        falsy: ?ShortLongPair = null,
        env_var: ?[]const u8 = null,

        default: ?bool = null,
        converter: ?ConverterSignature(flag_generics(.{ .ContextType = ContextType })) = null,
        eager: bool = false,
        exposed: bool = true,
        required: bool = false,
        secret: bool = false,
    };
}

fn CommandBuilder(comptime ContextType: type) type {
    return struct {
        param_spec: ncmeta.MutableTuple = .{},

        pub const UserContextType = ContextType;

        pub fn add_argument(
            comptime self: *@This(),
            comptime Result: type,
            comptime args: OptionConfig(arg_generics(.{ .ContextType = ContextType, .Result = Result })),
        ) void {
            self.param_spec.add(make_argument(
                arg_generics(.{ .ContextType = ContextType, .Result = Result }),
                args,
            ));
        }

        pub fn add_option(
            comptime self: *@This(),
            comptime Result: type,
            comptime args: OptionConfig(tag_generics(.{ .ContextType = ContextType, .Result = Result })),
        ) void {
            self.param_spec.add(make_option(
                tag_generics(.{ .ContextType = ContextType, .Result = Result }),
                args,
            ));
        }

        pub fn add_flag(
            comptime self: *@This(),
            comptime build_args: FlagBuilderArgs(ContextType),
        ) void {
            if (build_args.truthy == null and build_args.falsy == null and build_args.env_var == null) {
                @compileError(
                    "flag " ++
                        build_args.name ++
                        " must have at least one of truthy flags, falsy flags, or env_var flags",
                );
            }

            if (build_args.truthy) |truthy_pair| {
                if (truthy_pair.short_tag == null and truthy_pair.long_tag == null) {
                    @compileError(
                        "flag " ++
                            build_args.name ++
                            " truthy pair must have at least short or long tags set",
                    );
                }

                const generics = flag_generics(.{ .ContextType = ContextType, .flag_bias = .truthy });

                const args = OptionConfig(generics){
                    .name = build_args.name,
                    .short_tag = truthy_pair.short_tag,
                    .long_tag = truthy_pair.long_tag,
                    .env_var = null,
                    .default = build_args.default,
                    .converter = build_args.converter,
                    .eager = build_args.eager,
                    .exposed = build_args.exposed,
                    .secret = build_args.secret,
                };

                self.param_spec.add(make_option(generics, args));
            }

            if (build_args.falsy) |falsy_pair| {
                if (falsy_pair.short_tag == null and falsy_pair.long_tag == null) {
                    @compileError(
                        "flag " ++
                            build_args.name ++
                            " falsy pair must have at least short or long tags set",
                    );
                }

                const generics = flag_generics(.{ .ContextType = ContextType, .flag_bias = .falsy });
                const args = OptionConfig(generics){
                    .name = build_args.name,
                    .short_tag = falsy_pair.short_tag,
                    .long_tag = falsy_pair.long_tag,
                    .env_var = null,
                    .default = build_args.default,
                    .converter = build_args.converter,
                    .eager = build_args.eager,
                    .secret = build_args.secret,
                };

                self.param_spec.add(make_option(generics, args));
            }

            if (build_args.env_var) |env_var| {
                const generics = flag_generics(.{ .ContextType = ContextType, .flag_bias = .unbiased });
                const args = OptionConfig(generics){
                    .name = build_args.name,
                    .env_var = env_var,
                    .default = build_args.default,
                    .converter = build_args.converter,
                    .eager = build_args.eager,
                    .secret = build_args.secret,
                };

                self.param_spec.add(make_option(generics, args));
            }
        }

        fn generate(comptime self: @This()) self.param_spec.TupleType() {
            return self.param_spec.realTuple();
        }

        pub fn CallbackSignature(comptime self: @This()) type {
            return *const fn (ContextType, self.CommandOutput()) anyerror!void;
        }

        pub fn CommandOutput(comptime self: @This()) type {
            comptime {
                const spec = self.generate();
                var fields: []const StructField = &[0]StructField{};
                var flag_skip = 0;

                paramloop: for (spec, 0..) |param, idx| {
                    if (!param.exposed) continue :paramloop;
                    while (flag_skip > 0) {
                        flag_skip -= 1;
                        continue :paramloop;
                    }

                    const PType = @TypeOf(param);
                    if (PType.is_flag) {
                        var peek = idx + 1;
                        var bais_seen: [ncmeta.enum_length(FlagBias)]bool = [_]bool{false} ** ncmeta.enum_length(FlagBias);
                        bais_seen[@enumToInt(PType.flag_bias)] = true;
                        while (peek < spec.len) : (peek += 1) {
                            const peek_param = spec[peek];
                            const PeekType = @TypeOf(peek_param);

                            if (PeekType.is_flag and std.mem.eql(u8, param.name, peek_param.name)) {
                                if (bais_seen[@enumToInt(PeekType.flag_bias)] == true) {
                                    @compileError("redundant flag!!!! " ++ param.name ++ " and " ++ peek_param.name);
                                } else {
                                    bais_seen[@enumToInt(PeekType.flag_bias)] = true;
                                }
                                flag_skip += 1;
                            } else {
                                break;
                            }
                        }
                    }

                    // the default field is already the optional type. Stripping
                    // the optional wrapper is an interesting idea for required
                    // fields. I do not foresee this greatly increasing complexity here.
                    const FieldType = if (param.required)
                        std.meta.Child(std.meta.FieldType(PType, .default))
                    else
                        std.meta.FieldType(PType, .default);

                    // the wacky comptime slice extension hack
                    fields = &(@as([fields.len]StructField, fields[0..fields.len].*) ++ [1]StructField{.{
                        .name = param.name,
                        .type = FieldType,
                        .default_value = @ptrCast(?*const anyopaque, &param.default),
                        .is_comptime = false,
                        .alignment = @alignOf(FieldType),
                    }});
                }

                return @Type(.{ .Struct = .{
                    .layout = .Auto,
                    .fields = fields,
                    .decls = &.{},
                    .is_tuple = false,
                } });
            }
        }

        pub fn Intermediate(comptime self: @This()) type {
            comptime {
                const spec = self.generate();
                var fields: []const StructField = &[0]StructField{};
                var flag_skip = 0;

                paramloop: for (spec, 0..) |param, idx| {
                    while (flag_skip > 0) {
                        flag_skip -= 1;
                        continue :paramloop;
                    }

                    const PType = @TypeOf(param);
                    if (PType.is_flag) {
                        var peek = idx + 1;
                        var bais_seen: [ncmeta.enum_length(FlagBias)]bool = [_]bool{false} ** ncmeta.enum_length(FlagBias);
                        bais_seen[@enumToInt(PType.flag_bias)] = true;
                        while (peek < spec.len) : (peek += 1) {
                            const peek_param = spec[peek];
                            const PeekType = @TypeOf(peek_param);

                            if (PeekType.is_flag and std.mem.eql(u8, param.name, peek_param.name)) {
                                if (bais_seen[@enumToInt(PeekType.flag_bias)] == true) {
                                    @compileError("redundant flag!!!! " ++ param.name ++ " and " ++ peek_param.name);
                                } else {
                                    bais_seen[@enumToInt(PeekType.flag_bias)] = true;
                                }
                                flag_skip += 1;
                            } else {
                                break;
                            }
                        }
                    }

                    // the wacky comptime slice extension hack
                    fields = &(@as([fields.len]StructField, fields[0..fields.len].*) ++ [1]StructField{.{
                        .name = param.name,
                        .type = ?[]const u8,
                        .default_value = @ptrCast(?*const anyopaque, &@as(?[]const u8, null)),
                        .is_comptime = false,
                        .alignment = @alignOf(?[]const u8),
                    }});
                }

                return @Type(.{ .Struct = .{
                    .layout = .Auto,
                    .fields = fields,
                    .decls = &.{},
                    .is_tuple = false,
                } });
            }
        }

        pub fn bind(comptime self: @This(), comptime callback: self.CallbackSignature()) Parser(self, callback) {
            return Parser(self, callback){};
        }
    };
}

// the parser is generated by the bind method of the CommandBuilder, so we can
// be extremely type-sloppy here, which simplifies the signature.
fn Parser(comptime command: anytype, comptime callback: anytype) type {
    _ = callback;

    return struct {
        const ContextType = @TypeOf(command).UserContextType;
        // let there be fields! we can move some things to runtime.
        // We can get some better behavior if we defer converting non-eager
        // options until the entire command line has been parsed. However,
        // to do that, we effectively have to store the parameters as strings until the
        // entire line has been parsed.

        // a goal is to

        intermediate: command.Intermediate() = .{},
        consumed_args: u32 = 0,

        // pub fn add_subcommand(self: *@This(), verb: []const u8, parser: anytype) void {
        //     self.subcommands
        // }

        pub fn parse(
            self: *@This(),
            alloc: std.mem.Allocator,
            argit: *std.process.ArgIterator,
            env: std.process.EnvMap,
            context: ContextType,
        ) anyerror!void {
            _ = alloc;
            // _ = context;

            try self.read_environment(env);

            var forced_args = false;
            argloop: while (argit.next()) |arg| {
                if (!forced_args and std.mem.eql(u8, arg, "--")) {
                    forced_args = true;
                    continue :argloop;
                }

                parse_tags: {
                    if (forced_args or arg.len < 1 or arg[0] != '-') break :parse_tags;

                    if (arg.len > 2 and arg[1] == '-') {
                        try self.parse_long_tag(arg, argit, context);
                        continue :argloop;
                    } else if (arg.len > 1) {
                        for (arg[1..], 1..) |short, idx| {
                            // _ = short;
                            // _ = idx;
                            try self.parse_short_tag(short, arg.len - idx - 1, argit, context);
                        }
                        continue :argloop;
                    }
                }

                try self.parse_argument(arg, argit);
            }
        }

        inline fn parse_long_tag(
            self: *@This(),
            arg: []const u8,
            argit: *std.process.ArgIterator,
            context: ContextType,
        ) ParseError!void {
            _ = context;

            inline for (comptime command.generate()) |param| {
                const PType = @TypeOf(param);
                // removing the comptime here causes the compiler to die
                comptime if (PType.param_type != .Nominal or param.long_tag == null) continue;
                const tag = param.long_tag.?;

                if (comptime PType.is_flag) {
                    if (std.mem.eql(u8, arg, tag)) {
                        @field(self.intermediate, param.name) = if (comptime PType.flag_bias == .truthy) "true" else "false";
                        return;
                    }
                } else {
                    if (std.mem.startsWith(u8, arg, tag)) match: {
                        // TODO: handle more than one value
                        const next = if (arg.len == tag.len)
                            argit.next() orelse return ParseError.ValueMissing
                        else if (arg[tag.len] == '=')
                            arg[tag.len + 1 ..]
                        else
                            break :match;

                        @field(self.intermediate, param.name) = next;
                        // if (comptime param.eager) {
                        //     try param.converter()
                        // }
                        return;
                    }
                }
            }

            return ParseError.UnknownLongTagParameter;
        }

        inline fn parse_short_tag(
            self: *@This(),
            arg: u8,
            remaining: usize,
            argit: *std.process.ArgIterator,
            context: ContextType,
        ) ParseError!void {
            _ = context;

            inline for (comptime command.generate()) |param| {
                const PType = @TypeOf(param);
                // removing the comptime here causes the compiler to die
                comptime if (PType.param_type != .Nominal or param.short_tag == null) continue;
                const tag = param.short_tag.?;

                if (comptime PType.is_flag) {
                    if (arg == tag[1]) {
                        @field(self.intermediate, param.name) = if (comptime PType.flag_bias == .truthy) "true" else "false";
                        return;
                    }
                } else {
                    if (arg == tag[1]) {
                        if (remaining > 0) return ParseError.FusedShortTagValueMissing;
                        const next = argit.next() orelse return ParseError.ValueMissing;

                        @field(self.intermediate, param.name) = next;
                        return;
                    }
                }
            }

            return ParseError.UnknownShortTagParameter;
        }

        inline fn parse_argument(self: *@This(), arg: []const u8, argit: *std.process.ArgIterator) ParseError!void {
            _ = argit;

            comptime var arg_index: u32 = 0;
            inline for (comptime command.generate()) |param| {
                if (@TypeOf(param).param_type != .Ordinal) continue;

                if (self.consumed_args == arg_index) {
                    std.debug.print("n: {s}, c: {d}, i: {d}\n", .{ param.name, self.consumed_args, arg_index });
                    @field(self.intermediate, param.name) = arg;
                    self.consumed_args += 1;
                    return;
                }
                arg_index += 1;
            }
        }

        fn read_environment(self: *@This(), env: std.process.EnvMap) !void {
            inline for (comptime command.generate()) |param| {
                if (comptime param.env_var) |env_var| {
                    @field(self.intermediate, param.name) = env.get(env_var);
                }
            }
        }
    };
}

fn HelpBuilder(comptime command: anytype) type {
    _ = command;
}

pub fn command_builder(comptime ContextType: type) CommandBuilder(ContextType) {
    return CommandBuilder(ContextType){};
}

const Choice = enum { first, second };

const cli = cmd: {
    var cmd = command_builder(void);
    cmd.add_option(u8, .{
        .name = "test",
        .short_tag = "-t",
        .long_tag = "--test",
        .env_var = "NOCLIP_TEST",
    });
    cmd.add_flag(.{
        .name = "flag",
        .truthy = .{ .short_tag = "-f", .long_tag = "--flag" },
        .falsy = .{ .long_tag = "--no-flag" },
        .env_var = "NOCLIP_FLAG",
    });
    cmd.add_argument([]const u8, .{ .name = "arg" });
    cmd.add_argument([]const u8, .{ .name = "argtwo" });

    break :cmd cmd;
};

fn cli_handler(_: void, result: cli.CommandOutput()) !void {
    _ = result;
}

pub fn main() !void {
    // std.debug.print("hello\n", .{});
    var parser = cli.bind(cli_handler);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    var argit = try std.process.argsWithAllocator(allocator);
    var env = try std.process.getEnvMap(allocator);

    _ = argit.next();
    try parser.parse(allocator, &argit, env, {});

    inline for (@typeInfo(@TypeOf(parser.intermediate)).Struct.fields) |field| {
        std.debug.print("{s}: {?s}\n", .{ field.name, @field(parser.intermediate, field.name) });
    }
}
