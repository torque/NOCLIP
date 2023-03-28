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
    UnexpectedFailure,
    EmptyArgs,
    ValueMissing,
    UnexpectedValue,
    FusedShortTagValueMissing,
    UnknownLongTagParameter,
    UnknownShortTagParameter,
};

// in theory, we could also have a flexible value count, which could be followed by
// any number of fixed args and be well-defined. `mv` is a classic example
// of this pattern. But putting that logic in the parser seems to add a lot of
// complexity for little gain. The `mv` use case can be much more easily handled
// with a greedy value and then splitting in the value handler.
const ValueCount = union(enum) {
    fixed: u32,
    greedy: void,
};

const FlagBias = enum {
    falsy,
    truthy,
    unbiased,

    pub fn string(comptime self: @This()) []const u8 {
        return switch (self) {
            .truthy => "true",
            .falsy => "false",
            else => @compileError("flag tag with unbiased bias?"),
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

        value_count: ValueCount = if (generics.is_flag()) .{ .fixed = 0 } else .{ .fixed = 1 },
        default: ?generics.ResultType() = null,
        converter: ?ConverterSignature(generics) = null,
        description: []const u8 = "", // description for output in help text

        eager: bool = false,
        required: bool = generics.param_type == .Ordinal,
        global: bool = false,
        multi: bool = false,
        exposed: bool = true,
        secret: bool = false,
        nice_type_name: []const u8 = @typeName(generics.ResultType()),
    };
}

fn OptionType(comptime generics: ParameterGenerics) type {
    return struct {
        pub const param_type: ParameterType = generics.param_type;
        pub const is_flag: bool = generics.is_flag();
        pub const flag_bias: FlagBias = if (is_flag) generics.result.flag else .unbiased;

        name: []const u8,
        short_tag: ?[]const u8,
        long_tag: ?[]const u8,
        env_var: ?[]const u8,

        /// description for output in help text
        description: []const u8,
        default: ?generics.ResultType(),
        converter: ConverterSignature(generics),

        /// number of values this option wants to consume
        value_count: ValueCount,

        /// the option converter will be run eagerly, before full command line
        /// validation.
        eager: bool,

        /// the option cannot be omitted from the command line.
        required: bool,

        /// this option is parsed in a pre-parsing pass that consumes it. It
        /// may be present anywhere on the command line. A different way to
        /// solve this problem is by using an environment variable. It must be
        /// a tagged option.
        global: bool,
        /// allow this named parameter to be passed multiple times.
        /// values will be appended when it is encountered. If false, only the
        /// final encountered instance will be used.
        multi: bool,
        /// if false, do not expose the resulting value in the output type.
        /// the converter must have side effects for this option to do anything.
        exposed: bool,
        /// do not print help for this parameter
        secret: bool,

        nice_type_name: []const u8, // friendly type name (string better than []const u8)

        pub fn ResultType(comptime self: @This()) type {
            // is this the correct way to collapse this?
            return comptime if (self.multi)
                std.ArrayList(self._RType())
            else
                self._RType();
        }

        inline fn _RType(comptime self: @This()) type {
            comptime switch (self.value_count) {
                .fixed => |count| {
                    return switch (count) {
                        0, 1 => generics.ResultType(),
                        // TODO: use an ArrayList instead? it generalizes a bit better
                        // (i.e. can use the same codepath for multi-fixed and greedy)
                        else => [count]generics.ResultType(),
                    };
                },
                .greedy => return std.ArrayList(generics.ResultType()),
            };
        }
    };
}

fn check_short(comptime short_tag: ?[]const u8) void {
    if (short_tag) |short| {
        if (short.len != 2 or short[0] != '-') @compileError("bad short tag" ++ short);
    }
}

fn check_long(comptime long_tag: ?[]const u8) void {
    if (long_tag) |long| {
        if (long.len < 3 or long[0] != '-' or long[1] != '-') @compileError("bad long tag" ++ long);
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
        .description = opts.description,
        .default = opts.default,
        .converter = converter,
        .value_count = opts.value_count,
        .eager = opts.eager,
        .required = opts.required,
        .multi = opts.multi,
        .exposed = opts.exposed,
        .global = opts.global,
        .secret = opts.secret,
        .nice_type_name = opts.nice_type_name,
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

    if (opts.multi == true) @compileError("argument " ++ opts.name ++ " cannot be multi");

    return OptionType(generics){
        .name = opts.name,
        .short_tag = opts.short_tag,
        .long_tag = opts.long_tag,
        .env_var = opts.env_var,
        .description = opts.description,
        .default = opts.default,
        .converter = converter,
        .value_count = opts.value_count,
        .eager = opts.eager,
        .required = opts.required,
        .multi = opts.multi,
        .global = opts.global,
        .exposed = opts.exposed,
        .secret = opts.secret,
        .nice_type_name = opts.nice_type_name,
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
        description: []const u8 = "",

        default: ?bool = null,
        converter: ?ConverterSignature(flag_generics(.{ .ContextType = ContextType })) = null,
        eager: bool = false,
        required: bool = false,
        global: bool = false,
        multi: bool = false,
        exposed: bool = true,
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
            const generics = tag_generics(.{ .ContextType = ContextType, .Result = Result });
            if (comptime args.value_count == .fixed and args.value_count.fixed == 0) {
                @compileError(
                    "please use add_flag rather than add_option to " ++
                        "create a 0-argument option",
                );
            }

            self.param_spec.add(make_option(generics, args));
        }

        pub fn set_help_flag(
            comptime self: *@This(),
            comptime args: OptionConfig(flag_generics(.{ .ContextType = ContextType, .flag_bias = .truthy })),
        ) void {
            _ = self;
            _ = args;
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
                    .description = build_args.description,
                    .value_count = .{ .fixed = 0 },
                    .default = build_args.default,
                    .converter = build_args.converter,
                    .eager = build_args.eager,
                    .required = build_args.required,
                    .global = build_args.global,
                    .multi = build_args.multi,
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
                    .description = build_args.description,
                    .value_count = .{ .fixed = 0 },
                    .default = build_args.default,
                    .converter = build_args.converter,
                    .eager = build_args.eager,
                    .required = build_args.required,
                    .global = build_args.global,
                    .multi = build_args.multi,
                    .exposed = build_args.exposed,
                    .secret = build_args.secret,
                };

                self.param_spec.add(make_option(generics, args));
            }

            if (build_args.env_var) |env_var| {
                const generics = flag_generics(.{ .ContextType = ContextType, .flag_bias = .unbiased });
                const args = OptionConfig(generics){
                    .name = build_args.name,
                    .short_tag = null,
                    .long_tag = null,
                    .env_var = env_var,
                    .description = build_args.description,
                    .value_count = .{ .fixed = 0 },
                    .default = build_args.default,
                    .converter = build_args.converter,
                    .eager = build_args.eager,
                    .required = build_args.required,
                    .global = build_args.global,
                    .multi = build_args.multi,
                    .secret = build_args.secret,
                    .exposed = build_args.exposed,
                };

                self.param_spec.add(make_option(generics, args));
            }
        }

        fn generate(comptime self: @This()) self.param_spec.TupleType() {
            return self.param_spec.realTuple();
        }

        pub fn CallbackSignature(comptime self: @This()) type {
            return *const fn (ContextType, self.Output()) anyerror!void;
        }

        pub fn Output(comptime self: @This()) type {
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
                                    @compileError("redundant flag!!!! " ++ param.name);
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
                        param.ResultType()
                    else
                        ?param.ResultType();

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
                                    @compileError("redundant flag!!!! " ++ param.name);
                                } else {
                                    bais_seen[@enumToInt(PeekType.flag_bias)] = true;
                                }
                                flag_skip += 1;
                            } else {
                                break;
                            }
                        }
                    }

                    // This needs to be reconciled with options that take many
                    // arguments. We could make all of these be ArrayLists of string
                    // slices instead... but that makes the parsing code much more allocation heavy.
                    // The problem is essentially that `--long=multi,value` and `--long multi value`
                    // evaluate to a different number of strings for the same number of arguments.

                    const FieldType = switch (param.value_count) {
                        .fixed => |val| switch (val) {
                            0, 1 => []const u8,
                            else => std.ArrayList([]const u8),
                        },
                        else => std.ArrayList([]const u8),
                    };

                    fields = &(@as([fields.len]StructField, fields[0..fields.len].*) ++ [1]StructField{.{
                        .name = param.name,
                        .type = ?FieldType,
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

        pub fn bind(
            comptime self: @This(),
            comptime callback: self.CallbackSignature(),
            allocator: std.mem.Allocator,
        ) Parser(self, callback) {
            return Parser(self, callback){ .allocator = allocator };
        }
    };
}

fn push_unparsed_multi(comptime T: type, comptime field: []const u8, intermediate: *T, value: []const u8, alloc: std.mem.Allocator) !void {
    if (@field(intermediate, field) == null) {
        @field(intermediate, field) = std.ArrayList([]const u8).init(alloc);
    }

    try @field(intermediate, field).?.append(value);
}

fn push_unparsed_value(comptime T: type, comptime param: anytype, intermediate: *T, value: []const u8, alloc: std.mem.Allocator) ParseError!void {
    switch (comptime param.value_count) {
        .fixed => |val| switch (val) {
            0, 1 => @field(intermediate, param.name) = value,
            else => push_unparsed_multi(T, param.name, intermediate, value, alloc) catch return ParseError.UnexpectedFailure,
        },
        else => push_unparsed_multi(T, param.name, intermediate, value, alloc) catch return ParseError.UnexpectedFailure,
    }
}

fn ParserInterface(comptime ContextType: type) type {
    return struct {
        ctx: *anyopaque,
        methods: *const Interface,

        const Interface = struct {
            execute: *const fn (ctx: *anyopaque, context: ContextType) anyerror!void,
        };

        pub fn execute(self: @This(), context: ContextType) anyerror!void {
            return try self.methods.execute(self.ctx, context);
        }
    };
}

// the parser is generated by the bind method of the CommandBuilder, so we can
// be extremely type-sloppy here, which simplifies the signature.
fn Parser(comptime command: anytype, comptime callback: anytype) type {
    return struct {
        const ContextType = @TypeOf(command).UserContextType;
        const Intermediate = command.Intermediate();
        const Output = command.Output();

        intermediate: Intermediate = .{},
        output: Output = undefined,
        consumed_args: u32 = 0,
        progname: ?[]const u8 = null,
        has_global_tags: bool = false,
        allocator: std.mem.Allocator,

        // pub fn add_subcommand(self: *@This(), verb: []const u8, parser: anytype) void {
        //     self.subcommands
        // }

        pub fn interface(self: *@This()) ParserInterface(ContextType) {
            return .{ .ctx = self, .methods = &.{ .execute = wrap_execute } };
        }

        fn wrap_execute(ctx: *anyopaque, context: ContextType) anyerror!void {
            const self = @ptrCast(*@This(), @alignCast(@alignOf(@This()), ctx));
            return try self.execute(context);
        }

        pub fn execute(self: *@This(), context: ContextType) anyerror!void {
            const args = try std.process.argsAlloc(self.allocator);
            defer std.process.argsFree(self.allocator, args);
            var env = try std.process.getEnvMap(self.allocator);
            defer env.deinit();

            if (args.len < 1) return ParseError.EmptyArgs;

            self.progname = args[0];
            try self.parse(args[1..]);
            // run eager conversions
            // try self.convert_eager()
            // run normal conversions
            // try self.convert()
            // execute callback:
            try callback(context, self.output);

            inline for (@typeInfo(@TypeOf(self.intermediate)).Struct.fields) |field| {
                // @compileLog(@typeName(field.type));
                if (comptime std.mem.startsWith(u8, @typeName(field.type), "?array_list.ArrayList")) {
                    if (@field(self.intermediate, field.name)) |list| {
                        std.debug.print("{s}: [\n", .{field.name});
                        for (list.items) |item| std.debug.print("    {s},\n", .{item});
                        std.debug.print("]\n", .{});
                    } else {
                        std.debug.print("{s}: null\n", .{field.name});
                    }
                } else {
                    std.debug.print("{s}: {?s}\n", .{ field.name, @field(self.intermediate, field.name) });
                }
            }
        }

        pub fn parse(
            self: *@This(),
            args: [][:0]u8,
        ) anyerror!void {
            // actually: don't consider env variables until performing conversions. This
            // is the most reasonable way to treat the environment as a
            // separate "namespace" for e.g. multi options. we only want to use
            // environment values if there is nothing specified on the CLI, which cannot
            // be determined until the CLI parsing is complete.

            // try self.read_environment(env);

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
                        try self.parse_long_tag(arg, &argit);
                        continue :argloop;
                    } else if (arg.len > 1) {
                        for (arg[1..], 1..) |short, idx| {
                            try self.parse_short_tag(short, arg.len - idx - 1, &argit);
                        }
                        continue :argloop;
                    }

                    // if we've fallen through to here then we will be parsing ordinals
                    // exclusively from here on out.
                    forced_ordinal = true;
                }

                try self.parse_ordinals(arg, &argit);
            }
        }

        inline fn parse_long_tag(
            self: *@This(),
            arg: []const u8,
            argit: *ncmeta.SliceIterator([][:0]u8),
        ) ParseError!void {
            inline for (comptime command.generate()) |param| {
                const PType = @TypeOf(param);
                // removing the comptime here causes the compiler to die
                comptime if (PType.param_type != .Nominal or param.long_tag == null) continue;
                const tag = param.long_tag.?;

                if (comptime PType.is_flag) {
                    if (std.mem.eql(u8, arg, tag)) {
                        try self.apply_param_values(param, comptime PType.flag_bias.string(), argit);
                        return;
                    }
                } else {
                    if (std.mem.startsWith(u8, arg, tag)) match: {
                        // TODO: in case of --long=value we should split value
                        // on comma, so e.g. --long=one,two which is kinda docker-style.
                        // This adds complexity. Note that --long=one,two will also
                        // parse as a single value because we take a different
                        // codepath. In that case presumably the converter will choke if
                        // it needs to. Ideally the multi-value stuff would all be
                        // shoved into the converter layer, but we can't do that due to
                        // needing to consume multiple argv values in some cases. This
                        // could be an opportunity to become opinionated about CLI flag
                        // styles, but I will not do that for the time being.
                        if (arg.len == tag.len) {
                            const next = argit.next() orelse return ParseError.ValueMissing;
                            try self.apply_param_values(param, next, argit);
                        } else if (arg[tag.len] == '=') {
                            try self.apply_fused_values(param, arg[tag.len + 1 ..]);
                        } else break :match;

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
            argit: *ncmeta.SliceIterator([][:0]u8),
        ) ParseError!void {
            inline for (comptime command.generate()) |param| {
                const PType = @TypeOf(param);
                // removing the comptime here causes the compiler to die
                comptime if (PType.param_type != .Nominal or param.short_tag == null) continue;
                const tag = param.short_tag.?;

                if (comptime PType.is_flag) {
                    if (arg == tag[1]) {
                        try self.apply_param_values(param, comptime PType.flag_bias.string(), argit);
                        return;
                    }
                } else {
                    if (arg == tag[1]) {
                        if (remaining > 0) return ParseError.FusedShortTagValueMissing;
                        const next = argit.next() orelse return ParseError.ValueMissing;
                        try self.apply_param_values(param, next, argit);
                        return;
                    }
                }
            }

            return ParseError.UnknownShortTagParameter;
        }

        inline fn parse_ordinals(
            self: *@This(),
            arg: []const u8,
            argit: *ncmeta.SliceIterator([][:0]u8),
        ) ParseError!void {
            comptime var arg_index: u32 = 0;
            inline for (comptime command.generate()) |param| {
                comptime if (@TypeOf(param).param_type != .Ordinal) continue;

                if (self.consumed_args == arg_index) {
                    try self.apply_param_values(param, arg, argit);
                    self.consumed_args += 1;
                    return;
                }

                arg_index += 1;
            }

            // look for subcommands now
        }

        inline fn apply_param_values(self: *@This(), comptime param: anytype, value: []const u8, argit: *ncmeta.SliceIterator([][:0]u8)) ParseError!void {
            try push_unparsed_value(Intermediate, param, &self.intermediate, value, self.allocator);
            switch (comptime param.value_count) {
                .fixed => |count| switch (count) {
                    0, 1 => return,
                    else => |total| {
                        var consumed: u32 = 1;
                        while (consumed < total) : (consumed += 1) {
                            const next = argit.next() orelse return ParseError.ValueMissing;
                            try push_unparsed_value(
                                Intermediate,
                                param,
                                &self.intermediate,
                                next,
                                self.allocator,
                            );
                        }
                    },
                },
                .greedy => {
                    while (argit.next()) |next| {
                        try push_unparsed_value(
                            Intermediate,
                            param,
                            &self.intermediate,
                            next,
                            self.allocator,
                        );
                    }
                },
            }
        }

        inline fn apply_fused_values(self: *@This(), comptime param: anytype, value: []const u8) ParseError!void {
            switch (comptime param.value_count) {
                .fixed => |count| switch (count) {
                    0 => return ParseError.UnexpectedValue,
                    1 => try push_unparsed_value(Intermediate, param, &self.intermediate, value, self.allocator),
                    else => |total| {
                        var seen: u32 = 0;
                        var iterator = std.mem.split(u8, value, ",");
                        while (iterator.next()) |next| {
                            try push_unparsed_value(Intermediate, param, &self.intermediate, next, self.allocator);
                            seen += 1;
                        }
                        if (seen < total) return ParseError.ValueMissing else if (seen > total) return ParseError.UnexpectedValue;
                    },
                },
                .greedy => {
                    // huh. this is just an unchecked version of the fixed-many case.
                    var iterator = std.mem.split(u8, value, ",");
                    while (iterator.next()) |next| {
                        try push_unparsed_value(Intermediate, param, &self.intermediate, next, self.allocator);
                    }
                },
            }
        }

        fn read_environment(self: *@This(), env: std.process.EnvMap) !void {
            inline for (comptime command.generate()) |param| {
                if (comptime param.env_var) |env_var| {
                    const val = env.get(env_var) orelse return;

                    push_unparsed_value(
                        Intermediate,
                        param,
                        &self.intermediate,
                        val,
                        self.allocator,
                    ) catch return ParseError.UnexpectedFailure;
                    return;
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
        .value_count = .{ .fixed = 2 },
    });
    cmd.add_option(Choice, .{
        .name = "choice",
        .short_tag = "-c",
        .long_tag = "--choice",
        .env_var = "NOCLIP_CHOICE",
    });
    cmd.add_flag(.{
        .name = "flag",
        .truthy = .{ .short_tag = "-f", .long_tag = "--flag" },
        .falsy = .{ .long_tag = "--no-flag" },
        .env_var = "NOCLIP_FLAG",
    });
    cmd.add_argument([]const u8, .{
        .name = "arg",
        // .value_count = .{ .fixed = 3 },
        .value_count = .greedy,
    });

    break :cmd cmd;
};

fn cli_handler(_: void, result: cli.Output()) !void {
    _ = result;

    std.debug.print("callback is working\n", .{});
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = cli.bind(cli_handler, allocator);
    const iface = parser.interface();
    try iface.execute({});
}
