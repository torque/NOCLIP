const std = @import("std");
const StructField = std.builtin.Type.StructField;

const converters = @import("./converters.zig");
const ncmeta = @import("./meta.zig");

const ConverterSignature = converters.ConverterSignature;

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
    MissingValue,
    ExtraValue,
    FusedShortTagValueMissing,
    UnknownLongTagParameter,
    UnknownShortTagParameter,
};

const ParameterType = enum {
    Nominal,
    Ordinal,
    Executable,
};

// in theory, we could also have a flexible value count, which could be followed by
// any number of fixed args and be well-defined. `mv` is a classic example
// of this pattern. But putting that logic in the parser seems to add a lot of
// complexity for little gain. The `mv` use case can be much more easily handled
// with a greedy value and then splitting in the value handler.
const ValueCount = union(enum) {
    flag: void,
    count: void,
    fixed: u32,
    // variable value delimited by a character, e.g. `find -exec +` style
    // delimited: []const u8
    greedy: void,
};

const FlagBias = enum {
    falsy,
    truthy,
    unbiased,

    pub fn string(comptime self: @This()) []const u8 {
        return switch (comptime self) {
            .truthy => "true",
            .falsy => "false",
            else => @compileError("flag tag with unbiased bias?"),
        };
    }
};

pub const ParameterGenerics = struct {
    UserContext: type = void,
    OutputType: type = void,
    param_type: ParameterType,
    value_count: ValueCount,
    /// allow this named parameter to be passed multiple times.
    /// values will be appended when it is encountered. If false, only the
    /// final encountered instance will be used.
    multi: bool,

    pub fn has_context(comptime self: @This()) bool {
        return comptime self.UserContext != void;
    }

    pub fn is_flag(comptime self: @This()) bool {
        return comptime switch (self.value_count) {
            .flag, .count => true,
            .fixed, .greedy => false,
        };
    }

    pub fn ConvertedType(comptime self: @This()) type {
        // is this the correct way to collapse this?
        return comptime if (self.multi and self.value_count != .count)
            std.ArrayList(self.ReturnValue())
        else
            self.ReturnValue();
    }

    pub fn IntermediateType(comptime self: @This()) type {
        return comptime if (self.multi and self.value_count != .count)
            std.ArrayList(self.IntermediateValue())
        else
            self.IntermediateValue();
    }

    pub fn ReturnValue(comptime self: @This()) type {
        return comptime switch (self.value_count) {
            .flag => bool,
            .count => usize,
            .fixed => |count| switch (count) {
                0 => @compileError("bad fixed-zero parameter"),
                1 => self.OutputType,
                // it's actually impossible to use a list in the general case
                // because the result may have varying types. A tuple would
                // work, but cannot be iterated over without inline for. It may
                // be worth adding a ".structured" value count for a type that
                // consumes many inputs but produces a single output. It would
                // be nice to parse a tag into a struct directly. For that use
                // case, the output type must be decoupled from the input type.
                else => self.OutputType,
            },
            .greedy => std.ArrayList(self.OutputType),
        };
    }

    pub fn IntermediateValue(comptime self: @This()) type {
        return comptime switch (self.value_count) {
            .flag => []const u8,
            .count => usize,
            .fixed => |count| switch (count) {
                0 => @compileError("bad fixed-zero parameter"),
                1 => []const u8,
                else => std.ArrayList([]const u8),
            },
            .greedy => return std.ArrayList([]const u8),
        };
    }

    pub fn nonscalar(comptime self: @This()) bool {
        return comptime switch (self.value_count) {
            .flag, .count => false,
            .fixed => |count| switch (count) {
                0 => @compileError("bad fixed-zero parameter"),
                1 => false,
                else => true,
            },
            .greedy => true,
        };
    }
};

fn OptionConfig(comptime generics: ParameterGenerics) type {
    return struct {
        name: []const u8,

        short_tag: ?[]const u8 = null,
        long_tag: ?[]const u8 = null,
        env_var: ?[]const u8 = null,
        description: []const u8 = "", // description for output in help text

        default: ?generics.OutputType = null,
        converter: ?ConverterSignature(generics) = null,

        eager: bool = false,
        required: bool = generics.param_type == .Ordinal,
        global: bool = false,

        exposed: bool = true,
        secret: bool = false,
        nice_type_name: []const u8 = @typeName(generics.OutputType),
        flag_bias: FlagBias = .unbiased,
    };
}

fn FlagConfig(comptime generics: ParameterGenerics) type {
    const ShortLongPair = struct {
        short_tag: ?[]const u8 = null,
        long_tag: ?[]const u8 = null,
    };

    return struct {
        name: []const u8,

        truthy: ?ShortLongPair = null,
        falsy: ?ShortLongPair = null,
        env_var: ?[]const u8 = null,
        description: []const u8 = "",

        default: ?bool = null,
        converter: ?ConverterSignature(generics) = null,

        eager: bool = false,
        required: bool = false,
        global: bool = false,

        exposed: bool = true,
        secret: bool = false,
    };
}

fn OptionType(comptime generics: ParameterGenerics) type {
    return struct {
        pub const G: ParameterGenerics = generics;
        pub const param_type: ParameterType = generics.param_type;
        pub const is_flag: bool = generics.is_flag();
        pub const value_count: ValueCount = generics.value_count;
        pub const multi: bool = generics.multi;

        name: []const u8,
        short_tag: ?[]const u8,
        long_tag: ?[]const u8,
        env_var: ?[]const u8,
        /// description for output in help text
        description: []const u8,

        default: ?generics.OutputType,
        converter: ConverterSignature(generics),

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

        /// if false, do not expose the resulting value in the output type.
        /// the converter must have side effects for this option to do anything.
        exposed: bool,
        /// do not print help for this parameter
        secret: bool,

        /// friendly type name ("string" is better than "[]const u8")
        nice_type_name: []const u8,
        /// internal field for handling flag value biasing. Do not overwrite unless you
        /// want weird things to happen.
        flag_bias: FlagBias,

        pub fn IntermediateValue(comptime _: @This()) type {
            return generics.IntermediateValue();
        }
    };
}

fn check_short(comptime short_tag: ?[]const u8) void {
    const short = comptime short_tag orelse return;
    if (short.len != 2 or short[0] != '-') @compileError("bad short tag" ++ short);
}

fn check_long(comptime long_tag: ?[]const u8) void {
    const long = comptime long_tag orelse return;
    if (long.len < 3 or long[0] != '-' or long[1] != '-') @compileError("bad long tag" ++ long);
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
    const converter = opts.converter orelse
        (converters.default_converter(generics) orelse @compileError("no converter provided for " ++ opts.name ++ "and no default exists"));

    return OptionType(generics){
        .name = opts.name,
        //
        .short_tag = opts.short_tag,
        .long_tag = opts.long_tag,
        .env_var = opts.env_var,
        //
        .description = opts.description,
        .default = opts.default,
        .converter = converter,
        //
        .eager = opts.eager,
        .required = opts.required,
        .global = opts.global,
        //
        .exposed = opts.exposed,
        .secret = opts.secret,
        .nice_type_name = opts.nice_type_name,
        .flag_bias = opts.flag_bias,
    };
}

fn make_argument(
    comptime generics: ParameterGenerics,
    comptime opts: OptionConfig(generics),
) OptionType(generics) {
    comptime {
        if (opts.short_tag != null or opts.long_tag != null or opts.env_var != null) {
            @compileError("argument " ++ opts.name ++ " must not have a long or short tag or an env var");
        }

        const converter = opts.converter orelse
            (converters.default_converter(generics) orelse @compileError("no converter provided for " ++ opts.name ++ "and no default exists"));

        if (generics.multi == true)
            @compileError("argument " ++ opts.name ++ " cannot be multi");

        return OptionType(generics){
            .name = opts.name,
            //
            .short_tag = opts.short_tag,
            .long_tag = opts.long_tag,
            .env_var = opts.env_var,
            //
            .description = opts.description,
            .default = opts.default,
            .converter = converter,
            //
            .eager = opts.eager,
            .required = opts.required,
            .global = opts.global,
            //
            .exposed = opts.exposed,
            .secret = opts.secret,
            .nice_type_name = opts.nice_type_name,
            .flag_bias = .unbiased,
        };
    }
}

fn BuilderGenerics(comptime UserContext: type) type {
    return struct {
        OutputType: type = void,
        value_count: ValueCount = .{ .fixed = 1 },
        multi: bool = false,

        pub fn arg_gen(comptime self: @This()) ParameterGenerics {
            if (self.OutputType == void) @compileError("argument must have OutputType specified");
            if (self.value_count == .flag) @compileError("argument may not be a flag");
            if (self.value_count == .count) @compileError("argument may not be a count");

            return ParameterGenerics{
                .UserContext = UserContext,
                .OutputType = self.OutputType,
                .param_type = .Ordinal,
                .value_count = self.value_count,
                .multi = false,
            };
        }

        pub fn opt_gen(comptime self: @This()) ParameterGenerics {
            if (self.OutputType == void) @compileError("option must have OutputType specified");
            if (self.value_count == .flag) @compileError("option may not be a flag");

            return ParameterGenerics{
                .UserContext = UserContext,
                .OutputType = self.OutputType,
                .param_type = .Nominal,
                .value_count = self.value_count,
                .multi = self.multi,
            };
        }

        pub fn count_gen(comptime _: @This()) ParameterGenerics {
            return ParameterGenerics{
                .UserContext = UserContext,
                .OutputType = usize,
                .param_type = .Nominal,
                .value_count = .count,
                .multi = true,
            };
        }

        pub fn flag_gen(comptime self: @This()) ParameterGenerics {
            return ParameterGenerics{
                .UserContext = UserContext,
                .OutputType = bool,
                .param_type = .Nominal,
                .value_count = .flag,
                .multi = self.multi,
            };
        }
    };
}

fn CommandBuilder(comptime UserContext: type) type {
    return struct {
        param_spec: ncmeta.MutableTuple = .{},

        pub const UserContextType = UserContext;

        pub fn add_argument(
            comptime self: *@This(),
            comptime bgen: BuilderGenerics(UserContext),
            comptime config: OptionConfig(bgen.arg_gen()),
        ) void {
            self.param_spec.add(make_argument(bgen.arg_gen(), config));
        }

        pub fn add_option(
            comptime self: *@This(),
            comptime bgen: BuilderGenerics(UserContext),
            comptime config: OptionConfig(bgen.opt_gen()),
        ) void {
            if (comptime bgen.value_count == .fixed and bgen.value_count.fixed == 0) {
                @compileError(
                    "please use add_flag rather than add_option to " ++
                        "create a 0-argument option",
                );
            }

            self.param_spec.add(make_option(bgen.opt_gen(), config));
        }

        pub fn set_help_flag(
            comptime self: *@This(),
            comptime bgen: BuilderGenerics(UserContext),
            comptime config: FlagConfig(bgen.flag_gen()),
        ) void {
            _ = self;
            _ = config;
        }

        pub fn add_flag(
            comptime self: *@This(),
            comptime bgen: BuilderGenerics(UserContext),
            comptime config: FlagConfig(bgen.flag_gen()),
        ) void {
            comptime {
                if (config.truthy == null and config.falsy == null and config.env_var == null) {
                    @compileError(
                        "flag " ++
                            config.name ++
                            " must have at least one of truthy flags, falsy flags, or env_var flags",
                    );
                }

                const generics = bgen.flag_gen();
                var args = OptionConfig(generics){
                    .name = config.name,
                    //
                    .short_tag = null,
                    .long_tag = null,
                    .env_var = null,
                    //
                    .description = config.description,
                    .default = config.default,
                    .converter = config.converter,
                    //
                    .eager = config.eager,
                    .required = config.required,
                    .global = config.global,
                    //
                    .exposed = config.exposed,
                    .secret = config.secret,
                    .nice_type_name = "flag",
                };

                if (config.truthy) |truthy_pair| {
                    if (truthy_pair.short_tag == null and truthy_pair.long_tag == null) {
                        @compileError(
                            "flag " ++
                                config.name ++
                                " truthy pair must have at least short or long tags set",
                        );
                    }

                    args.short_tag = truthy_pair.short_tag;
                    args.long_tag = truthy_pair.long_tag;
                    args.flag_bias = .truthy;

                    self.param_spec.add(make_option(generics, args));
                }

                if (config.falsy) |falsy_pair| {
                    if (falsy_pair.short_tag == null and falsy_pair.long_tag == null) {
                        @compileError(
                            "flag " ++
                                config.name ++
                                " falsy pair must have at least short or long tags set",
                        );
                    }

                    args.short_tag = falsy_pair.short_tag;
                    args.long_tag = falsy_pair.long_tag;
                    args.flag_bias = .falsy;

                    self.param_spec.add(make_option(generics, args));
                }

                if (config.env_var) |env_var| {
                    args.short_tag = null;
                    args.long_tag = null;
                    args.env_var = env_var;
                    args.flag_bias = .unbiased;

                    self.param_spec.add(make_option(generics, args));
                }
            }
        }

        fn generate(comptime self: @This()) self.param_spec.TupleType() {
            return self.param_spec.realTuple();
        }

        pub fn CallbackSignature(comptime self: @This()) type {
            return *const fn (*UserContext, self.Output()) anyerror!void;
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
                        var bias_seen: [ncmeta.enum_length(FlagBias)]bool = [_]bool{false} ** ncmeta.enum_length(FlagBias);
                        bias_seen[@enumToInt(param.flag_bias)] = true;
                        while (peek < spec.len) : (peek += 1) {
                            const peek_param = spec[peek];

                            if (@TypeOf(peek_param).is_flag and std.mem.eql(u8, param.name, peek_param.name)) {
                                if (bias_seen[@enumToInt(peek_param.flag_bias)] == true) {
                                    @compileError("redundant flag!!!! " ++ param.name);
                                } else {
                                    bias_seen[@enumToInt(peek_param.flag_bias)] = true;
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
                        PType.G.ConvertedType()
                    else
                        ?PType.G.ConvertedType();

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
                        var bias_seen: [ncmeta.enum_length(FlagBias)]bool = [_]bool{false} ** ncmeta.enum_length(FlagBias);
                        bias_seen[@enumToInt(param.flag_bias)] = true;
                        while (peek < spec.len) : (peek += 1) {
                            const peek_param = spec[peek];

                            if (@TypeOf(peek_param).is_flag and std.mem.eql(u8, param.name, peek_param.name)) {
                                if (bias_seen[@enumToInt(peek_param.flag_bias)] == true) {
                                    @compileError("redundant flag!!!! " ++ param.name);
                                } else {
                                    bias_seen[@enumToInt(peek_param.flag_bias)] = true;
                                }
                                flag_skip += 1;
                            } else {
                                break;
                            }
                        }
                    }

                    const FieldType = if (PType.value_count == .count)
                        PType.G.IntermediateType()
                    else
                        ?PType.G.IntermediateType();

                    fields = &(@as([fields.len]StructField, fields[0..fields.len].*) ++ [1]StructField{.{
                        .name = param.name,
                        .type = FieldType,
                        .default_value = @ptrCast(
                            ?*const anyopaque,
                            &@as(
                                FieldType,
                                if (PType.value_count == .count) 0 else null,
                            ),
                        ),
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

// This is a slightly annoying hack to work around the fact that there's no way to
// provide a field value conditionally.
fn ParserInterfaceImpl(comptime Interface: type) type {
    return struct {
        pub fn execute(self: Interface) anyerror!void {
            return try self.methods.execute(self.ctx, self.context);
        }
    };
}

fn ParserInterface(comptime UserContext: type) type {
    const InterfaceVtable = struct {
        execute: *const fn (ctx: *anyopaque, context: *UserContext) anyerror!void,
    };

    // we can actually bind the user context object in the interface, since
    // it is exclusively parameterized around the context type.
    return if (@typeInfo(UserContext) == .Void)
        struct {
            ctx: *anyopaque,
            context: *UserContext = @constCast(&void{}),
            methods: *const InterfaceVtable,

            pub usingnamespace ParserInterfaceImpl(@This());
        }
    else
        struct {
            ctx: *anyopaque,
            context: *UserContext,
            methods: *const InterfaceVtable,

            pub usingnamespace ParserInterfaceImpl(@This());
        };
}

fn InterfaceGen(comptime ParserType: type, comptime UserContext: type) type {
    return if (@typeInfo(UserContext) == .Void) struct {
        pub fn interface(self: *ParserType) ParserInterface(UserContext) {
            return .{ .ctx = self, .methods = &.{ .execute = ParserType.wrap_execute } };
        }
    } else struct {
        pub fn interface(self: *ParserType, context: *UserContext) ParserInterface(UserContext) {
            return .{ .ctx = self, .context = context, .methods = &.{ .execute = ParserType.wrap_execute } };
        }
    };
}

// the parser is generated by the bind method of the CommandBuilder, so we can
// be extremely type-sloppy here, which simplifies the signature.
fn Parser(comptime command: anytype, comptime callback: anytype) type {
    const UserContext = @TypeOf(command).UserContextType;
    const Intermediate = command.Intermediate();
    const Output = command.Output();

    return struct {
        intermediate: Intermediate = .{},
        output: Output = undefined,
        consumed_args: u32 = 0,
        progname: ?[]const u8 = null,
        has_global_tags: bool = false,
        allocator: std.mem.Allocator,

        // pub fn add_subcommand(self: *@This(), verb: []const u8, parser: anytype) void {
        //     self.subcommands
        // }

        // This is a slightly annoying hack to work around the fact that there's no way to
        // provide a method signature conditionally.
        pub usingnamespace InterfaceGen(@This(), UserContext);

        fn wrap_execute(ctx: *anyopaque, context: *UserContext) anyerror!void {
            const self = @ptrCast(*@This(), @alignCast(@alignOf(@This()), ctx));
            return try self.execute(context);
        }

        pub fn execute(self: *@This(), context: *UserContext) anyerror!void {
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
                if (@field(self.intermediate, field.name) == null) {
                    std.debug.print("{s}: null,\n", .{field.name});
                } else {
                    std.debug.print("{s}: ", .{field.name});
                    self.print_value(@field(self.intermediate, field.name).?, "");
                }
            }
        }

        fn print_value(self: @This(), value: anytype, comptime indent: []const u8) void {
            if (comptime @hasField(@TypeOf(value), "items")) {
                std.debug.print("{s}[\n", .{indent});
                for (value.items) |item| {
                    self.print_value(item, indent ++ "   ");
                }
                std.debug.print("{s}]\n", .{indent});
            } else {
                std.debug.print("{s}{s}\n", .{ indent, value });
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

                if (std.mem.startsWith(u8, arg, tag)) match: {
                    if (arg.len == tag.len) {
                        try self.apply_param_values(param, argit, false);
                    } else if (arg[tag.len] == '=') {
                        try self.apply_fused_values(param, arg[tag.len + 1 ..]);
                    } else break :match;

                    return;
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

                if (arg == tag[1]) {
                    if (comptime !PType.is_flag)
                        if (remaining > 0)
                            return ParseError.FusedShortTagValueMissing;

                    try self.apply_param_values(param, argit, false);
                    return;
                }
            }

            return ParseError.UnknownShortTagParameter;
        }

        inline fn parse_ordinals(
            self: *@This(),
            arg: []const u8,
            argit: *ncmeta.SliceIterator([][:0]u8),
        ) ParseError!void {
            _ = arg;

            comptime var arg_index: u32 = 0;
            inline for (comptime command.generate()) |param| {
                comptime if (@TypeOf(param).param_type != .Ordinal) continue;

                if (self.consumed_args == arg_index) {
                    argit.rewind();
                    try self.apply_param_values(param, argit, false);
                    self.consumed_args += 1;
                    return;
                }

                arg_index += 1;
            }

            // look for subcommands now
        }

        inline fn push_intermediate_value(
            self: *@This(),
            comptime param: anytype,
            value: param.IntermediateValue(),
        ) ParseError!void {
            if (comptime @TypeOf(param).G.multi) {
                if (@field(self.intermediate, param.name) == null) {
                    @field(self.intermediate, param.name) = param.IntermediateType().init(self.allocator);
                }
                @field(self.intermediate, param.name).?.append(value) catch return ParseError.UnexpectedFailure;
            } else if (comptime @TypeOf(param).G.nonscalar()) {
                if (@field(self.intermediate, param.name)) |list| list.deinit();
                @field(self.intermediate, param.name) = value;
            } else {
                @field(self.intermediate, param.name) = value;
            }
        }

        inline fn apply_param_values(
            self: *@This(),
            comptime param: anytype,
            argit: anytype,
            bounded: bool,
        ) ParseError!void {
            switch (comptime @TypeOf(param).G.value_count) {
                .flag => try self.push_intermediate_value(param, comptime param.flag_bias.string()),
                .count => @field(self.intermediate, param.name) += 1,
                .fixed => |count| switch (count) {
                    0 => return ParseError.ExtraValue,
                    1 => try self.push_intermediate_value(param, argit.next() orelse return ParseError.MissingValue),
                    else => |total| {
                        var list = std.ArrayList([]const u8).initCapacity(self.allocator, total) catch
                            return ParseError.UnexpectedFailure;

                        var consumed: u32 = 0;
                        while (consumed < total) : (consumed += 1) {
                            const next = argit.next() orelse return ParseError.MissingValue;
                            list.append(next) catch return ParseError.UnexpectedFailure;
                        }
                        if (bounded and argit.next() != null) return ParseError.ExtraValue;

                        try self.push_intermediate_value(param, list);
                    },
                },
                .greedy => {
                    var list = std.ArrayList([]const u8).init(self.allocator);
                    while (argit.next()) |next| list.append(next) catch return ParseError.UnexpectedFailure;
                    try self.push_intermediate_value(param, list);
                },
            }
        }

        inline fn apply_fused_values(
            self: *@This(),
            comptime param: anytype,
            value: []const u8,
        ) ParseError!void {
            var iter = std.mem.split(u8, value, ",");
            return try self.apply_param_values(param, &iter, true);
        }

        fn read_environment(self: *@This(), env: std.process.EnvMap) !void {
            inline for (comptime command.generate()) |param| {
                if (comptime param.env_var) |env_var| {
                    if (@field(self.intermediate, param.name) != null) return;
                    const val = env.get(env_var) orelse return;
                    try self.apply_fused_values(param, val);
                    return;
                }
            }
        }
    };
}

fn HelpBuilder(comptime command: anytype) type {
    _ = command;
}

pub fn command_builder(comptime UserContext: type) CommandBuilder(UserContext) {
    return CommandBuilder(UserContext){};
}

const Choice = enum { first, second };

fn fixed_output(_: u32, _: std.ArrayList([]const u8)) converters.ConversionError!u8 {
    return 0;
}

fn greedy_output(_: u32, input: std.ArrayList([]const u8)) converters.ConversionError!std.ArrayList([]const u8) {
    var output = std.ArrayList([]const u8).initCapacity(input.allocator, 1) catch
        return converters.ConversionError.BadValue;

    output.appendAssumeCapacity("hello");
    return output;
}

const cli = cmd: {
    var cmd = command_builder(u32);
    cmd.add_option(.{
        .OutputType = u8,
        .value_count = .{ .fixed = 2 },
    }, .{
        .name = "test",
        .short_tag = "-t",
        .long_tag = "--test",
        .env_var = "NOCLIP_TEST",
        .converter = fixed_output,
    });
    cmd.add_option(.{ .OutputType = Choice }, .{
        .name = "choice",
        .short_tag = "-c",
        .long_tag = "--choice",
        .env_var = "NOCLIP_CHOICE",
    });
    // cmd.add_option(.{ .OutputType = u8, .multi = true }, .{
    //     .name = "multi",
    //     .short_tag = "-m",
    //     .long_tag = "--multi",
    //     .env_var = "NOCLIP_MULTI",
    // });
    cmd.add_flag(.{}, .{
        .name = "flag",
        .truthy = .{ .short_tag = "-f", .long_tag = "--flag" },
        .falsy = .{ .long_tag = "--no-flag" },
        .env_var = "NOCLIP_FLAG",
    });
    // cmd.add_flag(.{ .multi = true }, .{
    //     .name = "multiflag",
    //     .truthy = .{ .short_tag = "-M" },
    //     .env_var = "NOCLIP_MULTIFLAG",
    //     .multi = true,
    // });
    cmd.add_argument(.{
        .OutputType = []const u8,
        .value_count = .greedy,
    }, .{
        .name = "arg",
        .converter = greedy_output,
    });

    break :cmd cmd;
};

fn cli_handler(context: *u32, result: cli.Output()) !void {
    _ = result;

    std.debug.print("callback is working {d}\n", .{context.*});
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = cli.bind(cli_handler, allocator);
    var context: u32 = 2;
    const iface = parser.interface(&context);
    try iface.execute();
}
