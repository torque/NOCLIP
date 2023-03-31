const std = @import("std");
const StructField = std.builtin.Type.StructField;

const ncmeta = @import("./meta.zig");
const parameters = @import("./parameters.zig");
const parser = @import("./parser.zig");

const ValueCount = parameters.ValueCount;
const ParameterGenerics = parameters.ParameterGenerics;
const OptionConfig = parameters.OptionConfig;
const FlagConfig = parameters.FlagConfig;
const FlagBias = parameters.FlagBias;
const make_option = parameters.make_option;
const make_argument = parameters.make_argument;

const Parser = parser.Parser;
const ParserInterface = parser.ParserInterface;

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
                .value_count = ParameterGenerics.fixed_value_count(self.OutputType, self.value_count),
                .multi = self.multi,
            };
        }

        pub fn opt_gen(comptime self: @This()) ParameterGenerics {
            if (self.OutputType == void) @compileError("option must have OutputType specified");
            if (self.value_count == .flag) @compileError("option may not be a flag");

            return ParameterGenerics{
                .UserContext = UserContext,
                .OutputType = self.OutputType,
                .param_type = .Nominal,
                .value_count = ParameterGenerics.fixed_value_count(self.OutputType, self.value_count),
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

pub fn CommandBuilder(comptime UserContext: type) type {
    return struct {
        param_spec: ncmeta.MutableTuple = .{},

        pub const UserContextType = UserContext;

        pub fn init() @This() {
            return .{};
        }

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
                    // @compileLog(env_var);
                    args.short_tag = null;
                    args.long_tag = null;
                    args.env_var = env_var;
                    args.flag_bias = .unbiased;

                    self.param_spec.add(make_option(generics, args));
                }
            }
        }

        pub fn generate(comptime self: @This()) self.param_spec.TupleType() {
            return self.param_spec.realTuple();
        }

        pub fn CallbackSignature(comptime self: @This()) type {
            return *const fn (*UserContext, self.Output()) anyerror!void;
        }

        pub fn Output(comptime self: @This()) type {
            comptime {
                const spec = self.generate();
                var fields: []const StructField = &[_]StructField{};
                var flag_skip = 0;

                var tag_fields: []const StructField = &[_]StructField{};
                var env_var_fields: []const StructField = &[_]StructField{};

                paramloop: for (spec, 0..) |param, idx| {
                    // these three blocks are to check for redundantly defined tags and
                    // environment variables. This only works within a command. It
                    // doesn't support compile time checks for conflict into
                    // subcommands because those are attached at runtime. also, only
                    // global tags and env_vars would conflict, which is less common.
                    if (param.short_tag) |short|
                        tag_fields = tag_fields ++ &[_]StructField{.{
                            .name = short,
                            .type = void,
                            .default_value = null,
                            .is_comptime = false,
                            .alignment = 0,
                        }};

                    if (param.long_tag) |long|
                        tag_fields = tag_fields ++ &[_]StructField{.{
                            .name = long,
                            .type = void,
                            .default_value = null,
                            .is_comptime = false,
                            .alignment = 0,
                        }};

                    if (param.env_var) |env_var|
                        env_var_fields = env_var_fields ++ &[_]StructField{.{
                            .name = env_var,
                            .type = void,
                            .default_value = null,
                            .is_comptime = false,
                            .alignment = 0,
                        }};

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
                    const FieldType = if (param.required or param.default != null)
                        PType.G.ConvertedType()
                    else
                        ?PType.G.ConvertedType();

                    const default = if (param.default) |def| &@as(FieldType, def) else @as(?*const anyopaque, null);

                    fields = fields ++ &[_]StructField{.{
                        .name = param.name,
                        .type = FieldType,
                        .default_value = default,
                        .is_comptime = false,
                        .alignment = @alignOf(FieldType),
                    }};
                }

                _ = @Type(.{ .Struct = .{
                    .layout = .Auto,
                    .fields = tag_fields,
                    .decls = &.{},
                    .is_tuple = false,
                } });

                _ = @Type(.{ .Struct = .{
                    .layout = .Auto,
                    .fields = env_var_fields,
                    .decls = &.{},
                    .is_tuple = false,
                } });

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
            return Parser(self, callback){
                .allocator = allocator,
                .subcommands = std.hash_map.StringHashMap(ParserInterface).init(allocator),
            };
        }
    };
}
