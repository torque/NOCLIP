const std = @import("std");
const StructField = std.builtin.Type.StructField;

const help = @import("./help.zig");
const ncmeta = @import("./meta.zig");
const parameters = @import("./parameters.zig");
const parser = @import("./parser.zig");

const ValueCount = parameters.ValueCount;
const ParameterGenerics = parameters.ParameterGenerics;
const OptionConfig = parameters.OptionConfig;
const FlagConfig = parameters.FlagConfig;
const ShortLongPair = parameters.ShortLongPair;
const FlagBias = parameters.FlagBias;
const makeOption = parameters.makeOption;
const makeArgument = parameters.makeArgument;

const Parser = parser.Parser;
const ParserInterface = parser.ParserInterface;

fn BuilderGenerics(comptime UserContext: type) type {
    return struct {
        OutputType: type = void,
        value_count: ValueCount = .{ .fixed = 1 },
        multi: bool = false,

        pub fn argGen(comptime self: @This()) ParameterGenerics {
            if (self.value_count == .flag) @compileError("argument may not be a flag");
            if (self.value_count == .count) @compileError("argument may not be a count");

            return ParameterGenerics{
                .UserContext = UserContext,
                .OutputType = self.OutputType,
                .param_type = .Ordinal,
                .value_count = ParameterGenerics.fixedValueCount(self.OutputType, self.value_count),
                .multi = self.multi,
            };
        }

        pub fn optGen(comptime self: @This()) ParameterGenerics {
            if (self.value_count == .flag) @compileError("option may not be a flag");
            if (self.value_count == .count) @compileError("option may not be a count");

            return ParameterGenerics{
                .UserContext = UserContext,
                .OutputType = self.OutputType,
                .param_type = .Nominal,
                .value_count = ParameterGenerics.fixedValueCount(self.OutputType, self.value_count),
                .multi = self.multi,
            };
        }

        pub fn countGen(comptime _: @This()) ParameterGenerics {
            return ParameterGenerics{
                .UserContext = UserContext,
                .OutputType = usize,
                .param_type = .Nominal,
                .value_count = .count,
                .multi = true,
            };
        }

        pub fn flagGen(comptime self: @This()) ParameterGenerics {
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

pub const GroupOptions = struct {
    help_flag: ShortLongPair = .{ .short_tag = "-h", .long_tag = "--help" },
    description: []const u8,
};

pub fn commandGroup(allocator: std.mem.Allocator, comptime options: GroupOptions) !ParserInterface {
    const cmd = comptime CommandBuilder(void){
        .help_flag = options.help_flag,
        .description = options.description,
        .subcommand_required = true,
    };

    return try cmd.createInterface(allocator, cmd.noopCallback());
}

fn InterfaceCreator(comptime Command: type) type {
    return if (Command.ICC.InputType()) |Type|
        struct {
            pub fn createInterface(
                comptime self: Command,
                allocator: std.mem.Allocator,
                comptime callback: self.CallbackSignature(),
                context: Type,
            ) !ParserInterface {
                return try self._createInterfaceImpl(allocator, callback, context);
            }
        }
    else
        struct {
            pub fn createInterface(
                comptime self: Command,
                allocator: std.mem.Allocator,
                comptime callback: self.CallbackSignature(),
            ) !ParserInterface {
                return try self._createInterfaceImpl(allocator, callback, void{});
            }
        };
}

pub const InterfaceContextCategory = union(enum) {
    empty,
    pointer: type,
    value: type,

    pub fn fromType(comptime ContextType: type) InterfaceContextCategory {
        return switch (@typeInfo(ContextType)) {
            .Void => .empty,
            .Pointer => |info| if (info.size == .Slice) .{ .value = ContextType } else .{ .pointer = ContextType },
            // technically, i0, u0, and struct{} should be treated as empty, probably
            else => .{ .value = ContextType },
        };
    }

    pub fn InputType(comptime self: InterfaceContextCategory) ?type {
        return switch (self) {
            .empty => null,
            .pointer => |Type| Type,
            .value => |Type| *const Type,
        };
    }

    pub fn OutputType(comptime self: InterfaceContextCategory) type {
        return switch (self) {
            .empty => void,
            .pointer => |Type| Type,
            .value => |Type| Type,
        };
    }
};

pub fn CommandBuilder(comptime UserContext: type) type {
    return struct {
        param_spec: ncmeta.TupleBuilder = .{},
        // this is a strange hack, but it's easily the path of least resistance
        help_flag: ShortLongPair = .{ .short_tag = "-h", .long_tag = "--help" },
        /// if any subcommands are provided, one of them must be specified, or the command has failed.
        subcommand_required: bool = true,
        description: []const u8,

        pub const UserContextType = UserContext;
        pub const ICC: InterfaceContextCategory = InterfaceContextCategory.fromType(UserContextType);

        pub fn createParser(
            comptime self: @This(),
            comptime callback: self.CallbackSignature(),
            allocator: std.mem.Allocator,
        ) !Parser(self, callback) {
            // note: this is freed in Parser.deinit
            var arena = try allocator.create(std.heap.ArenaAllocator);
            arena.* = std.heap.ArenaAllocator.init(allocator);
            const arena_alloc = arena.allocator();

            return Parser(self, callback){
                .arena = arena,
                .allocator = arena_alloc,
                .subcommands = parser.CommandMap.init(arena_alloc),
                .help_builder = help.HelpBuilder(self).init(arena_alloc),
            };
        }

        pub usingnamespace InterfaceCreator(@This());

        fn _createInterfaceImpl(
            comptime self: @This(),
            allocator: std.mem.Allocator,
            comptime callback: self.CallbackSignature(),
            context: (ICC.InputType() orelse void),
        ) !ParserInterface {
            var arena = try allocator.create(std.heap.ArenaAllocator);
            arena.* = std.heap.ArenaAllocator.init(allocator);
            const arena_alloc = arena.allocator();

            var this_parser = try arena_alloc.create(Parser(self, callback));
            this_parser.* = .{
                .arena = arena,
                .allocator = arena_alloc,
                .subcommands = parser.CommandMap.init(arena_alloc),
                .help_builder = help.HelpBuilder(self).init(arena_alloc),
            };

            if (comptime ICC == .empty) {
                return this_parser.interface();
            } else {
                return this_parser.interface(context);
            }
        }

        pub fn setHelpFlag(
            comptime self: *@This(),
            comptime tags: ShortLongPair,
        ) void {
            self.help_flag = tags;
        }

        const string_generics = BuilderGenerics(UserContext){ .OutputType = [:0]const u8 };

        pub fn stringOption(
            comptime self: *@This(),
            comptime cfg: OptionConfig(string_generics.optGen()),
        ) void {
            const config = if (cfg.nice_type_name == null)
                ncmeta.copyStruct(@TypeOf(cfg), cfg, .{ .nice_type_name = "string" })
            else
                cfg;

            self.addOption(string_generics, config);
        }

        pub fn stringArgument(
            comptime self: *@This(),
            comptime cfg: OptionConfig(string_generics.argGen()),
        ) void {
            const config = if (cfg.nice_type_name == null)
                ncmeta.copyStruct(@TypeOf(cfg), cfg, .{ .nice_type_name = "string" })
            else
                cfg;

            self.addArgument(string_generics, config);
        }

        pub fn simpleFlag(
            comptime self: *@This(),
            comptime cfg: FlagConfig(string_generics.flagGen()),
        ) void {
            self.addFlag(string_generics, cfg);
        }

        pub fn addArgument(
            comptime self: *@This(),
            comptime bgen: BuilderGenerics(UserContext),
            comptime config: OptionConfig(bgen.argGen()),
        ) void {
            self.param_spec.add(makeArgument(bgen.argGen(), config));
        }

        pub fn addOption(
            comptime self: *@This(),
            comptime bgen: BuilderGenerics(UserContext),
            comptime config: OptionConfig(bgen.optGen()),
        ) void {
            if (comptime bgen.value_count == .fixed and bgen.value_count.fixed == 0) {
                @compileError(
                    "please use add_flag rather than add_option to " ++
                        "create a 0-argument option",
                );
            }

            self.param_spec.add(makeOption(bgen.optGen(), config));
        }

        pub fn addFlag(
            comptime self: *@This(),
            comptime bgen: BuilderGenerics(UserContext),
            comptime config: FlagConfig(bgen.flagGen()),
        ) void {
            comptime {
                if (config.truthy == null and config.falsy == null and config.env_var == null) {
                    @compileError(
                        "flag " ++
                            config.name ++
                            " must have at least one of truthy flags, falsy flags, or env_var flags",
                    );
                }

                const generics = bgen.flagGen();
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

                    self.param_spec.add(makeOption(generics, args));
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

                    self.param_spec.add(makeOption(generics, args));
                }

                if (config.env_var) |env_var| {
                    // @compileLog(env_var);
                    args.short_tag = null;
                    args.long_tag = null;
                    args.env_var = env_var;
                    args.flag_bias = .unbiased;

                    self.param_spec.add(makeOption(generics, args));
                }
            }
        }

        pub fn generate(comptime self: @This()) self.param_spec.TupleType() {
            return self.param_spec.realTuple();
        }

        pub fn noopCallback(comptime self: @This()) self.CallbackSignature() {
            return struct {
                fn callback(_: UserContextType, _: self.Output()) !void {}
            }.callback;
        }

        pub fn CallbackSignature(comptime self: @This()) type {
            return *const fn (UserContextType, self.Output()) anyerror!void;
        }

        pub fn Output(comptime self: @This()) type {
            comptime {
                const spec = self.generate();
                var fields: []const StructField = &[_]StructField{};
                var flag_skip = 0;

                var tag_fields: []const StructField = &[_]StructField{};
                var env_var_fields: []const StructField = &[_]StructField{};

                paramloop: for (spec, 0..) |param, idx| {
                    const PType = @TypeOf(param);
                    // these three blocks are to check for redundantly defined tags and
                    // environment variables. This only works within a command. It
                    // doesn't support compile time checks for conflict into
                    // subcommands because those are attached at runtime. also, only
                    // global tags and env_vars would conflict, which is less common.
                    if (param.short_tag) |short|
                        tag_fields = tag_fields ++ &[_]StructField{.{
                            // this goofy construct coerces the comptime []const u8 to
                            // [:0]const u8.
                            .name = short ++ "",
                            .type = void,
                            .default_value = null,
                            .is_comptime = false,
                            .alignment = 0,
                        }};

                    if (param.long_tag) |long|
                        tag_fields = tag_fields ++ &[_]StructField{.{
                            .name = long ++ "",
                            .type = void,
                            .default_value = null,
                            .is_comptime = false,
                            .alignment = 0,
                        }};

                    if (param.env_var) |env_var|
                        env_var_fields = env_var_fields ++ &[_]StructField{.{
                            .name = env_var ++ "",
                            .type = void,
                            .default_value = null,
                            .is_comptime = false,
                            .alignment = 0,
                        }};

                    if (!PType.has_output) continue :paramloop;

                    while (flag_skip > 0) {
                        flag_skip -= 1;
                        continue :paramloop;
                    }

                    if (PType.is_flag) {
                        var peek = idx + 1;
                        var bias_seen: [ncmeta.enumLength(FlagBias)]bool = [_]bool{false} ** ncmeta.enumLength(FlagBias);
                        bias_seen[@intFromEnum(param.flag_bias)] = true;
                        while (peek < spec.len) : (peek += 1) {
                            const peek_param = spec[peek];

                            if (@TypeOf(peek_param).is_flag and std.mem.eql(u8, param.name, peek_param.name)) {
                                if (bias_seen[@intFromEnum(peek_param.flag_bias)] == true) {
                                    @compileError("redundant flag!!!! " ++ param.name);
                                } else {
                                    bias_seen[@intFromEnum(peek_param.flag_bias)] = true;
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
                        .name = param.name ++ "",
                        .type = FieldType,
                        .default_value = @ptrCast(default),
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
                        var bias_seen: [ncmeta.enumLength(FlagBias)]bool = [_]bool{false} ** ncmeta.enumLength(FlagBias);
                        bias_seen[@intFromEnum(param.flag_bias)] = true;
                        while (peek < spec.len) : (peek += 1) {
                            const peek_param = spec[peek];

                            if (@TypeOf(peek_param).is_flag and std.mem.eql(u8, param.name, peek_param.name)) {
                                if (bias_seen[@intFromEnum(peek_param.flag_bias)] == true) {
                                    @compileError("redundant flag!!!! " ++ param.name);
                                } else {
                                    bias_seen[@intFromEnum(peek_param.flag_bias)] = true;
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
                        .name = param.name ++ "",
                        .type = FieldType,
                        .default_value = @ptrCast(&@as(
                            FieldType,
                            if (PType.value_count == .count) 0 else null,
                        )),
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
    };
}
