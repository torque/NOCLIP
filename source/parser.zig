pub fn Parser(comptime spec: type, comptime root: bool) type {
    return struct {
        arena: *std.heap.ArenaAllocator,
        context: ContextType(spec),
        globals: GlobalParams,
        locals: LocalParams,
        subcommands: Subcommands(spec, root),

        const OptKeyVal = struct { []const u8, tokenizer.TokenContext.OptionContext };
        const SubContext = struct { []const u8, *const tokenizer.TokenContext };
        const OptAmalgam = struct { tokenizer.TokenContext.Options, []const OptKeyVal };

        pub fn shortOptions(
            globals: []const OptKeyVal,
            level: tokenizer.TokenContext.NestLevel,
        ) OptAmalgam {
            comptime {
                return if (@hasDecl(spec, "parameters")) blk: {
                    var list: []const OptKeyVal = globals;
                    var glob: []const OptKeyVal = globals;
                    for (@typeInfo(spec.parameters).@"struct".decls) |decl| {
                        const param = @field(spec.parameters, decl.name);
                        for (param.shorts()) |short| {
                            const okv = &[_]OptKeyVal{.{ short.opt, .{
                                .global = if (short.scope == .global) level else .none,
                                .value = short.value,
                            } }};
                            list = list ++ okv;
                            if (short.scope == .global)
                                glob = glob ++ okv;
                        }
                    }
                    break :blk .{ .initComptime(list), glob };
                } else .{ .initComptime(&.{}), &.{} };
            }
        }
        pub fn longOptions(
            globals: []const OptKeyVal,
            level: tokenizer.TokenContext.NestLevel,
        ) OptAmalgam {
            comptime {
                return if (@hasDecl(spec, "parameters")) blk: {
                    var list: []const OptKeyVal = globals;
                    var glob: []const OptKeyVal = globals;
                    for (@typeInfo(spec.parameters).@"struct".decls) |decl| {
                        const param = @field(spec.parameters, decl.name);
                        for (param.longs()) |long| {
                            const okv = &[_]OptKeyVal{.{ long.opt, .{
                                .global = if (long.scope == .global) level else .none,
                                .value = long.value,
                            } }};
                            list = list ++ okv;
                            if (long.scope == .global)
                                glob = glob ++ okv;
                        }
                    }
                    break :blk .{ .initComptime(list), glob };
                } else .{ .initComptime(&.{}), &.{} };
            }
        }

        pub const tokenizerContext = if (root) rootTokenizerContext else subcommandTokenizerContext;

        fn rootTokenizerContext() *const tokenizer.TokenContext {
            comptime {
                return subcommandTokenizerContext(.{}, .root);
            }
        }

        fn subcommandTokenizerContext(
            comptime globals: struct { short: []const OptKeyVal = &.{}, long: []const OptKeyVal = &.{} },
            comptime level: tokenizer.TokenContext.NestLevel,
        ) *const tokenizer.TokenContext {
            comptime {
                const short, const short_glob = shortOptions(globals.short, level);
                const long, const long_glob = longOptions(globals.long, level);

                const subcommands: tokenizer.TokenContext.Subcommands = if (@hasDecl(spec, "subcommands")) blk: {
                    var subs: []const SubContext = &.{};
                    for (@typeInfo(spec.subcommands).@"struct".decls) |decl| {
                        subs = subs ++ &[_]SubContext{.{
                            decl.name,
                            Parser(@field(spec.subcommands, decl.name), false).tokenizerContext(
                                .{ .short = short_glob, .long = long_glob },
                                level.incr(),
                            ),
                        }};
                    }
                    break :blk .initComptime(subs);
                } else .initComptime(&.{});

                return &.{
                    .short = short,
                    .long = long,
                    .positional = &.{},
                    .subcommands = subcommands,
                };
            }
        }

        pub fn init(alloc: std.mem.Allocator, context: ContextType(spec)) !Self {
            const arena = try alloc.create(std.heap.ArenaAllocator);
            arena.* = std.heap.ArenaAllocator.init(alloc);

            const globals: GlobalParams, const locals: LocalParams = comptime blk: {
                var params: struct { global: GlobalParams, local: LocalParams } = .{
                    .global = .{ .short = &.{}, .long = &.{} },
                    .local = .{ .short = &.{}, .long = &.{}, .args = &.{} },
                };

                for (@typeInfo(spec.parameters).@"struct".decls) |dinf| {
                    const decl = @field(@TypeOf(spec.parameters), dinf.name);
                    switch (@TypeOf(decl).param_type) {
                        .bool_group => {
                            for (.{ "truthy", "falsy" }, .{ true, false }) |bias, value| {
                                for (.{ "short", "long" }) |style| {
                                    if (@field(@field(decl, bias), style)) |unw| {
                                        @field(@field(params, @tagName(decl.scope)), style) = @field(@field(params, @tagName(decl.scope)), style) ++ &.{
                                            .{
                                                .param = unw,
                                                .eager = decl.eager,
                                                .takes_value = false,
                                                .mutator = implicitSetter(spec, dinf.name, value),
                                            },
                                        };
                                    }
                                }
                            }
                        },
                        .constant => {
                            for (.{ "short", "long" }) |style| {
                                if (@field(decl, style)) |unw| {
                                    @field(@field(params, @tagName(decl.scope)), style) = @field(@field(params, @tagName(decl.scope)), style) ++ &.{.{
                                        .param = unw,
                                        .eager = decl.eager,
                                        .takes_value = false,
                                        .mutator = implicitSetter(spec, dinf.name, dinf.value),
                                    }};
                                }
                            }
                        },
                        .counter => {},
                        .option => {
                            for (.{ "short", "long" }) |style| {
                                if (@field(decl, style)) |unw| {
                                    @field(@field(params, @tagName(decl.scope)), style) = @field(@field(params, @tagName(decl.scope)), style) ++ &.{.{
                                        .param = unw,
                                        .eager = decl.eager,
                                        .takes_value = true,
                                        .mutator = defaultMutator(spec, dinf.name),
                                    }};
                                }
                            }
                        },
                        .argument => {
                            params.local.args = params.local.args ++ &.{
                                defaultMutator(spec, dinf.name),
                            };
                        },
                        .group => {},
                    }
                    break :blk .{ params.global, params.local };
                }
            };

            return .{
                .arena = arena,
                .context = context,
                .globals = globals,
                .locals = locals,
            };
        }

        pub fn deinit(self: Self) void {
            const pa = self.arena.child_allocator;
            self.arena.deinit();
            pa.destroy(self.arena);
        }

        pub fn parse(self: Self, args: []const [:0]const u8, _: std.process.EnvMap) noclip.Status(void) {
            const alloc = self.arena.allocator();
            var argt = ArgTraveler.fromSlice(alloc, args) catch return .fail("out of memory");
            // pre-parse globals. globals can only be named, which simplifies things
            var result = defaultInit(Result(spec));
            while (argt.current) |node| : (argt.next()) {
                const arg = node.data;
                if (arg.len > 2 and arg[0] == '-' and arg[1] == '-') {
                    if (self.globals.long.get(arg[2..])) |pctx| {
                        argt.drop();
                        const value: [:0]const u8 = if (pctx.takes_value)
                            argt.popNext()
                        else
                            "";
                        switch (pctx.mutator(alloc, self.context, &result, value)) {}
                    }
                } else if (arg.len > 1 and arg[0] == '-') {
                    const view = std.unicode.Utf8View.init(arg[1..]) catch return .fail("thats not valid utf8");
                    var iter = view.iterator();
                    while (iter.nextCodepointSlice()) |seq| {
                        if (self.globals.short.get(seq)) {
                            // we have to drop this byte sequence within the fused short params. ugly..................... hrngrk
                        }
                    }
                }
            }
            // var parse_mode: enum { mixed, ordered } = .mixed;

            // for (args) |arg| {
            //     if (arg.len > 2 and arg[0] == '-' and arg[1] == '-') {}
            //     // if (arg.len > 0 and arg[0] == '-')
            // }
        }

        const Self = @This();

        const NamedParameter = struct {
            eager: bool,
            takes_value: bool,
            mutator: Mutator(spec),
        };

        const PMap = std.StaticStringMap(NamedParameter);
        const ArgList = std.SinglyLinkedList([:0]const u8);

        pub const ArgTraveler = struct {
            first: ?*Node = null,
            current: ?*Node = null,
            prev: ?*Node = null,
            mem: []const Node,

            pub fn fromSlice(alloc: std.mem.Allocator, slice: []const [:0]const u8) error{OutOfMemory}!ArgTraveler {
                if (slice.len == 0) return .{ .mem = &.{} };

                const nmem = try alloc.alloc(Node, slice.len);
                nmem[0] = slice[0];
                for (slice[1..], nmem[1..], nmem[0 .. nmem.len - 1]) |arg, *current, *prev| {
                    current.* = .{ .data = arg };
                    prev.next = current;
                }

                return .{ .first = &nmem[0], .current = &nmem[0], .mem = nmem };
            }

            pub fn reset(self: *ArgTraveler) void {
                self.current = self.first;
            }

            pub fn next(self: *ArgTraveler) void {
                self.prev = self.current;
                if (self.current) |current| {
                    self.current = current.next;
                }
            }

            pub fn drop(self: *ArgTraveler) void {
                if (self.current == null) return;

                if (self.current == self.first)
                    self.first = self.current.?.next
                else if (self.prev) |prev|
                    prev.next = self.current.?.next;
            }

            pub fn popNext(self: *ArgTraveler) ?*Node {
                self.next();
                defer self.drop();
                return self.current;
            }

            pub const Node = struct {
                data: [:0]const u8,
                next: ?*Node = null,
            };
        };

        // const PMap = std.StringHashMap(NamedParameter);

        const GlobalParams = struct {
            short: PMap,
            long: PMap,
        };
        const LocalParams = struct {
            short: PMap,
            long: PMap,
            args: []const Mutator(spec),
        };
    };
}

pub fn Result(comptime spec: type) type {
    comptime {
        var fields: []const std.builtin.Type.StructField = &.{};

        for (@typeInfo(spec.parameters).@"struct".decls) |df| {
            const param = @field(spec.parameters, df.name);
            if (@TypeOf(param).Result == void) continue;

            const FType = ResultFieldType(param);
            fields = fields ++ &[_]std.builtin.Type.StructField{.{
                .name = df.name ++ "",
                .type = FType,
                .default_value_ptr = resultFieldDefault(param),
                .is_comptime = false,
                .alignment = @alignOf(FType),
            }};
        }

        return @Type(.{
            .@"struct" = .{
                .layout = .auto,
                .fields = &.{},
                .decls = &.{},
                .is_tuple = false,
            },
        });
    }
}

pub fn defaultInit(comptime T: type) T {
    var result: T = undefined;

    for (@typeInfo(T).Struct.fields) |field| {
        if (field.default_value_ptr) |def| {
            @field(result, field.name) = @as(*const field.type, @ptrCast(@alignCast(def))).*;
        } else switch (@typeInfo(field.type)) {
            .@"struct" => @field(result, field.name) = defaultInit(field.type),
            else => {},
        }
    }

    return result;
}

pub fn ResultFieldType(comptime param: anytype) type {
    if (param.mode() == .accumulate) {
        return param.Type();
    }
    // if (@typeInfo(param.Type()) == .optional) {
    return if (param.default != null or param.required)
        param.Type()
    else
        ?param.Type();
    // } else @compileError("you stepped in it now " ++ @typeName(param.Type()));
}

pub fn resultFieldDefault(comptime param: anytype) ?*anyopaque {
    if (param.mode() == .accumulate) {
        return &param.default;
    }
    // if (@typeInfo(param.Type()) == .optional) {
    return if (param.default) |def|
        @constCast(&@as(param.Type(), def))
    else
        null;
    // } else @compileError("doom");
}

pub fn Subcommands(comptime spec: type, comptime root: bool) type {
    comptime {
        if (!@hasDecl(spec, "subcommands")) return void;
        const decls = @typeInfo(spec.subcommands).@"struct".decls;
        if (decls.len == 0) return void;

        var fields: []const std.builtin.Type.StructField = &.{};

        for (decls) |dinf| {
            const decl = @field(spec.subcommands, dinf.name);
            const FType = Parser(decl, false);
            fields = fields ++ &[_]std.builtin.Type.StructField{.{
                .name = dinf.name ++ "",
                .type = FType,
                .default_value_ptr = null,
                .is_comptime = false,
                .alignment = @alignOf(FType),
            }};
        }
        if (root) {
            help: switch (spec.options.create_help_command) {
                .if_subcommands => if (fields.len > 0) continue :help .always,
                .always => {
                    const FType = Parser(HelpCommand(spec), false);
                    fields = fields ++ &[_]std.builtin.Type.StructField{.{
                        .name = "help",
                        .type = FType,
                        .default_value_ptr = null,
                        .is_comptime = false,
                        .alignment = @alignOf(FType),
                    }};
                },
                .never => {},
            }
            if (spec.options.create_completion_helper) {}
        }

        return @Type(.{
            .@"struct" = .{
                .layout = .auto,
                .fields = fields,
                .decls = &.{},
                .is_tuple = false,
            },
        });
    }
}

pub fn HelpCommand(comptime rootspec: type) type {
    return struct {
        pub const description =
            \\Get detailed help for a subcommand
        ;
        pub const options: noclip.CommandOptions = .{};
        pub const parameters = struct {
            command_path: noclip.Argument(noclip.Aggregate(noclip.String)) = .{
                .description =
                \\The name of the subcommand to print help for. Nested subcommands
                \\can be requested as well.
                ,
            },
        };

        pub fn run(args: Result(@This())) void {
            HelpGenerator(rootspec).lookupHelp(args.command_path);
        }
    };
}

pub fn HelpGenerator(comptime rootspec: type) type {
    return struct {
        pub fn lookupHelp(command_path: []const noclip.String) void {
            _ = rootspec;
            _ = command_path;
            std.debug.print("This is a stub\n", .{});
        }
    };
}

pub fn FieldType(comptime T: type, comptime field: []const u8) type {
    // return @FieldType(T, field);
    return switch (@typeInfo(T)) {
        .Enum => |ti| ti.tag_type,
        inline .Union, .Struct => |tf| l: for (tf.fields) |tfield| {
            if (std.mem.eql(u8, tfield.name, field)) break :l tfield.type;
        } else unreachable,
        else => unreachable,
    };
}

pub fn ResultFT(comptime spec: type, comptime field: []const u8) type {
    return FieldType(Result(spec), field);
}

pub fn ContextType(comptime spec: type) type {
    return spec.options.context_type;
}

pub fn Mutator(comptime spec: type) type {
    return *const fn (std.mem.Allocator, ContextType(spec), *Result(spec), []const u8) noclip.Status(void);
}

pub fn TrivialConverter(comptime T: type) type {
    return *const fn () noclip.Status(T);
}

pub fn SimpleConverter(comptime T: type, comptime alloc: bool) type {
    return if (alloc)
        *const fn (std.mem.Allocator, []const u8) noclip.Status(T)
    else
        *const fn ([]const u8) noclip.Status(T);
}

pub fn Converter(comptime spec: type, comptime FType: type) type {
    const Type = enum {
        trivial,
        implicit,
        simple,
        context,
        result,
        full,

        alloc_simple,
        alloc_context,
        alloc_result,
        alloc_full,
    };

    return union(Type) {
        trivial: TrivialConverter(FType),
        simple: SimpleConverter(FType, false),
        context: *const fn (ContextType(spec), []const u8) noclip.Status(FType),
        result: *const fn (*const Result(spec), []const u8) noclip.Status(FType),
        full: *const fn (ContextType(spec), *const Result(spec), []const u8) noclip.Status(FType),

        alloc_simple: SimpleConverter(FType, true),
        alloc_context: *const fn (std.mem.Allocator, ContextType(spec), []const u8) noclip.Status(FType),
        alloc_result: *const fn (std.mem.Allocator, *const Result(spec), []const u8) noclip.Status(FType),
        alloc_full: *const fn (std.mem.Allocator, ContextType(spec), *const Result(spec), []const u8) noclip.Status(FType),

        pub fn wrap(function: anytype) @This() {
            const t: Type = comptime blk: {
                const FuncType: type = switch (@typeInfo(@TypeOf(function))) {
                    .pointer => |ptr| ptr.child,
                    .@"fn" => @TypeOf(function),
                    else => unreachable,
                };

                for (std.meta.fields(Type)) |tf| {
                    if (@typeInfo(FieldType(@This(), tf.name)).pointer.child == FuncType)
                        break :blk @field(Type, tf.name);
                } else unreachable;
            };

            return @unionInit(@This(), @tagName(t), function);
        }

        pub fn invoke(
            self: @This(),
            alloc: std.mem.Allocator,
            context: ContextType(spec),
            res: *const Result(spec),
            rawvalue: []const u8,
        ) noclip.Status(FType) {
            return switch (self) {
                .trivial => |call| call(),
                .simple => |call| call(rawvalue),
                .context => |call| call(context, rawvalue),
                .result => |call| call(res, rawvalue),
                .full => |call| call(context, res, rawvalue),

                .alloc_simple => |call| call(alloc, rawvalue),
                .alloc_context => |call| call(alloc, context, rawvalue),
                .alloc_result => |call| call(alloc, res, rawvalue),
                .alloc_full => |call| call(alloc, context, res, rawvalue),
            };
        }
    };
}

pub fn defaultConverter(comptime spec: type, comptime FType: type) Converter(spec, FType) {
    if (FType == noclip.String) {
        return convertString;
    }
    return switch (@typeInfo(FType)) {
        .int => Converter(spec, FType).wrap(convertInt(FType, 0)),
        .@"enum" => Converter(spec, FType).wrap(convertEnum(FType)),
    };
}

fn defaultMutator(
    comptime spec: type,
    comptime field: []const u8,
) Mutator(spec) {
    const converter = defaultConverter(spec, ResultFT(spec, field));
    return struct {
        fn mut(alloc: std.mem.Allocator, ctx: ContextType(spec), res: *Result(spec), rawvalue: []const u8) noclip.Status(void) {
            switch (converter.invoke(alloc, ctx, res, rawvalue)) {
                .success => |val| @field(res, field) = val,
                .failure => |val| return .{ .failure = val },
            }
            return .success;
        }
    }.mut;
}

pub fn convertInt(comptime T: type, base: u8) SimpleConverter(T, true) {
    return struct {
        fn conv(alloc: std.mem.Allocator, input: []const u8) noclip.Status(T) {
            return if (std.fmt.parseInt(FieldType, input, base)) |res|
                .succeed(res)
            else |_|
                .fail(
                    std.fmt.allocPrint(
                        alloc,
                        "could not parse {s} as an integer",
                        .{input},
                    ) catch "out of memory",
                );
        }
    }.conv;
}

pub fn convertEnum(comptime T: type) SimpleConverter(T, true) {
    return struct {
        fn conv(alloc: std.mem.Allocator, input: []const u8) noclip.Status(T) {
            return if (std.meta.stringToEnum(T, input)) |val|
                .succeed(val)
            else
                .fail(
                    std.fmt.allocPrint(
                        alloc,
                        "`{s}` is not a member of {s}",
                        .{ input, @typeName(T) },
                    ) catch "out of memory",
                );
        }
    }.conv;
}

pub fn convertString(alloc: std.mem.Allocator, input: []const u8) noclip.Status(noclip.String) {
    return if (alloc.dupe(input)) |copy|
        .succeed(.{ .bytes = copy })
    else |_|
        .fail("out of memory");
}

fn incrementor(
    comptime spec: type,
    comptime field: []const u8,
    comptime step: ResultFT(spec, field),
) Mutator(spec) {
    return struct {
        fn mut(_: std.mem.Allocator, _: ContextType(spec), res: *Result(spec), _: []const u8) noclip.Status(void) {
            @field(res, field) +|= step;
            return .success;
        }
    }.mut;
}

fn implicitSetter(
    comptime spec: type,
    comptime field: []const u8,
    comptime value: ResultFT(spec, field),
) Mutator(spec) {
    return struct {
        fn mut(_: std.mem.Allocator, _: ContextType(spec), res: *Result(spec), _: []const u8) noclip.Status(void) {
            @field(res, field) = value;
            return .success;
        }
    }.mut;
}

fn setter(
    comptime spec: type,
    comptime field: []const u8,
    comptime converter_fn: anytype,
) Mutator(spec) {
    const converter = Converter(spec, ResultFT(spec, field)).wrap(converter_fn);
    return struct {
        fn mut(alloc: std.mem.Allocator, ctx: ContextType(spec), res: *Result(spec), rawvalue: []const u8) noclip.Status(void) {
            switch (converter.invoke(alloc, ctx, res, rawvalue)) {
                .success => |val| @field(res, field) = val,
                .failure => |val| return .{ .failure = val },
            }
            return .success;
        }
    }.mut;
}

const std = @import("std");
const noclip = @import("./noclip.zig");
const tokenizer = @import("./tokenizer.zig");

const Choice = enum { first, second };

const Basic = struct {
    pub const description = "A basic test";
    pub const options: noclip.CommandOptions = .{
        .create_help_command = .never,
    };

    pub const parameters = struct {
        pub const choice: noclip.Option(Choice) = .{
            .short = 'c',
            .long = "choice",
            .env = "NOCLIP_CHOICE",
            .description = "enum choice option",
        };
        pub const default: noclip.Option(u32) = .{
            .description = "default value integer option",
            .short = 'd',
            .long = "default",
            .env = "NOCLIP_DEFAULT",
            .default = 100,
            .scope = .global,
        };
        pub const flag: noclip.BoolGroup = .{
            .truthy = .{ .short = 'f', .long = "flag" },
            .falsy = .{ .short = 'F', .long = "no-flag" },
            .env = "NOCLIP_FLAG",
            .description = "boolean flag",
        };
    };

    pub const subcommands = struct {
        pub const @"test" = struct {
            pub const description = "a nested test";
            pub const options: noclip.CommandOptions = .{};
            pub const parameters = struct {
                pub const flag: noclip.BoolGroup = .{
                    .truthy = .{ .short = 'f', .long = "flag" },
                    .falsy = .{ .short = 'F', .long = "no-flag" },
                    .env = "NOCLIP_FLAG",
                    .description = "boolean flag",
                };
            };
        };
    };
};

test "hmm" {
    const P = Parser(Basic, true);

    const tc = comptime P.tokenizerContext();
    for (tc.short.keys()) |key| {
        std.debug.print("short: {s}\n", .{key});
    }
    for (tc.long.keys()) |key| {
        std.debug.print("long: {s}\n", .{key});
    }
    for (tc.subcommands.keys()) |key| {
        std.debug.print("subcommand: {s}\n", .{key});
        for (tc.subcommands.get(key).?.short.keys()) |skey| {
            std.debug.print("short: {s}\n", .{skey});
        }
    }
}
