fn Short(comptime spec: type) type {
    return struct {
        param: noclip.Codepoint,
        eager: bool,
        mutator: Mutator(spec),
    };
}

fn Long(comptime spec: type) type {
    return struct {
        param: []const u8,
        eager: bool,
        mutator: Mutator(spec),
    };
}

pub fn Parser(comptime spec: type) type {
    return struct {
        // this gets heap allocated because it cannot survive being copied
        arena: *std.heap.ArenaAllocator,
        context: ContextType(spec),
        globals: GlobalParams,
        locals: LocalParams,

        pub fn init(alloc: std.mem.Allocator, context: ContextType(spec)) !Self {
            const arena = try alloc.create(std.heap.ArenaAllocator);
            arena.* = std.heap.ArenaAllocator.init(alloc);

            const globals: GlobalParams, const locals: LocalParams = comptime blk: {
                var params: struct { global: GlobalParams, local: LocalParams } = .{
                    .global = .{ .short = &.{}, .long = &.{} },
                    .local = .{ .short = &.{}, .long = &.{}, .args = &.{} },
                };

                for (@typeInfo(@TypeOf(spec.parameters)).@"struct".decls) |dinf| {
                    const decl = @field(@TypeOf(spec.parameters), dinf.name);
                    switch (@TypeOf(decl).param_type) {
                        .flag => {
                            for (.{ "truthy", "falsy" }, .{ true, false }) |bias, value| {
                                for (.{ "short", "long" }) |style| {
                                    if (@field(@field(decl, bias), style)) |unw| {
                                        @field(@field(params, @tagName(decl.scope)), style) = @field(@field(params, @tagName(decl.scope)), style) ++ &.{
                                            .{
                                                .param = unw,
                                                .mutator = implicitSetter(spec, dinf.name, value),
                                            },
                                        };
                                    }
                                }
                            }
                        },
                        .option => {
                            for (.{ "short", "long" }) |style| {
                                if (@field(decl, style)) |unw| {
                                    @field(@field(params, @tagName(decl.scope)), style) = @field(@field(params, @tagName(decl.scope)), style) ++ &.{.{
                                        .param = unw,
                                        .mutator = defaultMutator(spec, dinf.name),
                                    }};
                                }
                            }
                        },
                        .argument => {},
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

        const Self = @This();
        const GlobalParams = struct {
            short: []const Short(spec),
            long: []const Long(spec),
        };
        const LocalParams = struct {
            short: []const Short(spec),
            long: []const Long(spec),
            args: []const Mutator(spec),
        };
    };
}

pub fn Result(comptime spec: type) type {
    comptime {
        var out: std.builtin.Type = .{
            .@"struct" = .{
                .layout = .auto,
                .fields = &.{},
                .decls = &.{},
                .is_tuple = false,
            },
        };

        for (@typeInfo(@TypeOf(spec.parameters)).@"struct".decls) |df| {
            const decl = @field(spec.parameters, df.name);
            const ftype = if (decl.default != null) @TypeOf(decl).Result else ?@TypeOf(decl).Result;
            out.@"struct".fields = out.@"struct".fields ++ &.{.{
                .name = df.name,
                .type = ftype,
                .default_value = decl.default orelse null,
                .is_comptime = false,
                .alignment = @alignOf(ftype),
            }};
        }

        return @Type(out);
    }
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
                .{ .success = res }
            else |_|
                .{ .failure = .{
                    .message = std.fmt.allocPrint(
                        alloc,
                        "could not parse {s} as an integer",
                        .{input},
                    ) catch "out of memory",
                } };
        }
    }.conv;
}

pub fn convertEnum(comptime T: type) SimpleConverter(T, true) {
    return struct {
        fn conv(alloc: std.mem.Allocator, input: []const u8) noclip.Status(T) {
            return if (std.meta.stringToEnum(T, input)) |val|
                .{ .success = val }
            else
                .{
                    .failure = .{ .message = std.fmt.allocPrint(
                        alloc,
                        "`{s}` is not a member of {s}",
                        .{ input, @typeName(T) } catch "out of memory",
                    ) },
                };
        }
    }.conv;
}

pub fn convertString(alloc: std.mem.Allocator, input: []const u8) noclip.Status(noclip.String) {
    return if (alloc.dupe(input)) |copy|
        .{ .success = .{ .bytes = copy } }
    else |_|
        .{ .failure = .{ .message = "out of memory" } };
}

fn incrementor(
    comptime spec: type,
    comptime field: []const u8,
    comptime step: ResultFT(spec, field),
) Mutator(spec) {
    return struct {
        fn mut(_: std.mem.Allocator, _: ContextType(spec), res: *Result(spec), _: []const u8) noclip.Status(void) {
            @field(res, field) += step;
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
