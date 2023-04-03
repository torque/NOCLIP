const std = @import("std");

const ConversionError = @import("./errors.zig").ConversionError;
const ncmeta = @import("./meta.zig");
const parameters = @import("./parameters.zig");

const ValueCount = parameters.ValueCount;
const ParameterGenerics = parameters.ParameterGenerics;

const ErrorWriter = std.ArrayList(u8).Writer;

pub fn ConverterSignature(comptime gen: ParameterGenerics) type {
    return *const fn (
        context: *gen.UserContext,
        input: gen.IntermediateType(),
        failure: ErrorWriter,
    ) ConversionError!gen.ConvertedType();
}

pub fn default_converter(comptime gen: ParameterGenerics) ?ConverterSignature(gen) {
    return if (comptime gen.multi)
        multi_converter(gen)
    else switch (@typeInfo(gen.OutputType)) {
        .Bool => flag_converter(gen),
        .Int => int_converter(gen),
        .Pointer => |info| if (info.size == .Slice and info.child == u8)
            string_converter(gen)
        else
            null,
        .Enum => |info| if (info.is_exhaustive) choice_converter(gen) else null,
        // TODO: how to handle structs with field defaults? maybe this should only work
        // for tuples, which I don't think can have defaults.
        .Struct => |info| if (gen.value_count == .fixed and gen.value_count.fixed == info.fields.len)
            struct_converter(gen)
        else
            null,
        else => null,
    };
}

fn multi_converter(comptime gen: ParameterGenerics) ?ConverterSignature(gen) {
    const converter = default_converter(
        ncmeta.copy_struct(ParameterGenerics, gen, .{ .multi = false }),
    ) orelse
        @compileError("no default converter");
    const Intermediate = gen.IntermediateType();

    return struct {
        pub fn handler(context: *gen.UserContext, input: Intermediate, failure: ErrorWriter) ConversionError!std.ArrayList(gen.OutputType) {
            var output = std.ArrayList(gen.OutputType).initCapacity(input.allocator, input.items.len) catch
                return ConversionError.ConversionFailed;

            for (input.items) |item| {
                output.appendAssumeCapacity(try converter(context, item, failure));
            }

            return output;
        }
    }.handler;
}

fn flag_converter(comptime gen: ParameterGenerics) ConverterSignature(gen) {
    return struct {
        pub fn handler(_: *gen.UserContext, input: []const u8, _: ErrorWriter) ConversionError!bool {
            // treat an empty string as falsy
            if (input.len == 0) return false;

            if (input.len <= 5) {
                var lowerBuf: [5]u8 = undefined;
                const comp = std.ascii.lowerString(&lowerBuf, input);

                inline for ([_][]const u8{ "false", "no", "0" }) |candidate| {
                    if (std.mem.eql(u8, comp, candidate)) return false;
                }
            }

            return true;
        }
    }.handler;
}

fn string_converter(comptime gen: ParameterGenerics) ConverterSignature(gen) {
    return struct {
        pub fn handler(_: *gen.UserContext, input: []const u8, _: ErrorWriter) ConversionError![]const u8 {
            return input;
        }
    }.handler;
}

fn int_converter(comptime gen: ParameterGenerics) ConverterSignature(gen) {
    const IntType = gen.OutputType;

    return struct {
        pub fn handler(_: *gen.UserContext, input: []const u8, failure: ErrorWriter) ConversionError!IntType {
            return std.fmt.parseInt(IntType, input, 0) catch {
                // ignore the error
                try failure.print("cannot interpret \"{s}\" as an integer", .{input});
                return ConversionError.ConversionFailed;
            };
        }
    }.handler;
}

fn struct_converter(comptime gen: ParameterGenerics) ConverterSignature(gen) {
    const StructType = gen.OutputType;
    const type_info = @typeInfo(StructType).Struct;
    const Intermediate = gen.IntermediateType();

    return struct {
        pub fn handler(context: *gen.UserContext, input: Intermediate, failure: ErrorWriter) ConversionError!StructType {
            if (input.items.len != type_info.fields.len) {
                try failure.print(
                    "Wrong number of fields provided. Got {d}, needed {d}",
                    .{ input.items.len, type_info.fields.len },
                );
                return ConversionError.ConversionFailed;
            }

            var result: StructType = undefined;
            inline for (comptime type_info.fields, 0..) |field, idx| {
                const converter = comptime default_converter(
                    ncmeta.copy_struct(ParameterGenerics, gen, .{
                        .OutputType = field.type,
                        .value_count = .{ .fixed = 1 },
                    }),
                ) orelse
                    @compileError("cannot get converter for field" ++ field.name);

                @field(result, field.name) = try converter(context, input.items[idx], failure);
            }

            return result;
        }
    }.handler;
}

fn choice_converter(comptime gen: ParameterGenerics) ConverterSignature(gen) {
    const EnumType = gen.OutputType;

    return struct {
        pub fn handler(_: *gen.UserContext, input: []const u8, failure: ErrorWriter) ConversionError!EnumType {
            return std.meta.stringToEnum(gen.ConvertedType(), input) orelse {
                try failure.print("\"{s}\" is not a valid choice", .{input});
                return ConversionError.ConversionFailed;
            };
        }
    }.handler;
}
