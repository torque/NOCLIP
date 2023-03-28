const std = @import("std");

const ParameterGenerics = @import("./doodle.zig").ParameterGenerics;
const CommandError = @import("./doodle.zig").Errors;

pub const ConversionError = error{
    BadValue,
};

pub fn ConverterSignature(comptime gen: ParameterGenerics) type {
    return *const fn (gen.ContextType, []const u8) ConversionError!gen.ResultType();
}

pub fn default_converter(comptime gen: ParameterGenerics) ?ConverterSignature(gen) {
    return switch (@typeInfo(gen.ResultType())) {
        .Bool => flag_converter(gen),
        .Int => int_converter(gen),
        .Pointer => |info| if (info.size == .Slice and info.child == u8)
            string_converter(gen)
        else
            null,
        .Enum => choice_converter(gen),
        else => null,
    };
}

fn flag_converter(comptime gen: ParameterGenerics) ConverterSignature(gen) {
    return struct {
        pub fn handler(_: gen.ContextType, input: []const u8) ConversionError!bool {
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
        pub fn handler(_: gen.ContextType, value: []const u8) ConversionError![]const u8 {
            return value;
        }
    }.handler;
}

fn int_converter(comptime gen: ParameterGenerics) ConverterSignature(gen) {
    const IntType = gen.ResultType();
    comptime std.debug.assert(@typeInfo(IntType) == .Int);

    return struct {
        pub fn handler(_: gen.ContextType, value: []const u8) ConversionError!IntType {
            return std.fmt.parseInt(IntType, value, 0) catch return ConversionError.BadValue;
        }
    }.handler;
}

fn choice_converter(comptime gen: ParameterGenerics) ConverterSignature(gen) {
    const EnumType = gen.ResultType();

    return struct {
        pub fn handler(_: gen.ContextType, value: []const u8) ConversionError!EnumType {
            return std.meta.stringToEnum(gen.ResultType(), value) orelse ConversionError.BadValue;
        }
    }.handler;
}
