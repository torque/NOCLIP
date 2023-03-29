const std = @import("std");

const ParameterGenerics = @import("./doodle.zig").ParameterGenerics;
const CommandError = @import("./doodle.zig").Errors;

pub const ConversionError = error{
    BadValue,
};

pub fn ConverterSignature(comptime gen: ParameterGenerics) type {
    return *const fn (gen.UserContext, gen.IntermediateType()) ConversionError!gen.ConvertedType();
}

pub fn FlagConverterSignature(comptime UserContext: type, comptime multi: bool) type {
    comptime if (multi)
        return *const fn (UserContext, std.ArrayList([]const u8)) ConversionError!std.ArrayList(bool)
    else
        return *const fn (UserContext, []const u8) ConversionError!bool;
}

pub fn default_converter(comptime gen: ParameterGenerics) ?ConverterSignature(gen) {
    return switch (@typeInfo(gen.OutputType)) {
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

// fn multi_converter(comptime gen: ParameterGenerics) ?ConverterSignature(gen) {
//     const converter = default_converter(gen) orelse @compileError("no default converter");

//     return struct {
//         pub fn handler(_: UserContext, input: std.ArrayList([]const u8)) ConversionError!std.ArrayList(OutputType) {
//             var output = std.ArrayList(OutputType).initCapacity(input.allocator, input.items.len) catch return ConversionError.BadValue;

//             for (input.items) |item| {
//                 output.appendAssumeCapacity()
//             }
//         }
//     }.handler;
// }

fn flag_converter(comptime gen: ParameterGenerics) ConverterSignature(gen) {
    return struct {
        pub fn handler(_: gen.UserContext, input: []const u8) ConversionError!bool {
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
        pub fn handler(_: gen.UserContext, value: []const u8) ConversionError![]const u8 {
            return value;
        }
    }.handler;
}

fn int_converter(comptime gen: ParameterGenerics) ConverterSignature(gen) {
    const IntType = gen.OutputType;
    comptime std.debug.assert(@typeInfo(IntType) == .Int);

    return struct {
        pub fn handler(_: gen.UserContext, value: []const u8) ConversionError!IntType {
            return std.fmt.parseInt(IntType, value, 0) catch return ConversionError.BadValue;
        }
    }.handler;
}

fn choice_converter(comptime gen: ParameterGenerics) ConverterSignature(gen) {
    const EnumType = gen.OutputType;

    return struct {
        pub fn handler(_: gen.UserContext, value: []const u8) ConversionError!EnumType {
            return std.meta.stringToEnum(gen.ConvertedType(), value) orelse ConversionError.BadValue;
        }
    }.handler;
}
