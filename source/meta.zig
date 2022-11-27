const std = @import("std");
const StructField = std.builtin.Type.StructField;

/// Given a type and a struct literal of defaults to add, this function creates
/// a simulacrum type with additional defaults set on its fields.
///
/// This function cannot remove default values from fields, but it can add some
/// to fields that don't have them, and it can overwrite existing defaults
pub fn UpdateDefaults(comptime input: type, comptime defaults: anytype) type {
    comptime {
        const inputInfo = @typeInfo(input);
        const fieldcount = switch (inputInfo) {
            .Struct => |spec| {
                if (spec.decls.len > 0) {
                    @compileError("UpdateDefaults only works on structs " ++
                        "without decls due to limitations in @Type.");
                }
                break spec.fields.len;
            },
            else => @compileError("can only add default value to struct type"),
        };

        var fields: [fieldcount]StructField = undefined;
        for (inputInfo.Struct.fields) |field, idx| {
            fields[idx] = .{
                .name = field.name,
                .field_type = field.field_type,
                // the cast ostensibly does type checking for us. It also makes
                // setting null defaults work, and it converts comptime_int to
                // the appropriate type, which is nice for ergonomics. Not sure
                // if it introduces weird edge cases. Probably it's fine?
                .default_value = if (@hasField(@TypeOf(defaults), field.name))
                    @ptrCast(?*const anyopaque, &@as(field.field_type, @field(defaults, field.name)))
                else
                    field.default_value,
                .is_comptime = field.is_comptime,
                .alignment = field.alignment,
            };
        }

        return @Type(.{ .Struct = .{
            .layout = inputInfo.Struct.layout,
            .backing_integer = inputInfo.Struct.backing_integer,
            .fields = &fields,
            .decls = inputInfo.Struct.decls,
            .is_tuple = inputInfo.Struct.is_tuple,
        } });
    }
}

test "add basic default" {
    const Base = struct { a: u8 };
    const Defaulted = UpdateDefaults(Base, .{ .a = 4 });

    const value = Defaulted{};
    try std.testing.expectEqual(@as(u8, 4), value.a);
}

test "overwrite basic default" {
    const Base = struct { a: u8 = 0 };
    const Defaulted = UpdateDefaults(Base, .{ .a = 1 });

    const value = Defaulted{};
    try std.testing.expectEqual(@as(u8, 1), value.a);
}

test "add string default" {
    const Base = struct { a: []const u8 };
    const Defaulted = UpdateDefaults(Base, .{ .a = "hello" });

    const value = Defaulted{};
    try std.testing.expectEqual(@as([]const u8, "hello"), value.a);
}

test "add null default" {
    const Base = struct { a: ?u8 };
    const Defaulted = UpdateDefaults(Base, .{ .a = null });

    const value = Defaulted{};
    try std.testing.expectEqual(@as(?u8, null), value.a);
}

test "add enum default" {
    const Options = enum { good, bad };
    const Base = struct { a: Options };
    const Defaulted = UpdateDefaults(Base, .{ .a = .good });

    const value = Defaulted{};
    try std.testing.expectEqual(Options.good, value.a);
}

test "preserve existing default" {
    const Base = struct { a: ?u8 = 2, b: u8 };
    const Defaulted = UpdateDefaults(Base, .{ .b = 3 });

    const value = Defaulted{};
    try std.testing.expectEqual(@as(?u8, 2), value.a);
    try std.testing.expectEqual(@as(?u8, 3), value.b);
}

test "add multiple defaults" {
    const Base = struct { a: u8, b: i8, c: ?u8 };
    const Defaulted = UpdateDefaults(Base, .{ .a = 3, .c = 2 });

    const value = Defaulted{ .b = -1 };
    try std.testing.expectEqual(@as(u8, 3), value.a);
    try std.testing.expectEqual(@as(i8, -1), value.b);
    try std.testing.expectEqual(@as(?u8, 2), value.c);
}
