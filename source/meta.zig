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
            .Struct => |spec| blk: {
                if (spec.decls.len > 0) {
                    @compileError("UpdateDefaults only works on structs " ++
                        "without decls due to limitations in @Type.");
                }
                break :blk spec.fields.len;
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

/// Stores type-erased pointers to items in comptime extensible data structures,
/// which allows e.g. assembling a tuple through multiple calls rather than all
/// at once.
pub const MutableTuple = struct {
    pointers: []const *const anyopaque = &[0]*const anyopaque{},
    types: []const type = &[0]type{},

    pub fn add(comptime self: *@This(), comptime item: anytype) void {
        self.pointers = &(@as([self.pointers.len]*const anyopaque, self.pointers[0..self.pointers.len].*) ++ [1]*const anyopaque{@as(*const anyopaque, &item)});
        self.types = &(@as([self.types.len]type, self.types[0..self.types.len].*) ++ [1]type{@TypeOf(item)});
    }

    pub fn retrieve(comptime self: @This(), comptime index: comptime_int) self.types[index] {
        return @ptrCast(*const self.types[index], @alignCast(@alignOf(*const self.types[index]), self.pointers[index])).*;
    }

    pub fn realTuple(comptime self: @This()) self.TupleType() {
        comptime {
            var result: self.TupleType() = undefined;
            var idx = 0;
            while (idx < self.types.len) : (idx += 1) {
                result[idx] = self.retrieve(idx);
            }
            return result;
        }
    }

    pub fn TupleType(comptime self: @This()) type {
        comptime {
            var fields: [self.types.len]StructField = undefined;
            for (self.types) |Type, idx| {
                var num_buf: [128]u8 = undefined;
                fields[idx] = .{
                    .name = std.fmt.bufPrint(&num_buf, "{d}", .{idx}) catch unreachable,
                    .field_type = Type,
                    .default_value = null,
                    // TODO: is this the right thing to do?
                    .is_comptime = false,
                    .alignment = if (@sizeOf(Type) > 0) @alignOf(Type) else 0,
                };
            }

            return @Type(.{ .Struct = .{
                .layout = .Auto,
                .fields = &fields,
                .decls = &.{},
                .is_tuple = true,
            } });
        }
    }
};

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
