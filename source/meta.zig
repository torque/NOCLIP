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
            .@"struct" => |spec| blk: {
                if (spec.decls.len > 0) {
                    @compileError("UpdateDefaults only works on structs " ++
                        "without decls due to limitations in @Type.");
                }
                break :blk spec.fields.len;
            },
            else => @compileError("can only add default value to struct type"),
        };

        var fields: [fieldcount]StructField = undefined;
        for (inputInfo.@"struct".fields, 0..) |field, idx| {
            fields[idx] = .{
                .name = field.name,
                .field_type = field.field_type,
                // the cast ostensibly does type checking for us. It also makes
                // setting null defaults work, and it converts comptime_int to
                // the appropriate type, which is nice for ergonomics. Not sure
                // if it introduces weird edge cases. Probably it's fine?
                .default_value_ptr = if (@hasField(@TypeOf(defaults), field.name))
                    @ptrCast(&@as(field.field_type, @field(defaults, field.name)))
                else
                    field.default_value_ptr,
                .is_comptime = field.is_comptime,
                .alignment = field.alignment,
            };
        }

        return @Type(.{ .@"struct" = .{
            .layout = inputInfo.@"struct".layout,
            .backing_integer = inputInfo.@"struct".backing_integer,
            .fields = &fields,
            .decls = inputInfo.@"struct".decls,
            .is_tuple = inputInfo.@"struct".is_tuple,
        } });
    }
}

pub fn enumLength(comptime T: type) comptime_int {
    return @typeInfo(T).@"enum".fields.len;
}

pub fn partition(comptime T: type, input: []const T, wedge: []const []const T) [3][]const T {
    var idx: usize = 0;
    while (idx < input.len) : (idx += 1) {
        for (wedge) |splitter| {
            if (input.len - idx < splitter.len) continue;
            if (std.mem.eql(T, input[idx .. idx + splitter.len], splitter)) {
                return [3][]const T{
                    input[0..idx],
                    input[idx..(idx + splitter.len)],
                    input[(idx + splitter.len)..],
                };
            }
        }
    }

    return [3][]const T{
        input[0..],
        input[input.len..],
        input[input.len..],
    };
}

pub fn ComptimeWriter(
    comptime Context: type,
    comptime writeFn: fn (comptime context: Context, comptime bytes: []const u8) error{}!usize,
) type {
    return struct {
        context: Context,

        const Self = @This();
        pub const Error = error{};

        pub fn write(comptime self: Self, comptime bytes: []const u8) Error!usize {
            return writeFn(self.context, bytes);
        }

        pub fn writeAll(comptime self: Self, comptime bytes: []const u8) Error!void {
            var index: usize = 0;
            while (index != bytes.len) {
                index += try self.write(bytes[index..]);
            }
        }

        pub fn print(comptime self: Self, comptime format: []const u8, args: anytype) Error!void {
            return std.fmt.format(self, format, args) catch @compileError("woah");
        }

        pub fn writeByte(comptime self: Self, byte: u8) Error!void {
            const array = [1]u8{byte};
            return self.writeAll(&array);
        }

        pub fn writeByteNTimes(comptime self: Self, byte: u8, n: usize) Error!void {
            var bytes: [256]u8 = undefined;
            std.mem.set(u8, bytes[0..], byte);

            var remaining: usize = n;
            while (remaining > 0) {
                const to_write = std.math.min(remaining, bytes.len);
                try self.writeAll(bytes[0..to_write]);
                remaining -= to_write;
            }
        }
    };
}

pub const ComptimeSliceBuffer = struct {
    buffer: []const u8 = &[_]u8{},

    const Writer = ComptimeWriter(*@This(), appendslice);

    pub fn writer(comptime self: *@This()) Writer {
        return .{ .context = self };
    }

    fn appendslice(comptime self: *@This(), comptime bytes: []const u8) error{}!usize {
        self.buffer = self.buffer ++ bytes;
        return bytes.len;
    }
};

pub fn SliceIterator(comptime T: type) type {
    // could be expanded to use std.meta.Elem, perhaps
    const ResultType = std.meta.Child(T);

    return struct {
        index: usize,
        data: T,

        pub const InitError = error{};

        pub fn wrap(value: T) @This() {
            return @This(){ .index = 0, .data = value };
        }

        pub fn next(self: *@This()) ?ResultType {
            if (self.index == self.data.len) return null;

            defer self.index += 1;
            return self.data[self.index];
        }

        pub fn peek(self: *@This()) ?ResultType {
            if (self.index == self.data.len) return null;

            return self.data[self.index];
        }

        pub fn rewind(self: *@This()) void {
            if (self.index == 0) return;

            self.index -= 1;
        }

        pub fn skip(self: *@This()) void {
            if (self.index == self.data.len) return;

            self.index += 1;
        }
    };
}

pub fn MutatingZSplitter(comptime T: type) type {
    return struct {
        buffer: [:0]T,
        delimiter: T,
        index: ?usize = 0,

        const Self = @This();

        /// Returns a slice of the next field, or null if splitting is complete.
        pub fn next(self: *Self) ?[:0]T {
            const start = self.index orelse return null;

            const end = if (std.mem.indexOfScalarPos(T, self.buffer, start, self.delimiter)) |delim_idx| blk: {
                self.buffer[delim_idx] = 0;
                self.index = delim_idx + 1;
                break :blk delim_idx;
            } else blk: {
                self.index = null;
                break :blk self.buffer.len;
            };

            return self.buffer[start..end :0];
        }

        /// Returns a slice of the remaining bytes. Does not affect iterator state.
        pub fn rest(self: Self) [:0]T {
            const end = self.buffer.len;
            const start = self.index orelse end;
            return self.buffer[start..end :0];
        }
    };
}

pub fn copyStruct(comptime T: type, source: T, field_overrides: anytype) T {
    var result: T = undefined;

    comptime for (@typeInfo(@TypeOf(field_overrides)).@"struct".fields) |field| {
        if (!@hasField(T, field.name)) @compileError("override contains bad field" ++ field);
    };

    inline for (comptime @typeInfo(T).@"struct".fields) |field| {
        if (comptime @hasField(@TypeOf(field_overrides), field.name))
            @field(result, field.name) = @field(field_overrides, field.name)
        else
            @field(result, field.name) = @field(source, field.name);
    }
    return result;
}

/// Stores type-erased pointers to items in comptime extensible data structures,
/// which allows e.g. assembling a tuple through multiple calls rather than all
/// at once.
pub const TupleBuilder = struct {
    pointers: []const *const anyopaque = &[0]*const anyopaque{},
    types: []const type = &[0]type{},

    pub fn add(comptime self: *@This(), comptime item: anytype) void {
        self.pointers = self.pointers ++ &[_]*const anyopaque{@as(*const anyopaque, &item)};
        self.types = self.types ++ &[_]type{@TypeOf(item)};
    }

    pub fn retrieve(comptime self: @This(), comptime index: comptime_int) self.types[index] {
        return @as(*const self.types[index], @ptrCast(@alignCast(self.pointers[index]))).*;
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
            for (self.types, 0..) |Type, idx| {
                fields[idx] = .{
                    .name = std.fmt.comptimePrint("{d}", .{idx}),
                    .type = Type,
                    .default_value_ptr = null,
                    // TODO: is this the right thing to do?
                    .is_comptime = false,
                    .alignment = if (@sizeOf(Type) > 0) @alignOf(Type) else 0,
                };
            }

            return @Type(.{ .@"struct" = .{
                .layout = .auto,
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
