pub const Partition = struct { lhs: []const u8, rhs: ?[]const u8 };

pub const Codepoint = u21;

pub fn partition(str: []const u8, char: u8) Partition {
    return if (std.mem.indexOfScalar(u8, str, char)) |idx|
        .{ .lhs = str[0..idx], .rhs = str[idx + 1 ..] }
    else
        .{ .lhs = str, .rhs = null };
}

pub fn SliceIter(comptime T: type) type {
    return struct {
        slice: []const T,
        index: usize = 0,

        pub fn pop(self: *@This()) ?T {
            defer self.index +|= 1;
            return self.peek();
        }

        pub fn peek(self: *@This()) ?T {
            if (self.index >= self.slice.len) return null;
            return self.slice[self.index];
        }
    };
}

pub fn encodeShort(comptime short: Codepoint) []const u8 {
    comptime {
        const encoded = enc: {
            const len = std.unicode.utf8CodepointSequenceLength(short) catch unreachable;
            var buf: [len]u8 = undefined;
            _ = std.unicode.utf8Encode(short, &buf) catch @compileError("invalid unicode character");
            break :enc buf;
        };
        return encoded[0..];
    }
}

const std = @import("std");
