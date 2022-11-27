const std = @import("std");
const builtin = std.builtin;

const noclip = @import("./noclip.zig");

pub fn stringHandler(comptime UserContext: type) HandlerType(.{ .UserContext = UserContext, .Output = []const u8 }) {
    return struct {
        pub fn handler(_: UserContext, buf: []const u8) ![]const u8 {
            return buf;
        }
    }.handler;
}

pub fn intHandler(comptime UserContext: type, comptime IntType: type) HandlerType(.{ .UserContext = UserContext, .Output = IntType }) {
    return struct {
        pub fn handler(_: UserContext, buf: []const u8) std.fmt.ParseIntError!IntType {
            return try std.fmt.parseInt(IntType, buf, 0);
        }
    }.handler;
}

pub fn HandlerType(comptime args: noclip.ParameterArgs) type {
    return *const fn (args.UserContext, []const u8) anyerror!args.Output;
}

pub fn getDefaultHandler(comptime args: noclip.ParameterArgs) ?HandlerType(args) {
    switch (@typeInfo(args.Output)) {
        .Optional => |info| return getDefaultHandler(.{ .Output = info.child, .UserContext = args.user }),
        .Int => return intHandler(args.UserContext, args.Output),
        .Pointer => |info| {
            if (info.size == .Slice and info.child == u8) {
                return stringHandler(args.UserContext);
            }
            return null;
        },
        else => return null,
    }
}
