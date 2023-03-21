const std = @import("std");
const handlers = @import("./handlers.zig");

pub const Brand = enum {
    Option,
    Flag,
    Argument,
    Command,
};

pub const ArgCount = union(enum) {
    // TODO: how is this different than .Some = 0?
    None: void,
    Some: u32,
    // TODO: how is this meaningfully different than .Some =  2 ** 32 - 1? (it
    // is unlikely anyone would specify 4 billion arguments on the command line,
    // or that the command line would tolerate such a thing particularly well)
    Many: void,
};

pub const ParameterArgs = struct {
    Output: type,
    UserContext: type,
};

pub fn Option(comptime args: ParameterArgs) type {
    // We use a combination of the resultType and default value to decide if an
    // option must be provided to the command line. The default is specified
    // when the type is constructed, so we cannot definitively decide it here.
    // It can be checked (along with the handler function) when constructing
    // the CommandResult type and thus be reasonably compile-time checked.

    comptime var result = struct {
        pub const brand: Brand = .Option;
        const mayBeOptional: bool = switch (@typeInfo(args.Output)) {
            .Optional => true,
            else => false,
        };
        pub const ResultType: type = args.Output;
        pub const ContextType: type = args.UserContext;

        name: []const u8,
        // Should this be unconditionally made an optional type? Adding an extra
        // layer of optional here doesn't seem to give us any advantage that I
        // can think of. An argument is optional if either mayBeOptional is true
        // or default is not null.
        default: (if (mayBeOptional) args.Output else ?args.Output) = null,
        // this is optional so that null can be provided as a default if there's
        // not a sane default handler that can be selected (or generated). The
        // handler can never actually be null, so we'll check for that when
        // creating CommandResult and cause a compileError there if the handler
        // is null. That will allow us to force unwrap these safely in the
        // parsing funcion.
        handler: ?handlers.HandlerType(args) = handlers.getDefaultHandler(args),
        short: ?*const [2]u8 = null,
        long: ?[]const u8 = null,
        help: ?[]const u8 = null,

        envVar: ?[]const u8 = null,
        hideResult: bool = false,

        // TODO: for ArgCount.Some > 1 semantics: automatically wrap args.Output
        // in an array? Eliminates the need for an allocator, but precludes
        // memory management techniques that may be better.
        args: ArgCount = .{ .Some = 1 },

        pub fn required(self: @This()) bool {
            return !@TypeOf(self).mayBeOptional and self.default == null;
        }
    };

    return result;
}

pub fn StringOption(comptime UserContext: type) type {
    return Option(.{ .Output = []const u8, .UserContext = UserContext });
}

// this could be Option(bool) except it allows truthy/falsy flag variants
// and it doesn't want to parse a value. with some contortions, it could be
// lowered into a pair of Option(bool), if we allowed multiple different
// arguments to specify the same output field name.

const ShortLong = struct {
    short: ?*const [2]u8 = null,
    long: ?[]const u8 = null,
};

// Flags don't have a conversion callback,
pub fn Flag(comptime UserContext: type) type {
    return struct {
        pub const brand: Brand = .Flag;
        // TODO: it may in some cases be useful to distinguish if the flag has been
        // entirely unspecified, but I can't think of any right now.
        pub const ResultType: type = bool;
        pub const ContextType: type = UserContext;

        name: []const u8,
        default: ?bool = false,
        truthy: ShortLong = .{},
        falsy: ShortLong = .{},
        help: ?[]const u8 = null,
        envVar: ?[]const u8 = null,
        hideResult: bool = false,
        eager: ?*const fn (UserContext, CommandData) anyerror!void = null,

        pub fn required(self: @This()) bool {
            if (self.default) return true;
            return false;
        }
    };
}

pub fn produceHelp(comptime UserContext: type) *const fn (UserContext, CommandData) anyerror!void {
    return struct {
        pub fn handler(_: UserContext, data: CommandData) !void {
            std.debug.print("{s}\n", .{data.help});
            std.process.exit(0);
        }
    }.handler;
}

// I haven't really figured out a way not to special case the help flag.
// Everything else assumes that it can be handled in a vacuum without worrying
// about intermediates (and must be so, as we don't have a deterministic order
// for assembling the result. We could make the parse order deterministic, but
// I suspect it would require increasing the parser complexity a fair amount).
// Flag types are created on the fly, so we can only actually hand pre-composed
// help text to whatever callback this provides.
pub const HelpFlagArgs = struct {
    name: []const u8 = "help",
    short: ?*const [2]u8 = "-h",
    long: ?[]const u8 = "--help",
    help: []const u8 = "print this help message",
};

pub fn HelpFlag(comptime UserContext: type, comptime args: HelpFlagArgs) Flag(UserContext) {
    return Flag(UserContext){
        .name = args.name,
        .truthy = .{ .short = args.short, .long = args.long },
        .help = args.help,
        .hideResult = true,
        .eager = produceHelp(UserContext),
    };
}

pub const defaultHelpFlag = HelpFlag(.{});

pub fn Argument(comptime args: ParameterArgs) type {
    // NOTE: optional arguments are kind of weird, since they're identified by
    // the order they're specified on the command line rather than by a named
    // flag. As long as the order is not violated, it's perfectly safe to omit
    // them if the provided specification supplies a default value.

    return struct {
        pub const brand: Brand = .Argument;
        const mayBeOptional: bool = switch (@typeInfo(args.Output)) {
            .Optional => true,
            else => false,
        };
        pub const ResultType: type = args.Output;
        pub const ContextType: type = args.UserContext;

        name: []const u8,
        default: (if (mayBeOptional) args.Output else ?args.Output) = null,
        handler: ?handlers.HandlerType(args) = handlers.getDefaultHandler(args),
        help: ?[]const u8 = null,
        hideResult: bool = false,
        // allow loading arguments from environmental variables? I don't think
        // it's possible to come up with sane semantics for this.

        pub fn required(self: @This()) bool {
            return !@TypeOf(self).mayBeOptional and self.default == null;
        }
    };
}

pub fn StringArg(comptime UserContext: type) type {
    return Argument(.{ .Output = []const u8, .UserContext = UserContext });
}

pub const CommandData = struct {
    pub const brand: Brand = .Command;

    name: []const u8,
    help: []const u8 = "",
    // cheesy way to allow deferred initialization of the subcommands
    subcommands: ?std.ArrayList(*CommandData) = null,
};
