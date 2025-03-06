pub const CommandOptions = struct {
    context_type: type = void,

    default_help_flags: bool = true,
    create_help_command: enum { always, never, if_subcommands } = .if_subcommands,
    create_completion_helper: bool = true,
    allow_colored_output: bool = true,
    output_strategy: enum { type, iterator } = .type,
    parse_error_behavior: enum { exit, propagate } = .propagate,
    // pop the callback stack after parsing all arguments for the current subcommand
    pipeline_subcommands: bool = false,
};

const __Canary = opaque {};

pub const ErrorReport = struct {
    message: []const u8,
};

pub fn Status(comptime T: type) type {
    return union(enum) {
        success: T,
        failure: ErrorReport,

        pub fn succeed(arg: T) @This() {
            return .{ .success = arg };
        }

        pub fn failFull(arg: ErrorReport) @This() {
            return .{ .failure = arg };
        }

        pub fn fail(msg: []const u8) @This() {
            return .{ .failure = .{ .message = msg } };
        }
    };
}

pub const String = struct {
    bytes: []const u8,
};

pub const Codepoint = u21;

pub const ParameterType = enum {
    bool_group,
    constant,
    counter,
    // counter
    // fixed_value
    // aggregate_flag
    option,
    // aggregate_option
    argument,
    // aggregate_argument
    group,
};

pub const Scope = enum { local, global };

pub const MultiMode = enum {
    first,
    last,
    accumulate,
    count,
};

pub fn Accumulate(comptime T: type) type {
    return struct {
        const __noclip_canary__ = __Canary;

        pub const Result: type = T;
        pub const multi_mode: MultiMode = .accumulate;
    };
}

pub fn Count(comptime T: type) type {
    if (!@typeInfo(T) == .int) unreachable;
    return struct {
        const __noclip_canary__ = __Canary;

        pub const Result: type = T;
        pub const multi_mode: MultiMode = .count;
    };
}

pub const BoolGroup = struct {
    pub const Result = bool;
    pub const param_type: ParameterType = .bool_group;
    pub const multi_mode: MultiMode = .last;

    // accessors to easily read decls from an instance
    pub fn Type(comptime _: *const BoolGroup) type {
        return BoolGroup.Result;
    }
    pub fn mode(comptime _: *const BoolGroup) MultiMode {
        return BoolGroup.multi_mode;
    }

    description: []const u8 = "",
    truthy: Pair = .{},
    falsy: Pair = .{},
    env: ?[]const u8 = null,
    /// If true, at least one of the variants of the flag must be provided by
    /// the user on the command line, otherwise a parse error will be produced.
    required: bool = false,
    /// A default value that will be forwarded if the option is not provided on
    /// the command line by the user. If a default is provided, then the
    /// corresponding parsed value will not be optional. Note that flags are
    /// tri-state values that may be `null`, `true`, or `false`. `null` will
    /// never be forwarded if this is set to `true` or `false`, as `null` only
    /// indicates that the flag was not specified on the command line.
    default: ?Result = null,
    // multi: Multi = .last,
    scope: Scope = .local,
    eager: bool = false,
    hidden: bool = false,

    pub const Pair = struct {
        /// a single unicode codepoint that identifies this flag on the command
        /// line, e.g. 'v'.
        short: ?Codepoint = null,
        /// a string, beginning with the long flag sequence `--` that identifies
        /// this flag on the command line, e.g. "--version". Multiple words
        /// should be skewercase, i.e. "--multiple-words".
        long: ?[]const u8 = null,
    };
};

// figure this out: this is a zero-parameter flag that produces a non-boolean
// value, e.g. an int. for like -9 on gz. A flag is just a FixedValue with
pub fn Constant(comptime R: type) type {
    return struct {
        const Self = @This();
        pub const Result = ScryResultType(R);
        pub const param_type: ParameterType = .constant;
        pub const multi_mode: MultiMode = scryMode(R);

        // accessors to easily read decls from an instance
        pub fn Type(comptime _: *const Self) type {
            return Self.Result;
        }
        pub fn mode(comptime _: *const Self) MultiMode {
            return Self.multi_mode;
        }

        description: []const u8 = "",
        short: ?Codepoint = null,
        long: ?[]const u8 = null,
        env: ?[]const u8 = null,
        /// Require that the user always provide a value for this option on the
        /// command line.
        required: bool = false,
        /// The value associated with this flag
        value: Result,
        scope: Scope = .local,
        eager: bool = false,
        hidden: bool = false,
    };
}

pub fn Counter(comptime R: type) type {
    return struct {
        const Self = @This();
        pub const Result = ScryResultType(R);
        pub const param_type: ParameterType = .counter;
        pub const multi_mode: MultiMode = scryMode(R);

        // accessors to easily read decls from an instance
        pub fn Type(comptime _: *const Self) type {
            return Self.Result;
        }
        pub fn mode(comptime _: *const Self) MultiMode {
            return Self.multi_mode;
        }

        description: []const u8 = "",
        short: ?Codepoint = null,
        long: ?[]const u8 = null,
        env: ?[]const u8 = null,
        /// Require that the user always provide a value for this option on the
        /// command line.
        required: bool = false,
        /// The value associated with this flag
        increment: Result = 1,
        scope: Scope = .local,
        eager: bool = false,
        hidden: bool = false,
    };
}

pub fn Option(comptime R: type) type {
    return struct {
        const Self = @This();
        pub const Result = ScryResultType(R);
        pub const param_type: ParameterType = .option;
        pub const multi_mode: MultiMode = scryMode(R);

        // accessors to easily read decls from an instance
        pub fn Type(comptime _: *const Self) type {
            return Self.Result;
        }
        pub fn mode(comptime _: *const Self) MultiMode {
            return Self.multi_mode;
        }

        description: []const u8 = "",
        short: ?Codepoint = null,
        long: ?[]const u8 = null,
        env: ?[]const u8 = null,
        /// Require that the user always provide a value for this option on the
        /// command line.
        required: bool = false,
        /// A default value that will be forwarded if the option is not provided
        /// on the command line by the user. If a default is provided, then the
        /// corresponding parsed value will not be optional.
        default: DefaultType(R) = defaultTypeDefault(R),
        /// note: .count is only valid for flags
        /// note: .accumulate requires R to be a slice
        // multi: Multi = .last,
        scope: Scope = .local,
        eager: bool = false,
        hidden: bool = false,
    };
}

pub fn Argument(comptime R: type) type {
    return struct {
        const Self = @This();
        pub const Result = ScryResultType(R);
        pub const param_type: ParameterType = .argument;
        pub const multi_mode: MultiMode = scryMode(R);

        // accessors to easily read decls from an instance
        pub fn Type(comptime _: *const Self) type {
            return Self.Result;
        }
        pub fn mode(comptime _: *const Self) MultiMode {
            return Self.multi_mode;
        }

        description: []const u8 = "",
    };
}

pub fn Group(comptime R: type) type {
    return struct {
        const Self = @This();
        pub const Result = ScryResultType(R);
        pub const multi_mode: MultiMode = scryMode(R);
        pub const param_type: ParameterType = .group;

        // accessors to easily read decls from an instance
        pub fn Type(comptime _: *const Self) type {
            return Self.Result;
        }
        pub fn mode(comptime _: *const Self) MultiMode {
            return Self.multi_mode;
        }

        description: []const u8 = "",
        env: ?[]const u8 = null,
        /// at least one of the parameters in the group must be provided
        required: bool = false,
        parameters: type,

        pub fn validate(self: @This()) Status(void) {
            comptime {
                for (@typeInfo(@TypeOf(self.parameters)).Struct.decls) |td| {
                    const decl = @field(@TypeOf(self.parameters), td.name);
                    std.debug.assert(decl.Result == Result);
                }
            }
        }
    };
}

fn hasCanary(comptime T: type) bool {
    return @hasDecl(T, "__noclip_canary__") and T.__noclip_canary__ == __Canary;
}

pub fn scryMode(comptime T: type) MultiMode {
    return if (hasCanary(T))
        T.multi_mode
    else
        .last;
}

pub fn ScryResultType(comptime T: type) type {
    return if (hasCanary(T)) switch (T.multi_mode) {
        .accumulate => []const T.Result,
        .count, .first, .last => T.Result,
    } else T;
}

pub fn DefaultType(comptime T: type) type {
    return if (hasCanary(T)) switch (T.multi_mode) {
        .accumulate => []const T.Result,
        .count => T.Result,
        .first, .last => ?T.Result,
    } else ?T;
}

pub fn defaultTypeDefault(comptime T: type) DefaultType(T) {
    return if (hasCanary(T)) switch (T.multi_mode) {
        .accumulate => &.{},
        .count => 0,
        .first, .last => null,
    } else null;
}

pub fn execute(comptime spec: type, args: ExecArgs(spec)) void {
    var parser = Parser(spec, true).init(args.alloc, args.context);
    defer parser.deinit();
    switch (parser.parse(args.args, args.env)) {
        .success => |callstack| {
            for (callstack) |runner| {
                try runner();
            }
        },
        .fail => |report| {
            switch (spec.info.options.parse_error_behavior) {
                .exit => {},
                .propagate => {
                    if (@hasField(spec, "err"))
                        spec.err(report);
                },
            }
        },
    }
}

pub fn ExecArgs(comptime spec: type) type {
    return struct {
        alloc: std.mem.Allocator,
        args: []const [:0]const u8,
        env: std.process.EnvMap,
        context: spec.info.context_type,
    };
}

pub fn ReturnType(comptime spec: type) type {
    const info = @typeInfo(@TypeOf(spec.run)).Fn;
    return switch (@typeInfo(info.return_type.?)) {
        .ErrorUnion => |eu| blk: {
            if (eu.payload != void) unreachable;
            break :blk info.return_type.?;
        },
        .Void => void,
        else => unreachable,
    };
}

pub const Parser = @import("./parser.zig");
const std = @import("std");
