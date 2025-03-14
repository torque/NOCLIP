pub const TokenContext = struct {
    forward_ddash: bool = false,
    short: Options,
    long: Options,
    positional: []const []const u8,
    subcommands: Subcommands,

    pub const Options = std.StaticStringMap(OptionContext);
    pub const Subcommands = std.StaticStringMap(*const TokenContext);

    pub const OptionContext = struct {
        global: NestLevel = .none,
        value: bool,
    };

    pub const NestLevel = enum(usize) {
        root = 0,
        none = std.math.maxInt(usize),
        _,

        pub fn wrap(lv: usize) NestLevel {
            return @enumFromInt(lv);
        }
        pub fn incr(self: NestLevel) NestLevel {
            return wrap(self.unwrap() + 1);
        }
        pub fn unwrap(self: NestLevel) usize {
            return @intFromEnum(self);
        }
    };
};

pub const Token = union(enum) {
    doubledash,
    short: u21,
    long: []const u8,
    shortvalue: struct { name: u21, value: []const u8 },
    longvalue: struct { name: []const u8, value: []const u8 },
    value: []const u8,
    subcommand: []const u8,

    pub fn dump(self: Token) void {
        switch (self) {
            .doubledash => std.debug.print("'--'\n", .{}),
            .short => |val| std.debug.print(".short => '{u}'\n", .{val}),
            .long => |val| std.debug.print(".long => \"{s}\"\n", .{val}),
            .shortvalue => |val| std.debug.print(".shortvalue => '{u}': \"{s}\"\n", .{ val.name, val.value }),
            .longvalue => |val| std.debug.print(".shortvalue => {s}: \"{s}\"\n", .{ val.name, val.value }),
            .value => |val| std.debug.print(".value => \"{s}\"\n", .{val}),
            .subcommand => |val| std.debug.print(".subcommand => \"{s}\"\n", .{val}),
        }
    }
};

const Assembler = struct {
    // this underallocates if fused short args are used and overallocates when
    // values are stored in a separate arg. it probably overallocates on
    // average, but we correct by growing it when fused arguments are
    // encountered, so it always overallocates
    tokens: std.ArrayListUnmanaged(Token) = .empty,
    // this overallocates in every case except the case where every argument is
    // a subcommand. There is no reason to change this after the initial
    // allocation.
    indices: [*]usize,
    len: usize,
    cap: usize,

    fn init(alloc: std.mem.Allocator, cap: usize) !Assembler {
        const idx = try alloc.alloc(usize, cap);
        errdefer alloc.free(idx);
        return .{
            .tokens = try .initCapacity(alloc, cap),
            .indices = idx.ptr,
            .len = 0,
            .cap = cap,
        };
    }

    fn addCapacity(self: *Assembler, alloc: std.mem.Allocator, extra: usize) !void {
        try self.tokens.ensureTotalCapacity(alloc, self.tokens.capacity + extra);
    }

    fn deinit(self: *Assembler, alloc: std.mem.Allocator) void {
        alloc.free(self.indices[0..self.cap]);
        self.tokens.deinit(alloc);
    }

    fn finish(self: *Assembler, alloc: std.mem.Allocator) ![]const Token {
        return try self.tokens.toOwnedSlice(alloc);
    }

    fn pushCommand(self: *Assembler) void {
        self.indices[self.len] = self.tokens.items.len;
        self.len += 1;
    }

    fn append(self: *Assembler, tok: Token) void {
        self.tokens.insertAssumeCapacity(self.indices[self.len - 1], tok);
        self.indices[self.len - 1] += 1;
    }

    fn insert(self: *Assembler, level: TokenContext.NestLevel, tok: Token) void {
        if (level == .none) {
            self.append(tok);
            return;
        }

        std.debug.assert(level.unwrap() < self.len);
        self.tokens.insertAssumeCapacity(self.indices[level.unwrap()], tok);
        for (level.unwrap()..self.len) |idx| {
            self.indices[idx] += 1;
        }
    }
};

// This tokenizer is very sloppy; it will happily create tokens that are
// mismatch the details of the TokenContext it has (e.g. it may produce a .short
// token without a value even if the context indicates that flag must produce a
// .shortvalue token). There are two reasons for this approach: the first is
// that tokenization is the wrong place to get persnickety about these details;
// the parser has a lot more context that it can use to produce useful errors
// when the token type mismatches its expectation. The seconds reason is that it
// allows us to use the tokenizer in situations where incomplete or incorrect
// input is expected and we want to get partial results, e.g. for an incomplete
// command line asking for completion options. Theoretically, the only true
// failure modes that the tokenizer can experience are allocation failures (OOM)
// and utf-8 decode failures.
//
// This is also the piece of code responsible for associating global parameters
// with the command that declares them. It is possible to do that here because
// the Parser guarantees that global parameters cannot be shadowed. This does
// generally make the true original order of the command line impossible to
// recover, although this could be rectified by keeping an index of when the
// token was actually encountered. Rearranging the globals here saves the need
// for a two-pass parsing strategy (though creating the tokens and then actually
// iterating the tokens is two passes, no parsing has to be done on the tokens,
// only value conversion).
//
// The produced list of tokens store references to the data contained in the
// provided argument vector. That is, the tokens do not own all of their memory,
// so the argument vector must be kept allocated until the end of the lifetime
// of the list of tokens.
pub fn tokenize(alloc: std.mem.Allocator, tokctx: *const TokenContext, argv: []const []const u8) ![]const Token {
    var assembler: Assembler = try .init(alloc, argv.len);
    defer assembler.deinit(alloc);
    assembler.pushCommand();

    var cmdctx: *const TokenContext = tokctx;
    var mode: enum { any, fused, ordered } = .any;
    var argit: mem.SliceIter([]const u8) = .{ .slice = argv };

    while (argit.pop()) |arg| {
        mod: switch (mode) {
            .any => if (std.mem.eql(u8, arg, "--") and !cmdctx.forward_ddash) {
                mode = .ordered;
            } else if (std.mem.startsWith(u8, arg, "--")) {
                const part = mem.partition(arg[2..], '=');
                if (part.rhs) |val| rhs: {
                    if (cmdctx.long.get(part.lhs)) |optctx| {
                        assembler.insert(optctx.global, .{
                            .longvalue = .{ .name = part.lhs, .value = val },
                        });
                        break :rhs;
                    }
                    assembler.append(
                        .{ .longvalue = .{ .name = part.lhs, .value = val } },
                    );
                } else norhs: {
                    if (cmdctx.long.get(part.lhs)) |optctx| {
                        if (optctx.value) {
                            if (argit.pop()) |val| {
                                assembler.insert(optctx.global, .{
                                    .longvalue = .{ .name = part.lhs, .value = val },
                                });
                                break :norhs;
                            }
                        }
                        assembler.insert(optctx.global, .{ .long = part.lhs });
                        break :norhs;
                    }
                    assembler.append(.{ .long = part.lhs });
                }
            } else if (std.mem.startsWith(u8, arg, "-") and arg.len > 1) {
                const cpcount = try std.unicode.utf8CountCodepoints(arg[1..]);
                if (cpcount > 1)
                    try assembler.addCapacity(alloc, cpcount);
                continue :mod .fused;
            } else {
                continue :mod .ordered;
            },
            .fused => {
                var iter: std.unicode.Utf8Iterator = .{ .bytes = arg[1..], .i = 0 };
                u8i: while (iter.nextCodepointSlice()) |cps| {
                    const codepoint = std.unicode.utf8Decode(cps) catch unreachable;
                    if (cmdctx.short.get(cps)) |optctx| {
                        if (optctx.value and iter.peek(1).len == 0) {
                            if (argit.pop()) |val| {
                                assembler.insert(optctx.global, .{
                                    .shortvalue = .{ .name = codepoint, .value = val },
                                });
                                continue :u8i;
                            }
                        }
                        assembler.insert(optctx.global, .{
                            .short = codepoint,
                        });
                        continue :u8i;
                    }
                    assembler.append(.{ .short = codepoint });
                }
            },
            .ordered => if (cmdctx.subcommands.get(arg)) |scmd| {
                mode = .any;
                cmdctx = scmd;
                assembler.pushCommand();
                assembler.append(.{ .subcommand = arg });
            } else {
                assembler.append(.{ .value = arg });
            },
        }
    }
    return try assembler.finish(alloc);
}

const std = @import("std");
const mem = @import("./mem.zig");

fn makeContext() *const TokenContext {
    const ToC = TokenContext.OptionContext;
    const Nl = TokenContext.NestLevel;

    const childa: TokenContext = .{
        .short = .initComptime(&.{
            .{ "z", ToC{ .global = .none, .value = false } },
            .{ "y", ToC{ .global = .none, .value = true } },
            .{ "x", ToC{ .global = .none, .value = false } },
            .{ "w", ToC{ .global = .none, .value = true } },
            // these are provided by the parent
            .{ "c", ToC{ .global = Nl.wrap(0), .value = false } },
            .{ "d", ToC{ .global = Nl.wrap(0), .value = true } },
        }),
        .long = .initComptime(&.{
            .{ "long-z", ToC{ .global = .none, .value = false } },
            .{ "global-a", ToC{ .global = Nl.wrap(0), .value = false } },
        }),
        .positional = &.{ "argument-z", "argument-y" },
        .subcommands = .initComptime(&.{}),
    };

    const ctx: TokenContext = .{
        .short = .initComptime(&.{
            .{ "a", ToC{ .global = .none, .value = false } },
            .{ "b", ToC{ .global = .none, .value = true } },
            // global arguments are not global on the command that defines them
            .{ "c", ToC{ .global = .none, .value = false } },
            .{ "d", ToC{ .global = .none, .value = true } },
        }),
        .long = .initComptime(&.{
            .{ "long-a", ToC{ .global = .none, .value = false } },
            .{ "global-a", ToC{ .global = .none, .value = false } },
        }),
        .positional = &.{},
        .subcommands = .initComptime(&.{
            .{ "subcommand-a", &childa },
        }),
    };

    return &ctx;
}

test "tokenize" {
    const alloc = std.testing.allocator;
    const context = comptime makeContext();

    {
        const tokens = try tokenize(alloc, context, &.{"-abc"});
        defer alloc.free(tokens);

        try std.testing.expectEqual(3, tokens.len);
        try std.testing.expectEqual(Token{ .short = 'a' }, tokens[0]);
        try std.testing.expectEqual(Token{ .short = 'b' }, tokens[1]);
        try std.testing.expectEqual(Token{ .short = 'c' }, tokens[2]);
    }

    {
        const tokens = try tokenize(alloc, context, &.{ "-abd", "dee" });
        defer alloc.free(tokens);

        try std.testing.expectEqual(3, tokens.len);
        try std.testing.expectEqual(Token{ .short = 'a' }, tokens[0]);
        try std.testing.expectEqual(Token{ .short = 'b' }, tokens[1]);
        try std.testing.expectEqual(Token{ .shortvalue = .{ .name = 'd', .value = "dee" } }, tokens[2]);
    }

    {
        const tokens = try tokenize(alloc, context, &.{ "-cba", "dee" });
        defer alloc.free(tokens);

        try std.testing.expectEqual(4, tokens.len);
        try std.testing.expectEqual(Token{ .short = 'c' }, tokens[0]);
        try std.testing.expectEqual(Token{ .short = 'b' }, tokens[1]);
        try std.testing.expectEqual(Token{ .short = 'a' }, tokens[2]);
        try std.testing.expectEqual(Token{ .value = "dee" }, tokens[3]);
    }

    {
        const tokens = try tokenize(alloc, context, &.{ "-acb", "dee", "-d", "-zyx" });
        defer alloc.free(tokens);

        try std.testing.expectEqual(4, tokens.len);
        try std.testing.expectEqual(Token{ .short = 'a' }, tokens[0]);
        try std.testing.expectEqual(Token{ .short = 'c' }, tokens[1]);
        try std.testing.expectEqual(Token{ .shortvalue = .{ .name = 'b', .value = "dee" } }, tokens[2]);
        try std.testing.expectEqual(Token{ .shortvalue = .{ .name = 'd', .value = "-zyx" } }, tokens[3]);
    }

    {
        const tokens = try tokenize(alloc, context, &.{ "-a", "-c", "subcommand-a", "-d", "dee", "-zyx", "--global-a" });
        defer alloc.free(tokens);

        try std.testing.expectEqual(8, tokens.len);
        try std.testing.expectEqual(Token{ .short = 'a' }, tokens[0]);
        try std.testing.expectEqual(Token{ .short = 'c' }, tokens[1]);
        try std.testing.expectEqual(Token{ .shortvalue = .{ .name = 'd', .value = "dee" } }, tokens[2]);
        try std.testing.expectEqualDeep(Token{ .long = "global-a" }, tokens[3]);
        try std.testing.expectEqual(Token{ .subcommand = "subcommand-a" }, tokens[4]);
        try std.testing.expectEqual(Token{ .short = 'z' }, tokens[5]);
        try std.testing.expectEqual(Token{ .short = 'y' }, tokens[6]);
        try std.testing.expectEqual(Token{ .short = 'x' }, tokens[7]);
    }
}

// parameter styles to accept:
// --name value
// --name=value
// -n value
// -fused (parsed as -f -u -s -e -d)
// -fused value (parsed as -f -u -s -e -d value)
// ordered
// a named parameter can only take zero or one values. Establish a convention for compound values:
// --name val1,val2
// --name=val1,val2
// --name="val1, val2" (probably should not consume whitespace, since the user has to go out of their way to do quoting for it)
// --name key1=val1,key2=val2
// --name=key1=val1,key2=val2 (should be familiar from docker)
