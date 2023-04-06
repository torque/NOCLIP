// this borrows code from zig-doctest
// zig-doctest is distributed under the MIT license Copyright (c) 2020 Loris Cro
// see: https://github.com/kristoff-it/zig-doctest/blob/db507d803dd23e2585166f5b7e479ffc96d8b5c9/LICENSE

const noclip = @import("noclip");
const std = @import("std");
const mem = std.mem;
const fs = std.fs;
const print = std.debug.print;

inline fn escape_char(out: anytype, char: u8) !void {
    return try switch (char) {
        '&' => out.writeAll("&amp;"),
        '<' => out.writeAll("&lt;"),
        '>' => out.writeAll("&gt;"),
        '"' => out.writeAll("&quot;"),
        else => out.writeByte(char),
    };
}

fn write_escaped(out: anytype, input: []const u8, class: TokenClass) !void {
    if (class == .whitespace) {
        try write_whitespace(out, input);
    } else {
        for (input) |c| try escape_char(out, c);
    }
}

fn write_whitespace(out: anytype, input: []const u8) !void {
    var state: enum { normal, maybe_comment, maybe_docstring, comment } = .normal;

    for (input) |c| {
        switch (state) {
            .normal => switch (c) {
                '/' => state = .maybe_comment,
                '\n' => try out.writeAll("</span>\n<span class=\"line\">"),
                else => try escape_char(out, c),
            },
            .maybe_comment => switch (c) {
                '/' => {
                    state = .maybe_docstring;
                },
                '\n' => {
                    try out.writeAll("</span>\n<span class=\"line\">");
                    state = .normal;
                },
                else => {
                    try out.writeByte('/');
                    try escape_char(out, c);
                    state = .normal;
                },
            },
            .maybe_docstring => switch (c) {
                '\n' => {
                    // actually it was an empty comment lol cool
                    try out.writeAll("<span class=\"comment\">//</span></span>\n<span class=\"line\">");
                    state = .normal;
                },
                '/', '!' => {
                    // it is a docstring, so don't respan it
                    try out.writeAll("//");
                    try out.writeByte(c);
                    state = .normal;
                },
                else => {
                    // this is also a comment
                    try out.writeAll("<span class=\"comment\">//");
                    try escape_char(out, c);
                    state = .comment;
                },
            },
            .comment => switch (c) {
                '\n' => {
                    try out.writeAll("</span></span>\n<span class=\"line\">");
                    state = .normal;
                },
                else => {
                    try escape_char(out, c);
                },
            },
        }
    }
}

// TODO: use more context to get better token resolution
// identifier preceded by (break | continue) colon is a label
// identifier followed by colon (inline | for | while | l_brace) is a label
//
// identifier preceded by dot, not preceded by name, and followed by (, | => | == | != | rbrace | rparen | and | or | ;) is an enum literal
// identifier preceded by dot and followed by = is a struct field initializer
//
// true, false, null are not keywords but we should be able to treat them as literals. They should all be tokenized as identifiers
//
// identifier followed by ( is always a function call
//
// identifier preceded by : is a type until = or , or ) (except after [, where its the terminator)
// identifier followed by { is a type
// identifier after | is a bind

const ContextToken = struct {
    tag: std.zig.Token.Tag,
    content: []const u8,
    class: TokenClass = .needs_context,
};

const TokenClass = enum {
    keyword,
    string,
    builtin,
    type,
    function,
    label,
    doc_comment,
    literal_primitive,
    literal_number,
    symbology,
    whitespace,
    context_free,

    needs_context,

    pub fn name(self: @This()) []const u8 {
        return switch (self) {
            .doc_comment => "doc comment",
            .literal_primitive => "literal primitive",
            .literal_number => "literal number",
            .symbology => "",
            .context_free => "",
            .whitespace => "",
            .needs_context => @panic("too late"),
            else => @tagName(self),
        };
    }
};

pub const ContextManager = struct {
    // const Queue = std.TailQueue(ContextToken);

    tokens: std.ArrayList(ContextToken),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .allocator = allocator,
            .tokens = std.ArrayList(ContextToken).init(allocator),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.tokens.deinit();
    }

    pub fn push_back(self: *@This(), token: ContextToken) !void {
        try self.tokens.append(token);
    }

    fn print_span(content: []const u8, class: TokenClass, out: anytype) !void {
        const classname = class.name();

        if (classname.len > 0) {
            try out.print("<span class=\"{s}\">", .{classname});
            try write_escaped(out, content, class);
            try out.writeAll("</span>");
        } else {
            try write_escaped(out, content, class);
        }
    }

    fn print_fused_span(tokens: []ContextToken, start: usize, end: usize, out: anytype) !void {
        const classname = tokens[start].class.name();

        if (classname.len > 0) try out.print("<span class=\"{s}\">", .{classname});

        for (tokens[start..end]) |*token| {
            try write_escaped(out, token.content, tokens[start].class);
        }

        if (classname.len > 0) try out.writeAll("</span>");
    }

    pub fn process(self: *@This(), out: anytype) !void {
        const tokens = self.tokens.items;
        if (tokens.len == 0) return;

        for (tokens, 0..) |*token, idx| {
            if (token.class == .needs_context)
                if (!contextualize_identifier(tokens, idx)) @panic("failed to context");
        }

        var idx: usize = 0;
        while (idx < tokens.len) : (idx += 1) {
            const span_start = idx;
            const token = &tokens[idx];
            // std.debug.print("tok {d}: {s} {}\n", .{ idx, token.content, token.class });

            var lookahead = idx + 1;
            while (lookahead < tokens.len) : (lookahead += 1) {
                // std.debug.print("look {d}: {s} {}\n", .{ lookahead, tokens[lookahead].content, tokens[lookahead].class });
                if (tokens[lookahead].class != .whitespace) {
                    if (tokens[lookahead].class == token.class)
                        idx = lookahead
                    else
                        break;
                } else {
                    if (std.mem.containsAtLeast(u8, tokens[lookahead].content, 1, "\n")) break;
                }
            }
            if (idx > span_start) {
                try print_fused_span(tokens, span_start, idx + 1, out);
            } else {
                try print_span(token.content, token.class, out);
            }
        }
    }

    fn contextualize_identifier(tokens: []ContextToken, current: usize) bool {
        return (contextualize_function(tokens, current) or
            contextualize_builtin_type(tokens, current) or
            contextualize_label(tokens, current) or
            contextualize_fallback(tokens, current));
    }

    fn contextualize_function(tokens: []ContextToken, current: usize) bool {
        const prev = prev_valid(tokens, current) orelse return false;

        if (tokens[prev].tag == .keyword_fn) {
            tokens[current].class = .function;
            return true;
        }

        return false;
    }

    fn contextualize_builtin_type(tokens: []ContextToken, current: usize) bool {
        const content = tokens[current].content;

        const is_int = blk: {
            if ((content[0] != 'i' and content[0] != 'u') or content.len < 2 or content.len > 6)
                break :blk false;

            for (content[1..]) |char|
                if (char < '0' or char > '9') break :blk false;

            break :blk true;
        };

        if (is_int or is_type(content)) {
            tokens[current].class = .type;
            return true;
        }

        return false;
    }

    fn contextualize_label(tokens: []ContextToken, current: usize) bool {
        blk: {
            const prev = prev_valid(tokens, current) orelse break :blk;

            if (tokens[prev].tag == .colon) {
                const prev2 = prev_valid(tokens, prev) orelse break :blk;

                switch (tokens[prev2].tag) {
                    .keyword_break, .keyword_continue => {
                        tokens[prev].class = .label;
                        tokens[current].class = .label;
                        return true;
                    },
                    else => break :blk,
                }
            }
        }

        blk: {
            const next = next_valid(tokens, current) orelse break :blk;

            if (tokens[next].tag == .colon) {
                const next2 = next_valid(tokens, next) orelse break :blk;

                switch (tokens[next2].tag) {
                    .keyword_inline, .keyword_for, .keyword_while, .l_brace => {
                        tokens[current].class = .label;
                        tokens[next].class = .label;
                        return true;
                    },
                    else => break :blk,
                }
            }
        }

        return false;
    }

    fn contextualize_fallback(tokens: []ContextToken, current: usize) bool {
        tokens[current].class = .context_free;
        return true;
    }

    fn next_valid(tokens: []ContextToken, current: usize) ?usize {
        var check = current + 1;
        while (check < tokens.len) : (check += 1) {
            if (tokens[check].class != .whitespace) return check;
        }
        return null;
    }

    fn prev_valid(tokens: []ContextToken, current: usize) ?usize {
        if (current == 0) return null;

        var check = current - 1;
        while (check > 0) : (check -= 1) {
            if (tokens[check].class != .whitespace) return check;
        }
        if (tokens[check].class != .whitespace) return check;
        return null;
    }
};

pub fn trimZ(comptime T: type, input: [:0]T, trimmer: []const T) [:0]T {
    var begin: usize = 0;
    var end: usize = input.len;
    while (begin < end and std.mem.indexOfScalar(T, trimmer, input[begin]) != null) : (begin += 1) {}
    while (end > begin and std.mem.indexOfScalar(T, trimmer, input[end - 1]) != null) : (end -= 1) {}
    input[end] = 0;
    return input[begin..end :0];
}

pub fn write_tokenized_html(raw_src: [:0]u8, allocator: std.mem.Allocator, out: anytype, full: bool) !void {
    const src = trimZ(u8, raw_src, "\n");
    var tokenizer = std.zig.Tokenizer.init(src);
    var last_token_end: usize = 0;

    if (full) try out.writeAll(html_preamble);
    try out.writeAll("<pre class=\"code-markup\"><code class=\"lang-zig\"><span class=\"line\">");
    var manager = ContextManager.init(allocator);
    defer manager.deinit();

    while (true) {
        const token = tokenizer.next();
        if (last_token_end < token.loc.start) {
            try manager.push_back(.{
                .tag = .invalid, // TODO: this is a big hack
                .content = src[last_token_end..token.loc.start],
                .class = .whitespace,
            });
        }

        switch (token.tag) {
            .eof => break,

            .keyword_addrspace,
            .keyword_align,
            .keyword_and,
            .keyword_asm,
            .keyword_async,
            .keyword_await,
            .keyword_break,
            .keyword_catch,
            .keyword_comptime,
            .keyword_const,
            .keyword_continue,
            .keyword_defer,
            .keyword_else,
            .keyword_enum,
            .keyword_errdefer,
            .keyword_error,
            .keyword_export,
            .keyword_extern,
            .keyword_for,
            .keyword_if,
            .keyword_inline,
            .keyword_noalias,
            .keyword_noinline,
            .keyword_nosuspend,
            .keyword_opaque,
            .keyword_or,
            .keyword_orelse,
            .keyword_packed,
            .keyword_anyframe,
            .keyword_pub,
            .keyword_resume,
            .keyword_return,
            .keyword_linksection,
            .keyword_callconv,
            .keyword_struct,
            .keyword_suspend,
            .keyword_switch,
            .keyword_test,
            .keyword_threadlocal,
            .keyword_try,
            .keyword_union,
            .keyword_unreachable,
            .keyword_usingnamespace,
            .keyword_var,
            .keyword_volatile,
            .keyword_allowzero,
            .keyword_while,
            .keyword_anytype,
            .keyword_fn,
            => try manager.push_back(.{
                .tag = token.tag,
                .content = src[token.loc.start..token.loc.end],
                .class = .keyword,
            }),

            .string_literal,
            .char_literal,
            => try manager.push_back(.{
                .tag = token.tag,
                .content = src[token.loc.start..token.loc.end],
                .class = .string,
            }),

            .multiline_string_literal_line => {
                try manager.push_back(.{
                    .tag = token.tag,
                    .content = src[token.loc.start .. token.loc.end - 1],
                    .class = .string,
                });
                // multiline string literals contain a newline, but we don't want to
                // tokenize it like that.
                try manager.push_back(.{
                    .tag = .invalid,
                    .content = src[token.loc.end - 1 .. token.loc.end],
                    .class = .whitespace,
                });
            },

            .builtin => try manager.push_back(.{
                .tag = token.tag,
                .content = src[token.loc.start..token.loc.end],
                .class = .builtin,
            }),

            .doc_comment,
            .container_doc_comment,
            => {
                try manager.push_back(.{
                    .tag = token.tag,
                    .content = src[token.loc.start..token.loc.end],
                    .class = .doc_comment,
                });
            },

            .identifier => {
                const content = src[token.loc.start..token.loc.end];
                try manager.push_back(.{
                    .tag = token.tag,
                    .content = content,
                    .class = if (mem.eql(u8, content, "undefined") or
                        mem.eql(u8, content, "null") or
                        mem.eql(u8, content, "true") or
                        mem.eql(u8, content, "false"))
                        .literal_primitive
                    else
                        .needs_context,
                });
            },

            .number_literal => try manager.push_back(.{
                .tag = token.tag,
                .content = src[token.loc.start..token.loc.end],
                .class = .literal_number,
            }),

            .bang,
            .pipe,
            .pipe_pipe,
            .pipe_equal,
            .equal,
            .equal_equal,
            .equal_angle_bracket_right,
            .bang_equal,
            .l_paren,
            .r_paren,
            .semicolon,
            .percent,
            .percent_equal,
            .l_brace,
            .r_brace,
            .l_bracket,
            .r_bracket,
            .period,
            .period_asterisk,
            .ellipsis2,
            .ellipsis3,
            .caret,
            .caret_equal,
            .plus,
            .plus_plus,
            .plus_equal,
            .plus_percent,
            .plus_percent_equal,
            .minus,
            .minus_equal,
            .minus_percent,
            .minus_percent_equal,
            .asterisk,
            .asterisk_equal,
            .asterisk_asterisk,
            .asterisk_percent,
            .asterisk_percent_equal,
            .arrow,
            .colon,
            .slash,
            .slash_equal,
            .comma,
            .ampersand,
            .ampersand_equal,
            .question_mark,
            .angle_bracket_left,
            .angle_bracket_left_equal,
            .angle_bracket_angle_bracket_left,
            .angle_bracket_angle_bracket_left_equal,
            .angle_bracket_right,
            .angle_bracket_right_equal,
            .angle_bracket_angle_bracket_right,
            .angle_bracket_angle_bracket_right_equal,
            .tilde,
            .plus_pipe,
            .plus_pipe_equal,
            .minus_pipe,
            .minus_pipe_equal,
            .asterisk_pipe,
            .asterisk_pipe_equal,
            .angle_bracket_angle_bracket_left_pipe,
            .angle_bracket_angle_bracket_left_pipe_equal,
            => try manager.push_back(.{
                .tag = token.tag,
                .content = src[token.loc.start..token.loc.end],
                .class = .symbology,
            }),

            .invalid,
            .invalid_periodasterisks,
            => return parseError(src, token, "syntax error", .{}),
        }

        last_token_end = token.loc.end;
    }

    try manager.process(out);

    try out.writeAll("</span></code></pre>");
    if (full) try out.writeAll(html_epilogue);
}

// TODO: this function returns anyerror, interesting
fn parseError(src: []const u8, token: std.zig.Token, comptime fmt: []const u8, args: anytype) anyerror {
    const loc = getTokenLocation(src, token);
    // const args_prefix = .{ tokenizer.source_file_name, loc.line + 1, loc.column + 1 };
    // print("{s}:{d}:{d}: error: " ++ fmt ++ "\n", args_prefix ++ args);

    const args_prefix = .{ loc.line + 1, loc.column + 1 };
    print("{d}:{d}: error: " ++ fmt ++ "\n", args_prefix ++ args);
    if (loc.line_start <= loc.line_end) {
        print("{s}\n", .{src[loc.line_start..loc.line_end]});
        {
            var i: usize = 0;
            while (i < loc.column) : (i += 1) {
                print(" ", .{});
            }
        }
        {
            const caret_count = token.loc.end - token.loc.start;
            var i: usize = 0;
            while (i < caret_count) : (i += 1) {
                print("~", .{});
            }
        }
        print("\n", .{});
    }
    return error.ParseError;
}

const builtin_types = [_][]const u8{
    "f16",         "f32",      "f64",    "f128",     "c_longdouble", "c_short",
    "c_ushort",    "c_int",    "c_uint", "c_long",   "c_ulong",      "c_longlong",
    "c_ulonglong", "c_char",   "c_void", "void",     "bool",         "isize",
    "usize",       "noreturn", "type",   "anyerror", "comptime_int", "comptime_float",
};

fn is_type(name: []const u8) bool {
    for (builtin_types) |t| {
        if (mem.eql(u8, t, name))
            return true;
    }
    return false;
}

const Location = struct {
    line: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
};

fn getTokenLocation(src: []const u8, token: std.zig.Token) Location {
    var loc = Location{
        .line = 0,
        .column = 0,
        .line_start = 0,
        .line_end = 0,
    };
    for (src, 0..) |c, i| {
        if (i == token.loc.start) {
            loc.line_end = i;
            while (loc.line_end < src.len and src[loc.line_end] != '\n') : (loc.line_end += 1) {}
            return loc;
        }
        if (c == '\n') {
            loc.line += 1;
            loc.column = 0;
            loc.line_start = i + 1;
        } else {
            loc.column += 1;
        }
    }
    return loc;
}

const html_preamble =
    \\<!DOCTYPE html>
    \\<html>
    \\    <head>
    \\        <style>
    \\:root {
    \\    --background: #2D2D2D;
    \\    --foreground: #D3D0C8;
    \\    --red: #F2777A;
    \\    --orange: #F99157;
    \\    --yellow: #FFCC66;
    \\    --green: #99CC99;
    \\    --aqua: #66CCCC;
    \\    --blue: #6699CC;
    \\    --purple: #CC99CC;
    \\    --pink: #FFCCFF;
    \\    --gray: #747369;
    \\}
    \\body {
    \\    background: var(--background);
    \\    color: var(--foreground);
    \\}
    \\.code-markup {
    \\    padding: 0;
    \\    font-size: 16pt;
    \\    line-height: 1.1;
    \\}
    \\.code-markup .keyword { color: var(--purple); }
    \\.code-markup .type { color: var(--purple); }
    \\.code-markup .builtin { color: var(--aqua); }
    \\.code-markup .string { color: var(--green); }
    \\.code-markup .comment { color: var(--gray); }
    \\.code-markup .literal { color: var(--orange); }
    \\.code-markup .name { color: var(--red); }
    \\.code-markup .function { color: var(--blue); }
    \\.code-markup .label { color: var(--yellow); }
    \\        </style>
    \\    </head>
    \\    <body>
;

const html_epilogue =
    \\
    \\    </body>
    \\</html>
;

const tokenator = cmd: {
    var cmd = noclip.CommandBuilder(TokCtx){
        .description =
        \\Tokenize one or more zig files into HTML.
        \\
        \\Each file provided on the command line will be tokenized and the output will
        \\be written to [filename].html. For example, 'tokenator foo.zig bar.zig' will
        \\write foo.zig.html and bar.zig.html. Files are written directly, and if an
        \\error occurs while processing a file, partial output will occur. When
        \\processing multiple files, a failure will exit without processing any
        \\successive files. Inputs should be less than 1MB in size.
        \\
        \\If the --stdout flag is provided, output will be written to the standard
        \\output instead of to named files. Each file written to stdout will be
        \\followed by a NUL character which acts as a separator for piping purposes.
        ,
    };
    cmd.simple_flag(.{
        .name = "write_stdout",
        .truthy = .{ .long_tag = "--stdout" },
        .default = false,
        .description = "write output to stdout instead of to files",
    });
    cmd.simple_flag(.{
        .name = "full",
        .truthy = .{ .short_tag = "-f", .long_tag = "--full" },
        .default = false,
        .description = "write full HTML files rather than just the pre fragment",
    });
    cmd.add_argument(.{ .OutputType = []const u8, .multi = true }, .{ .name = "files" });
    break :cmd cmd;
};

const TokCtx = struct {
    allocator: std.mem.Allocator,
};

fn tokenize_files(context: *TokCtx, parameters: tokenator.Output()) !void {
    const stdout = std.io.getStdOut().writer();

    for (parameters.files.items) |file_name| {
        const srcbuf = blk: {
            const file = try fs.cwd().openFile(file_name, .{ .mode = .read_only });
            defer file.close();

            break :blk try file.readToEndAllocOptions(
                context.allocator,
                1_000_000,
                null,
                @alignOf(u8),
                0,
            );
        };
        defer context.allocator.free(srcbuf);

        if (parameters.write_stdout) {
            try write_tokenized_html(srcbuf, context.allocator, stdout, parameters.full);
            try stdout.writeByte(0);
        } else {
            const outname = try std.mem.join(context.allocator, ".", &[_][]const u8{ file_name, "html" });
            print("writing: {s}\n", .{outname});
            defer context.allocator.free(outname);

            const output = try fs.cwd().createFile(outname, .{});
            defer output.close();

            try write_tokenized_html(srcbuf, context.allocator, output.writer(), parameters.full);
        }
    }
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var ctx = TokCtx{ .allocator = allocator };

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    var cli_parser = tokenator.create_parser(tokenize_files, arena.allocator());
    try cli_parser.execute(&ctx);

    return 0;
}
