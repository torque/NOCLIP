// this borrows code from zig-doctest
// zig-doctest is distributed under the MIT license Copyright (c) 2020 Loris Cro
// see: https://github.com/kristoff-it/zig-doctest/blob/db507d803dd23e2585166f5b7e479ffc96d8b5c9/LICENSE

const noclip = @import("noclip");
const std = @import("std");
const mem = std.mem;
const fs = std.fs;
const print = std.debug.print;

fn write_escaped(out: anytype, input: []const u8) !void {
    var comment = false;

    for (input, 0..) |c, idx| {
        switch (c) {
            '&' => try out.writeAll("&amp;"),
            '<' => try out.writeAll("&lt;"),
            '>' => try out.writeAll("&gt;"),
            '"' => try out.writeAll("&quot;"),
            '\n' => {
                if (comment) {
                    try out.writeAll("</span>");
                    comment = false;
                }
                try out.writeAll("</span>\n<span class=\"line\">");
            },
            '/' => {
                if (input[idx + 1] == '/') {
                    try out.writeAll("<span class=\"comment\">");
                    comment = true;
                }
                try out.writeByte('/');
            },
            else => try out.writeByte(c),
        }
    }
}

// TODO: use more context to get better token resolution
// identifier preceded by (break | continue) colon is a label
// identifier followed by colon (inline | for | while | {) is a label
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

pub fn write_tokenized_html(src: [:0]const u8, _: std.mem.Allocator, out: anytype) !void {
    try out.writeAll("<pre class=\"code-markup\"><code class=\"lang-zig\"><span class=\"line\">");
    var tokenizer = std.zig.Tokenizer.init(src);
    var index: usize = 0;
    var next_tok_is_fn = false;
    while (true) {
        const prev_tok_was_fn = next_tok_is_fn;
        next_tok_is_fn = false;

        const token = tokenizer.next();
        // short circuit on EOF to avoid
        if (token.tag == .eof) break;

        try write_escaped(out, src[index..token.loc.start]);
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
            => {
                try out.writeAll("<span class=\"keyword\">");
                try write_escaped(out, src[token.loc.start..token.loc.end]);
                try out.writeAll("</span>");
            },

            .keyword_fn => {
                try out.writeAll("<span class=\"keyword\">");
                try write_escaped(out, src[token.loc.start..token.loc.end]);
                try out.writeAll("</span>");
                next_tok_is_fn = true;
            },

            .string_literal, .char_literal => {
                try out.writeAll("<span class=\"string\">");
                try write_escaped(out, src[token.loc.start..token.loc.end]);
                try out.writeAll("</span>");
            },

            .multiline_string_literal_line => {
                // multiline string literals contain a newline
                try out.writeAll("<span class=\"string\">");
                try write_escaped(out, src[token.loc.start .. token.loc.end - 1]);
                try out.writeAll("</span></span>\n<span class=\"line\">");
            },

            .builtin => {
                try out.writeAll("<span class=\"builtin\">");
                try write_escaped(out, src[token.loc.start..token.loc.end]);
                try out.writeAll("</span>");
            },

            .doc_comment,
            .container_doc_comment,
            => {
                try out.writeAll("<span class=\"comment\">");
                try write_escaped(out, src[token.loc.start..token.loc.end]);
                try out.writeAll("</span>");
            },

            .identifier => {
                if (prev_tok_was_fn) {
                    try out.writeAll("<span class=\"function\">");
                    try write_escaped(out, src[token.loc.start..token.loc.end]);
                    try out.writeAll("</span>");
                    print("function: {s}\n", .{src[token.loc.start..token.loc.end]});
                } else {
                    print("identifier: {s}\n", .{src[token.loc.start..token.loc.end]});
                    const is_int = blk: {
                        if (src[token.loc.start] != 'i' and src[token.loc.start] != 'u')
                            break :blk false;
                        var i = token.loc.start + 1;
                        if (i == token.loc.end)
                            break :blk false;
                        while (i != token.loc.end) : (i += 1) {
                            if (src[i] < '0' or src[i] > '9')
                                break :blk false;
                        }
                        break :blk true;
                    };
                    if (is_int or is_type(src[token.loc.start..token.loc.end])) {
                        try out.writeAll("<span class=\"type\">");
                        try write_escaped(out, src[token.loc.start..token.loc.end]);
                        try out.writeAll("</span>");
                    } else {
                        try out.writeAll("<span class=\"name\">");
                        try write_escaped(out, src[token.loc.start..token.loc.end]);
                        try out.writeAll("</span>");
                    }
                }
            },

            .number_literal => {
                try out.writeAll("<span class=\"literal number\">");
                try write_escaped(out, src[token.loc.start..token.loc.end]);
                try out.writeAll("</span>");
            },

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
            => {
                // try out.writeAll("<span class=\"symbol\">");
                try write_escaped(out, src[token.loc.start..token.loc.end]);
                // try out.writeAll("</span>");
            },
            .invalid, .invalid_periodasterisks => return parseError(
                src,
                token,
                "syntax error",
                .{},
            ),
        }
        index = token.loc.end;
    }
    try out.writeAll("</span></code></pre>");
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

const tokenator = cmd: {
    var cmd = noclip.CommandBuilder(TokCtx){
        .description =
        \\Tokenize one or more zig files into HTML.
        \\
        \\Each file provided on the command line will be tokenized and the output will
        \\be written to [filename].html. For example, 'tokenator foo.zig bar.zig' will
        \\write foo.zig.html and bar.zig.html
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

        var writebuf = std.ArrayList(u8).init(context.allocator);
        defer writebuf.deinit();

        if (parameters.write_stdout) {
            try write_tokenized_html(srcbuf, context.allocator, stdout);
            try stdout.writeByte(0);
        } else {
            const outname = try std.mem.join(context.allocator, ".", &[_][]const u8{ file_name, "html" });
            print("writing: {s}\n", .{outname});
            defer context.allocator.free(outname);

            const output = try fs.cwd().createFile(outname, .{});
            defer output.close();

            try write_tokenized_html(srcbuf, context.allocator, output.writer());
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
