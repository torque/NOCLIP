const std = @import("std");
const noclip = @import("noclip");
const tokenator = @import("./tokenator.zig");

const cmark = @cImport({
    @cInclude("cmark.h");
    @cInclude("cmark_version.h");
    @cInclude("cmark_export.h");
});

const Directive = enum {
    section,
    description,
    example,
    include,
};

const ExampleFormat = enum {
    zig,
    console,

    pub fn default_format() ExampleFormat {
        return .zig;
    }
};

const DescriptionFormat = enum {
    markdown,
};

const ParseError = error{
    LeadingGarbage,
    ExpectedDirectivePrefix,
    ExpectedDirectiveSuffix,
    ExpectedNonemptySuffix,
    ExpectedDirectiveTerminator,
    UnknownDirective,
    UnknownSuffix,
    UnexpectedDirectiveMismatch,
    MissingRequiredTrailer,
    UnsupportedFormat,
    UnexpectedDirective,
};

const RawDirectiveLine = struct {
    directive: Directive,
    suffix: ?[]const u8, // the part after the dot. Null if there is no dot
    trailer: ?[]const u8, // the part after the colon. null if empty or whitespace-only

    // line has had its trailing newline stripped
    fn from_line(line: []const u8) ParseError!RawDirectiveLine {
        if (line.len < 1 or line[0] != '@') return error.ExpectedDirectivePrefix;
        var result: RawDirectiveLine = .{
            .directive = undefined,
            .suffix = null,
            .trailer = null,
        };
        var offset: usize = blk: {
            inline for (comptime std.meta.fields(Directive)) |field| {
                const len = field.name.len + 1;

                if (line.len > len and
                    (line[len] == ':' or line[len] == '.') and
                    std.mem.eql(u8, line[1..len], field.name))
                {
                    result.directive = @field(Directive, field.name);
                    break :blk len;
                }
            }

            return error.UnknownDirective;
        };

        if (line[offset] == '.') blk: {
            const suffix_start = offset + 1;
            while (offset < line.len) : (offset += 1) {
                if (line[offset] == ':') {
                    if (offset <= suffix_start) return error.ExpectedNonemptySuffix;

                    result.suffix = line[suffix_start..offset];
                    break :blk;
                }
            }

            return error.ExpectedDirectiveTerminator;
        }

        if (line[offset] != ':') return error.ExpectedDirectiveTerminator;
        offset += 1;
        while (offset < line.len) : (offset += 1) {
            if (!std.ascii.isWhitespace(line[offset])) {
                // TODO: also trim trailing whitespace
                result.trailer = line[offset..line.len];
                break;
            }
        }

        return result;
    }
};

fn expect_optional_string(candidate: ?[]const u8, expected: ?[]const u8) !void {
    if (expected) |exstr| {
        if (candidate) |canstr| {
            try std.testing.expectEqualStrings(exstr, canstr);
        } else {
            std.debug.print("Expected \"{s}\", got null\n", .{exstr});
            return error.TestExpectedEqual;
        }
    } else {
        if (candidate) |canstr| {
            std.debug.print("Expected null, got \"{s}\"\n", .{canstr});
            return error.TestExpectedEqual;
        }
    }
}

fn expect_rdl(candidate: RawDirectiveLine, expected: RawDirectiveLine) !void {
    try std.testing.expectEqual(expected.directive, candidate.directive);
    try expect_optional_string(candidate.suffix, expected.suffix);
    try expect_optional_string(candidate.trailer, expected.trailer);
}

test "RawDirectiveLine.from_line" {
    try expect_rdl(
        try RawDirectiveLine.from_line("@section:"),
        .{ .directive = .section, .suffix = null, .trailer = null },
    );
    try expect_rdl(
        try RawDirectiveLine.from_line("@example:"),
        .{ .directive = .example, .suffix = null, .trailer = null },
    );
    try expect_rdl(
        try RawDirectiveLine.from_line("@example.zig:"),
        .{ .directive = .example, .suffix = "zig", .trailer = null },
    );

    try expect_rdl(
        try RawDirectiveLine.from_line("@example.zig: ./example.file"),
        .{ .directive = .example, .suffix = "zig", .trailer = "./example.file" },
    );

    try std.testing.expectError(
        ParseError.UnknownDirective,
        RawDirectiveLine.from_line("@unknown:"),
    );
    try std.testing.expectError(
        ParseError.ExpectedDirectivePrefix,
        RawDirectiveLine.from_line("hello"),
    );
    // TODO: this would be better if it produced error.ExpectedDirectiveTerminator
    // instead, but it complicates the logic to do so.
    try std.testing.expectError(
        ParseError.UnknownDirective,
        RawDirectiveLine.from_line("@section"),
    );
    try std.testing.expectError(
        ParseError.ExpectedDirectiveTerminator,
        RawDirectiveLine.from_line("@example.tag"),
    );
    try std.testing.expectError(
        ParseError.ExpectedNonemptySuffix,
        RawDirectiveLine.from_line("@example.:"),
    );
}

const SectionDirectiveLine = struct {
    name: []const u8,

    fn from_raw(raw: RawDirectiveLine) ParseError!DirectiveLine {
        if (raw.directive != .section) return error.UnexpectedDirectiveMismatch;
        if (raw.suffix != null) return error.UnknownSuffix;
        if (raw.trailer == null) return error.MissingRequiredTrailer;

        return .{ .section = .{ .name = raw.trailer.? } };
    }
};

test "SectionDirectiveLine.from_raw" {
    const line = try SectionDirectiveLine.from_raw(.{ .directive = .section, .suffix = null, .trailer = "Section" });
    try std.testing.expectEqualStrings("Section", line.section.name);

    try std.testing.expectError(
        ParseError.UnexpectedDirectiveMismatch,
        SectionDirectiveLine.from_raw(.{ .directive = .example, .suffix = null, .trailer = "Section" }),
    );
    try std.testing.expectError(
        ParseError.UnknownSuffix,
        SectionDirectiveLine.from_raw(.{ .directive = .section, .suffix = "importante", .trailer = "Section" }),
    );
    try std.testing.expectError(
        ParseError.MissingRequiredTrailer,
        SectionDirectiveLine.from_raw(.{ .directive = .section, .suffix = null, .trailer = null }),
    );
}

const DescriptionDirectiveLine = struct {
    format: DescriptionFormat,
    include: ?[]const u8,

    fn from_raw(raw: RawDirectiveLine) ParseError!DirectiveLine {
        if (raw.directive != .description) return error.UnexpectedDirectiveMismatch;
        const format: DescriptionFormat = if (raw.suffix) |suffix|
            std.meta.stringToEnum(DescriptionFormat, suffix) orelse return error.UnsupportedFormat
        else
            .markdown;

        return .{ .description = .{ .format = format, .include = raw.trailer } };
    }
};

const ExampleDirectiveLine = struct {
    format: ExampleFormat,
    include: ?[]const u8,

    fn from_raw(raw: RawDirectiveLine) ParseError!DirectiveLine {
        if (raw.directive != .example) return error.UnexpectedDirectiveMismatch;
        const format: ExampleFormat = if (raw.suffix) |suffix|
            std.meta.stringToEnum(ExampleFormat, suffix) orelse return error.UnsupportedFormat
        else
            .zig;

        return .{ .example = .{ .format = format, .include = raw.trailer } };
    }
};

const IncludeDirectiveLine = struct {
    path: []const u8,

    fn from_raw(raw: RawDirectiveLine) ParseError!DirectiveLine {
        if (raw.directive != .include) return error.UnexpectedDirectiveMismatch;
        if (raw.suffix != null) return error.UnknownSuffix;
        if (raw.trailer == null) return error.MissingRequiredTrailer;

        return .{ .include = .{ .path = raw.trailer.? } };
    }
};

const DirectiveLine = union(Directive) {
    section: SectionDirectiveLine,
    description: DescriptionDirectiveLine,
    example: ExampleDirectiveLine,
    include: IncludeDirectiveLine,

    fn from_line(line: []const u8) ParseError!DirectiveLine {
        const raw = try RawDirectiveLine.from_line(line);
        return try switch (raw.directive) {
            .section => SectionDirectiveLine.from_raw(raw),
            .description => DescriptionDirectiveLine.from_raw(raw),
            .example => ExampleDirectiveLine.from_raw(raw),
            .include => IncludeDirectiveLine.from_raw(raw),
        };
    }
};

fn slugify(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
    const buf = try allocator.alloc(u8, source.len);
    for (source, 0..) |char, idx| {
        if (std.ascii.isAlphanumeric(char)) {
            buf[idx] = std.ascii.toLower(char);
        } else {
            buf[idx] = '-';
        }
    }

    return buf;
}

const Section = struct {
    name: []const u8,
    id: []const u8,
    segments: []const Segment,

    fn emit(self: Section, allocator: std.mem.Allocator, writer: anytype) !void {
        try writer.print(
            \\<section>
            \\<div id="{s}" class = "header">{s}</div>
            \\
        , .{ self.id, self.name });

        for (self.segments) |segment| {
            switch (segment) {
                inline else => |seg| try seg.emit(allocator, writer),
            }
        }

        try writer.writeAll("</section>");
    }
};

const Segment = union(enum) {
    description: Description,
    example: Example,
};

const Example = struct {
    format: ExampleFormat,
    body: Body,
    fn emit(self: Example, allocator: std.mem.Allocator, writer: anytype) !void {
        try writer.writeAll(
            \\<div class="example">
            \\<div class="codebox">
            \\
        );
        switch (self.format) {
            .zig => switch (self.body) {
                .in_line => |buf| try tokenator.tokenize_buffer(buf, allocator, writer, false),
                .include => |fln| try tokenator.tokenize_file(fln, allocator, writer, false),
            },
            .console => switch (self.body) {
                .in_line => |buf| try writer.print(
                    \\<pre class="code-markup"><code class="lang-console">{s}</code></pre>
                    \\
                , .{std.mem.trim(u8, buf, " \n")}),
                .include => @panic("included console example not supported"),
            },
        }
        try writer.writeAll(
            \\</div>
            \\</div>
            \\
        );
    }
};

const Description = struct {
    format: DescriptionFormat,
    body: Body,

    fn emit(self: Description, allocator: std.mem.Allocator, writer: anytype) !void {
        try writer.writeAll(
            \\<div class="description">
            \\
        );

        _ = allocator;
        switch (self.format) {
            .markdown => switch (self.body) {
                .in_line => |buf| {
                    const converted = cmark.cmark_markdown_to_html(buf.ptr, buf.len, 0);
                    if (converted == null) return error.OutOfMemory;
                    try writer.writeAll(std.mem.sliceTo(converted, 0));
                },
                .include => |fln| {
                    _ = fln;
                    @panic("include description not implemented");
                },
            },
        }

        try writer.writeAll("</div>\n");
    }
};

const Body = union(enum) {
    in_line: []const u8,
    include: []const u8,
};

const Document = []const Section;

fn read_directive(line: []const u8) ParseError!DirectiveLine {
    if (line[0] != '@') return error.ExpectedDirective;
    inline for (comptime std.meta.fields(Directive)) |field| {
        const len = field.name.len + 1;
        if (line.len > len and
            (line[len] == ':' or line[len] == '.') and
            std.mem.eql(u8, line[1..len], field.name))
        {
            return @field(Directive, field.name);
        }
    }
}

const ParserState = enum {
    section_or_include,
    any_directive,
};

fn slice_to_next_directive(lines: *std.mem.TokenIterator(u8)) ![]const u8 {
    const start = lines.index + 1;

    // the directive is the last line in the file
    if (start >= lines.buffer.len) return "";

    while (lines.peek()) |line| : (_ = lines.next()) {
        if (DirectiveLine.from_line(line)) |_| {
            return lines.buffer[start..lines.index];
        } else |err| switch (err) {
            error.ExpectedDirectivePrefix => {},
            else => return err,
        }
    }

    // we hit EOF
    return lines.buffer[start..lines.buffer.len];
}

pub fn parse(allocator: std.mem.Allocator, input: []const u8, directory: []const u8) !Document {
    var lines = std.mem.tokenize(u8, input, "\n");

    var doc_builder = std.ArrayList(Section).init(allocator);
    var section_builder = std.ArrayList(Segment).init(allocator);

    var state: ParserState = .section_or_include;

    var current_section: Section = .{
        .name = undefined,
        .id = undefined,
        .segments = undefined,
    };

    while (lines.next()) |line| {
        const dline = try DirectiveLine.from_line(line);
        switch (state) {
            .section_or_include => switch (dline) {
                .section => |sline| {
                    current_section.name = sline.name;
                    current_section.id = try slugify(allocator, sline.name);
                    state = .any_directive;
                },
                .include => |iline| {
                    // read the file at iline.path
                    const doc = try parse(allocator, iline.path, try std.fs.path.join(allocator, &[_][]const u8{directory}));
                    defer allocator.free(doc);
                    try doc_builder.appendSlice(doc);
                },
                else => return error.UnexpectedDirective,
            },
            .any_directive => switch (dline) {
                .section => |sline| {
                    current_section.segments = try section_builder.toOwnedSlice();
                    try doc_builder.append(current_section);
                    current_section.name = sline.name;
                    current_section.id = try slugify(allocator, sline.name);
                },
                .include => |iline| {
                    const doc = try parse(allocator, iline.path, try std.fs.path.join(allocator, &[_][]const u8{directory}));
                    defer allocator.free(doc);
                    try doc_builder.appendSlice(doc);
                    state = .section_or_include;
                },
                .example => |exline| {
                    try section_builder.append(.{ .example = .{
                        .format = exline.format,
                        .body = if (exline.include) |incl|
                            .{ .include = try std.fs.path.join(allocator, &[_][]const u8{ directory, incl }) }
                        else
                            .{ .in_line = try slice_to_next_directive(&lines) },
                    } });
                },
                .description => |desline| {
                    try section_builder.append(.{ .description = .{
                        .format = desline.format,
                        .body = if (desline.include) |incl|
                            .{ .include = try std.fs.path.join(allocator, &[_][]const u8{ directory, incl }) }
                        else
                            .{ .in_line = try slice_to_next_directive(&lines) },
                    } });
                },
            },
        }
    }
    current_section.segments = try section_builder.toOwnedSlice();
    try doc_builder.append(current_section);

    return doc_builder.toOwnedSlice();
}

pub fn free_doc(doc: Document, allocator: std.mem.Allocator) void {
    for (doc) |section| {
        allocator.free(section.id);
        allocator.free(section.segments);
    }
    allocator.free(doc);
}

test "parser" {
    const doc = try parse(
        std.testing.allocator,
        \\@section: first section
        \\@example:
        \\
        \\what
        \\have we got
        \\here
        \\@description: include
        \\@section: second
        \\@description:
        \\words
        \\@description:
        ,
    );

    defer free_doc(doc, std.testing.allocator);

    for (doc) |section| {
        std.debug.print("section: {s}\n", .{section.name});
        for (section.segments) |seg| {
            switch (seg) {
                .description => |desc| switch (desc.body) {
                    .include => |inc| std.debug.print("  seg: description, body: include {s}\n", .{inc}),
                    .in_line => |inl| std.debug.print("  seg: description, body: inline {s}\n", .{inl}),
                },
                .example => |desc| switch (desc.body) {
                    .include => |inc| std.debug.print("  seg: example, body: include {s}\n", .{inc}),
                    .in_line => |inl| std.debug.print("  seg: example, body: inline {s}\n", .{inl}),
                },
            }
        }
    }
}

const pre_nav = @embedFile("./templates/pre-nav.fragment.html");
const style = @embedFile("./templates/style.css");
const post_nav = @embedFile("./templates/post-nav.fragment.html");
const post_body = @embedFile("./templates/post-body.fragment.html");
const nav_item_template =
    \\<a href="#{s}"><div class="item">{s}</div></a>
    \\
;

const dezed_cmd = cmd: {
    var cmd = noclip.CommandBuilder(*ZedCtx){
        .description =
        \\Convert a ZED file into HTML
        \\
        ,
    };
    cmd.string_option(.{
        .name = "output",
        .short_tag = "-o",
        .long_tag = "--output",
        .description = "write output to file (- to write to stdout). If omitted, output will be written to <input>.html",
    });
    cmd.string_argument(.{ .name = "input" });
    break :cmd cmd;
};

const ZedCtx = struct {
    allocator: std.mem.Allocator,
};

fn dezed_cli(context: *ZedCtx, parameters: dezed_cmd.Output()) !void {
    const outname = parameters.output orelse if (std.mem.eql(u8, parameters.input, "-"))
        "-"
    else
        try std.mem.join(
            context.allocator,
            ".",
            &[_][]const u8{ parameters.input, "html" },
        );

    // this theoretically leaks the file handle, though we should be able to extract it
    // from the reader/writer
    const input = blk: {
        if (std.mem.eql(u8, parameters.input, "-")) {
            break :blk std.io.getStdIn().reader();
        } else {
            break :blk (try std.fs.cwd().openFile(parameters.input, .{ .mode = .read_only })).reader();
        }
    };

    const output = blk: {
        if (std.mem.eql(u8, outname, "-")) {
            break :blk std.io.getStdOut().writer();
        } else {
            break :blk (try std.fs.cwd().createFile(outname, .{})).writer();
        }
    };

    const cwd = try std.process.getCwdAlloc(context.allocator);
    const filedir = try std.fs.path.join(context.allocator, &[_][]const u8{ cwd, std.fs.path.dirname(outname) orelse return error.OutOfMemory });

    const data = try input.readAllAlloc(context.allocator, 1_000_000);
    const doc = try parse(context.allocator, data, filedir);
    defer free_doc(doc, context.allocator);

    try output.print(pre_nav, .{ "NOCLIP", style });

    for (doc) |section| {
        try output.print(nav_item_template, .{ section.id, section.name });
    }

    try output.writeAll(post_nav);

    for (doc) |section| {
        try section.emit(context.allocator, output);
    }

    try output.writeAll(post_body);
}

pub fn cli() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    var ctx: ZedCtx = .{ .allocator = arena.allocator() };

    var cli_parser = dezed_cmd.create_parser(dezed_cli, ctx.allocator);
    try cli_parser.execute(&ctx);

    return 0;
}

pub fn main() !u8 {
    return try cli();
}
