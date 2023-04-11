const std = @import("std");

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

const Body = union(enum) {
    in_line: []const u8,
    include: []const u8,
};

const Example = struct {
    format: ExampleFormat,
    body: Body,
};

const Section = struct {
    name: []const u8,
    segments: []const Segment,
};

const Segment = union(enum) {
    description: Description,
    example: Example,
};

const Description = struct {
    format: DescriptionFormat,
    body: Body,
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

fn slice_to_next_directive(lines: *std.mem.TokenIterator(u8)) []const u8 {
    const start = lines.index + 1;
    while (lines.peek()) |line| : (_ = lines.next()) {
        // this approach is likely too sloppy
        if (DirectiveLine.from_line(line)) |_| {
            return lines.buffer[start..lines.index];
        } else |_| {}
    }

    // we hit EOF
    return lines.buffer[start..lines.buffer.len];
}

pub fn parse(allocator: std.mem.Allocator, input: []const u8) !Document {
    var lines = std.mem.tokenize(u8, input, "\n");

    var doc_builder = std.ArrayList(Section).init(allocator);
    var section_builder = std.ArrayList(Segment).init(allocator);

    var state: ParserState = .section_or_include;

    var current_section: Section = undefined;

    while (lines.next()) |line| {
        const dline = try DirectiveLine.from_line(line);
        switch (state) {
            .section_or_include => switch (dline) {
                .section => |sline| {
                    current_section.name = sline.name;
                    state = .any_directive;
                },
                .include => |iline| {
                    // read the file at iline.path
                    const doc = try parse(allocator, iline.path);
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
                },
                .include => |iline| {
                    const doc = try parse(allocator, iline.path);
                    defer allocator.free(doc);
                    try doc_builder.appendSlice(doc);
                    state = .section_or_include;
                },
                .example => |exline| {
                    try section_builder.append(.{ .example = .{
                        .format = exline.format,
                        .body = if (exline.include) |incl|
                            .{ .include = incl }
                        else
                            .{ .in_line = slice_to_next_directive(&lines) },
                    } });
                },
                .description => |desline| {
                    try section_builder.append(.{ .description = .{
                        .format = desline.format,
                        .body = if (desline.include) |incl|
                            .{ .include = incl }
                        else
                            .{ .in_line = slice_to_next_directive(&lines) },
                    } });
                },
            },
        }
    }
    current_section.segments = try section_builder.toOwnedSlice();
    try doc_builder.append(current_section);

    return doc_builder.toOwnedSlice();
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

    for (doc) |section| std.testing.allocator.free(section.segments);
    std.testing.allocator.free(doc);
}
