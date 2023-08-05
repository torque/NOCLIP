const std = @import("std");

const converters = @import("./converters.zig");
const ConverterSignature = converters.ConverterSignature;

const ParameterType = enum { Nominal, Ordinal };

pub const FixedCount = u32;

pub const ValueCount = union(enum) {
    flag: void,
    count: void,
    fixed: FixedCount,
};

pub const FlagBias = enum {
    falsy,
    truthy,
    unbiased,

    pub fn string(comptime self: @This()) []const u8 {
        return switch (comptime self) {
            .truthy => "true",
            .falsy => "false",
            else => @compileError("flag tag with unbiased bias?"),
        };
    }
};

pub const ParameterGenerics = struct {
    UserContext: type = void,
    /// If void, do not expose this parameter in the aggregate converted parameter
    /// object. The converter for this parameter shall not return a value. This may be
    /// useful for implementing complex conversion that produces output through its
    /// side effects or by modifying the user context.
    OutputType: type = void,

    param_type: ParameterType,
    value_count: ValueCount,
    /// allow this named parameter to be passed multiple times.
    /// values will be appended when it is encountered. If false, only the
    /// final encountered instance will be used.
    // since we now use multi in place of greedy values for simplicity, we may want to
    // convert this an enum or add an additional flag to distinguish between the
    // many-to-many and the many-to-one cases.
    multi: bool,

    pub fn fixedValueCount(comptime OutputType: type, comptime value_count: ValueCount) ValueCount {
        return comptime if (value_count == .fixed)
            switch (@typeInfo(OutputType)) {
                .Struct => |info| .{ .fixed = info.fields.len },
                .Array => |info| .{ .fixed = info.len },
                // TODO: this is a bit sloppy, but it can be refined later.
                // .Pointer covers slices, which may be a many-to-many conversion.
                .Pointer => value_count,
                else => .{ .fixed = 1 },
            }
        else
            value_count;
    }

    pub fn hasContext(comptime self: @This()) bool {
        return comptime self.UserContext != void;
    }

    pub fn hasOutput(comptime self: @This()) bool {
        return self.OutputType != void;
    }

    pub fn isFlag(comptime self: @This()) bool {
        return comptime switch (self.value_count) {
            .flag, .count => true,
            .fixed => false,
        };
    }

    pub fn ConvertedType(comptime self: @This()) type {
        // is this the correct way to collapse this?
        return comptime if (self.multi and self.value_count != .count and self.OutputType != void)
            std.ArrayList(self.ReturnValue())
        else
            self.ReturnValue();
    }

    pub fn IntermediateType(comptime self: @This()) type {
        return comptime if (self.multi and self.value_count != .count)
            std.ArrayList(self.IntermediateValue())
        else
            self.IntermediateValue();
    }

    pub fn ReturnValue(comptime self: @This()) type {
        return comptime switch (self.value_count) {
            .flag => bool,
            .count => usize,
            .fixed => |count| switch (count) {
                0 => @compileError("bad fixed-zero parameter"),
                1 => self.OutputType,
                // it's actually impossible to use a list in the general case
                // because the result may have varying types. A tuple would
                // work, but cannot be iterated over without inline for. It may
                // be worth adding a ".structured" value count for a type that
                // consumes many inputs but produces a single output. It would
                // be nice to parse a tag into a struct directly. For that use
                // case, the output type must be decoupled from the input type.
                else => self.OutputType,
            },
        };
    }

    pub fn IntermediateValue(comptime self: @This()) type {
        return comptime switch (self.value_count) {
            .flag => []const u8,
            .count => usize,
            .fixed => |count| switch (count) {
                0 => @compileError("bad fixed-zero parameter"),
                1 => []const u8,
                else => std.ArrayList([]const u8),
            },
        };
    }

    pub fn nonscalar(comptime self: @This()) bool {
        return comptime switch (self.value_count) {
            .flag, .count => false,
            .fixed => |count| switch (count) {
                0 => @compileError("bad fixed-zero parameter"),
                1 => false,
                else => true,
            },
        };
    }
};

// Consider a "namespace" parameter e.g. -Dfoo=val style. The namespace would be "D" and
// it takes the place of the second "-", but otherwise this is a long-style parameter.
// Could be parsed as forced-fused. Would work for flags as well, e.g. -fno-flag
pub fn OptionConfig(comptime generics: ParameterGenerics) type {
    return struct {
        name: []const u8,

        short_tag: ?[]const u8 = null,
        long_tag: ?[]const u8 = null,
        env_var: ?[]const u8 = null,
        description: []const u8 = "", // description for output in help text

        default: ?generics.OutputType = null,
        converter: ?ConverterSignature(generics) = null,

        eager: bool = false,
        required: bool = generics.param_type == .Ordinal,
        global: bool = false,

        secret: bool = false,
        nice_type_name: ?[]const u8 = null,
        flag_bias: FlagBias = .unbiased,
    };
}

pub const ShortLongPair = struct {
    short_tag: ?[]const u8 = null,
    long_tag: ?[]const u8 = null,
};

pub fn FlagConfig(comptime generics: ParameterGenerics) type {
    return struct {
        name: []const u8,

        truthy: ?ShortLongPair = null,
        falsy: ?ShortLongPair = null,
        env_var: ?[]const u8 = null,
        description: []const u8 = "",

        default: ?bool = null,
        converter: ?ConverterSignature(generics) = null,

        eager: bool = false,
        required: bool = false,
        global: bool = false,

        secret: bool = false,
    };
}

fn OptionType(comptime generics: ParameterGenerics) type {
    return struct {
        pub const G: ParameterGenerics = generics;
        pub const param_type: ParameterType = generics.param_type;
        pub const is_flag: bool = generics.isFlag();
        pub const value_count: ValueCount = generics.value_count;
        pub const multi: bool = generics.multi;
        pub const has_output: bool = generics.hasOutput();

        name: []const u8,
        short_tag: ?[]const u8,
        long_tag: ?[]const u8,
        env_var: ?[]const u8,
        /// description for output in help text
        description: []const u8,

        default: ?generics.OutputType,
        converter: ConverterSignature(generics),

        /// the option converter will be run eagerly, before full command line
        /// validation.
        eager: bool,
        /// the option cannot be omitted from the command line.
        required: bool,
        /// this option is parsed in a pre-parsing pass that consumes it. It
        /// may be present anywhere on the command line. A different way to
        /// solve this problem is by using an environment variable. It must be
        /// a tagged option.
        global: bool,

        /// if false, do not expose the resulting value in the output type.
        /// the converter must have side effects for this option to do anything.
        /// do not print help for this parameter
        secret: bool,

        /// friendly type name ("string" is better than "[]const u8")
        nice_type_name: []const u8,
        /// internal field for handling flag value biasing. Do not overwrite unless you
        /// want weird things to happen.
        flag_bias: FlagBias,

        pub fn IntermediateValue(comptime _: @This()) type {
            return generics.IntermediateValue();
        }
    };
}

fn checkShort(comptime short_tag: ?[]const u8) void {
    const short = comptime short_tag orelse return;
    if (short.len != 2 or short[0] != '-') @compileError("bad short tag: " ++ short);
}

fn checkLong(comptime long_tag: ?[]const u8) void {
    const long = comptime long_tag orelse return;
    if (long.len < 3 or long[0] != '-' or long[1] != '-') @compileError("bad long tag: " ++ long);
}

pub fn makeOption(comptime generics: ParameterGenerics, comptime opts: OptionConfig(generics)) OptionType(generics) {
    if (opts.short_tag == null and opts.long_tag == null and opts.env_var == null) {
        @compileError(
            "option " ++
                opts.name ++
                " must have at least one of a short tag, a long tag, or an environment variable",
        );
    }

    checkShort(opts.short_tag);
    checkLong(opts.long_tag);

    // perform the logic to create the default converter here? Could be done
    // when creating the OptionConfig instead. Need to do it here because there
    // may be an error. That's the essential distinction between the OptionType
    // and the OptionConfig, is the OptionConfig is just unvalidated parameters,
    // whereas the OptionType is an instance of an object that has been
    // validated.
    const converter = opts.converter orelse
        (converters.DefaultConverter(generics) orelse @compileError(
        "no converter provided for " ++
            opts.name ++
            "and no default exists",
    ));

    return OptionType(generics){
        .name = opts.name,
        //
        .short_tag = opts.short_tag,
        .long_tag = opts.long_tag,
        .env_var = opts.env_var,
        //
        .description = opts.description,
        .default = opts.default,
        .converter = converter,
        //
        .eager = opts.eager,
        .required = opts.required,
        .global = opts.global,
        //
        .secret = opts.secret,
        .nice_type_name = opts.nice_type_name orelse @typeName(generics.OutputType),
        .flag_bias = opts.flag_bias,
    };
}

pub fn makeArgument(
    comptime generics: ParameterGenerics,
    comptime opts: OptionConfig(generics),
) OptionType(generics) {
    comptime {
        if (opts.short_tag != null or opts.long_tag != null or opts.env_var != null) {
            @compileError("argument " ++ opts.name ++ " must not have a long or short tag or an env var");
        }

        if (opts.global) {
            @compileError("argument " ++ opts.name ++ " cannot be global");
        }

        const converter = opts.converter orelse
            (converters.DefaultConverter(generics) orelse @compileError(
            "no converter provided for " ++
                opts.name ++
                "and no default exists",
        ));

        return OptionType(generics){
            .name = opts.name,
            //
            .short_tag = opts.short_tag,
            .long_tag = opts.long_tag,
            .env_var = opts.env_var,
            //
            .description = opts.description,
            .default = opts.default,
            .converter = converter,
            //
            .eager = opts.eager,
            .required = opts.required,
            .global = opts.global,
            //
            .secret = opts.secret,
            .nice_type_name = opts.nice_type_name orelse @typeName(generics.OutputType),
            .flag_bias = .unbiased,
        };
    }
}
