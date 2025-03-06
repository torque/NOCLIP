const std = @import("std");
const noclip = @import("noclip");

const Choice = enum { first, second };

const U8Tuple = struct { u8, u8 };

const Main = struct {
    pub const description =
        \\The definitive noclip demonstration utility.
        \\
        \\This command demonstrates the functionality of the noclip library. cool!
        \\
        \\>     // implementing factorial recursively is a silly thing to do
        \\>     pub fn fact(n: u64) u64 {
        \\>         if (n == 0) return 1;
        \\>         return n * fact(n - 1);
        \\>     }
        \\
        \\Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
        \\incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
        \\nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
        \\Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
        \\eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident,
        \\sunt in culpa qui officia deserunt mollit anim id est laborum.
    ;

    pub const options: noclip.CommandOptions = .{};

    pub const parameters = struct {
        // pub const tuple: noclip.Option(U8Tuple) = .{
        //     .short = 't',
        //     .long = "tuple",
        //     .env = "NOCLIP_TEST",
        //     .description = "tuple test option",
        // };
        pub const choice: noclip.Option(Choice) = .{
            .short = 'c',
            .long = "choice",
            .env = "NOCLIP_CHOICE",
            .description = "enum choice option",
        };
        pub const string: noclip.Option(noclip.String) = .{
            .short = 's',
            .long = "string",
            .env = "NOCLIP_STRING",
            .description = "A string value option",
        };
        pub const default: noclip.Option(u32) = .{
            .name = "default",
            .description = "default value integer option",
            .short = 'd',
            .long = "default",
            .env = "NOCLIP_DEFAULT",
            .default = 100,
            // .nice_type_name = "uint",
        };
        pub const counter: noclip.Counter(u32) = .{
            .name = "default",
            .description = "default value integer option",
            .short = 'd',
            .long = "default",
            .env = "NOCLIP_DEFAULT",
            // .nice_type_name = "uint",
        };
        pub const multi: noclip.Option(noclip.Accumulate(u8)) = .{
            .name = "multi",
            .short_tag = 'm',
            .long_tag = "multi",
            .description = "multiple specification test option",
        };
        pub const flag: noclip.BoolGroup = .{
            .name = "flag",
            .truthy = .{ .short = 'f', .long = "flag" },
            .falsy = .{ .short = 'F', .long = "no-flag" },
            .env = "NOCLIP_FLAG",
            .description = "boolean flag",
        };
        // pub const multiflag: noclip.Flag = .{
        //     .name = "multiflag",
        //     .truthy = .{ .short = 'M' },
        //     .description = "multiple specification test flag",
        //     .multi = .accumulate,
        // };
        pub const env_only: noclip.Option(u8) = .{
            .env = "NOCLIP_ENVIRON",
            .description = "environment variable only option",
        };
        // pub const group: noclip.Group(bool) = .{
        //     .parameters = struct {
        //         pub const a: noclip.Flag = .{
        //             .truthy = .{ .short = 'z' },
        //         };
        //     },
        // };
    };

    pub const subcommands = struct {
        pub const subcommand = Subcommand;
        pub const deeply = struct {
            pub const info: noclip.CommandInfo = .{ .description = "start of a deeply nested subcommand" };
            pub const subcommands = struct {
                pub const nested = struct {
                    pub const info: noclip.CommandInfo = .{ .description = "second level of a deeply nested subcommand" };
                    pub const subcommands = struct {
                        pub const subcommand = struct {
                            pub const info: noclip.CommandInfo = .{ .description = "third level of a deeply nested subcommand" };
                            pub const subcommands = struct {
                                pub const group = struct {
                                    pub const info: noclip.CommandInfo = .{ .description = "final level of a deeply nested subcommand" };
                                    pub fn run() void {
                                        std.debug.print("but it does nothing\n");
                                    }
                                };
                            };
                        };
                    };
                };
            };
        };
    };

    pub fn run(args: noclip.Result(Main)) void {
        if (args.choice) |choice| {
            std.debug.print("You chose: {s}\n", .{@tagName(choice)});
        } else {
            std.debug.print("You chose nothing!\n", .{});
        }
    }

    pub fn err(report: noclip.ErrorReport) void {
        _ = report;
    }
};

const Subcommand = struct {
    pub const info: noclip.CommandInfo = .{
        .name = "subcommand",
        .description =
        \\Demonstrate subcommand functionality
        \\
        \\This command demonstrates how subcommands work.
        ,
        .options = .{ .parse_error_behavior = .exit },
    };

    pub const parameters = struct {
        pub const flag: noclip.BoolGroup = .{
            .truthy = .{ .short = 'f', .long = "flag" },
            .falsy = .{ .long = "no-flag" },
            .env = "NOCLIP_SUBFLAG",
        };
        pub const first_arg: noclip.Argument(noclip.String) = .{};
        pub const second_arg: noclip.Argument(noclip.String) = .{
            .description = "This is an argument that doesn't really do anything, but it's very important.",
        };
    };
};

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    var env = try std.process.getEnvMap(allocator);
    defer env.deinit();

    try noclip.execute(Main, .{ .args = args, .env = env });

    return 0;
}
