const meta = @import("./meta.zig");
const params = @import("./params.zig");
const noclip = @import("./noclip.zig");

fn GenCommand(comptime UserContext: type, comptime cData: params.CommandData) type {
    return struct {
        argspec: meta.MutableTuple = .{},

        StringOption: type = params.StringOption(UserContext),
        StringArgument: type = params.StringArg(UserContext),
        Flag: type = params.Flag(UserContext),
        defaultHelpFlag: params.Flag(UserContext) = HelpFlag(undefined, .{}),

        pub fn Option(comptime _: @This(), comptime Output: type) type {
            return params.Option(.{ .Output = Output, .UserContext = UserContext });
        }

        pub fn Argument(comptime _: @This(), comptime Output: type) type {
            return params.Argument(.{ .Output = Output, .UserContext = UserContext });
        }

        pub fn HelpFlag(comptime _: @This(), comptime args: params.HelpFlagArgs) params.Flag(UserContext) {
            return params.HelpFlag(UserContext, args);
        }

        // This is really only sort of conditionally useful. It would be nice
        // to add the Subcommand directly to the argspec, except what we
        // actually have to have is the subcommand.Parser, and that can't be
        // created until all of the options are attached to that command. I
        // believe we could handle it with an `inline for` construct in the
        // parser executor, but I'm not particularly convinced that those
        // contortions provide a particularly real benefit. The main change
        // would be specifying the subcommands after the main command, whereas
        // in the current state of things, they're generally defined before the
        // main command.
        pub fn Subcommand(comptime subData: params.CommandData) GenCommand(UserContext, subData) {
            return Command(UserContext, subData);
        }

        pub fn add(comptime self: *@This(), comptime parameter: anytype) void {
            self.argspec.add(parameter);
        }

        pub fn commandSpec(comptime self: @This()) self.argspec.TupleType() {
            return self.argspec.realTuple();
        }

        pub fn CommandResult(comptime self: @This()) type {
            return noclip.CommandResult(self.commandSpec(), UserContext);
        }

        pub fn Parser(
            comptime self: @This(),
            comptime callback: *const fn (UserContext, noclip.CommandResult(self.commandSpec(), UserContext)) anyerror!void,
        ) noclip.CommandParser(cData, self.commandSpec(), UserContext, callback) {
            return noclip.CommandParser(cData, self.commandSpec(), UserContext, callback){};
        }
    };
}

pub fn Command(comptime UserContext: type, comptime cData: params.CommandData) GenCommand(UserContext, cData) {
    return GenCommand(UserContext, cData){};
}
