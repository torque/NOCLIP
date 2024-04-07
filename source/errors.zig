pub const ConversionError = error{
    OutOfMemory,
    ConversionFailed,
};

pub const ParseError = error{
    UnexpectedFailure,
    EmptyArgs,
    MissingValue,
    ExtraValue,
    FusedShortTagValueMissing,
    UnknownLongTagParameter,
    UnknownShortTagParameter,
    RequiredParameterMissing,
    OutOfMemory,
};

pub const NoclipError = ParseError || ConversionError;
