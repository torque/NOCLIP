pub const ConversionError = error{
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
};

pub const NoclipError = ParseError || ConversionError;
