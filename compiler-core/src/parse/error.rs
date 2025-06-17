use crate::ast::{SrcSpan, TypeAst};
use crate::parse::Token;
use ecow::EcoString;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct LexicalError {
    pub error: LexicalErrorType,
    pub location: SrcSpan,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum InvalidUnicodeEscapeError {
    MissingOpeningBrace,          // Expected '{'
    ExpectedHexDigitOrCloseBrace, // Expected hex digit or '}'
    InvalidNumberOfHexDigits,     // Expected between 1 and 6 hex digits
    InvalidCodepoint,             // Invalid Unicode codepoint
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LexicalErrorType {
    BadStringEscape,                                 // string contains an unescaped slash
    InvalidUnicodeEscape(InvalidUnicodeEscapeError), // \u{...} escape sequence is invalid
    DigitOutOfRadix,                                 // 0x012 , 2 is out of radix
    NumTrailingUnderscore,                           // 1_000_ is not allowed
    RadixIntNoValue,                                 // 0x, 0b, 0o without a value
    MissingExponent,     // 1.0e, for example, where there is no exponent
    UnexpectedStringEnd, // Unterminated string literal
    UnrecognizedToken { tok: char },
    InvalidTripleEqual,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub error: ParseErrorType,
    pub location: SrcSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorType {
    ExpectedEqual,              // expect "="
    ExpectedExpr,               // after "->" in a case clause
    ExpectedName,               // any token used when a Name was expected
    ExpectedPattern,            // after ':' where a pattern is expected
    ExpectedType,               // after ':' or '->' where a type annotation is expected
    ExpectedUpName,             // any token used when a UpName was expected
    ExpectedValue,              // no value after "="
    ExpectedDefinition,         // after attributes
    ExpectedDeprecationMessage, // after "deprecated"
    ExpectedFunctionDefinition, // after function-only attributes
    ExpectedTargetName,         // after "@target("
    ExprLparStart,              // it seems "(" was used to start an expression
    ExtraSeparator,             // #(1,,) <- the 2nd comma is an extra separator
    IncorrectName,              // UpName or DiscardName used when Name was expected
    IncorrectUpName,            // Name or DiscardName used when UpName was expected
    InvalidBitArraySegment,     // <<7:hello>> `hello` is an invalid BitArray segment
    InvalidBitArrayUnit,        // in <<1:unit(x)>> x must be 1 <= x <= 256
    InvalidTailPattern,         // only name and _name are allowed after ".." in list pattern
    InvalidTupleAccess,         // only positive int literals for tuple access
    LexError {
        error: LexicalError,
    },
    NestedBitArrayPattern,        // <<<<1>>, 2>>, <<1>> is not allowed in there
    NoLetBinding, // Bindings and rebinds always require let and must always bind to a value.
    NoValueAfterEqual, // = <something other than a value>
    NotConstType, // :fn(), name, _  are not valid const types
    OpNakedRight, // Operator with no value to the right
    OpaqueTypeAlias, // Type aliases cannot be opaque
    TooManyArgHoles, // a function call can have at most 1 arg hole
    DuplicateAttribute, // an attribute was used more than once
    UnknownAttribute, // an attribute was used that is not known
    UnknownTarget, // an unknown target was used
    ListSpreadWithoutElements, // Pointless spread: `[..xs]`
    ListSpreadFollowedByElements, // trying to append something after the spread: `[..xs, x]`
    ListSpreadWithAnotherSpread {
        first_spread_location: SrcSpan,
        second_spread_location: SrcSpan,
    }, // trying to use multiple spreads: `[..xs, ..ys]`
    LowcaseBooleanPattern, // most likely user meant True or False in patterns
    UnexpectedLabel, // argument labels were provided, but are not supported in this context
    UnexpectedEof,
    UnexpectedReservedWord, // reserved word used when a name was expected
    UnexpectedToken {
        token: Token,
        expected: Vec<EcoString>,
        hint: Option<EcoString>,
    },
    UnexpectedFunction, // a function was used called outside of another function
    // A variable was assigned or discarded on the left hand side of a <> pattern
    ConcatPatternVariableLeftHandSide,
    ListSpreadWithoutTail,               // let x = [1, ..]
    ExpectedFunctionBody,                // let x = fn()
    RedundantInternalAttribute,          // for a private definition marked as internal
    InvalidModuleTypePattern,            // for patterns that have a dot like: `name.thing`
    ListPatternSpreadFollowedByElements, // When there is a pattern after a spread [..rest, pattern]
    ExpectedRecordConstructor {
        name: EcoString,
        public: bool,
        opaque: bool,
        field: EcoString,
        field_type: Option<Box<TypeAst>>,
    },
    CallInClauseGuard, // case x { _ if f() -> 1 }
    IfExpression,
    ConstantRecordConstructorNoArguments, // const x = Record()
    TypeConstructorNoArguments,           // let a : Int()
    TypeDefinitionNoArguments,            // pub type Wibble() { ... }
    UnknownAttributeRecordVariant, // an attribute was used that is not know for a custom type variant
    // a Python-like import was written, such as `import gleam.io`, instead of `import gleam/io`
    IncorrectImportModuleSeparator {
        module: EcoString,
        item: EcoString,
    },
}

impl LexicalError {
    pub fn to_parse_error_info(&self) -> (&'static str, Vec<String>) {
        match &self.error {
            LexicalErrorType::BadStringEscape => (
                "I don't understand this escape code",
                vec![
                    "Hint: Add another backslash before it.".into(),
                    "See: https://tour.gleam.run/basics/strings".into(),
                ],
            ),
            LexicalErrorType::DigitOutOfRadix => {
                ("This digit is too big for the specified radix", vec![])
            }
            LexicalErrorType::NumTrailingUnderscore => (
                "Numbers cannot have a trailing underscore",
                vec!["Hint: remove it.".into()],
            ),
            LexicalErrorType::RadixIntNoValue => ("This integer has no value", vec![]),
            LexicalErrorType::MissingExponent => (
                "This float is missing an exponent",
                vec!["Hint: Add an exponent or remove the trailing `e`".into()],
            ),
            LexicalErrorType::UnexpectedStringEnd => {
                ("The string starting here was left open", vec![])
            }
            LexicalErrorType::UnrecognizedToken { tok } if *tok == ';' => (
                "Remove this semicolon",
                vec![
                    "Hint: Semicolons used to be whitespace and did nothing.".into(),
                    "You can safely remove them without your program changing.".into(),
                ],
            ),
            LexicalErrorType::UnrecognizedToken { tok } if *tok == '\'' => (
                "Unexpected single quote",
                vec!["Hint: Strings are written with double quotes.".into()],
            ),
            LexicalErrorType::UnrecognizedToken { .. } => (
                "I can't figure out what to do with this character",
                vec!["Hint: Is it a typo?".into()],
            ),
            LexicalErrorType::InvalidUnicodeEscape(
                InvalidUnicodeEscapeError::MissingOpeningBrace,
            ) => (
                "Expected '{' in Unicode escape sequence",
                vec!["Hint: Add it.".into()],
            ),
            LexicalErrorType::InvalidUnicodeEscape(
                InvalidUnicodeEscapeError::ExpectedHexDigitOrCloseBrace,
            ) => (
                "Expected hex digit or '}' in Unicode escape sequence",
                vec![
                    "Hint: Hex digits are digits from 0 to 9 and letters from a to f or A to F."
                        .into(),
                ],
            ),
            LexicalErrorType::InvalidUnicodeEscape(
                InvalidUnicodeEscapeError::InvalidNumberOfHexDigits,
            ) => (
                "Expected between 1 and 6 hex digits in Unicode escape sequence",
                vec![],
            ),
            LexicalErrorType::InvalidUnicodeEscape(InvalidUnicodeEscapeError::InvalidCodepoint) => {
                ("Invalid Unicode codepoint", vec![])
            }
            LexicalErrorType::InvalidTripleEqual => (
                "Did you mean `==`?",
                vec![
                    "Gleam uses `==` to check for equality between two values.".into(),
                    "See: https://tour.gleam.run/basics/equality".into(),
                ],
            ),
        }
    }
}
