use crate::ast::{SrcSpan, TypeAst};
use crate::error::wrap;
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

impl ParseError {
    pub fn details(&self) -> (&'static str, Vec<String>) {
        match &self.error {
            ParseErrorType::ExpectedEqual => ("I was expecting a '=' after this", vec![]),
            ParseErrorType::ExpectedExpr => ("I was expecting an expression after this", vec![]),
            ParseErrorType::ExpectedName => ("I was expecting a name here", vec![]),
            ParseErrorType::ExpectedPattern => ("I was expecting a pattern after this", vec![]),
            ParseErrorType::ExpectedType => (
                "I was expecting a type after this",
                vec!["See: https://tour.gleam.run/basics/assignments/".into()],
            ),
            ParseErrorType::ExpectedUpName => ("I was expecting a type name here", vec![]),
            ParseErrorType::ExpectedValue => ("I was expecting a value after this", vec![]),
            ParseErrorType::ExpectedStatement => ("I was expecting a statement after this", vec![]),
            ParseErrorType::ExpectedDefinition => {
                ("I was expecting a definition after this", vec![])
            }
            ParseErrorType::ExpectedDeprecationMessage => (
                "A deprecation attribute must have a string message.",
                vec![],
            ),
            ParseErrorType::ExpectedFunctionDefinition => {
                ("I was expecting a function definition after this", vec![])
            }
            ParseErrorType::ExpectedTargetName => (
                "I was expecting a target name after this",
                vec!["Try `erlang`, `javascript`.".into()],
            ),
            ParseErrorType::ExtraSeparator => (
                "This is an extra delimiter",
                vec!["Hint: Try removing it?".into()],
            ),
            ParseErrorType::ExprLparStart => (
                "This parenthesis cannot be understood here",
                vec![
                    "Hint: To group expressions in Gleam, use \"{\" and \"}\"; tuples are created with `#(` and `)`.".into(),
                ]
            ),
            ParseErrorType::IncorrectName => (
                "I'm expecting a lowercase name here",
                vec![wrap(
                    "Hint: Variable and module names start with a lowercase letter, \
and can contain a-z, 0-9, or _.",
                )],
            ),
            ParseErrorType::IncorrectUpName => (
                "I'm expecting a type name here",
                vec![wrap(
                    "Hint: Type names start with a uppercase letter, and can \
contain a-z, A-Z, or 0-9.",
                )],
            ),
            ParseErrorType::InvalidBitArraySegment => (
                "This is not a valid BitArray segment option",
                vec![
                    "Hint: Valid BitArray segment options are:".into(),
                    wrap(
                        "bits, bytes, int, float, utf8, utf16, utf32, utf8_codepoint, \
utf16_codepoint, utf32_codepoint, signed, unsigned, big, little, native, size, unit.",
                    ),
                    "See: https://tour.gleam.run/data-types/bit-arrays/".into(),
                ],
            ),
            ParseErrorType::InvalidBitArrayUnit => (
                "This is not a valid BitArray unit value",
                vec![
                    "Hint: unit must be an integer literal >= 1 and <= 256.".into(),
                    "See: https://tour.gleam.run/data-types/bit-arrays/".into(),
                ],
            ),
            ParseErrorType::InvalidTailPattern => (
                "This part of a list pattern can only be a name or a discard",
                vec![],
            ),
            ParseErrorType::InvalidTupleAccess => (
                "This integer is not valid for tuple access",
                vec![
                    "Hint: Only non negative integer literals like 0, or 1_000 can be used."
                        .to_string(),
                ],
            ),
            ParseErrorType::LexError { error: lex_err } => lex_err.to_parse_error_info(),
            ParseErrorType::NestedBitArrayPattern => ("BitArray patterns cannot be nested", vec![]),
            ParseErrorType::NotConstType => (
                "This type is not allowed in module constants",
                vec!["See: https://tour.gleam.run/basics/constants/".into()],
            ),
            ParseErrorType::NoExpression => (
                "There must be an expression in here",
                vec!["Hint: Put an expression in there or remove the brackets.".into()],
            ),
            ParseErrorType::NoLetBinding => (
                "There must be a 'let' to bind variable to value",
                vec![
                    "Hint: Use let for binding.".into(),
                    "See: https://tour.gleam.run/basics/assignments/".into(),
                ],
            ),
            ParseErrorType::NoValueAfterEqual => (
                "I was expecting to see a value after this equals sign",
                vec![],
            ),
            ParseErrorType::OpaqueTypeAlias => (
                "Type Aliases cannot be opaque",
                vec!["See: https://tour.gleam.run/basics/type-aliases/".into()],
            ),
            ParseErrorType::OpNakedRight => (
                "This operator has no value on its right side",
                vec!["Hint: Remove it or put a value after it.".into()],
            ),
            ParseErrorType::TooManyArgHoles => (
                "There is more than 1 argument hole in this function call",
                vec![
                    "Hint: Function calls can have at most one argument hole.".into(),
                    "See: https://tour.gleam.run/functions/functions/".into(),
                ],
            ),
            ParseErrorType::UnexpectedEof => ("The module ended unexpectedly", vec![]),
            ParseErrorType::ListSpreadWithoutElements => (
                "This spread does nothing",
                vec![
                    "Hint: Try prepending some elements [1, 2, ..list].".into(),
                    "See: https://tour.gleam.run/basics/lists/".into(),
                ],
            ),
            ParseErrorType::ListSpreadFollowedByElements => (
                "I wasn't expecting elements after this",
                vec![
                    "Lists are immutable and singly-linked, so to append items to them".into(),
                    "all the elements of a list would need to be copied into a new list.".into(),
                    "This would be slow, so there is no built-in syntax for it.".into(),
                    "".into(),
                    "Hint: prepend items to the list and then reverse it once you are done.".into(),
                ],
            ),
            ParseErrorType::ListPatternSpreadFollowedByElements => (
                "I wasn't expecting elements after this",
                vec![
                    "Lists are immutable and singly-linked, so to match on the end".into(),
                    "of a list would require the whole list to be traversed. This".into(),
                    "would be slow, so there is no built-in syntax for it. Pattern".into(),
                    "match on the start of the list instead.".into(),
                ],
            ),
            ParseErrorType::UnexpectedReservedWord => (
                "This is a reserved word",
                vec!["Hint: I was expecting to see a name here.".into()],
            ),
            ParseErrorType::LowcaseBooleanPattern => (
                "Did you want a Bool instead of a variable?",
                vec![
                    "Hint: In Gleam boolean literals are `True` and `False`.".into(),
                    "See: https://tour.gleam.run/basics/bools/".into(),
                ],
            ),
            ParseErrorType::UnexpectedLabel => (
                "Argument labels are not allowed for anonymous functions",
                vec!["Please remove the argument label.".into()],
            ),
            ParseErrorType::UnexpectedToken {
                token,
                expected,
                hint,
            } => {
                let found = match token {
                    Token::Int { .. } => "an Int".to_string(),
                    Token::Float { .. } => "a Float".to_string(),
                    Token::String { .. } => "a String".to_string(),
                    Token::CommentDoc { .. } => "a comment".to_string(),
                    Token::DiscardName { .. } => "a discard name".to_string(),
                    Token::Name { .. } | Token::UpName { .. } => "a name".to_string(),
                    _ if token.is_reserved_word() => format!("the keyword {token}"),
                    _ => token.to_string(),
                };

                let messages = std::iter::once(format!("Found {found}, expected one of: "))
                    .chain(expected.iter().map(|s| format!("- {s}")));

                let messages = match hint {
                    Some(hint_text) => messages
                        .chain(std::iter::once(format!("Hint: {hint_text}")))
                        .collect(),
                    _ => messages.collect(),
                };

                ("I was not expecting this", messages)
            }
            ParseErrorType::ExpectedBoolean => ("Did you mean to negate a boolean?", vec![]),
            ParseErrorType::ConcatPatternVariableLeftHandSide => (
                "This must be a string literal",
                vec![
                    "We can't tell what size this prefix should be so we don't know".into(),
                    "how to handle this pattern.".into(),
                    "".into(),
                    "If you want to match one character consider using `pop_grapheme`".into(),
                    "from the stdlib's `gleam/string` module.".into(),
                ],
            ),
            ParseErrorType::UnexpectedFunction => (
                "Functions can only be called within other functions",
                vec![],
            ),
            ParseErrorType::ListSpreadWithoutTail => (
                "I was expecting a value after this spread",
                vec!["If a list expression has a spread then a tail must also be given.".into()],
            ),
            ParseErrorType::UnknownAttribute => (
                "I don't recognise this attribute",
                vec!["Try `deprecated`, `external` or `target` instead.".into()],
            ),
            ParseErrorType::DuplicateAttribute => (
                "Duplicate attribute",
                vec!["This attribute has already been given.".into()],
            ),
            ParseErrorType::UnknownTarget => (
                "I don't recognise this target",
                vec!["Try `erlang`, `javascript`.".into()],
            ),
            ParseErrorType::ExpectedFunctionBody => ("This function does not have a body", vec![]),
            ParseErrorType::RedundantInternalAttribute => (
                "Redundant internal attribute",
                vec![
                    format!("Only a public definition can be annotated as internal."),
                    "Hint: remove the `@internal` annotation.".into(),
                ],
            ),
            ParseErrorType::InvalidModuleTypePattern => (
                "Invalid pattern",
                vec![
                    "I'm expecting a pattern here".into(),
                    "Hint: A pattern can be a constructor name, a literal value".into(),
                    "or a variable to bind a value to, etc.".into(),
                    "See: https://tour.gleam.run/flow-control/case-expressions/".into(),
                ],
            ),
            ParseErrorType::ExpectedRecordConstructor {
                name,
                public,
                opaque,
                field,
                field_type,
            } => {
                let (accessor, opaque) = match *public {
                    true if *opaque => ("pub ", "opaque "),
                    true => ("pub ", ""),
                    false => ("", ""),
                };

                let mut annotation = EcoString::new();
                match field_type {
                    Some(t) => t.print(&mut annotation),
                    None => annotation.push_str("Type"),
                };

                (
                    "I was not expecting this",
                    vec![
                        "Each custom type variant must have a constructor:\n".into(),
                        format!("{accessor}{opaque}type {name} {{"),
                        format!("  {name}("),
                        format!("    {field}: {annotation},"),
                        "  )".into(),
                        "}".into(),
                    ],
                )
            }
            ParseErrorType::CallInClauseGuard => (
                "Unsupported expression",
                vec!["Functions cannot be called in clause guards.".into()],
            ),
            ParseErrorType::IfExpression => (
                "Gleam doesn't have if expressions",
                vec![
                    "If you want to write a conditional expression you can use a `case`:".into(),
                    "".into(),
                    "    case condition {".into(),
                    "      True -> todo".into(),
                    "      False -> todo".into(),
                    "    }".into(),
                    "".into(),
                    "See: https://tour.gleam.run/flow-control/case-expressions/".into(),
                ],
            ),
            ParseErrorType::ConstantRecordConstructorNoArguments => (
                "I was expecting arguments here",
                vec!["A record must be passed arguments when constructed.".into()],
            ),
            ParseErrorType::TypeConstructorNoArguments => (
                "I was expecting arguments here",
                vec!["A type constructor must be passed arguments.".into()],
            ),
            ParseErrorType::TypeDefinitionNoArguments => (
                "I was expecting generic parameters here",
                vec![
                    "A generic type must have at least a generic parameter.".into(),
                    "Hint: If a type is not generic you should omit the `()`.".into(),
                ],
            ),
            ParseErrorType::UnknownAttributeRecordVariant => (
                "This attribute cannot be used on a variant.",
                vec!["Hint: Did you mean `@deprecated`?".into()],
            ),
            ParseErrorType::IncorrectImportModuleSeparator { module, item } => (
                "I was expecting either `/` or `.{` here.",
                vec![
                    "Perhaps you meant one of:".into(),
                    "".into(),
                    format!("    import {module}/{item}"),
                    format!("    import {module}.{{item}}"),
                ]
            )
        }
    }
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
    ExpectedStatement,          // no statement after "@<name>"
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
    NoExpression, // between "{" and "}" in expression position, there must be an expression
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
    LowcaseBooleanPattern, // most likely user meant True or False in patterns
    UnexpectedLabel, // argument labels were provided, but are not supported in this context
    UnexpectedEof,
    UnexpectedReservedWord, // reserved word used when a name was expected
    UnexpectedToken {
        token: Token,
        expected: Vec<EcoString>,
        hint: Option<EcoString>,
    },
    ExpectedBoolean,
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
