use crate::ast::{SrcSpan, TypeAst};
use crate::diagnostic::{ExtraLabel, Label};
use crate::error::wrap;
use crate::parse::Token;
use ecow::EcoString;
use itertools::Itertools;

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
    }, // trying to use multiple spreads: `[..xs, ..ys]`
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
    TypeDefinitionNoArguments,            // pub type Wibble() { ... }
    UnknownAttributeRecordVariant, // an attribute was used that is not know for a custom type variant
    // a Python-like import was written, such as `import gleam.io`, instead of `import gleam/io`
    IncorrectImportModuleSeparator {
        module: EcoString,
        item: EcoString,
    },
    /// This can happen when there's an empty block in a case clause guard.
    /// For example: `_ if a == {}`
    EmptyGuardBlock,
    // When the use tries to define a constant inside a function
    ConstantInsideFunction,
    FunctionDefinitionAngleGenerics, // fn something<T>() { ... }
    // let a: List<String> = []
    TypeUsageAngleGenerics {
        module: Option<EcoString>,
        name: EcoString,
        arguments: Vec<TypeAst>,
    },
    // type Something<T> {
    TypeDefinitionAngleGenerics {
        name: EcoString,
        arguments: Vec<EcoString>,
    },
}

pub(crate) struct ParseErrorDetails {
    pub text: String,
    pub label_text: EcoString,
    pub extra_labels: Vec<ExtraLabel>,
    pub hint: Option<String>,
}

impl ParseErrorType {
    pub(crate) fn details(&self) -> ParseErrorDetails {
        match self {
            ParseErrorType::ExpectedEqual => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "I was expecting a '=' after this".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ExpectedExpr => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "I was expecting an expression after this".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ExpectedName => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "I was expecting a name here".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ExpectedPattern => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "I was expecting a pattern after this".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ExpectedType => ParseErrorDetails {
                text: "See: https://tour.gleam.run/basics/assignments/".into(),
                hint: None,
                label_text: "I was expecting a type after this".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ExpectedUpName => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "I was expecting a type name here".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ExpectedValue => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "I was expecting a value after this".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ExpectedDefinition => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "I was expecting a definition after this".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ExpectedDeprecationMessage => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "A deprecation attribute must have a string message.".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ExpectedFunctionDefinition => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "I was expecting a function definition after this".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ExpectedTargetName => ParseErrorDetails {
                text: "Try `erlang`, `javascript`.".into(),
                hint: None,
                label_text: "I was expecting a target name after this".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ExtraSeparator => ParseErrorDetails {
                text: "".into(),
                hint: Some("Try removing it?".into()),
                label_text: "This is an extra delimiter".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ExprLparStart => ParseErrorDetails {
                text: "".into(),
                hint: Some(
                    "To group expressions in Gleam, use \"{\" and \"}\"; \
tuples are created with `#(` and `)`."
                        .into(),
                ),
                label_text: "This parenthesis cannot be understood here".into(),
                extra_labels: vec![],
            },

            ParseErrorType::IncorrectName => ParseErrorDetails {
                text: "".into(),
                hint: Some(wrap(
                    "Variable and module names start with a lowercase letter, \
and can contain a-z, 0-9, or _.",
                )),
                label_text: "I'm expecting a lowercase name here".into(),
                extra_labels: vec![],
            },

            ParseErrorType::IncorrectUpName => ParseErrorDetails {
                text: "".into(),
                hint: Some(wrap(
                    "Type names start with a uppercase letter, and can \
contain a-z, A-Z, or 0-9.",
                )),
                label_text: "I'm expecting a type name here".into(),
                extra_labels: vec![],
            },

            ParseErrorType::InvalidBitArraySegment => ParseErrorDetails {
                text: "See: https://tour.gleam.run/data-types/bit-arrays/".into(),
                hint: Some(format!(
                    "Valid BitArray segment options are:\n{}",
                    wrap(
                        "bits, bytes, int, float, utf8, utf16, utf32, utf8_codepoint, \
utf16_codepoint, utf32_codepoint, signed, unsigned, big, little, native, size, unit.",
                    )
                )),
                label_text: "This is not a valid BitArray segment option".into(),
                extra_labels: vec![],
            },

            ParseErrorType::InvalidBitArrayUnit => ParseErrorDetails {
                text: "See: https://tour.gleam.run/data-types/bit-arrays/".into(),
                hint: Some("Unit must be an integer literal >= 1 and <= 256.".into()),
                label_text: "This is not a valid BitArray unit value".into(),
                extra_labels: vec![],
            },

            ParseErrorType::InvalidTailPattern => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "This part of a list pattern can only be a name or a discard".into(),
                extra_labels: vec![],
            },

            ParseErrorType::InvalidTupleAccess => ParseErrorDetails {
                text: "".into(),
                hint: Some(
                    "Only non negative integer literals like 0, or 1_000 can be used.".into(),
                ),
                label_text: "This integer is not valid for tuple access".into(),
                extra_labels: vec![],
            },

            ParseErrorType::LexError { error: lex_err } => {
                let (label_text, text_lines) = lex_err.to_parse_error_info();
                let text = text_lines.join("\n");
                ParseErrorDetails {
                    text,
                    hint: None,
                    label_text: label_text.into(),
                    extra_labels: vec![],
                }
            }

            ParseErrorType::NestedBitArrayPattern => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "BitArray patterns cannot be nested".into(),
                extra_labels: vec![],
            },

            ParseErrorType::NotConstType => ParseErrorDetails {
                text: "See: https://tour.gleam.run/basics/constants/".into(),
                hint: None,
                label_text: "This type is not allowed in module constants".into(),
                extra_labels: vec![],
            },

            ParseErrorType::NoLetBinding => ParseErrorDetails {
                text: "See: https://tour.gleam.run/basics/assignments/".into(),
                hint: Some("Use let for binding.".into()),
                label_text: "There must be a 'let' to bind variable to value".into(),
                extra_labels: vec![],
            },

            ParseErrorType::NoValueAfterEqual => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "I was expecting to see a value after this equals sign".into(),
                extra_labels: vec![],
            },

            ParseErrorType::OpaqueTypeAlias => ParseErrorDetails {
                text: "See: https://tour.gleam.run/basics/type-aliases/".into(),
                hint: None,
                label_text: "Type Aliases cannot be opaque".into(),
                extra_labels: vec![],
            },

            ParseErrorType::OpNakedRight => ParseErrorDetails {
                text: "".into(),
                hint: Some("Remove it or put a value after it.".into()),
                label_text: "This operator has no value on its right side".into(),
                extra_labels: vec![],
            },

            ParseErrorType::TooManyArgHoles => ParseErrorDetails {
                text: "See: https://tour.gleam.run/functions/functions/".into(),
                hint: Some("Function calls can have at most one argument hole.".into()),
                label_text: "There is more than 1 argument hole in this function call".into(),
                extra_labels: vec![],
            },

            ParseErrorType::UnexpectedEof => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "The module ended unexpectedly".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ListSpreadWithoutElements => ParseErrorDetails {
                text: "See: https://tour.gleam.run/basics/lists/".into(),
                hint: Some("Try prepending some elements [1, 2, ..list].".into()),
                label_text: "This spread does nothing".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ListSpreadWithAnotherSpread {
                first_spread_location,
            } => ParseErrorDetails {
                text: [
                    "Lists are immutable and singly-linked, so to join two or more lists",
                    "all the elements of the lists would need to be copied into a new list.",
                    "This would be slow, so there is no built-in syntax for it.",
                ]
                .join("\n"),
                hint: None,
                label_text: "I wasn't expecting a second list here".into(),
                extra_labels: vec![ExtraLabel {
                    src_info: None,
                    label: Label {
                        text: Some("You're using a list here".into()),
                        span: *first_spread_location,
                    },
                }],
            },

            ParseErrorType::ListSpreadFollowedByElements => ParseErrorDetails {
                text: [
                    "Lists are immutable and singly-linked, so to append items to them",
                    "all the elements of a list would need to be copied into a new list.",
                    "This would be slow, so there is no built-in syntax for it.",
                    "",
                ]
                .join("\n"),
                hint: Some(
                    "Prepend items to the list and then reverse it once you are done.".into(),
                ),
                label_text: "I wasn't expecting elements after this".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ListPatternSpreadFollowedByElements => ParseErrorDetails {
                text: [
                    "Lists are immutable and singly-linked, so to match on the end",
                    "of a list would require the whole list to be traversed. This",
                    "would be slow, so there is no built-in syntax for it. Pattern",
                    "match on the start of the list instead.",
                ]
                .join("\n"),
                hint: None,
                label_text: "I wasn't expecting elements after this".into(),
                extra_labels: vec![],
            },

            ParseErrorType::UnexpectedReservedWord => ParseErrorDetails {
                text: "".into(),
                hint: Some("I was expecting to see a name here.".into()),
                label_text: "This is a reserved word".into(),
                extra_labels: vec![],
            },

            ParseErrorType::UnexpectedLabel => ParseErrorDetails {
                text: "Please remove the argument label.".into(),
                hint: None,
                label_text: "Argument labels are not allowed for anonymous functions".into(),
                extra_labels: vec![],
            },

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
                    Token::LeftParen
                    | Token::RightParen
                    | Token::LeftSquare
                    | Token::RightSquare
                    | Token::LeftBrace
                    | Token::RightBrace
                    | Token::Plus
                    | Token::Minus
                    | Token::Star
                    | Token::Slash
                    | Token::Less
                    | Token::Greater
                    | Token::LessEqual
                    | Token::GreaterEqual
                    | Token::Percent
                    | Token::PlusDot
                    | Token::MinusDot
                    | Token::StarDot
                    | Token::SlashDot
                    | Token::LessDot
                    | Token::GreaterDot
                    | Token::LessEqualDot
                    | Token::GreaterEqualDot
                    | Token::LtGt
                    | Token::Colon
                    | Token::Comma
                    | Token::Hash
                    | Token::Bang
                    | Token::Equal
                    | Token::EqualEqual
                    | Token::NotEqual
                    | Token::Vbar
                    | Token::VbarVbar
                    | Token::AmperAmper
                    | Token::LtLt
                    | Token::GtGt
                    | Token::Pipe
                    | Token::Dot
                    | Token::RArrow
                    | Token::LArrow
                    | Token::DotDot
                    | Token::At
                    | Token::EndOfFile
                    | Token::CommentNormal
                    | Token::CommentModule
                    | Token::NewLine
                    | Token::As
                    | Token::Assert
                    | Token::Auto
                    | Token::Case
                    | Token::Const
                    | Token::Delegate
                    | Token::Derive
                    | Token::Echo
                    | Token::Else
                    | Token::Fn
                    | Token::If
                    | Token::Implement
                    | Token::Import
                    | Token::Let
                    | Token::Macro
                    | Token::Opaque
                    | Token::Panic
                    | Token::Pub
                    | Token::Test
                    | Token::Todo
                    | Token::Type
                    | Token::Use => token.to_string(),
                };

                let messages = std::iter::once(format!("Found {found}, expected one of: "))
                    .chain(expected.iter().map(|s| format!("- {s}")));

                let messages = match hint {
                    Some(hint_text) => messages
                        .chain(std::iter::once(format!("Hint: {hint_text}")))
                        .collect_vec(),
                    _ => messages.collect(),
                };

                ParseErrorDetails {
                    text: messages.join("\n"),
                    hint: None,
                    label_text: "I was not expecting this".into(),
                    extra_labels: vec![],
                }
            }

            ParseErrorType::ConcatPatternVariableLeftHandSide => ParseErrorDetails {
                text: [
                    "We can't tell what size this prefix should be so we don't know",
                    "how to handle this pattern.",
                    "",
                    "If you want to match one character consider using `pop_grapheme`",
                    "from the stdlib's `gleam/string` module.",
                ]
                .join("\n"),
                hint: None,
                label_text: "This must be a string literal".into(),
                extra_labels: vec![],
            },

            ParseErrorType::UnexpectedFunction => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "Functions can only be called within other functions".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ListSpreadWithoutTail => ParseErrorDetails {
                text: "If a list expression has a spread then a tail must also be given.".into(),
                hint: None,
                label_text: "I was expecting a value after this spread".into(),
                extra_labels: vec![],
            },

            ParseErrorType::UnknownAttribute => ParseErrorDetails {
                text: "".into(),
                hint: Some("Try `deprecated`, `external` or `internal` instead.".into()),
                label_text: "I don't recognise this attribute".into(),
                extra_labels: vec![],
            },

            ParseErrorType::DuplicateAttribute => ParseErrorDetails {
                text: "This attribute has already been given.".into(),
                hint: None,
                label_text: "Duplicate attribute".into(),
                extra_labels: vec![],
            },

            ParseErrorType::UnknownTarget => ParseErrorDetails {
                text: "Try `erlang`, `javascript`.".into(),
                hint: None,
                label_text: "I don't recognise this target".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ExpectedFunctionBody => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "This function does not have a body".into(),
                extra_labels: vec![],
            },

            ParseErrorType::RedundantInternalAttribute => ParseErrorDetails {
                text: "Only a public definition can be annotated as internal.".into(),
                hint: Some("Remove the `@internal` annotation.".into()),
                label_text: "Redundant internal attribute".into(),
                extra_labels: vec![],
            },

            ParseErrorType::InvalidModuleTypePattern => ParseErrorDetails {
                text: [
                    "I'm expecting a pattern here",
                    "Hint: A pattern can be a constructor name, a literal value",
                    "or a variable to bind a value to, etc.",
                    "See: https://tour.gleam.run/flow-control/case-expressions/",
                ]
                .join("\n"),
                hint: None,
                label_text: "Invalid pattern".into(),
                extra_labels: vec![],
            },

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

                ParseErrorDetails {
                    text: [
                        "Each custom type variant must have a constructor:\n".into(),
                        format!("{accessor}{opaque}type {name} {{"),
                        format!("  {name}("),
                        format!("    {field}: {annotation},"),
                        "  )".into(),
                        "}".into(),
                    ]
                    .join("\n"),
                    hint: None,
                    label_text: "I was not expecting this".into(),
                    extra_labels: vec![],
                }
            }

            ParseErrorType::CallInClauseGuard => ParseErrorDetails {
                text: "Functions cannot be called in clause guards.".into(),
                hint: None,
                label_text: "Unsupported expression".into(),
                extra_labels: vec![],
            },

            ParseErrorType::IfExpression => ParseErrorDetails {
                text: [
                    "If you want to write a conditional expression you can use a `case`:",
                    "",
                    "    case condition {",
                    "      True -> todo",
                    "      False -> todo",
                    "    }",
                    "",
                    "See: https://tour.gleam.run/flow-control/case-expressions/",
                ]
                .join("\n"),
                hint: None,
                label_text: "Gleam doesn't have if expressions".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ConstantRecordConstructorNoArguments => ParseErrorDetails {
                text: "A record must be passed arguments when constructed.".into(),
                hint: None,
                label_text: "I was expecting arguments here".into(),
                extra_labels: vec![],
            },

            ParseErrorType::TypeDefinitionNoArguments => ParseErrorDetails {
                text: "A generic type must have at least a generic parameter.".into(),
                hint: Some("If a type is not generic you should omit the `()`.".into()),
                label_text: "I was expecting generic parameters here".into(),
                extra_labels: vec![],
            },

            ParseErrorType::UnknownAttributeRecordVariant => ParseErrorDetails {
                text: "".into(),
                hint: Some("Did you mean `@deprecated`?".into()),
                label_text: "This attribute cannot be used on a variant.".into(),
                extra_labels: vec![],
            },

            ParseErrorType::IncorrectImportModuleSeparator { module, item } => ParseErrorDetails {
                text: [
                    "Perhaps you meant one of:".into(),
                    "".into(),
                    format!("    import {module}/{item}"),
                    format!("    import {module}.{{item}}"),
                ]
                .join("\n"),
                hint: None,
                label_text: "I was expecting either `/` or `.{` here.".into(),
                extra_labels: vec![],
            },

            ParseErrorType::EmptyGuardBlock => ParseErrorDetails {
                text: "".into(),
                hint: None,
                label_text: "A clause guard block cannot be empty".into(),
                extra_labels: vec![],
            },

            ParseErrorType::ConstantInsideFunction => ParseErrorDetails {
                text: wrap(
                    "All variables are immutable in Gleam, so constants inside \
functions are not necessary.",
                ),
                hint: Some(
                    "Either move this into the global scope or use `let` binding instead.".into(),
                ),
                label_text: "Constants are not allowed inside functions".into(),
                extra_labels: vec![],
            },

            ParseErrorType::FunctionDefinitionAngleGenerics => ParseErrorDetails {
                text: "\
Generic function type variables do not need to be predeclared like they
would be in some other languages, instead they are written with lowercase
names.

    fn example(argument: generic) -> generic

See: https://tour.gleam.run/functions/generic-functions/"
                    .into(),
                hint: None,
                label_text: "I was expecting `(` here.".into(),
                extra_labels: vec![],
            },

            ParseErrorType::TypeUsageAngleGenerics {
                module,
                name,
                arguments,
            } => {
                let type_arguments = arguments
                    .iter()
                    .map(|argument| {
                        let mut argument_string = EcoString::new();
                        argument.print(&mut argument_string);
                        argument_string
                    })
                    .join(", ");
                let replacement_type = match module {
                    Some(module) => format!("{module}.{name}({type_arguments})"),
                    None => format!("{name}({type_arguments})"),
                };

                ParseErrorDetails {
                    text: format!(
                        "\
Type parameters use lowercase names and are surrounded by parentheses.

    {replacement_type}

See: https://tour.gleam.run/data-types/generic-custom-types/"
                    ),
                    hint: None,
                    label_text: "I was expecting `(` here.".into(),
                    extra_labels: vec![],
                }
            }

            ParseErrorType::TypeDefinitionAngleGenerics { name, arguments } => {
                let comma_separated_arguments = arguments.join(", ");

                ParseErrorDetails {
                    text: format!(
                        "\
Type parameters use lowercase names and are surrounded by parentheses.

    type {name}({comma_separated_arguments}) {{

See: https://tour.gleam.run/data-types/generic-custom-types/"
                    ),
                    hint: None,
                    label_text: "I was expecting `(` here.".into(),
                    extra_labels: vec![],
                }
            }
        }
    }
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
