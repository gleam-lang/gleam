use crate::ast::SrcSpan;
use crate::error::wrap;
use heck::{ToSnakeCase, ToUpperCamelCase};

#[derive(Debug, PartialEq, Clone)]
pub struct LexicalError {
    pub error: LexicalErrorType,
    pub location: SrcSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LexicalErrorType {
    BadStringEscape,       // string contains an unescaped slash
    DigitOutOfRadix,       // 0x012 , 2 is out of radix
    NumTrailingUnderscore, // 1_000_ is not allowed
    RadixIntNoValue,       // 0x, 0b, 0o without a value
    UnexpectedStringEnd,   // Unterminated string literal
    UnrecognizedToken { tok: char },
    BadName { name: String },
    BadDiscardName { name: String },
    BadUpname { name: String },
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub error: ParseErrorType,
    pub location: SrcSpan,
}

impl ParseError {
    pub fn details(&self) -> (&'static str, Vec<String>) {
        match &self.error {
            ParseErrorType::ExpectedExpr => ("I was expecting an expression after this.", vec![]),
            ParseErrorType::ExpectedName => ("I was expecting a name here.", vec![]),
            ParseErrorType::ExpectedPattern => (
                "I was expecting a pattern after this.",
                vec!["See: https://gleam.run/book/tour/patterns".to_string()],
            ),
            ParseErrorType::ExpectedType => (
                "I was expecting a type after this.",
                vec!["See: https://gleam.run/book/tour/type-annotations".to_string()],
            ),
            ParseErrorType::ExpectedUpName => ("I was expecting a type name here.", vec![]),
            ParseErrorType::ExpectedValue => (
                "I was expecting a value after this.",
                vec!["See: https://gleam.run/book/tour/patterns".to_string()],
            ),
            ParseErrorType::ExtraSeparator => (
                "This is an extra delimiter.",
                vec!["Hint: Try removing it?".to_string()],
            ),
            ParseErrorType::ExprLparStart => (
                "This parenthesis cannot be understood here.",
                vec!["Hint: To group expressions in gleam use \"{\" and \"}\"".to_string()],
            ),
            ParseErrorType::ExprThenlessTry => (
                "A `try` cannot be the last expression.",
                vec!["Hint: Try using the value?".to_string()],
            ),
            ParseErrorType::IncorrectName => (
                "I'm expecting a lowercase name here.",
                vec![wrap(
                    "Hint: Variable and module names start with a lowercase letter, \
and can contain a-z, 0-9, or _.",
                )],
            ),
            ParseErrorType::IncorrectUpName => (
                "I'm expecting a type name here.",
                vec![wrap(
                    "Hint: Type names start with a uppercase letter, and can \
contain a-z, A-Z, or 0-9.",
                )],
            ),
            ParseErrorType::InvalidBitStringSegment => (
                "This is not a valid BitString segment option.",
                vec![
                    "Hint: Valid BitString segment options are:".to_string(),
                    wrap(
                        "binary, int, float, bit_string, utf8, utf16, utf32, utf8_codepoint, \
utf16_codepoint, utf32_codepoint, signed, unsigned, big, little, native, size, unit",
                    ),
                    "See: https://gleam.run/book/tour/bit-strings".to_string(),
                ],
            ),
            ParseErrorType::InvalidBitStringUnit => (
                "This is not a valid BitString unit value.",
                vec![
                    "Hint: unit must be an integer literal >= 1 and <= 256".to_string(),
                    "See: https://gleam.run/book/tour/bit-strings".to_string(),
                ],
            ),
            ParseErrorType::InvalidTailPattern => (
                "This part of a list pattern can only be a name or a discard.",
                vec!["See: https://gleam.run/book/tour/patterns".to_string()],
            ),
            ParseErrorType::InvalidTupleAccess => (
                "This integer is not valid for tuple access.",
                vec![
                    "Hint: Only non negative integer literals like 0, or 1_000 can be used."
                        .to_string(),
                ],
            ),
            ParseErrorType::LexError { error: lex_err } => lex_err.to_parse_error_info(),
            ParseErrorType::NestedBitStringPattern => (
                "BitString patterns cannot be nested.",
                vec!["See: https://gleam.run/book/tour/patterns".to_string()],
            ),
            ParseErrorType::NoCaseClause => (
                "This case expression has no clauses.",
                vec!["See: https://gleam.run/book/tour/case-expressions".to_string()],
            ),
            ParseErrorType::NoConstructors => (
                "Custom types must have at least 1 constructor.",
                vec!["See: https://gleam.run/book/tour/custom-types".to_string()],
            ),
            ParseErrorType::NotConstType => (
                "This type is not allowed in module constants.",
                vec!["See: https://gleam.run/book/tour/constants".to_string()],
            ),
            ParseErrorType::NoExpression => (
                "There must be an expression in here.",
                vec!["Hint: Put an expression in there or remove the brackets.".to_string()],
            ),
            ParseErrorType::NoValueAfterEqual => (
                "I was expecting to see a value after this equals sign.",
                vec![],
            ),
            ParseErrorType::OpaqueTypeAlias => (
                "Type Aliases cannot be opaque",
                vec!["See: https://gleam.run/book/tour/type-aliases".to_string()],
            ),
            ParseErrorType::OpNakedRight => (
                "This operator has no value on its right side.",
                vec!["Hint: Remove it or put a value after it.".to_string()],
            ),
            ParseErrorType::TooManyArgHoles => (
                "There is more than 1 argument hole in this function call.",
                vec![
                    "Hint: Function calls can have at most one argument hole.".to_string(),
                    "See: https://gleam.run/book/tour/functions".to_string(),
                ],
            ),
            ParseErrorType::UnexpectedEof => ("The module ended unexpectedly.", vec![]),
            ParseErrorType::ListSpreadWithoutElements => (
                "This spread does nothing",
                vec![
                    "Hint: Try prepending some elements [1, 2, ..list].".to_string(),
                    "See: https://gleam.run/book/tour/lists.html".to_string(),
                ],
            ),
            ParseErrorType::UnexpectedReservedWord => (
                "This is a reserved word.",
                vec![
                    "Hint: I was expecting to see a name here.".to_string(),
                    "See: https://gleam.run/book/tour/reserved-words".to_string(),
                ],
            ),
            ParseErrorType::LowcaseBooleanPattern => (
                "Did you want a Bool instead of a variable?",
                vec![
                    "Hint: In Gleam boolean literals are True and False".to_string(),
                    "See: https://gleam.run/book/tour/bools.html".to_string(),
                ],
            ),
            ParseErrorType::UnexpectedToken { expected, hint } => {
                let mut messages = expected.clone();
                if let Some(s) = messages.first_mut() {
                    *s = format!("Expected one of: {}", s);
                }

                if let Some(hint_text) = hint {
                    messages.push(format!("Hint: {}", hint_text));
                }

                ("I was not expecting this.", messages)
            }
            ParseErrorType::ExpectedBoolean => (
                "Did you mean to negate a boolean?",
                // TODO (HarryET): Get a hint for missing boolean
                vec![],
            ),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseErrorType {
    ExpectedExpr,            // after "->" in a case clause
    ExpectedName,            // any token used when a Name was expected
    ExpectedPattern,         // after ':' where a pattern is expected
    ExpectedType,            // after ':' or '->' where a type annotation is expected
    ExpectedUpName,          // any token used when a UpName was expected
    ExpectedValue,           // no value after "="
    ExprLparStart,           // it seems "(" was used to start an expression
    ExprThenlessTry,         // a try in the tail position of an expression sequence
    ExtraSeparator,          // #(1,,) <- the 2nd comma is an extra separator
    IncorrectName,           // UpName or DiscardName used when Name was expected
    IncorrectUpName,         // Name or DiscardName used when UpName was expected
    InvalidBitStringSegment, // <<7:hello>> `hello` is an invalid bitstring segment
    InvalidBitStringUnit,    // in <<1:unit(x)>> x must be 1 <= x <= 256
    InvalidTailPattern,      // only name and _name are allowed after ".." in list pattern
    InvalidTupleAccess,      // only positive int literals for tuple access
    LexError {
        error: LexicalError,
    },
    NestedBitStringPattern,    // <<<<1>>, 2>>, <<1>> is not allowed in there
    NoConstructors,            // A type "A {}" must have at least one constructor
    NoCaseClause,              // a case with no claueses
    NoExpression, // between "{" and "}" in expression position, there must be an expression
    NoValueAfterEqual, // = <something other than a value>
    NotConstType, // :fn(), name, _  are not valid const types
    OpNakedRight, // Operator with no value to the right
    OpaqueTypeAlias, // Type aliases cannot be opaque
    TooManyArgHoles, // a function call can have at most 1 arg hole
    ListSpreadWithoutElements, // Pointless spread: `[..xs]`
    LowcaseBooleanPattern, // most likely user meant True or False in patterns
    UnexpectedEof,
    UnexpectedReservedWord, // reserved word used when a name was expected
    UnexpectedToken {
        expected: Vec<String>,
        hint: Option<String>,
    },
    ExpectedBoolean,
}

impl LexicalError {
    pub fn to_parse_error_info(&self) -> (&'static str, Vec<String>) {
        match &self.error {
            LexicalErrorType::BadStringEscape => (
                "I don't understand this escape code",
                vec![
                    "Hint: Add another backslash before it.".to_string(),
                    "See: https://gleam.run/book/tour/strings.html#escape-sequences".to_string(),
                ],
            ),
            LexicalErrorType::DigitOutOfRadix => {
                ("This digit is too big for the specified radix.", vec![])
            }
            LexicalErrorType::NumTrailingUnderscore => (
                "Numbers cannot have a trailing underscore.",
                vec!["Hint: remove it.".to_string()],
            ),
            LexicalErrorType::RadixIntNoValue => ("This integer has no value.", vec![]),
            LexicalErrorType::UnexpectedStringEnd => {
                ("The string starting here was left open.", vec![])
            }
            LexicalErrorType::UnrecognizedToken { .. } => (
                "I can't figure out what to do with this character.",
                vec!["Hint: Is it a typo?".to_string()],
            ),
            LexicalErrorType::BadName { name } => (
                "This is not a valid name.",
                vec![
                    "Hint: Names start with a lowercase letter and contain a-z, 0-9, or _."
                        .to_string(),
                    format!("Try: {}", name.to_snake_case()),
                ],
            ),
            LexicalErrorType::BadDiscardName { name } => (
                "This is not a valid discard name.",
                vec![
                    "Hint: Discard names start with _ and contain a-z, 0-9, or _.".to_string(),
                    format!("Try: _{}", name.to_snake_case()),
                ],
            ),
            LexicalErrorType::BadUpname { name } => (
                "This is not a valid upname.",
                vec![
                    "Hint: Upnames start with an uppercase letter and contain".to_string(),
                    "only lowercase letters, numbers, and uppercase letters.".to_string(),
                    format!("Try: {}", name.to_upper_camel_case()),
                ],
            ),
        }
    }
}
