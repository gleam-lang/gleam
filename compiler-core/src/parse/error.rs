use crate::ast::SrcSpan;
use crate::error::wrap;
use heck::{ToSnakeCase, ToUpperCamelCase};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LexicalError {
    pub error: LexicalErrorType,
    pub location: SrcSpan,
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    pub error: ParseErrorType,
    pub location: SrcSpan,
}

impl ParseError {
    pub fn details(&self) -> (&'static str, Vec<String>) {
        match &self.error {
            ParseErrorType::ExpectedEqual => ("I was expecting a '=' after this", vec![]),
            ParseErrorType::ExpectedExpr => ("I was expecting an expression after this.", vec![]),
            ParseErrorType::ExpectedName => ("I was expecting a name here.", vec![]),
            ParseErrorType::ExpectedPattern => (
                "I was expecting a pattern after this.",
                vec!["See: https://gleam.run/book/tour/patterns".into()],
            ),
            ParseErrorType::ExpectedType => (
                "I was expecting a type after this.",
                vec!["See: https://gleam.run/book/tour/type-annotations".into()],
            ),
            ParseErrorType::ExpectedUpName => ("I was expecting a type name here.", vec![]),
            ParseErrorType::ExpectedValue => (
                "I was expecting a value after this.",
                vec!["See: https://gleam.run/book/tour/patterns".into()],
            ),
            ParseErrorType::ExtraSeparator => (
                "This is an extra delimiter.",
                vec!["Hint: Try removing it?".into()],
            ),
            ParseErrorType::ExprLparStart => (
                "This parenthesis cannot be understood here.",
                vec!["Hint: To group expressions in gleam use \"{\" and \"}\"".into()],
            ),
            ParseErrorType::ExprThenlessTry => (
                "A `try` cannot be the last expression.",
                vec!["Hint: Try using the value?".into()],
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
                    "Hint: Valid BitString segment options are:".into(),
                    wrap(
                        "binary, int, float, bit_string, utf8, utf16, utf32, utf8_codepoint, \
utf16_codepoint, utf32_codepoint, signed, unsigned, big, little, native, size, unit",
                    ),
                    "See: https://gleam.run/book/tour/bit-strings".into(),
                ],
            ),
            ParseErrorType::InvalidBitStringUnit => (
                "This is not a valid BitString unit value.",
                vec![
                    "Hint: unit must be an integer literal >= 1 and <= 256".into(),
                    "See: https://gleam.run/book/tour/bit-strings".into(),
                ],
            ),
            ParseErrorType::InvalidTailPattern => (
                "This part of a list pattern can only be a name or a discard.",
                vec!["See: https://gleam.run/book/tour/patterns".into()],
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
                vec!["See: https://gleam.run/book/tour/patterns".into()],
            ),
            ParseErrorType::NoCaseClause => (
                "This case expression has no clauses.",
                vec!["See: https://gleam.run/book/tour/case-expressions".into()],
            ),
            ParseErrorType::NoConstructors => (
                "Custom types must have at least 1 constructor.",
                vec!["See: https://gleam.run/book/tour/custom-types".into()],
            ),
            ParseErrorType::NotConstType => (
                "This type is not allowed in module constants.",
                vec!["See: https://gleam.run/book/tour/constants".into()],
            ),
            ParseErrorType::NoExpression => (
                "There must be an expression in here.",
                vec!["Hint: Put an expression in there or remove the brackets.".into()],
            ),
            ParseErrorType::NoLetBinding => (
                "There must be a 'let' to bind variable to value",
                vec![
                    "Hint: Use let for binding".into(),
                    "See: https://gleam.run/book/tour/let-bindings".into(),
                ],
            ),
            ParseErrorType::NoValueAfterEqual => (
                "I was expecting to see a value after this equals sign.",
                vec![],
            ),
            ParseErrorType::OpaqueTypeAlias => (
                "Type Aliases cannot be opaque",
                vec!["See: https://gleam.run/book/tour/type-aliases".into()],
            ),
            ParseErrorType::OpNakedRight => (
                "This operator has no value on its right side.",
                vec!["Hint: Remove it or put a value after it.".into()],
            ),
            ParseErrorType::TooManyArgHoles => (
                "There is more than 1 argument hole in this function call.",
                vec![
                    "Hint: Function calls can have at most one argument hole.".into(),
                    "See: https://gleam.run/book/tour/functions".into(),
                ],
            ),
            ParseErrorType::UnexpectedEof => ("The module ended unexpectedly.", vec![]),
            ParseErrorType::ListSpreadWithoutElements => (
                "This spread does nothing",
                vec![
                    "Hint: Try prepending some elements [1, 2, ..list].".into(),
                    "See: https://gleam.run/book/tour/lists.html".into(),
                ],
            ),
            ParseErrorType::UnexpectedReservedWord => (
                "This is a reserved word.",
                vec![
                    "Hint: I was expecting to see a name here.".into(),
                    "See: https://gleam.run/book/tour/reserved-words".into(),
                ],
            ),
            ParseErrorType::LowcaseBooleanPattern => (
                "Did you want a Bool instead of a variable?",
                vec![
                    "Hint: In Gleam boolean literals are True and False".into(),
                    "See: https://gleam.run/book/tour/bools.html".into(),
                ],
            ),
            ParseErrorType::UnexpectedLabel => (
                "Argument labels are not allowed for anonymous functions.",
                vec!["Please remove the argument label.".into()],
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
                // TODO: (HarryET): Get a hint for missing boolean
                vec![],
            ),
            ParseErrorType::ConcatPatternVariableLeftHandSide => (
                "This must be a string literal",
                vec![
                    "We can't tell what size this prefix should be so we don't know".into(),
                    "how to handle this pattern.".into(),
                    "".into(),
                    "If you want to match one character consider using `pop_grapheme`".into(),
                    "from the stdlib's `gleam/string` module".into(),
                ],
            ),
            ParseErrorType::UnexpectedFunction => (
                "Functions can only be called within other functions.",
                vec![],
            ),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseErrorType {
    ExpectedEqual,           // expect "="
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
    NoCaseClause,              // a case with no clauses
    NoExpression, // between "{" and "}" in expression position, there must be an expression
    NoLetBinding, // Bindings and rebinds always require let and must always bind to a value.
    NoValueAfterEqual, // = <something other than a value>
    NotConstType, // :fn(), name, _  are not valid const types
    OpNakedRight, // Operator with no value to the right
    OpaqueTypeAlias, // Type aliases cannot be opaque
    TooManyArgHoles, // a function call can have at most 1 arg hole
    ListSpreadWithoutElements, // Pointless spread: `[..xs]`
    LowcaseBooleanPattern, // most likely user meant True or False in patterns
    UnexpectedLabel, // argument labels were provided, but are not supported in this context
    UnexpectedEof,
    UnexpectedReservedWord, // reserved word used when a name was expected
    UnexpectedToken {
        expected: Vec<String>,
        hint: Option<String>,
    },
    ExpectedBoolean,
    UnexpectedFunction, // a function was used called outside of another function
    // A variable was assigned or discarded on the left hand side of a <> pattern
    ConcatPatternVariableLeftHandSide,
}

impl LexicalError {
    pub fn to_parse_error_info(&self) -> (&'static str, Vec<String>) {
        match &self.error {
            LexicalErrorType::BadStringEscape => (
                "I don't understand this escape code",
                vec![
                    "Hint: Add another backslash before it.".into(),
                    "See: https://gleam.run/book/tour/strings.html#escape-sequences".into(),
                ],
            ),
            LexicalErrorType::DigitOutOfRadix => {
                ("This digit is too big for the specified radix.", vec![])
            }
            LexicalErrorType::NumTrailingUnderscore => (
                "Numbers cannot have a trailing underscore.",
                vec!["Hint: remove it.".into()],
            ),
            LexicalErrorType::RadixIntNoValue => ("This integer has no value.", vec![]),
            LexicalErrorType::UnexpectedStringEnd => {
                ("The string starting here was left open.", vec![])
            }
            LexicalErrorType::UnrecognizedToken { .. } => (
                "I can't figure out what to do with this character.",
                vec!["Hint: Is it a typo?".into()],
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
                    "Hint: Discard names start with _ and contain a-z, 0-9, or _.".into(),
                    format!("Try: _{}", name.to_snake_case()),
                ],
            ),
            LexicalErrorType::BadUpname { name } => (
                "This is not a valid upname.",
                vec![
                    "Hint: Upnames start with an uppercase letter and contain".into(),
                    "only lowercase letters, numbers, and uppercase letters.".into(),
                    format!("Try: {}", name.to_upper_camel_case()),
                ],
            ),
        }
    }
}
