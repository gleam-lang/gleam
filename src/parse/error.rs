use crate::ast::SrcSpan;

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct LexicalError {
    pub error: LexicalErrorType,
    pub location: usize,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum LexicalErrorType {
    UnexpectedStringEnd,
    NestingLeftOpen,
    NoNestingToClose,
    UnrecognizedToken { tok: char },
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub error: ParseErrorType,
    pub location: SrcSpan,
}

#[derive(Debug, PartialEq)]
pub enum ParseErrorType {
    ExpectedExpr,       // after "->" in a case clause
    ExpectedName,       // any token used when a Name was expected
    ExpectedPattern,    // after ':' where a pattern is expected
    ExpectedType,       // after ':' or '->' where a type annotation is expected
    ExpectedUpName,     // any token used when a UpName was expected
    ExpectedValue,      // no value after "="
    ExprLparStart,      // it seems "(" was used to start an expression
    ExprTailBinding,    // a binding in the tail position of an expression sequence
    ExtraSeparator,     // tuple(1,,) <- the 2nd comma is an extra separator
    IncorrectName,      // UpName or DiscardName used when Name was expected
    IncorrectUpName,    // Name or DiscardName used when UpName was expected
    InvalidTailPattern, // only name and _name are allowed after ".." in list pattern
    InvalidTupleAccess, // only positive int literals for tuple access
    LexError { error: LexicalError },
    ListNilNotAllowed, // [] is not allowed here
    NoConstructors,    // A type "A {}" must have at least one constructor
    NoCaseClause,      // a case with no claueses
    NoExpression,      // between "{" and "}" in expression position, there must be an expression
    NoValueAfterEqual, // = <something other than a value>
    NotConstType,      // :fn(), name, _  are not valid const types
    OpNakedRight,      // Operator with no value to the right
    OpaqueTypeAlias,   // Type aliases cannot be opaque
    TooManyArgHoles,   // a function call can have at most 1 arg hole
    UnexpectedEOF,
    UnexpectedReservedWord, // reserved word used when a name was expected
    UnexpectedToken { expected: Vec<String> },
}

impl LexicalError {
    pub fn to_parse_error_info(&self) -> (&str, Vec<String>) {
        match self.error {
            LexicalErrorType::UnexpectedStringEnd => {
                ("The string starting here was left open.", vec![])
            }
            LexicalErrorType::NestingLeftOpen => (
                "A bracket of some kind was left open.",
                vec!["Hint: Close it :)".to_string()],
            ),
            LexicalErrorType::NoNestingToClose => (
                "There is no corresponding openening bracket for this.",
                vec!["Hint: Remove it?".to_string()],
            ),
            LexicalErrorType::UnrecognizedToken { .. } => (
                "I can't figure out what to do with this character.",
                vec!["Hint: Is it a typo?".to_string()],
            ),
        }
    }
}
