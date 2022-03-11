use crate::ast::SrcSpan;
use crate::parse::error::{LexicalError, LexicalErrorType, ParseError, ParseErrorType};

use pretty_assertions::assert_eq;

macro_rules! assert_error {
    ($src:expr, $error:expr $(,)?) => {
        let result = crate::parse::parse_expression_sequence($src).expect_err("should not parse");
        assert_eq!(($src, $error), ($src, result),);
    };
}

#[test]
fn int_tests() {
    // bad binary digit
    assert_error!(
        "0b012",
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::DigitOutOfRadix,
                    location: SrcSpan { start: 4, end: 4 },
                }
            },
            location: SrcSpan { start: 4, end: 4 },
        }
    );
    // bad octal digit
    assert_error!(
        "0o12345678",
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::DigitOutOfRadix,
                    location: SrcSpan { start: 9, end: 9 },
                }
            },
            location: SrcSpan { start: 9, end: 9 },
        }
    );
    // no int value
    assert_error!(
        "0x",
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::RadixIntNoValue,
                    location: SrcSpan { start: 1, end: 1 },
                }
            },
            location: SrcSpan { start: 1, end: 1 },
        }
    );
    // trailing underscore
    assert_error!(
        "1_000_",
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::NumTrailingUnderscore,
                    location: SrcSpan { start: 5, end: 5 },
                }
            },
            location: SrcSpan { start: 5, end: 5 },
        }
    );
}

#[test]
fn string_tests() {
    // bad character escape
    assert_error!(
        r#""\g""#,
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::BadStringEscape,
                    location: SrcSpan { start: 1, end: 2 },
                }
            },
            location: SrcSpan { start: 1, end: 2 },
        }
    );

    // still bad character escape
    assert_error!(
        r#""\\\g""#,
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::BadStringEscape,
                    location: SrcSpan { start: 3, end: 4 },
                }
            },
            location: SrcSpan { start: 3, end: 4 },
        }
    );
}

#[test]
fn bit_string_tests() {
    // non int value in BitString unit option
    assert_error!(
        "let x = <<1:unit(0)>> x",
        ParseError {
            error: ParseErrorType::InvalidBitStringUnit,
            location: SrcSpan { start: 17, end: 18 }
        }
    );

    assert_error!(
        "let x = <<1:unit(257)>> x",
        ParseError {
            error: ParseErrorType::InvalidBitStringUnit,
            location: SrcSpan { start: 17, end: 20 }
        }
    );

    // patterns cannot be nested
    assert_error!(
        "case <<>> { <<<<1>>:bit_string>> -> 1 }",
        ParseError {
            error: ParseErrorType::NestedBitStringPattern,
            location: SrcSpan { start: 14, end: 19 }
        }
    );
}

#[test]
fn name_tests() {
    assert_error!(
        "let xS = 1",
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::BadName {
                        name: "xS".to_string()
                    },
                    location: SrcSpan { start: 4, end: 6 },
                }
            },
            location: SrcSpan { start: 4, end: 6 },
        }
    );

    assert_error!(
        "let _xS = 1",
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::BadDiscardName {
                        name: "_xS".to_string()
                    },
                    location: SrcSpan { start: 4, end: 7 },
                }
            },
            location: SrcSpan { start: 4, end: 7 },
        }
    );

    assert_error!(
        "type S_m = String",
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::BadUpname {
                        name: "S_m".to_string()
                    },
                    location: SrcSpan { start: 5, end: 8 },
                }
            },
            location: SrcSpan { start: 5, end: 8 },
        }
    );
}

// https://github.com/gleam-lang/gleam/issues/1231
#[test]
fn pointless_spread() {
    assert_error!(
        "let xs = [] [..xs]",
        ParseError {
            error: ParseErrorType::ListSpreadWithoutElements,
            location: SrcSpan { start: 12, end: 18 },
        }
    );
}

// https://github.com/gleam-lang/gleam/issues/1358
#[test]
fn lowcase_bool_in_pattern() {
    assert_error!(
        "case 42 > 42 { true -> 1; false -> 2; }",
        ParseError {
            error: ParseErrorType::LowcaseBooleanPattern,
            location: SrcSpan { start: 15, end: 19 },
        }
    );
}

// https://github.com/gleam-lang/gleam/issues/1404
#[test]
fn clause_mutiple_expressions() {
    assert_error!(
        "case True {
True ->
  let a = 1
  a + 1
False -> 0
}
",
        ParseError {
            location: SrcSpan { start: 36, end: 37 },
            error: ParseErrorType::UnexpectedToken {
                expected: vec!["\"->\"".to_string()],
                hint: Some("Did you mean to wrap a multi line clause in curly braces?".to_string())
            },
        }
    );
}
