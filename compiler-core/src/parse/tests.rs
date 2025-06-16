use crate::ast::SrcSpan;
use crate::parse::error::{
    InvalidUnicodeEscapeError, LexicalError, LexicalErrorType, ParseError, ParseErrorType,
};
use crate::parse::lexer::make_tokenizer;
use crate::parse::token::Token;
use crate::warning::WarningEmitter;
use camino::Utf8PathBuf;

use ecow::EcoString;
use itertools::Itertools;
use pretty_assertions::assert_eq;

macro_rules! assert_error {
    ($src:expr, $error:expr $(,)?) => {
        let result = crate::parse::parse_statement_sequence($src).expect_err("should not parse");
        assert_eq!(($src, $error), ($src, result),);
    };
    ($src:expr) => {
        let error = $crate::parse::tests::expect_error($src);
        let output = format!("----- SOURCE CODE\n{}\n\n----- ERROR\n{}", $src, error);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    };
}

macro_rules! assert_module_error {
    ($src:expr) => {
        let error = $crate::parse::tests::expect_module_error($src);
        let output = format!("----- SOURCE CODE\n{}\n\n----- ERROR\n{}", $src, error);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    };
}

macro_rules! assert_parse_module {
    ($src:expr) => {
        let result = crate::parse::parse_module(
            camino::Utf8PathBuf::from("test/path"),
            $src,
            &crate::warning::WarningEmitter::null(),
        )
        .expect("should parse");
        insta::assert_snapshot!(insta::internals::AutoName, &format!("{:#?}", result), $src);
    };
}

macro_rules! assert_parse {
    ($src:expr) => {
        let result = crate::parse::parse_statement_sequence($src).expect("should parse");
        insta::assert_snapshot!(insta::internals::AutoName, &format!("{:#?}", result), $src);
    };
}

pub fn expect_module_error(src: &str) -> String {
    let result =
        crate::parse::parse_module(Utf8PathBuf::from("test/path"), src, &WarningEmitter::null())
            .expect_err("should not parse");
    let error = crate::error::Error::Parse {
        src: src.into(),
        path: Utf8PathBuf::from("/src/parse/error.gleam"),
        error: Box::new(result),
    };
    error.pretty_string()
}

pub fn expect_error(src: &str) -> String {
    let result = crate::parse::parse_statement_sequence(src).expect_err("should not parse");
    let error = crate::error::Error::Parse {
        src: src.into(),
        path: Utf8PathBuf::from("/src/parse/error.gleam"),
        error: Box::new(result),
    };
    error.pretty_string()
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
fn string_bad_character_escape() {
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
}

#[test]
fn string_bad_character_escape_leading_backslash() {
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
fn string_freestanding_unicode_escape_sequence() {
    assert_error!(
        r#""\u""#,
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::InvalidUnicodeEscape(
                        InvalidUnicodeEscapeError::MissingOpeningBrace,
                    ),
                    location: SrcSpan { start: 2, end: 3 },
                }
            },
            location: SrcSpan { start: 2, end: 3 },
        }
    );
}

#[test]
fn string_unicode_escape_sequence_no_braces() {
    assert_error!(
        r#""\u65""#,
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::InvalidUnicodeEscape(
                        InvalidUnicodeEscapeError::MissingOpeningBrace,
                    ),
                    location: SrcSpan { start: 2, end: 3 },
                }
            },
            location: SrcSpan { start: 2, end: 3 },
        }
    );
}

#[test]
fn string_unicode_escape_sequence_invalid_hex() {
    assert_error!(
        r#""\u{z}""#,
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::InvalidUnicodeEscape(
                        InvalidUnicodeEscapeError::ExpectedHexDigitOrCloseBrace,
                    ),
                    location: SrcSpan { start: 4, end: 5 },
                }
            },
            location: SrcSpan { start: 4, end: 5 },
        }
    );
}

#[test]
fn string_unclosed_unicode_escape_sequence() {
    assert_error!(
        r#""\u{039a""#,
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::InvalidUnicodeEscape(
                        InvalidUnicodeEscapeError::ExpectedHexDigitOrCloseBrace,
                    ),
                    location: SrcSpan { start: 8, end: 9 },
                }
            },
            location: SrcSpan { start: 8, end: 9 },
        }
    );
}

#[test]
fn string_empty_unicode_escape_sequence() {
    assert_error!(
        r#""\u{}""#,
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::InvalidUnicodeEscape(
                        InvalidUnicodeEscapeError::InvalidNumberOfHexDigits,
                    ),
                    location: SrcSpan { start: 1, end: 5 },
                }
            },
            location: SrcSpan { start: 1, end: 5 },
        }
    );
}

#[test]
fn string_overlong_unicode_escape_sequence() {
    assert_error!(
        r#""\u{0011f601}""#,
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::InvalidUnicodeEscape(
                        InvalidUnicodeEscapeError::InvalidNumberOfHexDigits,
                    ),
                    location: SrcSpan { start: 1, end: 13 },
                }
            },
            location: SrcSpan { start: 1, end: 13 },
        }
    );
}

#[test]
fn string_invalid_unicode_escape_sequence() {
    assert_error!(
        r#""\u{110000}""#,
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::InvalidUnicodeEscape(
                        InvalidUnicodeEscapeError::InvalidCodepoint,
                    ),
                    location: SrcSpan { start: 1, end: 11 },
                }
            },
            location: SrcSpan { start: 1, end: 11 },
        }
    );
}

#[test]
fn bit_array() {
    // non int value in bit array unit option
    assert_error!(
        "let x = <<1:unit(0)>> x",
        ParseError {
            error: ParseErrorType::InvalidBitArrayUnit,
            location: SrcSpan { start: 17, end: 18 }
        }
    );
}

#[test]
fn bit_array1() {
    assert_error!(
        "let x = <<1:unit(257)>> x",
        ParseError {
            error: ParseErrorType::InvalidBitArrayUnit,
            location: SrcSpan { start: 17, end: 20 }
        }
    );
}

#[test]
fn bit_array2() {
    // patterns cannot be nested
    assert_error!(
        "case <<>> { <<<<1>>:bits>> -> 1 }",
        ParseError {
            error: ParseErrorType::NestedBitArrayPattern,
            location: SrcSpan { start: 14, end: 19 }
        }
    );
}

// https://github.com/gleam-lang/gleam/issues/3125
#[test]
fn triple_equals() {
    assert_error!(
        "let wobble:Int = 32
        wobble === 42",
        ParseError {
            error: ParseErrorType::LexError {
                error: LexicalError {
                    error: LexicalErrorType::InvalidTripleEqual,
                    location: SrcSpan { start: 35, end: 38 },
                }
            },
            location: SrcSpan { start: 35, end: 38 },
        }
    );
}

#[test]
fn triple_equals_with_whitespace() {
    assert_error!(
        "let wobble:Int = 32
        wobble ==     = 42",
        ParseError {
            error: ParseErrorType::OpNakedRight,
            location: SrcSpan { start: 35, end: 37 },
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

// https://github.com/gleam-lang/gleam/issues/1613
#[test]
fn anonymous_function_labeled_arguments() {
    assert_error!(
        "let anon_subtract = fn (minuend a: Int, subtrahend b: Int) -> Int {
  a - b
}",
        ParseError {
            location: SrcSpan { start: 24, end: 31 },
            error: ParseErrorType::UnexpectedLabel
        }
    );
}

#[test]
fn no_let_binding() {
    assert_error!(
        "wibble = 32",
        ParseError {
            location: SrcSpan { start: 7, end: 8 },
            error: ParseErrorType::NoLetBinding
        }
    );
}

#[test]
fn no_let_binding1() {
    assert_error!(
        "wibble:Int = 32",
        ParseError {
            location: SrcSpan { start: 6, end: 7 },
            error: ParseErrorType::NoLetBinding
        }
    );
}

#[test]
fn no_let_binding2() {
    assert_error!(
        "let wobble:Int = 32
        wobble = 42",
        ParseError {
            location: SrcSpan { start: 35, end: 36 },
            error: ParseErrorType::NoLetBinding
        }
    );
}

#[test]
fn no_let_binding3() {
    assert_error!(
        "[x] = [2]",
        ParseError {
            location: SrcSpan { start: 4, end: 5 },
            error: ParseErrorType::NoLetBinding
        }
    );
}

#[test]
fn with_let_binding3() {
    // The same with `let assert` must parse:
    assert_parse!("let assert [x] = [2]");
}

#[test]
fn with_let_binding3_and_annotation() {
    assert_parse!("let assert [x]: List(Int) = [2]");
}

#[test]
fn no_eq_after_binding() {
    assert_error!(
        "let wibble",
        ParseError {
            location: SrcSpan { start: 4, end: 10 },
            error: ParseErrorType::ExpectedEqual
        }
    );
}

#[test]
fn no_eq_after_binding1() {
    assert_error!(
        "let wibble
        wibble = 4",
        ParseError {
            location: SrcSpan { start: 4, end: 10 },
            error: ParseErrorType::ExpectedEqual
        }
    );
}

#[test]
fn echo_followed_by_expression_ends_where_expression_ends() {
    assert_parse!("echo wibble");
}

#[test]
fn echo_with_no_expressions_after_it() {
    assert_parse!("echo");
}

#[test]
fn echo_with_block() {
    assert_parse!("echo { 1 + 1 }");
}

#[test]
fn echo_has_lower_precedence_than_binop() {
    assert_parse!("echo 1 + 1");
}

#[test]
fn echo_in_a_pipeline() {
    assert_parse!("[] |> echo |> wibble");
}

#[test]
fn echo_has_lower_precedence_than_pipeline() {
    assert_parse!("echo wibble |> wobble |> woo");
}

#[test]
fn echo_cannot_have_an_expression_in_a_pipeline() {
    // So this is actually two pipelines!
    assert_parse!("[] |> echo fun |> wibble");
}

#[test]
fn panic_with_echo() {
    assert_parse!("panic as echo \"string\"");
}

#[test]
fn echo_with_panic() {
    assert_parse!("echo panic as \"a\"");
}

#[test]
fn repeated_echos() {
    assert_parse!("echo echo echo 1");
}

#[test]
fn echo_at_start_of_pipeline_wraps_the_whole_thing() {
    assert_parse!("echo 1 |> wibble |> wobble");
}

#[test]
fn no_let_binding_snapshot_1() {
    assert_error!("wibble = 4");
}

#[test]
fn no_let_binding_snapshot_2() {
    assert_error!("wibble:Int = 4");
}

#[test]
fn no_let_binding_snapshot_3() {
    assert_error!(
        "let wobble:Int = 32
        wobble = 42"
    );
}

#[test]
fn no_eq_after_binding_snapshot_1() {
    assert_error!("let wibble");
}
#[test]
fn no_eq_after_binding_snapshot_2() {
    assert_error!(
        "let wibble
        wibble = 4"
    );
}

#[test]
fn discard_left_hand_side_of_concat_pattern() {
    assert_error!(
        r#"
        case "" {
          _ <> rest -> rest
        }
        "#
    );
}

#[test]
fn assign_left_hand_side_of_concat_pattern() {
    assert_error!(
        r#"
        case "" {
          first <> rest -> rest
        }
        "#
    );
}

// https://github.com/gleam-lang/gleam/issues/1890
#[test]
fn valueless_list_spread_expression() {
    assert_error!(r#"let x = [1, 2, 3, ..]"#);
}

// https://github.com/gleam-lang/gleam/issues/2035
#[test]
fn semicolons() {
    assert_error!(r#"{ 2 + 3; - -5; }"#);
}

#[test]
fn bare_expression() {
    assert_parse!(r#"1"#);
}

// https://github.com/gleam-lang/gleam/issues/1991
#[test]
fn block_of_one() {
    assert_parse!(r#"{ 1 }"#);
}

// https://github.com/gleam-lang/gleam/issues/1991
#[test]
fn block_of_two() {
    assert_parse!(r#"{ 1 2 }"#);
}

// https://github.com/gleam-lang/gleam/issues/1991
#[test]
fn nested_block() {
    assert_parse!(r#"{ 1 { 1.0 2.0 } 3 }"#);
}

// https://github.com/gleam-lang/gleam/issues/1831
#[test]
fn argument_scope() {
    assert_error!(
        "
1 + let a = 5
a
"
    );
}

#[test]
fn multiple_external_for_same_project_erlang() {
    assert_module_error!(
        r#"
@external(erlang, "one", "two")
@external(erlang, "three", "four")
pub fn one(x: Int) -> Int {
  todo
}
"#
    );
}

#[test]
fn multiple_external_for_same_project_javascript() {
    assert_module_error!(
        r#"
@external(javascript, "one", "two")
@external(javascript, "three", "four")
pub fn one(x: Int) -> Int {
  todo
}
"#
    );
}

#[test]
fn unknown_external_target() {
    assert_module_error!(
        r#"
@external(erl, "one", "two")
pub fn one(x: Int) -> Int {
  todo
}"#
    );
}

#[test]
fn unknown_target() {
    assert_module_error!(
        r#"
@target(abc)
pub fn one() {}"#
    );
}

#[test]
fn missing_target() {
    assert_module_error!(
        r#"
@target()
pub fn one() {}"#
    );
}

#[test]
fn missing_target_and_bracket() {
    assert_module_error!(
        r#"
@target(
pub fn one() {}"#
    );
}

#[test]
fn unknown_attribute() {
    assert_module_error!(
        r#"@go_faster()
pub fn main() { 1 }"#
    );
}

#[test]
fn incomplete_function() {
    assert_error!("fn()");
}

#[test]
fn multiple_deprecation_attributes() {
    assert_module_error!(
        r#"
@deprecated("1")
@deprecated("2")
pub fn main() -> Nil {
  Nil
}
"#
    );
}

#[test]
fn deprecation_without_message() {
    assert_module_error!(
        r#"
@deprecated()
pub fn main() -> Nil {
  Nil
}
"#
    );
}

#[test]
fn multiple_internal_attributes() {
    assert_module_error!(
        r#"
@internal
@internal
pub fn main() -> Nil {
  Nil
}
"#
    );
}

#[test]
fn attributes_with_no_definition() {
    assert_module_error!(
        r#"
@deprecated("1")
@target(erlang)
"#
    );
}

#[test]
fn external_attribute_with_non_fn_definition() {
    assert_module_error!(
        r#"
@external(erlang, "module", "fun")
pub type Fun
"#
    );
}

#[test]
fn attributes_with_improper_definition() {
    assert_module_error!(
        r#"
@deprecated("1")
@external(erlang, "module", "fun")
"#
    );
}

#[test]
fn const_with_function_call() {
    assert_module_error!(
        r#"
pub fn wibble() { 123 }
const wib: Int = wibble()
"#
    );
}

#[test]
fn const_with_function_call_with_args() {
    assert_module_error!(
        r#"
pub fn wibble() { 123 }
const wib: Int = wibble(1, "wobble")
"#
    );
}

#[test]
fn import_type() {
    assert_parse_module!(r#"import wibble.{type Wobble, Wobble, type Wabble}"#);
}

#[test]
fn reserved_auto() {
    assert_module_error!(r#"const auto = 1"#);
}

#[test]
fn reserved_delegate() {
    assert_module_error!(r#"const delegate = 1"#);
}

#[test]
fn reserved_derive() {
    assert_module_error!(r#"const derive = 1"#);
}

#[test]
fn reserved_else() {
    assert_module_error!(r#"const else = 1"#);
}

#[test]
fn reserved_implement() {
    assert_module_error!(r#"const implement = 1"#);
}

#[test]
fn reserved_macro() {
    assert_module_error!(r#"const macro = 1"#);
}

#[test]
fn reserved_test() {
    assert_module_error!(r#"const test = 1"#);
}

#[test]
fn reserved_echo() {
    assert_module_error!(r#"const echo = 1"#);
}

#[test]
fn capture_with_name() {
    assert_module_error!(
        r#"
pub fn main() {
  add(_name, 1)
}

fn add(x, y) {
  x + y
}
"#
    );
}

#[test]
fn list_spread_with_no_tail_in_the_middle_of_a_list() {
    assert_module_error!(
        r#"
pub fn main() -> Nil {
  let xs = [1, 2, 3]
  [1, 2, .., 3 + 3, 4]
}
"#
    );
}

#[test]
fn list_spread_followed_by_extra_items() {
    assert_module_error!(
        r#"
pub fn main() -> Nil {
  let xs = [1, 2, 3]
  [1, 2, ..xs, 3 + 3, 4]
}
"#
    );
}

#[test]
fn list_spread_as_first_item_followed_by_other_items() {
    assert_module_error!(
        r#"
pub fn main() -> Nil {
  let xs = [1, 2, 3]
  [..xs, 3 + 3, 4]
}
"#
    );
}

// Tests for nested tuples and structs in tuples
// https://github.com/gleam-lang/gleam/issues/1980

#[test]
fn nested_tuples() {
    assert_parse!(
        r#"
let tup = #(#(5, 6))
{tup.0}.1
"#
    );
}

#[test]
fn nested_tuples_no_block() {
    assert_parse!(
        r#"
let tup = #(#(5, 6))
tup.0.1
"#
    );
}

#[test]
fn deeply_nested_tuples() {
    assert_parse!(
        r#"
let tup = #(#(#(#(4))))
{{{tup.0}.0}.0}.0
"#
    );
}

#[test]
fn deeply_nested_tuples_no_block() {
    assert_parse!(
        r#"
let tup = #(#(#(#(4))))
tup.0.0.0.0
"#
    );
}

#[test]
fn inner_single_quote_parses() {
    assert_parse!(
        r#"
let a = "inner 'quotes'"
"#
    );
}

#[test]
fn string_single_char_suggestion() {
    assert_module_error!(
        "
    pub fn main() {
        let a = 'example'
      }
    "
    );
}

#[test]
fn private_internal_const() {
    assert_module_error!(
        "
@internal
const wibble = 1
"
    );
}

#[test]
fn private_internal_type_alias() {
    assert_module_error!(
        "
@internal
type Alias = Int
"
    );
}

#[test]
fn private_internal_function() {
    assert_module_error!(
        "
@internal
fn wibble() { todo }
"
    );
}

#[test]
fn private_internal_type() {
    assert_module_error!(
        "
@internal
type Wibble {
  Wibble
}
"
    );
}

#[test]
fn wrong_record_access_pattern() {
    assert_module_error!(
        "
pub fn main() {
  case wibble {
    wibble.thing -> 1
  }
}
"
    );
}

#[test]
fn tuple_invalid_expr() {
    assert_module_error!(
        "
fn main() {
    #(1, 2, const)
}
"
    );
}

#[test]
fn bit_array_invalid_segment() {
    assert_module_error!(
        "
fn main() {
    <<72, 101, 108, 108, 111, 44, 32, 74, 111, 101, const>>
}
"
    );
}

#[test]
fn case_invalid_expression() {
    assert_module_error!(
        "
fn main() {
    case 1, type {
        _, _ -> 0
    }
}
"
    );
}

#[test]
fn case_invalid_case_pattern() {
    assert_module_error!(
        "
fn main() {
    case 1 {
        -> -> 0
    }
}
"
    );
}

#[test]
fn use_invalid_assignments() {
    assert_module_error!(
        "
fn main() {
    use fn <- result.try(get_username())
}
"
    );
}

#[test]
fn assignment_pattern_invalid_tuple() {
    assert_module_error!(
        "
fn main() {
    let #(a, case, c) = #(1, 2, 3)
}
"
    );
}

#[test]
fn assignment_pattern_invalid_bit_segment() {
    assert_module_error!(
        "
fn main() {
    let <<b1, pub>> = <<24, 3>>
}
"
    );
}

#[test]
fn case_list_pattern_after_spread() {
    assert_module_error!(
        "
fn main() {
    case somelist {
        [..rest, last] -> 1
        _ -> 2
    }
}
"
    );
}

#[test]
fn type_invalid_constructor() {
    assert_module_error!(
        "
type A {
    A(String)
    type
}
"
    );
}

// Tests whether diagnostic presents an example of how to formulate a proper
// record constructor based off a common user error pattern.
// https://github.com/gleam-lang/gleam/issues/3324

#[test]
fn type_invalid_record_constructor() {
    assert_module_error!(
        "
pub type User {
    name: String,
}
"
    );
}

#[test]
fn type_invalid_record_constructor_without_field_type() {
    assert_module_error!(
        "
pub opaque type User {
    name
}
"
    );
}

#[test]
fn type_invalid_record_constructor_invalid_field_type() {
    assert_module_error!(
        r#"
type User {
    name: "Test User",
}
"#
    );
}

#[test]
fn type_invalid_type_name() {
    assert_module_error!(
        "
type A(a, type) {
    A
}
"
    );
}

#[test]
fn type_invalid_constructor_arg() {
    assert_module_error!(
        "
type A {
    A(type: String)
}
"
    );
}

#[test]
fn type_invalid_record() {
    assert_module_error!(
        "
type A {
    One
    Two
    3
}
"
    );
}

#[test]
fn function_type_invalid_param_type() {
    assert_module_error!(
        "
fn f(g: fn(Int, 1) -> Int) -> Int {
  g(0, 1)
}
"
    );
}

#[test]
fn function_invalid_signature() {
    assert_module_error!(
        r#"
fn f(a, "b") -> String {
    a <> b
}
"#
    );
}

#[test]
fn const_invalid_tuple() {
    assert_module_error!(
        "
const a = #(1, 2, <-)
"
    );
}

#[test]
fn const_invalid_list() {
    assert_module_error!(
        "
const a = [1, 2, <-]
"
    );
}

#[test]
fn const_invalid_bit_array_segment() {
    assert_module_error!(
        "
const a = <<1, 2, <->>
"
    );
}

#[test]
fn const_invalid_record_constructor() {
    assert_module_error!(
        "
type A {
    A(String, Int)
}
const a = A(\"a\", let)
"
    );
}

// record access should parse even if there is no label written
#[test]
fn record_access_no_label() {
    assert_parse_module!(
        "
type Wibble {
    Wibble(wibble: String)
}

fn wobble() {
  Wibble(\"a\").
}
"
    );
}

#[test]
fn newline_tokens() {
    assert_eq!(
        make_tokenizer("1\n\n2\n").collect_vec(),
        [
            Ok((
                0,
                Token::Int {
                    value: "1".into(),
                    int_value: 1.into()
                },
                1
            )),
            Ok((1, Token::NewLine, 2)),
            Ok((2, Token::NewLine, 3)),
            Ok((
                3,
                Token::Int {
                    value: "2".into(),
                    int_value: 2.into()
                },
                4
            )),
            Ok((4, Token::NewLine, 5))
        ]
    );
}

// https://github.com/gleam-lang/gleam/issues/1756
#[test]
fn arithmetic_in_guards() {
    assert_parse!(
        "
case 2, 3 {
    x, y if x + y == 1 -> True
}"
    );
}

#[test]
fn const_string_concat() {
    assert_parse_module!(
        "
const cute = \"cute\"
const cute_bee = cute <> \"bee\"
"
    );
}

#[test]
fn const_string_concat_naked_right() {
    assert_module_error!(
        "
const no_cute_bee = \"cute\" <>
"
    );
}

#[test]
fn function_call_in_case_clause_guard() {
    assert_error!(
        r#"
let my_string = "hello"
case my_string {
    _ if length(my_string) > 2 -> io.debug("doesn't work')
}"#
    );
}

#[test]
fn dot_access_function_call_in_case_clause_guard() {
    assert_error!(
        r#"
let my_string = "hello"
case my_string {
    _ if string.length(my_string) > 2 -> io.debug("doesn't work')
}"#
    );
}

#[test]
fn invalid_left_paren_in_case_clause_guard() {
    assert_error!(
        r#"
let my_string = "hello"
case my_string {
    _ if string.length( > 2 -> io.debug("doesn't work')
}"#
    );
}

#[test]
fn invalid_label_shorthand() {
    assert_module_error!(
        "
pub fn main() {
  wibble(:)
}
"
    );
}

#[test]
fn invalid_label_shorthand_2() {
    assert_module_error!(
        "
pub fn main() {
  wibble(:,)
}
"
    );
}

#[test]
fn invalid_label_shorthand_3() {
    assert_module_error!(
        "
pub fn main() {
  wibble(:arg)
}
"
    );
}

#[test]
fn invalid_label_shorthand_4() {
    assert_module_error!(
        "
pub fn main() {
  wibble(arg::)
}
"
    );
}

#[test]
fn invalid_label_shorthand_5() {
    assert_module_error!(
        "
pub fn main() {
  wibble(arg::arg)
}
"
    );
}

#[test]
fn invalid_pattern_label_shorthand() {
    assert_module_error!(
        "
pub fn main() {
  let Wibble(:) = todo
}
"
    );
}

#[test]
fn invalid_pattern_label_shorthand_2() {
    assert_module_error!(
        "
pub fn main() {
  let Wibble(:arg) = todo
}
"
    );
}

#[test]
fn invalid_pattern_label_shorthand_3() {
    assert_module_error!(
        "
pub fn main() {
  let Wibble(arg::) = todo
}
"
    );
}

#[test]
fn invalid_pattern_label_shorthand_4() {
    assert_module_error!(
        "
pub fn main() {
  let Wibble(arg: arg:) = todo
}
"
    );
}

#[test]
fn invalid_pattern_label_shorthand_5() {
    assert_module_error!(
        "
pub fn main() {
  let Wibble(arg1: arg2:) = todo
}
"
    );
}

fn first_parsed_docstring(src: &str) -> EcoString {
    let parsed =
        crate::parse::parse_module(Utf8PathBuf::from("test/path"), src, &WarningEmitter::null())
            .expect("should parse");

    parsed
        .module
        .definitions
        .first()
        .expect("parsed a definition")
        .definition
        .get_doc()
        .expect("definition without doc")
}

#[test]
fn doc_comment_before_comment_is_not_attached_to_following_function() {
    assert_eq!(
        first_parsed_docstring(
            r#"
    /// Not included!
    // pub fn call()

    /// Doc!
    pub fn wibble() {}
"#
        ),
        " Doc!\n"
    )
}

#[test]
fn doc_comment_before_comment_is_not_attached_to_following_type() {
    assert_eq!(
        first_parsed_docstring(
            r#"
    /// Not included!
    // pub fn call()

    /// Doc!
    pub type Wibble
"#
        ),
        " Doc!\n"
    )
}

#[test]
fn doc_comment_before_comment_is_not_attached_to_following_type_alias() {
    assert_eq!(
        first_parsed_docstring(
            r#"
    /// Not included!
    // pub fn call()

    /// Doc!
    pub type Wibble = Int
"#
        ),
        " Doc!\n"
    )
}

#[test]
fn doc_comment_before_comment_is_not_attached_to_following_constant() {
    assert_eq!(
        first_parsed_docstring(
            r#"
    /// Not included!
    // pub fn call()

    /// Doc!
    pub const wibble = 1
"#
        ),
        " Doc!\n"
    );
}

#[test]
fn non_module_level_function_with_a_name() {
    assert_module_error!(
        r#"
pub fn main() {
  fn my() { 1 }
}
"#
    );
}

#[test]
fn error_message_on_variable_starting_with_underscore() {
    // https://github.com/gleam-lang/gleam/issues/3504
    assert_module_error!(
        "
  pub fn main() {
    let val = _func_starting_with_underscore(1)
  }"
    );
}

#[test]
fn non_module_level_function_with_not_a_name() {
    assert_module_error!(
        r#"
pub fn main() {
  fn @() { 1 }  // wrong token and not a name
}
"#
    );
}

#[test]
fn error_message_on_variable_starting_with_underscore2() {
    // https://github.com/gleam-lang/gleam/issues/3504
    assert_module_error!(
        "
  pub fn main() {
    case 1 {
      1 -> _with_underscore(1)
    }
  }"
    );
}

#[test]
fn function_inside_a_type() {
    assert_module_error!(
        r#"
type Wibble {
  fn wobble() {}
}
"#
    );
}

#[test]
fn pub_function_inside_a_type() {
    assert_module_error!(
        r#"
type Wibble {
  pub fn wobble() {}
}
"#
    );
}

#[test]
fn if_like_expression() {
    assert_module_error!(
        r#"
pub fn main() {
  let a = if wibble {
    wobble
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3730
#[test]
fn missing_constructor_arguments() {
    assert_module_error!(
        "
pub type A {
  A(Int)
}

const a = A()
"
    );
}

// https://github.com/gleam-lang/gleam/issues/3796
#[test]
fn missing_type_constructor_arguments_in_type_definition() {
    assert_module_error!(
        "
pub type A() {
  A(Int)
}
"
    );
}

#[test]
fn missing_type_constructor_arguments_in_type_annotation_1() {
    assert_module_error!("pub fn main() -> Int() {}");
}

#[test]
fn missing_type_constructor_arguments_in_type_annotation_2() {
    assert_module_error!(
        "pub fn main() {
  let a: Int() = todo
}"
    );
}

#[test]
fn tuple_without_hash() {
    assert_module_error!(
        r#"
pub fn main() {
    let triple = (1, 2.2, "three")
    io.debug(triple)
    let (a, *, *) = triple
    io.debug(a)
    io.debug(triple.1)
}
"#
    );
}

#[test]
fn deprecation_attribute_on_type_variant() {
    assert_parse_module!(
        r#"
type Wibble {
    @deprecated("1")
    Wibble1
    Wibble2
}
"#
    );
}

#[test]

fn float_empty_exponent() {
    assert_error!("1.32e");
}

#[test]
fn multiple_deprecation_attribute_on_type_variant() {
    assert_module_error!(
        r#"
type Wibble {
    @deprecated("1")
    @deprecated("2")
    Wibble1
    Wibble2
}
"#
    );
}

#[test]
fn target_attribute_on_type_variant() {
    assert_module_error!(
        r#"
type Wibble {
    @target(erlang)
    Wibble2
}
"#
    );
}

#[test]
fn internal_attribute_on_type_variant() {
    assert_module_error!(
        r#"
type Wibble {
    @internal
    Wibble1
}
"#
    );
}

#[test]
fn external_attribute_on_type_variant() {
    assert_module_error!(
        r#"
type Wibble {
    @external(erlang, "one", "two")
    Wibble1
}
"#
    );
}

#[test]
fn multiple_unsupported_attributes_on_type_variant() {
    assert_module_error!(
        r#"
type Wibble {
    @external(erlang, "one", "two")
    @target(erlang)
    @internal
    Wibble1
}
"#
    );
}

#[test]
// https://github.com/gleam-lang/gleam/issues/3870
fn nested_tuple_access_after_function() {
    assert_parse!("tuple().0.1");
}

#[test]
fn case_expression_without_body() {
    assert_parse!("case a");
}

#[test]
fn assert_statement() {
    assert_parse!("assert 10 != 11");
}

#[test]
fn assert_statement_with_message() {
    assert_parse!(r#"assert False as "Uh oh""#);
}

#[test]
fn assert_statement_without_expression() {
    assert_error!("assert");
}

#[test]
fn assert_statement_followed_by_statement() {
    assert_error!("assert let a = 10");
}

#[test]
fn special_error_for_pythonic_import() {
    assert_module_error!("import gleam.io");
}

#[test]
fn special_error_for_pythonic_neste_import() {
    assert_module_error!("import one.two.three");
}

#[test]
fn doesnt_issue_special_error_for_pythonic_import_if_slash() {
    assert_module_error!("import one/two.three");
}
