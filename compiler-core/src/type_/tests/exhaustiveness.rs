use crate::{assert_error, assert_module_error, assert_no_warnings, assert_warning};

#[test]
fn whatever() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    _ -> 0
  }
}
"
    );
}

#[test]
fn nil() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    Nil -> 0
  }
}
"
    );
}

#[test]
fn bool() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    True -> 1
    False -> 0
  }
}
"
    );
}

#[test]
fn bool_true() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    True -> 1
  }
}
"
    );
}

#[test]
fn bool_false() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    False -> 1
  }
}
"
    );
}

#[test]
fn result() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    Ok(_) -> 1
    Error(_) -> 2
  }
}
"
    );
}

#[test]
fn result_ok() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    Ok(_) -> 1
  }
}
"
    );
}

#[test]
fn result_error() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    Error(_) -> 1
  }
}
"
    );
}

#[test]
fn result_nil() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    Ok(Nil) -> 1
    Error(Nil) -> 2
  }
}
"
    );
}

#[test]
fn result_nil_ok() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    Ok(Nil) -> 1
  }
}
"
    );
}

#[test]
fn result_nil_error() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    Error(Nil) -> 1
  }
}
"
    );
}

#[test]
fn result_bool() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    Ok(True) -> 1
    Ok(False) -> 3
    Error(True) -> 2
    Error(False) -> 4
  }
}
"
    );
}

#[test]
fn result_bool_1() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    Ok(False) -> 1
    Error(True) -> 2
    Error(False) -> 3
  }
}
"
    );
}

#[test]
fn result_bool_2() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    Ok(True) -> 1
    Error(True) -> 2
    Error(False) -> 3
  }
}
"
    );
}

#[test]
fn result_bool_3() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    Ok(True) -> 1
    Ok(False) -> 2
    Error(False) -> 3
  }
}
"
    );
}

#[test]
fn result_bool_4() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    Ok(True) -> 1
    Ok(False) -> 2
    Error(True) -> 3
  }
}
"
    );
}

#[test]
fn result_bool_5() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    Ok(True) -> 1
    Ok(False) -> 2
  }
}
"
    );
}

#[test]
fn result_bool_6() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    Error(True) -> 1
    Error(False) -> 2
  }
}
"
    );
}

#[test]
fn result_bool_7() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    Error(True) -> 1
  }
}
"
    );
}

#[test]
fn result_bool_8() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    Ok(False) -> 1
  }
}
"
    );
}

#[test]
fn list() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    [_, ..] -> 1
    [] -> 2
  }
}
"
    );
}

#[test]
fn list_empty() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    [] -> 1
  }
}
"
    );
}

#[test]
fn list_non_empty() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    [_, ..] -> 1
  }
}
"
    );
}

#[test]
fn list_one() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    [_] -> 1
  }
}
"
    );
}

#[test]
fn list_one_two() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    [_] -> 1
    [_, _] -> 1
  }
}
"
    );
}

#[test]
fn list_zero_one_two() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    [] -> 1
    [_] -> 1
    [_, _] -> 1
  }
}
"
    );
}

#[test]
fn list_zero_one_two_any() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    [] -> 1
    [_] -> 1
    [_, _] -> 1
    [_, _, ..] -> 1
  }
}
"
    );
}

#[test]
fn list_zero_two_any() {
    assert_module_error!(
        "
pub fn main(x) {
  case x {
    [] -> 1
    [_, _] -> 1
    [_, _, ..] -> 1
  }
}
"
    );
}

#[test]
fn string() {
    assert_no_warnings!(
        r#"
pub fn main(x) {
  case x {
    "" -> 1
    "a" -> 1
    "b" -> 1
    _ -> 1
  }
}
"#
    );
}

#[test]
fn string_1() {
    assert_module_error!(
        r#"
pub fn main(x) {
  case x {
    "" -> 1
  }
}
"#
    );
}

#[test]
fn string_2() {
    assert_module_error!(
        r#"
pub fn main(x) {
  case x {
    "a" -> 1
  }
}
"#
    );
}

#[test]
fn string_3() {
    assert_module_error!(
        r#"
pub fn main(x) {
  case x {
    "a" -> 1
    "b" -> 1
  }
}
"#
    );
}

#[test]
fn bit_array() {
    assert_no_warnings!(
        r#"
pub fn main(x) {
  case x {
    <<>> -> 1
    <<1>> -> 1
    <<2>> -> 1
    _ -> 1
  }
}
"#
    );
}

#[test]
fn bit_array_1() {
    assert_module_error!(
        r#"
pub fn main(x) {
  case x {
    <<>> -> 1
    <<1>> -> 1
    <<2>> -> 1
  }
}
"#
    );
}

#[test]
fn bit_array_2() {
    assert_module_error!(
        r#"
pub fn main(x) {
  case x {
    <<>> -> 1
    <<1>> -> 1
  }
}
"#
    );
}

#[test]
fn int() {
    assert_no_warnings!(
        r#"
pub fn main(x) {
  case x {
    0 -> 1
    1 -> 1
    2 -> 1
    _ -> 1
  }
}
"#
    );
}

#[test]
fn int_1() {
    assert_module_error!(
        r#"
pub fn main(x) {
  case x {
    0 -> 1
    1 -> 1
    2 -> 1
  }
}
"#
    );
}

#[test]
fn int_2() {
    assert_module_error!(
        r#"
pub fn main(x) {
  case x {
    0 -> 1
    1 -> 1
  }
}
"#
    );
}

#[test]
fn float() {
    assert_no_warnings!(
        r#"
pub fn main(x) {
  case x {
    0.0 -> 1
    1.1 -> 1
    2.2 -> 1
    _ -> 1
  }
}
"#
    );
}

#[test]
fn float_1() {
    assert_module_error!(
        r#"
pub fn main(x) {
  case x {
    0.0 -> 1
    1.1 -> 1
    2.2 -> 1
  }
}
"#
    );
}

#[test]
fn float_2() {
    assert_module_error!(
        r#"
pub fn main(x) {
  case x {
    0.0 -> 1
    1.1 -> 1
  }
}
"#
    );
}

#[test]
fn list_bool_1() {
    assert_module_error!(
        r#"
pub fn main(x) {
  case x {
    [] -> 1
    [True] -> 2
    [_, _, ..] -> 2
  }
}
"#
    );
}

#[test]
fn list_bool_2() {
    assert_module_error!(
        r#"
pub fn main(x) {
  case x {
    [] -> 1
    [True] -> 2
    [_, False] -> 2
    [_, _, _, ..] -> 2
  }
}
"#
    );
}

#[test]
fn discard_all_fields() {
    assert_no_warnings!(
        r#"
pub type Thing {
  Thing(a: Bool, b: Bool)
}

pub fn main(x) {
  case x {
    Thing(..) -> 1
  }
}
"#
    );
}

#[test]
fn discard_1() {
    assert_no_warnings!(
        r#"
pub type Thing {
  Thing(a: Bool, b: Bool)
}

pub fn main(x) {
  case x {
    Thing(a: True, ..) -> 1
    Thing(a: False, ..) -> 1
  }
}
"#
    );
}

#[test]
fn discard_2() {
    assert_module_error!(
        r#"
pub type Thing {
  Thing(a: Bool, b: Bool)
}

pub fn main(x) {
  case x {
    Thing(a: True, ..) -> 1
  }
}
"#
    );
}

#[test]
fn discard_3() {
    assert_module_error!(
        r#"
pub type Thing {
  Thing(a: Bool, b: Bool)
}

pub fn main(x) {
  case x {
    Thing(a: False, ..) -> 1
  }
}
"#
    );
}

#[test]
fn discard_4() {
    assert_module_error!(
        r#"
pub type Thing {
  Thing(a: Bool, b: Bool)
}

pub fn main(x) {
  case x {
    Thing(a: True, ..) -> 1
  }
}
"#
    );
}

#[test]
fn discard_5() {
    assert_module_error!(
        r#"
pub type Thing {
  Thing(a: Bool, b: Bool)
}

pub fn main(x) {
  case x {
    Thing(a: False, ..) -> 1
  }
}
"#
    );
}

#[test]
fn discard_6() {
    assert_module_error!(
        r#"
pub type Thing {
  Thing(a: Bool, b: Bool)
}

pub fn main(x) {
  case x {
    Thing(False, ..) -> 1
  }
}
"#
    );
}

#[test]
fn label_1() {
    assert_module_error!(
        r#"
pub type Thing {
  Thing(a: Bool, b: Bool)
}

pub fn main(x) {
  case x {
    Thing(a: False, b: True) -> 1
    Thing(b: False, a: True) -> 1
  }
}
"#
    );
}

#[test]
fn guard() {
    assert_module_error!(
        r#"
pub fn main(x, y) {
  case x {
    _ if y -> 1
  }
}
"#
    );
}

#[test]
fn guard_1() {
    assert_module_error!(
        r#"
pub fn main(x, y) {
  case x {
    True if y -> 1
    False -> 2
  }
}
"#
    );
}

#[test]
fn custom_1() {
    assert_module_error!(
        r#"
pub type Type {
  One
  Two
}

pub fn main(x) {
  case x {
    One -> 1
  }
}
"#
    );
}

#[test]
fn custom_2() {
    assert_module_error!(
        r#"
pub type Type {
  One
  Two
  Three(Type)
}

pub fn main(x) {
  case x {
    One -> 1
    Two -> 2
    Three(One) -> 4
  }
}
"#
    );
}

#[test]
fn redundant_1() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    _ -> 1
    _ -> 2
  }
}
"#
    );
}

#[test]
fn redundant_2() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    True -> 1
    False -> 2
    True -> 3
  }
}
"#
    );
}

//https://github.com/gleam-lang/gleam/issues/2651
#[test]
fn redundant_3() {
    assert_warning!(
        r#"
pub fn main(x) {
case x {
59 -> "gleew"
14 -> "glabber"
1 -> ""
_ -> "glooper"
2 -> ""
3 -> "glen"
4 -> "glew"
}
}
"#
    );
}

#[test]
fn redundant_4() {
    assert_warning!(
        r#"
pub fn main(x) {
case x {
"P" -> 4
_ -> 3
"geeper!" -> 5
}
}
"#
    );
}

#[test]
fn redundant_5() {
    assert_warning!(
        r#"
pub fn main(x) {
case x {
"P" -> 4
"" -> 65
"P" -> 19
_ -> 3
}
}
"#
    );
}

#[test]
fn redundant_int_with_underscores() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    10 -> "ten"
    1_0 -> "also ten"
    _ -> "other"
  }
}
"#
    );
}

#[test]
fn redundant_int_with_multiple_underscores() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    1_000_000 -> "one million"
    1000000 -> "also one million"
    _ -> "other"
  }
}
"#
    );
}

#[test]
fn redundant_float_with_different_formatting() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    1.0 -> "one"
    1.00 -> "also one"
    _ -> "other"
  }
}
"#
    );
}

#[test]
fn redundant_float_with_no_trailing_decimal() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    1.0 -> "one"
    1. -> "another one"
    _ -> "other"
  }
}
"#
    );
}

#[test]
fn redundant_float_with_underscore() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    10.0 -> "ten"
    1_0.0 -> "also ten"
    _ -> "other"
  }
}
"#
    );
}

#[test]
fn redundant_float_scientific_notation() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    10.0 -> "ten"
    1.0e1 -> "also ten"
    _ -> "other"
  }
}
"#
    );
}

#[test]
fn redundant_float_scientific_notation_and_underscore() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    1.0e2 -> "one hundred"
    1_0_0.0 -> "one hundred again"
    _ -> "other"
  }
}
"#
    );
}

#[test]
fn let_1() {
    assert_module_error!(
        r#"
pub fn main(x) {
  let True = x
  0
}
"#
    );
}

#[test]
fn tuple_0() {
    assert_module_error!(
        r#"
pub fn main(x, y) {
  case #(x, y) {
    #(True, _) -> 1
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2577
#[test]
fn nested_type_parameter_usage() {
    assert_module_error!(
        r#"
pub type Returned(a) {
  Returned(List(a))
}

fn wibble(user: Returned(#())) -> Int {
  let Returned([#()]) = user
  1
}
"#
    );
}

#[test]
fn empty_case_of_external() {
    // This external type has no known constructors, and we want to make sure
    // that an empty case expression is not valid for it.
    assert_module_error!(
        r#"
pub type Thingy

pub fn main(x: Thingy) {
  case x {
  }
}
"#
    );
}

#[test]
fn empty_case_of_generic() {
    // This generic type has no known constructors, and we want to make sure
    // that an empty case expression is not valid for it.
    assert_module_error!(
        r#"
pub fn main(x: something) {
  case x {
  }
}
"#
    );
}

#[test]
fn reference_absent_type() {
    // This test is here because this code previously caused the compiler
    // to crash, and we want to make sure that it doesn't break again
    assert_module_error!(
        "
type Wibble {
    One(Int)
    Two(Absent)
}

pub fn main(wibble) {
    case wibble {
        One(x) -> x
    }
}
"
    );
}

#[test]
fn case_error_prints_module_names() {
    assert_module_error!(
        ("wibble", "pub type Wibble { Wibble Wobble }"),
        "
import wibble
pub type Things { Thing1 Thing2(Int) }
pub fn main(wobble_thing) {
    case wobble_thing {
        #(wibble.Wibble, Thing1) -> Nil
    }
}
",
    );
}

#[test]
fn case_error_prints_module_alias() {
    assert_module_error!(
        ("wibble", "pub type Wibble { Wibble Wobble }"),
        "
import wibble as wobble
pub fn main(wibble) {
    case wibble {
        wobble.Wibble -> Nil
    }
}
",
    );
}

#[test]
fn case_error_prints_unqualified_value() {
    assert_module_error!(
        ("wibble", "pub type Wibble { Wibble Wobble }"),
        "
import wibble.{Wibble, Wobble}
pub fn main(wibble) {
    case wibble {
        Wibble -> Nil
    }
}
",
    );
}

#[test]
fn case_error_prints_aliased_unqualified_value() {
    assert_module_error!(
        ("wibble", "pub type Wibble { Wibble Wobble }"),
        "
import wibble.{Wibble, Wobble as Wubble}
pub fn main(wibble) {
    case wibble {
        Wibble -> Nil
    }
}
",
    );
}

#[test]
fn case_error_prints_prelude_module_unqualified() {
    assert_module_error!(
        "
pub fn main(result: Result(Nil, Nil)) {
  case result {
    Ok(Nil) -> Nil
  }
}
"
    );
}

#[test]
fn case_error_prints_prelude_module_when_shadowed() {
    assert_module_error!(
        "
import gleam
type MyResult { Ok Error }
pub fn main(res: Result(Int, Nil)) {
  case res {
    gleam.Ok(n) -> Nil
  }
}
"
    );
}

#[test]
fn case_error_prints_module_when_shadowed() {
    assert_module_error!(
        ("mod", "pub type Wibble { Wibble Wobble }"),
        "
import mod.{Wibble}
type Wibble { Wibble Wobble }
pub fn main() {
  let wibble = mod.Wibble
  case wibble {
    mod.Wobble -> Nil
  }
}
"
    );
}

#[test]
fn case_error_prints_module_when_aliased_and_shadowed() {
    assert_module_error!(
        ("mod", "pub type Wibble { Wibble Wobble }"),
        "
import mod.{Wibble as Wobble}
type Wibble { Wobble Wubble }
pub fn main() {
  let wibble = mod.Wibble
  case wibble {
    mod.Wobble -> Nil
  }
}
"
    );
}

#[test]
fn case_error_prints_unqualifed_when_aliased() {
    assert_module_error!(
        ("mod", "pub type Wibble { Wibble Wobble }"),
        "
import mod.{Wibble as Wobble}
type Wibble { Wibble Wubble }
pub fn main() {
  let wibble = mod.Wibble
  case wibble {
    mod.Wobble -> Nil
  }
}
"
    );
}

// The following few tests all verify that the compiler provides useful errors
// when there are no case arms, instead of just suggesting `_` as it did previously.
#[test]
fn empty_case_of_bool() {
    assert_module_error!(
        "
pub fn main(b: Bool) {
  case b {}
}
"
    );
}

#[test]
fn empty_case_of_custom_type() {
    assert_module_error!(
        "
pub type Wibble { Wibble Wobble Wubble }
pub fn main(wibble: Wibble) {
  case wibble {}
}
"
    );
}

#[test]
fn empty_case_of_list() {
    assert_error!(
        "
let list = []
case list {}
"
    );
}

#[test]
fn empty_case_of_int() {
    assert_error!(
        "
let num = 24
case num {}
"
    );
}

#[test]
fn empty_case_of_float() {
    assert_error!(
        "
let age = 10.6
case age {}
"
    );
}

#[test]
fn empty_case_of_string() {
    assert_error!(
        r#"
let name = "John Doe"
case name {}
"#
    );
}

#[test]
fn empty_case_of_multi_pattern() {
    assert_module_error!(
        "
pub fn main(a: Result(a, b), b: Bool) {
  case a, b {}
}
"
    );
}

#[test]
fn inexhaustive_multi_pattern() {
    assert_error!(
        "
let a = Ok(1)
let b = True
case a, b {
  Error(_), _ -> Nil
}
"
    );
}

#[test]
fn inexhaustive_multi_pattern2() {
    assert_module_error!(
        "
pub fn main(a: Result(Int, Nil), b: Bool) {
  case a, b {
    Ok(1), True -> Nil
  }
}
"
    );
}

#[test]
fn inexhaustive_multi_pattern3() {
    assert_error!(
        "
let a = Ok(1)
let b = True
case a, b {
  _, False -> Nil
}
"
    );
}

#[test]
fn inexhaustive_multi_pattern4() {
    assert_module_error!(
        "
pub fn main(c: Bool) {
  let a = 12
  let b = 3.14
  case a, b, c {
    1, 2.0, True -> Nil
  }
}
"
    );
}

#[test]
fn inexhaustive_multi_pattern5() {
    assert_module_error!(
        "
pub fn main(c: Bool) {
  let a = 12
  let b = 3.14
  case a, b, c {
    12, _, False -> Nil
  }
}
"
    );
}

#[test]
fn inferred_variant() {
    assert_no_warnings!(
        "
pub type Wibble {
  Wibble(Bool)
  Wobble(Int)
}

pub fn main() {
  let wibble = Wibble(False)
  case wibble {
    Wibble(True) -> 1
    Wibble(False) -> 0
  }
}
",
    );
}

#[test]
fn inferred_variant2() {
    assert_no_warnings!(
        "
pub type Wibble {
  Wibble
  Wobble
}

pub fn main(b: Bool) {
  let wibble = Wibble
  case wibble, b {
    Wibble, True -> True
    Wibble, False -> False
  }
}
",
    );
}

#[test]
fn inferred_variant3() {
    assert_no_warnings!(
        "
pub type Wibble {
  Wibble(Int, Float, Bool)
  Wobble(String)
}

pub fn main() {
  let wibble = Wibble(1, 3.14, False)
  let Wibble(_int, _float, _bool) = wibble
}
",
    );
}

#[test]
fn other_variant_unreachable_when_inferred() {
    assert_warning!(
        "
pub type Wibble {
  Wibble
  Wobble
}

pub fn main() {
  let always_wobble = Wobble
  case always_wobble {
    Wibble -> panic
    Wobble -> Nil
  }
}
"
    );
}

#[test]
fn other_variant_unreachable_when_inferred2() {
    assert_warning!(
        "
pub type Wibble {
  Wibble
  Wobble
  Wubble
}

pub fn main() {
  let always_wobble = Wobble
  case always_wobble {
    Wibble | Wubble -> panic
    Wobble -> Nil
  }
}
"
    );
}

#[test]
fn unreachable_string_pattern_after_prefix() {
    assert_warning!(
        r#"pub fn main() {
  let string = ""
  case string {
    "wib" <> rest -> rest
    "wibble" -> "a"
    _ -> "b"
  }
}"#
    );
}

#[test]
fn reachable_string_pattern_after_prefix() {
    assert_no_warnings!(
        r#"pub fn main() {
  let string = ""
  case string {
    "wib" <> rest if True -> rest
    "wibble" -> "a"
    _ -> "b"
  }
}"#
    );
}

#[test]
fn reachable_string_pattern_after_prefix_1() {
    assert_no_warnings!(
        r#"pub fn main() {
  let string = ""
  case string {
    "wibble" <> rest -> rest
    "wib" -> "a"
    _ -> "b"
  }
}"#
    );
}

#[test]
fn unreachable_prefix_pattern_after_prefix() {
    assert_warning!(
        r#"pub fn main() {
  let string = ""
  case string {
    "wib" <> rest -> rest
    "wibble" <> rest -> rest
    _ -> "a"
  }
}"#
    );
}

#[test]
fn reachable_prefix_pattern_after_prefix() {
    assert_no_warnings!(
        r#"pub fn main() {
  let string = ""
  case string {
    "wib" <> rest if True -> rest
    "wibble" <> rest -> rest
    _ -> "a"
  }
}"#
    );
}

#[test]
fn reachable_prefix_pattern_after_prefix_1() {
    assert_no_warnings!(
        r#"pub fn main() {
  let string = ""
  case string {
    "wibble" <> rest -> rest
    "wib" <> rest -> rest
    _ -> "a"
  }
}"#
    );
}

#[test]
fn multiple_unreachable_prefix_patterns() {
    assert_warning!(
        r#"pub fn main() {
  let string = ""
  case string {
    "wib" <> rest -> rest
    "wibble" <> rest -> rest
    "wibblest" <> rest -> rest
    _ -> "a"
  }
}"#
    );
}

#[test]
fn multiple_unreachable_prefix_patterns_1() {
    assert_warning!(
        r#"pub fn main() {
  let string = ""
  case string {
    "wib" <> rest if True -> rest
    "wibble" <> rest -> rest
    "wibblest" <> rest -> rest
    _ -> "a"
  }
}"#
    );
}

#[test]
fn bit_array_bits_catches_everything() {
    assert_warning!(
        r#"pub fn main() {
  let bit_array = <<>>
  case bit_array {
    <<_:bits>> -> 1
    <<1>> -> 2
    _ -> 2
  }
}"#
    );
}

#[test]
fn bit_array_bytes_needs_catch_all() {
    assert_module_error!(
        r#"pub fn main() {
  let bit_array = <<>>
  case bit_array {
    <<_:bytes>> -> 1
  }
}"#
    );
}

#[test]
fn bit_array_overlapping_patterns_are_redundant() {
    assert_warning!(
        r#"pub fn main() {
  let bit_array = <<>>
  case bit_array {
    <<1, a:size(16)>> -> a
    <<1, b:size(8)-unit(2)>> -> b
    _ -> 2
  }
}"#
    );
}

#[test]
fn bit_array_similar_overlapping_patterns_are_not_redundant() {
    assert_no_warnings!(
        r#"pub fn main() {
  let bit_array = <<>>
  case bit_array {
    <<1, a:size(16)>> -> a
    <<2, b:size(8)-unit(2)>> -> b
    _ -> 2
  }
}"#
    );
}

#[test]
fn bit_array_overlapping_redundant_patterns_with_variable_size() {
    assert_warning!(
        r#"pub fn main() {
  let bit_array = <<>>
  let len = 3
  case bit_array {
    <<a:size(len), _:size(16)>> -> a
    <<_:size(len), b:size(8)-unit(2)>> -> b
    _ -> 2
  }
}"#
    );
}

#[test]
fn bit_array_overlapping_redundant_patterns_with_variable_size_2() {
    assert_warning!(
        r#"pub fn main() {
  let bit_array = <<>>
  case bit_array {
    <<len, _:size(len)-unit(3)>> -> 1
    <<len, _:size(len)-unit(2), 1:size(len)>> -> 2
    _ -> 2
  }
}"#
    );
}

#[test]
fn bit_array_overlapping_patterns_with_variable_size_not_redundant() {
    assert_no_warnings!(
        r#"pub fn main() {
  let bit_array = <<>>
  case bit_array {
    <<len, 1:size(len)-unit(3)>> -> 1
    <<len, _:size(len)-unit(2), 1:size(len)>> -> 2
    _ -> 2
  }
}"#
    );
}

#[test]
fn bit_array_patterns_with_different_length_with_same_name_are_not_redundant() {
    assert_no_warnings!(
        r#"pub fn main() {
  let bit_array = <<>>
  let len = 10
  case bit_array {
    <<_, _:size(len)-unit(3)>> -> 1
    // Down here len is not the same as the len above, so the branch below is
    // not redundant!
    <<len, _:size(len)-unit(3)>> -> 2
    _ -> 2
  }
}"#
    );
}

#[test]
fn bit_array_patterns_with_different_length_with_same_name_are_not_redundant_1() {
    assert_no_warnings!(
        r#"pub fn main() {
  let bit_array = <<>>
  let len = 10
  case bit_array {
    <<len, _:size(len)-unit(3)>> -> 1
    // Down here len is not the same as the len above, so the branch below is
    // not redundant!
    <<_, _:size(len)-unit(3)>> -> 2
    _ -> 2
  }
}"#
    );
}

#[test]
fn bit_array_patterns_with_different_length_with_same_name_are_not_redundant_2() {
    assert_no_warnings!(
        r#"pub fn main() {
  let bit_array = <<>>
  case bit_array {
    <<_, len, _:size(len)>> -> 1
    // Down here len is not the same as the len above, so the branch below is
    // not redundant!
    <<len, _, _:size(len)>> -> 2
    _ -> 2
  }
}"#
    );
}

#[test]
fn same_catch_all_bytes_are_redundant() {
    assert_warning!(
        r#"pub fn main() {
  let bit_array = <<>>
  case bit_array {
    <<_:bytes>> -> <<>>
    <<a:bytes>> -> a
    _ -> <<>>
  }
}"#
    );
}

#[test]
fn different_catch_all_bytes_are_not_redundant() {
    assert_no_warnings!(
        r#"pub fn main() {
  let bit_array = <<>>
  case bit_array {
    <<_, _:bytes>> -> <<>>
    <<_:bytes>> -> <<>>
    _ -> <<>>
  }
}"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2616
#[test]
fn duplicated_alternative_patterns() {
    assert_warning!(
        "
pub fn main() {
  let x = 1
  case x {
    2 | 2 -> 2
    _ -> panic
  }
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/2616
#[test]
fn duplicated_pattern_in_alternative() {
    assert_warning!(
        "
pub fn main() {
  let x = 1
  case x {
    2 -> x
    1 | 2 -> x - 4
    _ -> panic
  }
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/2616
#[test]
fn duplicated_pattern_with_multiple_alternatives() {
    assert_warning!(
        "
pub fn main() {
  let x = 1
  case x {
    1 -> 1
    3 -> 3
    5 -> 5
    1 | 2 | 3 | 4 | 5 -> x - 1
    _ -> panic
  }
}
"
    );
}

#[test]
fn unreachable_multi_pattern() {
    assert_warning!(
        "
pub fn main() {
  let x = 1
  let y = 2
  case x, y {
    1, 2 -> True
    1, 2 -> False
    _, _ -> panic
  }
}
"
    );
}

#[test]
fn unreachable_alternative_multi_pattern() {
    assert_warning!(
        "
pub fn main() {
  let x = 1
  let y = 2
  case x, y {
    1, 2 -> True
    3, 4 | 1, 2 -> False
    _, _ -> panic
  }
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4586
#[test]
fn compiler_does_not_crash_when_defining_duplicate_alternative_variables() {
    assert_error!(
        "
case todo {
  #(a, b) | #(a, a as b) -> todo
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4626
#[test]
fn correct_missing_patterns_for_opaque_type() {
    assert_module_error!(
        (
            "mod",
            "pub opaque type Wibble { Wibble(Int) Wobble(String) }"
        ),
        "
import mod

pub fn main(w: mod.Wibble) {
  case w {}
}
"
    );
}

#[test]
fn correct_missing_patterns_for_opaque_type_in_definition_module() {
    assert_module_error!(
        "
pub opaque type Wibble { Wibble(Int) Wobble(String) }

pub fn main(w: Wibble) {
  case w {}
}
"
    );
}

#[test]
// https://github.com/gleam-lang/gleam/issues/4278
fn redundant_missing_patterns() {
    assert_module_error!(
        r#"
fn wibble(b: Bool, i: Int) {
  case b, i {
    False, 1 -> todo
    True, 2 -> todo
  }
}

pub fn main() { wibble(False, 1) }"#
    );
}

#[test]
// https://github.com/gleam-lang/gleam/issues/5262
fn compiler_does_not_crash_when_matching_on_utfcodepoint() {
    assert_module_error!(
        (
            "gleam_stdlib",
            "gleam/string",
            r#"
@external(erlang, "gleam_stdlib", "identity")
fn unsafe_int_to_utf_codepoint(a: Int) -> UtfCodepoint

pub fn utf_codepoint(value: Int) -> Result(UtfCodepoint, Nil) {
  case value {
    i if i > 1_114_111 -> Error(Nil)
    i if i >= 55_296 && i <= 57_343 -> Error(Nil)
    i if i < 0 -> Error(Nil)
    i -> Ok(unsafe_int_to_utf_codepoint(i))
  }
}
            "#
        ),
        r#"
import gleam/string

pub fn main() {
  let assert Ok(wibble) = string.utf_codepoint(71)
  case wibble {
  }
}        "#
    );
}

// https://github.com/gleam-lang/gleam/issues/5286
#[test]
fn reachable_bit_array_pattern() {
    assert_no_warnings!(
        r#"
pub fn main(x) {
  case x {
    <<_, "==">> -> 1
    <<_, _, "=">> -> 2
    // ^^^ This should be reachable
    _ -> 3
  }
}
"#
    );
}
