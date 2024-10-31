use crate::{
    assert_error, assert_module_error, assert_no_warnings, assert_warning, assert_with_module_error,
};

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
    assert_with_module_error!(
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
    assert_with_module_error!(
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
    assert_with_module_error!(
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
    assert_with_module_error!(
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
pub fn main() {
  let result = Ok(Nil)
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
pub fn main() {
  let res = gleam.Ok(10)
  case res {
    gleam.Ok(n) -> Nil
  }
}
"
    );
}

#[test]
fn case_error_prints_module_when_shadowed() {
    assert_with_module_error!(
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
    assert_with_module_error!(
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
    assert_with_module_error!(
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
    assert_error!(
        "
let b = True
case b {}
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
    assert_error!(
        "
let a = Ok(1)
let b = True
case a, b {}
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
    assert_error!(
        "
let a = Ok(1)
let b = True
case a, b {
  Ok(1), True -> Nil
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
    assert_error!(
        "
let a = 12
let b = 3.14
let c = False
case a, b, c {
  1, 2.0, True -> Nil
}
"
    );
}

#[test]
fn inexhaustive_multi_pattern5() {
    assert_error!(
        "
let a = 12
let b = 3.14
let c = False
case a, b, c {
  12, _, False -> Nil
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
