use crate::{assert_module_error, assert_no_warnings, assert_warning};

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

fn foo(user: Returned(#())) -> Int {
  let Returned([#()]) = user
  1
}
"#
    );
}
