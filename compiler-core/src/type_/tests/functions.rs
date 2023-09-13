use crate::{assert_module_error, assert_module_infer};

// https://github.com/gleam-lang/gleam/issues/1860
#[test]
fn unlabelled_after_labelled() {
    assert_module_error!(
        "fn main(wibble wibber, wobber) {
  Nil
}"
    );
}

// https://github.com/gleam-lang/gleam/issues/1860
#[test]
fn unlabelled_after_labelled_with_type() {
    assert_module_error!(
        "fn main(wibble wibber, wobber: Int) {
  Nil
}"
    );
}

// https://github.com/gleam-lang/gleam/issues/1860
#[test]
fn unlabelled_after_labelled_external() {
    assert_module_error!(
        r#"
@external(erlang, "", "")
fn main(wibble x: Int, y: Int) -> Int
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1860
#[test]
fn all_labelled() {
    assert_module_infer!(
        r#"pub fn prepend(to list: List(a), this item: a) -> List(a) {
  [item, ..list]
}
"#,
        vec![(r#"prepend"#, r#"fn(List(a), a) -> List(a)"#)]
    );
}

// https://github.com/gleam-lang/gleam/issues/1814
#[test]
fn out_of_order_generalisation() {
    assert_module_infer!(
        r#"
pub fn main() {
  call(fn() {
    "Hello"
  })
}

fn call(f: fn() -> a) {
  f()
}
"#,
        vec![(r#"main"#, r#"fn() -> String"#)]
    );
}

// https://github.com/gleam-lang/gleam/issues/2275
#[test]
fn bug_2275() {
    assert_module_infer!(
        r#"
pub fn zero() {
  one()
}

fn one() {
  one()
  two()
}

fn two() {
  two
  Nil
}
"#,
        vec![(r#"zero"#, r#"fn() -> Nil"#)]
    );
}

// https://github.com/gleam-lang/gleam/issues/2275
#[test]
fn bug_2275_2_self_references() {
    assert_module_infer!(
        r#"
pub fn zero() {
  one()
}

fn one() {
  one()
  two()
}

fn two() {
  two
  two
  Nil
}
"#,
        vec![(r#"zero"#, r#"fn() -> Nil"#)]
    );
}

// https://github.com/gleam-lang/gleam/issues/2275
#[test]
fn bug_2275_again() {
    assert_module_infer!(
        r#"
pub fn aaa(input) {
  case [] {
    [] -> input

    _ -> {
      let input2 = bbb()
      aaa(input2)
    }
  }
}

pub fn bbb() {
  ccc() + bbb()
}

pub fn ccc() {
  ccc() + bbb()
}
"#,
        vec![
            (r#"aaa"#, r#"fn(Int) -> Int"#),
            (r#"bbb"#, r#"fn() -> Int"#),
            (r#"ccc"#, r#"fn() -> Int"#),
        ]
    );
}

#[test]
fn deprecated_function() {
    assert_module_infer!(
        r#"
@deprecated("use wibble instead")
pub fn main() {
  Nil
}"#,
        vec![(r#"main"#, r#"fn() -> Nil"#)]
    );
}

// https://github.com/gleam-lang/gleam/issues/2303
#[test]
fn recursive_type() {
    assert_module_error!(
        r#"
pub fn one(x) {
  two([x])
}

pub fn two(x) {
  one(x)
}
"#
    );
}
