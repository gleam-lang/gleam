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

#[test]
fn no_impl_function_fault_tolerance() {
    // A function not having an implementation does not stop analysis.
    assert_module_error!(
        r#"
pub fn no_impl() -> Nil

pub type X = UnknownType
"#
    );
}

#[test]
fn bad_body_function_fault_tolerance() {
    // A function having an invalid body does not stop analysis.
    assert_module_error!(
        r#"
pub fn bad(x: Int) -> Float {
  // Invalid body.
  "" + ""
}

pub fn user() -> Float {
  // This checks that the bad function is still usable, the types coming from
  // its annotations. This function is valid.
  bad(1)
}

// Another bad function to make sure that analysis has not stopped. This error
// should also be emitted.
pub fn bad_2() {
  bad(Nil)
}
"#
    );
}

#[test]
fn annotation_mismatch_function_fault_tolerance() {
    // A function having an invalid body does not stop analysis.
    assert_module_error!(
        r#"
pub fn bad(x: Int) -> Float {
  // This does not match the return annotation
  1
}

pub fn user() -> Float {
  // This checks that the bad function is still usable, the types coming from
  // its annotations. This function is valid.
  bad(1)
}

// Another bad function to make sure that analysis has not stopped. This error
// should also be emitted.
pub fn bad_2() {
  bad(Nil)
}
"#
    );
}

#[test]
fn invalid_javascript_external_do_not_stop_analysis() {
    // Both these have errors! We do not stop on the first one.
    assert_module_error!(
        r#"
@external(javascript, "somemodule", "() => 123")
pub fn one() -> Nil {
  Nil
}

pub fn two() -> Nil {
  ""
}
"#
    );
}

#[test]
fn multiple_bad_statement_assignment_fault_tolerance() {
    assert_module_error!(
        r#"
pub fn main() {
  let a = 1 + 2.0
  let b = 3 + 4.0
  let c = a + b
}
"#
    );
}

#[test]
fn multiple_bad_statement_assignment_with_annotation_fault_tolerance() {
    assert_module_error!(
        r#"
pub fn main() {
  let a: Int = "not an int"
  let b: String = 1
  let c = a + 2
}
"#
    );
}

#[test]
fn multiple_bad_statement_assignment_with_annotation_fault_tolerance2() {
    assert_module_error!(
        r#"
pub fn main() {
  // Since the value is invalid the type is the annotation
  let a: Int = Junk
  let b: String = 1
  let c = a + 2
}
"#
    );
}

#[test]
fn multiple_bad_statement_assignment_with_pattern_fault_tolerance2() {
    assert_module_error!(
        r#"
pub fn main() {
  // Since the pattern is invalid no variable is created
  let Junk(a) = 7
  // Pattern is valid but does not type check
  let Ok(b) = 1
  let c = a + b
}
"#
    );
}

#[test]
fn multiple_bad_statement_expression_fault_tolerance() {
    assert_module_error!(
        r#"
pub fn main() {
  1 + 2.0
  3 + 4.0
  let c = 1 + 2
}
"#
    );
}

#[test]
fn function_call_incorrect_arg_types_fault_tolerance() {
    assert_module_error!(
        r#"
fn add(x: Int, y: Int) {
  x + y
}

pub fn main() {
  add(1.0, 1.0)
}
"#
    );
}

#[test]
fn function_call_incorrect_arity_fault_tolerance() {
    assert_module_error!(
        r#"
fn add(x: Int, y: Int) {
  x + y
}

pub fn main() {
  add(1.0)
}
"#
    );
}

#[test]
fn function_call_incorrect_arity_with_labels_fault_tolerance() {
    assert_module_error!(
        r#"
fn wibble(wibble arg1: fn() -> Int, wobble arg2: Int) -> Int {
  arg1() + arg2
}

pub fn main() {
  wibble(wobble: "")
}
"#
    );
}

#[test]
fn function_call_incorrect_arity_with_labels_fault_tolerance2() {
    assert_module_error!(
        r#"
fn wibble(wibble arg1: fn() -> Int, wobble arg2: Int, wabble arg3: Int) -> Int {
  arg1() + arg2 + arg3
}

pub fn main() {
  wibble(fn() {""}, wobble: "")
}
"#
    );
}

#[test]
fn case_clause_pattern_fault_tolerance() {
    assert_module_error!(
        r#"
pub fn main() {
  let wibble = True
  case wibble {
    True -> 0
    Wibble -> 1
    Wibble2 -> 2
    _ -> 3
  }
}
"#
    );
}

#[test]
fn case_clause_guard_fault_tolerance() {
    assert_module_error!(
        r#"
pub fn main() {
  let wibble = True
  case wibble {
    a if a == Wibble -> 0
    b if b == Wibble -> 0
    _ -> 1
  }
}
"#
    );
}

#[test]
fn case_clause_then_fault_tolerance() {
    assert_module_error!(
        r#"
pub fn main() {
  let wibble = True
  case wibble {
    True -> {
      1.0 + 1.0
    }
    _ -> {
      1.0 + 1.0
    }
  }
}
"#
    );
}
