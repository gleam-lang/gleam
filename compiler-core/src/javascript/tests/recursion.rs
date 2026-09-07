// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2022 The Gleam contributors

use crate::assert_js;

#[test]
fn tco() {
    assert_js!(
        r#"
pub fn main(x) {
  case x {
    0 -> Nil
    _ -> main(x - 1)
  }
}
"#
    );
}

#[test]
fn tco_case_block() {
    assert_js!(
        r#"
pub fn main(x) {
  case x {
    0 -> Nil
    _ -> {
      let y = x
      main(y - 1)
    }
  }
}
"#
    );
}

#[test]
fn tco_unchanged_argument_is_not_reassigned() {
    assert_js!(
        r#"
pub fn main(x, unchanged) {
  case x {
    0 -> unchanged
    _ -> main(x - 1, unchanged)
  }
}
"#
    );
}

#[test]
fn tco_captured_argument_uses_loop_variable() {
    assert_js!(
        r#"
pub fn main(x, read) {
  case x {
    0 -> read()
    _ -> main(x - 1, fn() { x })
  }
}
"#
    );
}

#[test]
fn tco_nested_call_preserves_outer_dependencies() {
    assert_js!(
        r#"
pub fn main(x, y, remaining) {
  case remaining {
    0 -> x + y
    _ -> main(x - 1, main(x, y, 0), remaining - 1)
  }
}
"#
    );
}

#[test]
fn tco_unchanged_parameter_read_by_later_argument() {
    assert_js!(
        r#"
pub fn main(x, y, remaining) {
  case remaining {
    0 -> y
    _ -> main(x, x, remaining - 1)
  }
}
"#
    );
}

#[test]
fn tco_evaluates_discarded_argument() {
    assert_js!(
        r#"
@external(javascript, "utils", "observe")
fn observe(x: Int) -> Int

pub fn main(x, _) {
  case x {
    0 -> Nil
    _ -> main(x - 1, observe(x))
  }
}
"#
    );
}

#[test]
fn tco_shadowed_argument_is_reassigned() {
    assert_js!(
        r#"
pub fn main(x) {
  let x = x - 1
  main(x)
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1779
#[test]
fn not_tco_due_to_assignment() {
    assert_js!(
        r#"
pub fn main(x) {
  let z = {
    let y = x
    main(y - 1)
  }
  z
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2400
#[test]
fn shadowing_so_not_recursive() {
    // This funtion is calling an argument with the same name as itself, so it is not recursive
    assert_js!(
        r#"
pub fn map(map) {
  map()
}
"#
    );
}
