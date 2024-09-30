use crate::assert_js;

#[test]
fn int_literals() {
    assert_js!(
        r#"
fn go() {
    1
    2
    -3
    4001
    0b00001111
    0o17
    0xF
    1_000
}
"#,
    );
}

#[test]
fn float_literals() {
    assert_js!(
        r#"
fn go() {
    1.5
    2.0
    -0.1
    1.
}
"#,
    );
}

#[test]
fn float_scientific_literals() {
    assert_js!(
        r#"
fn go() {
    0.01e-1
    0.01e-0
    -10.01e-1
    -10.01e-0
    100.001e523
    -100.001e-523
    100.001e123_456_789
    -100.001e-123_456_789
}
"#,
    );
}

#[test]
fn int_operators() {
    assert_js!(
        r#"
fn go() {
    1 + 1 // => 2
    5 - 1 // => 4
    5 / 2 // => 2
    3 * 3 // => 9
    5 % 2 // => 1
    2 > 1  // => True
    2 < 1  // => False
    2 >= 1 // => True
    2 <= 1 // => False
}
"#,
    );
}

#[test]
fn int_divide_complex_expr() {
    assert_js!(
        r#"
fn go() {
  case 1 >= 0 {
    True -> 2
    False -> 4
  } / 2
}
"#,
    );
}

#[test]
fn int_mod_complex_expr() {
    assert_js!(
        r#"
fn go() {
  case 1 >= 0 {
    True -> 2
    False -> 4
  } % 2
}
"#,
    );
}

#[test]
fn float_operators() {
    assert_js!(
        r#"
fn go() {
    1.0 +. 1.4 // => 2.4
    5.0 -. 1.5 // => 3.5
    5.0 /. 2.0 // => 2.5
    3.0 *. 3.1 // => 9.3

    2.0 >. 1.0  // => True
    2.0 <. 1.0  // => False
    2.0 >=. 1.0 // => True
    2.0 <=. 1.0 // => False
}
"#,
    );
}

#[test]
fn float_divide_complex_expr() {
    assert_js!(
        r#"
fn go() {
  case 1.0 >=. 0.0 {
    True -> 2.0
    False -> 4.0
  } /. 2.0
}
"#,
    );
}

#[test]
fn wide_float_div() {
    assert_js!(
        r#"
fn go() {
  111111111111111111111111111111. /. 22222222222222222222222222222222222.
}
"#,
    );
}

#[test]
fn int_patterns() {
    assert_js!(
        r#"
fn go(x) {
  let assert 4 = x
}
"#,
    );
}

#[test]
fn int_equality() {
    assert_js!(
        r#"
fn go() {
  1 != 2
  1 == 2
}
"#,
    );
}

#[test]
fn int_equality1() {
    assert_js!(
        r#"
fn go(y) {
  let x = 1
  x == y
}
"#,
    );
}

#[test]
fn float_equality() {
    assert_js!(
        r#"
fn go() {
  1.0 != 2.0
  1.0 == 2.0
}
"#,
    );
}

#[test]
fn float_equality1() {
    assert_js!(
        r#"
fn go(y) {
  let x = 1.0
  x == y
}
"#,
    );
}

#[test]
fn operator_precedence() {
    assert_js!(
        r#"
fn go() {
  2.4 *. { 3.5 +. 6.0 }
}
"#,
    )
}

#[test]
fn remainder() {
    assert_js!(
        r#"
fn go() {
  5 % 0 // => 0
}
"#,
    );
}

#[test]
fn int_negation() {
    assert_js!(
        r#"
fn go() {
  let a = 3
  let b = -a
}
"#,
    );
}

#[test]
fn repeated_int_negation() {
    assert_js!(
        r#"
fn go() {
  let a = 3
  let b = --a
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2412
#[test]
fn preceeding_zeros_int() {
    assert_js!(
        r#"
fn main() {
  09_179
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2412
#[test]
fn preceeding_zeros_float() {
    assert_js!(
        r#"
fn main() {
  09_179.1
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2412
#[test]
fn preceeding_zeros_int_const() {
    assert_js!(
        r#"
const x = 09_179
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2412
#[test]
fn preceeding_zeros_float_const() {
    assert_js!(
        r#"
const x = 09_179.1
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2412
#[test]
fn preceeding_zeros_int_pattern() {
    assert_js!(
        r#"
fn main(x) {
  let assert 09_179 = x
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2412
#[test]
fn preceeding_zeros_float_pattern() {
    assert_js!(
        r#"
fn main(x) {
  let assert 09_179.1 = x
}
"#
    );
}
