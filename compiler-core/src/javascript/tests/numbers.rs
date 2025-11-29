use crate::{assert_js, assert_js_module_error};

#[test]
fn int_literals() {
    assert_js!(
        r#"
pub fn go() {
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
pub fn go() {
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
pub fn go() {
  0.01e-1
  0.01e-0
  -10.01e-1
  -10.01e-0
  -100.001e-523
  -100.001e-123_456_789
}
"#,
    );
}

#[test]
fn int_operators() {
    assert_js!(
        r#"
pub fn go() {
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
pub fn go() {
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
pub fn go() {
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
pub fn go() {
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
pub fn go() {
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
pub fn go() {
  111111111111111111111111111111. /. 22222222222222222222222222222222222.
}
"#,
    );
}

#[test]
fn int_patterns() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert 4 = x
}
"#,
    );
}

#[test]
fn int_equality() {
    assert_js!(
        r#"
pub fn go() {
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
pub fn go(y) {
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
pub fn go() {
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
pub fn go(y) {
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
pub fn go() {
  2.4 *. { 3.5 +. 6.0 }
}
"#,
    )
}

#[test]
fn remainder() {
    assert_js!(
        r#"
pub fn go() {
  5 % 0 // => 0
}
"#,
    );
}

#[test]
fn int_negation() {
    assert_js!(
        r#"
pub fn go() {
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
pub fn go() {
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
pub fn main() {
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
pub fn main() {
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
pub const x = 09_179
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2412
#[test]
fn preceeding_zeros_float_const() {
    assert_js!(
        r#"
pub const x = 09_179.1
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2412
#[test]
fn preceeding_zeros_int_pattern() {
    assert_js!(
        r#"
pub fn main(x) {
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
pub fn main(x) {
  let assert 09_179.1 = x
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/4459
#[test]
fn underscore_after_hexadecimal_prefix() {
    assert_js!(
        "
pub fn main() {
  0x_12_34
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4459
#[test]
fn underscore_after_octal_prefix() {
    assert_js!(
        "
pub fn main() {
  0o_12_34
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4459
#[test]
fn underscore_after_binary_prefix() {
    assert_js!(
        "
pub fn main() {
  0b_10_01
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4481
#[test]
fn underscore_after_zero_after_hex_prefix() {
    assert_js!(
        "
pub fn main() {
  0x0_1_2_3
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4481
#[test]
fn underscore_after_zero_after_octal_prefix() {
    assert_js!(
        "
pub fn main() {
  0o0_1_2_3
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4481
#[test]
fn underscore_after_zero_after_binary_prefix() {
    assert_js!(
        "
pub fn main() {
  0b0_1_0_1
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4481
#[test]
fn underscore_after_zero() {
    assert_js!(
        "
pub fn main() {
  0_1_2_3
}
"
    );
}

#[test]
fn zero_after_underscore_after_hex_prefix() {
    assert_js!(
        "
pub fn main() {
  0x_0_1_2_3
}
"
    );
}

#[test]
fn zero_after_underscore_after_octal_prefix() {
    assert_js!(
        "
pub fn main() {
  0o_0_1_2_3
}
"
    );
}

#[test]
fn zero_after_underscore_after_binary_prefix() {
    assert_js!(
        "
pub fn main() {
  0b_0_1_0_1
}
"
    );
}

#[test]
fn underscore_after_decimal_point() {
    assert_js!(
        "
pub fn main() {
  0._1
}
"
    );
}

#[test]
fn underscore_after_decimal_point_case_statement() {
    assert_js!(
        "
pub fn main(x) {
  case x {
    0._1 -> \"wobble\"
    _ -> \"wibble\"
  }
}
"
    );
}

#[test]
fn inf_float_case_statement() {
    assert_js_module_error!(
        "
pub fn main(x) {
  case x {
  100.001e123_456_789 -> \"wobble\"
    _ -> \"wibble\"
  }
}
"
    );
}

#[test]
fn division_inf_by_inf_float() {
    assert_js_module_error!(
        "
pub fn main(x) {
  -100.001e123_456_789 /. 100.001e123_456_789
}
"
    );
}

#[test]
fn division_by_zero_float() {
    assert_js!(
        "pub fn main() {
  1.1 /. 0.0
}"
    )
}

#[test]
fn division_by_non_zero_float() {
    assert_js!(
        "pub fn main() {
  1.1 /. 2.3
}"
    )
}

#[test]
fn complex_division_by_non_zero_float() {
    assert_js!(
        "pub fn main() {
  { 1.1 +. 2.0 } /. 2.3
}"
    )
}

#[test]
fn division_by_zero_int() {
    assert_js!(
        "pub fn main() {
  1 / 0
}"
    )
}

#[test]
fn division_by_non_zero_int() {
    assert_js!(
        "pub fn main() {
  1 / 2
}"
    )
}

#[test]
fn complex_division_by_non_zero_int() {
    assert_js!(
        "pub fn main() {
  { 1 + 2 } / 3
}"
    )
}

#[test]
fn remainder_by_zero_int() {
    assert_js!(
        "pub fn main() {
  1 % 0
}"
    )
}

#[test]
fn remainder_by_non_zero_int() {
    assert_js!(
        "pub fn main() {
  1 % 2
}"
    )
}

#[test]
fn complex_remainder_by_non_zero_int() {
    assert_js!(
        "pub fn main() {
  { 1 + 2 } % 3
}"
    )
}
