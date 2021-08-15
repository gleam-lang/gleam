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
  let 4 = x
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
