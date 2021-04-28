use super::*;
use crate::assert_js;

#[test]
fn integer_literals() {
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
        r#""use strict";

function go() {
  1;
  2;
  -3;
  4001;
  0b00001111;
  0o17;
  0xF;
  return 1_000;
}
"#
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
        r#""use strict";

function go() {
  1.5;
  2.0;
  -0.1;
  return 1.;
}
"#
    );
}

#[test]
fn integer_operators() {
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
        r#""use strict";

function go() {
  1 + 1;
  5 - 1;
  5 / 2 | 0;
  3 * 3;
  5 % 2;
  2 > 1;
  2 < 1;
  2 >= 1;
  return 2 <= 1;
}
"#
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
        r#""use strict";

function go() {
  1.0 + 1.4;
  5.0 - 1.5;
  $divide(5.0, 2.0);
  3.0 * 3.1;
  2.0 > 1.0;
  2.0 < 1.0;
  2.0 >= 1.0;
  return 2.0 <= 1.0;
}

function $divide(a, b) {
  if (b === 0) {
    return 0;
  }
  return a / b;
}
"#
    );

    assert_js!(
        r#"
fn go() {
  111111111111111111111111111111. /. 22222222222222222222222222222222222.
}
"#,
        r#""use strict";

function go() {
  return $divide(
    111111111111111111111111111111.,
    22222222222222222222222222222222222.
  );
}

function $divide(a, b) {
  if (b === 0) {
    return 0;
  }
  return a / b;
}
"#
    );
}
