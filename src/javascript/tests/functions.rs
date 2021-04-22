use crate::assert_js;

#[test]
fn exported_functions() {
    assert_js!(
        r#"
pub fn add(x, y) {
    x + y
}"#,
        r#""use strict";

export function add(x, y) {
  return x + y;
}
"#
    );
}

#[test]
fn calling_functions() {
    assert_js!(
        r#"
pub fn twice(f: fn(t) -> t, x: t) -> t {
    f(f(x))
}
pub fn add_one(x: Int) -> Int {
    x + 1
}
pub fn add_two(x: Int) -> Int {
    twice(add_one, x)
}

pub fn take_two(x: Int) -> Int {
    twice(fn(y) {y - 1}, x)
}
"#,
        r#""use strict";

export function twice(f, x) {
  return f(f(x));
}

export function add_one(x) {
  return x + 1;
}

export function add_two(x) {
  return twice(add_one, x);
}

export function take_two(x) {
  return twice((y) => { return y - 1; }, x);
}
"#
    );
}
