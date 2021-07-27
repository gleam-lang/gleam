use crate::assert_js;

#[test]
fn tuple_matching() {
    assert_js!(
        r#"
fn go(x) {
  let #(1, 2) = x
}
"#,
        r#""use strict";

function go(x) {
  if (x[0] !== 1 || x[1] !== 2) throw new Error("Bad match");
  return x;
}
"#
    )
}

#[test]
fn assert() {
    assert_js!(
        r#"fn go(x) { assert 1 = x }"#,
        r#""use strict";

function go(x) {
  if (x !== 1) throw new Error("Bad match");
  return x;
}
"#
    );

    assert_js!(
        r#"fn go(x) { assert #(1, 2) = x }"#,
        r#""use strict";

function go(x) {
  if (x[0] !== 1 || x[1] !== 2) throw new Error("Bad match");
  return x;
}
"#
    );
}

#[test]
fn nested_binding() {
    assert_js!(
        r#"
fn go(x) {
  let #(a, #(b, c, 2) as t, _, 1) = x
}
"#,
        r#""use strict";

function go(x) {
  if (x[1][2] !== 2 || x[3] !== 1) throw new Error("Bad match");
  let a = x[0];
  let t = x[1];
  let b = x[1][0];
  let c = x[1][1];
  return x;
}
"#
    )
}

#[test]
fn variable_renaming() {
    assert_js!(
        r#"

fn go(x, foo) {
  let a = 1
  foo(a)
  let a = 2
  foo(a)
  let #(a, 3) = x
  let b = a
  foo(b)
  let c = {
    let a = a
    #(a, b)
  }
  foo(a)
  // make sure arguments are counted in initial state
  let x = c
  x
}
"#,
        r#""use strict";

function go(x, foo) {
  let a = 1;
  foo(a);
  let a$1 = 2;
  foo(a$1);
  if (x[1] !== 3) throw new Error("Bad match");
  let a$2 = x[0];
  let b = a$2;
  foo(b);
  let c = (() => {
    let a$3 = a$2;
    return [a$3, b];
  })();
  foo(a$2);
  let x$1 = c;
  return x$1;
}
"#
    )
}

#[test]
fn constant_assignments() {
    assert_js!(
        r#"
const a = True

fn go() {
  a
  let a = 10
  a + 20
}

fn second() {
  let a = 10
  a + 20
}
"#,
        r#""use strict";

const a = true;

function go() {
  a;
  let a$1 = 10;
  return a$1 + 20;
}

function second() {
  let a$1 = 10;
  return a$1 + 20;
}
"#
    );
}

#[test]
fn returning_literal_subject() {
    assert_js!(
        r#"fn go(x) { assert 1 = x + 1 }"#,
        r#""use strict";

function go(x) {
  let $ = x + 1;
  if ($ !== 1) throw new Error("Bad match");
  return $;
}
"#
    );
}

#[test]
fn rebound_argument() {
    assert_js!(
        r#"pub fn main(x) {
  let x = False
  x
}
"#,
        r#""use strict";

export function main(x) {
  let x$1 = false;
  return x$1;
}
"#
    );
}

#[test]
fn rebound_function() {
    assert_js!(
        r#"pub fn x() { 
  Nil
}
        
pub fn main() {
  let x = False
  x
}
"#,
        r#""use strict";

export function x() {
  return undefined;
}

export function main() {
  let x$1 = false;
  return x$1;
}
"#
    );
}

#[test]
fn rebound_function_and_arg() {
    assert_js!(
        r#"pub fn x() { 
  Nil
}
        
pub fn main(x) {
  let x = False
  x
}
"#,
        r#""use strict";

export function x() {
  return undefined;
}

export function main(x) {
  let x$1 = false;
  return x$1;
}
"#
    );
}

#[test]
fn variable_used_in_pattern_and_assignment() {
    assert_js!(
        r#"pub fn main(x) {
  let #(x) = #(x)
  x
}
"#,
        r#""use strict";

export function main(x) {
  let $ = [x];
  let x$1 = $[0];
  return x$1;
}
"#
    );
}
