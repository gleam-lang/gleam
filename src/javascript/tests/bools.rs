use crate::assert_js;

#[test]
fn expressions() {
    assert_js!(
        r#"
fn go() {
    True
    False
    Nil
}
"#,
        r#""use strict";

function go() {
  true;
  false;
  return undefined;
}
"#
    );
}

#[test]
fn constants() {
    assert_js!(
        r#"
const a = True
const b = False
const c = Nil
"#,
        r#""use strict";

const a = true;

const b = false;

const c = undefined;
"#
    );
}

#[test]
fn operators() {
    assert_js!(
        r#"
fn go() {
    True && True
    False || False
}
"#,
        r#""use strict";

function go() {
  true && true;
  return false || false;
}
"#
    );
}

#[test]
fn assigning() {
    assert_js!(
        r#"
fn go(x, y) {
    assert True = x
    assert False = x
    assert Nil = y
}
"#,
        r#""use strict";

function go(x, y) {
  if (!x) throw new Error("Bad match");
  
  if (x) throw new Error("Bad match");
  
  if (y) throw new Error("Bad match");
  return y;
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1112
// differentiate between prelude constructors and custom type constructors
#[test]
fn shadowed_bools_and_nil() {
    assert_js!(
        r#"
pub type True { True False Nil }
fn go(x, y) {
    let True = x
    let False = x
    let Nil = y
}
"#,
        r#""use strict";

function go(x, y) {
  if (x.type !== "True") throw new Error("Bad match");
  
  if (x.type !== "False") throw new Error("Bad match");
  
  if (y.type !== "Nil") throw new Error("Bad match");
  return y;
}
"#
    );
}

#[test]
fn equality() {
    assert_js!(
        r#"
fn go(a, b) {
  a == True
  a != True
  a == False
  a != False
  a == a
  a != a
  b == Nil
  b != Nil
  b == b
}
"#,
        r#""use strict";

function go(a, b) {
  a === true;
  a !== true;
  a === false;
  a !== false;
  a === a;
  a !== a;
  b === undefined;
  b !== undefined;
  return b === b;
}
"#
    );
}

#[test]
fn case() {
    assert_js!(
        r#"
fn go(a) {
  case a {
    True -> 1
    False -> 0
  }
}
"#,
        r#""use strict";

function go(a) {
  if (a) {
    return 1;
  } else if (!a) {
    return 0;
  } else {
    throw new Error("Bad match");
  }
}
"#
    );
}

#[test]
fn nil_case() {
    assert_js!(
        r#"
fn go(a) {
  case a {
    Nil -> 0
  }
}
"#,
        r#""use strict";

function go(a) {
  if (!a) {
    return 0;
  } else {
    throw new Error("Bad match");
  }
}
"#
    );
}
