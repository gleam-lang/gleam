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

// // TODO: FIXME: https://github.com/gleam-lang/gleam/issues/1112
// #[test]
// fn shadowed_bools_and_nil() {
//     assert_js!(
//         r#"
// pub type Things { True False Nil }
// fn go(x, y) {
//     let True = x
//     let False = x
//     let Nil = y
// }
// "#,
//         r#""use strict";
//
// function go(x, y) {
//   if (x.type !== "True") throw new Error("Bad match");
//
//   if (x.type !== "False") throw new Error("Bad match");
//
//   if (x.type !== "Nil") throw new Error("Bad match");
//   return y;
// }
// "#
//     );
// }
