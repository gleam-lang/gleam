mod booleans;
mod custom_types;
mod functions;
mod lists;
mod numbers;
mod strings;

// TODO have a macro that allows us to compile multiple modules in certain tests.
// https://github.com/gleam-lang/gleam/pull/1070#discussion_r619806755
fn rocket_ship_module() -> crate::type_::Module {
    let src = r#"
pub const c = 299_792_458

pub fn launch() {
  Ok("launched")
}
pub fn fuel(amount: Int) {
  Ok("fueled")
}
  "#;
    let (mut ast, _) = crate::parse::parse_module(src).expect("syntax error");
    ast.name = vec!["rocket_ship".to_string()];
    let mut modules = std::collections::HashMap::new();
    let mut uid = 0;

    let _ = modules.insert(
        "gleam".to_string(),
        (
            crate::build::Origin::Src,
            crate::type_::build_prelude(&mut uid),
        ),
    );
    let ast = crate::type_::infer_module(&mut 0, ast, &modules, &mut vec![])
        .expect("should successfully infer");
    ast.type_info
}

#[macro_export]
macro_rules! assert_js {
    ($src:expr, $erl:expr $(,)?) => {{
        use crate::javascript::*;
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["the_app".to_string()];
        let mut modules = std::collections::HashMap::new();
        let mut uid = 0;
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert(
            "gleam".to_string(),
            (
                crate::build::Origin::Src,
                crate::type_::build_prelude(&mut uid),
            ),
        );
        let _ = modules.insert(
            "rocket_ship".to_string(),
            (crate::build::Origin::Src, rocket_ship_module()),
        );
        let ast = crate::type_::infer_module(&mut 0, ast, &modules, &mut vec![])
            .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module(&ast, &line_numbers, &mut output).unwrap();
        assert_eq!(($src, output), ($src, $erl.to_string()));
    }};
}

#[test]
fn sequences() {
    assert_js!(
        r#"
fn go() {
  "one"
  "two"
  "three"
}
"#,
        r#""use strict";

function go() {
  "one";
  "two";
  return "three";
}
"#
    );
}

#[test]
fn tuple() {
    assert_js!(
        r#"
fn go() {
  #("1", "2", "3")
}
"#,
        r#""use strict";

function go() {
  return ["1", "2", "3"];
}
"#
    );

    assert_js!(
        r#"
fn go() {
  #(
    "1111111111111111111111111111111",
    #("1111111111111111111111111111111", "2", "3"),
    "3",
  )
}
"#,
        r#""use strict";

function go() {
  return [
    "1111111111111111111111111111111",
    ["1111111111111111111111111111111", "2", "3"],
    "3",
  ];
}
"#
    );
}

#[test]
fn tuple_access() {
    assert_js!(
        r#"
fn go() {
  #(1, 2).0
}
"#,
        r#""use strict";

function go() {
  return [1, 2][0];
}
"#
    )
}

#[test]
fn tuple_with_block_element() {
    assert_js!(
        r#"
fn go() {
  #(
    "1", 
    {
      "2"
      "3"
    },
  )
}
"#,
        r#""use strict";

function go() {
  return [
    "1",
    (() => {
      "2";
      return "3";
    })(),
  ];
}
"#
    );

    assert_js!(
        r#"
fn go() {
  #(
    "1111111111111111111111111111111",
    #("1111111111111111111111111111111", "2", "3"),
    "3",
  )
}
"#,
        r#""use strict";

function go() {
  return [
    "1111111111111111111111111111111",
    ["1111111111111111111111111111111", "2", "3"],
    "3",
  ];
}
"#
    );
}

#[test]
fn constant_tuples() {
    assert_js!(
        r#"
const a = "Hello"
const b = 1;
const c = 2.0;
const e = #("bob", "dug")
        "#,
        r#""use strict";

const a = "Hello";

const b = 1;

const c = 2.0;

const e = ["bob", "dug"];
"#
    );

    assert_js!(
        r#"
const e = #(
  "loooooooooooooong", "loooooooooooong", "loooooooooooooong",
  "loooooooooooooong", "loooooooooooong", "loooooooooooooong",
)
        "#,
        r#""use strict";

const e = [
  "loooooooooooooong",
  "loooooooooooong",
  "loooooooooooooong",
  "loooooooooooooong",
  "loooooooooooong",
  "loooooooooooooong",
];
"#
    );
}

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
  let gleam$tmp = x;
  if (!(gleam$tmp[0] === 1 && gleam$tmp[1] === 2)) throw new Error("Bad match");
}
"#
    )
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
  let gleam$tmp = x;
  if (!(
    gleam$tmp[1][2] === 2 &&
    gleam$tmp[3] === 1
  )) throw new Error("Bad match");
  let a = gleam$tmp[0];
  let t = gleam$tmp[1];
  let b = gleam$tmp[1][0];
  let c = gleam$tmp[1][1];
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
  let gleam$tmp = x;
  if (!(gleam$tmp[1] === 3)) throw new Error("Bad match");
  let a$2 = gleam$tmp[0];
  
  let b = a$2;
  foo(b);
  let c = (() => {
    let a$3 = a$2;
    return [a$3, b]; })();
  foo(a$2);
  let x$1 = c;
  return x$1;
}
"#
    )
}

#[test]
fn external_functions() {
    assert_js!(
        r#"
pub external type Thing
external fn foo() -> Thing = "document" "foo"
pub external fn bar(Int, String) -> Thing = "document" "bar"
external fn baz(x: Int, y: String) -> Thing = "document" "baz"
 
"#,
        r#""use strict";

function foo() {
  return document.foo()
}

export function bar(arg0, arg1) {
  return document.bar(arg0, arg1)
}

function baz(x, y) {
  return document.baz(x, y)
}
"#
    );
}

#[test]
fn importing_a_module() {
    assert_js!(
        r#"
import rocket_ship
import rocket_ship as foo
import rocket_ship.{launch as boom_time, fuel}
pub fn go() {
    rocket_ship.fuel(100)
    boom_time()
    rocket_ship.c
}
"#,
        r#""use strict";

import * as rocket_ship from "./rocket_ship.js";

import * as foo from "./rocket_ship.js";

import * as rocket_ship from "./rocket_ship.js";
const { launch: boom_time, fuel } = rocket_ship;

export function go() {
  rocket_ship.fuel(100);
  boom_time();
  return rocket_ship.c;
}
"#
    );
}

#[test]
fn equality() {
    assert_js!(
        r#"
fn go() {
  1 == 2
  1 != 2
}
"#,
        r#""use strict";

function go() {
  $deepEqual(1, 2);
  return !$deepEqual(1, 2);
}

function $deepEqual(x, y) {
  if ($isObject(x) && $isObject(y)) {
    const kx = Object.keys(x);
    const ky = Object.keys(x);

    if (kx.length != ky.length) {
      return false;
    }

    for (const k of kx) {
      const a = x[k];
      const b = y[k];
      if !$deepEqual(a, b) {
        return false
      }
    }

    return true;

  } else {
    return x === y;
  }
}

function $isObject(object) {
  return object != null && typeof object === 'object';
}
"#
    );
}

#[test]
fn todo_throws_error() {
    assert_js!(
        r#"
fn go() {
    todo
}
"#,
        r#""use strict";

function go() {
  throw Object.assign(
    new Error("This has not yet been implemented"),
    { gleam_error: "todo", module: "the_app", function: "go", line: 3 }
  )
}
"#
    );
    assert_js!(
        r#"
fn go() {
    todo("I should do this");
}
"#,
        r#""use strict";

function go() {
  throw Object.assign(
    new Error("I should do this"),
    { gleam_error: "todo", module: "the_app", function: "go", line: 3 }
  )
}
"#
    );
}
