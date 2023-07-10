use crate::{assert_js, assert_module_error, assert_ts_def};

#[test]
fn type_() {
    assert_js!(r#"pub external type Thing"#,);
}

#[test]
fn module_fn() {
    assert_js!(r#"external fn show(anything) -> Nil = "utils" "inspect""#,);
}

#[test]
fn pub_module_fn() {
    assert_js!(r#"pub external fn show(anything) -> Nil = "utils" "inspect""#,);
}

#[test]
fn pub_module_fn_typescript() {
    assert_ts_def!(r#"pub external fn show(anything) -> Nil = "utils" "inspect""#,);
}

#[test]
fn global_fn() {
    assert_js!(r#"external fn down(Float) -> Float = "" "Math.floor""#,);
}

#[test]
fn pub_global_fn() {
    assert_js!(r#"pub external fn down(Float) -> Float = "" "Math.floor""#,);
}

#[test]
fn pub_global_fn_typescript() {
    assert_ts_def!(r#"pub external fn down(Float) -> Float = "" "Math.floor""#,);
}

#[test]
fn same_name_global_external() {
    assert_js!(r#"pub external fn fetch(Nil) -> Nil = "" "fetch""#,);
}

#[test]
fn same_module_multiple_imports() {
    assert_js!(
        r#"pub external fn one() -> Nil = "./the/module.mjs" "one"
pub external fn two() -> Nil = "./the/module.mjs" "two"
"#,
    );
}

#[test]
fn duplicate_import() {
    assert_js!(
        r#"pub external fn one() -> Nil = "./the/module.mjs" "dup"
pub external fn two() -> Nil = "./the/module.mjs" "dup"
"#,
    );
}

#[test]
fn name_to_escape() {
    assert_js!(
        r#"pub external fn class() -> Nil = "./the/module.mjs" "one"
"#,
    );
}

#[test]
fn external_type_typescript() {
    assert_ts_def!(
        r#"pub external type Queue(a)
pub external fn new() -> Queue(a) = "queue" "new"
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1636
#[test]
fn external_fn_escaping() {
    assert_js!(r#"pub external fn then(a) -> b = "./ffi.js" "then""#,);
}

// https://github.com/gleam-lang/gleam/issues/1954
#[test]
fn pipe_variable_shadow() {
    assert_js!(
        r#"
external fn name() -> String = "module" "string"

pub fn main() {
  let name = name()
  name
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2090
#[test]
fn tf_type_name_usage() {
    assert_ts_def!(
        r#"
pub external type TESTitem
pub external fn one(TESTitem) -> TESTitem = "" "one"
"#
    );
}

#[test]
fn attribute_erlang() {
    assert_js!(
        r#"
@external(erlang, "one", "one_erl")
pub fn one(x: Int) -> Int {
  todo
}

pub fn main() {
  one(1)
}
"#
    );
}

#[test]
fn attribute_javascript() {
    assert_js!(
        r#"
@external(javascript, "./one.mjs", "oneJs")
pub fn one(x: Int) -> Int {
  todo
}

pub fn main() {
  one(1)
}
"#
    );
}

#[test]
fn erlang_and_javascript() {
    assert_js!(
        r#"
@external(erlang, "one", "one")
@external(javascript, "./one.mjs", "oneJs")
pub fn one(x: Int) -> Int {
  todo
}

pub fn main() {
  one(1)
}
"#
    );
}

#[test]
fn private_attribute_erlang() {
    assert_js!(
        r#"
@external(erlang, "one", "one_erl")
fn one(x: Int) -> Int {
  todo
}

pub fn main() {
  one(1)
}
"#
    );
}

#[test]
fn private_attribute_javascript() {
    assert_js!(
        r#"
@external(javascript, "./one.mjs", "oneJs")
fn one(x: Int) -> Int {
  todo
}

pub fn main() {
  one(1)
}
"#
    );
}

#[test]
fn private_erlang_and_javascript() {
    assert_js!(
        r#"
@external(erlang, "one", "one")
@external(javascript, "./one.mjs", "oneJs")
fn one(x: Int) -> Int {
  todo
}

pub fn main() {
  one(1)
}
"#
    );
}

#[test]
fn no_body() {
    assert_js!(
        r#"
@external(javascript, "one", "one")
pub fn one(x: Int) -> Int
"#
    );
}

#[test]
fn no_module() {
    assert_module_error!(
        r#"
@external(javascript, "", "one")
pub fn one(x: Int) -> Int {
  1
}
"#
    );
}

#[test]
fn inline_function() {
    assert_module_error!(
        r#"
@external(javascript, "blah", "(x => x)")
pub fn one(x: Int) -> Int {
  1
}
"#
    );
}
