use crate::{assert_js, assert_module_error, assert_ts_def};

#[test]
fn type_() {
    assert_js!(r#"pub type Thing"#,);
}

#[test]
fn module_fn() {
    assert_js!(
        r#"
@external(javascript, "utils", "inspect")
fn show(x: anything) -> Nil"#,
    );
}

#[test]
fn pub_module_fn() {
    assert_js!(
        r#"
@external(javascript, "utils", "inspect")
pub fn show(x: anything) -> Nil"#,
    );
}

#[test]
fn pub_module_fn_typescript() {
    assert_ts_def!(
        r#"
@external(javascript, "utils", "inspect")
pub fn show(x: anything) -> Nil"#,
    );
}

#[test]
fn same_name_external() {
    assert_js!(
        r#"
@external(javascript, "thingy", "fetch")
pub fn fetch(request: Nil) -> Nil"#,
    );
}

#[test]
fn same_module_multiple_imports() {
    assert_js!(
        r#"
@external(javascript, "./the/module.mjs", "one")
pub fn one() -> Nil

@external(javascript, "./the/module.mjs", "two")
pub fn two() -> Nil
"#,
    );
}

#[test]
fn duplicate_import() {
    assert_js!(
        r#"
@external(javascript, "./the/module.mjs", "dup")
pub fn one() -> Nil

@external(javascript, "./the/module.mjs", "dup")
pub fn two() -> Nil
"#,
    );
}

#[test]
fn name_to_escape() {
    assert_js!(
        r#"
@external(javascript, "./the/module.mjs", "one")
pub fn class() -> Nil
"#,
    );
}

#[test]
fn external_type_typescript() {
    assert_ts_def!(
        r#"pub type Queue(a)

@external(javascript, "queue", "new")
pub fn new() -> Queue(a)
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1636
#[test]
fn external_fn_escaping() {
    assert_js!(
        r#"
@external(javascript, "./ffi.js", "then")
pub fn then(a: a) -> b"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1954
#[test]
fn pipe_variable_shadow() {
    assert_js!(
        r#"
@external(javascript, "module", "string")
fn name() -> String

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
pub type TESTitem

@external(javascript, "it", "one")
pub fn one(a: TESTitem) -> TESTitem
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
