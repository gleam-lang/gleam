use crate::{assert_erl, assert_module_error};

#[test]
fn integration_test1_3() {
    assert_erl!(
        r#"
@external(erlang, "Elixir.MyApp", "run")
pub fn run() -> Int 
"#
    );
}

#[test]
fn integration_test7() {
    assert_erl!(
        r#"
@external(erlang, "try", "and")
pub fn receive() -> Int
pub fn catch(x) { receive() }
"#
    );
}

#[test]
fn private_external_function_calls() {
    // Private external function calls are inlined
    assert_erl!(
        r#"
@external(erlang, "m", "f")
fn go(x x: Int, y y: Int) -> Int

pub fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }"#
    );
}

#[test]
fn public_local_function_calls() {
    // Public external function calls are inlined but the wrapper function is
    // also printed in the erlang output and exported
    assert_erl!(
        r#"
@external(erlang, "m", "f")
pub fn go(x x: Int, y y: Int) -> Int
fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }
"#
    );
}

#[test]
fn private_local_function_references() {
    // Private external function references are inlined
    assert_erl!(
        r#"
@external(erlang, "m", "f")
fn go(x: Int, y: Int) -> Int
pub fn x() { go }
"#
    );
}

#[test]
fn inlining_external_functions_from_another_module() {
    assert_erl!(
        (
            "lib",
            "atom",
            r#"
pub type Atom

@external(erlang, "erlang", "binary_to_atom")
pub fn make(x: String) -> String
"#
        ),
        r#"import atom
pub fn main() {
  atom.make("ok")
}
"#
    );
}

#[test]
fn unqualified_inlining_external_functions_from_another_module() {
    assert_erl!(
        (
            "lib",
            "atom",
            r#"
pub type Atom

@external(erlang, "erlang", "binary_to_atom")
pub fn make(x: String) -> String
"#
        ),
        "import atom.{make}
pub fn main() {
  make(\"ok\")
}
"
    );
}

#[test]
fn attribute_erlang() {
    assert_erl!(
        r#"
@external(erlang, "one", "one")
pub fn one(x: Int) -> Int {
  todo
}
"#
    );
}

#[test]
fn attribute_javascript() {
    assert_erl!(
        r#"
@external(javascript, "./one.mjs", "one")
pub fn one(x: Int) -> Int {
  todo
}
"#
    );
}

#[test]
fn erlang_and_javascript() {
    assert_erl!(
        r#"
@external(erlang, "one", "one")
@external(javascript, "./one.mjs", "one")
pub fn one(x: Int) -> Int {
  todo
}
"#
    );
}

#[test]
fn no_type_annotation_for_parameter() {
    assert_module_error!(
        r#"
@external(erlang, "one", "one")
pub fn one(x: Int, y) -> Int {
  todo
}
"#
    );
}

#[test]
fn no_type_annotation_for_return() {
    assert_module_error!(
        r#"
@external(erlang, "one", "one")
pub fn one(x: Int) {
  todo
}
"#
    );
}

#[test]
fn hole_parameter_erlang() {
    assert_module_error!(
        r#"
@external(erlang, "one", "one")
pub fn one(x: List(_)) -> Int {
  todo
}
"#
    );
}

#[test]
fn hole_return_erlang() {
    assert_module_error!(
        r#"
@external(erlang, "one", "one")
pub fn one(x: List(Int)) -> List(_) {
  todo
}
"#
    );
}

#[test]
fn hole_parameter_javascript() {
    assert_module_error!(
        r#"
@external(javascript, "one", "one")
pub fn one(x: List(_)) -> Int {
  todo
}
"#
    );
}

#[test]
fn hole_return_javascript() {
    assert_module_error!(
        r#"
@external(javascript, "one", "one")
pub fn one(x: List(Int)) -> List(_) {
  todo
}
"#
    );
}

#[test]
fn no_body() {
    assert_erl!(
        r#"
@external(erlang, "one", "one")
pub fn one(x: Int) -> Int
"#
    );
}

#[test]
fn no_body_or_implementation() {
    assert_module_error!(
        r#"
pub fn one(x: Int) -> Int
"#
    );
}

#[test]
fn private() {
    assert_erl!(
        r#"
pub fn main() {
  do()
}

@external(erlang, "library", "main")
fn do() -> Int
"#
    );
}

#[test]
fn elixir() {
    assert_erl!(
        r#"
pub fn main() {
  #(do, do())
}

@external(erlang, "Elixir.String", "main")
fn do() -> Int
"#
    );
}

#[test]
fn public_elixir() {
    assert_erl!(
        r#"
@external(erlang, "Elixir.String", "main")
pub fn do() -> Int
"#
    );
}
