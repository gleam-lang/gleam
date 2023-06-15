use crate::{assert_erl, assert_module_error};

#[test]
fn integration_test1_3() {
    assert_erl!(r#"pub external fn run() -> Int = "Elixir.MyApp" "run""#);
}

#[test]
fn integration_test7() {
    assert_erl!(
        r#"pub external fn receive() -> Int = "try" "and"
                    pub fn catch(x) { receive() }"#
    );
}

#[test]
fn private_external_function_calls() {
    // Private external function calls are inlined
    assert_erl!(
        r#"external fn go(x: Int, y: Int) -> Int = "m" "f"
pub fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }"#
    );
}

#[test]
fn public_local_function_calls() {
    // Public external function calls are inlined but the wrapper function is
    // also printed in the erlang output and exported
    assert_erl!(
        r#"pub external fn go(x: Int, y: Int) -> Int = "m" "f"
                    fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }"#
    );
}

#[test]
fn private_local_function_references() {
    // Private external function references are inlined
    assert_erl!(
        r#"external fn go(x: Int, y: Int) -> Int = "m" "f"
pub fn x() { go }"#
    );
}

// TODO: link to the issue
#[test]
fn inlining_external_functions_from_another_module() {
    assert_erl!(
        (
            "lib",
            "atom",
            "pub external type Atom
pub external fn make(String) -> String = \"erlang\" \"binary_to_atom\""
        ),
        "import atom
pub fn main() {
  atom.make(\"ok\")
}
"
    );
}

// TODO: link to the issue
#[test]
fn unqualified_inlining_external_functions_from_another_module() {
    assert_erl!(
        (
            "lib",
            "atom",
            "pub external type Atom
pub external fn make(String) -> String = \"erlang\" \"binary_to_atom\""
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
