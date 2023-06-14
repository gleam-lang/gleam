use crate::assert_erl;

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

// TODO: test duplicate externals for a single target are a compile error
// TODO: bodies absent is not an error if there is an external impl
// TODO: without a body or a suitable external impl is an error
// TODO: without type annotations is an error
