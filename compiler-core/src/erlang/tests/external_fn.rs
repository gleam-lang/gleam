use crate::{assert_erl, assert_js_module_error, assert_module_error};

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
pub fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }
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
pub fn make(x: String) -> Atom
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
pub fn make(x: String) -> Atom
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
fn reference_to_imported_elixir_external_fn() {
    assert_erl!(
        (
            "lib",
            "my_app",
            r#"
@external(erlang, "Elixir.MyApp", "run")
pub fn run() -> Int
"#
        ),
        r#"import my_app
pub fn main() {
  let x = my_app.run
  id(my_app.run)
}
fn id(x) { x }
"#
    );
}

#[test]
fn unqualified_reference_to_imported_elixir_external_fn() {
    assert_erl!(
        (
            "lib",
            "my_app",
            r#"
@external(erlang, "Elixir.MyApp", "run")
pub fn run() -> Int
"#
        ),
        r#"import my_app.{run}
pub fn main() {
  let x = run
  id(run)
}
fn id(x) { x }
"#
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
pub fn one(x: Int) -> Float
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

#[test]
fn javascript_only() {
    assert_erl!(
        r#"
pub fn should_be_generated(x: Int) -> Int {
  x
}

@external(javascript, "one", "one")
pub fn should_not_be_generated(x: Int) -> Int
"#
    );
}

#[test]
fn javascript_only_indirect() {
    assert_erl!(
        r#"
pub fn should_be_generated(x: Int) -> Int {
  x
}

@external(javascript, "one", "one")
pub fn should_not_be_generated(x: Int) -> Int

pub fn also_should_not_be_generated() {
  should_not_be_generated(1)
  |> should_be_generated
}
"#
    );
}

#[test]
fn both_externals_no_valid_impl() {
    assert_erl!(
        r#"
@external(javascript, "one", "one")
pub fn js() -> Nil

@external(erlang, "one", "one")
pub fn erl() -> Nil

pub fn should_not_be_generated() {
  js()
  erl()
}
"#
    );
}

#[test]
fn no_gleam_impl_no_annotations_function_fault_tolerance() {
    // A function not having annotations when required does not stop analysis.
    assert_module_error!(
        r#"
@external(erlang, "one", "two")
pub fn no_impl()

pub type X = UnknownType
"#
    );
}

#[test]
fn no_target_supported_function_fault_tolerance() {
    // A function not supporting the current target does not stop analysis.
    assert_js_module_error!(
        r#"
// This will error for having no support on this platform
@external(erlang, "one", "two")
pub fn no_impl() -> Int

pub fn main() {
  // This will due to no_impl not having an appropriate implementation for the
  // target, NOT because it doesn't exist. The analyser should still know about
  // it, even though it is invalid.
  no_impl()
}
"#
    );
}

#[test]
fn discarded_arg_in_external_are_passed_correctly() {
    assert_erl!(
        r#"
@external(erlang, "wibble", "wobble")
pub fn woo(_a: a) -> Nil
"#
    );
}

#[test]
fn multiple_discarded_args_in_external_are_passed_correctly() {
    assert_erl!(
        r#"
@external(erlang, "wibble", "wobble")
pub fn woo(_: a, _: b) -> Nil
"#
    );
}

#[test]
fn multiple_discarded_args_in_external_are_passed_correctly_2() {
    assert_erl!(
        r#"
@external(erlang, "wibble", "wobble")
pub fn woo(__: a, _two: b) -> Nil
"#
    );
}
