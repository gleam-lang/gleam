use insta::assert_debug_snapshot;
use itertools::Itertools;
use lsp_types::{CompletionItem, Position};

use super::*;

fn completion(tester: TestProject<'_>, position: Position) -> Vec<CompletionItem> {
    tester.at(position, |engine, param, src| {
        let response = engine.completion(param, src);

        let mut completions = response.result.unwrap().unwrap_or_default();
        completions.sort_by(|a, b| a.label.cmp(&b.label));
        completions
    })
}

fn completion_at_default_position(tester: TestProject<'_>) -> Vec<CompletionItem> {
    let src = &format!("fn typing_in_here() {{\n  0\n}}\n {}", tester.src);
    let tester = TestProject { src, ..tester };
    completion(tester, Position::new(1, 0))
        .into_iter()
        .filter(|c| c.label != "typing_in_here")
        .collect_vec()
}

#[test]
fn completions_for_outside_a_function() {
    let code = "

pub fn main() {
  0
}";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code),
        Position::new(0, 0)
    ));
}

#[test]
fn local_public_function() {
    let code = "
pub fn main() {
  0
}";

    assert_debug_snapshot!(completion_at_default_position(TestProject::for_source(
        code
    )),);
}

#[test]
fn local_public_function_with_documentation() {
    let code = "
/// Hello
pub fn main() {
  0
}";

    assert_debug_snapshot!(completion_at_default_position(TestProject::for_source(
        code
    )),);
}

#[test]
fn local_public_enum() {
    let code = "
pub type Direction {
  Left
  Right
}
";

    assert_debug_snapshot!(completion_at_default_position(TestProject::for_source(
        code
    )),);
}

#[test]
fn local_public_record() {
    let code = "
pub type Box {
/// Hello
  Box(Int, Int, Float)
}
";

    assert_debug_snapshot!(completion_at_default_position(TestProject::for_source(
        code
    )),);
}

#[test]
fn local_public_enum_with_documentation() {
    let code = "
pub type Direction {
  /// Hello
  Left
  /// Goodbye
  Right
}
";

    assert_debug_snapshot!(completion_at_default_position(TestProject::for_source(
        code
    )),);
}

#[test]
fn local_public_record_with_documentation() {
    let code = "
pub type Box {
  Box(Int, Int, Float)
}
";

    assert_debug_snapshot!(completion_at_default_position(TestProject::for_source(
        code
    )),);
}

#[test]
fn imported_module_function() {
    let code = "
import dep
";
    let dep = "
pub fn wobble() {
  Nil
}
";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("dep", dep)
    ),);
}

#[test]
fn importable_module_function() {
    let code = "
";
    let dep = "
pub fn wobble() {
  Nil
}
";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("dep", dep)
    ),);
}

#[test]
fn importable_module_function_with_existing_imports() {
    let code = "
//// Some module comments
// Some other whitespace

import dep2
";
    let dep = "
pub fn wobble() {
  Nil
}
";
    let dep2 = "
pub fn wobble() {
  Nil
}
";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("dep2", dep2)
    ),);
}

#[test]
fn importable_module_function_from_deep_module() {
    let code = "
";
    let dep = "
pub fn wobble() {
  Nil
}
";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("a/b/dep", dep)
    ),);
}

#[test]
fn imported_public_enum() {
    let code = "
import dep
";
    let dep = "
pub type Direction {
  Left
  Right
}
";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("dep", dep)
    ),);
}

#[test]
fn imported_public_record() {
    let code = "
import dep
";
    let dep = "
pub type Box {
  Box(Int)
}
";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("dep", dep)
    ),);
}

#[test]
fn imported_unqualified_module_function() {
    let code = "
import dep.{wobble}
";
    let dep = "
pub fn wobble() {
  Nil
}
";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("dep", dep)
    ),);
}

#[test]
fn imported_unqualified_public_enum() {
    let code = "
import dep.{Left}
";
    let dep = "
pub type Direction {
  Left
  Right
}
";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("dep", dep)
    ),);
}

#[test]
fn imported_unqualified_public_record() {
    let code = "
import dep.{Box}
";
    let dep = "
pub type Box {
  Box(Int)
}
";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("dep", dep)
    ),);
}

#[test]
fn private_function() {
    let code = "
fn private() {
  1
}
";
    let dep = "";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("dep", dep)
    ),);
}

#[test]
fn private_type() {
    let code = "
type Wibble {
  Wobble
}
";
    let dep = "";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("dep", dep)
    ),);
}

#[test]
fn opaque_type() {
    let code = "
pub opaque type Wibble {
  Wobble
}
";
    let dep = "";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("dep", dep)
    ),);
}

#[test]
fn private_function_in_dep() {
    let code = "import dep";
    let dep = "
fn private() {
  1
}
";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("dep", dep)
    ),);
}

#[test]
fn private_type_in_dep() {
    let code = "import dep";
    let dep = "
type Wibble {
  Wobble
}
";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("dep", dep)
    ),);
}

#[test]
fn in_custom_type_definition() {
    let code = "import dep";
    let dep = "
type Wibble {
  Wobble
}
";

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source(code).add_module("dep", dep)
    ),);
}

#[test]
fn for_custom_type_definition() {
    let code = "
pub type Wibble {
  Wobble
}";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code),
        Position::new(2, 0)
    ),);
}

#[test]
fn for_type_alias() {
    let code = "
pub type Wibble = Result(
  String,
  String
)
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code),
        Position::new(2, 0)
    ),);
}

#[test]
fn for_function_arguments() {
    let code = "
pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code),
        Position::new(2, 0)
    ),);
}

#[test]
fn imported_type() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep

pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(3, 0)
    ),);
}

#[test]
fn imported_type_cursor_after_dot() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep

pub fn wibble(
  _: dep.Zoo,
) -> Nil {
  Nil
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(3, 12)
    ),);
}

#[test]
fn imported_type_cursor_after_dot_other_matching_modules() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let dep2 = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep
import dep2

pub fn wibble(
  _: dep.Zoo,
) -> Nil {
  Nil
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("dep2", dep2),
        Position::new(4, 12)
    ),);
}

#[test]
fn imported_type_cursor_after_dot_other_modules() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let other = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep

pub fn wibble(
  _: dep.Zoo,
) -> Nil {
  Nil
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("other", other),
        Position::new(3, 12)
    ),);
}

#[test]
fn imported_type_cursor_mid_phrase_other_modules() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let other = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep

pub fn wibble(
  _: dep.Zoo,
) -> Nil {
  Nil
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("other", other),
        Position::new(3, 8)
    ),);
}

#[test]
fn importable_type() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "

pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(3, 0)
    ),);
}

#[test]
fn importable_type_with_existing_imports_at_top() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let dep2 = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep2

pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("dep2", dep2),
        Position::new(3, 0)
    ),);
}

#[test]
fn importable_type_with_existing_imports() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let dep2 = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "
//// Some module comments
// Some other whitespace

import dep2

pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("dep2", dep2),
        Position::new(7, 0)
    ),);
}

#[test]
fn importable_type_from_deep_module() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "

pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_module("a/b/dep", dep),
        Position::new(3, 0)
    ),);
}

#[test]
fn unqualified_imported_type() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep.{type Zoo}

pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(3, 0)
    ),);
}

#[test]
fn local_private_type() {
    let code = "
type Zoo = Int

pub fn wibble(
  x: String,
) -> String {
  \"ok\"
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code),
        Position::new(4, 0)
    ),);
}

#[test]
fn internal_values_from_root_package_are_in_the_completions() {
    let dep = r#"
@external(erlang, "rand", "uniform")
@internal pub fn random_float() -> Float
@internal pub fn main() { 0 }
@internal pub type Wibble { Wobble }
@internal pub const wibble = 1
"#;

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source("import dep").add_module("dep", dep)
    ),);
}

#[test]
fn internal_types_from_root_package_are_in_the_completions() {
    let code = "import dep

pub fn wibble(
    _: String,
) -> Nil {
    Nil
}";

    let dep = r#"
@internal pub type Alias = Int
@internal pub type AnotherType { Constructor }
"#;
    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(3, 0)
    ),);
}

#[test]
fn internal_values_from_the_same_module_are_in_the_completions() {
    let code = r#"
@external(erlang, "rand", "uniform")
@internal pub fn random_float() -> Float
@internal pub fn main() { 0 }
@internal pub type Wibble { Wobble }
@internal pub const wibble = 1
"#;

    assert_debug_snapshot!(completion_at_default_position(TestProject::for_source(
        code
    )),);
}

#[test]
fn internal_types_from_the_same_module_are_in_the_completions() {
    let code = "
@internal pub type Alias = Result(Int, String)
@internal pub type AnotherType {
  Wibble
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code),
        Position::new(3, 0)
    ),);
}

#[test]
fn internal_types_from_a_dependency_are_ignored() {
    let code = "import dep

pub fn wibble(
    _: String,
) -> Nil {
    Nil
}";

    let dep = r#"
@internal pub type Alias = Int
@internal pub type AnotherType { Constructor }
"#;

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_dep_module("dep", dep),
        Position::new(3, 0)
    ),);
}

#[test]
fn internal_values_from_a_dependency_are_ignored() {
    let dep = r#"
@external(erlang, "rand", "uniform")
@internal pub fn random_float() -> Float
@internal pub fn main() { 0 }
@internal pub type Wibble { Wobble }
@internal pub const wibble = 1
"#;

    assert_debug_snapshot!(completion_at_default_position(
        TestProject::for_source("import dep").add_dep_module("dep", dep)
    ),);
}

#[test]
fn completions_for_an_import() {
    let code = "import dep

pub fn main() {
  0
}";
    let dep = "";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(0, 10)
    ),);
}

#[test]
fn completions_for_an_import_no_test() {
    let code = "import gleam

pub fn main() {
  0
}";
    let test = "
import gleam

pub fn main() {
  0
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_test_module("my_tests", test),
        Position::new(0, 10)
    ),);
}

#[test]
fn completions_for_an_import_while_in_test() {
    let code = "import gleam

pub fn main() {
  0
}";
    let test = "
import gleam

pub fn main() {
  0
}
";
    let test_helper = "
pub fn test_helper() {
  0
}
";

    let (mut engine, position_param) = TestProject::for_source(code)
        .add_test_module("my_test", test)
        .add_test_module("test_helper", test_helper)
        .positioned_with_io_in_test(Position::new(0, 10), "my_test");

    let response = engine.completion(position_param, code.into());

    let mut completions = response.result.unwrap().unwrap_or_default();
    completions.sort_by(|a, b| a.label.cmp(&b.label));

    assert_debug_snapshot!(completions,);
}

#[test]
fn completions_for_an_import_with_docs() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "//// Some package
//// documentation!

pub fn main() { 1 }
    ";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_dep_module("dep", dep),
        Position::new(0, 10)
    ),);
}

#[test]
fn completions_for_an_import_from_dependency() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_hex_module("example_module", dep),
        Position::new(0, 10)
    ),);
}

#[test]
fn completions_for_an_import_not_from_indirect_dependency() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code)
            .add_hex_module("example_module", dep)
            .add_indirect_hex_module("indirect_module", ""),
        Position::new(0, 10)
    ),);
}

#[test]
fn completions_for_an_import_not_from_dev_dependency() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code)
            .add_hex_module("example_module", dep)
            .add_dev_hex_module("indirect_module", ""),
        Position::new(0, 10)
    ),);
}

#[test]
fn completions_for_an_import_not_from_dev_dependency_in_test() {
    let code = "import gleam

pub fn main() {
  0
}";
    let test = "import gleam

pub fn main() {
  0
}
";
    let dep = "";

    let (mut engine, position_param) = TestProject::for_source(code)
        .add_test_module("my_test", test)
        .add_hex_module("example_module", dep)
        .add_dev_hex_module("indirect_module", "")
        .positioned_with_io_in_test(Position::new(0, 10), "my_test");

    let response = engine.completion(position_param, code.into());

    let mut completions = response.result.unwrap().unwrap_or_default();
    completions.sort_by(|a, b| a.label.cmp(&b.label));

    assert_debug_snapshot!(completions,);
}

#[test]
fn completions_for_an_import_from_dependency_with_docs() {
    let code = "//// Main package
//// documentation!

import gleam

pub fn main() {
  0
}";
    let dep = "//// Some package
//// documentation!

pub fn main() { 1 }
    ";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_hex_module("example_module", dep),
        Position::new(3, 10)
    ),);
}

#[test]
fn completions_for_an_import_start() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_dep_module("dep", dep),
        Position::new(0, 0)
    ),);
}

#[test]
fn completions_for_an_import_preceeding_whitespace() {
    let code = " import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_dep_module("dep", dep),
        Position::new(0, 2)
    ),);
}

#[test]
fn internal_modules_from_same_package_are_included() {
    let code = "import gleam

pub fn main() {
  0
}";
    let internal_name = format!("{}/internal", LSP_TEST_ROOT_PACKAGE_NAME);

    assert_debug_snapshot!(completion(
        TestProject::for_source(code)
            // Not included
            .add_dep_module("dep/internal", "")
            // Included
            .add_module(&internal_name, ""),
        Position::new(0, 0)
    ),);
}

#[test]
fn completions_for_an_unqualified_import() {
    let code = "
import dep.{}

pub fn main() {
  0
}";
    let dep = "pub const wibble = \"wibble\"
const wobble = \"wobble\"
@internal
pub const wabble = \"wabble\"

pub fn myfun() {
    0
}

pub type Wibble = String
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(1, 12)
    ),);
}

#[test]
fn completions_for_an_unqualified_import_on_new_line() {
    let code = "
import dep.{
  wibble,

}

pub fn main() {
  0
}";
    let dep = "pub const wibble = \"wibble\"

pub fn myfun() {
    0
}

pub type Wibble = String
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_module("dep", dep),
        // putting cursor at beginning of line because some formatters
        // remove the empty whitespace in the test code.
        // Does also work with (3, 2) when empty spaces are not removed.
        Position::new(3, 0)
    ),);
}

#[test]
fn completions_for_an_unqualified_import_already_imported() {
    let code = "
import dep.{wibble,wabble,type Wibble}

pub fn main() {
  0
}";
    let dep = "pub const wibble = \"wibble\"
const wobble = \"wobble\"
@internal
pub const wabble = \"wabble\"

pub fn myfun() {
    0
}

pub type Wibble = String
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(1, 12)
    ),);
}

#[test]
fn completions_for_a_function_arg_annotation() {
    let code = "
pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code),
        Position::new(2, 11)
    ),);
}

#[test]
fn completions_for_a_function_return_annotation() {
    let code = "
pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code),
        Position::new(3, 7)
    ),);
}

#[test]
fn completions_for_a_var_annotation() {
    let code = "
pub fn main() {
  let wibble: Int = 7
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code),
        Position::new(2, 16)
    ),);
}

#[test]
fn completions_for_a_const_annotation() {
    let code = "

const wibble: Int = 7

pub fn main() {
  let wibble: Int = 7
}
";

    assert_debug_snapshot!(completion(
        TestProject::for_source(code),
        Position::new(2, 16)
    ),);
}

#[test]
fn ignore_completions_in_empty_comment() {
    // Reproducing issue #2161
    let code = "
pub fn main() {
  case 0 {
    //
    _ -> 1
  }
}
";

    // End of the comment (right after the last `/`)
    assert_eq!(
        completion(TestProject::for_source(code), Position::new(3, 6)),
        vec![],
    );
}

#[test]
fn ignore_completions_in_middle_of_comment() {
    // Reproducing issue #2161
    let code = "
pub fn main() {
  case 0 {
    // comment
    _ -> 1
  }
}
";

    // At `c`
    assert_eq!(
        completion(TestProject::for_source(code), Position::new(3, 7)),
        vec![],
    );
}

#[test]
fn ignore_completions_in_end_of_comment() {
    // Reproducing issue #2161
    let code = "
pub fn main() {
  case 0 {
    // comment
    _ -> 1
  }
}
";

    // End of the comment (after `t`)
    assert_eq!(
        completion(TestProject::for_source(code), Position::new(3, 14)),
        vec![],
    );
}
