use std::collections::HashMap;

use lsp_types::{Position, RenameParams, TextDocumentPositionParams, Url, WorkDoneProgressParams};

use crate::language_server::tests::{TestProject, find_position_of};

use super::hover;

fn rename(
    tester: &TestProject<'_>,
    new_name: &str,
    position: Position,
) -> Option<lsp_types::WorkspaceEdit> {
    let _ = tester.at(position, |engine, params, _| {
        let params = TextDocumentPositionParams {
            text_document: params.text_document,
            position,
        };
        engine.prepare_rename(params).result.unwrap()
    })?;

    tester.at(position, |engine, params, _| {
        let params = RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: params.text_document,
                position,
            },
            new_name: new_name.to_string(),
            work_done_progress_params: WorkDoneProgressParams::default(),
        };
        engine.rename(params).result.unwrap()
    })
}

fn apply_rename(
    tester: &TestProject<'_>,
    new_name: &str,
    position: Position,
) -> HashMap<String, String> {
    let changes = rename(tester, new_name, position)
        .expect("Rename failed")
        .changes
        .expect("No text edit found");
    apply_code_edit(tester, changes)
}

fn apply_code_edit(
    tester: &TestProject<'_>,
    changes: HashMap<Url, Vec<lsp_types::TextEdit>>,
) -> HashMap<String, String> {
    let mut modules = HashMap::new();
    for (uri, change) in changes {
        let module_name = tester.module_name_from_url(&uri).expect("Valid uri");
        let module_code = tester.src_from_module_url(&uri).expect("Module exists");

        _ = modules.insert(module_name, super::apply_code_edit(module_code, change));
    }

    modules
}

macro_rules! assert_rename {
    ($code:literal, $new_name:literal, $range:expr $(,)?) => {
        assert_rename!(TestProject::for_source($code), $new_name, $range);
    };

    (($module_name:literal, $module_src:literal), $code:literal, $new_name:literal, $range:expr $(,)?) => {
        assert_rename!(
            TestProject::for_source($code).add_module($module_name, $module_src),
            $new_name,
            $range
        );
    };

    ($project:expr, $new_name:literal, $range:expr $(,)?) => {
        let project = $project;
        let src = project.src;
        let range = $range.find_range(src);
        let result = apply_rename(&project, $new_name, range.start);

        let mut output = String::from("----- BEFORE RENAME\n");
        for (name, src) in project.root_package_modules.iter() {
            output.push_str(&format!("-- {name}.gleam\n{src}\n\n"));
        }
        output.push_str(&format!(
            "-- app.gleam\n{}\n\n----- AFTER RENAME\n",
            hover::show_hover(src, range, range.start)
        ));
        for (name, src) in project.root_package_modules.iter() {
            output.push_str(&format!(
                "-- {name}.gleam\n{}\n\n",
                result
                    .get(*name)
                    .map(|string| string.as_str())
                    .unwrap_or(*src)
            ));
        }
        output.push_str(&format!(
            "-- app.gleam\n{}",
            result
                .get("app")
                .map(|string| string.as_str())
                .unwrap_or(src)
        ));

        insta::assert_snapshot!(insta::internals::AutoName, output, src);
    };
}

macro_rules! assert_no_rename {
    ($code:literal, $new_name:literal, $range:expr $(,)?) => {
        let project = TestProject::for_source($code);
        assert_no_rename!(&project, $new_name, $range);
    };

    ($project:expr, $new_name:literal, $range:expr $(,)?) => {
        let src = $project.src;
        let range = $range.find_range(src);
        let result = rename($project, $new_name, range.start);
        assert_eq!(result, None);
    };
}

#[test]
fn rename_local_variable() {
    assert_rename!(
        "
pub fn main() {
  let wibble = 10
  wibble
}
",
        "wobble",
        find_position_of("wibble").nth_occurrence(2).to_selection(),
    );
}

#[test]
fn rename_shadowed_local_variable() {
    assert_rename!(
        "
pub fn main() {
  let wibble = 10
  let wibble = wibble / 2
  wibble
}
",
        "wobble",
        find_position_of("wibble /").to_selection(),
    );
}

#[test]
fn rename_shadowing_local_variable() {
    assert_rename!(
        "
pub fn main() {
  let wibble = 10
  let wibble = wibble / 2
  wibble
}
",
        "wobble",
        find_position_of("wibble").nth_occurrence(4).to_selection(),
    );
}

#[test]
fn rename_local_variable_record_access() {
    assert_rename!(
        "
type Wibble {
  Wibble(wibble: Int)
}

pub fn main() {
  let wibble = Wibble(wibble: 1)
  wibble.wibble
}
",
        "wobble",
        find_position_of("wibble.").to_selection(),
    );
}
#[test]
fn rename_local_variable_guard_clause() {
    assert_rename!(
        "
pub fn main() {
  let wibble = True
  case Nil {
    Nil if wibble -> todo
    _ -> panic
  }
  wibble || False
}
",
        "wobble",
        find_position_of("wibble ||").to_selection(),
    );
}

#[test]
fn rename_local_variable_from_definition() {
    assert_rename!(
        "
pub fn main() {
  let wibble = 10
  let wobble = wibble + 1
  wobble - wibble
}
",
        "some_value",
        find_position_of("wibble =").to_selection()
    );
}

#[test]
fn rename_local_variable_from_definition_nested_pattern() {
    assert_rename!(
        "
pub fn main() {
  let assert Ok([_, wibble, ..]) = Error(12)
  wibble
}
",
        "second_element",
        find_position_of("wibble,").to_selection()
    );
}

#[test]
fn rename_local_variable_assignment_pattern() {
    assert_rename!(
        "
pub fn main() {
  let assert Error(12 as something) = Error(12)
  something
}
",
        "the_error",
        find_position_of("something")
            .nth_occurrence(2)
            .to_selection()
    );
}

#[test]
fn rename_local_variable_from_definition_assignment_pattern() {
    assert_rename!(
        "
pub fn main() {
  let assert Error(12 as something) = Error(12)
  something
}
",
        "the_error",
        find_position_of("something)").to_selection()
    );
}

#[test]
fn rename_local_variable_argument() {
    assert_rename!(
        "
pub fn add(first_number: Int, x: Int) -> Int {
  x + first_number
}
",
        "second_number",
        find_position_of("x +").to_selection()
    );
}

#[test]
fn rename_local_variable_argument_from_definition() {
    assert_rename!(
        "
pub fn wibble(wibble: Float) {
  wibble /. 0.3
}
",
        "wobble",
        find_position_of("wibble:").to_selection()
    );
}

#[test]
fn rename_local_variable_label_shorthand() {
    assert_rename!(
        "
type Wibble {
  Wibble(wibble: Int)
}

pub fn main() {
  let Wibble(wibble:) = todo
  wibble + 1
}
",
        "wobble",
        find_position_of("wibble +").to_selection()
    );
}

#[test]
fn rename_local_variable_label_shorthand_from_definition() {
    assert_rename!(
        "
type Wibble {
  Wibble(wibble: Int)
}

pub fn main() {
  let Wibble(wibble:) = todo
  wibble + 1
}
",
        "wobble",
        find_position_of("wibble:)").to_selection()
    );
}

#[test]
fn rename_local_variable_in_bit_array_pattern() {
    assert_rename!(
        "
pub fn starts_with(bits: BitArray, prefix: BitArray) -> Bool {
  let prefix_size = bit_size(prefix)

  case bits {
    <<pref:bits-size(prefix_size), _:bits>> if pref == prefix -> True
    _ -> False
  }
}
",
        "size_of_prefix",
        find_position_of("prefix_size =").to_selection()
    );
}

#[test]
fn rename_local_variable_from_bit_array_pattern() {
    assert_rename!(
        "
pub fn starts_with(bits: BitArray, prefix: BitArray) -> Bool {
  let prefix_size = bit_size(prefix)

  case bits {
    <<pref:bits-size(prefix_size), _:bits>> if pref == prefix -> True
    _ -> False
  }
}
",
        "size_of_prefix",
        find_position_of("prefix_size)").to_selection()
    );
}

#[test]
fn no_rename_keyword() {
    assert_no_rename!(
        "
pub fn main() {}
",
        "wibble",
        find_position_of("fn").to_selection(),
    );
}

#[test]
fn no_rename_invalid_name() {
    assert_no_rename!(
        "
pub fn main() {
  let wibble = 10
  wibble
}
",
        "Not_AValid_Name",
        find_position_of("wibble").nth_occurrence(2).to_selection()
    );
}

#[test]
fn rename_function_from_definition() {
    assert_rename!(
        (
            "mod",
            "
import app

fn wibble() {
  app.something()
}
"
        ),
        "
pub fn something() {
  something()
}

fn something_else() {
  something()
}
",
        "some_function",
        find_position_of("something").to_selection()
    );
}

#[test]
fn rename_function_from_reference() {
    assert_rename!(
        (
            "mod",
            "
import app

fn wibble() {
  app.something()
}
"
        ),
        "
pub fn something() {
  something()
}

fn something_else() {
  something()
}
",
        "some_function",
        find_position_of("something")
            .nth_occurrence(2)
            .to_selection()
    );
}

#[test]
fn rename_function_from_qualified_reference() {
    assert_rename!(
        (
            "mod",
            "
pub fn wibble() {
  wibble()
}
"
        ),
        "
import mod

pub fn main() {
  mod.wibble()
}
",
        "some_function",
        find_position_of("wibble").to_selection()
    );
}

#[test]
fn rename_function_from_unqualified_reference() {
    assert_rename!(
        (
            "mod",
            "
pub fn wibble() {
  wibble()
}
"
        ),
        "
import mod.{wibble}

pub fn main() {
  wibble()
  mod.wibble()
}
",
        "some_function",
        find_position_of("wibble(").to_selection()
    );
}

#[test]
fn rename_aliased_function() {
    assert_rename!(
        (
            "mod",
            "
import app.{something as something_else}

fn wibble() {
  something_else()
}
"
        ),
        "
pub fn something() {
  something()
}

fn something_else() {
  something()
}
",
        "some_function",
        find_position_of("something").to_selection()
    );
}

#[test]
fn rename_function_shadowing_module() {
    let src = "
import gleam/list

pub fn list() {
  []
}

pub fn main() {
  list.map(todo, todo)
}
    ";

    assert_rename!(
        TestProject::for_source(src).add_hex_module("gleam/list", "pub fn map(_, _) {}"),
        "empty_list",
        find_position_of("list()").to_selection()
    );
}

#[test]
fn rename_function_shadowed_by_field_access() {
    assert_rename!(
        (
            "mod",
            "
import app

type App {
  App(something: Int)
}

pub fn main() {
  let app = App(10)
  app.something
}
"
        ),
        "
pub fn something() {
  todo
}
",
        "function",
        find_position_of("something").to_selection()
    );
}

#[test]
fn no_rename_function_with_invalid_name() {
    assert_no_rename!(
        "
pub fn main() {
  let wibble = 10
  wibble
}
",
        "Not_AValid_Name",
        find_position_of("main").to_selection()
    );
}

#[test]
fn rename_constant_from_definition() {
    assert_rename!(
        (
            "mod",
            "
import app

fn wibble() {
  app.something
}
"
        ),
        "
pub const something = 10

pub fn main() {
  something + { 4 * something }
}
",
        "ten",
        find_position_of("something").to_selection()
    );
}

#[test]
fn rename_constant_from_reference() {
    assert_rename!(
        (
            "mod",
            "
import app

fn wibble() {
  app.something
}
"
        ),
        "
pub const something = 10

pub fn main() {
  something + { 4 * something }
}
",
        "ten",
        find_position_of("something")
            .nth_occurrence(2)
            .to_selection()
    );
}

#[test]
fn rename_constant_from_qualified_reference() {
    assert_rename!(
        (
            "mod",
            "
pub const something = 10

fn wibble() {
  something
}
"
        ),
        "
import mod

pub fn main() {
  mod.something
}
",
        "ten",
        find_position_of("something").to_selection()
    );
}

#[test]
fn rename_constant_from_unqualified_reference() {
    assert_rename!(
        (
            "mod",
            "
pub const something = 10

fn wibble() {
  something
}
"
        ),
        "
import mod.{something}

pub fn main() {
  something + mod.something
}
",
        "ten",
        find_position_of("something +").to_selection()
    );
}

#[test]
fn rename_aliased_constant() {
    assert_rename!(
        (
            "mod",
            "
import app.{something as some_constant}

fn wibble() {
  some_constant
}
"
        ),
        "
pub const something = 10

pub fn main() {
  something + { 4 * something }
}
",
        "ten",
        find_position_of("something").to_selection()
    );
}

#[test]
fn rename_constant_shadowing_module() {
    let src = "
import gleam/list

const list = []

pub fn main() {
  list.map(todo, todo)
}
    ";

    assert_rename!(
        TestProject::for_source(src).add_hex_module("gleam/list", "pub fn map(_, _) {}"),
        "empty_list",
        find_position_of("list =").to_selection()
    );
}

#[test]
fn rename_constant_shadowed_by_field_access() {
    assert_rename!(
        (
            "mod",
            "
import app

type App {
  App(something: Int)
}

pub fn main() {
  let app = App(10)
  app.something
}
"
        ),
        "
pub const something = 10
",
        "constant",
        find_position_of("something").to_selection()
    );
}

#[test]
fn no_rename_constant_with_invalid_name() {
    assert_no_rename!(
        "
const value = 10
",
        "Ten",
        find_position_of("value").to_selection()
    );
}

#[test]
fn rename_type_variant_from_definition() {
    assert_rename!(
        (
            "mod",
            "
import app

fn wibble() {
  app.Constructor(4)
}
"
        ),
        "
pub type Wibble {
  Constructor(Int)
}

pub fn main() {
  Constructor(10)
}
",
        "Wibble",
        find_position_of("Constructor(Int").to_selection()
    );
}

#[test]
fn rename_type_variant_from_reference() {
    assert_rename!(
        (
            "mod",
            "
import app

fn wibble() {
  app.Constructor(4)
}
"
        ),
        "
pub type Wibble {
  Constructor(Int)
}

pub fn main() {
  Constructor(10)
}
",
        "Wibble",
        find_position_of("Constructor(10").to_selection()
    );
}

#[test]
fn rename_type_variant_from_qualified_reference() {
    assert_rename!(
        (
            "mod",
            "
pub type Wibble {
  Constructor(Int)
}

fn wibble() {
  Constructor(42)
}
"
        ),
        "
import mod

pub fn main() {
  mod.Constructor
}
",
        "Variant",
        find_position_of("Constructor").to_selection()
    );
}

#[test]
fn rename_type_variant_from_unqualified_reference() {
    assert_rename!(
        (
            "mod",
            "
pub type Wibble {
  Constructor(Int)
}

fn wibble() {
  Constructor(81)
}
"
        ),
        "
import mod.{Constructor}

pub fn main() {
  #(Constructor(75), mod.Constructor(57))
}
",
        "Number",
        find_position_of("Constructor(75").to_selection()
    );
}

#[test]
fn rename_aliased_type_variant() {
    assert_rename!(
        (
            "mod",
            "
import app.{Constructor as ValueConstructor}

fn wibble() {
  ValueConstructor(172)
}
"
        ),
        "
pub type Wibble {
  Constructor(Int)
}

pub fn main() {
  Constructor(42)
}
",
        "MakeAWibble",
        find_position_of("Constructor").to_selection()
    );
}

#[test]
fn no_rename_type_variant_with_invalid_name() {
    assert_no_rename!(
        "
pub type Wibble {
  Constructor(Int)
}
",
        "name_in_snake_case",
        find_position_of("Constructor").to_selection()
    );
}

#[test]
fn rename_custom_type_variant_pattern() {
    assert_rename!(
        "
pub type Type {
  X
  Y
}

pub fn main(t) {
  case t {
    X -> 0
    Y -> 0
  }
}
",
        "Renamed",
        find_position_of("X").to_selection()
    );
}

#[test]
fn rename_imported_custom_type_variant_pattern() {
    assert_rename!(
        (
            "other",
            "
import app

pub fn main(t) {
  case t {
    app.X -> 0
    app.Y -> 0
  }
}
"
        ),
        "
pub type Type {
  X
  Y
}
",
        "Renamed",
        find_position_of("X").to_selection()
    );
}

#[test]
fn rename_imported_unqualified_custom_type_variant_pattern() {
    assert_rename!(
        (
            "other",
            "
import app.{X, Y}

pub fn main(t) {
  case t {
    X -> 0
    Y -> 0
  }
}
"
        ),
        "
pub type Type {
  X
  Y
}
",
        "Renamed",
        find_position_of("X").to_selection()
    );
}
