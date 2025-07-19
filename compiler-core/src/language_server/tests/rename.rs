use std::collections::HashMap;

use lsp_types::{
    Position, Range, RenameParams, TextDocumentPositionParams, Url, WorkDoneProgressParams,
};

use crate::language_server::tests::{TestProject, find_position_of};

use super::hover;

fn rename(
    tester: &TestProject<'_>,
    new_name: &str,
    position: Position,
) -> Option<(Range, lsp_types::WorkspaceEdit)> {
    let prepare_rename_response = tester.at(position, |engine, params, _| {
        let params = TextDocumentPositionParams {
            text_document: params.text_document,
            position,
        };
        engine.prepare_rename(params).result.unwrap()
    })?;

    let lsp_types::PrepareRenameResponse::Range(range) = prepare_rename_response else {
        return None;
    };

    let edit = tester.at(position, |engine, params, _| {
        let params = RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: params.text_document,
                position,
            },
            new_name: new_name.to_string(),
            work_done_progress_params: WorkDoneProgressParams::default(),
        };
        engine.rename(params).result.unwrap()
    })?;

    Some((range, edit))
}

fn apply_rename(
    tester: &TestProject<'_>,
    new_name: &str,
    position: Position,
) -> (Range, HashMap<String, String>) {
    let (range, edit) = rename(tester, new_name, position).expect("Rename failed");
    let changes = edit.changes.expect("No text edit found");
    (range, apply_code_edit(tester, changes))
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
    ($code:literal, $new_name:literal, $position:expr $(,)?) => {
        assert_rename!(TestProject::for_source($code), $new_name, $position);
    };

    (($module_name:literal, $module_src:literal), $code:expr, $new_name:literal, $position:expr $(,)?) => {
        assert_rename!(
            TestProject::for_source($code).add_module($module_name, $module_src),
            $new_name,
            $position
        );
    };

    ($project:expr, $new_name:literal, $position:expr $(,)?) => {
        let project = $project;
        let src = project.src;
        let position = $position.find_position(src);
        let (range, result) = apply_rename(&project, $new_name, position);

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
    ($code:literal, $new_name:literal, $position:expr $(,)?) => {
        let project = TestProject::for_source($code);
        assert_no_rename!(&project, $new_name, $position);
    };

    ($project:expr, $new_name:literal, $position:expr $(,)?) => {
        let src = $project.src;
        let position = $position.find_position(src);
        let result = rename($project, $new_name, position);
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
        find_position_of("wibble").nth_occurrence(2),
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
        find_position_of("wibble /"),
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
        find_position_of("wibble").nth_occurrence(4),
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
        find_position_of("wibble."),
    );
}

#[test]
fn rename_local_variable_record_access_with_same_name_as_imported_module() {
    assert_rename!(
        &TestProject::for_source(
"import mod
type Record {
  Rec(name: String, available: Bool)
}

pub fn main() {
  let mod = Record(\"test\", false)
  echo mod.available
}"
        ).add_module("mod", "pub const c = 5"),
        "rec",
        find_position_of("mod.available")
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
        find_position_of("wibble ||"),
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
        find_position_of("wibble =")
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
        find_position_of("wibble,")
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
        find_position_of("something").nth_occurrence(2)
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
        find_position_of("something)")
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
        find_position_of("x +")
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
        find_position_of("wibble:")
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
        find_position_of("wibble +")
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
        find_position_of("wibble:)")
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
        find_position_of("prefix_size =")
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
        find_position_of("prefix_size)")
    );
}

#[test]
fn no_rename_keyword() {
    assert_no_rename!(
        "
pub fn main() {}
",
        "wibble",
        find_position_of("fn"),
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
        find_position_of("wibble").nth_occurrence(2)
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
        find_position_of("something")
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
        find_position_of("something").nth_occurrence(2)
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
        find_position_of("wibble")
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
        find_position_of("wibble(")
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
        find_position_of("something")
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
        find_position_of("list()")
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
        find_position_of("something")
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
        find_position_of("main")
    );
}

#[test]
fn no_rename_function_from_other_package() {
    let src = "
import wibble

pub fn main() {
  wibble.wobble()
}
";

    assert_no_rename!(
        &TestProject::for_source(src).add_hex_module("wibble", "pub fn wobble() { todo }"),
        "something",
        find_position_of("wobble")
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
        find_position_of("something")
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
        find_position_of("something").nth_occurrence(2)
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
        find_position_of("something")
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
        find_position_of("something +")
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
        find_position_of("something")
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
        find_position_of("list =")
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
        find_position_of("something")
    );
}

#[test]
fn no_rename_constant_with_invalid_name() {
    assert_no_rename!(
        "
const value = 10
",
        "Ten",
        find_position_of("value")
    );
}

#[test]
fn no_rename_constant_from_other_package() {
    let src = "
import wibble

pub fn main() {
  wibble.wobble
}
";

    assert_no_rename!(
        &TestProject::for_source(src).add_hex_module("wibble", "pub const wobble = 2"),
        "something",
        find_position_of("wobble")
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
        find_position_of("Constructor(Int")
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
        find_position_of("Constructor(10")
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
        find_position_of("Constructor")
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
        find_position_of("Constructor(75")
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
        find_position_of("Constructor")
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
        find_position_of("Constructor")
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
        find_position_of("X")
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
        find_position_of("X")
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
        find_position_of("X")
    );
}

#[test]
fn rename_type_variant_pattern_with_arguments() {
    assert_rename!(
        "
pub type Wibble {
  Wibble(Int)
  Wobble(Float)
}

fn wibble() {
  case Wibble(10) {
    Wibble(20) -> todo
    Wibble(_) -> panic
  }
}
",
        "Variant",
        find_position_of("Wibble(10)")
    );
}

#[test]
fn rename_type_variant_from_pattern() {
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
        find_position_of("X ->")
    );
}

#[test]
fn no_rename_type_variant_from_other_package() {
    let src = "
import wibble

pub fn main() {
  wibble.Wibble(10)
}
";

    assert_no_rename!(
        &TestProject::for_source(src).add_hex_module("wibble", "pub type Wibble { Wibble(Int) }"),
        "Constructor",
        find_position_of("Wibble")
    );
}

#[test]
fn rename_value_in_nested_module() {
    assert_rename!(
        (
            "sub/mod",
            "
pub fn wibble() {
  wibble()
}
"
        ),
        "
import sub/mod

pub fn main() {
  mod.wibble()
}
",
        "some_function",
        find_position_of("wibble")
    );
}

#[test]
fn rename_value_in_aliased_module() {
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
import mod as the_module

pub fn main() {
  the_module.wibble()
}
",
        "some_function",
        find_position_of("wibble")
    );
}

#[test]
fn rename_aliased_value() {
    assert_rename!(
        (
            "mod",
            "
import app.{Wibble as Wobble}

fn wobble() {
  Wobble
}
"
        ),
        "
pub type Wibble { Wibble }

pub fn main() {
  Wibble
}
",
        "Wubble",
        find_position_of("Wibble }")
    );
}

#[test]
fn rename_type_from_definition() {
    assert_rename!(
        (
            "mod",
            "
import app

fn wibble() -> app.Wibble { todo }
"
        ),
        "
pub type Wibble { Constructor }

pub fn main(w: Wibble) -> Wibble { todo }
",
        "SomeType",
        find_position_of("Wibble")
    );
}

#[test]
fn rename_type_from_reference() {
    assert_rename!(
        (
            "mod",
            "
import app

fn wibble() -> app.Wibble { todo }
"
        ),
        "
pub type Wibble { Constructor }

pub fn main(w: Wibble) -> Wibble { todo }
",
        "SomeType",
        find_position_of("Wibble").nth_occurrence(2)
    );
}

#[test]
fn rename_type_from_qualified_reference() {
    assert_rename!(
        (
            "mod",
            "
pub type Wibble { Constructor }

fn wibble(w: Wibble) -> Wibble { todo }
"
        ),
        "
import mod

pub fn main(w: mod.Wibble) -> mod.Wibble { todo }
",
        "SomeType",
        find_position_of("Wibble")
    );
}

#[test]
fn rename_type_from_unqualified_reference() {
    assert_rename!(
        (
            "mod",
            "
pub type Wibble { Constructor }

fn wibble(w: Wibble) -> Wibble { todo }
"
        ),
        "
import mod.{type Wibble}

pub fn main(w: Wibble) -> mod.Wibble { todo }
",
        "SomeType",
        find_position_of("Wibble)")
    );
}

#[test]
fn rename_aliased_type() {
    assert_rename!(
        (
            "mod",
            "
import app.{type Wibble as Wobble}

fn wibble() -> Wobble { todo }
"
        ),
        "
pub type Wibble { Constructor }

pub fn main(w: Wibble) -> Wibble { todo }
",
        "SomeType",
        find_position_of("Wibble")
    );
}

#[test]
fn no_rename_type_with_invalid_name() {
    assert_no_rename!(
        "
type Wibble { Wobble }
",
        "a_type_name",
        find_position_of("Wibble")
    );
}

#[test]
fn no_rename_type_from_other_package() {
    let src = "
import wibble

pub fn main() -> wibble.Wibble { todo }
";

    assert_no_rename!(
        &TestProject::for_source(src).add_hex_module("wibble", "pub type Wibble { Wibble }"),
        "SomeType",
        find_position_of("Wibble")
    );
}

// https://github.com/gleam-lang/gleam/issues/4372
#[test]
fn rename_type_referenced_in_variant_constructor_argument() {
    assert_rename!(
        (
            "mod",
            "
import app

pub type Wobble {
  Wobble(w: app.Wibble)
}
"
        ),
        "
pub type Wibble {
  Wibble
}

pub fn main() {
  let wibble = Wibble
}
",
        "SomeType",
        find_position_of("Wibble")
    );
}

// https://github.com/gleam-lang/gleam/issues/4372
#[test]
fn rename_type_from_variant_constructor_argument() {
    assert_rename!(
        (
            "mod",
            "
pub type Wibble {
  Wibble
}

pub fn main() {
  let wibble = Wibble
}
"
        ),
        "
import mod

pub type Wobble {
  Wobble(w: mod.Wibble)
}
",
        "SomeType",
        find_position_of("Wibble")
    );
}

// https://github.com/gleam-lang/gleam/issues/4553
#[test]
fn rename_local_variable_with_label_shorthand() {
    assert_rename!(
        "
pub type Wibble {
  Wibble(first: Int, second: Int)
}

pub fn main() {
  let second = 2
  Wibble(first: 1, second:)
}
",
        "something",
        find_position_of("second =")
    );
}

const MOCK_MODULE: &'static str =
"pub fn fn1() { Nil }
pub const const1 = 5
pub const const2 = <<0:8>>
pub const const3 = \"Hello\"
pub type Type { Variant1 Variant2(Int) }
pub type GenericType(inner) { Node(inner, GenericType(inner)) Leaf }";

const TEST_MODULE: &'static str =
"import mod

fn func(
  arg: mod.Type,
  arg2: mod.GenericType(mod.Type),
  arg3: #(mod.GenericType(mod.Type), mod.Type),
  arg4: List(mod.Type),
  arg5: fn(mod.Type) -> mod.Type
) {
  mod.fn1()
  arg5(mod.Variant1)

  let _: mod.Type = mod.Variant1
  let _: mod.GenericType(mod.Type) = mod.Node(mod.Variant1, mod.Leaf)
  let _ = mod.const1
  let _ = mod.fn1

  let _: #(mod.Type) = #(mod.Variant1)
  let _: #(mod.GenericType(mod.Type)) = #(mod.Node(mod.Variant1, mod.Leaf))
  let _ = #(mod.const1)
  let _ = #(mod.fn1())
  let _ = #(mod.fn1)

  let _: List(mod.Type) = [mod.Variant1]
  let _: List(mod.GenericType(mod.Type)) = [mod.Node(mod.Variant1, mod.Leaf)]
  let _ = [mod.const1]
  let _ = [mod.fn1()]
  let _ = [mod.fn1]

  let _: fn(mod.Type) -> mod.Type = fn(arg: mod.Type) -> mod.Type { arg }
  let _: fn(mod.GenericType(mod.Type)) -> mod.GenericType(mod.Type) = fn(arg: mod.GenericType(mod.Type)) -> mod.GenericType(mod.Type) { arg }

  let _ = <<mod.const2:bits, mod.const2:bits>>

  let _ = mod.const3 <> mod.const3

  case arg {
    mod.Variant1 -> todo
    mod.Variant2(3) -> todo
    mod.Variant2(_) -> todo
    mod.Variant2(..) -> todo
  }
  case arg2 {
    mod.Node(_, mod.Node(_, _)) -> todo
    mod.Node(_, _) -> todo
    mod.Leaf -> todo
  }
  case arg3 {
    #(mod.Node(_, mod.Leaf), mod.Variant1) -> todo
    _ -> todo
  }
  case arg4 {
    [mod.Variant1, ..] -> todo
    _ if mod.const1 == mod.const1 -> todo
    _ -> todo
  }
  let mod = Record(false)
  echo mod.bool
}

const c1: mod.Type = mod.Variant1
const c2: mod.GenericType(mod.Type) = mod.Node(mod.Variant1, mod.Leaf)
const c3 = mod.const1
const c4 = mod.fn1
const c5: #(mod.Type, Int, fn() -> Nil) = #(mod.Variant1, mod.const1, mod.fn1)
const c6: List(mod.Type) = [mod.Variant1]
const c7 = [mod.const1]
const c8 = [mod.fn1]
const c9 = <<mod.const2:bits>>
const c10 = mod.const3 <> mod.const3
type Type1 { Var(mod.Type) }
type Type2 = mod.GenericType(mod.Type)
type Record { Record (bool: Bool) }";

#[test]
fn rename_module_from_import() {
    assert_rename!(
        TestProject::for_source(TEST_MODULE).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("import mod")
    );
}

#[test]
fn rename_module_from_import_spaced() {
    let src =
"import         mod
const c = mod.Variant1";
    assert_rename!(
        TestProject::for_source(src).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod")
    );
}

#[test]
fn rename_module_from_import_with_alias() {
    let test_project = TestProject::for_source(TEST_MODULE).add_module("mod", MOCK_MODULE);
    let (_, result) = apply_rename(&test_project, "module", find_position_of("mod").find_position(test_project.src));
    assert_rename!(
        TestProject::for_source(result.get("app").unwrap()).add_module("mod", MOCK_MODULE),
        "alias",
        find_position_of("import mod as module")
    );
}

#[test]
fn rename_module_from_import_spaced_with_alias() {
    let src =
"import mod     as      module
const c = module.Variant1";
    assert_rename!(
        TestProject::for_source(src).add_module("mod", MOCK_MODULE),
        "module_alias",
        find_position_of("module")
    );
}

#[test]
fn rename_module_from_import_with_unqualified_member() {
    let src =
"import mod.{type Type}
const c1 = mod.Variant1
type Alias = Type";
    let test_project = TestProject::for_source(src).add_module("mod", MOCK_MODULE);
    assert_rename!(
        test_project,
        "module",
        find_position_of("import mod"),
    );
}

#[test]
fn rename_module_from_import_spaced_with_alias_and_unqualified_member() {
    let src=
"import     mod.{ type Type  }   as      module
const c = module.Variant1";
    let test_project = TestProject::for_source(src).add_module("mod", MOCK_MODULE);
    assert_rename!(
        test_project,
        "module_alias",
        find_position_of("module"),
    );
}

#[test]
fn rename_module_from_import_to_orig() {
    assert_rename!(
        TestProject::for_source(
            "import mod as module\nconst c = module.Variant1"
        ).add_module("mod", MOCK_MODULE),
        "mod",
        find_position_of("import mod as module")
    );
}

#[test]
fn rename_module_from_import_nested() {
    assert_rename!(
        TestProject::for_source(
            "import testing/mod\nconst c = mod.Variant1"
        ).add_module("testing/mod", MOCK_MODULE),
        "module",
        find_position_of("import testing/mod")
    );
}

#[test]
fn rename_module_from_import_nested_spaced_with_unqualified_member() {
    let src =
"import         testing/commons/mod.{ type Type     }
const c = mod.Variant1";
    assert_rename!(
        TestProject::for_source(src).add_module("testing/commons/mod", MOCK_MODULE),
        "module",
        find_position_of("import"),
    );
}

#[test]
fn rename_module_from_import_nested_spaced_with_alias_and_unqualified_member() {
    let src=
"import     testing/common/mod.{ type Type  }   as      module
const c = module.Variant1";
    assert_rename!(
        TestProject::for_source(src).add_module("testing/common/mod", MOCK_MODULE),
        "module_alias",
        find_position_of("module"),
    );
}

#[test]
fn rename_module_from_function_arg_simple_constructor() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func(arg: mod.Type) {todo}")
        .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Type")
    );
}

#[test]
fn rename_module_from_function_arg_recursive_constructor() {
    let test_project = TestProject::for_source(
        "import mod\nfn func(arg: mod.GenericType(mod.Type)) {todo}"
    ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.GenericType")
    );
    assert_rename!(
        test_project,
        "module",
        find_position_of("mod.Type")
    );
}

#[test]
fn rename_module_from_function_arg_list() {
    let test_project = TestProject::for_source(
        "import mod\nfn func(arg: List(mod.GenericType(mod.Type))) {todo}"
    ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.GenericType")
    );
    assert_rename!(
        test_project,
        "module",
        find_position_of("mod.Type")
    );
}

#[test]
fn rename_module_from_function_arg_tuple() {
    let test_project = TestProject::for_source(
        "import mod\nfn func(arg: #(mod.GenericType(mod.Type), mod.Type)) {todo}"
    ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.GenericType")
    );
    assert_rename!(
        test_project,
        "module",
        find_position_of("mod.Type").nth_occurrence(2)
    );
}

#[test]
fn rename_module_from_function_arg_fn_arg() {
    let test_project = TestProject::for_source(
        "import mod\nfn func(arg: fn(mod.GenericType(mod.Type)) -> mod.GenericType(mod.Type)) { todo }"
    ).add_module("mod", MOCK_MODULE);

    for i in 1..3 {
        assert_rename!(
            &test_project,
            "module",
            find_position_of("mod.GenericType").nth_occurrence(i)
        );
        assert_rename!(
            &test_project,
            "module",
            find_position_of("mod.Type").nth_occurrence(i)
        );
    }
}

#[test]
fn rename_module_from_function_call() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {mod.fn1()}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.fn1()"),
    );
}

#[test]
fn rename_module_from_function_call_arg() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func(arg: fn(mod.Type) -> a) {arg(mod.Variant1)}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Variant1"),
    );
}

#[test]
fn rename_module_from_local_var_annotation_simple() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _: mod.Type = mod.Variant1}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Type"),
    );
}

#[test]
fn rename_module_from_expr_simple_constructor() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _: mod.Type = mod.Variant1}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Variant1"),
    );
}

#[test]
fn rename_module_from_local_var_annotation_recursive() {
    let test_project = TestProject::for_source(
        "import mod\nfn func() {let _: mod.GenericType(mod.Type) = mod.Node(mod.Variant1, mod.Leaf)}"
    ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.GenericType"),
    );
    assert_rename!(
        test_project,
        "module",
        find_position_of("mod.Type"),
    );
}

#[test]
fn rename_module_from_expr_recursive_constructor() {
    let test_project = TestProject::for_source(
        "import mod\nfn func() {let _: mod.GenericType(mod.Type) = mod.Node(mod.Variant1, mod.Leaf)}"
    ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Node"),
    );
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Variant1"),
    );
    assert_rename!(
        test_project,
        "module",
        find_position_of("mod.Leaf"),
    );
}

#[test]
fn rename_module_from_expr_const() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _ = mod.const1}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.const1"),
    );
}

#[test]
fn rename_module_from_expr_fn() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _ = mod.fn1}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.fn1"),
    );
}

#[test]
fn rename_module_from_local_var_annotation_tuple_simple() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _: #(mod.Type) = #(mod.Variant1)}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Type"),
    );
}

#[test]
fn rename_module_from_expr_tuple_simple_constructor() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _: #(mod.Type) = #(mod.Variant1)}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Variant1"),
    );
}

#[test]
fn rename_module_from_local_var_annotation_tuple_recursive() {
    let test_project = TestProject::for_source(
        "import mod\nfn func() {let _: #(mod.GenericType(mod.Type)) = #(mod.Node(mod.Variant1, mod.Leaf))}"
    ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.GenericType"),
    );
    assert_rename!(
        test_project,
        "module",
        find_position_of("mod.Type"),
    );
}

#[test]
fn rename_module_from_expr_tuple_recursive_constructor() {
    let test_project = TestProject::for_source(
        "import mod\nfn func() {let _: #(mod.GenericType(mod.Type)) = #(mod.Node(mod.Variant1, mod.Leaf))}"
    ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Node"),
    );
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Variant1"),
    );
    assert_rename!(
        test_project,
        "module",
        find_position_of("mod.Leaf"),
    );
}

#[test]
fn rename_module_from_expr_tuple_const() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _ = #(mod.const1)}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.const1"),
    );
}

#[test]
fn rename_module_from_expr_tuple_fn_call() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _ = #(mod.fn1())}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.fn1()"),
    );
}

#[test]
fn rename_module_from_expr_tuple_fn() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _ = #(mod.fn1)}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.fn1"),
    );
}

#[test]
fn rename_module_from_local_var_annotation_list_simple() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _: List(mod.Type) = [mod.Variant1]}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Type"),
    );
}

#[test]
fn rename_module_from_expr_list_simple_constructor() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _: List(mod.Type) = [mod.Variant1]}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Variant1"),
    );
}

#[test]
fn rename_module_from_local_var_annotation_list_recursive() {
    let test_project = TestProject::for_source(
        "import mod\nfn func() { let _: List(mod.GenericType(mod.Type)) = [mod.Node(mod.Variant1, mod.Leaf)] }"
    ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.GenericType"),
    );
    assert_rename!(
        test_project,
        "module",
        find_position_of("mod.Type"),
    );
}

#[test]
fn rename_module_from_expr_list_recursive_constructor() {
    let test_project = TestProject::for_source(
        "import mod\nfn func() { let _: List(mod.GenericType(mod.Type)) = [mod.Node(mod.Variant1, mod.Leaf)] }"
    ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Node"),
    );
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Variant1"),
    );
    assert_rename!(
        test_project,
        "module",
        find_position_of("mod.Leaf"),
    );
}

#[test]
fn rename_module_from_expr_list_const() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _ = [mod.const1]}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.const1"),
    );
}

#[test]
fn rename_module_from_expr_list_fn_call() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _ = [mod.fn1()]}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.fn1()"),
    );
}

#[test]
fn rename_module_from_expr_list_fn() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _ = [mod.fn1]}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.fn1"),
    );
}

#[test]
fn rename_module_from_local_var_annotation_fn_simple() {
    let test_project = TestProject::for_source(
        "import mod\nfn func() { let _: fn(mod.Type) -> mod.Type = fn(arg: mod.Type) -> mod.Type { arg } }"
    ).add_module("mod", MOCK_MODULE);

    for i in 1..5 {
        assert_rename!(
            &test_project,
            "module",
            find_position_of("mod.Type").nth_occurrence(i),
        );
    }
}

#[test]
fn rename_module_from_local_var_annotation_fn_recursive() {
    let test_project = TestProject::for_source(
        "import mod\nfn func() { let _: fn(mod.GenericType(mod.Type)) -> mod.GenericType(mod.Type) = fn(arg: mod.GenericType(mod.Type)) -> mod.GenericType(mod.Type) { arg } }"
    ).add_module("mod", MOCK_MODULE);

    for i in 1..3 {
        assert_rename!(
            &test_project,
            "module",
            find_position_of("mod.GenericType").nth_occurrence(i),
        );
        assert_rename!(
            &test_project,
            "module",
            find_position_of("mod.Type").nth_occurrence(i),
        );
    }
}

#[test]
fn rename_module_from_expr_fn_recursive_constructor() {
    let test_project = TestProject::for_source(
        "import mod\nfn func() { let _: fn(mod.GenericType(mod.Type)) -> mod.GenericType(mod.Type) = fn(arg: mod.GenericType(mod.Type)) -> mod.GenericType(mod.Type) { arg } }"
    ).add_module("mod", MOCK_MODULE);

    for i in 3..5 {
        assert_rename!(
            &test_project,
            "module",
            find_position_of("mod.GenericType").nth_occurrence(i),
        );
        assert_rename!(
            &test_project,
            "module",
            find_position_of("mod.Type").nth_occurrence(i),
        );
    }
}

#[test]
fn rename_module_from_expr_bit_array() {
    assert_rename!(
        TestProject::for_source("import mod\nfn func() {let _ = <<mod.const2:bits>>}")
            .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.const2"),
    );
}

#[test]
fn rename_module_from_expr_string_concat() {
    let test_project = TestProject::for_source("import mod\nfn func() {let _ = mod.const3 <> mod.const3}")
            .add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.const3"),
    );
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.const3").nth_occurrence(2),
    );
}

#[test]
fn rename_module_from_pattern_simple_constructor() {
    let test_project = TestProject::for_source(
"import mod
fn func(arg: mod.Type) {
  case arg {
    mod.Variant1 -> todo
    mod.Variant2(3) -> todo
    mod.Variant2(_) -> todo
    mod.Variant2(..) -> todo
  }
}"
    ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Variant1"),
    );
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Variant2(3)"),
    );
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Variant2(_)"),
    );
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Variant2(..)"),
    );
}

#[test]
fn rename_module_from_pattern_recursive_constructor() {
    let test_project= TestProject::for_source(
"import mod
fn func(arg: mod.GenericType(mod.Type)) {
  case arg {
    mod.Node(mod.Variant1, mod.Node(_, _)) -> todo
    mod.Node(_, _) -> todo
    mod.Leaf -> todo
  }
}"
    ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Node"),
    );
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Node(_, _)"),
    );
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Variant1"),
    );
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Leaf"),
    );
}

#[test]
fn rename_module_from_pattern_tuple() {
    let test_project= TestProject::for_source(
"import mod
fn func(arg: #(mod.GenericType(mod.Type), mod.Type)) {
  case arg {
    #(mod.Node(_, mod.Leaf), mod.Variant1) -> todo
    _ -> todo
  }
}"
    ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Node"),
    );
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Leaf"),
    );
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Variant1"),
    );
}

#[test]
fn rename_module_from_pattern_list() {
    assert_rename!(
        TestProject::for_source(
"import mod
fn func(arg: List(mod.Type)) {
  case arg {
    [mod.Variant1, ..] -> todo
    _ -> todo
  }
}"
        ).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Variant1"),
    );
}

#[test]
fn rename_module_from_const_annotation_simple() {
    assert_rename!(
        TestProject::for_source("import mod\nconst c: mod.Type = mod.Variant1")
        .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Type")
    );
}

#[test]
fn rename_module_from_const_simple_constructor() {
    assert_rename!(
        TestProject::for_source("import mod\nconst c: mod.Type = mod.Variant1")
        .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Variant1")
    );
}

#[test]
fn rename_module_from_const_annotation_recursive() {
    let test_project = TestProject::for_source(
        "import mod\nconst c: mod.GenericType(mod.Type) = mod.Node(mod.Variant1, mod.Leaf)"
        ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.GenericType")
    );
    assert_rename!(
        test_project,
        "module",
        find_position_of("mod.Type")
    );
}

#[test]
fn rename_module_from_const_recursive_constructor() {
    let test_project = TestProject::for_source(
        "import mod\nconst c: mod.GenericType(mod.Type) = mod.Node(mod.Variant1, mod.Leaf)"
        ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Node")
    );
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.Variant1")
    );
    assert_rename!(
        test_project,
        "module",
        find_position_of("mod.Leaf")
    );
}

#[test]
fn rename_module_from_const_const() {
    assert_rename!(
        TestProject::for_source("import mod\nconst c = mod.const1")
        .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.const1")
    );
}

#[test]
fn rename_module_from_const_fn() {
    assert_rename!(
        TestProject::for_source("import mod\nconst c = mod.fn1")
        .add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.fn1")
    );
}

#[test]
fn rename_module_from_const_annotation_tuple() {
    assert_rename!(
        TestProject::for_source(
            "import mod\nconst c: #(mod.Type) = #(mod.Variant1)"
        ).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Type")
    );
}

#[test]
fn rename_module_from_const_tuple_constructor() {
    assert_rename!(
        TestProject::for_source(
            "import mod\nconst c: #(mod.Type) = #(mod.Variant1)"
        ).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Variant1")
    );
}

#[test]
fn rename_module_from_const_tuple_const() {
    assert_rename!(
        TestProject::for_source(
            "import mod\nconst c = #(mod.const1)"
        ).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.const1")
    );
}

#[test]
fn rename_module_from_const_tuple_fn() {
    assert_rename!(
        TestProject::for_source(
            "import mod\nconst c = #(mod.fn1)"
        ).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.fn1")
    );
}

#[test]
fn rename_module_from_const_annotation_list() {
    assert_rename!(
        TestProject::for_source(
            "import mod\nconst c: List(mod.Type) = [mod.Variant1]"
        ).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Type")
    );
}

#[test]
fn rename_module_from_const_list_constructor() {
    assert_rename!(
        TestProject::for_source(
            "import mod\nconst c: List(mod.Type) = [mod.Variant1]"
        ).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Variant1")
    );
}

#[test]
fn rename_module_from_const_list_const() {
    assert_rename!(
        TestProject::for_source(
            "import mod\nconst c = [mod.const1]"
        ).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.const1")
    );
}

#[test]
fn rename_module_from_const_list_fn() {
    assert_rename!(
        TestProject::for_source(
            "import mod\nconst c = [mod.fn1]"
        ).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.fn1")
    );
}

#[test]
fn rename_module_from_const_bit_array() {
    assert_rename!(
        TestProject::for_source(
            "import mod\nconst c = <<mod.const2:bits>>"
        ).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.const2")
    );
}

#[test]
fn rename_module_from_const_string_concat() {
    assert_rename!(
        TestProject::for_source(
            "import mod\nconst c = mod.const3 <> mod.const3"
        ).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.const3")
    );
}

#[test]
fn rename_module_from_custom_type() {
    assert_rename!(
        TestProject::for_source(
            "import mod\ntype Type { Var(mod.Type) }"
        ).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.Type")
    );
}

#[test]
fn rename_module_from_type_alias() {
    let test_project = TestProject::for_source(
        "import mod\ntype CustomList = mod.GenericType(mod.Type)"
    ).add_module("mod", MOCK_MODULE);
    assert_rename!(
        &test_project,
        "module",
        find_position_of("mod.GenericType")
    );
    assert_rename!(
        test_project,
        "module",
        find_position_of("mod.Type")
    );
}

#[test]
fn rename_module_from_clause_guard() {
    assert_rename!(
        TestProject::for_source(
"import mod
fn func(arg: Int) {
  case arg {
    x if x == mod.const1 -> todo
    _ -> todo
  }
}"
        ).add_module("mod", MOCK_MODULE),
        "module",
        find_position_of("mod.const1"),
    );
}
