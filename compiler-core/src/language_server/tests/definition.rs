use lsp_types::{
    GotoDefinitionParams, Location, Position, Range, Url, request::GotoTypeDefinitionParams,
};

use super::*;

fn definition(tester: &TestProject<'_>, position: Position) -> Option<Location> {
    tester.at(position, |engine, param, _| {
        let params = GotoDefinitionParams {
            text_document_position_params: param,
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        let response = engine.goto_definition(params);
        response.result.unwrap()
    })
}

fn pretty_definition(project: TestProject<'_>, position_finder: PositionFinder) -> String {
    let position = position_finder.find_position(project.src);
    let location = definition(&project, position).expect("a location to jump to");
    jump_locations_to_string(project, position, vec![location])
}

fn type_definition(tester: &TestProject<'_>, position: Position) -> Vec<Location> {
    tester.at(position, |engine, param, _| {
        let params = GotoTypeDefinitionParams {
            text_document_position_params: param,
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        let response = engine.goto_type_definition(params);

        response.result.unwrap()
    })
}

fn pretty_type_definition(project: TestProject<'_>, position_finder: PositionFinder) -> String {
    let position = position_finder.find_position(project.src);
    let location = type_definition(&project, position);
    format!(
        "Jumping to type definition\n\n{}",
        jump_locations_to_string(project, position, location)
    )
}

fn jump_locations_to_string(
    project: TestProject<'_>,
    original_position: Position,
    locations: Vec<Location>,
) -> String {
    let src = hover::show_hover(
        project.src,
        Range {
            start: original_position,
            end: original_position,
        },
        original_position,
    );

    let destinations = locations
        .iter()
        .map(|location| {
            let pretty_destination = location
                .uri
                .path_segments()
                .expect("a location to jump to")
                // To make snapshots the same both on windows and unix systems we need
                // to discard windows' `C:` path segment at the beginning of a uri.
                .skip_while(|segment| *segment == "C:")
                .join("/");

            let destination_code = hover::show_hover(
                project
                    .src_from_module_url(&location.uri)
                    .expect("a module to jump to"),
                location.range,
                location.range.start,
            );

            format!(
                "----- Jumped to `{pretty_destination}`
{destination_code}"
            )
        })
        .join("\n\n");

    format!(
        "----- Jumping from `src/app.gleam`
{src}
{destinations}",
    )
}

#[macro_export]
macro_rules! assert_goto {
    ($src:literal, $position:expr) => {
        let project = TestProject::for_source($src);
        assert_goto!(project, $position);
    };
    ($project:expr, $position:expr) => {
        let output = pretty_definition($project, $position);
        insta::assert_snapshot!(insta::internals::AutoName, output);
    };
}

#[macro_export]
macro_rules! assert_goto_type {
    ($src:literal, $position:expr) => {
        let project = TestProject::for_source($src);
        assert_goto_type!(project, $position);
    };
    ($project:expr, $position:expr) => {
        let output = pretty_type_definition($project, $position);
        insta::assert_snapshot!(insta::internals::AutoName, output);
    };
}

#[test]
fn goto_type_definition_in_same_file() {
    assert_goto_type!(
        "
pub type Wibble {
  Wibble
}

pub fn main() {
  let x = Wibble
  x
}",
        find_position_of("x").nth_occurrence(2)
    );
}

#[test]
fn goto_type_definition_in_different_file_of_same_project() {
    let src = "
import wibble.{type Wibble}

pub fn main() {
  use_wibble(todo)
}

pub fn use_wibble(wibble: Wibble) { todo }
";

    assert_goto_type!(
        TestProject::for_source(src).add_module("wibble", "pub type Wibble"),
        find_position_of("todo")
    );
}

#[test]
fn goto_type_definition_in_different_file_of_dependency() {
    let src = "
import wibble.{type Wibble}

pub fn main() {
  use_wibble(todo)
}

pub fn use_wibble(wibble: Wibble) { todo }
";

    assert_goto_type!(
        TestProject::for_source(src).add_dep_module("wibble", "pub type Wibble"),
        find_position_of("todo")
    );
}

#[test]
fn goto_type_definition_can_jump_to_multiple_types() {
    let src = "
import wibble.{type Wibble, Wibble}
import box.{Box}

pub fn main() {
  let a = Box(Wibble)
}
";

    assert_goto_type!(
        TestProject::for_source(src)
            .add_dep_module("wibble", "pub type Wibble { Wibble }")
            .add_dep_module("box", "pub type Box(a) { Box(a) }"),
        find_position_of("let a")
    );
}

#[test]
fn goto_type_definition_can_jump_to_all_types_in_a_tuple() {
    let src = "
import wibble.{type Wibble}
import wobble.{type Wobble}
import box.{type Box}

pub fn main() {
  let a: #(Box(Wibble), Wobble) = todo
}
";

    assert_goto_type!(
        TestProject::for_source(src)
            .add_dep_module("wibble", "pub type Wibble { Wibble }")
            .add_dep_module("wobble", "pub type Wobble { Wobble }")
            .add_dep_module("box", "pub type Box(a) { Box(a) }"),
        find_position_of("let a")
    );
}

#[test]
fn goto_type_definition_can_jump_to_all_types_in_a_function_type() {
    let src = "
import wibble.{type Wibble}
import wobble.{type Wobble}
import box.{type Box}

pub fn main() {
  let a = fn(wibble: Wibble) { box.Box(wobble.Wobble) }
}
";

    assert_goto_type!(
        TestProject::for_source(src)
            .add_dep_module("wibble", "pub type Wibble { Wibble }")
            .add_dep_module("wobble", "pub type Wobble { Wobble }")
            .add_dep_module("box", "pub type Box(a) { Box(a) }"),
        find_position_of("let a")
    );
}

#[test]
fn goto_definition_local_variable() {
    assert_goto!(
        "
pub fn main() {
  let x = 1
  x
}",
        find_position_of("x").nth_occurrence(2)
    );
}

#[test]
fn goto_definition_record_update() {
    assert_goto!(
        "
pub type Wibble { Wibble(one: Int, two: Int) }

pub fn main() {
  Wibble(..todo, one: 1)
}",
        find_position_of("Wibble").nth_occurrence(3)
    );
}

#[test]
fn goto_definition_same_module_constants() {
    assert_goto!(
        "
const x = 1

pub fn main() {
  x
}",
        find_position_of("x").nth_occurrence(2)
    );
}

#[test]
fn goto_definition_same_module_functions() {
    assert_goto!(
        "
fn add_2(x) {
  x + 2
}

pub fn main() {
  add_2(1)
}",
        find_position_of("add_2(1)")
    );
}

#[test]
fn goto_definition_same_module_records() {
    assert_goto!(
        "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}

pub fn main() {
  let a = Var1(1)
  let b = Var2(2, 3)
}",
        find_position_of("Var1(1)")
    );
}

#[test]
fn goto_definition_imported_module_constants() {
    let code = "
import example_module
fn main() {
  example_module.my_num
}
";

    assert_goto!(
        TestProject::for_source(code).add_module("example_module", "pub const my_num = 1"),
        find_position_of("my_num")
    );
}

#[test]
fn goto_definition_unqualified_imported_module_constants() {
    let code = "
import example_module.{my_num}
fn main() {
  my_num
}
";

    assert_goto!(
        TestProject::for_source(code).add_module("example_module", "pub const my_num = 1"),
        find_position_of("my_num").nth_occurrence(2)
    );
}

#[test]
fn goto_definition_module_function_calls() {
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    assert_goto!(
        TestProject::for_source(code).add_module("example_module", "pub fn my_fn() { Nil }"),
        find_position_of("my_fn")
    );
}

#[test]
fn goto_definition_imported_module_records() {
    let dep_src = "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}";

    let code = "
import example_module
fn main() {
  example_module.Var1(1)
}
";

    assert_goto!(
        TestProject::for_source(code).add_module("example_module", dep_src),
        find_position_of("Var1(1)")
    );
}

#[test]
fn goto_definition_unqualified_imported_module_records() {
    let dep_src = "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}";

    let code = "
import example_module.{Var1}
fn main() {
  Var1(1)
}
";

    assert_goto!(
        TestProject::for_source(code).add_module("example_module", dep_src),
        find_position_of("Var1(1)").under_char('a')
    );
}

#[test]
fn goto_definition_external_module_constants() {
    let code = "
import example_module
fn main() {
  example_module.my_num
}
";

    assert_goto!(
        TestProject::for_source(code).add_hex_module("example_module", "pub const my_num = 1"),
        find_position_of("my_num").under_char('u')
    );
}

#[test]
fn goto_definition_external_module_function_calls() {
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    assert_goto!(
        TestProject::for_source(code).add_hex_module("example_module", "pub fn my_fn() { Nil }"),
        find_position_of("my_fn")
    );
}

#[test]
fn goto_definition_external_module_function_calls_with_multiple_compiles() {
    let dep = "pub fn my_fn() { Nil }";
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    let (mut engine, position_param) = TestProject::for_source(code)
        .add_hex_module("example_module", dep)
        .positioned_with_io(Position::new(3, 20));

    let params = GotoDefinitionParams {
        text_document_position_params: position_param.clone(),
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let response = engine.goto_definition(params.clone());
    let response = response.result.unwrap();

    assert_eq!(
        response,
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\build\packages\hex\src\example_module.gleam"
            } else {
                "/build/packages/hex/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 14
                }
            }
        })
    );

    engine.compiler.sources.clear();
    let response = engine.compile_please();
    assert!(response.result.is_ok());

    let response = engine.goto_definition(params.clone());
    let response = response.result.unwrap();

    assert_eq!(
        response,
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\build\packages\hex\src\example_module.gleam"
            } else {
                "/build/packages/hex/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 14
                }
            }
        })
    )
}

#[test]
fn goto_definition_path_module_function_calls_with_multiple_compiles() {
    let dep = "pub fn my_fn() { Nil }";
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    let (mut engine, position_param) = TestProject::for_source(code)
        .add_dep_module("example_module", dep)
        .positioned_with_io(Position::new(3, 20));

    let params = GotoDefinitionParams {
        text_document_position_params: position_param.clone(),
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let response = engine.goto_definition(params.clone());
    let response = response.result.unwrap();

    assert_eq!(
        response,
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\dep\src\example_module.gleam"
            } else {
                "/dep/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 14
                }
            }
        })
    );

    engine.compiler.sources.clear();
    let response = engine.compile_please();
    assert!(response.result.is_ok());

    let response = engine.goto_definition(params.clone());
    let response = response.result.unwrap();

    assert_eq!(
        response,
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\dep\src\example_module.gleam"
            } else {
                "/dep/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 14
                }
            }
        })
    )
}

#[test]
fn goto_definition_external_module_records() {
    let hex_src = "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}
";

    let code = "
import example_module
fn main() {
  example_module.Var1(1)
}
";

    assert_goto!(
        TestProject::for_source(code).add_hex_module("example_module", hex_src),
        find_position_of("Var1(1)").under_char('r')
    );
}

#[test]
fn goto_definition_path_module_function_calls() {
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    assert_goto!(
        TestProject::for_source(code).add_dep_module("example_module", "pub fn my_fn() { Nil }"),
        find_position_of("my_fn").under_char('y')
    );
}

#[test]
fn goto_definition_type() {
    assert_goto!(
        "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}

pub fn make_var() -> Rec {
  Var1(1)
}",
        find_position_of("Rec").nth_occurrence(2)
    );
}

#[test]
fn goto_definition_type_in_module() {
    let hex_src = "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}
";

    let code = "
import example_module
fn make_var() -> example_module.Rec {
  example_module.Var1(1)
}
";

    assert_goto!(
        TestProject::for_source(code).add_hex_module("example_module", hex_src),
        find_position_of("Rec")
    );
}

#[test]
fn goto_definition_type_in_path_dep() {
    let dep = "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}
";

    let code = "
import example_module
fn make_var() -> example_module.Rec {
  example_module.Var1(1)
}
";

    assert_goto!(
        TestProject::for_source(code).add_dep_module("example_module", dep),
        find_position_of("Rec")
    );
}

#[test]
fn goto_definition_deep_type_in_module() {
    let hex_src = "
pub type Wobble {
  Wobble(Int)
}

pub type Wibble(a) {
  Wibble(a)
}

pub type Wabble(a) {
  Wabble(a)
}
";

    let code = "
import example_module
fn make_var() -> example_module.Wabble(example_module.Wibble(example_module.Wobble)) {
  example_module.Wabble(example_module.Wibble(example_module.Wobble(1)))
}
";

    assert_goto!(
        TestProject::for_source(code).add_hex_module("example_module", hex_src),
        find_position_of("Wobble").under_char('o')
    );
}

#[test]
fn goto_definition_import() {
    let code = "
import example_module
fn main() {
  example_module.my_num
}
";

    assert_goto!(
        TestProject::for_source(code).add_module("example_module", "pub const my_num = 1"),
        find_position_of("example_module").under_char('p')
    );
}

#[test]
fn goto_definition_import_aliased() {
    let code = "
import example_module as example
fn main() {
  example.my_num
}
";

    assert_goto!(
        TestProject::for_source(code).add_module("example_module", "pub const my_num = 1"),
        find_position_of("example")
            .nth_occurrence(2)
            .under_char('x')
    );
}

#[test]
fn goto_definition_import_unqualified_value() {
    let code = "
import example_module.{my_num}
fn main() {
  my_num
}
";

    assert_goto!(
        TestProject::for_source(code).add_module("example_module", "pub const my_num = 1"),
        find_position_of("my_num").under_char('_')
    );
}

#[test]
fn goto_definition_unqualified_function() {
    let code = "
import wibble.{wobble}
fn main() {
  wobble()
}
";

    assert_goto!(
        TestProject::for_source(code).add_module("wibble", "pub fn wobble() {}"),
        find_position_of("wobble").nth_occurrence(2).under_char('o')
    );
}

#[test]
fn goto_definition_import_unqualified_type() {
    let code = "
import example_module.{type MyType}
fn main() -> MyType {
  0
}
";

    assert_goto!(
        TestProject::for_source(code).add_module("example_module", "pub type MyType = Int"),
        find_position_of("MyType").under_char('T')
    );
}

// https://github.com/gleam-lang/gleam/issues/3610
#[test]
fn goto_definition_of_external_function_in_same_module() {
    let code = "
@external(erlang, \"wibble\", \"wobble\")
fn external_function() -> Nil

fn main() {
  external_function()
}
";

    assert_goto!(
        TestProject::for_source(code),
        find_position_of("external_function")
            .nth_occurrence(2)
            .under_char('l')
    );
}

// https://github.com/gleam-lang/gleam/issues/3758
#[test]
fn goto_definition_from_anonymous_function() {
    let code = "
pub type Wibble

pub fn main() {
  fn(w: Wibble) { todo }
}
";

    assert_goto!(
        TestProject::for_source(code),
        find_position_of("w: Wibble").under_char('i')
    );
}

#[test]
fn goto_definition_module() {
    let code = "
import wibble

pub fn main() {
  wibble.wibble()
}
";

    assert_goto!(
        TestProject::for_source(code).add_module("wibble", "pub fn wibble() {}"),
        find_position_of("wibble.").under_char('i')
    );
}

#[test]
fn goto_definition_constant() {
    assert_goto!(
        "
const value = 25

const my_constant = value
",
        find_position_of("= value").under_char('a')
    );
}

#[test]
fn goto_definition_constant_record() {
    assert_goto!(
        "
type Wibble {
  Wibble(Int)
}

const wibble = Wibble(10)
",
        find_position_of("Wibble(10)").under_char('l')
    );
}

#[test]
fn goto_definition_imported_constant() {
    let src = "
import wibble

const my_constant = wibble.value
";

    assert_goto!(
        TestProject::for_source(src).add_hex_module("wibble", "pub const value = 10"),
        find_position_of("value").under_char('v')
    );
}

#[test]
fn goto_definition_constant_imported_record() {
    let src = "
import wibble

const my_constant = wibble.Wibble(10)
";

    assert_goto!(
        TestProject::for_source(src).add_hex_module("wibble", "pub type Wibble { Wibble(Int) }"),
        find_position_of("Wibble(10)").under_char('W')
    );
}

#[test]
fn goto_definition_from_alternative_pattern() {
    assert_goto!(
        "
type Wibble {
  Wibble
  Wobble
}

fn warble(wibble: Wibble) {
  case wibble {
    Wibble | Wobble -> 0
  }
}
",
        find_position_of("Wobble ->")
    );
}
