use lsp_types::{Hover, HoverParams, Position, Range};

use super::*;

fn hover(tester: TestProject<'_>, position: Position) -> Option<Hover> {
    tester.at(position, |engine, param, _| {
        let params = HoverParams {
            text_document_position_params: param,
            work_done_progress_params: Default::default(),
        };
        let response = engine.hover(params);

        response.result.unwrap()
    })
}

fn show_hover(code: &str, range: Range, pointer: Position) -> String {
    let Range { start, end } = range;

    // When we display the over range the end character is always excluded!
    let end = Position::new(end.line, end.character);

    let mut str: String = "".into();
    for (line_number, line) in code.lines().enumerate() {
        let mut underline: String = "".into();
        let mut underline_empty = true;

        for (column_number, _) in line.chars().enumerate() {
            let position = Position::new(line_number as u32, column_number as u32);
            if position == pointer {
                underline_empty = false;
                underline.push('↑');
            } else if start.le(&position) && position.lt(&end) {
                underline_empty = false;
                underline.push('▔');
            } else {
                underline.push(' ');
            }
        }

        str.push_str(line);
        if !underline_empty {
            str.push('\n');
            str.push_str(&underline);
        }
        str.push('\n');
    }

    str
}

#[macro_export]
macro_rules! assert_hover {
    ($project:expr, $position:expr $(,)?) => {
        let src = $project.src;
        let result = hover($project, $position).expect("no hover produced");
        let pretty_hover = show_hover(src, result.range.expect("hover with no range"), $position);
        let output = format!(
            "{}\n\n----- Hover content -----\n{:#?}",
            pretty_hover, result.contents
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, src);
    };
}

#[test]
fn hover_function_definition() {
    let code = "
fn add_2(x) {
  x + 2
}
";
    let project = TestProject::for_source(code);
    assert_hover!(project, Position::new(1, 3));
}

#[test]
fn hover_local_function() {
    let code = "
fn my_fn() {
  Nil
}

fn main() {
  my_fn
}
";

    assert_hover!(TestProject::for_source(code), Position::new(6, 3));
}

// https://github.com/gleam-lang/gleam/issues/2654
#[test]
fn hover_local_function_in_pipe() {
    let code = "
fn add1(num: Int) -> Int {
  num + 1
}

pub fn main() {
  add1(1)

  1
  |> add1
  |> add1
  |> add1
}
";

    assert_hover!(TestProject::for_source(code), Position::new(6, 3));
}

// https://github.com/gleam-lang/gleam/issues/2654
#[test]
fn hover_local_function_in_pipe_1() {
    let code = "
fn add1(num: Int) -> Int {
  num + 1
}

pub fn main() {
  add1(1)

  1
  |> add1
  |> add1
  |> add1
}
";

    assert_hover!(TestProject::for_source(code), Position::new(9, 7));
}

// https://github.com/gleam-lang/gleam/issues/2654
#[test]
fn hover_local_function_in_pipe_2() {
    let code = "
fn add1(num: Int) -> Int {
  num + 1
}

pub fn main() {
  add1(1)

  1
  |> add1
  |> add1
  |> add1
}
";

    assert_hover!(TestProject::for_source(code), Position::new(10, 7));
}

// https://github.com/gleam-lang/gleam/issues/2654
#[test]
fn hover_local_function_in_pipe_3() {
    let code = "
fn add1(num: Int) -> Int {
  num + 1
}

pub fn main() {
  add1(1)

  1
  |> add1
  |> add1
  |> add1
}
";

    assert_hover!(TestProject::for_source(code), Position::new(11, 7));
}

#[test]
fn hover_imported_function() {
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    assert_hover!(
        TestProject::for_source(code).add_module("example_module", "pub fn my_fn() { Nil }"),
        Position::new(3, 19),
    );
}

#[test]
fn hover_external_imported_function() {
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("example_module", "pub fn my_fn() { Nil }"),
        Position::new(3, 19),
    );
}

#[test]
fn hover_external_imported_unqualified_function() {
    let code = "
import example_module.{my_fn}
fn main() {
  my_fn
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("example_module", "pub fn my_fn() { Nil }"),
        Position::new(3, 5),
    );
}

#[test]
fn hover_external_imported_function_renamed_module() {
    let code = "
import example_module as renamed_module
fn main() {
    renamed_module.my_fn
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("example_module", "pub fn my_fn() { Nil }"),
        Position::new(3, 22),
    );
}

#[test]
fn hover_external_unqualified_imported_function_renamed_module() {
    let code = "
import example_module.{my_fn} as renamed_module
fn main() {
    my_fn
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("example_module", "pub fn my_fn() { Nil }"),
        Position::new(3, 6),
    );
}

#[test]
fn hover_external_imported_function_nested_module() {
    // Example of HexDocs link with nested modules: https://hexdocs.pm/lustre/lustre/element/svg.html
    let code = "
import my/nested/example_module
fn main() {
    example_module.my_fn
}
";

    assert_hover!(
        TestProject::for_source(code)
            .add_hex_module("my/nested/example_module", "pub fn my_fn() { Nil }"),
        Position::new(3, 22),
    );
}

#[test]
fn hover_external_imported_ffi_renamed_function() {
    let code = r#"
import example_module
fn main() {
    example_module.my_fn
}
"#;

    assert_hover!(
        TestProject::for_source(code).add_hex_module(
            "example_module",
            r#"
@external(erlang, "my_mod_ffi", "renamed_fn")
pub fn my_fn() -> Nil
"#,
        ),
        Position::new(3, 22),
    );
}

#[test]
fn hover_external_imported_constants() {
    let code = "
import example_module
fn main() {
  example_module.my_const
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("example_module", "pub const my_const = 42"),
        Position::new(3, 19),
    );
}

#[test]
fn hover_external_imported_unqualified_constants() {
    let code = "
import example_module.{my_const}
fn main() {
  my_const
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("example_module", "pub const my_const = 42"),
        Position::new(3, 5),
    );
}

#[test]
fn hover_external_value_with_two_modules_same_name() {
    let code = "
import a/example_module as _
import b/example_module
fn main() {
    example_module.my_const
}
";

    assert_hover!(
        TestProject::for_source(code)
            .add_hex_module("a/example_module", "pub const my_const = 42")
            .add_hex_module("b/example_module", "pub const my_const = 42"),
        Position::new(4, 22),
    );
}

#[test]
fn hover_external_function_with_another_value_same_name() {
    let code = "
import a/example_module.{my_const as discarded}
import b/example_module.{my_const} as _
fn main() {
    my_const
}
";

    assert_hover!(
        TestProject::for_source(code)
            .add_hex_module("a/example_module", "pub const my_const = 42")
            .add_hex_module("b/example_module", "pub const my_const = 42"),
        Position::new(4, 8),
    );
}

#[test]
fn hover_function_definition_with_docs() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x, y) {
  x <> y
}
";

    assert_hover!(TestProject::for_source(code), Position::new(3, 3));
}

#[test]
fn hover_function_argument() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x, y) {
  x <> y
}
";

    assert_hover!(TestProject::for_source(code), Position::new(3, 10));
}

#[test]
fn hover_function_body() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x, y) {
  x <> y
}
";

    assert_eq!(
        hover(TestProject::for_source(code), Position::new(4, 1)),
        None
    );
}

#[test]
fn hover_expressions_in_function_body() {
    let code = "
fn append(x, y) {
  x <> y
}
";

    assert_hover!(TestProject::for_source(code), Position::new(2, 2));
}

#[test]
fn hover_module_constant() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
const one = 1
";

    assert_hover!(TestProject::for_source(code), Position::new(3, 6));
}

#[test]
fn hover_variable_in_use_expression() {
    let code = "
fn b(fun: fn(Int) -> String) {
  fun(42)
}

fn do_stuff() {
  let c = \"done\"

  use a <- b
  c
}
";

    assert_hover!(TestProject::for_source(code), Position::new(8, 6));
}

#[test]
fn hover_variable_in_use_expression_1() {
    let code = "
fn b(fun: fn(Int) -> String) {
  fun(42)
}

fn do_stuff() {
  let c = \"done\"

  use a <- b
  c
}
";

    assert_hover!(TestProject::for_source(code), Position::new(8, 11));
}

#[test]
fn hover_variable_in_use_expression_2() {
    let code = "
fn b(fun: fn(Int) -> String) {
  fun(42)
}

fn do_stuff() {
  let c = \"done\"

  use a <- b
  c
}
";

    assert_hover!(TestProject::for_source(code), Position::new(9, 2));
}

#[test]
fn hover_function_arg_annotation_2() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x: String, y: String) -> String {
  x <> y
}
";

    assert_hover!(TestProject::for_source(code), Position::new(3, 17));
}

#[test]
fn hover_function_return_annotation() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x: String, y: String) -> String {
  x <> y
}
";

    assert_hover!(TestProject::for_source(code), Position::new(3, 39));
}

#[test]
fn hover_function_return_annotation_with_tuple() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x: String, y: String) -> #(String, String) {
  #(x, y)
}
";

    assert_hover!(TestProject::for_source(code), Position::new(3, 39));
}

#[test]
fn hover_module_constant_annotation() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
const one: Int = 1
";

    assert_hover!(TestProject::for_source(code), Position::new(3, 13));
}

#[test]
fn hover_type_constructor_annotation() {
    let code = "
type Wibble {
    Wibble(arg: String)
}
";

    assert_hover!(TestProject::for_source(code), Position::new(2, 20));
}

#[test]
fn hover_type_alias_annotation() {
    let code = "
type Wibble = Int
";

    assert_hover!(TestProject::for_source(code), Position::new(1, 15));
}

#[test]
fn hover_assignment_annotation() {
    let code = "
fn wibble() {
    let wobble: Int = 7
    wobble
}
";

    assert_hover!(TestProject::for_source(code), Position::new(2, 18));
}

#[test]
fn hover_function_arg_annotation_with_documentation() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
type Wibble {
    Wibble(arg: String)
}

fn identity(x: Wibble) -> Wibble {
  x
}
";

    assert_hover!(TestProject::for_source(code), Position::new(7, 20));
}

#[test]
fn hover_import_unqualified_value() {
    let code = "
import example_module.{my_num}
fn main() {
  my_num
}
";

    assert_hover!(
        TestProject::for_source(code).add_module(
            "example_module",
            "
/// Exciting documentation
/// Maybe even multiple lines
pub const my_num = 1"
        ),
        Position::new(1, 26)
    );
}

#[test]
fn hover_import_unqualified_value_from_hex() {
    let code = "
import example_module.{my_num}
fn main() {
  my_num
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module(
            "example_module",
            "
/// Exciting documentation
/// Maybe even multiple lines
pub const my_num = 1"
        ),
        Position::new(1, 26)
    );
}

#[test]
fn hover_import_unqualified_type() {
    let code = "
import example_module.{type MyType, MyType}
fn main() -> MyType {
  MyType
}
";

    assert_hover!(
        TestProject::for_source(code).add_module(
            "example_module",
            "
/// Exciting documentation
/// Maybe even multiple lines
pub type MyType {
    MyType
}"
        ),
        Position::new(1, 33)
    );
}

#[test]
fn hover_works_even_for_invalid_code() {
    let code = "
fn invalid() { 1 + Nil }
fn valid() { Nil }
";

    assert_hover!(TestProject::for_source(code), Position::new(2, 3));
}

#[test]
fn hover_for_pattern_spread_ignoring_all_fields() {
    let code = "
pub type Model {
  Model(
    Int,
    Float,
    label1: Int,
    label2: String,
  )
}

pub fn main() {
  case todo {
    Model(..) -> todo
  }
}
";

    assert_eq!(
        hover(TestProject::for_source(code), Position::new(12, 10)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "Unused positional fields:
- `Int`
- `Float`

Unused labelled fields:
- `label1: Int`
- `label2: String`"
                    .into()
            )),
            range: Some(Range {
                start: Position::new(12, 10),
                end: Position::new(12, 12)
            }),
        })
    );
}

#[test]
fn hover_for_pattern_spread_ignoring_some_fields() {
    let code = "
pub type Model {
  Model(
    Int,
    Float,
    label1: Int,
    label2: String,
  )
}

pub fn main() {
  case todo {
    Model(_, label1: _, ..) -> todo
  }
}
";

    assert_eq!(
        hover(TestProject::for_source(code), Position::new(12, 25)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "Unused positional fields:
- `Float`

Unused labelled fields:
- `label2: String`"
                    .into()
            )),
            range: Some(Range {
                start: Position::new(12, 24),
                end: Position::new(12, 26)
            }),
        })
    );
}

#[test]
fn hover_for_pattern_spread_ignoring_all_positional_fields() {
    let code = "
pub type Model {
  Model(
    Int,
    Float,
    label1: Int,
    label2: String,
  )
}

pub fn main() {
  case todo {
    Model(_, _, _, ..) -> todo
  }
}
";

    assert_eq!(
        hover(TestProject::for_source(code), Position::new(12, 19)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "Unused labelled fields:\n- `label2: String`".into()
            )),
            range: Some(Range {
                start: Position::new(12, 19),
                end: Position::new(12, 21)
            }),
        })
    );
}
