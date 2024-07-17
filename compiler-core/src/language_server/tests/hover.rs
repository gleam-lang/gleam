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

pub fn show_hover(code: &str, range: Range, position: Position) -> String {
    let Range { start, end } = range;

    // When we display the over range the end character is always excluded!
    let end = Position::new(end.line, end.character);

    let mut str: String = "".into();
    for (line_number, line) in code.lines().enumerate() {
        let mut underline: String = "".into();
        let mut underline_empty = true;

        for (column_number, _) in line.chars().enumerate() {
            let current_position = Position::new(line_number as u32, column_number as u32);
            if current_position == position {
                underline_empty = false;
                underline.push('↑');
            } else if start.le(&current_position) && current_position.lt(&end) {
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
    ($code:literal, $position:expr $(,)?) => {
        let project = TestProject::for_source($code);
        assert_hover!(project, $position);
    };

    ($project:expr, $position:expr $(,)?) => {
        let src = $project.src;
        let position = $position.find_position(src);
        let result = hover($project, position).expect("no hover produced");
        let pretty_hover = show_hover(src, result.range.expect("hover with no range"), position);
        let output = format!(
            "{}\n\n----- Hover content -----\n{:#?}",
            pretty_hover, result.contents
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, src);
    };
}

#[test]
fn hover_function_definition() {
    assert_hover!(
        "
fn add_2(x) {
  x + 2
}
",
        find_position_of("add_2")
    );
}

#[test]
fn hover_local_function() {
    assert_hover!(
        "
fn my_fn() {
  Nil
}

fn main() {
  my_fn
}
",
        find_position_of("my_fn").under_char('y').nth_occurrence(2)
    );
}

// https://github.com/gleam-lang/gleam/issues/2654
#[test]
fn hover_local_function_in_pipe() {
    assert_hover!(
        "
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
",
        find_position_of("add1")
            .with_char_offset(1)
            .nth_occurrence(2)
    );
}

// https://github.com/gleam-lang/gleam/issues/2654
#[test]
fn hover_local_function_in_pipe_1() {
    assert_hover!(
        "
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
",
        find_position_of("add1")
            .with_char_offset(2)
            .nth_occurrence(3)
    );
}

// https://github.com/gleam-lang/gleam/issues/2654
#[test]
fn hover_local_function_in_pipe_2() {
    assert_hover!(
        "
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
",
        find_position_of("add1")
            .with_char_offset(2)
            .nth_occurrence(4)
    );
}

// https://github.com/gleam-lang/gleam/issues/2654
#[test]
fn hover_local_function_in_pipe_3() {
    assert_hover!(
        "
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
",
        find_position_of("add1")
            .with_char_offset(2)
            .nth_occurrence(5)
    );
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
        find_position_of("my_fn").under_char('_'),
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
        find_position_of("my_fn").under_char('_'),
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
        find_position_of("my_fn").under_char('f').nth_occurrence(2),
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
        find_position_of("my_fn").under_char('f'),
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
        find_position_of("my_fn").under_char('_').nth_occurrence(2),
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
        find_position_of("my_fn").under_char('f'),
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

    let hex_module = r#"
@external(erlang, "my_mod_ffi", "renamed_fn")
pub fn my_fn() -> Nil
"#;

    assert_hover!(
        TestProject::for_source(code).add_hex_module("example_module", hex_module,),
        find_position_of("my_fn").under_char('f'),
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
        find_position_of("my_const").under_char('_'),
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
        find_position_of("my_const")
            .under_char('c')
            .nth_occurrence(2),
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
        find_position_of("my_const").under_char('c'),
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
        find_position_of("my_const")
            .under_char('o')
            .nth_occurrence(3),
    );
}

#[test]
fn hover_function_definition_with_docs() {
    assert_hover!(
        "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x, y) {
  x <> y
}
",
        find_position_of("append")
    );
}

#[test]
fn hover_function_argument() {
    assert_hover!(
        "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x, y) {
  x <> y
}
",
        find_position_of("append(x, y)").under_char('x')
    );
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
    assert_hover!(
        "
fn append(x, y) {
  x <> y
}
",
        find_position_of("x").nth_occurrence(2)
    );
}

#[test]
fn hover_module_constant() {
    assert_hover!(
        "
/// Exciting documentation
/// Maybe even multiple lines
const one = 1
",
        find_position_of("one")
    );
}

#[test]
fn hover_variable_in_use_expression() {
    assert_hover!(
        "
fn b(fun: fn(Int) -> String) {
  fun(42)
}

fn do_stuff() {
  let c = \"done\"

  use a <- b
  c
}
",
        find_position_of("use a").under_last_char()
    );
}

#[test]
fn hover_variable_in_use_expression_1() {
    assert_hover!(
        "
fn b(fun: fn(Int) -> String) {
  fun(42)
}

fn do_stuff() {
  let c = \"done\"

  use a <- b
  c
}
",
        find_position_of("b").nth_occurrence(2)
    );
}

#[test]
fn hover_variable_in_use_expression_2() {
    assert_hover!(
        "
fn b(fun: fn(Int) -> String) {
  fun(42)
}

fn do_stuff() {
  let c = \"done\"

  use a <- b
  c
}
",
        find_position_of("c").nth_occurrence(2)
    );
}

#[test]
fn hover_function_arg_annotation_2() {
    assert_hover!(
        "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x: String, y: String) -> String {
  x <> y
}
",
        find_position_of("String").under_char('n')
    );
}

#[test]
fn hover_function_return_annotation() {
    assert_hover!(
        "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x: String, y: String) -> String {
  x <> y
}
",
        find_position_of("String").under_char('n').nth_occurrence(3)
    );
}

#[test]
fn hover_function_return_annotation_with_tuple() {
    assert_hover!(
        "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x: String, y: String) -> #(String, String) {
  #(x, y)
}
",
        find_position_of("String").under_char('r').nth_occurrence(3)
    );
}

#[test]
fn hover_module_constant_annotation() {
    assert_hover!(
        "
/// Exciting documentation
/// Maybe even multiple lines
const one: Int = 1
",
        find_position_of("Int").under_last_char()
    );
}

#[test]
fn hover_type_constructor_annotation() {
    assert_hover!(
        "
type Wibble {
    Wibble(arg: String)
}
",
        find_position_of("String").under_char('n')
    );
}

#[test]
fn hover_type_alias_annotation() {
    assert_hover!("type Wibble = Int", find_position_of("Int").under_char('n'));
}

#[test]
fn hover_assignment_annotation() {
    assert_hover!(
        "
fn wibble() {
    let wobble: Int = 7
    wobble
}
",
        find_position_of("Int").under_last_char()
    );
}

#[test]
fn hover_function_arg_annotation_with_documentation() {
    assert_hover!(
        "
/// Exciting documentation
/// Maybe even multiple lines
type Wibble {
    Wibble(arg: String)
}

fn identity(x: Wibble) -> Wibble {
  x
}
",
        find_position_of("Wibble")
            .under_last_char()
            .nth_occurrence(3)
    );
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
        find_position_of("my_num").under_char('n')
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
        find_position_of("my_num").under_char('n')
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
        find_position_of("MyType").under_last_char()
    );
}

#[test]
fn hover_works_even_for_invalid_code() {
    assert_hover!(
        "
fn invalid() { 1 + Nil }
fn valid() { Nil }
",
        find_position_of("fn valid").under_char('v')
    );
}

#[test]
fn hover_for_pattern_spread_ignoring_all_fields() {
    assert_hover!(
        "
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
",
        find_position_of("..")
    );
}

#[test]
fn hover_for_pattern_spread_ignoring_some_fields() {
    assert_hover!(
        "
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
",
        find_position_of("..").under_last_char()
    );
}

#[test]
fn hover_for_pattern_spread_ignoring_all_positional_fields() {
    assert_hover!(
        "
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
",
        find_position_of("..")
    );
}
