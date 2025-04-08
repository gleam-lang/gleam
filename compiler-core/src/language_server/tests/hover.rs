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

    let mut buffer: String = "".into();
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

        buffer.push_str(line);
        if !underline_empty {
            buffer.push('\n');
            buffer.push_str(&underline);
        }
        buffer.push('\n');
    }

    buffer
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

#[test]
fn hover_label_shorthand_in_call_arg() {
    assert_hover!(
        "
fn wibble(arg1 arg1: Int, arg2 arg2: Bool) { Nil }

fn main() {
  let arg1 = 1
  let arg2 = True
  wibble(arg2:, arg1:)
}
",
        find_position_of("arg2:").nth_occurrence(2)
    );
}

#[test]
fn hover_label_shorthand_in_pattern_call_arg() {
    assert_hover!(
        "
pub type Wibble { Wibble(arg1: Int, arg2: Bool) }

pub fn main() {
  case todo {
    Wibble(arg2:, ..) -> todo
  }
}
",
        find_position_of("arg2:")
            .nth_occurrence(2)
            .under_last_char()
    );
}

#[test]
fn hover_label_shorthand_in_pattern_call_arg_2() {
    assert_hover!(
        "
pub type Wibble { Wibble(arg1: Int, arg2: Bool) }

pub fn main() {
  let Wibble(arg2:, ..) = todo
}
",
        find_position_of("arg2:").nth_occurrence(2).under_char('r')
    );
}

#[test]
fn hover_contextual_type() {
    let code = "
import wibble/wobble
const value = wobble.Wobble
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wobble }"),
        find_position_of("value").under_char('v')
    );
}

#[test]
fn hover_contextual_type_aliased_module() {
    let code = "
import wibble/wobble as wubble
const value = wubble.Wobble
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wobble }"),
        find_position_of("value").under_char('v')
    );
}

#[test]
fn hover_contextual_type_unqualified() {
    let code = "
import wibble/wobble.{type Wibble}
const value = wobble.Wobble
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wobble }"),
        find_position_of("value").under_char('v')
    );
}

#[test]
fn hover_contextual_type_unqualified_aliased() {
    let code = "
import wibble/wobble.{type Wibble as Wobble}
const value = wobble.Wobble
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wobble }"),
        find_position_of("value").under_char('v')
    );
}

#[test]
fn hover_contextual_type_aliased() {
    let code = "
import wibble/wobble
type Local = wobble.Wibble
const value = wobble.Wobble
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wobble }"),
        find_position_of("value").under_char('v')
    );
}

#[test]
fn hover_contextual_type_function() {
    let code = "
import wibble/wobble
type MyInt = Int
fn func(value: wobble.Wibble) -> MyInt { 1 }
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wobble }"),
        find_position_of("func").under_char('f')
    );
}

#[test]
fn hover_contextual_type_unqualified_import() {
    let code = "
import wibble/wobble.{type Wibble as Wobble, Wobble}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wobble }"),
        find_position_of("Wobble}").under_char('W')
    );
}

#[test]
fn hover_contextual_type_pattern() {
    let code = "
import wibble/wobble.{Wibble, Wobble, Wubble}

pub fn cycle(wibble: wobble.Wibble) {
  case wibble {
    Wibble -> Wobble
    Wobble -> Wubble
    Wubble -> Wibble
  }
}
";

    assert_hover!(
        TestProject::for_source(code)
            .add_hex_module("wibble/wobble", "pub type Wibble { Wibble Wobble Wubble }"),
        find_position_of("Wubble ->").under_char('u')
    );
}

#[test]
fn hover_contextual_type_pattern_spread() {
    let code = "
import wibble/wobble.{type Wibble as Wobble}

type Thing {
  Thing(id: Int, value: Wobble)
}

pub fn main(thing: Thing) {
  case thing {
    Thing(id: 0, ..) -> 12
    _ -> 14
  }
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wibble }"),
        find_position_of("..").under_char('.')
    );
}

#[test]
fn hover_contextual_type_expression() {
    let code = "
import wibble/wobble

pub fn main() {
  let wibble = wobble.Wibble
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wibble }"),
        find_position_of(".Wibble").under_char('l')
    );
}

#[test]
fn hover_contextual_type_arg() {
    let code = "
import wibble/wobble

fn do_things(wibble: wobble.Wibble) { wibble }
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wibble }"),
        find_position_of("wibble:").under_char('w')
    );
}

#[test]
fn hover_print_type_variable_names() {
    let code = "
fn main(value: Result(ok, error)) {
  let v = value
  v
}
";

    assert_hover!(
        TestProject::for_source(code),
        find_position_of("let v").under_char('v')
    );
}

#[test]
fn hover_print_unbound_type_variable_names() {
    let code = "
fn make_ok(value: some_type) {
  let result = Ok(value)
  result
}
";

    assert_hover!(
        TestProject::for_source(code),
        find_position_of("result =").under_char('s')
    );
}

#[test]
fn hover_print_unbound_type_variable_name_without_conflicts() {
    let code = "
fn make_ok(value: a) {
  let result = Ok(value)
  result
}
";

    assert_hover!(
        TestProject::for_source(code),
        find_position_of("result =").under_char('s')
    );
}

#[test]
fn hover_print_imported_alias() {
    let code = "
import aliases.{type Aliased}
const thing: Aliased = 10
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("aliases", "pub type Aliased = Int"),
        find_position_of("thing").under_char('g')
    );
}

#[test]
fn hover_prelude_type() {
    let code = "
const number = 100
";

    assert_hover!(
        TestProject::for_source(code),
        find_position_of("number").under_char('b')
    );
}

#[test]
fn hover_shadowed_prelude_type() {
    let code = "
type Int { Int }
const number = 100
";

    assert_hover!(
        TestProject::for_source(code),
        find_position_of("number").under_char('b')
    );
}

#[test]
fn hover_shadowed_prelude_type_imported() {
    let code = "
import numbers.{type Int}
const number = 100
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("numbers", "pub type Int"),
        find_position_of("number =").under_char('b')
    );
}

#[test]
fn hover_contextual_type_annotation() {
    let code = "
import wibble/wobble

fn make_wibble() -> wobble.Wibble { wobble.Wibble }
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wibble }"),
        find_position_of("-> wobble.Wibble").under_char('i')
    );
}

#[test]
fn hover_contextual_type_annotation_prelude() {
    let code = "
fn add_one(a: Int) -> Int {
  a + 1
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wibble }"),
        find_position_of("-> Int").under_char('I')
    );
}

#[test]
fn hover_contextual_type_annotation_unqualified() {
    let code = "
import wibble/wobble.{type Wibble}

fn main(wibble: Wibble) {
  wibble
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wibble }"),
        find_position_of(": Wibble").under_char('W')
    );
}

#[test]
fn hover_contextual_type_annotation_unqualified_aliased() {
    let code = "
import wibble/wobble.{type Wibble as Wubble}

fn main(wibble: Wubble) {
  wibble
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wibble }"),
        find_position_of(": Wubble").under_char('W')
    );
}

#[test]
fn hover_contextual_type_annotation_aliased_module() {
    let code = "
import wibble/wobble as wubble

fn main(wibble: wubble.Wibble) {
  wibble
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wibble }"),
        find_position_of(": wubble.Wibble").under_char('W')
    );
}

#[test]
fn hover_contextual_type_annotation_aliased() {
    let code = "
import wibble/wobble

type Wubble = wobble.Wibble

fn main(wibble: Wubble) {
  wibble
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("wibble/wobble", "pub type Wibble { Wibble }"),
        find_position_of(": Wubble").under_char('e')
    );
}

#[test]
fn hover_print_underlying_for_alias_with_parameters() {
    let code = "
type LocalResult = Result(String, Int)

fn do_thing() -> LocalResult {
  Error(1)
}
";

    assert_hover!(
        TestProject::for_source(code),
        find_position_of("do_thing").under_char('d')
    );
}

#[test]
fn hover_print_alias_when_parameters_match() {
    let code = "
type MyResult(a, b) = Result(a, b)

fn do_thing() -> MyResult(Int, Int) {
  Error(1)
}
";

    assert_hover!(
        TestProject::for_source(code),
        find_position_of("do_thing").under_char('d')
    );
}

#[test]
fn hover_print_underlying_for_imported_alias() {
    let code = "
import alias.{type A}

fn wibble() -> Result(Int, String) {
  todo
}
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("alias", "pub type A = Result(Int, String)"),
        find_position_of("wibble").under_char('l')
    );
}

#[test]
fn hover_print_aliased_imported_generic_type() {
    let code = "
import gleam/option.{type Option as Maybe}

const none: Maybe(Int) = option.None
";

    assert_hover!(
        TestProject::for_source(code)
            .add_hex_module("gleam/option", "pub type Option(a) { None Some(a) }"),
        find_position_of("none").under_char('e')
    );
}

#[test]
fn hover_print_qualified_prelude_type_when_shadowed_by_alias() {
    let code = "
type Result = #(Bool, String)
const ok = Ok(10)
";

    assert_hover!(
        TestProject::for_source(code),
        find_position_of("ok").under_char('k')
    );
}

#[test]
fn hover_print_qualified_prelude_type_when_shadowed_by_imported_alias() {
    let code = "
import alias.{type Bool}
const value = True
";

    assert_hover!(
        TestProject::for_source(code).add_hex_module("alias", "pub type Bool = #(Int, Int)"),
        find_position_of("value").under_char('v')
    );
}

// https://github.com/gleam-lang/gleam/issues/3761
#[test]
fn hover_over_block_in_list_spread() {
    let code = "
pub fn main() {
  [1, 2, ..{
    let x = 1
    [x]
  }]
}
";

    assert_hover!(TestProject::for_source(code), find_position_of("x"));
}

// https://github.com/gleam-lang/gleam/issues/3758
#[test]
fn hover_for_anonymous_function_annotation() {
    let code = "
/// An example type.
pub type Wibble

pub fn main() {
  fn(w: Wibble) { todo }
}
";

    assert_hover!(
        TestProject::for_source(code),
        find_position_of("w: Wibble").under_char('b')
    );
}

#[test]
fn hover_for_label_in_pattern() {
    let code = "
type Wibble {
  Wibble(wibble: Int, wobble: Int)
}

pub fn main() {
  let Wibble(wibble: _, wobble: _) = todo
  todo
}
";

    assert_hover!(
        TestProject::for_source(code),
        find_position_of("wibble: _").under_char('l')
    );
}

#[test]
fn hover_for_label_in_expression() {
    let code = "
fn add(wibble a, wobble b) {
  a + b
}

pub fn main() {
  add(wibble: 1, wobble: 2)
}
";

    assert_hover!(
        TestProject::for_source(code),
        find_position_of("wibble:").under_char('i')
    );
}

#[test]
fn hover_for_pattern_in_use() {
    let code = "
type Wibble {
  Wibble(Int, Float)
}

pub fn main() {
  use Wibble(int, float) <- todo
  todo
}
";

    assert_hover!(
        TestProject::for_source(code),
        find_position_of("int").under_char('i')
    );
}

#[test]
fn hover_for_annotation_in_use() {
    let code = "
pub fn main() {
  use something: Int <- todo
  todo
}
";

    assert_hover!(
        TestProject::for_source(code),
        find_position_of("Int").under_char('n')
    );
}

#[test]
fn hover_on_pipe_with_invalid_step() {
    assert_hover!(
        "
pub fn main() {
  [1, 2, 3]
  |> map(wibble)
  |> filter(fn(value) { value })
}

fn map(list: List(a), fun: fn(a) -> b) -> List(b) {}
fn filter(list: List(a), fun: fn(a) -> Bool) -> List(a) {}
",
        find_position_of("[")
    );
}

#[test]
fn hover_on_pipe_with_invalid_step_1() {
    assert_hover!(
        "
pub fn main() {
  [1, 2, 3]
  |> map(wibble)
  |> filter(fn(value) { value })
}

fn map(list: List(a), fun: fn(a) -> b) -> List(b) {}
fn filter(list: List(a), fun: fn(a) -> Bool) -> List(a) {}
",
        find_position_of("1")
    );
}

#[test]
fn hover_on_pipe_with_invalid_step_2() {
    assert_hover!(
        "
pub fn main() {
  [1, 2, 3]
  |> map(wibble)
  |> filter(fn(value) { value })
}

fn map(list: List(a), fun: fn(a) -> b) -> List(b) {}
fn filter(list: List(a), fun: fn(a) -> Bool) -> List(a) {}
",
        find_position_of("map")
    );
}

#[test]
fn hover_on_pipe_with_invalid_step_3() {
    assert_hover!(
        "
pub fn main() {
  [1, 2, 3]
  |> map(wibble)
  |> filter(fn(value) { value })
}

fn map(list: List(a), fun: fn(a) -> b) -> List(b) {}
fn filter(list: List(a), fun: fn(a) -> Bool) -> List(a) {}
",
        find_position_of("wibble")
    );
}

#[test]
fn hover_on_pipe_with_invalid_step_4() {
    assert_hover!(
        "
pub fn main() {
  [1, 2, 3]
  |> map(wibble)
  |> filter(fn(value) { value })
}

fn map(list: List(a), fun: fn(a) -> b) -> List(b) {}
fn filter(list: List(a), fun: fn(a) -> Bool) -> List(a) {}
",
        find_position_of("filter")
    );
}

#[test]
fn hover_on_pipe_with_invalid_step_5() {
    assert_hover!(
        "
pub fn main() {
  [1, 2, 3]
  |> map(wibble)
  |> filter(fn(value) { value })
}

fn map(list: List(a), fun: fn(a) -> b) -> List(b) { todo }
fn filter(list: List(a), fun: fn(a) -> Bool) -> List(a) { todo }
",
        find_position_of("fn(value)")
    );
}

#[test]
fn hover_on_pipe_with_invalid_step_6() {
    assert_hover!(
        "
pub fn main() {
  [1, 2, 3]
  |> wibble
  |> filter(fn(value) { value })
}

fn filter(list: List(a), fun: fn(a) -> Bool) -> List(a) { todo }
",
        find_position_of("wibble")
    );
}

#[test]
fn hover_on_pipe_with_invalid_step_8() {
    assert_hover!(
        "
pub fn main() {
  [1, 2, 3]
  |> wibble
  |> filter(fn(value) { value })
}

fn filter(list: List(a), fun: fn(a) -> Bool) -> List(a) { todo }
",
        find_position_of("fn(value)")
    );
}

#[test]
fn hover_over_module_name() {
    let src = "
import wibble

pub fn main() {
  wibble.wibble()
}
";
    assert_hover!(
        TestProject::for_source(src).add_hex_module(
            "wibble",
            "
//// This is the wibble module.
//// Here is some documentation about it.
//// This module does stuff

pub fn wibble() {
  todo
}
"
        ),
        find_position_of("wibble.")
    );
}

#[test]
fn hover_over_module_with_path() {
    let src = "
import wibble/wobble

pub fn main() {
  wobble.wibble()
}
";
    assert_hover!(
        TestProject::for_source(src).add_hex_module(
            "wibble/wobble",
            "
//// The module documentation

pub fn wibble() {
  todo
}
"
        ),
        find_position_of("wobble.")
    );
}

#[test]
fn hover_over_module_name_in_annotation() {
    let src = "
import wibble

pub fn main(w: wibble.Wibble) {
  todo
}
";
    assert_hover!(
        TestProject::for_source(src).add_hex_module(
            "wibble",
            "
//// This is the wibble module.
//// Here is some documentation about it.
//// This module does stuff

pub type Wibble
"
        ),
        find_position_of("wibble.")
    );
}

#[test]
fn hover_over_imported_module() {
    let src = "
import wibble
";
    assert_hover!(
        TestProject::for_source(src).add_hex_module(
            "wibble",
            "
//// This is the wibble module.
//// Here is some documentation about it.
//// This module does stuff
"
        ),
        find_position_of("wibble")
    );
}

#[test]
fn no_hexdocs_link_when_hovering_over_local_module() {
    let src = "
import wibble
";
    assert_hover!(
        TestProject::for_source(src).add_module(
            "wibble",
            "
//// This is the wibble module.
//// Here is some documentation about it.
//// This module does stuff
"
        ),
        find_position_of("wibble")
    );
}

#[test]
fn hover_custom_type() {
    assert_hover!(
        "
/// Exciting documentation
/// Maybe even multiple lines
type Wibble {
    /// Some more exciting documentation
    Wibble(String)
    /// The most exciting documentation
    Wobble(arg: Int)
}
",
        find_position_of("Wibble")
    );
}

#[test]
fn hover_type_constructor() {
    assert_hover!(
        "
/// Exciting documentation
/// Maybe even multiple lines
type Wibble {
    /// Some more exciting documentation
    Wibble(arg: String)
    /// The most exciting documentation
    Wobble(Int)
}
",
        find_position_of("Wibble").nth_occurrence(2)
    );
}

#[test]
fn hover_type_constructor_with_label() {
    assert_hover!(
        "
/// Exciting documentation
/// Maybe even multiple lines
type Wibble {
    /// Some more exciting documentation
    Wibble(arg: String)
    /// The most exciting documentation
    Wobble(Int)
}
",
        find_position_of("Wobble")
    );
}

#[test]
fn hover_for_label_in_type_constructor() {
    assert_hover!(
        "
/// Exciting documentation
/// Maybe even multiple lines
type Wibble {
    /// Some more exciting documentation
    Wibble(arg: String)
    /// The most exciting documentation
    Wobble(Int)
}
",
        find_position_of("arg")
    );
}
