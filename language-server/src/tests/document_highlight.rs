use lsp_types::{
    DocumentHighlightParams, PartialResultParams, Position, Range, TextDocumentPositionParams,
    WorkDoneProgressParams,
};

use super::{TestProject, find_position_of};

fn find_highlights(tester: &TestProject<'_>, position: Position) -> Option<(String, Vec<Range>)> {
    tester.at(position, |engine, params, _| {
        let params = DocumentHighlightParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: params.text_document,
                position,
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };
        let module_name = tester
            .module_name_from_url(&params.text_document_position_params.text_document.uri)
            .expect("Valid uri");
        engine
            .document_highlight(params)
            .result
            .unwrap()
            .map(|highlights| {
                highlights
                    .into_iter()
                    .map(|highlight| highlight.range)
                    .collect()
            })
            .map(|highlights| (module_name, highlights))
    })
}

fn show_highlights(code: &str, position: Option<Position>, ranges: &[Range]) -> String {
    let mut buffer = String::new();

    for (line_number, line) in code.lines().enumerate() {
        let mut underline = String::new();
        let mut underline_empty = true;
        let line_number = line_number as u32;

        for (column_number, _) in line.chars().enumerate() {
            let current_position = Position::new(line_number, column_number as u32);

            // Check if any range covers this specific character
            let is_in_range = ranges
                .iter()
                .any(|range| range.start <= current_position && current_position < range.end);

            if Some(current_position) == position {
                underline_empty = false;
                underline.push('↑');
            } else if is_in_range {
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

macro_rules! assert_highlights {
    ($code:literal, $position:expr $(,)?) => {
        assert_highlights!(TestProject::for_source($code), $position);
    };

    (($module_name:literal, $module_src:literal), $code:literal, $position:expr $(,)?) => {
        assert_highlights!(
            TestProject::for_source($code).add_module($module_name, $module_src),
            $position
        );
    };

    ($project:expr, $position:expr $(,)?) => {
        let project = $project;
        let src = project.src;
        let position = $position.find_position(src);
        let (module_name, result) =
            find_highlights(&project, position).expect("Higlights not found");

        let mut output = String::new();
        for (name, src) in project.root_package_modules.iter() {
            let highlights_in_module = if *name == module_name {
                &result
            } else {
                &Vec::new()
            };
            output.push_str(&format!(
                "-- {name}.gleam\n{}\n\n",
                show_highlights(src, None, highlights_in_module)
            ));
        }
        let highlights_in_app_module = if module_name == "app" {
            &result
        } else {
            &Vec::new()
        };
        output.push_str(&format!(
            "-- app.gleam\n{}",
            show_highlights(src, Some(position), highlights_in_app_module)
        ));

        insta::assert_snapshot!(insta::internals::AutoName, output, src);
    };
}

macro_rules! assert_no_highlights {
    ($code:literal, $position:expr $(,)?) => {
        let project = TestProject::for_source($code);
        assert_no_highlights!(&project, $position);
    };

    ($project:expr, $position:expr $(,)?) => {
        let src = $project.src;
        let position = $position.find_position(src);
        let result = find_highlights($project, position);
        assert_eq!(result, None);
    };
}

#[test]
fn highlights_for_local_variable() {
    assert_highlights!(
        "
pub fn main() {
  let wibble = 10
  let wobble = wibble + 1
  wibble + wobble
}
",
        find_position_of("wibble").nth_occurrence(2),
    );
}

#[test]
fn highlights_for_local_variable_from_definition() {
    assert_highlights!(
        "
pub fn main() {
  let wibble = 10
  let wobble = wibble + 1
  wibble + wobble
}
",
        find_position_of("wibble"),
    );
}

#[test]
fn highlights_for_private_function() {
    assert_highlights!(
        "
fn wibble() {
  wibble()
}

pub fn main() {
  let _ = wibble()
  wibble() + 4
}

fn wobble() {
  wibble() || wobble()
}
",
        find_position_of("wibble"),
    );
}

#[test]
fn highlights_for_private_function_from_reference() {
    assert_highlights!(
        "
fn wibble() {
  wibble()
}

pub fn main() {
  let _ = wibble()
  wibble() + 4
}

fn wobble() {
  wibble() || wobble()
}
",
        find_position_of("wibble").nth_occurrence(2),
    );
}

#[test]
fn highlights_for_public_function() {
    assert_highlights!(
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

fn wobble() {
  mod.wibble()
}

fn other() {
  wibble()
}
",
        find_position_of("wibble").nth_occurrence(2),
    );
}

#[test]
fn highlights_for_function_from_qualified_reference() {
    assert_highlights!(
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
  let value = mod.wibble()
  mod.wibble()
  value
}
",
        find_position_of("wibble"),
    );
}

#[test]
fn highlights_for_function_from_unqualified_reference() {
    assert_highlights!(
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
  let value = wibble()
  mod.wibble()
  value
}
",
        find_position_of("wibble()"),
    );
}

#[test]
fn highlights_for_private_constant() {
    assert_highlights!(
        "
const wibble = 10

pub fn main() {
  let _ = wibble
  wibble + 4
}

fn wobble() {
  wibble + wobble()
}
",
        find_position_of("wibble"),
    );
}

#[test]
fn highlights_for_private_constant_from_reference() {
    assert_highlights!(
        "
const wibble = 10

pub fn main() {
  let _ = wibble
  wibble + 4
}

fn wobble() {
  wibble + wobble()
}
",
        find_position_of("wibble").nth_occurrence(2),
    );
}

#[test]
fn highlights_for_public_constant() {
    assert_highlights!(
        (
            "mod",
            "
pub const wibble = 10

pub fn main() {
  wibble
}
"
        ),
        "
import mod.{wibble}

fn wobble() {
  mod.wibble
}

fn other() {
  wibble
}
",
        find_position_of("wibble").nth_occurrence(2),
    );
}

#[test]
fn highlights_for_constant_from_qualified_reference() {
    assert_highlights!(
        (
            "mod",
            "
pub const wibble = 10

fn wobble() {
  wibble
}
"
        ),
        "
import mod

pub fn main() {
  let value = mod.wibble
  mod.wibble + value
}
",
        find_position_of("wibble"),
    );
}

#[test]
fn highlights_for_constant_from_unqualified_reference() {
    assert_highlights!(
        (
            "mod",
            "
pub const wibble = 10

fn wobble() {
  wibble
}
"
        ),
        "
import mod.{wibble}

pub fn main() {
  let value = mod.wibble
  wibble + value
}
",
        find_position_of("wibble +"),
    );
}

#[test]
fn higlights_for_private_type_variant() {
    assert_highlights!(
        "
type Wibble { Wibble }

fn main() {
  let _ = Wibble
  Wibble
}

fn wobble() {
  Wibble
  wobble()
}
",
        find_position_of("Wibble }"),
    );
}

#[test]
fn higlights_for_private_type_variant_from_reference() {
    assert_highlights!(
        "
type Wibble { Wibble }

fn main() {
  let _ = Wibble
  Wibble
}

fn wobble() {
  Wibble
  wobble()
}
",
        find_position_of(" = Wibble").under_char('W'),
    );
}

#[test]
fn higlights_for_public_type_variant() {
    assert_highlights!(
        (
            "mod",
            "
pub type Wibble { Wibble }

pub fn main() {
  Wibble
}
"
        ),
        "
import mod.{Wibble}

fn wobble() {
  mod.Wibble
}

fn other() {
  Wibble
}
",
        find_position_of("Wibble").nth_occurrence(3),
    );
}

#[test]
fn higlights_for_type_variant_from_qualified_reference() {
    assert_highlights!(
        (
            "mod",
            "
pub type Wibble { Wibble }

fn wobble() {
  Wibble
}
"
        ),
        "
import mod

pub fn main() {
  let value = mod.Wibble
  mod.Wibble
  value
}
",
        find_position_of("Wibble"),
    );
}

#[test]
fn higlights_for_type_variant_from_unqualified_reference() {
    assert_highlights!(
        (
            "mod",
            "
pub type Wibble { Wibble }

fn wobble() {
  Wibble
}
"
        ),
        "
import mod.{Wibble}

pub fn main() {
  let value = mod.Wibble
  Wibble
}
",
        find_position_of("Wibble").nth_occurrence(3),
    );
}

#[test]
fn no_higlights_for_keyword() {
    assert_no_highlights!(
        "
pub fn wibble() {
  todo
}
",
        find_position_of("fn")
    );
}

#[test]
fn higlights_for_aliased_value() {
    assert_highlights!(
        (
            "mod",
            "
pub type Wibble { Wibble }

pub fn main() {
  Wibble
}
"
        ),
        "
import mod.{Wibble as Wobble}

fn wobble() {
  Wobble
}
",
        find_position_of("Wobble").nth_occurrence(2),
    );
}

#[test]
fn higlights_for_private_type() {
    assert_highlights!(
        "
type Wibble { Wibble }

fn main() -> Wibble {
  todo
}

fn wobble(w: Wibble) {
  todo
}
",
        find_position_of("Wibble"),
    );
}

#[test]
fn higlights_for_private_type_from_reference() {
    assert_highlights!(
        "
type Wibble { Wibble }

fn main() -> Wibble {
  todo
}

fn wobble(w: Wibble) {
  todo
}
",
        find_position_of("-> Wibble").under_char('W'),
    );
}

#[test]
fn higlights_for_public_type() {
    assert_highlights!(
        (
            "mod",
            "
pub type Wibble { Wibble }

pub fn main() -> Wibble {
  todo
}
"
        ),
        "
import mod.{type Wibble}

fn wobble() -> Wibble {
  todo
}

fn other(w: mod.Wibble) {
  todo
}
",
        find_position_of("-> Wibble").under_char('W'),
    );
}

#[test]
fn higlights_for_type_from_qualified_reference() {
    assert_highlights!(
        (
            "mod",
            "
pub type Wibble { Wibble }

fn wobble() -> Wibble {
  todo
}
"
        ),
        "
import mod

pub fn main() -> mod.Wibble {
  let _: mod.Wibble = todo
}
",
        find_position_of("Wibble"),
    );
}

#[test]
fn higlights_for_type_from_unqualified_reference() {
    assert_highlights!(
        (
            "mod",
            "
pub type Wibble { Wibble }

fn wobble() -> Wibble {
  todo
}
"
        ),
        "
import mod.{type Wibble}

pub fn main() -> Wibble {
  let _: mod.Wibble = todo
}
",
        find_position_of("Wibble").nth_occurrence(2),
    );
}

#[test]
fn higlights_for_aliased_type() {
    assert_highlights!(
        (
            "mod",
            "
pub type Wibble { Wibble }

pub fn main() -> Wibble {
  todo
}
"
        ),
        "
import mod.{type Wibble as Wobble}

fn wobble() -> Wobble {
  todo
}

fn other(w: mod.Wibble) {
  todo
}
",
        find_position_of("-> Wobble").under_char('W'),
    );
}

#[test]
fn higlights_for_type_from_let_annotation() {
    assert_highlights!(
        (
            "mod",
            "
pub type Wibble { Wibble }

fn wobble() -> Wibble {
  todo
}
"
        ),
        "
import mod.{type Wibble}

pub fn main() -> Wibble {
  let _: mod.Wibble = todo
}
",
        find_position_of("mod.Wibble").under_char('W'),
    );
}

#[test]
fn higlights_for_prefix_string_suffix_variable_in_case() {
    assert_highlights!(
        "
pub fn main() -> String {
  let wibble = \"1-wibble\"
  let rest = case wibble {
    \"1\" <> rest -> rest
    other -> other
  }
  rest
}
",
        find_position_of("rest").nth_occurrence(2)
    );
}

#[test]
fn higlights_for_prefix_string_suffix_variable_in_case_triggered_from_usage() {
    assert_highlights!(
        "
pub fn main() -> String {
  let wibble = \"1-wibble\"
  let rest = case wibble {
    \"1\" <> rest -> rest
    other -> other
  }
  rest
}
",
        find_position_of("rest").nth_occurrence(3)
    );
}

#[test]
fn higlights_for_prefix_string_suffix_variable_with_alternative_definition_in_case() {
    assert_highlights!(
        "
pub fn main() -> String {
  let wibble = \"1-wibble\"
  let rest = case wibble {
    \"1\" <> rest | \"2\" <> rest -> rest
    other -> other
  }
  rest
}
",
        find_position_of("rest").nth_occurrence(2),
    );
}

#[test]
fn higlights_for_prefix_string_suffix_variable_with_alternative_definition_triggered_from_second_pattern()
 {
    assert_highlights!(
        "
pub fn main() -> String {
  let wibble = \"1-wibble\"
  let rest = case wibble {
    \"1\" <> rest | \"2\" <> rest -> rest
    other -> other
  }
  rest
}
",
        find_position_of("rest").nth_occurrence(3),
    );
}

#[test]
fn higlights_for_prefix_string_suffix_variable_with_alternative_definition_triggered_from_usage() {
    assert_highlights!(
        "
pub fn main() -> String {
  let wibble = \"1-wibble\"
  let rest = case wibble {
    \"1\" <> rest | \"2\" <> rest -> rest
    other -> other
  }
  rest
}
",
        find_position_of("rest").nth_occurrence(4),
    );
}

#[test]
fn higlights_for_prefix_string_suffix_variable_in_let_assert() {
    assert_highlights!(
        "
pub fn main() -> String {
  let assert \"1\" <> rest = \"1-wibble\"
  rest
}
",
        find_position_of("rest").nth_occurrence(1),
    );
}

#[test]
fn higlights_for_prefix_string_suffix_variable_in_let_assert_triggered_from_usage() {
    assert_highlights!(
        "
pub fn main() -> String {
  let assert \"1\" <> rest = \"1-wibble\"
  rest
}
",
        find_position_of("rest").nth_occurrence(2),
    );
}

#[test]
fn higlights_for_prefix_string_alias_in_case() {
    assert_highlights!(
        "
pub fn main() -> String {
  let wibble = \"1-wibble\"
  let digit = case wibble {
    \"1\" as digit <> _rest -> digit
    other -> other
  }
  digit
}
",
        find_position_of("digit").nth_occurrence(2)
    );
}

#[test]
fn higlights_for_prefix_string_alias_in_case_triggered_from_usage() {
    assert_highlights!(
        "
pub fn main() -> String {
  let wibble = \"1-wibble\"
  let digit = case wibble {
    \"1\" as digit <> _rest -> digit
    other -> other
  }
  digit
}
",
        find_position_of("digit").nth_occurrence(3)
    );
}

#[test]
fn higlights_for_prefix_string_alias_with_alternative_definitions_in_case() {
    assert_highlights!(
        "
pub fn main() -> String {
  let wibble = \"1-wibble\"
  let digit = case wibble {
    \"1\" as digit <> _rest | \"2\" as digit <> _rest -> digit
    other -> other
  }
  digit
}
",
        find_position_of("digit").nth_occurrence(2)
    );
}

#[test]
fn higlights_for_prefix_string_alias_with_alternative_definitions_triggered_from_second_pattern() {
    assert_highlights!(
        "
pub fn main() -> String {
  let wibble = \"1-wibble\"
  let digit = case wibble {
    \"1\" as digit <> _rest | \"2\" as digit <> _rest -> digit
    other -> other
  }
  digit
}
",
        find_position_of("digit").nth_occurrence(3)
    );
}

#[test]
fn higlights_for_prefix_string_alias_with_alternative_definitions_triggered_from_usage() {
    assert_highlights!(
        "
pub fn main() -> String {
  let wibble = \"1-wibble\"
  let digit = case wibble {
    \"1\" as digit <> _rest | \"2\" as digit <> _rest -> digit
    other -> other
  }
  digit
}
",
        find_position_of("digit").nth_occurrence(4)
    );
}

#[test]
fn higlights_for_prefix_string_alias_in_let_assert() {
    assert_highlights!(
        "
pub fn main() -> String {
  let assert \"1\" as digit <> _rest = \"1-wibble\"
  digit
}
",
        find_position_of("digit").nth_occurrence(1)
    );
}

#[test]
fn higlights_for_prefix_string_alias_in_let_assert_triggered_from_usage() {
    assert_highlights!(
        "
pub fn main() -> String {
  let assert \"1\" as digit <> _rest = \"1-wibble\"
  digit
}
",
        find_position_of("digit").nth_occurrence(2)
    );
}

#[test]
fn higlights_for_prefix_string_suffix_variable_nested_in_tuple() {
    assert_highlights!(
        "
fn main() {
  case #(\"1-wibble\", 0) {
    #(\"1\" <> rest, _) -> rest
    _ -> \"\"
  }
}
",
        find_position_of("rest").nth_occurrence(1)
    );
}

#[test]
fn higlights_for_prefix_string_alias_used_in_guard() {
    assert_highlights!(
        "
fn main() {
  case \"1-wibble\" {
    \"1\" as digit <> _rest if digit == \"1\" -> digit
    _ -> \"\"
  }
}
",
        find_position_of("digit").nth_occurrence(1)
    );
}

#[test]
fn higlights_for_prefix_string_suffix_used_in_guard() {
    assert_highlights!(
        "
fn main() {
  case \"1-wibble\" {
    \"1\" <> rest if rest == \"-wibble\" -> rest
    _ -> \"\"
  }
}
",
        find_position_of("rest").nth_occurrence(1)
    );
}

#[test]
fn higlights_for_prefix_string_suffix_shadowing_outer_variable() {
    assert_highlights!(
        "
fn main() {
  let rest = \"outer\"
  case \"1-wibble\" {
    \"1\" <> rest -> rest
    _ -> rest
  }
}
",
        find_position_of("rest").nth_occurrence(2)
    );
}

#[test]
fn higlights_for_prefix_string_alias_and_suffix_complex_guard() {
    assert_highlights!(
        "
fn main() {
  case \"1-wibble\" {
    \"1\" as digit <> rest if digit == \"1\" && rest == \"-wibble\" -> #(digit, rest)
    _ -> #(\"\", \"\")
  }
}
",
        find_position_of("digit").nth_occurrence(1)
    );
}

#[test]
fn higlights_for_local_variable_from_guard() {
    assert_highlights!(
        "
pub fn main() {
  let wibble = True
  let wobble = False
  case wibble {
    True if wobble -> !wibble
    False if !wobble -> wibble
    _ -> wobble
  }
}
",
        find_position_of("wobble").nth_occurrence(2).under_char('o')
    );
}

#[test]
fn higlights_for_module_select_from_guard() {
    assert_highlights!(
        ("mod", "pub const wibble = 10"),
        "
import mod

pub fn main() {
  let wibble = True
  case wibble {
    True if mod.wibble < 5 -> !wibble
    False if mod.wibble != 10 -> wibble
    _ -> mod.wibble + 1
  }
}
",
        find_position_of("mod.wibble").under_char('w')
    );
}
