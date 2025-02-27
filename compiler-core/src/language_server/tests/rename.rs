use std::collections::HashMap;

use lsp_types::{Position, RenameParams, TextDocumentPositionParams, Url, WorkDoneProgressParams};

use crate::language_server::tests::{TestProject, find_position_of};

use super::hover;

fn rename(
    tester: TestProject<'_>,
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

fn apply_rename(tester: TestProject<'_>, new_name: &str, position: Position) -> String {
    let src = tester.src;
    let changes = rename(tester, new_name, position)
        .expect("Rename failed")
        .changes
        .expect("No text edit found");
    apply_code_edit(src, changes)
}

fn apply_code_edit(src: &str, changes: HashMap<Url, Vec<lsp_types::TextEdit>>) -> String {
    let mut result = src.to_string();
    for (_, change) in changes {
        result = super::apply_code_edit(result.as_str(), change);
    }
    result
}

macro_rules! assert_rename {
    ($code:literal, $new_name:literal, $range:expr $(,)?) => {
        let project = TestProject::for_source($code);
        assert_rename!(project, $new_name, $range);
    };

    ($project:expr, $new_name:literal, $range:expr $(,)?) => {
        let src = $project.src;
        let range = $range.find_range(src);
        let result = apply_rename($project, $new_name, range.start);
        let output = format!(
            "----- BEFORE RENAME\n{}\n\n----- AFTER RENAME\n{}",
            hover::show_hover(src, range, range.start),
            result
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, src);
    };
}

macro_rules! assert_no_rename {
    ($code:literal, $new_name:literal, $range:expr $(,)?) => {
        let project = TestProject::for_source($code);
        assert_no_rename!(project, $new_name, $range);
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
