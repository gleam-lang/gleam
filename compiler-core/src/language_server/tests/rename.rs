use std::collections::HashMap;

use lsp_types::{Position, RenameParams, TextDocumentPositionParams, Url, WorkDoneProgressParams};

use crate::language_server::tests::{find_position_of, TestProject};

use super::hover;

fn rename(
    tester: TestProject<'_>,
    new_name: &str,
    position: Position,
) -> Option<lsp_types::WorkspaceEdit> {
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
