use std::assert_eq;

use crate::{language_server::engine, line_numbers::LineNumbers};
use lsp_types::{
    CodeActionContext, CodeActionParams, PartialResultParams, Position, Range,
    TextDocumentIdentifier, Url, WorkDoneProgressParams, WorkspaceEdit,
};

use super::*;

const TEST_FILE_PATH: &str = match cfg!(target_family = "windows") {
    true => r"\\?\C:\src\app.gleam",
    false => "/src/app.gleam",
};

fn test_file_url() -> Url {
    Url::from_file_path(Utf8PathBuf::from(TEST_FILE_PATH)).expect("file path is valid url")
}

fn engine_response(src: &str, line: u32) -> engine::Response<Option<Vec<lsp_types::CodeAction>>> {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    // inject stdlib stubs
    _ = io.src_module("list", "");
    _ = io.src_module(
        "result",
        "pub fn is_ok() {}\npub fn is_err() {}\npub fn all() {}",
    );
    _ = io.src_module("map", "pub type Map(key, value)\npub fn delete() {}");
    _ = io.src_module("option", "");

    _ = io.src_module("app", src);
    engine.compile_please().result.expect("compiled");

    let params = CodeActionParams {
        text_document: TextDocumentIdentifier::new(test_file_url()),
        context: CodeActionContext {
            diagnostics: vec![],
            only: None,
            trigger_kind: None,
        },
        range: Range::new(Position::new(0, 0), Position::new(line + 1, 0)),
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: PartialResultParams {
            partial_result_token: None,
        },
    };

    engine.action(params)
}

const REMOVE_UNUSED_IMPORTS_TITLE: &str = "Remove unused imports";
const REMOVE_REDUNDANT_TUPLES: &str = "Remove redundant tuples";

fn apply_first_code_action_with_title(src: &str, line: u32, title: &str) -> String {
    let response = engine_response(src, line)
        .result
        .unwrap()
        .and_then(|actions| actions.into_iter().find(|action| action.title == title));
    if let Some(action) = response {
        apply_code_action(src, &test_file_url(), &action)
    } else {
        panic!("No code action produced by the engine")
    }
}

fn apply_code_action(src: &str, url: &Url, action: &lsp_types::CodeAction) -> String {
    match &action.edit {
        Some(WorkspaceEdit { changes, .. }) => match changes {
            Some(changes) => apply_code_edit(src, url, changes),
            None => panic!("No text edit found"),
        },
        _ => panic!("No workspace edit found"),
    }
}

// This function replicates how the text editor applies TextEdit
fn apply_code_edit(
    src: &str,
    url: &Url,
    changes: &HashMap<Url, Vec<lsp_types::TextEdit>>,
) -> String {
    let mut result = src.to_string();
    let line_numbers = LineNumbers::new(src);
    let mut offset = 0;
    for (change_url, change) in changes {
        if url != change_url {
            panic!("Unknown url {}", change_url)
        }
        for edit in change {
            let start =
                line_numbers.byte_index(edit.range.start.line, edit.range.start.character) - offset;
            let end =
                line_numbers.byte_index(edit.range.end.line, edit.range.end.character) - offset;
            let range = (start as usize)..(end as usize);
            offset += end - start - edit.new_text.len() as u32;
            result.replace_range(range, &edit.new_text);
        }
    }
    result
}

#[test]
fn test_remove_unused_simple() {
    let code = "
// test
import // comment
  list as lispy
import result
import option

pub fn main() {
  result.is_ok
}
";
    let expected = "
// test
import result

pub fn main() {
  result.is_ok
}
";
    assert_eq!(
        apply_first_code_action_with_title(code, 2, REMOVE_UNUSED_IMPORTS_TITLE),
        expected.to_string()
    )
}

#[test]
fn test_remove_unused_start_of_file() {
    let code = "import option
import result

pub fn main() {
  result.is_ok
}
";
    let expected = "import result

pub fn main() {
  result.is_ok
}
";
    assert_eq!(
        apply_first_code_action_with_title(code, 2, REMOVE_UNUSED_IMPORTS_TITLE),
        expected.to_string()
    )
}

#[test]
fn test_remove_unused_alias() {
    let code = "
// test
import result.{is_ok} as res
import option

pub fn main() {
  is_ok
}
";
    let expected = "
// test
import result.{is_ok}%SPACE%

pub fn main() {
  is_ok
}
";
    assert_eq!(
        apply_first_code_action_with_title(code, 2, REMOVE_UNUSED_IMPORTS_TITLE),
        expected.replace("%SPACE%", " ")
    )
}

#[test]
fn test_remove_redundant_tuple_in_case_subject_simple() {
    let code = "
pub fn main() {
  case #(1) { #(a) -> 0 }
  case #(1, 2) { #(a, b) -> 0 }
}
";

    let expected = "
pub fn main() {
  case 1 { a -> 0 }
  case 1, 2 { a, b -> 0 }
}
";

    assert_eq!(
        apply_first_code_action_with_title(code, 7, REMOVE_REDUNDANT_TUPLES),
        expected
    );
}

#[test]
fn test_remove_redundant_tuple_in_case_subject_nested() {
    let code = "
pub fn main() {
  case #(case #(0) { #(a) -> 0 }) { #(b) -> 0 }
}
";

    let expected = "
pub fn main() {
  case case 0 { a -> 0 } { b -> 0 }
}
";

    assert_eq!(
        apply_first_code_action_with_title(code, 7, REMOVE_REDUNDANT_TUPLES),
        expected
    );
}

#[test]
fn test_remove_redundant_tuple_in_case_retain_extras() {
    let code = "
pub fn main() {
  case
    #(
      // first comment
      1,
      // second comment
      2,
      3 // third comment before comma

      ,

      // fourth comment after comma

    )
  {
    #(
      // first comment
      a,
      // second comment
      b,
      c // third comment before comma

      ,

      // fourth comment after comma

    ) -> 0
  }
}
";

    let result = apply_first_code_action_with_title(code, 20, REMOVE_REDUNDANT_TUPLES);

    insta::assert_snapshot!(result);
}

#[test]
fn test_remove_redundant_tuple_in_case_subject_ignore_empty_tuple() {
    let code = "
pub fn main() {
  case #() { #() -> 0 }
}
";

    assert!(engine_response(code, 11)
        .result
        .expect("ok response")
        .is_none());
}

#[test]
fn test_remove_redundant_tuple_in_case_subject_only_safe_remove() {
    let code = "
pub fn main() {
  case #(0), #(1) {
    #(1), #(b) -> 0
    a, #(0) -> 1 // The first of this clause is not a tuple
    #(a), #(b) -> 2
  }
}
";

    let expected = "
pub fn main() {
  case #(0), 1 {
    #(1), b -> 0
    a, 0 -> 1 // The first of this clause is not a tuple
    #(a), b -> 2
  }
}
";

    assert_eq!(
        apply_first_code_action_with_title(code, 11, REMOVE_REDUNDANT_TUPLES),
        expected
    );
}

/* TODO: implement qualified unused location
#[test]
fn test_remove_unused_qualified_action() {
    let code = "
// test
import map.{Map, delete}
";
    let expected = "
// test

";
    assert_eq!(remove_unused_action(code), expected.to_string())
}

#[test]
fn test_remove_unused_qualified_partial_action() {
    let code = "
// test
import result.{is_ok, is_err}

pub fn main() {
  is_ok
}
";
    let expected = "
// test
import result.{is_ok}

pub fn main() {
  is_ok
}
";
    assert_eq!(remove_unused_action(code), expected.to_string())
}

#[test]
fn test_remove_unused_qualified_partial2_action() {
    let code = "
// test
import result.{all, is_ok, is_err}

pub fn main() {
  is_ok
}
";
    let expected = "
// test
import result.{ is_ok}

pub fn main() {
  is_ok
}
";
    assert_eq!(remove_unused_action(code), expected.to_string())
}

#[test]
fn test_remove_unused_qualified_partial3_action() {
    let code = "
// test
import result.{all, is_ok, is_err} as res

pub fn main() {
  is_ok
}
";
    let expected = "
// test
import result.{ is_ok} as res

pub fn main() {
  is_ok
}
";
    assert_eq!(remove_unused_action(code), expected.to_string())
}
*/
