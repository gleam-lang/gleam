use crate::line_numbers::LineNumbers;
use lsp_types::{
    CodeActionContext, CodeActionParams, PartialResultParams, Position, Range,
    TextDocumentIdentifier, Url, WorkDoneProgressParams, WorkspaceEdit,
};

use super::*;

// Add the code action to the src
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
            offset += end - start;
            result.replace_range(range, &edit.new_text);
        }
    }
    result
}

fn remove_unused_action(src: &str, line: u32) -> String {
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

    // create the code action request
    let path = Utf8PathBuf::from(if cfg!(target_family = "windows") {
        r"\\?\C:\src\app.gleam"
    } else {
        "/src/app.gleam"
    });

    let url = Url::from_file_path(path).unwrap();

    let params = CodeActionParams {
        text_document: TextDocumentIdentifier::new(url.clone()),
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

    // find the remove unused action response
    let response = engine.action(params).result.unwrap().and_then(|actions| {
        actions
            .into_iter()
            .find(|action| action.title == "Remove unused imports")
    });
    if let Some(action) = response {
        apply_code_action(src, &url, &action)
    } else {
        panic!("No code action produced by the engine")
    }
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
    assert_eq!(remove_unused_action(code, 2), expected.to_string())
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
        remove_unused_action(code, 2),
        expected.replace("%SPACE%", " ")
    )
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

fn add_annotation_actions(src: &str, lines: Vec<u32>) -> String {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    _ = io.src_module("app", src);
    engine.compile_please().result.expect("compiled");

    let path = Utf8PathBuf::from("/src/app.gleam");
    let url = Url::from_file_path(path).unwrap();

    let mut result = src.to_string();
    for line in lines {
        let params = CodeActionParams {
            text_document: TextDocumentIdentifier::new(url.clone()),
            context: CodeActionContext {
                diagnostics: vec![],
                only: None,
                trigger_kind: None,
            },
            range: Range::new(Position::new(line, 0), Position::new(line, std::u32::MAX)),
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
        };

        if let Ok(Some(actions)) = engine.action(params).result {
            for action in actions {
                if action.title.contains("annotation") {
                    result = apply_code_action(&result, &url, &action);
                    // Assuming each action can be applied without conflict
                }
            }
        }
    }

    if src == result {
        panic!("No code action produced by the engine that modifies the source")
    }

    result
}

#[test]
fn test_add_assignment_annotation_simple() {
    let code = "
pub fn main() {
  let n = 1
}
";
    let expected = "
pub fn main() {
  let n: Int = 1
}
";
    assert_eq!(add_annotation_actions(code, vec![2]), expected.to_string())
}

// Kind of just throwing the kitchen sink at this one, better than 10 individual
// tests
#[test]
fn test_add_annotation_complex() {
    let code = "
pub type Point {
  Point(x: Int, y: Int)
}

pub fn create_point() {
  let x = 2
  let y = -8
  let _data = #(1234, \"Hello\")
  let _point = Point(x, y)
  let point2 = Point(y, x)
  point2
}
";

    let expected = "
pub type Point {
  Point(x: Int, y: Int)
}

pub fn create_point() -> Point {
  let x: Int = 2
  let y: Int = -8
  let _data: #(Int, String) = #(1234, \"Hello\")
  let _point: Point = Point(x, y)
  let point2: Point = Point(y, x)
  point2
}
";
    assert_eq!(
        add_annotation_actions(code, vec![5, 6, 7, 8, 9, 10]),
        expected.to_string()
    )
}
