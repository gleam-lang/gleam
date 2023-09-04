use crate::line_numbers::LineNumbers;
use lsp_types::{
    CodeActionContext, CodeActionParams, Diagnostic, PartialResultParams, Position, Range,
    TextDocumentIdentifier, Url, WorkDoneProgressParams, WorkspaceEdit,
};

use super::*;

fn remove_unused_action(src: &str) -> String {
    do_remove_unused_action(src).unwrap_or_else(|| src.into())
}

fn do_remove_unused_action(src: &str) -> Option<String> {
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

    let path = Utf8PathBuf::from(if cfg!(target_family = "windows") {
        r#"\\?\C:\src\app.gleam"#
    } else {
        "/src/app.gleam"
    });

    let url = Url::from_file_path(path).unwrap();

    let diag = Diagnostic {
        range: Range::new(Position::new(0, 0), Position::new(0, 0)),
        severity: None,
        code: None,
        code_description: None,
        source: None,
        message: "Unused".to_string(),
        related_information: None,
        tags: None,
        data: None,
    };

    let params = CodeActionParams {
        text_document: TextDocumentIdentifier::new(url),
        context: CodeActionContext {
            diagnostics: vec![diag],
            only: None,
        },
        range: Range::new(Position::new(0, 0), Position::new(0, 0)),
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: PartialResultParams {
            partial_result_token: None,
        },
    };
    let response = engine.action(params).result.unwrap();
    if let Some(action) = response {
        let action = action.iter().next()?;
        Some(apply_code_action(src, action))
    } else {
        None
    }
}

fn apply_code_action(src: &str, action: &lsp_types::CodeAction) -> String {
    let mut result = src.to_string();
    let line_numbers = LineNumbers::new(src);
    let mut offset = 0;
    match &action.edit {
        Some(WorkspaceEdit { changes, .. }) => match changes {
            Some(changes) => {
                for change in changes.values() {
                    for edit in change {
                        let start = line_numbers
                            .byte_index(edit.range.start.line, edit.range.start.character)
                            - offset;
                        let end = line_numbers
                            .byte_index(edit.range.end.line, edit.range.end.character)
                            - offset;
                        let range = (start as usize)..(end as usize);
                        offset += end - start;
                        result.replace_range(range, &edit.new_text);
                    }
                }
            }
            None => (),
        },
        _ => (),
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
    assert_eq!(remove_unused_action(code), expected.to_string())
}

#[ignore] // This need https://github.com/gleam-lang/gleam/pull/2327
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
import result.{is_ok}

pub fn main() {
  is_ok
}
";
    assert_eq!(remove_unused_action(code), expected.to_string())
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
