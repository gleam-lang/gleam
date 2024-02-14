use crate::line_numbers::LineNumbers;
use lsp_types::{
    CodeActionContext, CodeActionParams, PartialResultParams, Position, Range,
    TextDocumentIdentifier, Url, WorkDoneProgressParams, WorkspaceEdit,
};

use super::*;

macro_rules! assert_code_action {
    ($src:expr, $position_start:expr, $position_end:expr) => {
        let result = inline_variable_refactor($src, $position_start, $position_end);
        insta::assert_snapshot!(insta::internals::AutoName, result, $src);
    };
}

fn inline_variable_refactor(src: &str, position_start: Position, position_end: Position) -> String {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    _ = io.src_module(
        "list",
        r#"
            pub fn map(list: List(a), with fun: fn(a) -> b) -> List(b) {
                do_map(list, fun, [])
            }
            fn do_map(list: List(a), fun: fn(a) -> b, acc: List(b)) -> List(b) {
                case list {
                    [] -> reverse(acc)
                    [x, ..xs] -> do_map(xs, fun, [fun(x), ..acc])
                }
            }
            pub fn take_while(
                in list: List(a),
                satisfying predicate: fn(a) -> Bool,
              ) -> List(a) {
                do_take_while(list, predicate, [])
            }
            fn do_take_while(
                list: List(a),
                predicate: fn(a) -> Bool,
                acc: List(a),
              ) -> List(a) {
                case list {
                  [] -> reverse(acc)
                  [first, ..rest] ->
                    case predicate(first) {
                      True -> do_take_while(rest, predicate, [first, ..acc])
                      False -> reverse(acc)
                    }
                }
              }
            
            pub fn reverse(xs: List(a)) -> List(a) {
                do_reverse(xs)
            }
            
            fn do_reverse(list) {
                do_reverse_acc(list, [])
            }
            
            fn do_reverse_acc(remaining, accumulator) {
                case remaining {
                    [] -> accumulator
                    [item, ..rest] -> do_reverse_acc(rest, [item, ..accumulator])
                }
            }
            pub fn is_ok() {}
        "#,
    );

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
        range: Range::new(position_start, position_end),
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: PartialResultParams {
            partial_result_token: None,
        },
    };

    let response = engine.action(params).result.unwrap().and_then(|actions| {
        actions
            .into_iter()
            .find(|action| action.title == "Inline Variable Refactor")
    });
    if let Some(action) = response {
        apply_code_action(src, &url, &action)
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
            offset += end - start;
            result.replace_range(range, &edit.new_text);
        }
    }
    result
}

#[test]
fn test_inlining_func_arg() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = [1,2,3]
  let result = list.reverse(x)
}
    "#,
        Position::new(5, 28),
        Position::new(5, 29)
    );
}

#[test]
fn test_inlining_with_multiple_candidates() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = [1,2,3]
  let result1 = list.reverse(x)
  let y = [4, 5, 6]
  let result2 = list.reverse(y)
}
        "#,
        Position::new(7, 29),
        Position::new(7, 30)
    );
}

#[test]
fn test_inlining_pipeline() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = [1,2,3]
  let result =
  x
  |> list.reverse()
  |> list.map(fn(x) { x * 2})
}
        "#,
        Position::new(6, 2),
        Position::new(6, 3)
    );
}

#[test]
fn test_inlining_bin_op() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = 1
  let y = x + 1
}
        "#,
        Position::new(5, 10),
        Position::new(5, 11)
    );
}

#[test]
fn test_inlining_tuple() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = [1, 2, 3]
  #(1, x, 3)
}
        "#,
        Position::new(5, 7),
        Position::new(5, 8)
    );
}
#[test]
fn test_inlining_into_expression() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = [1, 2, 3]
  list.reverse(x)
}
        "#,
        Position::new(5, 15),
        Position::new(5, 16)
    );
}

#[test]
fn test_inlining_block_expression() {
    assert_code_action!(
        r#"
import list

fn main() {
  {
    let x = [1, 2, 3]
    list.reverse(x)
  }
}
        "#,
        Position::new(6, 17),
        Position::new(6, 18)
    );
}

#[test]
fn test_inlining_block_expression_with_var_outside_block() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = [1, 2, 3]
  { list.reverse(x) }
}
        "#,
        Position::new(5, 17),
        Position::new(5, 18)
    );
}

#[test]
fn test_inlining_list() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = 2
  let y = [1, x, 3]
}
        "#,
        Position::new(5, 14),
        Position::new(5, 15)
    );
}

#[test]
fn test_inlining_variable_in_fn_body() {
    assert_code_action!(
        r#"
import list

fn main() {
  let y = 2
  let func = fn(x) { x + y } 
  list.map([1, 2, 3], func)
}
        "#,
        Position::new(5, 25),
        Position::new(5, 26)
    );
}

#[test]
fn test_inlining_tuple_as_var() {
    assert_code_action!(
        r#"
import list

fn main() {
  let y = #(1, 2, 3)
  let z = #(y, #(3, 4, 5))
}
        "#,
        Position::new(5, 12),
        Position::new(5, 13)
    );
}

#[test]
fn test_inlining_bin_op_as_var() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = 1 + 2
  let y = x + 3
}
        "#,
        Position::new(5, 10),
        Position::new(5, 11)
    );
}

#[test]
fn test_inlining_fn_call() {
    assert_code_action!(
        r#"
import list

fn main() {
  let q = list.reverse([1, 2, 3])
  let z = list.reverse(q)
}
        "#,
        Position::new(5, 23),
        Position::new(5, 24)
    );
}

#[test]
fn test_inlining_other_variable() {
    assert_code_action!(
        r#"
import list

fn main() {
  let u = 1 + 2
  let y = u + 3
  let z = y + 4
}
        "#,
        Position::new(6, 10),
        Position::new(6, 11)
    );
}
