use crate::line_numbers::LineNumbers;
use lsp_types::{
    CodeActionContext, CodeActionParams, PartialResultParams, Position, Range,
    TextDocumentIdentifier, Url, WorkDoneProgressParams, WorkspaceEdit,
};

use super::*;

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
    let code = "
import list

fn main() {
  let x = [1,2,3]
  let result = list.reverse(x)
}
";
    let expected = "
import list

fn main() {
  
  let result = list.reverse([1, 2, 3])
}
";

    let position_start = Position::new(5, 0);
    let position_end = Position::new(5, 61);

    assert_eq!(
        inline_variable_refactor(code, position_start, position_end),
        expected
    );
}
#[test]
fn test_inlining_with_multiple_candidates() {
    let code = "
import list

fn main() {
  let x = [1,2,3]
  let result1 = list.reverse(x)
  let y = [4, 5, 6]
  let result2 = list.reverse(y)
}
";
    let expected = "
import list

fn main() {
  let x = [1,2,3]
  let result1 = list.reverse(x)
  
  let result2 = list.reverse([4, 5, 6])
}
";

    let position_start = Position::new(7, 0);
    let position_end = Position::new(7, 61);

    assert_eq!(
        inline_variable_refactor(code, position_start, position_end),
        expected
    );
}
#[test]
fn test_inlining_pipeline() {
    let code = "
import list

fn main() {
  let x = [1,2,3]
  let result =
  x
  |> list.reverse()
  |> list.map(fn(x) { x * 2})
}
";
    let expected = "
import list

fn main() {
  
  let result =
  [1, 2, 3]
  |> list.reverse()
  |> list.map(fn(x) { x * 2})
}
";

    let position_start = Position::new(6, 0);
    let position_end = Position::new(6, 61);

    assert_eq!(
        inline_variable_refactor(code, position_start, position_end),
        expected
    );
}

#[test]
fn test_inlining_bin_op() {
    let code = "
import list

fn main() {
  let x = 1
  let y = x + 1
}
";
    let expected = "
import list

fn main() {
  
  let y = 1 + 1
}
";

    let position_start = Position::new(5, 0);
    let position_end = Position::new(5, 61);

    assert_eq!(
        inline_variable_refactor(code, position_start, position_end),
        expected
    );
}

#[test]
fn test_inlining_tuple() {
    let code = "
import list

fn main() {
  let x = [1, 2, 3]
  #(1, x, 3)
}
";
    let expected = "
import list

fn main() {
  
  #(1, [1, 2, 3], 3)
}
";

    let position_start = Position::new(5, 0);
    let position_end = Position::new(5, 61);

    assert_eq!(
        inline_variable_refactor(code, position_start, position_end),
        expected
    );
}

#[test]
fn test_inlining_bare_expression() {
    let code = "
import list

fn main() {
  let x = [1, 2, 3]
  list.reverse(x)
}
";
    let expected = "
import list

fn main() {
  
  list.reverse([1, 2, 3])
}
";

    let position_start = Position::new(5, 0);
    let position_end = Position::new(5, 61);

    assert_eq!(
        inline_variable_refactor(code, position_start, position_end),
        expected
    );
}
#[test]
fn test_inlining_block_expression() {
    let code = "
import list

fn main() {
  {
    let x = [1, 2, 3]
    list.reverse(x)
  }
}
";
    let expected = "
import list

fn main() {
  {
    
    list.reverse([1, 2, 3])
  }
}
";

    let position_start = Position::new(6, 0);
    let position_end = Position::new(6, 61);

    assert_eq!(
        inline_variable_refactor(code, position_start, position_end),
        expected
    );
}

#[test]
fn test_inlining_block_expression_with_var_outside_block() {
    let code = "
import list

fn main() {
  let x = [1, 2, 3]
  { list.reverse(x) }
}
";
    let expected = "
import list

fn main() {
  
  { list.reverse([1, 2, 3]) }
}
";

    let position_start = Position::new(5, 0);
    let position_end = Position::new(5, 61);

    assert_eq!(
        inline_variable_refactor(code, position_start, position_end),
        expected
    );
}

#[test]
fn test_inlining_list() {
    let code = "
import list

fn main() {
  let x = 2
  let y = [1, x, 3]
}
";
    let expected = "
import list

fn main() {
  
  let y = [1, 2, 3]
}
";

    let position_start = Position::new(5, 0);
    let position_end = Position::new(5, 61);

    assert_eq!(
        inline_variable_refactor(code, position_start, position_end),
        expected
    );
}
#[test]
fn test_inlining_variable_in_fn_body() {
    let code = "
import list

fn main() {
  let y = 2
  let func = fn(x) { x + y } 
  list.map([1, 2, 3], func)
}
";
    let expected = "
import list

fn main() {
  
  let func = fn(x) { x + 2 } 
  list.map([1, 2, 3], func)
}
";

    let position_start = Position::new(5, 0);
    let position_end = Position::new(5, 30);

    assert_eq!(
        inline_variable_refactor(code, position_start, position_end),
        expected
    );
}

#[test]
fn test_inlining_tuple_as_var() {
    let code = "
import list

fn main() {
  let y = #(1, 2, 3)
  let z = #(y, #(3, 4, 5))
}
";
    let expected = "
import list

fn main() {
  
  let z = #(#(1, 2, 3), #(3, 4, 5))
}
";

    let position_start = Position::new(5, 0);
    let position_end = Position::new(5, 30);

    assert_eq!(
        inline_variable_refactor(code, position_start, position_end),
        expected
    );
}

#[test]
fn test_inlining_bin_op_as_var() {
    let code = "
import list

fn main() {
  let x = 1 + 2
  let y = x + 3
}
";
    let expected = "
import list

fn main() {
  
  let y = 1 + 2 + 3
}
";

    let position_start = Position::new(5, 0);
    let position_end = Position::new(5, 30);

    assert_eq!(
        inline_variable_refactor(code, position_start, position_end),
        expected
    );
}