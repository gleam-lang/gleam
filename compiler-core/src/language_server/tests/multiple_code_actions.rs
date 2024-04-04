use lsp_types::{
    CodeActionContext, CodeActionParams, CodeActionTriggerKind, PartialResultParams, Position, Range, TextDocumentIdentifier, Url, WorkDoneProgressParams, WorkspaceEdit
};

use crate::line_numbers::LineNumbers;

use super::*;

macro_rules! assert_code_action {
    ($src:expr, $position_start:expr, $position_end:expr) => {

        let result = return_multiple_code_actions(
            $src,
            $position_start,
            $position_end,
        );
        insta::assert_snapshot!(insta::internals::AutoName, result, $src);
    };
    
}

fn return_multiple_code_actions(
    src: &str,
    position_start: Position,
    position_end: Position,
) -> String {
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
            trigger_kind: Some(CodeActionTriggerKind::INVOKED),
        },
        range: Range::new(position_start, position_end),
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: PartialResultParams {
            partial_result_token: None,
        },
    };

    let codeactions = engine.action(params).result.unwrap().unwrap();
    
    let mut source = src.to_owned();
    for action in codeactions{
        source = apply_code_action(&source, &url, &action).clone();
    }
    
    source.to_owned()
       
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
            let start = line_numbers.byte_index(edit.range.start.line, edit.range.start.character);
            let end = line_numbers.byte_index(edit.range.end.line, edit.range.end.character);
            let range = adjust_code_for_offset(start, end, offset);

            offset += edit.new_text.len() as i32 - (end - start) as i32;
            result.replace_range(range, &edit.new_text);
        }
    }
    result
}

fn adjust_code_for_offset(start: u32, end: u32, offset: i32) -> std::ops::Range<usize> {
    let adjusted_start = if offset.is_negative() {
        start.checked_sub(offset.abs() as u32).unwrap_or(0) as usize
    } else {
        start as usize + offset as usize
    };

    let adjusted_end = if offset.is_positive() {
        end.checked_add(offset as u32).unwrap_or(std::u32::MAX) as usize
    } else {
        end as usize - offset.abs() as usize
    };

    adjusted_start..adjusted_end
}


#[test]
fn test_multiple_code_actions_in_range() {
    assert_code_action!(
        r#"
    import list

    fn main() {
      let x = 1
      let y = [x, 2, 3]
      let u = 2
      let v = [1, u, 3]
      let result = list.reverse([1, 2, 3, 4, 5])  
    }
            "#,
        Position::new(4, 0),
        Position::new(8, 40)
    );
}
