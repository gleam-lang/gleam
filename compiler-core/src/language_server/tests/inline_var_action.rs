use lsp_types::{
    CodeActionContext, CodeActionParams, PartialResultParams, Position, Range,
    TextDocumentIdentifier, Url, WorkDoneProgressParams,
};

use super::*;

macro_rules! assert_code_action {
    ($src:expr, $position_start:expr, $position_end:expr) => {
        assert_code_action!($src, $position_start, $position_end, true);
    };
    ($src:expr, $position_start:expr, $position_end:expr, $codeaction_is_to_expected:expr) => {
        let result = inline_variable_refactor(
            $src,
            $position_start,
            $position_end,
            $codeaction_is_to_expected,
        );
        insta::assert_snapshot!(insta::internals::AutoName, result, $src);
    };
}

fn inline_variable_refactor(
    src: &str,
    position_start: Position,
    position_end: Position,
    codeaction_is_to_expected: bool,
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

    // let response = engine.action(params).result.unwrap().and_then(|actions| {
    //     actions
    //         .into_iter()
    //         .filter_map(|action| {
    //             if action.title == "Inline Variable Refactor" {
    //                 Some(action)
    //             } else {
    //                 None
    //             }
    //         })
    //         .last()
    // });

    if let Some(action) = response {
        apply_code_action(src, &url, &action)
    } else {
        if codeaction_is_to_expected {
            panic!("No code action produced by the engine")
        } else {
            "No codeaction produced, check if mark is hit...".into()
        }
    }
}

//Inline Let
#[test]
fn test_inline_local_var_into_multiple_call_let() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = [1,2,3]
  let y = list.reverse(x)
  let z = list.reverse(x)
  let u = list.reverse(x)
  let v = list.reverse(x)
}
    "#,
        Position::new(4, 6),
        Position::new(4, 7)
    );
}

#[test]
fn test_inline_local_var_into_pipeline_let() {
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
        Position::new(4, 6),
        Position::new(4, 7)
    );
}

#[test]
fn test_inline_local_var_into_bin_op_let() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = 1
  let y = x + 1
}
    "#,
        Position::new(4, 6),
        Position::new(4, 7)
    );
}

#[test]
fn test_inline_local_var_into_tuple_let() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = #(1, 2, 3)
  let res = #(x, #(4, 5, 6))
}
    "#,
        Position::new(4, 6),
        Position::new(4, 7)
    );
}

#[test]
fn test_inline_local_var_inside_block_let() {
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
        Position::new(5, 8),
        Position::new(5, 9)
    );
}

#[test]
fn test_inline_local_var_inside_block_with_let_outside_block_let() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = [1, 2, 3]
  { list.reverse(x) }
}
    "#,
        Position::new(4, 6),
        Position::new(4, 7)
    );
}

#[test]
fn test_inline_local_var_into_list_let() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = 2
  let y = [1, x, 3]
}
    "#,
        Position::new(4, 6),
        Position::new(4, 7)
    );
}

#[test]
fn test_inline_local_var_into_fn_body_let() {
    assert_code_action!(
        r#"
import list

fn main() {
  let y = 2
  let func = fn(x) { x + y }
  list.map([1, 2, 3], func)
}
    "#,
        Position::new(4, 6),
        Position::new(4, 7)
    );
}

//Different let values
#[test]
fn test_inline_local_var_with_assign_value_bin_op_let() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = 1 + 2
  let y = x + 3
}
    "#,
        Position::new(4, 6),
        Position::new(4, 7)
    );
}

#[test]
fn test_inline_local_var_with_assign_val_tuple_let() {
    assert_code_action!(
        r#"
import list

fn main() {
  let y = #(1, 2, 3)
  let z = #(y, #(3, 4, 5))
}
        "#,
        Position::new(4, 6),
        Position::new(4, 7)
    );
}

#[test]
fn test_inline_local_var_with_assign_value_fn_call_let() {
    assert_code_action!(
        r#"
import list

fn main() {
  let q = list.reverse([1, 2, 3])
  let z = list.reverse(q)
}
    "#,
        Position::new(4, 6),
        Position::new(4, 7)
    );
}

#[test]
fn test_inline_local_var_with_assign_value_var_op_let() {
    assert_code_action!(
        r#"
import list

fn main() {
  let u = 1 + 2
  let y = u
  let z = y + 4
}
        "#,
        Position::new(5, 6),
        Position::new(5, 7)
    );
}

#[test]
fn test_inline_local_var_into_expression_let() {
    assert_code_action!(
        r#"
import list

fn main() {
  let x = [1, 2, 3]
  list.reverse(x)
}
    "#,
        Position::new(4, 6),
        Position::new(4, 7)
    );
}

// Inline Usage
#[test]
fn test_inline_local_var_no_delete_let_usage() {
    cov_mark::check!(do_not_delete_let);
    assert_code_action!(
        r#"
import list

fn main() {
  let x = [1, 2, 3]
  list.reverse(x)
  list.reverse(x)
}
    "#,
        Position::new(5, 15),
        Position::new(5, 16)
    );
}

#[test]
fn test_inline_local_var_func_arg_usage() {
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
fn test_inline_local_var_bin_op_usage() {
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
fn test_inline_local_var_block_usage() {
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
fn test_inline_local_var_block_outside_usage() {
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
fn test_inline_local_var_list_usage() {
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
fn test_inline_local_var_fn_body_usage() {
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
fn test_inline_local_var_do_not_inline_unused_var() {
    cov_mark::check!(do_not_inline_unused);
    assert_code_action!(
        r#"
    import list

    fn main() {
      let x = 1
      let y = 2
      [1, y, 3]
    }
            "#,
        Position::new(4, 6),
        Position::new(4, 7),
        false
    );
}
