use lsp_types::{
    CodeActionContext, CodeActionParams, CodeActionTriggerKind, PartialResultParams, Position,
    Range, TextDocumentIdentifier, Url, WorkDoneProgressParams,
};

use super::*;

macro_rules! assert_code_action {
    ($src:expr, $position_start:expr, $position_end:expr) => {
        assert_code_action!($src, $position_start, $position_end, true);
    };
    ($src:expr, $position_start:expr, $position_end:expr, $codeaction_is_to_expected:expr) => {
        let result = convert_to_pipeline(
            $src,
            $position_start,
            $position_end,
            $codeaction_is_to_expected,
        );
        insta::assert_snapshot!(insta::internals::AutoName, result, $src);
    };
}

fn convert_to_pipeline(
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

            pub fn map2(
                list1: List(a),
                list2: List(b),
                with fun: fn(a, b) -> c,
              ) -> List(c) {
                do_map2(list1, list2, fun, [])
              }

              fn do_map2(
                list1: List(a),
                list2: List(b),
                fun: fn(a, b) -> c,
                acc: List(c),
              ) -> List(c) {
                case list1, list2 {
                  [], _ | _, [] -> reverse(acc)
                  [a, ..as_], [b, ..bs] -> do_map2(as_, bs, fun, [fun(a, b), ..acc])
                }
            }

            fn do_zip(xs: List(a), ys: List(b), acc: List(#(a, b))) -> List(#(a, b)) {
                case xs, ys {
                  [x, ..xs], [y, ..ys] -> do_zip(xs, ys, [#(x, y), ..acc])
                  _, _ -> reverse(acc)
                }
              }

              pub fn zip(list: List(a), with other: List(b)) -> List(#(a, b)) {
                do_zip(list, other, [])
              }

              pub fn do_stuff(){

              }
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

    let response = engine.action(params).result.unwrap().and_then(|actions| {
        actions
            .into_iter()
            .find(|action| action.title == "Apply Pipeline Rewrite")
    });
    if let Some(action) = response {
        apply_code_action(src, &url, &action)
    } else {
        if codeaction_is_to_expected {
            panic!("No code action produced by the engine")
        } else {
            "No codeaction produced...".into()
        }
    }
}

#[test]
fn test_pipeline_simple() {
    assert_code_action!(
        r#"
import list

fn main() {
  list.reverse([1, 2, 3, 4, 5])
}
"#,
        Position::new(4, 2),
        Position::new(4, 31)
    );
}

#[test]
fn test_converting_assign_to_pipeline() {
    assert_code_action!(
        r#"
import list

fn main() {
  let result = list.reverse(list.zip(list.reverse([1,2,3]), list.reverse([4,5,6])))
}
"#,
        Position::new(4, 15),
        Position::new(4, 63)
    );
}

#[test]
fn test_conversion_to_pipeline_with_call_as_input() {
    assert_code_action!(
        r#"
import list

fn main() {
  let result = list.reverse(list.map(buildlist(), add1))
}

fn buildlist() -> List(Int) {
    [1, 2, 3]
}

fn add1(i: Int) -> Int{
    i + 1
}
"#,
        Position::new(4, 15),
        Position::new(4, 63)
    );
}

#[test]
fn test_converting_expr_to_pipeline() {
    assert_code_action!(
        r#"
import list

fn main() {
  list.reverse(list.zip(list.reverse([1,2,3]), list.reverse([4,5,6])))
}
"#,
        Position::new(4, 2),
        Position::new(4, 47)
    );
}

#[test]
fn test_pipeline_code_action_no_pipeline_when_no_stringification_for_expression() {
    cov_mark::check!(no_stringification_for_expression);
    assert_code_action!(
        r#"
import list

fn main() {
  list.take_while(list.reverse(list.map([1,2,3], fn(x){ x + 1 })), fn(x){x<3})
}
"#,
        Position::new(4, 2),
        Position::new(4, 47),
        false
    );
}

#[test]
fn test_pipeline_code_action_no_pipeline_input_cannot_be_stringified() {
    assert_code_action!(
        r#"
import list

pub fn main() {
  list.reverse([1, 2, 3, 4, 5]
               |> list.reverse())
}
"#,
        Position::new(4, 2),
        Position::new(4, 3),
        false
    );
}

#[test]
fn test_pipeline_code_action_apply_pipeline_locally() {
    assert_code_action!(
        r#"
import list

fn main() {
  list.reverse(list.reverse([1, 2, 3, 4, 5]))
}

"#,
        Position::new(4, 15),
        Position::new(4, 19)
    );
}

#[test]
fn test_pipeline_code_action_no_pipeline_when_no_call_chain() {
    cov_mark::check!(chain_is_empty);
    assert_code_action!(
        r#"
import list

pub fn main() {
  list.do_stuff()
}
"#,
        Position::new(4, 2),
        Position::new(4, 11),
        false
    );
}

#[test]
fn test_pipeline_code_action_no_pipeline_for_empty_call_chain_as_part_of_pipeline() {
    cov_mark::check!(empty_call_chain_as_part_of_pipeline);
    assert_code_action!(
        r#"
import list

pub fn main() {
  list.reverse([1, 2, 3, 4, 5]
               |> list.reverse())
}
"#,
        Position::new(5, 18),
        Position::new(5, 19),
        false
    );
}
