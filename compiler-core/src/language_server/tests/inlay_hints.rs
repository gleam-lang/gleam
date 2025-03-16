use crate::language_server::{
    configuration::{Configuration, InlayHintsConfig},
    tests::{LanguageServerTestIO, TestProject, setup_engine},
};
use lsp_types::{InlayHintParams, Position, Range};

#[test]
fn no_hints_when_same_line() {
    let src = r#"
    fn identity(x) {
      x
    }

    fn ret_str(_x) {
      "abc"
    }

    pub fn example_pipe() {
      0 |> ret_str() |> identity()
    }
"#;

    let hints = pipeline_hints(src);
    insta::assert_snapshot!(hints);
}

#[test]
fn no_hints_when_value_is_literal() {
    let src = r#"
    pub fn ret_str(f1) {
      "abc"
      |> f1()
    }

    pub fn ret_int(f2) {
      42
      |> f2()
    }

    pub fn ret_float(f3) {
      42.2
      |> f3()
    }

    pub fn ret_bit_array(f4) {
      <<1, 2>>
      |> f4()
    }
"#;

    let hints = pipeline_hints(src);
    insta::assert_snapshot!(hints);
}

#[test]
fn show_many_hints() {
    let src = r#"
          const int_val = 0

          fn identity(x) {
            x
          }

          fn ret_str(_x) {
            "abc"
          }

          pub fn example_pipe() {
            int_val
            |> ret_str()
            |> identity()
          }
      "#;

    let hints = pipeline_hints(src);
    insta::assert_snapshot!(hints);
}

#[test]
fn hints_nested_in_case_block() {
    let src = r#"
          const int_val = 0

          fn identity(x) {
            x
          }

          fn main(a) {
            case a {
              _ -> {
                  int_val
                  |> identity()
              }
            }
          }
      "#;

    let hints = pipeline_hints(src);
    insta::assert_snapshot!(hints);
}

#[test]
fn hints_nested_for_apply_fn_let() {
    let src = r#"
          const int_val = 0

          fn identity(x) {
            x
          }

          fn main() {
            let f = identity(fn() {
              int_val
              |> identity()
            })
          }
      "#;

    let hints = pipeline_hints(src);
    insta::assert_snapshot!(hints);
}

#[test]
fn hints_in_use() {
    let src = r#"
          const int_val = 0

          fn identity(x) {
            x
          }

          fn main(f) {
            use a <- f()
            int_val
            |> identity()
          }
      "#;

    let hints = pipeline_hints(src);
    insta::assert_snapshot!(hints);
}

#[test]
fn show_hints_in_params() {
    let src = r#"
      fn example_fn(
        flex_type_arg,
        b: do_not_show_this,
      ) { 0 }
      "#;

    let hints = parameter_hints(src);
    insta::assert_snapshot!(hints);
}

#[test]
fn show_hints_in_return() {
    let src = r#"
      fn example_fn() { 0 }
      "#;

    let hints = return_hints(src);
    insta::assert_snapshot!(hints);
}

#[test]
fn show_correct_type_names_in_functions() {
    let src = r#"
      fn complex(
        x, //: b
        y: rigid_type_var, //: rigid_type_var
      ) { //-> fn(a, rigid_type_var) -> #(b, rigid_type_var, a)
        fn(
          z, //: a
        ) { //-> #(b, rigid_type_var, a)
          #(x, y, z)
        }
      }
      "#;

    let hints = inlay_hints_for_config(
        src,
        InlayHintsConfig {
            function_parameter_types: true,
            function_return_types: true,
            ..Default::default()
        },
    );
    insta::assert_snapshot!(hints);
}

#[test]
fn do_not_show_hints_by_default() {
    let src = r#"
          const int_val = 0

          fn identity(x) {
            x
          }

          fn ret_str(_x) {
            "abc"
          }

          pub fn example_pipe() {
            int_val
            |> ret_str()
            |> identity()
          }
      "#;

    let hints = inlay_hints_for_config(src, InlayHintsConfig::default());
    insta::assert_snapshot!(hints);
}

fn pipeline_hints(src: &str) -> String {
    inlay_hints_for_config(
        src,
        InlayHintsConfig {
            pipelines: true,
            ..Default::default()
        },
    )
}

fn parameter_hints(src: &str) -> String {
    inlay_hints_for_config(
        src,
        InlayHintsConfig {
            function_parameter_types: true,
            ..Default::default()
        },
    )
}

fn return_hints(src: &str) -> String {
    inlay_hints_for_config(
        src,
        InlayHintsConfig {
            function_return_types: true,
            ..Default::default()
        },
    )
}

fn inlay_hints_for_config(src: &str, inlay_hints_config: InlayHintsConfig) -> String {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);
    {
        let mut config = engine.user_config.write().expect("cannot write config");
        *config = Configuration {
            inlay_hints: inlay_hints_config,
            ..Default::default()
        };
    }

    _ = io.src_module("app", src);
    let response = engine.compile_please();
    assert!(response.result.is_ok());

    let params = InlayHintParams {
        text_document: TestProject::build_path(),
        work_done_progress_params: Default::default(),
        range: Range::new(
            Position::new(0, 0),
            Position::new(
                src.lines().count() as u32,
                src.lines().last().unwrap_or_default().len() as u32,
            ),
        ),
    };

    let hints = engine
        .inlay_hints(params)
        .result
        .expect("inlay hint request should not fail");

    let stringified = serde_json::to_string_pretty(&hints).expect("json pprint should not fail");

    stringified
}
