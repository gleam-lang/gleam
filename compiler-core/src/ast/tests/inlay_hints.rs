use super::*;
use crate::{
    ast::inlay_hints::{get_inlay_hints, InlayHint},
    line_numbers::LineNumbers,
};

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

    assert_inlay_hints(src, vec![]);
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

    assert_inlay_hints(
        src,
        vec![
            InlayHint {
                label: "a".to_string(),
                offset: index_of_end(src, "|> f1()"),
            },
            InlayHint {
                label: "a".to_string(),
                offset: index_of_end(src, "|> f2()"),
            },
            InlayHint {
                label: "a".to_string(),
                offset: index_of_end(src, "|> f3()"),
            },
            InlayHint {
                label: "a".to_string(),
                offset: index_of_end(src, "|> f4()"),
            },
        ],
    );
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

    assert_inlay_hints(
        src,
        vec![
            InlayHint {
                label: "Int".to_string(),
                offset: index_of_end(src, "  int_val"),
            },
            InlayHint {
                label: "String".to_string(),
                offset: index_of_end(src, "|> ret_str()"),
            },
            InlayHint {
                label: "String".to_string(),
                offset: index_of_end(src, "|> identity()"),
            },
        ],
    );
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

    assert_inlay_hints(
        src,
        vec![
            InlayHint {
                label: "Int".to_string(),
                offset: index_of_end(src, "  int_val"),
            },
            InlayHint {
                label: "Int".to_string(),
                offset: index_of_end(src, "|> identity()"),
            },
        ],
    );
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

    assert_inlay_hints(
        src,
        vec![
            InlayHint {
                label: "Int".to_string(),
                offset: index_of_end(src, "   int_val"),
            },
            InlayHint {
                label: "Int".to_string(),
                offset: index_of_end(src, "|> identity()"),
            },
        ],
    );
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

    assert_inlay_hints(
        src,
        vec![
            InlayHint {
                label: "Int".to_string(),
                offset: index_of_end(src, "  int_val"),
            },
            InlayHint {
                label: "Int".to_string(),
                offset: index_of_end(src, "|> identity()"),
            },
        ],
    );
}

#[test]
fn test_index_of() {
    let src = r#"a[Z]c"#;
    assert_eq!(index_of_end(src, "[Z]"), 4);
}

fn index_of_end(src: &str, search_for: &str) -> u32 {
    let lookup = src.find(search_for).expect("Expected to find lookup");
    (lookup + search_for.len()) as u32
}

fn assert_inlay_hints(src: &str, expected_inlay_hints: Vec<InlayHint>) {
    let typed_module = compile_module(src);
    let line_numbers = LineNumbers::new(src);
    let inlay_hints = get_inlay_hints(typed_module, &line_numbers);

    assert_eq!(inlay_hints, expected_inlay_hints);
}
