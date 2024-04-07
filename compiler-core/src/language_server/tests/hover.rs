use lsp_types::{Hover, HoverContents, HoverParams, MarkedString, Position, Range};

use super::*;

fn hover(tester: TestProject<'_>, position: Position) -> Option<Hover> {
    tester.at(position, |engine, param, _| {
        let params = HoverParams {
            text_document_position_params: param,
            work_done_progress_params: Default::default(),
        };
        let response = engine.hover(params);

        response.result.unwrap()
    })
}

#[test]
fn hover_function_definition() {
    let code = "
fn add_2(x) {
  x + 2
}
";

    assert_eq!(
        hover(TestProject::for_source(code), Position::new(1, 3)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
fn(Int) -> Int
```
"
                .to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 1,
                    character: 0,
                },
                end: Position {
                    line: 1,
                    character: 11,
                },
            },),
        })
    );
}

#[test]
fn hover_local_function() {
    let code = "
fn my_fn() {
  Nil
}

fn main() {
  my_fn
}
";

    assert_eq!(
        hover(TestProject::for_source(code), Position::new(6, 3)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
fn() -> Nil
```
"
                .to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 6,
                    character: 2,
                },
                end: Position {
                    line: 6,
                    character: 7,
                },
            },),
        })
    );
}

// https://github.com/gleam-lang/gleam/issues/2654
#[test]
fn hover_local_function_in_pipe() {
    let code = "
fn add1(num: Int) -> Int {
  num + 1
}

pub fn main() {
  add1(1)

  1
  |> add1
  |> add1
  |> add1
}
";

    assert_eq!(
        hover(TestProject::for_source(code), Position::new(6, 3)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
fn(Int) -> Int
```
"
                .to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 6,
                    character: 2,
                },
                end: Position {
                    line: 6,
                    character: 6,
                },
            },),
        })
    );
    assert_eq!(
        hover(TestProject::for_source(code), Position::new(9, 7)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
fn(Int) -> Int
```
"
                .to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 9,
                    character: 5,
                },
                end: Position {
                    line: 9,
                    character: 9,
                },
            },),
        })
    );
    assert_eq!(
        hover(TestProject::for_source(code), Position::new(10, 7)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
fn(Int) -> Int
```
"
                .to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 10,
                    character: 5,
                },
                end: Position {
                    line: 10,
                    character: 9,
                },
            },),
        })
    );
    assert_eq!(
        hover(TestProject::for_source(code), Position::new(11, 7)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
fn(Int) -> Int
```
"
                .to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 11,
                    character: 5,
                },
                end: Position {
                    line: 11,
                    character: 9,
                },
            },),
        })
    );
}

#[test]
fn hover_imported_function() {
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    // hovering over "my_fn"
    let hover = hover(
        TestProject::for_source(code).add_module("example_module", "pub fn my_fn() { Nil }"),
        Position::new(3, 19),
    )
    .unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_function() {
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    // hovering over "my_fn"
    let hover = hover(
        TestProject::for_source(code).add_hex_module("example_module", "pub fn my_fn() { Nil }"),
        Position::new(3, 19),
    )
    .unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_unqualified_function() {
    let code = "
import example_module.{my_fn}
fn main() {
  my_fn
}
";

    // hovering over "my_fn"
    let hover = hover(
        TestProject::for_source(code).add_hex_module("example_module", "pub fn my_fn() { Nil }"),
        Position::new(3, 5),
    )
    .unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_function_renamed_module() {
    let code = "
import example_module as renamed_module
fn main() {
    renamed_module.my_fn
}
";

    // hovering over "my_fn"
    let hover = hover(
        TestProject::for_source(code).add_hex_module("example_module", "pub fn my_fn() { Nil }"),
        Position::new(3, 22),
    )
    .unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_unqualified_imported_function_renamed_module() {
    let code = "
import example_module.{my_fn} as renamed_module
fn main() {
    my_fn
}
";

    // hovering over "my_fn"
    let hover = hover(
        TestProject::for_source(code).add_hex_module("example_module", "pub fn my_fn() { Nil }"),
        Position::new(3, 6),
    )
    .unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_function_nested_module() {
    // Example of HexDocs link with nested modules: https://hexdocs.pm/lustre/lustre/element/svg.html
    let code = "
import my/nested/example_module
fn main() {
    example_module.my_fn
}
";

    // hovering over "my_fn"
    let hover = hover(
        TestProject::for_source(code)
            .add_hex_module("my/nested/example_module", "pub fn my_fn() { Nil }"),
        Position::new(3, 22),
    )
    .unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_ffi_renamed_function() {
    let code = r#"
import example_module
fn main() {
    example_module.my_fn
}
"#;

    // hovering over "my_fn"
    let hover = hover(
        TestProject::for_source(code).add_hex_module(
            "example_module",
            r#"
@external(erlang, "my_mod_ffi", "renamed_fn")
pub fn my_fn() -> Nil
"#,
        ),
        Position::new(3, 22),
    )
    .unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_constants() {
    let code = "
import example_module
fn main() {
  example_module.my_const
}
";

    // hovering over "my_const"
    let hover = hover(
        TestProject::for_source(code).add_hex_module("example_module", "pub const my_const = 42"),
        Position::new(3, 19),
    )
    .unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_unqualified_constants() {
    let code = "
import example_module.{my_const}
fn main() {
  my_const
}
";

    // hovering over "my_const"
    let hover = hover(
        TestProject::for_source(code).add_hex_module("example_module", "pub const my_const = 42"),
        Position::new(3, 5),
    )
    .unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_value_with_two_modules_same_name() {
    let code = "
import a/example_module as _
import b/example_module
fn main() {
    example_module.my_const
}
";

    // hovering over "my_const"
    let hover = hover(
        TestProject::for_source(code)
            .add_hex_module("a/example_module", "pub const my_const = 42")
            .add_hex_module("b/example_module", "pub const my_const = 42"),
        Position::new(4, 22),
    )
    .unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_function_with_another_value_same_name() {
    let code = "
import a/example_module.{my_const as discarded}
import b/example_module.{my_const} as _
fn main() {
    my_const
}
";

    // hovering over "my_const"
    let hover = hover(
        TestProject::for_source(code)
            .add_hex_module("a/example_module", "pub const my_const = 42")
            .add_hex_module("b/example_module", "pub const my_const = 42"),
        Position::new(4, 8),
    )
    .unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_function_definition_with_docs() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x, y) {
  x <> y
}
";

    assert_eq!(
        hover(TestProject::for_source(code), Position::new(3, 3)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
fn(String, String) -> String
```
 Exciting documentation
 Maybe even multiple lines
"
                .to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 3,
                    character: 0,
                },
                end: Position {
                    line: 3,
                    character: 15,
                },
            },),
        })
    );
}

#[test]
fn hover_function_argument() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x, y) {
  x <> y
}
";

    assert_eq!(
        hover(TestProject::for_source(code), Position::new(3, 10)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam\nString\n```".to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 3,
                    character: 10,
                },
                end: Position {
                    line: 3,
                    character: 11,
                },
            },),
        })
    );
}

#[test]
fn hover_function_body() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x, y) {
  x <> y
}
";

    assert_eq!(
        hover(TestProject::for_source(code), Position::new(4, 1)),
        None
    );
}

#[test]
fn hover_expressions_in_function_body() {
    let code = "
fn append(x, y) {
  x <> y
}
";

    assert_eq!(
        hover(TestProject::for_source(code), Position::new(2, 2)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
String
```
A locally defined variable."
                    .to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 2,
                    character: 2
                },
                end: Position {
                    line: 2,
                    character: 3
                }
            }),
        })
    );
}

#[test]
fn hover_module_constant() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
const one = 1
";

    assert_eq!(
        hover(TestProject::for_source(code), Position::new(3, 6)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
Int
```
 Exciting documentation
 Maybe even multiple lines
"
                .to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 3,
                    character: 6
                },
                end: Position {
                    line: 3,
                    character: 9
                },
            }),
        })
    );
}

#[test]
fn hover_variable_in_use_expression() {
    let code = "
fn b(fun: fn(Int) -> String) {
  fun(42)
}

fn do_stuff() {
  let c = \"done\"

  use a <- b
  c
}
";

    // hover over `a`
    assert_eq!(
        hover(TestProject::for_source(code), Position::new(8, 6)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("```gleam\nInt\n```".to_string())),
            range: Some(Range::new(Position::new(8, 6), Position::new(8, 7))),
        })
    );

    // hover over `b`
    assert_eq!(
        hover(TestProject::for_source(code), Position::new(8, 11)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam\nfn(fn(Int) -> String) -> String\n```\n".to_string()
            )),
            range: Some(Range::new(Position::new(8, 11), Position::new(8, 12))),
        })
    );

    // hover over `c`
    assert_eq!(
        hover(TestProject::for_source(code), Position::new(9, 2)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam\nString\n```\nA locally defined variable.".to_string()
            )),
            range: Some(Range::new(Position::new(9, 2), Position::new(9, 3))),
        })
    );
}
