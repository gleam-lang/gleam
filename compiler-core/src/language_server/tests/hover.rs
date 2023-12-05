use lsp_types::{
    Hover, HoverContents, HoverParams, MarkedString, Position, Range, TextDocumentIdentifier,
    TextDocumentPositionParams, Url,
};

use super::*;

static PROJECT_NAME: &'static str = "my_project";

fn positioned_hover_with_imports(
    src: &str,
    position: Position,
    modules: HashMap<&str, &str>,
) -> Option<Hover> {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);
    engine.compiler.project_compiler.config.name = PROJECT_NAME.into();

    _ = io.src_module("app", src);
    for (k, src) in modules {
        _ = io.src_module(k, src);
    }

    let response = engine.compile_please();
    assert!(response.result.is_ok());

    let path = Utf8PathBuf::from(if cfg!(target_family = "windows") {
        r"\\?\C:\src\app.gleam"
    } else {
        "/src/app.gleam"
    });

    let url = Url::from_file_path(path).unwrap();

    let params = HoverParams {
        text_document_position_params: TextDocumentPositionParams::new(
            TextDocumentIdentifier::new(url),
            position,
        ),
        work_done_progress_params: Default::default(),
    };
    let response = engine.hover(params);

    response.result.unwrap()
}

fn positioned_hover(src: &str, position: Position) -> Option<Hover> {
    positioned_hover_with_imports(src, position, HashMap::new())
}

#[test]
fn hover_function_definition() {
    let code = "
fn add_2(x) {
  x + 2
}
";

    assert_eq!(
        positioned_hover(code, Position::new(1, 3)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
fn(Int) -> Int
```

View on [hexdocs](https://hexdocs.pm/my_project/app.html#add_2)"
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
        positioned_hover(code, Position::new(6, 3)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
fn() -> Nil
```

View on [hexdocs](https://hexdocs.pm/my_project/app.html#my_fn)"
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

#[test]
fn hover_imported_function() {
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    assert_eq!(
        // hovering over "my_fn"
        positioned_hover_with_imports(
            code,
            Position::new(3, 19),
            HashMap::from([("example_module", "pub fn my_fn() { Nil }")])
        ),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
fn() -> Nil
```

View on [hexdocs](https://hexdocs.pm/my_project/example_module.html#my_fn)"
                    .to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 3,
                    character: 16,
                },
                end: Position {
                    line: 3,
                    character: 22,
                },
            },),
        })
    );
}

#[test]
fn hover_imported_unqualified_function() {
    let code = "
import example_module.{my_fn}
fn main() {
  my_fn
}
";

    assert_eq!(
        // hovering over "my_fn"
        positioned_hover_with_imports(
            code,
            Position::new(3, 5),
            HashMap::from([("example_module", "pub fn my_fn() { Nil }")])
        ),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
fn() -> Nil
```

View on [hexdocs](https://hexdocs.pm/my_project/example_module.html#my_fn)"
                    .to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 3,
                    character: 2,
                },
                end: Position {
                    line: 3,
                    character: 7,
                },
            },),
        })
    );
}

#[test]
fn hover_imported_constants() {
    let code = "
import example_module
fn main() {
  example_module.my_const
}
";

    assert_eq!(
        // hovering over "my_const"
        positioned_hover_with_imports(
            code,
            Position::new(3, 19),
            HashMap::from([("example_module", "pub const my_const = 42")])
        ),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
Int
```

View on [hexdocs](https://hexdocs.pm/my_project/example_module.html#my_const)"
                    .to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 3,
                    character: 16,
                },
                end: Position {
                    line: 3,
                    character: 25,
                },
            },),
        })
    );
}

#[test]
fn hover_imported_unqualified_constants() {
    let code = "
import example_module.{my_const}
fn main() {
  my_const
}
";

    assert_eq!(
        // hovering over "my_const"
        positioned_hover_with_imports(
            code,
            Position::new(3, 5),
            HashMap::from([("example_module", "pub const my_const = 42")])
        ),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
Int
```

View on [hexdocs](https://hexdocs.pm/my_project/example_module.html#my_const)"
                    .to_string()
            )),
            range: Some(Range {
                start: Position {
                    line: 3,
                    character: 2,
                },
                end: Position {
                    line: 3,
                    character: 10,
                },
            },),
        })
    );
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
        positioned_hover(code, Position::new(3, 3)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam
fn(String, String) -> String
```
 Exciting documentation
 Maybe even multiple lines

View on [hexdocs](https://hexdocs.pm/my_project/app.html#append)"
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
        positioned_hover(code, Position::new(3, 10)),
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

    assert_eq!(positioned_hover(code, Position::new(4, 1)), None);
}

#[test]
fn hover_expressions_in_function_body() {
    let code = "
fn append(x, y) {
  x <> y
}
";

    assert_eq!(
        positioned_hover(code, Position::new(2, 2)),
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
        positioned_hover(code, Position::new(3, 6)),
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
        positioned_hover(code, Position::new(8, 6)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("```gleam\nInt\n```".to_string())),
            range: Some(Range::new(Position::new(8, 6), Position::new(8, 7))),
        })
    );

    // hover over `b`
    assert_eq!(
        positioned_hover(code, Position::new(8, 11)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(format!(
                "```gleam
fn(fn(Int) -> String) -> String
```

View on [hexdocs](https://hexdocs.pm/my_project/app.html#b)"
            ))),
            range: Some(Range::new(Position::new(8, 11), Position::new(8, 12))),
        })
    );

    // hover over `c`
    assert_eq!(
        positioned_hover(code, Position::new(9, 2)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam\nString\n```\nA locally defined variable.".to_string()
            )),
            range: Some(Range::new(Position::new(9, 2), Position::new(9, 3))),
        })
    );
}
