use lsp_types::{
    Hover, HoverContents, HoverParams, MarkedString, Position, Range, TextDocumentIdentifier,
    TextDocumentPositionParams, Url,
};

use super::*;

fn positioned_with_io(src: &str, position: Position, io: &LanguageServerTestIO) -> Option<Hover> {
    let mut engine = setup_engine(io);

    _ = io.src_module("app", src);
    for package in &io.manifest.packages {
        add_package_from_manifest(&mut engine, package.clone());
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
    positioned_with_io(src, position, &LanguageServerTestIO::new())
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
        positioned_hover(code, Position::new(6, 3)),
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

#[test]
fn hover_imported_function() {
    let io = LanguageServerTestIO::new();
    _ = io.src_module("example_module", "pub fn my_fn() { Nil }");

    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    // hovering over "my_fn"
    let hover = positioned_with_io(code, Position::new(3, 19), &io).unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_function() {
    let mut io = LanguageServerTestIO::new();
    io.add_hex_package("my_dep");
    _ = io.hex_dep_module("my_dep", "example_module", "pub fn my_fn() { Nil }");

    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    // hovering over "my_fn"
    let hover = positioned_with_io(code, Position::new(3, 19), &io).unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_unqualified_function() {
    let mut io = LanguageServerTestIO::new();
    io.add_hex_package("my_dep");
    _ = io.hex_dep_module("my_dep", "example_module", "pub fn my_fn() { Nil }");

    let code = "
import example_module.{my_fn}
fn main() {
  my_fn
}
";

    // hovering over "my_fn"
    let hover = positioned_with_io(code, Position::new(3, 5), &io).unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_function_renamed_module() {
    let mut io = LanguageServerTestIO::new();
    io.add_hex_package("my_dep");
    _ = io.hex_dep_module("my_dep", "example_module", "pub fn my_fn() { Nil }");

    let code = "
import example_module as renamed_module
fn main() {
    renamed_module.my_fn
}
";

    // hovering over "my_fn"
    let hover = positioned_with_io(code, Position::new(3, 22), &io).unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_unqualified_imported_function_renamed_module() {
    let mut io = LanguageServerTestIO::new();
    io.add_hex_package("my_dep");
    _ = io.hex_dep_module("my_dep", "example_module", "pub fn my_fn() { Nil }");

    let code = "
import example_module.{my_fn} as renamed_module
fn main() {
    my_fn
}
";

    // hovering over "my_fn"
    let hover = positioned_with_io(code, Position::new(3, 6), &io).unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_function_nested_module() {
    let mut io = LanguageServerTestIO::new();
    io.add_hex_package("my_dep");
    _ = io.hex_dep_module(
        "my_dep",
        "my/nested/example_module",
        "pub fn my_fn() { Nil }",
    );

    // Example of HexDocs link with nested modules: https://hexdocs.pm/lustre/lustre/element/svg.html
    let code = "
import my/nested/example_module
fn main() {
    example_module.my_fn
}
";

    // hovering over "my_fn"
    let hover = positioned_with_io(code, Position::new(3, 22), &io).unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_ffi_renamed_function() {
    let mut io = LanguageServerTestIO::new();
    io.add_hex_package("my_dep");
    _ = io.hex_dep_module(
        "my_dep",
        "example_module",
        r#"
@external(erlang, "my_mod_ffi", "renamed_fn")
pub fn my_fn() -> Nil
"#,
    );

    let code = r#"
import example_module
fn main() {
    example_module.my_fn
}
"#;

    // hovering over "my_fn"
    let hover = positioned_with_io(code, Position::new(3, 22), &io).unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_constants() {
    let mut io = LanguageServerTestIO::new();
    io.add_hex_package("my_dep");
    _ = io.hex_dep_module("my_dep", "example_module", "pub const my_const = 42");

    let code = "
import example_module
fn main() {
  example_module.my_const
}
";

    // hovering over "my_const"
    let hover = positioned_with_io(code, Position::new(3, 19), &io).unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_imported_unqualified_constants() {
    let mut io = LanguageServerTestIO::new();
    io.add_hex_package("my_dep");
    _ = io.hex_dep_module("my_dep", "example_module", "pub const my_const = 42");

    let code = "
import example_module.{my_const}
fn main() {
  my_const
}
";

    // hovering over "my_const"
    let hover = positioned_with_io(code, Position::new(3, 5), &io).unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_value_with_two_modules_same_name() {
    let mut io = LanguageServerTestIO::new();
    io.add_hex_package("my_dep");
    _ = io.hex_dep_module("my_dep", "a/example_module", "pub const my_const = 42");
    _ = io.hex_dep_module("my_dep", "b/example_module", "pub const my_const = 42");

    let code = "
import a/example_module as _
import b/example_module
fn main() {
    example_module.my_const
}
";

    // hovering over "my_const"
    let hover = positioned_with_io(code, Position::new(4, 22), &io).unwrap();
    insta::assert_debug_snapshot!(hover);
}

#[test]
fn hover_external_function_with_another_value_same_name() {
    let mut io = LanguageServerTestIO::new();
    io.add_hex_package("my_dep");
    _ = io.hex_dep_module("my_dep", "a/example_module", "pub const my_const = 42");
    _ = io.hex_dep_module("my_dep", "b/example_module", "pub const my_const = 42");

    let code = "
import a/example_module.{my_const as discarded}
import b/example_module.{my_const} as _
fn main() {
    my_const
}
";

    // hovering over "my_const"
    let hover = positioned_with_io(code, Position::new(4, 8), &io).unwrap();
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
        positioned_hover(code, Position::new(3, 3)),
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
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam\nfn(fn(Int) -> String) -> String\n```\n".to_string()
            )),
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
