use itertools::Itertools;
use lsp_types::{
    CompletionItem, CompletionItemKind, Documentation, Hover, HoverContents, HoverParams,
    MarkedString, MarkupContent, MarkupKind, Position, Range, TextDocumentIdentifier,
    TextDocumentPositionParams, Url,
};

use super::*;

// fn expression_completions(src: &str, dep: &str) -> Vec<CompletionItem> {
//     let src = format!("fn typing_in_here() {{\n  0\n}}\n {src}");
//     let position = Position::new(1, 0);
//     positioned_expression_completions(&src, dep, position)
//         .into_iter()
//         .filter(|c| c.label != "typing_in_here")
//         .collect_vec()
// }

fn positioned_hover(src: &str, position: Position) -> Option<Hover> {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    _ = io.src_module("app", src);
    let response = engine.compile_please();
    assert!(response.result.is_ok());

    let path = Utf8PathBuf::from(if cfg!(target_family = "windows") {
        r#"\\?\C:\src\app.gleam"#
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

#[test]
fn hover_function_definition() {
    let code = "
fn add_2(x) {
  x + 2
}
";

    assert_eq!(
        positioned_hover(&code, Position::new(1, 3)),
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
fn hover_function_definition_with_docs() {
    let code = "
/// Exciting documentation
/// Maybe even multiple lines
fn append(x, y) {
  x <> y
}
";

    assert_eq!(
        positioned_hover(&code, Position::new(3, 3)),
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
        positioned_hover(&code, Position::new(3, 10)),
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

    assert_eq!(positioned_hover(&code, Position::new(4, 1)), None);
}
