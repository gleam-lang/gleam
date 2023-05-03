use lsp_types::{
    CompletionItem, CompletionItemKind, Documentation, MarkupContent, MarkupKind, Position,
    TextDocumentIdentifier, TextDocumentPositionParams, Url,
};

use super::*;

fn expression_completions_for(src: &str) -> Vec<CompletionItem> {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    io.src_module("app", &format!("fn typing_in_here() {{\n  0\n}}\n {src}"));
    let response = engine.compile_please();
    assert!(response.result.is_ok());

    let response = engine.completion(TextDocumentPositionParams::new(
        TextDocumentIdentifier::new(Url::parse("file:///src/app.gleam").unwrap()),
        Position::new(2, 1),
    ));

    let mut completions = response.result.unwrap().unwrap();
    completions.sort_by(|a, b| a.label.cmp(&b.label));
    completions
}

#[test]
fn local_public_function() {
    let code = "
pub fn main() {
  0
}";

    assert_eq!(
        expression_completions_for(code),
        vec![CompletionItem {
            label: "main".into(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("fn() -> Int".into()),
            documentation: None,
            ..Default::default()
        }]
    );
}

#[test]
fn local_public_function_with_documentation() {
    let code = "
/// Hello
pub fn main() {
  0
}";

    assert_eq!(
        expression_completions_for(code),
        vec![CompletionItem {
            label: "main".into(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("fn() -> Int".into()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: " Hello\n".into(),
            })),
            ..Default::default()
        }]
    );
}

#[test]
fn local_public_enum() {
    let code = "
pub type Direction {
  Left
  Right
}
";

    assert_eq!(
        expression_completions_for(code),
        vec![
            CompletionItem {
                label: "Left".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "Right".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            }
        ]
    );
}

#[test]
fn local_public_record() {
    let code = "
pub type Box {
/// Hello
  Box(Int, Int, Float)
}
";

    assert_eq!(
        expression_completions_for(code),
        vec![CompletionItem {
            label: "Box".into(),
            kind: Some(CompletionItemKind::CONSTRUCTOR),
            detail: Some("fn(Int, Int, Float) -> Box".into()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: " Hello\n".into(),
            })),
            ..Default::default()
        }]
    );
}

#[test]
fn local_public_enum_with_documentation() {
    let code = "
pub type Direction {
  /// Hello
  Left
  /// Goodbye
  Right
}
";

    assert_eq!(
        expression_completions_for(code),
        vec![
            CompletionItem {
                label: "Left".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: " Hello\n".into(),
                })),
                ..Default::default()
            },
            CompletionItem {
                label: "Right".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: " Goodbye\n".into(),
                })),
                ..Default::default()
            }
        ]
    );
}

#[test]
fn local_public_record_with_documentation() {
    let code = "
pub type Box {
  Box(Int, Int, Float)
}
";

    assert_eq!(
        expression_completions_for(code),
        vec![CompletionItem {
            label: "Box".into(),
            kind: Some(CompletionItemKind::CONSTRUCTOR),
            detail: Some("fn(Int, Int, Float) -> Box".into()),
            documentation: None,
            ..Default::default()
        }]
    );
}
