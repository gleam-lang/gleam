use itertools::Itertools;
use lsp_types::{
    CompletionItem, CompletionItemKind, Documentation, MarkupContent, MarkupKind, Position,
    TextDocumentIdentifier, TextDocumentPositionParams, Url,
};

use super::*;

fn expression_completions(src: &str, dep: &str) -> Vec<CompletionItem> {
    let src = format!("fn typing_in_here() {{\n  0\n}}\n {src}");
    let position = Position::new(1, 0);
    positioned_expression_completions(&src, dep, position)
        .into_iter()
        .filter(|c| c.label != "typing_in_here")
        .sorted_by(|a, b| a.label.cmp(&b.label))
        .collect_vec()
}

fn positioned_expression_completions(
    src: &str,
    dep: &str,
    position: Position,
) -> Vec<CompletionItem> {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    io.src_module("dep", dep);
    io.src_module("app", src);
    let response = engine.compile_please();
    assert!(response.result.is_ok());

    let path = PathBuf::from(if cfg!(target_family = "windows") {
        r#"\\?\C:\src\app.gleam"#
    } else {
        "/src/app.gleam"
    });

    let url = Url::from_file_path(path).unwrap();

    let response = engine.completion(TextDocumentPositionParams::new(
        TextDocumentIdentifier::new(url),
        position,
    ));

    response.result.unwrap().unwrap_or_default()
}

#[test]
fn completions_for_outside_a_function() {
    let code = "

pub fn main() {
  0
}";

    assert_eq!(
        positioned_expression_completions(code, "", Position::new(0, 0)),
        vec![]
    );
}

#[test]
fn local_public_function() {
    let code = "
pub fn main() {
  0
}";

    assert_eq!(
        expression_completions(code, ""),
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
        expression_completions(code, ""),
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
        expression_completions(code, ""),
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
        expression_completions(code, ""),
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
        expression_completions(code, ""),
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
        expression_completions(code, ""),
        vec![CompletionItem {
            label: "Box".into(),
            kind: Some(CompletionItemKind::CONSTRUCTOR),
            detail: Some("fn(Int, Int, Float) -> Box".into()),
            documentation: None,
            ..Default::default()
        }]
    );
}

#[test]
fn imported_module_function() {
    let code = "
import dep
";
    let dep = "
pub fn wobble() {
  Nil
}
";

    assert_eq!(
        expression_completions(code, dep),
        vec![CompletionItem {
            label: "dep.wobble".into(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("fn() -> Nil".into()),
            documentation: None,
            ..Default::default()
        }]
    );
}

#[test]
fn imported_public_enum() {
    let code = "
import dep
";
    let dep = "
pub type Direction {
  Left
  Right
}
";

    assert_eq!(
        expression_completions(code, dep),
        vec![
            CompletionItem {
                label: "dep.Left".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "dep.Right".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            }
        ]
    );
}

#[test]
fn imported_public_record() {
    let code = "
import dep
";
    let dep = "
pub type Box {
  Box(Int)
}
";

    assert_eq!(
        expression_completions(code, dep),
        vec![CompletionItem {
            label: "dep.Box".into(),
            kind: Some(CompletionItemKind::CONSTRUCTOR),
            detail: Some("fn(Int) -> Box".into()),
            documentation: None,
            ..Default::default()
        }]
    );
}

#[test]
fn imported_unqualifed_module_function() {
    let code = "
import dep.{wobble}
";
    let dep = "
pub fn wobble() {
  Nil
}
";

    assert_eq!(
        expression_completions(code, dep),
        vec![
            CompletionItem {
                label: "dep.wobble".into(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("fn() -> Nil".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "wobble".into(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("fn() -> Nil".into()),
                documentation: None,
                ..Default::default()
            },
        ]
    );
}

#[test]
fn imported_unqualifed_public_enum() {
    let code = "
import dep.{Left}
";
    let dep = "
pub type Direction {
  Left
  Right
}
";

    assert_eq!(
        expression_completions(code, dep),
        vec![
            CompletionItem {
                label: "Left".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "dep.Left".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "dep.Right".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            },
        ]
    );
}

#[test]
fn imported_unqualifed_public_record() {
    let code = "
import dep.{Box}
";
    let dep = "
pub type Box {
  Box(Int)
}
";

    assert_eq!(
        expression_completions(code, dep),
        vec![
            CompletionItem {
                label: "Box".into(),
                kind: Some(CompletionItemKind::CONSTRUCTOR),
                detail: Some("fn(Int) -> Box".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "dep.Box".into(),
                kind: Some(CompletionItemKind::CONSTRUCTOR),
                detail: Some("fn(Int) -> Box".into()),
                documentation: None,
                ..Default::default()
            },
        ]
    );
}

#[test]
fn private_function() {
    let code = "
fn private() {
  1
}
";
    let dep = "";

    assert_eq!(
        expression_completions(code, dep),
        vec![CompletionItem {
            label: "private".into(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("fn() -> Int".into()),
            documentation: None,
            ..Default::default()
        },]
    );
}

#[test]
fn private_type() {
    let code = "
type Wibble {
  Wobble
}
";
    let dep = "";

    assert_eq!(
        expression_completions(code, dep),
        vec![CompletionItem {
            label: "Wobble".into(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some("Wibble".into()),
            documentation: None,
            ..Default::default()
        },]
    );
}

#[test]
fn opaque_type() {
    let code = "
pub opaque type Wibble {
  Wobble
}
";
    let dep = "";

    assert_eq!(
        expression_completions(code, dep),
        vec![CompletionItem {
            label: "Wobble".into(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some("Wibble".into()),
            documentation: None,
            ..Default::default()
        },]
    );
}

#[test]
fn private_function_in_dep() {
    let code = "import dep";
    let dep = "
fn private() {
  1
}
";

    assert_eq!(expression_completions(code, dep), vec![]);
}

#[test]
fn private_type_in_dep() {
    let code = "import dep";
    let dep = "
type Wibble {
  Wobble
}
";

    assert_eq!(expression_completions(code, dep), vec![]);
}
