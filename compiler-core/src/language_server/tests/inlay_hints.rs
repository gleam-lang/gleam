use super::*;
use crate::language_server::configuration::InlayHintsConfig;
use lsp_types::{
    InlayHint, InlayHintKind, InlayHintParams, Position, Range, TextDocumentIdentifier, TextEdit,
    Url,
};

fn expect_hints(
    src: &str,
    config: InlayHintsConfig,
    range: Option<Range>,
    expected_hints: Vec<InlayHint>,
) {
    let hints = inlay_hints(src, config, range).expect("should return hints");

    // InlayHint doesn't implement PartialEq so we're serialising to compare them
    let hints = serde_json::to_value(hints).expect("serialisation shouldn't fail");
    let expected_hints =
        serde_json::to_value(expected_hints).expect("serialisation shouldn't fail");
    assert_eq!(hints, expected_hints);
}

fn inlay_hints(
    src: &str,
    config: InlayHintsConfig,
    range: Option<Range>,
) -> Option<Vec<InlayHint>> {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine_with_config(
        &io,
        Configuration {
            inlay_hints: config,
            ..Default::default()
        },
    );

    _ = io.src_module("app", src);
    let response = engine.compile_please();
    assert!(response.result.is_ok());

    let path = Utf8PathBuf::from(if cfg!(target_family = "windows") {
        r"\\?\C:\src\app.gleam"
    } else {
        "/src/app.gleam"
    });

    let url = Url::from_file_path(path).expect("should be valid path for url");

    let params = InlayHintParams {
        text_document: TextDocumentIdentifier::new(url),
        work_done_progress_params: Default::default(),
        range: range.unwrap_or_else(|| {
            Range::new(
                Position::new(0, 0),
                Position::new(
                    src.lines().count() as u32,
                    src.lines().last().unwrap_or_default().len() as u32,
                ),
            )
        }),
    };
    let response = engine.inlay_hint(params);

    response.result.expect("inlay hint request should not fail")
}

#[test]
fn whole_range() {
    let code = "
const a = 42
const b = 43
";
    expect_hints(
        code,
        InlayHintsConfig {
            module_constants: true,
            ..Default::default()
        },
        Some(Range::new(Position::new(1, 0), Position::new(2, 0))),
        vec![InlayHint {
            position: Position::new(1, 7),
            label: ": Int".to_owned().into(),
            kind: Some(InlayHintKind::TYPE),
            text_edits: Some(vec![TextEdit {
                range: Range::new(Position::new(1, 7), Position::new(1, 7)),
                new_text: ": Int".to_owned(),
            }]),
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        }],
    );
}

#[test]
fn starts_near_end_of_range() {
    let code = "
const a = 42
";
    expect_hints(
        code,
        InlayHintsConfig {
            module_constants: true,
            ..Default::default()
        },
        Some(Range::new(Position::new(0, 0), Position::new(1, 5))),
        vec![InlayHint {
            position: Position::new(1, 7),
            label: ": Int".to_owned().into(),
            kind: Some(InlayHintKind::TYPE),
            text_edits: Some(vec![TextEdit {
                range: Range::new(Position::new(1, 7), Position::new(1, 7)),
                new_text: ": Int".to_owned(),
            }]),
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        }],
    );
}

#[test]
fn ends_near_start_of_range() {
    let code = "
const a = 42
";
    expect_hints(
        code,
        InlayHintsConfig {
            module_constants: true,
            ..Default::default()
        },
        Some(Range::new(Position::new(1, 6), Position::new(2, 0))),
        vec![InlayHint {
            position: Position::new(1, 7),
            label: ": Int".to_owned().into(),
            kind: Some(InlayHintKind::TYPE),
            text_edits: Some(vec![TextEdit {
                range: Range::new(Position::new(1, 7), Position::new(1, 7)),
                new_text: ": Int".to_owned(),
            }]),
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        }],
    );
}

#[test]
fn start_and_end_between_range() {
    let code = "
const a = 42
";
    expect_hints(
        code,
        InlayHintsConfig {
            module_constants: true,
            ..Default::default()
        },
        Some(Range::new(Position::new(1, 2), Position::new(1, 4))),
        vec![InlayHint {
            position: Position::new(1, 7),
            label: ": Int".to_owned().into(),
            kind: Some(InlayHintKind::TYPE),
            text_edits: Some(vec![TextEdit {
                range: Range::new(Position::new(1, 7), Position::new(1, 7)),
                new_text: ": Int".to_owned(),
            }]),
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        }],
    );
}

#[test]
fn module_constants() {
    let code = "
const n = 42
";
    expect_hints(
        code,
        InlayHintsConfig {
            module_constants: true,
            ..Default::default()
        },
        None,
        vec![InlayHint {
            position: Position::new(1, 7),
            label: ": Int".to_owned().into(),
            kind: Some(InlayHintKind::TYPE),
            text_edits: Some(vec![TextEdit {
                range: Range::new(Position::new(1, 7), Position::new(1, 7)),
                new_text: ": Int".to_owned(),
            }]),
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        }],
    );
}

#[test]
fn module_constants_already_annotated() {
    let code = "
const n: Int = 42
";
    expect_hints(
        code,
        InlayHintsConfig {
            module_constants: true,
            ..Default::default()
        },
        None,
        vec![],
    );
}

#[test]
fn module_constants_disabled() {
    let code = "
const n = 42
";
    expect_hints(
        code,
        InlayHintsConfig {
            module_constants: false,
            ..Default::default()
        },
        None,
        vec![],
    );
}

#[test]
fn function_definitions() {
    let code = "
fn add(lhs, rhs) {
    lhs + rhs
}
";
    expect_hints(
        code,
        InlayHintsConfig {
            function_definitions: true,
            ..Default::default()
        },
        None,
        vec![
            InlayHint {
                position: Position::new(1, 16),
                label: "-> Int".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(1, 16), Position::new(1, 16)),
                    new_text: " -> Int".to_owned(),
                }]),
                tooltip: None,
                padding_left: Some(true),
                padding_right: None,
                data: None,
            },
            InlayHint {
                position: Position::new(1, 10),
                label: ": Int".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(1, 10), Position::new(1, 10)),
                    new_text: ": Int".to_owned(),
                }]),
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            },
            InlayHint {
                position: Position::new(1, 15),
                label: ": Int".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(1, 15), Position::new(1, 15)),
                    new_text: ": Int".to_owned(),
                }]),
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            },
        ],
    );
}

#[test]
fn function_definitions_already_annotated() {
    let code = "
fn add(lhs: Int, rhs: Int) -> Int {
    lhs + rhs
}
";
    expect_hints(
        code,
        InlayHintsConfig {
            function_definitions: true,
            ..Default::default()
        },
        None,
        vec![],
    );
}

#[test]
fn function_definitions_partially_annotated() {
    let code = "
fn add(lhs, rhs: Int) {
    lhs + rhs
}
";
    expect_hints(
        code,
        InlayHintsConfig {
            function_definitions: true,
            ..Default::default()
        },
        None,
        vec![
            InlayHint {
                position: Position::new(1, 21),
                label: "-> Int".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(1, 21), Position::new(1, 21)),
                    new_text: " -> Int".to_owned(),
                }]),
                tooltip: None,
                padding_left: Some(true),
                padding_right: None,
                data: None,
            },
            InlayHint {
                position: Position::new(1, 10),
                label: ": Int".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(1, 10), Position::new(1, 10)),
                    new_text: ": Int".to_owned(),
                }]),
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            },
        ],
    );
}

#[test]
fn function_definitions_disabled() {
    let code = "
fn add(lhs, rhs) {
    lhs + rhs
}
";
    expect_hints(
        code,
        InlayHintsConfig {
            function_definitions: false,
            ..Default::default()
        },
        None,
        vec![],
    );
}
