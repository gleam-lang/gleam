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
    io: &LanguageServerTestIO,
) {
    let hints = inlay_hints(src, config, range, io);

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
    io: &LanguageServerTestIO,
) -> Vec<InlayHint> {
    let mut engine = setup_engine_with_config(
        &io,
        Configuration {
            inlay_hints: config,
            ..Default::default()
        },
    );
    for package in &io.manifest.packages {
        add_package_from_manifest(&mut engine, package.clone());
    }

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
        &LanguageServerTestIO::new(),
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
        &LanguageServerTestIO::new(),
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
        &LanguageServerTestIO::new(),
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
        &LanguageServerTestIO::new(),
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
        &LanguageServerTestIO::new(),
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
        &LanguageServerTestIO::new(),
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
        &LanguageServerTestIO::new(),
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
        &LanguageServerTestIO::new(),
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
        &LanguageServerTestIO::new(),
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
        &LanguageServerTestIO::new(),
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
        &LanguageServerTestIO::new(),
    );
}

#[test]
fn function_definition_with_single_type_parameter() {
    let code = "
fn identity(value: some_value) {
    value
}
";
    expect_hints(
        code,
        InlayHintsConfig {
            function_definitions: true,
            ..Default::default()
        },
        None,
        vec![InlayHint {
            position: Position::new(1, 30),
            label: "-> some_value".to_owned().into(),
            kind: Some(InlayHintKind::TYPE),
            text_edits: Some(vec![TextEdit {
                range: Range::new(Position::new(1, 30), Position::new(1, 30)),
                new_text: " -> some_value".to_owned(),
            }]),
            tooltip: None,
            padding_left: Some(true),
            padding_right: None,
            data: None,
        }],
        &LanguageServerTestIO::new(),
    );
}

#[test]
fn function_definition_with_type_qualifiers() {
    let code = "
import mod1.{type Value as V1}
import mod2.{type Value as V2}

fn foo() {
  let x: V1 = mod1.v()
  let y: V2 = mod2.v()
  #(x, y)
}
";

    let mut io = LanguageServerTestIO::new();
    io.add_hex_package("dep1");
    io.add_hex_package("dep2");

    _ = io.hex_dep_module(
        "dep1",
        "mod1",
        "pub type Value {
	C
	D
    }
    pub fn v() -> Value {
	C
    }",
    );

    _ = io.hex_dep_module(
        "dep2",
        "mod2",
        "pub type Value{
	C
	D
    }

    pub fn v() -> Value{
	C
    }",
    );

    expect_hints(
        code,
        InlayHintsConfig {
            function_definitions: true,
            ..Default::default()
        },
        None,
        vec![InlayHint {
            position: Position::new(4, 8),
            label: "-> #(V1, V2)".to_owned().into(),
            kind: Some(InlayHintKind::TYPE),
            text_edits: Some(vec![TextEdit {
                range: Range::new(Position::new(4, 8), Position::new(4, 8)),
                new_text: " -> #(V1, V2)".to_owned(),
            }]),
            tooltip: None,
            padding_left: Some(true),
            padding_right: None,
            data: None,
        }],
        &io,
    );
}

#[test]
fn function_definition_with_qualified_types_and_type_paramaeters() {
    let code = "
import mod1.{type Value as V1}
import mod2.{type Value as V2}

fn foo(num, z: a) {
  let a = num + 4
  let x: V1 = mod1.v()
  let y: V2 = mod2.v()
  #(x, y, z, num)
}
";

    let mut io = LanguageServerTestIO::new();
    io.add_hex_package("dep1");
    io.add_hex_package("dep2");

    _ = io.hex_dep_module(
        "dep1",
        "mod1",
        "pub type Value {
	C
	D
    }
    pub fn v() -> Value {
	C
    }",
    );

    _ = io.hex_dep_module(
        "dep2",
        "mod2",
        "pub type Value{
	C
	D
    }

    pub fn v() -> Value{
	C
    }",
    );

    expect_hints(
        code,
        InlayHintsConfig {
            function_definitions: true,
            ..Default::default()
        },
        None,
        vec![
            InlayHint {
                position: Position::new(5, 7),
                label: ": Int".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(5, 7), Position::new(5, 7)),
                    new_text: ": Int".to_owned(),
                }]),
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            },
            InlayHint {
                position: Position::new(4, 17),
                label: "-> #(V1, V2, a, Int)".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(4, 17), Position::new(4, 17)),
                    new_text: " -> #(V1, V2, a, Int)".to_owned(),
                }]),
                tooltip: None,
                padding_left: Some(true),
                padding_right: None,
                data: None,
            },
            InlayHint {
                position: Position::new(4, 10),
                label: ": Int".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(4, 10), Position::new(4, 10)),
                    new_text: ": Int".to_owned(),
                }]),
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            },
        ],
        &io,
    );
}

#[test]
fn function_definition_with_qualified_modules_and_type_paramaeters() {
    let code = "
import mod1 as a
import mod2 as b

fn foo(num, z: a) {
  let a = num + 4
  let x = a.v()
  let y = b.v()
  #(x, y, z, num)
}
";

    let mut io = LanguageServerTestIO::new();
    io.add_hex_package("dep1");
    io.add_hex_package("dep2");

    _ = io.hex_dep_module(
        "dep1",
        "mod1",
        "pub type Value {
	C
	D
    }
    pub fn v() -> Value {
	C
    }",
    );

    _ = io.hex_dep_module(
        "dep2",
        "mod2",
        "pub type Value{
	C
	D
    }

    pub fn v() -> Value{
	C
    }",
    );

    expect_hints(
        code,
        InlayHintsConfig {
            function_definitions: true,
            ..Default::default()
        },
        None,
        vec![
            InlayHint {
                position: Position::new(5, 7),
                label: ": Int".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(5, 7), Position::new(5, 7)),
                    new_text: ": Int".to_owned(),
                }]),
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            },
            InlayHint {
                position: Position::new(6, 7),
                label: ": a.Value".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(6, 7), Position::new(6, 7)),
                    new_text: ": a.Value".to_owned(),
                }]),
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            },
            InlayHint {
                position: Position::new(7, 7),
                label: ": b.Value".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(7, 7), Position::new(7, 7)),
                    new_text: ": b.Value".to_owned(),
                }]),
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            },
            InlayHint {
                position: Position::new(4, 17),
                label: "-> #(a.Value, b.Value, a, Int)".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(4, 17), Position::new(4, 17)),
                    new_text: " -> #(a.Value, b.Value, a, Int)".to_owned(),
                }]),
                tooltip: None,
                padding_left: Some(true),
                padding_right: None,
                data: None,
            },
            InlayHint {
                position: Position::new(4, 10),
                label: ": Int".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(4, 10), Position::new(4, 10)),
                    new_text: ": Int".to_owned(),
                }]),
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            },
        ],
        &io,
    );
}

#[test]
fn function_definition_with_type_parameter_within_function_body() {
    let code = "
fn identity(x) {
    let y: value = x
    y
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
                position: Position::new(1, 14),
                label: "-> value".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(1, 14), Position::new(1, 14)),
                    new_text: " -> value".to_owned(),
                }]),
                tooltip: None,
                padding_left: Some(true),
                padding_right: None,
                data: None,
            },
            InlayHint {
                position: Position::new(1, 13),
                label: ": value".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(1, 13), Position::new(1, 13)),
                    new_text: ": value".to_owned(),
                }]),
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            },
        ],
        &LanguageServerTestIO::new(),
    );
}

#[test]
fn function_definition_with_partially_defined_type_parameter() {
    let code = "
fn equals(a: value, b) {
    a == b
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
                position: Position::new(1, 22),
                label: "-> Bool".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(1, 22), Position::new(1, 22)),
                    new_text: " -> Bool".to_owned(),
                }]),
                tooltip: None,
                padding_left: Some(true),
                padding_right: None,
                data: None,
            },
            InlayHint {
                position: Position::new(1, 21),
                label: ": value".to_owned().into(),
                kind: Some(InlayHintKind::TYPE),
                text_edits: Some(vec![TextEdit {
                    range: Range::new(Position::new(1, 21), Position::new(1, 21)),
                    new_text: ": value".to_owned(),
                }]),
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            },
        ],
        &LanguageServerTestIO::new(),
    );
}
