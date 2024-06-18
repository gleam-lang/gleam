use crate::language_server::tests::{setup_engine, LanguageServerTestIO};
use camino::Utf8PathBuf;
use lsp_types::{
    InlayHint, InlayHintKind, InlayHintLabel, InlayHintParams, Position, Range,
    TextDocumentIdentifier, Url,
};

#[test]
fn example_t() {
    let code = "
  fn to_str(x) { \"abc\" }

  fn main() {
    42
    |> to_str()
  }
  ";

    expect_hints(
        code,
        vec![
            default_hint(Position::new(4, 6), "Int"),
            default_hint(Position::new(5, 15), "String"),
        ],
    );
}

fn default_hint(position: Position, label: &str) -> InlayHint {
    InlayHint {
        position,
        label: InlayHintLabel::String(label.to_string()),
        kind: Some(InlayHintKind::TYPE),
        text_edits: None,
        tooltip: None,
        padding_left: Some(true),
        padding_right: None,
        data: None,
    }
}

fn expect_hints(src: &str, expected_hints: Vec<InlayHint>) {
    let hints = inlay_hints(src);

    // InlayHint doesn't implement PartialEq so we're serialising to compare them
    let hints = serde_json::to_value(hints).expect("serialisation shouldn't fail");
    let expected_hints =
        serde_json::to_value(expected_hints).expect("serialisation shouldn't fail");
    assert_eq!(hints, expected_hints);
}

fn inlay_hints(src: &str) -> Vec<InlayHint> {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

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
        range: Range::new(
            Position::new(0, 0),
            Position::new(
                src.lines().count() as u32,
                src.lines().last().unwrap_or_default().len() as u32,
            ),
        ),
    };
    let response = engine.inlay_hints(params);

    response.result.expect("inlay hint request should not fail")
}
