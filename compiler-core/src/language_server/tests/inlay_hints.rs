use crate::language_server::tests::{setup_engine, LanguageServerTestIO};
use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, InlayHintParams, Position, Range};

#[test]
fn render_inlay_hints() {
    let code = "
  fn get_int() { 42 }
  fn to_str(x) { \"abc\" }

  fn main() {
    get_int()
    |> to_str()
  }
  ";

    expect_hints(
        code,
        vec![
            default_hint(Position::new(5, 13), "Int"),
            default_hint(Position::new(6, 15), "String"),
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

    let params = InlayHintParams {
        text_document: super::TestProject::build_path(),
        work_done_progress_params: Default::default(),
        range: Range::new(
            Position::new(0, 0),
            Position::new(
                src.lines().count() as u32,
                src.lines().last().unwrap_or_default().len() as u32,
            ),
        ),
    };

    engine
        .inlay_hints(params)
        .result
        .expect("inlay hint request should not fail")
}
