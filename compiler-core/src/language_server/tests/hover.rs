use lsp_types::{
    Hover, HoverContents, HoverParams, MarkedString, Position, Range, TextDocumentIdentifier,
    TextDocumentPositionParams, Url,
};

use super::*;

fn hover_information(src: &str, dep: &str, position: Position) -> Option<Hover> {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    _ = io.src_module("dep", dep);
    _ = io.src_module("app", src);
    let response = engine.compile_please();
    assert!(response.result.is_ok());

    let path = Utf8PathBuf::from(if cfg!(target_family = "windows") {
        r"\\?\C:\src\app.gleam"
    } else {
        "/src/app.gleam"
    });

    let url = Url::from_file_path(path).unwrap();

    let response = engine.hover(HoverParams {
        text_document_position_params: TextDocumentPositionParams::new(
            TextDocumentIdentifier::new(url),
            position,
        ),
        work_done_progress_params: Default::default(),
    });

    response.result.unwrap()
}

#[test]
fn nil_expression() {
    let code = "
fn wobble() {
  Nil
}";

    assert_eq!(
        hover_information(code, "", Position::new(2, 2)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam\nNil\n```\n".to_string()
            )),
            range: Some(Range {
                start: Position::new(2, 2),
                end: Position::new(2, 5)
            }),
        })
    );
}

#[test]
fn function_argument() {
    let code = "
fn wobble(x) {
  x <> \"!\"
}";

    assert_eq!(
        hover_information(code, "", Position::new(1, 10)),
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "```gleam\nString\n```\nA function argument.".to_string()
            )),
            range: Some(Range {
                start: Position::new(1, 10),
                end: Position::new(1, 11)
            }),
        })
    );
}
