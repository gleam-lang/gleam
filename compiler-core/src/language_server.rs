mod code_action;
mod compiler;
mod completer;
mod edits;
mod engine;
mod feedback;
mod files;
mod messages;
mod progress;
mod rename;
mod router;
mod server;
mod signature_help;

#[cfg(test)]
mod tests;

pub use server::LanguageServer;

use crate::{
    ast::SrcSpan, build::Target, line_numbers::LineNumbers, manifest::Manifest,
    paths::ProjectPaths, Result,
};
use camino::Utf8PathBuf;
use lsp_types::{Position, Range, TextEdit, Url};
use std::any::Any;

#[derive(Debug)]
pub struct LockGuard(pub Box<dyn Any>);

pub trait Locker {
    fn lock_for_build(&self) -> LockGuard;
}

pub trait MakeLocker {
    fn make_locker(&self, paths: &ProjectPaths, target: Target) -> Result<Box<dyn Locker>>;
}

pub trait DownloadDependencies {
    fn download_dependencies(&self, paths: &ProjectPaths) -> Result<Manifest>;
}

pub fn src_span_to_lsp_range(location: SrcSpan, line_numbers: &LineNumbers) -> Range {
    let start = line_numbers.line_and_column_number(location.start);
    let end = line_numbers.line_and_column_number(location.end);

    Range::new(
        Position::new(start.line - 1, start.column - 1),
        Position::new(end.line - 1, end.column - 1),
    )
}

/// A little wrapper around LineNumbers to make it easier to build text edits.
///
#[derive(Debug)]
pub struct TextEdits<'a> {
    line_numbers: &'a LineNumbers,
    edits: Vec<TextEdit>,
}

impl<'a> TextEdits<'a> {
    pub fn new(line_numbers: &'a LineNumbers) -> Self {
        TextEdits {
            line_numbers,
            edits: vec![],
        }
    }

    pub fn src_span_to_lsp_range(&self, location: SrcSpan) -> Range {
        src_span_to_lsp_range(location, self.line_numbers)
    }

    pub fn replace(&mut self, location: SrcSpan, new_text: String) {
        self.edits.push(TextEdit {
            range: src_span_to_lsp_range(location, self.line_numbers),
            new_text,
        })
    }

    pub fn insert(&mut self, at: u32, new_text: String) {
        self.replace(SrcSpan { start: at, end: at }, new_text)
    }

    pub fn delete(&mut self, location: SrcSpan) {
        self.replace(location, "".to_string())
    }

    fn delete_range(&mut self, range: Range) {
        self.edits.push(TextEdit {
            range,
            new_text: "".into(),
        })
    }
}

fn path(uri: &Url) -> Utf8PathBuf {
    // The to_file_path method is available on these platforms
    #[cfg(any(unix, windows, target_os = "redox", target_os = "wasi"))]
    return Utf8PathBuf::from_path_buf(uri.to_file_path().expect("URL file"))
        .expect("Non Utf8 Path");

    #[cfg(not(any(unix, windows, target_os = "redox", target_os = "wasi")))]
    return Utf8PathBuf::from_path_buf(uri.path().into()).expect("Non Utf8 Path");
}
