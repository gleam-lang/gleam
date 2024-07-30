mod code_action;
mod compiler;
mod completer;
mod configuration;
mod edits;
mod engine;
mod feedback;
mod files;
mod inlay_hints;
mod messages;
mod progress;
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
use lsp_types::{Position, Range, Url};
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

pub fn src_offset_to_lsp_position(offset: u32, line_numbers: &LineNumbers) -> Position {
    let line_col = line_numbers.line_and_column_number(offset);
    Position::new(line_col.line - 1, line_col.column - 1)
}

pub fn src_span_to_lsp_range(location: SrcSpan, line_numbers: &LineNumbers) -> Range {
    Range::new(
        src_offset_to_lsp_position(location.start, line_numbers),
        src_offset_to_lsp_position(location.end, line_numbers),
    )
}

fn path(uri: &Url) -> Utf8PathBuf {
    // The to_file_path method is available on these platforms
    #[cfg(any(unix, windows, target_os = "redox", target_os = "wasi"))]
    return Utf8PathBuf::from_path_buf(uri.to_file_path().expect("URL file"))
        .expect("Non Utf8 Path");

    #[cfg(not(any(unix, windows, target_os = "redox", target_os = "wasi")))]
    return Utf8PathBuf::from_path_buf(uri.path().into()).expect("Non Utf8 Path");
}
