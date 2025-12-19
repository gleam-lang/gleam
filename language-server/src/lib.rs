#![warn(
    clippy::all,
    clippy::dbg_macro,
    clippy::todo,
    clippy::mem_forget,
    clippy::filter_map_next,
    clippy::needless_continue,
    clippy::needless_borrow,
    clippy::match_wildcard_for_single_variants,
    clippy::imprecise_flops,
    clippy::suboptimal_flops,
    clippy::lossy_float_literal,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::fn_params_excessive_bools,
    clippy::inefficient_to_string,
    clippy::linkedlist,
    clippy::macro_use_imports,
    clippy::option_option,
    clippy::verbose_file_reads,
    clippy::unnested_or_patterns,
    rust_2018_idioms,
    missing_debug_implementations,
    missing_copy_implementations,
    trivial_casts,
    trivial_numeric_casts,
    nonstandard_style,
    unexpected_cfgs,
    unused_import_braces,
    unused_qualifications
)]
#![deny(
    clippy::await_holding_lock,
    clippy::disallowed_methods,
    clippy::if_let_mutex,
    clippy::indexing_slicing,
    clippy::mem_forget,
    clippy::ok_expect,
    clippy::unimplemented,
    clippy::unwrap_used,
    unsafe_code,
    unstable_features,
    unused_results
)]
#![allow(
    clippy::assign_op_pattern,
    clippy::to_string_trait_impl,
    clippy::match_single_binding,
    clippy::match_like_matches_macro,
    clippy::inconsistent_struct_constructor,
    clippy::len_without_is_empty
)]

mod code_action;
mod compiler;
mod completer;
mod edits;
mod engine;
mod feedback;
mod files;
mod messages;
mod progress;
mod reference;
mod rename;
mod router;
mod server;
mod signature_help;

#[cfg(test)]
mod tests;

pub use server::LanguageServer;

use camino::Utf8PathBuf;
use gleam_core::{
    Result, ast::SrcSpan, build::Target, line_numbers::LineNumbers, manifest::Manifest,
    paths::ProjectPaths,
};
use lsp_types::{Position, Range, TextEdit, Url};
use std::any::Any;

#[derive(Debug)]
pub struct LockGuard(pub Box<dyn Any>);

pub trait Locker {
    fn lock_for_build(&self) -> Result<LockGuard>;
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

pub fn lsp_range_to_src_span(range: Range, line_numbers: &LineNumbers) -> SrcSpan {
    let start = line_numbers.byte_index(range.start);
    let end = line_numbers.byte_index(range.end);
    SrcSpan { start, end }
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

    pub fn lsp_range_to_src_span(&self, range: Range) -> SrcSpan {
        lsp_range_to_src_span(range, self.line_numbers)
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

fn url_from_path(path: &str) -> Option<Url> {
    // The targets for which `from_file_path` is defined
    #[cfg(any(
        unix,
        windows,
        target_os = "redox",
        target_os = "wasi",
        target_os = "hermit"
    ))]
    let uri = Url::from_file_path(path).ok();

    #[cfg(not(any(
        unix,
        windows,
        target_os = "redox",
        target_os = "wasi",
        target_os = "hermit"
    )))]
    let uri = Url::parse(&format!("file://{path}")).ok();

    uri
}
