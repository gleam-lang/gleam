mod code_action;
mod compiler;
mod engine;
mod feedback;
mod files;
mod progress;
mod router;
mod server;

#[cfg(test)]
mod tests;

pub use server::LanguageServer;

use crate::{
    ast::SrcSpan, build::Target, line_numbers::LineNumbers, manifest::Manifest,
    paths::ProjectPaths, Result,
};
use lsp_types::{CodeActionParams, CodeActionTriggerKind, Position, Range};
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

pub fn determine_resolve_strategy(params: &CodeActionParams) -> ResolveStrategy {
    if let Some(trigger_kind) = params.context.trigger_kind{
        if let CodeActionTriggerKind::INVOKED = trigger_kind {
            ResolveStrategy::Eager
        } else {
            ResolveStrategy::Lazy
        }
    } else{
       ResolveStrategy::Lazy
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolveStrategy{
    Eager,
    Lazy
}

impl ResolveStrategy {
    pub fn is_eager(&self) -> bool {
        matches!(self, ResolveStrategy::Eager)
    }

    pub fn is_lazy(&self) -> bool {
        matches!(self, ResolveStrategy::Lazy)
    }
}