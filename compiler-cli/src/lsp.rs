// This module is a prototype-y mess. It has lots of TODO comments in it. Let's
// resolve them all, inject all the IO, wrap a bunch of tests around it, and
// move it into the `gleam_core` package.

// TODO: Make a new router class which finds the root of the project a message
// is for and dispatches to the correct language server, making one for that
// root if it does not exist. This will require the compiler to be modified so
// that it can run on projects where the root is not the cwd.

mod engine;
mod protocol_adapter;

use crate::{
    build_lock::BuildLock, dependencies::UseManifest, fs::ProjectIO,
    lsp::protocol_adapter::LanguageServerProtocolAdapter,
};
use gleam_core::{
    build::{Mode, NullTelemetry, Target},
    diagnostic::{Diagnostic, Level},
    language_server::{Feedback, LockGuard, Locker},
    line_numbers::LineNumbers,
    manifest::Manifest,
    paths::ProjectPaths,
    Result,
};
use lsp_types::{self as lsp, Url};
use std::path::PathBuf;

#[cfg(target_os = "windows")]
use urlencoding::decode;

pub fn main() -> Result<()> {
    tracing::info!("language_server_starting");

    // Read the project config. If we are running in the context of a Gleam
    // fall back to a non-compiling mode that can only do formatting.
    let paths = crate::project_paths_at_current_directory();
    let config = if paths.root_config().exists() {
        tracing::info!("gleam_project_detected");
        Some(crate::config::root_config()?)
    } else {
        tracing::info!("gleam_project_not_found");
        None
    };

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = lsp_server::Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let io = ProjectIO::new();
    LanguageServerProtocolAdapter::new(
        &connection,
        config,
        dependencies_downloader,
        paths,
        io,
        make_locker,
    )?
    .run()?;
    io_threads.join().expect("joining_lsp_threads");

    // Shut down gracefully.
    tracing::info!("language_server_stopped");
    Ok(())
}

fn make_locker(paths: &ProjectPaths, target: Target) -> Result<Box<dyn Locker>> {
    let locker = LspLocker::new(paths, target)?;
    Ok(Box::new(locker))
}

fn dependencies_downloader(paths: &ProjectPaths) -> Result<Manifest> {
    crate::dependencies::download(paths, NullTelemetry, None, UseManifest::Yes)
}

fn convert_response<T>(result: engine::Response<T>) -> (serde_json::Value, Feedback)
where
    T: serde::Serialize,
{
    (
        serde_json::to_value(result.payload).expect("json to_value"),
        result.feedback,
    )
}

fn diagnostic_to_lsp(diagnostic: Diagnostic) -> Vec<lsp::Diagnostic> {
    let severity = match diagnostic.level {
        Level::Error => lsp::DiagnosticSeverity::ERROR,
        Level::Warning => lsp::DiagnosticSeverity::WARNING,
    };
    let hint = diagnostic.hint;
    let mut text = diagnostic.title;

    if let Some(label) = diagnostic
        .location
        .as_ref()
        .and_then(|location| location.label.text.as_deref())
    {
        text.push_str("\n\n");
        text.push_str(label);
        if !label.ends_with(['.', '?']) {
            text.push('.');
        }
    }

    if !diagnostic.text.is_empty() {
        text.push_str("\n\n");
        text.push_str(&diagnostic.text);
    }

    // TODO: Redesign the diagnostic type so that we can be sure there is always
    // a location. Locationless diagnostics would be handled separately.
    let location = diagnostic
        .location
        .expect("Diagnostic given to LSP without location");
    let line_numbers = LineNumbers::new(&location.src);

    let main = lsp::Diagnostic {
        range: engine::src_span_to_lsp_range(location.label.span, &line_numbers),
        severity: Some(severity),
        code: None,
        code_description: None,
        source: None,
        message: text,
        related_information: None,
        tags: None,
        data: None,
    };

    match hint {
        Some(hint) => {
            let hint = lsp::Diagnostic {
                severity: Some(lsp::DiagnosticSeverity::HINT),
                message: hint,
                ..main.clone()
            };
            vec![main, hint]
        }
        None => vec![main],
    }
}

fn path_to_uri(path: PathBuf) -> Url {
    // Canonicalise the paths to avoid having `./` at the start.
    // Really what we want to do is to always use absolute paths in the compiler
    // and only make them relative before showing them to the user in error
    // messages etc.
    // TODO: make all compiler paths absolute, converting to relative paths in
    // errors.
    let path = path.canonicalize().unwrap_or(path);

    let mut file: String = "file://".into();
    file.push_str(&path.as_os_str().to_string_lossy());
    Url::parse(&file).expect("path_to_uri URL parse")
}

#[derive(Debug)]
pub struct LspLocker(BuildLock);

impl LspLocker {
    pub fn new(paths: &ProjectPaths, target: Target) -> Result<Self> {
        let build_lock = BuildLock::new_target(paths, Mode::Lsp, target)?;
        Ok(Self(build_lock))
    }
}

impl Locker for LspLocker {
    fn lock_for_build(&self) -> LockGuard {
        LockGuard(Box::new(self.0.lock(&NullTelemetry)))
    }
}
