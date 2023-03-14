// This module is a prototype-y mess. It has lots of TODO comments in it. Let's
// resolve them all, inject all the IO, wrap a bunch of tests around it, and
// move it into the `gleam_core` package.

// TODO: Make a new router class which finds the root of the project a message
// is for and dispatches to the correct language server, making one for that
// root if it does not exist. This will require the compiler to be modified so
// that it can run on projects where the root is not the cwd.

mod protocol_adapter;
mod server;

use crate::{
    build_lock::BuildLock, fs::ProjectIO, lsp::protocol_adapter::LanguageServerProtocolAdapter,
};
use gleam_core::{
    ast::SrcSpan,
    build::{Mode, NullTelemetry, Target},
    diagnostic::{Diagnostic, Level},
    language_server::{Feedback, LockGuard, Locker},
    line_numbers::LineNumbers,
    paths::ProjectPaths,
    Result,
};
use itertools::Itertools;
use lsp_types::{self as lsp, Position, Range, Url};
use std::path::{Path, PathBuf};

#[cfg(target_os = "windows")]
use urlencoding::decode;

pub fn main() -> Result<()> {
    tracing::info!("language_server_starting");

    // Read the project config. If we are running in the context of a Gleam
    // fall back to a non-compiling mode that can only do formatting.
    let paths = ProjectPaths::at_current_directory();
    let config = if paths.root_config().exists() {
        tracing::info!("gleam_project_detected");
        Some(crate::config::root_config(&paths)?)
    } else {
        tracing::info!("gleam_project_not_found");
        None
    };

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = lsp_server::Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    LanguageServerProtocolAdapter::new(&connection, config, ProjectIO::new())?.run()?;
    io_threads.join().expect("joining_lsp_threads");

    // Shut down gracefully.
    tracing::info!("language_server_stopped");
    Ok(())
}

#[cfg(target_os = "windows")]
fn uri_to_module_name(uri: &Url, root: &Path) -> Option<String> {
    let mut uri_path = decode(&*uri.path().replace('/', "\\"))
        .expect("Invalid formatting")
        .to_string();
    if uri_path.starts_with("\\") {
        uri_path = uri_path
            .strip_prefix("\\")
            .expect("Failed to remove \"\\\" prefix")
            .to_string();
    }
    let path = PathBuf::from(uri_path);
    let components = path
        .strip_prefix(&root)
        .ok()?
        .components()
        .skip(1)
        .map(|c| c.as_os_str().to_string_lossy());
    let module_name = Itertools::intersperse(components, "/".into())
        .collect::<String>()
        .strip_suffix(".gleam")?
        .to_string();
    tracing::info!("(uri_to_module_name) module_name: {}", module_name);
    Some(module_name)
}

#[test]
#[cfg(target_os = "windows")]
fn uri_to_module_name_test() {
    let root = PathBuf::from("/projects/app");
    let uri = Url::parse("file:///b%3A/projects/app/src/one/two/three.rs").unwrap();
    assert_eq!(uri_to_module_name(&uri, &root), None);

    let root = PathBuf::from("/projects/app");
    let uri = Url::parse("file:///c%3A/projects/app/src/one/two/three.rs").unwrap();
    assert_eq!(uri_to_module_name(&uri, &root), None);
}

#[cfg(not(target_os = "windows"))]
fn uri_to_module_name(uri: &Url, root: &Path) -> Option<String> {
    let path = PathBuf::from(uri.path());
    let components = path
        .strip_prefix(root)
        .ok()?
        .components()
        .skip(1)
        .map(|c| c.as_os_str().to_string_lossy());
    let module_name = Itertools::intersperse(components, "/".into())
        .collect::<String>()
        .strip_suffix(".gleam")?
        .to_string();
    Some(module_name)
}

#[test]
#[cfg(not(target_os = "windows"))]
fn uri_to_module_name_test() {
    let root = PathBuf::from("/projects/app");
    let uri = Url::parse("file:///projects/app/src/one/two/three.gleam").unwrap();
    assert_eq!(
        uri_to_module_name(&uri, &root),
        Some("one/two/three".into())
    );

    let root = PathBuf::from("/projects/app");
    let uri = Url::parse("file:///projects/app/test/one/two/three.gleam").unwrap();
    assert_eq!(
        uri_to_module_name(&uri, &root),
        Some("one/two/three".into())
    );

    let root = PathBuf::from("/projects/app");
    let uri = Url::parse("file:///somewhere/else/src/one/two/three.gleam").unwrap();
    assert_eq!(uri_to_module_name(&uri, &root), None);

    let root = PathBuf::from("/projects/app");
    let uri = Url::parse("file:///projects/app/src/one/two/three.rs").unwrap();
    assert_eq!(uri_to_module_name(&uri, &root), None);
}

fn convert_response<T>(result: server::Response<T>) -> (serde_json::Value, Feedback)
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
        range: src_span_to_lsp_range(location.label.span, &line_numbers),
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
    pub fn new(target: Target) -> Result<Self> {
        let build_lock = BuildLock::new_target(Mode::Lsp, target)?;
        Ok(Self(build_lock))
    }
}

impl Locker for LspLocker {
    fn lock_for_build(&self) -> LockGuard {
        LockGuard(Box::new(self.0.lock(&NullTelemetry)))
    }
}

fn src_span_to_lsp_range(location: SrcSpan, line_numbers: &LineNumbers) -> Range {
    let start = line_numbers.line_and_column_number(location.start);
    let end = line_numbers.line_and_column_number(location.end);

    Range {
        start: Position {
            line: start.line - 1,
            character: start.column - 1,
        },
        end: Position {
            line: end.line - 1,
            character: end.column - 1,
        },
    }
}
