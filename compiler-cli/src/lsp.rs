// This module is a prototype-y mess. It has lots of TODO comments in it. Let's
// resolve them all, inject all the IO, wrap a bunch of tests around it, and
// move it into the `gleam_core` package.

// TODO: Make the language server methods return a struct that indicates what to
// send back to the client. i.e. The response, any diagnostics to add or remove,
// any messages. Each of these can be added in multiple PRs.

// TODO: Make the language server responsible for deciding when to push what
// diagnostics.

// TODO: Remove all use of the Connection from the LanguageServer. That is the
// job of the protocol adapter.

// TODO: Make a new router class which finds the root of the project a message
// is for and dispatches to the correct language server, making one for that
// root if it does not exist. This will require the compiler to be modified so
// that it can run on projects where the root is not the cwd.

mod protocol_adapter;
mod server;

use crate::{
    build_lock::BuildLock, dependencies::UseManifest, fs,
    lsp::protocol_adapter::LanguageServerProtocolAdapter, telemetry::NullTelemetry,
};
use gleam_core::{
    ast::SrcSpan,
    build::{self, Module, ProjectCompiler},
    config::PackageConfig,
    diagnostic::{Diagnostic, Level},
    io::{CommandExecutor, FileSystemIO, Stdio},
    line_numbers::LineNumbers,
    paths, Error, Result,
};
use gleam_core::{build::Mode, warning::VectorWarningEmitterIO};
use itertools::Itertools;
use lsp_types::{self as lsp, HoverProviderCapability, InitializeParams, Position, Range, Url};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

#[cfg(target_os = "windows")]
use urlencoding::decode;

const COMPILING_PROGRESS_TOKEN: &str = "compiling-gleam";
const CREATE_COMPILING_PROGRESS_TOKEN: &str = "create-compiling-progress-token";

pub fn main() -> Result<()> {
    tracing::info!("language_server_starting");

    // Read the project config. If we are running in the context of a Gleam
    // project then there will be one. If not there will not be one and we'll
    // fall back to a non-compiling mode that can only do formatting.
    let config = if paths::root_config().exists() {
        tracing::info!("gleam_project_detected");
        Some(crate::config::root_config()?)
    } else {
        tracing::info!("gleam_project_not_found");
        None
    };

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = lsp_server::Connection::stdio();
    let server_capabilities = server_capabilities();

    let server_capabilities_json =
        serde_json::to_value(server_capabilities).expect("server_capabilities_serde");

    let initialization_params: InitializeParams = serde_json::from_value(
        connection
            .initialize(server_capabilities_json)
            .expect("LSP initialize"),
    )
    .expect("LSP InitializeParams from json");

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    LanguageServerProtocolAdapter::new(initialization_params, config)?.run(connection)?;
    io_threads.join().expect("joining_lsp_threads");

    // Shut down gracefully.
    tracing::info!("language_server_stopped");
    Ok(())
}

fn server_capabilities() -> lsp::ServerCapabilities {
    lsp::ServerCapabilities {
        text_document_sync: Some(lsp::TextDocumentSyncCapability::Options(
            lsp::TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(lsp::TextDocumentSyncKind::FULL),
                will_save: None,
                will_save_wait_until: None,
                save: Some(lsp::TextDocumentSyncSaveOptions::SaveOptions(
                    lsp::SaveOptions {
                        include_text: Some(false),
                    },
                )),
            },
        )),
        selection_range_provider: None,
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        completion_provider: Some(lsp::CompletionOptions {
            resolve_provider: None,
            trigger_characters: Some(vec![".".into(), " ".into()]),
            all_commit_characters: None,
            work_done_progress_options: lsp::WorkDoneProgressOptions {
                work_done_progress: None,
            },
        }),
        signature_help_provider: None,
        definition_provider: Some(lsp::OneOf::Left(true)),
        type_definition_provider: None,
        implementation_provider: None,
        references_provider: None,
        document_highlight_provider: None,
        document_symbol_provider: None,
        workspace_symbol_provider: None,
        code_action_provider: None,
        code_lens_provider: None,
        document_formatting_provider: Some(lsp::OneOf::Left(true)),
        document_range_formatting_provider: None,
        document_on_type_formatting_provider: None,
        rename_provider: None,
        document_link_provider: None,
        color_provider: None,
        folding_range_provider: None,
        declaration_provider: None,
        execute_command_provider: None,
        workspace: None,
        call_hierarchy_provider: None,
        semantic_tokens_provider: None,
        moniker_provider: None,
        linked_editing_range_provider: None,
        experimental: None,
    }
}

#[derive(Debug)]
pub struct LspMessage {
    level: Level,
    text: String,
}

#[derive(Debug)]
pub struct ModuleSourceInformation {
    /// The path to the source file from within the project root
    path: String,

    /// Useful for converting from Gleam's byte index offsets to the LSP line
    /// and column number positions.
    line_numbers: LineNumbers,
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

fn cast_request<R>(request: lsp_server::Request) -> Result<R::Params, lsp_server::Request>
where
    R: lsp::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    let (_, params) = request.extract(R::METHOD)?;
    Ok(params)
}

fn cast_notification<N>(
    notification: lsp_server::Notification,
) -> Result<N::Params, lsp_server::Notification>
where
    N: lsp::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    let params = notification.extract::<N::Params>(N::METHOD)?;
    Ok(params)
}

fn convert_response<T>(
    id: lsp_server::RequestId,
    result: server::Response<T>,
) -> (lsp_server::Response, Vec<Diagnostic>)
where
    T: serde::Serialize,
{
    let server::Response {
        diagnostics,
        payload,
    } = result;

    let response = match payload {
        server::Payload::Ok(t) => lsp_server::Response {
            id,
            error: None,
            result: Some(serde_json::to_value(t).expect("json to_value")),
        },

        server::Payload::Null => lsp_server::Response {
            id,
            error: None,
            result: Some(serde_json::Value::Null),
        },

        server::Payload::Err(message) => lsp_server::Response {
            id,
            error: Some(lsp_server::ResponseError {
                code: 0, // TODO: Assign a code to each error.
                message,
                data: None,
            }),
            result: None,
        },
    };

    (response, diagnostics)
}

#[allow(clippy::large_enum_variant)]
enum LspDisplayable {
    Diagnostic(PathBuf, lsp::Diagnostic),
    Message(LspMessage),
}

fn diagnostic_to_lsp(diagnostic: Diagnostic) -> LspDisplayable {
    let severity = match diagnostic.level {
        Level::Error => lsp::DiagnosticSeverity::ERROR,
        Level::Warning => lsp::DiagnosticSeverity::WARNING,
    };
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

    match diagnostic.location {
        Some(location) => {
            let line_numbers = LineNumbers::new(&location.src);
            let diagnostic = lsp::Diagnostic {
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
            let path = location.path.canonicalize().expect("canonicalize");

            LspDisplayable::Diagnostic(path, diagnostic)
        }
        None => LspDisplayable::Message(LspMessage {
            level: diagnostic.level,
            text,
        }),
    }
}

fn path_to_uri(path: PathBuf) -> Url {
    let mut file: String = "file://".into();
    file.push_str(&path.as_os_str().to_string_lossy());
    Url::parse(&file).expect("path_to_uri URL parse")
}

/// A wrapper around the project compiler which makes it possible to repeatedly
/// recompile the top level package, reusing the information about the already
/// compiled dependency packages.
///
#[derive(Debug)]
pub struct LspProjectCompiler<IO> {
    project_compiler: ProjectCompiler<IO>,

    /// Whether the dependencies have been compiled previously.
    dependencies_compiled: bool,

    /// Information on compiled modules.
    modules: HashMap<String, Module>,
    sources: HashMap<String, ModuleSourceInformation>,

    /// The storage for the warning emitter.
    warnings: Arc<VectorWarningEmitterIO>,

    /// A lock to ensure the LSP and the CLI don't try and use build directory.
    /// at the same time.
    build_lock: BuildLock,
}

impl<IO> LspProjectCompiler<IO>
where
    IO: CommandExecutor + FileSystemIO + Clone,
{
    pub fn new(config: PackageConfig, io: IO) -> Result<Self> {
        let telemetry = NullTelemetry;
        let manifest = crate::dependencies::download(telemetry, None, UseManifest::Yes)?;
        let target = config.target;
        let name = config.name.clone();
        let build_lock = BuildLock::new_target(Mode::Lsp, target)?;
        let warnings = Arc::new(VectorWarningEmitterIO::default());

        let options = build::Options {
            warnings_as_errors: false,
            mode: build::Mode::Lsp,
            target: None,
            codegen: build::Codegen::None,
        };
        let mut project_compiler = ProjectCompiler::new(
            config,
            options,
            manifest.packages,
            Box::new(telemetry),
            warnings.clone(),
            io,
        );

        // To avoid the Erlang compiler printing to stdout (and thus
        // violating LSP which is currently using stdout) we silence it.
        project_compiler.subprocess_stdio = Stdio::Null;

        // The build caches do not contain all the information we need in the
        // LSP (e.g. the typed AST) so delete the caches for the top level
        // package before we run for the first time.
        // TODO: remove this once the caches have contain all the information
        {
            let _guard = build_lock.lock(&telemetry);
            fs::delete_dir(&paths::build_package(Mode::Lsp, target, &name))?;
        }

        Ok(Self {
            warnings,
            project_compiler,
            modules: HashMap::new(),
            sources: HashMap::new(),
            build_lock,
            dependencies_compiled: false,
        })
    }

    pub fn compile(&mut self) -> Result<(), Error> {
        // Lock the build directory to ensure to ensure we are the only one compiling
        let _lock = self.build_lock.lock(&NullTelemetry);

        if !self.dependencies_compiled {
            // TODO: store compiled module info
            self.project_compiler.compile_dependencies()?;
            self.dependencies_compiled = true;
        }

        // Save the state prior to compilation of the root package
        let checkpoint = self.project_compiler.checkpoint();

        // Do that there compilation. We don't use `?` to return early in the
        // event of an error because we _always_ want to do the restoration of
        // state afterwards.
        let result = self.project_compiler.compile_root_package();

        // Restore the state so that later we can compile the root again
        self.project_compiler.restore(checkpoint);

        // Return any error
        let package = result?;

        // Store the compiled module information
        for module in package.modules {
            let path = module.input_path.canonicalize().expect("Canonicalize");
            let path = path.as_os_str().to_string_lossy().to_string();
            let line_numbers = LineNumbers::new(&module.code);
            let source = ModuleSourceInformation { path, line_numbers };
            let _ = self.sources.insert(module.name.to_string(), source);
            let _ = self.modules.insert(module.name.to_string(), module);
        }

        Ok(())
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
