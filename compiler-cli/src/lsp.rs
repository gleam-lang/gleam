// TODO: remove this
#![allow(clippy::unwrap_used)]
#![allow(clippy::todo)]
#![allow(dead_code)]

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

use gleam_core::{
    ast::SrcSpan,
    build::{self, Package, ProjectCompiler},
    diagnostic::Level,
    io::{CommandExecutor, FileSystemIO},
    line_numbers::LineNumbers,
    Error, Result,
};
use lsp_types::{
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidSaveTextDocument},
    request::Formatting,
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidSaveTextDocumentParams, InitializeParams, OneOf, Position, PublishDiagnosticsParams, Range,
    SaveOptions, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions, TextEdit, Url,
};

use crate::{cli, fs::ProjectIO};

pub fn main() -> Result<()> {
    tracing::info!("language_server_starting");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = lsp_server::Connection::stdio();

    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: None,
                change: Some(TextDocumentSyncKind::FULL),
                will_save: None,
                will_save_wait_until: None,
                save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                    include_text: Some(false),
                })),
            },
        )),
        selection_range_provider: None,
        hover_provider: None,
        completion_provider: None,
        signature_help_provider: None,
        definition_provider: None,
        type_definition_provider: None,
        implementation_provider: None,
        references_provider: None,
        document_highlight_provider: None,
        document_symbol_provider: None,
        workspace_symbol_provider: None,
        code_action_provider: None,
        code_lens_provider: None,
        document_formatting_provider: Some(OneOf::Left(true)),
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
    };

    let server_capabilities_json =
        serde_json::to_value(&server_capabilities).expect("server_capabilities_serde");

    let initialization_params: InitializeParams =
        serde_json::from_value(connection.initialize(server_capabilities_json).unwrap()).unwrap();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    LanguageServer::new(initialization_params)?.run(connection)?;
    io_threads.join().expect("joining_lsp_threads");

    // Shut down gracefully.
    tracing::info!("language_server_stopped");
    Ok(())
}

pub struct LanguageServer {
    _initialise_params: InitializeParams,

    /// Files that have been edited in memory
    edited: HashMap<String, String>,

    /// Diagnostics that hae been emitted by the compiler but not yet published
    /// to the client
    stored_diagnostics: HashMap<PathBuf, Vec<Diagnostic>>,

    /// Files for which there are active diagnostics
    published_diagnostics: HashSet<Url>,

    /// A compiler for the project that supports repeat compilation of the root
    /// package.
    /// In the event the the project config changes this will need to be
    /// discarded and reloaded to handle any changes to dependencies.
    compiler: LspProjectCompiler<ProjectIO>,
}

impl LanguageServer {
    pub fn new(initialise_params: InitializeParams) -> Result<Self> {
        let io = ProjectIO::new();
        let compiler = LspProjectCompiler::new(io)?;
        Ok(Self {
            _initialise_params: initialise_params,
            edited: HashMap::new(),
            stored_diagnostics: HashMap::new(),
            published_diagnostics: HashSet::new(),
            compiler,
        })
    }

    /// Publish all stored diagnostics to the client.
    /// Any previously publish diagnostics are cleared before the new set are
    /// published to the client.
    fn publish_stored_diagnostics(&mut self, connection: &lsp_server::Connection) {
        self.clear_all_diagnostics(connection).unwrap();

        for (path, diagnostics) in self.stored_diagnostics.drain() {
            let uri = path_to_uri(path);

            // Record that we have published diagnostics to this file so we can
            // clear it later when they are outdated.
            let _ = self.published_diagnostics.insert(uri.clone());

            // Publish the diagnostics
            let diagnostic_params = PublishDiagnosticsParams {
                uri,
                diagnostics,
                version: None,
            };
            let notification = lsp_server::Notification {
                method: "textDocument/publishDiagnostics".into(),
                params: serde_json::to_value(diagnostic_params).unwrap(),
            };
            connection
                .sender
                .send(lsp_server::Message::Notification(notification))
                .unwrap();
        }
    }

    /// Store a diagnostic locally so that it can later be published to the
    /// client with `publish_stored_diagnostics`
    fn store_diagnostic(&mut self, path: PathBuf, diagnostic: Diagnostic) {
        self.stored_diagnostics
            .entry(path)
            .or_default()
            .push(diagnostic);
    }

    /// Clear all diagnostics that have been previously published to the client
    fn clear_all_diagnostics(&mut self, connection: &lsp_server::Connection) -> Result<(), Error> {
        for file in self.published_diagnostics.drain() {
            let notification = lsp_server::Notification {
                method: "textDocument/publishDiagnostics".into(),
                params: serde_json::to_value(PublishDiagnosticsParams {
                    uri: file,
                    diagnostics: vec![],
                    version: None,
                })
                .unwrap(),
            };
            connection
                .sender
                .send(lsp_server::Message::Notification(notification))
                .unwrap();
        }
        Ok(())
    }

    pub fn run(&mut self, connection: lsp_server::Connection) -> Result<()> {
        // Compile the project once so we have all the state and any initial errors
        let result = self.compiler.compile();
        self.store_result_diagnostics(result)?;
        self.publish_stored_diagnostics(&connection);

        // Enter the message loop, handling each message that comes in from the client
        for message in &connection.receiver {
            match message {
                lsp_server::Message::Request(request) => {
                    if connection.handle_shutdown(&request).expect("LSP error") {
                        return Ok(());
                    }
                    let id = request.id.clone();
                    let result = self.handle_request(request);
                    let (response, diagnostics) = result_to_response(result, id);
                    if let Some((path, diagnostics)) = diagnostics {
                        self.store_diagnostic(path, diagnostics);
                    }
                    self.take_and_store_warning_diagnostics();
                    self.publish_stored_diagnostics(&connection);
                    connection
                        .sender
                        .send(lsp_server::Message::Response(response))
                        .unwrap();
                }

                lsp_server::Message::Response(_) => {
                    todo!("Unexpected response message")
                }

                lsp_server::Message::Notification(notification) => {
                    self.handle_notification(&connection, notification).unwrap();
                }
            }
        }
        Ok(())
    }

    fn take_and_store_warning_diagnostics(&mut self) {
        let warnings = self.compiler.project_compiler.take_warnings();
        for warn in warnings {
            let diagnostic = warn.to_diagnostic();
            match to_lsp_diagnostic(diagnostic.clone()) {
                Some((path, lsp_diagnostic)) => {
                    self.store_diagnostic(path.clone(), lsp_diagnostic.clone());

                    if let Some(hint) = diagnostic.hint {
                        let lsp_hint = Diagnostic {
                            severity: Some(DiagnosticSeverity::HINT),
                            message: hint,
                            ..lsp_diagnostic
                        };
                        self.store_diagnostic(path, lsp_hint);
                    };
                }
                None => todo!("Locationless diagnostic"),
            }
        }
    }

    fn publish_result_diagnostics<T>(
        &mut self,
        result: Result<T>,
        connection: &lsp_server::Connection,
    ) -> Result<()> {
        self.store_result_diagnostics(result)?;
        self.publish_stored_diagnostics(connection);
        Ok(())
    }

    fn store_result_diagnostics<T>(&mut self, result: Result<T>) -> Result<()> {
        // Store warning diagnostics
        self.take_and_store_warning_diagnostics();

        // Store error diagnostics, if there are any
        if let Err(error) = result {
            match error_to_diagnostic(&error) {
                Some((path, diagnostic)) => self.store_diagnostic(path, diagnostic),
                None => return Err(error),
            }
        }

        Ok(())
    }

    fn handle_notification(
        &mut self,
        connection: &lsp_server::Connection,
        request: lsp_server::Notification,
    ) -> Result<()> {
        match request.method.as_str() {
            "textDocument/didSave" => {
                let params = cast_notification::<DidSaveTextDocument>(request).unwrap();
                let result = self.did_save(params);
                self.publish_result_diagnostics(result, connection)
            }

            "textDocument/didClose" => {
                let params = cast_notification::<DidCloseTextDocument>(request).unwrap();
                self.did_close(params)
            }

            "textDocument/didChange" => {
                let params = cast_notification::<DidChangeTextDocument>(request).unwrap();
                self.did_change(params)
            }

            _ => Ok(()),
        }
    }

    fn did_save(&mut self, params: DidSaveTextDocumentParams) -> Result<()> {
        // The file is in sync with the file system, discard our cache of the changes
        let _ = self.edited.remove(params.text_document.uri.path());
        // The files on disc have changed, so compile the project with the new changes
        let _ = self.compiler.compile()?;
        Ok(())
    }

    fn did_close(&mut self, params: DidCloseTextDocumentParams) -> Result<()> {
        // The file is in sync with the file system, discard our cache of the changes
        let _ = self.edited.remove(params.text_document.uri.path());
        Ok(())
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) -> Result<()> {
        // A file has changed in the editor so store a copy of the new content in memory
        let path = params.text_document.uri.path().to_string();
        if let Some(changes) = params.content_changes.into_iter().next() {
            let _ = self.edited.insert(path, changes.text);
        }
        Ok(())
    }

    fn handle_request(&self, request: lsp_server::Request) -> Result<serde_json::Value> {
        match request.method.as_str() {
            "textDocument/formatting" => {
                let params = cast_request::<Formatting>(request).unwrap();
                let text_edit = self.format(params)?;
                Ok(serde_json::to_value(text_edit).unwrap())
            }
            _ => todo!("Unsupported LSP request"),
        }
    }

    fn format(&self, params: lsp_types::DocumentFormattingParams) -> Result<Vec<TextEdit>> {
        let path = params.text_document.uri.path();
        let mut new_text = String::new();

        match self.edited.get(path) {
            // If we have a cached version of the file in memory format that
            Some(src) => {
                gleam_core::format::pretty(&mut new_text, src, Path::new(path))?;
            }

            // Otherwise format the file from disc
            None => {
                let src = crate::fs::read(&path)?;
                gleam_core::format::pretty(&mut new_text, &src, Path::new(path))?;
            }
        };

        Ok(vec![text_edit_replace(new_text)])
    }
}

fn cast_request<R>(request: lsp_server::Request) -> Result<R::Params, lsp_server::Request>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    let (_, params) = request.extract(R::METHOD)?;
    Ok(params)
}

fn cast_notification<N>(
    notification: lsp_server::Notification,
) -> Result<N::Params, lsp_server::Notification>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    let params = notification.extract::<N::Params>(N::METHOD)?;
    Ok(params)
}

fn text_edit_replace(new_text: String) -> TextEdit {
    TextEdit {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: u32::MAX,
                character: 0,
            },
        },
        new_text,
    }
}

fn result_to_response(
    result: Result<serde_json::Value>,
    id: lsp_server::RequestId,
) -> (lsp_server::Response, Option<(PathBuf, Diagnostic)>) {
    match result {
        Ok(result) => {
            let response = lsp_server::Response {
                id,
                error: None,
                result: Some(result),
            };
            (response, None)
        }

        Err(error) => match error_to_diagnostic(&error) {
            None => {
                let response = lsp_server::Response {
                    id,
                    error: Some(error_to_response_error(error)),
                    result: None,
                };
                (response, None)
            }

            Some(diagnostic_information) => {
                let response = lsp_server::Response {
                    id,
                    error: None,
                    result: Some(serde_json::json!(null)),
                };
                (response, Some(diagnostic_information))
            }
        },
    }
}

fn error_to_diagnostic(error: &Error) -> Option<(PathBuf, Diagnostic)> {
    let diagnostic = error.to_diagnostic();

    match to_lsp_diagnostic(diagnostic) {
        Some(lsp_diagnostic) => Some(lsp_diagnostic),
        None => todo!("Locationless error for LSP"),
    }
}

fn error_to_response_error(error: Error) -> lsp_server::ResponseError {
    lsp_server::ResponseError {
        code: 1, // We should assign a code to each error.
        message: error.pretty_string(),
        data: None,
    }
}

fn to_lsp_diagnostic(
    diagnostic: gleam_core::diagnostic::Diagnostic,
) -> Option<(PathBuf, Diagnostic)> {
    if let Some(location) = diagnostic.location {
        let (prefix, severity) = match diagnostic.level {
            Level::Error => ("Error", DiagnosticSeverity::ERROR),
            Level::Warning => ("Warning", DiagnosticSeverity::WARNING),
        };

        let mut message = format!("{}: {}", prefix, diagnostic.title);

        if let Some(label) = location.label.text {
            message.push_str("\n\n");
            message.push_str(&label);
            if !label.ends_with(['.', '?']) {
                message.push('.');
            }
        }

        if !diagnostic.text.is_empty() {
            message.push_str("\n\n");
            message.push_str(&diagnostic.text);
        }

        let line_numbers = LineNumbers::new(&location.src);
        let diagnostic = Diagnostic {
            range: src_span_to_lsp_range(location.label.span, line_numbers),
            severity: Some(severity),
            code: None,
            code_description: None,
            source: None,
            message,
            related_information: None,
            tags: None,
            data: None,
        };
        let path = location.path.canonicalize().unwrap();

        Some((path, diagnostic))
    } else {
        todo!("Locationless warning for LSP")
    }
}

fn path_to_uri(path: PathBuf) -> Url {
    let mut file: String = "file://".into();
    file.push_str(&path.as_os_str().to_string_lossy());
    Url::parse(&file).unwrap()
}

fn to_severity(level: Level) -> DiagnosticSeverity {
    match level {
        Level::Error => DiagnosticSeverity::ERROR,
        Level::Warning => DiagnosticSeverity::WARNING,
    }
}

/// A wrapper around the project compiler which makes it possible to repeatedly
/// recompile the top level package, reusing the information about the already
/// compiled dependency packages.
///
#[derive(Debug)]
pub struct LspProjectCompiler<IO> {
    project_compiler: ProjectCompiler<IO>,

    /// Whether the dependencies have been compiled previously
    dependencies_compiled: bool,
}

impl<IO> LspProjectCompiler<IO>
where
    IO: CommandExecutor + FileSystemIO + Clone,
{
    pub fn new(io: IO) -> Result<Self> {
        // TODO: different telemetry that doesn't write to stdout
        let telemetry = Box::new(cli::Reporter::new());
        let manifest = crate::dependencies::download(None)?;
        let config = crate::config::root_config()?;

        let options = build::Options {
            mode: build::Mode::Dev,
            target: None,
            perform_codegen: false,
        };
        let project_compiler =
            ProjectCompiler::new(config, options, manifest.packages, telemetry, io);

        Ok(Self {
            project_compiler,
            dependencies_compiled: false,
        })
    }

    pub fn compile(&mut self) -> Result<Package, Error> {
        if !self.dependencies_compiled {
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

        result
    }
}

fn src_span_to_lsp_range(location: SrcSpan, line_numbers: LineNumbers) -> Range {
    let start = line_numbers.line_and_column_number(location.start);
    let end = line_numbers.line_and_column_number(location.end);
    Range {
        start: Position {
            line: start.line as u32 - 1,
            character: start.column as u32 - 1,
        },
        end: Position {
            line: end.line as u32 - 1,
            character: end.column as u32 - 1,
        },
    }
}
