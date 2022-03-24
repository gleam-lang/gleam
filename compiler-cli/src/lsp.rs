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
    build::{self, Module, Package, ProjectCompiler},
    diagnostic::{self, Level},
    io::{CommandExecutor, FileSystemIO},
    line_numbers::LineNumbers,
    type_::{pretty::Printer, HasType},
    Error, Result,
};
use itertools::Itertools;
use lsp_types::{
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidSaveTextDocument},
    request::{Formatting, HoverRequest},
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidSaveTextDocumentParams, Hover, HoverContents, HoverProviderCapability, InitializeParams,
    MarkedString, OneOf, Position, PublishDiagnosticsParams, Range, SaveOptions,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, TextEdit, Url,
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
        hover_provider: Some(HoverProviderCapability::Simple(true)),
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
    initialise_params: InitializeParams,

    /// A cached copy of the absolute path of the project root
    project_root: PathBuf,

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

    /// The result of the previous succesful compilation
    modules: HashMap<String, Module>,
}

impl LanguageServer {
    pub fn new(initialise_params: InitializeParams) -> Result<Self> {
        let compiler = LspProjectCompiler::new(ProjectIO::new())?;
        let project_root = PathBuf::from("./").canonicalize().expect("Absolute root");
        Ok(Self {
            initialise_params,
            edited: HashMap::new(),
            modules: HashMap::new(),
            stored_diagnostics: HashMap::new(),
            published_diagnostics: HashSet::new(),
            project_root,
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

    fn push_diagnostic(&mut self, path: PathBuf, diagnostic: Diagnostic) {
        self.stored_diagnostics
            .entry(path)
            .or_default()
            .push(diagnostic);
    }

    /// Store a diagnostic locally so that it can later be published to the
    /// client with `publish_stored_diagnostics`
    fn store_diagnostic(&mut self, mut diagnostic: diagnostic::Diagnostic) {
        let hint = diagnostic.hint.take();
        match to_lsp_diagnostic(diagnostic) {
            Some((path, lsp_diagnostic)) => {
                self.push_diagnostic(path.clone(), lsp_diagnostic.clone());

                if let Some(hint) = hint {
                    let lsp_hint = Diagnostic {
                        severity: Some(DiagnosticSeverity::HINT),
                        message: hint,
                        ..lsp_diagnostic
                    };
                    self.push_diagnostic(path, lsp_hint);
                }
            }
            None => todo!("Locationless lsp diagnostic"),
        }
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
        self.start_watching_gleam_toml(&connection);

        // Compile the project once so we have all the state and any initial errors
        self.compile()?;
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
                    let (response, diagnostic) = result_to_response(result, id);
                    if let Some(diagnostic) = diagnostic {
                        self.store_diagnostic(diagnostic);
                        self.publish_stored_diagnostics(&connection);
                    }
                    connection
                        .sender
                        .send(lsp_server::Message::Response(response))
                        .unwrap();
                }

                lsp_server::Message::Response(_) => (),

                lsp_server::Message::Notification(notification) => {
                    self.handle_notification(&connection, notification).unwrap();
                }
            }
        }
        Ok(())
    }

    fn start_watching_gleam_toml(&mut self, connection: &lsp_server::Connection) {
        let supports_watch_files = self
            .initialise_params
            .capabilities
            .workspace
            .as_ref()
            .and_then(|w| w.did_change_watched_files)
            .map(|wf| wf.dynamic_registration == Some(true))
            .unwrap_or(false);
        if supports_watch_files {
            // Register gleam.toml as a watched file so we get a notification when
            // it changes and thus know that we need to rebuild the entire project.
            let watch_config = lsp_types::Registration {
                id: "watch-gleam-toml".into(),
                method: "workspace/didChangeWatchedFiles".into(),
                register_options: Some(
                    serde_json::value::to_value(
                        lsp_types::DidChangeWatchedFilesRegistrationOptions {
                            watchers: vec![lsp_types::FileSystemWatcher {
                                glob_pattern: "gleam.toml".into(),
                                kind: Some(lsp_types::WatchKind::Change),
                            }],
                        },
                    )
                    .unwrap(),
                ),
            };
            let request = lsp_server::Request {
                id: 1.into(),
                method: "client/registerCapability".into(),
                params: serde_json::value::to_value(lsp_types::RegistrationParams {
                    registrations: vec![watch_config],
                })
                .unwrap(),
            };
            connection
                .sender
                .send(lsp_server::Message::Request(request))
                .unwrap();
        } else {
            tracing::warn!("lsp_client_cannot_watch_gleam_toml");
        }
    }

    fn compile(&mut self) -> Result<(), Error> {
        let result = self.compiler.compile().map(|compiled| {
            self.modules = compiled.into_modules_hashmap();
        });

        self.store_result_diagnostics(result)?;
        Ok(())
    }

    fn take_and_store_warning_diagnostics(&mut self) {
        let warnings = self.compiler.project_compiler.take_warnings();
        for warn in warnings {
            let diagnostic = warn.to_diagnostic();
            self.store_diagnostic(diagnostic);
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
                Some(diagnostic) => self.store_diagnostic(diagnostic),
                None => return Err(error),
            }
        }

        Ok(())
    }

    fn handle_notification(
        &mut self,
        connection: &lsp_server::Connection,
        notification: lsp_server::Notification,
    ) -> Result<()> {
        match notification.method.as_str() {
            "textDocument/didSave" => {
                let params = cast_notification::<DidSaveTextDocument>(notification).unwrap();
                let result = self.text_document_did_save(params);
                self.publish_result_diagnostics(result, connection)
            }

            "textDocument/didClose" => {
                let params = cast_notification::<DidCloseTextDocument>(notification).unwrap();
                self.text_document_did_close(params)
            }

            "textDocument/didChange" => {
                let params = cast_notification::<DidChangeTextDocument>(notification).unwrap();
                self.text_document_did_change(params)
            }

            "workspace/didChangeWatchedFiles" => {
                tracing::info!("gleam_toml_changed_so_recompiling_full_project");
                self.compiler = LspProjectCompiler::new(ProjectIO::new())?;
                let _ = self.compiler.compile()?;
                Ok(())
            }

            _ => Ok(()),
        }
    }

    fn text_document_did_save(&mut self, params: DidSaveTextDocumentParams) -> Result<()> {
        // The file is in sync with the file system, discard our cache of the changes
        let _ = self.edited.remove(params.text_document.uri.path());
        // The files on disc have changed, so compile the project with the new changes
        self.compile()?;
        Ok(())
    }

    fn text_document_did_close(&mut self, params: DidCloseTextDocumentParams) -> Result<()> {
        // The file is in sync with the file system, discard our cache of the changes
        let _ = self.edited.remove(params.text_document.uri.path());
        Ok(())
    }

    fn text_document_did_change(&mut self, params: DidChangeTextDocumentParams) -> Result<()> {
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

            "textDocument/hover" => {
                let params = cast_request::<HoverRequest>(request).unwrap();
                let text_edit = self.hover(params)?;
                Ok(serde_json::to_value(text_edit).unwrap())
            }

            _ => todo!("Unsupported LSP request"),
        }
    }

    fn hover(&self, params: lsp_types::HoverParams) -> Result<Option<Hover>> {
        // TODO: compile the project before the hover

        let params = params.text_document_position_params;

        // Look up the type information for the module being hovered in
        let module = match self.get_module_for_uri(&params.text_document.uri) {
            Some(module) => module,
            // If we don't have a compiled version of the module for this URI
            // then there's nothing to show, so return None.
            None => return Ok(None),
        };

        let line_numbers = LineNumbers::new(&module.code);
        let byte_index = line_numbers.byte_index(params.position.line, params.position.character);

        // Find the AST node at the position of the hover, if there is one
        let expression = match module.find_node(byte_index) {
            Some(expression) => expression,
            None => return Ok(None),
        };

        // Show the type of the hovered node to the user
        let type_ = Printer::new().pretty_print(expression.type_().as_ref(), 0);
        let contents = format!(
            "```gleam
{}
```",
            type_
        );
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(contents)),
            range: Some(src_span_to_lsp_range(expression.location(), line_numbers)),
        }))
    }

    fn get_module_for_uri(&self, uri: &Url) -> Option<&Module> {
        let module_name = uri_to_module_name(uri, &self.project_root).unwrap();
        self.modules.get(&module_name)
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

fn uri_to_module_name(uri: &Url, root: &Path) -> Option<String> {
    let path = PathBuf::from(uri.path());
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
    Some(module_name)
}

#[test]
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
) -> (lsp_server::Response, Option<diagnostic::Diagnostic>) {
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

fn error_to_diagnostic(error: &Error) -> Option<diagnostic::Diagnostic> {
    let diagnostic = error.to_diagnostic();

    match diagnostic.location {
        Some(_) => Some(diagnostic),
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
