// This module is a prototype-y mess. It has lots of TODO comments in it. Let's
// resolve them all, inject all the IO, wrap a bunch of tests around it, and
// move it into the `gleam_core` package.

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

use crate::{build_lock::BuildLock, fs::ProjectIO, telemetry::NullTelemetry};
use gleam_core::{
    ast::{SrcSpan, TypedExpr},
    build::{self, Module, ProjectCompiler},
    config::PackageConfig,
    diagnostic::{self, Level},
    io::{CommandExecutor, FileSystemIO},
    line_numbers::LineNumbers,
    paths,
    type_::{pretty::Printer, HasType},
    Error, Result,
};
use itertools::Itertools;
use lsp::request::GotoDefinition;
use lsp_types::{
    self as lsp,
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidSaveTextDocument},
    request::{Completion, Formatting, HoverRequest},
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidSaveTextDocumentParams, Hover,
    HoverContents, HoverProviderCapability, InitializeParams, MarkedString, Position,
    PublishDiagnosticsParams, Range, TextEdit, Url,
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
        serde_json::to_value(&server_capabilities).expect("server_capabilities_serde");

    let initialization_params: InitializeParams = serde_json::from_value(
        connection
            .initialize(server_capabilities_json)
            .expect("LSP initialize"),
    )
    .expect("LSP InitializeParams from json");

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    LanguageServer::new(initialization_params, config)?.run(connection)?;
    io_threads.join().expect("joining_lsp_threads");

    // Shut down gracefully.
    tracing::info!("language_server_stopped");
    Ok(())
}

fn server_capabilities() -> lsp::ServerCapabilities {
    lsp::ServerCapabilities {
        text_document_sync: Some(lsp::TextDocumentSyncCapability::Options(
            lsp::TextDocumentSyncOptions {
                open_close: None,
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
        // completion_provider: Some(lsp::CompletionOptions {
        //     resolve_provider: None,
        //     trigger_characters: Some(vec![".".into()]), // TODO: can we include pipe here?
        //     all_commit_characters: None,
        //     work_done_progress_options: lsp::WorkDoneProgressOptions {
        //         work_done_progress: None,
        //     },
        // }),
        completion_provider: None,
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

pub struct LanguageServer {
    initialise_params: InitializeParams,

    /// A cached copy of the absolute path of the project root
    project_root: PathBuf,

    /// Files that have been edited in memory
    edited: HashMap<String, String>,

    /// Diagnostics that have been emitted by the compiler but not yet published
    /// to the client
    stored_diagnostics: HashMap<PathBuf, Vec<lsp::Diagnostic>>,
    /// Diagnostics that have been emitted by the compiler but not yet published
    /// to the client. These are likely locationless Gleam diagnostics, as LSP
    /// diagnostics always need a location.
    stored_messages: Vec<LspMessage>,

    /// Files for which there are active diagnostics
    published_diagnostics: HashSet<Url>,

    /// A compiler for the project that supports repeat compilation of the root
    /// package.
    /// In the event the the project config changes this will need to be
    /// discarded and reloaded to handle any changes to dependencies.
    compiler: Option<LspProjectCompiler<ProjectIO>>,

    config: Option<PackageConfig>,
}

impl LanguageServer {
    pub fn new(initialise_params: InitializeParams, config: Option<PackageConfig>) -> Result<Self> {
        let project_root = std::env::current_dir().expect("Project root");
        let mut language_server = Self {
            initialise_params,
            edited: HashMap::new(),
            stored_messages: Vec::new(),
            stored_diagnostics: HashMap::new(),
            published_diagnostics: HashSet::new(),
            project_root,
            compiler: None,
            config,
        };
        language_server.create_new_compiler()?;
        Ok(language_server)
    }

    /// Publish all stored diagnostics to the client.
    /// Any previously publish diagnostics are cleared before the new set are
    /// published to the client.
    fn publish_stored_diagnostics(&mut self, connection: &lsp_server::Connection) -> Result<()> {
        self.clear_all_diagnostics(connection)?;

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
                params: serde_json::to_value(diagnostic_params)
                    .expect("textDocument/publishDiagnostics to json"),
            };
            connection
                .sender
                .send(lsp_server::Message::Notification(notification))
                .expect("send textDocument/publishDiagnostics");
        }

        for message in self.stored_messages.drain(..) {
            let params = lsp::ShowMessageParams {
                typ: match message.level {
                    Level::Error => lsp::MessageType::ERROR,
                    Level::Warning => lsp::MessageType::WARNING,
                },
                message: message.text,
            };

            let notification = lsp_server::Notification {
                method: "window/showMessage".into(),
                params: serde_json::to_value(params).expect("window/showMessage to json"),
            };

            connection
                .sender
                .send(lsp_server::Message::Notification(notification))
                .expect("send window/showMessage");
        }
        Ok(())
    }

    fn push_diagnostic(&mut self, path: PathBuf, diagnostic: lsp::Diagnostic) {
        self.stored_diagnostics
            .entry(path)
            .or_default()
            .push(diagnostic);
    }

    /// Convert Gleam diagnostics into 1 or more LSP diagnostics and store them
    /// so that they can later be published to the client with
    /// `publish_stored_diagnostics`
    ///
    /// If the Gleam diagnostic cannot be converted to LSP diagnostic (due to it
    /// not having a location) it is stored as a message suitable for use with
    /// the `showMessage` notification instead.
    ///
    fn process_gleam_diagnostic(&mut self, mut diagnostic: diagnostic::Diagnostic) {
        let hint = diagnostic.hint.take();
        match diagnostic_to_lsp(diagnostic) {
            LspDisplayable::Diagnostic(path, lsp_diagnostic) => {
                self.push_diagnostic(path.clone(), lsp_diagnostic.clone());

                if let Some(hint) = hint {
                    let lsp_hint = lsp::Diagnostic {
                        severity: Some(lsp::DiagnosticSeverity::HINT),
                        message: hint,
                        ..lsp_diagnostic
                    };
                    self.push_diagnostic(path, lsp_hint);
                }
            }
            LspDisplayable::Message(message) => self.stored_messages.push(message),
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
                .expect("textDocument/publishDiagnostics to json"),
            };
            connection
                .sender
                .send(lsp_server::Message::Notification(notification))
                .expect("send textDocument/publishDiagnostics");
        }
        Ok(())
    }

    pub fn run(&mut self, connection: lsp_server::Connection) -> Result<()> {
        self.create_compilation_progress_token(&connection);
        self.start_watching_gleam_toml(&connection);

        // Compile the project once so we have all the state and any initial errors
        self.compile(&connection)?;
        self.publish_stored_diagnostics(&connection)?;

        // Enter the message loop, handling each message that comes in from the client
        for message in &connection.receiver {
            match message {
                lsp_server::Message::Request(request) => {
                    if connection.handle_shutdown(&request).expect("LSP shutdown") {
                        return Ok(());
                    }
                    let id = request.id.clone();
                    let result = self.handle_request(request);
                    let (response, diagnostic) = result_to_response(result, id);
                    if let Some(diagnostic) = diagnostic {
                        self.process_gleam_diagnostic(diagnostic);
                        self.publish_stored_diagnostics(&connection)?;
                    }
                    connection
                        .sender
                        .send(lsp_server::Message::Response(response))
                        .expect("channel send LSP response")
                }

                lsp_server::Message::Response(_) => (),

                lsp_server::Message::Notification(notification) => {
                    self.handle_notification(&connection, notification)?;
                }
            }
        }
        Ok(())
    }

    fn create_compilation_progress_token(&mut self, connection: &lsp_server::Connection) {
        let params = lsp::WorkDoneProgressCreateParams {
            token: lsp::NumberOrString::String(COMPILING_PROGRESS_TOKEN.into()),
        };
        let request = lsp_server::Request {
            id: CREATE_COMPILING_PROGRESS_TOKEN.to_string().into(),
            method: "window/workDoneProgress/create".into(),
            params: serde_json::to_value(&params).expect("WorkDoneProgressCreateParams json"),
        };
        connection
            .sender
            .send(lsp_server::Message::Request(request))
            .expect("WorkDoneProgressCreate");
    }

    fn notify_client_of_compilation_start(&self, connection: &lsp_server::Connection) {
        self.send_work_done_notification(
            connection,
            lsp::WorkDoneProgress::Begin(lsp::WorkDoneProgressBegin {
                title: "Compiling Gleam".into(),
                cancellable: Some(false),
                message: None,
                percentage: None,
            }),
        );
    }

    fn notify_client_of_compilation_end(&self, connection: &lsp_server::Connection) {
        self.send_work_done_notification(
            connection,
            lsp::WorkDoneProgress::End(lsp::WorkDoneProgressEnd { message: None }),
        );
    }

    fn send_work_done_notification(
        &self,
        connection: &lsp_server::Connection,
        work_done: lsp::WorkDoneProgress,
    ) {
        tracing::info!("sending {:?}", work_done);
        let params = lsp::ProgressParams {
            token: lsp::NumberOrString::String(COMPILING_PROGRESS_TOKEN.to_string()),
            value: lsp::ProgressParamsValue::WorkDone(work_done),
        };
        let notification = lsp_server::Notification {
            method: "$/progress".into(),
            params: serde_json::to_value(&params).expect("ProgressParams json"),
        };
        connection
            .sender
            .send(lsp_server::Message::Notification(notification))
            .expect("send_work_done_notification send")
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

        if !supports_watch_files {
            tracing::warn!("lsp_client_cannot_watch_gleam_toml");
            return;
        }

        // Register gleam.toml as a watched file so we get a notification when
        // it changes and thus know that we need to rebuild the entire project.
        let watch_config = lsp::Registration {
            id: "watch-gleam-toml".into(),
            method: "workspace/didChangeWatchedFiles".into(),
            register_options: Some(
                serde_json::value::to_value(lsp::DidChangeWatchedFilesRegistrationOptions {
                    watchers: vec![lsp::FileSystemWatcher {
                        glob_pattern: "gleam.toml".into(),
                        kind: Some(lsp::WatchKind::Change),
                    }],
                })
                .expect("workspace/didChangeWatchedFiles to json"),
            ),
        };
        let request = lsp_server::Request {
            id: 1.into(),
            method: "client/registerCapability".into(),
            params: serde_json::value::to_value(lsp::RegistrationParams {
                registrations: vec![watch_config],
            })
            .expect("client/registerCapability to json"),
        };
        connection
            .sender
            .send(lsp_server::Message::Request(request))
            .expect("send client/registerCapability");
    }

    /// Compile the project if we are in one. Otherwise do nothing.
    fn compile(&mut self, connection: &lsp_server::Connection) -> Result<(), Error> {
        self.notify_client_of_compilation_start(connection);
        if let Some(compiler) = self.compiler.as_mut() {
            let result = compiler.compile();
            self.store_result_diagnostics(result)?;
        }
        self.notify_client_of_compilation_end(connection);
        Ok(())
    }

    fn take_and_store_warning_diagnostics(&mut self) {
        if let Some(compiler) = self.compiler.as_mut() {
            let warnings = compiler.project_compiler.take_warnings();
            for warn in warnings {
                let diagnostic = warn.to_diagnostic();
                self.process_gleam_diagnostic(diagnostic);
            }
        }
    }

    fn publish_result_diagnostics<T>(
        &mut self,
        result: Result<T>,
        connection: &lsp_server::Connection,
    ) -> Result<()> {
        self.store_result_diagnostics(result)?;
        self.publish_stored_diagnostics(connection)?;
        Ok(())
    }

    fn store_result_diagnostics<T>(&mut self, result: Result<T>) -> Result<()> {
        // Store warning diagnostics
        self.take_and_store_warning_diagnostics();

        // Store error diagnostics, if there are any
        if let Err(error) = result {
            self.process_gleam_diagnostic(error.to_diagnostic());
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
                let params = cast_notification::<DidSaveTextDocument>(notification)
                    .expect("cast DidSaveTextDocument");
                let result = self.text_document_did_save(params, connection);
                self.publish_result_diagnostics(result, connection)
            }

            "textDocument/didClose" => {
                let params = cast_notification::<DidCloseTextDocument>(notification)
                    .expect("cast DidCloseTextDocument");
                self.text_document_did_close(params)
            }

            "textDocument/didChange" => {
                let params = cast_notification::<DidChangeTextDocument>(notification)
                    .expect("cast DidChangeTextDocument");
                self.text_document_did_change(params)
            }

            "workspace/didChangeWatchedFiles" => {
                tracing::info!("gleam_toml_changed_so_recompiling_full_project");
                self.create_new_compiler()?;
                let _ = self.compile(connection)?;
                Ok(())
            }

            _ => Ok(()),
        }
    }

    fn create_new_compiler(&mut self) -> Result<(), Error> {
        if let Some(config) = self.config.as_ref() {
            let compiler = LspProjectCompiler::new(config.clone(), ProjectIO::new())?;
            self.compiler = Some(compiler);
        }
        Ok(())
    }

    fn text_document_did_save(
        &mut self,
        params: DidSaveTextDocumentParams,
        connection: &lsp_server::Connection,
    ) -> Result<()> {
        // The file is in sync with the file system, discard our cache of the changes
        let _ = self.edited.remove(params.text_document.uri.path());
        // The files on disc have changed, so compile the project with the new changes
        self.compile(connection)?;
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
                let params = cast_request::<Formatting>(request).expect("cast Formatting");
                let text_edit = self.format(params)?;
                Ok(serde_json::to_value(text_edit).expect("TextEdits to json"))
            }

            "textDocument/hover" => {
                let params = cast_request::<HoverRequest>(request).expect("cast HoverRequest");
                let text_edit = self.hover(params)?;
                Ok(serde_json::to_value(text_edit).expect("Hover to json"))
            }

            "textDocument/definition" => {
                let params = cast_request::<GotoDefinition>(request).expect("cast GotoDefinition");
                let location = self.goto_definition(params)?;
                Ok(serde_json::to_value(location).expect("Location to json"))
            }

            "textDocument/completion" => {
                let params = cast_request::<Completion>(request).expect("cast Completion");
                let completions = self.completion(params)?;
                Ok(serde_json::to_value(completions).expect("Completions to json"))
            }

            _ => panic!("Unsupported LSP request"),
        }
    }

    // TODO: test local variables
    // TODO: test same module constants
    // TODO: test imported module constants
    // TODO: test unqualified imported module constants
    // TODO: test same module records
    // TODO: test imported module records
    // TODO: test unqualified imported module records
    // TODO: test same module functions
    // TODO: test module function calls
    // TODO: test different package module function calls
    //
    //
    //
    // TODO: implement unqualified imported module functions
    // TODO: implement goto definition of modules that do not belong to the top
    // level package.
    //
    fn goto_definition(&self, params: lsp::GotoDefinitionParams) -> Result<Option<lsp::Location>> {
        let params = params.text_document_position_params;
        let (line_numbers, node) = match self.node_at_position(&params) {
            Some(location) => location,
            None => return Ok(None),
        };

        let location = match node.definition_location() {
            Some(location) => location,
            None => return Ok(None),
        };

        let (uri, line_numbers) = match location.module {
            None => (params.text_document.uri, &line_numbers),
            Some(name) => {
                let module = match self
                    .compiler
                    .as_ref()
                    .and_then(|compiler| compiler.sources.get(name))
                {
                    Some(module) => module,
                    // TODO: support goto definition for functions defined in
                    // different packages. Currently it is not possible as the
                    // required LineNumbers and source file path information is
                    // not stored in the module metadata.
                    None => return Ok(None),
                };
                let url = Url::parse(&format!("file:///{}", &module.path))
                    .expect("goto definition URL parse");
                (url, &module.line_numbers)
            }
        };
        let range = src_span_to_lsp_range(location.span, line_numbers);

        Ok(Some(lsp::Location { uri, range }))
    }

    // TODO: function & constructor labels
    // TODO: importable modules
    // TODO: module types (including private)
    // TODO: module values (including private)
    // TODO: locally defined variables
    // TODO: imported module values
    // TODO: imported module types
    // TODO: record accessors
    fn completion(&self, params: lsp::CompletionParams) -> Result<Vec<lsp::CompletionItem>> {
        // Look up the type information for the module being hovered in
        let module = match self.module_for_uri(&params.text_document_position.text_document.uri) {
            Some(module) => module,
            // If we don't have a compiled version of the module for this URI
            // then there's nothing to show, so return None.
            None => return Ok(vec![]),
        };

        let mut items = vec![];

        // TODO: replace this with one that includes private values too
        for (name, _constructor) in module.ast.type_info.values.iter() {
            items.push(lsp::CompletionItem {
                label: name.clone(),
                kind: None,
                documentation: None,
                ..Default::default()
            })
        }

        // TODO: replace this with one that includes private types too
        for (name, _constructor) in module.ast.type_info.types.iter() {
            items.push(lsp::CompletionItem {
                label: name.clone(),
                kind: None,
                documentation: None,
                ..Default::default()
            })
        }

        items.push(lsp::CompletionItem {
            label: "import gleam/result".into(),
            kind: None,
            documentation: None,
            ..Default::default()
        });
        items.push(lsp::CompletionItem {
            label: "import gleam/map".into(),
            kind: None,
            documentation: None,
            ..Default::default()
        });
        items.push(lsp::CompletionItem {
            label: "import gleam/list".into(),
            kind: None,
            documentation: None,
            ..Default::default()
        });
        items.push(lsp::CompletionItem {
            label: "list.map".into(),
            kind: None,
            documentation: None,
            ..Default::default()
        });

        Ok(items)
    }

    fn hover(&self, params: lsp::HoverParams) -> Result<Option<Hover>> {
        let params = params.text_document_position_params;

        let (line_numbers, expression) = match self.node_at_position(&params) {
            Some(value) => value,
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
            range: Some(src_span_to_lsp_range(expression.location(), &line_numbers)),
        }))
    }

    fn node_at_position(
        &self,
        params: &lsp::TextDocumentPositionParams,
    ) -> Option<(LineNumbers, &TypedExpr)> {
        let module = self.module_for_uri(&params.text_document.uri)?;
        let line_numbers = LineNumbers::new(&module.code);
        let byte_index = line_numbers.byte_index(params.position.line, params.position.character);
        let expression = module.find_node(byte_index)?;
        Some((line_numbers, expression))
    }

    fn module_for_uri(&self, uri: &Url) -> Option<&Module> {
        self.compiler.as_ref().and_then(|compiler| {
            let module_name =
                uri_to_module_name(uri, &self.project_root).expect("uri to module name");
            compiler.modules.get(&module_name)
        })
    }

    fn format(&self, params: lsp::DocumentFormattingParams) -> Result<Vec<TextEdit>> {
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

        Err(error) => {
            let diagnostic = error.to_diagnostic();
            if diagnostic.location.is_some() {
                let response = lsp_server::Response {
                    id,
                    error: None,
                    result: Some(serde_json::json!(null)),
                };
                (response, Some(diagnostic))
            } else {
                let response = lsp_server::Response {
                    id,
                    error: Some(error_to_response_error(error)),
                    result: None,
                };
                (response, None)
            }
        }
    }
}

fn error_to_response_error(error: Error) -> lsp_server::ResponseError {
    lsp_server::ResponseError {
        code: 1, // We should assign a code to each error.
        message: error.pretty_string(),
        data: None,
    }
}

#[allow(clippy::large_enum_variant)]
enum LspDisplayable {
    Diagnostic(PathBuf, lsp::Diagnostic),
    Message(LspMessage),
}

fn diagnostic_to_lsp(diagnostic: gleam_core::diagnostic::Diagnostic) -> LspDisplayable {
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

    /// Whether the dependencies have been compiled previously
    dependencies_compiled: bool,

    // Information on compiled modules
    modules: HashMap<String, Module>,
    sources: HashMap<String, ModuleSourceInformation>,

    /// A lock to ensure the LSP and the CLI don't try and use build directory
    /// at the same time.
    build_lock: BuildLock,
}

impl<IO> LspProjectCompiler<IO>
where
    IO: CommandExecutor + FileSystemIO + Clone,
{
    pub fn new(config: PackageConfig, io: IO) -> Result<Self> {
        // TODO: different telemetry that doesn't write to stdout
        let telemetry = NullTelemetry;
        let manifest = crate::dependencies::download(telemetry, None)?;

        let options = build::Options {
            mode: build::Mode::Dev,
            target: None,
            perform_codegen: false,
        };
        let mut project_compiler =
            ProjectCompiler::new(config, options, manifest.packages, Box::new(telemetry), io);
        // To avoid the Erlang compiler printing to stdout (and thus
        // violating LSP which is currently using stdout) we silence it.
        project_compiler.silence_subprocess_stdout = true;

        Ok(Self {
            project_compiler,
            modules: HashMap::new(),
            sources: HashMap::new(),
            build_lock: BuildLock::new()?,
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
            let _ = self.sources.insert(module.name.clone(), source);
            let _ = self.modules.insert(module.name.clone(), module);
        }

        Ok(())
    }
}

fn src_span_to_lsp_range(location: SrcSpan, line_numbers: &LineNumbers) -> Range {
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
