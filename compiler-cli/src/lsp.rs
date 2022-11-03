// This module is a prototype-y mess. It has lots of TODO comments in it. Let's
// resolve them all, inject all the IO, wrap a bunch of tests around it, and
// move it into the `gleam_core` package.

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

use crate::{
    build_lock::BuildLock, dependencies::UseManifest, fs::ProjectIO, telemetry::NullTelemetry,
};
use gleam_core::{
    ast::{SrcSpan, Statement},
    build::{self, Located, Module, ProjectCompiler},
    config::PackageConfig,
    diagnostic::{self, Level},
    io::{CommandExecutor, FileSystemIO, Stdio},
    line_numbers::LineNumbers,
    paths,
    type_::pretty::Printer,
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

        use std::fs::OpenOptions;
        use std::io::prelude::*;
        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .append(true)
            .open("/tmp/lsp.log")
            .unwrap();

        // Enter the message loop, handling each message that comes in from the client
        for message in &connection.receiver {
            writeln!(file, "got msg {:?}", message);
            file.flush();

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
                self.compile(connection)?;
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
                let completions = self.completion(params);
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
    // TODO: module types (including private)
    // TODO: module values (including private)
    // TODO: locally defined variables
    // TODO: imported module values
    // TODO: imported module types
    // TODO: record accessors
    fn completion(&self, params: lsp::CompletionParams) -> Option<Vec<lsp::CompletionItem>> {
        use std::fs::OpenOptions;
        use std::io::prelude::*;
        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .append(true)
            .open("/tmp/lsp.log")
            .unwrap();

        writeln!(file, "{:?}", self.edited);

        writeln!(file, "completiong called (*)");
        file.flush();

        let position = &params.text_document_position;
        let nm = position.text_document.uri.to_string();
        let nm1 = nm.strip_prefix("file://");

        writeln!(file, "nm {:?}", nm1);
        file.flush();

        if let Some(name) = nm1 {
            let module = self.edited.get(name);
            if let Some(module) = module {
                writeln!(file, "{:?}", "before line numbers");
                file.flush();

                //let line_numbers = LineNumbers::new(&module);
                //let byte_index =
                //    line_numbers.byte_index(position.position.line, position.position.character);
                //let node = module.find_node(byte_index);
                let tree = new_tree(module);

                writeln!(file, "this {:?}", tree);
                file.flush();

                let res = dot_for_node(
                    &tree,
                    tree_sitter::Point::new(
                        position.position.line as usize,
                        (position.position.character - 1) as usize,
                    ),
                    LspEnv {},
                );

                writeln!(file, "should be {:?}", res);
                file.flush();

                //  Option<Vec<lsp::CompletionItem>>
                if let Some(WhatToDisplay::Names(x)) = res {
                    return Some(
                        x.into_iter()
                            .map(|label| lsp::CompletionItem {
                                label,
                                kind: None,
                                documentation: None,
                                ..Default::default()
                            })
                            .collect(),
                    );
                }
            } else {
                writeln!(file, "module not found");
                file.flush();
            }
        }

        return None;
    }

    fn completion_for_import(&self) -> Option<Vec<lsp::CompletionItem>> {
        let compiler = self.compiler.as_ref()?;
        let dependencies_modules = compiler
            .project_compiler
            .get_importable_modules()
            .keys()
            .cloned();
        let project_modules = compiler
            .modules
            .iter()
            .filter(|(_name, module)| module.origin.is_src())
            .map(|(name, _module)| name)
            .cloned();
        let modules = dependencies_modules
            .chain(project_modules)
            .map(|label| lsp::CompletionItem {
                label,
                kind: None,
                documentation: None,
                ..Default::default()
            })
            .collect();
        Some(modules)
    }

    fn hover(&self, params: lsp::HoverParams) -> Result<Option<Hover>> {
        let params = params.text_document_position_params;

        let (line_numbers, found) = match self.node_at_position(&params) {
            Some(value) => value,
            None => return Ok(None),
        };

        let expression = match found {
            Located::Expression(expression) => expression,
            Located::Statement(_) => return Ok(None),
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
    ) -> Option<(LineNumbers, Located<'_>)> {
        let module = self.module_for_uri(&params.text_document.uri);
        let module = module?;
        let line_numbers = LineNumbers::new(&module.code);
        let byte_index = line_numbers.byte_index(params.position.line, params.position.character);
        let node = module.find_node(byte_index);
        let node = node?;
        Some((line_numbers, node))
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
                let src = crate::fs::read(path)?;
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
        let manifest = crate::dependencies::download(telemetry, None, UseManifest::Yes)?;

        let options = build::Options {
            mode: build::Mode::Dev,
            target: None,
            perform_codegen: false,
        };
        let mut project_compiler =
            ProjectCompiler::new(config, options, manifest.packages, Box::new(telemetry), io);
        // To avoid the Erlang compiler printing to stdout (and thus
        // violating LSP which is currently using stdout) we silence it.
        project_compiler.subprocess_stdio = Stdio::Null;

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
            line: start.line - 1,
            character: start.column - 1,
        },
        end: Position {
            line: end.line - 1,
            character: end.column - 1,
        },
    }
}

use tree_sitter::{Parser, Tree, TreeCursor};

use tree_sitter::Point;

#[test]
fn lsp_binds_in_scope() {
    let contents: &str = r#"fn main(p,  p1 : Bool) {
  let a = 1
  let #(b, _) = #(1, 1)
  let c : Int = 1
  io.
}
const co = "lol";
"#;
    let lex = gleam_core::parse::lexer::make_tokenizer(contents);
    let mut parser = gleam_core::parse::Parser::new(lex);
    let expr = parser.parse_statement();
    //let expr = parser.ensure_no_errors_or_remaining_input(expr);

    println!("wtf");
    println!("expression: {expr:#?}");

    let tree = new_tree(contents);

    tree.tree.root_node().print(0);

    let res = binds_in_scope(&tree, tree_sitter::Point::new(4, 4));
    assert_eq!(
        res,
        [
            ("a".to_string(), "Int".to_string()),
            ("b".to_string(), "Int".to_string()),
            ("c".to_string(), "unknown".to_string()),
            ("p".to_string(), "unknown".to_string()),
            ("p1".to_string(), "Wow1".to_string())
        ]
        .to_vec()
    );
}

//let mut statements : Vec<gleam_core::ast::Statement> = Vec::new();

#[derive(Debug)]
struct Bindings {
    name: String,
    type_notation: String,
}

use gleam_core::ast::{Arg, ArgNames};

fn parse_function_args(text: &str) -> Vec<Bindings> {
    let lex = gleam_core::parse::lexer::make_tokenizer(text);
    let mut parser = gleam_core::parse::Parser::new(lex);

    let _ = parser.expect_one(&gleam_core::parse::token::Token::LeftParen);
    let expr = parser.parse_function_args(false);
    // let expr = parser.ensure_no_errors_or_remaining_input(expr);

    println!("{expr:#?}");

    match expr {
        Ok(args) => args
            .iter()
            .map(|arg| match arg {
                Arg {
                    names: ArgNames::Named { name: n },
                    ..
                } => Some(Bindings {
                    name: n.clone(),
                    type_notation: "".to_string(),
                }),
                _ => None,
            })
            .filter(|x| x.is_some())
            .map(|x| x.expect("is some"))
            .collect(),
        _ => Vec::new(),
    }
}

fn parse_function_body(text: &str) -> Vec<Bindings> {
    let lex = gleam_core::parse::lexer::make_tokenizer(text);
    let mut parser = gleam_core::parse::Parser::new(lex);

    let expr = parser.parse_expression_seq();

    println!("{text:#?}");
    println!("{expr:#?}");

    let mut outbindings = Vec::new();

    if let Ok(expr) = expr {
        if let Some(ast) = expr {
            extract_bindings_from_ast(&ast.0, &mut outbindings)
        }
    }

    outbindings
}

fn extract_binding_from_assign(pattern: &gleam_core::ast::Pattern<(), ()>, to: &mut Vec<Bindings>) {
    match pattern {
        gleam_core::ast::Pattern::Var { name: n, .. } => to.push(Bindings {
            name: n.to_string(),
            type_notation: "unknown".to_string(),
        }),
        gleam_core::ast::Pattern::Tuple { elems, .. } => elems
            .iter()
            .for_each(|x| extract_binding_from_assign(x, to)),
        _ => (),
    }
}

fn extract_bindings_from_ast(ast: &UntypedExpr, to: &mut Vec<Bindings>) {
    match ast {
        gleam_core::ast::UntypedExpr::Assignment { pattern: p, .. } => {
            extract_binding_from_assign(p, to)
        }
        gleam_core::ast::UntypedExpr::Sequence {
            expressions: exprs, ..
        } => exprs.iter().for_each(|x| extract_bindings_from_ast(x, to)),
        something => println!("unhandled: {something:?}"),
    }
}

use std::fs::OpenOptions;
use std::io::prelude::*;

fn binds_in_scope(module: &PartialParsedModule, point: Point) -> Vec<(String, String)> {
    let prev_point = Point {
        column: if point.column > 0 {
            point.column - 1
        } else {
            point.column
        },
        ..point
    };
    let mut node = module
        .tree
        .root_node()
        .named_descendant_for_point_range(prev_point, point);

    let mut file: std::fs::File = OpenOptions::new()
        .write(true)
        .create(true)
        .append(true)
        .open("/tmp/lsp.log")
        .unwrap();

    if node.is_none() {
        writeln!(file, "cursor was none");
        file.flush();

        return Vec::new();
    }

    writeln!(file, "cursor was {:?}", node);
    file.flush();

    //are we in a function ?
    //let tc : TreeCursor = node.expect("has one").walk();
    let mut collected: Vec<tree_sitter::Node<'_>> = Vec::new();
    let mut bindings: Vec<Bindings> = Vec::new();

    // get its parameters
    // get the previous statements, parent by parent
    loop {
        if node == None {
            break;
        }
        let parent = node.expect("already has").parent();
        //reached top level
        if parent.is_none() {
            break;
        }

        let prev_sibling = node.expect("already has").prev_sibling();
        //reached top level
        if prev_sibling.is_none() {
            node = parent;
        } else {
            match node.expect("already has").kind() {
                "function_body" => bindings.append(&mut parse_function_body(
                    node.expect("")
                        .utf8_text(module.source_code.as_bytes())
                        .expect("source should have code"),
                )),
                "function_parameters" => bindings.append(&mut parse_function_args(
                    node.expect("")
                        .utf8_text(module.source_code.as_bytes())
                        .expect("source should have code"),
                )),
                "function" => collected.push(node.unwrap()),
                _ => (),
            }
            node = prev_sibling;
        }
    }
    println!("{collected:?}");
    println!("bindings {bindings:?}");

    Vec::new()
}

#[test]
fn lsp_complete_fun_ret_type() {
    let contents: &str = "import gleam/io as io 

type Was{
 Was( in_struct: Int )
}

fn main() {
  ErrorOnPurpose
  otherError
}

fn main() {
  let io = fn(a) { Was( in_struct: a) };
  io(1).
}
";

    let tree = new_tree(contents);

    tree.tree.root_node().print(0);

    let res = dot_for_node(&tree, tree_sitter::Point::new(13, 8), LspEnv {});
    assert!(res.is_some());
    //todo should be ["in_struct"]
}

#[test]
fn lsp_complete_as_import() {
    let contents: &str = "import gleam/io as koto

pub fn main() {
  io.println(\"Hello from t1!\");
  koto.
}

";

    let tree = new_tree(contents);

    tree.tree.root_node().print(0);

    let res = dot_for_node(&tree, tree_sitter::Point::new(4, 6), LspEnv {});
    assert!(res.is_some());
}

#[test]
fn lsp_complete_bind() {
    let contents: &str = "import gleam/io.{println}

type Pub {
  Pub(something: Int)
}

pub fn main() {
  let io = Pub(1);
  io.
}
";

    let tree = new_tree(contents);
    let res = dot_for_node(&tree, tree_sitter::Point::new(8, 4), LspEnv {});
    println!("{res:?}");
    assert_eq!(
        res,
        Some(WhatToDisplay::Names(["something".to_string()].to_vec()))
    );
}

#[test]
fn lsp_complete_import() {
    let contents: &str = "import gleam/io.{println}

pub fn main() {
  io.println(\"Hello from t1!\");
  io.
}

";

    let tree = new_tree(contents);
    let res = dot_for_node(&tree, tree_sitter::Point::new(4, 4), LspEnv {});

    assert!(res.is_some());
}

#[derive(Clone, Debug)]
pub struct PartialParsedModule {
    tree: Tree,
    imports: Vec<Import>,
    source_code: String,
}

#[derive(Clone, Debug)]
pub struct Import {
    module: String,
    module_import_name: String,
    qualified_imports: Vec<String>,
    //   as_alias: Option<String>
}

trait Seek {
    fn seek_next(self: &mut Self, kind: &String) -> bool;
}
impl Seek for TreeCursor<'_> {
    fn seek_next(self: &mut Self, kind: &String) -> bool {
        loop {
            if self.node().kind() == kind {
                return true;
            }
            if !(self.goto_next_sibling()) {
                return false;
            }
        }
    }
}

trait Extractor {
    fn print(&self, level: usize);
    fn as_import(&self, source_code: &str) -> Option<Import>;
    fn all_childs_of(&self, kind: &str) -> Vec<tree_sitter::Node<'_>>;
    fn as_string(&self, source_code: &str) -> String;
}

trait Navigator {
    fn as_kind(&self, kind: &str) -> Option<tree_sitter::Node<'_>>;
    fn first_child_of_kind(&self, kind: &str) -> Option<tree_sitter::Node<'_>>;
    fn next_sibling(&self) -> Option<tree_sitter::Node<'_>>;
}

impl Navigator for tree_sitter::Node<'_> {
    fn next_sibling(&self) -> Option<tree_sitter::Node<'_>> {
        let mut cursor = self.walk();
        if cursor.goto_next_sibling() {
            return Some(cursor.node());
        }
        return None;
    }

    fn first_child_of_kind(&self, kind: &str) -> Option<tree_sitter::Node<'_>> {
        let mut cursor = self.walk();

        if cursor.goto_first_child() && cursor.seek_next(&kind.to_string()) {
            return Some(cursor.node());
        }
        return None;
    }

    fn as_kind(&self, kind: &str) -> Option<tree_sitter::Node<'_>> {
        if self.kind() == kind {
            Some(*self)
        } else {
            None
        }
    }
}

impl Navigator for Option<tree_sitter::Node<'_>> {
    fn next_sibling(&self) -> Option<tree_sitter::Node<'_>> {
        if let Some(node) = self {
            return node.next_sibling();
        }
        return None;
    }

    fn first_child_of_kind(&self, kind: &str) -> Option<tree_sitter::Node<'_>> {
        if let Some(node) = self {
            return node.first_child_of_kind(kind);
        }
        return None;
    }

    fn as_kind(&self, kind: &str) -> Option<tree_sitter::Node<'_>> {
        if let Some(node) = self {
            return node.as_kind(kind);
        }
        return None;
    }
}

impl Extractor for tree_sitter::Node<'_> {
    fn as_string(&self, source_code: &str) -> String {
        self.utf8_text(source_code.as_bytes())
            .expect("should work")
            .to_string()
    }

    fn print(&self, level: usize) {
        print!(
            "{}",
            std::iter::repeat("    ").take(level).collect::<String>()
        );
        println!("{:?}", self);

        for i in 0..self.child_count() {
            let x = self.child(i).expect("should exists");
            x.print(level + 1);
        }
    }

    fn all_childs_of(&self, kind: &str) -> Vec<tree_sitter::Node<'_>> {
        let mut ret = Vec::new();

        for i in 0..self.child_count() {
            let x = self.child(i).expect("should exists");
            if x.kind() == kind {
                ret.push(x);
            }
        }

        ret
    }

    fn as_import(&self, source_code: &str) -> Option<Import> {
        match self.kind() {
            "import" => {
                let mut qa: Vec<String> = Vec::new();

                let module = self.child_by_field_name("module").and_then(|x| {
                    Some(
                        x.utf8_text(source_code.as_bytes())
                            .expect("should work")
                            .to_string(),
                    )
                });

                match self.all_childs_of("unqualified_imports").first() {
                    Some(nx) => {
                        qa = nx
                            .all_childs_of("unqualified_import")
                            .iter()
                            .map(|x| {
                                x.utf8_text(source_code.as_bytes())
                                    .expect("should work")
                                    .to_string()
                            })
                            .collect()
                    }
                    _ => (),
                }

                let mut cursor = self.walk();
                let mut as_alias = None;

                if cursor.goto_first_child()
                    && cursor.seek_next(&"as".to_string())
                    && cursor.goto_next_sibling()
                {
                    let as_node = cursor.node();
                    if as_node.kind() == "identifier" {
                        as_alias = Some(
                            as_node
                                .utf8_text(source_code.as_bytes())
                                .expect("should work")
                                .to_string(),
                        );
                    }
                }

                module.and_then(|m| {
                    let import_name = m.split("/").last().expect("at least one").to_string();
                    let module_import_name = as_alias.as_ref().unwrap_or(&import_name).clone();

                    Some(Import {
                        module: m.to_string(),
                        qualified_imports: qa,
                        module_import_name: module_import_name,
                        //as_alias: as_alias.clone()
                    })
                })
            }

            _ => None,
        }
    }
}

pub fn collect_all_imports(tree: &Tree, source_code: &str) -> Vec<Import> {
    let node = tree.root_node();

    node.all_childs_of("import")
        .iter()
        .map(|x| x.as_import(source_code))
        .filter(|x| x.is_some())
        .map(|x| x.expect("after filter"))
        .collect()
}

pub fn new_tree(source_code: &str) -> PartialParsedModule {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_gleam::language())
        .expect("Error loading gleam language");
    let tree: Tree = parser.parse(source_code, None).unwrap();

    let imports = collect_all_imports(&tree, source_code);
    //println!("{:?}", imports);

    //let mut cursor = Tree::walk(&tree);

    PartialParsedModule {
        tree: tree,
        imports: imports,
        source_code: source_code.to_string(),
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum WhatToDisplay {
    Names(Vec<String>),
}

#[derive(Debug)]
pub struct LspEnv {}

macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( let _ = map.insert($key, $val); )*
         map
    }}
}

impl LspEnv {
    fn get_importable_module(self: Self, module_name: &str) -> Option<ModuleMook> {
        if module_name == "gleam/io" {
            let values = hashmap!["println".to_string() => "1".to_string()];
            Some(ModuleMook { values: values })
        } else {
            None
        }
    }
}
//gleam_core::type_::

#[derive(Debug)]
pub struct ModuleMook {
    values: HashMap<String, String>,
}

pub fn dot_for_node(
    module: &PartialParsedModule,
    point: Point,
    environment: LspEnv,
) -> Option<WhatToDisplay> {
    let prev_point = Point {
        column: point.column - 1,
        ..point
    };

    let mut file: std::fs::File = OpenOptions::new()
        .write(true)
        .create(true)
        .append(true)
        .open("/tmp/lsp.log")
        .unwrap();

    let node = module
        .tree
        .root_node()
        .named_descendant_for_point_range(prev_point, point);

    if node.is_none() {
        writeln!(file, "cursor was none");
        file.flush();
    }

    writeln!(file, "cursor was {:?}", node);
    file.flush();

    let res = node.and_then(|node| match node.kind() {
        "identifier" => {
            let text = node
                .utf8_text(module.source_code.as_bytes())
                .expect("should have text");
            let imported = module
                .imports
                .iter()
                .find(|x| x.module_import_name == *text);
            let module_name = match imported {
                Some(alias) => &alias.module,
                _ => text,
            };

            //println!("identifier {text:?} {module_name:?}",);
            if let Some(module) = environment.get_importable_module(module_name) {
                Some(WhatToDisplay::Names(
                    module.values.keys().map(|x| x.to_string()).collect(),
                ))
            } else {
                None
            }
        }
        _ => None,
    });

    //println!("{res:?}");
    res
}

use gleam_core::ast::UntypedExpr;
use gleam_core::parse::error::ParseError;

type UntypedStatements = Vec<Result<Option<Statement<(), UntypedExpr, (), ()>>, ParseError>>;

#[derive(Debug)]
pub struct PartiallyInferedModule {
    pub tree: Tree,
    pub source_code: String,
    //components
    pub statements: UntypedStatements,
    pub infer_state: Vec<usize>,
    pub ts_nodes: Vec<usize>,
    pub untyped: Vec<Result<Option<(UntypedExpr, u32)>, ParseError>>,

    //indexes
    pub name_to_statement: HashMap<String, usize>,
}

impl PartiallyInferedModule {
    fn new(source_code: String) -> Self {
        let mut statements: UntypedStatements = Vec::new();
        let mut infer_state: Vec<usize> = Vec::new();
        let mut ts_nodes: Vec<usize> = Vec::new();
        let untyped: Vec<Result<Option<(UntypedExpr, u32)>, ParseError>> = Vec::new();

        let mut name_to_statement: HashMap<String, usize> = HashMap::new();

        let tree = new_tree(&source_code);

        let root_node = tree.tree.root_node();

        for i in 0..root_node.child_count() {
            let node = root_node.child(i).unwrap();

            ts_nodes.push(node.id());

            let text = node.utf8_text(&source_code.as_bytes()).unwrap();
            println!("{:#?}", text);
            let lex = gleam_core::parse::lexer::make_tokenizer(text);
            let mut parser = gleam_core::parse::Parser::new(lex);

            let expr = parser.parse_statement();
            //let expr = parser.ensure_no_errors_or_remaining_input(expr);

            statements.push(expr);
            infer_state.push(0);
            //untyped.push();

            let name = get_statement_name(&source_code, node);
            if let Some(name) = name {
                let _ = name_to_statement.insert(name, i);
            }
        }

        //tree.tree.root_node().print(0);

        PartiallyInferedModule {
            tree: tree.tree,
            source_code: source_code,
            statements: statements,
            infer_state: infer_state,
            ts_nodes: ts_nodes,
            untyped: untyped,

            name_to_statement: name_to_statement,
        }
    }
}

pub fn get_statement_name(source_code: &str, node: tree_sitter::Node<'_>) -> Option<String> {
    match node.kind() {
        "function" => get_fn_name(source_code, node),
        "external_function" => get_fn_name(source_code, node),
        "constant" => get_const_name(source_code, node),
        "type" => get_type_name(source_code, node),
        "type_alias" => get_type_name(source_code, node),
        "external_type" => get_type_name(source_code, node),
        "import" => get_import_name(source_code, node),
        "type_definition" => get_type_name(source_code, node),
        _ => None,
    }
}

fn get_fn_name(source_code: &str, node: tree_sitter::Node<'_>) -> Option<String> {
    node.first_child_of_kind("fn")
        .next_sibling()
        .as_kind("identifier")
        .map(|node| node.as_string(source_code))
}

fn get_const_name(source_code: &str, node: tree_sitter::Node<'_>) -> Option<String> {
    node.first_child_of_kind("const")
        .next_sibling()
        .as_kind("identifier")
        .map(|node| node.as_string(source_code))
}
fn get_type_name(source_code: &str, node: tree_sitter::Node<'_>) -> Option<String> {
    let mut cursor = node.walk();

    if cursor.goto_first_child()
        && cursor.seek_next(&"type".to_string())
        && cursor.goto_next_sibling()
    {
        let as_node = cursor.node();
        let _name = as_node.as_string(source_code);
        if as_node.kind() == "type_name" {
            if let Some(n) = as_node.child(0) {
                return Some(n.as_string(source_code));
            }
        }
    }
    return None;
}

fn get_import_name<'a>(source_code: &str, node: tree_sitter::Node<'a>) -> Option<String> {
    let module_as = node
        .first_child_of_kind("as")
        .next_sibling()
        .as_kind("identifier")
        .map(|node| node.as_string(source_code));

    if module_as.is_some() {
        return module_as;
    }

    //default name
    node.first_child_of_kind("import")
        .next_sibling()
        .as_kind("module")
        .map(|node| {
            node.as_string(source_code)
                .split("/")
                .last()
                .expect("should have at least one")
                .to_string()
        })
}

#[test]
fn partial_infer_statement_names() {
    let data: &str = r#"
pub type Headers =
       List(#(String, String))

pub type Cat {
   Cat(name: String, cuteness: Int)
}
 
pub external fn random_float() -> Float = "rand" "uniform"
 
pub fn random_cat() -> Int { 0 }
 
pub external type Queue(a)
 
import unix/cat
// Import with alias
import animal/cat as kitty
 
pub const start_year = 2101
const end_year = 2111
 
"#;
    let pi = PartiallyInferedModule::new(data.to_string());
    assert!(pi.name_to_statement.get("Headers").is_some());
    assert!(pi.name_to_statement.get("Cat").is_some());
    assert!(pi.name_to_statement.get("random_float").is_some());
    assert!(pi.name_to_statement.get("Queue").is_some());
    assert!(pi.name_to_statement.get("Cat").is_some());
    assert!(pi.name_to_statement.get("kitty").is_some());
    assert!(pi.name_to_statement.get("start_year").is_some());
    assert!(pi.name_to_statement.get("end_year").is_some());
    assert!(pi.name_to_statement.get("random_cat").is_some());
}

#[test]
fn partial_infer() {
    let data: &str = r#"
pub type Headers =
       List(#(String, String))

pub type Cat {
   Cat(name: String, cuteness: Int)
}
 
pub external fn random_float() -> Float = "rand" "uniform"
 
pub fn random_cat() -> Int { 0 }
 
pub external type Queue(a)
 
import unix/cat
// Import with alias
import animal/cat as kitty
 
pub const start_year = 2101
const end_year = 2111
 
"#;
    let pi = PartiallyInferedModule::new(data.to_string());
    println!("{:#?}", pi.statements);
    assert!(false);
}

use gleam_core::type_::environment::*;
use gleam_core::type_::Warning;

use gleam_core::uid::UniqueIdGenerator;

pub fn infer_module(
    statements: &UntypedStatements,
    package: &str,
    modules: &im::HashMap<String, gleam_core::type_::Module>,
    warnings: &mut Vec<Warning>,
) {
    let name = ["random_name".to_string()];
    let mut ids: UniqueIdGenerator = UniqueIdGenerator::new();
    let mut environment = Environment::new(ids.clone(), &name, modules, warnings);

    let mut type_names = HashMap::with_capacity(statements.len());
    let mut value_names = HashMap::with_capacity(statements.len());
    let mut hydrators = HashMap::with_capacity(statements.len());

    // Register any modules, types, and values being imported
    // We process imports first so that anything imported can be referenced
    // anywhere in the module.
    for s in statements {
        if let Ok(Some(s)) = s {
            gleam_core::type_::register_import(s, &mut environment);
        }
    }

    // Register types so they can be used in constructors and functions
    // earlier in the module.
    for s in statements {
        if let Ok(Some(s)) = s {
            gleam_core::type_::register_types(
                s,
                &name,
                &mut hydrators,
                &mut type_names,
                &mut environment,
            );
        }
    }

    // Register values so they can be used in functions earlier in the module.
    for s in statements {
        if let Ok(Some(s)) = s {
            gleam_core::type_::register_values(
                s,
                &name,
                &mut hydrators,
                &mut value_names,
                &mut environment,
            );
        }
    }

    // Infer the types of each statement in the module
    // We first infer all the constants so they can be used in functions defined
    // anywhere in the module.
    let mut new_statements = Vec::with_capacity(statements.len());
    let mut consts = vec![];
    let mut not_consts = vec![];
    for statement in statements.clone().into_iter() {
        if let Ok(Some(statement)) = statement {
            match statement {
                Statement::Fn { .. }
                | Statement::TypeAlias { .. }
                | Statement::CustomType { .. }
                | Statement::ExternalFn { .. }
                | Statement::ExternalType { .. }
                | Statement::Import { .. } => not_consts.push(statement),

                Statement::ModuleConstant { .. } => consts.push(statement),
            }
        }
    }

    for statement in consts.into_iter().chain(not_consts) {
        let statement = gleam_core::type_::infer_statement(
            statement.clone(),
            &name,
            &mut hydrators,
            &mut environment,
        );
        new_statements.push(statement);
    }

}
