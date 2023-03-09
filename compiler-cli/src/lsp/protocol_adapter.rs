use super::{
    cast_notification, cast_request, convert_response, diagnostic_to_lsp, path_to_uri,
    server::LanguageServer, LspDisplayable, LspMessage, COMPILING_PROGRESS_TOKEN,
    CREATE_COMPILING_PROGRESS_TOKEN,
};
use gleam_core::{
    config::PackageConfig,
    diagnostic::{Diagnostic, Level},
    Result,
};
use lsp::{notification::DidOpenTextDocument, request::GotoDefinition};
use lsp_types::InitializeParams;
use lsp_types::{
    self as lsp,
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidSaveTextDocument},
    request::{Completion, Formatting, HoverRequest},
    PublishDiagnosticsParams, Url,
};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

/// This class is responsible for handling the language server protocol and
/// delegating the work to the `LanguageServer` itself.
///
/// - Configuring watching of the `gleam.toml` file.
/// - Decoding requests.
/// - Encoding responses.
///
/// TODO: move as much of this into the language server as possible while still
/// keeping the transport and encoding/decoding separate.
/// - Performing the initialisation handshake.
///
/// TODO: move this into the language server, keeping only the transport and
/// encoding/decoding.
/// - Managing and publishing diagnostics.
///
/// TODO: move the transport out into a new class and then move this into
/// `gleam_core`.
///
pub struct LanguageServerProtocolAdapter {
    initialise_params: InitializeParams,
    language_server: LanguageServer,

    // TODO: move to language server
    /// Diagnostics that have been emitted by the compiler but not yet published
    /// to the client
    stored_diagnostics: HashMap<PathBuf, Vec<lsp::Diagnostic>>,

    // TODO: move to language server
    /// Files for which there are active diagnostics
    published_diagnostics: HashSet<Url>,

    // TODO: move to language server
    /// Diagnostics that have been emitted by the compiler but not yet published
    /// to the client. These are likely locationless Gleam diagnostics, as LSP
    /// diagnostics always need a location.
    stored_messages: Vec<LspMessage>,
}

impl LanguageServerProtocolAdapter {
    pub fn new(initialise_params: InitializeParams, config: Option<PackageConfig>) -> Result<Self> {
        let language_server = LanguageServer::new(config)?;
        Ok(Self {
            stored_messages: Vec::new(),
            stored_diagnostics: HashMap::new(),
            published_diagnostics: HashSet::new(),
            initialise_params,
            language_server,
        })
    }

    pub fn run(&mut self, connection: lsp_server::Connection) -> Result<()> {
        self.create_compilation_progress_token(&connection);
        self.start_watching_gleam_toml(&connection);

        // Compile the project once so we have all the state and any initial errors
        let result = self.language_server.compile(&connection);
        self.store_result_diagnostics(result);
        self.publish_stored_diagnostics(&connection);

        // Enter the message loop, handling each message that comes in from the client
        for message in &connection.receiver {
            match message {
                lsp_server::Message::Request(request) => {
                    if connection.handle_shutdown(&request).expect("LSP shutdown") {
                        break;
                    }
                    self.handle_request(&connection, request);
                }

                lsp_server::Message::Notification(notification) => {
                    self.handle_notification(&connection, notification);
                }

                lsp_server::Message::Response(_) => (),
            }
        }

        Ok(())
    }

    fn handle_request(
        &mut self,
        connection: &lsp_server::Connection,
        request: lsp_server::Request,
    ) {
        let id = request.id.clone();
        let (response, diagnostics) = match request.method.as_str() {
            "textDocument/formatting" => {
                let params = cast_request::<Formatting>(request).expect("cast Formatting");
                convert_response(id, self.language_server.format(params))
            }

            "textDocument/hover" => {
                let params = cast_request::<HoverRequest>(request).expect("cast HoverRequest");
                convert_response(id, self.language_server.hover(params))
            }

            "textDocument/definition" => {
                let params = cast_request::<GotoDefinition>(request).expect("cast GotoDefinition");
                convert_response(id, self.language_server.goto_definition(params))
            }

            "textDocument/completion" => {
                let params = cast_request::<Completion>(request).expect("cast Completion");
                convert_response(id, self.language_server.completion(params))
            }

            _ => panic!("Unsupported LSP request"),
        };

        for diagnostic in diagnostics {
            self.process_gleam_diagnostic(diagnostic);
        }
        self.publish_stored_diagnostics(&connection);
        connection
            .sender
            .send(lsp_server::Message::Response(response))
            .expect("channel send LSP response")
    }

    fn handle_notification(
        &mut self,
        connection: &lsp_server::Connection,
        notification: lsp_server::Notification,
    ) {
        // TODO: the diagnostics should be returned by the server methods
        match notification.method.as_str() {
            "textDocument/didOpen" => {
                let params = cast_notification::<DidOpenTextDocument>(notification)
                    .expect("case DidOpenTextDocument");
                tracing::info!("Document opened: {:?}", params);
                let result = self
                    .language_server
                    .text_document_did_open(params, connection);
                self.publish_result_diagnostics(result, connection);
            }

            "textDocument/didSave" => {
                let params = cast_notification::<DidSaveTextDocument>(notification)
                    .expect("cast DidSaveTextDocument");
                let result = self
                    .language_server
                    .text_document_did_save(params, connection);
                self.publish_result_diagnostics(result, connection);
            }

            "textDocument/didClose" => {
                let params = cast_notification::<DidCloseTextDocument>(notification)
                    .expect("cast DidCloseTextDocument");
                let result = self.language_server.text_document_did_close(params);
                self.publish_result_diagnostics(result, connection);
            }

            "textDocument/didChange" => {
                let params = cast_notification::<DidChangeTextDocument>(notification)
                    .expect("cast DidChangeTextDocument");
                let result = self
                    .language_server
                    .text_document_did_change(params, connection);
                self.publish_result_diagnostics(result, connection);
            }

            "workspace/didChangeWatchedFiles" => {
                tracing::info!("gleam_toml_changed_so_recompiling_full_project");
                self.language_server.create_new_compiler().expect("create");
                let result = self.language_server.compile(connection);
                self.store_result_diagnostics(result);
                self.publish_stored_diagnostics(connection);
            }

            _ => (),
        }
    }

    fn push_diagnostic(&mut self, path: PathBuf, diagnostic: lsp::Diagnostic) {
        self.stored_diagnostics
            .entry(path)
            .or_default()
            .push(diagnostic);
    }

    fn process_gleam_diagnostic(&mut self, mut diagnostic: Diagnostic) {
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

    /// Publish all stored diagnostics to the client.
    /// Any previously publish diagnostics are cleared before the new set are
    /// published to the client.
    fn publish_stored_diagnostics(&mut self, connection: &lsp_server::Connection) {
        self.clear_all_diagnostics(connection);

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
    }

    /// Clear all diagnostics that have been previously published to the client
    fn clear_all_diagnostics(&mut self, connection: &lsp_server::Connection) {
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
    }

    fn publish_result_diagnostics<T>(
        &mut self,
        result: Result<T>,
        connection: &lsp_server::Connection,
    ) {
        self.store_result_diagnostics(result);
        self.publish_stored_diagnostics(connection);
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

    fn store_result_diagnostics<T>(&mut self, result: Result<T>) {
        // Store error diagnostics, if there are any
        if let Err(error) = result {
            self.process_gleam_diagnostic(error.to_diagnostic());
        }
    }
}
