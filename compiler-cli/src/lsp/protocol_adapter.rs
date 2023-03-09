use super::{
    convert_response, diagnostic_to_lsp, path_to_uri,
    server::{LanguageServer, Notified},
    LspDisplayable, LspMessage, COMPILING_PROGRESS_TOKEN, CREATE_COMPILING_PROGRESS_TOKEN,
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
    server: LanguageServer,

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
            server: language_server,
        })
    }

    pub fn run(&mut self, connection: lsp_server::Connection) -> Result<()> {
        self.create_compilation_progress_token(&connection);
        self.start_watching_gleam_toml(&connection);

        // Compile the project once so we have all the state and any initial errors
        let notified = self.server.compile_please(&connection);
        self.handle_server_notified(&connection, notified);

        // Enter the message loop, handling each message that comes in from the client
        for message in &connection.receiver {
            match self.handle_message(&connection, message) {
                Next::Continue => (),
                Next::Break => break,
            }
        }

        Ok(())
    }

    fn handle_message(
        &mut self,
        connection: &lsp_server::Connection,
        message: lsp_server::Message,
    ) -> Next {
        match message {
            lsp_server::Message::Request(request)
                if connection.handle_shutdown(&request).expect("LSP shutdown") =>
            {
                Next::Break
            }

            lsp_server::Message::Request(request) => {
                self.handle_request(&connection, request);
                Next::Continue
            }

            lsp_server::Message::Notification(notification) => {
                self.handle_notification(&connection, notification);
                Next::Continue
            }

            lsp_server::Message::Response(_) => Next::Continue,
        }
    }

    fn handle_request(
        &mut self,
        connection: &lsp_server::Connection,
        request: lsp_server::Request,
    ) {
        let id = request.id.clone();
        let (response, diagnostics) = match request.method.as_str() {
            "textDocument/formatting" => {
                let params = cast_request::<Formatting>(request);
                convert_response(id, self.server.format(params))
            }

            "textDocument/hover" => {
                let params = cast_request::<HoverRequest>(request);
                convert_response(id, self.server.hover(params))
            }

            "textDocument/definition" => {
                let params = cast_request::<GotoDefinition>(request);
                convert_response(id, self.server.goto_definition(params))
            }

            "textDocument/completion" => {
                let params = cast_request::<Completion>(request);
                convert_response(id, self.server.completion(params))
            }

            _ => panic!("Unsupported LSP request"),
        };

        // DUPE: send-vector-of-diagnostics
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
        let notified: Notified = match notification.method.as_str() {
            "textDocument/didOpen" => {
                let params = cast_notification::<DidOpenTextDocument>(notification);
                tracing::info!("Document opened: {:?}", params);
                self.server.text_document_did_open(params, connection)
            }

            "textDocument/didSave" => {
                let params = cast_notification::<DidSaveTextDocument>(notification);
                self.server.text_document_did_save(params, connection)
            }

            "textDocument/didClose" => {
                let params = cast_notification::<DidCloseTextDocument>(notification);
                self.server.text_document_did_close(params)
            }

            "textDocument/didChange" => {
                let params = cast_notification::<DidChangeTextDocument>(notification);
                self.server.text_document_did_change(params, connection)
            }

            "workspace/didChangeWatchedFiles" => {
                tracing::info!("gleam_toml_changed_so_recompiling_full_project");
                self.server.create_new_compiler().expect("create");
                self.server.compile_please(connection)
            }

            _ => return (),
        };

        self.handle_server_notified(connection, notified);
    }

    fn handle_server_notified(&mut self, connection: &lsp_server::Connection, notified: Notified) {
        // DUPE: send-vector-of-diagnostics
        for diagnostic in notified.diagnostics {
            self.process_gleam_diagnostic(diagnostic);
        }
        self.publish_stored_diagnostics(&connection);

        if let Some(error) = notified.error {
            // TODO: Present the error to the user if it exists
            tracing::error!("Error: {}", error);
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
}

#[derive(Debug, Clone, Copy)]
enum Next {
    Continue,
    Break,
}

fn cast_request<R>(request: lsp_server::Request) -> R::Params
where
    R: lsp::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    let (_, params) = request.extract(R::METHOD).expect("cast request");
    params
}

fn cast_notification<N>(notification: lsp_server::Notification) -> N::Params
where
    N: lsp::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    notification
        .extract::<N::Params>(N::METHOD)
        .expect("cast notification")
}
