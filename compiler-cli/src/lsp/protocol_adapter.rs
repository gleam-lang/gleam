use super::{
    convert_response, diagnostic_to_lsp, feedback::Feedback, path_to_uri, server::LanguageServer,
    COMPILING_PROGRESS_TOKEN, CREATE_COMPILING_PROGRESS_TOKEN,
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
    PublishDiagnosticsParams,
};
use std::{collections::HashMap, path::PathBuf};

/// This class is responsible for handling the language server protocol and
/// delegating the work to the `LanguageServer` itself.
///
/// - Configuring watching of the `gleam.toml` file.
/// - Decoding requests.
/// - Encoding responses.
/// - Sending diagnostics and messages to the client.
///
/// TODO: move as much of this into the language server as possible while still
/// keeping the transport and encoding/decoding separate.
/// - Performing the initialisation handshake.
///
/// TODO: move the transport out into a new class and then move this into
/// `gleam_core`.
///
pub struct LanguageServerProtocolAdapter<'a> {
    initialise_params: InitializeParams,
    connection: &'a lsp_server::Connection,
    server: LanguageServer,
}

impl<'a> LanguageServerProtocolAdapter<'a> {
    pub fn new(
        initialise_params: InitializeParams,
        connection: &'a lsp_server::Connection,
        config: Option<PackageConfig>,
    ) -> Result<Self> {
        let language_server = LanguageServer::new(config)?;
        Ok(Self {
            connection,
            initialise_params,
            server: language_server,
        })
    }

    pub fn run(&mut self) -> Result<()> {
        self.create_compilation_progress_token();
        self.start_watching_gleam_toml();

        // Compile the project once so we have all the state and any initial errors
        let feedback = self.server.compile_please(self.connection);
        self.publish_feedback(feedback);

        // Enter the message loop, handling each message that comes in from the client
        for message in &self.connection.receiver {
            match self.handle_message(message) {
                Next::Continue => (),
                Next::Break => break,
            }
        }

        Ok(())
    }

    fn handle_message(&mut self, message: lsp_server::Message) -> Next {
        match message {
            lsp_server::Message::Request(request) if self.handle_shutdown(&request) => Next::Break,

            lsp_server::Message::Request(request) => {
                self.handle_request(request);
                Next::Continue
            }

            lsp_server::Message::Notification(notification) => {
                self.handle_notification(notification);
                Next::Continue
            }

            lsp_server::Message::Response(_) => Next::Continue,
        }
    }

    fn handle_shutdown(&mut self, request: &lsp_server::Request) -> bool {
        self.connection
            .handle_shutdown(request)
            .expect("LSP shutdown")
    }

    fn handle_request(&mut self, request: lsp_server::Request) {
        let id = request.id.clone();
        let (payload, feedback) = match request.method.as_str() {
            "textDocument/formatting" => {
                let params = cast_request::<Formatting>(request);
                convert_response(self.server.format(params))
            }

            "textDocument/hover" => {
                let params = cast_request::<HoverRequest>(request);
                convert_response(self.server.hover(params))
            }

            "textDocument/definition" => {
                let params = cast_request::<GotoDefinition>(request);
                convert_response(self.server.goto_definition(params))
            }

            "textDocument/completion" => {
                let params = cast_request::<Completion>(request);
                convert_response(self.server.completion(params))
            }

            _ => panic!("Unsupported LSP request"),
        };

        self.publish_feedback(feedback);

        let response = lsp_server::Response {
            id,
            error: None,
            result: Some(payload),
        };
        self.connection
            .sender
            .send(lsp_server::Message::Response(response))
            .expect("channel send LSP response")
    }

    fn handle_notification(&mut self, notification: lsp_server::Notification) {
        let feedback = match notification.method.as_str() {
            "textDocument/didOpen" => {
                let params = cast_notification::<DidOpenTextDocument>(notification);
                tracing::info!("Document opened: {:?}", params);
                self.server.text_document_did_open(params, self.connection)
            }

            "textDocument/didSave" => {
                let params = cast_notification::<DidSaveTextDocument>(notification);
                self.server.text_document_did_save(params, self.connection)
            }

            "textDocument/didClose" => {
                let params = cast_notification::<DidCloseTextDocument>(notification);
                self.server.text_document_did_close(params)
            }

            "textDocument/didChange" => {
                let params = cast_notification::<DidChangeTextDocument>(notification);
                self.server
                    .text_document_did_change(params, self.connection)
            }

            "workspace/didChangeWatchedFiles" => {
                tracing::info!("gleam_toml_changed_so_recompiling_full_project");
                self.server.create_new_compiler().expect("create");
                self.server.compile_please(self.connection)
            }

            _ => return,
        };

        self.publish_feedback(feedback);
    }

    fn publish_feedback(&self, feedback: Feedback) {
        self.publish_diagnostics(feedback.diagnostics);
        self.publish_notifications(feedback.messages);
    }

    fn publish_diagnostics(&self, diagnostics: HashMap<PathBuf, Vec<Diagnostic>>) {
        for (path, diagnostics) in diagnostics {
            let diagnostics = diagnostics
                .into_iter()
                .flat_map(diagnostic_to_lsp)
                .collect::<Vec<_>>();
            let uri = path_to_uri(path);

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
            self.connection
                .sender
                .send(lsp_server::Message::Notification(notification))
                .expect("send textDocument/publishDiagnostics");
        }
    }

    fn start_watching_gleam_toml(&mut self) {
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
        self.connection
            .sender
            .send(lsp_server::Message::Request(request))
            .expect("send client/registerCapability");
    }

    fn create_compilation_progress_token(&mut self) {
        let params = lsp::WorkDoneProgressCreateParams {
            token: lsp::NumberOrString::String(COMPILING_PROGRESS_TOKEN.into()),
        };
        let request = lsp_server::Request {
            id: CREATE_COMPILING_PROGRESS_TOKEN.to_string().into(),
            method: "window/workDoneProgress/create".into(),
            params: serde_json::to_value(&params).expect("WorkDoneProgressCreateParams json"),
        };
        self.connection
            .sender
            .send(lsp_server::Message::Request(request))
            .expect("WorkDoneProgressCreate");
    }

    fn publish_notifications(&self, messages: Vec<Diagnostic>) {
        for message in messages {
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
            self.connection
                .sender
                .send(lsp_server::Message::Notification(notification))
                .expect("send window/showMessage");
        }
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
