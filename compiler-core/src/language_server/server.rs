use crate::{
    diagnostic::{Diagnostic, Level},
    io::{CommandExecutor, FileSystemReader, FileSystemWriter},
    language_server::{
        engine::{self, LanguageServerEngine},
        feedback::{Feedback, FeedbackBookKeeper},
        files::FileSystemProxy,
        router::Router,
        src_span_to_lsp_range, DownloadDependencies, MakeLocker,
    },
    line_numbers::LineNumbers,
    Result,
};
use debug_ignore::DebugIgnore;
use lsp::{
    notification::{DidChangeWatchedFiles, DidOpenTextDocument},
    request::GotoDefinition,
    HoverProviderCapability, Position, Range, TextEdit, Url,
};
use lsp_types::{
    self as lsp,
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidSaveTextDocument},
    request::{CodeActionRequest, Completion, Formatting, HoverRequest},
    InitializeParams, PublishDiagnosticsParams,
};
use serde_json::Value as Json;
use std::collections::HashMap;

use camino::Utf8PathBuf;

use super::progress::ConnectionProgressReporter;

/// This class is responsible for handling the language server protocol and
/// delegating the work to the engine.
///
/// - Configuring watching of the `gleam.toml` file.
/// - Decoding requests.
/// - Encoding responses.
/// - Sending diagnostics and messages to the client.
/// - Tracking the state of diagnostics and messages.
/// - Performing the initialisation handshake.
///
#[derive(Debug)]
pub struct LanguageServer<'a, IO> {
    initialise_params: InitializeParams,
    connection: DebugIgnore<&'a lsp_server::Connection>,
    outside_of_project_feedback: FeedbackBookKeeper,
    router: Router<IO, ConnectionProgressReporter<'a>>,
    io: FileSystemProxy<IO>,
}

impl<'a, IO> LanguageServer<'a, IO>
where
    IO: FileSystemReader
        + FileSystemWriter
        + CommandExecutor
        + DownloadDependencies
        + MakeLocker
        + Clone,
{
    pub fn new(connection: &'a lsp_server::Connection, io: IO) -> Result<Self> {
        let initialise_params = initialisation_handshake(connection);
        let reporter = ConnectionProgressReporter::new(connection, &initialise_params);
        let io = FileSystemProxy::new(io);
        let router = Router::new(reporter, io.clone());
        Ok(Self {
            connection: connection.into(),
            initialise_params,
            outside_of_project_feedback: FeedbackBookKeeper::default(),
            router,
            io,
        })
    }

    pub fn run(&mut self) -> Result<()> {
        self.start_watching_gleam_toml();

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
                self.format(params)
            }

            "textDocument/hover" => {
                let params = cast_request::<HoverRequest>(request);
                self.hover(params)
            }

            "textDocument/definition" => {
                let params = cast_request::<GotoDefinition>(request);
                self.goto_definition(params)
            }

            "textDocument/completion" => {
                let params = cast_request::<Completion>(request);
                self.completion(params)
            }

            "textDocument/codeAction" => {
                let params = cast_request::<CodeActionRequest>(request);
                self.code_action(params)
            }

            name => panic!("Unsupported LSP request {}", name),
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
                self.text_document_did_open(params)
            }

            "textDocument/didSave" => {
                let params = cast_notification::<DidSaveTextDocument>(notification);
                self.text_document_did_save(params)
            }

            "textDocument/didClose" => {
                let params = cast_notification::<DidCloseTextDocument>(notification);
                self.text_document_did_close(params)
            }

            "textDocument/didChange" => {
                let params = cast_notification::<DidChangeTextDocument>(notification);
                self.text_document_did_change(params)
            }

            "workspace/didChangeWatchedFiles" => {
                let params = cast_notification::<DidChangeWatchedFiles>(notification);
                self.watched_files_changed(params)
            }

            _ => return,
        };

        self.publish_feedback(feedback);
    }

    fn publish_feedback(&self, feedback: Feedback) {
        self.publish_diagnostics(feedback.diagnostics);
        self.publish_messages(feedback.messages);
    }

    fn publish_diagnostics(&self, diagnostics: HashMap<Utf8PathBuf, Vec<Diagnostic>>) {
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
                        glob_pattern: "**/gleam.toml".into(),
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

    fn publish_messages(&self, messages: Vec<Diagnostic>) {
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

    fn respond_with_engine<T, Handler>(
        &mut self,
        path: Utf8PathBuf,
        handler: Handler,
    ) -> (Json, Feedback)
    where
        T: serde::Serialize,
        Handler: FnOnce(
            &mut LanguageServerEngine<IO, ConnectionProgressReporter<'a>>,
        ) -> engine::Response<T>,
    {
        match self.router.project_for_path(&path) {
            Ok(Some(project)) => {
                let engine::Response {
                    result,
                    warnings,
                    compilation,
                } = handler(&mut project.engine);
                match result {
                    Ok(value) => {
                        let feedback = project.feedback.response(compilation, warnings);
                        let json = serde_json::to_value(value).expect("response to json");
                        (json, feedback)
                    }
                    Err(e) => {
                        let feedback = project.feedback.build_with_error(e, compilation, warnings);
                        (Json::Null, feedback)
                    }
                }
            }

            Ok(None) => (Json::Null, Feedback::default()),

            Err(error) => (Json::Null, self.outside_of_project_feedback.error(error)),
        }
    }

    fn notified_with_engine(
        &mut self,
        path: Utf8PathBuf,
        handler: impl FnOnce(
            &mut LanguageServerEngine<IO, ConnectionProgressReporter<'a>>,
        ) -> engine::Response<()>,
    ) -> Feedback {
        self.respond_with_engine(path, handler).1
    }

    fn format(&mut self, params: lsp::DocumentFormattingParams) -> (Json, Feedback) {
        let path = path(&params.text_document.uri);
        let mut new_text = String::new();
        let mut error_response = |error| {
            let feedback = match self.router.project_for_path(&path) {
                Ok(Some(project)) => project.feedback.error(error),
                Ok(None) | Err(_) => self.outside_of_project_feedback.error(error),
            };
            (Json::Null, feedback)
        };

        let src = match self.io.read(&path) {
            Ok(src) => src.into(),
            Err(error) => return error_response(error),
        };

        if let Err(error) = crate::format::pretty(&mut new_text, &src, &path) {
            return error_response(error);
        }

        let line_count = src.lines().count() as u32;

        let edit = TextEdit {
            range: Range::new(Position::new(0, 0), Position::new(line_count, 0)),
            new_text,
        };
        let json = serde_json::to_value(vec![edit]).expect("to JSON value");

        (json, Feedback::default())
    }

    fn hover(&mut self, params: lsp::HoverParams) -> (Json, Feedback) {
        let path = path(&params.text_document_position_params.text_document.uri);
        self.respond_with_engine(path, |engine| engine.hover(params))
    }

    fn goto_definition(&mut self, params: lsp::GotoDefinitionParams) -> (Json, Feedback) {
        let path = path(&params.text_document_position_params.text_document.uri);
        self.respond_with_engine(path, |engine| engine.goto_definition(params))
    }

    fn completion(&mut self, params: lsp::CompletionParams) -> (Json, Feedback) {
        let path = path(&params.text_document_position.text_document.uri);
        self.respond_with_engine(path, |engine| {
            engine.completion(params.text_document_position)
        })
    }

    fn code_action(&mut self, params: lsp::CodeActionParams) -> (Json, Feedback) {
        eprintln!("Got code action {:?}", params);
        todo!()
    }

    /// A file opened in the editor may be unsaved, so store a copy of the
    /// new content in memory and compile.
    fn text_document_did_open(&mut self, params: lsp::DidOpenTextDocumentParams) -> Feedback {
        let path = path(&params.text_document.uri);
        if let Err(error) = self.io.write_mem_cache(&path, &params.text_document.text) {
            return self.outside_of_project_feedback.error(error);
        }

        self.notified_with_engine(path, |engine| engine.compile_please())
    }

    fn text_document_did_save(&mut self, params: lsp::DidSaveTextDocumentParams) -> Feedback {
        let path = path(&params.text_document.uri);

        // The file is in sync with the file system, discard our cache of the changes
        if let Err(error) = self.io.delete_mem_cache(&path) {
            return self.outside_of_project_feedback.error(error);
        }

        // The files on disc have changed, so compile the project with the new changes
        self.notified_with_engine(path, |engine| engine.compile_please())
    }

    fn text_document_did_close(&mut self, params: lsp::DidCloseTextDocumentParams) -> Feedback {
        let path = path(&params.text_document.uri);

        // The file is in sync with the file system, discard our cache of the changes
        if let Err(error) = self.io.delete_mem_cache(&path) {
            return self.outside_of_project_feedback.error(error);
        }

        Feedback::default()
    }

    /// A file has changed in the editor, so store a copy of the new content in
    /// memory and compile.
    fn text_document_did_change(&mut self, params: lsp::DidChangeTextDocumentParams) -> Feedback {
        let path = path(&params.text_document.uri);

        let changes = match params.content_changes.into_iter().last() {
            Some(changes) => changes,
            None => return Feedback::default(),
        };

        if let Err(error) = self.io.write_mem_cache(&path, changes.text.as_str()) {
            return self.outside_of_project_feedback.error(error);
        }

        // The files on disc have changed, so compile the project with the new changes
        self.notified_with_engine(path, |engine| engine.compile_please())
    }

    fn watched_files_changed(&mut self, params: lsp::DidChangeWatchedFilesParams) -> Feedback {
        let changes = match params.changes.into_iter().last() {
            Some(changes) => changes,
            None => return Feedback::default(),
        };

        let path = path(&changes.uri);
        self.router.delete_engine_for_path(&path);
        self.notified_with_engine(path, LanguageServerEngine::compile_please)
    }
}

fn initialisation_handshake(connection: &lsp_server::Connection) -> InitializeParams {
    let server_capabilities = lsp::ServerCapabilities {
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
            trigger_characters: None,
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
        code_action_provider: Some(lsp::CodeActionProviderCapability::Simple(true)),
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
    };
    let server_capabilities_json =
        serde_json::to_value(server_capabilities).expect("server_capabilities_serde");
    let initialise_params_json = connection
        .initialize(server_capabilities_json)
        .expect("LSP initialize");
    let initialise_params: InitializeParams =
        serde_json::from_value(initialise_params_json).expect("LSP InitializeParams from json");
    initialise_params
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

fn path_to_uri(path: Utf8PathBuf) -> Url {
    let mut file: String = "file://".into();
    file.push_str(&path.as_os_str().to_string_lossy());
    Url::parse(&file).expect("path_to_uri URL parse")
}

fn path(uri: &Url) -> Utf8PathBuf {
    // The to_file_path method is available on these platforms
    #[cfg(any(unix, windows, target_os = "redox", target_os = "wasi"))]
    return Utf8PathBuf::from_path_buf(uri.to_file_path().expect("URL file"))
        .expect("Non Utf8 Path");

    #[cfg(not(any(unix, windows, target_os = "redox", target_os = "wasi")))]
    return Utf8PathBuf::from_path_buf(uri.path().into()).expect("Non Utf8 Path");
}
