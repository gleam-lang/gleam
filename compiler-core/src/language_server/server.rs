use super::{
    messages::{Message, MessageBuffer, Next, Notification, Request},
    progress::ConnectionProgressReporter,
};
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
use camino::{Utf8Path, Utf8PathBuf};
use debug_ignore::DebugIgnore;
use itertools::Itertools;
use lsp_types::{
    self as lsp, HoverProviderCapability, InitializeParams, Position, PublishDiagnosticsParams,
    Range, TextEdit, Url,
};
use serde_json::Value as Json;
use std::collections::{HashMap, HashSet};

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
    changed_projects: HashSet<Utf8PathBuf>,
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
            changed_projects: HashSet::new(),
            outside_of_project_feedback: FeedbackBookKeeper::default(),
            router,
            io,
        })
    }

    pub fn run(&mut self) -> Result<()> {
        self.start_watching_gleam_toml();
        let mut buffer = MessageBuffer::new();

        loop {
            match buffer.receive(*self.connection) {
                Next::Stop => break,
                Next::MorePlease => (),
                Next::Handle(messages) => {
                    for message in messages {
                        self.handle_message(message);
                    }
                }
            }
        }

        Ok(())
    }

    fn handle_message(&mut self, message: Message) {
        match message {
            Message::Request(id, request) => self.handle_request(id, request),
            Message::Notification(notification) => self.handle_notification(notification),
        }
    }

    fn handle_request(&mut self, id: lsp_server::RequestId, request: Request) {
        let (payload, feedback) = match request {
            Request::Format(param) => self.format(param),
            Request::Hover(param) => self.hover(param),
            Request::GoToDefinition(param) => self.goto_definition(param),
            Request::Completion(param) => self.completion(param),
            Request::CodeAction(param) => self.code_action(param),
            Request::SignatureHelp(param) => self.signature_help(param),
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

    fn handle_notification(&mut self, notification: Notification) {
        let feedback = match notification {
            Notification::CompilePlease => self.compile_please(),
            Notification::SourceFileMatchesDisc { path } => self.discard_in_memory_cache(path),
            Notification::SourceFileChangedInMemory { path, text } => {
                self.cache_file_in_memory(path, text)
            }
            Notification::ConfigFileChanged { path } => self.watched_files_changed(path),
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
            .map(|wf| wf.dynamic_registration.unwrap_or(false))
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
                        glob_pattern: "**/gleam.toml".to_string().into(),
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
        match self.router.project_for_path(path) {
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

    fn path_error_response(&mut self, path: Utf8PathBuf, error: crate::Error) -> (Json, Feedback) {
        let feedback = match self.router.project_for_path(path) {
            Ok(Some(project)) => project.feedback.error(error),
            Ok(None) | Err(_) => self.outside_of_project_feedback.error(error),
        };
        (Json::Null, feedback)
    }

    fn format(&mut self, params: lsp::DocumentFormattingParams) -> (Json, Feedback) {
        let path = super::path(&params.text_document.uri);
        let mut new_text = String::new();

        let src = match self.io.read(&path) {
            Ok(src) => src.into(),
            Err(error) => return self.path_error_response(path, error),
        };

        if let Err(error) = crate::format::pretty(&mut new_text, &src, &path) {
            return self.path_error_response(path, error);
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
        let path = super::path(&params.text_document_position_params.text_document.uri);
        self.respond_with_engine(path, |engine| engine.hover(params))
    }

    fn goto_definition(&mut self, params: lsp::GotoDefinitionParams) -> (Json, Feedback) {
        let path = super::path(&params.text_document_position_params.text_document.uri);
        self.respond_with_engine(path, |engine| engine.goto_definition(params))
    }

    fn completion(&mut self, params: lsp::CompletionParams) -> (Json, Feedback) {
        let path = super::path(&params.text_document_position.text_document.uri);

        let src = match self.io.read(&path) {
            Ok(src) => src.into(),
            Err(error) => return self.path_error_response(path, error),
        };
        self.respond_with_engine(path, |engine| {
            engine.completion(params.text_document_position, src)
        })
    }

    fn signature_help(&mut self, params: lsp_types::SignatureHelpParams) -> (Json, Feedback) {
        let path = super::path(&params.text_document_position_params.text_document.uri);
        self.respond_with_engine(path, |engine| engine.signature_help(params))
    }

    fn code_action(&mut self, params: lsp::CodeActionParams) -> (Json, Feedback) {
        let path = super::path(&params.text_document.uri);
        self.respond_with_engine(path, |engine| engine.code_actions(params))
    }

    fn cache_file_in_memory(&mut self, path: Utf8PathBuf, text: String) -> Feedback {
        self.project_changed(&path);
        if let Err(error) = self.io.write_mem_cache(&path, &text) {
            return self.outside_of_project_feedback.error(error);
        }
        Feedback::none()
    }

    fn discard_in_memory_cache(&mut self, path: Utf8PathBuf) -> Feedback {
        self.project_changed(&path);
        if let Err(error) = self.io.delete_mem_cache(&path) {
            return self.outside_of_project_feedback.error(error);
        }
        Feedback::none()
    }

    fn watched_files_changed(&mut self, path: Utf8PathBuf) -> Feedback {
        self.router.delete_engine_for_path(&path);
        Feedback::none()
    }

    fn compile_please(&mut self) -> Feedback {
        let mut accumulator = Feedback::none();
        let projects = std::mem::take(&mut self.changed_projects);
        for path in projects {
            let (_, feedback) = self.respond_with_engine(path, |e| e.compile_please());
            accumulator.append_feedback(feedback);
        }
        accumulator
    }

    fn project_changed(&mut self, path: &Utf8Path) {
        let project_path = self.router.project_path(path);
        if let Some(project_path) = project_path {
            _ = self.changed_projects.insert(project_path);
        }
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
            trigger_characters: Some(vec![".".into()]),
            all_commit_characters: None,
            work_done_progress_options: lsp::WorkDoneProgressOptions {
                work_done_progress: None,
            },
            completion_item: None,
        }),
        signature_help_provider: Some(lsp::SignatureHelpOptions {
            trigger_characters: Some(vec!["(".into(), ",".into(), ":".into()]),
            retrigger_characters: None,
            work_done_progress_options: lsp::WorkDoneProgressOptions {
                work_done_progress: None,
            },
        }),
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
        position_encoding: None,
        inline_value_provider: None,
        inlay_hint_provider: None,
        diagnostic_provider: None,
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
    let path = path_to_uri(location.path);
    let range = src_span_to_lsp_range(location.label.span, &line_numbers);

    let related_info = location
        .extra_labels
        .iter()
        .map(|extra| {
            let message = extra.label.text.clone().unwrap_or_default();
            let location = if let Some((src, path)) = &extra.src_info {
                let line_numbers = LineNumbers::new(src);
                lsp::Location {
                    uri: path_to_uri(path.clone()),
                    range: src_span_to_lsp_range(extra.label.span, &line_numbers),
                }
            } else {
                lsp::Location {
                    uri: path.clone(),
                    range: src_span_to_lsp_range(extra.label.span, &line_numbers),
                }
            };
            lsp::DiagnosticRelatedInformation { location, message }
        })
        .collect_vec();

    let main = lsp::Diagnostic {
        range,
        severity: Some(severity),
        code: None,
        code_description: None,
        source: None,
        message: text,
        related_information: if related_info.is_empty() {
            None
        } else {
            Some(related_info)
        },
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
