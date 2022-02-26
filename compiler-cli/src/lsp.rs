// TODO: remove this
#![allow(clippy::unwrap_used)]
#![allow(clippy::unimplemented)]
#![allow(dead_code)]

use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

use gleam_core::{
    ast::SrcSpan,
    build::{self, Package, ProjectCompiler},
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
    _params: InitializeParams,

    /// Files that have been edited in memory
    edited: HashMap<String, String>,

    /// Files for which there are active diagnostics
    active_diagnostics: HashSet<Url>,

    /// A compiler for the project that supports repeat compilation of the root
    /// package.
    /// In the event the the project config changes this will need to be
    /// discarded and reloaded to handle any changes to dependencies.
    compiler: LspProjectCompiler<ProjectIO>,
}

impl LanguageServer {
    pub fn new(params: InitializeParams) -> Result<Self> {
        let io = ProjectIO::new();
        let compiler = LspProjectCompiler::load_and_compile_dependencies(io)?;
        Ok(Self {
            _params: params,
            edited: HashMap::new(),
            active_diagnostics: HashSet::new(),
            compiler,
        })
    }

    fn clear_diagnostics(&mut self, connection: &lsp_server::Connection) -> Result<(), Error> {
        for file in self.active_diagnostics.drain() {
            connection
                .sender
                .send(lsp_server::Message::Notification(
                    lsp_server::Notification {
                        method: "textDocument/publishDiagnostics".into(),
                        params: serde_json::to_value(PublishDiagnosticsParams {
                            uri: file,
                            diagnostics: vec![],
                            version: None,
                        })
                        .unwrap(),
                    },
                ))
                .unwrap();
        }
        Ok(())
    }

    pub fn run(&mut self, connection: lsp_server::Connection) -> Result<()> {
        for msg in &connection.receiver {
            tracing::debug!("{:?}", msg);
            match msg {
                lsp_server::Message::Request(request) => {
                    if connection.handle_shutdown(&request).unwrap() {
                        return Ok(());
                    }
                    let id = request.id.clone();
                    let result = self.handle_request(request);
                    if result.is_ok() {
                        self.clear_diagnostics(&connection)?;
                    }
                    let (response, diagnostics) = result_to_response(result, id);
                    if let Some(diagnostics) = diagnostics {
                        self.publish_diagnostics(diagnostics, &connection);
                    }
                    connection
                        .sender
                        .send(lsp_server::Message::Response(response))
                        .unwrap();
                }

                lsp_server::Message::Response(_) => {
                    // Nothing to do here...
                }

                lsp_server::Message::Notification(notification) => {
                    if let Err(error) = self.handle_notification(&connection, notification) {
                        match error_to_diagnostic(&error) {
                            Some(diagnostic) => {
                                self.publish_diagnostics(diagnostic, &connection);
                            }

                            None => return Err(error),
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn publish_diagnostics(
        &mut self,
        diagnostics: PublishDiagnosticsParams,
        connection: &lsp_server::Connection,
    ) {
        let _ = self.active_diagnostics.insert(diagnostics.uri.clone());
        connection
            .sender
            .send(lsp_server::Message::Notification(
                lsp_server::Notification {
                    method: "textDocument/publishDiagnostics".into(),
                    params: serde_json::to_value(diagnostics).unwrap(),
                },
            ))
            .unwrap();
    }

    fn handle_notification(
        &mut self,
        connection: &lsp_server::Connection,
        request: lsp_server::Notification,
    ) -> Result<()> {
        match request.method.as_str() {
            "textDocument/didSave" => self.did_save(
                connection,
                cast_notification::<DidSaveTextDocument>(request).unwrap(),
            ),

            "textDocument/didClose" => {
                self.did_close(cast_notification::<DidCloseTextDocument>(request).unwrap())
            }

            "textDocument/didChange" => {
                self.did_change(cast_notification::<DidChangeTextDocument>(request).unwrap())
            }

            _ => Ok(()),
        }
    }

    // TODO: build project
    fn did_save(
        &mut self,
        connection: &lsp_server::Connection,
        params: DidSaveTextDocumentParams,
    ) -> Result<()> {
        // The file is in sync with the file system, discard our cache of the changes
        let _ = self.edited.remove(params.text_document.uri.path());
        // Recompile the project to detect any errors
        let _ = self.compiler.compile()?;
        // Clear any diagnostics from the previous run
        self.clear_diagnostics(connection)?;
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
            _ => unimplemented!("Unsupported LSP request"),
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
) -> (lsp_server::Response, Option<PublishDiagnosticsParams>) {
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

            Some(diagnostic) => {
                let response = lsp_server::Response {
                    id,
                    error: None,
                    result: Some(serde_json::json!(null)),
                };
                (response, Some(diagnostic))
            }
        },
    }
}

fn error_to_diagnostic(error: &Error) -> Option<PublishDiagnosticsParams> {
    match error {
        Error::Parse {
            path, error, src, ..
        } => {
            let location = error.location;
            let line_numbers = LineNumbers::new(src);
            let (detail, extra) = error.details();
            let mut message = "Parse error: ".to_string();
            message.push_str(detail);
            for extra in extra {
                message.push('\n');
                message.push('\n');
                message.push_str(&extra);
            }
            message.push('\n');
            let diagnostic = Diagnostic {
                range: src_span_to_lsp_range(location, line_numbers),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: None,
                message,
                related_information: None,
                tags: None,
                data: None,
            };
            let path = path.canonicalize().unwrap();
            let mut file: String = "file://".into();
            file.push_str(&path.as_os_str().to_string_lossy());
            let uri = Url::parse(&file).unwrap();
            let diagnostic_params = PublishDiagnosticsParams {
                uri,
                diagnostics: vec![diagnostic],
                version: None,
            };
            Some(diagnostic_params)
        }

        Error::Type { .. }
        | Error::UnknownImport { .. }
        | Error::DuplicateModule { .. }
        | Error::DuplicateSourceFile { .. }
        | Error::SrcImportingTest { .. }
        | Error::ImportCycle { .. }
        | Error::PackageCycle { .. }
        | Error::FileIo { .. }
        | Error::GitInitialization { .. }
        | Error::StandardIo { .. }
        | Error::Format { .. }
        | Error::Hex(_)
        | Error::ExpandTar { .. }
        | Error::AddTar { .. }
        | Error::TarFinish(_)
        | Error::Gzip(_)
        | Error::ShellProgramNotFound { .. }
        | Error::ShellCommand { .. }
        | Error::InvalidProjectName { .. }
        | Error::InvalidVersionFormat { .. }
        | Error::ProjectRootAlreadyExist { .. }
        | Error::UnableToFindProjectRoot { .. }
        | Error::VersionDoesNotMatch { .. }
        | Error::MetadataDecodeError { .. }
        | Error::ForbiddenWarnings { .. }
        | Error::JavaScript { .. }
        | Error::DownloadPackageError { .. }
        | Error::Http(_)
        | Error::DependencyResolutionFailed(_)
        | Error::DuplicateDependency(_)
        | Error::MissingHexPublishFields { .. }
        | Error::UnsupportedBuildTool { .. } => None,
    }
}

fn error_to_response_error(error: Error) -> lsp_server::ResponseError {
    lsp_server::ResponseError {
        code: 1, // We should assign a code to each error.
        message: error.pretty_string(),
        data: None,
    }
}

/// A wrapper around the project compiler which makes it possible to repeatedly
/// recompile the top level package, reusing the information about the already
/// compiled dependency packages.
///
#[derive(Debug)]
pub struct LspProjectCompiler<IO> {
    project_compiler: ProjectCompiler<IO>,
}

impl<IO> LspProjectCompiler<IO>
where
    IO: CommandExecutor + FileSystemIO + Clone,
{
    pub fn load_and_compile_dependencies(io: IO) -> Result<Self> {
        // TODO: different telemetry that doesn't write to stdout
        let telemetry = Box::new(cli::Reporter::new());
        let manifest = crate::dependencies::download(None)?;
        let config = crate::config::root_config()?;

        let options = build::Options {
            mode: build::Mode::Dev,
            target: None,
            perform_codegen: false,
        };
        let mut project_compiler =
            ProjectCompiler::new(config, options, manifest.packages, telemetry, io);

        project_compiler.compile_dependencies()?;
        Ok(Self { project_compiler })
    }

    pub fn compile(&mut self) -> Result<Package, Error> {
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
