// TODO: remove this
#![allow(clippy::unwrap_used)]
#![allow(clippy::unimplemented)]

use std::path::PathBuf;

use gleam_core::Result;
use lsp_server::Message;
use lsp_types::{
    notification::DidSaveTextDocument, request::Formatting, DidSaveTextDocumentParams,
    InitializeParams, OneOf, Position, Range, SaveOptions, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncOptions, TextDocumentSyncSaveOptions, TextEdit,
};

pub fn main() -> Result<()> {
    tracing::info!("language_server_starting");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = lsp_server::Connection::stdio();

    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: None,
                change: None,
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
    LanguageServer::new(connection, initialization_params).run()?;
    io_threads.join().expect("joining_lsp_threads");

    // Shut down gracefully.
    tracing::info!("language_server_stopped");
    Ok(())
}

pub struct LanguageServer {
    connection: lsp_server::Connection,
    _params: InitializeParams,
}

impl LanguageServer {
    pub fn new(connection: lsp_server::Connection, params: InitializeParams) -> Self {
        Self {
            connection,
            _params: params,
        }
    }

    pub fn run(&self) -> Result<()> {
        for msg in &self.connection.receiver {
            tracing::debug!("Got message {:?}", msg);
            match msg {
                Message::Request(request) => {
                    if self.connection.handle_shutdown(&request).unwrap() {
                        return Ok(());
                    }
                    let id = request.id.clone();
                    let response = match self.handle_request(request) {
                        Ok(result) => lsp_server::Response {
                            id,
                            error: None,
                            result: Some(result),
                        },
                        Err(error) => lsp_server::Response {
                            id,
                            error: Some(error),
                            result: None,
                        },
                    };
                    self.connection
                        .sender
                        .send(Message::Response(response))
                        .unwrap();
                }

                Message::Response(_) => {
                    // Nothing to do here...
                }

                Message::Notification(notification) => {
                    self.handle_notification(notification).unwrap();
                }
            }
        }
        Ok(())
    }

    fn handle_notification(&self, request: lsp_server::Notification) -> Result<()> {
        match request.method.as_str() {
            "textDocument/didSave" => {
                did_save(cast_notification::<DidSaveTextDocument>(request).unwrap())?;
                Ok(())
            }

            _ => Ok(()),
        }
    }

    fn handle_request(
        &self,
        request: lsp_server::Request,
    ) -> Result<serde_json::Value, lsp_server::ResponseError> {
        match request.method.as_str() {
            "textDocument/formatting" => {
                let text_edit = format(cast_request::<Formatting>(request).unwrap())?;
                Ok(serde_json::to_value(text_edit).unwrap())
            }
            _ => unimplemented!("Unsupported LSP request"),
        }
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

// TODO: handle unable to read file
// TODO: handle syntax errors
fn format(
    params: lsp_types::DocumentFormattingParams,
) -> Result<Vec<TextEdit>, lsp_server::ResponseError> {
    let path = PathBuf::from(params.text_document.uri.path());
    let src = crate::fs::read(&path).unwrap();
    let mut new_text = String::new();
    gleam_core::format::pretty(&mut new_text, &src).unwrap();

    Ok(vec![text_edit_replace(new_text)])
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

fn did_save(params: DidSaveTextDocumentParams) -> Result<()> {
    tracing::info!("{:?}", params);
    Ok(())
}
