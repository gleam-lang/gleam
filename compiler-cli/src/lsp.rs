// TODO: remove this
#![allow(clippy::unwrap_used)]
#![allow(clippy::unimplemented)]

use std::collections::HashMap;

use gleam_core::{Error, Result};
use lsp_server::Message;
use lsp_types::{
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidSaveTextDocument},
    request::Formatting,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidSaveTextDocumentParams,
    InitializeParams, OneOf, Position, Range, SaveOptions, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, TextEdit,
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
    LanguageServer::new(initialization_params).run(connection)?;
    io_threads.join().expect("joining_lsp_threads");

    // Shut down gracefully.
    tracing::info!("language_server_stopped");
    Ok(())
}

pub struct LanguageServer {
    _params: InitializeParams,

    /// Files that have been edited in memory
    edited: HashMap<String, String>,
}

impl LanguageServer {
    pub fn new(params: InitializeParams) -> Self {
        Self {
            _params: params,
            edited: HashMap::new(),
        }
    }

    pub fn run(&mut self, connection: lsp_server::Connection) -> Result<()> {
        for msg in &connection.receiver {
            tracing::debug!("{:?}", msg);
            match msg {
                Message::Request(request) => {
                    if connection.handle_shutdown(&request).unwrap() {
                        return Ok(());
                    }
                    let id = request.id.clone();
                    let result = self.handle_request(request);
                    let response = result_to_response(result, id);
                    connection.sender.send(Message::Response(response)).unwrap();
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

    fn handle_notification(&mut self, request: lsp_server::Notification) -> Result<()> {
        match request.method.as_str() {
            "textDocument/didSave" => {
                self.did_save(cast_notification::<DidSaveTextDocument>(request).unwrap())
            }

            "textDocument/didClose" => {
                self.did_close(cast_notification::<DidCloseTextDocument>(request).unwrap())
            }

            "textDocument/didChange" => {
                self.did_change(cast_notification::<DidChangeTextDocument>(request).unwrap())
            }

            _ => Ok(()),
        }
    }

    fn did_save(&mut self, params: DidSaveTextDocumentParams) -> Result<()> {
        // The file is in sync with the file system, discard our cache of the changes
        let _ = self.edited.remove(params.text_document.uri.path());
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
                let text_edit = self.format(cast_request::<Formatting>(request).unwrap())?;
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
                gleam_core::format::pretty(&mut new_text, src)?;
            }

            // Otherwise format the file from disc
            None => {
                let src = crate::fs::read(&path)?;
                gleam_core::format::pretty(&mut new_text, &src)?;
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
) -> lsp_server::Response {
    match result {
        Ok(result) => lsp_server::Response {
            id,
            error: None,
            result: Some(result),
        },

        Err(error) => lsp_server::Response {
            id,
            error: Some(error_to_response_error(error)),
            result: None,
        },
    }
}

fn error_to_response_error(error: Error) -> lsp_server::ResponseError {
    lsp_server::ResponseError {
        code: 0, // We should assign a code to each error.
        message: error.pretty_string(),
        data: None,
    }
}
