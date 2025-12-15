use camino::Utf8PathBuf;
use lsp::{
    notification::{DidChangeWatchedFiles, DidOpenTextDocument},
    request::GotoDefinition,
};
use lsp_types::{
    self as lsp,
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidSaveTextDocument},
    request::{
        CodeActionRequest, Completion, DocumentSymbolRequest, Formatting, GotoTypeDefinition,
        HoverRequest, PrepareRenameRequest, References, Rename, SignatureHelpRequest,
    },
};
use std::time::Duration;

#[derive(Debug)]
pub enum Message {
    Request(lsp_server::RequestId, Request),
    Notification(Notification),
}

#[derive(Debug)]
pub enum Request {
    Format(lsp::DocumentFormattingParams),
    Hover(lsp::HoverParams),
    GoToDefinition(lsp::GotoDefinitionParams),
    GoToTypeDefinition(lsp::GotoDefinitionParams),
    Completion(lsp::CompletionParams),
    CodeAction(lsp::CodeActionParams),
    SignatureHelp(lsp::SignatureHelpParams),
    DocumentSymbol(lsp::DocumentSymbolParams),
    PrepareRename(lsp::TextDocumentPositionParams),
    Rename(lsp::RenameParams),
    FindReferences(lsp::ReferenceParams),
}

impl Request {
    fn extract(request: lsp_server::Request) -> Option<Message> {
        let id = request.id.clone();
        match request.method.as_str() {
            "textDocument/formatting" => {
                let params = cast_request::<Formatting>(request);
                Some(Message::Request(id, Request::Format(params)))
            }
            "textDocument/hover" => {
                let params = cast_request::<HoverRequest>(request);
                Some(Message::Request(id, Request::Hover(params)))
            }
            "textDocument/definition" => {
                let params = cast_request::<GotoDefinition>(request);
                Some(Message::Request(id, Request::GoToDefinition(params)))
            }
            "textDocument/completion" => {
                let params = cast_request::<Completion>(request);
                Some(Message::Request(id, Request::Completion(params)))
            }
            "textDocument/codeAction" => {
                let params = cast_request::<CodeActionRequest>(request);
                Some(Message::Request(id, Request::CodeAction(params)))
            }
            "textDocument/signatureHelp" => {
                let params = cast_request::<SignatureHelpRequest>(request);
                Some(Message::Request(id, Request::SignatureHelp(params)))
            }
            "textDocument/documentSymbol" => {
                let params = cast_request::<DocumentSymbolRequest>(request);
                Some(Message::Request(id, Request::DocumentSymbol(params)))
            }
            "textDocument/rename" => {
                let params = cast_request::<Rename>(request);
                Some(Message::Request(id, Request::Rename(params)))
            }
            "textDocument/prepareRename" => {
                let params = cast_request::<PrepareRenameRequest>(request);
                Some(Message::Request(id, Request::PrepareRename(params)))
            }
            "textDocument/typeDefinition" => {
                let params = cast_request::<GotoTypeDefinition>(request);
                Some(Message::Request(id, Request::GoToTypeDefinition(params)))
            }
            "textDocument/references" => {
                let params = cast_request::<References>(request);
                Some(Message::Request(id, Request::FindReferences(params)))
            }
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Notification {
    /// A Gleam file has been modified in memory, and the new text is provided.
    SourceFileChangedInMemory { path: Utf8PathBuf, text: String },
    /// A Gleam file has been saved or closed in the editor.
    SourceFileMatchesDisc { path: Utf8PathBuf },
    /// gleam.toml has changed.
    ConfigFileChanged { path: Utf8PathBuf },
    /// It's time to compile all open projects.
    CompilePlease,
}

impl Notification {
    fn extract(notification: lsp_server::Notification) -> Option<Message> {
        match notification.method.as_str() {
            "textDocument/didOpen" => {
                let params = cast_notification::<DidOpenTextDocument>(notification);
                let notification = Notification::SourceFileChangedInMemory {
                    path: super::path(&params.text_document.uri),
                    text: params.text_document.text,
                };
                Some(Message::Notification(notification))
            }
            "textDocument/didChange" => {
                let params = cast_notification::<DidChangeTextDocument>(notification);
                let notification = Notification::SourceFileChangedInMemory {
                    path: super::path(&params.text_document.uri),
                    text: params.content_changes.into_iter().next_back()?.text,
                };
                Some(Message::Notification(notification))
            }

            "textDocument/didSave" => {
                let params = cast_notification::<DidSaveTextDocument>(notification);
                let notification = Notification::SourceFileMatchesDisc {
                    path: super::path(&params.text_document.uri),
                };
                Some(Message::Notification(notification))
            }
            "textDocument/didClose" => {
                let params = cast_notification::<DidCloseTextDocument>(notification);
                let notification = Notification::SourceFileMatchesDisc {
                    path: super::path(&params.text_document.uri),
                };
                Some(Message::Notification(notification))
            }

            "workspace/didChangeWatchedFiles" => {
                let params = cast_notification::<DidChangeWatchedFiles>(notification);
                let notification = Notification::ConfigFileChanged {
                    path: super::path(&params.changes.into_iter().next_back()?.uri),
                };
                Some(Message::Notification(notification))
            }
            _ => None,
        }
    }
}

pub enum Next {
    MorePlease,
    Handle(Vec<Message>),
    Stop,
}

/// The message buffer pulls messages from the client until one of the following
/// happens:
/// - A shutdown request is received.
/// - A short pause in messages is detected, indicating the programmer has
///   stopped typing for a moment and would benefit from feedback.
/// - A request type message is received, which requires an immediate response.
///
pub struct MessageBuffer {
    messages: Vec<Message>,
}

impl MessageBuffer {
    pub fn new() -> Self {
        Self {
            messages: Vec::new(),
        }
    }

    pub fn receive(&mut self, conn: &lsp_server::Connection) -> Next {
        let pause = Duration::from_millis(100);

        // If the buffer is empty, wait indefinitely for the first message.
        // If the buffer is not empty, wait for a short time to see if more messages are
        // coming before processing the ones we have.
        let message = if self.messages.is_empty() {
            Some(conn.receiver.recv().expect("Receiving LSP message"))
        } else {
            conn.receiver.recv_timeout(pause).ok()
        };

        // If have have not received a message then it means there is a pause in the
        // messages from the client, implying the programmer has stopped typing. Process
        // the currently enqueued messages.
        let message = match message {
            Some(message) => message,
            None => {
                // A compile please message it added in the instance of this
                // pause of activity so that the client gets feedback on the
                // state of the code as it is now.
                self.push_compile_please_message();
                return Next::Handle(self.take_messages());
            }
        };

        match message {
            lsp_server::Message::Request(r) if self.shutdown(conn, &r) => Next::Stop,
            lsp_server::Message::Request(r) => self.request(r),
            lsp_server::Message::Response(r) => self.response(r),
            lsp_server::Message::Notification(n) => self.notification(n),
        }
    }

    fn request(&mut self, r: lsp_server::Request) -> Next {
        let Some(message) = Request::extract(r) else {
            return Next::MorePlease;
        };

        // Compile the code prior to attempting to process the response, to
        // ensure that the response is based on the latest code.
        self.push_compile_please_message();
        self.messages.push(message);
        Next::Handle(self.take_messages())
    }

    fn notification(&mut self, n: lsp_server::Notification) -> Next {
        // A new notification telling us that an edit has been made, or
        // something along those lines.
        if let Some(message) = Notification::extract(n) {
            self.messages.push(message);
        }
        // Ask for more messages (or a pause), at which point we'll start processing.
        Next::MorePlease
    }

    fn response(&mut self, _: lsp_server::Response) -> Next {
        // We do not use or expect responses from the client currently.
        Next::MorePlease
    }

    /// Add a `CompilePlease` message which will prompt the engine to compile
    /// the projects.
    ///
    fn push_compile_please_message(&mut self) {
        let message = Notification::CompilePlease;
        let value = Message::Notification(message);
        self.messages.push(value);
    }

    fn take_messages(&mut self) -> Vec<Message> {
        std::mem::take(&mut self.messages)
    }

    fn shutdown(
        &mut self,
        connection: &lsp_server::Connection,
        request: &lsp_server::Request,
    ) -> bool {
        connection.handle_shutdown(request).expect("LSP shutdown")
    }
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
