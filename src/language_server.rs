pub(crate) mod format;

use crate::language_server::format::format;

use tokio::runtime::Runtime;

use tower_lsp::jsonrpc::{ Error, ErrorCode, Result };
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct ServerBackend {
    client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for ServerBackend {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        let mut result = InitializeResult::default();
        result.capabilities.document_formatting_provider = Some(true);
        Ok(result)
    }
    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let doc_uri = params.text_document.uri;

        if(doc_uri.scheme() != "file") {
            return Err(Error::new(ErrorCode::InvalidParams));
        }

        let doc_contents = match std::fs::read_to_string(doc_uri.path()) {
            Ok(contents) => contents,
            Err(io_error) => return Err(Error { code: ErrorCode::InternalError, message: io_error.to_string(), data: None }),
        };

        match format(doc_contents) {
            Ok(x) => Ok(Some(x)),
            Err(s) => Err(Error { code: ErrorCode::ParseError, message: s, data: None }),
        }
    }
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

pub fn command() -> std::result::Result<(), crate::error::Error> {

    let mut rt = Runtime::new().unwrap();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, messages) = LspService::new(|client| ServerBackend { client });

    rt.block_on(async {
        Server::new(stdin, stdout)
                .interleave(messages)
                .serve(service)
                .await;
    });

    Ok(())
}