pub(crate) mod format;

use crate::error::Error::LSPError;
use crate::language_server::format::format_doc;

use lsp_types::{
    request::Formatting, InitializeParams, ServerCapabilities,
};

use lsp_server::{Connection, Message, Request, RequestId, Response};

#[allow(unused_variables)]
pub fn command(project_root: String) -> Result<(), crate::error::Error> {
    match run_language_server() {
        Ok(()) => Ok(()),
        Err(e) => Err(LSPError(e.to_string())),
    }
}

fn run_language_server() -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
    tracing::info!("Starting language server");

    let (connection, io_threads) = Connection::stdio();

    let capabilities = serde_json::to_value(&ServerCapabilities::default()).unwrap();
    let init_params_json = connection.initialize(capabilities)?;
    let initialization_params = serde_json::from_value(init_params_json)?;

    lsp_loop(&connection, &initialization_params)?;

    io_threads.join()?;

    tracing::info!("Shutting down language server");

    Ok(())
}

#[allow(unused_variables)]
fn lsp_loop(connection: &Connection, params: &InitializeParams) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
    for message in &connection.receiver {
        match message {
            Message::Request(request) => {
                tracing::info!("Received request: {:?}", request);
                if connection.handle_shutdown(&request)? {
                    return Ok(())
                }

                match cast::<Formatting>(request) {
                    Ok((id, params)) => {
                        let result = format_doc(params.text_document, params.options);
                        let result = serde_json::to_value(&result).unwrap();
                        let response = Response { id, result: Some(result), error: None };
                        connection.sender.send(Message::Response(response))?;
                        continue;
                    }
                    Err(req) => req,
                };
            }
            Message::Response(response) => {
                tracing::info!("Received response: {:?}", response);
            }
            Message::Notification(notification) => {
                tracing::info!("Received notification: {:?}", notification);
            }
        }
    }

    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), Request>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}