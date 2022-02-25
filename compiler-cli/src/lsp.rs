// TODO: remove this
#![allow(clippy::unwrap_used)]

use gleam_core::Result;
use lsp_server::Message;
use lsp_types::{InitializeParams, ServerCapabilities};

pub fn main() -> Result<()> {
    tracing::info!("language_server_starting");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = lsp_server::Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = ServerCapabilities::default();
    let server_capabilities_json =
        serde_json::to_value(&server_capabilities).expect("server_capabilities_serde");

    let initialization_params = connection
        .initialize(server_capabilities_json)
        // TODO: handle protocol error
        .unwrap();

    main_loop(connection, initialization_params)?;
    io_threads.join().expect("joining_lsp_threads");

    // Shut down gracefully.
    tracing::info!("language_server_stopped");
    Ok(())
}

fn main_loop(connection: lsp_server::Connection, params: serde_json::Value) -> Result<()> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    for msg in &connection.receiver {
        tracing::info!("in {:?}", msg);
        match msg {
            Message::Request(req) => {
                if connection
                    .handle_shutdown(&req)
                    // TODO: handle protocol error
                    .unwrap()
                {
                    return Ok(());
                }
                eprintln!("got request: {:?}", req);
                // match cast::<GotoDefinition>(req) {
                //     Ok((id, params)) => {
                //         eprintln!("got gotoDefinition request #{}: {:?}", id, params);
                //         let result = Some(GotoDefinitionResponse::Array(Vec::new()));
                //         let result = serde_json::to_value(&result).unwrap();
                //         let resp = Response {
                //             id,
                //             result: Some(result),
                //             error: None,
                //         };
                //         connection.sender.send(Message::Response(resp))?;
                //         continue;
                //     }
                //     Err(req) => req,
                // };
                // // ...
            }
            Message::Response(resp) => {
                eprintln!("got response: {:?}", resp);
            }
            Message::Notification(not) => {
                eprintln!("got notification: {:?}", not);
            }
        }
    }
    Ok(())
}
