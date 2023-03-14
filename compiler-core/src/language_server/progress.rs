use debug_ignore::DebugIgnore;
use lsp_types::{
    InitializeParams, NumberOrString, ProgressParams, ProgressParamsValue, WorkDoneProgress,
    WorkDoneProgressBegin, WorkDoneProgressCreateParams, WorkDoneProgressEnd,
};

const COMPILING_PROGRESS_TOKEN: &str = "compiling-gleam";
const CREATE_COMPILING_PROGRESS_TOKEN: &str = "create-compiling-progress-token";

// Used to publish progress notifications to the client without waiting for
// the usual request-response loop of the language server.
#[derive(Debug)]
pub struct ProgressReporter<'a> {
    connection: DebugIgnore<&'a lsp_server::Connection>,
}

impl<'a> ProgressReporter<'a> {
    pub fn new(
        connection: &'a lsp_server::Connection,
        // We don't actually need these but we take them anyway to ensure that
        // this object is only created after the server has been initialised.
        // If it was created before then the creation of the progress token
        // would fail.
        _initialise_params: &InitializeParams,
    ) -> Self {
        create_compilation_progress_token(connection);
        Self {
            connection: connection.into(),
        }
    }

    pub fn started(&self) {
        self.send_notification(WorkDoneProgress::Begin(WorkDoneProgressBegin {
            title: "Compiling Gleam".into(),
            cancellable: Some(false),
            message: None,
            percentage: None,
        }));
    }

    pub fn finished(&self) {
        self.send_notification(WorkDoneProgress::End(WorkDoneProgressEnd { message: None }));
    }

    fn send_notification(&self, work_done: WorkDoneProgress) {
        let params = ProgressParams {
            token: NumberOrString::String(COMPILING_PROGRESS_TOKEN.to_string()),
            value: ProgressParamsValue::WorkDone(work_done),
        };
        let notification = lsp_server::Notification {
            method: "$/progress".into(),
            params: serde_json::to_value(&params).expect("ProgressParams json"),
        };
        self.connection
            .sender
            .send(lsp_server::Message::Notification(notification))
            .expect("send_work_done_notification send")
    }
}

fn create_compilation_progress_token(connection: &lsp_server::Connection) {
    let params = WorkDoneProgressCreateParams {
        token: NumberOrString::String(COMPILING_PROGRESS_TOKEN.into()),
    };
    let request = lsp_server::Request {
        id: CREATE_COMPILING_PROGRESS_TOKEN.to_string().into(),
        method: "window/workDoneProgress/create".into(),
        params: serde_json::to_value(&params).expect("WorkDoneProgressCreateParams json"),
    };
    connection
        .sender
        .send(lsp_server::Message::Request(request))
        .expect("WorkDoneProgressCreate");
}
