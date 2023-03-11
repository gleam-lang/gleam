use debug_ignore::DebugIgnore;
use lsp_types::{
    NumberOrString, ProgressParams, ProgressParamsValue, WorkDoneProgress, WorkDoneProgressBegin,
    WorkDoneProgressCreateParams, WorkDoneProgressEnd,
};

const COMPILING_PROGRESS_TOKEN: &str = "compiling-gleam";
const CREATE_COMPILING_PROGRESS_TOKEN: &str = "create-compiling-progress-token";

#[derive(Debug)]
pub struct ProgressReporter<'a> {
    connection: DebugIgnore<&'a lsp_server::Connection>,
}

impl<'a> ProgressReporter<'a> {
    pub fn new(connection: &'a lsp_server::Connection) -> Self {
        create_compilation_progress_token(connection);
        Self {
            connection: connection.into(),
        }
    }

    pub fn start(&self) {
        self.send_notification(WorkDoneProgress::Begin(WorkDoneProgressBegin {
            title: "Compiling Gleam".into(),
            cancellable: Some(false),
            message: None,
            percentage: None,
        }));
    }

    fn finish(&self) {
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
