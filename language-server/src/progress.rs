use debug_ignore::DebugIgnore;
use lsp_types::{
    InitializeParams, NumberOrString, ProgressParams, ProgressParamsValue, WorkDoneProgress,
    WorkDoneProgressBegin, WorkDoneProgressCreateParams, WorkDoneProgressEnd,
};

const DOWNLOADING_TOKEN: &str = "downloading-dependencies";

pub trait ProgressReporter {
    fn compilation_started(&self);
    fn compilation_finished(&self);
    fn dependency_downloading_started(&self);
    fn dependency_downloading_finished(&self);
}

// Used to publish progress notifications to the client without waiting for
// the usual request-response loop of the language server.
#[derive(Debug, Clone)]
pub struct ConnectionProgressReporter<'a> {
    connection: DebugIgnore<&'a lsp_server::Connection>,
}

impl<'a> ConnectionProgressReporter<'a> {
    pub fn new(
        connection: &'a lsp_server::Connection,
        // We don't actually need these but we take them anyway to ensure that
        // this object is only created after the server has been initialised.
        // If it was created before then the creation of the progress token
        // would fail.
        _initialise_params: &InitializeParams,
    ) -> Self {
        create_token(DOWNLOADING_TOKEN, connection);
        Self {
            connection: connection.into(),
        }
    }

    fn send_notification(&self, token: &str, work_done: WorkDoneProgress) {
        let params = ProgressParams {
            token: NumberOrString::String(token.to_string()),
            value: ProgressParamsValue::WorkDone(work_done),
        };
        let notification = lsp_server::Notification {
            method: "$/progress".into(),
            params: serde_json::to_value(params).expect("ProgressParams json"),
        };
        self.connection
            .sender
            .send(lsp_server::Message::Notification(notification))
            .expect("send_work_done_notification send")
    }
}

impl ProgressReporter for ConnectionProgressReporter<'_> {
    fn compilation_started(&self) {
        // Do nothing. This is only used for tests currently.
        // In future we could make this emit a message to the client if compilation is taking a
        // long time.
    }

    fn compilation_finished(&self) {
        // Do nothing. This is only used for tests currently.
    }

    fn dependency_downloading_started(&self) {
        let title = "Downloading Gleam dependencies";
        self.send_notification(DOWNLOADING_TOKEN, begin_message(title));
    }

    fn dependency_downloading_finished(&self) {
        self.send_notification(DOWNLOADING_TOKEN, end_message());
    }
}

fn end_message() -> WorkDoneProgress {
    WorkDoneProgress::End(WorkDoneProgressEnd { message: None })
}

fn begin_message(title: &str) -> WorkDoneProgress {
    WorkDoneProgress::Begin(WorkDoneProgressBegin {
        title: title.into(),
        cancellable: Some(false),
        message: None,
        percentage: None,
    })
}

fn create_token(token: &str, connection: &lsp_server::Connection) {
    let params = WorkDoneProgressCreateParams {
        token: NumberOrString::String(token.into()),
    };
    let request = lsp_server::Request {
        id: format!("create-token--{token}").into(),
        method: "window/workDoneProgress/create".into(),
        params: serde_json::to_value(params).expect("WorkDoneProgressCreateParams json"),
    };
    connection
        .sender
        .send(lsp_server::Message::Request(request))
        .expect("WorkDoneProgressCreate");
}
