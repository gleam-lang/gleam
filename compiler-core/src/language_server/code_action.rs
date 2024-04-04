use lsp_types::{CodeAction, Url};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct CodeActionData {
    pub id: ActionId,
    pub code_action_params: lsp_types::CodeActionParams,
    pub location: u32
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ActionId {
    Pipeline,
    InlineLocalVar,
    UnusedImports,
}


#[derive(Debug)]
pub struct CodeActionBuilder {
    action: CodeAction,
}

impl CodeActionBuilder {
    pub fn new(title: &str) -> Self {
        Self {
            action: CodeAction {
                title: title.to_string(),
                kind: None,
                diagnostics: None,
                edit: None,
                command: None,
                is_preferred: None,
                disabled: None,
                data: None,
            },
        }
    }

    pub fn kind(mut self, kind: lsp_types::CodeActionKind) -> Self {
        self.action.kind = Some(kind);
        self
    }

    pub fn changes(mut self, uri: Url, edits: Vec<lsp_types::TextEdit>) -> Self {
        let mut edit = self.action.edit.take().unwrap_or_default();
        let mut changes = edit.changes.take().unwrap_or_default();
        _ = changes.insert(uri, edits);

        edit.changes = Some(changes);
        self.action.edit = Some(edit);
        self
    }

    pub fn preferred(mut self, is_preferred: bool) -> Self {
        self.action.is_preferred = Some(is_preferred);
        self
    }

    pub fn data(mut self, id: ActionId, code_action_params: lsp_types::CodeActionParams, location: u32) -> Self {
        let code_action_data = CodeActionData {
            id,
            code_action_params,
            location
        };
        let js = serde_json::to_value(code_action_data).unwrap_or_default();
        self.action.data = Some(js);
        self
    }

    pub fn push_to(self, actions: &mut Vec<CodeAction>) {
        actions.push(self.action);
    }

}