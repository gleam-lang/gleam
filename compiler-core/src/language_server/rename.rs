use std::collections::HashMap;

use lsp_types::{RenameParams, TextEdit, Url, WorkspaceEdit};

use crate::{ast::SrcSpan, build::Module, line_numbers::LineNumbers};

use super::TextEdits;

fn workspace_edit(uri: Url, edits: Vec<TextEdit>) -> WorkspaceEdit {
    let mut changes = HashMap::new();
    let _ = changes.insert(uri, edits);

    WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    }
}

pub fn rename_local_variable(
    _module: &Module,
    line_numbers: &LineNumbers,
    params: &RenameParams,
    location: SrcSpan,
    definition_location: SrcSpan,
) -> Option<WorkspaceEdit> {
    let uri = params.text_document_position.text_document.uri.clone();
    let mut edits = TextEdits::new(line_numbers);

    edits.replace(location, params.new_name.clone());
    edits.replace(definition_location, params.new_name.clone());

    Some(workspace_edit(uri, edits.edits))
}
