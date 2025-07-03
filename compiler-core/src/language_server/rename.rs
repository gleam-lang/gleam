use std::collections::HashMap;

use ecow::EcoString;
use lsp_types::{RenameParams, TextEdit, Url, WorkspaceEdit};

use crate::{
    analyse::name,
    ast::{self, SrcSpan},
    build::Module,
    line_numbers::LineNumbers,
    reference::ReferenceKind,
    type_::{ModuleInterface, error::Named},
};

use super::{
    TextEdits,
    compiler::ModuleSourceInformation,
    reference::{VariableReferenceKind, find_variable_references},
    url_from_path,
};

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
    module: &Module,
    line_numbers: &LineNumbers,
    params: &RenameParams,
    definition_location: SrcSpan,
    name: EcoString,
    kind: VariableReferenceKind,
) -> Option<WorkspaceEdit> {
    if name::check_name_case(
        Default::default(),
        &params.new_name.as_str().into(),
        Named::Variable,
    )
    .is_err()
    {
        return None;
    }

    let uri = params.text_document_position.text_document.uri.clone();
    let mut edits = TextEdits::new(line_numbers);

    let references = find_variable_references(&module.ast, definition_location, name);

    match kind {
        VariableReferenceKind::Variable => {
            edits.replace(definition_location, params.new_name.clone())
        }
        VariableReferenceKind::LabelShorthand => {
            edits.insert(definition_location.end, format!(" {}", params.new_name))
        }
    }

    for reference in references {
        match reference.kind {
            VariableReferenceKind::Variable => {
                edits.replace(reference.location, params.new_name.clone())
            }
            VariableReferenceKind::LabelShorthand => {
                edits.insert(reference.location.end, format!(" {}", params.new_name))
            }
        }
    }

    Some(workspace_edit(uri, edits.edits))
}

pub enum RenameTarget {
    Qualified,
    Unqualified,
    Definition,
}

pub struct Renamed<'a> {
    pub module_name: &'a EcoString,
    pub name: &'a EcoString,
    pub name_kind: Named,
    pub target_kind: RenameTarget,
    pub layer: ast::Layer,
}

pub fn rename_module_entity(
    params: &RenameParams,
    current_module: &Module,
    modules: &im::HashMap<EcoString, ModuleInterface>,
    sources: &HashMap<EcoString, ModuleSourceInformation>,
    renamed: Renamed<'_>,
) -> Option<WorkspaceEdit> {
    if name::check_name_case(
        // We don't care about the actual error here, just whether the name is valid,
        // so we just use the default span.
        SrcSpan::default(),
        &params.new_name.as_str().into(),
        renamed.name_kind,
    )
    .is_err()
    {
        return None;
    }

    match renamed.target_kind {
        RenameTarget::Unqualified if renamed.module_name != &current_module.name => {
            return alias_references_in_module(
                params,
                current_module,
                renamed.module_name,
                renamed.name,
                renamed.layer,
            );
        }
        RenameTarget::Unqualified | RenameTarget::Qualified | RenameTarget::Definition => {}
    }

    let mut workspace_edit = WorkspaceEdit {
        changes: Some(HashMap::new()),
        document_changes: None,
        change_annotations: None,
    };

    for module in modules.values() {
        if &module.name == renamed.module_name
            || module
                .references
                .imported_modules
                .contains(renamed.module_name)
        {
            let Some(source_information) = sources.get(&module.name) else {
                continue;
            };

            rename_references_in_module(
                module,
                source_information,
                &mut workspace_edit,
                renamed.module_name,
                renamed.name,
                params.new_name.clone(),
                renamed.layer,
            );
        }
    }

    Some(workspace_edit)
}

fn rename_references_in_module(
    module: &ModuleInterface,
    source_information: &ModuleSourceInformation,
    workspace_edit: &mut WorkspaceEdit,
    module_name: &EcoString,
    name: &EcoString,
    new_name: String,
    layer: ast::Layer,
) {
    let reference_map = match layer {
        ast::Layer::Value => &module.references.value_references,
        ast::Layer::Type => &module.references.type_references,
    };

    let Some(references) = reference_map.get(&(module_name.clone(), name.clone())) else {
        return;
    };

    let mut edits = TextEdits::new(&source_information.line_numbers);

    for reference in references {
        match reference.kind {
            // If the reference is an alias, the alias name will remain unchanged.
            ReferenceKind::Alias => {}
            ReferenceKind::Qualified
            | ReferenceKind::Unqualified
            | ReferenceKind::Import
            | ReferenceKind::Definition => edits.replace(reference.location, new_name.clone()),
        }
    }

    let Some(uri) = url_from_path(source_information.path.as_str()) else {
        return;
    };

    if let Some(changes) = workspace_edit.changes.as_mut() {
        _ = changes.insert(uri, edits.edits);
    }
}

fn alias_references_in_module(
    params: &RenameParams,
    module: &Module,
    module_name: &EcoString,
    name: &EcoString,
    layer: ast::Layer,
) -> Option<WorkspaceEdit> {
    let reference_map = match layer {
        ast::Layer::Value => &module.ast.type_info.references.value_references,
        ast::Layer::Type => &module.ast.type_info.references.type_references,
    };

    let references = reference_map.get(&(module_name.clone(), name.clone()))?;

    let mut edits = TextEdits::new(&module.ast.type_info.line_numbers);

    for reference in references {
        match reference.kind {
            ReferenceKind::Qualified => {}
            ReferenceKind::Unqualified | ReferenceKind::Alias => {
                edits.replace(reference.location, params.new_name.clone())
            }
            ReferenceKind::Import => {
                edits.insert(reference.location.end, format!(" as {}", params.new_name))
            }
            ReferenceKind::Definition => {}
        }
    }

    Some(workspace_edit(
        params.text_document_position.text_document.uri.clone(),
        edits.edits,
    ))
}
