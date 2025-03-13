use std::collections::HashMap;

use ecow::EcoString;
use lsp_types::{RenameParams, TextEdit, Url, WorkspaceEdit};

use crate::{
    analyse::name,
    ast::{self, SrcSpan, TypedModule, visit::Visit},
    build::Module,
    line_numbers::LineNumbers,
    reference::ReferenceKind,
    type_::{ModuleInterface, ValueConstructor, ValueConstructorVariant, error::Named},
};

use super::{TextEdits, compiler::ModuleSourceInformation};

fn workspace_edit(uri: Url, edits: Vec<TextEdit>) -> WorkspaceEdit {
    let mut changes = HashMap::new();
    let _ = changes.insert(uri, edits);

    WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    }
}

pub enum VariableRenameKind {
    Variable,
    LabelShorthand,
}

pub fn rename_local_variable(
    module: &Module,
    line_numbers: &LineNumbers,
    params: &RenameParams,
    definition_location: SrcSpan,
    kind: VariableRenameKind,
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

    let references = find_variable_references(&module.ast, definition_location);

    match kind {
        VariableRenameKind::Variable => edits.replace(definition_location, params.new_name.clone()),
        VariableRenameKind::LabelShorthand => {
            edits.insert(definition_location.end, format!(" {}", params.new_name))
        }
    }

    for location in references {
        edits.replace(location, params.new_name.clone());
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
}

pub fn rename_module_value(
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
) {
    let Some(references) = module
        .references
        .value_references
        .get(&(module_name.clone(), name.clone()))
    else {
        return;
    };

    let mut edits = TextEdits::new(&source_information.line_numbers);

    for reference in references {
        edits.replace(reference.location, new_name.clone());
    }

    let Some(uri) = url_from_path(source_information.path.as_str()) else {
        return;
    };

    if let Some(changes) = workspace_edit.changes.as_mut() {
        _ = changes.insert(uri, edits.edits);
    }
}

fn url_from_path(path: &str) -> Option<Url> {
    // The targets for which `from_file_path` is defined
    #[cfg(any(
        unix,
        windows,
        target_os = "redox",
        target_os = "wasi",
        target_os = "hermit"
    ))]
    let uri = Url::from_file_path(path).ok();

    #[cfg(not(any(
        unix,
        windows,
        target_os = "redox",
        target_os = "wasi",
        target_os = "hermit"
    )))]
    let uri = Url::parse(&format!("file://{path}")).ok();

    uri
}

fn alias_references_in_module(
    params: &RenameParams,
    module: &Module,
    module_name: &EcoString,
    name: &EcoString,
) -> Option<WorkspaceEdit> {
    let references = module
        .ast
        .type_info
        .references
        .value_references
        .get(&(module_name.clone(), name.clone()))?;

    let mut edits = TextEdits::new(&module.ast.type_info.line_numbers);

    for reference in references {
        match reference.kind {
            ReferenceKind::Qualified => {}
            ReferenceKind::Unqualified => {
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

pub fn find_variable_references(
    module: &TypedModule,
    definition_location: SrcSpan,
) -> Vec<SrcSpan> {
    let mut finder = FindVariableReferences {
        references: Vec::new(),
        definition_location,
    };
    finder.visit_typed_module(module);
    finder.references
}

struct FindVariableReferences {
    references: Vec<SrcSpan>,
    definition_location: SrcSpan,
}

impl<'ast> Visit<'ast> for FindVariableReferences {
    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        if fun.full_location().contains(self.definition_location.start) {
            ast::visit::visit_typed_function(self, fun);
        }
    }

    fn visit_typed_expr_var(
        &mut self,
        location: &'ast SrcSpan,
        constructor: &'ast ValueConstructor,
        _name: &'ast EcoString,
    ) {
        match constructor.variant {
            ValueConstructorVariant::LocalVariable {
                location: definition_location,
                ..
            } if definition_location == self.definition_location => self.references.push(*location),
            _ => {}
        }
    }

    fn visit_typed_clause_guard_var(
        &mut self,
        location: &'ast SrcSpan,
        _name: &'ast EcoString,
        _type_: &'ast std::sync::Arc<crate::type_::Type>,
        definition_location: &'ast SrcSpan,
    ) {
        if *definition_location == self.definition_location {
            self.references.push(*location)
        }
    }

    fn visit_typed_pattern_var_usage(
        &mut self,
        location: &'ast SrcSpan,
        _name: &'ast EcoString,
        constructor: &'ast Option<ValueConstructor>,
        _type_: &'ast std::sync::Arc<crate::type_::Type>,
    ) {
        let variant = match constructor {
            Some(constructor) => &constructor.variant,
            None => return,
        };
        match variant {
            ValueConstructorVariant::LocalVariable {
                location: definition_location,
                ..
            } if *definition_location == self.definition_location => {
                self.references.push(*location)
            }
            _ => {}
        }
    }
}
