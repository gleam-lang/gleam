use std::sync::Arc;

use lsp_types::{CodeAction, CodeActionKind, CodeActionParams, TextEdit, Url};

use crate::{
    ast::{self, visit::Visit as _, AssignName, AssignmentKind, SrcSpan, TypedPattern},
    build::Module,
    line_numbers::LineNumbers,
    type_::Type,
};
use ecow::EcoString;

use super::{engine::overlaps, src_span_to_lsp_range};

pub mod move_imports_to_top;
pub mod redundant_tuple_in_case_subject;

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

    pub fn kind(mut self, kind: CodeActionKind) -> Self {
        self.action.kind = Some(kind);
        self
    }

    pub fn changes(mut self, uri: Url, edits: Vec<TextEdit>) -> Self {
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

    pub fn build(self) -> CodeAction {
        self.action
    }
}

// Builder for code action to convert `let assert` into a case expression
pub struct LetAssertToCase<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    actions: Vec<CodeAction>,
    line_numbers: LineNumbers,
    pattern_variables: Vec<EcoString>,
}

impl<'ast> ast::visit::Visit<'ast> for LetAssertToCase<'_> {
    fn visit_typed_assignment(&mut self, assignment: &'ast ast::TypedAssignment) {
        // To prevent weird behaviour when `let assert` statements are nested,
        // we only check for the code action between the `let` and `=`.
        let code_action_location =
            SrcSpan::new(assignment.location.start, assignment.value.location().start);
        let code_action_range = src_span_to_lsp_range(code_action_location, &self.line_numbers);

        self.visit_typed_expr(&assignment.value);

        // Only offer the code action if the cursor is over the statement
        if !overlaps(code_action_range, self.params.range) {
            return;
        }

        // This pattern only applies to `let assert`
        if !matches!(assignment.kind, AssignmentKind::Assert { .. }) {
            return;
        };

        // Get the source code for the tested expression
        let location = assignment.value.location();
        let expr = self
            .module
            .code
            .get(location.start as usize..location.end as usize)
            .expect("Location must be valid");

        // Get the source code for the pattern
        let pattern_location = assignment.pattern.location();
        let pattern = self
            .module
            .code
            .get(pattern_location.start as usize..pattern_location.end as usize)
            .expect("Location must be valid");

        let range = src_span_to_lsp_range(assignment.location, &self.line_numbers);
        let indent = " ".repeat(range.start.character as usize);

        // Figure out which variables are assigned in the pattern
        self.pattern_variables.clear();
        self.visit_typed_pattern(&assignment.pattern);
        let variables = std::mem::take(&mut self.pattern_variables);

        let assigned = match variables.len() {
            0 => "_",
            1 => variables.first().expect("Variables is length one"),
            _ => &format!("#({})", variables.join(", ")),
        };

        let edit = TextEdit {
            range,
            new_text: format!(
                "let {assigned} = case {expr} {{
{indent}  {pattern} -> {value}
{indent}  _ -> panic
{indent}}}",
                // "_" isn't a valid expression, so we just return Nil from the case expression
                value = if assigned == "_" { "Nil" } else { assigned }
            ),
        };

        let uri = &self.params.text_document.uri;

        let action = CodeActionBuilder::new("Convert to case")
            .kind(CodeActionKind::REFACTOR)
            .changes(uri.clone(), vec![edit])
            .preferred(true)
            .build();

        self.actions.push(action);
    }

    fn visit_typed_pattern_variable(
        &mut self,
        _location: &'ast SrcSpan,
        name: &'ast EcoString,
        _type: &'ast Arc<Type>,
    ) {
        self.pattern_variables.push(name.clone());
    }

    fn visit_typed_pattern_assign(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        pattern: &'ast TypedPattern,
    ) {
        self.pattern_variables.push(name.clone());
        ast::visit::visit_typed_pattern_assign(self, location, name, pattern);
    }

    fn visit_typed_pattern_string_prefix(
        &mut self,
        _location: &'ast SrcSpan,
        _left_location: &'ast SrcSpan,
        left_side_assignment: &'ast Option<(EcoString, SrcSpan)>,
        _right_location: &'ast SrcSpan,
        _left_side_string: &'ast EcoString,
        right_side_assignment: &'ast AssignName,
    ) {
        if let Some((name, _)) = left_side_assignment {
            self.pattern_variables.push(name.clone());
        }
        if let AssignName::Variable(name) = right_side_assignment {
            self.pattern_variables.push(name.clone());
        }
    }
}

impl<'a> LetAssertToCase<'a> {
    pub fn new(module: &'a Module, params: &'a CodeActionParams) -> Self {
        let line_numbers = LineNumbers::new(&module.code);
        Self {
            module,
            params,
            actions: Vec::new(),
            line_numbers,
            pattern_variables: Vec::new(),
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);
        self.actions
    }
}
