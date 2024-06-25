use std::{iter, sync::Arc};

use crate::{
    ast::{self, visit::Visit as _, AssignName, AssignmentKind, Pattern, SrcSpan, TypedPattern},
    build::Module,
    line_numbers::LineNumbers,
    parse::extra::ModuleExtra,
    type_::Type,
};
use ecow::EcoString;
use lsp_types::{CodeAction, CodeActionKind, CodeActionParams, TextEdit, Url};

use super::{engine::overlaps, src_span_to_lsp_range};

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

    pub fn push_to(self, actions: &mut Vec<CodeAction>) {
        actions.push(self.action);
    }
}

/// Code action to remove literal tuples in case subjects, essentially making
/// the elements of the tuples into the case's subjects.
///
/// The code action is only available for the i'th subject if:
/// - it is a non-empty tuple, and
/// - the i'th pattern (including alternative patterns) is a literal tuple for all clauses.
///
/// # Basic example:
///
/// The following case expression:
///
/// ```gleam
/// case #(1, 2) {
///     #(a, b) -> 0
/// }
/// ```
///
/// Becomes:
///
/// ```gleam
/// case 1, 2 {
///     a, b -> 0
/// }
/// ```
///
/// # Another example:
///
/// The following case expression does not produce any code action
///
/// ```gleam
/// case #(1, 2) {
///     a -> 0 // <- the pattern is not a tuple
/// }
/// ```
pub struct RedundantTupleInCaseSubject<'a> {
    line_numbers: LineNumbers,
    code: &'a EcoString,
    extra: &'a ModuleExtra,
    params: &'a CodeActionParams,
    module: &'a ast::TypedModule,
    edits: Vec<TextEdit>,
    hovered: bool,
}

impl<'ast> ast::visit::Visit<'ast> for RedundantTupleInCaseSubject<'_> {
    fn visit_typed_expr_case(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        subjects: &'ast [ast::TypedExpr],
        clauses: &'ast [ast::TypedClause],
    ) {
        'subj: for (subject_idx, subject) in subjects.iter().enumerate() {
            let ast::TypedExpr::Tuple {
                location, elems, ..
            } = subject
            else {
                continue;
            };

            // Ignore empty tuple
            if elems.is_empty() {
                continue;
            }

            let mut clause_edits = vec![];
            for clause in clauses {
                match clause.pattern.get(subject_idx) {
                    Some(Pattern::Tuple { location, elems }) => {
                        clause_edits.extend(self.delete_tuple_tokens(
                            *location,
                            elems.last().map(|elem| elem.location()),
                        ))
                    }

                    Some(Pattern::Discard { location, .. }) => {
                        clause_edits.push(self.discard_tuple_items(*location, elems.len()))
                    }

                    // Do not edit for this subject at all and go to the next subject
                    _ => continue 'subj,
                }
            }

            let range = src_span_to_lsp_range(*location, &self.line_numbers);
            self.hovered = self.hovered || overlaps(self.params.range, range);

            self.edits.extend(
                self.delete_tuple_tokens(*location, elems.last().map(|elem| elem.location())),
            );
            self.edits.extend(clause_edits);
        }

        ast::visit::visit_typed_expr_case(self, location, typ, subjects, clauses)
    }
}

impl<'a> RedundantTupleInCaseSubject<'a> {
    pub fn new(module: &'a Module, params: &'a CodeActionParams) -> Self {
        Self {
            line_numbers: LineNumbers::new(&module.code),
            code: &module.code,
            extra: &module.extra,
            params,
            module: &module.ast,
            edits: vec![],
            hovered: false,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(self.module);
        if !self.hovered {
            return vec![];
        }

        self.edits.sort_by_key(|edit| edit.range.start);

        let mut actions = vec![];
        CodeActionBuilder::new("Remove redundant tuples")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits)
            .preferred(true)
            .push_to(&mut actions);

        actions
    }

    fn delete_tuple_tokens(
        &self,
        location: SrcSpan,
        last_elem_location: Option<SrcSpan>,
    ) -> Vec<TextEdit> {
        let tuple_code = self
            .code
            .get(location.start as usize..location.end as usize)
            .expect("valid span");

        let mut edits = vec![];

        // Delete `#`
        edits.push(TextEdit {
            range: src_span_to_lsp_range(
                SrcSpan::new(location.start, location.start + 1),
                &self.line_numbers,
            ),
            new_text: "".to_string(),
        });

        // Delete `(`
        let (lparen_offset, _) = tuple_code
            .match_indices('(')
            // Ignore in comments
            .find(|(i, _)| !self.extra.is_within_comment(location.start + *i as u32))
            .expect("`(` not found in tuple");

        edits.push(TextEdit {
            range: src_span_to_lsp_range(
                SrcSpan::new(
                    location.start + lparen_offset as u32,
                    location.start + lparen_offset as u32 + 1,
                ),
                &self.line_numbers,
            ),
            new_text: "".to_string(),
        });

        // Delete trailing `,` (if applicable)
        if let Some(last_elem_location) = last_elem_location {
            // Get the code after the last element until the tuple's `)`
            let code_after_last_elem = self
                .code
                .get(last_elem_location.end as usize..location.end as usize)
                .expect("valid span");

            if let Some((trailing_comma_offset, _)) = code_after_last_elem
                .rmatch_indices(',')
                // Ignore in comments
                .find(|(i, _)| {
                    !self
                        .extra
                        .is_within_comment(last_elem_location.end + *i as u32)
                })
            {
                edits.push(TextEdit {
                    range: src_span_to_lsp_range(
                        SrcSpan::new(
                            last_elem_location.end + trailing_comma_offset as u32,
                            last_elem_location.end + trailing_comma_offset as u32 + 1,
                        ),
                        &self.line_numbers,
                    ),
                    new_text: "".to_string(),
                });
            }
        }

        // Delete )
        edits.push(TextEdit {
            range: src_span_to_lsp_range(
                SrcSpan::new(location.end - 1, location.end),
                &self.line_numbers,
            ),
            new_text: "".to_string(),
        });

        edits
    }

    fn discard_tuple_items(&self, discard_location: SrcSpan, tuple_items: usize) -> TextEdit {
        // Replace the old discard with multiple discard, one for each of the
        // tuple items.
        TextEdit {
            range: src_span_to_lsp_range(discard_location, &self.line_numbers),
            new_text: itertools::intersperse(iter::repeat("_").take(tuple_items), ", ").collect(),
        }
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
        let range = src_span_to_lsp_range(assignment.location, &self.line_numbers);

        // Only offer the code action if the cursor is over the statement
        if !overlaps(range, self.params.range) {
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

        let indent = " ".repeat(range.start.character as usize);

        // Figure out which variables are assigned in the pattern
        self.traverse_pattern(&assignment.pattern);
        let variables = std::mem::take(&mut self.pattern_variables);

        let assigned = match variables.len() {
            0 => "",
            1 => variables.first().expect("Variables is length one"),
            _ => &format!("#({})", variables.join(", ")),
        };

        let edit = TextEdit {
            range,
            new_text: format!(
                "let {assigned} = case {expr} {{
{indent}  {pattern} -> {assigned}
{indent}  _ -> panic
{indent}}}",
                // "_" isn't a valid expression, so we just return Nil from the case expression
                assigned = if assigned == "_" { "Nil" } else { assigned }
            ),
        };

        let uri = &self.params.text_document.uri;

        CodeActionBuilder::new("Convert to case")
            .kind(CodeActionKind::REFACTOR)
            .changes(uri.clone(), vec![edit])
            .preferred(true)
            .push_to(&mut self.actions);
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

    // Recursively traverses a pattern and finds which variables get bound
    fn traverse_pattern(&mut self, pattern: &TypedPattern) {
        println!("{pattern:#?}");
        match pattern {
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::VarUsage { .. }
            | Pattern::Discard { .. }
            | Pattern::Invalid { .. }
            | Pattern::String { .. } => {}
            Pattern::Variable { name, .. } => self.pattern_variables.push(name.clone()),
            Pattern::Assign { name, pattern, .. } => {
                println!("Assign");
                self.pattern_variables.push(name.clone());
                self.traverse_pattern(pattern);
            }
            Pattern::List { elements, tail, .. } => {
                for elem in elements {
                    self.traverse_pattern(elem);
                }
                if let Some(tail) = tail {
                    self.traverse_pattern(tail);
                }
            }
            Pattern::Constructor { arguments, .. } => {
                for arg in arguments {
                    self.traverse_pattern(&arg.value);
                }
            }
            Pattern::Tuple { elems, .. } => {
                for elem in elems {
                    self.traverse_pattern(elem);
                }
            }
            Pattern::BitArray { segments, .. } => {
                for segment in segments {
                    self.traverse_pattern(&segment.value);
                }
            }
            Pattern::StringPrefix {
                left_side_assignment,
                right_side_assignment,
                ..
            } => {
                if let Some((name, _)) = left_side_assignment {
                    self.pattern_variables.push(name.clone());
                }
                if let AssignName::Variable(name) = right_side_assignment {
                    self.pattern_variables.push(name.clone());
                }
            }
        }
    }
}
