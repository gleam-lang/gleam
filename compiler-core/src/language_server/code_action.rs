use std::{iter, sync::Arc};

use crate::{
    ast::{
        self,
        visit::{
            visit_typed_call_arg, visit_typed_expr_call, visit_typed_pattern_call_arg,
            visit_typed_record_update_arg, Visit as _,
        },
        AssignName, AssignmentKind, CallArg, ImplicitCallArgOrigin, Pattern, SrcSpan, TypedExpr,
        TypedPattern, TypedRecordUpdateArg,
    },
    build::Module,
    line_numbers::LineNumbers,
    parse::extra::ModuleExtra,
    type_::{self, error::ModuleSuggestion, FieldMap, ModuleValueConstructor, Type, TypedCallArg},
    Error,
};
use ecow::EcoString;
use im::HashMap;
use itertools::Itertools;
use lsp_types::{CodeAction, CodeActionKind, CodeActionParams, TextEdit, Url};

use super::{
    edits::{add_newlines_after_import, get_import_edit, position_of_first_definition_if_import},
    engine::{overlaps, within},
    src_span_to_lsp_range,
};

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
        type_: &'ast Arc<Type>,
        subjects: &'ast [TypedExpr],
        clauses: &'ast [ast::TypedClause],
    ) {
        'subj: for (subject_idx, subject) in subjects.iter().enumerate() {
            let TypedExpr::Tuple {
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

        ast::visit::visit_typed_expr_case(self, location, type_, subjects, clauses)
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

/// Builder for code action to convert `let assert` into a case expression.
///
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

        CodeActionBuilder::new("Convert to case")
            .kind(CodeActionKind::REFACTOR)
            .changes(uri.clone(), vec![edit])
            .preferred(true)
            .push_to(&mut self.actions);
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

/// Builder for code action to apply the label shorthand syntax on arguments
/// where the label has the same name as the variable.
///
pub struct LabelShorthandSyntax<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    line_numbers: LineNumbers,
    edits: Vec<TextEdit>,
}

impl<'a> LabelShorthandSyntax<'a> {
    pub fn new(module: &'a Module, params: &'a CodeActionParams) -> Self {
        let line_numbers = LineNumbers::new(&module.code);
        Self {
            module,
            params,
            line_numbers,
            edits: vec![],
        }
    }

    fn push_delete_edit(&mut self, location: &SrcSpan) {
        self.edits.push(TextEdit {
            range: src_span_to_lsp_range(*location, &self.line_numbers),
            new_text: "".into(),
        })
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);
        if self.edits.is_empty() {
            return vec![];
        }
        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Use label shorthand syntax")
            .kind(CodeActionKind::REFACTOR)
            .changes(self.params.text_document.uri.clone(), self.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for LabelShorthandSyntax<'_> {
    fn visit_typed_call_arg(&mut self, arg: &'ast TypedCallArg) {
        let arg_range = src_span_to_lsp_range(arg.location, &self.line_numbers);
        let is_selected = overlaps(arg_range, self.params.range);

        match arg {
            CallArg {
                label: Some(label),
                value: TypedExpr::Var { name, location, .. },
                ..
            } if is_selected && !arg.uses_label_shorthand() && label == name => {
                self.push_delete_edit(location)
            }
            _ => (),
        }

        visit_typed_call_arg(self, arg)
    }

    fn visit_typed_pattern_call_arg(&mut self, arg: &'ast CallArg<TypedPattern>) {
        let arg_range = src_span_to_lsp_range(arg.location, &self.line_numbers);
        let is_selected = overlaps(arg_range, self.params.range);

        match arg {
            CallArg {
                label: Some(label),
                value: TypedPattern::Variable { name, location, .. },
                ..
            } if is_selected && !arg.uses_label_shorthand() && label == name => {
                self.push_delete_edit(location)
            }
            _ => (),
        }

        visit_typed_pattern_call_arg(self, arg)
    }

    fn visit_typed_record_update_arg(&mut self, arg: &'ast TypedRecordUpdateArg) {
        let arg_range = src_span_to_lsp_range(arg.location, &self.line_numbers);
        let is_selected = overlaps(arg_range, self.params.range);

        match arg {
            TypedRecordUpdateArg {
                label,
                value: TypedExpr::Var { name, location, .. },
                ..
            } if is_selected && !arg.uses_label_shorthand() && label == name => {
                self.push_delete_edit(location)
            }
            _ => (),
        }

        visit_typed_record_update_arg(self, arg)
    }
}

/// Builder for code action to apply the fill in the missing labelled arguments
/// of the selected function call.
///
pub struct FillInMissingLabelledArgs<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    line_numbers: LineNumbers,
    selected_call: Option<(SrcSpan, &'a FieldMap, &'a [TypedCallArg])>,
}

impl<'a> FillInMissingLabelledArgs<'a> {
    pub fn new(module: &'a Module, params: &'a CodeActionParams) -> Self {
        let line_numbers = LineNumbers::new(&module.code);
        Self {
            module,
            params,
            line_numbers,
            selected_call: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        if let Some((call_location, field_map, args)) = self.selected_call {
            let mut missing_labels = field_map
                .fields
                .iter()
                .map(|(l, i)| (i, l))
                .collect::<HashMap<_, _>>();

            for arg in args.iter() {
                match arg.implicit {
                    Some(ImplicitCallArgOrigin::Use | ImplicitCallArgOrigin::IncorrectArityUse) => {
                        _ = missing_labels.remove(&(field_map.arity - 1))
                    }
                    Some(ImplicitCallArgOrigin::Pipe) => _ = missing_labels.remove(&0),
                    // We do not support this action for functions that have
                    // already been explicitly supplied an argument!
                    Some(ImplicitCallArgOrigin::PatternFieldSpread) | None => return vec![],
                }
            }

            // If we couldn't find any missing label to insert we just return.
            if missing_labels.is_empty() {
                return vec![];
            }

            let add_labels_edit = TextEdit {
                range: src_span_to_lsp_range(
                    SrcSpan {
                        start: call_location.end - 1,
                        end: call_location.end - 1,
                    },
                    &self.line_numbers,
                ),
                new_text: missing_labels
                    .iter()
                    .sorted_by_key(|(position, _label)| *position)
                    .map(|(_, label)| format!("{label}: todo"))
                    .join(", "),
            };

            let mut action = Vec::with_capacity(1);
            CodeActionBuilder::new("Fill labels")
                .kind(CodeActionKind::REFACTOR)
                .changes(self.params.text_document.uri.clone(), vec![add_labels_edit])
                .preferred(false)
                .push_to(&mut action);
            return action;
        }

        vec![]
    }
}

impl<'ast> ast::visit::Visit<'ast> for FillInMissingLabelledArgs<'ast> {
    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        args: &'ast [TypedCallArg],
    ) {
        let call_range = src_span_to_lsp_range(*location, &self.line_numbers);
        if !within(self.params.range, call_range) {
            return;
        }

        let field_map = match fun {
            TypedExpr::Var { constructor, .. } => constructor.field_map(),
            TypedExpr::ModuleSelect { constructor, .. } => match constructor {
                ModuleValueConstructor::Record { field_map, .. }
                | ModuleValueConstructor::Fn { field_map, .. } => field_map.as_ref(),
                ModuleValueConstructor::Constant { .. } => None,
            },
            _ => None,
        };

        if let Some(field_map) = field_map {
            self.selected_call = Some((*location, field_map, args))
        }

        // We only want to take into account the innermost function call
        // containing the current selection so we can't stop at the first call
        // we find (the outermost one) and have to keep traversing it in case
        // we're inside a nested call.
        visit_typed_expr_call(self, location, type_, fun, args)
    }
}

struct MissingImport {
    location: SrcSpan,
    suggestions: Vec<ImportSuggestion>,
}

struct ImportSuggestion {
    // The name to replace with, if the user made a typo
    name: EcoString,
    // The optional module to import, if suggesting an importable module
    import: Option<EcoString>,
}

pub fn code_action_import_module(
    module: &Module,
    params: &CodeActionParams,
    error: &Option<Error>,
    actions: &mut Vec<CodeAction>,
) {
    let uri = &params.text_document.uri;
    let Some(Error::Type { errors, .. }) = error else {
        return;
    };

    let missing_imports = errors
        .into_iter()
        .filter_map(|e| match e {
            type_::Error::UnknownModule {
                location,
                suggestions,
                ..
            } => suggest_imports(*location, suggestions),
            _ => None,
        })
        .collect_vec();

    if missing_imports.is_empty() {
        return;
    }

    let line_numbers = LineNumbers::new(&module.code);
    let first_import_pos = position_of_first_definition_if_import(module, &line_numbers);
    let first_is_import = first_import_pos.is_some();
    let import_location = first_import_pos.unwrap_or_default();

    let after_import_newlines = add_newlines_after_import(
        import_location,
        first_is_import,
        &line_numbers,
        &module.code,
    );

    for missing_import in missing_imports {
        let range = src_span_to_lsp_range(missing_import.location, &line_numbers);
        if !overlaps(params.range, range) {
            continue;
        }

        for suggestion in missing_import.suggestions {
            let mut edits = vec![TextEdit {
                range,
                new_text: suggestion.name.to_string(),
            }];
            if let Some(import) = &suggestion.import {
                edits.push(get_import_edit(
                    import_location,
                    import,
                    &after_import_newlines,
                ))
            };

            let title = if let Some(import) = &suggestion.import {
                &format!("Import `{import}`")
            } else {
                &format!("Did you mean `{}`", suggestion.name)
            };

            CodeActionBuilder::new(title)
                .kind(CodeActionKind::QUICKFIX)
                .changes(uri.clone(), edits)
                .preferred(true)
                .push_to(actions);
        }
    }
}

fn suggest_imports(
    location: SrcSpan,
    importable_modules: &[ModuleSuggestion],
) -> Option<MissingImport> {
    let suggestions = importable_modules
        .iter()
        .map(|suggestion| {
            let imported_name = suggestion.last_name_component();
            match suggestion {
                ModuleSuggestion::Importable(name) => ImportSuggestion {
                    name: imported_name.into(),
                    import: Some(name.clone()),
                },
                ModuleSuggestion::Imported(_) => ImportSuggestion {
                    name: imported_name.into(),
                    import: None,
                },
            }
        })
        .collect_vec();

    if suggestions.is_empty() {
        None
    } else {
        Some(MissingImport {
            location,
            suggestions,
        })
    }
}

pub fn code_action_add_missing_patterns(
    module: &Module,
    params: &CodeActionParams,
    error: &Option<Error>,
    actions: &mut Vec<CodeAction>,
) {
    let uri = &params.text_document.uri;
    let Some(Error::Type { errors, .. }) = error else {
        return;
    };
    let missing_patterns = errors
        .iter()
        .filter_map(|error| match error {
            type_::Error::InexhaustiveCaseExpression { location, missing } => {
                Some((*location, missing))
            }
            _ => None,
        })
        .collect_vec();

    if missing_patterns.is_empty() {
        return;
    }

    let line_numbers = LineNumbers::new(&module.code);

    for (location, missing) in missing_patterns {
        let range = src_span_to_lsp_range(location, &line_numbers);
        if !overlaps(params.range, range) {
            return;
        }

        let mut edits = Vec::new();
        // Add 2 to the indent for nesting
        let indent = " ".repeat(range.start.character as usize + 2);
        // Insert the missing patterns just before the closing brace
        let insert_range = src_span_to_lsp_range(
            SrcSpan {
                start: location.end - 1,
                end: location.end - 1,
            },
            &line_numbers,
        );
        let len = missing.len();
        for (i, pattern) in missing.iter().enumerate() {
            let new_text = format!(
                "\n{indent}{pattern} -> todo{}",
                // Add a newline to the last pattern
                if i == len - 1 { "\n" } else { "" }
            );
            edits.push(TextEdit {
                range: insert_range,
                new_text,
            })
        }

        CodeActionBuilder::new("Add missing patterns")
            .kind(CodeActionKind::QUICKFIX)
            .changes(uri.clone(), edits)
            .preferred(true)
            .push_to(actions);
    }
}
