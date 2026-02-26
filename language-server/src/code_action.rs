use std::{collections::HashSet, iter, sync::Arc};

use ecow::{EcoString, eco_format};
use gleam_core::{
    Error, STDLIB_PACKAGE_NAME,
    analyse::Inferred,
    ast::{
        self, ArgNames, AssignName, AssignmentKind, BitArraySegmentTruncation, BoundVariable,
        BoundVariableName, CallArg, CustomType, FunctionLiteralKind, ImplicitCallArgOrigin, Import,
        InvalidExpression, PIPE_PRECEDENCE, Pattern, PatternUnusedArguments,
        PipelineAssignmentKind, Publicity, RecordConstructor, SrcSpan, TodoKind, TypedArg,
        TypedAssignment, TypedClauseGuard, TypedDefinitions, TypedExpr, TypedFunction,
        TypedModuleConstant, TypedPattern, TypedPipelineAssignment, TypedRecordConstructor,
        TypedStatement, TypedTailPattern, TypedUse, visit::Visit as _,
    },
    build::{Located, Module},
    config::PackageConfig,
    exhaustiveness::CompiledCase,
    line_numbers::LineNumbers,
    parse::{extra::ModuleExtra, lexer::str_to_keyword},
    strings::to_snake_case,
    type_::{
        self, FieldMap, ModuleValueConstructor, Opaque, Type, TypeVar, TypedCallArg,
        ValueConstructor,
        error::{ModuleSuggestion, VariableDeclaration, VariableOrigin},
        printer::Printer,
    },
};
use im::HashMap;
use itertools::Itertools;
use lsp_types::{CodeAction, CodeActionKind, CodeActionParams, Position, Range, TextEdit, Url};
use vec1::{Vec1, vec1};

use super::{
    TextEdits,
    compiler::LspProjectCompiler,
    edits,
    edits::{add_newlines_after_import, get_import_edit, position_of_first_definition_if_import},
    engine::{overlaps, within},
    files::FileSystemProxy,
    reference::{FindVariableReferences, VariableReferenceKind},
    src_span_to_lsp_range, url_from_path,
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

/// A small helper function to get the indentation at a given position.
fn count_indentation(code: &str, line_numbers: &LineNumbers, line: u32) -> usize {
    let mut indent_size = 0;
    let line_start = *line_numbers
        .line_starts
        .get(line as usize)
        .expect("Line number should be valid");

    let mut chars = code[line_start as usize..].chars();
    while chars.next() == Some(' ') {
        indent_size += 1;
    }

    indent_size
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
    edits: TextEdits<'a>,
    code: &'a EcoString,
    extra: &'a ModuleExtra,
    params: &'a CodeActionParams,
    module: &'a ast::TypedModule,
    hovered: bool,
}

impl<'ast> ast::visit::Visit<'ast> for RedundantTupleInCaseSubject<'_> {
    fn visit_typed_expr_case(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        subjects: &'ast [TypedExpr],
        clauses: &'ast [ast::TypedClause],
        compiled_case: &'ast CompiledCase,
    ) {
        for (subject_idx, subject) in subjects.iter().enumerate() {
            let TypedExpr::Tuple {
                location, elements, ..
            } = subject
            else {
                continue;
            };

            // Ignore empty tuple
            if elements.is_empty() {
                continue;
            }

            // We cannot rewrite clauses whose i-th pattern is not a discard or
            // tuples.
            let all_ith_patterns_are_tuples_or_discards = clauses
                .iter()
                .map(|clause| clause.pattern.get(subject_idx))
                .all(|pattern| {
                    matches!(
                        pattern,
                        Some(Pattern::Tuple { .. } | Pattern::Discard { .. })
                    )
                });

            if !all_ith_patterns_are_tuples_or_discards {
                continue;
            }

            self.delete_tuple_tokens(*location, elements.last().map(|element| element.location()));

            for clause in clauses {
                match clause.pattern.get(subject_idx) {
                    Some(Pattern::Tuple { location, elements }) => self.delete_tuple_tokens(
                        *location,
                        elements.last().map(|element| element.location()),
                    ),
                    Some(Pattern::Discard { location, .. }) => {
                        self.discard_tuple_items(*location, elements.len())
                    }
                    _ => panic!("safe: we've just checked all patterns must be discards/tuples"),
                }
            }
            let range = self.edits.src_span_to_lsp_range(*location);
            self.hovered = self.hovered || overlaps(self.params.range, range);
        }

        ast::visit::visit_typed_expr_case(self, location, type_, subjects, clauses, compiled_case)
    }
}

impl<'a> RedundantTupleInCaseSubject<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            edits: TextEdits::new(line_numbers),
            code: &module.code,
            extra: &module.extra,
            params,
            module: &module.ast,
            hovered: false,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(self.module);
        if !self.hovered {
            return vec![];
        }

        self.edits.edits.sort_by_key(|edit| edit.range.start);

        let mut actions = vec![];
        CodeActionBuilder::new("Remove redundant tuples")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(true)
            .push_to(&mut actions);

        actions
    }

    fn delete_tuple_tokens(&mut self, location: SrcSpan, last_elem_location: Option<SrcSpan>) {
        let tuple_code = self
            .code
            .get(location.start as usize..location.end as usize)
            .expect("valid span");

        // Delete `#`
        self.edits
            .delete(SrcSpan::new(location.start, location.start + 1));

        // Delete `(`
        let (lparen_offset, _) = tuple_code
            .match_indices('(')
            // Ignore in comments
            .find(|(i, _)| !self.extra.is_within_comment(location.start + *i as u32))
            .expect("`(` not found in tuple");

        self.edits.delete(SrcSpan::new(
            location.start + lparen_offset as u32,
            location.start + lparen_offset as u32 + 1,
        ));

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
                self.edits.delete(SrcSpan::new(
                    last_elem_location.end + trailing_comma_offset as u32,
                    last_elem_location.end + trailing_comma_offset as u32 + 1,
                ));
            }
        }

        // Delete )
        self.edits
            .delete(SrcSpan::new(location.end - 1, location.end));
    }

    fn discard_tuple_items(&mut self, discard_location: SrcSpan, tuple_items: usize) {
        // Replace the old discard with multiple discard, one for each of the
        // tuple items.
        self.edits.replace(
            discard_location,
            itertools::intersperse(iter::repeat_n("_", tuple_items), ", ").collect(),
        )
    }
}

/// Builder for code action to convert `let assert` into a case expression.
///
pub struct LetAssertToCase<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    actions: Vec<CodeAction>,
    edits: TextEdits<'a>,
}

impl<'ast> ast::visit::Visit<'ast> for LetAssertToCase<'_> {
    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        let assignment_range = self.edits.src_span_to_lsp_range(assignment.location);
        let assignment_start_range = self.edits.src_span_to_lsp_range(SrcSpan {
            start: assignment.location.start,
            end: assignment.value.location().start,
        });
        self.visit_typed_expr(&assignment.value);

        // Only offer the code action if the cursor is over the statement and
        // to prevent weird behaviour when `let assert` statements are nested,
        // we only check for the code action between the `let` and `=`.
        if !(within(self.params.range, assignment_range)
            && overlaps(self.params.range, assignment_start_range))
        {
            return;
        }

        // This pattern only applies to `let assert`
        let AssignmentKind::Assert { message, .. } = &assignment.kind else {
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

        let message = message.as_ref().map(|message| {
            let location = message.location();
            self.module
                .code
                .get(location.start as usize..location.end as usize)
                .expect("Location must be valid")
        });

        let range = self.edits.src_span_to_lsp_range(assignment.location);

        // Figure out which variables are assigned in the pattern
        let variables = PatternVariableFinder::find_variables_in_pattern(&assignment.pattern);

        let assigned = match variables.len() {
            0 => "_",
            1 => variables.first().expect("Variables is length one"),
            _ => &format!("#({})", variables.join(", ")),
        };

        let mut new_text = format!("let {assigned} = ");
        let panic_message = if let Some(message) = message {
            &format!("panic as {message}")
        } else {
            "panic"
        };
        let clauses = vec![
            // The existing pattern
            CaseClause {
                pattern,
                // `_` is not a valid expression, so if we are not
                // binding any variables in the pattern, we simply return Nil.
                expression: if assigned == "_" { "Nil" } else { assigned },
            },
            CaseClause {
                pattern: "_",
                expression: panic_message,
            },
        ];
        print_case_expression(range.start.character as usize, expr, clauses, &mut new_text);

        let uri = &self.params.text_document.uri;

        CodeActionBuilder::new("Convert to case")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(uri.clone(), vec![TextEdit { range, new_text }])
            .preferred(false)
            .push_to(&mut self.actions);
    }
}

impl<'a> LetAssertToCase<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            actions: Vec::new(),
            edits: TextEdits::new(line_numbers),
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);
        self.actions
    }
}

struct PatternVariableFinder {
    pattern_variables: Vec<EcoString>,
}

impl PatternVariableFinder {
    fn new() -> Self {
        Self {
            pattern_variables: Vec::new(),
        }
    }

    fn find_variables_in_pattern(pattern: &TypedPattern) -> Vec<EcoString> {
        let mut finder = Self::new();
        finder.visit_typed_pattern(pattern);
        finder.pattern_variables
    }
}

impl<'ast> ast::visit::Visit<'ast> for PatternVariableFinder {
    fn visit_typed_pattern_variable(
        &mut self,
        _location: &'ast SrcSpan,
        name: &'ast EcoString,
        _type: &'ast Arc<Type>,
        _origin: &'ast VariableOrigin,
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

pub fn code_action_inexhaustive_let_to_case(
    module: &Module,
    line_numbers: &LineNumbers,
    params: &CodeActionParams,
    error: &Option<Error>,
    actions: &mut Vec<CodeAction>,
) {
    let Some(Error::Type { errors, .. }) = error else {
        return;
    };
    let inexhaustive_assignments = errors
        .iter()
        .filter_map(|error| {
            if let type_::Error::InexhaustiveLetAssignment { location, missing } = error {
                Some((*location, missing))
            } else {
                None
            }
        })
        .collect_vec();

    if inexhaustive_assignments.is_empty() {
        return;
    }

    for (location, missing) in inexhaustive_assignments {
        let mut text_edits = TextEdits::new(line_numbers);

        let range = text_edits.src_span_to_lsp_range(location);
        if !overlaps(params.range, range) {
            return;
        }

        let Some(Located::Statement(TypedStatement::Assignment(assignment))) =
            module.find_node(location.start)
        else {
            continue;
        };

        let TypedAssignment {
            value,
            pattern,
            kind: AssignmentKind::Let,
            location,
            compiled_case: _,
            annotation: _,
        } = assignment.as_ref()
        else {
            continue;
        };

        // Get the source code for the tested expression
        let value_location = value.location();
        let expr = module
            .code
            .get(value_location.start as usize..value_location.end as usize)
            .expect("Location must be valid");

        // Get the source code for the pattern
        let pattern_location = pattern.location();
        let pattern_code = module
            .code
            .get(pattern_location.start as usize..pattern_location.end as usize)
            .expect("Location must be valid");

        let range = text_edits.src_span_to_lsp_range(*location);

        // Figure out which variables are assigned in the pattern
        let variables = PatternVariableFinder::find_variables_in_pattern(pattern);

        let assigned = match variables.len() {
            0 => "_",
            1 => variables.first().expect("Variables is length one"),
            _ => &format!("#({})", variables.join(", ")),
        };

        let mut new_text = format!("let {assigned} = ");
        print_case_expression(
            range.start.character as usize,
            expr,
            iter::once(CaseClause {
                pattern: pattern_code,
                expression: if assigned == "_" { "Nil" } else { assigned },
            })
            .chain(missing.iter().map(|pattern| CaseClause {
                pattern,
                expression: "todo",
            }))
            .collect(),
            &mut new_text,
        );

        let uri = &params.text_document.uri;

        text_edits.replace(*location, new_text);

        CodeActionBuilder::new("Convert to case")
            .kind(CodeActionKind::QUICKFIX)
            .changes(uri.clone(), text_edits.edits)
            .preferred(true)
            .push_to(actions);
    }
}

struct CaseClause<'a> {
    pattern: &'a str,
    expression: &'a str,
}

fn print_case_expression(
    indent_size: usize,
    subject: &str,
    clauses: Vec<CaseClause<'_>>,
    buffer: &mut String,
) {
    let indent = " ".repeat(indent_size);

    // Print the beginning of the expression
    buffer.push_str("case ");
    buffer.push_str(subject);
    buffer.push_str(" {");

    for clause in clauses.iter() {
        // Print the newline and indentation for this clause
        buffer.push('\n');
        buffer.push_str(&indent);
        // Indent this clause one level deeper than the case expression
        buffer.push_str("  ");

        // Print the clause
        buffer.push_str(clause.pattern);
        buffer.push_str(" -> ");
        buffer.push_str(clause.expression);
    }

    // If there are no clauses to print, the closing brace should be
    // on the same line as the opening one, with no space between.
    if !clauses.is_empty() {
        buffer.push('\n');
        buffer.push_str(&indent);
    }
    buffer.push('}');
}

/// Builder for code action to apply the label shorthand syntax on arguments
/// where the label has the same name as the variable.
///
pub struct UseLabelShorthandSyntax<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
}

impl<'a> UseLabelShorthandSyntax<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);
        if self.edits.edits.is_empty() {
            return vec![];
        }
        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Use label shorthand syntax")
            .kind(CodeActionKind::REFACTOR)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for UseLabelShorthandSyntax<'_> {
    fn visit_typed_call_arg(&mut self, arg: &'ast TypedCallArg) {
        let arg_range = self.edits.src_span_to_lsp_range(arg.location);
        let is_selected = overlaps(arg_range, self.params.range);

        match arg {
            CallArg {
                label: Some(label),
                value: TypedExpr::Var { name, location, .. },
                ..
            } if is_selected && !arg.uses_label_shorthand() && label == name => {
                self.edits.delete(*location)
            }
            _ => (),
        }

        ast::visit::visit_typed_call_arg(self, arg)
    }

    fn visit_typed_pattern_call_arg(&mut self, arg: &'ast CallArg<TypedPattern>) {
        let arg_range = self.edits.src_span_to_lsp_range(arg.location);
        let is_selected = overlaps(arg_range, self.params.range);

        match arg {
            CallArg {
                label: Some(label),
                value: TypedPattern::Variable { name, location, .. },
                ..
            } if is_selected && !arg.uses_label_shorthand() && label == name => {
                self.edits.delete(*location)
            }
            _ => (),
        }

        ast::visit::visit_typed_pattern_call_arg(self, arg)
    }
}

/// Builder for code action to apply the fill in the missing labelled arguments
/// of the selected function call.
///
pub struct FillInMissingLabelledArgs<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    use_right_hand_side_location: Option<SrcSpan>,
    selected_call: Option<SelectedCall<'a>>,
    current_function: Option<&'a TypedFunction>,
}

struct SelectedCall<'a> {
    location: SrcSpan,
    field_map: &'a FieldMap,
    arguments: Vec<CallArg<()>>,
    kind: SelectedCallKind,
    fun_type: Option<Arc<Type>>,
    enclosing_function: Option<&'a TypedFunction>,
}

enum SelectedCallKind {
    Value,
    Pattern,
}

impl<'a> FillInMissingLabelledArgs<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            use_right_hand_side_location: None,
            selected_call: None,
            current_function: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        if let Some(SelectedCall {
            location: call_location,
            field_map,
            arguments,
            kind,
            fun_type,
            enclosing_function,
        }) = self.selected_call
        {
            let is_use_call = arguments.iter().any(|arg| arg.is_use_implicit_callback());
            let missing_labels = field_map.missing_labels(&arguments);

            // If we're applying the code action to a use call, then we know
            // that the last missing argument is going to be implicitly inserted
            // by the compiler, so in that case we don't want to also add that
            // last label to the completions.
            let missing_labels = missing_labels.iter().peekable();
            let mut missing_labels = if is_use_call {
                missing_labels.dropping_back(1)
            } else {
                missing_labels
            };

            // If we couldn't find any missing label to insert we just return.
            if missing_labels.peek().is_none() {
                return vec![];
            }

            // A pattern could have been written with no parentheses at all!
            // So we need to check for the last character to see if parentheses
            // are there or not before filling the arguments in
            let has_parentheses = ")"
                == code_at(
                    self.module,
                    SrcSpan::new(call_location.end - 1, call_location.end),
                );
            let label_insertion_start = if has_parentheses {
                // If it ends with a parentheses we'll need to start inserting
                // right before the closing one...
                call_location.end - 1
            } else {
                // ...otherwise we just append the result
                call_location.end
            };

            // Now we need to figure out if there's a comma at the end of the
            // arguments list:
            //
            //   call(one, |)
            //             ^ Cursor here, with a comma behind
            //
            //   call(one|)
            //           ^ Cursor here, no comma behind, we'll have to add one!
            //
            let has_comma_after_last_argument =
                if let Some(last_arg) = arguments.iter().rfind(|arg| !arg.is_implicit()) {
                    self.module
                        .code
                        .get(last_arg.location.end as usize..=label_insertion_start as usize)
                        .is_some_and(|text| text.contains(','))
                } else {
                    false
                };

            let variables_in_scope = enclosing_function
                .map(|fun| {
                    ScopeVariableCollector::new(call_location.start).collect_from_function(fun)
                })
                .unwrap_or_default();

            let labels_list = missing_labels
                .map(|label| {
                    Self::format_label(label, &kind, &fun_type, field_map, &variables_in_scope)
                })
                .join(", ");

            let has_no_explicit_arguments = arguments
                .iter()
                .filter(|arg| !arg.is_implicit())
                .peekable()
                .peek()
                .is_none();

            let labels_list = if has_no_explicit_arguments || has_comma_after_last_argument {
                labels_list
            } else {
                format!(", {labels_list}")
            };

            let edit = if has_parentheses {
                labels_list
            } else {
                // If the variant whose arguments we're filling in was written
                // with no parentheses we need to add those as well to make it a
                // valid constructor.
                format!("({labels_list})")
            };

            self.edits.insert(label_insertion_start, edit);

            let mut action = Vec::with_capacity(1);
            CodeActionBuilder::new("Fill labels")
                .kind(CodeActionKind::QUICKFIX)
                .changes(self.params.text_document.uri.clone(), self.edits.edits)
                .preferred(true)
                .push_to(&mut action);
            return action;
        }

        vec![]
    }

    /// Formats a label for insertion. Uses `label:` syntax if there's a variable
    /// in scope with matching name and type, otherwise uses `label: todo`.
    fn format_label(
        label: &EcoString,
        kind: &SelectedCallKind,
        fun_type: &Option<Arc<Type>>,
        field_map: &FieldMap,
        variables_in_scope: &HashMap<EcoString, Arc<Type>>,
    ) -> String {
        if matches!(kind, SelectedCallKind::Pattern) {
            return format!("{label}:");
        }

        if let Some(var_type) = variables_in_scope.get(label) {
            let expected_type = fun_type.as_ref().and_then(|ft| {
                let (arg_types, _) = ft.fn_types()?;
                let &index = field_map.fields.get(label)?;
                arg_types.get(index as usize).cloned()
            });

            if let Some(expected) = expected_type
                && expected.same_as(var_type)
            {
                return format!("{label}:");
            }
        }

        format!("{label}: todo")
    }

    fn empty_argument<A>(argument: &CallArg<A>) -> CallArg<()> {
        CallArg {
            label: argument.label.clone(),
            location: argument.location,
            value: (),
            implicit: argument.implicit,
        }
    }
}

impl<'ast> ast::visit::Visit<'ast> for FillInMissingLabelledArgs<'ast> {
    fn visit_typed_function(&mut self, fun: &'ast TypedFunction) {
        // We store the current function as the containing function while we traverse
        // it, allowing handling nested functions correctly.
        let previous_function = self.current_function;
        self.current_function = Some(fun);
        ast::visit::visit_typed_function(self, fun);
        self.current_function = previous_function;
    }

    fn visit_typed_use(&mut self, use_: &'ast TypedUse) {
        // If we're adding labels to a use call the correct location of the
        // function we need to add labels to is `use_right_hand_side_location`.
        // So we store it for when we're typing the use call.
        let previous = self.use_right_hand_side_location;
        self.use_right_hand_side_location = Some(use_.right_hand_side_location);
        ast::visit::visit_typed_use(self, use_);
        self.use_right_hand_side_location = previous;
    }

    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        arguments: &'ast [TypedCallArg],
    ) {
        let call_range = self.edits.src_span_to_lsp_range(*location);
        if !within(self.params.range, call_range) {
            return;
        }

        if let Some(field_map) = fun.field_map() {
            let location = self.use_right_hand_side_location.unwrap_or(*location);
            self.selected_call = Some(SelectedCall {
                location,
                field_map,
                arguments: arguments.iter().map(Self::empty_argument).collect(),
                kind: SelectedCallKind::Value,
                fun_type: Some(fun.type_()),
                enclosing_function: self.current_function,
            })
        }

        // We only want to take into account the innermost function call
        // containing the current selection so we can't stop at the first call
        // we find (the outermost one) and have to keep traversing it in case
        // we're inside a nested call.
        let previous = self.use_right_hand_side_location;
        self.use_right_hand_side_location = None;
        ast::visit::visit_typed_expr_call(self, location, type_, fun, arguments);
        self.use_right_hand_side_location = previous;
    }

    fn visit_typed_pattern_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name_location: &'ast SrcSpan,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<TypedPattern>>,
        module: &'ast Option<(EcoString, SrcSpan)>,
        constructor: &'ast Inferred<type_::PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        let call_range = self.edits.src_span_to_lsp_range(*location);
        if !within(self.params.range, call_range) {
            return;
        }

        if let Some(field_map) = constructor.field_map() {
            self.selected_call = Some(SelectedCall {
                location: *location,
                field_map,
                arguments: arguments.iter().map(Self::empty_argument).collect(),
                kind: SelectedCallKind::Pattern,
                fun_type: None,
                enclosing_function: self.current_function,
            })
        }

        ast::visit::visit_typed_pattern_constructor(
            self,
            location,
            name_location,
            name,
            arguments,
            module,
            constructor,
            spread,
            type_,
        );
    }
}

/// Collects variables that are in scope at a given cursor position.
struct ScopeVariableCollector {
    cursor: u32,
    variables: HashMap<EcoString, Arc<Type>>,
}

impl ScopeVariableCollector {
    fn new(cursor: u32) -> Self {
        Self {
            cursor,
            variables: HashMap::new(),
        }
    }

    fn collect_from_function(mut self, fun: &TypedFunction) -> HashMap<EcoString, Arc<Type>> {
        for arg in &fun.arguments {
            if let Some(name) = arg.get_variable_name() {
                _ = self.variables.insert(name.clone(), arg.type_.clone());
            }
        }

        for statement in &fun.body {
            self.visit_typed_statement(statement);
        }

        self.variables
    }
}

impl<'ast> ast::visit::Visit<'ast> for ScopeVariableCollector {
    fn visit_typed_statement(&mut self, statement: &'ast TypedStatement) {
        // We need to consider variables defined only before the cursor
        if statement.location().start >= self.cursor {
            return;
        }

        ast::visit::visit_typed_statement(self, statement);
    }

    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        // if cursor is inside the assignment, we only visit the value expression,
        // because the variable being defined isn't in scope yet.
        if assignment.location.contains(self.cursor) {
            self.visit_typed_expr(&assignment.value);
        } else {
            self.visit_typed_pattern(&assignment.pattern);
        }
    }

    fn visit_typed_expr_fn(
        &mut self,
        _location: &'ast SrcSpan,
        _type_: &'ast Arc<Type>,
        _kind: &'ast FunctionLiteralKind,
        _arguments: &'ast [TypedArg],
        _body: &'ast Vec1<TypedStatement>,
        _return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        // We don't descend into nested functions, as their variables aren't in
        // our scope
    }

    fn visit_typed_expr_block(
        &mut self,
        location: &'ast SrcSpan,
        statements: &'ast [TypedStatement],
    ) {
        if self.cursor >= location.end {
            return;
        }
        ast::visit::visit_typed_expr_block(self, location, statements);
    }

    fn visit_typed_pattern_variable(
        &mut self,
        _location: &'ast SrcSpan,
        name: &'ast EcoString,
        type_: &'ast Arc<Type>,
        _origin: &'ast VariableOrigin,
    ) {
        if !name.starts_with('_') {
            _ = self.variables.insert(name.clone(), type_.clone());
        }
    }

    fn visit_typed_pattern_assign(
        &mut self,
        _location: &'ast SrcSpan,
        name: &'ast EcoString,
        pattern: &'ast Pattern<Arc<Type>>,
    ) {
        self.visit_typed_pattern(pattern);
        if !name.starts_with('_') {
            _ = self.variables.insert(name.clone(), pattern.type_().clone());
        }
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
    line_numbers: &LineNumbers,
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
        .filter_map(|e| {
            if let type_::Error::UnknownModule {
                location,
                suggestions,
                ..
            } = e
            {
                suggest_imports(*location, suggestions)
            } else {
                None
            }
        })
        .collect_vec();

    if missing_imports.is_empty() {
        return;
    }

    let first_import_pos = position_of_first_definition_if_import(module, line_numbers);
    let first_is_import = first_import_pos.is_some();
    let import_location = first_import_pos.unwrap_or_default();

    let after_import_newlines =
        add_newlines_after_import(import_location, first_is_import, line_numbers, &module.code);

    for missing_import in missing_imports {
        let range = src_span_to_lsp_range(missing_import.location, line_numbers);
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

            let title = match &suggestion.import {
                Some(import) => &format!("Import `{import}`"),
                _ => &format!("Did you mean `{}`", suggestion.name),
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
    line_numbers: &LineNumbers,
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
        .filter_map(|error| {
            if let type_::Error::InexhaustiveCaseExpression { location, missing } = error {
                Some((*location, missing))
            } else {
                None
            }
        })
        .collect_vec();

    if missing_patterns.is_empty() {
        return;
    }

    for (location, missing) in missing_patterns {
        let mut edits = TextEdits::new(line_numbers);
        let range = edits.src_span_to_lsp_range(location);
        if !overlaps(params.range, range) {
            return;
        }

        let Some(Located::Expression {
            expression: TypedExpr::Case {
                clauses, subjects, ..
            },
            ..
        }) = module.find_node(location.start)
        else {
            continue;
        };

        let indent_size = count_indentation(&module.code, edits.line_numbers, range.start.line);

        let indent = " ".repeat(indent_size);

        // Insert the missing patterns just after the final clause, or just before
        // the closing brace if there are no clauses

        let insert_at = clauses
            .last()
            .map(|clause| clause.location.end)
            .unwrap_or(location.end - 1);

        for pattern in missing {
            let new_text = format!("\n{indent}  {pattern} -> todo");
            edits.insert(insert_at, new_text);
        }

        // Add a newline + indent after the last pattern if there are no clauses
        //
        // This improves the generated code for this case:
        // ```gleam
        // case True {}
        // ```
        // This produces:
        // ```gleam
        // case True {
        //   True -> todo
        //   False -> todo
        // }
        // ```
        // Instead of:
        // ```gleam
        // case True {
        //   True -> todo
        //   False -> todo}
        // ```
        //
        if clauses.is_empty() {
            let last_subject_location = subjects
                .last()
                .expect("Case expressions have at least one subject")
                .location()
                .end;

            // Find the opening brace of the case expression
            let chars = module.code[last_subject_location as usize..].chars();
            let mut start_brace_location = last_subject_location;
            for char in chars {
                start_brace_location += 1;
                if char == '{' {
                    break;
                }
            }

            // Remove any blank spaces/lines between the start brace and end brace
            edits.delete(SrcSpan::new(start_brace_location, insert_at));
            edits.insert(insert_at, format!("\n{indent}"));
        }

        CodeActionBuilder::new("Add missing patterns")
            .kind(CodeActionKind::QUICKFIX)
            .changes(uri.clone(), edits.edits)
            .preferred(true)
            .push_to(actions);
    }
}

/// Builder for code action to add annotations to an assignment or function
///
pub struct AddAnnotations<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    printer: Printer<'a>,
}

impl<'ast> ast::visit::Visit<'ast> for AddAnnotations<'_> {
    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        self.visit_typed_expr(&assignment.value);

        // We only offer this code action between `let` and `=`, because
        // otherwise it could lead to confusing behaviour if inside a block
        // which is part of a let binding.
        let pattern_location = assignment.pattern.location();
        let location = SrcSpan::new(assignment.location.start, pattern_location.end);
        let code_action_range = self.edits.src_span_to_lsp_range(location);

        // Only offer the code action if the cursor is over the statement
        if !overlaps(code_action_range, self.params.range) {
            return;
        }

        // We don't need to add an annotation if there already is one
        if assignment.annotation.is_some() {
            return;
        }

        // Various expressions such as pipelines and `use` expressions generate assignments
        // internally. However, these cannot be annotated and so we don't offer a code action here.
        if matches!(assignment.kind, AssignmentKind::Generated) {
            return;
        }

        self.edits.insert(
            pattern_location.end,
            format!(": {}", self.printer.print_type(&assignment.type_())),
        );
    }

    fn visit_typed_module_constant(&mut self, constant: &'ast TypedModuleConstant) {
        // Since type variable names are local to definitions, any type variables
        // in other parts of the module shouldn't affect what we print for the
        // annotations of this constant.
        self.printer.clear_type_variables();

        let code_action_range = self.edits.src_span_to_lsp_range(constant.location);

        // Only offer the code action if the cursor is over the statement
        if !overlaps(code_action_range, self.params.range) {
            return;
        }

        // We don't need to add an annotation if there already is one
        if constant.annotation.is_some() {
            return;
        }

        self.edits.insert(
            constant.name_location.end,
            format!(": {}", self.printer.print_type(&constant.type_)),
        );
    }

    fn visit_typed_function(&mut self, fun: &'ast TypedFunction) {
        // Since type variable names are local to definitions, any type variables
        // in other parts of the module shouldn't affect what we print for the
        // annotations of this functions. The only variables which cannot clash
        // are ones defined in the signature of this function, which we register
        // when we visit the parameters of this function inside `collect_type_variables`.
        self.printer.clear_type_variables();
        collect_type_variables(&mut self.printer, fun);

        ast::visit::visit_typed_function(self, fun);

        let code_action_range = self.edits.src_span_to_lsp_range(
            fun.body_start
                .map(|body_start| SrcSpan {
                    start: fun.location.start,
                    end: body_start,
                })
                .unwrap_or(fun.location),
        );

        // Only offer the code action if the cursor is over the statement
        if !overlaps(code_action_range, self.params.range) {
            return;
        }

        // Annotate each argument separately
        for argument in fun.arguments.iter() {
            // Don't annotate the argument if it's already annotated
            if argument.annotation.is_some() {
                continue;
            }

            self.edits.insert(
                argument.location.end,
                format!(": {}", self.printer.print_type(&argument.type_)),
            );
        }

        // Annotate the return type if it isn't already annotated
        if fun.return_annotation.is_none() {
            self.edits.insert(
                fun.location.end,
                format!(" -> {}", self.printer.print_type(&fun.return_type)),
            );
        }
    }

    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        arguments: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        ast::visit::visit_typed_expr_fn(
            self,
            location,
            type_,
            kind,
            arguments,
            body,
            return_annotation,
        );

        // If the function doesn't have a head, we can't annotate it
        let location = match kind {
            // Function captures don't need any type annotations
            FunctionLiteralKind::Capture { .. } => return,
            FunctionLiteralKind::Anonymous { head } => head,
            FunctionLiteralKind::Use { location } => location,
        };

        let code_action_range = self.edits.src_span_to_lsp_range(*location);

        // Only offer the code action if the cursor is over the expression
        if !overlaps(code_action_range, self.params.range) {
            return;
        }

        // Annotate each argument separately
        for argument in arguments.iter() {
            // Don't annotate the argument if it's already annotated
            if argument.annotation.is_some() {
                continue;
            }

            self.edits.insert(
                argument.location.end,
                format!(": {}", self.printer.print_type(&argument.type_)),
            );
        }

        // Annotate the return type if it isn't already annotated, and this is
        // an anonymous function.
        if return_annotation.is_none() && matches!(kind, FunctionLiteralKind::Anonymous { .. }) {
            let return_type = &type_.return_type().expect("Type must be a function");
            let pretty_type = self.printer.print_type(return_type);
            self.edits
                .insert(location.end, format!(" -> {pretty_type}"));
        }
    }
}

impl<'a> AddAnnotations<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            // We need to use the same printer for all the edits because otherwise
            // we could get duplicate type variable names.
            printer: Printer::new_without_type_variables(&module.ast.names),
        }
    }

    pub fn code_action(mut self, actions: &mut Vec<CodeAction>) {
        self.visit_typed_module(&self.module.ast);

        let uri = &self.params.text_document.uri;

        let title = match self.edits.edits.len() {
            // We don't offer a code action if there is no action to perform
            0 => return,
            1 => "Add type annotation",
            _ => "Add type annotations",
        };

        CodeActionBuilder::new(title)
            .kind(CodeActionKind::REFACTOR)
            .changes(uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(actions);
    }
}

/// Code action to add type annotations to all top level definitions
///
pub struct AnnotateTopLevelDefinitions<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    is_hovering_definition_requiring_annotations: bool,
}

impl<'a> AnnotateTopLevelDefinitions<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            is_hovering_definition_requiring_annotations: false,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        // We only want to trigger the action if we're over one of the definition
        // which is lacking some annotations in the module
        if !self.is_hovering_definition_requiring_annotations {
            return vec![];
        };

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Annotate all top level definitions")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for AnnotateTopLevelDefinitions<'_> {
    fn visit_typed_module_constant(&mut self, constant: &'ast TypedModuleConstant) {
        let code_action_range = self.edits.src_span_to_lsp_range(constant.location);

        // We don't need to add an annotation if there already is one
        if constant.annotation.is_some() {
            return;
        }

        // We're hovering definition which needs some annotations
        if overlaps(code_action_range, self.params.range) {
            self.is_hovering_definition_requiring_annotations = true;
        }

        self.edits.insert(
            constant.name_location.end,
            format!(
                ": {}",
                // Create new printer to ignore type variables from other definitions
                Printer::new_without_type_variables(&self.module.ast.names)
                    .print_type(&constant.type_)
            ),
        );
    }

    fn visit_typed_function(&mut self, fun: &'ast TypedFunction) {
        // Don't annotate already annotated arguments
        let arguments_to_annotate = fun
            .arguments
            .iter()
            .filter(|argument| argument.annotation.is_none())
            .collect::<Vec<_>>();
        let needs_return_annotation = fun.return_annotation.is_none();

        if arguments_to_annotate.is_empty() && !needs_return_annotation {
            return;
        }

        let code_action_range = self.edits.src_span_to_lsp_range(fun.location);
        if overlaps(code_action_range, self.params.range) {
            self.is_hovering_definition_requiring_annotations = true;
        }

        // Create new printer to ignore type variables from other definitions
        let mut printer = Printer::new_without_type_variables(&self.module.ast.names);
        collect_type_variables(&mut printer, fun);

        // Annotate each argument separately
        for argument in arguments_to_annotate {
            self.edits.insert(
                argument.location.end,
                format!(": {}", printer.print_type(&argument.type_)),
            );
        }

        // Annotate the return type if it isn't already annotated
        if needs_return_annotation {
            self.edits.insert(
                fun.location.end,
                format!(" -> {}", printer.print_type(&fun.return_type)),
            );
        }
    }
}

struct TypeVariableCollector<'a, 'b> {
    printer: &'a mut Printer<'b>,
}

/// Collect type variables defined within a function and register them for a
/// `Printer`
fn collect_type_variables(printer: &mut Printer<'_>, function: &TypedFunction) {
    TypeVariableCollector { printer }.visit_typed_function(function);
}

impl<'ast, 'a, 'b> ast::visit::Visit<'ast> for TypeVariableCollector<'a, 'b> {
    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        for argument in fun.arguments.iter() {
            if let Some(annotation) = &argument.annotation {
                register_type_variables_from_annotation(
                    self.printer,
                    annotation,
                    argument.type_.as_ref(),
                );
            }
        }

        if let Some(annotation) = &fun.return_annotation {
            register_type_variables_from_annotation(
                self.printer,
                annotation,
                fun.return_type.as_ref(),
            );
        }

        ast::visit::visit_typed_function(self, fun);
    }

    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        arguments: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        if let Type::Fn {
            arguments: argument_types,
            return_: return_type,
            ..
        } = type_.as_ref()
        {
            for (argument, argument_type) in arguments.iter().zip(argument_types) {
                if let Some(annotation) = &argument.annotation {
                    register_type_variables_from_annotation(
                        self.printer,
                        annotation,
                        argument_type.as_ref(),
                    );
                }
            }

            if let Some(annotation) = return_annotation {
                register_type_variables_from_annotation(
                    self.printer,
                    annotation,
                    return_type.as_ref(),
                );
            }
        }

        ast::visit::visit_typed_expr_fn(
            self,
            location,
            type_,
            kind,
            arguments,
            body,
            return_annotation,
        );
    }

    fn visit_type_ast_var(&mut self, _location: &'ast SrcSpan, name: &'ast EcoString) {
        // Register this type variable so that we don't duplicate names when
        // adding annotations.
        self.printer.register_type_variable(name.clone());
    }
}

fn register_type_variables_from_annotation(
    printer: &mut Printer<'_>,
    annotation: &ast::TypeAst,
    type_: &Type,
) {
    // fn wibble(a, b, c) {
    //   fn(a: b, b: c) -> d { ... }
    //                     ^
    // Without this tracking the printer could rename `d` to a fresh `h`.
    match (annotation, type_) {
        (ast::TypeAst::Var(ast::TypeAstVar { name, .. }), Type::Var { type_ }) => {
            match &*type_.borrow() {
                TypeVar::Generic { id } | TypeVar::Unbound { id } => {
                    let id = *id;
                    printer.register_type_variable(name.clone());
                    printer.register_type_variable_with_id(id, name.clone());
                }
                TypeVar::Link { type_ } => {
                    register_type_variables_from_annotation(printer, annotation, type_.as_ref());
                }
            }
        }

        (
            ast::TypeAst::Fn(ast::TypeAstFn {
                arguments: annotation_arguments,
                return_: annotation_return,
                ..
            }),
            Type::Fn {
                arguments: type_arguments,
                return_: type_return,
                ..
            },
        ) => {
            for (argument_annotation, argument_type) in
                annotation_arguments.iter().zip(type_arguments)
            {
                // Maintain the names from each `fn(arg: name, ...)` position.
                register_type_variables_from_annotation(
                    printer,
                    argument_annotation,
                    argument_type.as_ref(),
                );
            }

            // And likewise propagate the annotated return variable.
            register_type_variables_from_annotation(
                printer,
                annotation_return.as_ref(),
                type_return.as_ref(),
            );
        }

        (
            ast::TypeAst::Constructor(ast::TypeAstConstructor {
                arguments: annotation_arguments,
                ..
            }),
            Type::Named {
                arguments: type_arguments,
                ..
            },
        ) => {
            for (argument_annotation, argument_type) in
                annotation_arguments.iter().zip(type_arguments)
            {
                // Track aliases introduced inside named type arguments.
                register_type_variables_from_annotation(
                    printer,
                    argument_annotation,
                    argument_type.as_ref(),
                );
            }
        }

        (
            ast::TypeAst::Tuple(ast::TypeAstTuple {
                elements: annotation_elements,
                ..
            }),
            Type::Tuple {
                elements: type_elements,
                ..
            },
        ) => {
            for (element_annotation, element_type) in annotation_elements.iter().zip(type_elements)
            {
                // Tuples can hide extra annotations; ensure each slot retains its label.
                register_type_variables_from_annotation(
                    printer,
                    element_annotation,
                    element_type.as_ref(),
                );
            }
        }

        (_, Type::Var { type_ }) => {
            if let TypeVar::Link { type_ } = &*type_.borrow() {
                register_type_variables_from_annotation(printer, annotation, type_.as_ref());
            }
        }

        _ => {}
    }
}

pub struct QualifiedConstructor<'a> {
    import: &'a Import<EcoString>,
    used_name: EcoString,
    constructor: EcoString,
    layer: ast::Layer,
}

impl QualifiedConstructor<'_> {
    fn constructor_import(&self) -> String {
        if self.layer.is_value() {
            self.constructor.to_string()
        } else {
            format!("type {}", self.constructor)
        }
    }
}

pub struct QualifiedToUnqualifiedImportFirstPass<'a, IO> {
    module: &'a Module,
    compiler: &'a LspProjectCompiler<FileSystemProxy<IO>>,
    params: &'a CodeActionParams,
    line_numbers: &'a LineNumbers,
    qualified_constructor: Option<QualifiedConstructor<'a>>,
}

impl<'a, IO> QualifiedToUnqualifiedImportFirstPass<'a, IO> {
    fn new(
        module: &'a Module,
        compiler: &'a LspProjectCompiler<FileSystemProxy<IO>>,
        params: &'a CodeActionParams,
        line_numbers: &'a LineNumbers,
    ) -> Self {
        Self {
            module,
            compiler,
            params,
            line_numbers,
            qualified_constructor: None,
        }
    }

    fn get_module_import(
        &self,
        module_name: &EcoString,
        constructor: &EcoString,
        layer: ast::Layer,
    ) -> Option<&'a Import<EcoString>> {
        let mut matching_import = None;

        for import in &self.module.ast.definitions.imports {
            if import.used_name().as_deref() == Some(module_name)
                && let Some(module) = self.compiler.get_module_interface(&import.module)
            {
                // If the import is the one we're referring to, we see if the
                // referred module exports the type/value we are trying to
                // unqualify: we don't want to offer the action indiscriminately if
                // it would generate invalid code!
                let module_exports_constructor = match layer {
                    ast::Layer::Value => module.get_public_value(constructor).is_some(),
                    ast::Layer::Type => module.get_public_type(constructor).is_some(),
                };
                if module_exports_constructor {
                    matching_import = Some(import);
                }
            } else {
                // If the import refers to another module we still want to check
                // if in its unqualified import list there is a name that's equal
                // to the one we're trying to unqualify. In this case we can't
                // offer the action as it would generate invalid code.
                //
                // For example:
                // ```gleam
                // import wibble.{Some}
                // import option
                //
                // pub fn something() {
                //   option.Some(1)
                //          ^^^^ We can't unqualify this because `Some` is already
                //               imported unqualified from the `wibble` module
                // }
                // ```
                //
                let imported = match layer {
                    ast::Layer::Value => &import.unqualified_values,
                    ast::Layer::Type => &import.unqualified_types,
                };
                let constructor_already_imported_by_other_module = imported
                    .iter()
                    .any(|value| value.used_name() == constructor);

                if constructor_already_imported_by_other_module {
                    return None;
                }
            }
        }

        matching_import
    }
}

impl<'ast, IO> ast::visit::Visit<'ast> for QualifiedToUnqualifiedImportFirstPass<'ast, IO> {
    fn visit_type_ast_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name_location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        arguments: &'ast Vec<ast::TypeAst>,
    ) {
        let range = src_span_to_lsp_range(*location, self.line_numbers);
        if overlaps(self.params.range, range)
            && let Some((module_alias, _)) = module
            && let Some(import) = self.get_module_import(module_alias, name, ast::Layer::Type)
        {
            self.qualified_constructor = Some(QualifiedConstructor {
                import,
                used_name: module_alias.clone(),
                constructor: name.clone(),
                layer: ast::Layer::Type,
            });
        }
        ast::visit::visit_type_ast_constructor(
            self,
            location,
            name_location,
            module,
            name,
            arguments,
        );
    }

    fn visit_typed_expr_module_select(
        &mut self,
        location: &'ast SrcSpan,
        field_start: &'ast u32,
        type_: &'ast Arc<Type>,
        label: &'ast EcoString,
        module_name: &'ast EcoString,
        module_alias: &'ast EcoString,
        constructor: &'ast ModuleValueConstructor,
    ) {
        // When hovering over a Record Value Constructor, we want to expand the source span to
        // include the module name:
        // option.Some
        //  ↑
        // This allows us to offer a code action when hovering over the module name.
        let range = src_span_to_lsp_range(*location, self.line_numbers);
        if overlaps(self.params.range, range)
            && let ModuleValueConstructor::Record {
                name: constructor_name,
                ..
            } = constructor
            && let Some(import) =
                self.get_module_import(module_alias, constructor_name, ast::Layer::Value)
        {
            self.qualified_constructor = Some(QualifiedConstructor {
                import,
                used_name: module_alias.clone(),
                constructor: constructor_name.clone(),
                layer: ast::Layer::Value,
            });
        }
        ast::visit::visit_typed_expr_module_select(
            self,
            location,
            field_start,
            type_,
            label,
            module_name,
            module_alias,
            constructor,
        )
    }

    fn visit_typed_pattern_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name_location: &'ast SrcSpan,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<TypedPattern>>,
        module: &'ast Option<(EcoString, SrcSpan)>,
        constructor: &'ast Inferred<type_::PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        let range = src_span_to_lsp_range(*location, self.line_numbers);
        if overlaps(self.params.range, range)
            && let Some((module_alias, _)) = module
            && let Inferred::Known(_) = constructor
            && let Some(import) = self.get_module_import(module_alias, name, ast::Layer::Value)
        {
            self.qualified_constructor = Some(QualifiedConstructor {
                import,
                used_name: module_alias.clone(),
                constructor: name.clone(),
                layer: ast::Layer::Value,
            });
        }
        ast::visit::visit_typed_pattern_constructor(
            self,
            location,
            name_location,
            name,
            arguments,
            module,
            constructor,
            spread,
            type_,
        );
    }

    fn visit_typed_constant_record(
        &mut self,
        location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<ast::TypedConstant>>,
        tag: &'ast EcoString,
        type_: &'ast Arc<Type>,
        field_map: &'ast Inferred<FieldMap>,
        record_constructor: &'ast Option<Box<ValueConstructor>>,
    ) {
        let range = src_span_to_lsp_range(*location, self.line_numbers);
        if overlaps(self.params.range, range)
            && let Some((module_alias, _)) = module
            && let Some(import) = self.get_module_import(module_alias, name, ast::Layer::Value)
        {
            self.qualified_constructor = Some(QualifiedConstructor {
                import,
                used_name: module_alias.clone(),
                constructor: name.clone(),
                layer: ast::Layer::Value,
            });
        }
        ast::visit::visit_typed_constant_record(
            self,
            location,
            module,
            name,
            arguments,
            tag,
            type_,
            field_map,
            record_constructor,
        );
    }

    fn visit_typed_constant_var(
        &mut self,
        location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        constructor: &'ast Option<Box<ValueConstructor>>,
        type_: &'ast Arc<Type>,
    ) {
        let range = src_span_to_lsp_range(*location, self.line_numbers);
        if overlaps(self.params.range, range)
            && let Some((module_alias, _)) = module
            && let Some(constructor) = constructor
            && let type_::ValueConstructorVariant::Record { .. } = &constructor.variant
            && let Some(import) = self.get_module_import(module_alias, name, ast::Layer::Value)
        {
            self.qualified_constructor = Some(QualifiedConstructor {
                import,
                used_name: module_alias.clone(),
                constructor: name.clone(),
                layer: ast::Layer::Value,
            });
        }
        ast::visit::visit_typed_constant_var(self, location, module, name, constructor, type_);
    }
}

pub struct QualifiedToUnqualifiedImportSecondPass<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    qualified_constructor: QualifiedConstructor<'a>,
}

impl<'a> QualifiedToUnqualifiedImportSecondPass<'a> {
    pub fn new(
        module: &'a Module,
        params: &'a CodeActionParams,
        line_numbers: &'a LineNumbers,
        qualified_constructor: QualifiedConstructor<'a>,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            qualified_constructor,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);
        if self.edits.edits.is_empty() {
            return vec![];
        }
        self.edit_import();
        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new(&format!(
            "Unqualify {}.{}",
            self.qualified_constructor.used_name, self.qualified_constructor.constructor
        ))
        .kind(CodeActionKind::REFACTOR)
        .changes(self.params.text_document.uri.clone(), self.edits.edits)
        .preferred(false)
        .push_to(&mut action);
        action
    }

    fn remove_module_qualifier(&mut self, location: SrcSpan) {
        self.edits.delete(SrcSpan {
            start: location.start,
            end: location.start + self.qualified_constructor.used_name.len() as u32 + 1, // plus .
        })
    }

    fn edit_import(&mut self) {
        let QualifiedConstructor {
            constructor,
            layer,
            import,
            ..
        } = &self.qualified_constructor;
        let is_imported = if layer.is_value() {
            import
                .unqualified_values
                .iter()
                .any(|value| value.used_name() == constructor)
        } else {
            import
                .unqualified_types
                .iter()
                .any(|type_| type_.used_name() == constructor)
        };
        if is_imported {
            return;
        }
        let (insert_pos, new_text) = edits::insert_unqualified_import(
            import,
            &self.module.code,
            self.qualified_constructor.constructor_import(),
        );
        let span = SrcSpan::new(insert_pos, insert_pos);
        self.edits.replace(span, new_text);
    }
}

impl<'ast> ast::visit::Visit<'ast> for QualifiedToUnqualifiedImportSecondPass<'ast> {
    fn visit_type_ast_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name_location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        arguments: &'ast Vec<ast::TypeAst>,
    ) {
        if let Some((module_name, _)) = module {
            let QualifiedConstructor {
                used_name,
                constructor,
                layer,
                ..
            } = &self.qualified_constructor;

            if !layer.is_value() && used_name == module_name && name == constructor {
                self.remove_module_qualifier(*location);
            }
        }
        ast::visit::visit_type_ast_constructor(
            self,
            location,
            name_location,
            module,
            name,
            arguments,
        );
    }

    fn visit_typed_expr_module_select(
        &mut self,
        location: &'ast SrcSpan,
        field_start: &'ast u32,
        type_: &'ast Arc<Type>,
        label: &'ast EcoString,
        module_name: &'ast EcoString,
        module_alias: &'ast EcoString,
        constructor: &'ast ModuleValueConstructor,
    ) {
        if let ModuleValueConstructor::Record { name, .. } = constructor {
            let QualifiedConstructor {
                used_name,
                constructor,
                layer,
                ..
            } = &self.qualified_constructor;

            if layer.is_value() && used_name == module_alias && name == constructor {
                self.remove_module_qualifier(*location);
            }
        }
        ast::visit::visit_typed_expr_module_select(
            self,
            location,
            field_start,
            type_,
            label,
            module_name,
            module_alias,
            constructor,
        )
    }

    fn visit_typed_pattern_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name_location: &'ast SrcSpan,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<TypedPattern>>,
        module: &'ast Option<(EcoString, SrcSpan)>,
        constructor: &'ast Inferred<type_::PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        if let Some((module_alias, _)) = module
            && let Inferred::Known(_) = constructor
        {
            let QualifiedConstructor {
                used_name,
                constructor,
                layer,
                ..
            } = &self.qualified_constructor;

            if layer.is_value() && used_name == module_alias && name == constructor {
                self.remove_module_qualifier(*location);
            }
        }
        ast::visit::visit_typed_pattern_constructor(
            self,
            location,
            name_location,
            name,
            arguments,
            module,
            constructor,
            spread,
            type_,
        );
    }

    fn visit_typed_constant_record(
        &mut self,
        location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<ast::TypedConstant>>,
        tag: &'ast EcoString,
        type_: &'ast Arc<Type>,
        field_map: &'ast Inferred<FieldMap>,
        record_constructor: &'ast Option<Box<ValueConstructor>>,
    ) {
        if let Some((module_alias, _)) = module {
            let QualifiedConstructor {
                used_name,
                constructor,
                layer,
                ..
            } = &self.qualified_constructor;

            if layer.is_value() && used_name == module_alias && name == constructor {
                self.remove_module_qualifier(*location);
            }
        }
        ast::visit::visit_typed_constant_record(
            self,
            location,
            module,
            name,
            arguments,
            tag,
            type_,
            field_map,
            record_constructor,
        );
    }

    fn visit_typed_constant_var(
        &mut self,
        location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        constructor: &'ast Option<Box<ValueConstructor>>,
        type_: &'ast Arc<Type>,
    ) {
        if let Some((module_alias, _)) = module {
            let QualifiedConstructor {
                used_name,
                constructor: wanted_constructor,
                layer,
                ..
            } = &self.qualified_constructor;

            if layer.is_value() && used_name == module_alias && name == wanted_constructor {
                self.remove_module_qualifier(*location);
            }
        }
        ast::visit::visit_typed_constant_var(self, location, module, name, constructor, type_);
    }
}

pub fn code_action_convert_qualified_constructor_to_unqualified<IO>(
    module: &Module,
    compiler: &LspProjectCompiler<FileSystemProxy<IO>>,
    line_numbers: &LineNumbers,
    params: &CodeActionParams,
    actions: &mut Vec<CodeAction>,
) {
    let mut first_pass =
        QualifiedToUnqualifiedImportFirstPass::new(module, compiler, params, line_numbers);
    first_pass.visit_typed_module(&module.ast);
    let Some(qualified_constructor) = first_pass.qualified_constructor else {
        return;
    };
    let second_pass = QualifiedToUnqualifiedImportSecondPass::new(
        module,
        params,
        line_numbers,
        qualified_constructor,
    );
    let new_actions = second_pass.code_actions();
    actions.extend(new_actions);
}

struct UnqualifiedConstructor<'a> {
    module_name: EcoString,
    constructor: &'a ast::UnqualifiedImport,
    layer: ast::Layer,
}

struct UnqualifiedToQualifiedImportFirstPass<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    line_numbers: &'a LineNumbers,
    unqualified_constructor: Option<UnqualifiedConstructor<'a>>,
}

impl<'a> UnqualifiedToQualifiedImportFirstPass<'a> {
    fn new(
        module: &'a Module,
        params: &'a CodeActionParams,
        line_numbers: &'a LineNumbers,
    ) -> Self {
        Self {
            module,
            params,
            line_numbers,
            unqualified_constructor: None,
        }
    }

    fn get_module_import_from_value_constructor(
        &mut self,
        module_name: &EcoString,
        constructor_name: &EcoString,
    ) {
        self.unqualified_constructor = self
            .module
            .ast
            .definitions
            .imports
            .iter()
            .filter(|import| import.module == *module_name)
            .find_map(|import| {
                import
                    .unqualified_values
                    .iter()
                    .find(|value| value.used_name() == constructor_name)
                    .and_then(|value| {
                        Some(UnqualifiedConstructor {
                            constructor: value,
                            module_name: import.used_name()?,
                            layer: ast::Layer::Value,
                        })
                    })
            })
    }

    fn get_module_import_from_type_constructor(&mut self, constructor_name: &EcoString) {
        self.unqualified_constructor =
            self.module
                .ast
                .definitions
                .imports
                .iter()
                .find_map(|import| {
                    if let Some(ty) = import
                        .unqualified_types
                        .iter()
                        .find(|ty| ty.used_name() == constructor_name)
                    {
                        return Some(UnqualifiedConstructor {
                            constructor: ty,
                            module_name: import.used_name()?,
                            layer: ast::Layer::Type,
                        });
                    }
                    None
                })
    }
}

impl<'ast> ast::visit::Visit<'ast> for UnqualifiedToQualifiedImportFirstPass<'ast> {
    fn visit_type_ast_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name_location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        arguments: &'ast Vec<ast::TypeAst>,
    ) {
        if module.is_none()
            && overlaps(
                self.params.range,
                src_span_to_lsp_range(*location, self.line_numbers),
            )
        {
            self.get_module_import_from_type_constructor(name);
        }

        ast::visit::visit_type_ast_constructor(
            self,
            location,
            name_location,
            module,
            name,
            arguments,
        );
    }

    fn visit_typed_expr_var(
        &mut self,
        location: &'ast SrcSpan,
        constructor: &'ast ValueConstructor,
        name: &'ast EcoString,
    ) {
        let range = src_span_to_lsp_range(*location, self.line_numbers);
        if overlaps(self.params.range, range)
            && let Some(module_name) = match &constructor.variant {
                type_::ValueConstructorVariant::ModuleConstant { module, .. }
                | type_::ValueConstructorVariant::ModuleFn { module, .. }
                | type_::ValueConstructorVariant::Record { module, .. } => Some(module),

                type_::ValueConstructorVariant::LocalVariable { .. } => None,
            }
        {
            self.get_module_import_from_value_constructor(module_name, name);
        }
        ast::visit::visit_typed_expr_var(self, location, constructor, name);
    }

    fn visit_typed_pattern_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name_location: &'ast SrcSpan,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<TypedPattern>>,
        module: &'ast Option<(EcoString, SrcSpan)>,
        constructor: &'ast Inferred<type_::PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        if module.is_none()
            && overlaps(
                self.params.range,
                src_span_to_lsp_range(*location, self.line_numbers),
            )
            && let Inferred::Known(constructor) = constructor
        {
            self.get_module_import_from_value_constructor(&constructor.module, name);
        }

        ast::visit::visit_typed_pattern_constructor(
            self,
            location,
            name_location,
            name,
            arguments,
            module,
            constructor,
            spread,
            type_,
        );
    }

    fn visit_typed_constant_record(
        &mut self,
        location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<ast::TypedConstant>>,
        _tag: &'ast EcoString,
        _type_: &'ast Arc<Type>,
        _field_map: &'ast Inferred<FieldMap>,
        record_constructor: &'ast Option<Box<ValueConstructor>>,
    ) {
        if module.is_none()
            && overlaps(
                self.params.range,
                src_span_to_lsp_range(*location, self.line_numbers),
            )
            && let Some(record_constructor) = record_constructor
            && let Some(module_name) = match &record_constructor.variant {
                type_::ValueConstructorVariant::ModuleConstant { module, .. }
                | type_::ValueConstructorVariant::ModuleFn { module, .. }
                | type_::ValueConstructorVariant::Record { module, .. } => Some(module),

                type_::ValueConstructorVariant::LocalVariable { .. } => None,
            }
        {
            self.get_module_import_from_value_constructor(module_name, name);
        }
        ast::visit::visit_typed_constant_record(
            self,
            location,
            module,
            name,
            arguments,
            _tag,
            _type_,
            _field_map,
            record_constructor,
        );
    }

    fn visit_typed_constant_var(
        &mut self,
        location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        constructor: &'ast Option<Box<ValueConstructor>>,
        type_: &'ast Arc<Type>,
    ) {
        if module.is_none()
            && overlaps(
                self.params.range,
                src_span_to_lsp_range(*location, self.line_numbers),
            )
            && let Some(constructor) = constructor
            && let Some(module_name) = match &constructor.variant {
                type_::ValueConstructorVariant::ModuleConstant { module, .. }
                | type_::ValueConstructorVariant::ModuleFn { module, .. }
                | type_::ValueConstructorVariant::Record { module, .. } => Some(module),

                type_::ValueConstructorVariant::LocalVariable { .. } => None,
            }
        {
            self.get_module_import_from_value_constructor(module_name, name);
        }
        ast::visit::visit_typed_constant_var(self, location, module, name, constructor, type_);
    }
}

struct UnqualifiedToQualifiedImportSecondPass<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    unqualified_constructor: UnqualifiedConstructor<'a>,
}

impl<'a> UnqualifiedToQualifiedImportSecondPass<'a> {
    pub fn new(
        module: &'a Module,
        params: &'a CodeActionParams,
        line_numbers: &'a LineNumbers,
        unqualified_constructor: UnqualifiedConstructor<'a>,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            unqualified_constructor,
        }
    }

    fn add_module_qualifier(&mut self, location: SrcSpan) {
        let src_span = SrcSpan::new(
            location.start,
            location.start + self.unqualified_constructor.constructor.used_name().len() as u32,
        );

        self.edits.replace(
            src_span,
            format!(
                "{}.{}",
                self.unqualified_constructor.module_name,
                self.unqualified_constructor.constructor.name
            ),
        );
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);
        if self.edits.edits.is_empty() {
            return vec![];
        }
        self.edit_import();
        let mut action = Vec::with_capacity(1);
        let UnqualifiedConstructor {
            module_name,
            constructor,
            ..
        } = self.unqualified_constructor;
        CodeActionBuilder::new(&format!(
            "Qualify {} as {}.{}",
            constructor.used_name(),
            module_name,
            constructor.name,
        ))
        .kind(CodeActionKind::REFACTOR)
        .changes(self.params.text_document.uri.clone(), self.edits.edits)
        .preferred(false)
        .push_to(&mut action);
        action
    }

    fn edit_import(&mut self) {
        let UnqualifiedConstructor {
            constructor:
                ast::UnqualifiedImport {
                    location: constructor_import_span,
                    ..
                },
            ..
        } = self.unqualified_constructor;

        let mut last_char_pos = constructor_import_span.end as usize;
        while self.module.code.get(last_char_pos..last_char_pos + 1) == Some(" ") {
            last_char_pos += 1;
        }
        if self.module.code.get(last_char_pos..last_char_pos + 1) == Some(",") {
            last_char_pos += 1;
        }
        if self.module.code.get(last_char_pos..last_char_pos + 1) == Some(" ") {
            last_char_pos += 1;
        }

        self.edits.delete(SrcSpan::new(
            constructor_import_span.start,
            last_char_pos as u32,
        ));
    }
}

impl<'ast> ast::visit::Visit<'ast> for UnqualifiedToQualifiedImportSecondPass<'ast> {
    fn visit_type_ast_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name_location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        arguments: &'ast Vec<ast::TypeAst>,
    ) {
        if module.is_none() {
            let UnqualifiedConstructor {
                constructor, layer, ..
            } = &self.unqualified_constructor;
            if !layer.is_value() && constructor.used_name() == name {
                self.add_module_qualifier(*location);
            }
        }
        ast::visit::visit_type_ast_constructor(
            self,
            location,
            name_location,
            module,
            name,
            arguments,
        );
    }

    fn visit_typed_expr_var(
        &mut self,
        location: &'ast SrcSpan,
        constructor: &'ast ValueConstructor,
        name: &'ast EcoString,
    ) {
        let UnqualifiedConstructor {
            constructor: wanted_constructor,
            layer,
            ..
        } = &self.unqualified_constructor;

        if layer.is_value()
            && wanted_constructor.used_name() == name
            && !constructor.is_local_variable()
        {
            self.add_module_qualifier(*location);
        }
        ast::visit::visit_typed_expr_var(self, location, constructor, name);
    }

    fn visit_typed_pattern_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name_location: &'ast SrcSpan,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<TypedPattern>>,
        module: &'ast Option<(EcoString, SrcSpan)>,
        constructor: &'ast Inferred<type_::PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        if module.is_none() {
            let UnqualifiedConstructor {
                constructor: wanted_constructor,
                layer,
                ..
            } = &self.unqualified_constructor;
            if layer.is_value() && wanted_constructor.used_name() == name {
                self.add_module_qualifier(*location);
            }
        }
        ast::visit::visit_typed_pattern_constructor(
            self,
            location,
            name_location,
            name,
            arguments,
            module,
            constructor,
            spread,
            type_,
        );
    }

    fn visit_typed_constant_record(
        &mut self,
        location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<ast::TypedConstant>>,
        tag: &'ast EcoString,
        type_: &'ast Arc<Type>,
        field_map: &'ast Inferred<FieldMap>,
        record_constructor: &'ast Option<Box<ValueConstructor>>,
    ) {
        if module.is_none() {
            let UnqualifiedConstructor {
                constructor: wanted_constructor,
                layer,
                ..
            } = &self.unqualified_constructor;
            if layer.is_value() && wanted_constructor.used_name() == name {
                self.add_module_qualifier(*location);
            }
        }
        ast::visit::visit_typed_constant_record(
            self,
            location,
            module,
            name,
            arguments,
            tag,
            type_,
            field_map,
            record_constructor,
        );
    }

    fn visit_typed_constant_var(
        &mut self,
        location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        constructor: &'ast Option<Box<ValueConstructor>>,
        type_: &'ast Arc<Type>,
    ) {
        if module.is_none() {
            let UnqualifiedConstructor {
                constructor: wanted_constructor,
                layer,
                ..
            } = &self.unqualified_constructor;
            if layer.is_value() && wanted_constructor.used_name() == name {
                self.add_module_qualifier(*location);
            }
        }
        ast::visit::visit_typed_constant_var(self, location, module, name, constructor, type_);
    }
}

pub fn code_action_convert_unqualified_constructor_to_qualified(
    module: &Module,
    line_numbers: &LineNumbers,
    params: &CodeActionParams,
    actions: &mut Vec<CodeAction>,
) {
    let mut first_pass = UnqualifiedToQualifiedImportFirstPass::new(module, params, line_numbers);
    first_pass.visit_typed_module(&module.ast);
    let Some(unqualified_constructor) = first_pass.unqualified_constructor else {
        return;
    };
    let second_pass = UnqualifiedToQualifiedImportSecondPass::new(
        module,
        params,
        line_numbers,
        unqualified_constructor,
    );
    let new_actions = second_pass.code_actions();
    actions.extend(new_actions);
}

/// Builder for code action to apply the convert from use action, turning a use
/// expression into a regular function call.
///
pub struct ConvertFromUse<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    selected_use: Option<&'a TypedUse>,
}

impl<'a> ConvertFromUse<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            selected_use: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(use_) = self.selected_use else {
            return vec![];
        };

        let TypedExpr::Call { arguments, fun, .. } = use_.call.as_ref() else {
            return vec![];
        };

        // If the use callback we're desugaring is using labels, that means we
        // have to add the last argument's label when writing the callback;
        // otherwise, it would result in invalid code.
        //
        //     use acc, item <- list.fold(over: list, from: 1)
        //     todo
        //
        // Needs to be rewritten as:
        //
        //     list.fold(over: list, from: 1, with: fn(acc, item) { ... })
        //                                    ^^^^^ We cannot forget to add this label back!
        //
        let callback_label = if arguments.iter().any(|arg| arg.label.is_some()) {
            fun.field_map()
                .and_then(|field_map| field_map.missing_labels(arguments).last().cloned())
                .map(|label| eco_format!("{label}: "))
                .unwrap_or(EcoString::from(""))
        } else {
            EcoString::from("")
        };

        // The use callback is not necessarily the last argument. If you have
        // the following function: `wibble(a a, b b) { todo }`
        // And use it like this: `use <- wibble(b: 1)`, the first argument `a`
        // is going to be the use callback, not the last one!
        let use_callback = arguments.iter().find(|arg| arg.is_use_implicit_callback());
        let Some(CallArg {
            implicit: Some(ImplicitCallArgOrigin::Use),
            value: TypedExpr::Fn { body, type_, .. },
            ..
        }) = use_callback
        else {
            return vec![];
        };

        // If there's arguments on the left hand side of the function we extract
        // those so we can paste them back as the anonymous function arguments.
        let assignments = if type_.fn_arity().is_some_and(|arity| arity >= 1) {
            let assignments_range =
                use_.assignments_location.start as usize..use_.assignments_location.end as usize;
            self.module
                .code
                .get(assignments_range)
                .expect("use assignments")
        } else {
            ""
        };

        // We first delete everything on the left hand side of use and the use
        // arrow.
        self.edits.delete(SrcSpan {
            start: use_.location.start,
            end: use_.right_hand_side_location.start,
        });

        let use_line_end = use_.right_hand_side_location.end;
        let use_rhs_function_has_some_explicit_arguments = arguments
            .iter()
            .filter(|argument| !argument.is_use_implicit_callback())
            .peekable()
            .peek()
            .is_some();

        let use_rhs_function_ends_with_closed_parentheses = self
            .module
            .code
            .get(use_line_end as usize - 1..use_line_end as usize)
            == Some(")");

        let last_explicit_arg = arguments.iter().rfind(|argument| !argument.is_implicit());
        let last_arg_end = last_explicit_arg.map_or(use_line_end - 1, |arg| arg.location.end);

        // This is the piece of code between the end of the last argument and
        // the end of the use_expression:
        //
        //   use <- wibble(a, b,    )
        //                     ^^^^^ This piece right here, from `,` included
        //                           up to `)` excluded.
        //
        let text_after_last_argument = self
            .module
            .code
            .get(last_arg_end as usize..use_line_end as usize - 1);
        let use_rhs_has_comma_after_last_argument =
            text_after_last_argument.is_some_and(|code| code.contains(','));
        let needs_space_before_callback =
            text_after_last_argument.is_some_and(|code| !code.is_empty() && !code.ends_with(' '));

        if use_rhs_function_ends_with_closed_parentheses {
            // If the function on the right hand side of use ends with a closed
            // parentheses then we have to remove it and add it later at the end
            // of the anonymous function we're inserting.
            //
            //     use <- wibble()
            //                   ^ To add the fn() we need to first remove this
            //
            // So here we write over the last closed parentheses to remove it.
            let callback_start = format!("{callback_label}fn({assignments}) {{");
            self.edits.replace(
                SrcSpan {
                    start: use_line_end - 1,
                    end: use_line_end,
                },
                // If the function on the rhs of use has other orguments besides
                // the implicit fn expression then we need to put a comma after
                // the last argument.
                if use_rhs_function_has_some_explicit_arguments
                    && !use_rhs_has_comma_after_last_argument
                {
                    format!(", {callback_start}")
                } else if needs_space_before_callback {
                    format!(" {callback_start}")
                } else {
                    callback_start.to_string()
                },
            )
        } else {
            // On the other hand, if the function on the right hand side doesn't
            // end with a closed parenthese then we have to manually add it.
            //
            //     use <- wibble
            //                  ^ No parentheses
            //
            self.edits
                .insert(use_line_end, format!("(fn({assignments}) {{"))
        };

        // Then we have to increase indentation for all the lines of the use
        // body.
        let first_fn_expression_range = self.edits.src_span_to_lsp_range(body.first().location());
        let use_body_range = self.edits.src_span_to_lsp_range(use_.call.location());

        for line in first_fn_expression_range.start.line..=use_body_range.end.line {
            self.edits.edits.push(TextEdit {
                range: Range {
                    start: Position { line, character: 0 },
                    end: Position { line, character: 0 },
                },
                new_text: "  ".to_string(),
            })
        }

        let final_line_indentation = " ".repeat(use_body_range.start.character as usize);
        self.edits.insert(
            use_.call.location().end,
            format!("\n{final_line_indentation}}})"),
        );

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Convert from `use`")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for ConvertFromUse<'ast> {
    fn visit_typed_use(&mut self, use_: &'ast TypedUse) {
        let use_range = self.edits.src_span_to_lsp_range(use_.location);

        // If the use expression is using patterns that are not just variable
        // assignments then we can't automatically rewrite it as it would result
        // in a syntax error as we can't pattern match in an anonymous function
        // head.
        // At the same time we can't safely add bindings inside the anonymous
        // function body by picking placeholder names as we'd risk shadowing
        // variables coming from the outer scope.
        // So we just skip those use expressions we can't safely rewrite!
        if within(self.params.range, use_range)
            && use_
                .assignments
                .iter()
                .all(|assignment| assignment.pattern.is_variable())
        {
            self.selected_use = Some(use_);
        }

        // We still want to visit the use expression so that we always end up
        // picking the innermost, most relevant use under the cursor.
        self.visit_typed_expr(&use_.call);
    }
}

/// Builder for code action to apply the convert to use action.
///
pub struct ConvertToUse<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    selected_call: Option<CallLocations>,
}

/// All the locations we'll need to transform a function call into a use
/// expression.
///
struct CallLocations {
    call_span: SrcSpan,
    called_function_span: SrcSpan,
    callback_arguments_span: Option<SrcSpan>,
    arg_before_callback_span: Option<SrcSpan>,
    callback_body_span: SrcSpan,
}

impl<'a> ConvertToUse<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            selected_call: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(CallLocations {
            call_span,
            called_function_span,
            callback_arguments_span,
            arg_before_callback_span,
            callback_body_span,
        }) = self.selected_call
        else {
            return vec![];
        };

        // This is the nesting level of the `use` keyword we've inserted, we
        // want to move the entire body of the anonymous function to this level.
        let use_nesting_level = self.edits.src_span_to_lsp_range(call_span).start.character;
        let indentation = " ".repeat(use_nesting_level as usize);

        // First we move the callback arguments to the left hand side of the
        // call and add the `use` keyword.
        let left_hand_side_text = if let Some(arguments_location) = callback_arguments_span {
            let arguments_start = arguments_location.start as usize;
            let arguments_end = arguments_location.end as usize;
            let arguments_text = self
                .module
                .code
                .get(arguments_start..arguments_end)
                .expect("fn args");
            format!("use {arguments_text} <- ")
        } else {
            "use <- ".into()
        };

        self.edits.insert(call_span.start, left_hand_side_text);

        match arg_before_callback_span {
            // If the function call has no other arguments besides the callback then
            // we just have to remove the `fn(...) {` part.
            //
            //     wibble(fn(...) { ... })
            //           ^^^^^^^^^^ This goes from the end of the called function
            //                      To the start of the first thing in the anonymous
            //                      function's body.
            //
            None => self.edits.replace(
                SrcSpan::new(called_function_span.end, callback_body_span.start),
                format!("\n{indentation}"),
            ),
            // If it has other arguments we'll have to remove those and add a closed
            // parentheses too:
            //
            //     wibble(1, 2, fn(...) { ... })
            //                ^^^^^^^^^^^ We have to replace this with a `)`, it
            //                            goes from the end of the second-to-last
            //                            argument to the start of the first thing
            //                            in the anonymous function's body.
            //
            Some(arg_before_callback) => self.edits.replace(
                SrcSpan::new(arg_before_callback.end, callback_body_span.start),
                format!(")\n{indentation}"),
            ),
        };

        // Then we have to remove two spaces of indentation from each line of
        // the callback function's body.
        let body_range = self.edits.src_span_to_lsp_range(callback_body_span);
        for line in body_range.start.line + 1..=body_range.end.line {
            self.edits.delete_range(Range::new(
                Position { line, character: 0 },
                Position { line, character: 2 },
            ))
        }

        // Then we have to remove the anonymous fn closing `}` and the call's
        // closing `)`.
        self.edits
            .delete(SrcSpan::new(callback_body_span.end, call_span.end));

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Convert to `use`")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for ConvertToUse<'ast> {
    fn visit_typed_function(&mut self, fun: &'ast TypedFunction) {
        // The cursor has to be inside the last statement of the function to
        // offer the code action.
        if let Some(last) = &fun.body.last()
            && within(
                self.params.range,
                self.edits.src_span_to_lsp_range(last.location()),
            )
            && let Some(call_data) = turn_statement_into_use(last)
        {
            self.selected_call = Some(call_data);
        }

        ast::visit::visit_typed_function(self, fun)
    }

    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        arguments: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        // The cursor has to be inside the last statement of the body to
        // offer the code action.
        let last_statement_range = self.edits.src_span_to_lsp_range(body.last().location());
        if within(self.params.range, last_statement_range)
            && let Some(call_data) = turn_statement_into_use(body.last())
        {
            self.selected_call = Some(call_data);
        }

        ast::visit::visit_typed_expr_fn(
            self,
            location,
            type_,
            kind,
            arguments,
            body,
            return_annotation,
        );
    }

    fn visit_typed_expr_block(
        &mut self,
        location: &'ast SrcSpan,
        statements: &'ast [TypedStatement],
    ) {
        let Some(last_statement) = statements.last() else {
            return;
        };

        // The cursor has to be inside the last statement of the block to offer
        // the code action.
        let statement_range = self.edits.src_span_to_lsp_range(last_statement.location());
        if within(self.params.range, statement_range) {
            // Only the last statement of a block can be turned into a use!
            if let Some(selected_call) = turn_statement_into_use(last_statement) {
                self.selected_call = Some(selected_call)
            }
        }

        ast::visit::visit_typed_expr_block(self, location, statements);
    }
}

fn turn_statement_into_use(statement: &TypedStatement) -> Option<CallLocations> {
    match statement {
        ast::Statement::Use(_) | ast::Statement::Assignment(_) | ast::Statement::Assert(_) => None,
        ast::Statement::Expression(expression) => turn_expression_into_use(expression),
    }
}

fn turn_expression_into_use(expr: &TypedExpr) -> Option<CallLocations> {
    let TypedExpr::Call {
        arguments,
        location: call_span,
        fun: called_function,
        ..
    } = expr
    else {
        return None;
    };

    // The function arguments in the ast are reordered using function's field map.
    // This means that in the `args` array they might not appear in the same order
    // in which they are written by the user. Since the rest of the code relies
    // on their order in the written code we first have to sort them by their
    // source position.
    let arguments = arguments
        .iter()
        .sorted_by_key(|argument| argument.location.start)
        .collect_vec();

    let CallArg {
        value: last_arg,
        implicit: None,
        ..
    } = arguments.last()?
    else {
        return None;
    };

    let TypedExpr::Fn {
        arguments: callback_arguments,
        body,
        ..
    } = last_arg
    else {
        return None;
    };

    let callback_arguments_span = match (callback_arguments.first(), callback_arguments.last()) {
        (Some(first), Some(last)) => Some(first.location.merge(&last.location)),
        _ => None,
    };

    let arg_before_callback_span = if arguments.len() >= 2 {
        arguments
            .get(arguments.len() - 2)
            .map(|call_arg| call_arg.location)
    } else {
        None
    };

    let callback_body_span = body.first().location().merge(&body.last().last_location());

    Some(CallLocations {
        call_span: *call_span,
        called_function_span: called_function.location(),
        callback_arguments_span,
        arg_before_callback_span,
        callback_body_span,
    })
}

/// Builder for code action to extract expression into a variable.
/// The action will wrap the expression in a block if needed in the appropriate scope.
///
/// For using the code action on the following selection:
///
/// ```gleam
/// fn void() {
///   case result {
///     Ok(value) -> 2 * value + 1
/// //               ^^^^^^^^^
///     Error(_) -> panic
///   }
/// }
/// ```
///
/// Will result:
///
/// ```gleam
/// fn void() {
///   case result {
///     Ok(value) -> {
///       let int = 2 * value
///       int + 1
///     }
///     Error(_) -> panic
///   }
/// }
/// ```
pub struct ExtractVariable<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    position: Option<ExtractVariablePosition>,
    selected_expression: Option<ExtractedToVariable>,
    statement_before_selected_expression: Option<SrcSpan>,
    latest_statement: Option<SrcSpan>,
    to_be_wrapped: bool,
    name_generator: NameGenerator,
}

pub enum ExtractedToVariable {
    Expression { location: SrcSpan, type_: Arc<Type> },
    StartOfPipeline { location: SrcSpan, type_: Arc<Type> },
}

/// The Position of the selected code
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum ExtractVariablePosition {
    InsideCaptureBody,
    /// Full statements (i.e. assignments, `use`s, and simple expressions).
    TopLevelStatement,
    /// The call on the right hand side of a pipe `|>`.
    PipelineCall,
    /// The right hand side of the `->` in a case expression.
    InsideCaseClause,
    // A call argument. This can also be a `use` callback.
    CallArg,
}

impl<'a> ExtractVariable<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            position: None,
            selected_expression: None,
            latest_statement: None,
            statement_before_selected_expression: None,
            to_be_wrapped: false,
            name_generator: NameGenerator::new(),
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let (Some(extracted_value), Some(insert_location)) = (
            self.selected_expression,
            self.statement_before_selected_expression,
        ) else {
            return vec![];
        };

        let expression_type = match &extracted_value {
            ExtractedToVariable::Expression { type_, .. }
            | ExtractedToVariable::StartOfPipeline { type_, .. } => type_,
        };
        let expression_span = match &extracted_value {
            ExtractedToVariable::Expression { location, .. }
            | ExtractedToVariable::StartOfPipeline { location, .. } => location,
        };

        let variable_name = self.name_generator.generate_name_from_type(expression_type);
        let content = self
            .module
            .code
            .get(expression_span.start as usize..expression_span.end as usize)
            .expect("selected expression");

        let range = self.edits.src_span_to_lsp_range(insert_location);

        let indent_size =
            count_indentation(&self.module.code, self.edits.line_numbers, range.start.line);

        let mut indent = " ".repeat(indent_size);

        // We insert the variable declaration
        // Wrap in a block if needed
        let mut insertion = match extracted_value {
            ExtractedToVariable::Expression { .. } => format!("let {variable_name} = {content}"),
            ExtractedToVariable::StartOfPipeline { .. } => {
                format!("let {variable_name} =\n{indent}  {content}\n")
            }
        };

        if self.to_be_wrapped {
            let line_end = self
                .edits
                .line_numbers
                .line_starts
                .get((range.end.line + 1) as usize)
                .expect("Line number should be valid");

            self.edits.insert(*line_end, format!("{indent}}}\n"));
            indent += "  ";
            insertion = format!("{{\n{indent}{insertion}");
        };

        self.edits
            .insert(insert_location.start, format!("{insertion}\n{indent}"));

        self.edits
            .replace(*expression_span, String::from(variable_name));

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Extract variable")
            .kind(CodeActionKind::REFACTOR_EXTRACT)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }

    fn at_position<F>(&mut self, position: ExtractVariablePosition, fun: F)
    where
        F: Fn(&mut Self),
    {
        self.at_optional_position(Some(position), fun);
    }

    fn at_optional_position<F>(&mut self, position: Option<ExtractVariablePosition>, fun: F)
    where
        F: Fn(&mut Self),
    {
        let previous_statement = self.latest_statement;
        let previous_position = self.position;
        self.position = position;
        fun(self);
        self.position = previous_position;
        self.latest_statement = previous_statement;
    }
}

impl<'ast> ast::visit::Visit<'ast> for ExtractVariable<'ast> {
    fn visit_typed_statement(&mut self, statement: &'ast TypedStatement) {
        let range = self.edits.src_span_to_lsp_range(statement.location());
        if !within(self.params.range, range) {
            self.latest_statement = Some(statement.location());
            ast::visit::visit_typed_statement(self, statement);
            return;
        }

        match self.position {
            // A capture body is comprised of just a single expression statement
            // that is inserted by the compiler, we don't really want to put
            // anything before that; so in this case we avoid tracking it.
            Some(ExtractVariablePosition::InsideCaptureBody) => {}
            Some(ExtractVariablePosition::PipelineCall) => {
                // Insert above the pipeline start
                self.latest_statement = Some(statement.location());
            }
            _ => {
                // Insert below the previous statement
                self.latest_statement = Some(statement.location());
                self.statement_before_selected_expression = self.latest_statement;
            }
        }

        self.at_position(ExtractVariablePosition::TopLevelStatement, |this| {
            ast::visit::visit_typed_statement(this, statement);
        });
    }

    fn visit_typed_function(&mut self, fun: &'ast TypedFunction) {
        let fun_range = self.edits.src_span_to_lsp_range(SrcSpan {
            start: fun.location.start,
            end: fun.end_position,
        });

        if !within(self.params.range, fun_range) {
            return;
        }

        // We reset the name generator to purge the variable names from other scopes.
        // We then add the reserve the constant names.
        self.name_generator = NameGenerator::new();
        self.name_generator
            .reserve_module_value_names(&self.module.ast.definitions);

        ast::visit::visit_typed_function(self, fun);
    }

    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        if let Pattern::Variable { name, .. } = &assignment.pattern {
            self.name_generator.add_used_name(name.clone())
        };
        ast::visit::visit_typed_assignment(self, assignment);
    }

    fn visit_typed_expr_pipeline(
        &mut self,
        location: &'ast SrcSpan,
        first_value: &'ast TypedPipelineAssignment,
        assignments: &'ast [(TypedPipelineAssignment, PipelineAssignmentKind)],
        finally: &'ast TypedExpr,
        finally_kind: &'ast PipelineAssignmentKind,
    ) {
        let expr_range = self.edits.src_span_to_lsp_range(*location);
        if !within(self.params.range, expr_range) {
            ast::visit::visit_typed_expr_pipeline(
                self,
                location,
                first_value,
                assignments,
                finally,
                finally_kind,
            );
            return;
        };

        // Visiting a pipeline requires a bit of care, we don't want to extract
        // intermediate steps as variables (those are function calls)!
        // So we start by checking if the selected section contains multiple
        // steps including the first one: in that case we can extract all those
        // steps as a single variable.
        let selection = self.edits.lsp_range_to_src_span(self.params.range);
        let is_inside_first_step = first_value.location.contains(selection.start);
        let last_included_step = assignments.iter().find_map(|(assignment, _kind)| {
            if assignment.location.contains(selection.end) {
                Some(assignment)
            } else {
                None
            }
        });

        if let Some(last) = last_included_step
            && is_inside_first_step
        {
            let location = first_value.location.merge(&last.value.location());
            self.selected_expression = Some(ExtractedToVariable::StartOfPipeline {
                location,
                type_: last.type_(),
            });
            return;
        }

        // Otherwise we visit all the steps individually to see  if there's
        // something _inside_ a step that might be extracted.
        let all_assignments =
            iter::once(first_value).chain(assignments.iter().map(|(assignment, _kind)| assignment));
        for assignment in all_assignments {
            // With the position as "PipelineCall" we know we can't extract the
            // pipeline step itself!
            self.at_position(ExtractVariablePosition::PipelineCall, |this| {
                this.visit_typed_pipeline_assignment(assignment);
            });
        }

        self.at_position(ExtractVariablePosition::PipelineCall, |this| {
            this.visit_typed_expr(finally)
        });
    }

    fn visit_typed_expr(&mut self, expr: &'ast TypedExpr) {
        let expr_location = expr.location();
        let expr_range = self.edits.src_span_to_lsp_range(expr_location);
        if !within(self.params.range, expr_range) {
            ast::visit::visit_typed_expr(self, expr);
            return;
        }

        // If the expression is a top level statement we don't want to extract
        // it into a variable. It would mean we would turn this:
        //
        // ```gleam
        // pub fn main() {
        //   let wibble = 1
        //   //           ^ cursor here
        // }
        //
        // // into:
        //
        // pub fn main() {
        //   let int = 1
        //   let wibble = int
        // }
        // ```
        //
        // Not all that useful!
        //
        match self.position {
            Some(
                ExtractVariablePosition::TopLevelStatement | ExtractVariablePosition::PipelineCall,
            ) => {
                self.at_optional_position(None, |this| {
                    ast::visit::visit_typed_expr(this, expr);
                });
                return;
            }
            Some(
                ExtractVariablePosition::InsideCaptureBody
                | ExtractVariablePosition::InsideCaseClause
                | ExtractVariablePosition::CallArg,
            )
            | None => {}
        }

        match expr {
            TypedExpr::Fn {
                kind: FunctionLiteralKind::Anonymous { .. },
                ..
            } => {
                self.at_position(ExtractVariablePosition::TopLevelStatement, |this| {
                    ast::visit::visit_typed_expr(this, expr);
                });
                return;
            }

            // Expressions that don't make sense to extract
            TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Invalid { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::Var { .. } => (),

            TypedExpr::Int { location, .. }
            | TypedExpr::Float { location, .. }
            | TypedExpr::String { location, .. }
            | TypedExpr::Pipeline { location, .. }
            | TypedExpr::Fn { location, .. }
            | TypedExpr::Todo { location, .. }
            | TypedExpr::List { location, .. }
            | TypedExpr::Call { location, .. }
            | TypedExpr::BinOp { location, .. }
            | TypedExpr::Case { location, .. }
            | TypedExpr::RecordAccess { location, .. }
            | TypedExpr::Tuple { location, .. }
            | TypedExpr::TupleIndex { location, .. }
            | TypedExpr::BitArray { location, .. }
            | TypedExpr::RecordUpdate { location, .. }
            | TypedExpr::NegateBool { location, .. }
            | TypedExpr::NegateInt { location, .. } => {
                if let Some(ExtractVariablePosition::CallArg) = self.position {
                    // Don't update latest statement, we don't want to insert the extracted
                    // variable inside the parenthesis where the call argument is located.
                } else {
                    self.statement_before_selected_expression = self.latest_statement;
                };
                self.selected_expression = Some(ExtractedToVariable::Expression {
                    location: *location,
                    type_: expr.type_(),
                });
            }
        }

        ast::visit::visit_typed_expr(self, expr);
    }

    fn visit_typed_use(&mut self, use_: &'ast TypedUse) {
        let range = self.edits.src_span_to_lsp_range(use_.call.location());
        if !within(self.params.range, range) {
            ast::visit::visit_typed_use(self, use_);
            return;
        }

        // Insert code under the `use`
        self.statement_before_selected_expression = Some(use_.call.location());
        self.at_position(ExtractVariablePosition::TopLevelStatement, |this| {
            ast::visit::visit_typed_use(this, use_);
        });
    }

    fn visit_typed_clause(&mut self, clause: &'ast ast::TypedClause) {
        let range = self.edits.src_span_to_lsp_range(clause.location());
        if !within(self.params.range, range) {
            ast::visit::visit_typed_clause(self, clause);
            return;
        }

        // Insert code after the `->`
        self.latest_statement = Some(clause.then.location());
        self.to_be_wrapped = true;
        self.at_position(ExtractVariablePosition::InsideCaseClause, |this| {
            ast::visit::visit_typed_clause(this, clause);
        });
    }

    fn visit_typed_expr_block(
        &mut self,
        location: &'ast SrcSpan,
        statements: &'ast [TypedStatement],
    ) {
        let range = self.edits.src_span_to_lsp_range(*location);
        if !within(self.params.range, range) {
            ast::visit::visit_typed_expr_block(self, location, statements);
            return;
        }

        // Don't extract block as variable
        let mut position = self.position;
        if let Some(ExtractVariablePosition::InsideCaseClause) = position {
            position = None;
            self.to_be_wrapped = false;
        }

        self.at_optional_position(position, |this| {
            ast::visit::visit_typed_expr_block(this, location, statements);
        });
    }

    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        arguments: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        let range = self.edits.src_span_to_lsp_range(*location);
        if !within(self.params.range, range) {
            ast::visit::visit_typed_expr_fn(
                self,
                location,
                type_,
                kind,
                arguments,
                body,
                return_annotation,
            );
            return;
        }

        let position = match kind {
            // If a fn is a capture `int.wibble(1, _)` its body will consist of
            // just a single expression statement. When visiting we must record
            // we're inside a capture body.
            FunctionLiteralKind::Capture { .. } => Some(ExtractVariablePosition::InsideCaptureBody),
            FunctionLiteralKind::Use { .. } => Some(ExtractVariablePosition::TopLevelStatement),
            FunctionLiteralKind::Anonymous { .. } => self.position,
        };

        self.at_optional_position(position, |this| {
            ast::visit::visit_typed_expr_fn(
                this,
                location,
                type_,
                kind,
                arguments,
                body,
                return_annotation,
            );
        });
    }

    fn visit_typed_call_arg(&mut self, arg: &'ast TypedCallArg) {
        let range = self.edits.src_span_to_lsp_range(arg.location);
        if !within(self.params.range, range) {
            ast::visit::visit_typed_call_arg(self, arg);
            return;
        }

        // An implicit record update arg in inserted by the compiler, we don't
        // want folks to interact with this since it doesn't translate to
        // anything in the source code despite having a default position.
        if let Some(ImplicitCallArgOrigin::RecordUpdate) = arg.implicit {
            return;
        }

        let position = if arg.is_use_implicit_callback() {
            Some(ExtractVariablePosition::TopLevelStatement)
        } else {
            Some(ExtractVariablePosition::CallArg)
        };

        self.at_optional_position(position, |this| {
            ast::visit::visit_typed_call_arg(this, arg);
        });
    }

    // We don't want to offer the action if the cursor is over some invalid
    // piece of code.
    fn visit_typed_expr_invalid(
        &mut self,
        location: &'ast SrcSpan,
        _type_: &'ast Arc<Type>,
        _extra_information: &'ast Option<InvalidExpression>,
    ) {
        let invalid_range = self.edits.src_span_to_lsp_range(*location);
        if within(self.params.range, invalid_range) {
            self.selected_expression = None;
        }
    }
}

/// Builder for code action to convert a literal use into a const.
///
/// For using the code action on each of the following lines:
///
/// ```gleam
/// fn void() {
///   let var = [1, 2, 3]
///   let res = function("Statement", var)
/// }
/// ```
///
/// Both value literals will become:
///
/// ```gleam
/// const var = [1, 2, 3]
/// const string = "Statement"
///
/// fn void() {
///   let res = function(string, var)
/// }
/// ```
pub struct ExtractConstant<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    /// The whole selected expression
    selected_expression: Option<SrcSpan>,
    /// The location of the start of the function containing the expression.
    /// It includes function's documentation as well
    container_function_start: Option<u32>,
    /// The variant of the extractable expression being extracted (if any)
    variant_of_extractable: Option<ExtractableToConstant>,
    /// The name of the newly created constant
    name_to_use: Option<EcoString>,
    /// The right hand side expression of the newly created constant
    value_to_use: Option<EcoString>,
}

/// Used when an expression can be extracted to a constant
enum ExtractableToConstant {
    /// Used for collections and operator uses. This means that elements
    /// inside, are also extractable as constants.
    ComposedValue,
    /// Used for single values. Literals in Gleam can be Ints, Floats, Strings
    /// and type variants (not records).
    SingleValue,
    /// Used for whole variable assignments. If the right hand side of the
    /// expression can be extracted, the whole expression extracted and use the
    /// local variable as a constant.
    Assignment,
}

fn can_be_constant(
    module: &Module,
    expr: &TypedExpr,
    module_constants: Option<&HashSet<&EcoString>>,
) -> bool {
    // We pass the `module_constants` on recursion to not compute them each time
    let module_constants = match module_constants {
        Some(module_constants) => module_constants,
        None => &module
            .ast
            .definitions
            .constants
            .iter()
            .map(|constant| &constant.name)
            .collect(),
    };

    match expr {
        // Attempt to extract whole list as long as it's comprised of only literals
        TypedExpr::List { elements, tail, .. } => {
            elements
                .iter()
                .all(|element| can_be_constant(module, element, Some(module_constants)))
                && tail.is_none()
        }

        // Attempt to extract whole bit array as long as it's made up of literals
        TypedExpr::BitArray { segments, .. } => {
            segments
                .iter()
                .all(|segment| can_be_constant(module, &segment.value, Some(module_constants)))
                && segments.iter().all(|segment| {
                    segment.options.iter().all(|option| match option {
                        ast::BitArrayOption::Size { value, .. } => {
                            can_be_constant(module, value, Some(module_constants))
                        }

                        ast::BitArrayOption::Bytes { .. }
                        | ast::BitArrayOption::Int { .. }
                        | ast::BitArrayOption::Float { .. }
                        | ast::BitArrayOption::Bits { .. }
                        | ast::BitArrayOption::Utf8 { .. }
                        | ast::BitArrayOption::Utf16 { .. }
                        | ast::BitArrayOption::Utf32 { .. }
                        | ast::BitArrayOption::Utf8Codepoint { .. }
                        | ast::BitArrayOption::Utf16Codepoint { .. }
                        | ast::BitArrayOption::Utf32Codepoint { .. }
                        | ast::BitArrayOption::Signed { .. }
                        | ast::BitArrayOption::Unsigned { .. }
                        | ast::BitArrayOption::Big { .. }
                        | ast::BitArrayOption::Little { .. }
                        | ast::BitArrayOption::Native { .. }
                        | ast::BitArrayOption::Unit { .. } => true,
                    })
                })
        }

        // Attempt to extract whole tuple as long as it's comprised of only literals
        TypedExpr::Tuple { elements, .. } => elements
            .iter()
            .all(|element| can_be_constant(module, element, Some(module_constants))),

        // Extract literals directly
        TypedExpr::Int { .. } | TypedExpr::Float { .. } | TypedExpr::String { .. } => true,

        // Extract non-record types directly
        TypedExpr::Var {
            constructor, name, ..
        } => {
            matches!(
                constructor.variant,
                type_::ValueConstructorVariant::Record { arity: 0, .. }
            ) || module_constants.contains(name)
        }

        // Extract record types as long as arguments can be constant
        TypedExpr::Call { arguments, fun, .. } => {
            fun.is_record_literal()
                && arguments
                    .iter()
                    .all(|arg| can_be_constant(module, &arg.value, Some(module_constants)))
        }

        // Extract concat binary operation if both sides can be constants
        TypedExpr::BinOp {
            name, left, right, ..
        } => {
            matches!(name, ast::BinOp::Concatenate)
                && can_be_constant(module, left, Some(module_constants))
                && can_be_constant(module, right, Some(module_constants))
        }

        TypedExpr::Block { .. }
        | TypedExpr::Pipeline { .. }
        | TypedExpr::Fn { .. }
        | TypedExpr::Case { .. }
        | TypedExpr::RecordAccess { .. }
        | TypedExpr::PositionalAccess { .. }
        | TypedExpr::ModuleSelect { .. }
        | TypedExpr::TupleIndex { .. }
        | TypedExpr::Todo { .. }
        | TypedExpr::Panic { .. }
        | TypedExpr::Echo { .. }
        | TypedExpr::RecordUpdate { .. }
        | TypedExpr::NegateBool { .. }
        | TypedExpr::NegateInt { .. }
        | TypedExpr::Invalid { .. } => false,
    }
}

/// Takes the list of already existing constants and functions and creates a
/// name that doesn't conflict with them
///
fn generate_new_name_for_constant(module: &Module, expr: &TypedExpr) -> EcoString {
    let mut name_generator = NameGenerator::new();
    name_generator.reserve_module_value_names(&module.ast.definitions);
    name_generator.generate_name_from_type(&expr.type_())
}

/// Converts the source start position of a documentation comment's contents into
/// the position of the leading slash in its marker ('///').
fn get_doc_marker_position(content_pos: u32) -> u32 {
    content_pos.saturating_sub(3)
}

impl<'a> ExtractConstant<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            selected_expression: None,
            container_function_start: None,
            variant_of_extractable: None,
            name_to_use: None,
            value_to_use: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let (
            Some(expr_span),
            Some(function_start),
            Some(type_of_extractable),
            Some(new_const_name),
            Some(const_value),
        ) = (
            self.selected_expression,
            self.container_function_start,
            self.variant_of_extractable,
            self.name_to_use,
            self.value_to_use,
        )
        else {
            return vec![];
        };

        // We insert the constant declaration
        self.edits.insert(
            function_start,
            format!("const {new_const_name} = {const_value}\n\n"),
        );

        // We remove or replace the selected expression
        match type_of_extractable {
            // The whole expression is deleted for assignments
            ExtractableToConstant::Assignment => {
                let range = self
                    .edits
                    .src_span_to_lsp_range(self.selected_expression.expect("Real range value"));

                let indent_size =
                    count_indentation(&self.module.code, self.edits.line_numbers, range.start.line);

                let expr_span_with_new_line = SrcSpan {
                    // We remove leading indentation + 1 to remove the newline with it
                    start: expr_span.start - (indent_size as u32 + 1),
                    end: expr_span.end,
                };
                self.edits.delete(expr_span_with_new_line);
            }

            // Only  right hand side is replaced for collection or values
            ExtractableToConstant::ComposedValue | ExtractableToConstant::SingleValue => {
                self.edits.replace(expr_span, String::from(new_const_name));
            }
        }

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Extract constant")
            .kind(CodeActionKind::REFACTOR_EXTRACT)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for ExtractConstant<'ast> {
    /// To get the position of the function containing the value or assignment
    /// to extract
    fn visit_typed_function(&mut self, fun: &'ast TypedFunction) {
        let fun_location = fun.location;
        let fun_range = self.edits.src_span_to_lsp_range(SrcSpan {
            start: fun_location.start,
            end: fun.end_position,
        });

        if !within(self.params.range, fun_range) {
            return;
        }

        // Here we need to get position of the function, starting from the leading slash in the
        // documentation comment's marker ('///'), not from comment's content (of which
        // we have the position), so we must convert the content start position
        // to the leading slash's position.
        self.container_function_start = Some(
            fun.documentation
                .as_ref()
                .map(|(doc_start, _)| get_doc_marker_position(*doc_start))
                .unwrap_or(fun_location.start),
        );

        ast::visit::visit_typed_function(self, fun);
    }

    /// To extract the whole assignment
    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        let expr_location = assignment.location;

        // We only offer this code action for extracting the whole assignment
        // between `let` and `=`.
        let pattern_location = assignment.pattern.location();
        let location = SrcSpan::new(assignment.location.start, pattern_location.end);
        let code_action_range = self.edits.src_span_to_lsp_range(location);

        if !within(self.params.range, code_action_range) {
            ast::visit::visit_typed_assignment(self, assignment);
            return;
        }

        // Has to be variable because patterns can't be constants.
        if assignment.pattern.is_variable() && can_be_constant(self.module, &assignment.value, None)
        {
            self.variant_of_extractable = Some(ExtractableToConstant::Assignment);
            self.selected_expression = Some(expr_location);
            self.name_to_use = if let Pattern::Variable { name, .. } = &assignment.pattern {
                Some(name.clone())
            } else {
                None
            };
            self.value_to_use = Some(EcoString::from(
                self.module
                    .code
                    .get(
                        (assignment.value.location().start as usize)
                            ..(assignment.location.end as usize),
                    )
                    .expect("selected expression"),
            ));
        }
    }

    /// To extract only the literal
    fn visit_typed_expr(&mut self, expr: &'ast TypedExpr) {
        let expr_location = expr.location();
        let expr_range = self.edits.src_span_to_lsp_range(expr_location);

        if !within(self.params.range, expr_range) {
            ast::visit::visit_typed_expr(self, expr);
            return;
        }

        // Keep going down recursively if:
        // - It's no extractable has been found yet (`None`).
        // - It's a collection, which may or may not contain a value that can
        //   be extracted.
        // - It's a binary operator, which may or may not operate on
        //   extractable values.
        if matches!(
            self.variant_of_extractable,
            None | Some(ExtractableToConstant::ComposedValue)
        ) && can_be_constant(self.module, expr, None)
        {
            self.variant_of_extractable = match expr {
                TypedExpr::Var { .. }
                | TypedExpr::Int { .. }
                | TypedExpr::Float { .. }
                | TypedExpr::String { .. } => Some(ExtractableToConstant::SingleValue),

                TypedExpr::List { .. }
                | TypedExpr::Tuple { .. }
                | TypedExpr::BitArray { .. }
                | TypedExpr::BinOp { .. }
                | TypedExpr::Call { .. } => Some(ExtractableToConstant::ComposedValue),

                TypedExpr::Block { .. }
                | TypedExpr::Pipeline { .. }
                | TypedExpr::Fn { .. }
                | TypedExpr::Case { .. }
                | TypedExpr::RecordAccess { .. }
                | TypedExpr::PositionalAccess { .. }
                | TypedExpr::ModuleSelect { .. }
                | TypedExpr::TupleIndex { .. }
                | TypedExpr::Todo { .. }
                | TypedExpr::Panic { .. }
                | TypedExpr::Echo { .. }
                | TypedExpr::RecordUpdate { .. }
                | TypedExpr::NegateBool { .. }
                | TypedExpr::NegateInt { .. }
                | TypedExpr::Invalid { .. } => None,
            };

            self.selected_expression = Some(expr_location);
            self.name_to_use = Some(generate_new_name_for_constant(self.module, expr));
            self.value_to_use = Some(EcoString::from(
                self.module
                    .code
                    .get((expr_location.start as usize)..(expr_location.end as usize))
                    .expect("selected expression"),
            ));
        }

        ast::visit::visit_typed_expr(self, expr);
    }
}

/// Builder for code action to apply the "expand function capture" action.
///
pub struct ExpandFunctionCapture<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    function_capture_data: Option<FunctionCaptureData>,
}

pub struct FunctionCaptureData {
    function_span: SrcSpan,
    hole_span: SrcSpan,
    hole_type: Arc<Type>,
    reserved_names: VariablesNames,
}

impl<'a> ExpandFunctionCapture<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            function_capture_data: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(FunctionCaptureData {
            function_span,
            hole_span,
            hole_type,
            reserved_names,
        }) = self.function_capture_data
        else {
            return vec![];
        };

        let mut name_generator = NameGenerator::new();
        name_generator.reserve_variable_names(reserved_names);
        let name = name_generator.generate_name_from_type(&hole_type);

        self.edits.replace(hole_span, name.clone().into());
        self.edits.insert(function_span.end, " }".into());
        self.edits
            .insert(function_span.start, format!("fn({name}) {{ "));

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Expand function capture")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for ExpandFunctionCapture<'ast> {
    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        arguments: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        let fn_range = self.edits.src_span_to_lsp_range(*location);
        if within(self.params.range, fn_range)
            && kind.is_capture()
            && let [argument] = arguments
        {
            self.function_capture_data = Some(FunctionCaptureData {
                function_span: *location,
                hole_span: argument.location,
                hole_type: argument.type_.clone(),
                reserved_names: VariablesNames::from_statements(body),
            });
        }

        ast::visit::visit_typed_expr_fn(
            self,
            location,
            type_,
            kind,
            arguments,
            body,
            return_annotation,
        )
    }
}

struct VariablesNames {
    names: HashSet<EcoString>,
}

impl VariablesNames {
    fn from_statements(statements: &[TypedStatement]) -> Self {
        let mut variables = Self {
            names: HashSet::new(),
        };

        for statement in statements {
            variables.visit_typed_statement(statement);
        }
        variables
    }
}

impl<'ast> ast::visit::Visit<'ast> for VariablesNames {
    fn visit_typed_expr_var(
        &mut self,
        _location: &'ast SrcSpan,
        _constructor: &'ast ValueConstructor,
        name: &'ast EcoString,
    ) {
        let _ = self.names.insert(name.clone());
    }
}

/// Builder for code action to apply the "generate dynamic decoder action.
///
pub struct GenerateDynamicDecoder<'a, IO> {
    compiler: &'a LspProjectCompiler<FileSystemProxy<IO>>,
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    printer: Printer<'a>,
    actions: &'a mut Vec<CodeAction>,
}

const DECODE_MODULE: &str = "gleam/dynamic/decode";

impl<'a, IO> GenerateDynamicDecoder<'a, IO> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
        actions: &'a mut Vec<CodeAction>,
        compiler: &'a LspProjectCompiler<FileSystemProxy<IO>>,
    ) -> Self {
        // Since we are generating a new function, type variables from other
        // functions and constants are irrelevant to the types we print.
        let printer = Printer::new_without_type_variables(&module.ast.names);
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            printer,
            actions,
            compiler,
        }
    }

    pub fn code_actions(&mut self) {
        self.visit_typed_module(&self.module.ast);
    }

    fn custom_type_decoder_body(
        &mut self,
        custom_type: &CustomType<Arc<Type>>,
    ) -> Option<EcoString> {
        // We cannot generate a decoder for an external type with no constructors!
        let constructors_size = custom_type.constructors.len();
        let (first, rest) = custom_type.constructors.split_first()?;
        let mode = EncodingMode::for_custom_type(custom_type);

        // We generate a decoder for a type with a single constructor: it does not
        // require pattern matching on a tag as there's no variants to tell apart.
        if rest.is_empty() && mode == EncodingMode::ObjectWithNoTypeTag {
            return self.constructor_decoder(mode, custom_type, first, 0);
        }

        // Otherwise we need to generate a decoder that has to tell apart different
        // variants, depending on the mode we might have to decode a type field or
        // plain strings!
        let module = self.printer.print_module(DECODE_MODULE);
        let discriminant = if mode == EncodingMode::PlainString {
            eco_format!("use variant <- {module}.then({module}.string)")
        } else {
            eco_format!("use variant <- {module}.field(\"type\", {module}.string)")
        };

        let mut clauses = Vec::with_capacity(constructors_size);
        for constructor in iter::once(first).chain(rest) {
            let body = self.constructor_decoder(mode, custom_type, constructor, 4)?;
            let name = to_snake_case(&constructor.name);
            clauses.push(eco_format!(r#"    "{name}" -> {body}"#));
        }

        let failure_clause = self.failure_clause(custom_type);

        let cases = clauses.join("\n");
        Some(eco_format!(
            r#"{{
  {discriminant}
  case variant {{
{cases}
{failure_clause}
  }}
}}"#,
        ))
    }

    fn constructor_decoder(
        &mut self,
        mode: EncodingMode,
        custom_type: &ast::TypedCustomType,
        constructor: &TypedRecordConstructor,
        nesting: usize,
    ) -> Option<EcoString> {
        let decode_module = self.printer.print_module(DECODE_MODULE);
        let constructor_name = &constructor.name;

        // If the constructor was encoded as a plain string with no additional
        // fields it means there's nothing else to decode and we can just
        // succeed.
        if mode == EncodingMode::PlainString {
            return Some(eco_format!("{decode_module}.success({constructor_name})"));
        }

        // Otherwise we have to decode all the constructor fields to build it.
        let mut fields = Vec::with_capacity(constructor.arguments.len());
        for argument in constructor.arguments.iter() {
            let (_, name) = argument.label.as_ref()?;
            let field = RecordField {
                label: RecordLabel::Labeled(name),
                type_: &argument.type_,
            };
            fields.push(field);
        }

        let mut decoder_printer = DecoderPrinter::new(
            &mut self.printer,
            custom_type.name.clone(),
            self.module.name.clone(),
            self.compiler,
        );

        let decoders = fields
            .iter()
            .map(|field| decoder_printer.decode_field(field, nesting + 2))
            .join("\n");

        let indent = " ".repeat(nesting);

        Some(if decoders.is_empty() {
            eco_format!("{decode_module}.success({constructor_name})")
        } else {
            let field_names = fields
                .iter()
                .map(|field| format!("{}:", field.label.variable_name()))
                .join(", ");

            eco_format!(
                "{{
{decoders}
{indent}  {decode_module}.success({constructor_name}({field_names}))
{indent}}}",
            )
        })
    }

    /// Generates the failure/catch-all clause in a decoder function for a
    /// type with `EncodingMode::ObjectWithTypeTag` i.e. the `_ -> decode.failure()`
    /// clause executed when the `type` field does not match any of the known variants.
    ///
    /// # Arguments
    /// * `custom_type` -  The root type we are printing a decoder for
    fn failure_clause(&mut self, custom_type: &CustomType<Arc<Type>>) -> EcoString {
        let decode_module = self.printer.print_module(DECODE_MODULE);
        let type_name = &custom_type.name;

        let mut decoder_printer = DecoderPrinter::new(
            &mut self.printer,
            type_name.clone(),
            self.module.name.clone(),
            self.compiler,
        );

        // The construction of the zero value might necessitate importing
        // modules that aren't currently in scope. We keep track of them here.
        let mut modules_to_import = HashSet::new();
        // We also need to keep track of the path we are tracing through the types
        // to make sure we don't get stuck in an infinite loop building a recursive
        // type.
        let mut path = vec![];

        if let Some(zero_value) = decoder_printer.zero_value_for_custom_type(
            &self.module.name,
            type_name,
            &mut path,
            &mut modules_to_import,
        ) {
            for module_name in modules_to_import {
                maybe_import(&mut self.edits, self.module, &module_name);
            }
            eco_format!(r#"    _ -> {decode_module}.failure({zero_value}, "{type_name}")"#,)
        } else {
            eco_format!(
                r#"    _ -> {decode_module}.failure(todo as "Zero value for {type_name}", "{type_name}")"#
            )
        }
    }
}

impl<'ast, IO> ast::visit::Visit<'ast> for GenerateDynamicDecoder<'ast, IO> {
    fn visit_typed_custom_type(&mut self, custom_type: &'ast ast::TypedCustomType) {
        let range = self.edits.src_span_to_lsp_range(custom_type.location);
        if !overlaps(self.params.range, range) {
            return;
        }

        let name = eco_format!("{}_decoder", to_snake_case(&custom_type.name));
        let Some(function_body) = self.custom_type_decoder_body(custom_type) else {
            return;
        };

        let parameters = match custom_type.parameters.len() {
            0 => EcoString::new(),
            _ => eco_format!(
                "({})",
                custom_type
                    .parameters
                    .iter()
                    .map(|(_, name)| name)
                    .join(", ")
            ),
        };

        let decoder_type = self.printer.print_type(&Type::Named {
            publicity: Publicity::Public,
            package: STDLIB_PACKAGE_NAME.into(),
            module: DECODE_MODULE.into(),
            name: "Decoder".into(),
            arguments: vec![],
            inferred_variant: None,
        });

        let function = format!(
            "\n\nfn {name}() -> {decoder_type}({type_name}{parameters}) {function_body}",
            type_name = custom_type.name,
        );

        self.edits.insert(custom_type.end_position, function);
        maybe_import(&mut self.edits, self.module, DECODE_MODULE);

        CodeActionBuilder::new("Generate dynamic decoder")
            .kind(CodeActionKind::REFACTOR)
            .preferred(false)
            .changes(
                self.params.text_document.uri.clone(),
                std::mem::take(&mut self.edits.edits),
            )
            .push_to(self.actions);
    }
}

/// If `module_name` is not already imported inside `module`, adds an edit to
/// add that import.
/// This function also makes sure not to import a module in itself.
///
fn maybe_import(edits: &mut TextEdits<'_>, module: &Module, module_name: &str) {
    if module.ast.names.is_imported(module_name) || module.name == module_name {
        return;
    }

    let first_import_pos = position_of_first_definition_if_import(module, edits.line_numbers);
    let first_is_import = first_import_pos.is_some();
    let import_location = first_import_pos.unwrap_or_default();
    let after_import_newlines = add_newlines_after_import(
        import_location,
        first_is_import,
        edits.line_numbers,
        &module.code,
    );

    edits.edits.push(get_import_edit(
        import_location,
        module_name,
        &after_import_newlines,
    ));
}

struct DecoderPrinter<'a, 'b, IO> {
    printer: &'a mut Printer<'b>,
    /// The name of the root type we are printing a decoder for
    type_name: EcoString,
    /// The module name of the root type we are printing a decoder for
    type_module: EcoString,
    compiler: &'a LspProjectCompiler<FileSystemProxy<IO>>,
}

const DYNAMIC_MODULE: &str = "gleam/dynamic";
const DICT_MODULE: &str = "gleam/dict";
const OPTION_MODULE: &str = "gleam/option";

struct RecordField<'a> {
    label: RecordLabel<'a>,
    type_: &'a Type,
}

enum RecordLabel<'a> {
    Labeled(&'a str),
    Unlabeled(usize),
}

impl RecordLabel<'_> {
    fn field_key(&self) -> EcoString {
        match self {
            RecordLabel::Labeled(label) => eco_format!("\"{label}\""),
            RecordLabel::Unlabeled(index) => {
                eco_format!("{index}")
            }
        }
    }

    fn variable_name(&self) -> EcoString {
        match self {
            RecordLabel::Labeled(label) => (*label).into(),
            &RecordLabel::Unlabeled(mut index) => {
                let mut characters = Vec::new();
                let alphabet_length = 26;
                let alphabet_offset = b'a';
                loop {
                    let alphabet_index = (index % alphabet_length) as u8;
                    characters.push((alphabet_offset + alphabet_index) as char);
                    index /= alphabet_length;

                    if index == 0 {
                        break;
                    }
                    index -= 1;
                }
                characters.into_iter().rev().collect()
            }
        }
    }
}

impl<'a, 'b, IO> DecoderPrinter<'a, 'b, IO> {
    fn new(
        printer: &'a mut Printer<'b>,
        type_name: EcoString,
        type_module: EcoString,
        compiler: &'a LspProjectCompiler<FileSystemProxy<IO>>,
    ) -> Self {
        Self {
            type_name,
            type_module,
            printer,
            compiler,
        }
    }

    fn decoder_for(&mut self, type_: &Type, indent: usize) -> EcoString {
        let module_name = self.printer.print_module(DECODE_MODULE);
        if type_.is_bit_array() {
            eco_format!("{module_name}.bit_array")
        } else if type_.is_bool() {
            eco_format!("{module_name}.bool")
        } else if type_.is_float() {
            eco_format!("{module_name}.float")
        } else if type_.is_int() {
            eco_format!("{module_name}.int")
        } else if type_.is_string() {
            eco_format!("{module_name}.string")
        } else if type_.is_nil() {
            eco_format!("{module_name}.success(Nil)")
        } else {
            match type_.tuple_types() {
                Some(types) => {
                    let fields = types
                        .iter()
                        .enumerate()
                        .map(|(index, type_)| RecordField {
                            type_,
                            label: RecordLabel::Unlabeled(index),
                        })
                        .collect_vec();
                    let decoders = fields
                        .iter()
                        .map(|field| self.decode_field(field, indent + 2))
                        .join("\n");
                    let mut field_names = fields.iter().map(|field| field.label.variable_name());

                    eco_format!(
                        "{{
{decoders}

{indent}  {module_name}.success(#({fields}))
{indent}}}",
                        fields = field_names.join(", "),
                        indent = " ".repeat(indent)
                    )
                }
                _ => {
                    let type_information = type_.named_type_information();
                    let type_information =
                        type_information.as_ref().map(|(module, name, arguments)| {
                            (module.as_str(), name.as_str(), arguments.as_slice())
                        });

                    match type_information {
                        Some(("gleam/dynamic", "Dynamic", _)) => {
                            eco_format!("{module_name}.dynamic")
                        }
                        Some(("gleam", "List", [element])) => {
                            eco_format!("{module_name}.list({})", self.decoder_for(element, indent))
                        }
                        Some(("gleam/option", "Option", [some])) => {
                            eco_format!(
                                "{module_name}.optional({})",
                                self.decoder_for(some, indent)
                            )
                        }
                        Some(("gleam/dict", "Dict", [key, value])) => {
                            eco_format!(
                                "{module_name}.dict({}, {})",
                                self.decoder_for(key, indent),
                                self.decoder_for(value, indent)
                            )
                        }
                        Some((module, name, _))
                            if module == self.type_module && name == self.type_name =>
                        {
                            eco_format!("{}_decoder()", to_snake_case(name))
                        }
                        _ => eco_format!(
                            r#"todo as "Decoder for {}""#,
                            self.printer.print_type(type_)
                        ),
                    }
                }
            }
        }
    }

    fn decode_field(&mut self, field: &RecordField<'_>, indent: usize) -> EcoString {
        let decoder = self.decoder_for(field.type_, indent);

        eco_format!(
            r#"{indent}use {variable} <- {module}.field({field}, {decoder})"#,
            indent = " ".repeat(indent),
            variable = field.label.variable_name(),
            field = field.label.field_key(),
            module = self.printer.print_module(DECODE_MODULE)
        )
    }

    /// Performs best-effort generation of zero values for a given type.
    /// Will succeed for all prelude types and most stdlib types.
    /// For specifics on how custom user-defined types are handled, see
    /// `zero_value_for_custom_type`.
    fn zero_value_for_type(
        &mut self,
        type_: &Type,
        // Keeps track of types visited already to ensure we don't end up in an infinite
        // loop due to recursive types
        path: &mut Vec<(EcoString, EcoString)>,
        modules_to_import: &mut HashSet<EcoString>,
    ) -> Option<EcoString> {
        if type_.is_bit_array() {
            return Some("<<>>".into());
        }
        if type_.is_bool() {
            return Some("False".into());
        }
        if type_.is_float() {
            return Some("0.0".into());
        }
        if type_.is_int() {
            return Some("0".into());
        }
        if type_.is_string() {
            return Some("\"\"".into());
        }
        if type_.is_list() {
            return Some("[]".into());
        }
        if type_.is_nil() {
            return Some("Nil".into());
        }

        if let Some(types) = type_.tuple_types() {
            // We try generating zero values for the tuple members and exit early if any of them fail.
            let field_zeroes = types
                .iter()
                .map_while(|type_| self.zero_value_for_type(type_, path, modules_to_import))
                .collect_vec();

            if field_zeroes.len() < types.len() {
                return None;
            } else {
                return Some(eco_format!("#({})", field_zeroes.iter().join(", ")));
            }
        };

        let (module, name, _) = type_.named_type_information()?;
        match (module.as_str(), name.as_str()) {
            (OPTION_MODULE, "Option") => {
                let _ = modules_to_import.insert(OPTION_MODULE.into());
                Some(eco_format!(
                    "{}.None",
                    self.printer.print_module(OPTION_MODULE)
                ))
            }

            (DYNAMIC_MODULE, "Dynamic") => {
                let _ = modules_to_import.insert(DYNAMIC_MODULE.into());
                Some(eco_format!(
                    "{}.nil()",
                    self.printer.print_module(DYNAMIC_MODULE)
                ))
            }

            (DICT_MODULE, "Dict") => {
                let _ = modules_to_import.insert(DICT_MODULE.into());
                Some(eco_format!(
                    "{}.new()",
                    self.printer.print_module(DICT_MODULE)
                ))
            }

            _ => self.zero_value_for_custom_type(&module, &name, path, modules_to_import),
        }
    }

    /// Best-effort zero value generation for user-defined types in the
    /// current package.
    fn zero_value_for_custom_type(
        &mut self,
        custom_type_module: &EcoString,
        custom_type_name: &EcoString,
        path: &mut Vec<(EcoString, EcoString)>,
        modules_to_import: &mut HashSet<EcoString>,
    ) -> Option<EcoString> {
        // First we check that we have not already visited this type before to
        // avoid cycles.
        let already_seen_type = path.iter().any(|(path_module, path_type_name)| {
            path_module == custom_type_module && path_type_name == custom_type_name
        });

        if already_seen_type {
            return None;
        };

        let type_is_inside_current_module = &self.type_module == custom_type_module;

        let current_module_interface = self
            .compiler
            .modules
            .get(&self.type_module)
            .map(|module| &module.ast.type_info)?;

        let type_module_interface = if !type_is_inside_current_module {
            self.compiler.get_module_interface(custom_type_module)?
        } else {
            current_module_interface
        };

        // We only try and generate zero values for user-defined types in the current
        // package. If you are expanding the scope of this functionality to
        // remove this limitation, make sure to check for internal modules.
        if current_module_interface.package != type_module_interface.package {
            return None;
        }

        let constructors = type_module_interface
            .types_value_constructors
            .get(custom_type_name)?;

        // Opaque types cannot be constructed outside the module they were defined in,
        // so we will be unable to produce a zero value.
        if !type_is_inside_current_module && constructors.opaque == Opaque::Opaque {
            return None;
        }

        // Ideally, we want to use the "smallest" (i.e. fewest fields) constructor
        // to construct our zero value to reduce visual noise, but this might not always
        // be possible. So we check all constructors in increasing order of size to
        // find the first one that succeeds, and then short circuit.
        constructors
            .variants
            .iter()
            .sorted_by_key(|v| v.parameters.len())
            .find_map(|zero_constructor| {
                self.zero_value_for_custom_type_constructor(
                    custom_type_module,
                    custom_type_name,
                    zero_constructor,
                    type_is_inside_current_module,
                    path,
                    modules_to_import,
                )
            })
    }

    /// Attempts to construct a zero value for one specific constructor
    /// of a custom type. Use `zero_value_for_custom_type` instead as it performs
    /// _important checks on type visibility that this method does NOT_.
    /// This is a helper method extracted for readability.
    fn zero_value_for_custom_type_constructor(
        &mut self,
        custom_type_module: &EcoString,
        custom_type_name: &EcoString,
        zero_constructor: &type_::TypeValueConstructor,
        type_is_inside_current_module: bool,
        path: &mut Vec<(EcoString, EcoString)>,
        modules_to_import: &mut HashSet<EcoString>,
    ) -> Option<EcoString> {
        path.push((custom_type_module.clone(), custom_type_name.clone()));

        // Try to generate zero values for all fields in the constructor
        let zero_params = zero_constructor
            .parameters
            .iter()
            .map_while(|parameter| {
                let zero = self.zero_value_for_type(&parameter.type_, path, modules_to_import)?;

                if let Some(label) = &parameter.label {
                    Some(eco_format!("{label}: {zero}"))
                } else {
                    Some(zero)
                }
            })
            .collect_vec();

        // We need to make sure to clean up the path once we've finished
        // "visiting" this type.
        let _ = path.pop();

        // Only proceed if we were able to construct every field successfully
        if zero_params.len() < zero_constructor.parameters.len() {
            return None;
        };

        let zero_constructor = if !type_is_inside_current_module {
            // Type constructors from other modules need to be qualified appropriately,
            // and they might need to be brought into scope.
            let _ = modules_to_import.insert(custom_type_module.clone());
            eco_format!(
                "{}.{}",
                self.printer.print_module(custom_type_module),
                zero_constructor.name
            )
        } else {
            eco_format!("{}", zero_constructor.name)
        };

        if zero_params.is_empty() {
            Some(eco_format!("{zero_constructor}"))
        } else {
            let zero_args = zero_params.iter().join(", ");
            Some(eco_format!("{zero_constructor}({zero_args})"))
        }
    }
}

/// Builder for code action to apply the "Generate to-JSON function" action.
///
pub struct GenerateJsonEncoder<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    printer: Printer<'a>,
    actions: &'a mut Vec<CodeAction>,
    config: &'a PackageConfig,
}

const JSON_MODULE: &str = "gleam/json";
const JSON_PACKAGE_NAME: &str = "gleam_json";

#[derive(Eq, PartialEq, Copy, Clone)]
enum EncodingMode {
    PlainString,
    ObjectWithTypeTag,
    ObjectWithNoTypeTag,
}

impl EncodingMode {
    pub fn for_custom_type(type_: &CustomType<Arc<Type>>) -> Self {
        match type_.constructors.as_slice() {
            [constructor] if constructor.arguments.is_empty() => EncodingMode::PlainString,
            [_constructor] => EncodingMode::ObjectWithNoTypeTag,
            constructors if constructors.iter().all(|c| c.arguments.is_empty()) => {
                EncodingMode::PlainString
            }
            _constructors => EncodingMode::ObjectWithTypeTag,
        }
    }
}

impl<'a> GenerateJsonEncoder<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
        actions: &'a mut Vec<CodeAction>,
        config: &'a PackageConfig,
    ) -> Self {
        // Since we are generating a new function, type variables from other
        // functions and constants are irrelevant to the types we print.
        let printer = Printer::new_without_type_variables(&module.ast.names);
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            printer,
            actions,
            config,
        }
    }

    pub fn code_actions(&mut self) {
        if self.config.dependencies.contains_key(JSON_PACKAGE_NAME)
            || self.config.dev_dependencies.contains_key(JSON_PACKAGE_NAME)
        {
            self.visit_typed_module(&self.module.ast);
        }
    }

    fn custom_type_encoder_body(
        &mut self,
        record_name: EcoString,
        custom_type: &CustomType<Arc<Type>>,
    ) -> Option<EcoString> {
        // We cannot generate a decoder for an external type with no constructors!
        let constructors_size = custom_type.constructors.len();
        let (first, rest) = custom_type.constructors.split_first()?;
        let mode = EncodingMode::for_custom_type(custom_type);

        // We generate an encoder for a type with a single constructor: it does not
        // require pattern matching on the argument as we can access all its fields
        // with the usual record access syntax.
        if rest.is_empty() {
            let encoder = self.constructor_encoder(mode, first, custom_type.name.clone(), 2)?;
            let unpacking = if first.arguments.is_empty() {
                ""
            } else {
                &eco_format!(
                    "let {name}({fields}:) = {record_name}\n  ",
                    name = first.name,
                    fields = first
                        .arguments
                        .iter()
                        .filter_map(|argument| {
                            argument.label.as_ref().map(|(_location, label)| label)
                        })
                        .join(":, ")
                )
            };
            return Some(eco_format!("{unpacking}{encoder}"));
        }

        // Otherwise we generate an encoder for a type with multiple constructors:
        // it will need to pattern match on the various constructors and encode each
        // one separately.
        let mut clauses = Vec::with_capacity(constructors_size);
        for constructor in iter::once(first).chain(rest) {
            let RecordConstructor { name, .. } = constructor;
            let encoder =
                self.constructor_encoder(mode, constructor, custom_type.name.clone(), 4)?;
            if constructor.arguments.is_empty() {
                clauses.push(eco_format!("    {name} -> {encoder}"));
            } else {
                let unpacking = constructor
                    .arguments
                    .iter()
                    .filter_map(|argument| {
                        argument.label.as_ref().map(|(_location, label)| {
                            if is_nil_like(&argument.type_) {
                                eco_format!("{}: _", label)
                            } else {
                                eco_format!("{}:", label)
                            }
                        })
                    })
                    .join(", ");
                clauses.push(eco_format!("    {name}({unpacking}) -> {encoder}"));
            }
        }

        let clauses = clauses.join("\n");
        Some(eco_format!(
            "case {record_name} {{
{clauses}
  }}",
        ))
    }

    fn constructor_encoder(
        &mut self,
        mode: EncodingMode,
        constructor: &TypedRecordConstructor,
        type_name: EcoString,
        nesting: usize,
    ) -> Option<EcoString> {
        let json_module = self.printer.print_module(JSON_MODULE);
        let tag = to_snake_case(&constructor.name);
        let indent = " ".repeat(nesting);

        // If the variant is encoded as a simple json string we just call the
        // `json.string` with the variant tag as an argument.
        if mode == EncodingMode::PlainString {
            return Some(eco_format!("{json_module}.string(\"{tag}\")"));
        }

        // Otherwise we turn it into an object with a `type` tag field.
        let mut encoder_printer =
            JsonEncoderPrinter::new(&mut self.printer, type_name, self.module.name.clone());

        // These are the fields of the json object to encode.
        let mut fields = Vec::with_capacity(constructor.arguments.len());
        if mode == EncodingMode::ObjectWithTypeTag {
            // Any needed type tag is always going to be the first field in the object
            fields.push(eco_format!(
                "{indent}  #(\"type\", {json_module}.string(\"{tag}\"))"
            ));
        }

        for argument in constructor.arguments.iter() {
            let (_, label) = argument.label.as_ref()?;
            let field = RecordField {
                label: RecordLabel::Labeled(label),
                type_: &argument.type_,
            };
            let encoder = encoder_printer.encode_field(&field, nesting + 2);
            fields.push(encoder);
        }

        let fields = fields.join(",\n");
        Some(eco_format!(
            "{json_module}.object([
{fields},
{indent}])"
        ))
    }
}

/// When generating an encoder, we need to know when we can ignore the fields
/// destructured from a constructor.
/// If a field is of type `Nil` or is a tuple type composed entirely of `Nil`
/// types, then we will never need to use the field in our encoder function.
fn is_nil_like(type_: &Type) -> bool {
    if type_.is_nil() {
        return true;
    }

    match type_.tuple_types() {
        Some(types) => types.iter().all(|type_| is_nil_like(type_)),
        _ => false,
    }
}

impl<'ast> ast::visit::Visit<'ast> for GenerateJsonEncoder<'ast> {
    fn visit_typed_custom_type(&mut self, custom_type: &'ast ast::TypedCustomType) {
        let range = self.edits.src_span_to_lsp_range(custom_type.location);
        if !overlaps(self.params.range, range) {
            return;
        }

        let record_name = to_snake_case(&custom_type.name);
        let name = eco_format!("{record_name}_to_json");
        let Some(encoder) = self.custom_type_encoder_body(record_name.clone(), custom_type) else {
            return;
        };

        let json_type = self.printer.print_type(&Type::Named {
            publicity: Publicity::Public,
            package: JSON_PACKAGE_NAME.into(),
            module: JSON_MODULE.into(),
            name: "Json".into(),
            arguments: vec![],
            inferred_variant: None,
        });

        let type_ = if custom_type.parameters.is_empty() {
            custom_type.name.clone()
        } else {
            let parameters = custom_type
                .parameters
                .iter()
                .map(|(_, name)| name)
                .join(", ");
            eco_format!("{}({})", custom_type.name, parameters)
        };

        let function = format!(
            "

fn {name}({record_name}: {type_}) -> {json_type} {{
  {encoder}
}}",
        );

        self.edits.insert(custom_type.end_position, function);
        maybe_import(&mut self.edits, self.module, JSON_MODULE);

        CodeActionBuilder::new("Generate to-JSON function")
            .kind(CodeActionKind::REFACTOR)
            .preferred(false)
            .changes(
                self.params.text_document.uri.clone(),
                std::mem::take(&mut self.edits.edits),
            )
            .push_to(self.actions);
    }
}

struct JsonEncoderPrinter<'a, 'b> {
    printer: &'a mut Printer<'b>,
    /// The name of the root type we are printing an encoder for
    type_name: EcoString,
    /// The module name of the root type we are printing an encoder for
    type_module: EcoString,
}

impl<'a, 'b> JsonEncoderPrinter<'a, 'b> {
    fn new(printer: &'a mut Printer<'b>, type_name: EcoString, type_module: EcoString) -> Self {
        Self {
            type_name,
            type_module,
            printer,
        }
    }

    fn encoder_for(&mut self, encoded_value: &str, type_: &Type, indent: usize) -> EcoString {
        let module_name = self.printer.print_module(JSON_MODULE);
        let is_capture = encoded_value == "_";
        let maybe_capture = |mut function: EcoString| {
            if is_capture {
                function
            } else {
                function.push('(');
                function.push_str(encoded_value);
                function.push(')');
                function
            }
        };

        if type_.is_bool() {
            maybe_capture(eco_format!("{module_name}.bool"))
        } else if type_.is_float() {
            maybe_capture(eco_format!("{module_name}.float"))
        } else if type_.is_int() {
            maybe_capture(eco_format!("{module_name}.int"))
        } else if type_.is_string() {
            maybe_capture(eco_format!("{module_name}.string"))
        } else if type_.is_nil() {
            if is_capture {
                eco_format!("fn(_) {{ {module_name}.null() }}")
            } else {
                eco_format!("{module_name}.null()")
            }
        } else {
            match type_.tuple_types() {
                Some(types) => {
                    let (tuple, new_indent) = if is_capture {
                        ("value", indent + 4)
                    } else {
                        (encoded_value, indent + 2)
                    };

                    // We need to iterate over all of the tuple's fields
                    // to obtain an encoder for each one, so we reuse the
                    // iteration to check whether this tuple can be ignored
                    // in the encoder without calling `is_nil_like`.
                    let mut encoders = Vec::new();
                    let all_values_are_nil = types.iter().enumerate().fold(
                        true,
                        |all_values_are_nil, (index, type_)| {
                            encoders.push(self.encoder_for(
                                &format!("{tuple}.{index}"),
                                type_,
                                new_indent,
                            ));
                            all_values_are_nil && is_nil_like(type_)
                        },
                    );

                    if is_capture {
                        eco_format!(
                            "fn({value}) {{
{indent}  {module_name}.preprocessed_array([
{indent}    {encoders},
{indent}  ])
{indent}}}",
                            value = if all_values_are_nil { "_" } else { "value" },
                            indent = " ".repeat(indent),
                            encoders = encoders.join(&format!(",\n{}", " ".repeat(new_indent))),
                        )
                    } else {
                        eco_format!(
                            "{module_name}.preprocessed_array([
{indent}  {encoders},
{indent}])",
                            indent = " ".repeat(indent),
                            encoders = encoders.join(&format!(",\n{}", " ".repeat(new_indent))),
                        )
                    }
                }
                _ => {
                    let type_information = type_.named_type_information();
                    let type_information: Option<(&str, &str, &[Arc<Type>])> =
                        type_information.as_ref().map(|(module, name, arguments)| {
                            (module.as_str(), name.as_str(), arguments.as_slice())
                        });

                    match type_information {
                        Some(("gleam", "List", [element])) => {
                            eco_format!(
                                "{module_name}.array({encoded_value}, {map_function})",
                                map_function = self.encoder_for("_", element, indent)
                            )
                        }
                        Some(("gleam/option", "Option", [some])) => {
                            eco_format!(
                                "case {encoded_value} {{
{indent}  {none} -> {module_name}.null()
{indent}  {some}({value}) -> {encoder}
{indent}}}",
                                indent = " ".repeat(indent),
                                none = self
                                    .printer
                                    .print_constructor(&"gleam/option".into(), &"None".into()),
                                some = self
                                    .printer
                                    .print_constructor(&"gleam/option".into(), &"Some".into()),
                                value = if is_nil_like(some) { "_" } else { "value" },
                                encoder = self.encoder_for("value", some, indent + 2)
                            )
                        }
                        Some(("gleam/dict", "Dict", [key, value])) => {
                            let stringify_function = match key
                                .named_type_information()
                                .as_ref()
                                .map(|(module, name, arguments)| {
                                    (module.as_str(), name.as_str(), arguments.as_slice())
                                }) {
                                Some(("gleam", "String", [])) => "fn(string) { string }",
                                _ => &format!(
                                    r#"todo as "Function to stringify {}""#,
                                    self.printer.print_type(key)
                                ),
                            };
                            eco_format!(
                                "{module_name}.dict({encoded_value}, {stringify_function}, {})",
                                self.encoder_for("_", value, indent)
                            )
                        }
                        Some((module, name, _))
                            if module == self.type_module && name == self.type_name =>
                        {
                            maybe_capture(eco_format!("{}_to_json", to_snake_case(name)))
                        }
                        _ => eco_format!(
                            r#"todo as "Encoder for {}""#,
                            self.printer.print_type(type_)
                        ),
                    }
                }
            }
        }
    }

    fn encode_field(&mut self, field: &RecordField<'_>, indent: usize) -> EcoString {
        let field_name = field.label.variable_name();
        let encoder = self.encoder_for(&field_name, field.type_, indent);

        eco_format!(
            r#"{indent}#("{field_name}", {encoder})"#,
            indent = " ".repeat(indent),
        )
    }
}

/// Builder for code action to pattern match on things like (anonymous) function
/// arguments or variables.
/// For example:
///
/// ```gleam
/// pub fn wibble(arg: #(key, value)) {
/// //            ^ [pattern match on argument]
/// }
///
/// // Generates
///
/// pub fn wibble(arg: #(key, value)) {
///   let #(value_0, value_1) = arg
/// }
/// ```
///
/// Another example with variables:
///
/// ```gleam
/// pub fn main() {
///   let pair = #(1, 3)
///   //   ^ [pattern match on value]
/// }
///
/// // Generates
///
/// pub fn main() {
///   let pair = #(1, 3)
///   let #(value_0, value_1) = pair
/// }
/// ```
///
pub struct PatternMatchOnValue<'a, A> {
    module: &'a Module,
    params: &'a CodeActionParams,
    compiler: &'a LspProjectCompiler<A>,
    pattern_variable_under_cursor: Option<(&'a EcoString, PatternLocation, Arc<Type>)>,
    selected_value: Option<PatternMatchedValue<'a>>,
    edits: TextEdits<'a>,
}

/// A value we might want to pattern match on.
/// Each variant will also contain all the info needed to know how to properly
/// print and format the corresponding pattern matching code; that's why you'll
/// see `Range`s and `SrcSpan` besides the type of the thing being matched.
///
#[derive(Clone)]
pub enum PatternMatchedValue<'a> {
    FunctionArgument {
        /// The argument being pattern matched on.
        ///
        arg: &'a TypedArg,
        /// The first statement inside the function body. Used to correctly
        /// position the inserted pattern matching.
        ///
        first_statement: &'a TypedStatement,
        /// The range of the entire function holding the argument.
        ///
        function_range: Range,
    },
    LetVariable {
        variable_name: &'a EcoString,
        variable_type: Arc<Type>,
        /// The location of the entire let assignment the variable is part of,
        /// so that we can add the pattern matching _after_ it.
        ///
        assignment_location: SrcSpan,
    },
    /// A variable that is bound in a case branch's pattern. For example:
    /// ```gleam
    /// case wibble {
    ///   wobble -> 1
    /// // ^^^^^ This!
    /// }
    /// ```
    ///
    ClausePatternVariable {
        variable_type: Arc<Type>,
        variable_location: PatternLocation,
        clause_location: SrcSpan,
        /// All the names in the clause that are already taken by pattern variables.
        /// We need this to avoid generating invalid code were two pattern variables
        /// have the same name.
        ///
        /// For example:
        ///
        /// ```gleam
        /// case wibble {
        ///   [first, ..rest] -> todo
        ///    ^^^^^ When expanding `first` we can't add any variable pattern
        ///          called `rest` as it would clash with the `rest` tail that is
        ///          already there.
        /// }
        /// ```
        ///
        bound_variables: Vec<BoundVariable>,
    },
    UseVariable {
        variable_name: &'a EcoString,
        variable_type: Arc<Type>,
        /// The location of the entire use expression the variable is part of,
        /// so that we can add the pattern matching _after_ it.
        ///
        use_location: SrcSpan,
    },
}

#[derive(Clone)]
pub enum PatternLocation {
    /// Any pattern that doesn't need any special handling.
    ///
    Regular { location: SrcSpan },
    /// List tails need some care to not generate invalid syntax when pattern
    /// matched on in case expressions.
    ///
    ListTail {
        /// This location covers the entire list tail pattern, including the `..`
        location: SrcSpan,
    },
}

impl PatternLocation {
    fn regular(location: SrcSpan) -> Self {
        Self::Regular { location }
    }
}

impl<'a, IO> PatternMatchOnValue<'a, IO> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
        compiler: &'a LspProjectCompiler<IO>,
    ) -> Self {
        Self {
            module,
            params,
            compiler,
            selected_value: None,
            pattern_variable_under_cursor: None,
            edits: TextEdits::new(line_numbers),
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let action_title = match self.selected_value.clone() {
            Some(PatternMatchedValue::FunctionArgument {
                arg,
                first_statement: function_body,
                function_range,
            }) => {
                self.match_on_function_argument(arg, function_body, function_range);
                "Pattern match on argument"
            }
            Some(
                PatternMatchedValue::LetVariable {
                    variable_name,
                    variable_type,
                    assignment_location: location,
                }
                | PatternMatchedValue::UseVariable {
                    variable_name,
                    variable_type,
                    use_location: location,
                },
            ) => {
                self.match_on_let_variable(variable_name, variable_type, location);
                "Pattern match on variable"
            }

            Some(PatternMatchedValue::ClausePatternVariable {
                variable_type,
                variable_location,
                clause_location,
                bound_variables,
            }) => {
                self.match_on_clause_variable(
                    variable_type,
                    variable_location,
                    clause_location,
                    &bound_variables,
                );
                "Pattern match on variable"
            }

            None => return vec![],
        };

        if self.edits.edits.is_empty() {
            return vec![];
        }

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new(action_title)
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }

    fn match_on_function_argument(
        &mut self,
        arg: &TypedArg,
        first_statement: &TypedStatement,
        function_range: Range,
    ) {
        let Some(arg_name) = arg.get_variable_name() else {
            return;
        };

        let Some(patterns) =
            self.type_to_destructure_patterns(arg.type_.as_ref(), &mut NameGenerator::new())
        else {
            return;
        };

        let first_statement_location = first_statement.location();
        let first_statement_range = self.edits.src_span_to_lsp_range(first_statement_location);

        // If we're trying to insert the pattern matching on the same
        // line as the one where the function is defined we will want to
        // put it on a new line instead. So in that case the nesting will
        // be the default 2 spaces.
        let needs_newline = function_range.start.line == first_statement_range.start.line;
        let nesting = if needs_newline {
            String::from("  ")
        } else {
            " ".repeat(first_statement_range.start.character as usize)
        };

        let pattern_matching = if patterns.len() == 1 {
            let pattern = patterns.first();
            format!("let {pattern} = {arg_name}")
        } else {
            let patterns = patterns
                .iter()
                .map(|p| format!("  {nesting}{p} -> todo"))
                .join("\n");
            format!("case {arg_name} {{\n{patterns}\n{nesting}}}")
        };

        let pattern_matching = if needs_newline {
            format!("\n{nesting}{pattern_matching}")
        } else {
            pattern_matching
        };

        let has_empty_body = match first_statement {
            ast::Statement::Expression(TypedExpr::Todo {
                kind: TodoKind::EmptyFunction { .. },
                ..
            }) => true,
            ast::Statement::Expression(_)
            | ast::Statement::Assignment(_)
            | ast::Statement::Use(_)
            | ast::Statement::Assert(_) => false,
        };

        // If the pattern matching is added to a function with an empty
        // body then we do not add any nesting after it, or we would be
        // increasing the nesting of the closing `}`!
        let pattern_matching = if has_empty_body {
            format!("{pattern_matching}\n")
        } else {
            format!("{pattern_matching}\n{nesting}")
        };

        self.edits
            .insert(first_statement_location.start, pattern_matching);
    }

    fn match_on_let_variable(
        &mut self,
        variable_name: &EcoString,
        variable_type: Arc<Type>,
        assignment_location: SrcSpan,
    ) {
        let Some(patterns) =
            self.type_to_destructure_patterns(variable_type.as_ref(), &mut NameGenerator::new())
        else {
            return;
        };

        let assignment_range = self.edits.src_span_to_lsp_range(assignment_location);
        let nesting = " ".repeat(assignment_range.start.character as usize);

        let pattern_matching = if patterns.len() == 1 {
            let pattern = patterns.first();
            format!("let {pattern} = {variable_name}")
        } else {
            let patterns = patterns
                .iter()
                .map(|p| format!("  {nesting}{p} -> todo"))
                .join("\n");
            format!("case {variable_name} {{\n{patterns}\n{nesting}}}")
        };

        self.edits.insert(
            assignment_location.end,
            format!("\n{nesting}{pattern_matching}"),
        );
    }

    fn match_on_clause_variable(
        &mut self,
        variable_type: Arc<Type>,
        variable_location: PatternLocation,
        clause_location: SrcSpan,
        bound_variables: &[BoundVariable],
    ) {
        let mut names = NameGenerator::new();
        names.reserve_bound_variables(bound_variables);

        let patterns = if matches!(variable_location, PatternLocation::ListTail { .. }) {
            // Here we're dealing with a special case: if someone wants to expand the tail
            // of a list we can't just replace it with the usual list patterns `[]`, `[first, ..rest]`.
            // That would result in invalid syntax. So we have to generate list patterns
            // that have no square brackets.
            let first = names.rename_to_avoid_shadowing("first".into());
            let rest = names.rename_to_avoid_shadowing("rest".into());
            vec1!["".into(), eco_format!("{first}, ..{rest}")]
        } else if let Some(patterns) =
            self.type_to_destructure_patterns(variable_type.as_ref(), &mut names)
        {
            patterns
        } else {
            return;
        };

        let clause_range = self.edits.src_span_to_lsp_range(clause_location);
        let nesting = " ".repeat(clause_range.start.character as usize);

        let variable_location = match variable_location {
            PatternLocation::Regular { location } => location,
            PatternLocation::ListTail { location } => location,
        };

        let variable_start = (variable_location.start - clause_location.start) as usize;
        let variable_end = variable_start + variable_location.len();

        let clause_code = code_at(self.module, clause_location);
        let patterns = patterns
            .iter()
            .map(|pattern| {
                let mut clause_code = clause_code.to_string();
                // If we're replacing a variable that's using the shorthand
                // syntax we want to add a space to separate it from the
                // preceding `:`.
                let pattern = if variable_start == variable_end {
                    &eco_format!(" {pattern}")
                } else {
                    pattern
                };

                clause_code.replace_range(variable_start..variable_end, pattern);
                clause_code
            })
            .join(&format!("\n{nesting}"));

        self.edits.replace(clause_location, patterns);
    }

    /// Will produce a pattern that can be used on the left hand side of a let
    /// assignment to destructure a value of the given type. For example given
    /// this type:
    ///
    /// ```gleam
    /// pub type Wibble {
    ///   Wobble(Int, label: String)
    /// }
    /// ```
    ///
    /// The produced pattern will look like this: `Wobble(value_0, label:)`.
    /// The pattern will use the correct qualified/unqualified name for the
    /// constructor if it comes from another package.
    ///
    /// The function will only produce a list of patterns that can be used from
    /// the current module. So if the type comes from another module it must be
    /// public! Otherwise this function will return an empty vec.
    ///
    fn type_to_destructure_patterns(
        &mut self,
        type_: &Type,
        names: &mut NameGenerator,
    ) -> Option<Vec1<EcoString>> {
        match type_ {
            Type::Fn { .. } => None,
            Type::Var { type_ } => self.type_var_to_destructure_patterns(&type_.borrow(), names),

            // We special case lists, they don't have "regular" constructors
            // like other types. Instead we always add the two clauses covering
            // the empty and non empty list.
            Type::Named { .. } if type_.is_list() => {
                let first = names.rename_to_avoid_shadowing("first".into());
                let rest = names.rename_to_avoid_shadowing("rest".into());
                Some(vec1![
                    EcoString::from("[]"),
                    eco_format!("[{first}, ..{rest}]")
                ])
            }

            Type::Named {
                module: type_module,
                name: type_name,
                ..
            } => {
                let mut patterns = vec![];
                let constructors =
                    get_type_constructors(self.compiler, &self.module.name, type_module, type_name);
                for constructor in constructors {
                    let names_before = names.clone();
                    if let Some(pattern) =
                        self.record_constructor_to_destructure_pattern(constructor, names)
                    {
                        patterns.push(pattern);
                    }
                    *names = names_before;
                }

                Vec1::try_from_vec(patterns).ok()
            }

            // We don't want to suggest this action for empty tuple as it
            // doesn't make a lot of sense to match on those.
            Type::Tuple { elements } if elements.is_empty() => None,
            Type::Tuple { elements } => {
                let elements = elements
                    .iter()
                    .map(|element| names.generate_name_from_type(element))
                    .join(", ");
                Some(vec1![eco_format!("#({elements})")])
            }
        }
    }

    fn type_var_to_destructure_patterns(
        &mut self,
        type_var: &TypeVar,
        names: &mut NameGenerator,
    ) -> Option<Vec1<EcoString>> {
        match type_var {
            TypeVar::Unbound { .. } | TypeVar::Generic { .. } => None,
            TypeVar::Link { type_ } => self.type_to_destructure_patterns(type_, names),
        }
    }

    /// Given the value constructor of a record, returns a string with the
    /// pattern used to match on that specific variant.
    ///
    /// Note how:
    /// - If the constructor is internal to another module or comes from another
    ///   module, then this returns `None` since one cannot pattern match on it.
    /// - If the provided `ValueConstructor` is not a record constructor this
    ///   will return `None`.
    ///
    fn record_constructor_to_destructure_pattern(
        &self,
        constructor: &ValueConstructor,
        names: &mut NameGenerator,
    ) -> Option<EcoString> {
        let type_::ValueConstructorVariant::Record {
            name: constructor_name,
            arity: constructor_arity,
            module: constructor_module,
            field_map,
            ..
        } = &constructor.variant
        else {
            // The constructor should always be a record, in case it's not
            // there's not much we can do and just fail.
            return None;
        };

        // Since the constructor is a record constructor we know that its type
        // is either `Named` or a `Fn` type, in either case we have to get the
        // arguments types out of it.
        let Some(arguments_types) = constructor
            .type_
            .fn_types()
            .map(|(arguments_types, _return)| arguments_types)
            .or_else(|| constructor.type_.constructor_types())
        else {
            // This should never happen but just in case we don't want to unwrap
            // and panic.
            return None;
        };

        let index_to_label = match field_map {
            None => HashMap::new(),
            Some(field_map) => {
                names.reserve_all_labels(field_map);

                field_map
                    .fields
                    .iter()
                    .map(|(label, index)| (index, label))
                    .collect::<HashMap<_, _>>()
            }
        };

        let mut pattern =
            pretty_constructor_name(self.module, constructor_module, constructor_name)?;

        if *constructor_arity == 0 {
            return Some(pattern);
        }

        pattern.push('(');
        let arguments = (0..*constructor_arity as u32)
            .map(|i| match index_to_label.get(&i) {
                Some(label) => eco_format!("{label}:"),
                None => match arguments_types.get(i as usize) {
                    None => names.rename_to_avoid_shadowing(EcoString::from("value")),
                    Some(type_) => names.generate_name_from_type(type_),
                },
            })
            .join(", ");

        pattern.push_str(&arguments);
        pattern.push(')');
        Some(pattern)
    }
}

fn code_at(module: &Module, span: SrcSpan) -> &str {
    module
        .code
        .get(span.start as usize..span.end as usize)
        .expect("code location must be valid")
}

impl<'ast, IO> ast::visit::Visit<'ast> for PatternMatchOnValue<'ast, IO> {
    fn visit_typed_function(&mut self, fun: &'ast TypedFunction) {
        // If we're not inside the function there's no point in exploring its
        // ast further.
        let function_span = SrcSpan {
            start: fun.location.start,
            end: fun.end_position,
        };
        let function_range = self.edits.src_span_to_lsp_range(function_span);
        if !within(self.params.range, function_range) {
            return;
        }

        for arg in &fun.arguments {
            // If the cursor is placed on one of the arguments, then we can try
            // and generate code for that one.
            let arg_range = self.edits.src_span_to_lsp_range(arg.location);
            if within(self.params.range, arg_range)
                && let Some(first_statement) = fun.body.first()
            {
                self.selected_value = Some(PatternMatchedValue::FunctionArgument {
                    arg,
                    first_statement,
                    function_range,
                });
                return;
            }
        }

        // If the cursor is not on any of the function arguments then we keep
        // exploring the function body as we might want to destructure the
        // argument of an expression function!
        ast::visit::visit_typed_function(self, fun);
    }

    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        arguments: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        // If we're not inside the function there's no point in exploring its
        // ast further.
        let function_range = self.edits.src_span_to_lsp_range(*location);
        if !within(self.params.range, function_range) {
            return;
        }

        for argument in arguments {
            // If the cursor is placed on one of the arguments, then we can try
            // and generate code for that one.
            let arg_range = self.edits.src_span_to_lsp_range(argument.location);
            if within(self.params.range, arg_range) {
                self.selected_value = Some(PatternMatchedValue::FunctionArgument {
                    arg: argument,
                    first_statement: body.first(),
                    function_range,
                });
                return;
            }
        }

        // If the cursor is not on any of the function arguments then we keep
        // exploring the function body as we might want to destructure the
        // argument of an expression function!
        ast::visit::visit_typed_expr_fn(
            self,
            location,
            type_,
            kind,
            arguments,
            body,
            return_annotation,
        );
    }

    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        // If we're not inside the assignment there's no point in exploring its
        // ast further.
        let assignment_range = self.edits.src_span_to_lsp_range(assignment.location);
        if !within(self.params.range, assignment_range) {
            return;
        }

        ast::visit::visit_typed_assignment(self, assignment);
        if let Some((name, _, ref type_)) = self.pattern_variable_under_cursor {
            self.selected_value = Some(PatternMatchedValue::LetVariable {
                variable_name: name,
                variable_type: type_.clone(),
                assignment_location: assignment.location,
            });
        }
    }

    fn visit_typed_clause(&mut self, clause: &'ast ast::TypedClause) {
        // If we're not inside the clause there's no point in exploring its
        // ast further.
        let clause_range = self.edits.src_span_to_lsp_range(clause.location);
        if !within(self.params.range, clause_range) {
            return;
        }

        for pattern in clause.pattern.iter() {
            self.visit_typed_pattern(pattern);
        }
        for patterns in clause.alternative_patterns.iter() {
            for pattern in patterns {
                self.visit_typed_pattern(pattern);
            }
        }

        if let Some((_, variable_location, type_)) = self.pattern_variable_under_cursor.take() {
            self.selected_value = Some(PatternMatchedValue::ClausePatternVariable {
                variable_type: type_,
                variable_location,
                clause_location: clause.location(),
                bound_variables: clause.bound_variables().collect_vec(),
            });
        } else {
            self.visit_typed_expr(&clause.then);
        }
    }

    fn visit_typed_use(&mut self, use_: &'ast TypedUse) {
        if let Some(assignments) = use_.callback_arguments() {
            for variable in assignments {
                let ast::Arg {
                    names: ArgNames::Named { name, .. },
                    location: variable_location,
                    type_,
                    ..
                } = variable
                else {
                    continue;
                };

                // If we use a pattern in a use assignment, that will end up
                // being called `_use` something. We don't want to offer the
                // action when hovering a pattern so we ignore those.
                if name.starts_with("_use") {
                    continue;
                }

                let variable_range = self.edits.src_span_to_lsp_range(*variable_location);
                if within(self.params.range, variable_range) {
                    self.selected_value = Some(PatternMatchedValue::UseVariable {
                        variable_name: name,
                        variable_type: type_.clone(),
                        use_location: use_.location,
                    });
                    // If we've found the variable to pattern match on, there's no
                    // point in keeping traversing the AST.
                    return;
                }
            }
        }

        ast::visit::visit_typed_use(self, use_);
    }

    fn visit_typed_pattern_variable(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        type_: &'ast Arc<Type>,
        _origin: &'ast VariableOrigin,
    ) {
        if within(
            self.params.range,
            self.edits.src_span_to_lsp_range(*location),
        ) {
            let location = PatternLocation::regular(*location);
            self.pattern_variable_under_cursor = Some((name, location, type_.clone()));
        }
    }

    fn visit_typed_pattern_call_arg(&mut self, arg: &'ast CallArg<TypedPattern>) {
        if let Some(name) = arg.label_shorthand_name()
            && within(
                self.params.range,
                self.edits.src_span_to_lsp_range(arg.location),
            )
        {
            let location = PatternLocation::regular(SrcSpan {
                start: arg.location.end,
                end: arg.location.end,
            });
            self.pattern_variable_under_cursor = Some((name, location, arg.value.type_()));
            return;
        }

        ast::visit::visit_typed_pattern_call_arg(self, arg);
    }

    fn visit_typed_pattern_string_prefix(
        &mut self,
        _location: &'ast SrcSpan,
        _left_location: &'ast SrcSpan,
        left_side_assignment: &'ast Option<(EcoString, SrcSpan)>,
        right_location: &'ast SrcSpan,
        _left_side_string: &'ast EcoString,
        right_side_assignment: &'ast AssignName,
    ) {
        if let Some((name, location)) = left_side_assignment
            && within(
                self.params.range,
                self.edits.src_span_to_lsp_range(*location),
            )
        {
            let location = PatternLocation::regular(*location);
            self.pattern_variable_under_cursor = Some((name, location, type_::string()));
        } else if let AssignName::Variable(name) = right_side_assignment
            && within(
                self.params.range,
                self.edits.src_span_to_lsp_range(*right_location),
            )
        {
            let location = PatternLocation::regular(*right_location);
            self.pattern_variable_under_cursor = Some((name, location, type_::string()));
        }
    }

    fn visit_typed_pattern_list(
        &mut self,
        location: &'ast SrcSpan,
        elements: &'ast Vec<TypedPattern>,
        tail: &'ast Option<Box<TypedTailPattern>>,
        type_: &'ast Arc<Type>,
    ) {
        let (name, tail_location, tail_type) = if let Some(tail) = tail
            && let Pattern::Variable { name, type_, .. } = &tail.pattern
        {
            (name, tail.location, type_)
        } else {
            ast::visit::visit_typed_pattern_list(self, location, elements, tail, type_);
            return;
        };

        let tail_range = self.edits.src_span_to_lsp_range(tail_location);
        if !within(self.params.range, tail_range) {
            ast::visit::visit_typed_pattern_list(self, location, elements, tail, type_);
            return;
        }

        let location = PatternLocation::ListTail {
            location: tail_location,
        };
        self.pattern_variable_under_cursor = Some((name, location, tail_type.clone()))
    }
}

/// Given a type and its module, returns a list of its *importable*
/// constructors.
///
/// Since this focuses just on importable constructors, if either the module or
/// the type are internal the returned array will be empty!
///
fn get_type_constructors<'a, 'b, IO>(
    compiler: &'a LspProjectCompiler<IO>,
    current_module: &'b EcoString,
    type_module: &'b EcoString,
    type_name: &'b EcoString,
) -> Vec<&'a ValueConstructor> {
    let type_is_inside_current_module = current_module == type_module;
    let module_interface = if !type_is_inside_current_module {
        // If the type is outside of the module we're in, we can only pattern
        // match on it if the module can be imported.
        // The `get_module_interface` already takes care of making this check.
        compiler.get_module_interface(type_module)
    } else {
        // However, if the type is defined in the module we're in, we can always
        // pattern match on it. So we get the current module's interface.
        compiler
            .modules
            .get(current_module)
            .map(|module| &module.ast.type_info)
    };

    let Some(module_interface) = module_interface else {
        return vec![];
    };

    // If the type is in an internal module that is not the current one, we
    // cannot use its constructors!
    if !type_is_inside_current_module && module_interface.is_internal {
        return vec![];
    }

    let Some(constructors) = module_interface.types_value_constructors.get(type_name) else {
        return vec![];
    };

    constructors
        .variants
        .iter()
        .filter_map(|variant| {
            let constructor = module_interface.values.get(&variant.name)?;
            if type_is_inside_current_module || constructor.publicity.is_public() {
                Some(constructor)
            } else {
                None
            }
        })
        .collect_vec()
}

/// Returns a pretty printed record constructor name, the way it would be used
/// inside the given `module` (with the correct name and qualification).
///
/// If the constructor cannot be used inside the module because it's not
/// imported, then this function will return `None`.
///
fn pretty_constructor_name(
    module: &Module,
    constructor_module: &EcoString,
    constructor_name: &EcoString,
) -> Option<EcoString> {
    match module
        .ast
        .names
        .named_constructor(constructor_module, constructor_name)
    {
        type_::printer::NameContextInformation::Unimported(_, _) => None,
        type_::printer::NameContextInformation::Unqualified(constructor_name) => {
            Some(eco_format!("{constructor_name}"))
        }
        type_::printer::NameContextInformation::Qualified(module_name, constructor_name) => {
            Some(eco_format!("{module_name}.{constructor_name}"))
        }
    }
}

/// Builder for the "generate function" code action.
/// Whenever someone hovers an invalid expression that is inferred to have a
/// function type the language server can generate a function definition for it.
/// For example:
///
/// ```gleam
/// pub fn main() {
///   wibble(1, 2, "hello")
///  //  ^ [generate function]
/// }
/// ```
///
/// Will generate the following definition:
///
/// ```gleam
/// pub fn wibble(arg_0: Int, arg_1: Int, arg_2: String) -> a {
///   todo
/// }
/// ```
///
pub struct GenerateFunction<'a> {
    module: &'a Module,
    modules: &'a std::collections::HashMap<EcoString, Module>,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    last_visited_definition_end: Option<u32>,
    function_to_generate: Option<FunctionToGenerate<'a>>,
}

struct FunctionToGenerate<'a> {
    module: Option<&'a str>,
    name: &'a str,
    arguments_types: Vec<Arc<Type>>,

    /// The arguments actually supplied as input to the function, if any.
    /// A function to generate might as well be just a name passed as an argument
    /// `list.map([1, 2, 3], to_generate)` so it's not guaranteed to actually
    /// have any actual arguments!
    given_arguments: Option<&'a [TypedCallArg]>,
    return_type: Arc<Type>,
    previous_definition_end: Option<u32>,
}

impl<'a> GenerateFunction<'a> {
    pub fn new(
        module: &'a Module,
        modules: &'a std::collections::HashMap<EcoString, Module>,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            modules,
            params,
            edits: TextEdits::new(line_numbers),
            last_visited_definition_end: None,
            function_to_generate: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(
            function_to_generate @ FunctionToGenerate {
                module,
                previous_definition_end: Some(insert_at),
                ..
            },
        ) = self.function_to_generate.take()
        else {
            return vec![];
        };

        if let Some(module) = module {
            if let Some(module) = self.modules.get(module) {
                let insert_at = module.code.len() as u32;
                self.code_action_for_module(
                    module,
                    Publicity::Public,
                    function_to_generate,
                    insert_at,
                )
            } else {
                Vec::new()
            }
        } else {
            let module = self.module;
            self.code_action_for_module(module, Publicity::Private, function_to_generate, insert_at)
        }
    }

    fn code_action_for_module(
        mut self,
        module: &'a Module,
        publicity: Publicity,
        function_to_generate: FunctionToGenerate<'a>,
        insert_at: u32,
    ) -> Vec<CodeAction> {
        let FunctionToGenerate {
            name,
            arguments_types,
            given_arguments,
            return_type,
            ..
        } = function_to_generate;

        // This might be triggered on variants as well, in that case we don't
        // want to offer this action. The "generate variant" action will be
        // offered instead.
        if !is_valid_lowercase_name(name) {
            return vec![];
        }

        // Labels do not share the same namespace as argument so we use two
        // separate generators to avoid renaming a label in case it shares a
        // name with an argument.
        let mut label_names = NameGenerator::new();
        let mut argument_names = NameGenerator::new();

        // Since we are generating a new function, type variables from other
        // functions and constants are irrelevant to the types we print.
        let mut printer = Printer::new_without_type_variables(&module.ast.names);
        let arguments = arguments_types
            .iter()
            .enumerate()
            .map(|(index, argument_type)| {
                let call_argument = given_arguments.and_then(|arguments| arguments.get(index));
                let (label, name) =
                    argument_names.generate_label_and_name(call_argument, argument_type);
                let pretty_type = printer.print_type(argument_type);
                if let Some(label) = label {
                    let label = label_names.rename_to_avoid_shadowing(label.clone());
                    format!("{label} {name}: {pretty_type}")
                } else {
                    format!("{name}: {pretty_type}")
                }
            })
            .join(", ");

        let return_type = printer.print_type(&return_type);

        let publicity = if publicity.is_public() { "pub " } else { "" };

        // Make sure we use the line number information of the module we are
        // editing, which might not be the module where the code action is
        // triggered.
        self.edits.line_numbers = &module.ast.type_info.line_numbers;
        self.edits.insert(
            insert_at,
            format!("\n\n{publicity}fn {name}({arguments}) -> {return_type} {{\n  todo\n}}"),
        );

        let Some(uri) = url_from_path(module.input_path.as_str()) else {
            return Vec::new();
        };
        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Generate function")
            .kind(CodeActionKind::QUICKFIX)
            .changes(uri, self.edits.edits)
            .preferred(true)
            .push_to(&mut action);
        action
    }

    fn try_save_function_to_generate(
        &mut self,
        name: &'a EcoString,
        function_type: &Arc<Type>,
        given_arguments: Option<&'a [TypedCallArg]>,
    ) {
        match function_type.fn_types() {
            None => {}
            Some((arguments_types, return_type)) => {
                self.function_to_generate = Some(FunctionToGenerate {
                    name,
                    arguments_types,
                    given_arguments,
                    return_type,
                    previous_definition_end: self.last_visited_definition_end,
                    module: None,
                })
            }
        }
    }

    fn try_save_function_from_other_module(
        &mut self,
        module: &'a str,
        name: &'a str,
        function_type: &Arc<Type>,
        given_arguments: Option<&'a [TypedCallArg]>,
    ) {
        if let Some((arguments_types, return_type)) = function_type.fn_types()
            && is_valid_lowercase_name(name)
        {
            self.function_to_generate = Some(FunctionToGenerate {
                name,
                arguments_types,
                given_arguments,
                return_type,
                previous_definition_end: self.last_visited_definition_end,
                module: Some(module),
            })
        }
    }
}

impl<'ast> ast::visit::Visit<'ast> for GenerateFunction<'ast> {
    fn visit_typed_function(&mut self, fun: &'ast TypedFunction) {
        self.last_visited_definition_end = Some(fun.end_position);
        ast::visit::visit_typed_function(self, fun);
    }

    fn visit_typed_module_constant(&mut self, constant: &'ast TypedModuleConstant) {
        self.last_visited_definition_end = Some(constant.value.location().end);
        ast::visit::visit_typed_module_constant(self, constant);
    }

    fn visit_typed_expr_invalid(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        extra_information: &'ast Option<InvalidExpression>,
    ) {
        let invalid_range = self.edits.src_span_to_lsp_range(*location);
        if within(self.params.range, invalid_range) {
            match extra_information {
                Some(InvalidExpression::ModuleSelect { module_name, label }) => {
                    self.try_save_function_from_other_module(module_name, label, type_, None)
                }
                Some(InvalidExpression::UnknownVariable { name }) => {
                    self.try_save_function_to_generate(name, type_, None)
                }
                None => {}
            }
        }

        ast::visit::visit_typed_expr_invalid(self, location, type_, extra_information);
    }

    fn visit_typed_constant_invalid(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        extra_information: &'ast Option<InvalidExpression>,
    ) {
        let constant_range = self.edits.src_span_to_lsp_range(*location);
        if let Some(extra_information) = extra_information
            && within(self.params.range, constant_range)
        {
            match extra_information {
                InvalidExpression::ModuleSelect { module_name, label } => {
                    self.try_save_function_from_other_module(module_name, label, type_, None)
                }
                InvalidExpression::UnknownVariable { name } => {
                    self.try_save_function_to_generate(name, type_, None)
                }
            }
        }
    }

    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        arguments: &'ast [TypedCallArg],
    ) {
        // If the function being called is invalid we need to generate a
        // function that has the proper labels.
        let fun_range = self.edits.src_span_to_lsp_range(fun.location());

        if within(self.params.range, fun_range) {
            if !labels_are_correct(arguments) {
                return;
            }

            match fun {
                TypedExpr::Invalid {
                    type_,
                    extra_information: Some(InvalidExpression::ModuleSelect { module_name, label }),
                    location: _,
                } => {
                    return self.try_save_function_from_other_module(
                        module_name,
                        label,
                        type_,
                        Some(arguments),
                    );
                }
                TypedExpr::Invalid {
                    type_,
                    extra_information: Some(InvalidExpression::UnknownVariable { name }),
                    location: _,
                } => {
                    return self.try_save_function_to_generate(name, type_, Some(arguments));
                }
                TypedExpr::Int { .. }
                | TypedExpr::Float { .. }
                | TypedExpr::String { .. }
                | TypedExpr::Block { .. }
                | TypedExpr::Pipeline { .. }
                | TypedExpr::Var { .. }
                | TypedExpr::Fn { .. }
                | TypedExpr::List { .. }
                | TypedExpr::Call { .. }
                | TypedExpr::BinOp { .. }
                | TypedExpr::Case { .. }
                | TypedExpr::RecordAccess { .. }
                | TypedExpr::PositionalAccess { .. }
                | TypedExpr::ModuleSelect { .. }
                | TypedExpr::Tuple { .. }
                | TypedExpr::TupleIndex { .. }
                | TypedExpr::Todo { .. }
                | TypedExpr::Panic { .. }
                | TypedExpr::Echo { .. }
                | TypedExpr::BitArray { .. }
                | TypedExpr::RecordUpdate { .. }
                | TypedExpr::NegateBool { .. }
                | TypedExpr::NegateInt { .. }
                | TypedExpr::Invalid { .. } => {}
            }
        }

        ast::visit::visit_typed_expr_call(self, location, type_, fun, arguments);
    }
}

/// Builder for the "generate variant" code action. This will generate a variant
/// for a type if it can tell the type it should come from. It will work with
/// non-existing variants both used as expressions
///
/// ```gleam
/// let a = IDoNotExist(1)
/// //      ^^^^^^^^^^^ It would generate this variant here
/// ```
///
/// And as patterns:
///
/// ```gleam
/// let assert IDoNotExist(1) = todo
///            ^^^^^^^^^^^ It would generate this variant here
/// ```
///
pub struct GenerateVariant<'a, IO> {
    module: &'a Module,
    compiler: &'a LspProjectCompiler<FileSystemProxy<IO>>,
    params: &'a CodeActionParams,
    line_numbers: &'a LineNumbers,
    variant_to_generate: Option<VariantToGenerate<'a>>,
}

struct VariantToGenerate<'a> {
    name: &'a str,
    end_position: u32,
    arguments_types: Vec<Arc<Type>>,

    /// Wether the type we're adding the variant to is written with braces or
    /// not. We need this information to add braces when missing.
    ///
    type_braces: TypeBraces,

    /// The module this variant will be added to.
    ///
    module_name: EcoString,

    /// The arguments actually supplied as input to the variant, if any.
    /// A variant to generate might as well be just a name passed as an argument
    /// `list.map([1, 2, 3], ToGenerate)` so it's not guaranteed to actually
    /// have any actual arguments!
    ///
    given_arguments: Option<Arguments<'a>>,
}

#[derive(Debug, Clone, Copy)]
enum TypeBraces {
    /// If the type is written like this: `pub type Wibble`
    HasBraces,
    /// If the type is written like this: `pub type Wibble {}`
    NoBraces,
}

/// The arguments to an invalid call or pattern we can use to generate a variant.
///
enum Arguments<'a> {
    /// These are the arguments provided to the invalid variant constructor
    /// when it's used as a function: `let a = Wibble(1, 2)`.
    ///
    Expressions(&'a [TypedCallArg]),
    /// These are the arguments provided to the invalid variant constructor when
    /// it's used in a pattern: `let assert Wibble(1, 2) = a`
    ///
    Patterns(&'a [CallArg<TypedPattern>]),
}

/// An invalid variant might be used both as a pattern in a case expression or
/// as a regular value in an expression. We want to generate the variant in both
/// cases, so we use this enum to tell apart the two cases and be able to reuse
/// most of the code for both as they are very similar.
///
enum Argument<'a> {
    Expression(&'a TypedCallArg),
    Pattern(&'a CallArg<TypedPattern>),
}

impl<'a> Arguments<'a> {
    fn get(&self, index: usize) -> Option<Argument<'a>> {
        match self {
            Arguments::Patterns(call_arguments) => call_arguments.get(index).map(Argument::Pattern),
            Arguments::Expressions(call_arguments) => {
                call_arguments.get(index).map(Argument::Expression)
            }
        }
    }

    fn types(&self) -> Vec<Arc<Type>> {
        match self {
            Arguments::Expressions(call_arguments) => call_arguments
                .iter()
                .map(|argument| argument.value.type_())
                .collect_vec(),

            Arguments::Patterns(call_arguments) => call_arguments
                .iter()
                .map(|argument| argument.value.type_())
                .collect_vec(),
        }
    }
}

impl Argument<'_> {
    fn label(&self) -> Option<EcoString> {
        match self {
            Argument::Expression(call_arg) => call_arg.label.clone(),
            Argument::Pattern(call_arg) => call_arg.label.clone(),
        }
    }
}

impl<'a, IO> GenerateVariant<'a, IO> {
    pub fn new(
        module: &'a Module,
        compiler: &'a LspProjectCompiler<FileSystemProxy<IO>>,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            compiler,
            line_numbers,
            variant_to_generate: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(VariantToGenerate {
            name,
            arguments_types,
            given_arguments,
            module_name,
            end_position,
            type_braces,
        }) = &self.variant_to_generate
        else {
            return vec![];
        };

        let Some((variant_module, variant_edits)) = self.edits_to_create_variant(
            name,
            arguments_types,
            given_arguments,
            module_name,
            *end_position,
            *type_braces,
        ) else {
            return vec![];
        };

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Generate variant")
            .kind(CodeActionKind::QUICKFIX)
            .changes(variant_module, variant_edits)
            .preferred(true)
            .push_to(&mut action);
        action
    }

    /// Returns the edits needed to add this new variant to the given module.
    /// It also returns the uri of the module the edits should be applied to.
    ///
    fn edits_to_create_variant(
        &self,
        variant_name: &str,
        arguments_types: &[Arc<Type>],
        given_arguments: &Option<Arguments<'_>>,
        module_name: &EcoString,
        end_position: u32,
        type_braces: TypeBraces,
    ) -> Option<(Url, Vec<TextEdit>)> {
        let mut label_names = NameGenerator::new();
        let mut printer = Printer::new(&self.module.ast.names);
        let arguments = arguments_types
            .iter()
            .enumerate()
            .map(|(index, argument_type)| {
                let label = given_arguments
                    .as_ref()
                    .and_then(|arguments| arguments.get(index)?.label())
                    .map(|label| label_names.rename_to_avoid_shadowing(label));

                let pretty_type = printer.print_type(argument_type);
                if let Some(arg_label) = label {
                    format!("{arg_label}: {pretty_type}")
                } else {
                    format!("{pretty_type}")
                }
            })
            .join(", ");

        let variant = if arguments.is_empty() {
            variant_name.to_string()
        } else {
            format!("{variant_name}({arguments})")
        };

        let (new_text, insert_at) = match type_braces {
            TypeBraces::HasBraces => (format!("  {variant}\n"), end_position - 1),
            TypeBraces::NoBraces => (format!(" {{\n  {variant}\n}}"), end_position),
        };

        if *module_name == self.module.name {
            // If we're editing the current module we can use the line numbers that
            // were already computed before-hand without wasting any time to add the
            // new edit.
            let mut edits = TextEdits::new(self.line_numbers);
            edits.insert(insert_at, new_text);
            Some((self.params.text_document.uri.clone(), edits.edits))
        } else {
            // Otherwise we're changing a different module and we need to get its
            // code and line numbers to properly apply the new edit.
            let module = self
                .compiler
                .modules
                .get(module_name)
                .expect("module to exist");
            let line_numbers = LineNumbers::new(&module.code);
            let mut edits = TextEdits::new(&line_numbers);
            edits.insert(insert_at, new_text);
            Some((url_from_path(module.input_path.as_str())?, edits.edits))
        }
    }

    fn try_save_variant_to_generate(
        &mut self,
        function_name_location: SrcSpan,
        function_type: &Arc<Type>,
        given_arguments: Option<Arguments<'a>>,
    ) {
        let variant_to_generate =
            self.variant_to_generate(function_name_location, function_type, given_arguments);
        if variant_to_generate.is_some() {
            self.variant_to_generate = variant_to_generate;
        }
    }

    fn variant_to_generate(
        &mut self,
        function_name_location: SrcSpan,
        type_: &Arc<Type>,
        given_arguments: Option<Arguments<'a>>,
    ) -> Option<VariantToGenerate<'a>> {
        let name = code_at(self.module, function_name_location);
        if !is_valid_uppercase_name(name) {
            return None;
        }

        let (arguments_types, custom_type) = match (type_.fn_types(), &given_arguments) {
            (Some(result), _) => result,
            (None, Some(arguments)) => (arguments.types(), type_.clone()),
            (None, None) => (vec![], type_.clone()),
        };

        let (module_name, type_name, _) = custom_type.named_type_information()?;
        let module = self.compiler.modules.get(&module_name)?;
        let (end_position, type_braces) = (module.ast.definitions.custom_types.iter())
            .filter(|custom_type| custom_type.name == type_name)
            .find_map(|custom_type| {
                // If there's already a variant with this name then we definitely
                // don't want to generate a new variant with the same name!
                let variant_with_this_name_already_exists = custom_type
                    .constructors
                    .iter()
                    .map(|constructor| &constructor.name)
                    .any(|existing_constructor_name| existing_constructor_name == name);
                if variant_with_this_name_already_exists {
                    return None;
                }
                let type_braces = if custom_type.end_position == custom_type.location.end {
                    TypeBraces::NoBraces
                } else {
                    TypeBraces::HasBraces
                };
                Some((custom_type.end_position, type_braces))
            })?;

        Some(VariantToGenerate {
            name,
            arguments_types,
            given_arguments,
            module_name,
            end_position,
            type_braces,
        })
    }
}

impl<'ast, IO> ast::visit::Visit<'ast> for GenerateVariant<'ast, IO> {
    fn visit_typed_expr_invalid(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        extra_information: &'ast Option<InvalidExpression>,
    ) {
        let invalid_range = src_span_to_lsp_range(*location, self.line_numbers);
        if within(self.params.range, invalid_range) {
            self.try_save_variant_to_generate(*location, type_, None);
        }
        ast::visit::visit_typed_expr_invalid(self, location, type_, extra_information);
    }

    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        arguments: &'ast [TypedCallArg],
    ) {
        // If the function being called is invalid we need to generate a
        // function that has the proper labels.
        let fun_range = src_span_to_lsp_range(fun.location(), self.line_numbers);
        if within(self.params.range, fun_range) && fun.is_invalid() {
            if labels_are_correct(arguments) {
                self.try_save_variant_to_generate(
                    fun.location(),
                    &fun.type_(),
                    Some(Arguments::Expressions(arguments)),
                );
            }
        } else {
            ast::visit::visit_typed_expr_call(self, location, type_, fun, arguments);
        }
    }

    fn visit_typed_pattern_invalid(&mut self, location: &'ast SrcSpan, type_: &'ast Arc<Type>) {
        let invalid_range = src_span_to_lsp_range(*location, self.line_numbers);
        if within(self.params.range, invalid_range) {
            self.try_save_variant_to_generate(*location, type_, None);
        }
        ast::visit::visit_typed_pattern_invalid(self, location, type_);
    }

    fn visit_typed_pattern_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name_location: &'ast SrcSpan,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<TypedPattern>>,
        module: &'ast Option<(EcoString, SrcSpan)>,
        constructor: &'ast Inferred<type_::PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        let pattern_range = src_span_to_lsp_range(*location, self.line_numbers);
        if within(self.params.range, pattern_range) {
            if labels_are_correct(arguments) {
                self.try_save_variant_to_generate(
                    *name_location,
                    type_,
                    Some(Arguments::Patterns(arguments)),
                );
            }
        } else {
            ast::visit::visit_typed_pattern_constructor(
                self,
                location,
                name_location,
                name,
                arguments,
                module,
                constructor,
                spread,
                type_,
            );
        }
    }
}

#[must_use]
/// Checks the labels in the given arguments are correct: that is there's no
/// duplicate labels and all labelled arguments come after the unlabelled ones.
fn labels_are_correct<A>(arguments: &[CallArg<A>]) -> bool {
    let mut labelled_arg_found = false;
    let mut used_labels = HashSet::new();

    for argument in arguments {
        match &argument.label {
            // Labels are invalid if there's duplicate ones or if an unlabelled
            // argument comes after a labelled one.
            Some(label) if used_labels.contains(label) => return false,
            None if labelled_arg_found => return false,
            // Otherwise we just add the label to the used ones.
            Some(label) => {
                labelled_arg_found = true;
                let _ = used_labels.insert(label);
            }
            None => {}
        }
    }

    true
}

#[derive(Clone)]
struct NameGenerator {
    used_names: HashSet<EcoString>,
}

impl NameGenerator {
    pub fn new() -> Self {
        NameGenerator {
            used_names: HashSet::new(),
        }
    }

    pub fn rename_to_avoid_shadowing(&mut self, base: EcoString) -> EcoString {
        let mut i = 1;
        let mut candidate_name = base.clone();

        loop {
            if self.used_names.contains(&candidate_name) {
                i += 1;
                candidate_name = eco_format!("{base}_{i}");
            } else {
                let _ = self.used_names.insert(candidate_name.clone());
                return candidate_name;
            }
        }
    }

    /// Given an argument type and the actual call argument (if any), comes up
    /// with a label and a name to use for that argument when generating a
    /// function.
    ///
    pub fn generate_label_and_name(
        &mut self,
        call_argument: Option<&CallArg<TypedExpr>>,
        argument_type: &Arc<Type>,
    ) -> (Option<EcoString>, EcoString) {
        let label = call_argument.and_then(|argument| argument.label.clone());
        let argument_name = call_argument
            // We always favour a name derived from the expression (for example if
            // the argument is a variable)
            .and_then(|argument| self.generate_name_from_expression(&argument.value))
            // If we don't have such a name and there's a label we use that name.
            .or_else(|| Some(self.rename_to_avoid_shadowing(label.clone()?)))
            // If all else fails we fallback to using a name derived from the
            // argument's type.
            .unwrap_or_else(|| self.generate_name_from_type(argument_type));

        (label, argument_name)
    }

    pub fn generate_name_from_type(&mut self, type_: &Arc<Type>) -> EcoString {
        let type_to_base_name = |type_: &Arc<Type>| {
            type_
                .named_type_name()
                .map(|(_type_module, type_name)| to_snake_case(&type_name))
                .filter(|name| is_valid_lowercase_name(name))
                .unwrap_or(EcoString::from("value"))
        };

        let base_name = match type_.list_type() {
            None => type_to_base_name(type_),
            // If we're coming up with a name for a list we want to use the
            // plural form for the name of the inner type. For example:
            // `List(Pokemon)` should generate `pokemons`.
            Some(inner_type) => {
                let base_name = type_to_base_name(&inner_type);
                // If the inner type name already ends in "s" we leave it as it
                // is, or it would look funny.
                if base_name.ends_with('s') {
                    base_name
                } else {
                    eco_format!("{base_name}s")
                }
            }
        };

        self.rename_to_avoid_shadowing(base_name)
    }

    fn generate_name_from_expression(&mut self, expression: &TypedExpr) -> Option<EcoString> {
        match expression {
            // If the argument is a record, we can't use it as an argument name.
            // Similarly, we don't want to base the variable name off a
            // compiler-generated variable like `_pipe`.
            TypedExpr::Var {
                name, constructor, ..
            } if !constructor.variant.is_record()
                && !constructor.variant.is_generated_variable() =>
            {
                Some(self.rename_to_avoid_shadowing(name.clone()))
            }

            // If the argument is a record access, we generate a name from the
            // label used.
            // For example if we have `wibble.id` we would end up picking `id`.
            TypedExpr::RecordAccess { label, .. } => {
                Some(self.rename_to_avoid_shadowing(label.clone()))
            }

            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => None,
        }
    }

    /// Given some typed definitions this reserves all the value names defined
    /// by all the top level definitions. That is: all function names, constant
    /// names, and imported modules names.
    pub fn reserve_module_value_names(&mut self, definitions: &TypedDefinitions) {
        for constant in &definitions.constants {
            self.add_used_name(constant.name.clone());
        }

        for function in &definitions.functions {
            if let Some((_, name)) = &function.name {
                self.add_used_name(name.clone());
            }
        }

        for import in &definitions.imports {
            let module_name = match &import.used_name() {
                Some(used_name) => used_name.clone(),
                None => import.module.clone(),
            };
            self.add_used_name(module_name);
        }
    }

    pub fn add_used_name(&mut self, name: EcoString) {
        let _ = self.used_names.insert(name);
    }

    pub fn reserve_all_labels(&mut self, field_map: &FieldMap) {
        field_map
            .fields
            .iter()
            .for_each(|(label, _)| self.add_used_name(label.clone()));
    }

    pub fn reserve_variable_names(&mut self, variable_names: VariablesNames) {
        variable_names
            .names
            .iter()
            .for_each(|name| self.add_used_name(name.clone()));
    }

    fn reserve_bound_variables(&mut self, bound_variables: &[BoundVariable]) {
        for variable in bound_variables {
            self.add_used_name(variable.name());
        }
    }
}

#[must_use]
fn is_valid_lowercase_name(name: &str) -> bool {
    if !name.starts_with(|char: char| char.is_ascii_lowercase()) {
        return false;
    }

    for char in name.chars() {
        let is_valid_char = char.is_ascii_digit() || char.is_ascii_lowercase() || char == '_';
        if !is_valid_char {
            return false;
        }
    }

    str_to_keyword(name).is_none()
}

#[must_use]
fn is_valid_uppercase_name(name: &str) -> bool {
    if !name.starts_with(|char: char| char.is_ascii_uppercase()) {
        return false;
    }

    for char in name.chars() {
        if !char.is_ascii_alphanumeric() {
            return false;
        }
    }

    true
}

/// Code action to rewrite a single-step pipeline into a regular function call.
/// For example: `a |> b(c, _)` would be rewritten as `b(c, a)`.
///
pub struct ConvertToFunctionCall<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    locations: Option<ConvertToFunctionCallLocations>,
}

/// All the different locations the "Convert to function call" code action needs
/// to properly rewrite a pipeline into a function call.
///
struct ConvertToFunctionCallLocations {
    /// This is the location of the value being piped into a call.
    ///
    /// ```gleam
    ///    [1, 2, 3] |> list.length
    /// // ^^^^^^^^^ This one here
    /// ```
    ///
    first_value: SrcSpan,

    /// This is the location of the call the value is being piped into.
    ///
    /// ```gleam
    ///    [1, 2, 3] |> list.length
    /// //              ^^^^^^^^^^^ This one here
    /// ```
    ///
    call: SrcSpan,

    /// This is the kind of desugaring that is taking place when piping
    /// `first_value` into `call`.
    ///
    call_kind: PipelineAssignmentKind,
}

impl<'a> ConvertToFunctionCall<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            locations: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        // If we couldn't find a pipeline to rewrite we don't return any action.
        let Some(ConvertToFunctionCallLocations {
            first_value,
            call,
            call_kind,
        }) = self.locations
        else {
            return vec![];
        };

        // We first delete the first value of the pipeline as it's going to be
        // inlined as a function call argument.
        self.edits.delete(SrcSpan {
            start: first_value.start,
            end: call.start,
        });

        // Then we have to insert the piped value in the appropriate position.
        // This will change based on how the pipeline is being desugared, we
        // know this thanks to the `call_kind`
        let first_value_text = self
            .module
            .code
            .get(first_value.start as usize..first_value.end as usize)
            .expect("invalid code span")
            .to_string();

        match call_kind {
            // When piping into a `_` we replace the hole with the piped value:
            // `[1, 2] |> map(_, todo)` becomes `map([1, 2], todo)`.
            PipelineAssignmentKind::Hole { hole } => self.edits.replace(hole, first_value_text),

            // When piping is desguared as a function call we need to add the
            // missing parentheses:
            // `[1, 2] |> length` becomes `length([1, 2])`
            PipelineAssignmentKind::FunctionCall => {
                self.edits.insert(call.end, format!("({first_value_text})"))
            }

            // When the piped value is inserted as the first argument there's two
            // possible scenarios:
            // - there's a second argument as well: in that case we insert it
            //   before the second arg and add a comma
            // - there's no other argument: `[1, 2] |> length()` becomes
            //   `length([1, 2])`, we insert the value between the empty
            //   parentheses
            PipelineAssignmentKind::FirstArgument {
                second_argument: Some(SrcSpan { start, .. }),
            } => self.edits.insert(start, format!("{first_value_text}, ")),
            PipelineAssignmentKind::FirstArgument {
                second_argument: None,
            } => self.edits.insert(call.end - 1, first_value_text),

            // When the value is piped into an echo, to rewrite the pipeline we
            // have to insert the value after the `echo` with no parentheses:
            // `a |> echo` is rewritten as `echo a`.
            PipelineAssignmentKind::Echo => {
                self.edits.insert(call.end, format!(" {first_value_text}"))
            }
        }

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Convert to function call")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for ConvertToFunctionCall<'ast> {
    fn visit_typed_expr_pipeline(
        &mut self,
        location: &'ast SrcSpan,
        first_value: &'ast TypedPipelineAssignment,
        assignments: &'ast [(TypedPipelineAssignment, PipelineAssignmentKind)],
        finally: &'ast TypedExpr,
        finally_kind: &'ast PipelineAssignmentKind,
    ) {
        let pipeline_range = self.edits.src_span_to_lsp_range(*location);
        if within(self.params.range, pipeline_range) {
            // We will always desugar the pipeline's first step. If there's no
            // intermediate assignment it means we're dealing with a single step
            // pipeline and the call is `finally`.
            let (call, call_kind) = assignments
                .first()
                .map(|(call, kind)| (call.location, *kind))
                .unwrap_or_else(|| (finally.location(), *finally_kind));

            self.locations = Some(ConvertToFunctionCallLocations {
                first_value: first_value.location,
                call,
                call_kind,
            });

            ast::visit::visit_typed_expr_pipeline(
                self,
                location,
                first_value,
                assignments,
                finally,
                finally_kind,
            );
        }
    }
}

/// Builder for code action to inline a variable.
///
pub struct InlineVariable<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    actions: Vec<CodeAction>,
}

impl<'a> InlineVariable<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            actions: Vec::new(),
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        self.actions
    }

    fn maybe_inline(&mut self, location: SrcSpan, name: EcoString) {
        let references =
            FindVariableReferences::new(location, name).find_in_module(&self.module.ast);
        let reference = if references.len() == 1 {
            references
                .into_iter()
                .next()
                .expect("References has length 1")
        } else {
            return;
        };

        let Some(ast::Statement::Assignment(assignment)) =
            self.module.ast.find_statement(location.start)
        else {
            return;
        };

        // If the assignment does not simple bind a variable, for example:
        // ```gleam
        // let #(first, second, third)
        // io.println(first)
        // //         ^ Inline here
        // ```
        // We can't inline it.
        if !matches!(assignment.pattern, Pattern::Variable { .. }) {
            return;
        }

        // If the assignment was generated by the compiler, it doesn't have a
        // syntactical representation, so we can't inline it.
        if matches!(assignment.kind, AssignmentKind::Generated) {
            return;
        }

        let value_location = assignment.value.location();
        let value = self
            .module
            .code
            .get(value_location.start as usize..value_location.end as usize)
            .expect("Span is valid");

        match reference.kind {
            VariableReferenceKind::Variable => {
                self.edits.replace(reference.location, value.into());
            }
            VariableReferenceKind::LabelShorthand => {
                self.edits
                    .insert(reference.location.end, format!(" {value}"));
            }
        }

        let mut location = assignment.location;

        let mut chars = self.module.code[location.end as usize..].chars();
        // Delete any whitespace after the removed statement
        while chars.next().is_some_and(char::is_whitespace) {
            location.end += 1;
        }

        self.edits.delete(location);

        CodeActionBuilder::new("Inline variable")
            .kind(CodeActionKind::REFACTOR_INLINE)
            .changes(
                self.params.text_document.uri.clone(),
                std::mem::take(&mut self.edits.edits),
            )
            .preferred(false)
            .push_to(&mut self.actions);
    }
}

impl<'ast> ast::visit::Visit<'ast> for InlineVariable<'ast> {
    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        let TypedPattern::Variable { location, name, .. } = &assignment.pattern else {
            ast::visit::visit_typed_assignment(self, assignment);
            return;
        };

        // We special case assignment variables because we want to trigger the
        // code action also if we're over the let keyword:
        //
        // ```gleam
        //    let wibble = 11
        // // ^^^^^^^^^^ Here!
        // ```
        //
        let assignment_range = self
            .edits
            .src_span_to_lsp_range(SrcSpan::new(assignment.location.start, location.end));
        if !within(self.params.range, assignment_range) {
            ast::visit::visit_typed_assignment(self, assignment);
            return;
        }

        self.maybe_inline(*location, name.clone());
    }

    fn visit_typed_expr_var(
        &mut self,
        location: &'ast SrcSpan,
        constructor: &'ast ValueConstructor,
        name: &'ast EcoString,
    ) {
        let range = self.edits.src_span_to_lsp_range(*location);

        if !within(self.params.range, range) {
            return;
        }

        let type_::ValueConstructorVariant::LocalVariable { location, origin } =
            &constructor.variant
        else {
            return;
        };

        // We can only inline variables assigned by `let` statements, as it
        //doesn't make sense to do so with any other kind of variable.
        match origin.declaration {
            VariableDeclaration::LetPattern => {}
            VariableDeclaration::UsePattern
            | VariableDeclaration::ClausePattern
            | VariableDeclaration::FunctionParameter { .. }
            | VariableDeclaration::Generated => return,
        }

        self.maybe_inline(*location, name.clone());
    }

    fn visit_typed_pattern_variable(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        _type: &'ast Arc<Type>,
        origin: &'ast VariableOrigin,
    ) {
        // We can only inline variables assigned by `let` statements, as it
        //doesn't make sense to do so with any other kind of variable.
        match origin.declaration {
            VariableDeclaration::LetPattern => {}
            VariableDeclaration::UsePattern
            | VariableDeclaration::ClausePattern
            | VariableDeclaration::FunctionParameter { .. }
            | VariableDeclaration::Generated => return,
        }

        let range = self.edits.src_span_to_lsp_range(*location);

        if !within(self.params.range, range) {
            return;
        }

        self.maybe_inline(*location, name.clone());
    }
}

/// Builder for the "convert to pipe" code action.
///
/// ```gleam
/// pub fn main() {
///   wibble(wobble, woo)
///  //  ^ [convert to pipe]
/// }
/// ```
///
/// Will turn the code into the following pipeline:
///
/// ```gleam
/// pub fn main() {
///   wobble |> wibble(woo)
/// }
/// ```
///
pub struct ConvertToPipe<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    argument_to_pipe: Option<ConvertToPipeArg<'a>>,
    visited_item: VisitedItem,
}

pub enum VisitedItem {
    RegularExpression,
    UseRightHandSide,
    PipelineFinalStep,
}

/// Holds all the data needed by the "convert to pipe" code action to properly
/// rewrite a call into a pipe. Here's what each span means:
///
/// ```gleam
///    wibble(wobb|le, woo)
/// // ^^^^^^^^^^^^^^^^^^^^ call
/// // ^^^^^^ called
/// //        ^^^^^^^ arg
/// //                 ^^^ next arg
/// ```
///
/// In this example `position` is 0, since the cursor is over the first
/// argument.
///
pub struct ConvertToPipeArg<'a> {
    /// The span of the called function.
    called: SrcSpan,
    /// The span of the entire function call.
    call: SrcSpan,
    /// The position (0-based) of the argument.
    position: usize,
    /// The argument we have to pipe.
    arg: &'a TypedCallArg,
    /// The span of the argument following the one we have to pipe, if there's
    /// any.
    next_arg: Option<SrcSpan>,
}

impl<'a> ConvertToPipe<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            visited_item: VisitedItem::RegularExpression,
            argument_to_pipe: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(ConvertToPipeArg {
            called,
            call,
            position,
            arg,
            next_arg,
        }) = self.argument_to_pipe
        else {
            return vec![];
        };

        let arg_location = if arg.uses_label_shorthand() {
            SrcSpan {
                start: arg.location.start,
                end: arg.location.end - 1,
            }
        } else if arg.label.is_some() {
            arg.value.location()
        } else {
            arg.location
        };

        let arg_text = code_at(self.module, arg_location);
        // If the expression being piped is a binary operation with
        // precedence lower than pipes then we have to wrap it in curly
        // braces to not mess with the order of operations.
        let arg_text = if let TypedExpr::BinOp { name, .. } = arg.value
            && name.precedence() < PIPE_PRECEDENCE
        {
            &format!("{{ {arg_text} }}")
        } else {
            arg_text
        };

        match next_arg {
            // When extracting an argument we never want to remove any explicit
            // label that was written down, so in case it is labelled (be it a
            // shorthand or not) we'll always replace the value with a `_`
            _ if arg.uses_label_shorthand() => self.edits.insert(arg.location.end, " _".into()),
            _ if arg.label.is_some() => self.edits.replace(arg.value.location(), "_".into()),

            // Now we can deal with unlabelled arguments:
            // If we're removing the first argument and there's other arguments
            // after it, we need to delete the comma that was separating the
            // two.
            Some(next_arg) if position == 0 => self.edits.delete(SrcSpan {
                start: arg.location.start,
                end: next_arg.start,
            }),
            // Otherwise, if we're deleting the first argument and there's
            // no other arguments following it, we remove the call's
            // parentheses.
            None if position == 0 => self.edits.delete(SrcSpan {
                start: called.end,
                end: call.end,
            }),
            // In all other cases we're piping something that is not the first
            // argument so we just replace it with an `_`.
            _ => self.edits.replace(arg.location, "_".into()),
        };

        // Finally we can add the argument that was removed as the first step
        // of the newly defined pipeline.
        self.edits.insert(call.start, format!("{arg_text} |> "));

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Convert to pipe")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for ConvertToPipe<'ast> {
    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        _type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        arguments: &'ast [TypedCallArg],
    ) {
        if arguments.iter().any(|arg| arg.is_capture_hole()) {
            return;
        }

        // If we're visiting the typed function produced by typing a use, we
        // skip the thing itself and only visit its arguments and called
        // function, that is the body of the use.
        match self.visited_item {
            VisitedItem::RegularExpression => (),
            VisitedItem::UseRightHandSide | VisitedItem::PipelineFinalStep => {
                self.visited_item = VisitedItem::RegularExpression;
                ast::visit::visit_typed_expr(self, fun);
                arguments
                    .iter()
                    .for_each(|arg| ast::visit::visit_typed_call_arg(self, arg));
                return;
            }
        }

        // We only visit a call if the cursor is somewhere within its location,
        // otherwise we skip it entirely.
        let call_range = self.edits.src_span_to_lsp_range(*location);
        if !within(self.params.range, call_range) {
            return;
        }

        // If the cursor is over any of the arguments then we'll use that as
        // the one to extract.
        // Otherwise the cursor must be over the called function, in that case
        // we extract the first argument (if there's one):
        //
        // ```gleam
        //    wibble(wobble, woo)
        // // ^^^^^^^^^^^^^ pipe the first argument if I'm here
        // //                ^^^ pipe the second argument if I'm here
        // ```
        let argument_to_pipe = arguments
            .iter()
            .enumerate()
            .find_map(|(position, arg)| {
                let arg_range = self.edits.src_span_to_lsp_range(arg.location);
                if within(self.params.range, arg_range) {
                    Some((position, arg))
                } else {
                    None
                }
            })
            .or_else(|| arguments.first().map(|argument| (0, argument)));

        // If we're not hovering over any of the arguments _or_ there's no
        // argument to extract at all we just return, there's nothing we can do
        // on this call or any of its arguments (since we've determined the
        // cursor is not over any of those).
        let Some((position, arg)) = argument_to_pipe else {
            return;
        };

        self.argument_to_pipe = Some(ConvertToPipeArg {
            called: fun.location(),
            call: *location,
            position,
            arg,
            next_arg: arguments
                .get(position + 1)
                .map(|argument| argument.location),
        });

        // We still want to visit the arguments so that if we're hovering a
        // nested pipeline, that's going to be the one we transform:
        //
        // ```gleam
        // wibble(Wobble(
        //   field: call(other(last(1)))
        //  //      ^^^^ We want to convert this one if we hover over it,
        //  //           not the outer `wibble(Wobble(...))` call
        // ))
        // ```
        //
        for argument in arguments {
            ast::visit::visit_typed_call_arg(self, argument);
        }
    }

    fn visit_typed_expr_pipeline(
        &mut self,
        _location: &'ast SrcSpan,
        first_value: &'ast TypedPipelineAssignment,
        _assignments: &'ast [(TypedPipelineAssignment, PipelineAssignmentKind)],
        finally: &'ast TypedExpr,
        _finally_kind: &'ast PipelineAssignmentKind,
    ) {
        // We can only apply the action on the first step of a pipeline, so we
        // visit just that one and skip all the others.
        ast::visit::visit_typed_pipeline_assignment(self, first_value);
        self.visited_item = VisitedItem::PipelineFinalStep;
        ast::visit::visit_typed_expr(self, finally);
    }

    fn visit_typed_use(&mut self, use_: &'ast TypedUse) {
        self.visited_item = VisitedItem::UseRightHandSide;
        ast::visit::visit_typed_use(self, use_);
    }
}

/// Code action to interpolate a string. If the cursor is inside the string
/// (not selecting anything) the language server will offer to split it:
///
/// ```gleam
/// "wibble | wobble"
/// //      ^ [Split string]
/// // Will produce the following
/// "wibble " <> todo <> " wobble"
/// ```
///
/// If the cursor is selecting an entire valid gleam name, then the language
/// server will offer to interpolate it as a variable:
///
/// ```gleam
/// "wibble wobble woo"
/// //      ^^^^^^ [Interpolate variable]
/// // Will produce the following
/// "wibble " <> wobble <> " woo"
/// ```
///
/// > Note: the cursor won't end up right after the inserted variable/todo.
/// > that's a bit annoying, but in a future LSP version we will be able to
/// > isnert tab stops to allow one to jump to the newly added variable/todo.
///
pub struct InterpolateString<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    string_interpolation: Option<(SrcSpan, StringInterpolation)>,
    string_literal_position: StringLiteralPosition,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum StringLiteralPosition {
    FirstPipelineStep,
    Other,
}

#[derive(Clone, Copy)]
enum StringInterpolation {
    InterpolateValue { value_location: SrcSpan },
    SplitString { split_at: u32 },
}

impl<'a> InterpolateString<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            string_interpolation: None,
            string_literal_position: StringLiteralPosition::Other,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some((string_location, interpolation)) = self.string_interpolation else {
            return vec![];
        };

        if self.string_literal_position == StringLiteralPosition::FirstPipelineStep {
            self.edits.insert(string_location.start, "{ ".into());
        }

        match interpolation {
            StringInterpolation::InterpolateValue { value_location } => {
                let name = self
                    .module
                    .code
                    .get(value_location.start as usize..value_location.end as usize)
                    .expect("invalid value range");

                // We trust that the programmer has correctly selected the part
                // of the string they want to interpolate and simply "cut it out"
                // for them. In future, we could try and parse their selection to
                // see if it is a valid expression in Gleam.
                if value_location.start == string_location.start + 1 {
                    self.edits
                        .insert(string_location.start, format!("{name} <> "));
                } else if value_location.end == string_location.end - 1 {
                    self.edits
                        .insert(string_location.end, format!(" <> {name}"));
                } else {
                    self.edits
                        .insert(value_location.start, format!("\" <> {name} <> \""));
                }
                self.edits.delete(value_location);
            }

            StringInterpolation::SplitString { split_at } if self.can_split_string_at(split_at) => {
                self.edits.insert(split_at, "\" <> todo <> \"".into());
            }

            StringInterpolation::SplitString { .. } => return vec![],
        };

        if self.string_literal_position == StringLiteralPosition::FirstPipelineStep {
            self.edits.insert(string_location.end, " }".into());
        }

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Interpolate string")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }

    fn can_split_string_at(&self, at: u32) -> bool {
        self.string_interpolation
            .is_some_and(|(string_location, _)| {
                !(at <= string_location.start + 1 || at >= string_location.end - 1)
            })
    }

    fn visit_literal_string(
        &mut self,
        string_location: SrcSpan,
        string_position: StringLiteralPosition,
    ) {
        // We can only interpolate/split a string if the cursor is somewhere
        // within its location, otherwise we skip it.
        let string_range = self.edits.src_span_to_lsp_range(string_location);
        if !within(self.params.range, string_range) {
            return;
        }

        let selection @ SrcSpan { start, end } =
            self.edits.lsp_range_to_src_span(self.params.range);

        // We can't interpolate/split if the double quotes delimiting the
        // string have been selected.
        if start == string_location.start || end == string_location.end {
            return;
        }

        let name = self
            .module
            .code
            .get(start as usize..end as usize)
            .expect("invalid value range");

        // TUI editors like Helix and Kakoune that use the selection-action edit
        // model are always in the equivalent of Vim's VISUAL mode, i.e. they always
        // have something selected. For programmers using these editors, the
        // smallest selection possible is a 1-character selection. The best we can do
        // to provide parity with other editors is to consider a single-character SPACE
        // selection as an empty selection, as they most likely want to split the
        // string instead of interpolating a variable.
        let interpolation = if start == end || (end - start == 1 && name == " ") {
            StringInterpolation::SplitString { split_at: start }
        } else {
            StringInterpolation::InterpolateValue {
                value_location: selection,
            }
        };
        self.string_interpolation = Some((string_location, interpolation));
        self.string_literal_position = string_position;
    }
}

impl<'ast> ast::visit::Visit<'ast> for InterpolateString<'ast> {
    fn visit_typed_expr_string(
        &mut self,
        location: &'ast SrcSpan,
        _type_: &'ast Arc<Type>,
        _value: &'ast EcoString,
    ) {
        self.visit_literal_string(*location, StringLiteralPosition::Other);
    }

    fn visit_typed_expr_pipeline(
        &mut self,
        _location: &'ast SrcSpan,
        first_value: &'ast TypedPipelineAssignment,
        assignments: &'ast [(TypedPipelineAssignment, PipelineAssignmentKind)],
        finally: &'ast TypedExpr,
        _finally_kind: &'ast PipelineAssignmentKind,
    ) {
        if first_value.value.is_literal_string() {
            self.visit_literal_string(
                first_value.location,
                StringLiteralPosition::FirstPipelineStep,
            );
        } else {
            ast::visit::visit_typed_pipeline_assignment(self, first_value);
        }

        assignments
            .iter()
            .for_each(|(a, _)| ast::visit::visit_typed_pipeline_assignment(self, a));
        self.visit_typed_expr(finally);
    }
}

/// Code action to replace a `..` in a pattern with all the missing fields that
/// have not been explicitly provided; labelled ones are introduced with the
/// shorthand syntax.
///
/// ```gleam
/// pub type Pokemon {
///   Pokemon(Int, name: String, moves: List(String))
/// }
///
/// pub fn main() {
///   let Pokemon(..) = todo
/// //            ^^ Cursor over the spread
/// }
/// ```
/// Would become
/// ```gleam
/// pub fn main() {
///   let Pokemon(int, name:, moves:) = todo
/// }
///
pub struct FillUnusedFields<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    data: Option<FillUnusedFieldsData>,
}

pub struct FillUnusedFieldsData {
    /// All the missing positional and labelled fields.
    positional: Vec<Arc<Type>>,
    labelled: Vec<(EcoString, Arc<Type>)>,
    /// We need this in order to tell where the missing positional arguments
    /// should be inserted.
    first_labelled_argument_start: Option<u32>,
    /// The end of the final argument before the spread, if there's any.
    /// We'll use this to delete everything that comes after the final argument,
    /// after adding all the ignored fields.
    last_argument_end: Option<u32>,
    spread_location: SrcSpan,
}

impl<'a> FillUnusedFields<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            data: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(FillUnusedFieldsData {
            positional,
            labelled,
            first_labelled_argument_start,
            last_argument_end,
            spread_location,
        }) = self.data
        else {
            return vec![];
        };

        // Do not suggest this code action if there's no ignored fields at all.
        if positional.is_empty() && labelled.is_empty() {
            return vec![];
        };

        // We add all the missing positional arguments before the first
        // labelled one (and so after all the already existing positional ones).
        if !positional.is_empty() {
            // We want to make sure that all positional args will have a name
            // that's different from any label. So we add those as already used
            // names.
            let mut names = NameGenerator::new();
            for (label, _) in labelled.iter() {
                names.add_used_name(label.clone());
            }

            let positional_arguments = positional
                .iter()
                .map(|type_| names.generate_name_from_type(type_))
                .join(", ");
            let insert_at = first_labelled_argument_start.unwrap_or(spread_location.start);

            // The positional arguments are going to be followed by some other
            // arguments if there's some already existing labelled args
            // (`last_argument_end.is_some`), of if we're adding those labelled args
            // ourselves (`!labelled.is_empty()`). So we need to put a comma after the
            // final positional argument we're adding to separate it from the ones that
            // are going to come after.
            let has_arguments_after = last_argument_end.is_some() || !labelled.is_empty();
            let positional_arguments = if has_arguments_after {
                format!("{positional_arguments}, ")
            } else {
                positional_arguments
            };

            self.edits.insert(insert_at, positional_arguments);
        }

        if !labelled.is_empty() {
            // If there's labelled arguments to add, we replace the existing spread
            // with the arguments to be added. This way commas and all should already
            // be correct.
            let labelled_arguments = labelled
                .iter()
                .map(|(label, _)| format!("{label}:"))
                .join(", ");
            self.edits.replace(spread_location, labelled_arguments);
        } else if let Some(delete_start) = last_argument_end {
            // However, if there's no labelled arguments to insert we still need
            // to delete the entire spread: we start deleting from the end of the
            // final argument, if there's one.
            // This way we also get rid of any comma separating the last argument
            // and the spread to be removed.
            self.edits
                .delete(SrcSpan::new(delete_start, spread_location.end))
        } else {
            // Otherwise we just delete the spread.
            self.edits.delete(spread_location)
        }

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Fill unused fields")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for FillUnusedFields<'ast> {
    fn visit_typed_pattern(&mut self, pattern: &'ast TypedPattern) {
        // We can only interpolate/split a string if the cursor is somewhere
        // within its location, otherwise we skip it.
        let pattern_range = self.edits.src_span_to_lsp_range(pattern.location());
        if !within(self.params.range, pattern_range) {
            return;
        }

        if let TypedPattern::Constructor {
            arguments,
            spread: Some(spread_location),
            ..
        } = pattern
            && let Some(PatternUnusedArguments {
                positional,
                labelled,
            }) = pattern.unused_arguments()
        {
            // If there's any unused argument that's being ignored we want to
            // suggest the code action.
            let first_labelled_argument_start = arguments
                .iter()
                .find(|arg| !arg.is_implicit() && arg.label.is_some())
                .map(|arg| arg.location.start);

            let last_argument_end = arguments
                .iter()
                .rfind(|arg| !arg.is_implicit())
                .map(|arg| arg.location.end);

            self.data = Some(FillUnusedFieldsData {
                positional,
                labelled,
                first_labelled_argument_start,
                last_argument_end,
                spread_location: *spread_location,
            });
        };

        ast::visit::visit_typed_pattern(self, pattern);
    }
}

/// Code action to remove an echo.
///
pub struct RemoveEchos<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    is_hovering_echo: bool,
    echo_spans_to_delete: Vec<SrcSpan>,
    // We need to keep a reference to the two latest pipeline assignments we
    // run into to properly delete an echo that's inside a pipeline.
    latest_pipe_step: Option<SrcSpan>,
    second_to_latest_pipe_step: Option<SrcSpan>,
}

impl<'a> RemoveEchos<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            is_hovering_echo: false,
            echo_spans_to_delete: vec![],
            latest_pipe_step: None,
            second_to_latest_pipe_step: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        // We only want to trigger the action if we're over one of the echos in
        // the module
        if !self.is_hovering_echo {
            return vec![];
        };

        for span in self.echo_spans_to_delete {
            self.edits.delete(span);
        }

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Remove all `echo`s from this module")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }

    fn visit_function_statements(&mut self, statements: &'a [TypedStatement]) {
        for i in 0..statements.len() {
            let statement = statements
                .get(i)
                .expect("Statement must exist in iteration");
            let next_statement = statements.get(i + 1);
            let is_last = i == statements.len() - 1;

            match statement {
                // We remove any echo that is used as a standalone statement used
                // to print a literal value.
                //
                // ```gleam
                // pub fn main() {
                //   echo "I'm here"
                //   do_something()
                //   echo "Safe!"
                //   do_something_else()
                // }
                // ```
                //
                // Here we want to remove not just the echo but also the literal
                // strings they're printing.
                //
                // It's safe to do this only if echo is not the last expression
                // in a function's block (otherwise we might change the function's
                // return type by removing the entire line) and the value being
                // printed is a literal expression.
                //
                ast::Statement::Expression(TypedExpr::Echo {
                    location,
                    expression,
                    ..
                }) if !is_last
                    && expression.as_ref().is_some_and(|expression| {
                        expression.is_literal() || expression.is_var()
                    }) =>
                {
                    let echo_range = self.edits.src_span_to_lsp_range(*location);
                    if within(self.params.range, echo_range) {
                        self.is_hovering_echo = true;
                    }

                    let end = next_statement
                        .map(|next| {
                            let echo_end = location.end;
                            let next_start = next.location().start;
                            // We want to remove everything until the start of the
                            // following statement. However, we have to be careful not to
                            // delete any comments. So if there's any comment between the
                            // echo to remove and the next statement, we just delete until
                            // the comment's start.
                            self.module
                                .extra
                                .first_comment_between(echo_end, next_start)
                                // For comments we record the start of their content, not of the `//`
                                // so we're subtracting 2 here to not delete the `//` as well
                                .map(|comment| comment.start - 2)
                                .unwrap_or(next_start)
                        })
                        .unwrap_or(location.end);

                    self.echo_spans_to_delete.push(SrcSpan {
                        start: location.start,
                        end,
                    });
                }

                // Otherwise we visit the statement as usual.
                ast::Statement::Expression(_)
                | ast::Statement::Assignment(_)
                | ast::Statement::Use(_)
                | ast::Statement::Assert(_) => ast::visit::visit_typed_statement(self, statement),
            }
        }
    }
}

impl<'ast> ast::visit::Visit<'ast> for RemoveEchos<'ast> {
    fn visit_typed_function(&mut self, fun: &'ast TypedFunction) {
        self.visit_function_statements(&fun.body);
    }

    fn visit_typed_expr_fn(
        &mut self,
        _location: &'ast SrcSpan,
        _type_: &'ast Arc<Type>,
        _kind: &'ast FunctionLiteralKind,
        _arguments: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        _return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        self.visit_function_statements(body);
    }

    fn visit_typed_expr_echo(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        expression: &'ast Option<Box<TypedExpr>>,
        message: &'ast Option<Box<TypedExpr>>,
    ) {
        // We also want to trigger the action if we're hovering over the expression
        // being printed. So we create a unique span starting from the start of echo
        // end ending at the end of the expression.
        //
        // ```
        // echo 1 + 2
        // ^^^^^^^^^^ This is `location`, we want to trigger the action if we're
        //            inside it, not just the keyword
        // ```
        //
        let echo_range = self.edits.src_span_to_lsp_range(*location);
        if within(self.params.range, echo_range) {
            self.is_hovering_echo = true;
        }

        // We also want to remove the echo message!
        if message.is_some() {
            let start = expression
                .as_ref()
                .map(|expression| expression.location().end)
                .unwrap_or(location.start + 4);

            self.echo_spans_to_delete
                .push(SrcSpan::new(start, location.end));
        }

        if let Some(expression) = expression {
            // If there's an expression we delete everything we find until its
            // start (excluded).
            let span_to_delete = SrcSpan::new(location.start, expression.location().start);
            self.echo_spans_to_delete.push(span_to_delete);
        } else {
            // Othwerise we know we're inside a pipeline, we take the closest step
            // that is not echo itself and delete everything from its end until the
            // end of the echo keyword:
            //
            // ```txt
            // wibble |> echo |> wobble
            //       ^^^^^^^^ This span right here
            // ```
            let step_preceding_echo = self
                .latest_pipe_step
                .filter(|l| l != location)
                .or(self.second_to_latest_pipe_step);
            if let Some(step_preceding_echo) = step_preceding_echo {
                let span_to_delete = SrcSpan::new(step_preceding_echo.end, location.start + 4);
                self.echo_spans_to_delete.push(span_to_delete);
            }
        }

        ast::visit::visit_typed_expr_echo(self, location, type_, expression, message);
    }

    fn visit_typed_pipeline_assignment(&mut self, assignment: &'ast TypedPipelineAssignment) {
        if self.latest_pipe_step.is_some() {
            self.second_to_latest_pipe_step = self.latest_pipe_step;
        }
        self.latest_pipe_step = Some(assignment.location);
        ast::visit::visit_typed_pipeline_assignment(self, assignment);
    }
}

/// Code action to wrap assignment and case clause values in a block.
///
/// ```gleam
/// pub type PokemonType {
///   Fire
///   Water
/// }
///
/// pub fn main() {
///   let pokemon_type: PokemonType = todo
///   case pokemon_type {
///     Water -> soak()
///              ^^^^^^ Cursor over the spread
///     Fire -> burn()
///   }
/// }
/// ```
/// Becomes
/// ```gleam
/// pub type PokemonType {
///   Fire
///   Water
/// }
///
/// pub fn main() {
///   let pokemon_type: PokemonType = todo
///   case pokemon_type {
///     Water -> {
///       soak()
///     }
///     Fire -> burn()
///   }
/// }
/// ```
///
pub struct WrapInBlock<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    selected_expression: Option<SrcSpan>,
}

impl<'a> WrapInBlock<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            selected_expression: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(expr_span) = self.selected_expression else {
            return vec![];
        };

        let Some(expr_string) = self
            .module
            .code
            .get(expr_span.start as usize..(expr_span.end as usize + 1))
        else {
            return vec![];
        };

        let range = self
            .edits
            .src_span_to_lsp_range(self.selected_expression.expect("Real range value"));

        let indent_size =
            count_indentation(&self.module.code, self.edits.line_numbers, range.start.line);

        let expr_indent_size = indent_size + 2;

        let indent = " ".repeat(indent_size);
        let inner_indent = " ".repeat(expr_indent_size);

        self.edits.replace(
            expr_span,
            format!("{{\n{inner_indent}{expr_string}{indent}}}"),
        );

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Wrap in block")
            .kind(CodeActionKind::REFACTOR_EXTRACT)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for WrapInBlock<'ast> {
    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        ast::visit::visit_typed_expr(self, &assignment.value);
        if !within(
            self.params.range,
            self.edits
                .src_span_to_lsp_range(assignment.value.location()),
        ) {
            return;
        }
        match &assignment.value {
            // To avoid wrapping the same expression in multiple, nested blocks.
            TypedExpr::Block { .. } => {}
            TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => {
                self.selected_expression = Some(assignment.value.location());
            }
        };
        ast::visit::visit_typed_assignment(self, assignment);
    }

    fn visit_typed_clause(&mut self, clause: &'ast ast::TypedClause) {
        ast::visit::visit_typed_clause(self, clause);

        if !within(
            self.params.range,
            self.edits.src_span_to_lsp_range(clause.then.location()),
        ) {
            return;
        }

        // To avoid wrapping the same expression in multiple, nested blocks.
        if !matches!(clause.then, TypedExpr::Block { .. }) {
            self.selected_expression = Some(clause.then.location());
        };

        ast::visit::visit_typed_clause(self, clause);
    }
}

/// Code action to fix wrong binary operators when the compiler can easily tell
/// what the correct alternative is.
///
/// ```gleam
/// 1 +. 2 // becomes 1 + 2
/// 1.0 + 2.3 // becomes 1.0 +. 2.3
/// ```
///
pub struct FixBinaryOperation<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    fix: Option<(SrcSpan, ast::BinOp)>,
}

impl<'a> FixBinaryOperation<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            fix: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some((location, replacement)) = self.fix else {
            return vec![];
        };

        self.edits.replace(location, replacement.name().into());

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new(&format!("Use `{}`", replacement.name()))
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(true)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for FixBinaryOperation<'ast> {
    fn visit_typed_expr_bin_op(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        name: &'ast ast::BinOp,
        name_location: &'ast SrcSpan,
        left: &'ast TypedExpr,
        right: &'ast TypedExpr,
    ) {
        let binop_range = self.edits.src_span_to_lsp_range(*location);
        if !within(self.params.range, binop_range) {
            return;
        }

        if name.is_int_operator() && left.type_().is_float() && right.type_().is_float() {
            self.fix = name.float_equivalent().map(|fix| (*name_location, fix));
        } else if name.is_float_operator() && left.type_().is_int() && right.type_().is_int() {
            self.fix = name.int_equivalent().map(|fix| (*name_location, fix))
        } else if *name == ast::BinOp::AddInt
            && left.type_().is_string()
            && right.type_().is_string()
        {
            self.fix = Some((*name_location, ast::BinOp::Concatenate))
        }

        ast::visit::visit_typed_expr_bin_op(
            self,
            location,
            type_,
            name,
            name_location,
            left,
            right,
        );
    }
}

/// Code action builder to automatically fix segments that have a value that's
/// guaranteed to overflow.
///
pub struct FixTruncatedBitArraySegment<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    truncation: Option<BitArraySegmentTruncation>,
}

impl<'a> FixTruncatedBitArraySegment<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            truncation: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(truncation) = self.truncation else {
            return vec![];
        };

        let replacement = truncation.truncated_into.to_string();
        self.edits
            .replace(truncation.value_location, replacement.clone());

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new(&format!("Replace with `{replacement}`"))
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(true)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for FixTruncatedBitArraySegment<'ast> {
    fn visit_typed_expr_bit_array_segment(&mut self, segment: &'ast ast::TypedExprBitArraySegment) {
        let segment_range = self.edits.src_span_to_lsp_range(segment.location);
        if !within(self.params.range, segment_range) {
            return;
        }

        if let Some(truncation) = segment.check_for_truncated_value() {
            self.truncation = Some(truncation);
        }

        ast::visit::visit_typed_expr_bit_array_segment(self, segment);
    }
}

/// Code action builder to remove unused imports and values.
///
pub struct RemoveUnusedImports<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
}

#[derive(Debug)]
enum UnusedImport {
    ValueOrType(SrcSpan),
    Module(SrcSpan),
    ModuleAlias(SrcSpan),
}

impl UnusedImport {
    fn location(&self) -> SrcSpan {
        match self {
            UnusedImport::ValueOrType(location)
            | UnusedImport::Module(location)
            | UnusedImport::ModuleAlias(location) => *location,
        }
    }
}

impl<'a> RemoveUnusedImports<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
        }
    }

    /// Given an import location, returns a list of the spans of all the
    /// unqualified values it's importing. Sorted by SrcSpan location.
    ///
    fn imported_values(&self, import_location: SrcSpan) -> Vec<SrcSpan> {
        self.module
            .ast
            .definitions
            .imports
            .iter()
            .find(|import| import.location.contains(import_location.start))
            .map(|import| {
                let types = import.unqualified_types.iter().map(|type_| type_.location);
                let values = import.unqualified_values.iter().map(|value| value.location);
                types
                    .chain(values)
                    .sorted_by_key(|location| location.start)
                    .collect_vec()
            })
            .unwrap_or_default()
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        // If there's no import in the module then there can't be any unused
        // import to remove.
        if self.module.ast.definitions.imports.is_empty() {
            return vec![];
        }

        let unused_imports = self
            .module
            .ast
            .type_info
            .warnings
            .iter()
            .filter_map(|warning| match warning {
                type_::Warning::UnusedImportedValue { location, .. } => {
                    Some(UnusedImport::ValueOrType(*location))
                }
                type_::Warning::UnusedType {
                    location,
                    imported: true,
                    ..
                } => Some(UnusedImport::ValueOrType(*location)),
                type_::Warning::UnusedImportedModule { location, .. } => {
                    Some(UnusedImport::Module(*location))
                }
                type_::Warning::UnusedImportedModuleAlias { location, .. } => {
                    Some(UnusedImport::ModuleAlias(*location))
                }
                type_::Warning::Todo { .. }
                | type_::Warning::ImplicitlyDiscardedResult { .. }
                | type_::Warning::UnusedLiteral { .. }
                | type_::Warning::UnusedValue { .. }
                | type_::Warning::NoFieldsRecordUpdate { .. }
                | type_::Warning::AllFieldsRecordUpdate { .. }
                | type_::Warning::UnusedType { .. }
                | type_::Warning::UnusedConstructor { .. }
                | type_::Warning::UnusedPrivateModuleConstant { .. }
                | type_::Warning::UnusedPrivateFunction { .. }
                | type_::Warning::UnusedVariable { .. }
                | type_::Warning::UnnecessaryDoubleIntNegation { .. }
                | type_::Warning::UnnecessaryDoubleBoolNegation { .. }
                | type_::Warning::InefficientEmptyListCheck { .. }
                | type_::Warning::TransitiveDependencyImported { .. }
                | type_::Warning::DeprecatedItem { .. }
                | type_::Warning::UnreachableCasePattern { .. }
                | type_::Warning::UnusedDiscardPattern { .. }
                | type_::Warning::CaseMatchOnLiteralCollection { .. }
                | type_::Warning::CaseMatchOnLiteralValue { .. }
                | type_::Warning::OpaqueExternalType { .. }
                | type_::Warning::InternalTypeLeak { .. }
                | type_::Warning::RedundantAssertAssignment { .. }
                | type_::Warning::AssertAssignmentOnImpossiblePattern { .. }
                | type_::Warning::TodoOrPanicUsedAsFunction { .. }
                | type_::Warning::UnreachableCodeAfterPanic { .. }
                | type_::Warning::RedundantPipeFunctionCapture { .. }
                | type_::Warning::FeatureRequiresHigherGleamVersion { .. }
                | type_::Warning::JavaScriptIntUnsafe { .. }
                | type_::Warning::AssertLiteralBool { .. }
                | type_::Warning::BitArraySegmentTruncatedValue { .. }
                | type_::Warning::ModuleImportedTwice { .. }
                | type_::Warning::TopLevelDefinitionShadowsImport { .. }
                | type_::Warning::RedundantComparison { .. }
                | type_::Warning::UnusedRecursiveArgument { .. } => None,
            })
            .sorted_by_key(|import| import.location())
            .collect_vec();

        // If the cursor is not over any of the unused imports then we don't offer
        // the code action.
        let hovering_unused_import = unused_imports.iter().any(|import| {
            let unused_range = self.edits.src_span_to_lsp_range(import.location());
            overlaps(self.params.range, unused_range)
        });
        if !hovering_unused_import {
            return vec![];
        }

        // Otherwise we start removing all unused imports:
        for import in &unused_imports {
            match import {
                // When an entire module is unused we can delete its entire location
                // in the source code.
                UnusedImport::Module(location) | UnusedImport::ModuleAlias(location) => {
                    if self.edits.line_numbers.spans_entire_line(location) {
                        // If the unused module spans over the entire line then
                        // we also take care of removing the following newline
                        // characther!
                        self.edits.delete(SrcSpan {
                            start: location.start,
                            end: location.end + 1,
                        })
                    } else {
                        self.edits.delete(*location)
                    }
                }

                // When removing unused imported values we have to be a bit more
                // careful: an unused value might be followed or preceded by a
                // comma that we also need to remove!
                UnusedImport::ValueOrType(location) => {
                    let imported = self.imported_values(*location);
                    let unused_index = imported.binary_search(location);
                    let is_last = unused_index.is_ok_and(|index| index == imported.len() - 1);
                    let next_value = unused_index
                        .ok()
                        .and_then(|value_index| imported.get(value_index + 1));
                    let previous_value = unused_index.ok().and_then(|value_index| {
                        value_index
                            .checked_sub(1)
                            .and_then(|previous_index| imported.get(previous_index))
                    });
                    let previous_is_unused = previous_value.is_some_and(|previous| {
                        unused_imports
                            .as_slice()
                            .binary_search_by_key(previous, |import| import.location())
                            .is_ok()
                    });

                    match (previous_value, next_value) {
                        // If there's a value following the unused import we need
                        // to remove all characters until its start!
                        //
                        // ```gleam
                        // import wibble.{unused,    used}
                        // //             ^^^^^^^^^^^ We need to remove all of this!
                        // ```
                        //
                        (_, Some(next_value)) => self.edits.delete(SrcSpan {
                            start: location.start,
                            end: next_value.start,
                        }),

                        // If this unused import is the last of the unuqualified
                        // list and is preceded by another used value then we
                        // need to do some additional cleanup and remove all
                        // characters starting from its end.
                        // (If the previous one is unused as well it will take
                        // care of removing all the extra space)
                        //
                        // ```gleam
                        // import wibble.{used,     unused}
                        // //                 ^^^^^^^^^^^^ We need to remove all of this!
                        // ```
                        //
                        (Some(previous_value), _) if is_last && !previous_is_unused => {
                            self.edits.delete(SrcSpan {
                                start: previous_value.end,
                                end: location.end,
                            })
                        }

                        // In all other cases it means that this is the only
                        // item in the import list. We can just remove it.
                        //
                        // ```gleam
                        // import wibble.{unused}
                        // //             ^^^^^^ We remove this import, the formatter will already
                        // //                    take care of removing the empty curly braces
                        // ```
                        //
                        (_, _) => self.edits.delete(*location),
                    }
                }
            }
        }

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Remove unused imports")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(true)
            .push_to(&mut action);
        action
    }
}

/// Code action to remove a block wrapping a single expression.
///
pub struct RemoveBlock<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    block_span: Option<SrcSpan>,
    position: RemoveBlockPosition,
}

#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd)]
enum RemoveBlockPosition {
    InsideBinOp,
    OutsideBinOp,
}

impl<'a> RemoveBlock<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            block_span: None,
            position: RemoveBlockPosition::OutsideBinOp,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(SrcSpan { start, end }) = self.block_span else {
            return vec![];
        };

        self.edits.delete(SrcSpan::new(start, start + 1));
        self.edits.delete(SrcSpan::new(end - 1, end));

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Remove block")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(true)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for RemoveBlock<'ast> {
    fn visit_typed_expr_bin_op(
        &mut self,
        _location: &'ast SrcSpan,
        _type_: &'ast Arc<Type>,
        _name: &'ast ast::BinOp,
        _name_location: &'ast SrcSpan,
        left: &'ast TypedExpr,
        right: &'ast TypedExpr,
    ) {
        let old_position = self.position;
        self.position = RemoveBlockPosition::InsideBinOp;
        ast::visit::visit_typed_expr(self, left);
        self.position = RemoveBlockPosition::InsideBinOp;
        ast::visit::visit_typed_expr(self, right);
        self.position = old_position;
    }

    fn visit_typed_expr_block(
        &mut self,
        location: &'ast SrcSpan,
        statements: &'ast [TypedStatement],
    ) {
        let block_range = self.edits.src_span_to_lsp_range(*location);
        if !within(self.params.range, block_range) {
            return;
        }

        match statements {
            [] | [_, _, ..] => (),
            [value] => match value {
                ast::Statement::Use(_)
                | ast::Statement::Assert(_)
                | ast::Statement::Assignment(_) => {
                    ast::visit::visit_typed_expr_block(self, location, statements)
                }

                ast::Statement::Expression(expr) => match expr {
                    TypedExpr::Int { .. }
                    | TypedExpr::Float { .. }
                    | TypedExpr::String { .. }
                    | TypedExpr::Block { .. }
                    | TypedExpr::Var { .. }
                    | TypedExpr::Fn { .. }
                    | TypedExpr::List { .. }
                    | TypedExpr::Call { .. }
                    | TypedExpr::Case { .. }
                    | TypedExpr::RecordAccess { .. }
                    | TypedExpr::PositionalAccess { .. }
                    | TypedExpr::ModuleSelect { .. }
                    | TypedExpr::Tuple { .. }
                    | TypedExpr::TupleIndex { .. }
                    | TypedExpr::Todo { .. }
                    | TypedExpr::Panic { .. }
                    | TypedExpr::Echo { .. }
                    | TypedExpr::BitArray { .. }
                    | TypedExpr::RecordUpdate { .. }
                    | TypedExpr::NegateBool { .. }
                    | TypedExpr::NegateInt { .. }
                    | TypedExpr::Invalid { .. } => {
                        self.block_span = Some(*location);
                    }
                    TypedExpr::BinOp { .. } | TypedExpr::Pipeline { .. } => {
                        if self.position == RemoveBlockPosition::OutsideBinOp {
                            self.block_span = Some(*location);
                        }
                    }
                },
            },
        }

        ast::visit::visit_typed_expr_block(self, location, statements);
    }
}

/// Code action to remove `opaque` from a private type.
///
pub struct RemovePrivateOpaque<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    opaque_span: Option<SrcSpan>,
}

impl<'a> RemovePrivateOpaque<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            opaque_span: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(opaque_span) = self.opaque_span else {
            return vec![];
        };

        self.edits.delete(opaque_span);

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Remove opaque from private type")
            .kind(CodeActionKind::QUICKFIX)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(true)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for RemovePrivateOpaque<'ast> {
    fn visit_typed_custom_type(&mut self, custom_type: &'ast ast::TypedCustomType) {
        let custom_type_range = self.edits.src_span_to_lsp_range(custom_type.location);
        if !within(self.params.range, custom_type_range) {
            return;
        }

        if custom_type.opaque && custom_type.publicity.is_private() {
            self.opaque_span = Some(SrcSpan {
                start: custom_type.location.start,
                end: custom_type.location.start + 7,
            })
        }
    }
}

/// Code action to rewrite a case expression as part of an outer case expression
/// branch. For example:
///
/// ```gleam
/// case wibble {
///   Ok(a) -> case a {
///     1 -> todo
///     _ -> todo
///   }
///   Error(_) -> todo
/// }
/// ```
///
/// Would become:
///
/// ```gleam
/// case wibble {
///   Ok(1) -> todo
///   Ok(_) -> todo
///   Error(_) -> todo
/// }
/// ```
///
pub struct CollapseNestedCase<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    collapsed: Option<Collapsed<'a>>,
}

/// This holds all the needed data about the pattern to collapse.
/// We'll use this piece of code as an example:
/// ```gleam
/// case something {
///   User(username: _, NotAdmin) -> "Stranger!!"
///   User(username:, Admin) if wibble ->
///     case username {                // <- We're collapsing this nested case
///       "Joe" -> "Hello, Joe!"
///       _ -> "I don't know you, " <> username
///     }
/// }
/// ```
///
struct Collapsed<'a> {
    /// This is the span covering the entire clause being collapsed:
    ///
    /// ```gleam
    /// case something {
    ///   User(username: _, NotAdmin) -> "Stranger!!"
    ///   User(username:, Admin) if wibble ->
    ///   ┬ It goes all the way from here...
    /// ╭─╯
    /// │   case username {
    /// │     "Joe" -> "Hello, Joe!"
    /// │     _ -> "I don't know you, " <> username
    /// │   }
    /// │   ┬ ...to here!
    /// ╰───╯
    /// }
    /// ```
    ///
    outer_clause_span: SrcSpan,

    /// The (optional) guard of the outer branch. In this exmaple it's this one:
    ///
    /// ```gleam
    /// case something {
    ///   User(username: _, NotAdmin) -> "Stranger!!"
    ///   User(username:, Admin) if wibble ->
    ///                          ┬────────
    ///                          ╰─ `outer_guard`
    ///     case username {
    ///       "Joe" -> "Hello, Joe!"
    ///       _ -> "I don't know you, " <> username
    ///     }
    /// }
    /// ```
    ///
    outer_guard: &'a Option<TypedClauseGuard>,

    /// The pattern variable being matched on:
    ///
    /// ```gleam
    /// case something {
    ///   User(username: _, NotAdmin) -> "Stranger!!"
    ///   User(username:, Admin) if wibble ->
    ///        ┬───────
    ///        ╰─ `matched_variable`
    ///     case username {
    ///       "Joe" -> "Hello, Joe!"
    ///       _ -> "I don't know you, " <> username
    ///     }
    /// }
    /// ```
    ///
    matched_variable: BoundVariable,

    /// The span covering the entire pattern that is bringing the matched
    /// variable in scope:
    ///
    /// ```gleam
    /// case something {
    ///   User(username: _, NotAdmin) -> "Stranger!!"
    ///   User(username:, Admin) if wibble ->
    ///   ┬─────────────────────
    ///   ╰─ `matched_pattern_span`
    ///     case username {
    ///       "Joe" -> "Hello, Joe!"
    ///       _ -> "I don't know you, " <> username
    ///     }
    /// }
    /// ```
    ///
    matched_pattern_span: SrcSpan,

    /// The clauses matching on the `username` variable. In this case they are:
    /// ```gleam
    /// "Joe" -> "Hello, Joe!"
    /// _ -> "I don't know you, " <> username
    /// ```
    ///
    inner_clauses: &'a Vec<ast::TypedClause>,
}

impl<'a> CollapseNestedCase<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            collapsed: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(Collapsed {
            outer_clause_span,
            outer_guard,
            ref matched_variable,
            matched_pattern_span,
            inner_clauses,
        }) = self.collapsed
        else {
            return vec![];
        };

        // Now comes the tricky part: we need to replace the current pattern
        // that is bringing the variable into scope with many new patterns, one
        // for each of the inner clauses.
        //
        // Each time we will have to replace the matched variable with the
        // pattern used in the inner clause. Let's look at an example:
        //
        // ```gleam
        // Ok(a) -> case a {
        //   1 -> wibble
        //   2 | 3 -> wobble
        //   _ -> woo
        // }
        // ```
        //
        // Here we will replace `a` in the `Ok(a)` outer pattern with `1`, then
        // with `2` and `3`, and finally with `_`. Obtaining something like
        // this:
        //
        // ```gleam
        // Ok(1) -> wibble
        // Ok(2) | Ok(3) -> wobble
        // Ok(_) -> woo
        // ```
        //
        // Notice one key detail: since alternative patterns can't be nested we
        // can't simply write `Ok(2 | 3)` but we have to write `Ok(2) | Ok(3)`!

        let pattern_text: String = code_at(self.module, matched_pattern_span).into();
        let matched_variable_span = matched_variable.location;

        let pattern_with_variable = |mut new_content: String| {
            let mut new_pattern = pattern_text.clone();

            match matched_variable {
                BoundVariable {
                    name: BoundVariableName::Regular { .. } | BoundVariableName::ListTail { .. },
                    ..
                } => {
                    let trimmed_contents = new_content.trim();

                    let pattern_is_literal_list =
                        trimmed_contents.starts_with("[") && trimmed_contents.ends_with("]");
                    let pattern_is_discard = trimmed_contents == "_";

                    let span_to_replace = match (
                        &matched_variable.name,
                        // We verify whether the pattern is compatible with the list prefix `..`.
                        // For example, `..var` is valid syntax, but `..[]` and `.._` are not.
                        pattern_is_literal_list || pattern_is_discard,
                    ) {
                        // We normally replace the selected variable with the pattern.
                        (BoundVariableName::Regular { .. }, _) => matched_variable_span,

                        // If the selected pattern is not a list, we also replace it normally.
                        (BoundVariableName::ListTail { .. }, false) => matched_variable_span,
                        // If the pattern is a list to also remove the list tail prefix.
                        (BoundVariableName::ListTail { tail_location, .. }, true) => {
                            // When it's a list literal, we remove the surrounding brackets.
                            let len = trimmed_contents.len();
                            if let Some(slice) = new_content.trim().get(1..(len - 1)) {
                                new_content = slice.to_string()
                            };

                            *tail_location
                        }

                        (BoundVariableName::ShorthandLabel { .. }, _) => unreachable!(),
                    };

                    let start_of_pattern =
                        (span_to_replace.start - matched_pattern_span.start) as usize;
                    let pattern_length = span_to_replace.len();

                    let end_of_pattern = start_of_pattern + pattern_length;
                    let replaced_range = start_of_pattern..end_of_pattern;

                    new_pattern.replace_range(replaced_range, &new_content);
                }

                BoundVariable {
                    name: BoundVariableName::ShorthandLabel { .. },
                    ..
                } => {
                    // But if it's introduced using the shorthand syntax we can't
                    // just replace it's location with the new pattern: we would be
                    // removing the label!!
                    // So we instead insert the pattern right after the label.
                    new_pattern.insert_str(
                        (matched_variable_span.end - matched_pattern_span.start) as usize,
                        &format!(" {new_content}"),
                    );
                }
            }

            new_pattern
        };

        let mut new_clauses = vec![];
        for clause in inner_clauses {
            // Here we take care of unrolling any alterantive patterns: for each
            // of the alternatives we build a new pattern and then join
            // everything together with ` | `.

            let references_to_matched_variable =
                FindVariableReferences::new(matched_variable_span, matched_variable.name())
                    .find(&clause.then);

            let new_patterns = iter::once(&clause.pattern)
                .chain(&clause.alternative_patterns)
                .map(|patterns| {
                    // If we've reached this point we've already made in the
                    // traversal that the inner clause is matching on a single
                    // subject. So this should be safe to expect!
                    let pattern_location =
                        patterns.first().expect("must have a pattern").location();

                    let mut pattern_code = code_at(self.module, pattern_location).to_string();
                    if !references_to_matched_variable.is_empty() {
                        pattern_code = format!("{pattern_code} as {}", matched_variable.name());
                    };
                    pattern_with_variable(pattern_code)
                })
                .join(" | ");

            let clause_code = code_at(self.module, clause.then.location());
            let guard_code = match (outer_guard, &clause.guard) {
                (Some(outer), Some(inner)) => {
                    let mut outer_code = code_at(self.module, outer.location()).to_string();
                    let mut inner_code = code_at(self.module, inner.location()).to_string();
                    if ast::BinOp::And.precedence() > outer.precedence() {
                        outer_code = format!("{{ {outer_code} }}")
                    }
                    if ast::BinOp::And.precedence() > inner.precedence() {
                        inner_code = format!("{{ {inner_code} }}")
                    }
                    format!(" if {outer_code} && {inner_code}")
                }
                (None, Some(guard)) | (Some(guard), None) => {
                    format!(" if {}", code_at(self.module, guard.location()))
                }
                (None, None) => "".into(),
            };

            new_clauses.push(format!("{new_patterns}{guard_code} -> {clause_code}"));
        }

        let pattern_nesting = self
            .edits
            .src_span_to_lsp_range(outer_clause_span)
            .start
            .character;
        let indentation = " ".repeat(pattern_nesting as usize);

        self.edits.replace(
            outer_clause_span,
            new_clauses.join(&format!("\n{indentation}")),
        );

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Collapse nested case")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }

    /// If the clause can be flattened because it's matching on a single variable
    /// defined in it, this function will return the info needed by the language
    /// server to flatten that case.
    ///
    /// We can only flatten a case expression in a very specific case:
    /// - This pattern may be introducing multiple variables,
    /// - The expression following this branch must be a case, and
    /// - It must be matching on one of those variables
    ///
    /// For example:
    ///
    /// ```gleam
    /// Wibble(a, b, 1) -> case a { ... }
    /// Wibble(a, b, 1) -> case b { ... }
    /// ```
    ///
    fn flatten_clause(&self, clause: &'a ast::TypedClause) -> Option<Collapsed<'a>> {
        let ast::TypedClause {
            pattern,
            alternative_patterns,
            then,
            location,
            guard,
        } = clause;

        if !alternative_patterns.is_empty() {
            return None;
        }

        // The `then` clause must be a single case expression matching on a
        // single variable.
        let Some(TypedExpr::Case {
            subjects, clauses, ..
        }) = single_expression(then)
        else {
            return None;
        };

        let [TypedExpr::Var { name, .. }] = subjects.as_slice() else {
            return None;
        };

        // That variable must be one the variables we brought into scope in this
        // branch.
        let variable = pattern
            .iter()
            .flat_map(|pattern| pattern.bound_variables())
            .find(|variable| variable.name() == *name)?;

        // There's one last condition to trigger the code action: we must
        // actually be with the cursor over the pattern or the nested case
        // expression!
        //
        // ```gleam
        // case wibble {
        //   Ok(a) -> case a {
        // //^^^^^^^^^^^^^^^ Anywhere over here!
        //   }
        // }
        // ```
        //
        let first_pattern = pattern.first().expect("at least one pattern");
        let last_pattern = pattern.last().expect("at least one pattern");
        let pattern_location = first_pattern.location().merge(&last_pattern.location());

        let last_inner_subject = subjects.last().expect("at least one subject");
        let trigger_location = pattern_location.merge(&last_inner_subject.location());
        let trigger_range = self.edits.src_span_to_lsp_range(trigger_location);

        if within(self.params.range, trigger_range) {
            Some(Collapsed {
                outer_clause_span: *location,
                outer_guard: guard,
                matched_variable: variable,
                matched_pattern_span: pattern_location,
                inner_clauses: clauses,
            })
        } else {
            None
        }
    }
}

impl<'ast> ast::visit::Visit<'ast> for CollapseNestedCase<'ast> {
    fn visit_typed_clause(&mut self, clause: &'ast ast::TypedClause) {
        if let Some(collapsed) = self.flatten_clause(clause) {
            self.collapsed = Some(collapsed);

            // We're done, there's no need to keep exploring as we know the
            // cursor is over this pattern and it can't be over any other one!
            return;
        };

        ast::visit::visit_typed_clause(self, clause);
    }
}

/// If the expression is a single expression, or a block containing a single
/// expression, this function will return it.
/// But if the expression is a block with multiple statements, an assignment
/// of a use, this will return None.
///
fn single_expression(expression: &TypedExpr) -> Option<&TypedExpr> {
    match expression {
        // If a block has a single statement, we can flatten it into a
        // single expression if that one statement is an expression.
        TypedExpr::Block { statements, .. } if statements.len() == 1 => match statements.first() {
            ast::Statement::Expression(expression) => single_expression(expression),
            ast::Statement::Assignment(_) | ast::Statement::Use(_) | ast::Statement::Assert(_) => {
                None
            }
        },

        // If a block has multiple statements then it can't be flattened
        // into a single expression.
        TypedExpr::Block { .. } => None,

        TypedExpr::Int { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::String { .. }
        | TypedExpr::Pipeline { .. }
        | TypedExpr::Var { .. }
        | TypedExpr::Fn { .. }
        | TypedExpr::List { .. }
        | TypedExpr::Call { .. }
        | TypedExpr::BinOp { .. }
        | TypedExpr::Case { .. }
        | TypedExpr::RecordAccess { .. }
        | TypedExpr::PositionalAccess { .. }
        | TypedExpr::ModuleSelect { .. }
        | TypedExpr::Tuple { .. }
        | TypedExpr::TupleIndex { .. }
        | TypedExpr::Todo { .. }
        | TypedExpr::Panic { .. }
        | TypedExpr::Echo { .. }
        | TypedExpr::BitArray { .. }
        | TypedExpr::RecordUpdate { .. }
        | TypedExpr::NegateBool { .. }
        | TypedExpr::NegateInt { .. }
        | TypedExpr::Invalid { .. } => Some(expression),
    }
}

/// Code action to remove unreachable clauses from a case expression.
///
pub struct RemoveUnreachableCaseClauses<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    /// The source location of the patterns of all the unreachable clauses in
    /// the current module.
    ///
    unreachable_clauses: HashSet<SrcSpan>,
    clauses_to_delete: Vec<SrcSpan>,
}

impl<'a> RemoveUnreachableCaseClauses<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        let unreachable_clauses = module
            .ast
            .type_info
            .warnings
            .iter()
            .filter_map(|warning| {
                if let type_::Warning::UnreachableCasePattern { location, .. } = warning {
                    Some(*location)
                } else {
                    None
                }
            })
            .collect();

        Self {
            unreachable_clauses,
            module,
            params,
            edits: TextEdits::new(line_numbers),
            clauses_to_delete: vec![],
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);
        if self.clauses_to_delete.is_empty() {
            return vec![];
        }

        for branch in self.clauses_to_delete {
            self.edits.delete(branch);
        }

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Remove unreachable clauses")
            .kind(CodeActionKind::QUICKFIX)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(true)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for RemoveUnreachableCaseClauses<'ast> {
    fn visit_typed_expr_case(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        subjects: &'ast [TypedExpr],
        clauses: &'ast [ast::TypedClause],
        compiled_case: &'ast CompiledCase,
    ) {
        // We're showing the code action only if we're within one of the
        // unreachable patterns. And the code action is going to remove all the
        // unreachable patterns for this case.
        let is_hovering_clause = clauses.iter().any(|clause| {
            let pattern_range = self.edits.src_span_to_lsp_range(clause.pattern_location());
            within(self.params.range, pattern_range)
        });
        if is_hovering_clause {
            self.clauses_to_delete = clauses
                .iter()
                .filter(|clause| {
                    self.unreachable_clauses
                        .contains(&clause.pattern_location())
                })
                .map(|clause| clause.location())
                .collect_vec();
            return;
        }

        // If we're not hovering any of the clauses then we want to
        // keep visiting the case expression as the unreachable branch might be
        // in one of the nested cases.
        ast::visit::visit_typed_expr_case(self, location, type_, subjects, clauses, compiled_case);
    }
}

/// Code action to add labels to a constructor/call where all the labels where
/// omitted.
///
pub struct AddOmittedLabels<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    arguments_and_omitted_labels: Option<Vec<CallArgumentWithOmittedLabel>>,
}

struct CallArgumentWithOmittedLabel {
    location: SrcSpan,

    /// If the argument has a label this will be the label we can use for it.
    ///
    omitted_label: Option<EcoString>,

    /// If the argument is a variable that has the same name as the omitted label
    /// and could use the shorthand syntax.
    ///
    can_use_shorthand_syntax: bool,
}

impl<'a> AddOmittedLabels<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            arguments_and_omitted_labels: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(call_arguments) = self.arguments_and_omitted_labels else {
            return vec![];
        };

        for call_argument in call_arguments {
            let Some(label) = call_argument.omitted_label else {
                continue;
            };
            if call_argument.can_use_shorthand_syntax {
                self.edits.insert(call_argument.location.end, ":".into());
            } else {
                self.edits
                    .insert(call_argument.location.start, format!("{label}: "))
            }
        }

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Add omitted labels")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for AddOmittedLabels<'ast> {
    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        arguments: &'ast [TypedCallArg],
    ) {
        let called_function_range = self.edits.src_span_to_lsp_range(fun.location());
        if !within(self.params.range, called_function_range) {
            ast::visit::visit_typed_expr_call(self, location, type_, fun, arguments);
            return;
        }

        let Some(field_map) = fun.field_map() else {
            ast::visit::visit_typed_expr_call(self, location, type_, fun, arguments);
            return;
        };
        let argument_index_to_label = field_map.indices_to_labels();

        let mut omitted_labels = Vec::with_capacity(arguments.len());
        for (index, argument) in arguments.iter().enumerate() {
            // If the argument already has a label we don't want to add a label
            // for it, so we skip it.
            if let Some(label) = &argument.label {
                // Though, before skipping, we want to make sure that the label
                // is actually right for the function call. If it's not then we
                // give up on adding labels because there wouldn't be no way of
                // knowing which label to add.
                if !field_map.fields.contains_key(label) {
                    return;
                } else {
                    continue;
                }
            }
            // No labels for pipes, uses, etc!
            if argument.is_implicit() {
                continue;
            }

            let label = argument_index_to_label
                .get(&(index as u32))
                .cloned()
                .cloned();

            let can_use_shorthand_syntax = match (&label, &argument.value) {
                (Some(label), TypedExpr::Var { name, .. }) => name == label,
                (Some(_) | None, _) => false,
            };

            omitted_labels.push(CallArgumentWithOmittedLabel {
                location: argument.location,
                omitted_label: label,
                can_use_shorthand_syntax,
            })
        }
        self.arguments_and_omitted_labels = Some(omitted_labels);
    }
}

/// Code action to extract selected code into a separate function.
/// If a user selected a portion of code in a function, we offer a code action
/// to extract it into a new one. This can either be a single expression, such
/// as in the following example:
///
/// ```gleam
/// pub fn main() {
///   let value = {
///   //          ^ User selects from here
///     ...
///   }
/// //^ Until here
/// }
/// ```
///
/// Here, we would extract the selected block expression. It could also be a
/// series of statements. For example:
///
/// ```gleam
/// pub fn main() {
///   let a = 1
/// //^ User selects from here
///   let b = 2
///   let c = a + b
///   //          ^ Until here
///
///   do_more_things(c)
/// }
/// ```
///
/// Here, we want to extract the statements inside the user's selection.
///
pub struct ExtractFunction<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    function: Option<ExtractedFunction<'a>>,
    function_end_position: Option<u32>,
    /// Since the `visit_typed_statement` visitor function doesn't tell us when
    /// a statement is the last in a block or function, we need to track that
    /// manually.
    last_statement_location: Option<SrcSpan>,
}

/// Information about a section of code we are extracting as a function.
struct ExtractedFunction<'a> {
    /// A list of parameters which need to be passed to the extracted function.
    /// These are any variables used in the extracted code, which are defined
    /// outside of the extracted code.
    parameters: Vec<(EcoString, Arc<Type>)>,
    /// A list of values which need to be returned from the extracted function.
    /// These are the variables defined in the extracted code which are used
    /// outside of the extracted section.
    returned_variables: Vec<(EcoString, Arc<Type>)>,
    /// The piece of code to be extracted. This is either a single expression or
    /// a list of statements, as explained in the documentation of `ExtractFunction`
    value: ExtractedValue<'a>,
}

impl<'a> ExtractedFunction<'a> {
    fn new(value: ExtractedValue<'a>) -> Self {
        Self {
            value,
            parameters: Vec::new(),
            returned_variables: Vec::new(),
        }
    }

    fn location(&self) -> SrcSpan {
        match &self.value {
            ExtractedValue::Expression(expression) => expression.location(),
            ExtractedValue::Statements { location, .. } => *location,
        }
    }
}

#[derive(Debug)]
enum ExtractedValue<'a> {
    Expression(&'a TypedExpr),
    Statements {
        location: SrcSpan,
        position: StatementPosition,
    },
}

/// When we are extracting multiple statements, there are two possible cases:
/// The first is if we are extracting statements in the middle of a function.
/// In this case, we will need to return some number of arguments, or `Nil`.
/// For example:
///
/// ```gleam
/// pub fn main() {
///   let message = "Hello!"
///   let log_message = "[INFO] " <> message
/// //^ Select from here
///   io.println(log_message)
///   //                    ^ Until here
///
///   do_some_more_things()
/// }
/// ```
///
/// Here, the extracted function doesn't bind any variables which we need
/// afterwards, it purely performs side effects. In this case we can just return
/// `Nil` from the new function.
///
/// However, consider the following:
///
/// ```gleam
/// pub fn main() {
///   let a = 1
///   let b = 2
/// //^ Select from here
///   a + b
///   //  ^ Until here
/// }
/// ```
///
/// Here, despite us not needing any variables from the extracted code, there
/// is one key difference: the `a + b` expression is at the end of the function,
/// and so its value is returned from the entire function. This is known as the
/// "tail" position. In that case, we can't return `Nil` as that would make the
/// `main` function return `Nil` instead of the result of the addition. If we
/// extract the tail-position statement, we need to return that last value rather
/// than `Nil`.
///
#[derive(Debug)]
enum StatementPosition {
    Tail { type_: Arc<Type> },
    NotTail,
}

impl<'a> ExtractFunction<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            function: None,
            function_end_position: None,
            last_statement_location: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        // If no code is selected, then there is no function to extract and we
        // can return no code actions.
        if self.params.range.start == self.params.range.end {
            return Vec::new();
        }

        self.visit_typed_module(&self.module.ast);

        let Some(end) = self.function_end_position else {
            return Vec::new();
        };

        // If nothing was found in the selected range, there is no code action.
        let Some(extracted) = self.function.take() else {
            return Vec::new();
        };

        match extracted.value {
            // If we extract a block, it isn't very helpful to have the body of the
            // extracted function just be a single block expression, so instead we
            // extract the statements inside the block. For example, the following
            // code:
            //
            // ```gleam
            // pub fn main() {
            //   let x = {
            //   //      ^ Select from here
            //     let a = 1
            //     let b = 2
            //     a + b
            //   }
            // //^ Until here
            //   x
            // }
            // ```
            //
            // Would produce the following extracted function:
            //
            // ```gleam
            // fn function() {
            //   let a = 1
            //   let b = 2
            //   a + b
            // }
            // ```
            //
            // Rather than:
            //
            // ```gleam
            // fn function() {
            //   {
            //     let a = 1
            //     let b = 2
            //     a + b
            //   }
            // }
            // ```
            //
            ExtractedValue::Expression(TypedExpr::Block {
                statements,
                location: full_location,
            }) => {
                let location = statements
                    .first()
                    .location()
                    .merge(&statements.last().location());

                self.extract_code_in_tail_position(
                    *full_location,
                    location,
                    statements.last().type_(),
                    extracted.parameters,
                    end,
                )
            }
            ExtractedValue::Expression(TypedExpr::Fn {
                type_,
                location: full_location,
                kind: FunctionLiteralKind::Anonymous { .. },
                arguments,
                body,
                ..
            }) => {
                let location = body.first().location().merge(&body.last().location());
                let return_type = type_.return_type().expect("Fn should have a return type");

                if extracted.parameters.is_empty() {
                    self.extract_anonymous_function(
                        *full_location,
                        location,
                        arguments,
                        return_type,
                        end,
                    )
                } else if arguments.len() == 1 {
                    self.extract_anonymous_function_with_capture_hole(
                        *full_location,
                        location,
                        arguments.first().expect("There is exactly one argument"),
                        extracted.parameters,
                        return_type,
                        end,
                    )
                } else {
                    self.extract_anonymous_function_body(
                        location,
                        arguments,
                        extracted.parameters,
                        return_type,
                        end,
                    )
                }
            }
            ExtractedValue::Expression(expression) => {
                let expression_type = if let TypedExpr::Fn {
                    type_,
                    kind: FunctionLiteralKind::Use { .. },
                    ..
                } = expression
                {
                    type_.fn_types().expect("use callback to be a function").1
                } else {
                    expression.type_()
                };
                self.extract_code_in_tail_position(
                    expression.location(),
                    expression.location(),
                    expression_type,
                    extracted.parameters,
                    end,
                )
            }
            ExtractedValue::Statements {
                location,
                position: StatementPosition::NotTail,
            } => self.extract_statements(
                location,
                extracted.parameters,
                extracted.returned_variables,
                end,
            ),
            ExtractedValue::Statements {
                location,
                position: StatementPosition::Tail { type_ },
            } => self.extract_code_in_tail_position(
                location,
                location,
                type_,
                extracted.parameters,
                end,
            ),
        }

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Extract function")
            .kind(CodeActionKind::REFACTOR_EXTRACT)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }

    /// Choose a suitable name for an extracted function to make sure it doesn't
    /// clash with existing functions defined in the module and cause an error.
    fn function_name(&self) -> EcoString {
        if !self.module.ast.type_info.values.contains_key("function") {
            return "function".into();
        }

        let mut number = 2;
        loop {
            let name = eco_format!("function_{number}");
            if !self.module.ast.type_info.values.contains_key(&name) {
                return name;
            }
            number += 1;
        }
    }

    /// For anonymous functions that do not capture any variables from an outer scope.
    /// Moves the function so it is defined at the module top-level instead,
    /// replacing the original literal with a reference to the new function.
    fn extract_anonymous_function(
        &mut self,
        location: SrcSpan,
        code_location: SrcSpan,
        arguments: &[TypedArg],
        return_type: Arc<Type>,
        function_end: u32,
    ) {
        // --- BEFORE
        // ```gleam
        // pub fn main() {
        //   list.each([1, 2, 3], fn(x) { io.println(int.to_string(x)) })
        //                        ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔↑
        // }
        // ```
        //
        // --- AFTER
        // ```gleam
        // pub fn main() {
        //   list.each([1, 2, 3], function)
        // }
        //
        // fn function(x: Int) -> Nil {
        //   io.println(int.to_string(x))
        // }
        // ```

        let name = self.function_name();
        self.edits.replace(location, name.to_string());

        let mut printer = Printer::new(&self.module.ast.names);

        let return_type = printer.print_type(&return_type);
        let function_body = code_at(self.module, code_location);
        let mut name_generator = NameGenerator::new();
        let arguments = arguments
            .iter()
            .map(|arg| {
                if let Some(name) = arg.get_variable_name() {
                    eco_format!("{name}: {}", printer.print_type(&arg.type_))
                } else {
                    let name = name_generator.generate_name_from_type(&arg.type_);
                    eco_format!("_{name}: {}", printer.print_type(&arg.type_))
                }
            })
            .join(", ");

        let function = format!(
            "\n\nfn {name}({arguments}) -> {return_type} {{
  {function_body}
}}"
        );
        self.edits.insert(function_end, function);
    }

    /// For anonymous functions that capture variables from an external scope
    /// but only expect a single argument.
    /// Uses function caputre syntax to provide a more concise refactoring than
    /// `extract_anonymous_function_body`.
    fn extract_anonymous_function_with_capture_hole(
        &mut self,
        location: SrcSpan,
        code_location: SrcSpan,
        argument: &TypedArg,
        extra_parameters: Vec<(EcoString, Arc<Type>)>,
        return_type: Arc<Type>,
        function_end: u32,
    ) {
        let name = self.function_name();

        // --- BEFORE
        // ```gleam
        // pub fn main() {
        //   let needle = 42
        //   let haystack = [25, 81, 74, 42, 33]
        //   list.filter(haystack, fn(x) { x == needle })
        //                         ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔↑
        // }
        // ```
        //
        // --- AFTER
        // ```gleam
        // pub fn main() {
        //   let needle = 42
        //   let haystack = [25, 81, 74, 42, 33]
        //   list.filter(haystack, function(_, needle))
        // }
        //
        // fn function(x: Int, needle: Int) -> Bool {
        //   x == needle
        // }
        // ```

        let call = format!(
            "{name}(_, {})",
            extra_parameters.iter().map(|(name, _)| name).join(", ")
        );
        self.edits.replace(location, call);

        let mut printer = Printer::new(&self.module.ast.names);

        // build up the code for the newly generated function
        let return_type = printer.print_type(&return_type);
        let function_body = code_at(self.module, code_location);
        let argument = if let Some(name) = argument.get_variable_name() {
            eco_format!("{name}: {}", printer.print_type(&argument.type_))
        } else {
            let name = NameGenerator::new().generate_name_from_type(&argument.type_);
            eco_format!("_{name}: {}", printer.print_type(&argument.type_))
        };
        let extra_parameters = extra_parameters
            .iter()
            .map(|(name, type_)| eco_format!("{name}: {}", printer.print_type(type_)))
            .join(", ");

        let function = format!(
            "\n\nfn {name}({argument}, {extra_parameters}) -> {return_type} {{
  {function_body}
}}"
        );
        self.edits.insert(function_end, function);
    }

    /// For non-unary anonymous functions that capture variables from an external scope.
    /// Replaces just the _function body_ with a call to the newly generated function.
    fn extract_anonymous_function_body(
        &mut self,
        location: SrcSpan,
        arguments: &[TypedArg],
        extra_parameters: Vec<(EcoString, Arc<Type>)>,
        return_type: Arc<Type>,
        function_end: u32,
    ) {
        let name = self.function_name();
        // --- BEFORE
        // ```gleam
        // pub fn main() {
        //   let factor = 2
        //   list.fold([], 0, fn(acc, value) { acc + value * factor })
        //                    ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔↑
        // }
        // ```
        //
        // --- AFTER
        // ```gleam
        // pub fn main() {
        //   let factor = 2
        //   list.fold([], 0, fn(acc, value) { function(acc, value, factor) })
        // }
        //
        // fn function(acc: Int, value: Int, factor: Int) -> Int {
        //   acc + value * factor
        // }
        // ```

        // if the programmer has ignored an argument, the generated function
        // cannot take it as an parameter
        let arguments = arguments
            .iter()
            .filter_map(|arg| arg.get_variable_name().map(|name| (name, &arg.type_)))
            .chain(extra_parameters.iter().map(|(name, type_)| (name, type_)))
            .collect::<Vec<(&EcoString, &Arc<Type>)>>();

        let call = format!(
            "{name}({})",
            arguments.iter().map(|(name, _)| name).join(", ")
        );
        self.edits.replace(location, call);

        let mut printer = Printer::new(&self.module.ast.names);

        let return_type = printer.print_type(&return_type);
        let function_body = code_at(self.module, location);
        let arguments = arguments
            .iter()
            .map(|(name, type_)| eco_format!("{name}: {}", printer.print_type(type_)))
            .join(", ");

        let function = format!(
            "\n\nfn {name}({arguments}) -> {return_type} {{
  {function_body}
}}"
        );
        self.edits.insert(function_end, function);
    }

    /// Extracts code from the end of a function or block. This could either be
    /// a single expression, or multiple statements followed by a final expression.
    fn extract_code_in_tail_position(
        &mut self,
        location: SrcSpan,
        code_location: SrcSpan,
        type_: Arc<Type>,
        parameters: Vec<(EcoString, Arc<Type>)>,
        function_end: u32,
    ) {
        let expression_code = code_at(self.module, code_location);

        let name = self.function_name();
        let arguments = parameters.iter().map(|(name, _)| name).join(", ");
        let call = format!("{name}({arguments})");

        // Since we are only extracting a single expression, we can just replace
        // it with the call and preserve all other semantics; only one value can
        // be returned from the expression, unlike when extracting multiple
        // statements.
        self.edits.replace(location, call);

        let mut printer = Printer::new(&self.module.ast.names);

        let parameters = parameters
            .iter()
            .map(|(name, type_)| eco_format!("{name}: {}", printer.print_type(type_)))
            .join(", ");
        let return_type = printer.print_type(&type_);

        let function = format!(
            "\n\nfn {name}({parameters}) -> {return_type} {{
  {expression_code}
}}"
        );

        self.edits.insert(function_end, function);
    }

    fn extract_statements(
        &mut self,
        location: SrcSpan,
        parameters: Vec<(EcoString, Arc<Type>)>,
        returned_variables: Vec<(EcoString, Arc<Type>)>,
        function_end: u32,
    ) {
        let code = code_at(self.module, location);

        let returns_anything = !returned_variables.is_empty();

        // Here, we decide what value to return from the function. There are
        // three cases:
        // The first is when the extracted code is purely for side-effects, and
        // does not produce any values which are needed outside of the extracted
        // code. For example:
        //
        // ```gleam
        // pub fn main() {
        //   let message = "Something important"
        // //^ Select from here
        //   io.println("Something important")
        //   io.println("Something else which is repeated")
        //   //                                           ^ Until here
        //
        //   do_final_thing()
        // }
        // ```
        //
        // It doesn't make sense to return any values from this function, since
        // no values from the extract code are used afterwards, so we simply
        // return `Nil`.
        //
        // The next is when we need just a single value defined in the extracted
        // function, such as in this piece of code:
        //
        // ```gleam
        // pub fn main() {
        //   let a = 10
        // //^ Select from here
        //   let b = 20
        //   let c = a + b
        //   //          ^ Until here
        //
        //   echo c
        // }
        // ```
        //
        // Here, we can just return the single value, `c`.
        //
        // The last situation is when we need multiple defined values, such as
        // in the following code:
        //
        // ```gleam
        // pub fn main() {
        //   let a = 10
        // //^ Select from here
        //   let b = 20
        //   let c = a + b
        //   //          ^ Until here
        //
        //   echo a
        //   echo b
        //   echo c
        // }
        // ```
        //
        // In this case, we must return a tuple containing `a`, `b` and `c` in
        // order for the calling function to have access to the correct values.
        let (return_type, return_value) = match returned_variables.as_slice() {
            [] => (type_::nil(), "Nil".into()),
            [(name, type_)] => (type_.clone(), name.clone()),
            _ => {
                let values = returned_variables.iter().map(|(name, _)| name).join(", ");
                let type_ = type_::tuple(
                    returned_variables
                        .into_iter()
                        .map(|(_, type_)| type_)
                        .collect(),
                );

                (type_, eco_format!("#({values})"))
            }
        };

        let name = self.function_name();
        let arguments = parameters.iter().map(|(name, _)| name).join(", ");

        // If any values are returned from the extracted function, we need to
        // bind them so that they are accessible in the current scope.
        let call = if returns_anything {
            format!("let {return_value} = {name}({arguments})")
        } else {
            format!("{name}({arguments})")
        };
        self.edits.replace(location, call);

        let mut printer = Printer::new(&self.module.ast.names);

        let parameters = parameters
            .iter()
            .map(|(name, type_)| eco_format!("{name}: {}", printer.print_type(type_)))
            .join(", ");

        let return_type = printer.print_type(&return_type);

        let function = format!(
            "\n\nfn {name}({parameters}) -> {return_type} {{
  {code}
  {return_value}
}}"
        );

        self.edits.insert(function_end, function);
    }

    /// When a variable is referenced, we need to decide if we need to do anything
    /// to ensure that the reference is still valid after extracting a function.
    /// If the variable is defined outside the extracted function, but used inside
    /// it, then we need to add it as a parameter of the function. Similarly, if
    /// a variable is defined inside the extracted code, but used outside of it,
    /// we need to ensure that value is returned from the function so that it is
    /// accessible.
    fn register_referenced_variable(
        &mut self,
        name: &EcoString,
        type_: &Arc<Type>,
        location: SrcSpan,
        definition_location: SrcSpan,
    ) {
        let Some(extracted) = &mut self.function else {
            return;
        };

        let extracted_location = extracted.location();

        // If a variable defined outside the extracted code is referenced inside
        // it, we need to add it to the list of parameters.
        let variables = if extracted_location.contains_span(location)
            && !extracted_location.contains_span(definition_location)
        {
            &mut extracted.parameters
        // If a variable defined inside the extracted code is referenced outside
        // it, then we need to ensure that it is returned from the function.
        } else if extracted_location.contains_span(definition_location)
            && !extracted_location.contains_span(location)
        {
            &mut extracted.returned_variables
        } else {
            return;
        };

        // If the variable has already been tracked, no need to register it again.
        // We use a `Vec` here rather than a `HashMap` because we want to ensure
        // the order of arguments is consistent; in this case it will be determined
        // by the order the variables are used. This isn't always desired, but it's
        // better than random order, and makes it easier to write tests too.
        // The cost of iterating the list here is minimal; it is unlikely that
        // a given function will ever have more than 10 or so parameters.
        if variables.iter().any(|(variable, _)| variable == name) {
            return;
        }

        variables.push((name.clone(), type_.clone()));
    }

    fn can_extract(&self, location: SrcSpan) -> bool {
        let expression_range = self.edits.src_span_to_lsp_range(location);
        let selected_range = self.params.range;

        // If the selected range doesn't touch the expression at all, then there
        // is no reason to extract it.
        if !overlaps(expression_range, selected_range) {
            return false;
        }

        // Determine whether the selected range falls completely within the
        // expression. For example:
        // ```gleam
        // pub fn main() {
        //   let something = {
        //     let a = 1
        //     let b = 2
        //     let c = a + b
        //   //^ The user has selected from here
        //     let d = a * b
        //     c / d
        //     //  ^ Until here
        //   }
        // }
        // ```
        //
        // Here, the selected range does overlap with the `let something`
        // statement; but we don't want to extract that whole statement! The
        // user only wanted to extract the statements inside the block. So if
        // the selected range falls completely within the expression, we ignore
        // it and traverse the tree further until we find exactly what the user
        // selected.
        //
        let selected_within_expression = selected_range.start > expression_range.start
            && selected_range.start < expression_range.end
            && selected_range.end > expression_range.start
            && selected_range.end < expression_range.end;

        // If the selected range is completely within the expression, we don't
        // want to extract it.
        !selected_within_expression
    }
}

impl<'ast> ast::visit::Visit<'ast> for ExtractFunction<'ast> {
    fn visit_typed_function(&mut self, function: &'ast TypedFunction) {
        let range = self.edits.src_span_to_lsp_range(function.full_location());

        if within(self.params.range, range) {
            self.function_end_position = Some(function.end_position);
            self.last_statement_location = function.body.last().map(|last| last.location());

            ast::visit::visit_typed_function(self, function);
        }
    }

    fn visit_typed_expr_block(
        &mut self,
        location: &'ast SrcSpan,
        statements: &'ast [TypedStatement],
    ) {
        let last_statement_location = self.last_statement_location;
        self.last_statement_location = statements.last().map(|last| last.location());

        ast::visit::visit_typed_expr_block(self, location, statements);

        self.last_statement_location = last_statement_location;
    }

    fn visit_typed_expr(&mut self, expression: &'ast TypedExpr) {
        // If we have already determined what code we want to extract, we don't
        // want to extract this instead. This expression would be inside the
        // piece of code we already are going to extract, leading to us
        // extracting just a single literal in any selection, which is of course
        // not desired.
        if self.function.is_none() {
            // If this expression is fully selected, we mark it as being extracted.
            if self.can_extract(expression.location()) {
                self.function = Some(ExtractedFunction::new(ExtractedValue::Expression(
                    expression,
                )));
            }
        }
        ast::visit::visit_typed_expr(self, expression);
    }

    fn visit_typed_statement(&mut self, statement: &'ast TypedStatement) {
        let statement_location = statement.location();

        if self.can_extract(statement_location) {
            let is_in_tail_position =
                self.last_statement_location
                    .is_some_and(|last_statement_location| {
                        last_statement_location == statement_location
                    });

            // A use is always eating up the entire block, if we're extracting it,
            // it will be in tail position there and the extracted function should
            // return its returned value.
            let position = if statement.is_use() || is_in_tail_position {
                StatementPosition::Tail {
                    type_: statement.type_(),
                }
            } else {
                StatementPosition::NotTail
            };

            match &mut self.function {
                None => {
                    self.function = Some(ExtractedFunction::new(ExtractedValue::Statements {
                        location: statement_location,
                        position,
                    }));
                }
                // If we have already chosen an expression to extract, that means
                // that this statement is within the already extracted expression,
                // so we don't want to extract this instead.
                Some(ExtractedFunction {
                    value: ExtractedValue::Expression(_),
                    ..
                }) => {}
                // If we are selecting multiple statements, this statement should
                // be included within list, so we merge the spans to ensure it
                // is included.
                Some(ExtractedFunction {
                    value:
                        ExtractedValue::Statements {
                            location,
                            position: extracted_position,
                        },
                    ..
                }) => {
                    *location = location.merge(&statement_location);
                    *extracted_position = position;
                }
            }
        }
        ast::visit::visit_typed_statement(self, statement);
    }

    fn visit_typed_expr_var(
        &mut self,
        location: &'ast SrcSpan,
        constructor: &'ast ValueConstructor,
        name: &'ast EcoString,
    ) {
        if let type_::ValueConstructorVariant::LocalVariable {
            location: definition_location,
            ..
        } = &constructor.variant
        {
            self.register_referenced_variable(
                name,
                &constructor.type_,
                *location,
                *definition_location,
            );
        }
    }

    fn visit_typed_clause_guard_var(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        type_: &'ast Arc<Type>,
        definition_location: &'ast SrcSpan,
    ) {
        self.register_referenced_variable(name, type_, *location, *definition_location);
    }

    fn visit_typed_bit_array_size_variable(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        constructor: &'ast Option<Box<ValueConstructor>>,
        type_: &'ast Arc<Type>,
    ) {
        let variant = match constructor {
            Some(constructor) => &constructor.variant,
            None => return,
        };
        if let type_::ValueConstructorVariant::LocalVariable {
            location: definition_location,
            ..
        } = variant
        {
            self.register_referenced_variable(name, type_, *location, *definition_location);
        }
    }
}

/// Code action to merge two identical branches together.
///
pub struct MergeCaseBranches<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    /// These are the positions of the patterns of all the consecutive branches
    /// we've determined can be merged, for example if we're mergin the first
    /// two branches here:
    ///
    /// ```gleam
    ///   case wibble {
    ///     1 -> todo
    /// //  ^ this location here
    ///     20 -> todo
    /// //  ^^ and this location here
    ///   _ -> todo
    /// }
    /// ```
    ///
    /// We need those to delete all the space between each consecutive pattern,
    /// replacing it with the `|` for alternatives
    ///
    patterns_to_merge: Option<MergeableBranches>,
}

struct MergeableBranches {
    /// The span of the body to keep when merging multiple branches. For
    /// example:
    ///
    /// ```gleam
    /// case n {
    ///   // Imagine we're merging the first three branches together...
    ///   1 -> todo
    ///   2 -> n * 2
    /// //     ^^^^^ This would be the location of the one body to keep
    ///   3 -> todo
    ///   _ -> todo
    /// }
    /// ```
    ///
    body_to_keep: SrcSpan,

    /// The location body of the last of the branches that are going to be
    /// merged; that is where we're going to place the code of the body to keep
    /// once the action is done. For example:
    ///
    /// ```gleam
    /// case n {
    ///   // Imagine we're merging the first three branches together...
    ///   1 -> todo
    ///   2 -> n * 2
    ///   3 -> todo
    /// //     ^^^^ This would be the location of the final body
    ///   _ -> todo
    /// }
    /// ```
    ///
    final_body: SrcSpan,

    /// The span of the patterns whose branches are going to be merged. For
    /// example:
    ///
    /// ```gleam
    /// case n {
    ///   // Imagine we're merging the first three branches together...
    ///    1 -> todo
    /// // ^
    ///    2 -> n * 2
    /// // ^
    ///    3 -> todo
    /// // ^ These would be the locations of the patterns
    ///   _ -> todo
    /// }
    /// ```
    ///
    patterns_to_merge: Vec<SrcSpan>,
}

impl<'a> MergeCaseBranches<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            patterns_to_merge: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(mergeable_branches) = self.patterns_to_merge else {
            return vec![];
        };

        for (one, next) in mergeable_branches.patterns_to_merge.iter().tuple_windows() {
            self.edits
                .replace(SrcSpan::new(one.end, next.start), " | ".into());
        }

        self.edits.replace(
            mergeable_branches.final_body,
            code_at(self.module, mergeable_branches.body_to_keep).into(),
        );

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Merge case branches")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }

    fn select_mergeable_branches(
        &self,
        clauses: &'a [ast::TypedClause],
    ) -> Option<MergeableBranches> {
        let mut clauses = clauses
            .iter()
            // We want to skip all the branches at the beginning of the case
            // expression that the cursor is not hovering over. For example:
            //
            // ```gleam
            // case wibble {
            //   a -> 1   <- we want to skip this one here that is not selected
            //   b -> 2
            //     ^^^^   this is the selection
            //   _ -> 3
            //   ^^
            // }
            // ```
            .skip_while(|clause| {
                let clause_range = self.edits.src_span_to_lsp_range(clause.location);
                !overlaps(self.params.range, clause_range)
            })
            // Then we only want to take the clauses that we're hovering over
            // with our selection (even partially!)
            // In the provious example they would be `b -> 2` and `_ -> 3`.
            .take_while(|clause| {
                let clause_range = self.edits.src_span_to_lsp_range(clause.location);
                overlaps(self.params.range, clause_range)
            });

        let first_hovered_clause = clauses.next()?;

        // This is the clause we're comparing all the others with. We need to
        // make sure that all the clauses we're going to join can be merged with
        // this one.
        let mut reference_clause = first_hovered_clause;
        let mut clause_patterns_to_merge = vec![reference_clause.pattern_location()];
        let mut final_body = first_hovered_clause.then.location();

        for clause in clauses {
            // As soon as we find a clause that can't be merged with the current
            // reference we know we're done looking for consecutive clauses to
            // merge.
            if !clauses_can_be_merged(reference_clause, clause) {
                break;
            }

            clause_patterns_to_merge.push(clause.pattern_location());
            final_body = clause.then.location();

            // If the current reference is a `todo` expression, we want to use
            // the newly found mergeable clause as the next reference. The
            // reference clause is the one whose body will be kept around, so if
            // we can we avoid keeping `todo`s
            if reference_clause.then.is_todo_with_no_message() {
                reference_clause = clause;
            }
        }

        // We only offer the code action if we have found two or more clauses
        // to merge.
        if clause_patterns_to_merge.len() >= 2 {
            Some(MergeableBranches {
                final_body,
                body_to_keep: reference_clause.then.location(),
                patterns_to_merge: clause_patterns_to_merge,
            })
        } else {
            None
        }
    }
}

fn clauses_can_be_merged(one: &ast::TypedClause, other: &ast::TypedClause) -> bool {
    // Two clauses cannot be merged if any of those has an if guard
    if one.guard.is_some() || other.guard.is_some() {
        return false;
    }

    // Two clauses can only be merged if they define the same variables,
    // otherwise joining them would result in invalid code.
    let variables_one = one
        .bound_variables()
        .map(|variable| (variable.name(), variable.type_))
        .collect::<HashMap<_, _>>();

    let variables_other = other
        .bound_variables()
        .map(|variable| (variable.name(), variable.type_))
        .collect::<HashMap<_, _>>();

    for (name, type_) in variables_one.iter() {
        if let Some(type_other) = variables_other.get(name)
            && type_other.same_as(type_)
        {
            continue;
        }

        // There's a variable that is not defined in the second branch but
        // is defined in the first one, or it's defined in the second branch
        // but it has an incompatible type.
        return false;
    }

    for (name, _) in variables_other.iter() {
        if !variables_one.contains_key(name) {
            // There's some variables defined in the second branch that are not
            // defined in the first one, so they can't be merged!
            return false;
        }
    }

    // Anything can be merged with a simple todo, or the two bodies must be
    // syntactically equal.
    one.then.is_todo_with_no_message()
        || other.then.is_todo_with_no_message()
        || one.then.syntactically_eq(&other.then)
}

impl<'ast> ast::visit::Visit<'ast> for MergeCaseBranches<'ast> {
    fn visit_typed_expr_case(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        subjects: &'ast [TypedExpr],
        clauses: &'ast [ast::TypedClause],
        compiled_case: &'ast CompiledCase,
    ) {
        // We only trigger the code action if we are within a case expression,
        // otherwise there's no point in exploring the expression any further.
        let case_range = self.edits.src_span_to_lsp_range(*location);
        if !within(self.params.range, case_range) {
            return;
        }

        if let result @ Some(_) = self.select_mergeable_branches(clauses) {
            self.patterns_to_merge = result
        }

        // We still need to visit the case expression in case we want to apply
        // the code action to some case expression that is nested in one of its
        // branches!
        ast::visit::visit_typed_expr_case(self, location, type_, subjects, clauses, compiled_case);
    }
}

/// Code action to add a missing type parameter to custom types.
/// If a custom type is missing a type parameter, as it is the case
/// in the following example, this action will offer to add the
/// type parameter to the type definition.
///
/// Before:
/// ```gleam
/// type Wibble {
///   Wibble(field: t)
/// }
/// ```
///
/// After:
/// ```gleam
/// type Wibble(t) {
///   Wibble(field: t)
/// }
/// ```
///
pub struct AddMissingTypeParameter<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    /// The source location where the parameters should be defined.
    /// This might be a zero-length span if there are no parameters yet,
    /// or it might cover the already existing type parameter definitions.
    parameters_location: Option<SrcSpan>,
    /// If the type definition already had existing parameters before.
    has_existing_parameters: bool,
    /// The set of all type parameter names in the different variants of the type
    /// that are not already part of the type parameter definition on the type.
    missing_parameters: HashSet<EcoString>,
}

impl<'a> AddMissingTypeParameter<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            parameters_location: None,
            has_existing_parameters: false,
            missing_parameters: HashSet::new(),
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(type_parameters_location) = self.parameters_location else {
            return vec![];
        };

        if self.missing_parameters.is_empty() {
            return vec![];
        }

        let mut new_parameters = self.missing_parameters.iter().sorted().join(", ");
        if self.has_existing_parameters {
            let has_trailing_comma = self
                .module
                .extra
                .trailing_commas
                .iter()
                .any(|&trailing_comma| type_parameters_location.contains(trailing_comma));

            if !has_trailing_comma {
                new_parameters.insert_str(0, ", ");
            }

            self.edits
                .insert(type_parameters_location.end - 1, new_parameters);
        } else {
            self.edits
                .insert(type_parameters_location.end, format!("({new_parameters})"));
        }

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Add missing type parameter")
            .kind(CodeActionKind::QUICKFIX)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(true)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for AddMissingTypeParameter<'ast> {
    fn visit_typed_custom_type(&mut self, custom_type: &'ast ast::TypedCustomType) {
        let full_type_definition_range = self.edits.src_span_to_lsp_range(SrcSpan::new(
            custom_type.location.start,
            custom_type.end_position,
        ));

        // Only continue, if the action was selected anywhere within the custom type definition.
        if !overlaps(self.params.range, full_type_definition_range) {
            return;
        }

        self.parameters_location = Some(SrcSpan::new(
            custom_type.name_location.end,
            custom_type.location.end,
        ));

        self.has_existing_parameters = !custom_type.typed_parameters.is_empty();

        // Collect the remaining type parameters from the variant constructors.
        for record in &custom_type.constructors {
            for argument in &record.arguments {
                if let Type::Var { .. } = argument.type_.as_ref()
                    && !custom_type.typed_parameters.contains(&argument.type_)
                {
                    let mut name = EcoString::new();
                    argument.ast.print(&mut name);
                    let _ = self.missing_parameters.insert(name);
                }
            }
        }
    }
}
