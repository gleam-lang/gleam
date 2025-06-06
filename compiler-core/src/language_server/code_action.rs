use std::{collections::HashSet, iter, sync::Arc};

use crate::{
    Error, STDLIB_PACKAGE_NAME, analyse,
    ast::{
        self, AssignName, AssignmentKind, BitArraySegmentTruncation, CallArg, CustomType,
        FunctionLiteralKind, ImplicitCallArgOrigin, Import, PIPE_PRECEDENCE, Pattern,
        PatternUnusedArguments, PipelineAssignmentKind, RecordConstructor, SrcSpan, TodoKind,
        TypedArg, TypedAssignment, TypedExpr, TypedModuleConstant, TypedPattern,
        TypedPipelineAssignment, TypedRecordConstructor, TypedStatement, TypedUse,
        visit::Visit as _,
    },
    build::{Located, Module},
    config::PackageConfig,
    exhaustiveness::CompiledCase,
    io::{BeamCompiler, CommandExecutor, FileSystemReader, FileSystemWriter},
    line_numbers::LineNumbers,
    parse::{extra::ModuleExtra, lexer::str_to_keyword},
    strings::to_snake_case,
    type_::{
        self, FieldMap, ModuleValueConstructor, Type, TypeVar, TypedCallArg, ValueConstructor,
        error::{ModuleSuggestion, VariableDeclaration, VariableOrigin},
        printer::{Names, Printer},
    },
};
use ecow::{EcoString, eco_format};
use im::HashMap;
use itertools::Itertools;
use lsp_types::{CodeAction, CodeActionKind, CodeActionParams, Position, Range, TextEdit, Url};
use vec1::{Vec1, vec1};

use super::{
    TextEdits,
    compiler::LspProjectCompiler,
    edits::{add_newlines_after_import, get_import_edit, position_of_first_definition_if_import},
    engine::{overlaps, within},
    files::FileSystemProxy,
    reference::{VariableReferenceKind, find_variable_references},
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
        // To prevent weird behaviour when `let assert` statements are nested,
        // we only check for the code action between the `let` and `=`.
        let code_action_location =
            SrcSpan::new(assignment.location.start, assignment.value.location().start);
        let code_action_range =
            src_span_to_lsp_range(code_action_location, self.edits.line_numbers);

        self.visit_typed_expr(&assignment.value);

        // Only offer the code action if the cursor is over the statement
        if !overlaps(code_action_range, self.params.range) {
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

        let range = src_span_to_lsp_range(assignment.location, self.edits.line_numbers);

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
        .filter_map(|error| match error {
            type_::Error::InexhaustiveLetAssignment { location, missing } => {
                Some((*location, missing))
            }
            _ => None,
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
}

struct SelectedCall<'a> {
    location: SrcSpan,
    field_map: &'a FieldMap,
    arguments: Vec<CallArg<()>>,
    kind: SelectedCallKind,
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
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        if let Some(SelectedCall {
            location: call_location,
            field_map,
            arguments,
            kind,
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

            // Now we need to figure out if there's a comma at the end of the
            // arguments list:
            //
            //   call(one, |)
            //             ^ Cursor here, with a comma behind
            //
            //   call(one|)
            //           ^ Cursor here, no comma behind, we'll have to add one!
            //
            let label_insertion_start = call_location.end - 1;
            let has_comma_after_last_argument = if let Some(last_arg) = arguments
                .iter()
                .filter(|arg| !arg.is_implicit())
                .next_back()
            {
                self.module
                    .code
                    .get(last_arg.location.end as usize..=label_insertion_start as usize)
                    .is_some_and(|text| text.contains(','))
            } else {
                false
            };

            let format_label = match kind {
                SelectedCallKind::Value => |label| format!("{label}: todo"),
                SelectedCallKind::Pattern => |label| format!("{label}:"),
            };

            let labels_list = missing_labels.map(format_label).join(", ");

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

            self.edits.insert(label_insertion_start, labels_list);

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
        args: &'ast [TypedCallArg],
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
                arguments: args.iter().map(Self::empty_argument).collect(),
                kind: SelectedCallKind::Value,
            })
        }

        // We only want to take into account the innermost function call
        // containing the current selection so we can't stop at the first call
        // we find (the outermost one) and have to keep traversing it in case
        // we're inside a nested call.
        let previous = self.use_right_hand_side_location;
        self.use_right_hand_side_location = None;
        ast::visit::visit_typed_expr_call(self, location, type_, fun, args);
        self.use_right_hand_side_location = previous;
    }

    fn visit_typed_pattern_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name_location: &'ast SrcSpan,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<TypedPattern>>,
        module: &'ast Option<(EcoString, SrcSpan)>,
        constructor: &'ast analyse::Inferred<type_::PatternConstructor>,
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

    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        ast::visit::visit_typed_function(self, fun);

        let code_action_range = self.edits.src_span_to_lsp_range(fun.location);

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
        args: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        ast::visit::visit_typed_expr_fn(self, location, type_, kind, args, body, return_annotation);

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
        for argument in args.iter() {
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
            printer: Printer::new(&module.ast.names),
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

pub struct QualifiedConstructor<'a> {
    import: &'a Import<EcoString>,
    module_aliased: bool,
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

pub struct QualifiedToUnqualifiedImportFirstPass<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    line_numbers: &'a LineNumbers,
    qualified_constructor: Option<QualifiedConstructor<'a>>,
}

impl<'a> QualifiedToUnqualifiedImportFirstPass<'a> {
    fn new(
        module: &'a Module,
        params: &'a CodeActionParams,
        line_numbers: &'a LineNumbers,
    ) -> Self {
        Self {
            module,
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

        for def in &self.module.ast.definitions {
            if let ast::Definition::Import(import) = def {
                let imported = if layer.is_value() {
                    &import.unqualified_values
                } else {
                    &import.unqualified_types
                };

                if import.module != *module_name
                    && imported.iter().any(|imp| imp.used_name() == constructor)
                {
                    return None;
                }

                if import.module == *module_name {
                    matching_import = Some(import);
                }
            }
        }

        matching_import
    }
}

impl<'ast> ast::visit::Visit<'ast> for QualifiedToUnqualifiedImportFirstPass<'ast> {
    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        args: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        for arg in args {
            if let Some(annotation) = &arg.annotation {
                self.visit_type_ast(annotation);
            }
        }
        if let Some(return_) = return_annotation {
            self.visit_type_ast(return_);
        }
        ast::visit::visit_typed_expr_fn(self, location, type_, kind, args, body, return_annotation);
    }

    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        for arg in &fun.arguments {
            if let Some(annotation) = &arg.annotation {
                self.visit_type_ast(annotation);
            }
        }

        if let Some(return_annotation) = &fun.return_annotation {
            self.visit_type_ast(return_annotation);
        }
        ast::visit::visit_typed_function(self, fun);
    }

    fn visit_type_ast_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name_location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        arguments: &'ast Vec<ast::TypeAst>,
    ) {
        let range = src_span_to_lsp_range(*location, self.line_numbers);
        if overlaps(self.params.range, range) {
            if let Some((module_alias, _)) = module {
                if let Some(import) = self.module.find_node(location.end).and_then(|node| {
                    if let Located::Annotation { type_, .. } = node {
                        if let Some((module, _)) = type_.named_type_name() {
                            return self.get_module_import(&module, name, ast::Layer::Type);
                        }
                    }
                    None
                }) {
                    self.qualified_constructor = Some(QualifiedConstructor {
                        import,
                        module_aliased: import.as_name.is_some(),
                        used_name: module_alias.clone(),
                        constructor: name.clone(),
                        layer: ast::Layer::Type,
                    });
                }
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
        // When hovering over a Record Value Constructor, we want to expand the source span to
        // include the module name:
        // option.Some
        //  ↑
        // This allows us to offer a code action when hovering over the module name.
        let range = src_span_to_lsp_range(*location, self.line_numbers);
        if overlaps(self.params.range, range) {
            if let ModuleValueConstructor::Record {
                name: constructor_name,
                ..
            } = constructor
            {
                if let Some(import) =
                    self.get_module_import(module_name, constructor_name, ast::Layer::Value)
                {
                    self.qualified_constructor = Some(QualifiedConstructor {
                        import,
                        module_aliased: import.as_name.is_some(),
                        used_name: module_alias.clone(),
                        constructor: constructor_name.clone(),
                        layer: ast::Layer::Value,
                    });
                }
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
        constructor: &'ast analyse::Inferred<type_::PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        let range = src_span_to_lsp_range(*location, self.line_numbers);
        if overlaps(self.params.range, range) {
            if let Some((module_alias, _)) = module {
                if let analyse::Inferred::Known(constructor) = constructor {
                    if let Some(import) =
                        self.get_module_import(&constructor.module, name, ast::Layer::Value)
                    {
                        self.qualified_constructor = Some(QualifiedConstructor {
                            import,
                            module_aliased: import.as_name.is_some(),
                            used_name: module_alias.clone(),
                            constructor: name.clone(),
                            layer: ast::Layer::Value,
                        });
                    }
                }
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
        let (insert_pos, new_text) = self.determine_insert_position_and_text();
        let span = SrcSpan::new(insert_pos, insert_pos);
        self.edits.replace(span, new_text);
    }

    fn find_last_char_before_closing_brace(&self) -> Option<(usize, char)> {
        let QualifiedConstructor {
            import: Import { location, .. },
            ..
        } = self.qualified_constructor;
        let import_code = self.get_import_code();
        let closing_brace_pos = import_code.rfind('}')?;

        let bytes = import_code.as_bytes();
        let mut pos = closing_brace_pos;
        while pos > 0 {
            pos -= 1;
            let c = (*bytes.get(pos)?) as char;
            if c.is_whitespace() {
                continue;
            }
            if c == '{' {
                break;
            }
            return Some((location.start as usize + pos, c));
        }
        None
    }

    fn get_import_code(&self) -> &str {
        let QualifiedConstructor {
            import: Import { location, .. },
            ..
        } = self.qualified_constructor;
        self.module
            .code
            .get(location.start as usize..location.end as usize)
            .expect("import not found")
    }

    fn determine_insert_position_and_text(&self) -> (u32, String) {
        let QualifiedConstructor { module_aliased, .. } = &self.qualified_constructor;

        let name = self.qualified_constructor.constructor_import();
        let import_code = self.get_import_code();
        let has_brace = import_code.contains('}');

        if has_brace {
            self.insert_into_braced_import(name)
        } else {
            self.insert_into_unbraced_import(name, *module_aliased)
        }
    }

    // Handle inserting into an unbraced import
    fn insert_into_unbraced_import(&self, name: String, module_aliased: bool) -> (u32, String) {
        let QualifiedConstructor {
            import: Import { location, .. },
            ..
        } = self.qualified_constructor;
        if !module_aliased {
            // Case: import module
            (location.end, format!(".{{{}}}", name))
        } else {
            // Case: import module as alias
            let import_code = &self.get_import_code();
            let as_pos = import_code
                .find(" as ")
                .expect("Expected ' as ' in import statement");
            let before_as_pos = import_code
                .get(..as_pos)
                .and_then(|s| s.rfind(|c: char| !c.is_whitespace()))
                .map(|pos| location.start as usize + pos + 1)
                .expect("Expected non-whitespace character before ' as '");
            (before_as_pos as u32, format!(".{{{}}}", name))
        }
    }

    // Handle inserting into a braced import
    fn insert_into_braced_import(&self, name: String) -> (u32, String) {
        let QualifiedConstructor {
            import: Import { location, .. },
            ..
        } = self.qualified_constructor;
        if let Some((pos, c)) = self.find_last_char_before_closing_brace() {
            // Case: import module.{Existing, } (as alias)
            if c == ',' {
                (pos as u32 + 1, format!(" {}", name))
            } else {
                // Case: import module.{Existing} (as alias)
                (pos as u32 + 1, format!(", {}", name))
            }
        } else {
            // Case: import module.{} (as alias)
            let import_code = self.get_import_code();
            let left_brace_pos = import_code
                .find('{')
                .map(|pos| location.start as usize + pos)
                .expect("Expected '{' in import statement");
            (left_brace_pos as u32 + 1, name)
        }
    }
}

impl<'ast> ast::visit::Visit<'ast> for QualifiedToUnqualifiedImportSecondPass<'ast> {
    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        args: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        for arg in args {
            if let Some(annotation) = &arg.annotation {
                self.visit_type_ast(annotation);
            }
        }
        if let Some(return_) = return_annotation {
            self.visit_type_ast(return_);
        }
        ast::visit::visit_typed_expr_fn(self, location, type_, kind, args, body, return_annotation);
    }

    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        for arg in &fun.arguments {
            if let Some(annotation) = &arg.annotation {
                self.visit_type_ast(annotation);
            }
        }

        if let Some(return_annotation) = &fun.return_annotation {
            self.visit_type_ast(return_annotation);
        }
        ast::visit::visit_typed_function(self, fun);
    }

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
        constructor: &'ast analyse::Inferred<type_::PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        if let Some((module_alias, _)) = module {
            if let analyse::Inferred::Known(_) = constructor {
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

pub fn code_action_convert_qualified_constructor_to_unqualified(
    module: &Module,
    line_numbers: &LineNumbers,
    params: &CodeActionParams,
    actions: &mut Vec<CodeAction>,
) {
    let mut first_pass = QualifiedToUnqualifiedImportFirstPass::new(module, params, line_numbers);
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
        self.unqualified_constructor =
            self.module
                .ast
                .definitions
                .iter()
                .find_map(|def| match def {
                    ast::Definition::Import(import) if import.module == *module_name => import
                        .unqualified_values
                        .iter()
                        .find(|value| value.used_name() == constructor_name)
                        .and_then(|value| {
                            Some(UnqualifiedConstructor {
                                constructor: value,
                                module_name: import.used_name()?,
                                layer: ast::Layer::Value,
                            })
                        }),
                    _ => None,
                })
    }

    fn get_module_import_from_type_constructor(&mut self, constructor_name: &EcoString) {
        self.unqualified_constructor =
            self.module
                .ast
                .definitions
                .iter()
                .find_map(|def| match def {
                    ast::Definition::Import(import) => {
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
                    }
                    _ => None,
                })
    }
}

impl<'ast> ast::visit::Visit<'ast> for UnqualifiedToQualifiedImportFirstPass<'ast> {
    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        args: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        for arg in args {
            if let Some(annotation) = &arg.annotation {
                self.visit_type_ast(annotation);
            }
        }
        if let Some(return_) = return_annotation {
            self.visit_type_ast(return_);
        }
        ast::visit::visit_typed_expr_fn(self, location, type_, kind, args, body, return_annotation);
    }

    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        for arg in &fun.arguments {
            if let Some(annotation) = &arg.annotation {
                self.visit_type_ast(annotation);
            }
        }

        if let Some(return_annotation) = &fun.return_annotation {
            self.visit_type_ast(return_annotation);
        }
        ast::visit::visit_typed_function(self, fun);
    }
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
        if overlaps(self.params.range, range) {
            if let Some(module_name) = match &constructor.variant {
                type_::ValueConstructorVariant::ModuleConstant { module, .. }
                | type_::ValueConstructorVariant::ModuleFn { module, .. }
                | type_::ValueConstructorVariant::Record { module, .. } => Some(module),

                type_::ValueConstructorVariant::LocalVariable { .. }
                | type_::ValueConstructorVariant::LocalConstant { .. } => None,
            } {
                self.get_module_import_from_value_constructor(module_name, name);
            }
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
        constructor: &'ast analyse::Inferred<type_::PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        if module.is_none()
            && overlaps(
                self.params.range,
                src_span_to_lsp_range(*location, self.line_numbers),
            )
        {
            if let analyse::Inferred::Known(constructor) = constructor {
                self.get_module_import_from_value_constructor(&constructor.module, name);
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
    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        args: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        for arg in args {
            if let Some(annotation) = &arg.annotation {
                self.visit_type_ast(annotation);
            }
        }
        if let Some(return_) = return_annotation {
            self.visit_type_ast(return_);
        }
        ast::visit::visit_typed_expr_fn(self, location, type_, kind, args, body, return_annotation);
    }

    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        for arg in &fun.arguments {
            if let Some(annotation) = &arg.annotation {
                self.visit_type_ast(annotation);
            }
        }

        if let Some(return_annotation) = &fun.return_annotation {
            self.visit_type_ast(return_annotation);
        }
        ast::visit::visit_typed_function(self, fun);
    }

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
        constructor: &'ast analyse::Inferred<type_::PatternConstructor>,
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

        let TypedExpr::Call { args, fun, .. } = use_.call.as_ref() else {
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
        let callback_label = if args.iter().any(|arg| arg.label.is_some()) {
            fun.field_map()
                .and_then(|field_map| field_map.missing_labels(args).last().cloned())
                .map(|label| eco_format!("{label}: "))
                .unwrap_or(EcoString::from(""))
        } else {
            EcoString::from("")
        };

        // The use callback is not necessarily the last argument. If you have
        // the following function: `wibble(a a, b b) { todo }`
        // And use it like this: `use <- wibble(b: 1)`, the first argument `a`
        // is going to be the use callback, not the last one!
        let use_callback = args.iter().find(|arg| arg.is_use_implicit_callback());
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
        let use_rhs_function_has_some_explicit_args = args
            .iter()
            .filter(|arg| !arg.is_use_implicit_callback())
            .peekable()
            .peek()
            .is_some();

        let use_rhs_function_ends_with_closed_parentheses = self
            .module
            .code
            .get(use_line_end as usize - 1..use_line_end as usize)
            == Some(")");

        let last_explicit_arg = args.iter().filter(|arg| !arg.is_implicit()).next_back();
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
                if use_rhs_function_has_some_explicit_args && !use_rhs_has_comma_after_last_argument
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
                .insert(use_line_end, format!("(fn({}) {{", assignments))
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
    callback_args_span: Option<SrcSpan>,
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
            callback_args_span,
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
        let left_hand_side_text = if let Some(args_location) = callback_args_span {
            let args_start = args_location.start as usize;
            let args_end = args_location.end as usize;
            let args_text = self.module.code.get(args_start..args_end).expect("fn args");
            format!("use {args_text} <- ")
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
    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        // The cursor has to be inside the last statement of the function to
        // offer the code action.
        let last_statement_range = self.edits.src_span_to_lsp_range(fun.body.last().location());
        if within(self.params.range, last_statement_range) {
            if let Some(call_data) = turn_statement_into_use(fun.body.last()) {
                self.selected_call = Some(call_data);
            }
        }

        ast::visit::visit_typed_function(self, fun)
    }

    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        args: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        // The cursor has to be inside the last statement of the body to
        // offer the code action.
        let last_statement_range = self.edits.src_span_to_lsp_range(body.last().location());
        if within(self.params.range, last_statement_range) {
            if let Some(call_data) = turn_statement_into_use(body.last()) {
                self.selected_call = Some(call_data);
            }
        }

        ast::visit::visit_typed_expr_fn(self, location, type_, kind, args, body, return_annotation);
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
        args,
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
    let args = args
        .iter()
        .sorted_by_key(|arg| arg.location.start)
        .collect_vec();

    let CallArg {
        value: last_arg,
        implicit: None,
        ..
    } = args.last()?
    else {
        return None;
    };

    let TypedExpr::Fn {
        args: callback_args,
        body,
        ..
    } = last_arg
    else {
        return None;
    };

    let callback_args_span = match (callback_args.first(), callback_args.last()) {
        (Some(first), Some(last)) => Some(first.location.merge(&last.location)),
        _ => None,
    };

    let arg_before_callback_span = if args.len() >= 2 {
        args.get(args.len() - 2).map(|call_arg| call_arg.location)
    } else {
        None
    };

    let callback_body_span = body.first().location().merge(&body.last().last_location());

    Some(CallLocations {
        call_span: *call_span,
        called_function_span: called_function.location(),
        callback_args_span,
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
    selected_expression: Option<(SrcSpan, Arc<Type>)>,
    statement_before_selected_expression: Option<SrcSpan>,
    latest_statement: Option<SrcSpan>,
    to_be_wrapped: bool,
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
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let (Some((expression_span, expression_type)), Some(insert_location)) = (
            self.selected_expression,
            self.statement_before_selected_expression,
        ) else {
            return vec![];
        };

        let mut name_generator = NameGenerator::new();
        let variable_name = name_generator.generate_name_from_type(&expression_type);

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
        let mut insertion = format!("let {variable_name} = {content}");
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
            .insert(insert_location.start, insertion + &format!("\n{indent}"));
        self.edits
            .replace(expression_span, String::from(variable_name));

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
    fn visit_typed_statement(&mut self, stmt: &'ast TypedStatement) {
        let range = self.edits.src_span_to_lsp_range(stmt.location());
        if !within(self.params.range, range) {
            self.latest_statement = Some(stmt.location());
            ast::visit::visit_typed_statement(self, stmt);
            return;
        }

        match self.position {
            // A capture body is comprised of just a single expression statement
            // that is inserted by the compiler, we don't really want to put
            // anything before that; so in this case we avoid tracking it.
            Some(ExtractVariablePosition::InsideCaptureBody) => {}
            Some(ExtractVariablePosition::PipelineCall) => {
                // Insert above the pipeline start
                self.latest_statement = Some(stmt.location());
            }
            _ => {
                // Insert below the previous statement
                self.latest_statement = Some(stmt.location());
                self.statement_before_selected_expression = self.latest_statement;
            }
        }

        self.at_position(ExtractVariablePosition::TopLevelStatement, |this| {
            ast::visit::visit_typed_statement(this, stmt);
        });
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

        // When visiting the assignments or the final pipeline call we want to
        // keep track of out position so that we can avoid extracting those.
        let all_assignments =
            iter::once(first_value).chain(assignments.iter().map(|(assignment, _kind)| assignment));

        for assignment in all_assignments {
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
            | None => {
                match expr {
                    // We don't extract variables, they're already good.
                    // And we don't extract module selects by themselves but always
                    // want to consider those as part of a function call.
                    TypedExpr::Var { .. } | TypedExpr::ModuleSelect { .. } => (),
                    _ => {
                        self.selected_expression = Some((expr_location, expr.type_()));

                        if !matches!(self.position, Some(ExtractVariablePosition::CallArg)) {
                            self.statement_before_selected_expression = self.latest_statement;
                        }
                    }
                }
            }
        }

        match expr {
            // Expressions that don't make sense to extract
            TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Invalid { .. }
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
                self.selected_expression = Some((*location, expr.type_()));
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
        args: &'ast [TypedArg],
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
                args,
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
                args,
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
    fn visit_typed_expr_invalid(&mut self, location: &'ast SrcSpan, _type_: &'ast Arc<Type>) {
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
    /// The location of the start of the function containing the expression
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
    let mc = match module_constants {
        None => &module
            .ast
            .definitions
            .iter()
            .filter_map(|definition| match definition {
                ast::Definition::ModuleConstant(module_constant) => Some(&module_constant.name),

                ast::Definition::Function(_)
                | ast::Definition::TypeAlias(_)
                | ast::Definition::CustomType(_)
                | ast::Definition::Import(_) => None,
            })
            .collect(),
        Some(mc) => mc,
    };

    match expr {
        // Attempt to extract whole list as long as it's comprised of only literals
        TypedExpr::List { elements, tail, .. } => {
            elements
                .iter()
                .all(|element| can_be_constant(module, element, Some(mc)))
                && tail.is_none()
        }

        // Attempt to extract whole bit array as long as it's made up of literals
        TypedExpr::BitArray { segments, .. } => {
            segments
                .iter()
                .all(|segment| can_be_constant(module, &segment.value, Some(mc)))
                && segments.iter().all(|segment| {
                    segment.options.iter().all(|option| match option {
                        ast::BitArrayOption::Size { value, .. } => {
                            can_be_constant(module, value, Some(mc))
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
            .all(|element| can_be_constant(module, element, Some(mc))),

        // Extract literals directly
        TypedExpr::Int { .. } | TypedExpr::Float { .. } | TypedExpr::String { .. } => true,

        // Extract non-record types directly
        TypedExpr::Var {
            constructor, name, ..
        } => {
            matches!(
                constructor.variant,
                type_::ValueConstructorVariant::Record { arity: 0, .. }
            ) || mc.contains(name)
        }

        // Extract record types as long as arguments can be constant
        TypedExpr::Call { args, fun, .. } => {
            fun.is_record_builder()
                && args
                    .iter()
                    .all(|arg| can_be_constant(module, &arg.value, module_constants))
        }

        // Extract concat binary operation if both sides can be constants
        TypedExpr::BinOp {
            name, left, right, ..
        } => {
            matches!(name, ast::BinOp::Concatenate)
                && can_be_constant(module, left, Some(mc))
                && can_be_constant(module, right, Some(mc))
        }

        TypedExpr::Block { .. }
        | TypedExpr::Pipeline { .. }
        | TypedExpr::Fn { .. }
        | TypedExpr::Case { .. }
        | TypedExpr::RecordAccess { .. }
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
    let already_taken_names = VariablesNames {
        names: module
            .ast
            .definitions
            .iter()
            .filter_map(|definition| match definition {
                ast::Definition::ModuleConstant(constant) => Some(constant.name.clone()),
                ast::Definition::Function(function) => function.name.as_ref().map(|n| n.1.clone()),

                ast::Definition::TypeAlias(_)
                | ast::Definition::CustomType(_)
                | ast::Definition::Import(_) => None,
            })
            .collect(),
    };
    name_generator.reserve_variable_names(already_taken_names);

    name_generator.generate_name_from_type(&expr.type_())
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
    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        let fun_location = fun.location;
        let fun_range = self.edits.src_span_to_lsp_range(SrcSpan {
            start: fun_location.start,
            end: fun.end_position,
        });

        if !within(self.params.range, fun_range) {
            return;
        }
        self.container_function_start = Some(fun.location.start);

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
            self.name_to_use = match &assignment.pattern {
                Pattern::Variable { name, .. } => Some(name.clone()),
                _ => None,
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
        args: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        let fn_range = self.edits.src_span_to_lsp_range(*location);
        if within(self.params.range, fn_range) && kind.is_capture() {
            if let [arg] = args {
                self.function_capture_data = Some(FunctionCaptureData {
                    function_span: *location,
                    hole_span: arg.location,
                    hole_type: arg.type_.clone(),
                    reserved_names: VariablesNames::from_statements(body),
                });
            }
        }

        ast::visit::visit_typed_expr_fn(self, location, type_, kind, args, body, return_annotation)
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
pub struct GenerateDynamicDecoder<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    printer: Printer<'a>,
    actions: &'a mut Vec<CodeAction>,
}

const DECODE_MODULE: &str = "gleam/dynamic/decode";

impl<'a> GenerateDynamicDecoder<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
        actions: &'a mut Vec<CodeAction>,
    ) -> Self {
        let printer = Printer::new(&module.ast.names);
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            printer,
            actions,
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

        let mut branches = Vec::with_capacity(constructors_size);
        for constructor in iter::once(first).chain(rest) {
            let body = self.constructor_decoder(mode, custom_type, constructor, 4)?;
            let name = to_snake_case(&constructor.name);
            branches.push(eco_format!(r#"    "{name}" -> {body}"#));
        }

        let cases = branches.join("\n");
        let type_name = &custom_type.name;
        Some(eco_format!(
            r#"{{
  {discriminant}
  case variant {{
{cases}
    _ -> {module}.failure(todo as "Zero value for {type_name}", "{type_name}")
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
            &self.module.ast.names,
            custom_type.name.clone(),
            self.module.name.clone(),
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
}

impl<'ast> ast::visit::Visit<'ast> for GenerateDynamicDecoder<'ast> {
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
            publicity: ast::Publicity::Public,
            package: STDLIB_PACKAGE_NAME.into(),
            module: DECODE_MODULE.into(),
            name: "Decoder".into(),
            args: vec![],
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

struct DecoderPrinter<'a> {
    printer: Printer<'a>,
    /// The name of the root type we are printing a decoder for
    type_name: EcoString,
    /// The module name of the root type we are printing a decoder for
    type_module: EcoString,
}

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

impl<'a> DecoderPrinter<'a> {
    fn new(names: &'a Names, type_name: EcoString, type_module: EcoString) -> Self {
        Self {
            type_name,
            type_module,
            printer: Printer::new(names),
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
        let printer = Printer::new(&module.ast.names);
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
        let mut branches = Vec::with_capacity(constructors_size);
        for constructor in iter::once(first).chain(rest) {
            let RecordConstructor { name, .. } = constructor;
            let encoder =
                self.constructor_encoder(mode, constructor, custom_type.name.clone(), 4)?;
            let unpacking = if constructor.arguments.is_empty() {
                ""
            } else {
                &eco_format!(
                    "({}:)",
                    constructor
                        .arguments
                        .iter()
                        .filter_map(|argument| {
                            argument.label.as_ref().map(|(_location, label)| label)
                        })
                        .join(":, ")
                )
            };
            branches.push(eco_format!("    {name}{unpacking} -> {encoder}"));
        }

        let branches = branches.join("\n");
        Some(eco_format!(
            "case {record_name} {{
{branches}
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
            JsonEncoderPrinter::new(&self.module.ast.names, type_name, self.module.name.clone());

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
            publicity: ast::Publicity::Public,
            package: JSON_PACKAGE_NAME.into(),
            module: JSON_MODULE.into(),
            name: "Json".into(),
            args: vec![],
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

struct JsonEncoderPrinter<'a> {
    printer: Printer<'a>,
    /// The name of the root type we are printing an encoder for
    type_name: EcoString,
    /// The module name of the root type we are printing an encoder for
    type_module: EcoString,
}

impl<'a> JsonEncoderPrinter<'a> {
    fn new(names: &'a Names, type_name: EcoString, type_module: EcoString) -> Self {
        Self {
            type_name,
            type_module,
            printer: Printer::new(names),
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
        } else {
            match type_.tuple_types() {
                Some(types) => {
                    let (tuple, new_indent) = if is_capture {
                        ("value", indent + 4)
                    } else {
                        (encoded_value, indent + 2)
                    };

                    let encoders = types
                        .iter()
                        .enumerate()
                        .map(|(index, type_)| {
                            self.encoder_for(&format!("{tuple}.{index}"), type_, new_indent)
                        })
                        .collect_vec();

                    if is_capture {
                        eco_format!(
                            "fn(value) {{
{indent}  {module_name}.preprocessed_array([
{indent}    {encoders},
{indent}  ])
{indent}}}",
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
{indent}  {some}(value) -> {encoder}
{indent}}}",
                                indent = " ".repeat(indent),
                                none = self
                                    .printer
                                    .print_constructor(&"gleam/option".into(), &"None".into()),
                                some = self
                                    .printer
                                    .print_constructor(&"gleam/option".into(), &"Some".into()),
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
    selected_value: Option<PatternMatchedValue<'a>>,
    edits: TextEdits<'a>,
}

/// A value we might want to pattern match on.
/// Each variant will also contain all the info needed to know how to properly
/// print and format the corresponding pattern matching code; that's why you'll
/// see `Range`s and `SrcSpan` besides the type of the thing being matched.
///
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
        variable_type: &'a Arc<Type>,
        /// The location of the entire let assignment the variable is part of,
        /// so that we can add the pattern matching _after_ it.
        ///
        assignment_location: SrcSpan,
    },
}

impl<'a, IO> PatternMatchOnValue<'a, IO>
where
    IO: CommandExecutor + FileSystemWriter + FileSystemReader + BeamCompiler + Clone,
{
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
            edits: TextEdits::new(line_numbers),
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let action_title = match self.selected_value {
            Some(PatternMatchedValue::FunctionArgument {
                arg,
                first_statement: function_body,
                function_range,
            }) => {
                self.match_on_function_argument(arg, function_body, function_range);
                "Pattern match on argument"
            }
            Some(PatternMatchedValue::LetVariable {
                variable_name,
                variable_type,
                assignment_location,
            }) => {
                self.match_on_let_variable(variable_name, variable_type, assignment_location);
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

        let Some(patterns) = self.type_to_destructure_patterns(arg.type_.as_ref()) else {
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
            _ => false,
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
        variable_type: &Arc<Type>,
        assignment_location: SrcSpan,
    ) {
        let Some(patterns) = self.type_to_destructure_patterns(variable_type.as_ref()) else {
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
    fn type_to_destructure_patterns(&mut self, type_: &Type) -> Option<Vec1<EcoString>> {
        match type_ {
            Type::Fn { .. } => None,
            Type::Var { type_ } => self.type_var_to_destructure_patterns(&type_.borrow()),
            Type::Named {
                module: type_module,
                name: type_name,
                ..
            } => {
                let patterns =
                    get_type_constructors(self.compiler, &self.module.name, type_module, type_name)
                        .iter()
                        .filter_map(|c| self.record_constructor_to_destructure_pattern(c))
                        .collect_vec();

                Vec1::try_from_vec(patterns).ok()
            }
            // We don't want to suggest this action for empty tuple as it
            // doesn't make a lot of sense to match on those.
            Type::Tuple { elements } if elements.is_empty() => None,
            Type::Tuple { elements } => Some(vec1![eco_format!(
                "#({})",
                (0..elements.len() as u32)
                    .map(|i| format!("value_{i}"))
                    .join(", ")
            )]),
        }
    }

    fn type_var_to_destructure_patterns(&mut self, type_var: &TypeVar) -> Option<Vec1<EcoString>> {
        match type_var {
            TypeVar::Unbound { .. } | TypeVar::Generic { .. } => None,
            TypeVar::Link { type_ } => self.type_to_destructure_patterns(type_),
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

        let mut name_generator = NameGenerator::new();
        let index_to_label = match field_map {
            None => HashMap::new(),
            Some(field_map) => {
                name_generator.reserve_all_labels(field_map);

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
        let args = (0..*constructor_arity as u32)
            .map(|i| match index_to_label.get(&i) {
                Some(label) => eco_format!("{label}:"),
                None => match arguments_types.get(i as usize) {
                    None => name_generator.rename_to_avoid_shadowing(EcoString::from("value")),
                    Some(type_) => name_generator.generate_name_from_type(type_),
                },
            })
            .join(", ");

        pattern.push_str(&args);
        pattern.push(')');
        Some(pattern)
    }
}

impl<'ast, IO> ast::visit::Visit<'ast> for PatternMatchOnValue<'ast, IO>
where
    IO: CommandExecutor + FileSystemWriter + FileSystemReader + BeamCompiler + Clone,
{
    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
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
            if within(self.params.range, arg_range) {
                self.selected_value = Some(PatternMatchedValue::FunctionArgument {
                    arg,
                    first_statement: fun.body.first(),
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
        args: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        // If we're not inside the function there's no point in exploring its
        // ast further.
        let function_range = self.edits.src_span_to_lsp_range(*location);
        if !within(self.params.range, function_range) {
            return;
        }

        for arg in args {
            // If the cursor is placed on one of the arguments, then we can try
            // and generate code for that one.
            let arg_range = self.edits.src_span_to_lsp_range(arg.location);
            if within(self.params.range, arg_range) {
                self.selected_value = Some(PatternMatchedValue::FunctionArgument {
                    arg,
                    first_statement: body.first(),
                    function_range,
                });
                return;
            }
        }

        // If the cursor is not on any of the function arguments then we keep
        // exploring the function body as we might want to destructure the
        // argument of an expression function!
        ast::visit::visit_typed_expr_fn(self, location, type_, kind, args, body, return_annotation);
    }

    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        if let Pattern::Variable {
            name,
            location,
            type_,
            ..
        } = &assignment.pattern
        {
            let variable_range = self.edits.src_span_to_lsp_range(*location);
            if within(self.params.range, variable_range) {
                self.selected_value = Some(PatternMatchedValue::LetVariable {
                    variable_name: name,
                    variable_type: type_,
                    assignment_location: assignment.location,
                });
                // If we've found the variable to pattern match on, there's no
                // point in keeping traversing the AST.
                return;
            }
        }

        ast::visit::visit_typed_assignment(self, assignment);
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
) -> Vec<&'a ValueConstructor>
where
    IO: CommandExecutor + FileSystemWriter + FileSystemReader + BeamCompiler + Clone,
{
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
        type_::printer::NameContextInformation::Unimported(_) => None,
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
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    last_visited_function_end: Option<u32>,
    function_to_generate: Option<FunctionToGenerate<'a>>,
}

struct FunctionToGenerate<'a> {
    name: &'a str,
    arguments_types: Vec<Arc<Type>>,

    /// The arguments actually supplied as input to the function, if any.
    /// A function to generate might as well be just a name passed as an argument
    /// `list.map([1, 2, 3], to_generate)` so it's not guaranteed to actually
    /// have any actual arguments!
    given_arguments: Option<&'a [TypedCallArg]>,
    return_type: Arc<Type>,
    previous_function_end: Option<u32>,
}

impl<'a> GenerateFunction<'a> {
    pub fn new(
        module: &'a Module,
        line_numbers: &'a LineNumbers,
        params: &'a CodeActionParams,
    ) -> Self {
        Self {
            module,
            params,
            edits: TextEdits::new(line_numbers),
            last_visited_function_end: None,
            function_to_generate: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(FunctionToGenerate {
            name,
            arguments_types,
            given_arguments,
            previous_function_end: Some(insert_at),
            return_type,
        }) = self.function_to_generate
        else {
            return vec![];
        };

        // Labels do not share the same namespace as argument so we use two separate
        // generators to avoid renaming a label in case it shares a name with an argument.
        let mut label_names = NameGenerator::new();
        let mut argument_names = NameGenerator::new();
        let mut printer = Printer::new(&self.module.ast.names);
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

        self.edits.insert(
            insert_at,
            format!("\n\nfn {name}({arguments}) -> {return_type} {{\n  todo\n}}"),
        );

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Generate function")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }

    fn try_save_function_to_generate(
        &mut self,
        function_name_location: SrcSpan,
        function_type: &Arc<Type>,
        given_arguments: Option<&'a [TypedCallArg]>,
    ) {
        let name_range = function_name_location.start as usize..function_name_location.end as usize;
        let candidate_name = self.module.code.get(name_range);
        match (candidate_name, function_type.fn_types()) {
            (None, _) | (_, None) => (),
            (Some(name), _) if !is_valid_lowercase_name(name) => (),
            (Some(name), Some((arguments_types, return_type))) => {
                self.function_to_generate = Some(FunctionToGenerate {
                    name,
                    arguments_types,
                    given_arguments,
                    return_type,
                    previous_function_end: self.last_visited_function_end,
                })
            }
        }
    }
}

impl<'ast> ast::visit::Visit<'ast> for GenerateFunction<'ast> {
    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        self.last_visited_function_end = Some(fun.end_position);
        ast::visit::visit_typed_function(self, fun);
    }

    fn visit_typed_expr_invalid(&mut self, location: &'ast SrcSpan, type_: &'ast Arc<Type>) {
        let invalid_range = self.edits.src_span_to_lsp_range(*location);
        if within(self.params.range, invalid_range) {
            self.try_save_function_to_generate(*location, type_, None);
        }

        ast::visit::visit_typed_expr_invalid(self, location, type_);
    }

    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        args: &'ast [TypedCallArg],
    ) {
        // If the function being called is invalid we need to generate a
        // function that has the proper labels.
        let fun_range = self.edits.src_span_to_lsp_range(fun.location());

        if within(self.params.range, fun_range) && fun.is_invalid() {
            if labels_are_correct(args) {
                self.try_save_function_to_generate(fun.location(), &fun.type_(), Some(args));
            }
        } else {
            ast::visit::visit_typed_expr_call(self, location, type_, fun, args);
        }
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
            Arguments::Patterns(call_args) => call_args.get(index).map(Argument::Pattern),
            Arguments::Expressions(call_args) => call_args.get(index).map(Argument::Expression),
        }
    }

    fn types(&self) -> Vec<Arc<Type>> {
        match self {
            Arguments::Expressions(call_args) => call_args
                .iter()
                .map(|argument| argument.value.type_())
                .collect_vec(),

            Arguments::Patterns(call_args) => call_args
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

impl<'a, IO> GenerateVariant<'a, IO>
where
    IO: FileSystemReader + FileSystemWriter + BeamCompiler + CommandExecutor + Clone,
{
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
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(variant_module, variant_edits)
            .preferred(false)
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
        let name_range = function_name_location.start as usize..function_name_location.end as usize;
        let name = self.module.code.get(name_range).expect("valid code range");
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
        let (end_position, type_braces) =
            (module.ast.definitions.iter()).find_map(|definition| match definition {
                ast::Definition::CustomType(custom_type) if custom_type.name == type_name => {
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
                }
                _ => None,
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

impl<'ast, IO> ast::visit::Visit<'ast> for GenerateVariant<'ast, IO>
where
    IO: FileSystemReader + FileSystemWriter + BeamCompiler + CommandExecutor + Clone,
{
    fn visit_typed_expr_invalid(&mut self, location: &'ast SrcSpan, type_: &'ast Arc<Type>) {
        let invalid_range = src_span_to_lsp_range(*location, self.line_numbers);
        if within(self.params.range, invalid_range) {
            self.try_save_variant_to_generate(*location, type_, None);
        }
        ast::visit::visit_typed_expr_invalid(self, location, type_);
    }

    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        args: &'ast [TypedCallArg],
    ) {
        // If the function being called is invalid we need to generate a
        // function that has the proper labels.
        let fun_range = src_span_to_lsp_range(fun.location(), self.line_numbers);
        if within(self.params.range, fun_range) && fun.is_invalid() {
            if labels_are_correct(args) {
                self.try_save_variant_to_generate(
                    fun.location(),
                    &fun.type_(),
                    Some(Arguments::Expressions(args)),
                );
            }
        } else {
            ast::visit::visit_typed_expr_call(self, location, type_, fun, args);
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
        constructor: &'ast analyse::Inferred<type_::PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        let pattern_range = src_span_to_lsp_range(*location, self.line_numbers);
        // TODO)) Solo se il pattern non è valido!!!!!
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
fn labels_are_correct<A>(args: &[CallArg<A>]) -> bool {
    let mut labelled_arg_found = false;
    let mut used_labels = HashSet::new();

    for arg in args {
        match &arg.label {
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
            _ => None,
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

    fn maybe_inline(&mut self, location: SrcSpan) {
        let reference = match find_variable_references(&self.module.ast, location).as_slice() {
            [only_reference] => *only_reference,
            _ => return,
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
    fn visit_typed_expr_var(
        &mut self,
        location: &'ast SrcSpan,
        constructor: &'ast ValueConstructor,
        _name: &'ast EcoString,
    ) {
        let range = self.edits.src_span_to_lsp_range(*location);

        if !overlaps(self.params.range, range) {
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
            | VariableDeclaration::FunctionParameter
            | VariableDeclaration::Generated => return,
        }

        self.maybe_inline(*location);
    }

    fn visit_typed_pattern_variable(
        &mut self,
        location: &'ast SrcSpan,
        _name: &'ast EcoString,
        _type: &'ast Arc<Type>,
        origin: &'ast VariableOrigin,
    ) {
        // We can only inline variables assigned by `let` statements, as it
        //doesn't make sense to do so with any other kind of variable.
        match origin.declaration {
            VariableDeclaration::LetPattern => {}
            VariableDeclaration::UsePattern
            | VariableDeclaration::ClausePattern
            | VariableDeclaration::FunctionParameter
            | VariableDeclaration::Generated => return,
        }

        let range = self.edits.src_span_to_lsp_range(*location);

        if !overlaps(self.params.range, range) {
            return;
        }

        self.maybe_inline(*location);
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
    /// this will be true if we're visiting the call on the right hand side of a
    /// use expression. So we can skip it and not try to turn it into a
    /// function.
    visiting_use_call: bool,
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
            visiting_use_call: false,
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

        let arg_range = if arg.uses_label_shorthand() {
            arg.location.start as usize..arg.location.end as usize - 1
        } else if arg.label.is_some() {
            let value = arg.value.location();
            value.start as usize..value.end as usize
        } else {
            arg.location.start as usize..arg.location.end as usize
        };

        let arg_text = self.module.code.get(arg_range).expect("invalid srcspan");
        let arg_text = match arg.value {
            // If the expression being piped is a binary operation with
            // precedence lower than pipes then we have to wrap it in curly
            // braces to not mess with the order of operations.
            TypedExpr::BinOp { name, .. } if name.precedence() < PIPE_PRECEDENCE => {
                &format!("{{ {arg_text} }}")
            }
            _ => arg_text,
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
        args: &'ast [TypedCallArg],
    ) {
        if args.iter().any(|arg| arg.is_capture_hole()) {
            return;
        }

        // If we're visiting the typed function produced by typing a use, we
        // skip the thing itself and only visit its arguments and called
        // function, that is the body of the use.
        if self.visiting_use_call {
            self.visiting_use_call = false;
            ast::visit::visit_typed_expr(self, fun);
            args.iter()
                .for_each(|arg| ast::visit::visit_typed_call_arg(self, arg));
            return;
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
        let argument_to_pipe = args
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
            .or_else(|| args.first().map(|arg| (0, arg)));

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
            next_arg: args.get(position + 1).map(|arg| arg.location),
        })
    }

    fn visit_typed_expr_pipeline(
        &mut self,
        _location: &'ast SrcSpan,
        first_value: &'ast TypedPipelineAssignment,
        _assignments: &'ast [(TypedPipelineAssignment, PipelineAssignmentKind)],
        _finally: &'ast TypedExpr,
        _finally_kind: &'ast PipelineAssignmentKind,
    ) {
        // We can only apply the action on the first step of a pipeline, so we
        // visit just that one and skip all the others.
        ast::visit::visit_typed_pipeline_assignment(self, first_value);
    }

    fn visit_typed_use(&mut self, use_: &'ast TypedUse) {
        self.visiting_use_call = true;
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

                if is_valid_lowercase_name(name) {
                    self.edits
                        .insert(value_location.start, format!("\" <> {name} <> \""));
                    self.edits.delete(value_location);
                } else if self.can_split_string_at(value_location.end) {
                    // If the string is not a valid name we just try and split
                    // the string at the end of the selection.
                    self.edits
                        .insert(value_location.end, "\" <> todo <> \"".into());
                } else {
                    // Otherwise there's no meaningful action we can do.
                    return vec![];
                }
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

        let interpolation = if start == end {
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

            let positional_args = positional
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
            let positional_args = if has_arguments_after {
                format!("{positional_args}, ")
            } else {
                positional_args
            };

            self.edits.insert(insert_at, positional_args);
        }

        if !labelled.is_empty() {
            // If there's labelled arguments to add, we replace the existing spread
            // with the arguments to be added. This way commas and all should already
            // be correct.
            let labelled_args = labelled
                .iter()
                .map(|(label, _)| format!("{label}:"))
                .join(", ");
            self.edits.replace(spread_location, labelled_args);
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
        {
            if let Some(PatternUnusedArguments {
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
                    .filter(|arg| !arg.is_implicit())
                    .next_back()
                    .map(|arg| arg.location.end);

                self.data = Some(FillUnusedFieldsData {
                    positional,
                    labelled,
                    first_labelled_argument_start,
                    last_argument_end,
                    spread_location: *spread_location,
                });
            };
        }

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
    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        self.visit_function_statements(&fun.body);
    }

    fn visit_typed_expr_fn(
        &mut self,
        _location: &'ast SrcSpan,
        _type_: &'ast Arc<Type>,
        _kind: &'ast FunctionLiteralKind,
        _args: &'ast [TypedArg],
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

        ast::visit::visit_typed_expr_echo(self, location, type_, expression);
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
///
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
        CodeActionBuilder::new(&format!("Replace with `{}`", replacement))
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
    imports: Vec<&'a Import<EcoString>>,
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
            imports: vec![],
        }
    }

    /// Given an import location, returns a list of the spans of all the
    /// unqualified values it's importing. Sorted by SrcSpan location.
    ///
    fn imported_values(&self, import_location: SrcSpan) -> Vec<SrcSpan> {
        self.imports
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
        self.visit_typed_module(&self.module.ast);
        if self.imports.is_empty() {
            return vec![];
        }

        let unused_imports = (self.module.ast.type_info.warnings.iter())
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
                _ => None,
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

impl<'ast> ast::visit::Visit<'ast> for RemoveUnusedImports<'ast> {
    fn visit_typed_module(&mut self, module: &'ast ast::TypedModule) {
        self.imports = module
            .definitions
            .iter()
            .filter_map(|definition| match definition {
                ast::Definition::Import(import) => Some(import),
                _ => None,
            })
            .collect_vec();
    }
}
