use std::{collections::HashSet, iter, sync::Arc};

use crate::{
    ast::{
        self,
        visit::{visit_typed_call_arg, visit_typed_pattern_call_arg, Visit as _},
        AssignName, AssignmentKind, CallArg, FunctionLiteralKind, ImplicitCallArgOrigin, Pattern,
        SrcSpan, TodoKind, TypedArg, TypedAssignment, TypedExpr, TypedModuleConstant, TypedPattern,
        TypedStatement, TypedUse,
    },
    build::{Located, Module},
    io::{BeamCompiler, CommandExecutor, FileSystemReader, FileSystemWriter},
    line_numbers::LineNumbers,
    parse::{extra::ModuleExtra, lexer::str_to_keyword},
    type_::{
        self,
        error::{ModuleSuggestion, VariableOrigin},
        printer::{Names, Printer},
        FieldMap, ModuleValueConstructor, Type, TypeVar, TypedCallArg, ValueConstructor,
    },
    Error, STDLIB_PACKAGE_NAME,
};
use ecow::{eco_format, EcoString};
use heck::ToSnakeCase;
use im::HashMap;
use itertools::Itertools;
use lsp_types::{CodeAction, CodeActionKind, CodeActionParams, Position, Range, TextEdit, Url};
use vec1::{vec1, Vec1};

use super::{
    compiler::LspProjectCompiler,
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

/// A little wrapper around LineNumbers to make it easier to build text edits.
///
struct TextEdits<'a> {
    line_numbers: &'a LineNumbers,
    edits: Vec<TextEdit>,
}

impl<'a> TextEdits<'a> {
    pub fn new(line_numbers: &'a LineNumbers) -> Self {
        TextEdits {
            line_numbers,
            edits: vec![],
        }
    }

    pub fn src_span_to_lsp_range(&self, location: SrcSpan) -> Range {
        src_span_to_lsp_range(location, self.line_numbers)
    }

    pub fn replace(&mut self, location: SrcSpan, new_text: String) {
        self.edits.push(TextEdit {
            range: src_span_to_lsp_range(location, self.line_numbers),
            new_text,
        })
    }

    pub fn insert(&mut self, at: u32, new_text: String) {
        self.replace(SrcSpan { start: at, end: at }, new_text)
    }

    pub fn delete(&mut self, location: SrcSpan) {
        self.replace(location, "".to_string())
    }

    fn delete_range(&mut self, range: Range) {
        self.edits.push(TextEdit {
            range,
            new_text: "".into(),
        })
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
    ) {
        for (subject_idx, subject) in subjects.iter().enumerate() {
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

            self.delete_tuple_tokens(*location, elems.last().map(|elem| elem.location()));

            for clause in clauses {
                match clause.pattern.get(subject_idx) {
                    Some(Pattern::Tuple { location, elems }) => self
                        .delete_tuple_tokens(*location, elems.last().map(|elem| elem.location())),
                    Some(Pattern::Discard { location, .. }) => {
                        self.discard_tuple_items(*location, elems.len())
                    }
                    _ => panic!("safe: we've just checked all patterns must be discards/tuples"),
                }
            }
            let range = self.edits.src_span_to_lsp_range(*location);
            self.hovered = self.hovered || overlaps(self.params.range, range);
        }

        ast::visit::visit_typed_expr_case(self, location, type_, subjects, clauses)
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
            itertools::intersperse(iter::repeat("_").take(tuple_items), ", ").collect(),
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

        let Some(Located::Statement(TypedStatement::Assignment(TypedAssignment {
            value,
            pattern,
            kind: AssignmentKind::Let,
            location,
            annotation: _,
        }))) = module.find_node(location.start)
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
pub struct LabelShorthandSyntax<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
}

impl<'a> LabelShorthandSyntax<'a> {
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

impl<'ast> ast::visit::Visit<'ast> for LabelShorthandSyntax<'_> {
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

        visit_typed_call_arg(self, arg)
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

        visit_typed_pattern_call_arg(self, arg)
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
    selected_call: Option<(SrcSpan, &'a FieldMap, &'a [TypedCallArg])>,
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

        if let Some((call_location, field_map, args)) = self.selected_call {
            let is_use_call = args.iter().any(|arg| arg.is_use_implicit_callback());
            let missing_labels = field_map.missing_labels(args);

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
            let has_comma_after_last_argument =
                if let Some(last_arg) = args.iter().filter(|arg| !arg.is_implicit()).last() {
                    self.module
                        .code
                        .get(last_arg.location.end as usize..=label_insertion_start as usize)
                        .is_some_and(|text| text.contains(','))
                } else {
                    false
                };

            let labels_list = missing_labels
                .map(|label| format!("{label}: todo"))
                .join(", ");

            let has_no_explicit_arguments = args
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
            self.selected_call = Some((location, field_map, args))
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

        let Some(Located::Expression(TypedExpr::Case {
            clauses, subjects, ..
        })) = module.find_node(location.start)
        else {
            continue;
        };

        // Find the start of the line. We can't just use the start of the case
        // expression for cases like:
        //
        //```gleam
        // let value = case a {}
        //```
        //
        // Here, the start of the expression is part-way through the line, meaning
        // we think we are more indented than we actually are
        //
        let mut indent_size = 0;
        let line_start = *edits
            .line_numbers
            .line_starts
            .get(range.start.line as usize)
            .expect("Line number should be valid");
        let chars = module.code.chars();
        let mut chars = chars.skip(line_start as usize);
        // Count indentation
        while chars.next() == Some(' ') {
            indent_size += 1;
        }

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

            // Calculate the number of characters from the start of the line to the end of the
            // last subject, to skip, so we can find the opening brace.
            // That is: the location we want to get to, minus the start of the line which we skipped to begin with,
            // minus the number we skipped for the indent, minus one more because we go one past the end of indentation
            let num_to_skip = last_subject_location - line_start - indent_size as u32 - 1;
            let chars = chars.skip(num_to_skip as usize);
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
        if assignment.kind.is_generated() {
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
            FunctionLiteralKind::Capture => return,
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
    import: &'a ast::Import<EcoString>,
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
    line_numbers: LineNumbers,
    qualified_constructor: Option<QualifiedConstructor<'a>>,
}

impl<'a> QualifiedToUnqualifiedImportFirstPass<'a> {
    fn new(module: &'a Module, params: &'a CodeActionParams, line_numbers: LineNumbers) -> Self {
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
    ) -> Option<&'a ast::Import<EcoString>> {
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
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        arguments: &'ast Vec<ast::TypeAst>,
    ) {
        let range = src_span_to_lsp_range(*location, &self.line_numbers);
        if overlaps(self.params.range, range) {
            if let Some((module_alias, module_location)) = module {
                if let Some(import) =
                    self.module
                        .find_node(module_location.start)
                        .and_then(|node| {
                            if let Located::Annotation(_, ty) = node {
                                if let Some((module, _)) = ty.named_type_name() {
                                    return self.get_module_import(&module, name, ast::Layer::Type);
                                }
                            }
                            None
                        })
                {
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
        ast::visit::visit_type_ast_constructor(self, location, module, name, arguments);
    }

    fn visit_typed_expr_module_select(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        label: &'ast EcoString,
        module_name: &'ast EcoString,
        module_alias: &'ast EcoString,
        constructor: &'ast ModuleValueConstructor,
    ) {
        let range = src_span_to_lsp_range(*location, &self.line_numbers);
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
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<TypedPattern>>,
        module: &'ast Option<(EcoString, SrcSpan)>,
        constructor: &'ast crate::analyse::Inferred<type_::PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        let range = src_span_to_lsp_range(*location, &self.line_numbers);
        if overlaps(self.params.range, range) {
            if let Some((module_alias, _)) = module {
                if let crate::analyse::Inferred::Known(constructor) = constructor {
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

enum QualifiedConstructorType {
    Type,
    RecordValue,
    PatternRecord,
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

    fn remove_module_qualifier(
        &mut self,
        location: SrcSpan,
        constructor: QualifiedConstructorType,
    ) {
        // Find the start and end of the module qualifier

        // The src_span for Type Constructors and Pattern Record Constructors is
        // : option.Option / option.Some but for Record Constructors is: option.Some
        //   ↑           ↑   ↑         ↑                                       ↑   ↑
        // start       end start      end                                    start end
        let span = if matches!(constructor, QualifiedConstructorType::RecordValue) {
            SrcSpan::new(
                location.start - self.qualified_constructor.used_name.len() as u32,
                location.start + 1,
            )
        } else {
            SrcSpan::new(
                location.start,
                location.start + self.qualified_constructor.used_name.len() as u32 + 1, // plus .
            )
        };
        self.edits.delete(span);
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
                .unqualified_values
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
            import: ast::Import { location, .. },
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
            import: ast::Import { location, .. },
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
            import: ast::Import { location, .. },
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
            import: ast::Import { location, .. },
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
                self.remove_module_qualifier(*location, QualifiedConstructorType::Type);
            }
        }
        ast::visit::visit_type_ast_constructor(self, location, module, name, arguments);
    }

    fn visit_typed_expr_module_select(
        &mut self,
        location: &'ast SrcSpan,
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
                self.remove_module_qualifier(*location, QualifiedConstructorType::RecordValue);
            }
        }
        ast::visit::visit_typed_expr_module_select(
            self,
            location,
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
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<TypedPattern>>,
        module: &'ast Option<(EcoString, SrcSpan)>,
        constructor: &'ast crate::analyse::Inferred<type_::PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        if let Some((module_alias, _)) = module {
            if let crate::analyse::Inferred::Known(_) = constructor {
                let QualifiedConstructor {
                    used_name,
                    constructor,
                    layer,
                    ..
                } = &self.qualified_constructor;

                if layer.is_value() && used_name == module_alias && name == constructor {
                    self.remove_module_qualifier(
                        *location,
                        QualifiedConstructorType::PatternRecord,
                    );
                }
            }
        }
        ast::visit::visit_typed_pattern_constructor(
            self,
            location,
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
    let mut first_pass =
        QualifiedToUnqualifiedImportFirstPass::new(module, params, line_numbers.clone());
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
    line_numbers: LineNumbers,
    unqualified_constructor: Option<UnqualifiedConstructor<'a>>,
}

impl<'a> UnqualifiedToQualifiedImportFirstPass<'a> {
    fn new(module: &'a Module, params: &'a CodeActionParams, line_numbers: LineNumbers) -> Self {
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
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        arguments: &'ast Vec<ast::TypeAst>,
    ) {
        if module.is_none()
            && overlaps(
                self.params.range,
                src_span_to_lsp_range(*location, &self.line_numbers),
            )
        {
            self.get_module_import_from_type_constructor(name);
        }

        ast::visit::visit_type_ast_constructor(self, location, module, name, arguments);
    }

    fn visit_typed_expr_var(
        &mut self,
        location: &'ast SrcSpan,
        constructor: &'ast ValueConstructor,
        name: &'ast EcoString,
    ) {
        let range = src_span_to_lsp_range(*location, &self.line_numbers);
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
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<TypedPattern>>,
        module: &'ast Option<(EcoString, SrcSpan)>,
        constructor: &'ast crate::analyse::Inferred<type_::PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        if module.is_none()
            && overlaps(
                self.params.range,
                src_span_to_lsp_range(*location, &self.line_numbers),
            )
        {
            if let crate::analyse::Inferred::Known(constructor) = constructor {
                self.get_module_import_from_value_constructor(&constructor.module, name);
            }
        }

        ast::visit::visit_typed_pattern_constructor(
            self,
            location,
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
        ast::visit::visit_type_ast_constructor(self, location, module, name, arguments);
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
        if layer.is_value() && wanted_constructor.used_name() == name {
            self.add_module_qualifier(*location);
        }
        ast::visit::visit_typed_expr_var(self, location, constructor, name);
    }

    fn visit_typed_pattern_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<TypedPattern>>,
        module: &'ast Option<(EcoString, SrcSpan)>,
        constructor: &'ast crate::analyse::Inferred<type_::PatternConstructor>,
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
    let mut first_pass =
        UnqualifiedToQualifiedImportFirstPass::new(module, params, line_numbers.clone());
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

/// Builder for code action to apply the desugar use expression.
///
pub struct DesugarUse<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    selected_use: Option<&'a TypedUse>,
}

impl<'a> DesugarUse<'a> {
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

        let last_explicit_arg = args.iter().filter(|arg| !arg.is_implicit()).last();
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

impl<'ast> ast::visit::Visit<'ast> for DesugarUse<'ast> {
    fn visit_typed_use(&mut self, use_: &'ast TypedUse) {
        // We only want to take into account the innermost use we find ourselves
        // into, so we can't stop at the first use we find (the outermost one)
        // and have to keep traversing it in case we're inside some nested
        // `use`s.
        let use_src_span = use_.location.merge(&use_.call.location());
        let use_range = self.edits.src_span_to_lsp_range(use_src_span);
        if !within(self.params.range, use_range) {
            return;
        }

        // If the use expression is using patterns that are not just variable
        // assignments then we can't automatically rewrite it as it would result
        // in a syntax error as we can't pattern match in an anonymous function
        // head.
        // At the same time we can't safely add bindings inside the anonymous
        // function body by picking placeholder names as we'd risk shadowing
        // variables coming from the outer scope.
        //
        // So we just skip those use expressions we can't safely rewrite!
        if use_
            .assignments
            .iter()
            .all(|assignment| assignment.pattern.is_variable())
        {
            self.selected_use = Some(use_);
        }

        self.visit_typed_expr(&use_.call);
    }
}

/// Builder for code action to apply the turn into use expression.
///
pub struct TurnIntoUse<'a> {
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

impl<'a> TurnIntoUse<'a> {
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

impl<'ast> ast::visit::Visit<'ast> for TurnIntoUse<'ast> {
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
        ast::Statement::Use(_) | ast::Statement::Assignment(_) => None,
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

/// Builder for code action to apply the turn into use expression.
///
pub struct ExtractVariable<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    position: Option<ExtractVariablePosition>,
    selected_expression: Option<SrcSpan>,
    statement_before_selected_expression: Option<SrcSpan>,
    latest_statement: Option<SrcSpan>,
}

#[derive(PartialEq, Eq, Copy, Clone)]
enum ExtractVariablePosition {
    InsideCaptureBody,
    TopLevelStatement,
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
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some(location) = self.selected_expression else {
            return vec![];
        };

        if let Some(container_location) = self.statement_before_selected_expression {
            let nesting = self
                .edits
                .src_span_to_lsp_range(container_location)
                .start
                .character;
            let nesting = " ".repeat(nesting as usize);
            let content = self
                .module
                .code
                .get(location.start as usize..location.end as usize)
                .expect("selected expression");
            self.edits.insert(
                container_location.start,
                format!("let value = {content}\n{nesting}"),
            );
        }
        self.edits.replace(location, "value".into());

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Extract variable")
            .kind(CodeActionKind::REFACTOR_EXTRACT)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
    }
}

impl<'ast> ast::visit::Visit<'ast> for ExtractVariable<'ast> {
    fn visit_typed_statement(&mut self, stmt: &'ast TypedStatement) {
        // A capture body is comprised of just a single expression statement
        // that is inserted by the compiler, we don't really want to put
        // anything before that; so in this case we avoid tracking it.
        if self.position != Some(ExtractVariablePosition::InsideCaptureBody) {
            self.latest_statement = Some(stmt.location());
        }

        let previous_position = self.position;
        self.position = Some(ExtractVariablePosition::TopLevelStatement);
        ast::visit::visit_typed_statement(self, stmt);
        self.position = previous_position;
    }

    fn visit_typed_expr(&mut self, expr: &'ast TypedExpr) {
        let expr_location = expr.location();
        let expr_range = self.edits.src_span_to_lsp_range(expr_location);

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
        //   let value = 1
        //   let wibble = value
        // }
        // ```
        //
        // Not all that useful!
        //
        if self.position != Some(ExtractVariablePosition::TopLevelStatement)
            && within(self.params.range, expr_range)
        {
            match expr {
                // We don't extract variables, they're already good.
                // And we don't extract module selects by themselves but always
                // want to consider those as part of a function call.
                TypedExpr::Var { .. } | TypedExpr::ModuleSelect { .. } => (),
                _ => {
                    self.selected_expression = Some(expr_location);
                    self.statement_before_selected_expression = self.latest_statement;
                }
            }
        }

        let previous_position = self.position;
        self.position = None;
        ast::visit::visit_typed_expr(self, expr);
        self.position = previous_position;
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
        let previous_position = self.position;
        self.position = match kind {
            // If a fn is a capture `int.wibble(1, _)` its body will consist of
            // just a single expression statement. When visiting we must record
            // we're inside a capture body.
            FunctionLiteralKind::Capture => Some(ExtractVariablePosition::InsideCaptureBody),
            FunctionLiteralKind::Anonymous { .. } | FunctionLiteralKind::Use { .. } => {
                self.position
            }
        };
        ast::visit::visit_typed_expr_fn(self, location, type_, kind, args, body, return_annotation);
        self.position = previous_position;
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

/// Builder for code action to apply the "expand function capture" action.
///
pub struct ExpandFunctionCapture<'a> {
    module: &'a Module,
    params: &'a CodeActionParams,
    edits: TextEdits<'a>,
    location: Option<(SrcSpan, SrcSpan, VariablesNames)>,
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
            location: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(&self.module.ast);

        let Some((function, hole, names)) = self.location else {
            return vec![];
        };

        let name = names.first_available_name("value");
        self.edits.replace(hole, name.clone().into());
        self.edits.insert(function.end, " }".into());
        self.edits.insert(function.start, format!("fn({name}) {{ "));

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
                self.location = Some((
                    *location,
                    arg.location,
                    VariablesNames::from_statements(body),
                ));
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

    fn first_available_name(&self, name: &str) -> EcoString {
        let mut i = 0;
        loop {
            let name = if i == 0 {
                EcoString::from(name)
            } else {
                eco_format!("{name}{i}")
            };

            if !self.names.contains(&name) {
                return name;
            }
            i += 1;
        }
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
}

impl<'ast> ast::visit::Visit<'ast> for GenerateDynamicDecoder<'ast> {
    fn visit_typed_custom_type(&mut self, custom_type: &'ast ast::TypedCustomType) {
        let range = self.edits.src_span_to_lsp_range(custom_type.location);
        if !overlaps(self.params.range, range) {
            return;
        }

        // For now, we only generate dynamic decoders for types with one variant.
        let constructor = match custom_type.constructors.as_slice() {
            [constructor] => constructor,
            _ => return,
        };

        let name = eco_format!("{}_decoder", custom_type.name.to_snake_case());

        let Some(fields): Option<Vec<_>> = constructor
            .arguments
            .iter()
            .map(|argument| {
                Some(RecordField {
                    label: RecordLabel::Labeled(
                        argument.label.as_ref().map(|(_, name)| name.as_str())?,
                    ),
                    type_: &argument.type_,
                })
            })
            .collect()
        else {
            return;
        };

        let mut decoder_printer = DecoderPrinter::new(
            &self.module.ast.names,
            custom_type.name.clone(),
            self.module.name.clone(),
        );

        let decoders = fields
            .iter()
            .map(|field| decoder_printer.decode_field(field, 2))
            .join("\n");

        let decoder_type = self.printer.print_type(&Type::Named {
            publicity: ast::Publicity::Public,
            package: STDLIB_PACKAGE_NAME.into(),
            module: DECODE_MODULE.into(),
            name: "Decoder".into(),
            args: vec![],
            inferred_variant: None,
        });
        let decode_module = self.printer.print_module(DECODE_MODULE);

        let mut field_names = fields.iter().map(|field| field.label.variable_name());
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

        let function = format!(
            "

fn {name}() -> {decoder_type}({type_name}{parameters}) {{
{decoders}
  {decode_module}.success({constructor_name}({fields}:))
}}",
            type_name = custom_type.name,
            constructor_name = constructor.name,
            fields = field_names.join(":, ")
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
            RecordLabel::Unlabeled(mut index) => {
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
        } else if let Some(types) = type_.tuple_types() {
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
        } else {
            let type_information = type_.named_type_information();
            let type_information = type_information.as_ref().map(|(module, name, arguments)| {
                (module.as_str(), name.as_str(), arguments.as_slice())
            });

            match type_information {
                Some(("gleam/dynamic", "Dynamic", _)) => eco_format!("{module_name}.dynamic"),
                Some(("gleam", "List", [element])) => {
                    eco_format!("{module_name}.list({})", self.decoder_for(element, indent))
                }
                Some(("gleam/option", "Option", [some])) => {
                    eco_format!("{module_name}.optional({})", self.decoder_for(some, indent))
                }
                Some(("gleam/dict", "Dict", [key, value])) => {
                    eco_format!(
                        "{module_name}.dict({}, {})",
                        self.decoder_for(key, indent),
                        self.decoder_for(value, indent)
                    )
                }
                Some((module, name, _)) if module == self.type_module && name == self.type_name => {
                    eco_format!("{}_decoder()", name.to_snake_case())
                }
                _ => eco_format!(
                    r#"todo as "Decoder for {}""#,
                    self.printer.print_type(type_)
                ),
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
    /// assignment to destructure a value of the given type. For example given this
    /// type:
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
    /// Be careful how:
    /// - If the type is internal this function will return `None`.
    /// - If the type has multiple constructors, it won't be safe to use
    ///   in a let binding and this function will return `None`.
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
            Type::Tuple { elems } if elems.is_empty() => None,
            Type::Tuple { elems } => Some(vec1![eco_format!(
                "#({})",
                (0..elems.len() as u32)
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

        let index_to_label = match field_map {
            None => HashMap::new(),
            Some(field_map) => field_map
                .fields
                .iter()
                .map(|(label, index)| (index, label))
                .collect::<HashMap<_, _>>(),
        };

        let mut pattern =
            pretty_constructor_name(self.module, constructor_module, constructor_name)?;

        if *constructor_arity == 0 {
            return Some(pattern);
        }

        pattern.push('(');
        let args = if *constructor_arity <= 1 && index_to_label.get(&0).is_none() {
            // If there's a single argument and its not labelled we don't add a
            // number suffix to it and just call it "value".
            String::from("value")
        } else {
            // Otherwise all unlabelled arguments will be called "value_<n>".
            // Labelled arguments, on the other hand, will always use the
            // shorthand syntax.
            (0..*constructor_arity as u32)
                .map(|i| match index_to_label.get(&i) {
                    Some(label) => format!("{label}:"),
                    None => format!("value_{i}"),
                })
                .join(", ")
        };
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
    let Some(module_interface) = compiler.get_module_interface(type_module) else {
        return vec![];
    };
    // If the type is in an internal module that is not the current one, we
    // cannot use its constructors!
    let outside_of_current_module = *current_module != module_interface.name;
    if outside_of_current_module && module_interface.is_internal {
        return vec![];
    }
    let Some(constructors) = module_interface.types_value_constructors.get(type_name) else {
        return vec![];
    };

    constructors
        .variants
        .iter()
        .filter_map(|variant| {
            let constructor = module_interface.get_public_value(&variant.name)?;
            if constructor.publicity.is_public() {
                Some(constructor)
            } else if constructor.publicity.is_internal() && !outside_of_current_module {
                // An internal constructor can only be used from within its own
                // module, otherwise we don't suggest any action.
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
            previous_function_end: Some(insert_at),
            return_type,
        }) = self.function_to_generate
        else {
            return vec![];
        };

        let mut printer = Printer::new(&self.module.ast.names);
        let args = if let [arg_type] = arguments_types.as_slice() {
            let arg_name = arg_type
                .named_type_name()
                .map(|(_type_module, type_name)| type_name.to_snake_case())
                .filter(|name| is_valid_function_name(name))
                .unwrap_or(String::from("value"));

            format!("{arg_name}: {}", printer.print_type(arg_type))
        } else {
            arguments_types
                .iter()
                .enumerate()
                .map(|(index, arg_type)| {
                    let type_ = printer.print_type(arg_type);
                    format!("arg_{}: {}", index + 1, type_)
                })
                .join(", ")
        };
        let return_type = printer.print_type(&return_type);

        self.edits.insert(
            insert_at,
            format!("\n\nfn {name}({args}) -> {return_type} {{\n  todo\n}}"),
        );

        let mut action = Vec::with_capacity(1);
        CodeActionBuilder::new("Generate function")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits.edits)
            .preferred(false)
            .push_to(&mut action);
        action
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
            let name_range = location.start as usize..location.end as usize;
            let candidate_name = self.module.code.get(name_range);
            match (candidate_name, type_.fn_types()) {
                (None, _) | (_, None) => return,
                (Some(name), _) if !is_valid_function_name(name) => return,
                (Some(name), Some((arguments_types, return_type))) => {
                    self.function_to_generate = Some(FunctionToGenerate {
                        name,
                        arguments_types,
                        return_type,
                        previous_function_end: self.last_visited_function_end,
                    })
                }
            }
        }

        ast::visit::visit_typed_expr_invalid(self, location, type_);
    }
}

#[must_use]
fn is_valid_function_name(name: &str) -> bool {
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
