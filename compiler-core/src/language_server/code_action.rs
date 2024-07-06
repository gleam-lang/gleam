use std::{iter, sync::Arc};

use crate::{
    ast::{self, visit::Visit as _, AssignName, AssignmentKind, Pattern, SrcSpan, TypedPattern},
    build::Module,
    line_numbers::LineNumbers,
    parse::extra::ModuleExtra,
    type_::{Type, TypedCallArg},
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

/// Code action to desugar "use" expressions.
///
/// Turns:
/// ```gleam
/// use x <- result.map(Ok(1))
/// io.debug(x)
/// ```
///
/// Into:
/// ```gleam
/// result.map(Ok(1), fn(x) { io.debug(x) })
/// ```
///
/// The code action is available for the i'th subject if:
/// - the whole "use" expression body overlaps with the selection.
/// - if multiple "use" expressions match, only the inner-most is updated.
pub struct DesugarUseExpressionCodeAction<'a> {
    line_numbers: LineNumbers,
    code: &'a EcoString,
    params: &'a CodeActionParams,
    module: &'a ast::TypedModule,
    edits: Vec<TextEdit>,
}

impl<'ast> ast::visit::Visit<'ast> for DesugarUseExpressionCodeAction<'_> {
    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        fun: &'ast ast::TypedExpr,
        args: &'ast [TypedCallArg],
    ) {
        // Perform only a single edit.
        if !self.edits.is_empty() {
            return;
        }
        ast::visit::visit_typed_expr_call(self, location, typ, fun, args);

        // Perform only a single edit.
        if !self.edits.is_empty() {
            return;
        }

        // Perform edit after the "visit_typed_expr_call" recursion call to ensure that the inner-most
        // "use" expression is updated.
        if let Some(edits) = self.on_typed_expr_call(location, fun, args) {
            self.edits.extend(edits);
        }
    }
}

impl<'a> DesugarUseExpressionCodeAction<'a> {
    pub fn new(module: &'a Module, params: &'a CodeActionParams) -> Self {
        Self {
            line_numbers: LineNumbers::new(&module.code),
            code: &module.code,
            params,
            module: &module.ast,
            edits: vec![],
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(self.module);
        if self.edits.is_empty() {
            return vec![];
        }

        self.edits.sort_by_key(|edit| edit.range.start);

        let mut actions = vec![];
        CodeActionBuilder::new("Desugar use expression")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits)
            .preferred(true)
            .push_to(&mut actions);

        actions
    }

    fn on_typed_expr_call<'ast>(
        &mut self,
        location: &'ast SrcSpan,
        fun: &'ast ast::TypedExpr,
        args: &'ast [TypedCallArg],
    ) -> Option<Vec<TextEdit>> {
        if !self.expr_call_starts_with_use(location.start as usize) || args.is_empty() {
            return None;
        }
        let range = src_span_to_lsp_range(*location, &self.line_numbers);
        if !overlaps(self.params.range, range) {
            return None;
        }
        let mut edits: Vec<TextEdit> = vec![];

        let fn_name_location = self.get_fn_name_location(fun, location)?;

        edits.push(self.remove_use_expression(location, &fn_name_location)?);
        edits.extend(self.insert_callback_fn(location, &fn_name_location, args)?);

        Some(edits)
    }

    fn get_fn_name_location(
        &self,
        fun: &ast::TypedExpr,
        use_location: &SrcSpan,
    ) -> Option<SrcSpan> {
        match fun {
            ast::TypedExpr::Var { location, .. } => Some(*location),
            ast::TypedExpr::TupleIndex { location, .. } => Some(*location),
            ast::TypedExpr::ModuleSelect { location, .. } => {
                let start = self
                    .code
                    .get(0..location.start as usize)?
                    .rfind(|c: char| !c.is_ascii_alphanumeric() && c != '.' && c != '_')?
                    as u32;
                if start < use_location.start {
                    return None;
                }
                Some(SrcSpan::new(start + 1, location.end))
            }
            _ => None,
        }
    }

    // Generate a TextEdit to remove the "use _ -> " expression.
    fn remove_use_expression(
        &self,
        use_location: &SrcSpan,
        fn_name_location: &SrcSpan,
    ) -> Option<TextEdit> {
        // Remove "use _ -> " expression.
        Some(TextEdit {
            range: src_span_to_lsp_range(
                SrcSpan::new(use_location.start, fn_name_location.start),
                &self.line_numbers,
            ),
            new_text: "".to_string(),
        })
    }

    fn get_use_indentation(&self, use_location: &SrcSpan) -> Option<usize> {
        // Line number of the "use _ -> wobble()".
        let line_of_use = self.line_numbers.line_number(use_location.start);
        let start_of_line_of_use = self
            .line_numbers
            .line_starts
            .get(line_of_use as usize - 1)?;
        // Additional indentation of the inside of the callback.
        Some((use_location.start - start_of_line_of_use) as usize)
    }

    // Inserts the callback function as "wobble(fn() { ... })", by:
    // 1. Appending "fn(" after the fn_name_location
    // 2. Indenting the body of the callback function
    // 3. Appending "})" after the callback fn body.
    //
    // * use_location - the full "use" expression location.
    // * fn_name_location - the location of the function name being called as part of the "use" expression.
    // * args - all the function call arguments, including the last one which is a callback function.
    fn insert_callback_fn(
        &self,
        use_location: &SrcSpan,
        fn_name_location: &SrcSpan,
        args: &[TypedCallArg],
    ) -> Option<Vec<TextEdit>> {
        let mut edits: Vec<TextEdit> = vec![];

        // The last argument is the "use" body that should be converted to callback fn.
        let callback = args.last()?;
        let use_args = self.extract_use_callback_arguments(callback);

        let callback_location =
            self.get_fn_prefix_location(args, use_location, fn_name_location)?;

        // The callback function that will be inserted.
        let mut fn_prefix_text = format!("fn({}) {{", use_args.join(", ")).to_string();

        if args.len() > 1 {
            fn_prefix_text = format!(", {}", fn_prefix_text);
        }

        if args.len() == 1 {
            edits.push(TextEdit {
                range: src_span_to_lsp_range(callback_location, &self.line_numbers),
                new_text: format!("({}", fn_prefix_text),
            });
        } else {
            edits.push(TextEdit {
                range: src_span_to_lsp_range(callback_location, &self.line_numbers),
                new_text: fn_prefix_text,
            });
        }

        // For indenting the callback body and the labmda closing brackets "})".
        let indent = self.get_use_indentation(use_location)?;

        let line_start = self
            .line_numbers
            .line_and_column_number(callback.location.start);
        let line_end = self
            .line_numbers
            .line_and_column_number(callback.location.end);
        // Iterate through the callback body, and add indentation to each line.
        for i in line_start.line - 1..line_end.line {
            let ls = self.line_numbers.byte_index(i, 0);
            edits.push(TextEdit {
                range: src_span_to_lsp_range(SrcSpan::new(ls + 1, ls + 1), &self.line_numbers),
                new_text: " ".repeat(indent),
            });
        }

        // Add "})" for the callback function.
        let fn_suffix_pos = callback.location.end;
        let mut fn_suffix_text = "\n".to_string();
        fn_suffix_text.push_str(" ".repeat(indent).as_str());
        fn_suffix_text.push_str("})");
        edits.push(TextEdit {
            range: src_span_to_lsp_range(
                SrcSpan::new(fn_suffix_pos, fn_suffix_pos),
                &self.line_numbers,
            ),
            new_text: fn_suffix_text,
        });

        Some(edits)
    }

    // Returns true if the fn call expression starts with "use ".
    fn expr_call_starts_with_use(&self, expr_start: usize) -> bool {
        self.code
            .get(expr_start..expr_start + 4)
            .map(|prefix| prefix == "use " || prefix == "use\t")
            .unwrap_or(false)
    }

    // Returns position to insert the callback function prefix.
    // Example of the prefix: "fn(a, b) {".
    fn get_fn_prefix_location(
        &self,
        args: &[TypedCallArg],
        use_location: &SrcSpan,
        fn_name_location: &SrcSpan,
    ) -> Option<SrcSpan> {
        // The last argument is the "use" body that should be converted to callback.
        let callback = args.last()?;

        let insert_callback_start: usize = if args.len() > 1 {
            // If there are arguments (besides the callback function), use the end of the last argument as the
            // starting position.
            args.get(args.len() - 2).map(|a| a.location.end as usize)?
        } else {
            // If no extra arguments, use the opening parenthesis of the function used by the "use" expression.
            let start = self
                .code
                .get(0..callback.location.start as usize)
                .and_then(|before_start| before_start.rfind('(').map(|f| f + 1));
            match start {
                Some(v) if use_location.start < v as u32 => v - 1,
                // If no "(" found, possibly we're dealing with a function call without parenthesis.
                // Example:  use x <- wobble
                _ => return Some(SrcSpan::new(fn_name_location.end, fn_name_location.end)),
            }
        };

        let insert_callback_end = self
            .code
            .get(0..callback.location.start as usize)
            .and_then(|before_start| before_start.rfind(')').map(|f| f + 1))?;

        Some(SrcSpan::new(
            insert_callback_start as u32,
            insert_callback_end as u32,
        ))
    }

    // Extracts arguments from the use expression.
    // Given "use a, b, c <- wobble()", it returns ["a", "b", "c"].
    fn extract_use_callback_arguments(&self, arg: &ast::CallArg<ast::TypedExpr>) -> Vec<String> {
        return if let ast::TypedExpr::Fn { args, .. } = &arg.value {
            args.iter()
                .map(|a| {
                    self.code
                        .get(a.location.start as usize..a.location.end as usize)
                        .unwrap_or("_")
                        .to_string()
                })
                .collect()
        } else {
            vec![]
        };
    }
}
