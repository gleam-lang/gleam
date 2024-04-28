use std::sync::Arc;

use ecow::EcoString;
use lsp_types::{CodeAction, CodeActionKind, CodeActionParams, TextEdit, Url};

use crate::{
    ast::{self, visit::Visit as _, SrcSpan},
    build,
    line_numbers::LineNumbers,
    parse::extra::ModuleExtra,
    type_::Type,
};

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
        for subject in subjects {
            let ast::TypedExpr::Tuple {
                location, elems, ..
            } = subject
            else {
                continue;
            };

            let range = src_span_to_lsp_range(*location, &self.line_numbers);
            self.hovered = self.hovered || overlaps(self.params.range, range);

            let code = self
                .code
                .get(location.start as usize..location.end as usize)
                .expect("valid span");

            // Delete `#`
            self.edits.push(TextEdit {
                range: src_span_to_lsp_range(
                    SrcSpan::new(location.start, location.start + 1),
                    &self.line_numbers,
                ),
                new_text: "".to_string(),
            });

            // Delete `(`
            let (lparen_offset, _) = code
                .match_indices('(')
                .find(|(i, _)| !self.extra.contains(*i as u32))
                .expect("`(` not found in tuple");

            self.edits.push(TextEdit {
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
            if let Some(last_elem) = elems.last() {
                let last_elem_span = last_elem.location();

                // Get the code after the last element until the tuple's `)`
                let code = self
                    .code
                    .get(last_elem_span.end as usize..location.end as usize)
                    .expect("valid span");

                if let Some((trailing_comma_offset, _)) = code
                    .rmatch_indices(',')
                    .find(|(i, _)| !self.extra.contains(*i as u32))
                {
                    self.edits.push(TextEdit {
                        range: src_span_to_lsp_range(
                            SrcSpan::new(
                                last_elem_span.end + trailing_comma_offset as u32,
                                last_elem_span.end + trailing_comma_offset as u32 + 1,
                            ),
                            &self.line_numbers,
                        ),
                        new_text: "".to_string(),
                    });
                }
            }

            // Delete )
            self.edits.push(TextEdit {
                range: src_span_to_lsp_range(
                    SrcSpan::new(location.end - 1, location.end),
                    &self.line_numbers,
                ),
                new_text: "".to_string(),
            });
        }

        ast::visit::visit_typed_expr_case(self, location, typ, subjects, clauses)
    }
}

impl<'a> RedundantTupleInCaseSubject<'a> {
    pub fn new(module: &'a build::Module, params: &'a CodeActionParams) -> Self {
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
        CodeActionBuilder::new("Remove redundant tuple")
            .kind(CodeActionKind::REFACTOR_REWRITE)
            .changes(self.params.text_document.uri.clone(), self.edits)
            .preferred(true)
            .push_to(&mut actions);

        actions
    }
}
