use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel};

use crate::{
    ast::{
        PipelineAssignmentKind, SrcSpan, TypedExpr, TypedModule, TypedPipelineAssignment,
        visit::Visit,
    },
    line_numbers::LineNumbers,
    type_::{self, Type},
};

use super::src_offset_to_lsp_position;

struct InlayHintsVisitor<'a> {
    module_names: &'a type_::printer::Names,
    current_declaration_printer: type_::printer::Printer<'a>,

    hints: Vec<InlayHint>,
    line_numbers: &'a LineNumbers,
}

impl<'a> InlayHintsVisitor<'a> {
    fn new(
        line_numbers: &'a LineNumbers,
        module_names: &'a type_::printer::Names,
    ) -> InlayHintsVisitor<'a> {
        InlayHintsVisitor {
            module_names,
            current_declaration_printer: type_::printer::Printer::new(module_names),
            hints: vec![],
            line_numbers,
        }
    }
}

fn default_inlay_hint(line_numbers: &LineNumbers, offset: u32, label: String) -> InlayHint {
    let position = src_offset_to_lsp_position(offset, line_numbers);

    InlayHint {
        position,
        label: InlayHintLabel::String(label),
        kind: Some(InlayHintKind::TYPE),
        text_edits: None,
        tooltip: None,
        padding_left: Some(true),
        padding_right: None,
        data: None,
    }
}

impl InlayHintsVisitor<'_> {
    pub fn push_binding_annotation(&mut self, type_: &Type, span: &SrcSpan) {
        let label = format!(": {}", self.current_declaration_printer.print_type(type_));

        let mut hint = default_inlay_hint(self.line_numbers, span.end, label);
        hint.padding_left = Some(false);

        self.hints.push(hint);
    }

    pub fn push_return_annotation(&mut self, type_: &Type, span: &SrcSpan) {
        let label = format!("-> {}", self.current_declaration_printer.print_type(type_));

        let hint = default_inlay_hint(self.line_numbers, span.end, label);

        self.hints.push(hint);
    }
}

impl<'ast> Visit<'ast> for InlayHintsVisitor<'_> {
    fn visit_typed_function(&mut self, fun: &'ast crate::ast::TypedFunction) {
        // This must be reset on every statement
        self.current_declaration_printer = type_::printer::Printer::new(self.module_names);

        for arg in &fun.arguments {
            if arg.annotation.is_none() {
                self.push_binding_annotation(&arg.type_, &arg.location);
            }
        }

        if fun.return_annotation.is_none() {
            self.push_return_annotation(&fun.return_type, &fun.location);
        }

        for st in &fun.body {
            self.visit_typed_statement(st);
        }
    }

    fn visit_typed_expr_fn(
        &mut self,
        _location: &'ast SrcSpan,
        type_: &'ast std::sync::Arc<Type>,
        kind: &'ast crate::ast::FunctionLiteralKind,
        args: &'ast [crate::ast::TypedArg],
        body: &'ast vec1::Vec1<crate::ast::TypedStatement>,
        return_annotation: &'ast Option<crate::ast::TypeAst>,
    ) {
        if let crate::ast::FunctionLiteralKind::Anonymous { head } = kind {
            for arg in args {
                if arg.annotation.is_none() {
                    self.push_binding_annotation(&arg.type_, &arg.location);
                }
            }

            if return_annotation.is_none() {
                if let Some((_args, ret_type)) = type_.fn_types() {
                    self.push_return_annotation(&ret_type, head);
                }
            }
        }

        for st in body {
            self.visit_typed_statement(st);
        }
    }

    fn visit_typed_expr_pipeline(
        &mut self,
        _location: &'ast SrcSpan,
        first_value: &'ast TypedPipelineAssignment,
        assignments: &'ast [(TypedPipelineAssignment, PipelineAssignmentKind)],
        finally: &'ast TypedExpr,
        _finally_kind: &'ast PipelineAssignmentKind,
    ) {
        let mut prev_hint: Option<(u32, Option<InlayHint>)> = None;

        let assigments_values =
            std::iter::once(first_value).chain(assignments.iter().map(|p| &p.0));

        for assign in assigments_values {
            let this_line: u32 = self
                .line_numbers
                .line_and_column_number(assign.location.end)
                .line;

            if let Some((prev_line, prev_hint)) = prev_hint {
                if prev_line != this_line {
                    if let Some(prev_hint) = prev_hint {
                        self.hints.push(prev_hint);
                    }
                }
            };

            let this_hint = default_inlay_hint(
                self.line_numbers,
                assign.location.end,
                self.current_declaration_printer
                    .print_type(assign.type_().as_ref())
                    .to_string(),
            );

            prev_hint = Some((
                this_line,
                if assign.value.is_simple_lit() {
                    None
                } else {
                    Some(this_hint)
                },
            ));

            self.visit_typed_expr(&assign.value);
        }

        if let Some((prev_line, prev_hint)) = prev_hint {
            let this_line = self
                .line_numbers
                .line_and_column_number(finally.location().end)
                .line;
            if this_line != prev_line {
                if let Some(prev_hint) = prev_hint {
                    self.hints.push(prev_hint);
                }
                let hint = default_inlay_hint(
                    self.line_numbers,
                    finally.location().end,
                    self.current_declaration_printer
                        .print_type(finally.type_().as_ref())
                        .to_string(),
                );
                self.hints.push(hint);
            }
        }

        self.visit_typed_expr(finally);
    }
}

pub fn get_inlay_hints(typed_module: TypedModule, line_numbers: &LineNumbers) -> Vec<InlayHint> {
    let mut visitor = InlayHintsVisitor::new(line_numbers, &typed_module.names);
    visitor.visit_typed_module(&typed_module);
    visitor.hints
}
