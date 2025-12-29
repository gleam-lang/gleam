use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel};

use gleam_core::{
    ast::{
        PipelineAssignmentKind, SrcSpan, Statement, TypeAst, TypedExpr, TypedModule,
        TypedPipelineAssignment, visit::Visit,
    },
    line_numbers::LineNumbers,
    type_::{self, Type},
};
use super::{configuration::InlayHintsConfig, src_offset_to_lsp_position};

struct InlayHintsVisitor<'a> {
    config: InlayHintsConfig,
    module_names: &'a type_::printer::Names,
    current_declaration_printer: type_::printer::Printer<'a>,

    hints: Vec<InlayHint>,
    line_numbers: &'a LineNumbers,
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
    /// Add an inlay hint for function parameter bindings
    pub fn push_binding_annotation(
        &mut self,
        type_: &Type,
        type_annotation_ast: Option<&TypeAst>,
        span: &SrcSpan,
    ) {
        if type_annotation_ast.is_some() {
            return;
        }

        let label = format!(": {}", self.current_declaration_printer.print_type(type_));

        let mut hint = default_inlay_hint(self.line_numbers, span.end, label);
        hint.padding_left = Some(false);

        self.hints.push(hint);
    }

    /// Add an inlay hint for the return type of a function
    pub fn push_return_annotation(
        &mut self,
        type_: &Type,
        type_annotation_ast: Option<&TypeAst>,
        span: &SrcSpan,
    ) {
        if type_annotation_ast.is_some() {
            return;
        }

        let label = format!("-> {}", self.current_declaration_printer.print_type(type_));

        let hint = default_inlay_hint(self.line_numbers, span.end, label);

        self.hints.push(hint);
    }
}

impl<'ast> Visit<'ast> for InlayHintsVisitor<'_> {
    fn visit_typed_statement(&mut self, stmt: &'ast gleam_core::ast::TypedStatement) {
        // This must be reset on every statement
        self.current_declaration_printer = type_::printer::Printer::new(self.module_names);

        match stmt {
            Statement::Expression(expr) => self.visit_typed_expr(expr),
            Statement::Assignment(assignment) => self.visit_typed_assignment(assignment),
            Statement::Use(use_) => self.visit_typed_use(use_),
            Statement::Assert(assert) => self.visit_typed_assert(assert),
        }
    }

    fn visit_typed_function(&mut self, fun: &'ast gleam_core::ast::TypedFunction) {
        for statement in &fun.body {
            self.visit_typed_statement(statement);
        }

        if self.config.function_parameter_types {
            for argument in &fun.arguments {
                self.push_binding_annotation(
                    &argument.type_,
                    argument.annotation.as_ref(),
                    &argument.location,
                );
            }
        }

        if self.config.function_return_types {
            self.push_return_annotation(
                &fun.return_type,
                fun.return_annotation.as_ref(),
                &fun.location,
            );
        }
    }

    fn visit_typed_expr_fn(
        &mut self,
        _location: &'ast SrcSpan,
        type_: &'ast std::sync::Arc<Type>,
        kind: &'ast gleam_core::ast::FunctionLiteralKind,
        args: &'ast [gleam_core::ast::TypedArg],
        body: &'ast vec1::Vec1<gleam_core::ast::TypedStatement>,
        return_annotation: &'ast Option<TypeAst>,
    ) {
        for statement in body {
            self.visit_typed_statement(statement);
        }

        let gleam_core::ast::FunctionLiteralKind::Anonymous { head } = kind else {
            return;
        };

        if self.config.function_parameter_types {
            for argument in args {
                self.push_binding_annotation(&argument.type_, argument.annotation.as_ref(), &argument.location);
            }
        }

        if self.config.function_return_types && let Some((_arguments, ret_type)) = type_.fn_types() {
            self.push_return_annotation(&ret_type, return_annotation.as_ref(), head);
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
        self.visit_typed_pipeline_assignment(first_value);
        for (assignment, _kind) in assignments {
            self.visit_typed_pipeline_assignment(assignment);
        }
        self.visit_typed_expr(finally);

        if !self.config.pipelines {
            return;
        }

        let mut prev_hint: Option<(u32, Option<InlayHint>)> = None;

        let assignments_values =
            std::iter::once(first_value).chain(assignments.iter().map(|p| &p.0));

        for assign in assignments_values {
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
                if should_show_inlay_hint(&assign.value) {
                    Some(this_hint)
                } else {
                    None
                },
            ));
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
    }
}

pub fn get_inlay_hints(
    config: InlayHintsConfig,
    typed_module: &TypedModule,
    line_numbers: &LineNumbers,
) -> Vec<InlayHint> {
    let mut visitor = InlayHintsVisitor {
        config,
        module_names: &typed_module.names,
        current_declaration_printer: type_::printer::Printer::new(&typed_module.names),
        hints: vec![],
        line_numbers,
    };

    visitor.visit_typed_module(&typed_module);
    visitor.hints
}

/// Determines if an inlay hint should be shown for an expression in a pipeline chain.
fn should_show_inlay_hint(typed_expr: &TypedExpr) -> bool {
    match typed_expr {
        TypedExpr::Int { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::String { .. }
        | TypedExpr::BitArray { .. }
        | TypedExpr::PositionalAccess { .. } => false,
        TypedExpr::Block { .. }
        | TypedExpr::Pipeline { .. }
        | TypedExpr::Var { .. }
        | TypedExpr::Fn { .. }
        | TypedExpr::List { .. }
        | TypedExpr::Call { .. }
        | TypedExpr::BinOp { .. }
        | TypedExpr::Case { .. }
        | TypedExpr::RecordAccess { .. }
        | TypedExpr::ModuleSelect { .. }
        | TypedExpr::Tuple { .. }
        | TypedExpr::TupleIndex { .. }
        | TypedExpr::Todo { .. }
        | TypedExpr::Panic { .. }
        | TypedExpr::RecordUpdate { .. }
        | TypedExpr::NegateBool { .. }
        | TypedExpr::NegateInt { .. }
        | TypedExpr::Invalid { .. }
        | TypedExpr::Echo { .. } => true,
    }
}
