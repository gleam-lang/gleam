use crate::{
    ast::{
        visit::{self, Visit},
        SrcSpan, TypedAssignment, TypedExpr, TypedModule,
    },
    line_numbers::LineNumbers,
    type_::pretty::Printer,
};

#[derive(Debug, Eq, PartialEq)]
pub struct InlayHint {
    pub label: String,
    pub offset: u32,
}

/// Determines if the expression is a simple literal whose inlayHints must not be showed
/// in a pipeline chain
fn is_simple_lit(expr: &TypedExpr) -> bool {
    matches!(
        expr,
        TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::BitArray { .. }
    )
}

struct InlayHintsVisitor<'a> {
    hints: Vec<InlayHint>,
    line_numbers: &'a LineNumbers,
}

impl<'a> InlayHintsVisitor<'a> {
    fn new(line_numbers: &'a LineNumbers) -> InlayHintsVisitor<'a> {
        InlayHintsVisitor {
            hints: vec![],
            line_numbers,
        }
    }
}

impl<'a, 'ast> Visit<'ast> for InlayHintsVisitor<'a> {
    fn visit_typed_expr_pipeline(
        &mut self,
        _location: &'ast SrcSpan,
        assignments: &'ast [TypedAssignment],
        finally: &'ast TypedExpr,
    ) {
        let mut prev_hint: Option<(u32, Option<InlayHint>)> = None;
        for assign in assignments {
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

            let this_hint = InlayHint {
                label: Printer::new().pretty_print(assign.type_().as_ref(), 0),
                offset: assign.location.end,
            };
            prev_hint = Some((
                this_line,
                if is_simple_lit(&assign.value) {
                    None
                } else {
                    Some(this_hint)
                },
            ));

            visit::visit_typed_expr(self, &assign.value);
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
                self.hints.push(InlayHint {
                    label: Printer::new().pretty_print(finally.type_().as_ref(), 0),
                    offset: finally.location().end,
                });
            }
        }

        visit::visit_typed_expr(self, finally);
    }
}

pub fn get_inlay_hints(typed_module: TypedModule, line_numbers: &LineNumbers) -> Vec<InlayHint> {
    let mut visitor = InlayHintsVisitor::new(line_numbers);
    visitor.visit_typed_module(&typed_module);
    visitor.hints
}
