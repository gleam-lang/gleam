use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel};

use crate::{
    ast::{
        visit::{self, Visit},
        SrcSpan, TypedAssignment, TypedExpr, TypedModule,
    },
    line_numbers::LineNumbers,
    type_::pretty::Printer,
};

use super::src_offset_to_lsp_position;

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

impl<'ast> Visit<'ast> for InlayHintsVisitor<'_> {
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

            let this_hint = default_inlay_hint(
                self.line_numbers,
                assign.location.end,
                Printer::new().pretty_print(assign.type_().as_ref(), 0),
            );

            prev_hint = Some((
                this_line,
                if assign.value.is_simple_lit() {
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
                let hint = default_inlay_hint(
                    self.line_numbers,
                    finally.location().end,
                    Printer::new().pretty_print(finally.type_().as_ref(), 0),
                );
                self.hints.push(hint);
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
