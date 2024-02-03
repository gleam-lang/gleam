use crate::ast::Assignment;

use super::*;

pub fn inline_variable(
    module: &Module,
    params: &lsp::CodeActionParams,
    actions: &mut Vec<CodeAction>,
) {
    dbg!(&module);
    let uri = &params.text_document.uri;
    let line_numbers = LineNumbers::new(&module.code);
    let mut inline_refactors = Vec::new();

    module
        .ast
        .definitions
        .iter()
        .filter_map(get_function)
        .for_each(|func| detect_possible_inlinings_for_function(func, &mut inline_refactors));

    if inline_refactors.is_empty() {
        return;
    }

    let mut edits = Vec::new();

    create_edits_for_inline_refactor(inline_refactors, line_numbers, params, &mut edits);

    if !edits.is_empty() {
        CodeActionBuilder::new("Inline Variable Refactor")
            .kind(lsp_types::CodeActionKind::REFACTOR_INLINE)
            .changes(uri.clone(), edits)
            .preferred(true)
            .push_to(actions);
    }
}

fn detect_possible_inlinings_for_function(
    function: &Function<Arc<Type>, TypedExpr>,
    inline_refactors: &mut Vec<InlineRefactor>,
) {
    let assign_statements: Vec<&Assignment<Arc<Type>, TypedExpr>> = function
        .body
        .iter()
        .filter_map(|statement| {
            if let Statement::Assignment(assign) = statement {
                Some(assign)
            } else {
                None
            }
        })
        .collect();

    function.body.iter().for_each(|statement| {
        detect_possible_inlinings_in_body(statement, &assign_statements, inline_refactors)
    });
}

fn detect_possible_inlinings_in_body<'a>(
    statement: &Statement<Arc<Type>, TypedExpr>,
    assign_statements: &'a Vec<&Assignment<Arc<Type>, TypedExpr>>,
    expressions_to_inline: &mut Vec<InlineRefactor>,
) {
    if let Statement::Assignment(assign) = statement {
        detect_possible_inlining_for_expression(
            &assign.value,
            assign_statements,
            expressions_to_inline,
        );
    }

    if let Statement::Expression(expr) = statement {
        detect_possible_inlining_for_expression(expr, assign_statements, expressions_to_inline)
    }
}

fn detect_possible_inlining_for_expression<'a>(
    expr: &TypedExpr,
    assign_statements: &'a Vec<&Assignment<Arc<Type>, TypedExpr>>,
    expressions_to_inline: &mut Vec<InlineRefactor>,
) {
    if let TypedExpr::Var { constructor, .. } = expr {
        inline_refactor(constructor, assign_statements, expr, expressions_to_inline);
    }

    if let TypedExpr::Call { args, .. } = expr {
        args.iter().for_each(|arg| {
            if let TypedExpr::Call { .. } = &arg.value {
                detect_possible_inlining_for_expression(
                    &arg.value,
                    assign_statements,
                    expressions_to_inline,
                )
            }
            if let TypedExpr::Var { constructor, .. } = &arg.value {
                inline_refactor(constructor, assign_statements, expr, expressions_to_inline);
            }
        });
    }

    if let TypedExpr::Pipeline {
        location: _,
        assignments,
        finally,
    } = expr
    {
        assignments.iter().for_each(|assignment| {
            detect_possible_inlining_for_expression(
                &assignment.value,
                assign_statements,
                expressions_to_inline,
            )
        });

        detect_possible_inlining_for_expression(&finally, assign_statements, expressions_to_inline);
    }

    if let TypedExpr::BinOp { left, right, .. } = expr {
        detect_possible_inlining_for_expression(left, assign_statements, expressions_to_inline);
        detect_possible_inlining_for_expression(right, assign_statements, expressions_to_inline);
    }

    if let TypedExpr::Tuple { elems, .. } = expr {
        elems.iter().for_each(|elem| {
            detect_possible_inlining_for_expression(elem, assign_statements, expressions_to_inline)
        })
    }

    if let TypedExpr::List { elements, .. } = expr {
        elements.iter().for_each(|elem| {
            detect_possible_inlining_for_expression(elem, assign_statements, expressions_to_inline)
        })
    }

    if let TypedExpr::Fn { body, .. } = expr {
        body.iter().for_each(|statement| {
            detect_possible_inlinings_in_body(statement, assign_statements, expressions_to_inline)
        })
    }

    //moet ie dan niet alleen de variable bekijken die in block gedefined zijn??
    //NOG EEN CHECK OP VARIABLE BUITEN BLOCK?? --> MOGELIJK MAKEN OM VARIABELEN TE INLINEN
    //DIE BUITEN BLOCK ZIJN GEDEFINIEERD..
    if let TypedExpr::Block { statements, .. } = expr {
        let assign_statements: Vec<&Assignment<Arc<Type>, TypedExpr>> = statements
            .iter()
            .filter_map(|statement| {
                if let Statement::Assignment(assign) = statement {
                    Some(assign)
                } else {
                    None
                }
            })
            .collect();

        statements.iter().for_each(|statement| {
            detect_possible_inlinings_in_body(statement, &assign_statements, expressions_to_inline)
        })
    }
}

fn inline_refactor<'a>(
    constructor: &crate::type_::ValueConstructor,
    assignments: &'a Vec<&Assignment<Arc<Type>, TypedExpr>>,
    expr: &TypedExpr,
    expressions_to_inline: &mut Vec<InlineRefactor>,
) {
    if let ValueConstructorVariant::LocalVariable { location } = constructor.variant {
        let result = assignments
            .iter()
            .find(|assign| assign.pattern.location() == location);

        if let Some(found) = result {
            if let TypedExpr::Call { ref args, .. } = expr {
                let loc_inlinable_assign = found.pattern.location();
                if let Some(callarg) = args.iter().find(|callarg| match &callarg.value {
                    TypedExpr::Var { .. } => {
                        location.start == loc_inlinable_assign.start
                            && location.end == loc_inlinable_assign.end
                    }
                    _ => false,
                }) {
                    expressions_to_inline.push(InlineRefactor {
                        source: found.location,
                        destination: callarg.location,
                        value_to_inline: *found.value.clone(),
                    });
                }
            }

            if let TypedExpr::Var { ref location, .. } = expr {
                expressions_to_inline.push(InlineRefactor {
                    source: found.location,
                    destination: location.clone(),
                    value_to_inline: *found.value.clone(),
                });
            }
        }
    }
}

fn create_edits_for_inline_refactor(
    inline_refactors: Vec<InlineRefactor>,
    line_numbers: LineNumbers,
    params: &lsp::CodeActionParams,
    edits: &mut Vec<lsp::TextEdit>,
) {
    inline_refactors.iter().for_each(|refactor| {
        let range_inline_destination = src_span_to_lsp_range(refactor.destination, &line_numbers);
        if range_includes(&params.range, &range_inline_destination) {
            edits.push(lsp_types::TextEdit {
                range: src_span_to_lsp_range(refactor.source, &line_numbers),
                new_text: "".into(),
            });

            edits.push(lsp_types::TextEdit {
                range: range_inline_destination,
                new_text: format!("{}", refactor.value_to_inline.to_string()),
            });
        }
    });
}

struct InlineRefactor {
    source: SrcSpan,
    destination: SrcSpan,
    value_to_inline: TypedExpr,
}
