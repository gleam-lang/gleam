use crate::ast::Assignment;

use super::*;

pub fn inline_variable(
    module: &Module,
    params: &lsp::CodeActionParams,
    actions: &mut Vec<CodeAction>,
) {
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
        detect_possible_inlinings_for_statement(statement, &assign_statements, inline_refactors)
    });
}

fn detect_possible_inlinings_for_statement(
    statement: &Statement<Arc<Type>, TypedExpr>,
    assign_statements: &Vec<&Assignment<Arc<Type>, TypedExpr>>,
    expressions_to_inline: &mut Vec<InlineRefactor>,
) {
    if let Statement::Assignment(assign) = statement {
        traverse_expression(&assign.value, assign_statements, expressions_to_inline);
    }

    if let Statement::Expression(expr) = statement {
        traverse_expression(expr, assign_statements, expressions_to_inline)
    }
}

fn traverse_expression(
    expr: &TypedExpr,
    assign_statements: &Vec<&Assignment<Arc<Type>, TypedExpr>>,
    expressions_to_inline: &mut Vec<InlineRefactor>,
) {
    if let TypedExpr::Var { constructor, .. } = expr {
        inline_refactor(constructor, assign_statements, expr, expressions_to_inline);
    }

    if let TypedExpr::Call { args, .. } = expr {
        args.iter().for_each(|arg| {
            if let TypedExpr::Call { .. } = &arg.value {
                traverse_expression(&arg.value, assign_statements, expressions_to_inline)
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
            traverse_expression(&assignment.value, assign_statements, expressions_to_inline)
        });

        traverse_expression(&finally, assign_statements, expressions_to_inline);
    }

    if let TypedExpr::BinOp { left, right, .. } = expr {
        traverse_expression(left, assign_statements, expressions_to_inline);
        traverse_expression(right, assign_statements, expressions_to_inline);
    }

    if let TypedExpr::Tuple { elems, .. } = expr {
        elems
            .iter()
            .for_each(|elem| traverse_expression(elem, assign_statements, expressions_to_inline))
    }

    if let TypedExpr::List { elements, .. } = expr {
        elements
            .iter()
            .for_each(|elem| traverse_expression(elem, assign_statements, expressions_to_inline))
    }

    if let TypedExpr::Fn { body, .. } = expr {
        body.iter().for_each(|statement| {
            detect_possible_inlinings_for_statement(
                statement,
                assign_statements,
                expressions_to_inline,
            )
        })
    }

    if let TypedExpr::Block { statements, .. } = expr {
        let mut block_and_outer_assignments: Vec<&Assignment<Arc<Type>, TypedExpr>> = statements
            .iter()
            .filter_map(|statement| {
                if let Statement::Assignment(assign) = statement {
                    Some(assign)
                } else {
                    None
                }
            })
            .collect();

        block_and_outer_assignments.extend(assign_statements.iter());

        statements.iter().for_each(|statement| {
            detect_possible_inlinings_for_statement(
                statement,
                &block_and_outer_assignments,
                expressions_to_inline,
            )
        })
    }
}

fn inline_refactor(
    constructor: &crate::type_::ValueConstructor,
    assignments: &Vec<&Assignment<Arc<Type>, TypedExpr>>,
    expr: &TypedExpr,
    expressions_to_inline: &mut Vec<InlineRefactor>,
) {
    //Only provide inlining for locally defined variables
    if let ValueConstructorVariant::LocalVariable { location } = constructor.variant {
        let result = assignments
            .iter()
            .find(|assign| assign.pattern.location() == location);

        if let Some(found) = result {
            if !expr_can_be_inlined(&found.value) {
                return;
            }

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
                        value_to_inline: found.value.to_string().unwrap(),
                    });
                }
            }

            if let TypedExpr::Var { ref location, .. } = expr {
                expressions_to_inline.push(InlineRefactor {
                    source: found.location,
                    destination: location.clone(),
                    value_to_inline: found.value.to_string().unwrap(),
                });
            }
        }
    }
}

fn expr_can_be_inlined(value: &Box<TypedExpr>) -> bool {
    matches!(value.to_string(), Some(_))
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
                new_text: format!("{}", refactor.value_to_inline),
            });
        }
    });
}

struct InlineRefactor {
    source: SrcSpan,
    destination: SrcSpan,
    value_to_inline: EcoString,
}