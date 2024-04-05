use crate::{ast::SrcSpan, language_server::code_action::ActionId};

use super::*;

pub fn convert_to_pipeline(
    module: &Module,
    params: &lsp::CodeActionParams,
    actions: &mut Vec<CodeAction>,
    strategy: ResolveStrategy,
    nodes: &[Located<'_>],
) {
    let uri = &params.text_document.uri;
    let line_numbers = LineNumbers::new(&module.code);

    nodes
        .iter()
        .filter_map(|node| extract_call_expression(node))
        .for_each(|call| {
            let mut call_chain: Vec<&TypedExpr> = Vec::new();

            detect_call_chain(call.expression, &mut call_chain);

            if call_chain.is_empty() {
                cov_mark::hit!(chain_is_empty);
                return;
            }

            if strategy.is_eager() {
                let pipeline_parts = match convert_call_chain_to_pipeline(call_chain) {
                    Some(parts) => parts,
                    //input for pipeline cannot be stringified
                    //so no code action to be suggested
                    None => return,
                };

                let indent = line_numbers.line_and_column_number(call.location).column - 1;

                if let Some(edit) = create_edit(pipeline_parts, &line_numbers, indent) {
                    CodeActionBuilder::new("Apply Pipeline Rewrite")
                        .kind(lsp_types::CodeActionKind::REFACTOR_REWRITE)
                        .changes(uri.clone(), vec![edit])
                        .data(ActionId::Pipeline, params.clone(), call.location)
                        .preferred(true)
                        .push_to(actions);
                }
            } else {
                CodeActionBuilder::new("Apply Pipeline Rewrite")
                    .kind(lsp_types::CodeActionKind::REFACTOR_REWRITE)
                    .data(ActionId::Pipeline, params.clone(), call.location)
                    .preferred(true)
                    .push_to(actions);
            }
        });
}

fn extract_call_expression<'a>(node: &Located<'a>) -> Option<CallExpression<'a>> {
    match node {
        Located::Expression(expr) => {
            if let TypedExpr::Call { .. } = expr {
                Some(CallExpression {
                    expression: expr,
                    location: expr.location().start,
                })
            } else {
                None
            }
        }
        Located::Statement(Statement::Assignment(assign)) => {
            if let TypedExpr::Call { .. } = *assign.value {
                Some(CallExpression {
                    expression: &assign.value,
                    location: assign.value.location().start,
                })
            } else {
                None
            }
        }
        _ => None,
    }
}

fn detect_call_chain<'a>(call_expression: &'a TypedExpr, call_chain: &mut Vec<&'a TypedExpr>) {
    if let TypedExpr::Call { args, .. } = call_expression {
        if let Some(arg) = args.first() {
            if let TypedExpr::Var { name, .. } = &arg.value {
                if name == "_pipe" {
                    cov_mark::hit!(empty_call_chain_as_part_of_pipeline);
                    return;
                }
            }

            call_chain.push(call_expression);

            // Recurse on its first argument to detect the full call chain
            if let TypedExpr::Call { .. } = &arg.value {
                detect_call_chain(&arg.value, call_chain);
            }
        }
    }
}

fn convert_call_chain_to_pipeline(mut call_chain: Vec<&TypedExpr>) -> Option<PipelineParts> {
    call_chain.reverse();

    //remove the first argument in order to convert the chain to its piped equivalent.
    let modified_chain: Vec<_> = call_chain
        .iter()
        .filter_map(|expr| match expr {
            TypedExpr::Call {
                location,
                typ,
                fun,
                args,
            } if !args.is_empty() => {
                let args = args.get(1..).unwrap_or(&[]).to_vec();
                Some(TypedExpr::Call {
                    location: *location,
                    typ: typ.clone(),
                    fun: fun.clone(),
                    args,
                })
            }
            _ => None,
        })
        .collect();

    //We need the last call in order retrieve the input for the pipeline.
    let last_call = call_chain.first()?;

    //Returns None in case the input for the pipeline cannot be stringified.
    //There is no code action to be suggested.
    let input = match last_call {
        TypedExpr::Call { args, .. } => args.first()?.value.to_string()?,
        _ => return None,
    };

    Some(PipelineParts {
        input,
        location: call_chain.last()?.location(),
        calls: modified_chain,
    })
}

fn create_edit(
    pipeline_parts: PipelineParts,
    line_numbers: &LineNumbers,
    indent: u32,
) -> Option<lsp::TextEdit> {
    let mut edit_str = EcoString::new();

    edit_str.push_str(&format!("{} \n", pipeline_parts.input));

    //In case there is a typed expression for which we do not have a string representation,
    //the function should return None. Indicating there is no code action possible here.
    if let Err(()) = pipeline_parts
        .calls
        .iter()
        .try_for_each(|part| match part.to_string() {
            Some(s) => {
                for _ in 0..indent {
                    edit_str.push(' ');
                }
                edit_str.push_str(&format!("|> {}\n", s));
                Ok(())
            }
            None => Err(()),
        })
    {
        cov_mark::hit!(no_stringification_for_expression);
        return None;
    }

    Some(lsp::TextEdit {
        range: src_span_to_lsp_range(pipeline_parts.location, line_numbers),
        new_text: edit_str.to_string(),
    })
}

struct PipelineParts {
    input: EcoString,
    location: SrcSpan,
    calls: Vec<TypedExpr>,
}

struct CallExpression<'a> {
    expression: &'a TypedExpr,
    location: u32,
}
