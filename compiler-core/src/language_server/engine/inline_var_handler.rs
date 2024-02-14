use super::*;

pub fn inline_local_variable(
    module: &Module,
    params: &lsp::CodeActionParams,
    actions: &mut Vec<CodeAction>,
) {
    let uri = &params.text_document.uri;
    let line_numbers = LineNumbers::new(&module.code);
    let byte_index = line_numbers.byte_index(params.range.start.line, params.range.start.character);

    //expression under cursor
    let exp = match module.find_node(byte_index) {
        Some(Located::Expression(e)) => e,
        _ => return,
    };

    if let TypedExpr::Var { constructor, .. } = exp {
        let source = match constructor.variant {
            ValueConstructorVariant::LocalVariable { location } => {
                match module.find_node(location.start) {
                    Some(s) => s,
                    None => return,
                }
            }
            _ => return,
        };

        let assignment = match source {
            Located::Statement(Statement::Assignment(a)) => a,
            _ => return,
        };

        let value_to_inline = match assignment.value.to_string() {
            Some(s) => s,
            None => return,
        };

        let replace_target_edit = lsp_types::TextEdit {
            range: src_span_to_lsp_range(exp.location(), &line_numbers),
            new_text: value_to_inline.to_string(),
        };

        let delete_source_edit = lsp_types::TextEdit {
            range: src_span_to_lsp_range(assignment.location, &line_numbers),
            new_text: "".into(),
        };

        let mut edits: Vec<lsp_types::TextEdit> = Vec::new();
        edits.push(delete_source_edit);
        edits.push(replace_target_edit);

        if !edits.is_empty() {
            CodeActionBuilder::new("Inline Variable Refactor")
                .kind(lsp_types::CodeActionKind::REFACTOR_INLINE)
                .changes(uri.clone(), edits)
                .preferred(true)
                .push_to(actions);
        }
    }
}
