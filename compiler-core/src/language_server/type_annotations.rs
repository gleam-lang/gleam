use super::code_action::CodeActionBuilder;
use crate::{
    ast::{TypedFunction, TypedModuleConstant},
    line_numbers::LineNumbers,
    type_::pretty::Printer,
};
use lsp_types::{
    CodeActionKind, InlayHint, InlayHintKind, InlayHintLabel, Position, Range, TextEdit, Url,
};

#[derive(Debug, Clone, Default)]
pub struct TypeAnnotations {
    annotations: Vec<(Position, String)>,
}

impl TypeAnnotations {
    pub fn from_module_constant(
        constant: &TypedModuleConstant,
        line_numbers: &LineNumbers,
    ) -> Self {
        if constant.annotation.is_none() {
            let linecol = line_numbers.line_and_column_number(constant.location.end);
            let position = Position::new(linecol.line - 1, linecol.column - 1);
            let type_text = Printer::new().pretty_print(&constant.type_, 0);
            let text = format!(": {type_text}");

            Self {
                annotations: vec![(position, text)],
            }
        } else {
            Self::default()
        }
    }

    pub fn from_function_definition(function: &TypedFunction, line_numbers: &LineNumbers) -> Self {
        let mut type_parameters = im::HashMap::new();
        if let Some(annotation) = &function.return_annotation {
            if let crate::type_::Type::Var { type_ } = &*function.return_type {
                if let crate::type_::TypeVar::Generic { id } = &*type_.borrow() {
                    let _ = type_parameters.insert(*id, annotation);
                }
            }
        }
        for argument in &function.arguments {
            if let Some(annotation) = &argument.annotation {
                if let crate::type_::Type::Var { type_ } = &*argument.type_ {
                    if let crate::type_::TypeVar::Generic { id } = &*type_.borrow() {
                        let _ = type_parameters.insert(*id, annotation);
                    }
                }
            }
        }

        let mut annotations = vec![];
        if function.return_annotation.is_none() {
            let linecol = line_numbers.line_and_column_number(function.location.end);
            let position = Position::new(linecol.line - 1, linecol.column - 1);
            let mut type_text = Printer::new().pretty_print(&function.return_type, 0);

            if let crate::type_::Type::Var { type_ } = &*function.return_type {
                if let crate::type_::TypeVar::Generic { id } = &*type_.borrow() {
                    if let Some(crate::ast::TypeAst::Var(type_var)) = type_parameters.get(id) {
                        type_text = type_var.name.to_string();
                    }
                }
            }

            let text = format!(" -> {type_text}");
            annotations.push((position, text))
        }
        for argument in &function.arguments {
            if argument.annotation.is_none() {
                let linecol = line_numbers.line_and_column_number(argument.location.end);
                let position = Position::new(linecol.line - 1, linecol.column - 1);
                let mut type_text = Printer::new().pretty_print(&argument.type_, 0);

                if let crate::type_::Type::Var { type_ } = &*argument.type_ {
                    if let crate::type_::TypeVar::Generic { id } = &*type_.borrow() {
                        if let Some(crate::ast::TypeAst::Var(type_var)) = type_parameters.get(id) {
                            type_text = type_var.name.to_string();
                        }
                    }
                }

                let text = format!(": {type_text}");
                annotations.push((position, text));
            }
        }

        Self { annotations }
    }

    pub fn into_inlay_hints(self) -> impl Iterator<Item = InlayHint> {
        self.annotations
            .into_iter()
            .map(|(position, text)| InlayHint {
                position,
                kind: Some(InlayHintKind::TYPE),
                tooltip: None,
                padding_left: text.starts_with(' ').then_some(true),
                padding_right: None,
                data: None,
                label: InlayHintLabel::String(text.trim_start().to_owned()),
                text_edits: Some(vec![TextEdit::new(Range::new(position, position), text)]),
            })
    }

    pub fn into_code_action(self, uri: &Url) -> Option<CodeActionBuilder> {
        if self.annotations.is_empty() {
            None
        } else {
            Some(
                CodeActionBuilder::new("Annotate type(s)")
                    .kind(CodeActionKind::REFACTOR_REWRITE)
                    .changes(
                        uri.clone(),
                        self.annotations
                            .into_iter()
                            .map(|(position, text)| {
                                TextEdit::new(Range::new(position, position), text)
                            })
                            .collect(),
                    )
                    .preferred(true),
            )
        }
    }
}
