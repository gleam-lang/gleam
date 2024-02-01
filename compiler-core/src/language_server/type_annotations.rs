use super::code_action::CodeActionBuilder;
use crate::{
    ast::{Statement, TypeAst, TypedExpr, TypedFunction, TypedModuleConstant},
    line_numbers::LineNumbers,
    type_::{lsp_pretty::LspPrinter, pretty::Printer, Type, TypeVar},
};
use ecow::EcoString;
use im::HashMap;
use lsp_types::{
    CodeActionKind, InlayHint, InlayHintKind, InlayHintLabel, Position, Range, TextEdit, Url,
};
use std::sync::Arc;
use vec1::Vec1;

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

    /**
     * Returns a list of annotations and makes note of any `type_parameter` annotations given by the user in the function body.
     */
    fn process_function_body<'a>(
        body: &'a Vec1<Statement<Arc<Type>, TypedExpr>>,
        type_parameters: &mut HashMap<u64, &'a TypeAst>,
        line_numbers: &LineNumbers,
        type_qualifiers: &HashMap<EcoString, EcoString>,
        module_qualifiers: &HashMap<EcoString, EcoString>,
    ) -> Vec<(Position, String)> {
        let mut annotations = vec![];
        for statement in body {
            if let Statement::Assignment(st) = statement {
                match &st.annotation {
                    None => {
                        let linecol =
                            line_numbers.line_and_column_number(st.pattern.location().end);
                        let position = Position::new(linecol.line - 1, linecol.column - 1);
                        let mut type_text = Self::get_type_text(
                            &st.value.type_(),
                            type_qualifiers,
                            module_qualifiers,
                        );
                        if let Type::Var { type_ } = &*st.value.type_() {
                            // If type is generic type variable check if its id has a overwriting type_text given by the user via a type annotation elsewhere in the function
                            if let TypeVar::Generic { id } = &*type_.borrow() {
                                if let Some(TypeAst::Var(type_var)) = type_parameters.get(id) {
                                    type_text = type_var.name.to_string();
                                }
                            }
                        }
                        annotations.push((position, format!(": {type_text}")));
                    }
                    Some(annotation) => {
                        // If user has provided an annotation that is a type parameter, make a note of it
                        if let Type::Var { type_ } = &*st.value.type_() {
                            if let TypeVar::Generic { id } = &*type_.borrow() {
                                let _ = type_parameters.insert(*id, annotation);
                            }
                        }
                    }
                }
            }
        }
        annotations
    }

    // Helper function that chooses between the LspPrinter's pretty_print of a type or a qualified name given to the type by the user.
    fn get_type_text(
        typ: &Type,
        type_qualifiers: &HashMap<EcoString, EcoString>,
        module_qualifiers: &HashMap<EcoString, EcoString>,
    ) -> String {
        let typ_text = EcoString::from(
            LspPrinter::new(type_qualifiers, module_qualifiers).pretty_print(typ, 0),
        );
        type_qualifiers
            .get(&typ_text)
            .unwrap_or(&typ_text)
            .to_string()
    }

    pub fn from_function_definition(
        function: &TypedFunction,
        line_numbers: &LineNumbers,
        type_qualifiers: &HashMap<EcoString, EcoString>,
        module_qualifiers: &HashMap<EcoString, EcoString>,
    ) -> Self {
        let mut type_parameters = HashMap::new();
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

        let mut annotations = Self::process_function_body(
            &function.body,
            &mut type_parameters,
            line_numbers,
            type_qualifiers,
            module_qualifiers,
        );

        if function.return_annotation.is_none() {
            let linecol = line_numbers.line_and_column_number(function.location.end);
            let position = Position::new(linecol.line - 1, linecol.column - 1);
            let mut type_text =
                Self::get_type_text(&function.return_type, type_qualifiers, module_qualifiers);

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
                let mut type_text =
                    Self::get_type_text(&argument.type_, type_qualifiers, module_qualifiers);

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