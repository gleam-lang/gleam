use crate::{
    ast::{self, visit::Visit as _, Import, SrcSpan},
    build,
    line_numbers::LineNumbers,
};
use ecow::EcoString;
use itertools::Itertools;
use lsp_types::{CodeAction, CodeActionKind, CodeActionParams, Position, Range, TextEdit};

use super::{super::engine::overlaps, super::src_span_to_lsp_range, CodeActionBuilder};

#[derive(Debug)]
/// Code action to move all the imports to the top of the module.
pub struct MoveImportsToTop<'a> {
    line_numbers: LineNumbers,
    code: &'a EcoString,
    params: &'a CodeActionParams,
    module: &'a ast::TypedModule,

    is_over_import: bool,
    imports: Vec<&'a Import<EcoString>>,
    earlier_definition_end: Option<u32>,
}

impl<'ast> ast::visit::Visit<'ast> for MoveImportsToTop<'ast> {
    fn visit_typed_definition(&mut self, def: &'ast ast::TypedDefinition) {
        match def {
            ast::Definition::Import(import) => {
                if overlaps(
                    self.params.range,
                    src_span_to_lsp_range(import.location, &self.line_numbers),
                ) {
                    self.is_over_import = true;
                }
                self.imports.push(import)
            }
            ast::Definition::Function(_)
            | ast::Definition::TypeAlias(_)
            | ast::Definition::CustomType(_)
            | ast::Definition::ModuleConstant(_) => {
                let def_location = def.location();
                match self.earlier_definition_end {
                    Some(end) if def_location.end < end => {
                        self.earlier_definition_end = Some(def_location.end)
                    }
                    Some(_) => (),
                    None => self.earlier_definition_end = Some(def_location.end),
                }
            }
        }
    }
}

impl<'a> MoveImportsToTop<'a> {
    pub fn new(module: &'a build::Module, params: &'a CodeActionParams) -> Self {
        Self {
            line_numbers: LineNumbers::new(&module.code),
            code: &module.code,
            params,
            module: &module.ast,
            imports: vec![],
            is_over_import: false,
            earlier_definition_end: None,
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(self.module);

        if self.imports.is_empty() || !self.is_over_import {
            return vec![];
        }

        let Some(end) = self.earlier_definition_end else {
            return vec![];
        };

        let edits = self
            .imports
            .iter()
            .filter(|import| import.location.start > end)
            .flat_map(|import| self.move_import_edits(import))
            .collect_vec();

        if edits.is_empty() {
            vec![]
        } else {
            vec![
                CodeActionBuilder::new("Move all imports to the top of the module")
                    .kind(CodeActionKind::REFACTOR_REWRITE)
                    .changes(self.params.text_document.uri.clone(), edits)
                    .preferred(false)
                    .build(),
            ]
        }
    }

    fn move_import_edits(&self, import: &Import<EcoString>) -> Vec<TextEdit> {
        let import_text = self
            .code
            .get(import.location.start as usize..import.location.end as usize)
            .expect("import location");

        vec![
            TextEdit {
                range: src_span_to_lsp_range(
                    SrcSpan {
                        start: import.location.start,
                        // This way we will take care of any eventual new line
                        // after the import.
                        end: import.location.end + 1,
                    },
                    &self.line_numbers,
                ),
                new_text: "".into(),
            },
            TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: 0,
                    },
                },
                new_text: format!("{import_text}\n"),
            },
        ]
    }
}
