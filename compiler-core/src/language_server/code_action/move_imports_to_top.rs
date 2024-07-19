use crate::{
    ast::{self, visit::Visit as _},
    build,
    line_numbers::LineNumbers,
};
use ecow::EcoString;
use lsp_types::{CodeAction, CodeActionKind, CodeActionParams, Position, Range, TextEdit};

use super::{super::src_span_to_lsp_range, CodeActionBuilder};

#[derive(Debug)]
/// Code action to move all the imports to the top of the module.
pub struct MoveImportsToTop<'a> {
    line_numbers: LineNumbers,
    code: &'a EcoString,
    params: &'a CodeActionParams,
    module: &'a ast::TypedModule,

    past_top_imports: bool,
    edits: Vec<TextEdit>,
}

impl<'ast> ast::visit::Visit<'ast> for MoveImportsToTop<'_> {
    fn visit_typed_definition(&mut self, def: &'ast ast::TypedDefinition) {
        match def {
            ast::Definition::Import(import) if self.past_top_imports => {
                tracing::info!("found not top level import");
                self.edits.push(TextEdit {
                    range: src_span_to_lsp_range(import.location, &self.line_numbers),
                    new_text: "".into(),
                });

                let import_start = import.location.start as usize;
                let import_end = import.location.end as usize;
                let import_text = self
                    .code
                    .get(import_start..import_end)
                    .expect("import")
                    .into();

                self.edits.push(TextEdit {
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
                    new_text: import_text,
                });
            }
            ast::Definition::Import(import_) => {
                tracing::info!("found top level import", import_);
            }
            ast::Definition::Function(_)
            | ast::Definition::TypeAlias(_)
            | ast::Definition::CustomType(_)
            | ast::Definition::ModuleConstant(_) => {
                tracing::info!("found definition after top level imports");
                self.past_top_imports = true
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
            past_top_imports: false,
            edits: vec![],
        }
    }

    pub fn code_actions(mut self) -> Vec<CodeAction> {
        self.visit_typed_module(self.module);
        if self.edits.is_empty() {
            return vec![];
        }

        self.edits.sort_by_key(|edit| edit.range.start);

        vec![
            CodeActionBuilder::new("Move all imports to the top of the module")
                .kind(CodeActionKind::SOURCE_ORGANIZE_IMPORTS)
                .changes(self.params.text_document.uri.clone(), self.edits)
                .preferred(true)
                .build(),
        ]
    }
}
