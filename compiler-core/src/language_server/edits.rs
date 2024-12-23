use ecow::EcoString;
use lsp_types::{Position, Range, TextEdit};

use crate::{
    ast::{Definition, Import, TypedDefinition},
    build::Module,
    line_numbers::LineNumbers,
};

use super::src_span_to_lsp_range;

// Gets the position of the import statement if it's the first definition in the module.
pub fn position_of_first_definition_if_import(
    module: &Module,
    line_numbers: &LineNumbers,
) -> Option<Position> {
    // As "self.module.ast.definitions"  could be sorted, let's find the actual first definition by position.
    let first_definition = module
        .ast
        .definitions
        .iter()
        .min_by(|a, b| a.location().start.cmp(&b.location().start));
    let import = first_definition.and_then(get_import);
    import.map(|import| src_span_to_lsp_range(import.location, line_numbers).start)
}

pub fn get_import(statement: &TypedDefinition) -> Option<&Import<EcoString>> {
    match statement {
        Definition::Import(import) => Some(import),
        _ => None,
    }
}

pub enum Newlines {
    Single,
    Double,
}

// Returns how many newlines should be added after an import statement. By default `Newlines::Single`,
// but if there's not any import statement, it returns `Newlines::Double`.
//
// * ``import_location`` - The position of the first import statement in the source code.
pub fn add_newlines_after_import(
    import_location: Position,
    has_imports: bool,
    line_numbers: &LineNumbers,
    src: &str,
) -> Newlines {
    let import_start_cursor =
        line_numbers.byte_index(import_location.line, import_location.character);
    let is_new_line = src
        .chars()
        .nth(import_start_cursor as usize)
        .unwrap_or_default()
        == '\n';
    match !has_imports && !is_new_line {
        true => Newlines::Double,
        false => Newlines::Single,
    }
}

pub fn get_import_edit(
    import_location: Position,
    module_full_name: &str,
    insert_newlines: &Newlines,
) -> TextEdit {
    let new_lines = match insert_newlines {
        Newlines::Single => "\n",
        Newlines::Double => "\n\n",
    };
    TextEdit {
        range: Range {
            start: import_location,
            end: import_location,
        },
        new_text: ["import ", module_full_name, new_lines].concat(),
    }
}
