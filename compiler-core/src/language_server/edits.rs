use ecow::EcoString;
use lsp_types::{Position, Range, TextEdit};

use crate::{
    ast::{Definition, Import, Layer, SrcSpan, TypedDefinition},
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
    let import_start_cursor = line_numbers.byte_index(import_location);
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

pub fn add_import_with_unqualified(
    import_position: Position,
    module_name: &str,
    name_to_import: &str,
    layer: Layer,
    insert_newlines: &Newlines,
) -> TextEdit {
    let new_lines = match insert_newlines {
        Newlines::Single => "\n",
        Newlines::Double => "\n\n",
    };
    let prefix = match layer {
        Layer::Type => "type ",
        Layer::Value => "",
    };
    TextEdit {
        range: Range {
            start: import_position,
            end: import_position,
        },
        new_text: [
            "import ",
            module_name,
            ".{",
            prefix,
            name_to_import,
            "}",
            new_lines,
        ]
        .concat(),
    }
}

pub fn add_unqualified_import(
    name: &EcoString,
    layer: Layer,
    module: &Module,
    import: &Import<EcoString>,
    line_numbers: &LineNumbers,
) -> TextEdit {
    let import_code = get_import_code(module, import);
    let has_brace = import_code.contains('}');

    let name = match layer {
        Layer::Type => &format!("type {}", name).into(),
        Layer::Value => name,
    };

    let (pos, new_text) = if has_brace {
        insert_into_braced_import(module, name, import)
    } else {
        insert_into_unbraced_import(module, name, import)
    };

    TextEdit {
        range: src_span_to_lsp_range(
            SrcSpan {
                start: pos,
                end: pos,
            },
            line_numbers,
        ),
        new_text,
    }
}

fn get_import_code<'a>(module: &'a Module, import: &'a Import<EcoString>) -> &'a str {
    module
        .code
        .get(import.location.start as usize..import.location.end as usize)
        .expect(
            format!(
                "The code of the import of {} couldn't be found in the module {}",
                import.module, module.name,
            )
            .as_str(),
        )
}

// Handle inserting into an unbraced import
fn insert_into_unbraced_import(
    module: &Module,
    name: &EcoString,
    import: &Import<EcoString>,
) -> (u32, String) {
    let location = import.location;
    match import.alias_location() {
        // Case: import module
        None => (location.end, format!(".{{{}}}", name)),
        Some(as_pos) => {
            // Case: import module as alias
            let import_code = &get_import_code(module, import);
            let before_as_pos = import_code
                .get(..(as_pos.start as usize))
                .and_then(|s| s.rfind(|c: char| !c.is_whitespace()))
                .map(|pos| location.start as usize + pos + 1)
                .expect("Expected non-whitespace character before ' as '");
            (before_as_pos as u32, format!(".{{{}}}", name))
        }
    }
}

// Handle inserting into a braced import
fn insert_into_braced_import(
    module: &Module,
    name: &EcoString,
    import: &Import<EcoString>,
) -> (u32, String) {
    if let Some((pos, c)) = find_last_char_before_closing_brace(module, import) {
        // Case: import module.{Existing, } (as alias)
        if c == ',' {
            (pos as u32 + 1, format!(" {}", name))
        } else {
            // Case: import module.{Existing} (as alias)
            (pos as u32 + 1, format!(", {}", name))
        }
    } else {
        // Case: import module.{} (as alias)
        let import_code = get_import_code(module, import);
        let left_brace_pos = import_code
            .find('{')
            .map(|pos| import.location.start as usize + pos)
            .expect("Expected '{' in import statement");
        (left_brace_pos as u32 + 1, name.to_string())
    }
}

fn find_last_char_before_closing_brace(
    module: &Module,
    import: &Import<EcoString>,
) -> Option<(usize, char)> {
    let import_code = get_import_code(module, import);
    let closing_brace_pos = import_code.rfind('}')?;

    let bytes = import_code.as_bytes();
    let mut pos = closing_brace_pos;
    while pos > 0 {
        pos -= 1;
        let c = (*bytes.get(pos)?) as char;
        if c.is_whitespace() {
            continue;
        }
        if c == '{' {
            break;
        }
        return Some((import.location.start as usize + pos, c));
    }
    None
}
