use ecow::EcoString;
use lsp_types::{Position, Range, TextEdit};

use crate::{
    ast::{Definition, Import, SrcSpan, TypedDefinition},
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

pub fn insert_unqualified_import(
    import: &Import<EcoString>,
    code: &str,
    name: String,
) -> (u32, String) {
    let SrcSpan { start, end } = import.location;

    let import_code = code
        .get(start as usize..end as usize)
        .expect("Import location is invalid");
    let has_brace = import_code.contains('}');

    if has_brace {
        insert_into_braced_import(name, import.location, import_code)
    } else {
        insert_into_unbraced_import(name, import, import_code)
    }
}

// Handle inserting into an unbraced import
fn insert_into_unbraced_import(
    name: String,
    import: &Import<EcoString>,
    import_code: &str,
) -> (u32, String) {
    let location = import.location;
    if import.as_name.is_none() {
        // Case: import module
        (location.end, format!(".{{{name}}}"))
    } else {
        // Case: import module as alias
        let as_pos = import_code
            .find(" as ")
            .expect("Expected ' as ' in import statement");
        let before_as_pos = import_code
            .get(..as_pos)
            .and_then(|s| s.rfind(|c: char| !c.is_whitespace()))
            .map(|pos| location.start as usize + pos + 1)
            .expect("Expected non-whitespace character before ' as '");
        (before_as_pos as u32, format!(".{{{name}}}"))
    }
}

// Handle inserting into a braced import
fn insert_into_braced_import(name: String, location: SrcSpan, import_code: &str) -> (u32, String) {
    if let Some((pos, c)) = find_last_char_before_closing_brace(location, import_code) {
        // Case: import module.{Existing, } (as alias)
        if c == ',' {
            (pos as u32 + 1, format!(" {name}"))
        } else {
            // Case: import module.{Existing} (as alias)
            (pos as u32 + 1, format!(", {name}"))
        }
    } else {
        // Case: import module.{} (as alias)
        let left_brace_pos = import_code
            .find('{')
            .map(|pos| location.start as usize + pos)
            .expect("Expected '{' in import statement");
        (left_brace_pos as u32 + 1, name)
    }
}

fn find_last_char_before_closing_brace(
    location: SrcSpan,
    import_code: &str,
) -> Option<(usize, char)> {
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
        return Some((location.start as usize + pos, c));
    }
    None
}
