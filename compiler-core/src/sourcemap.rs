use sourcemap::SourceMapBuilder;

use crate::line_numbers::LineColumn;

#[derive(Debug, Clone, Copy)]
pub enum SourceMapSupport {
    Emit,
    None,
}

pub enum SourceMapEmitter {
    Null,
    Emit(Box<SourceMapBuilder>),
}

// A manual implementation of Debug since SourceMapBuilder doesn't support Debug
impl std::fmt::Debug for SourceMapEmitter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SourceMapEmitter::Null => f.write_str("Null"),
            SourceMapEmitter::Emit(_) => f.write_str("Emit"),
        }
    }
}

impl SourceMapEmitter {
    pub fn add_mapping(
        &mut self,
        generated_code_line: u32,
        generated_code_column: u32,
        source_location: LineColumn,
    ) {
        match self {
            SourceMapEmitter::Null => (),
            SourceMapEmitter::Emit(source_map) => {
                let _ = source_map.add_raw(
                    generated_code_line,
                    generated_code_column,
                    // -1 Because sourcemaps are 0-indexed, LineColumn isn't
                    source_location.line - 1,
                    source_location.column - 1,
                    Some(0), // Because one Gleam file -> one target file
                    None,
                    false,
                );
            }
        }
    }

    pub fn null() -> Self {
        SourceMapEmitter::Null
    }
}
