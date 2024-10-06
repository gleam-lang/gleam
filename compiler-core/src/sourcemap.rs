use sourcemap::SourceMapBuilder;

use crate::line_numbers::LineColumn;

#[derive(Debug, Clone, Copy)]
pub enum SourceMapSupport {
    Emit,
    None,
}

pub enum SourceMapEmitter {
    Null,
    Emit(SourceMapBuilder),
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
    pub fn add_mapping(&mut self, dst_line: u32, dst_col: u32, src_location: LineColumn) {
        match self {
            SourceMapEmitter::Null => (),
            SourceMapEmitter::Emit(source_map) => {
                tracing::debug!("emitting one sourcemap entry");
                let _ = source_map.add_raw(
                    dst_line,
                    dst_col,
                    // -1 Because sourcemaps are 0-indexed, LineColumn isn't
                    src_location.line - 1,
                    src_location.column - 1,
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
