// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use lsp_types::Position;
use std::collections::HashMap;

#[cfg(test)]
mod tests;

#[derive(
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Default,
    Clone,
    Copy,
    serde::Serialize,
    serde::Deserialize,
    Hash,
)]
pub struct SrcSpan {
    pub start: u32,
    pub end: u32,
}

impl SrcSpan {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub fn contains(&self, byte_index: u32) -> bool {
        byte_index >= self.start && byte_index <= self.end
    }

    pub fn contains_span(&self, span: SrcSpan) -> bool {
        self.contains(span.start) && self.contains(span.end)
    }

    /// Merges two spans into a new one that starts at the start of the smaller
    /// one and ends at the end of the bigger one. For example:
    ///
    /// ```txt
    /// wibble    wobble
    /// ─┬────    ─┬────
    ///  │         ╰─ one span
    ///  ╰─ the other span
    /// ─┬──────────────
    ///  ╰─ the span you get by merging the two
    /// ```
    pub fn merge(&self, with: &SrcSpan) -> SrcSpan {
        Self {
            start: self.start.min(with.start),
            end: self.end.max(with.end),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }
}

/// A 1-index line and column position
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineColumn {
    pub line: u32,
    pub column: u32,
}

/// A struct which contains information about line numbers of a source file,
/// and can convert between byte offsets that are used in the compiler and
/// line-column pairs used in LSP.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct LineNumbers {
    /// The byte offsets of the start of each line of the source file
    pub line_starts: Vec<u32>,
    /// The total length of the source file
    pub length: u32,
    /// A mapping of byte offsets to character length information. This is used
    /// when converting between byte indices and line-column numbers, because
    /// LSP uses UTF-16, while Rust encodes strings as UTF-8.
    ///
    /// This only contains characters which are more than one byte in UTF-8,
    /// because one byte UTF-8 characters are one UTF-16 segment also, so no
    /// translation is needed.
    ///
    /// We could store the whole source file here instead, however that would
    /// be quite wasteful. Most Gleam programs use only ASCII characters, meaning
    /// UTF-8 offsets are the same as UTF-16 ones. With this representation, we
    /// only need to store a few characters.
    ///
    /// In most programs this will be empty because they will only be using
    /// ASCII characters.
    pub mapping: HashMap<usize, Character>,
}

/// Information about how a character is encoded in UTF-8 and UTF-16.
#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct Character {
    /// The number of bytes needed to encode this in UTF-8.
    pub length_utf8: u8,
    /// The number of 16-bit segments needed to encode this in UTF-16.
    pub length_utf16: u8,
}

impl LineNumbers {
    pub fn new(src: &str) -> Self {
        Self {
            length: src.len() as u32,
            line_starts: std::iter::once(0)
                .chain(src.match_indices('\n').map(|(i, _)| i as u32 + 1))
                .collect(),
            mapping: Self::mapping(src),
        }
    }

    fn mapping(src: &str) -> HashMap<usize, Character> {
        let mut map = HashMap::new();

        for (i, char) in src.char_indices() {
            let length = char.len_utf8();
            if length != 1 {
                _ = map.insert(
                    i,
                    Character {
                        length_utf8: length as u8,
                        length_utf16: char.len_utf16() as u8,
                    },
                );
            }
        }

        map
    }

    /// Returns the 1-indexed line number of a given byte index
    pub fn line_number(&self, byte_index: u32) -> u32 {
        self.line_starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1) as u32
            + 1
    }

    /// Returns the 1-indexed line and column number of a given byte index,
    /// using a UTF-8 character offset.
    pub fn line_and_utf8_column_number(&self, byte_index: u32) -> LineColumn {
        let line = self.line_number(byte_index);
        let line_start = self
            .line_starts
            .get(line as usize - 1)
            .copied()
            .unwrap_or_default();

        LineColumn {
            line,
            column: byte_index - line_start,
        }
    }

    /// Returns the 1-indexed line and column number of a given byte index,
    /// using a UTF-16 character offset.
    pub fn line_and_utf16_column_number(&self, byte_index: u32) -> LineColumn {
        let line = self.line_number(byte_index);
        let line_start = self
            .line_starts
            .get(line as usize - 1)
            .copied()
            .unwrap_or_default();

        let mut u8_offset = line_start;
        let mut u16_offset = 0;

        loop {
            if u8_offset >= byte_index {
                break;
            }

            if let Some(length) = self.mapping.get(&(u8_offset as usize)) {
                u8_offset += length.length_utf8 as u32;
                u16_offset += length.length_utf16 as u32;
            } else {
                u16_offset += 1;
                u8_offset += 1;
            }
        }

        LineColumn {
            line,
            column: u16_offset + 1,
        }
    }

    /// Returns the byte index of the corresponding LSP line-column `Position`,
    /// translating from a UTF-16 character index to a UTF-8 byte index.
    pub fn byte_index(&self, position: Position) -> u32 {
        let line_start = match self.line_starts.get(position.line as usize) {
            Some(&line_start) => line_start,
            None => return self.length,
        };

        let mut u8_offset = line_start;
        let mut u16_offset = 0;

        loop {
            if u16_offset >= position.character {
                break;
            }

            if let Some(length) = self.mapping.get(&(u8_offset as usize)) {
                u8_offset += length.length_utf8 as u32;
                u16_offset += length.length_utf16 as u32;
            } else {
                u16_offset += 1;
                u8_offset += 1;
            }
        }

        u8_offset
    }

    /// Checks if the given span spans an entire line (excluding the newline
    /// character itself).
    pub fn spans_entire_line(&self, span: &SrcSpan) -> bool {
        self.line_starts.iter().any(|&line_start| {
            line_start == span.start && self.line_starts.contains(&(span.end + 1))
        })
    }
}
