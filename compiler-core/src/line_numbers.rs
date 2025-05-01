use std::collections::HashMap;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct LineNumbers {
    pub line_starts: Vec<u32>,
    pub length: u32,
    pub mapping: HashMap<usize, Character>,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct Character {
    pub length_utf8: u8,
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

    /// Get the line number for a byte index
    pub fn line_number(&self, byte_index: u32) -> u32 {
        self.line_starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1) as u32
            + 1
    }

    pub fn line_and_column_number_utf8(&self, byte_index: u32) -> LineColumn {
        let line = self.line_number(byte_index);
        let column = byte_index
            - self
                .line_starts
                .get(line as usize - 1)
                .copied()
                .unwrap_or_default()
            + 1;
        LineColumn { line, column }
    }

    pub fn line_and_column_number(&self, byte_index: u32) -> LineColumn {
        let mut line_and_column = self.line_and_column_number_utf8(byte_index);

        let mut u8_offset = 0;
        let mut u16_offset = 0;
        let target = line_and_column.column - 1;

        loop {
            if u8_offset >= target {
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

        line_and_column.column = u16_offset + 1;

        line_and_column
    }

    /// 0 indexed line and character to byte index
    pub fn byte_index(&self, line: u32, character: u32) -> u32 {
        let line_start = match self.line_starts.get(line as usize) {
            Some(&line_start) => line_start,
            None => return self.length,
        };

        let mut u8_offset = 0;
        let mut u16_offset = 0;
        let target = character;

        loop {
            if u16_offset >= target {
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

        line_start + u8_offset
    }
}

#[test]
fn byte_index() {
    let src = r#"import gleam/io

pub fn main() {
  io.println("Hello, world!")
}
"#;
    let line_numbers = LineNumbers::new(src);

    assert_eq!(line_numbers.byte_index(0, 0), 0);
    assert_eq!(line_numbers.byte_index(0, 4), 4);
    assert_eq!(line_numbers.byte_index(100, 1), src.len() as u32);
    assert_eq!(line_numbers.byte_index(2, 1), 18);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineColumn {
    pub line: u32,
    pub column: u32,
}
