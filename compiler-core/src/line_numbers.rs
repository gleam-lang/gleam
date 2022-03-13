#[derive(Debug)]
pub struct LineNumbers {
    line_starts: Vec<usize>,
    length: usize,
}

impl LineNumbers {
    pub fn new(src: &str) -> Self {
        Self {
            length: src.len(),
            line_starts: std::iter::once(0)
                .chain(src.match_indices('\n').map(|(i, _)| i + 1))
                .collect(),
        }
    }

    /// Get the line number for a byte index
    pub fn line_number(&self, byte_index: usize) -> usize {
        self.line_starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1)
            + 1
    }

    // TODO: handle unicode characters that may be more than 1 byte in width
    pub fn line_and_column_number(&self, byte_index: usize) -> LineColumn {
        let line = self.line_number(byte_index);
        let column = byte_index - self.line_starts.get(line - 1).copied().unwrap_or_default() + 1;
        LineColumn { line, column }
    }

    // TODO: handle unicode characters that may be more than 1 byte in width
    /// 0 indexed line and character to byte index
    pub fn byte_index(&self, line: u32, character: u32) -> usize {
        match self.line_starts.get((line) as usize) {
            Some(line_index) => *line_index + character as usize,
            None => self.length,
        }
    }
}

#[test]
fn byte_index() {
    let src = &r#"import gleam/io

pub fn main() {
  io.println("Hello, world!")
}
"#;
    let line_numbers = LineNumbers::new(src);

    assert_eq!(line_numbers.byte_index(0, 0), 0);
    assert_eq!(line_numbers.byte_index(0, 4), 4);
    assert_eq!(line_numbers.byte_index(100, 1), src.len());
    assert_eq!(line_numbers.byte_index(2, 1), 18);
}

#[derive(Debug, Clone, Copy)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}
