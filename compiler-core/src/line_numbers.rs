#[derive(Debug)]
pub struct LineNumbers {
    line_starts: Vec<usize>,
}

impl LineNumbers {
    pub fn new(src: &str) -> Self {
        Self {
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
}
