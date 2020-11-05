use tower_lsp::lsp_types::TextDocumentContentChangeEvent;

#[derive(Debug)]
pub struct Document {
    version: i64,
    contents: String,
}

impl Document {
    pub fn new(contents: String) -> Self {
        Document {
            version: 0,
            contents,
        }
    }

    pub fn apply_content_changes(&mut self, content_changes: Vec<TextDocumentContentChangeEvent>) {
        self.version += 1;

        self.contents = content_changes.iter().fold(self.contents().to_string(), |contents, change| apply_content_change(&contents,change));
    }

    pub fn contents(&self) -> &str {
        &self.contents
    }
}

fn apply_content_change(contents: &str, change: &TextDocumentContentChangeEvent) -> String {
    match change.range {
        Some(range) => {
            let lines: Vec<&str> = contents.lines().collect();

            let start_line = range.start.line as usize;
            let end_line = range.end.line as usize;
        
            let start_col = range.start.character as usize;
            let end_col = range.end.character as usize;
        
            let unchanged_init = lines[..start_line].join("\n");
            let unchanged_tail = lines[end_line+1..].join("\n");
        
            let changed_head = &lines[start_line][..start_col];
            let changed_last = &lines[end_line][end_col..];
            
            format!("{}{}{}{}{}{}{}",
                unchanged_init,
                if unchanged_init.is_empty() { "" } else { "\n" },
                changed_head,
                change.text,
                changed_last,
                if unchanged_tail.is_empty() { "" } else { "\n" },
                unchanged_tail)
        },
        None => {
            change.text.clone()
        }
    }    
}

#[cfg(test)]
mod test {
    use super::*;

    use tower_lsp::lsp_types::{ Position, Range };

    #[test]
    fn apply_content_change_no_range_replaces_all_text() {
        let change = TextDocumentContentChangeEvent {
            range: None,
            range_length: None,
            text: "Salut, tout la monde!".to_string(),
        };

        let original = "Hello, world!";
        let result = apply_content_change(original, &change);
        let expected = "Salut, tout la monde!";

        assert_eq!(result, expected);
    }

    #[test]
    fn apply_content_change_empty_range_inserts_text() {
        let comma_pos = Position{
            line: 0u64,
            character: 5u64,
        };

        let change = TextDocumentContentChangeEvent {
            range: Some(Range { start: comma_pos, end: comma_pos }),
            range_length: None,
            text: ",".to_string(),
        };

        let original = "Hello world!";
        let result = apply_content_change(original, &change);
        let expected = "Hello, world!";

        assert_eq!(result, expected);
    }

    #[test]
    fn apply_content_change_empty_text_deletes_range() {
        let comma_start = Position{
            line: 0u64,
            character: 5u64,
        };
        let comma_end = Position{
            line: 0u64,
            character: 6u64,
        };

        let change = TextDocumentContentChangeEvent {
            range: Some(Range { start: comma_start, end: comma_end }),
            range_length: None,
            text: "".to_string(),
        };

        let original = "Hello, world!";
        let result = apply_content_change(original, &change);
        let expected = "Hello world!";

        assert_eq!(result, expected);
    }

    #[test]
    fn apply_content_change_replaces_range() {
        let range_start = Position{
            line: 0u64,
            character: 7u64,
        };
        let range_end = Position{
            line: 0u64,
            character: 12u64,
        };
        let change = TextDocumentContentChangeEvent {
            range: Some(Range { start: range_start, end: range_end }),
            range_length: None,
            text: "everyone".to_string(),
        };

        let original = "Hello, world!";
        let result = apply_content_change(original, &change);
        let expected = "Hello, everyone!";

        assert_eq!(result, expected);
    }

    #[test]
    fn apply_content_change_replaces_range_multiline() {
        let range_start = Position{
            line: 2u64,
            character: 0u64,
        };
        let range_end = Position{
            line: 3u64,
            character: 1u64,
        };

        let change = TextDocumentContentChangeEvent {
            range: Some(Range { start: range_start, end: range_end }),
            range_length: None,
            text: "3, 4".to_string(),
        };
        let original = "a\nb\nc\nd\ne";
        let result = apply_content_change(original, &change);
        let expected = "a\nb\n3, 4\ne";

        assert_eq!(result, expected);
    }
}