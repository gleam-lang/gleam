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
            let unchanged_tail = lines[end_line..].join("\n");
        
            let changed_head = &lines[start_line][..start_col];
            let changed_last = &lines[end_line][end_col..];

            format!("{}{}{}{}{}", unchanged_init, changed_head, change.text, changed_last, unchanged_tail).to_string()
        },
        None => {
            change.text.clone()
        }
    }


    
}