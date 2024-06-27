use std::cmp::Ordering;

use ecow::EcoString;

use crate::ast::SrcSpan;

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ModuleExtra {
    pub module_comments: Vec<SrcSpan>,
    pub doc_comments: Vec<SrcSpan>,
    pub comments: Vec<SrcSpan>,
    pub empty_lines: Vec<u32>,
    pub new_lines: Vec<u32>,
}

impl ModuleExtra {
    pub fn new() -> Self {
        Default::default()
    }

    /// Detects if a byte index is in a comment context
    pub fn is_within_comment(&self, byte_index: u32) -> bool {
        let cmp = |span: &SrcSpan| {
            if byte_index < span.start {
                Ordering::Less
            } else if span.end < byte_index {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        };

        self.comments.binary_search_by(cmp).is_ok()
            || self.doc_comments.binary_search_by(cmp).is_ok()
            || self.module_comments.binary_search_by(cmp).is_ok()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Comment<'a> {
    pub start: u32,
    pub content: &'a str,
}

impl<'a> From<(&SrcSpan, &'a EcoString)> for Comment<'a> {
    fn from(value: (&SrcSpan, &'a EcoString)) -> Self {
        Self::from((value.0, value.1.as_str()))
    }
}

impl<'a> From<(&SrcSpan, &'a str)> for Comment<'a> {
    fn from(src: (&SrcSpan, &'a str)) -> Comment<'a> {
        let start = src.0.start;
        let end = src.0.end as usize;
        Comment {
            start,
            content: src
                .1
                .get(start as usize..end)
                .expect("From span to comment"),
        }
    }
}
