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

    pub fn is_within_comment(&self, byte_index: u32) -> bool {
        self.comments
            .binary_search_by(|span| span.cmp_byte_index(byte_index))
            .is_ok()
            || self
                .doc_comments
                .binary_search_by(|span| span.cmp_byte_index(byte_index))
                .is_ok()
            || self
                .module_comments
                .binary_search_by(|span| span.cmp_byte_index(byte_index))
                .is_ok()
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
