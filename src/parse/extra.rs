use crate::ast::SrcSpan;

#[derive(Debug, PartialEq)]
pub struct ModuleExtra {
    pub module_comments: Vec<SrcSpan>,
    pub doc_comments: Vec<SrcSpan>,
    pub comments: Vec<SrcSpan>,
    pub empty_lines: Vec<usize>,
}

impl ModuleExtra {
    pub fn new() -> Self {
        ModuleExtra {
            module_comments: vec![],
            doc_comments: vec![],
            comments: vec![],
            empty_lines: vec![],
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Comment<'a> {
    pub start: usize,
    pub content: &'a str,
}

impl<'a> From<(&SrcSpan, &'a str)> for Comment<'a> {
    fn from(src: (&SrcSpan, &'a str)) -> Comment<'a> {
        Comment {
            start: src.0.start,
            content: &src.1[src.0.start..src.0.end],
        }
    }
}
