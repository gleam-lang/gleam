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
    pub trailing_commas: Vec<u32>,
}

impl ModuleExtra {
    pub fn new() -> Self {
        Default::default()
    }

    /// Detects if a byte index is in a comment context
    pub fn is_within_comment(&self, byte_index: u32) -> bool {
        let cmp = |span: &SrcSpan| {
            if byte_index < span.start {
                Ordering::Greater
            } else if byte_index > span.end {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        };

        self.comments.binary_search_by(cmp).is_ok()
            || self.doc_comments.binary_search_by(cmp).is_ok()
            || self.module_comments.binary_search_by(cmp).is_ok()
    }

    pub(crate) fn has_comment_between(&self, start: u32, end: u32) -> bool {
        self.first_comment_between(start, end).is_some()
    }

    fn index_of_first_comment_between(&self, start: u32, end: u32) -> Option<usize> {
        self.comments
            .binary_search_by(|comment| {
                if comment.end < start {
                    Ordering::Less
                } else if comment.start > end {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            })
            .ok()
    }

    pub fn first_comment_between(&self, start: u32, end: u32) -> Option<SrcSpan> {
        self.index_of_first_comment_between(start, end)
            .and_then(|index| self.comments.get(index).copied())
    }

    pub fn last_comment_between(&self, start: u32, end: u32) -> Option<SrcSpan> {
        // We start from the first comment that we can find in between the given
        // start and end, this is really fast as we can find such index through
        // binary search.
        let mut index_of_last_comment = self.index_of_first_comment_between(start, end)?;

        // Then we go over all the comments we can find from that one that are
        // still in between the given indices.
        loop {
            let next_comment = self.comments.get(index_of_last_comment + 1);
            if let Some(next_comment) = next_comment
                && start <= next_comment.start
                && next_comment.end <= end
            {
                // The next comment is still in between the two indices, we keep
                // going.
                index_of_last_comment += 1;
            } else {
                // The next comment is outside of the given range, that means
                // the current one is the last comment we were looking for.
                // We can return it!
                return self.comments.get(index_of_last_comment).cloned();
            }
        }
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
