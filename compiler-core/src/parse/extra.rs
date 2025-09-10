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

    /// Returns the first comment overlapping the given source locations (inclusive)
    /// Note that the returned span covers the text of the comment, not the `//`
    pub(crate) fn first_comment_between(&self, start: u32, end: u32) -> Option<SrcSpan> {
        let inner = |comments: &[SrcSpan], start, end| {
            if comments.is_empty() {
                return None;
            }

            comments
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
        };

        let mut best = None;
        let mut search_list = &self.comments[..];
        while let Some(index) = inner(search_list, start, end) {
            best = self.comments.get(index);
            search_list = search_list.get(0..index).unwrap_or(&[]);
        }
        best.copied()
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

#[cfg(test)]
mod tests {
    use crate::{ast::SrcSpan, parse::extra::ModuleExtra};

    fn set_up_extra() -> ModuleExtra {
        let mut extra = ModuleExtra::new();
        extra.comments = vec![
            SrcSpan { start: 0, end: 10 },
            SrcSpan { start: 20, end: 30 },
            SrcSpan { start: 40, end: 50 },
            SrcSpan { start: 60, end: 70 },
            SrcSpan { start: 80, end: 90 },
            SrcSpan {
                start: 90,
                end: 100,
            },
        ];
        extra
    }

    #[test]
    fn first_comment_between() {
        let extra = set_up_extra();
        assert!(matches!(
            extra.first_comment_between(15, 85),
            Some(SrcSpan { start: 20, end: 30 })
        ));
    }

    #[test]
    fn first_comment_between_equal_to_range() {
        let extra = set_up_extra();
        assert!(matches!(
            extra.first_comment_between(40, 50),
            Some(SrcSpan { start: 40, end: 50 })
        ));
    }

    #[test]
    fn first_comment_between_overlapping_start_of_range() {
        let extra = set_up_extra();
        assert!(matches!(
            extra.first_comment_between(45, 80),
            Some(SrcSpan { start: 40, end: 50 })
        ));
    }

    #[test]
    fn first_comment_between_overlapping_end_of_range() {
        let extra = set_up_extra();
        assert!(matches!(
            extra.first_comment_between(35, 45),
            Some(SrcSpan { start: 40, end: 50 })
        ));
    }

    #[test]
    fn first_comment_between_at_end_of_range() {
        let extra = set_up_extra();
        assert!(matches!(
            dbg!(extra.first_comment_between(55, 60)),
            Some(SrcSpan { start: 60, end: 70 })
        ));
    }
}
