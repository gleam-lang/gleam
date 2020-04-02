use super::doc;
use itertools::Itertools;

#[derive(Debug, PartialEq)]
pub enum Error {
    TooManyHolesInCapture {
        location: crate::ast::SrcSpan,
        count: usize,
    },
}

pub type LalrpopError = lalrpop_util::ParseError<usize, (usize, String), Error>;

#[derive(Debug)]
pub struct ModuleComments<'a> {
    pub doc_comments: Vec<Comment<'a>>,
    pub comments: Vec<Comment<'a>>,
}

#[derive(Debug)]
pub struct Comment<'a> {
    pub start: usize,
    pub content: &'a str,
}

pub fn take_before<'a>(
    comments: &'a [Comment<'a>],
    limit: usize,
) -> (Option<String>, &'a [Comment<'a>]) {
    let mut end = 0;
    for (i, comment) in comments.iter().enumerate() {
        if comment.start > limit {
            break;
        }
        end = i;
    }

    if end == 0 {
        (None, comments)
    } else {
        let comment = comments[0..end].iter().map(|c| c.content).join("\n");
        (Some(comment), &comments[end..])
    }
}

/// Blanks out comments, semicolons, etc
///
pub fn strip_extra(
    src: &str,
) -> (
    String,
    doc::block_manager::DocBlockManager,
    ModuleComments<'_>,
) {
    enum Mode {
        Normal,
        String,
        Comment(usize),
        DocComment(usize),
    };

    let mut buffer = String::with_capacity(src.len());
    let mut mode = Mode::Normal;
    let mut chars = src.chars().enumerate();
    let mut doc_comment_buffer = String::with_capacity(500);
    let mut doc_block = doc::block_manager::DocBlockManager::new();
    let mut comments = ModuleComments {
        doc_comments: vec![],
        comments: vec![],
    };

    while let Some((outer_char_no, c)) = chars.next() {
        match mode {
            Mode::Normal => match c {
                ';' => buffer.push(' '),

                '"' => {
                    mode = Mode::String;
                    buffer.push(c);
                }

                '/' => match chars.next() {
                    Some((_, '/')) => {
                        buffer.push(' ');
                        buffer.push(' ');
                        match chars.next() {
                            Some((i, '/')) => {
                                mode = Mode::DocComment(i);
                                buffer.push(' ');
                            }
                            Some((i, _c2)) => {
                                mode = Mode::Comment(i);
                                buffer.push(' ');
                            }
                            None => {
                                buffer.push(c);
                            }
                        }
                    }
                    Some((_, c2)) => {
                        buffer.push(c);
                        buffer.push(c2);
                    }
                    None => buffer.push(c),
                },

                _ => buffer.push(c),
            },

            Mode::String => match c {
                '\\' => {
                    buffer.push(c);
                    if let Some((_, c)) = chars.next() {
                        buffer.push(c)
                    }
                }

                '"' => {
                    mode = Mode::Normal;
                    buffer.push(c);
                }

                _ => buffer.push(c),
            },

            Mode::Comment(start) => match c {
                '\n' => {
                    mode = Mode::Normal;
                    comments.comments.push(Comment {
                        start,
                        content: &src[start..outer_char_no],
                    });
                    buffer.push('\n');
                }
                _ => buffer.push(' '),
            },

            Mode::DocComment(start) => match c {
                '\n' => {
                    mode = Mode::Normal;
                    doc_block.add_line(outer_char_no, doc_comment_buffer.to_string());
                    doc_comment_buffer.clear();
                    comments.doc_comments.push(Comment {
                        start,
                        content: &src[start..outer_char_no],
                    });
                    buffer.push('\n');
                }
                c => {
                    doc_comment_buffer.push(c);
                    buffer.push(' ');
                }
            },
        }
    }

    (buffer, doc_block, comments)
}

#[test]
fn strip_extra_test() {
    assert_eq!(strip_extra(&"").0, "".to_string());
    assert_eq!(strip_extra(&" ; ").0, "   ".to_string());
    assert_eq!(strip_extra(&" // hi\n ").0, "      \n ".to_string());
    assert_eq!(strip_extra(&r#""\"//" hi"#).0, r#""\"//" hi"#.to_string());
    assert_eq!(
        strip_extra(&"/// Something\n").0,
        "             \n".to_string()
    );
    assert_eq!(
        strip_extra(&" /// Something\n").0,
        "              \n".to_string()
    );
    let str = "
/// Something
/// Something else

/// This is a function
pub fn() -> {}
"
    .trim_start();

    let expected = "             \n                  \n
                      \npub fn() -> {}\n";
    let multi_doc_result = strip_extra(&str);

    assert_eq!(multi_doc_result.0, expected.to_string());
}

pub fn seq(mut exprs: Vec<crate::ast::UntypedExpr>) -> crate::ast::UntypedExpr {
    use crate::ast::*;

    let head = exprs.pop().unwrap();
    exprs
        .into_iter()
        .rev()
        .fold(head, |acc, expr| UntypedExpr::Seq {
            first: Box::new(expr),
            then: Box::new(acc),
        })
}

pub fn location(start: usize, end: usize) -> crate::ast::SrcSpan {
    crate::ast::SrcSpan { start, end }
}
