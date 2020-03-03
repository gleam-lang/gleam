use super::doc;

#[derive(Debug, PartialEq)]
pub enum Error {
    TooManyHolesInCapture {
        location: crate::ast::SrcSpan,
        count: usize,
    },
}

pub type LalrpopError = lalrpop_util::ParseError<usize, (usize, String), Error>;

/// Blanks out comments, semicolons, etc
///
pub fn strip_extra(src: &str) -> (String, doc::DocBlockManager) {
    enum Mode {
        Normal,
        String,
        Comment,
        DocComment,
    };

    let mut buffer = String::with_capacity(src.len());
    let mut mode = Mode::Normal;
    let mut chars = src.chars().enumerate();
    let mut at_bol = true;
    let mut doc_comment_buffer = String::with_capacity(500);
    let mut doc_block = doc::DocBlockManager::new();

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
                        match (chars.next(), at_bol) {
                            (Some((_, '/')), true) => {
                                mode = Mode::DocComment;
                                buffer.push(' ');
                            }

                            (Some((_, _c2)), _) => {
                                mode = Mode::Comment;
                                buffer.push(' ');
                            }
                            (None, _) => {
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

            Mode::Comment => match c {
                '\n' => {
                    mode = Mode::Normal;
                    buffer.push('\n');
                }
                _ => buffer.push(' '),
            },

            Mode::DocComment => match c {
                '\n' => {
                    mode = Mode::Normal;
                    doc_block.add_line(
                        outer_char_no - 3 - doc_comment_buffer.len(),
                        doc_comment_buffer.trim_start().to_string(),
                    );
                    doc_comment_buffer.clear();
                    buffer.push('\n');
                }
                c => {
                    doc_comment_buffer.push(c);
                    buffer.push(' ');
                }
            },
        }

        if c == '\n' {
            at_bol = true;
        } else {
            at_bol = false;
        }
    }

    (buffer, doc_block)
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

    let expected = "             
                  

                      
pub fn() -> {}
";
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
