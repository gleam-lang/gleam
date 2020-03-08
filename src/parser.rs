#[derive(Debug, PartialEq)]
pub enum Error {
    TooManyHolesInCapture {
        meta: crate::ast::Meta,
        count: usize,
    },
}

/// Blanks out comments, semicolons, etc
///
pub fn strip_extra(src: &str) -> String {
    enum Mode {
        Normal,
        String,
        Comment,
    };

    let mut buffer = String::with_capacity(src.len());
    let mut mode = Mode::Normal;
    let mut chars = src.chars();
    while let Some(c) = chars.next() {
        match mode {
            Mode::Normal => match c {
                ';' => buffer.push(' '),

                '"' => {
                    mode = Mode::String;
                    buffer.push(c);
                }

                '/' => match chars.next() {
                    Some('/') => {
                        mode = Mode::Comment;
                        buffer.push(' ');
                        buffer.push(' ');
                    }
                    Some(c2) => {
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
                    if let Some(c) = chars.next() {
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
        }
    }
    buffer
}

#[test]
fn strip_extra_test() {
    assert_eq!(strip_extra(&""), "".to_string());
    assert_eq!(strip_extra(&" ; "), "   ".to_string());
    assert_eq!(strip_extra(&" // hi\n "), "      \n ".to_string());
    assert_eq!(strip_extra(&r#""\"//" hi"#), r#""\"//" hi"#.to_string());
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

pub fn meta(start: usize, end: usize) -> crate::ast::Meta {
    crate::ast::Meta { start, end }
}
