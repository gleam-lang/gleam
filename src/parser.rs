use itertools::Itertools;

#[derive(Debug, PartialEq)]
pub enum Error {
    TooManyHolesInCapture {
        location: crate::ast::SrcSpan,
        count: usize,
    },
}

pub type LalrpopError = lalrpop_util::ParseError<usize, (usize, String), Error>;

#[derive(Debug, PartialEq)]
pub struct ModuleComments<'a> {
    pub doc_comments: Vec<Comment<'a>>,
    pub comments: Vec<Comment<'a>>,
}

#[derive(Debug, PartialEq)]
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
pub fn strip_extra(src: &str) -> (String, ModuleComments<'_>) {
    #[derive(Clone, Copy)]
    enum Kind {
        Regular,
        Doc,
    }
    enum Mode {
        Normal,
        String,
        Comment(Kind, usize),
    };

    let mut buffer = String::with_capacity(src.len());
    let mut mode = Mode::Normal;
    let mut chars = src.chars().enumerate().peekable();
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
                        match chars.peek() {
                            Some((_, '/')) => {
                                mode = Mode::Comment(Kind::Doc, outer_char_no);
                                buffer.push(' ');
                                chars.next();
                            }
                            Some((_, _c2)) => {
                                mode = Mode::Comment(Kind::Regular, outer_char_no);
                            }
                            None => {
                                buffer.push(c);
                                chars.next();
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

            Mode::Comment(kind, start) => match c {
                '\n' => {
                    mode = Mode::Normal;
                    let content_start = match &kind {
                        Kind::Doc => start + 3,
                        Kind::Regular => start + 2,
                    };
                    let comment = Comment {
                        start,
                        content: &src[content_start..outer_char_no],
                    };
                    match &kind {
                        Kind::Doc => &mut comments.doc_comments,
                        Kind::Regular => &mut comments.comments,
                    }
                    .push(comment);
                    buffer.push('\n');
                }
                _ => buffer.push(' '),
            },
        }
    }

    if let Mode::Comment(kind, start) = mode {
        let content_start = match &kind {
            Kind::Doc => start + 3,
            Kind::Regular => start + 2,
        };
        let comment = Comment {
            start,
            content: &src[content_start..],
        };
        match &kind {
            Kind::Doc => &mut comments.doc_comments,
            Kind::Regular => &mut comments.comments,
        }
        .push(comment);
    };

    (buffer, comments)
}

#[test]
fn strip_extra_test() {
    macro_rules! assert_stripped {
        ($input:expr, $out:expr $(,)?) => {
            println!("\n\n{:?}", $input);
            assert_eq!($out, strip_extra($input).0.as_str());
        };
        ($input:expr, $out:expr, $comments:expr $(,)?) => {
            println!("\n\n{:?}", $input);
            let (stripped, comments) = strip_extra($input);
            assert_eq!($out, stripped.as_str());
            assert_eq!($comments, comments);
        };
    };

    assert_stripped!("", "");
    assert_stripped!(" ; ", "   ");
    assert_stripped!("//\n", "  \n");
    assert_stripped!("// \n", "   \n");
    assert_stripped!(" // hi\n ", "      \n ");
    assert_stripped!(r#""\"//" hi"#, r#""\"//" hi"#);
    assert_stripped!("/// Something\n", "             \n");
    assert_stripped!(" /// Something\n", "              \n");
    assert_stripped!(
        "/// Something
/// Something else

/// This is a function
pub fn() -> {}
",
        "             \n                  \n
                      \npub fn() -> {}\n"
    );

    // https://github.com/gleam-lang/gleam/issues/449
    assert_stripped!("//\nexternal type A\n", "  \nexternal type A\n",);
    assert_stripped!(
        r#"//
pub external fn a() -> Nil =
"1" "2"
"#,
        "  \npub external fn a() -> Nil =
\"1\" \"2\"
",
    );

    // Testing comment collection

    assert_stripped!(
        "// hello\n",
        "        \n",
        ModuleComments {
            doc_comments: vec![],
            comments: vec![Comment {
                start: 0,
                content: " hello",
            }],
        }
    );

    assert_stripped!(
        "// hello",
        "        ",
        ModuleComments {
            doc_comments: vec![],
            comments: vec![Comment {
                start: 0,
                content: " hello",
            }],
        }
    );

    // Testing doc comment collection

    assert_stripped!(
        "/// hello\n",
        "         \n",
        ModuleComments {
            doc_comments: vec![Comment {
                start: 0,
                content: " hello",
            }],
            comments: vec![],
        }
    );

    assert_stripped!(
        "/// hello",
        "         ",
        ModuleComments {
            doc_comments: vec![Comment {
                start: 0,
                content: " hello",
            }],
            comments: vec![],
        }
    );
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
