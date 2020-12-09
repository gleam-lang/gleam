use crate::ast::*;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug, PartialEq)]
pub enum Error {
    TooManyHolesInCapture { location: SrcSpan, count: usize },
}

pub type LalrpopError = lalrpop_util::ParseError<usize, (usize, String), Error>;

#[derive(Debug, PartialEq)]
pub struct ModuleComments<'a> {
    pub module_comments: Vec<&'a str>,
    pub doc_comments: Vec<Comment<'a>>,
    pub comments: Vec<Comment<'a>>,
    pub empty_lines: Vec<usize>,
}

#[derive(Debug, PartialEq)]
pub struct Comment<'a> {
    pub start: usize,
    pub content: &'a str,
}

pub fn take_before<'a>(
    comments: &'a [Comment<'a>],
    limit: usize,
) -> (impl Iterator<Item = &'a str>, &'a [Comment<'a>]) {
    let mut end = 0;
    for (i, comment) in comments.iter().enumerate() {
        if comment.start > limit {
            break;
        }
        end = i + 1;
    }

    let popped = comments[0..end].iter().map(|c| c.content);
    (popped, &comments[end..])
}

/// Blanks out comments, semicolons, etc
///
pub fn strip_extra(src: &str) -> (String, ModuleComments<'_>) {
    #[derive(Clone, Copy)]
    enum Kind {
        Module,
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
    let mut chars = UnicodeSegmentation::grapheme_indices(src, true).peekable();
    let mut comments = ModuleComments {
        module_comments: vec![],
        doc_comments: vec![],
        comments: vec![],
        empty_lines: vec![],
    };

    while let Some((outer_char_no, c)) = chars.next() {
        match mode {
            Mode::Normal => match c {
                ";" => buffer.push(' '),

                "\"" => {
                    mode = Mode::String;
                    buffer.push_str(c);
                }

                "/" => match chars.next() {
                    Some((_, "/")) => {
                        buffer.push(' ');
                        buffer.push(' ');
                        match chars.peek() {
                            Some((_, "/")) => {
                                mode = Mode::Comment(Kind::Doc, outer_char_no);
                                buffer.push(' ');
                                chars.next();
                                if let Some((_, "/")) = chars.peek() {
                                    mode = Mode::Comment(Kind::Module, outer_char_no);
                                    buffer.push(' ');
                                    chars.next();
                                }
                            }
                            Some((_, _c2)) => {
                                mode = Mode::Comment(Kind::Regular, outer_char_no);
                            }
                            None => {
                                buffer.push_str(c);
                                chars.next();
                            }
                        }
                    }
                    Some((_, c2)) => {
                        buffer.push_str(c);
                        buffer.push_str(c2);
                    }
                    None => buffer.push_str(c),
                },

                "\r\n" | "\n" => {
                    buffer.push_str(c);
                    chomp_newlines(&mut buffer, outer_char_no, &mut comments, &mut chars);
                }

                _ => buffer.push_str(c),
            },

            Mode::String => match c {
                "\\" => {
                    buffer.push_str(c);
                    if let Some((_, c)) = chars.next() {
                        buffer.push_str(c)
                    }
                }

                "\"" => {
                    mode = Mode::Normal;
                    buffer.push_str(c);
                }

                _ => buffer.push_str(c),
            },

            Mode::Comment(kind, start) => match c {
                "\r\n" | "\n" => {
                    mode = Mode::Normal;
                    let content_start = match &kind {
                        Kind::Module => start + 4,
                        Kind::Doc => start + 3,
                        Kind::Regular => start + 2,
                    };
                    let comment = Comment {
                        start,
                        content: &src[content_start..outer_char_no],
                    };
                    match &kind {
                        Kind::Module => comments.module_comments.push(comment.content),
                        Kind::Doc => comments.doc_comments.push(comment),
                        Kind::Regular => comments.comments.push(comment),
                    };
                    buffer.push_str(c);

                    chomp_newlines(&mut buffer, start, &mut comments, &mut chars)
                }
                grapheme => {
                    for _ in 0..grapheme.len() {
                        buffer.push(' ');
                    }
                }
            },
        }
    }

    if let Mode::Comment(kind, start) = mode {
        let content_start = match &kind {
            Kind::Module => start + 4,
            Kind::Doc => start + 3,
            Kind::Regular => start + 2,
        };
        let comment = Comment {
            start,
            content: &src[content_start..],
        };
        match &kind {
            Kind::Module => comments.module_comments.push(comment.content),
            Kind::Doc => comments.doc_comments.push(comment),
            Kind::Regular => comments.comments.push(comment),
        };
    };

    (buffer, comments)
}

fn chomp_newlines(
    buffer: &mut String,
    position: usize,
    comments: &mut ModuleComments<'_>,
    chars: &mut std::iter::Peekable<unicode_segmentation::GraphemeIndices<'_>>,
) {
    let mut found_new_line = false;
    // Consume all spaces and new lines - mark this line as empty if we find at least one new line
    // This considers a line to be empty if it only has spaces
    while let Some((_, grapheme @ "\r\n"))
    | Some((_, grapheme @ "\n"))
    | Some((_, grapheme @ " ")) = chars.peek()
    {
        if let "\n" | "\r\n" = *grapheme {
            found_new_line = true
        }

        buffer.push_str(grapheme);
        chars.next();
    }

    if found_new_line {
        comments.empty_lines.push(position + 1);
    }
}

#[cfg(test)]
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
}

#[test]
fn strip_extra_test() {
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
        "// 👨‍👩‍👧‍👧 unicode\n",
        "                                    \n",
        ModuleComments {
            module_comments: vec![],
            doc_comments: vec![],
            comments: vec![Comment {
                start: 0,
                content: " 👨‍👩‍👧‍👧 unicode",
            }],
            empty_lines: vec![],
        }
    );

    assert_stripped!(
        "// hello\n",
        "        \n",
        ModuleComments {
            module_comments: vec![],
            doc_comments: vec![],
            comments: vec![Comment {
                start: 0,
                content: " hello",
            }],
            empty_lines: vec![],
        }
    );

    assert_stripped!(
        "// hello",
        "        ",
        ModuleComments {
            module_comments: vec![],
            doc_comments: vec![],
            comments: vec![Comment {
                start: 0,
                content: " hello",
            }],
            empty_lines: vec![],
        }
    );

    // Testing doc comment collection

    assert_stripped!(
        "/// hello\n",
        "         \n",
        ModuleComments {
            module_comments: vec![],
            doc_comments: vec![Comment {
                start: 0,
                content: " hello",
            }],
            empty_lines: vec![],
            comments: vec![],
        }
    );

    assert_stripped!(
        "/// hello",
        "         ",
        ModuleComments {
            module_comments: vec![],
            doc_comments: vec![Comment {
                start: 0,
                content: " hello",
            }],
            comments: vec![],
            empty_lines: vec![],
        }
    );

    assert_stripped!(
        "/// one
///two
fn main() {
  Nil
}
",
        "       \n      \nfn main() {
  Nil
}
",
        ModuleComments {
            module_comments: vec![],
            doc_comments: vec![
                Comment {
                    start: 0,
                    content: " one",
                },
                Comment {
                    start: 8,
                    content: "two",
                }
            ],
            comments: vec![],
            empty_lines: vec![],
        }
    );

    assert_stripped!(
        "

fn main() {
  //

  ///

  Nil

  Nil



}
",
        "

fn main() {
    \n
     \n
  Nil

  Nil



}
",
        ModuleComments {
            module_comments: vec![],
            doc_comments: vec![Comment {
                start: 22,
                content: ""
            }],
            comments: vec![Comment {
                start: 16,
                content: ""
            }],
            empty_lines: vec![1, 17, 23, 33, 40],
        }
    );

    assert_stripped!(
        "//// This module rocks!
//// Yes it does

// ok

fn main() {
  1
}
//// OK?
",
        "                       \n                \n
     \n
fn main() {
  1
}
        \n",
        ModuleComments {
            module_comments: vec![" This module rocks!", " Yes it does", " OK?"],
            doc_comments: vec![],
            comments: vec![Comment {
                start: 42,
                content: " ok",
            }],
            empty_lines: vec![25, 43],
        }
    );
}

#[test]
fn strip_extra_windows_line_endings_test() {
    assert_stripped!("// hi\r\n1", "     \r\n1");
}

pub fn location(start: usize, end: usize) -> SrcSpan {
    SrcSpan { start, end }
}

pub fn attach_doc_comments<'a, A, B, C, D>(
    module: &mut Module<A, B, C, D>,
    mut comments: &'a [Comment<'a>],
) {
    for statement in &mut module.statements {
        let location = statement.location();
        let (doc, rest) = take_before(comments, location.start);
        comments = rest;
        statement.put_doc(doc);

        if let crate::ast::Statement::CustomType { constructors, .. } = statement {
            for constructor in constructors {
                let (doc, rest) = take_before(comments, constructor.location.start);
                comments = rest;
                constructor.put_doc(doc);
            }
        }
    }
}

pub enum ParserArg {
    Arg(CallArg<UntypedExpr>),
    Hole {
        location: SrcSpan,
        label: Option<String>,
    },
}

pub fn make_call(
    fun: UntypedExpr,
    args: Vec<ParserArg>,
    s: usize,
    e: usize,
) -> Result<UntypedExpr, Error> {
    let mut num_holes = 0;
    let args = args
        .into_iter()
        .map(|a| match a {
            ParserArg::Arg(arg) => arg,
            ParserArg::Hole { location, label } => {
                num_holes += 1;
                CallArg {
                    label,
                    location,
                    value: UntypedExpr::Var {
                        location,
                        name: CAPTURE_VARIABLE.to_string(),
                    },
                }
            }
        })
        .collect();
    let call = UntypedExpr::Call {
        location: location(s, e),
        fun: Box::new(fun),
        args,
    };
    match num_holes {
        // A normal call
        0 => Ok(call),

        // An anon function using the capture syntax run(_, 1, 2)
        1 => Ok(UntypedExpr::Fn {
            location: call.location(),
            is_capture: true,
            args: vec![Arg {
                location: location(0, 0),
                annotation: None,
                names: ArgNames::Named {
                    name: CAPTURE_VARIABLE.to_string(),
                },
                typ: (),
            }],
            body: Box::new(call),
            return_annotation: None,
        }),

        count => Err(Error::TooManyHolesInCapture {
            location: call.location(),
            count,
        }),
    }
}

pub fn is_gleam_reserved_word(s: &str) -> bool {
    match s {
        "pub" | "fn" | "import" | "as" | "type" | "external" | "case" | "let" | "try"
        | "assert" => true,
        _ => false,
    }
}
