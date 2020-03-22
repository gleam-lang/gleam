//!  This module implements the functionality described in
//!  ["Strictly Pretty" (2000) by Christian Lindig][0], with a few
//!  extensions.
//!
//!  [0]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200

use im::vector::Vector;

pub trait Documentable {
    fn to_doc(self) -> Document;
}

impl Documentable for &str {
    fn to_doc(self) -> Document {
        Document::Text(self.to_string())
    }
}

impl Documentable for String {
    fn to_doc(self) -> Document {
        Document::Text(self)
    }
}

impl Documentable for isize {
    fn to_doc(self) -> Document {
        Document::Text(format!("{}", self))
    }
}

impl Documentable for i64 {
    fn to_doc(self) -> Document {
        Document::Text(format!("{}", self))
    }
}

impl Documentable for usize {
    fn to_doc(self) -> Document {
        Document::Text(format!("{}", self))
    }
}

impl Documentable for f64 {
    fn to_doc(self) -> Document {
        Document::Text(format!("{:?}", self))
    }
}

impl Documentable for u64 {
    fn to_doc(self) -> Document {
        Document::Text(format!("{:?}", self))
    }
}

impl Documentable for Document {
    fn to_doc(self) -> Document {
        self
    }
}

impl Documentable for Vec<Document> {
    fn to_doc(self) -> Document {
        self.into_iter().rev().fold(Document::Nil, |acc, doc| {
            Document::Cons(Box::new(doc), Box::new(acc))
        })
    }
}

pub fn concat(docs: impl Iterator<Item = Document>) -> Document {
    docs.fold(Document::Nil, |acc, doc| {
        Document::Cons(Box::new(acc), Box::new(doc))
    })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Document {
    /// Returns a document entity used to represent nothingness
    Nil,

    /// A mandatory linebreak
    Line(usize),

    /// Forces contained groups to break
    ForceBreak,

    /// Renders `broken` if group is broken, `unbroken` otherwise
    Break { broken: String, unbroken: String },

    /// Join 2 documents together
    Cons(Box<Document>, Box<Document>),

    /// Nests the given document by the given indent
    Nest(isize, Box<Document>),

    /// Nests the given document to the current cursor position
    NestCurrent(Box<Document>),

    /// Nests the given document to the current cursor position
    Group(Box<Document>),

    /// A string to render
    Text(String),
}

#[derive(Debug, Clone)]
enum Mode {
    Broken,
    Unbroken,
}

fn fits(mut limit: isize, mut docs: Vector<(isize, Mode, Document)>) -> bool {
    loop {
        if limit < 0 {
            return false;
        };

        let (indent, mode, document) = match docs.pop_front() {
            Some(x) => x,
            None => return true,
        };

        match document {
            Document::Nil => (),

            Document::Line(_) => return true,

            Document::ForceBreak => return false,

            Document::Nest(i, doc) => docs.push_front((i + indent, mode, *doc)),

            Document::NestCurrent(doc) => docs.push_front((indent, mode, *doc)),

            Document::Group(doc) => docs.push_front((indent, Mode::Unbroken, *doc)),

            Document::Text(s) => limit -= s.len() as isize,

            Document::Break { unbroken, .. } => match mode {
                Mode::Broken => return true,
                Mode::Unbroken => limit -= unbroken.len() as isize,
            },

            Document::Cons(left, right) => {
                docs.push_front((indent, mode.clone(), *right));
                docs.push_front((indent, mode, *left));
            }
        }
    }
}

pub fn format(limit: isize, doc: Document) -> String {
    let mut buffer = String::new();
    fmt(
        &mut buffer,
        limit,
        0,
        vector![(0, Mode::Unbroken, Document::Group(Box::new(doc)))],
    );
    buffer
}

fn fmt(b: &mut String, limit: isize, mut width: isize, mut docs: Vector<(isize, Mode, Document)>) {
    while let Some((indent, mode, document)) = docs.pop_front() {
        match document {
            Document::Nil | Document::ForceBreak => (),

            Document::Line(i) => {
                for _ in 0..i {
                    b.push_str("\n");
                }
                b.push_str(" ".repeat(indent as usize).as_str());
                width = indent;
            }

            Document::Break { broken, unbroken } => {
                width = match mode {
                    Mode::Unbroken => {
                        b.push_str(unbroken.as_str());
                        width + unbroken.len() as isize
                    }
                    Mode::Broken => {
                        b.push_str(broken.as_str());
                        b.push_str("\n");
                        b.push_str(" ".repeat(indent as usize).as_str());
                        indent as isize
                    }
                };
            }

            Document::Text(s) => {
                width += s.len() as isize;
                b.push_str(s.as_str());
            }

            Document::Cons(left, right) => {
                docs.push_front((indent, mode.clone(), *right));
                docs.push_front((indent, mode, *left));
            }

            Document::Nest(i, doc) => {
                docs.push_front((indent + i, mode, *doc));
            }

            Document::NestCurrent(doc) => {
                docs.push_front((width, mode, *doc));
            }

            Document::Group(doc) => {
                docs.push_front((indent, Mode::Unbroken, (*doc).clone()));
                if !fits(limit - width, docs.clone()) {
                    docs[0] = (indent, Mode::Broken, (*doc).clone());
                }
            }
        }
    }
}

#[test]
fn fits_test() {
    use self::Document::*;
    use self::Mode::*;

    // Negative limits never fit
    assert!(!fits(-1, vector![]));

    // If no more documents it always fits
    assert!(fits(0, vector![]));

    // ForceBreak never fits
    assert!(!fits(100, vector![(0, Unbroken, ForceBreak)]));
    assert!(!fits(100, vector![(0, Broken, ForceBreak)]));

    // Break in Broken fits always
    assert!(fits(
        1,
        vector![(
            0,
            Broken,
            Break {
                broken: "12".to_string(),
                unbroken: "".to_string()
            }
        )]
    ));

    // Break in Unbroken mode fits if `unbroken` fits
    assert!(fits(
        3,
        vector![(
            0,
            Unbroken,
            Break {
                broken: "".to_string(),
                unbroken: "123".to_string()
            }
        )]
    ));
    assert!(!fits(
        2,
        vector![(
            0,
            Unbroken,
            Break {
                broken: "".to_string(),
                unbroken: "123".to_string()
            }
        )]
    ));

    // Line always fits
    assert!(fits(0, vector![(0, Broken, Line(100))]));
    assert!(fits(0, vector![(0, Unbroken, Line(100))]));

    // String fits if smaller than limit
    assert!(fits(5, vector![(0, Broken, Text("Hello".to_string()))]));
    assert!(fits(5, vector![(0, Unbroken, Text("Hello".to_string()))]));
    assert!(!fits(4, vector![(0, Broken, Text("Hello".to_string()))]));
    assert!(!fits(4, vector![(0, Unbroken, Text("Hello".to_string()))]));

    // Cons fits if combined smaller than limit
    assert!(fits(
        2,
        vector![(
            0,
            Broken,
            Cons(
                Box::new(Text("1".to_string())),
                Box::new(Text("2".to_string()))
            )
        )]
    ));
    assert!(fits(
        2,
        vector![(
            0,
            Unbroken,
            Cons(
                Box::new(Text("1".to_string())),
                Box::new(Text("2".to_string()))
            )
        )]
    ));
    assert!(!fits(
        1,
        vector![(
            0,
            Broken,
            Cons(
                Box::new(Text("1".to_string())),
                Box::new(Text("2".to_string()))
            )
        )]
    ));
    assert!(!fits(
        1,
        vector![(
            0,
            Unbroken,
            Cons(
                Box::new(Text("1".to_string())),
                Box::new(Text("2".to_string()))
            )
        )]
    ));

    // Nest fits if combined smaller than limit
    assert!(fits(
        2,
        vector![(0, Broken, Nest(1, Box::new(Text("12".to_string())),))]
    ));
    assert!(fits(
        2,
        vector![(0, Unbroken, Nest(1, Box::new(Text("12".to_string())),))]
    ));
    assert!(!fits(
        1,
        vector![(0, Broken, Nest(1, Box::new(Text("12".to_string())),))]
    ));
    assert!(!fits(
        1,
        vector![(0, Unbroken, Nest(1, Box::new(Text("12".to_string()))))]
    ));

    // Nest fits if combined smaller than limit
    assert!(fits(
        2,
        vector![(0, Broken, NestCurrent(Box::new(Text("12".to_string())),))]
    ));
    assert!(fits(
        2,
        vector![(0, Unbroken, NestCurrent(Box::new(Text("12".to_string())),))]
    ));
    assert!(!fits(
        1,
        vector![(0, Broken, NestCurrent(Box::new(Text("12".to_string())),))]
    ));
    assert!(!fits(
        1,
        vector![(0, Unbroken, NestCurrent(Box::new(Text("12".to_string()))))]
    ));
}

#[test]
fn format_test() {
    use self::Document::*;

    let doc = Text("Hi".to_string());
    assert_eq!("Hi".to_string(), format(10, doc));

    let doc = Cons(
        Box::new(Text("Hi".to_string())),
        Box::new(Text(", world!".to_string())),
    );
    assert_eq!("Hi, world!".to_string(), format(10, doc));

    let doc = Nil;
    assert_eq!("".to_string(), format(10, doc));

    let doc = Break {
        broken: "broken".to_string(),
        unbroken: "unbroken".to_string(),
    };
    assert_eq!("unbroken".to_string(), format(10, doc));

    let doc = Break {
        broken: "broken".to_string(),
        unbroken: "unbroken".to_string(),
    };
    assert_eq!("broken\n".to_string(), format(5, doc));

    let doc = Nest(
        2,
        Box::new(Cons(
            Box::new(Text("1".to_string())),
            Box::new(Cons(Box::new(Line(1)), Box::new(Text("2".to_string())))),
        )),
    );
    assert_eq!("1\n  2".to_string(), format(1, doc));

    let doc = Cons(
        Box::new(Text("111".to_string())),
        Box::new(NestCurrent(Box::new(Cons(
            Box::new(Line(1)),
            Box::new(Text("2".to_string())),
        )))),
    );
    assert_eq!("111\n   2".to_string(), format(1, doc));

    let doc = Cons(
        Box::new(ForceBreak),
        Box::new(Break {
            broken: "broken".to_string(),
            unbroken: "unbroken".to_string(),
        }),
    );
    assert_eq!("broken\n".to_string(), format(100, doc));
}

pub fn nil() -> Document {
    Document::Nil
}

pub fn line() -> Document {
    Document::Line(1)
}

pub fn lines(i: usize) -> Document {
    Document::Line(i)
}

pub fn force_break() -> Document {
    Document::ForceBreak
}

pub fn break_(broken: &str, unbroken: &str) -> Document {
    Document::Break {
        broken: broken.to_string(),
        unbroken: unbroken.to_string(),
    }
}

pub fn delim(d: &str) -> Document {
    Document::Break {
        broken: d.to_string(),
        unbroken: format!("{} ", d),
    }
}

impl Document {
    pub fn group(self) -> Document {
        Document::Group(Box::new(self))
    }

    pub fn nest(self, indent: isize) -> Document {
        Document::Nest(indent, Box::new(self))
    }

    pub fn nest_current(self) -> Document {
        Document::NestCurrent(Box::new(self))
    }

    pub fn append(self, x: impl Documentable) -> Document {
        Document::Cons(Box::new(self), Box::new(x.to_doc()))
    }

    pub fn format(self, limit: isize) -> String {
        format(limit, self)
    }

    pub fn surround(self, open: impl Documentable, closed: impl Documentable) -> Document {
        open.to_doc().append(self).append(closed)
    }
}
