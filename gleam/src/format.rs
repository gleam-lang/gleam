//!  This module implements the functionality described in
//!  ["Strictly Pretty" (2000) by Christian Lindig][0], with a few
//!  extensions.
//!
//!  [0]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200

#![allow(dead_code)]

use im::vector::Vector;

#[derive(Debug, Clone)]
pub enum Document {
    /// Returns a document entity used to represent nothingness
    Nil,

    /// A mandatory linebreak
    Line,

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

fn fits(limit: isize, mut docs: Vector<(isize, Mode, Document)>) -> bool {
    if limit < 0 {
        return false;
    };

    let (indent, mode, document) = match docs.pop_front() {
        Some(x) => x,
        None => return true,
    };

    match document {
        Document::Line => true,
        Document::ForceBreak => false,
        Document::Nil => fits(limit, docs),
        Document::Break { broken, unbroken } => match mode {
            Mode::Broken => fits(limit - broken.len() as isize, docs),
            Mode::Unbroken => fits(limit - unbroken.len() as isize, docs),
        },
        Document::Cons(left, right) => {
            docs.push_front((indent, mode.clone(), *left));
            docs.push_front((indent, mode, *right));
            fits(limit, docs)
        }
        Document::Nest(i, doc) => {
            docs.push_front((i + indent, mode, *doc));
            fits(limit, docs)
        }
        Document::NestCurrent(doc) => {
            docs.push_front((indent, mode, *doc));
            fits(limit, docs)
        }
        Document::Group(doc) => {
            docs.push_front((indent, Mode::Unbroken, *doc));
            fits(limit, docs)
        }
        Document::Text(s) => fits(limit - s.len() as isize, docs),
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

    // Break in Broken fits if `broken` fits
    assert!(fits(
        2,
        vector![(
            0,
            Broken,
            Break {
                broken: "12".to_string(),
                unbroken: "".to_string()
            }
        )]
    ));
    assert!(!fits(
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
    assert!(fits(0, vector![(0, Broken, Line)]));
    assert!(fits(0, vector![(0, Unbroken, Line)]));

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

pub fn format(limit: isize, doc: Document) -> String {
    let mut buffer = String::new();
    fmt(
        &mut buffer,
        limit,
        0,
        vector![(0, Mode::Broken, Document::Group(Box::new(doc)))],
    );
    buffer
}

fn fmt(b: &mut String, limit: isize, width: isize, mut docs: Vector<(isize, Mode, Document)>) {
    let (indent, mode, document) = match docs.pop_front() {
        Some(x) => x,
        None => return,
    };

    match document {
        Document::Nil => fmt(b, limit, width, docs),

        Document::ForceBreak => fmt(b, limit, width, docs),

        Document::Line => {
            b.push_str("\n");
            b.push_str(" ".repeat(indent as usize).as_str());
            fmt(b, limit, indent, docs);
        }

        Document::Break { broken, unbroken } => {
            let x = match mode {
                Mode::Unbroken => unbroken,
                Mode::Broken => broken,
            };
            let new_width = width + x.len() as isize;
            b.push_str(x.as_str());
            fmt(b, limit, new_width, docs);
        }

        Document::Text(s) => {
            let new_width = width + s.len() as isize;
            b.push_str(s.as_str());
            fmt(b, limit, new_width, docs);
        }

        Document::Cons(left, right) => {
            docs.push_front((indent, mode.clone(), *right));
            docs.push_front((indent, mode, *left));
            fmt(b, limit, width, docs);
        }

        Document::Nest(i, doc) => {
            docs.push_front((indent + i, mode, *doc));
            fmt(b, limit, width, docs);
        }

        Document::NestCurrent(doc) => {
            docs.push_front((width, mode, *doc));
            fmt(b, limit, width, docs);
        }

        Document::Group(doc) => {
            let mut flat_docs = docs.clone();
            flat_docs.push_front((indent, Mode::Unbroken, (*doc).clone()));
            if fits(limit - width, flat_docs.clone()) {
                fmt(b, limit, width, flat_docs);
            } else {
                docs.push_front((indent, Mode::Broken, (*doc).clone()));
                fmt(b, limit, width, docs);
            }
        }
    }
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
    assert_eq!("broken".to_string(), format(5, doc));

    let doc = Nest(
        2,
        Box::new(Cons(
            Box::new(Text("1".to_string())),
            Box::new(Cons(Box::new(Line), Box::new(Text("2".to_string())))),
        )),
    );
    assert_eq!("1\n  2".to_string(), format(1, doc));

    let doc = Cons(
        Box::new(Text("111".to_string())),
        Box::new(NestCurrent(Box::new(Cons(
            Box::new(Line),
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
    assert_eq!("broken".to_string(), format(100, doc));
}
