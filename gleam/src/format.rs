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
    String(String),
}

#[derive(Debug, Clone)]
enum Mode {
    Broken,
    Unbroken,
}

fn fits(limit: isize, mut docs: Vector<(Mode, Document)>) -> bool {
    if limit < 0 {
        return false;
    };

    let (mode, document) = match docs.pop_front() {
        Some(pair) => pair,
        None => return true,
    };

    match document {
        Document::Line => true,
        Document::Nil => fits(limit, docs),
        Document::ForceBreak => false,
        Document::Break { broken, unbroken } => match mode {
            Mode::Broken => fits(limit - broken.len() as isize, docs),
            Mode::Unbroken => fits(limit - unbroken.len() as isize, docs),
        },
        Document::Cons(left, right) => {
            docs.push_front((mode.clone(), *left));
            docs.push_front((mode, *right));
            fits(limit, docs)
        }
        Document::Nest(_, contents) => {
            docs.push_front((mode, *contents));
            fits(limit, docs)
        }
        Document::NestCurrent(contents) => {
            docs.push_front((mode, *contents));
            fits(limit, docs)
        }
        Document::Group(contents) => {
            docs.push_front((Mode::Unbroken, *contents));
            fits(limit, docs)
        }
        Document::String(contents) => fits(limit - contents.len() as isize, docs),
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
    assert!(!fits(100, vector![(Unbroken, ForceBreak)]));
    assert!(!fits(100, vector![(Broken, ForceBreak)]));

    // Break in Broken fits if `broken` fits
    assert!(fits(
        2,
        vector![(
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
            Unbroken,
            Break {
                broken: "".to_string(),
                unbroken: "123".to_string()
            }
        )]
    ));

    // Line always fits
    assert!(fits(0, vector![(Broken, Line)]));
    assert!(fits(0, vector![(Unbroken, Line)]));

    // String fits if smaller than limit
    assert!(fits(5, vector![(Broken, String("Hello".to_string()))]));
    assert!(fits(5, vector![(Unbroken, String("Hello".to_string()))]));
    assert!(!fits(4, vector![(Broken, String("Hello".to_string()))]));
    assert!(!fits(4, vector![(Unbroken, String("Hello".to_string()))]));

    // Cons fits if combined smaller than limit
    assert!(fits(
        2,
        vector![(
            Broken,
            Cons(
                Box::new(String("1".to_string())),
                Box::new(String("2".to_string()))
            )
        )]
    ));
    assert!(fits(
        2,
        vector![(
            Unbroken,
            Cons(
                Box::new(String("1".to_string())),
                Box::new(String("2".to_string()))
            )
        )]
    ));
    assert!(!fits(
        1,
        vector![(
            Broken,
            Cons(
                Box::new(String("1".to_string())),
                Box::new(String("2".to_string()))
            )
        )]
    ));
    assert!(!fits(
        1,
        vector![(
            Unbroken,
            Cons(
                Box::new(String("1".to_string())),
                Box::new(String("2".to_string()))
            )
        )]
    ));

    // Nest fits if combined smaller than limit
    assert!(fits(
        2,
        vector![(Broken, Nest(1, Box::new(String("12".to_string())),))]
    ));
    assert!(fits(
        2,
        vector![(Unbroken, Nest(1, Box::new(String("12".to_string())),))]
    ));
    assert!(!fits(
        1,
        vector![(Broken, Nest(1, Box::new(String("12".to_string())),))]
    ));
    assert!(!fits(
        1,
        vector![(Unbroken, Nest(1, Box::new(String("12".to_string()))))]
    ));

    // Nest fits if combined smaller than limit
    assert!(fits(
        2,
        vector![(Broken, NestCurrent(Box::new(String("12".to_string())),))]
    ));
    assert!(fits(
        2,
        vector![(Unbroken, NestCurrent(Box::new(String("12".to_string())),))]
    ));
    assert!(!fits(
        1,
        vector![(Broken, NestCurrent(Box::new(String("12".to_string())),))]
    ));
    assert!(!fits(
        1,
        vector![(Unbroken, NestCurrent(Box::new(String("12".to_string()))))]
    ));
}
