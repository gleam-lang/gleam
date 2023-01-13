use super::Document::*;
use super::Mode::*;
use super::*;

use im::vector;
use pretty_assertions::assert_eq;

#[test]
fn fits_test() {
    // Negative limits never fit
    assert!(!fits(-1, 0, vector![]));

    // If no more documents it always fits
    assert!(fits(0, 0, vector![]));

    // ForceBreak never fits
    let doc = ForceBroken(Box::new(nil()));
    assert!(!fits(100, 0, vector![(0, Unbroken, &doc)]));
    let doc = ForceBroken(Box::new(nil()));
    assert!(!fits(100, 0, vector![(0, Broken, &doc)]));

    // Break in Broken fits always
    assert!(fits(
        1,
        0,
        vector![(
            0,
            Broken,
            &Break {
                broken: "12",
                unbroken: "",
                kind: BreakKind::Strict,
            }
        )]
    ));

    // Break in Unbroken mode fits if `unbroken` fits
    assert!(fits(
        3,
        0,
        vector![(
            0,
            Unbroken,
            &Break {
                broken: "",
                unbroken: "123",
                kind: BreakKind::Strict,
            }
        )]
    ));
    assert!(!fits(
        2,
        0,
        vector![(
            0,
            Unbroken,
            &Break {
                broken: "",
                unbroken: "123",
                kind: BreakKind::Strict,
            }
        )]
    ));

    // Line always fits
    assert!(fits(0, 0, vector![(0, Broken, &Line(100))]));
    assert!(fits(0, 0, vector![(0, Unbroken, &Line(100))]));

    // String fits if smaller than limit
    let doc = String("Hello".into());
    assert!(fits(5, 0, vector![(0, Broken, &doc)]));
    let doc = String("Hello".into());
    assert!(fits(5, 0, vector![(0, Unbroken, &doc)]));
    let doc = String("Hello".into());
    assert!(!fits(4, 0, vector![(0, Broken, &doc)]));
    let doc = String("Hello".into());
    assert!(!fits(4, 0, vector![(0, Unbroken, &doc)]));

    // Cons fits if combined smaller than limit
    let doc = String("1".into()).append(String("2".into()));
    assert!(fits(2, 0, vector![(0, Broken, &doc)]));
    let doc = String("1".into()).append(String("2".into()));
    assert!(fits(2, 0, vector![(0, Unbroken, &doc,)]));
    let doc = String("1".into()).append(String("2".into()));
    assert!(!fits(1, 0, vector![(0, Broken, &doc)]));
    let doc = String("1".into()).append(String("2".into()));
    assert!(!fits(1, 0, vector![(0, Unbroken, &doc)]));

    // Nest fits if combined smaller than limit
    let doc = Nest(1, Box::new(String("12".into())));
    assert!(fits(2, 0, vector![(0, Broken, &doc)]));
    assert!(fits(2, 0, vector![(0, Unbroken, &doc)]));
    assert!(!fits(1, 0, vector![(0, Broken, &doc)]));
    assert!(!fits(1, 0, vector![(0, Unbroken, &doc)]));

    // Nest fits if combined smaller than limit
    let doc = Nest(0, Box::new(String("12".into())));
    assert!(fits(2, 0, vector![(0, Broken, &doc)]));
    assert!(fits(2, 0, vector![(0, Unbroken, &doc)]));
    assert!(!fits(1, 0, vector![(0, Broken, &doc)]));
    assert!(!fits(1, 0, vector![(0, Unbroken, &doc)]));
}

#[test]
fn format_test() {
    let doc = String("Hi".into());
    assert_eq!("Hi", doc.to_pretty_string(10));

    let doc = String("Hi".into()).append(String(", world!".into()));
    assert_eq!("Hi, world!", doc.clone().to_pretty_string(10));

    let doc = &Break {
        broken: "broken",
        unbroken: "unbroken",
        kind: BreakKind::Strict,
    }
    .group();
    assert_eq!("unbroken", doc.clone().to_pretty_string(10));

    let doc = &Break {
        broken: "broken",
        unbroken: "unbroken",
        kind: BreakKind::Strict,
    }
    .group();
    assert_eq!("broken\n".into(), doc.clone().to_pretty_string(5));

    let doc = Nest(
        2,
        Box::new(String("1".into()).append(Line(1).append(String("2".into())))),
    );
    assert_eq!("1\n  2".into(), doc.to_pretty_string(1));

    let doc = ForceBroken(Box::new(Break {
        broken: "broken",
        unbroken: "unbroken",
        kind: BreakKind::Strict,
    }));
    assert_eq!("broken\n".to_string(), doc.to_pretty_string(100));

    let doc = ForceBroken(Box::new(Break {
        broken: "broken",
        unbroken: "unbroken",
        kind: BreakKind::Flex,
    }));
    assert_eq!("unbroken".to_string(), doc.to_pretty_string(100));
}

#[test]
fn let_left_side_fits_test() {
    let elems = break_("", "").append("1").nest(2).append(break_("", ""));
    let list = "[".to_doc().append(elems).append("]").group();
    let doc = list.clone().append(" = ").append(list);

    assert_eq!(
        "[1] = [
  1
]",
        doc.clone().to_pretty_string(7)
    );

    assert_eq!(
        "[
  1
] = [
  1
]",
        doc.clone().to_pretty_string(2)
    );

    assert_eq!("[1] = [1]", doc.clone().to_pretty_string(16));
}

#[test]
fn empty_documents() {
    // nil
    assert!(nil().is_empty());

    // lines
    assert!(lines(0).is_empty());
    assert!(!line().is_empty());

    // force break
    assert!(nil().force_break().is_empty());
    assert!(!"ok".to_doc().force_break().is_empty());

    // strings
    assert!("".to_doc().is_empty());
    assert!(!"foo".to_doc().is_empty());
    assert!(!" ".to_doc().is_empty());
    assert!(!"\n".to_doc().is_empty());

    // containers
    assert!("".to_doc().nest(2).is_empty());
    assert!(!"foo".to_doc().nest(2).is_empty());
    assert!("".to_doc().group().is_empty());
    assert!(!"foo".to_doc().group().is_empty());
    assert!(break_("", "").is_empty());
    assert!(!break_("foo", "foo").is_empty());
    assert!(!break_("foo\nbar", "foo bar").is_empty());
    assert!("".to_doc().append("".to_doc()).is_empty());
    assert!(!"foo".to_doc().append("".to_doc()).is_empty());
    assert!(!"".to_doc().append("foo".to_doc()).is_empty());
}
