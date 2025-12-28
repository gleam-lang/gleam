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
    let doc = Document::str("Hello");
    assert!(fits(5, 0, vector![(0, Broken, &doc)]));
    let doc = Document::str("Hello");
    assert!(fits(5, 0, vector![(0, Unbroken, &doc)]));
    let doc = Document::str("Hello");
    assert!(!fits(4, 0, vector![(0, Broken, &doc)]));
    let doc = Document::str("Hello");
    assert!(!fits(4, 0, vector![(0, Unbroken, &doc)]));

    // Cons fits if combined smaller than limit
    let doc = Document::str("1").append(Document::str("2"));
    assert!(fits(2, 0, vector![(0, Broken, &doc)]));
    let doc = Document::str("1").append(Document::str("2"));
    assert!(fits(2, 0, vector![(0, Unbroken, &doc,)]));
    let doc = Document::str("1").append(Document::str("2"));
    assert!(!fits(1, 0, vector![(0, Broken, &doc)]));
    let doc = Document::str("1").append(Document::str("2"));
    assert!(!fits(1, 0, vector![(0, Unbroken, &doc)]));

    // Nest fits if combined smaller than limit
    let doc = Nest(
        1,
        NestMode::Increase,
        NestCondition::Always,
        Box::new(Document::str("12")),
    );
    assert!(fits(2, 0, vector![(0, Broken, &doc)]));
    assert!(fits(2, 0, vector![(0, Unbroken, &doc)]));
    assert!(!fits(1, 0, vector![(0, Broken, &doc)]));
    assert!(!fits(1, 0, vector![(0, Unbroken, &doc)]));

    // Nest fits if combined smaller than limit
    let doc = Nest(
        0,
        NestMode::Increase,
        NestCondition::Always,
        Box::new(Document::str("12")),
    );
    assert!(fits(2, 0, vector![(0, Broken, &doc)]));
    assert!(fits(2, 0, vector![(0, Unbroken, &doc)]));
    assert!(!fits(1, 0, vector![(0, Broken, &doc)]));
    assert!(!fits(1, 0, vector![(0, Unbroken, &doc)]));

    let doc = ZeroWidthString {
        string: "this is a very long string that doesn't count towards line width".into(),
    };
    assert!(fits(10, 0, vector![(0, Unbroken, &doc)]));
    assert!(fits(10, 9, vector![(0, Unbroken, &doc)]));
    let string_doc = "hello!".to_doc();
    assert!(fits(
        10,
        0,
        vector![(0, Unbroken, &string_doc), (0, Unbroken, &doc)]
    ));
}

#[test]
fn format_test() {
    let doc = Document::str("Hi");
    assert_eq!("Hi", doc.to_pretty_string(10));

    let doc = Document::str("Hi").append(Document::str(", world!"));
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
    assert_eq!("broken\n", doc.clone().to_pretty_string(5));

    let doc = Nest(
        2,
        NestMode::Increase,
        NestCondition::Always,
        Box::new(Document::str("1").append(Line(1).append(Document::str("2")))),
    );
    assert_eq!("1\n  2", doc.to_pretty_string(1));

    let doc = Group(Box::new(ForceBroken(Box::new(Break {
        broken: "broken",
        unbroken: "unbroken",
        kind: BreakKind::Strict,
    }))));
    assert_eq!("broken\n".to_string(), doc.to_pretty_string(100));

    let doc = ForceBroken(Box::new(Break {
        broken: "broken",
        unbroken: "unbroken",
        kind: BreakKind::Flex,
    }));
    assert_eq!("unbroken".to_string(), doc.to_pretty_string(100));

    let doc = Vec(vec![
        Break {
            broken: "broken",
            unbroken: "unbroken",
            kind: BreakKind::Strict,
        },
        zero_width_string("<This will not cause a line break>".into()),
        Break {
            broken: "broken",
            unbroken: "unbroken",
            kind: BreakKind::Strict,
        },
    ]);
    assert_eq!(
        "unbroken<This will not cause a line break>unbroken",
        doc.to_pretty_string(20)
    );
}

#[test]
fn forcing_test() {
    let docs = join(
        [
            "hello".to_doc(),
            "a".to_doc(),
            "b".to_doc(),
            "c".to_doc(),
            "d".to_doc(),
        ],
        break_("", " "),
    );

    assert_eq!(
        "hello\na\nb\nc\nd",
        docs.clone().force_break().group().to_pretty_string(80)
    );
    assert_eq!(
        "hello a b c d",
        docs.clone()
            .force_break()
            .next_break_fits(NextBreakFitsMode::Enabled)
            .group()
            .to_pretty_string(80)
    );
    assert_eq!(
        "hello\na\nb\nc\nd",
        docs.clone()
            .force_break()
            .next_break_fits(NextBreakFitsMode::Enabled)
            .next_break_fits(NextBreakFitsMode::Disabled)
            .group()
            .to_pretty_string(80)
    );
}

#[test]
fn nest_if_broken_test() {
    assert_eq!(
        "hello\n  world",
        concat(["hello".to_doc(), break_("", " "), "world".to_doc()])
            .nest_if_broken(2)
            .group()
            .to_pretty_string(10)
    );

    let list_doc = concat([
        concat([
            break_("[", "["),
            "a,".to_doc(),
            break_("", " "),
            "b".to_doc(),
        ])
        .nest(2),
        break_(",", ""),
        "]".to_doc(),
    ])
    .group();

    let arguments_doc = concat([
        break_("", ""),
        "one".to_doc(),
        ",".to_doc(),
        break_("", " "),
        list_doc.group().next_break_fits(NextBreakFitsMode::Enabled),
    ])
    .nest_if_broken(2)
    .group();

    let function_call_doc = concat([
        "some_function_call(".to_doc(),
        arguments_doc,
        break_("", ""),
        ")".to_doc(),
    ])
    .group();

    assert_eq!(
        "some_function_call(\n  one,\n  [\n    a,\n    b,\n  ]\n)",
        function_call_doc.clone().to_pretty_string(2)
    );
    assert_eq!(
        "some_function_call(\n  one,\n  [a, b]\n)",
        function_call_doc.clone().to_pretty_string(20)
    );
    assert_eq!(
        "some_function_call(one, [\n  a,\n  b,\n])",
        function_call_doc.clone().to_pretty_string(25)
    );
    assert_eq!(
        "some_function_call(one, [a, b])",
        function_call_doc.clone().to_pretty_string(80)
    );
}

#[test]
fn let_left_side_fits_test() {
    let elements = break_("", "").append("1").nest(2).append(break_("", ""));
    let list = "[".to_doc().append(elements).append("]").group();
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
    assert!(!"wibble".to_doc().is_empty());
    assert!(!" ".to_doc().is_empty());
    assert!(!"\n".to_doc().is_empty());

    // containers
    assert!("".to_doc().nest(2).is_empty());
    assert!(!"wibble".to_doc().nest(2).is_empty());
    assert!("".to_doc().group().is_empty());
    assert!(!"wibble".to_doc().group().is_empty());
    assert!(break_("", "").is_empty());
    assert!(!break_("wibble", "wibble").is_empty());
    assert!(!break_("wibble\nwobble", "wibble wobble").is_empty());
    assert!("".to_doc().append("".to_doc()).is_empty());
    assert!(!"wibble".to_doc().append("".to_doc()).is_empty());
    assert!(!"".to_doc().append("wibble".to_doc()).is_empty());
    assert!(!zero_width_string("wibble".into()).is_empty());
}

#[test]
fn set_nesting() {
    let doc = Vec(vec!["wibble".to_doc(), break_("", " "), "wobble".to_doc()]).group();
    assert_eq!(
        "wibble\nwobble",
        doc.set_nesting(0).nest(2).to_pretty_string(1)
    );
}
