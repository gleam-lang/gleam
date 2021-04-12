use super::Document::*;
use super::Mode::*;
use super::*;

#[test]
fn fits_test() {
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
                broken: "12",
                unbroken: "",
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
                broken: "",
                unbroken: "123",
            }
        )]
    ));
    assert!(!fits(
        2,
        vector![(
            0,
            Unbroken,
            Break {
                broken: "",
                unbroken: "123",
            }
        )]
    ));

    // Line always fits
    assert!(fits(0, vector![(0, Broken, Line(100))]));
    assert!(fits(0, vector![(0, Unbroken, Line(100))]));

    // String fits if smaller than limit
    assert!(fits(5, vector![(0, Broken, String("Hello".to_string()))]));
    assert!(fits(5, vector![(0, Unbroken, String("Hello".to_string()))]));
    assert!(!fits(4, vector![(0, Broken, String("Hello".to_string()))]));
    assert!(!fits(
        4,
        vector![(0, Unbroken, String("Hello".to_string()))]
    ));

    // Cons fits if combined smaller than limit
    assert!(fits(
        2,
        vector![(
            0,
            Broken,
            String("1".to_string()).append(String("2".to_string()))
        )]
    ));
    assert!(fits(
        2,
        vector![(
            0,
            Unbroken,
            String("1".to_string()).append(String("2".to_string()))
        )]
    ));
    assert!(!fits(
        1,
        vector![(
            0,
            Broken,
            String("1".to_string()).append(String("2".to_string()))
        )]
    ));
    assert!(!fits(
        1,
        vector![(
            0,
            Unbroken,
            String("1".to_string()).append(String("2".to_string()))
        )]
    ));

    // Nest fits if combined smaller than limit
    assert!(fits(
        2,
        vector![(0, Broken, Nest(1, Box::new(String("12".to_string())),))]
    ));
    assert!(fits(
        2,
        vector![(0, Unbroken, Nest(1, Box::new(String("12".to_string())),))]
    ));
    assert!(!fits(
        1,
        vector![(0, Broken, Nest(1, Box::new(String("12".to_string())),))]
    ));
    assert!(!fits(
        1,
        vector![(0, Unbroken, Nest(1, Box::new(String("12".to_string()))))]
    ));

    // Nest fits if combined smaller than limit
    assert!(fits(
        2,
        vector![(0, Broken, NestCurrent(Box::new(String("12".to_string())),))]
    ));
    assert!(fits(
        2,
        vector![(
            0,
            Unbroken,
            NestCurrent(Box::new(String("12".to_string())),)
        )]
    ));
    assert!(!fits(
        1,
        vector![(0, Broken, NestCurrent(Box::new(String("12".to_string())),))]
    ));
    assert!(!fits(
        1,
        vector![(0, Unbroken, NestCurrent(Box::new(String("12".to_string()))))]
    ));
}

#[test]
fn format_test() {
    let doc = String("Hi".to_string());
    assert_eq!("Hi".to_string(), doc.to_pretty_string(10));

    let doc = String("Hi".to_string()).append(String(", world!".to_string()));
    assert_eq!("Hi, world!".to_string(), doc.to_pretty_string(10));

    let doc = Break {
        broken: "broken",
        unbroken: "unbroken",
    }
    .group();
    assert_eq!("unbroken".to_string(), doc.to_pretty_string(10));

    let doc = Break {
        broken: "broken",
        unbroken: "unbroken",
    }
    .group();
    assert_eq!("broken\n".to_string(), doc.to_pretty_string(5));

    let doc = Nest(
        2,
        Box::new(String("1".to_string()).append(Line(1).append(String("2".to_string())))),
    );
    assert_eq!("1\n  2".to_string(), doc.to_pretty_string(1));

    let doc = String("111".to_string()).append(NestCurrent(Box::new(
        Line(1).append(String("2".to_string())),
    )));
    assert_eq!("111\n   2".to_string(), doc.to_pretty_string(1));

    let doc = ForceBreak.append(Break {
        broken: "broken",
        unbroken: "unbroken",
    });
    assert_eq!("broken\n".to_string(), doc.to_pretty_string(100));
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
