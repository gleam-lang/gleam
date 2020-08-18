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
    }
    .group();
    assert_eq!("unbroken".to_string(), format(10, doc));

    let doc = Break {
        broken: "broken".to_string(),
        unbroken: "unbroken".to_string(),
    }
    .group();
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

#[test]
fn let_left_side_fits_test() {
    let elems = break_("", "").append("1").nest(2).append(break_("", ""));
    let list = "[".to_doc().append(elems).append("]").group();
    let doc = list.clone().append(" = ").append(list);

    assert_eq!(
        "[1] = [
  1
]",
        format(7, doc.clone())
    );

    assert_eq!(
        "[
  1
] = [
  1
]",
        format(2, doc.clone())
    );

    assert_eq!("[1] = [1]", format(16, doc.clone()));
}
