use super::Document::*;
use super::*;

use im::vector;
use pretty_assertions::assert_eq;

#[test]
fn fits_test() {
    // Negative limits never fit
    assert!(!fits(-1, vector![]));

    // If no more documents it always fits
    assert!(fits(0, vector![]));

    // ForceBreak never fits
    assert!(!fits(100, vector![&ForceBreak]));

    // Break in Broken fits always
    assert!(fits(
        1,
        vector![&Break {
            broken: "12",
            unbroken: "",
        }]
    ));

    // Break in Unbroken mode fits if `unbroken` fits
    assert!(fits(
        3,
        vector![&Break {
            broken: "",
            unbroken: "123",
        }]
    ));
    assert!(!fits(
        2,
        vector![&Break {
            broken: "",
            unbroken: "123",
        }]
    ));

    // Line always fits
    assert!(fits(0, vector![&Line(100)]));

    // String fits if smaller than limit
    let doc = String("Hello".to_string());
    assert!(fits(5, vector![&doc]));
    let doc = String("Hello".to_string());
    assert!(!fits(4, vector![&doc]));

    // Cons fits if combined smaller than limit
    let doc = String("1".to_string()).append(String("2".to_string()));
    assert!(fits(2, vector![&doc]));
    let doc = String("1".to_string()).append(String("2".to_string()));
    assert!(!fits(1, vector![&doc]));

    // Nest fits if combined smaller than limit
    let doc = Nest(1, Box::new(String("12".to_string())));
    assert!(fits(2, vector![&doc]));
    let doc = Nest(1, Box::new(String("12".to_string())));
    assert!(!fits(1, vector![&doc]));

    // Nest fits if combined smaller than limit
    let doc = NestCurrent(Box::new(String("12".to_string())));
    assert!(fits(2, vector![&doc]));
    let doc = NestCurrent(Box::new(String("12".to_string())));
    assert!(!fits(1, vector![&doc]));
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
