use crate::{
    ast::{SrcSpan, TypedPattern},
    type_,
};

use super::pattern::*;

#[test]
fn register_int() {
    let mut patterns = Patterns::new();
    let input = TypedPattern::Int {
        location: SrcSpan::default(),
        value: "123".into(),
    };
    let id = patterns.register(&input);
    assert_eq!(
        patterns.get(id),
        Some(&Pattern::Int {
            value: "123".into()
        })
    )
}

#[test]
fn register_float() {
    let mut patterns = Patterns::new();
    let input = TypedPattern::Float {
        location: SrcSpan::default(),
        value: "1.1".into(),
    };
    let id = patterns.register(&input);
    assert_eq!(
        patterns.get(id),
        Some(&Pattern::Float {
            value: "1.1".into()
        })
    )
}

#[test]
fn register_string() {
    let mut patterns = Patterns::new();
    let input = TypedPattern::String {
        location: SrcSpan::default(),
        value: "Hello, Joe!".into(),
    };
    let id = patterns.register(&input);
    assert_eq!(
        patterns.get(id),
        Some(&Pattern::String {
            value: "Hello, Joe!".into()
        })
    )
}

#[test]
fn register_bit_string() {
    let mut patterns = Patterns::new();
    let input = TypedPattern::BitString {
        location: SrcSpan::new(123, 456),
        segments: vec![],
    };
    let id = patterns.register(&input);
    assert_eq!(
        patterns.get(id),
        Some(&Pattern::BitString {
            value: "123:456".into()
        })
    )
}

#[test]
fn register_variable() {
    let mut patterns = Patterns::new();
    let input = TypedPattern::Variable {
        location: SrcSpan::new(123, 456),
        name: "wibble".into(),
        type_: type_::int(),
    };
    let id = patterns.register(&input);
    assert_eq!(
        patterns.get(id),
        Some(&Pattern::Variable {
            name: "wibble".into()
        })
    )
}

#[test]
fn register_assign() {
    let mut patterns = Patterns::new();
    let input = TypedPattern::Assign {
        name: "wibble".into(),
        location: SrcSpan::default(),
        pattern: Box::new(TypedPattern::Int {
            value: "123".into(),
            location: SrcSpan::default(),
        }),
    };
    let id = patterns.register(&input);

    let Pattern::Assign { name, pattern: inner } = patterns.get(id).unwrap()
        else { panic!() };

    assert_eq!(name, "wibble");
    assert_eq!(
        patterns.get(*inner),
        Some(&Pattern::Int {
            value: "123".into()
        })
    )
}

#[test]
fn register_tuple() {
    let mut patterns = Patterns::new();
    let input = TypedPattern::Tuple {
        location: SrcSpan::default(),
        elems: vec![
            TypedPattern::Int {
                value: "123".into(),
                location: SrcSpan::default(),
            },
            TypedPattern::Float {
                value: "4.5".into(),
                location: SrcSpan::default(),
            },
        ],
    };
    let id = patterns.register(&input);

    let Pattern::Tuple { elements } = patterns.get(id).unwrap()
        else { panic!() };

    assert_eq!(elements.len(), 2);
    assert_eq!(
        patterns.get(elements[0]),
        Some(&Pattern::Int {
            value: "123".into()
        })
    );
    assert_eq!(
        patterns.get(elements[1]),
        Some(&Pattern::Float {
            value: "4.5".into()
        })
    );
}
