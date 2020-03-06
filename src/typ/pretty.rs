use super::{Type, TypeVar};
use crate::pretty::*;
use itertools::Itertools;

const INDENT: isize = 2;

pub struct Printer {
    names: im::HashMap<usize, String>,
    uid: usize,
}

impl Printer {
    pub fn new() -> Self {
        Self {
            names: im::HashMap::new(),
            uid: 0,
        }
    }

    /// Render a Type as a well formatted string.
    ///
    pub fn pretty_print(&mut self, typ: &Type, initial_indent: usize) -> String {
        let mut buffer = String::with_capacity(initial_indent);
        for _ in 0..initial_indent {
            buffer.push(' ');
        }
        buffer
            .to_doc()
            .append(self.type_doc(typ))
            .nest(initial_indent as isize)
            .format(80)
    }

    fn type_doc(&mut self, typ: &Type) -> Document {
        match typ {
            Type::App { name, args, .. } => {
                if args.is_empty() {
                    name.clone().to_doc()
                } else {
                    name.clone()
                        .to_doc()
                        .append("(")
                        .append(self.args_to_gleam_doc(args))
                        .append(")")
                }
            }
            Type::Fn { args, retrn } => "fn("
                .to_doc()
                .append(self.args_to_gleam_doc(args))
                .append(") -> ")
                .append(self.type_doc(retrn)),

            Type::Var { typ, .. } => self.type_var_doc(&*typ.borrow()),

            Type::Tuple { elems, .. } => self.args_to_gleam_doc(elems).surround("tuple(", ")"),
        }
    }

    fn type_var_doc(&mut self, typ: &TypeVar) -> Document {
        match typ {
            TypeVar::Link { ref typ, .. } => self.type_doc(typ),

            TypeVar::Unbound { id, .. } => self.type_var_doc(&TypeVar::Generic { id: *id }),

            TypeVar::Generic { id, .. } => match self.names.get(&id) {
                Some(n) => n.clone().to_doc(),
                None => {
                    let n = self.next_letter();
                    self.names.insert(*id, n.clone());
                    n.to_doc()
                }
            },
        }
    }

    fn next_letter(&mut self) -> String {
        let alphabet_length = 26;
        let char_offset = 97;
        let mut chars = vec![];
        let mut n;
        let mut rest = self.uid;

        loop {
            n = rest % alphabet_length;
            rest /= alphabet_length;
            chars.push((n as u8 + char_offset) as char);

            if rest == 0 {
                break;
            }
            rest -= 1
        }

        self.uid += 1;
        chars.into_iter().rev().collect()
    }

    fn args_to_gleam_doc(&mut self, args: &[Type]) -> Document {
        match args.len() {
            0 => nil(),
            _ => args
                .iter()
                .map(|t| self.type_doc(t).group())
                .intersperse(break_(",", ", "))
                .collect::<Vec<_>>()
                .to_doc()
                .nest(INDENT)
                .append(break_(",", ""))
                .group(),
        }
    }
}

#[test]
fn next_letter_test() {
    let mut printer = Printer::new();
    assert_eq!(printer.next_letter(), "a".to_string());
    assert_eq!(printer.next_letter(), "b".to_string());
    assert_eq!(printer.next_letter(), "c".to_string());
    assert_eq!(printer.next_letter(), "d".to_string());
    assert_eq!(printer.next_letter(), "e".to_string());
    assert_eq!(printer.next_letter(), "f".to_string());
    assert_eq!(printer.next_letter(), "g".to_string());
    assert_eq!(printer.next_letter(), "h".to_string());
    assert_eq!(printer.next_letter(), "i".to_string());
    assert_eq!(printer.next_letter(), "j".to_string());
    assert_eq!(printer.next_letter(), "k".to_string());
    assert_eq!(printer.next_letter(), "l".to_string());
    assert_eq!(printer.next_letter(), "m".to_string());
    assert_eq!(printer.next_letter(), "n".to_string());
    assert_eq!(printer.next_letter(), "o".to_string());
    assert_eq!(printer.next_letter(), "p".to_string());
    assert_eq!(printer.next_letter(), "q".to_string());
    assert_eq!(printer.next_letter(), "r".to_string());
    assert_eq!(printer.next_letter(), "s".to_string());
    assert_eq!(printer.next_letter(), "t".to_string());
    assert_eq!(printer.next_letter(), "u".to_string());
    assert_eq!(printer.next_letter(), "v".to_string());
    assert_eq!(printer.next_letter(), "w".to_string());
    assert_eq!(printer.next_letter(), "x".to_string());
    assert_eq!(printer.next_letter(), "y".to_string());
    assert_eq!(printer.next_letter(), "z".to_string());
    assert_eq!(printer.next_letter(), "aa".to_string());
    assert_eq!(printer.next_letter(), "ab".to_string());
    assert_eq!(printer.next_letter(), "ac".to_string());
    assert_eq!(printer.next_letter(), "ad".to_string());
    assert_eq!(printer.next_letter(), "ae".to_string());
    assert_eq!(printer.next_letter(), "af".to_string());
    assert_eq!(printer.next_letter(), "ag".to_string());
    assert_eq!(printer.next_letter(), "ah".to_string());
    assert_eq!(printer.next_letter(), "ai".to_string());
    assert_eq!(printer.next_letter(), "aj".to_string());
    assert_eq!(printer.next_letter(), "ak".to_string());
    assert_eq!(printer.next_letter(), "al".to_string());
    assert_eq!(printer.next_letter(), "am".to_string());
    assert_eq!(printer.next_letter(), "an".to_string());
    assert_eq!(printer.next_letter(), "ao".to_string());
    assert_eq!(printer.next_letter(), "ap".to_string());
    assert_eq!(printer.next_letter(), "aq".to_string());
    assert_eq!(printer.next_letter(), "ar".to_string());
    assert_eq!(printer.next_letter(), "as".to_string());
    assert_eq!(printer.next_letter(), "at".to_string());
    assert_eq!(printer.next_letter(), "au".to_string());
    assert_eq!(printer.next_letter(), "av".to_string());
    assert_eq!(printer.next_letter(), "aw".to_string());
    assert_eq!(printer.next_letter(), "ax".to_string());
    assert_eq!(printer.next_letter(), "ay".to_string());
    assert_eq!(printer.next_letter(), "az".to_string());
    assert_eq!(printer.next_letter(), "ba".to_string());
    assert_eq!(printer.next_letter(), "bb".to_string());
    assert_eq!(printer.next_letter(), "bc".to_string());
    assert_eq!(printer.next_letter(), "bd".to_string());
    assert_eq!(printer.next_letter(), "be".to_string());
    assert_eq!(printer.next_letter(), "bf".to_string());
    assert_eq!(printer.next_letter(), "bg".to_string());
    assert_eq!(printer.next_letter(), "bh".to_string());
    assert_eq!(printer.next_letter(), "bi".to_string());
    assert_eq!(printer.next_letter(), "bj".to_string());
    assert_eq!(printer.next_letter(), "bk".to_string());
    assert_eq!(printer.next_letter(), "bl".to_string());
    assert_eq!(printer.next_letter(), "bm".to_string());
    assert_eq!(printer.next_letter(), "bn".to_string());
    assert_eq!(printer.next_letter(), "bo".to_string());
    assert_eq!(printer.next_letter(), "bp".to_string());
    assert_eq!(printer.next_letter(), "bq".to_string());
    assert_eq!(printer.next_letter(), "br".to_string());
    assert_eq!(printer.next_letter(), "bs".to_string());
    assert_eq!(printer.next_letter(), "bt".to_string());
    assert_eq!(printer.next_letter(), "bu".to_string());
    assert_eq!(printer.next_letter(), "bv".to_string());
    assert_eq!(printer.next_letter(), "bw".to_string());
    assert_eq!(printer.next_letter(), "bx".to_string());
    assert_eq!(printer.next_letter(), "by".to_string());
    assert_eq!(printer.next_letter(), "bz".to_string());
}

#[test]
fn pretty_print_test() {
    use std::cell::RefCell;
    use std::sync::Arc;

    macro_rules! assert_string {
        ($src:expr, $typ:expr $(,)?) => {
            let mut printer = Printer::new();
            assert_eq!($typ.to_string(), printer.pretty_print(&$src, 0),);
        };
    }

    assert_string!(
        Type::App {
            module: vec!["whatever".to_string()],
            name: "Int".to_string(),
            public: true,
            args: vec![],
        },
        "Int",
    );
    assert_string!(
        Type::App {
            module: vec![],
            name: "Pair".to_string(),
            public: true,
            args: vec![
                Type::App {
                    module: vec!["whatever".to_string()],
                    name: "Int".to_string(),
                    public: true,
                    args: vec![],
                },
                Type::App {
                    module: vec!["whatever".to_string()],
                    name: "Bool".to_string(),
                    public: true,
                    args: vec![],
                },
            ],
        },
        "Pair(Int, Bool)",
    );
    assert_string!(
        Type::Fn {
            args: vec![
                Type::App {
                    args: vec![],
                    module: vec!["whatever".to_string()],
                    name: "Int".to_string(),
                    public: true,
                },
                Type::App {
                    args: vec![],
                    module: vec!["whatever".to_string()],
                    name: "Bool".to_string(),
                    public: true,
                },
            ],
            retrn: Box::new(Type::App {
                args: vec![],
                module: vec!["whatever".to_string()],
                name: "Bool".to_string(),
                public: true,
            }),
        },
        "fn(Int, Bool) -> Bool",
    );
    assert_string!(
        Type::Var {
            typ: Arc::new(RefCell::new(TypeVar::Link {
                typ: Box::new(Type::App {
                    args: vec![],
                    module: vec!["whatever".to_string()],
                    name: "Int".to_string(),
                    public: true,
                }),
            })),
        },
        "Int",
    );
    assert_string!(
        Type::Var {
            typ: Arc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 2231 })),
        },
        "a",
    );
    assert_string!(
        Type::Fn {
            args: vec![Type::Var {
                typ: Arc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 78 })),
            }],
            retrn: Box::new(Type::Var {
                typ: Arc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 2 })),
            }),
        },
        "fn(a) -> b",
    );
    assert_string!(
        Type::Fn {
            args: vec![Type::Var {
                typ: Arc::new(RefCell::new(TypeVar::Generic { id: 78 })),
            }],
            retrn: Box::new(Type::Var {
                typ: Arc::new(RefCell::new(TypeVar::Generic { id: 2 })),
            }),
        },
        "fn(a) -> b",
    );
}
