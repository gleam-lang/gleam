use super::{Type, TypeVar};
use crate::{
    docvec,
    pretty::{nil, *},
};
use ecow::EcoString;
use std::sync::Arc;

#[cfg(test)]
use super::*;
#[cfg(test)]
use std::cell::RefCell;

#[cfg(test)]
use pretty_assertions::assert_eq;

const INDENT: isize = 2;

#[derive(Debug, Default)]
pub struct Printer {
    names: im::HashMap<u64, EcoString>,
    uid: u64,
    // A mapping of printd type names to the module that they are defined in.
    printed_types: im::HashMap<EcoString, EcoString>,
}

impl Printer {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with_names(&mut self, names: im::HashMap<u64, EcoString>) {
        self.names = names;
    }

    /// Render a Type as a well formatted string.
    ///
    pub fn pretty_print(&mut self, type_: &Type, initial_indent: usize) -> String {
        let mut buffer = String::with_capacity(initial_indent);
        for _ in 0..initial_indent {
            buffer.push(' ');
        }
        buffer
            .to_doc()
            .append(self.print(type_))
            .nest(initial_indent as isize)
            .to_pretty_string(80)
    }

    // TODO: have this function return a Document that borrows from the Type.
    // Is this possible? The lifetime would have to go through the Arc<Refcell<Type>>
    // for TypeVar::Link'd types.
    pub fn print<'a>(&mut self, type_: &Type) -> Document<'a> {
        match type_ {
            Type::Named {
                name,
                arguments,
                module,
                ..
            } => {
                let doc = if self.name_clashes_if_unqualified(name, module) {
                    qualify_type_name(module, name)
                } else {
                    let _ = self.printed_types.insert(name.clone(), module.clone());
                    name.to_doc()
                };
                if arguments.is_empty() {
                    doc
                } else {
                    doc.append("(")
                        .append(self.arguments_to_gleam_doc(arguments))
                        .append(")")
                }
            }

            Type::Fn { arguments, return_ } => "fn("
                .to_doc()
                .append(self.arguments_to_gleam_doc(arguments))
                .append(") ->")
                .append(
                    break_("", " ")
                        .append(self.print(return_))
                        .nest(INDENT)
                        .group(),
                ),

            Type::Var { type_, .. } => self.type_var_doc(&type_.borrow()),

            Type::Tuple { elements, .. } => {
                self.arguments_to_gleam_doc(elements).surround("#(", ")")
            }
        }
    }

    fn name_clashes_if_unqualified(&mut self, type_: &EcoString, module: &str) -> bool {
        match self.printed_types.get(type_) {
            None => false,
            Some(previous_module) if module == previous_module => false,
            Some(_different_module) => true,
        }
    }

    fn type_var_doc<'a>(&mut self, type_: &TypeVar) -> Document<'a> {
        match type_ {
            TypeVar::Link { type_, .. } => self.print(type_),
            TypeVar::Unbound { id, .. } | TypeVar::Generic { id, .. } => self.generic_type_var(*id),
        }
    }

    pub fn generic_type_var<'a>(&mut self, id: u64) -> Document<'a> {
        match self.names.get(&id) {
            Some(n) => {
                let _ = self.printed_types.insert(n.clone(), "".into());
                n.to_doc()
            }
            None => {
                let n = self.next_letter();
                let _ = self.names.insert(id, n.clone());
                let _ = self.printed_types.insert(n.clone(), "".into());
                n.to_doc()
            }
        }
    }

    fn next_letter(&mut self) -> EcoString {
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

    fn arguments_to_gleam_doc(&mut self, arguments: &[Arc<Type>]) -> Document<'static> {
        if arguments.is_empty() {
            return nil();
        }

        let arguments = join(
            arguments.iter().map(|type_| self.print(type_).group()),
            break_(",", ", "),
        );
        break_("", "")
            .append(arguments)
            .nest(INDENT)
            .append(break_(",", ""))
            .group()
    }
}

fn qualify_type_name(module: &str, type_name: &str) -> Document<'static> {
    docvec![EcoString::from(module), ".", EcoString::from(type_name)]
}

#[test]
fn next_letter_test() {
    let mut printer = Printer::new();
    assert_eq!(printer.next_letter().as_str(), "a");
    assert_eq!(printer.next_letter().as_str(), "b");
    assert_eq!(printer.next_letter().as_str(), "c");
    assert_eq!(printer.next_letter().as_str(), "d");
    assert_eq!(printer.next_letter().as_str(), "e");
    assert_eq!(printer.next_letter().as_str(), "f");
    assert_eq!(printer.next_letter().as_str(), "g");
    assert_eq!(printer.next_letter().as_str(), "h");
    assert_eq!(printer.next_letter().as_str(), "i");
    assert_eq!(printer.next_letter().as_str(), "j");
    assert_eq!(printer.next_letter().as_str(), "k");
    assert_eq!(printer.next_letter().as_str(), "l");
    assert_eq!(printer.next_letter().as_str(), "m");
    assert_eq!(printer.next_letter().as_str(), "n");
    assert_eq!(printer.next_letter().as_str(), "o");
    assert_eq!(printer.next_letter().as_str(), "p");
    assert_eq!(printer.next_letter().as_str(), "q");
    assert_eq!(printer.next_letter().as_str(), "r");
    assert_eq!(printer.next_letter().as_str(), "s");
    assert_eq!(printer.next_letter().as_str(), "t");
    assert_eq!(printer.next_letter().as_str(), "u");
    assert_eq!(printer.next_letter().as_str(), "v");
    assert_eq!(printer.next_letter().as_str(), "w");
    assert_eq!(printer.next_letter().as_str(), "x");
    assert_eq!(printer.next_letter().as_str(), "y");
    assert_eq!(printer.next_letter().as_str(), "z");
    assert_eq!(printer.next_letter().as_str(), "aa");
    assert_eq!(printer.next_letter().as_str(), "ab");
    assert_eq!(printer.next_letter().as_str(), "ac");
    assert_eq!(printer.next_letter().as_str(), "ad");
    assert_eq!(printer.next_letter().as_str(), "ae");
    assert_eq!(printer.next_letter().as_str(), "af");
    assert_eq!(printer.next_letter().as_str(), "ag");
    assert_eq!(printer.next_letter().as_str(), "ah");
    assert_eq!(printer.next_letter().as_str(), "ai");
    assert_eq!(printer.next_letter().as_str(), "aj");
    assert_eq!(printer.next_letter().as_str(), "ak");
    assert_eq!(printer.next_letter().as_str(), "al");
    assert_eq!(printer.next_letter().as_str(), "am");
    assert_eq!(printer.next_letter().as_str(), "an");
    assert_eq!(printer.next_letter().as_str(), "ao");
    assert_eq!(printer.next_letter().as_str(), "ap");
    assert_eq!(printer.next_letter().as_str(), "aq");
    assert_eq!(printer.next_letter().as_str(), "ar");
    assert_eq!(printer.next_letter().as_str(), "as");
    assert_eq!(printer.next_letter().as_str(), "at");
    assert_eq!(printer.next_letter().as_str(), "au");
    assert_eq!(printer.next_letter().as_str(), "av");
    assert_eq!(printer.next_letter().as_str(), "aw");
    assert_eq!(printer.next_letter().as_str(), "ax");
    assert_eq!(printer.next_letter().as_str(), "ay");
    assert_eq!(printer.next_letter().as_str(), "az");
    assert_eq!(printer.next_letter().as_str(), "ba");
    assert_eq!(printer.next_letter().as_str(), "bb");
    assert_eq!(printer.next_letter().as_str(), "bc");
    assert_eq!(printer.next_letter().as_str(), "bd");
    assert_eq!(printer.next_letter().as_str(), "be");
    assert_eq!(printer.next_letter().as_str(), "bf");
    assert_eq!(printer.next_letter().as_str(), "bg");
    assert_eq!(printer.next_letter().as_str(), "bh");
    assert_eq!(printer.next_letter().as_str(), "bi");
    assert_eq!(printer.next_letter().as_str(), "bj");
    assert_eq!(printer.next_letter().as_str(), "bk");
    assert_eq!(printer.next_letter().as_str(), "bl");
    assert_eq!(printer.next_letter().as_str(), "bm");
    assert_eq!(printer.next_letter().as_str(), "bn");
    assert_eq!(printer.next_letter().as_str(), "bo");
    assert_eq!(printer.next_letter().as_str(), "bp");
    assert_eq!(printer.next_letter().as_str(), "bq");
    assert_eq!(printer.next_letter().as_str(), "br");
    assert_eq!(printer.next_letter().as_str(), "bs");
    assert_eq!(printer.next_letter().as_str(), "bt");
    assert_eq!(printer.next_letter().as_str(), "bu");
    assert_eq!(printer.next_letter().as_str(), "bv");
    assert_eq!(printer.next_letter().as_str(), "bw");
    assert_eq!(printer.next_letter().as_str(), "bx");
    assert_eq!(printer.next_letter().as_str(), "by");
    assert_eq!(printer.next_letter().as_str(), "bz");
}

#[test]
fn pretty_print_test() {
    macro_rules! assert_string {
        ($src:expr, $type_:expr $(,)?) => {
            let mut printer = Printer::new();
            assert_eq!($type_.to_string(), printer.pretty_print(&$src, 0),);
        };
    }

    assert_string!(
        Type::Named {
            module: "whatever".into(),
            package: "whatever".into(),
            name: "Int".into(),
            publicity: Publicity::Public,
            arguments: vec![],
            inferred_variant: None,
        },
        "Int",
    );
    assert_string!(
        Type::Named {
            module: "themodule".into(),
            package: "whatever".into(),
            name: "Pair".into(),
            publicity: Publicity::Public,
            arguments: vec![
                Arc::new(Type::Named {
                    module: "whatever".into(),
                    package: "whatever".into(),
                    name: "Int".into(),
                    publicity: Publicity::Public,
                    arguments: vec![],
                    inferred_variant: None,
                }),
                Arc::new(Type::Named {
                    module: "whatever".into(),
                    package: "whatever".into(),
                    name: "Bool".into(),
                    publicity: Publicity::Public,
                    arguments: vec![],
                    inferred_variant: None,
                }),
            ],
            inferred_variant: None,
        },
        "Pair(Int, Bool)",
    );
    assert_string!(
        Type::Fn {
            arguments: vec![
                Arc::new(Type::Named {
                    arguments: vec![],
                    module: "whatever".into(),
                    package: "whatever".into(),
                    name: "Int".into(),
                    publicity: Publicity::Public,
                    inferred_variant: None,
                }),
                Arc::new(Type::Named {
                    arguments: vec![],
                    module: "whatever".into(),
                    package: "whatever".into(),
                    name: "Bool".into(),
                    publicity: Publicity::Public,
                    inferred_variant: None,
                }),
            ],
            return_: Arc::new(Type::Named {
                arguments: vec![],
                module: "whatever".into(),
                package: "whatever".into(),
                name: "Bool".into(),
                publicity: Publicity::Public,
                inferred_variant: None,
            }),
        },
        "fn(Int, Bool) -> Bool",
    );
    assert_string!(
        Type::Var {
            type_: Arc::new(RefCell::new(TypeVar::Link {
                type_: Arc::new(Type::Named {
                    arguments: vec![],
                    module: "whatever".into(),
                    package: "whatever".into(),
                    name: "Int".into(),
                    publicity: Publicity::Public,
                    inferred_variant: None,
                }),
            })),
        },
        "Int",
    );
    assert_string!(
        Type::Var {
            type_: Arc::new(RefCell::new(TypeVar::Unbound { id: 2231 })),
        },
        "a",
    );
    assert_string!(
        fn_(
            vec![Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Unbound { id: 78 })),
            })],
            Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Unbound { id: 2 })),
            }),
        ),
        "fn(a) -> b",
    );
    assert_string!(
        fn_(
            vec![Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Generic { id: 78 })),
            })],
            Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Generic { id: 2 })),
            }),
        ),
        "fn(a) -> b",
    );
}

#[test]
fn function_test() {
    assert_eq!(pretty_print(fn_(vec![], int())), "fn() -> Int");

    assert_eq!(
        pretty_print(fn_(vec![int(), int(), int()], int())),
        "fn(Int, Int, Int) -> Int"
    );

    assert_eq!(
        pretty_print(fn_(
            vec![
                float(),
                float(),
                float(),
                float(),
                float(),
                float(),
                float(),
                float(),
                float(),
                float(),
                float(),
                float(),
                float()
            ],
            float()
        )),
        "fn(
  Float,
  Float,
  Float,
  Float,
  Float,
  Float,
  Float,
  Float,
  Float,
  Float,
  Float,
  Float,
  Float,
) -> Float"
    );

    assert_eq!(
        pretty_print(fn_(
            vec![
                tuple(vec![float(), float(), float(), float(), float(), float()]),
                float(),
                float(),
                float(),
                float(),
                float(),
                float(),
                float()
            ],
            float()
        )),
        "fn(
  #(Float, Float, Float, Float, Float, Float),
  Float,
  Float,
  Float,
  Float,
  Float,
  Float,
  Float,
) -> Float"
    );

    assert_eq!(
        pretty_print(fn_(
            vec![tuple(vec![
                float(),
                float(),
                float(),
                float(),
                float(),
                float()
            ]),],
            tuple(vec![
                tuple(vec![float(), float(), float(), float(), float(), float()]),
                tuple(vec![float(), float(), float(), float(), float(), float()]),
            ]),
        )),
        "fn(#(Float, Float, Float, Float, Float, Float)) ->
  #(
    #(Float, Float, Float, Float, Float, Float),
    #(Float, Float, Float, Float, Float, Float),
  )"
    );
}

#[cfg(test)]
fn pretty_print(type_: Arc<Type>) -> String {
    Printer::new().pretty_print(&type_, 0)
}
