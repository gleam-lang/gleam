use crate::ast::{
    self, Arg, BinOp, Clause, Expr, Meta, Module, Pattern, Statement, TypedExpr, TypedModule,
    UntypedExpr, UntypedModule,
};
use crate::pretty::*;
use itertools::Itertools;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

const INDENT: isize = 2;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    App {
        public: bool,
        module: Vec<String>,
        name: String,
        args: Vec<Type>,
    },

    AnonStruct {
        elems: Vec<Type>,
    },

    Fn {
        args: Vec<Type>,
        retrn: Box<Type>,
    },

    Map {
        row: Box<Type>,
    },

    Var {
        typ: Rc<RefCell<TypeVar>>,
    },

    RowNil,

    RowCons {
        label: String,
        head: Box<Type>,
        tail: Box<Type>,
    },
}

impl Type {
    /// Render a Type as a well formatted string.
    ///
    pub fn pretty_print(&self, initial_indent: usize) -> String {
        let mut b = String::with_capacity(initial_indent);
        for _ in 0..initial_indent {
            b.push(' ');
        }
        b.to_doc()
            .append(self.to_gleam_doc(&mut im::hashmap![], &mut initial_indent.clone()))
            .nest(initial_indent as isize)
            .format(80)
    }

    pub fn to_gleam_doc(
        &self,
        names: &mut im::HashMap<usize, String>,
        uid: &mut usize,
    ) -> Document {
        match self {
            Type::App { name, args, .. } => {
                if args.len() == 0 {
                    name.clone().to_doc()
                } else {
                    name.clone()
                        .to_doc()
                        .append("(")
                        .append(args_to_gleam_doc(args, names, uid))
                        .append(")")
                }
            }
            Type::Fn { args, retrn } => "fn("
                .to_doc()
                .append(args_to_gleam_doc(args, names, uid))
                .append(") -> ")
                .append(retrn.to_gleam_doc(names, uid)),

            Type::AnonStruct { elems, .. } => {
                args_to_gleam_doc(elems, names, uid).surround("struct(", ")")
            }

            Type::Map { row } => {
                if let Type::RowNil { .. } = **row {
                    return "{}".to_doc();
                }
                let mut row_to_doc = |row: &Type| {
                    let mut fields = ordmap![];
                    let tail = row.gather_fields(&mut fields);
                    let fields_doc = fields
                        .into_iter()
                        .map(|(label, typ)| {
                            label.to_doc().append(" =").append(
                                break_("", " ")
                                    .append(typ.to_gleam_doc(names, uid))
                                    .nest(INDENT)
                                    .group(),
                            )
                        })
                        .intersperse(break_(",", ", "))
                        .collect::<Vec<_>>()
                        .to_doc();
                    match tail {
                        None => fields_doc,
                        Some(tail) => tail
                            .to_gleam_doc(names, uid)
                            .group()
                            .append(" | ")
                            .append(fields_doc),
                    }
                };
                "{".to_doc()
                    .append(
                        break_("", " ")
                            .append(row_to_doc(row))
                            .nest(INDENT)
                            .append(break_("", " "))
                            .group(),
                    )
                    .append("}")
            }

            Type::Var { typ, .. } => typ.borrow().to_gleam_doc(names, uid),

            Type::RowCons { .. } => unreachable!(),

            Type::RowNil { .. } => nil(),
        }
    }

    fn gather_fields(&self, fields: &mut im::OrdMap<String, Type>) -> Option<Type> {
        match self {
            Type::RowNil => None,

            Type::RowCons { label, head, tail } => {
                if !fields.contains_key(label) {
                    fields.insert(label.clone(), *head.clone());
                }
                tail.gather_fields(fields)
            }

            Type::Var { typ } => match &*typ.borrow() {
                TypeVar::Link { typ } => typ.gather_fields(fields),
                _other => Some(self.clone()),
            },

            other => Some(other.clone()),
        }
    }

    pub fn collapse_links(self) -> Type {
        if let Type::Var { typ } = &self {
            if let TypeVar::Link { typ } = &*typ.borrow() {
                return *typ.clone();
            }
        }
        self
    }

    /// Get the args for the type if the type is a specific Type::App.
    /// Returns None if the type is not a Type::App or is an incorrect Type:App
    ///
    pub fn get_app_args(
        &self,
        public: bool,
        module: &Vec<String>,
        name: &str,
        arity: usize,
        env: &mut Env,
    ) -> Option<Vec<Type>> {
        match self {
            Type::App {
                module: m,
                name: n,
                args,
                ..
            } => {
                if module == m && name == n && args.len() == arity {
                    Some(args.clone())
                } else {
                    None
                }
            }

            Type::Var { typ } => {
                enum Action {
                    Link(Vec<Type>),
                }

                let action = match &*typ.borrow() {
                    TypeVar::Link { typ } => {
                        return typ.get_app_args(public, module, name, arity, env);
                    }

                    TypeVar::Unbound { level, .. } => {
                        Action::Link((0..arity).map(|_| env.new_unbound_var(*level)).collect())
                    }

                    TypeVar::Generic { .. } => return None,
                };

                match action {
                    Action::Link(args) => {
                        *typ.borrow_mut() = TypeVar::Link {
                            typ: Box::new(Type::App {
                                name: name.to_string(),
                                module: module.clone(),
                                args: args.clone(),
                                public,
                            }),
                        };
                        Some(args)
                    }
                }
            }

            _ => None,
        }
    }

    pub fn find_private_type(&self) -> Option<Type> {
        match self {
            Type::RowNil { .. } => None,

            Type::Map { row, .. } => row.find_private_type(),

            Type::AnonStruct { elems, .. } => elems.iter().find_map(|t| t.find_private_type()),

            Type::App { public: false, .. } => Some(self.clone()),

            Type::App { args, .. } => args.iter().find_map(|t| t.find_private_type()),

            Type::RowCons { head, tail, .. } => head
                .find_private_type()
                .or_else(|| tail.find_private_type()),

            Type::Fn { retrn, args, .. } => retrn
                .find_private_type()
                .or_else(|| args.iter().find_map(|t| t.find_private_type())),

            Type::Var { typ, .. } => match &*typ.borrow() {
                TypeVar::Unbound { .. } => None,

                TypeVar::Generic { .. } => None,

                TypeVar::Link { typ, .. } => typ.find_private_type(),
            },
        }
    }
}

impl TypeVar {
    pub fn to_gleam_doc(
        &self,
        names: &mut im::HashMap<usize, String>,
        uid: &mut usize,
    ) -> Document {
        match self {
            TypeVar::Link { ref typ, .. } => typ.to_gleam_doc(names, uid),

            TypeVar::Unbound { id, .. } => TypeVar::Generic { id: *id }.to_gleam_doc(names, uid),

            TypeVar::Generic { id, .. } => match names.get(&id) {
                Some(n) => n.clone().to_doc(),
                None => {
                    let n = next_letter(uid);
                    names.insert(*id, n.clone());
                    n.to_doc()
                }
            },
        }
    }

    pub fn is_unbound(&self) -> bool {
        match self {
            TypeVar::Unbound { .. } => true,
            _ => false,
        }
    }
}

fn next_letter(i: &mut usize) -> String {
    let alphabet_length = 26;
    let char_offset = 97;
    let mut chars = vec![];
    let mut n;
    let mut rest = *i;

    loop {
        n = rest % alphabet_length;
        rest = rest / alphabet_length;
        chars.push((n as u8 + char_offset) as char);

        if rest <= 0 {
            break;
        }
        rest -= 1
    }

    *i += 1;
    chars.into_iter().rev().collect()
}

#[test]
fn next_letter_test() {
    let mut x = 0;
    assert_eq!(next_letter(&mut x), "a".to_string());
    assert_eq!(next_letter(&mut x), "b".to_string());
    assert_eq!(next_letter(&mut x), "c".to_string());
    assert_eq!(next_letter(&mut x), "d".to_string());
    assert_eq!(next_letter(&mut x), "e".to_string());
    assert_eq!(next_letter(&mut x), "f".to_string());
    assert_eq!(next_letter(&mut x), "g".to_string());
    assert_eq!(next_letter(&mut x), "h".to_string());
    assert_eq!(next_letter(&mut x), "i".to_string());
    assert_eq!(next_letter(&mut x), "j".to_string());
    assert_eq!(next_letter(&mut x), "k".to_string());
    assert_eq!(next_letter(&mut x), "l".to_string());
    assert_eq!(next_letter(&mut x), "m".to_string());
    assert_eq!(next_letter(&mut x), "n".to_string());
    assert_eq!(next_letter(&mut x), "o".to_string());
    assert_eq!(next_letter(&mut x), "p".to_string());
    assert_eq!(next_letter(&mut x), "q".to_string());
    assert_eq!(next_letter(&mut x), "r".to_string());
    assert_eq!(next_letter(&mut x), "s".to_string());
    assert_eq!(next_letter(&mut x), "t".to_string());
    assert_eq!(next_letter(&mut x), "u".to_string());
    assert_eq!(next_letter(&mut x), "v".to_string());
    assert_eq!(next_letter(&mut x), "w".to_string());
    assert_eq!(next_letter(&mut x), "x".to_string());
    assert_eq!(next_letter(&mut x), "y".to_string());
    assert_eq!(next_letter(&mut x), "z".to_string());
    assert_eq!(next_letter(&mut x), "aa".to_string());
    assert_eq!(next_letter(&mut x), "ab".to_string());
    assert_eq!(next_letter(&mut x), "ac".to_string());
    assert_eq!(next_letter(&mut x), "ad".to_string());
    assert_eq!(next_letter(&mut x), "ae".to_string());
    assert_eq!(next_letter(&mut x), "af".to_string());
    assert_eq!(next_letter(&mut x), "ag".to_string());
    assert_eq!(next_letter(&mut x), "ah".to_string());
    assert_eq!(next_letter(&mut x), "ai".to_string());
    assert_eq!(next_letter(&mut x), "aj".to_string());
    assert_eq!(next_letter(&mut x), "ak".to_string());
    assert_eq!(next_letter(&mut x), "al".to_string());
    assert_eq!(next_letter(&mut x), "am".to_string());
    assert_eq!(next_letter(&mut x), "an".to_string());
    assert_eq!(next_letter(&mut x), "ao".to_string());
    assert_eq!(next_letter(&mut x), "ap".to_string());
    assert_eq!(next_letter(&mut x), "aq".to_string());
    assert_eq!(next_letter(&mut x), "ar".to_string());
    assert_eq!(next_letter(&mut x), "as".to_string());
    assert_eq!(next_letter(&mut x), "at".to_string());
    assert_eq!(next_letter(&mut x), "au".to_string());
    assert_eq!(next_letter(&mut x), "av".to_string());
    assert_eq!(next_letter(&mut x), "aw".to_string());
    assert_eq!(next_letter(&mut x), "ax".to_string());
    assert_eq!(next_letter(&mut x), "ay".to_string());
    assert_eq!(next_letter(&mut x), "az".to_string());
    assert_eq!(next_letter(&mut x), "ba".to_string());
    assert_eq!(next_letter(&mut x), "bb".to_string());
    assert_eq!(next_letter(&mut x), "bc".to_string());
    assert_eq!(next_letter(&mut x), "bd".to_string());
    assert_eq!(next_letter(&mut x), "be".to_string());
    assert_eq!(next_letter(&mut x), "bf".to_string());
    assert_eq!(next_letter(&mut x), "bg".to_string());
    assert_eq!(next_letter(&mut x), "bh".to_string());
    assert_eq!(next_letter(&mut x), "bi".to_string());
    assert_eq!(next_letter(&mut x), "bj".to_string());
    assert_eq!(next_letter(&mut x), "bk".to_string());
    assert_eq!(next_letter(&mut x), "bl".to_string());
    assert_eq!(next_letter(&mut x), "bm".to_string());
    assert_eq!(next_letter(&mut x), "bn".to_string());
    assert_eq!(next_letter(&mut x), "bo".to_string());
    assert_eq!(next_letter(&mut x), "bp".to_string());
    assert_eq!(next_letter(&mut x), "bq".to_string());
    assert_eq!(next_letter(&mut x), "br".to_string());
    assert_eq!(next_letter(&mut x), "bs".to_string());
    assert_eq!(next_letter(&mut x), "bt".to_string());
    assert_eq!(next_letter(&mut x), "bu".to_string());
    assert_eq!(next_letter(&mut x), "bv".to_string());
    assert_eq!(next_letter(&mut x), "bw".to_string());
    assert_eq!(next_letter(&mut x), "bx".to_string());
    assert_eq!(next_letter(&mut x), "by".to_string());
    assert_eq!(next_letter(&mut x), "bz".to_string());
}

#[test]
fn letter_test() {
    let mut i = 0;
    assert_eq!("a", next_letter(&mut i));
    assert_eq!("b", next_letter(&mut i));
    assert_eq!("c", next_letter(&mut i));
}

fn args_to_gleam_doc(
    args: &[Type],
    names: &mut im::HashMap<usize, String>,
    uid: &mut usize,
) -> Document {
    match args.len() {
        0 => nil(),
        _ => args
            .iter()
            .map(|t| t.to_gleam_doc(names, uid).group())
            .intersperse(break_(",", ", "))
            .collect::<Vec<_>>()
            .to_doc()
            .nest(INDENT)
            .append(break_(",", ""))
            .group(),
    }
}

#[test]
fn to_gleam_doc_test() {
    let cases = [
        (
            Type::App {
                module: vec!["whatever".to_string()],
                name: "Int".to_string(),
                public: true,
                args: vec![],
            },
            "Int",
        ),
        (
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
        ),
        (
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
        ),
        (
            Type::Var {
                typ: Rc::new(RefCell::new(TypeVar::Link {
                    typ: Box::new(Type::App {
                        args: vec![],
                        module: vec!["whatever".to_string()],
                        name: "Int".to_string(),
                        public: true,
                    }),
                })),
            },
            "Int",
        ),
        (
            Type::Var {
                typ: Rc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 2231 })),
            },
            "a",
        ),
        (
            Type::Fn {
                args: vec![Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 78 })),
                }],
                retrn: Box::new(Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 2 })),
                }),
            },
            "fn(a) -> b",
        ),
        (
            Type::Fn {
                args: vec![Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Generic { id: 78 })),
                }],
                retrn: Box::new(Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Generic { id: 2 })),
                }),
            },
            "fn(a) -> b",
        ),
    ];

    for (typ, s) in cases.into_iter() {
        assert_eq!(
            s.to_string(),
            typ.to_gleam_doc(&mut hashmap![], &mut 0).format(80)
        );
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVar {
    Unbound { id: usize, level: usize },
    Link { typ: Box<Type> },
    Generic { id: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeConstructorInfo {
    pub public: bool,
    pub module: Vec<String>,
    pub arity: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueConstructor {
    pub variant: ValueConstructorVariant,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueConstructorVariant {
    /// A locally defined variable or function parameter
    LocalVariable,

    /// An enum constructor or singleton
    Enum { arity: usize },

    /// A function belonging to the module
    ModuleFn { module: Vec<String>, arity: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleTypeInfo {
    pub name: Vec<String>,
    pub type_constructors: HashMap<String, TypeConstructorInfo>,
    pub value_constructors: HashMap<String, ValueConstructor>,
}

#[derive(Debug, Clone)]
pub struct Env<'a> {
    uid: usize,
    annotated_generic_types: im::HashSet<usize>,
    variables: im::HashMap<String, ValueConstructor>,
    importable_modules: &'a HashMap<String, ModuleTypeInfo>,
    imported_modules: HashMap<String, ModuleTypeInfo>,
    type_constructors: HashMap<String, TypeConstructorInfo>,
    public_module_value_constructors: HashMap<String, ValueConstructor>,
}

#[derive(Debug, Clone, Copy)]
pub enum NewTypeAction {
    Disallow,
    MakeGeneric,
}

impl<'a> Env<'a> {
    pub fn new(importable_modules: &'a HashMap<String, ModuleTypeInfo>) -> Self {
        let mut env = Self {
            uid: 0,
            annotated_generic_types: im::HashSet::new(),
            type_constructors: HashMap::new(),
            public_module_value_constructors: HashMap::new(),
            imported_modules: HashMap::new(),
            variables: hashmap![],
            importable_modules,
        };

        env.insert_type_constructor(
            "Int".to_string(),
            TypeConstructorInfo {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_variable(
            "True".to_string(),
            ValueConstructorVariant::Enum { arity: 0 },
            bool(),
        );
        env.insert_variable(
            "False".to_string(),
            ValueConstructorVariant::Enum { arity: 0 },
            bool(),
        );
        env.insert_type_constructor(
            "Bool".to_string(),
            TypeConstructorInfo {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_type_constructor(
            "List".to_string(),
            TypeConstructorInfo {
                arity: 1,
                module: vec![],
                public: true,
            },
        );

        env.insert_type_constructor(
            "Float".to_string(),
            TypeConstructorInfo {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_type_constructor(
            "String".to_string(),
            TypeConstructorInfo {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_type_constructor(
            "Result".to_string(),
            TypeConstructorInfo {
                arity: 2,
                module: vec![],
                public: true,
            },
        );

        env.insert_variable(
            "Nil".to_string(),
            ValueConstructorVariant::Enum { arity: 0 },
            bool(),
        );
        env.insert_type_constructor(
            "Nil".to_string(),
            TypeConstructorInfo {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_variable(
            "+".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "-".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "*".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "/".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "+.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        env.insert_variable(
            "-.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        env.insert_variable(
            "*.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        env.insert_variable(
            "||".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![bool(), bool()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "&&".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![bool(), bool()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "%".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "%.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        env.insert_variable(
            "/.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        let a = env.new_generic_var();
        let b = env.new_generic_var();
        let f = Type::Fn {
            args: vec![a.clone()],
            retrn: Box::new(b.clone()),
        };
        env.insert_variable(
            "|>".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![a, f],
                retrn: Box::new(b),
            },
        );

        let a = env.new_generic_var();
        env.insert_variable(
            "==".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![a.clone(), a],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            ">".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            ">=".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "<".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "<=".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            ">.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            ">=.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "<.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "<=.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(bool()),
            },
        );

        let a = env.new_generic_var();
        env.insert_variable(
            "!=".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![a.clone(), a],
                retrn: Box::new(bool()),
            },
        );

        let result = |ok, error| Type::App {
            name: "Result".to_string(),
            module: vec![],
            public: true,
            args: vec![ok, error],
        };

        let ok = env.new_generic_var();
        let error = env.new_generic_var();
        env.insert_variable(
            "Ok".to_string(),
            ValueConstructorVariant::Enum { arity: 1 },
            Type::Fn {
                args: vec![ok.clone()],
                retrn: Box::new(result(ok, error)),
            },
        );

        let ok = env.new_generic_var();
        let error = env.new_generic_var();
        env.insert_variable(
            "Error".to_string(),
            ValueConstructorVariant::Enum { arity: 1 },
            Type::Fn {
                args: vec![error.clone()],
                retrn: Box::new(result(ok, error)),
            },
        );

        env
    }

    fn next_uid(&mut self) -> usize {
        let i = self.uid;
        self.uid += 1;
        i
    }

    fn previous_uid(&self) -> usize {
        self.uid - 1
    }

    /// Create a new unbound type that is a specific type, we just don't
    /// know which one yet.
    ///
    pub fn new_unbound_var(&mut self, level: usize) -> Type {
        Type::Var {
            typ: Rc::new(RefCell::new(TypeVar::Unbound {
                id: self.next_uid(),
                level,
            })),
        }
    }

    /// Create a new generic type that can stand in for any type.
    ///
    pub fn new_generic_var(&mut self) -> Type {
        Type::Var {
            typ: Rc::new(RefCell::new(TypeVar::Generic {
                id: self.next_uid(),
            })),
        }
    }

    /// Map a variable in the current scope.
    ///
    pub fn insert_variable(&mut self, name: String, variant: ValueConstructorVariant, typ: Type) {
        self.variables
            .insert(name, ValueConstructor { variant, typ });
    }

    /// Lookup a variable in the current scope.
    ///
    pub fn get_variable(&self, name: &str) -> Option<&ValueConstructor> {
        self.variables.get(name)
    }

    /// Map a type in the current scope.
    ///
    pub fn insert_type_constructor(&mut self, name: String, info: TypeConstructorInfo) {
        self.type_constructors.insert(name, info);
    }

    /// Lookup a type in the current scope.
    ///
    pub fn get_type_constructor(
        &self,
        module_alias: &Option<String>,
        name: &str,
    ) -> Result<&TypeConstructorInfo, GetTypeConstructorError> {
        match module_alias {
            None => self.type_constructors.get(name).ok_or_else(|| {
                GetTypeConstructorError::UnknownType {
                    name: name.to_string(),
                    type_constructors: self.type_constructors.clone(),
                }
            }),

            Some(m) => {
                let module = &self.imported_modules.get(m).ok_or_else(|| {
                    GetTypeConstructorError::UnknownModule {
                        name: name.to_string(),
                        imported_modules: self.importable_modules.clone(),
                    }
                })?;
                module.type_constructors.get(name).ok_or_else(|| {
                    GetTypeConstructorError::UnknownModuleType {
                        name: name.to_string(),
                        module_name: module.name.clone(),
                        type_constructors: module.type_constructors.clone(),
                    }
                })
            }
        }
    }

    /// Lookup a value constructor in the current scope.
    ///
    fn get_value_constructor(
        &self,
        module: &Option<String>,
        name: &String,
    ) -> Result<&ValueConstructor, GetValueConstructorError> {
        match module {
            None => self.variables.get(&*name).ok_or_else(|| {
                GetValueConstructorError::UnknownVariable {
                    name: name.to_string(),
                    variables: self.variables.clone(),
                }
            }),

            Some(module) => {
                let module = self.imported_modules.get(&*module).ok_or_else(|| {
                    GetValueConstructorError::UnknownModule {
                        name: name.to_string(),
                        imported_modules: self.importable_modules.clone(),
                    }
                })?;
                module.value_constructors.get(&*name).ok_or_else(|| {
                    GetValueConstructorError::UnknownModuleValue {
                        name: name.to_string(),
                        module_name: module.name.clone(),
                        value_constructors: module.value_constructors.clone(),
                    }
                })
            }
        }
    }

    /// Construct a Type from an AST Type annotation.
    ///
    /// Type variables are managed using a HashMap of names to types- this permits the
    /// same type vars being shared between multiple annotations (such as in the arguments
    /// of an external function declaration)
    ///
    pub fn type_from_ast(
        &mut self,
        ast: &ast::Type,
        vars: &mut im::HashMap<String, (usize, Type)>,
        new: NewTypeAction,
    ) -> Result<Type, Error> {
        match ast {
            ast::Type::Constructor {
                meta,
                module,
                name,
                args,
            } => {
                let args = args
                    .iter()
                    .map(|t| self.type_from_ast(t, vars, new))
                    .collect::<Result<Vec<_>, _>>()?;
                let info = self
                    .get_type_constructor(module, name)
                    .map_err(|e| convert_get_type_constructor_error(e, &meta))?;
                if args.len() != info.arity {
                    return Err(Error::IncorrectTypeArity {
                        meta: meta.clone(),
                        name: name.to_string(),
                        expected: info.arity,
                        given: args.len(),
                    });
                }
                Ok(Type::App {
                    name: name.to_string(),
                    module: info.module.clone(),
                    public: info.public,
                    args,
                })
            }

            ast::Type::Map { fields, tail, .. } => Ok(Type::Map {
                row: Box::new(self.row_from_ast(fields, tail, vars, new)?),
            }),

            ast::Type::AnonStruct { elems, .. } => {
                let elems = elems
                    .iter()
                    .map(|t| self.type_from_ast(t, vars, new))
                    .collect::<Result<_, _>>()?;
                Ok(Type::AnonStruct { elems })
            }

            ast::Type::Fn { args, retrn, .. } => {
                let args = args
                    .iter()
                    .map(|t| self.type_from_ast(t, vars, new))
                    .collect::<Result<_, _>>()?;
                let retrn = self.type_from_ast(retrn, vars, new)?;
                Ok(Type::Fn {
                    args,
                    retrn: Box::new(retrn),
                })
            }

            ast::Type::Var { name, .. } => match vars.get(name) {
                Some((_, var)) => Ok(var.clone()),

                None => {
                    match new {
                        NewTypeAction::MakeGeneric => {
                            let var = self.new_generic_var();
                            vars.insert(name.to_string(), (self.previous_uid(), var.clone()));
                            Ok(var)
                        }
                        NewTypeAction::Disallow => {
                            // TODO: test that enum constructors using unknown vars in their
                            // definitions is not permitted.
                            unimplemented!()
                        }
                    }
                }
            },
        }
    }

    fn row_from_ast(
        &mut self,
        fields: &Vec<(String, ast::Type)>,
        tail: &Option<Box<ast::Type>>,
        vars: &mut im::HashMap<String, (usize, Type)>,
        new: NewTypeAction,
    ) -> Result<Type, Error> {
        let tail = match tail {
            None => Type::RowNil,
            Some(t) => self.type_from_ast(t, vars, new)?,
        };
        fields
            .iter()
            .map(|(label, t)| Ok((label, self.type_from_ast(t, vars, new)?)))
            .fold_results(tail, |acc, (label, t)| Type::RowCons {
                label: label.to_string(),
                head: Box::new(t),
                tail: Box::new(acc),
            })
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    UnknownVariable {
        meta: Meta,
        name: String,
        variables: im::HashMap<String, ValueConstructor>,
    },

    UnknownType {
        meta: Meta,
        name: String,
        types: HashMap<String, TypeConstructorInfo>,
    },

    UnknownModule {
        meta: Meta,
        name: String,
        imported_modules: HashMap<String, ModuleTypeInfo>,
    },

    UnknownModuleType {
        meta: Meta,
        name: String,
        module_name: Vec<String>,
        type_constructors: HashMap<String, TypeConstructorInfo>,
    },

    UnknownModuleValue {
        meta: Meta,
        name: String,
        module_name: Vec<String>,
        value_constructors: HashMap<String, ValueConstructor>,
    },

    NotFn {
        meta: Meta,
        typ: Type,
    },

    IncorrectArity {
        meta: Meta,
        expected: usize,
        given: usize,
    },

    IncorrectTypeArity {
        meta: Meta,
        name: String,
        expected: usize,
        given: usize,
    },

    CouldNotUnify {
        meta: Meta,
        expected: Type,
        given: Type,
    },

    RecursiveType {
        meta: Meta,
    },

    DuplicateName {
        meta: Meta,
        name: String,
    },

    PrivateTypeLeak {
        meta: Meta,
        leaked: Type,
    },

    ExtraField {
        meta: Meta,
        label: String,
        container_typ: RowContainerType,
    },

    FieldNotFound {
        meta: Meta,
        label: String,
        container_typ: RowContainerType,
    },
}

#[derive(Debug, PartialEq)]
pub enum GetValueConstructorError {
    UnknownVariable {
        name: String,
        variables: im::HashMap<String, ValueConstructor>,
    },

    UnknownModule {
        name: String,
        imported_modules: HashMap<String, ModuleTypeInfo>,
    },

    UnknownModuleValue {
        name: String,
        module_name: Vec<String>,
        value_constructors: HashMap<String, ValueConstructor>,
    },
}

fn convert_get_value_constructor_error(e: GetValueConstructorError, meta: &Meta) -> Error {
    match e {
        GetValueConstructorError::UnknownVariable { name, variables } => Error::UnknownVariable {
            meta: meta.clone(),
            name,
            variables,
        },

        GetValueConstructorError::UnknownModule {
            name,
            imported_modules,
        } => Error::UnknownModule {
            meta: meta.clone(),
            name,
            imported_modules,
        },

        GetValueConstructorError::UnknownModuleValue {
            name,
            module_name,
            value_constructors,
        } => Error::UnknownModuleValue {
            meta: meta.clone(),
            name,
            module_name,
            value_constructors,
        },
    }
}

#[derive(Debug, PartialEq)]
pub enum GetTypeConstructorError {
    UnknownType {
        name: String,
        type_constructors: HashMap<String, TypeConstructorInfo>,
    },

    UnknownModule {
        name: String,
        imported_modules: HashMap<String, ModuleTypeInfo>,
    },

    UnknownModuleType {
        name: String,
        module_name: Vec<String>,
        type_constructors: HashMap<String, TypeConstructorInfo>,
    },
}

fn convert_get_type_constructor_error(e: GetTypeConstructorError, meta: &Meta) -> Error {
    match e {
        GetTypeConstructorError::UnknownType {
            name,
            type_constructors,
        } => Error::UnknownType {
            meta: meta.clone(),
            name,
            types: type_constructors,
        },

        GetTypeConstructorError::UnknownModule {
            name,
            imported_modules,
        } => Error::UnknownModule {
            meta: meta.clone(),
            name,
            imported_modules,
        },

        GetTypeConstructorError::UnknownModuleType {
            name,
            module_name,
            type_constructors,
        } => Error::UnknownModuleType {
            meta: meta.clone(),
            name,
            module_name,
            type_constructors,
        },
    }
}

#[derive(Debug, PartialEq)]
pub enum RowContainerType {
    Module,
    Map,
}

impl RowContainerType {
    pub fn to_string(&self) -> String {
        match self {
            RowContainerType::Module => "module".to_string(),
            RowContainerType::Map => "map".to_string(),
        }
    }
}

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer_module(
    module: UntypedModule,
    modules: &HashMap<String, ModuleTypeInfo>,
) -> Result<TypedModule, Error> {
    let mut env = Env::new(modules);
    let module_name = &module.name;

    let statements: Vec<Statement<_, Type>> = module
        .statements
        .into_iter()
        .map(|s| match s {
            Statement::Fn {
                meta,
                name,
                public,
                args,
                body,
                return_annotation,
            } => {
                let level = 1;

                // Ensure function has not already been defined in this module
                if let Some(ValueConstructor {
                    variant: ValueConstructorVariant::ModuleFn { .. },
                    ..
                }) = env.get_variable(&name)
                {
                    return Err(Error::DuplicateName { meta, name });
                };

                // Register a var for the function so that it can call itself recursively
                let rec = env.new_unbound_var(level + 1);
                env.insert_variable(
                    name.clone(),
                    ValueConstructorVariant::ModuleFn {
                        module: module_name.clone(),
                        arity: args.len(),
                    },
                    rec.clone(),
                );

                // Infer the type
                let (args_types, body) =
                    infer_fun(&args, body, &return_annotation, level + 1, &mut env)?;
                let typ = Type::Fn {
                    args: args_types,
                    retrn: Box::new(body.typ().clone()),
                };

                // Assert that the inferred type matches the type of any recursive call
                unify(&rec, &typ, &mut env).map_err(|e| convert_unify_error(e, &meta))?;

                // Insert the function into the environment
                let typ = generalise(typ, level);
                env.insert_variable(
                    name.clone(),
                    ValueConstructorVariant::ModuleFn {
                        module: module_name.clone(),
                        arity: args.len(),
                    },
                    typ.clone(),
                );

                // Insert the function into the module's interface
                if public {
                    if let Some(leaked) = typ.find_private_type() {
                        return Err(Error::PrivateTypeLeak {
                            meta: meta.clone(),
                            leaked,
                        });
                    }
                    env.public_module_value_constructors.insert(
                        name.clone(),
                        ValueConstructor {
                            typ,
                            variant: ValueConstructorVariant::ModuleFn {
                                module: module_name.clone(),
                                arity: args.len(),
                            },
                        },
                    );
                }
                Ok(Statement::Fn {
                    meta,
                    name,
                    public,
                    args,
                    body,
                    return_annotation,
                })
            }

            Statement::ExternalFn {
                meta,
                name,
                public,
                args,
                retrn,
                module,
                fun,
            } => {
                // Construct type of function from AST
                let mut type_vars = hashmap![];
                let retrn_type =
                    env.type_from_ast(&retrn, &mut type_vars, NewTypeAction::MakeGeneric)?;
                let mut args_types = Vec::with_capacity(args.len());
                for arg in args.iter() {
                    let t = env.type_from_ast(arg, &mut type_vars, NewTypeAction::MakeGeneric)?;
                    args_types.push(t)
                }
                let typ = Type::Fn {
                    args: args_types,
                    retrn: Box::new(retrn_type),
                };

                // Insert function into module's public interface
                if public {
                    if let Some(leaked) = typ.find_private_type() {
                        return Err(Error::PrivateTypeLeak {
                            meta: meta.clone(),
                            leaked,
                        });
                    }
                    env.public_module_value_constructors.insert(
                        name.clone(),
                        ValueConstructor {
                            typ: typ.clone(),
                            variant: ValueConstructorVariant::ModuleFn {
                                module: vec![module.clone()],
                                arity: args.len(),
                            },
                        },
                    );
                }

                // Insert function into module's internal scope
                env.insert_variable(
                    name.clone(),
                    ValueConstructorVariant::ModuleFn {
                        module: vec![module.clone()],
                        arity: args.len(),
                    },
                    typ,
                );
                Ok(Statement::ExternalFn {
                    meta,
                    name,
                    public,
                    args,
                    retrn,
                    module,
                    fun,
                })
            }

            Statement::Enum {
                meta,
                public,
                name,
                args,
                constructors,
            } => {
                // Register type
                env.insert_type_constructor(
                    name.clone(),
                    TypeConstructorInfo {
                        module: module_name.clone(),
                        public,
                        arity: args.len(),
                    },
                );
                // Build return type and collect type vars that can be used in constructors
                let mut type_vars = hashmap![];
                let args_types: Vec<_> = args
                    .iter()
                    .map(|arg| ast::Type::Var {
                        meta: meta.clone(),
                        name: arg.to_string(),
                    })
                    .map(|ast| env.type_from_ast(&ast, &mut type_vars, NewTypeAction::MakeGeneric))
                    .collect::<Result<_, _>>()?;

                let retrn = Type::App {
                    public: public.clone(),
                    module: module_name.clone(),
                    name: name.clone(),
                    args: args_types,
                };
                // Check and register constructors
                for constructor in constructors.iter() {
                    let args_types = constructor
                        .args
                        .iter()
                        .map(|arg| env.type_from_ast(&arg, &mut type_vars, NewTypeAction::Disallow))
                        .collect::<Result<Vec<_>, _>>()?;
                    // Insert constructor function into module scope
                    let typ = match constructor.args.len() {
                        0 => retrn.clone(),
                        _ => Type::Fn {
                            args: args_types,
                            retrn: Box::new(retrn.clone()),
                        },
                    };
                    if public {
                        if let Some(leaked) = typ.find_private_type() {
                            return Err(Error::PrivateTypeLeak {
                                meta: constructor.meta.clone(),
                                leaked,
                            });
                        }
                        env.public_module_value_constructors.insert(
                            constructor.name.clone(),
                            ValueConstructor {
                                typ: typ.clone(),
                                variant: ValueConstructorVariant::Enum { arity: args.len() },
                            },
                        );
                    };
                    env.insert_variable(
                        constructor.name.clone(),
                        ValueConstructorVariant::Enum {
                            arity: constructor.args.len(),
                        },
                        typ,
                    );
                }
                Ok(Statement::Enum {
                    meta,
                    public,
                    name,
                    args,
                    constructors,
                })
            }

            Statement::ExternalType {
                meta,
                public,
                name,
                args,
            } => {
                // Register type
                env.insert_type_constructor(
                    name.clone(),
                    TypeConstructorInfo {
                        module: module_name.clone(),
                        public,
                        arity: args.len(),
                    },
                );
                // Check contained types are valid
                let mut type_vars = hashmap![];
                for arg in args.iter() {
                    let var = ast::Type::Var {
                        meta: meta.clone(),
                        name: arg.to_string(),
                    };
                    env.type_from_ast(&var, &mut type_vars, NewTypeAction::MakeGeneric)?;
                }
                Ok(Statement::ExternalType {
                    meta,
                    public,
                    name,
                    args,
                })
            }

            Statement::Import {
                meta,
                module,
                as_name,
            } => {
                let module_info = env.importable_modules.get(&module.join("/")).expect(
                    "COMPILER BUG: Typer could not find a module being imported.
This should not be possible. Please report this crash",
                );
                let name = match &as_name {
                    None => module[module.len() - 1].clone(),
                    Some(name) => name.clone(),
                };
                env.imported_modules.insert(name, module_info.clone());
                Ok(Statement::Import {
                    meta,
                    module,
                    as_name,
                })
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    // Remove private type constructors to create the public interface
    env.type_constructors.retain(|_, info| info.public);

    Ok(Module {
        name: module.name.clone(),
        statements,
        type_info: ModuleTypeInfo {
            name: module.name,
            type_constructors: env.type_constructors,
            value_constructors: env.public_module_value_constructors,
        },
    })
}

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer(expr: UntypedExpr, level: usize, env: &mut Env) -> Result<TypedExpr, Error> {
    match expr {
        Expr::Int {
            meta,
            value,
            typ: _,
        } => Ok(Expr::Int {
            meta,
            value,
            typ: int(),
        }),

        Expr::Float {
            meta,
            value,
            typ: _,
        } => Ok(Expr::Float {
            meta,
            value,
            typ: float(),
        }),

        Expr::String {
            meta,
            value,
            typ: _,
        } => Ok(Expr::String {
            meta,
            value,
            typ: string(),
        }),

        Expr::Nil { meta, typ: _ } => Ok(Expr::Nil {
            meta,
            typ: list(env.new_unbound_var(level)),
        }),

        Expr::Seq {
            meta,
            first,
            then,
            typ: _,
        } => {
            let first = infer(*first, level, env)?;
            let then = infer(*then, level, env)?;
            Ok(Expr::Seq {
                meta,
                typ: then.typ().clone(),
                first: Box::new(first),
                then: Box::new(then),
            })
        }

        Expr::Fn {
            meta,
            is_capture,
            args,
            body,
            typ: _,
        } => {
            let (args_types, body) = infer_fun(&args, *body, &None, level, env)?;
            let typ = Type::Fn {
                args: args_types,
                retrn: Box::new(body.typ().clone()),
            };

            Ok(Expr::Fn {
                meta,
                typ,
                is_capture,
                args,
                body: Box::new(body),
            })
        }

        Expr::Let {
            meta,
            pattern,
            value,
            then,
            typ: _,
        } => {
            let value = infer(*value, level + 1, env)?;
            let value_typ = generalise(value.typ().clone(), level + 1);
            unify_pattern(&pattern, &value_typ, level, env)?;
            let then = infer(*then, level, env)?;
            let typ = then.typ().clone();
            Ok(Expr::Let {
                meta,
                typ,
                pattern,
                value: Box::new(value),
                then: Box::new(then),
            })
        }

        Expr::Case {
            meta,
            subject,
            clauses,
            typ: _,
        } => {
            let return_type = env.new_unbound_var(level); // TODO: should this be level + 1 ?
            let mut typed_clauses = Vec::with_capacity(clauses.len());
            let subject = infer(*subject, level + 1, env)?;
            let subject_type = generalise(subject.typ().clone(), level + 1);

            for clause in clauses.into_iter() {
                let vars = env.variables.clone();

                unify_pattern(&clause.pattern, &subject_type, level, env)?;

                let then = infer(clause.then, level, env)?;
                unify(&return_type, then.typ(), env)
                    .map_err(|e| convert_unify_error(e, then.meta()))?;
                typed_clauses.push(Clause {
                    meta: clause.meta,
                    pattern: clause.pattern,
                    then,
                });

                env.variables = vars;
            }
            Ok(Expr::Case {
                meta,
                typ: return_type,
                subject: Box::new(subject),
                clauses: typed_clauses,
            })
        }

        Expr::Cons {
            meta,
            head,
            tail,
            typ: _,
        } => {
            let head = infer(*head, level, env)?;
            let tail = infer(*tail, level, env)?;
            unify(tail.typ(), &list(head.typ().clone()), env)
                .map_err(|e| convert_unify_error(e, &meta))?;
            Ok(Expr::Cons {
                meta,
                typ: tail.typ().clone(),
                head: Box::new(head),
                tail: Box::new(tail),
            })
        }

        Expr::Call {
            meta,
            fun,
            args,
            typ: _,
        } => {
            let (fun, args, typ) = infer_call(*fun, args, level, &meta, env)?;
            Ok(Expr::Call {
                meta,
                typ,
                args,
                fun: Box::new(fun),
            })
        }

        Expr::AnonStruct {
            meta,
            elems,
            typ: _,
        } => {
            let elems = elems
                .into_iter()
                .map(|e| infer(e, level, env))
                .collect::<Result<Vec<_>, _>>()?;
            let typ = Type::AnonStruct {
                elems: elems.iter().map(|e| e.typ().clone()).collect(),
            };
            Ok(Expr::AnonStruct { meta, elems, typ })
        }

        Expr::BinOp {
            meta,
            name,
            left,
            right,
            typ: _,
        } => {
            let fun = Expr::Var {
                meta: meta.clone(),
                constructor: (),
                name: bin_op_name(&name),
            };
            let (_fun, mut args, typ) = infer_call(fun, vec![*left, *right], level, &meta, env)?;
            Ok(Expr::BinOp {
                meta,
                name,
                typ,
                right: Box::new(args.pop().unwrap()),
                left: Box::new(args.pop().unwrap()),
            })
        }

        Expr::MapNil { meta, typ: _ } => Ok(Expr::MapNil {
            meta,
            typ: Type::Map {
                row: Box::new(Type::RowNil),
            },
        }),

        Expr::MapCons {
            meta,
            tail,
            label,
            value,
            typ: _,
        } => {
            let value = infer(*value, level, env)?;
            let tail = infer(*tail, level, env)?;

            let value_type = env.new_unbound_var(level);
            unify(&value_type, value.typ(), env).map_err(|e| convert_unify_error(e, &meta))?;

            let tail_row_type = env.new_unbound_var(level);
            unify(
                &Type::Map {
                    row: Box::new(tail_row_type.clone()),
                },
                tail.typ(),
                env,
            )
            .map_err(|e| convert_unify_error(e, &meta))?;

            let typ = Type::Map {
                row: Box::new(Type::RowCons {
                    label: label.clone(),
                    head: Box::new(value_type),
                    tail: Box::new(tail_row_type),
                }),
            };
            Ok(Expr::MapCons {
                meta,
                typ,
                label,
                tail: Box::new(tail),
                value: Box::new(value),
            })
        }

        Expr::Var {
            meta,
            name,
            constructor: _,
        } => {
            let constructor = infer_var(&name, level, &meta, env)?;
            Ok(Expr::Var {
                constructor,
                meta,
                name,
            })
        }

        Expr::FieldSelect {
            meta: select_meta,
            label,
            map: container,
            typ: _,
        } => match &*container {
            Expr::Var { name, meta, .. } if !env.variables.contains_key(name) => {
                infer_module_select(name, label, level, meta, select_meta, env)
            }

            _ => infer_value_field_select(*container, label, level, select_meta, env),
        },

        // This node is not created by the parser, it is constructed by the typer from
        // the more general FieldSelect. Because of this it should never be present in AST
        // being inferred.
        Expr::ModuleSelect { .. } => panic!(
            "Expr::ModuleSelect erroneously passed to typer.
The is a bug in the Gleam compiler, please report it here:
https://github.com/lpil/gleam/issues
"
        ),
    }
}

fn infer_module_select(
    module_alias: &String,
    label: String,
    level: usize,
    module_meta: &Meta,
    select_meta: Meta,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let (module_name, constructor) = {
        let module_info =
            env.imported_modules
                .get(&*module_alias)
                .ok_or_else(|| Error::UnknownModule {
                    name: module_alias.to_string(),
                    meta: module_meta.clone(),
                    imported_modules: env.imported_modules.clone(),
                })?;

        let constructor = module_info.value_constructors.get(&label).ok_or_else(|| {
            Error::UnknownModuleValue {
                name: label.clone(),
                meta: select_meta.clone(),
                module_name: module_info.name.clone(),
                value_constructors: module_info.value_constructors.clone(),
            }
        })?;

        (module_info.name.clone(), constructor.clone())
    };

    Ok(Expr::ModuleSelect {
        label,
        typ: instantiate(constructor.typ.clone(), level, env),
        meta: select_meta,
        module_name,
        module_alias: module_alias.clone(),
    })
}

fn infer_value_field_select(
    container: UntypedExpr,
    label: String,
    level: usize,
    meta: Meta,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let map = infer(container, level, env)?;
    // We unify the map with a dummy map in order to determine the type of the
    // selected field.
    let other_fields_typ = env.new_unbound_var(level);
    let selected_field_typ = env.new_unbound_var(level);
    let dummy_map = Type::Map {
        row: Box::new(Type::RowCons {
            label: label.clone(),
            head: Box::new(selected_field_typ.clone()),
            tail: Box::new(other_fields_typ),
        }),
    };

    unify(&dummy_map, map.typ(), env)
        .map_err(|e| convert_unify_error(set_map_row_type(e), &meta))?;

    Ok(Expr::FieldSelect {
        meta,
        label,
        map: Box::new(map),
        typ: selected_field_typ,
    })
}

/// When we have an assignment or a case expression we unify the pattern with the
/// inferred type of the subject in order to determine what variables to insert
/// into the environment (or to detect a type error).
///
fn unify_pattern(pattern: &Pattern, typ: &Type, level: usize, env: &mut Env) -> Result<(), Error> {
    //
    // TODO: I think we might be unifying backwards for some of these.
    // The typ should be the `expected` and the `pattern` is the actual?
    // Or perhaps because it's a pattern there should be a different Error variant
    // so we can display a more specific error message.
    //
    match pattern {
        Pattern::Discard { .. } => Ok(()),

        Pattern::Var { name, .. } => {
            env.insert_variable(
                name.to_string(),
                ValueConstructorVariant::LocalVariable,
                typ.clone(),
            );
            Ok(())
        }

        Pattern::Int { meta, .. } => {
            unify(&int(), typ, env).map_err(|e| convert_unify_error(e, &meta))
        }

        Pattern::Float { meta, .. } => {
            unify(&float(), typ, env).map_err(|e| convert_unify_error(e, &meta))
        }

        Pattern::String { meta, .. } => {
            unify(&string(), typ, env).map_err(|e| convert_unify_error(e, &meta))
        }

        Pattern::Nil { meta, .. } => unify(&list(env.new_unbound_var(level)), typ, env)
            .map_err(|e| convert_unify_error(e, &meta)),

        Pattern::Cons {
            meta, head, tail, ..
        } => match typ.get_app_args(true, &vec![], "List", 1, env) {
            Some(args) => {
                unify_pattern(head, &args[0], level, env)?;
                unify_pattern(tail, typ, level, env)
            }

            None => Err(Error::CouldNotUnify {
                given: list(env.new_unbound_var(level)),
                expected: typ.clone(),
                meta: meta.clone(),
            }),
        },

        Pattern::Constructor {
            meta,
            module,
            name,
            args: pattern_args,
        } => {
            let constructor_typ = env
                .get_value_constructor(module, name)
                .map_err(|e| convert_get_value_constructor_error(e, &meta))?
                .typ
                .clone();

            match instantiate(constructor_typ, level, env) {
                Type::Fn { args, retrn } => {
                    if args.len() == pattern_args.len() {
                        for (pattern, typ) in pattern_args.iter().zip(args) {
                            unify_pattern(pattern, &typ, level, env)?;
                        }
                        unify(&retrn, &typ, env).map_err(|e| convert_unify_error(e, &meta))
                    } else {
                        // TODO: Incorrect number of args given to constructor
                        unimplemented!()
                    }
                }

                c @ Type::App { .. } => {
                    if pattern_args.is_empty() {
                        unify(&c, &typ, env).map_err(|e| convert_unify_error(e, &meta))
                    } else {
                        // Error: singleton given args
                        unimplemented!()
                    }
                }

                typ => {
                    dbg!(typ);
                    unimplemented!();
                }
            }
        }

        Pattern::AnonStruct { elems, meta, .. } => match typ.clone().collapse_links() {
            Type::AnonStruct { elems: type_elems } => {
                for (pattern, typ) in elems.iter().zip(type_elems) {
                    unify_pattern(pattern, &typ, level, env)?;
                }
                Ok(())
            }

            typ @ Type::Var { .. } => {
                let elems = (0..(elems.len()))
                    .map(|_| env.new_unbound_var(level))
                    .collect();
                unify(&Type::AnonStruct { elems }, &typ, env)
                    .map_err(|e| convert_unify_error(e, &meta))?;
                unify_pattern(pattern, &typ, level, env)
            }

            other => {
                dbg!(&other);
                unimplemented!();
            }
        },
    }
}

fn infer_var(
    name: &str,
    level: usize,
    meta: &Meta,
    env: &mut Env,
) -> Result<ValueConstructor, Error> {
    let ValueConstructor { variant, typ } =
        env.get_variable(name)
            .cloned()
            .ok_or_else(|| Error::UnknownVariable {
                meta: meta.clone(),
                name: name.to_string(),
                variables: env.variables.clone(),
            })?;
    let typ = instantiate(typ, level, env);
    Ok(ValueConstructor { variant, typ })
}

fn infer_call(
    fun: UntypedExpr,
    args: Vec<UntypedExpr>,
    level: usize,
    meta: &Meta,
    env: &mut Env,
) -> Result<(TypedExpr, Vec<TypedExpr>, Type), Error> {
    let fun = infer(fun, level, env)?;
    let (mut args_types, return_type) = match_fun_type(fun.typ(), args.len(), env)
        .map_err(|e| convert_not_fun_error(e, fun.meta(), &meta))?;
    let args = args_types
        .iter_mut()
        .zip(args)
        .map(|(typ, arg): (&mut Type, _)| {
            let arg = infer(arg, level, env)?;
            unify(typ, arg.typ(), env).map_err(|e| convert_unify_error(e, arg.meta()))?;
            Ok(arg)
        })
        .collect::<Result<_, _>>()?;
    Ok((fun, args, return_type))
}

fn infer_fun(
    args: &[Arg],
    body: UntypedExpr,
    return_annotation: &Option<ast::Type>,
    level: usize,
    env: &mut Env,
) -> Result<(Vec<Type>, TypedExpr), Error> {
    // Construct an initial type for each argument of the function- either an unbound type variable
    // or a type provided by an annotation.
    let mut type_vars = hashmap![];
    let args_types: Vec<_> = args
        .iter()
        .map(|arg| {
            arg.annotation
                .clone()
                .map(|t| env.type_from_ast(&t, &mut type_vars, NewTypeAction::MakeGeneric))
                .unwrap_or_else(|| Ok(env.new_unbound_var(level)))
        })
        .collect::<Result<_, _>>()?;

    // Record generic type variables that comes from type annotations.
    // They cannot be instantiated so we need to keep track of them.
    let previous_annotated_generic_types = env.annotated_generic_types.clone();
    for (id, _type) in type_vars.values() {
        env.annotated_generic_types.insert(*id);
    }

    // Insert arguments into function body scope.
    let previous_vars = env.variables.clone();
    for (arg, t) in args.iter().zip(args_types.iter()) {
        match &arg.name {
            Some(name) => env.insert_variable(
                name.to_string(),
                ValueConstructorVariant::LocalVariable,
                (*t).clone(),
            ),
            None => (),
        };
    }

    let body = infer(body, level, env)?;

    // Check that any return type annotation is accurate.
    if let Some(ann) = return_annotation {
        let ret_typ = env.type_from_ast(ann, &mut type_vars, NewTypeAction::MakeGeneric)?;
        unify(&ret_typ, body.typ(), env).map_err(|e| convert_unify_error(e, body.meta()))?;
    }

    // Reset the env now that the scope of the function has ended.
    env.variables = previous_vars;
    env.annotated_generic_types = previous_annotated_generic_types;
    Ok((args_types, body))
}

fn bin_op_name(name: &BinOp) -> String {
    match name {
        BinOp::Pipe => "|>".to_string(),
        BinOp::And => "&&".to_string(),
        BinOp::Or => "||".to_string(),
        BinOp::LtInt => "<".to_string(),
        BinOp::LtEqInt => "<=".to_string(),
        BinOp::LtFloat => "<.".to_string(),
        BinOp::LtEqFloat => "<=.".to_string(),
        BinOp::Eq => "==".to_string(),
        BinOp::NotEq => "!=".to_string(),
        BinOp::GtEqInt => ">=".to_string(),
        BinOp::GtInt => ">".to_string(),
        BinOp::GtEqFloat => ">=.".to_string(),
        BinOp::GtFloat => ">.".to_string(),
        BinOp::AddInt => "+".to_string(),
        BinOp::AddFloat => "+.".to_string(),
        BinOp::SubInt => "-".to_string(),
        BinOp::SubFloat => "-.".to_string(),
        BinOp::MultInt => "*".to_string(),
        BinOp::MultFloat => "*.".to_string(),
        BinOp::DivInt => "/".to_string(),
        BinOp::DivFloat => "/.".to_string(),
        BinOp::ModuloInt => "%".to_string(),
    }
}

fn convert_unify_error(e: UnifyError, meta: &Meta) -> Error {
    match e {
        UnifyError::CouldNotUnify { expected, given } => Error::CouldNotUnify {
            meta: meta.clone(),
            expected,
            given,
        },

        UnifyError::FieldNotFound {
            label,
            container_typ,
        } => Error::FieldNotFound {
            meta: meta.clone(),
            container_typ,
            label,
        },

        UnifyError::ExtraField {
            label,
            container_typ,
        } => Error::ExtraField {
            meta: meta.clone(),
            container_typ,
            label,
        },

        UnifyError::RecursiveType => Error::RecursiveType { meta: meta.clone() },
    }
}

/// Instantiate converts generic variables into unbound ones.
///
fn instantiate(typ: Type, ctx_level: usize, env: &mut Env) -> Type {
    fn go(t: Type, ctx_level: usize, ids: &mut im::HashMap<usize, Type>, env: &mut Env) -> Type {
        match t {
            Type::App {
                public,
                name,
                module,
                args,
            } => Type::App {
                public,
                name,
                module,
                args: args
                    .into_iter()
                    .map(|t| go(t, ctx_level, ids, env))
                    .collect(),
            },

            Type::Var { typ } => {
                match &*typ.borrow() {
                    TypeVar::Link { typ } => return go(*typ.clone(), ctx_level, ids, env),

                    TypeVar::Unbound { .. } => return Type::Var { typ: typ.clone() },

                    TypeVar::Generic { id } => match ids.get(id) {
                        Some(t) => return t.clone(),
                        None => {
                            if !env.annotated_generic_types.contains(id) {
                                let v = env.new_unbound_var(ctx_level);
                                ids.insert(*id, v.clone());
                                return v;
                            }
                        }
                    },
                }
                Type::Var { typ }
            }

            Type::AnonStruct { elems } => Type::AnonStruct {
                elems: elems
                    .into_iter()
                    .map(|t| go(t, ctx_level, ids, env))
                    .collect(),
            },

            Type::Fn { args, retrn, .. } => {
                let args = args
                    .into_iter()
                    .map(|t| go(t, ctx_level, ids, env))
                    .collect();
                let retrn = Box::new(go(*retrn, ctx_level, ids, env));
                Type::Fn { args, retrn }
            }

            Type::Map { row } => Type::Map {
                row: Box::new(go(*row, ctx_level, ids, env)),
            },

            Type::RowCons { label, head, tail } => Type::RowCons {
                label,
                head: Box::new(go(*head, ctx_level, ids, env)),
                tail: Box::new(go(*tail, ctx_level, ids, env)),
            },

            Type::RowNil => t,
        }
    }

    go(typ, ctx_level, &mut hashmap![], env)
}

#[derive(Debug, PartialEq)]
enum UnifyError {
    CouldNotUnify {
        expected: Type,
        given: Type,
    },

    RecursiveType,

    FieldNotFound {
        container_typ: RowContainerType,
        label: String,
    },

    ExtraField {
        container_typ: RowContainerType,
        label: String,
    },
}

fn unify(t1: &Type, t2: &Type, env: &mut Env) -> Result<(), UnifyError> {
    if t1 == t2 {
        return Ok(());
    }

    // Collapse right hand side type links. Left hand side will be collapsed in the next block.
    if let Type::Var { typ } = t2 {
        if let TypeVar::Link { typ } = &*typ.borrow() {
            return unify(t1, typ, env);
        }
    }

    if let Type::Var { typ } = t1 {
        enum Action {
            Unify(Type),
            CouldNotUnify,
            Link,
        }

        let action = match &*typ.borrow() {
            TypeVar::Link { typ } => Action::Unify((**typ).clone()),

            TypeVar::Unbound { id, level } => {
                update_levels(t2, *level, *id)?;
                Action::Link
            }

            TypeVar::Generic { id } => {
                if let Type::Var { typ } = t2 {
                    if typ.borrow().is_unbound() {
                        *typ.borrow_mut() = TypeVar::Generic { id: *id };
                        return Ok(());
                    }
                }
                Action::CouldNotUnify
            }
        };

        return match action {
            Action::Link => {
                *typ.borrow_mut() = TypeVar::Link {
                    typ: Box::new((*t2).clone()),
                };
                Ok(())
            }

            Action::Unify(t) => unify(&t, t2, env),

            Action::CouldNotUnify => Err(UnifyError::CouldNotUnify {
                expected: (*t1).clone(),
                given: (*t2).clone(),
            }),
        };
    }

    if let Type::Var { .. } = t2 {
        return unify(t2, t1, env).map_err(flip_unify_error);
    }

    match (t1, t2) {
        (
            Type::App {
                module: m1,
                name: n1,
                args: args1,
                ..
            },
            Type::App {
                module: m2,
                name: n2,
                args: args2,
                ..
            },
        ) => {
            if m1 == m2 && n1 == n2 && args1.len() == args2.len() {
                for (a, b) in args1.iter().zip(args2) {
                    unify(a, b, env)?;
                }
                Ok(())
            } else {
                Err(UnifyError::CouldNotUnify {
                    expected: (*t1).clone(),
                    given: (*t2).clone(),
                })
            }
        }

        (Type::AnonStruct { elems: elems1, .. }, Type::AnonStruct { elems: elems2, .. }) => {
            if elems1.len() == elems2.len() {
                for (a, b) in elems1.iter().zip(elems2) {
                    unify(a, b, env)?;
                }
                Ok(())
            } else {
                Err(UnifyError::CouldNotUnify {
                    expected: (*t1).clone(),
                    given: (*t2).clone(),
                })
            }
        }

        (
            Type::Fn {
                args: args1,
                retrn: retrn1,
                ..
            },
            Type::Fn {
                args: args2,
                retrn: retrn2,
                ..
            },
        ) => {
            if args1.len() == args2.len() {
                for (a, b) in args1.iter().zip(args2) {
                    unify(a, b, env)?;
                }
                unify(retrn1, retrn2, env)
            } else {
                Err(UnifyError::CouldNotUnify {
                    expected: (*t1).clone(),
                    given: (*t2).clone(),
                })
            }
        }

        (Type::Map { row: row1 }, Type::Map { row: row2 }) => {
            unify(row1, row2, env).map_err(set_map_row_type)
        }

        (Type::RowNil, Type::RowNil) => Ok(()),

        (Type::RowNil, Type::RowCons { label, .. }) => Err(UnifyError::ExtraField {
            label: label.to_string(),
            container_typ: RowContainerType::Module,
        }),

        (
            Type::RowCons {
                label: label1,
                head: head1,
                tail: tail1,
            },
            Type::RowCons {
                label: label2,
                head: head2,
                tail: tail2,
            },
        ) => {
            let unbound = match &**tail1 {
                Type::Var { typ } => match &*typ.borrow() {
                    TypeVar::Unbound { .. } => Some(typ.clone()),
                    _ => None,
                },
                _ => None,
            };

            // TODO: If we use Rc for types then we clone the Rc not the type
            let t2 = Type::RowCons {
                label: (*label2).clone(),
                head: (*head2).clone(),
                tail: (*tail2).clone(),
            };
            let tail2 = unify_row_field(t2, label1.clone(), *head1.clone(), env)?;

            if let Some(typ) = unbound {
                if let TypeVar::Link { .. } = &*typ.borrow() {
                    // TODO: Recursive row type
                    unimplemented!()
                }
            }
            unify(tail1, &tail2, env)
        }

        (_, _) => Err(UnifyError::CouldNotUnify {
            expected: (*t1).clone(),
            given: (*t2).clone(),
        }),
    }
}

fn set_map_row_type(e: UnifyError) -> UnifyError {
    match e {
        UnifyError::FieldNotFound { label, .. } => UnifyError::FieldNotFound {
            label,
            container_typ: RowContainerType::Map,
        },

        UnifyError::ExtraField { label, .. } => UnifyError::ExtraField {
            label,
            container_typ: RowContainerType::Map,
        },

        other => other,
    }
}

/// Takes a type (expected to be a Row) and searches within it for a field
/// with a particular label.
/// If found the type of the label is unified with the expected type,
/// possibly returning a unification error.
///
/// If the unification is successful the row is returned minus the field
/// that has been unified, so that the caller may continue to check other
/// fields in the row without having to pass over the field that has already
/// been checked.
///
fn unify_row_field(
    row: Type,
    label1: String,
    head1: Type,
    env: &mut Env,
) -> Result<Type, UnifyError> {
    match row {
        Type::RowNil => Err(UnifyError::FieldNotFound {
            label: label1,
            container_typ: RowContainerType::Module,
        }),

        Type::RowCons { label, head, tail } => {
            if label == label1 {
                unify(&head1, &head, env)?;
                Ok(*tail)
            } else {
                let tail = unify_row_field(*tail, label1, head1, env)?;
                Ok(Type::RowCons {
                    label,
                    head,
                    tail: Box::new(tail),
                })
            }
        }

        Type::Var { typ } => {
            let (new_typ, tail) = match &*typ.borrow() {
                TypeVar::Unbound { level, .. } => {
                    let tail = env.new_unbound_var(*level);
                    let t = Type::RowCons {
                        label: label1,
                        head: Box::new(head1),
                        tail: Box::new(tail.clone()), // return this var as it is the tail
                    };
                    (t, tail)
                }

                TypeVar::Link { typ } => return unify_row_field(*typ.clone(), label1, head1, env),

                _ => unimplemented!(), // Expected a row
            };
            *typ.borrow_mut() = TypeVar::Link {
                typ: Box::new(new_typ),
            };

            Ok(tail)
        }

        _ => unimplemented!(), // TODO: Expected a row
    }
}

#[test]
fn rewrite_row_test() {
    let mods = HashMap::new();
    let mut env = Env::new(&mods);

    let row = Type::RowCons {
        label: "a".to_string(),
        head: Box::new(int()),
        tail: Box::new(Type::RowNil {}),
    };
    assert_eq!(
        Ok(Type::RowNil {}),
        unify_row_field(row, "a".to_string(), int(), &mut env)
    );

    let row = Type::RowCons {
        label: "b".to_string(),
        head: Box::new(int()),
        tail: Box::new(Type::RowCons {
            label: "a".to_string(),
            head: Box::new(int()),
            tail: Box::new(Type::RowNil {}),
        }),
    };
    assert_eq!(
        Ok(Type::RowCons {
            label: "b".to_string(),
            head: Box::new(int()),
            tail: Box::new(Type::RowNil {}),
        }),
        unify_row_field(row, "a".to_string(), int(), &mut env)
    );

    let row = Type::RowCons {
        label: "b".to_string(),
        head: Box::new(int()),
        tail: Box::new(env.new_unbound_var(1)),
    };
    assert_eq!(
        Ok(Type::RowCons {
            label: "b".to_string(),
            head: Box::new(int()),
            tail: Box::new(Type::Var {
                typ: Rc::new(RefCell::new(TypeVar::Unbound { id: 9, level: 1 }))
            }),
        }),
        unify_row_field(row, "a".to_string(), int(), &mut env)
    );
}

fn flip_unify_error(e: UnifyError) -> UnifyError {
    match e {
        UnifyError::CouldNotUnify { expected, given } => UnifyError::CouldNotUnify {
            expected: given,
            given: expected,
        },
        other => other,
    }
}

/// This function makes sure that the type variable being unified
/// doesn't occur within the type it is being unified with. This
/// prevents the algorithm from inferring recursive types, which
/// could cause naively-implemented type checking to diverge.
/// While traversing the type tree, this function also takes care
/// of updating the levels of the type variables appearing within
/// the type, thus ensuring the type will be correctly generalized.
///
fn update_levels(typ: &Type, own_level: usize, own_id: usize) -> Result<(), UnifyError> {
    if let Type::Var { typ } = &typ {
        let new_value = match &*typ.borrow() {
            TypeVar::Link { typ, .. } => return update_levels(typ, own_level, own_id),

            TypeVar::Unbound { id, level } => {
                if id == &own_id {
                    return Err(UnifyError::RecursiveType);
                } else if *level > own_level {
                    Some(TypeVar::Unbound {
                        id: *id,
                        level: own_level,
                    })
                } else {
                    return Ok(());
                }
            }

            TypeVar::Generic { .. } => return Ok(()),
        };

        if let Some(t) = new_value {
            *typ.borrow_mut() = t;
        }
        return Ok(());
    }

    match typ {
        Type::App { args, .. } => {
            for arg in args.iter() {
                update_levels(arg, own_level, own_id)?
            }
            Ok(())
        }

        Type::AnonStruct { elems, .. } => {
            for elem in elems.iter() {
                update_levels(elem, own_level, own_id)?
            }
            Ok(())
        }

        Type::Fn { args, retrn } => {
            for arg in args.iter() {
                update_levels(arg, own_level, own_id)?;
            }
            update_levels(retrn, own_level, own_id)
        }

        Type::Map { row, .. } => update_levels(row, own_level, own_id),

        Type::Var { .. } => unreachable!(),

        Type::RowCons { head, tail, .. } => {
            update_levels(head, own_level, own_id)?;
            update_levels(tail, own_level, own_id)
        }

        Type::RowNil { .. } => Ok(()),
    }
}

fn match_fun_type(
    typ: &Type,
    arity: usize,
    env: &mut Env,
) -> Result<(Vec<Type>, Type), MatchFunTypeError> {
    if let Type::Var { typ } = &typ {
        let new_value = match &*typ.borrow() {
            TypeVar::Link { typ, .. } => return match_fun_type(typ, arity, env),

            TypeVar::Unbound { level, .. } => {
                let args: Vec<_> = (0..arity).map(|_| env.new_unbound_var(*level)).collect();
                let retrn = env.new_unbound_var(*level);
                Some((args, retrn))
            }

            TypeVar::Generic { .. } => None,
        };

        if let Some((args, retrn)) = new_value {
            *typ.borrow_mut() = TypeVar::Link {
                typ: Box::new(Type::Fn {
                    args: args.clone(),
                    retrn: Box::new(retrn.clone()),
                }),
            };
            return Ok((args, retrn));
        }
    }

    if let Type::Fn { args, retrn } = typ {
        return if args.len() != arity {
            Err(MatchFunTypeError::IncorrectArity {
                expected: args.len(),
                given: arity,
            })
        } else {
            Ok((args.clone(), (**retrn).clone()))
        };
    }

    Err(MatchFunTypeError::NotFn { typ: typ.clone() })
}

enum MatchFunTypeError {
    IncorrectArity { expected: usize, given: usize },
    NotFn { typ: Type },
}

fn convert_not_fun_error(e: MatchFunTypeError, fn_meta: &Meta, call_meta: &Meta) -> Error {
    match e {
        MatchFunTypeError::IncorrectArity { expected, given } => Error::IncorrectArity {
            meta: call_meta.clone(),
            expected,
            given,
        },

        MatchFunTypeError::NotFn { typ } => Error::NotFn {
            meta: fn_meta.clone(),
            typ,
        },
    }
}

/// Takes a level and a type and turns all type variables within the type that have
/// level higher than the input level into generalized (polymorphic) type variables.
///
fn generalise(t: Type, ctx_level: usize) -> Type {
    match t {
        Type::Var { typ } => {
            let new_var = match &*typ.borrow() {
                TypeVar::Unbound { id, level } => {
                    let id = *id;
                    if *level > ctx_level {
                        return Type::Var {
                            typ: Rc::new(RefCell::new(TypeVar::Generic { id })),
                        };
                    } else {
                        Some(TypeVar::Unbound { id, level: *level })
                    }
                }

                TypeVar::Link { typ } => return generalise((**typ).clone(), ctx_level),

                TypeVar::Generic { .. } => None,
            };

            if let Some(v) = new_var {
                *typ.borrow_mut() = v;
            }
            Type::Var { typ }
        }

        Type::App {
            public,
            module,
            name,
            args,
        } => {
            let args = args.into_iter().map(|t| generalise(t, ctx_level)).collect();
            Type::App {
                public,
                module,
                name,
                args,
            }
        }

        Type::Fn { args, retrn } => {
            let args = args.into_iter().map(|t| generalise(t, ctx_level)).collect();
            let retrn = generalise(*retrn, ctx_level);
            Type::Fn {
                args,
                retrn: Box::new(retrn),
            }
        }

        Type::AnonStruct { elems } => Type::AnonStruct {
            elems: elems
                .into_iter()
                .map(|t| generalise(t, ctx_level))
                .collect(),
        },

        Type::Map { row } => Type::Map {
            row: Box::new(generalise(*row, ctx_level)),
        },

        Type::RowCons { label, head, tail } => Type::RowCons {
            label,
            head: Box::new(generalise(*head, ctx_level)),
            tail: Box::new(generalise(*tail, ctx_level)),
        },

        Type::RowNil => t,
    }
}

pub fn int() -> Type {
    Type::App {
        public: true,
        name: "Int".to_string(),
        module: vec![],
        args: vec![],
    }
}

pub fn float() -> Type {
    Type::App {
        args: vec![],
        public: true,
        name: "Float".to_string(),
        module: vec![],
    }
}

pub fn bool() -> Type {
    Type::App {
        args: vec![],
        public: true,
        name: "Bool".to_string(),
        module: vec![],
    }
}

pub fn string() -> Type {
    Type::App {
        args: vec![],
        public: true,
        name: "String".to_string(),
        module: vec![],
    }
}

pub fn list(t: Type) -> Type {
    Type::App {
        public: true,
        name: "List".to_string(),
        module: vec![],
        args: vec![t],
    }
}

#[cfg(test)]
pub fn map_nil() -> Type {
    Type::Map {
        row: Box::new(Type::RowNil),
    }
}

#[test]
fn infer_test() {
    struct Case {
        src: &'static str,
        typ: &'static str,
    }
    let cases = [
        Case {
            src: "True",
            typ: "Bool",
        },
        Case {
            src: "False",
            typ: "Bool",
        },
        Case {
            src: "1",
            typ: "Int",
        },
        Case {
            src: "-2",
            typ: "Int",
        },
        Case {
            src: "1.0",
            typ: "Float",
        },
        Case {
            src: "-8.0",
            typ: "Float",
        },
        Case {
            src: "\"ok\"",
            typ: "String",
        },
        Case {
            src: "\"ok\"",
            typ: "String",
        },
        Case {
            src: "[]",
            typ: "List(a)",
        },
        Case {
            src: "4 % 1",
            typ: "Int",
        },
        Case {
            src: "4 > 1",
            typ: "Bool",
        },
        Case {
            src: "4 >= 1",
            typ: "Bool",
        },
        Case {
            src: "4 <= 1",
            typ: "Bool",
        },
        Case {
            src: "4 < 1",
            typ: "Bool",
        },
        /* Assignments

        */
        Case {
            src: "let x = 1 2",
            typ: "Int",
        },
        Case {
            src: "let x = 1 x",
            typ: "Int",
        },
        Case {
            src: "let x = 2.0 x",
            typ: "Float",
        },
        Case {
            src: "let x = 2 let y = x y",
            typ: "Int",
        },
        /* Lists

        */
        Case {
            src: "[]",
            typ: "List(a)",
        },
        Case {
            src: "[1]",
            typ: "List(Int)",
        },
        Case {
            src: "[1, 2, 3]",
            typ: "List(Int)",
        },
        Case {
            src: "[[]]",
            typ: "List(List(a))",
        },
        Case {
            src: "[[1.0, 2.0]]",
            typ: "List(List(Float))",
        },
        Case {
            src: "[fn(x) { x }]",
            typ: "List(fn(a) -> a)",
        },
        Case {
            src: "[fn(x) { x + 1 }]",
            typ: "List(fn(Int) -> Int)",
        },
        Case {
            src: "[struct([], [])]",
            typ: "List(struct(List(a), List(b)))",
        },
        Case {
            src: "[fn(x) { x }, fn(x) { x + 1 }]",
            typ: "List(fn(Int) -> Int)",
        },
        Case {
            src: "[fn(x) { x + 1 }, fn(x) { x }]",
            typ: "List(fn(Int) -> Int)",
        },
        Case {
            src: "[[], []]",
            typ: "List(List(a))",
        },
        Case {
            src: "[[], [1]]",
            typ: "List(List(Int))",
        },
        Case {
            src: "[1 | [2 | []]]",
            typ: "List(Int)",
        },
        Case {
            src: "[fn(x) { x } | []]",
            typ: "List(fn(a) -> a)",
        },
        Case {
            src: "let f = fn(x) { x } [f, f]",
            typ: "List(fn(a) -> a)",
        },
        Case {
            src: "let x = [1 | []] [2 | x]",
            typ: "List(Int)",
        },
        /* AnonStructs

        */
        Case {
            src: "struct(1)",
            typ: "struct(Int)",
        },
        Case {
            src: "struct(1, 2.0)",
            typ: "struct(Int, Float)",
        },
        Case {
            src: "struct(1, 2.0, 3)",
            typ: "struct(Int, Float, Int)",
        },
        Case {
            src: "struct(1, 2.0, struct(1, 1))",
            typ: "struct(Int, Float, struct(Int, Int))",
        },
        /* Fns

        */
        Case {
            src: "fn(x) { x }",
            typ: "fn(a) -> a",
        },
        Case {
            src: "fn(x) { x }",
            typ: "fn(a) -> a",
        },
        Case {
            src: "fn(x, y) { x }",
            typ: "fn(a, b) -> a",
        },
        Case {
            src: "fn(x, y) { [] }",
            typ: "fn(a, b) -> List(c)",
        },
        Case {
            src: "let x = 1.0 1",
            typ: "Int",
        },
        Case {
            src: "let id = fn(x) { x } id(1)",
            typ: "Int",
        },
        Case {
            src: "let x = fn() { 1.0 } x()",
            typ: "Float",
        },
        Case {
            src: "fn(x) { x }(1)",
            typ: "Int",
        },
        Case {
            src: "fn() { 1 }",
            typ: "fn() -> Int",
        },
        Case {
            src: "fn() { 1.1 }",
            typ: "fn() -> Float",
        },
        Case {
            src: "fn(x) { 1.1 }",
            typ: "fn(a) -> Float",
        },
        Case {
            src: "fn(x) { x }",
            typ: "fn(a) -> a",
        },
        Case {
            src: "let x = fn(x) { 1.1 } x",
            typ: "fn(a) -> Float",
        },
        Case {
            src: "fn(x, y, z) { 1 }",
            typ: "fn(a, b, c) -> Int",
        },
        Case {
            src: "fn(x) { let y = x y }",
            typ: "fn(a) -> a",
        },
        Case {
            src: "fn(x) { struct(1, x) }",
            typ: "fn(a) -> struct(Int, a)",
        },
        Case {
            src: "let id = fn(x) { x } id(1)",
            typ: "Int",
        },
        Case {
            src: "let constant = fn(x) { fn(y) { x } } let one = constant(1) one(2.0)",
            typ: "Int",
        },
        Case {
            src: "fn(f) { f(1) }",
            typ: "fn(fn(Int) -> a) -> a",
        },
        Case {
            src: "fn(f, x) { f(x) }",
            typ: "fn(fn(a) -> b, a) -> b",
        },
        Case {
            src: "fn(f) { fn(x) { f(x) } }",
            typ: "fn(fn(a) -> b) -> fn(a) -> b",
        },
        Case {
            src: "fn(f) { fn(x) { fn(y) { f(x, y) } } }",
            typ: "fn(fn(a, b) -> c) -> fn(a) -> fn(b) -> c",
        },
        Case {
            src: "fn(f) { fn(x, y) { f(x)(y) } }",
            typ: "fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c",
        },
        Case {
            src: "fn(f) { fn(x) { let ff = f ff(x) } }",
            typ: "fn(fn(a) -> b) -> fn(a) -> b",
        },
        Case {
            src: "fn(f) { fn(x, y) { let ff = f(x) ff(y) } }",
            typ: "fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c",
        },
        Case {
            src: "fn(x) { fn(y) { x } }",
            typ: "fn(a) -> fn(b) -> a",
        },
        Case {
            src: "fn(f) { f() }",
            typ: "fn(fn() -> a) -> a",
        },
        Case {
            src: "fn(f, x) { f(f(x)) }",
            typ: "fn(fn(a) -> a, a) -> a",
        },
        Case {
            src: "fn(x, y) { struct(x, y) }",
            typ: "fn(a, b) -> struct(a, b)",
        },
        Case {
            src: "fn(x) { struct(x, x) }",
            typ: "fn(a) -> struct(a, a)",
        },
        Case {
            src: "let id = fn(a) { a } fn(x) { x(id) }",
            typ: "fn(fn(fn(a) -> a) -> b) -> b",
        },
        Case {
            src: "let add = fn(x, y) { x + y } add(_, 2)",
            typ: "fn(Int) -> Int",
        },
        /* Maps

        */
        Case {
            src: "{}",
            typ: "{}",
        },
        Case {
            src: "{{} | a = 1}",
            typ: "{ a = Int }",
        },
        Case {
            src: "{a = 1}",
            typ: "{ a = Int }",
        },
        Case {
            src: "{a = 1, b = 2}",
            typ: "{ a = Int, b = Int }",
        },
        Case {
            src: "{a = 1, b = 2.0, c = -1}",
            typ: "{ a = Int, b = Float, c = Int }",
        },
        Case {
            src: "{a = {a = 1}}",
            typ: "{ a = { a = Int } }",
        },
        Case {
            src: "{} == {}",
            typ: "Bool",
        },
        Case {
            src: "{ a = 1 } == { a = 2 }",
            typ: "Bool",
        },
        Case {
            src: "{a = 1, b = 1} == {a = 1, b = 1}",
            typ: "Bool",
        },
        Case {
            src: "{a = fn(x) { x }} == {a = fn(a) { a }}",
            typ: "Bool",
        },
        Case {
            src: "{b = 1, a = 1} == {a = 1, b = 1}",
            typ: "Bool",
        },
        Case {
            src: "{{a = 1.0} | a = 1}",
            typ: "{ a = Int }",
        },
        Case {
            src: "let a = {} {a | b = 1}",
            typ: "{ b = Int }",
        },
        Case {
            src: "fn(r) { { r | x = 1 } }",
            typ: "fn({ a |  }) -> { a | x = Int }",
        },
        Case {
            src: "{{} | a = 1}",
            typ: "{ a = Int }",
        },
        /* case

        */
        Case {
            src: "case 1 { | a -> 1 }",
            typ: "Int",
        },
        Case {
            src: "case 1 { | a -> 1.0 | b -> 2.0 | c -> 3.0 }",
            typ: "Float",
        },
        Case {
            src: "case 1 { | a -> a }",
            typ: "Int",
        },
        Case {
            src: "case 1 { | 1 -> 10 | 2 -> 20 | x -> x * 10 }",
            typ: "Int",
        },
        Case {
            src: "case 2.0 { | 2.0 -> 1 | x -> 0 }",
            typ: "Int",
        },
        Case {
            src: r#"case "ok" { | "ko" -> 1 | x -> 0 }"#,
            typ: "Int",
        },
        /* let

        */
        Case {
            src: "let struct(tag, x) = struct(1.0, 1) x",
            typ: "Int",
        },
        Case {
            src: "let [] = [] 1",
            typ: "Int",
        },
        Case {
            src: "let [a] = [1] a",
            typ: "Int",
        },
        Case {
            src: "let [a, 2] = [1] a",
            typ: "Int",
        },
        Case {
            src: "let [a | [b | []]] = [1] a",
            typ: "Int",
        },
        Case {
            src: "fn(x) { let [a] = x a }",
            typ: "fn(List(a)) -> a",
        },
        Case {
            src: "fn(x) { let [a] = x a + 1 }",
            typ: "fn(List(Int)) -> Int",
        },
        Case {
            src: "fn(x) { let struct(a, b) = x a }",
            typ: "fn(struct(a, b)) -> a",
        },
        Case {
            src: "let _x = 1 2.0",
            typ: "Float",
        },
        Case {
            src: "let _ = 1 2.0",
            typ: "Float",
        },
        /* Map select

        */
        Case {
            src: "{a = 1, b = 2.0}.b",
            typ: "Float",
        },
        Case {
            src: "let r = {a = 1, b = 2} r.a",
            typ: "Int",
        },
        Case {
            src: "let r = {a = 1, b = 2} r.a + r.b",
            typ: "Int",
        },
        Case {
            src: "fn(x) { x.t }",
            typ: "fn({ b | t = a }) -> a",
        },
        Case {
            src: "let f = fn(x) { x } let r = {a = f} r.a",
            typ: "fn(a) -> a",
        },
        Case {
            src: "let r = {a = fn(x) { x }} r.a",
            typ: "fn(a) -> a",
        },
        Case {
            src: "let a = {b = 1} {a | b = 1.0}.b",
            typ: "Float",
        },
        Case {
            src: "let r = {a = fn(x) { x }, b = fn(x) { x }} [r.a, r.b]",
            typ: "List(fn(a) -> a)",
        },
        Case {
            src: "let f = fn(x) { x.t } f({ t = 1 })",
            typ: "Int",
        },
        Case {
            src: "let f = fn(x) { x.t(1) } f({t = fn(x) { x + 1 }})",
            typ: "Int",
        },
        Case {
            src: "{a = 1, b = 2.0}.a",
            typ: "Int",
        },
        Case {
            src: "fn(r) { r.x }",
            typ: "fn({ b | x = a }) -> a",
        },
        Case {
            src: "fn(r) { r.x + 1 }",
            typ: "fn({ a | x = Int }) -> Int",
        },
    ];

    for Case { src, typ } in cases.into_iter() {
        let ast = crate::grammar::ExprParser::new()
            .parse(src)
            .expect("syntax error");
        let result =
            infer(ast, 1, &mut Env::new(&HashMap::new())).expect("should successfully infer");
        assert_eq!(
            (
                src,
                result
                    .typ()
                    .to_gleam_doc(&mut hashmap![], &mut 0)
                    .format(80)
            ),
            (src, typ.to_string()),
        );
    }
}

#[test]
fn infer_error_test() {
    struct Case {
        src: &'static str,
        error: Error,
    }

    let cases = [
        Case {
            src: "1 + 1.0",
            error: Error::CouldNotUnify {
                meta: Meta { start: 4, end: 7 },
                expected: int(),
                given: float(),
            },
        },
        Case {
            src: "1 +. 1.0",
            error: Error::CouldNotUnify {
                meta: Meta { start: 0, end: 1 },
                expected: float(),
                given: int(),
            },
        },
        Case {
            src: "1 == 1.0",
            error: Error::CouldNotUnify {
                meta: Meta { start: 5, end: 8 },
                expected: int(),
                given: float(),
            },
        },
        Case {
            src: "1 > 1.0",
            error: Error::CouldNotUnify {
                meta: Meta { start: 4, end: 7 },
                expected: int(),
                given: float(),
            },
        },
        Case {
            src: "1.0 >. 1",
            error: Error::CouldNotUnify {
                meta: Meta { start: 7, end: 8 },
                expected: float(),
                given: int(),
            },
        },
        Case {
            src: "x",
            error: Error::UnknownVariable {
                meta: Meta { start: 0, end: 1 },
                name: "x".to_string(),
                variables: Env::new(&HashMap::new()).variables,
            },
        },
        Case {
            src: "x",
            error: Error::UnknownVariable {
                meta: Meta { start: 0, end: 1 },
                name: "x".to_string(),
                variables: Env::new(&HashMap::new()).variables,
            },
        },
        Case {
            src: "let id = fn(x) { x } id()",
            error: Error::IncorrectArity {
                meta: Meta { start: 21, end: 25 },
                expected: 1,
                given: 0,
            },
        },
        Case {
            src: "let id = fn(x) { x } id(1, 2)",
            error: Error::IncorrectArity {
                meta: Meta { start: 21, end: 29 },
                expected: 1,
                given: 2,
            },
        },
        Case {
            src: "case 1 { | a -> 1 | b -> 2.0 }",
            error: Error::CouldNotUnify {
                meta: Meta { start: 25, end: 28 },
                expected: int(),
                given: float(),
            },
        },
        Case {
            src: "case 1.0 { | 1 -> 1 }",
            error: Error::CouldNotUnify {
                meta: Meta { start: 13, end: 14 },
                expected: int(),
                given: float(),
            },
        },
        Case {
            src: "case 1 { | 1.0 -> 1 }",
            error: Error::CouldNotUnify {
                meta: Meta { start: 11, end: 14 },
                expected: float(),
                given: int(),
            },
        },
        Case {
            src: "struct(1, 2) == struct(1, 2, 3)",
            error: Error::CouldNotUnify {
                meta: Meta { start: 16, end: 31 },
                expected: Type::AnonStruct {
                    elems: vec![int(), int()],
                },
                given: Type::AnonStruct {
                    elems: vec![int(), int(), int()],
                },
            },
        },
        Case {
            src: "{} == {a = 2}",
            error: Error::ExtraField {
                meta: Meta { start: 11, end: 12 },
                label: "a".to_string(),
                container_typ: RowContainerType::Map,
            },
        },
        Case {
            src: "fn() { 1 } == fn(x) { x + 1 }",
            error: Error::CouldNotUnify {
                meta: Meta { start: 14, end: 29 },
                expected: Type::Fn {
                    args: vec![],
                    retrn: Box::new(int()),
                },
                given: Type::Fn {
                    args: vec![Type::Var {
                        typ: Rc::new(RefCell::new(TypeVar::Link {
                            typ: Box::new(int()),
                        })),
                    }],
                    retrn: Box::new(int()),
                },
            },
        },
        Case {
            src: "let f = fn(x: Int) { x } f(1.0)",
            error: Error::CouldNotUnify {
                meta: Meta { start: 27, end: 30 },
                expected: Type::App {
                    public: true,
                    module: vec![],
                    name: "Int".to_string(),
                    args: vec![],
                },
                given: Type::App {
                    public: true,
                    module: vec![],
                    name: "Float".to_string(),
                    args: vec![],
                },
            },
        },
        Case {
            src: "case 1 { | x -> 1 | 1 -> x }",
            error: Error::UnknownVariable {
                meta: Meta { start: 25, end: 26 },
                name: "x".to_string(),
                variables: Env::new(&HashMap::new()).variables,
            },
        },
        Case {
            src: "let id = fn(x) { x(x) } 1",
            error: Error::RecursiveType {
                meta: Meta { start: 19, end: 20 },
            },
        },
    ];

    for Case { src, error } in cases.into_iter() {
        let ast = crate::grammar::ExprParser::new()
            .parse(src)
            .expect("syntax error");
        let result =
            infer(ast, 1, &mut Env::new(&HashMap::new())).expect_err("should infer an error");
        assert_eq!((src, error), (src, &result));
    }
}

#[test]
fn infer_module_test() {
    struct Case {
        src: &'static str,
        module: Vec<(&'static str, &'static str)>,
    }

    let cases = [
        Case {
            src: "
        pub fn repeat(i, x) {
          case i {
          | 0 -> []
          | i -> [x | repeat(i - 1, x)]
          }
        }",
            module: vec![("repeat", "fn(Int, a) -> List(a)")],
        },
        Case {
            src: "fn private() { 1 }
                  pub fn public() { 1 }",
            module: vec![("public", "fn() -> Int")],
        },
        Case {
            src: "fn empty() { {} }
                  pub fn run() { { empty() | level = 1 } }",
            module: vec![("run", "fn() -> { level = Int }")],
        },
        Case {
            src: "pub fn ok(x) { struct(1, x) }",
            module: vec![("ok", "fn(a) -> struct(Int, a)")],
        },
        Case {
            src: "pub fn empty() {
                    let map = {}
                    map
                  }",
            module: vec![("empty", "fn() -> {}")],
        },
        Case {
            src: "pub fn add_name(map, name) {
                    let map = { map | name = name }
                    map
                  }",
            module: vec![("add_name", "fn({ a |  }, b) -> { a | name = b }")],
        },
        Case {
            src: "
                pub enum Is = | Yes | No
                pub fn yes() { Yes }
                pub fn no() { No }",
            module: vec![
                ("No", "Is"),
                ("Yes", "Is"),
                ("no", "fn() -> Is"),
                ("yes", "fn() -> Is"),
            ],
        },
        Case {
            src: "
                pub enum Num = | I(Int)
                pub fn one() { I(1) }",
            module: vec![("I", "fn(Int) -> Num"), ("one", "fn() -> Num")],
        },
        Case {
            src: "
                pub fn id(x) { x }
                pub fn float() { id(1.0) }
                pub fn int() { id(1) }",
            module: vec![
                ("float", "fn() -> Float"),
                ("id", "fn(a) -> a"),
                ("int", "fn() -> Int"),
            ],
        },
        Case {
            src: "
        pub enum Box(a) = | Box(a)
        pub fn int() { Box(1) }
        pub fn float() { Box(1.0) }",
            module: vec![
                ("Box", "fn(a) -> Box(a)"),
                ("float", "fn() -> Box(Float)"),
                ("int", "fn() -> Box(Int)"),
            ],
        },
        Case {
            src: "
        pub enum Singleton = | Singleton
        pub fn go(x) { let Singleton = x 1 }",
            module: vec![("Singleton", "Singleton"), ("go", "fn(Singleton) -> Int")],
        },
        Case {
            src: "
        pub enum Box(a) = | Box(a)
        pub fn unbox(x) { let Box(a) = x a }",
            module: vec![("Box", "fn(a) -> Box(a)"), ("unbox", "fn(Box(a)) -> a")],
        },
        Case {
            src: "
        pub enum I = | I(Int)
        pub fn open(x) { case x { | I(i) -> i  } }",
            module: vec![("I", "fn(Int) -> I"), ("open", "fn(I) -> Int")],
        },
        Case {
            src: "pub fn status() { 1 }
                  pub fn list_of(x) { [x] }
                  pub fn get_age(person) { person.age }",
            module: vec![
                ("get_age", "fn({ b | age = a }) -> a"),
                ("list_of", "fn(a) -> List(a)"),
                ("status", "fn() -> Int"),
            ],
        },
        Case {
            src: "pub external fn go(String) -> String = \"\" \"\"",
            module: vec![("go", "fn(String) -> String")],
        },
        Case {
            src: "pub external fn go(Int) -> Float = \"\" \"\"",
            module: vec![("go", "fn(Int) -> Float")],
        },
        Case {
            src: "pub external fn go(Int) -> Int = \"\" \"\"",
            module: vec![("go", "fn(Int) -> Int")],
        },
        Case {
            src: "external fn go(Int) -> Int = \"\" \"\"",
            module: vec![],
        },
        Case {
            src: "pub external fn ok(Int) -> struct(Int, Int) = \"\" \"\"",
            module: vec![("ok", "fn(Int) -> struct(Int, Int)")],
        },
        Case {
            src: "pub external fn ok() -> fn(Int) -> Int = \"\" \"\"",
            module: vec![("ok", "fn() -> fn(Int) -> Int")],
        },
        Case {
            src: "pub external fn go(Int) -> b = \"\" \"\"",
            module: vec![("go", "fn(Int) -> a")],
        },
        Case {
            src: "pub external fn go(Bool) -> b = \"\" \"\"",
            module: vec![("go", "fn(Bool) -> a")],
        },
        Case {
            src: "pub external fn go(List(a)) -> a = \"\" \"\"",
            module: vec![("go", "fn(List(a)) -> a")],
        },
        Case {
            src: "pub external fn go(struct(a, c)) -> c = \"\" \"\"",
            module: vec![("go", "fn(struct(a, b)) -> b")],
        },
        Case {
            src: "pub external fn ok() -> {} = \"\" \"\"",
            module: vec![("ok", "fn() -> {}")],
        },
        Case {
            src: "pub external fn ok() -> {a = Int, b = Int} = \"\" \"\"",
            module: vec![("ok", "fn() -> { a = Int, b = Int }")],
        },
        Case {
            src: "pub external fn ok() -> {a | a = Int} = \"\" \"\"",
            module: vec![("ok", "fn() -> { a | a = Int }")],
        },
        Case {
            src: "pub external fn ok() -> {a | } = \"\" \"\"",
            module: vec![("ok", "fn() -> { a |  }")],
        },
        Case {
            src: "
        external fn go(Int) -> b = \"\" \"\"
        pub fn x() {
          go(1)
        }",
            module: vec![("x", "fn() -> a")],
        },
        Case {
            src: "
        external fn id(a) -> a = \"\" \"\"
        pub fn i(x) { id(x) }
        pub fn a() { id(1) }
        pub fn b() { id(1.0) }",
            module: vec![
                ("a", "fn() -> Int"),
                ("b", "fn() -> Float"),
                ("i", "fn(a) -> a"),
            ],
        },
        Case {
            src: "pub external fn len(List(a)) -> Int = \"\" \"\"",
            module: vec![("len", "fn(List(a)) -> Int")],
        },
        Case {
            src: "
        pub external type Connection\n
        pub external fn is_open(Connection) -> Bool = \"\" \"\"",
            module: vec![("is_open", "fn(Connection) -> Bool")],
        },
        Case {
            src: "
        pub external type Pair(thing, thing)\n
        pub external fn pair(a) -> Pair(a, a) = \"\" \"\"",
            module: vec![("pair", "fn(a) -> Pair(a, a)")],
        },
        Case {
            src: "
pub fn one() { 1 }
pub fn zero() { one() - 1 }
pub fn two() { one() + zero() }",
            module: vec![
                ("one", "fn() -> Int"),
                ("two", "fn() -> Int"),
                ("zero", "fn() -> Int"),
            ],
        },
        Case {
            src: "
        pub fn one() { 1 }
        pub fn zero() { one() - 1 }
        pub fn two() { one() + zero() }",
            module: vec![
                ("one", "fn() -> Int"),
                ("two", "fn() -> Int"),
                ("zero", "fn() -> Int"),
            ],
        },
        // Type annotations
        Case {
            src: "pub fn go(x: Int) { x }",
            module: vec![("go", "fn(Int) -> Int")],
        },
        Case {
            src: "pub fn go(x: b) -> b { x }",
            module: vec![("go", "fn(a) -> a")],
        },
        Case {
            src: "pub fn go(x) -> b { x }",
            module: vec![("go", "fn(a) -> a")],
        },
        Case {
            src: "pub fn go(x: b) { x }",
            module: vec![("go", "fn(a) -> a")],
        },
        Case {
            src: "pub fn go(x: List(b)) -> List(b) { x }",
            module: vec![("go", "fn(List(a)) -> List(a)")],
        },
        Case {
            src: "pub fn go(x: List(b)) { x }",
            module: vec![("go", "fn(List(a)) -> List(a)")],
        },
        Case {
            src: "pub fn go(x: List(String)) { x }",
            module: vec![("go", "fn(List(String)) -> List(String)")],
        },
        Case {
            src: "pub fn go(x: b, y: c) { x }",
            module: vec![("go", "fn(a, b) -> a")],
        },
        Case {
            src: "pub fn go(x) -> Int { x }",
            module: vec![("go", "fn(Int) -> Int")],
        },
        // // Type aliases
        // Case {
        //     src: "
        // type Html = String
        // pub fn go() { 1 }",
        //     module: vec![("go", "fn() -> Int")],
        // },
        Case {
            src: "pub fn length(list) {
                    case list {
                    | [] -> 0
                    | [x | xs] -> length(xs) + 1
                    }
                  }",
            module: vec![("length", "fn(List(a)) -> Int")],
        },
        Case {
            src: "external fn the_map({a = Int, b = Int}) -> Int = \"\" \"\"
            pub fn go(map) {
                    let a = map.a
                    the_map(map)
                  }",
            module: vec![("go", "fn({ a = Int, b = Int }) -> Int")],
        },
        // % TODO: mutual recursion
        //    // % {
        //    // %pub fn length(list) {\n
        //    // %  case list {\n
        //    // %  | [] -> 0\n
        //    // %  | _ :: tail -> helper_length(tail) + 1\n
        //    // %  }\n
        //    // %}
        //    // %fn helper_length(list) { length(list) }
        //    // %   ,
        //    // %module {
        //    // % fn length(List(a)) -> Int
        //    // %}
        //    // % }
    ];

    for Case { src, module } in cases.into_iter() {
        let ast = crate::grammar::ModuleParser::new()
            .parse(src)
            .expect("syntax error");
        let result = infer_module(ast, &HashMap::new()).expect("should successfully infer");
        let mut constructors: Vec<(_, _)> = result
            .type_info
            .value_constructors
            .iter()
            .map(|(k, v)| (k.clone(), v.typ.pretty_print(0)))
            .collect();
        constructors.sort();
        let expected: Vec<_> = module
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();
        assert_eq!((src, constructors), (src, expected));
    }
}

#[test]
fn infer_module_error_test() {
    struct Case {
        src: &'static str,
        error: Error,
    }

    let cases = [
        Case {
            src: "fn go() { 1 + 2.0 }",
            error: Error::CouldNotUnify {
                meta: Meta { start: 14, end: 17 },
                expected: int(),
                given: float(),
            },
        },
        Case {
            src: "
fn id(x: a, y: a) { x }
pub fn x() { id(1, 1.0) }
                ",
            error: Error::CouldNotUnify {
                meta: Meta { start: 44, end: 47 },
                expected: int(),
                given: float(),
            },
        },
        Case {
            src: "external fn go(List(a, b)) -> a = \"\" \"\"",
            error: Error::IncorrectTypeArity {
                meta: Meta { start: 15, end: 25 },
                name: "List".to_string(),
                expected: 1,
                given: 2,
            },
        },
        Case {
            src: "fn dupe() { 1 }
                  fn dupe() { 2 }",
            error: Error::DuplicateName {
                meta: Meta { start: 34, end: 49 },
                name: "dupe".to_string(),
            },
        },
        Case {
            src: "fn dupe() { 1 }
                  fn dupe(x) { x }",
            error: Error::DuplicateName {
                meta: Meta { start: 34, end: 50 },
                name: "dupe".to_string(),
            },
        },
        Case {
            src: r#"external type PrivateType
                    pub external fn leak_type() -> PrivateType = "" """#,
            error: Error::PrivateTypeLeak {
                meta: Meta { start: 46, end: 96 },
                leaked: Type::App {
                    args: vec![],
                    public: false,
                    module: vec![],
                    name: "PrivateType".to_string(),
                },
            },
        },
        Case {
            src: r#"external type PrivateType
                    external fn go() -> PrivateType = "" ""
                    pub fn leak_type() { go() }"#,
            error: Error::PrivateTypeLeak {
                meta: Meta {
                    start: 106,
                    end: 133,
                },
                leaked: Type::App {
                    args: vec![],
                    public: false,
                    module: vec![],
                    name: "PrivateType".to_string(),
                },
            },
        },
        Case {
            src: r#"external type PrivateType
                    external fn go() -> PrivateType = "" ""
                    pub fn leak_type() { [go()] }"#,
            error: Error::PrivateTypeLeak {
                meta: Meta {
                    start: 106,
                    end: 135,
                },
                leaked: Type::App {
                    args: vec![],
                    public: false,
                    module: vec![],
                    name: "PrivateType".to_string(),
                },
            },
        },
        Case {
            src: r#"external type PrivateType
                    pub external fn go(PrivateType) -> Int = "" """#,
            error: Error::PrivateTypeLeak {
                meta: Meta { start: 46, end: 92 },
                leaked: Type::App {
                    args: vec![],
                    public: false,
                    module: vec![],
                    name: "PrivateType".to_string(),
                },
            },
        },
        Case {
            src: r#"external type PrivateType
                    pub enum LeakType =
                      | Variant(PrivateType)"#,
            error: Error::PrivateTypeLeak {
                meta: Meta {
                    start: 90,
                    end: 110,
                },
                leaked: Type::App {
                    args: vec![],
                    public: false,
                    module: vec![],
                    name: "PrivateType".to_string(),
                },
            },
        },
    ];

    for Case { src, error } in cases.into_iter() {
        let ast = crate::grammar::ModuleParser::new()
            .parse(src)
            .expect("syntax error");
        let result = infer_module(ast, &HashMap::new()).expect_err("should infer an error");
        assert_eq!((src, error), (src, &result));
    }

    // Cases were we can't so easily check for equality- i.e. because the contents of the error are
    // non-deterministic.
    let cases = ["fn inc(x: a) { x + 1 }"];
    for src in cases.into_iter() {
        let ast = crate::grammar::ModuleParser::new()
            .parse(src)
            .expect("syntax error");
        infer_module(ast, &HashMap::new()).expect_err("should infer an error");
    }
}
