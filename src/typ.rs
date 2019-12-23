use crate::ast::{
    self, Arg, ArgNames, BinOp, CallArg, Clause, Expr, Meta, Pattern, Statement, StructField,
    TypeAst, TypedExpr, TypedModule, TypedPattern, UnqualifiedImport, UntypedExpr, UntypedModule,
    UntypedPattern,
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

    Fn {
        args: Vec<Type>,
        retrn: Box<Type>,
    },

    Var {
        typ: Rc<RefCell<TypeVar>>,
    },

    AnonStruct {
        elems: Vec<Type>,
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
        let mut initial_indent = initial_indent;
        b.to_doc()
            .append(self.to_gleam_doc(&mut im::hashmap![], &mut initial_indent))
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
                if args.is_empty() {
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

            Type::Var { typ, .. } => typ.borrow().to_gleam_doc(names, uid),

            Type::AnonStruct { elems, .. } => {
                args_to_gleam_doc(elems, names, uid).surround("struct(", ")")
            }
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
        module: &[String],
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
                if *module == m[..] && name == n && args.len() == arity {
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
                                module: module.to_owned(),
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
            Type::App { public: false, .. } => Some(self.clone()),

            Type::App { args, .. } => args.iter().find_map(|t| t.find_private_type()),

            Type::AnonStruct { elems, .. } => elems.iter().find_map(|t| t.find_private_type()),

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
        rest /= alphabet_length;
        chars.push((n as u8 + char_offset) as char);

        if rest == 0 {
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
    macro_rules! assert_string {
        ($src:expr, $typ:expr $(,)?) => {
            assert_eq!(
                $typ.to_string(),
                $src.to_gleam_doc(&mut hashmap![], &mut 0).format(80)
            );
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
    );
    assert_string!(
        Type::Var {
            typ: Rc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 2231 })),
        },
        "a",
    );
    assert_string!(
        Type::Fn {
            args: vec![Type::Var {
                typ: Rc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 78 })),
            }],
            retrn: Box::new(Type::Var {
                typ: Rc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 2 })),
            }),
        },
        "fn(a) -> b",
    );
    assert_string!(
        Type::Fn {
            args: vec![Type::Var {
                typ: Rc::new(RefCell::new(TypeVar::Generic { id: 78 })),
            }],
            retrn: Box::new(Type::Var {
                typ: Rc::new(RefCell::new(TypeVar::Generic { id: 2 })),
            }),
        },
        "fn(a) -> b",
    );
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldMap {
    arity: usize,
    fields: HashMap<String, usize>,
}

pub struct DuplicateField {}

impl FieldMap {
    pub fn new(arity: usize) -> Self {
        Self {
            arity,
            fields: HashMap::new(),
        }
    }

    pub fn insert(&mut self, label: String, index: usize) -> Result<(), DuplicateField> {
        match self.fields.insert(label, index) {
            Some(_) => Err(DuplicateField {}),
            None => Ok(()),
        }
    }

    pub fn into_option(self) -> Option<Self> {
        if self.fields.is_empty() {
            None
        } else {
            Some(self)
        }
    }

    /// Reorder an argument list so that labelled fields supplied out-of-order are in the correct
    /// order.
    ///
    fn reorder<A>(&self, args: &mut Vec<CallArg<A>>, meta: &Meta) -> Result<(), Error> {
        let mut labelled_arguments_given = false;
        let mut seen = std::collections::HashSet::new();

        if self.arity != args.len() {
            return Err(Error::IncorrectArity {
                meta: meta.clone(),
                expected: self.arity,
                given: args.len(),
            });
        }

        for i in 0..args.len() {
            let (label, meta) = match &args[i].label {
                // A labelled argument, we may need to reposition it in the array vector
                Some(l) => {
                    labelled_arguments_given = true;
                    (l, &args[i].meta)
                }

                // Not a labelled argument
                None => {
                    if labelled_arguments_given {
                        return Err(Error::PositionalArgumentAfterLabelled {
                            meta: args[i].meta.clone(),
                        });
                    }
                    continue;
                }
            };

            let position = match self.fields.get(label) {
                None => {
                    return Err(Error::UnknownLabel {
                        meta: meta.clone(),
                        labels: self.fields.clone(),
                        label: label.to_string(),
                    })
                }
                Some(p) => p,
            };

            if *position == i {
                continue;
            }

            if seen.contains(position) {
                return Err(Error::DuplicateArgument {
                    meta: meta.clone(),
                    label: label.to_string(),
                });
            }

            seen.insert(*position);
            args.swap(*position, i);
        }
        Ok(())
    }
}

#[test]
fn field_map_reorder_test() {
    let int = |value| Expr::Int {
        value,
        typ: (),
        meta: Meta { start: 0, end: 0 },
    };

    struct Case {
        arity: usize,
        fields: HashMap<String, usize>,
        args: Vec<CallArg<UntypedExpr>>,
        expected_result: Result<(), Error>,
        expected_args: Vec<CallArg<UntypedExpr>>,
    }

    impl Case {
        fn test(self) {
            let mut args = self.args;
            let fm = FieldMap {
                arity: self.arity,
                fields: self.fields,
            };
            let meta = &Meta { start: 0, end: 0 };
            assert_eq!(self.expected_result, fm.reorder(&mut args, meta));
            assert_eq!(self.expected_args, args);
        }
    }

    Case {
        arity: 0,
        fields: HashMap::new(),
        args: vec![],
        expected_result: Ok(()),
        expected_args: vec![],
    }
    .test();

    Case {
        arity: 3,
        fields: HashMap::new(),
        args: vec![
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(1),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(2),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(3),
            },
        ],
        expected_result: Ok(()),
        expected_args: vec![
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(1),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(2),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(3),
            },
        ],
    }
    .test();

    Case {
        arity: 3,
        fields: [("last".to_string(), 2)].iter().cloned().collect(),
        args: vec![
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(1),
            },
            CallArg {
                meta: Default::default(),
                label: Some("last".to_string()),
                value: int(2),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(3),
            },
        ],
        expected_result: Ok(()),
        expected_args: vec![
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(1),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(3),
            },
            CallArg {
                meta: Default::default(),
                label: Some("last".to_string()),
                value: int(2),
            },
        ],
    }
    .test();

    Case {
        arity: 3,
        fields: [("last".to_string(), 2)].iter().cloned().collect(),
        args: vec![
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(1),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(2),
            },
            CallArg {
                meta: Default::default(),
                label: Some("last".to_string()),
                value: int(3),
            },
        ],
        expected_result: Ok(()),
        expected_args: vec![
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(1),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(2),
            },
            CallArg {
                meta: Default::default(),
                label: Some("last".to_string()),
                value: int(3),
            },
        ],
    }
    .test();
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueConstructorVariant {
    /// A locally defined variable or function parameter
    LocalVariable,

    /// A function belonging to the module
    ModuleFn {
        field_map: Option<FieldMap>,
        module: Vec<String>,
        arity: usize,
    },

    /// A constructor for a custom type
    CustomType {
        name: String,
        field_map: Option<FieldMap>,
        arity: usize,
    },
}

impl ValueConstructorVariant {
    fn to_module_value_constructor(&self) -> ModuleValueConstructor {
        match self {
            ValueConstructorVariant::CustomType { name, .. } => {
                ModuleValueConstructor::CustomType { name: name.clone() }
            }

            ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::ModuleFn { .. } => ModuleValueConstructor::Fn,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleValueConstructor {
    CustomType { name: String },
    Fn,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: Vec<String>,
    pub types: HashMap<String, TypeConstructor>,
    pub values: HashMap<String, ValueConstructor>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternConstructor {
    CustomType { name: String },
}

#[derive(Debug, Clone)]
pub struct Env<'a> {
    uid: usize,
    annotated_generic_types: im::HashSet<usize>,
    importable_modules: &'a HashMap<String, Module>,
    imported_modules: HashMap<String, Module>,

    // Values defined in the current function (or the prelude)
    // TODO: Once we look up names in env.module_values we don't need to store the constructor type
    // in here, it will always be a local variable.
    local_values: im::HashMap<String, ValueConstructor>,

    // Types defined in the current module (or the prelude)
    module_types: HashMap<String, TypeConstructor>,

    // Values defined in the current module
    module_values: HashMap<String, ValueConstructor>,
    // TODO:
    // Once we have tracked _all_ module values we can look up from here and the local scope,
    // meaning we don't need to insert into both. Is that better? I'm not sure yet.
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVar {
    Unbound { id: usize, level: usize },
    Link { typ: Box<Type> },
    Generic { id: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeConstructor {
    pub public: bool,
    pub module: Vec<String>,
    pub arity: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueConstructor {
    pub public: bool,
    pub origin: Meta,
    pub variant: ValueConstructorVariant,
    pub typ: Type,
}

impl ValueConstructor {
    fn field_map(&self) -> Option<&FieldMap> {
        match self.variant {
            ValueConstructorVariant::ModuleFn { ref field_map, .. }
            | ValueConstructorVariant::CustomType { ref field_map, .. } => field_map.as_ref(),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum NewTypeAction {
    Disallow,
    MakeGeneric,
}

impl<'a> Env<'a> {
    pub fn new(importable_modules: &'a HashMap<String, Module>) -> Self {
        let mut env = Self {
            uid: 0,
            annotated_generic_types: im::HashSet::new(),
            module_types: HashMap::new(),
            module_values: HashMap::new(),
            imported_modules: HashMap::new(),
            local_values: hashmap![],
            importable_modules,
        };

        env.insert_type_constructor(
            "Int".to_string(),
            TypeConstructor {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_variable(
            "True".to_string(),
            ValueConstructorVariant::CustomType {
                name: "True".to_string(),
                field_map: None,
                arity: 0,
            },
            bool(),
        );
        env.insert_variable(
            "False".to_string(),
            ValueConstructorVariant::CustomType {
                name: "False".to_string(),
                field_map: None,
                arity: 0,
            },
            bool(),
        );
        env.insert_type_constructor(
            "Bool".to_string(),
            TypeConstructor {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_type_constructor(
            "List".to_string(),
            TypeConstructor {
                arity: 1,
                module: vec![],
                public: true,
            },
        );

        env.insert_type_constructor(
            "Float".to_string(),
            TypeConstructor {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_type_constructor(
            "String".to_string(),
            TypeConstructor {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_type_constructor(
            "Result".to_string(),
            TypeConstructor {
                arity: 2,
                module: vec![],
                public: true,
            },
        );

        env.insert_variable(
            "Nil".to_string(),
            ValueConstructorVariant::CustomType {
                name: "Nil".to_string(),
                field_map: None,
                arity: 0,
            },
            Type::App {
                args: vec![],
                public: true,
                name: "Nil".to_string(),
                module: vec![],
            },
        );
        env.insert_type_constructor(
            "Nil".to_string(),
            TypeConstructor {
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
            ValueConstructorVariant::CustomType {
                name: "Ok".to_string(),
                field_map: None,
                arity: 1,
            },
            Type::Fn {
                args: vec![ok.clone()],
                retrn: Box::new(result(ok, error)),
            },
        );

        let ok = env.new_generic_var();
        let error = env.new_generic_var();
        env.insert_variable(
            "Error".to_string(),
            ValueConstructorVariant::CustomType {
                name: "Error".to_string(),
                field_map: None,
                arity: 1,
            },
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
        self.local_values.insert(
            name,
            ValueConstructor {
                public: false,
                origin: Default::default(), // TODO: remove
                variant,
                typ,
            },
        );
    }

    /// Lookup a variable in the current scope.
    ///
    pub fn get_variable(&self, name: &str) -> Option<&ValueConstructor> {
        self.local_values.get(name)
    }

    /// Map a type in the current scope.
    ///
    pub fn insert_type_constructor(&mut self, name: String, info: TypeConstructor) {
        self.module_types.insert(name, info);
    }

    /// Lookup a type in the current scope.
    ///
    pub fn get_type_constructor(
        &self,
        module_alias: &Option<String>,
        name: &str,
    ) -> Result<&TypeConstructor, GetTypeConstructorError> {
        match module_alias {
            None => {
                self.module_types
                    .get(name)
                    .ok_or_else(|| GetTypeConstructorError::UnknownType {
                        name: name.to_string(),
                        type_constructors: self.module_types.clone(),
                    })
            }

            Some(m) => {
                let module = &self.imported_modules.get(m).ok_or_else(|| {
                    GetTypeConstructorError::UnknownModule {
                        name: name.to_string(),
                        imported_modules: self.importable_modules.clone(),
                    }
                })?;
                module
                    .types
                    .get(name)
                    .ok_or_else(|| GetTypeConstructorError::UnknownModuleType {
                        name: name.to_string(),
                        module_name: module.name.clone(),
                        type_constructors: module.types.clone(),
                    })
            }
        }
    }

    /// Lookup a value constructor in the current scope.
    ///
    fn get_value_constructor(
        &self,
        module: Option<&String>,
        name: &str,
    ) -> Result<&ValueConstructor, GetValueConstructorError> {
        match module {
            None => {
                self.local_values.get(name).ok_or_else(|| {
                    GetValueConstructorError::UnknownVariable {
                        name: name.to_string(),
                        variables: self.local_values.clone(), // TODO: include module values too
                    }
                })
            }

            Some(module) => {
                let module = self.imported_modules.get(&*module).ok_or_else(|| {
                    GetValueConstructorError::UnknownModule {
                        name: name.to_string(),
                        imported_modules: self.importable_modules.clone(),
                    }
                })?;
                module.values.get(&*name).ok_or_else(|| {
                    GetValueConstructorError::UnknownModuleValue {
                        name: name.to_string(),
                        module_name: module.name.clone(),
                        value_constructors: module.values.clone(),
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
        ast: &TypeAst,
        vars: &mut im::HashMap<String, (usize, Type)>,
        new: NewTypeAction,
    ) -> Result<Type, Error> {
        match ast {
            TypeAst::Constructor {
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

            TypeAst::AnonStruct { elems, .. } => {
                let elems = elems
                    .iter()
                    .map(|t| self.type_from_ast(t, vars, new))
                    .collect::<Result<_, _>>()?;
                Ok(Type::AnonStruct { elems })
            }

            TypeAst::Fn { args, retrn, .. } => {
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

            TypeAst::Var { name, meta, .. } => match vars.get(name) {
                Some((_, var)) => Ok(var.clone()),

                None => match new {
                    NewTypeAction::MakeGeneric => {
                        let var = self.new_generic_var();
                        vars.insert(name.to_string(), (self.previous_uid(), var.clone()));
                        Ok(var)
                    }
                    NewTypeAction::Disallow => Err(Error::UnknownType {
                        name: name.to_string(),
                        meta: meta.clone(),
                        types: self.module_types.clone(),
                    }),
                },
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    UnknownLabel {
        meta: Meta,
        label: String,
        labels: HashMap<String, usize>,
    },

    UnknownVariable {
        meta: Meta,
        name: String,
        variables: im::HashMap<String, ValueConstructor>,
    },

    UnknownType {
        meta: Meta,
        name: String,
        types: HashMap<String, TypeConstructor>,
    },

    UnknownModule {
        meta: Meta,
        name: String,
        imported_modules: HashMap<String, Module>,
    },

    UnknownModuleType {
        meta: Meta,
        name: String,
        module_name: Vec<String>,
        type_constructors: HashMap<String, TypeConstructor>,
    },

    UnknownModuleValue {
        meta: Meta,
        name: String,
        module_name: Vec<String>,
        value_constructors: HashMap<String, ValueConstructor>,
    },

    UnknownModuleField {
        meta: Meta,
        name: String,
        module_name: Vec<String>,
        value_constructors: HashMap<String, ValueConstructor>,
        type_constructors: HashMap<String, TypeConstructor>,
    },

    NotFn {
        meta: Meta,
        typ: Type,
    },

    NotModule {
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

    DuplicateArgument {
        meta: Meta,
        label: String,
    },

    DuplicateField {
        meta: Meta,
        label: String,
    },

    PrivateTypeLeak {
        meta: Meta,
        leaked: Type,
    },

    UnexpectedLabelledArg {
        meta: Meta,
        label: String,
    },

    PositionalArgumentAfterLabelled {
        meta: Meta,
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
        imported_modules: HashMap<String, Module>,
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
        type_constructors: HashMap<String, TypeConstructor>,
    },

    UnknownModule {
        name: String,
        imported_modules: HashMap<String, Module>,
    },

    UnknownModuleType {
        name: String,
        module_name: Vec<String>,
        type_constructors: HashMap<String, TypeConstructor>,
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

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer_module(
    module: UntypedModule,
    modules: &HashMap<String, Module>,
) -> Result<TypedModule, Error> {
    let mut env = Env::new(modules);
    let module_name = &module.name;

    let statements: Vec<Statement<_, _, _, Type>> = module
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

                let mut field_map = FieldMap::new(args.len());
                for (i, arg) in args.iter().enumerate() {
                    if let ArgNames::NamedLabelled { label, .. } = &arg.names {
                        field_map
                            .insert(label.clone(), i)
                            .map_err(|_| Error::DuplicateField {
                                label: label.to_string(),
                                meta: meta.clone(),
                            })?;
                    }
                }
                let field_map = field_map.into_option();

                // Register a var for the function so that it can call itself recursively
                let rec = env.new_unbound_var(level + 1);
                env.insert_variable(
                    name.clone(),
                    ValueConstructorVariant::ModuleFn {
                        field_map: field_map.clone(),
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
                let typ = generalise(typ, level);

                // Insert the function into the module's interface
                // TODO
                env.module_values.insert(
                    name.clone(),
                    ValueConstructor {
                        public: public,
                        origin: meta.clone(),
                        typ: typ.clone(),
                        variant: ValueConstructorVariant::ModuleFn {
                            field_map: field_map.clone(),
                            module: module_name.clone(),
                            arity: args.len(),
                        },
                    },
                );

                // Insert the function into the environment
                env.insert_variable(
                    name.clone(),
                    ValueConstructorVariant::ModuleFn {
                        field_map,
                        module: module_name.clone(),
                        arity: args.len(),
                    },
                    typ,
                );

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
                let mut field_map = FieldMap::new(args.len());
                for (i, arg) in args.iter().enumerate() {
                    let t =
                        env.type_from_ast(&arg.typ, &mut type_vars, NewTypeAction::MakeGeneric)?;
                    args_types.push(t);
                    if let Some(label) = &arg.label {
                        field_map
                            .insert(label.clone(), i)
                            .map_err(|_| Error::DuplicateField {
                                label: label.to_string(),
                                meta: meta.clone(),
                            })?;
                    }
                }
                let field_map = field_map.into_option();
                let typ = Type::Fn {
                    args: args_types,
                    retrn: Box::new(retrn_type),
                };

                // Insert function into module's public interface
                // TODO
                env.module_values.insert(
                    name.clone(),
                    ValueConstructor {
                        public: public,
                        typ: typ.clone(),
                        origin: meta.clone(),
                        variant: ValueConstructorVariant::ModuleFn {
                            field_map: field_map.clone(),
                            module: module_name.clone(),
                            arity: args.len(),
                        },
                    },
                );

                // Insert function into module's internal scope
                env.insert_variable(
                    name.clone(),
                    ValueConstructorVariant::ModuleFn {
                        module: module_name.clone(),
                        arity: args.len(),
                        field_map,
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

            Statement::Struct {
                meta,
                public,
                name,
                type_args,
                fields,
            } => {
                // Register type
                env.insert_type_constructor(
                    name.clone(),
                    TypeConstructor {
                        module: module_name.clone(),
                        public,
                        arity: type_args.len(),
                    },
                );
                // Build return type and collect type vars that can be used by the constructor
                let mut type_vars = hashmap![];
                let type_args_types: Vec<_> = type_args
                    .iter()
                    .map(|arg| TypeAst::Var {
                        meta: meta.clone(),
                        name: arg.to_string(),
                    })
                    .map(|ast| env.type_from_ast(&ast, &mut type_vars, NewTypeAction::MakeGeneric))
                    .collect::<Result<_, _>>()?;

                let retrn = Type::App {
                    public,
                    module: module_name.clone(),
                    name: name.clone(),
                    args: type_args_types,
                };
                // Create FieldMap which later can be used to rewrite labelled arguments
                let mut field_map = FieldMap::new(fields.len());
                for (i, StructField { label, meta, .. }) in fields.iter().enumerate() {
                    field_map
                        .insert(label.clone(), i)
                        .map_err(|_| Error::DuplicateField {
                            label: label.to_string(),
                            meta: meta.clone(),
                        })?;
                }
                let constructor_variant = ValueConstructorVariant::CustomType {
                    name: name.clone(),
                    arity: fields.len(),
                    field_map: field_map.into_option(),
                };
                // Register constructor
                let args_types = fields
                    .iter()
                    .map(|StructField { typ: arg, .. }| {
                        env.type_from_ast(&arg, &mut type_vars, NewTypeAction::Disallow)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                // Insert constructor function into module scope
                let typ = match fields.len() {
                    0 => retrn.clone(),
                    _ => Type::Fn {
                        args: args_types,
                        retrn: Box::new(retrn.clone()),
                    },
                };
                // If the struct is public then record it so that it can be used in other modules
                // TODO
                env.module_values.insert(
                    name.clone(),
                    ValueConstructor {
                        public: public,
                        origin: meta.clone(),
                        typ: typ.clone(),
                        variant: constructor_variant.clone(),
                    },
                );
                env.insert_variable(name.clone(), constructor_variant, typ);
                Ok(Statement::Struct {
                    meta,
                    public,
                    name,
                    type_args,
                    fields,
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
                    TypeConstructor {
                        module: module_name.clone(),
                        public,
                        arity: args.len(),
                    },
                );
                // Build return type and collect type vars that can be used in constructors
                let mut type_vars = hashmap![];
                let args_types: Vec<_> = args
                    .iter()
                    .map(|arg| TypeAst::Var {
                        meta: meta.clone(),
                        name: arg.to_string(),
                    })
                    .map(|ast| env.type_from_ast(&ast, &mut type_vars, NewTypeAction::MakeGeneric))
                    .collect::<Result<_, _>>()?;

                let retrn = Type::App {
                    public,
                    module: module_name.clone(),
                    name: name.clone(),
                    args: args_types,
                };
                // Check and register constructors
                for constructor in constructors.iter() {
                    let mut field_map = FieldMap::new(constructor.args.len());
                    let mut args_types = Vec::with_capacity(constructor.args.len());
                    for (i, (label, arg)) in constructor.args.iter().enumerate() {
                        let t = env.type_from_ast(&arg, &mut type_vars, NewTypeAction::Disallow)?;
                        args_types.push(t);
                        if let Some(label) = label {
                            field_map.insert(label.clone(), i).map_err(|_| {
                                Error::DuplicateField {
                                    label: label.to_string(),
                                    meta: meta.clone(),
                                }
                            })?;
                        }
                    }
                    let field_map = field_map.into_option();
                    // Insert constructor function into module scope
                    let typ = match constructor.args.len() {
                        0 => retrn.clone(),
                        _ => Type::Fn {
                            args: args_types,
                            retrn: Box::new(retrn.clone()),
                        },
                    };
                    // TODO
                    env.module_values.insert(
                        constructor.name.clone(),
                        ValueConstructor {
                            public: public,
                            typ: typ.clone(),
                            origin: constructor.meta.clone(),
                            variant: ValueConstructorVariant::CustomType {
                                name: constructor.name.clone(),
                                arity: args.len(),
                                field_map: field_map.clone(),
                            },
                        },
                    );
                    env.insert_variable(
                        constructor.name.clone(),
                        ValueConstructorVariant::CustomType {
                            name: constructor.name.clone(),
                            arity: constructor.args.len(),
                            field_map,
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
                    TypeConstructor {
                        module: module_name.clone(),
                        public,
                        arity: args.len(),
                    },
                );
                // Check contained types are valid
                let mut type_vars = hashmap![];
                for arg in args.iter() {
                    let var = TypeAst::Var {
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
                unqualified,
            } => {
                // Find imported module
                let module_info = env.importable_modules.get(&module.join("/")).expect(
                    "COMPILER BUG: Typer could not find a module being imported.
This should not be possible. Please report this crash",
                );

                // Determine local alias of imported module
                let module_name = match &as_name {
                    None => module[module.len() - 1].clone(),
                    Some(name) => name.clone(),
                };

                // Insert unqualified imports into scope
                for UnqualifiedImport {
                    name,
                    meta,
                    as_name,
                } in &unqualified
                {
                    let mut imported = false;

                    let name = match &as_name {
                        None => name,
                        Some(alias) => alias,
                    };

                    if let Some(value) = module_info.values.get(name) {
                        env.insert_variable(name.clone(), value.variant.clone(), value.typ.clone());
                        imported = true;
                    }

                    if let Some(typ) = module_info.types.get(name) {
                        env.insert_type_constructor(name.clone(), typ.clone());
                        imported = true;
                    }

                    if !imported {
                        return Err(Error::UnknownModuleField {
                            meta: meta.clone(),
                            name: name.clone(),
                            module_name: module.clone(),
                            value_constructors: module_info.values.clone(),
                            type_constructors: module_info.types.clone(),
                        });
                    }
                }

                // Insert imported module into scope
                env.imported_modules
                    .insert(module_name, module_info.clone());

                Ok(Statement::Import {
                    meta,
                    module,
                    as_name,
                    unqualified,
                })
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    // Remove private and imported types and values to create the public interface
    env.module_types
        .retain(|_, info| info.public && &info.module == module_name);
    env.module_values.retain(|_, info| info.public);

    // Ensure no exported values have private types in their type signature
    for (_, value) in env.module_values.iter() {
        if let Some(leaked) = value.typ.find_private_type() {
            return Err(Error::PrivateTypeLeak {
                meta: value.origin.clone(),
                leaked,
            });
        }
    }

    Ok(ast::Module {
        name: module.name.clone(),
        statements,
        type_info: Module {
            name: module.name,
            types: env.module_types,
            values: env.module_values,
        },
    })
}

#[test]
fn infer_module_type_retention_test() {
    let module: UntypedModule = crate::ast::Module {
        name: vec!["ok".to_string()],
        statements: vec![],
        type_info: (),
    };

    let module = infer_module(module, &HashMap::new()).expect("Should infer OK");

    assert_eq!(
        module.type_info,
        Module {
            name: vec!["ok".to_string()],
            types: HashMap::new(), // Core type constructors like String and Int are not included
            values: HashMap::new(),
        }
    );
}

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer(expr: UntypedExpr, level: usize, env: &mut Env) -> Result<TypedExpr, Error> {
    match expr {
        Expr::Int { meta, value, .. } => Ok(Expr::Int {
            meta,
            value,
            typ: int(),
        }),

        Expr::Float { meta, value, .. } => Ok(Expr::Float {
            meta,
            value,
            typ: float(),
        }),

        Expr::String { meta, value, .. } => Ok(Expr::String {
            meta,
            value,
            typ: string(),
        }),

        Expr::Nil { meta, .. } => Ok(Expr::Nil {
            meta,
            typ: list(env.new_unbound_var(level)),
        }),

        Expr::Seq { first, then, .. } => {
            let first = infer(*first, level, env)?;
            let then = infer(*then, level, env)?;
            Ok(Expr::Seq {
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
            ..
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
            ..
        } => {
            let value = infer(*value, level + 1, env)?;
            let value_typ = generalise(value.typ().clone(), level + 1);
            let pattern = unify_pattern(pattern, &value_typ, level, env)?;
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
            subjects,
            clauses,
            ..
        } => {
            let subjects_count = subjects.len();
            let mut typed_subjects = Vec::with_capacity(subjects_count);
            let mut subject_types = Vec::with_capacity(subjects_count);
            let mut typed_clauses = Vec::with_capacity(clauses.len());

            let return_type = env.new_unbound_var(level); // TODO: should this be level + 1 ?

            for subject in subjects.into_iter() {
                let subject = infer(subject, level + 1, env)?;
                let subject_type = generalise(subject.typ().clone(), level + 1);
                typed_subjects.push(subject);
                subject_types.push(subject_type);
            }

            for clause in clauses.into_iter() {
                let vars = env.local_values.clone();
                if subjects_count != clause.patterns.len() {
                    panic!("incorrect number of patterns")
                }

                let mut typed_patterns = Vec::new();
                for (pattern, subject_type) in clause.patterns.into_iter().zip(subject_types.iter())
                {
                    let pattern = unify_pattern(pattern, &subject_type, level, env)?;
                    typed_patterns.push(pattern);
                }

                let then = infer(clause.then, level, env)?;
                unify(&return_type, then.typ(), env)
                    .map_err(|e| convert_unify_error(e, then.meta()))?;
                typed_clauses.push(Clause {
                    meta: clause.meta,
                    patterns: typed_patterns,
                    then,
                });

                env.local_values = vars;
            }
            Ok(Expr::Case {
                meta,
                typ: return_type,
                subjects: typed_subjects,
                clauses: typed_clauses,
            })
        }

        Expr::Cons {
            meta, head, tail, ..
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
            meta, fun, args, ..
        } => {
            let (fun, args, typ) = infer_call(*fun, args, level, &meta, env)?;
            Ok(Expr::Call {
                meta,
                typ,
                args,
                fun: Box::new(fun),
            })
        }

        Expr::AnonStruct { meta, elems, .. } => {
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
            ..
        } => {
            let fun = Expr::Var {
                meta: meta.clone(),
                constructor: (),
                name: bin_op_name(&name),
            };
            let args = vec![
                CallArg {
                    meta: Default::default(),
                    label: None,
                    value: *left,
                },
                CallArg {
                    meta: Default::default(),
                    label: None,
                    value: *right,
                },
            ];
            let (_fun, mut args, typ) = infer_call(fun, args, level, &meta, env)?;
            Ok(Expr::BinOp {
                meta,
                name,
                typ,
                right: Box::new(args.pop().unwrap().value),
                left: Box::new(args.pop().unwrap().value),
            })
        }

        Expr::Var { meta, name, .. } => {
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
            container,
            ..
        } => match &*container {
            Expr::Var { name, meta, .. } if !env.local_values.contains_key(name) => {
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
    module_alias: &str,
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

        let constructor =
            module_info
                .values
                .get(&label)
                .ok_or_else(|| Error::UnknownModuleValue {
                    name: label.clone(),
                    meta: select_meta.clone(),
                    module_name: module_info.name.clone(),
                    value_constructors: module_info.values.clone(),
                })?;

        (module_info.name.clone(), constructor.clone())
    };

    Ok(Expr::ModuleSelect {
        label,
        typ: instantiate(constructor.typ, level, &mut hashmap![], env),
        meta: select_meta,
        module_name,
        module_alias: module_alias.to_string(),
        constructor: constructor.variant.to_module_value_constructor(),
    })
}

fn infer_value_field_select(
    container: UntypedExpr,
    _label: String,
    level: usize,
    _meta: Meta,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    Err(Error::NotModule {
        meta: container.meta().clone(),
        typ: infer(container, level, env)?.typ().clone(),
    })
}

/// When we have an assignment or a case expression we unify the pattern with the
/// inferred type of the subject in order to determine what variables to insert
/// into the environment (or to detect a type error).
///
fn unify_pattern(
    pattern: UntypedPattern,
    typ: &Type,
    level: usize,
    env: &mut Env,
) -> Result<TypedPattern, Error> {
    match pattern {
        Pattern::Discard { meta } => Ok(Pattern::Discard { meta }),

        Pattern::Var { name, meta } => {
            env.insert_variable(
                name.to_string(),
                ValueConstructorVariant::LocalVariable,
                typ.clone(),
            );
            Ok(Pattern::Var { name, meta })
        }

        Pattern::Int { meta, value } => {
            unify(typ, &int(), env).map_err(|e| convert_unify_error(e, &meta))?;
            Ok(Pattern::Int { meta, value })
        }

        Pattern::Float { meta, value } => {
            unify(typ, &float(), env).map_err(|e| convert_unify_error(e, &meta))?;
            Ok(Pattern::Float { meta, value })
        }

        Pattern::String { meta, value } => {
            unify(typ, &string(), env).map_err(|e| convert_unify_error(e, &meta))?;
            Ok(Pattern::String { meta, value })
        }

        Pattern::Nil { meta } => {
            unify(typ, &list(env.new_unbound_var(level)), env)
                .map_err(|e| convert_unify_error(e, &meta))?;
            Ok(Pattern::Nil { meta })
        }

        Pattern::Cons { meta, head, tail } => match typ.get_app_args(true, &[], "List", 1, env) {
            Some(args) => {
                let head = Box::new(unify_pattern(*head, &args[0], level, env)?);
                let tail = Box::new(unify_pattern(*tail, typ, level, env)?);
                Ok(Pattern::Cons { meta, head, tail })
            }

            None => Err(Error::CouldNotUnify {
                given: list(env.new_unbound_var(level)),
                expected: typ.clone(),
                meta: meta.clone(),
            }),
        },

        Pattern::AnonStruct { elems, meta } => match typ.clone().collapse_links() {
            Type::AnonStruct { elems: type_elems } => {
                let elems = elems
                    .into_iter()
                    .zip(type_elems)
                    .map(|(pattern, typ)| unify_pattern(pattern, &typ, level, env))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Pattern::AnonStruct { elems, meta })
            }

            typ @ Type::Var { .. } => {
                let elems_types = (0..(elems.len()))
                    .map(|_| env.new_unbound_var(level))
                    .collect();
                unify(&Type::AnonStruct { elems: elems_types }, &typ, env)
                    .map_err(|e| convert_unify_error(e, &meta))?;
                unify_pattern(Pattern::AnonStruct { elems, meta }, &typ, level, env)
            }

            other => {
                dbg!(&other);
                unimplemented!();
            }
        },

        Pattern::Constructor {
            meta,
            module,
            name,
            args: mut pattern_args,
            ..
        } => {
            let cons = env
                .get_value_constructor(module.as_ref(), &name)
                .map_err(|e| convert_get_value_constructor_error(e, &meta))?;

            match cons.field_map() {
                // The fun has a field map so labelled arguments may be present and need to be reordered.
                Some(field_map) => field_map.reorder(&mut pattern_args, &meta)?,

                // The fun has no field map and so we error if arguments have been labelled
                None => assert_no_labelled_arguments(&pattern_args)?,
            }

            let constructor_typ = cons.typ.clone();
            let constructor = match cons.variant {
                ValueConstructorVariant::CustomType { ref name, .. } => {
                    PatternConstructor::CustomType { name: name.clone() }
                }
                ValueConstructorVariant::LocalVariable
                | ValueConstructorVariant::ModuleFn { .. } => panic!(
                    "Unexpected value constructor type for a constructor pattern.
This is a bug in the Gleam compiler.
Please report this to https://github.com/lpil/gleam/issues"
                ),
            };

            match instantiate(constructor_typ, level, &mut hashmap![], env) {
                Type::Fn { args, retrn } => {
                    if args.len() == pattern_args.len() {
                        let pattern_args = pattern_args
                            .into_iter()
                            .zip(args)
                            .map(|(arg, typ)| {
                                let CallArg { value, meta, label } = arg;
                                let value = unify_pattern(value, &typ, level, env)?;
                                Ok(CallArg { value, meta, label })
                            })
                            .collect::<Result<Vec<_>, _>>()?;
                        unify(&typ, &retrn, env).map_err(|e| convert_unify_error(e, &meta))?;
                        Ok(Pattern::Constructor {
                            meta,
                            module,
                            name,
                            args: pattern_args,
                            constructor,
                        })
                    } else {
                        Err(Error::IncorrectArity {
                            meta,
                            expected: args.len(),
                            given: pattern_args.len(),
                        })
                    }
                }

                c @ Type::App { .. } => {
                    if pattern_args.is_empty() {
                        unify(&typ, &c, env).map_err(|e| convert_unify_error(e, &meta))?;
                        Ok(Pattern::Constructor {
                            meta,
                            module,
                            name,
                            args: vec![],
                            constructor,
                        })
                    } else {
                        Err(Error::IncorrectArity {
                            meta,
                            expected: 0,
                            given: pattern_args.len(),
                        })
                    }
                }

                typ => {
                    dbg!(typ);
                    unimplemented!();
                }
            }
        }
    }
}

fn infer_var(
    name: &str,
    level: usize,
    meta: &Meta,
    env: &mut Env,
) -> Result<ValueConstructor, Error> {
    // TODO: check the local scope then the module scope.
    let ValueConstructor {
        public,
        variant,
        origin,
        typ,
    } = env
        .get_variable(name)
        .cloned()
        .ok_or_else(|| Error::UnknownVariable {
            meta: meta.clone(),
            name: name.to_string(),
            variables: env.local_values.clone(), // TODO: include module values too
        })?;
    let typ = instantiate(typ, level, &mut hashmap![], env);
    Ok(ValueConstructor {
        public,
        variant,
        origin,
        typ,
    })
}

fn infer_call(
    fun: UntypedExpr,
    mut args: Vec<CallArg<UntypedExpr>>,
    level: usize,
    meta: &Meta,
    env: &mut Env,
) -> Result<(TypedExpr, Vec<CallArg<TypedExpr>>, Type), Error> {
    let fun = infer(fun, level, env)?;

    match get_field_map(&fun, env).map_err(|e| convert_get_value_constructor_error(e, meta))? {
        // The fun has a field map so labelled arguments may be present and need to be reordered.
        Some(field_map) => field_map.reorder(&mut args, meta)?,

        // The fun has no field map and so we error if arguments have been labelled
        None => assert_no_labelled_arguments(&args)?,
    }

    let (mut args_types, return_type) = match_fun_type(fun.typ(), args.len(), env)
        .map_err(|e| convert_not_fun_error(e, fun.meta(), &meta))?;
    let args = args_types
        .iter_mut()
        .zip(args)
        .map(|(typ, CallArg { label, value, meta }): (&mut Type, _)| {
            let value = infer(value, level, env)?;
            unify(typ, value.typ(), env).map_err(|e| convert_unify_error(e, value.meta()))?;
            Ok(CallArg { label, value, meta })
        })
        .collect::<Result<_, _>>()?;
    Ok((fun, args, return_type))
}

fn assert_no_labelled_arguments<A>(args: &[CallArg<A>]) -> Result<(), Error> {
    for arg in args {
        if let Some(label) = &arg.label {
            return Err(Error::UnexpectedLabelledArg {
                meta: arg.meta.clone(),
                label: label.to_string(),
            });
        }
    }
    Ok(())
}

fn get_field_map<'a>(
    constructor: &TypedExpr,
    env: &'a Env,
) -> Result<Option<&'a FieldMap>, GetValueConstructorError> {
    let (module, name) = match constructor {
        Expr::ModuleSelect {
            module_alias,
            label,
            ..
        } => (Some(module_alias), label),

        Expr::Var { name, .. } => (None, name),

        _ => return Ok(None),
    };

    Ok(env.get_value_constructor(module, name)?.field_map())
}

fn infer_fun(
    args: &[Arg],
    body: UntypedExpr,
    return_annotation: &Option<TypeAst>,
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
    let previous_vars = env.local_values.clone();
    for (arg, t) in args.iter().zip(args_types.iter()) {
        match &arg.names {
            ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => env.insert_variable(
                name.to_string(),
                ValueConstructorVariant::LocalVariable,
                (*t).clone(),
            ),
            ArgNames::Discard => (),
        };
    }

    let body = infer(body, level, env)?;

    // Check that any return type annotation is accurate.
    if let Some(ann) = return_annotation {
        let ret_typ = env.type_from_ast(ann, &mut type_vars, NewTypeAction::MakeGeneric)?;
        unify(&ret_typ, body.typ(), env).map_err(|e| convert_unify_error(e, body.meta()))?;
    }

    // Reset the env now that the scope of the function has ended.
    env.local_values = previous_vars;
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

        UnifyError::RecursiveType => Error::RecursiveType { meta: meta.clone() },
    }
}

/// Instantiate converts generic variables into unbound ones.
///
fn instantiate(
    t: Type,
    ctx_level: usize,
    ids: &mut im::HashMap<usize, Type>,
    env: &mut Env,
) -> Type {
    match t {
        Type::App {
            public,
            name,
            module,
            args,
        } => {
            let args = args
                .into_iter()
                .map(|t| instantiate(t, ctx_level, ids, env))
                .collect();
            Type::App {
                public,
                name,
                module,
                args,
            }
        }

        Type::Var { typ } => {
            match &*typ.borrow() {
                TypeVar::Link { typ } => return instantiate(*typ.clone(), ctx_level, ids, env),

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

        Type::Fn { args, retrn, .. } => {
            let args = args
                .into_iter()
                .map(|t| instantiate(t, ctx_level, ids, env))
                .collect();
            let retrn = Box::new(instantiate(*retrn, ctx_level, ids, env));
            Type::Fn { args, retrn }
        }

        Type::AnonStruct { elems } => Type::AnonStruct {
            elems: elems
                .into_iter()
                .map(|t| instantiate(t, ctx_level, ids, env))
                .collect(),
        },
    }
}

#[derive(Debug, PartialEq)]
enum UnifyError {
    CouldNotUnify { expected: Type, given: Type },

    RecursiveType,
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
        ) if m1 == m2 && n1 == n2 && args1.len() == args2.len() => {
            for (a, b) in args1.iter().zip(args2) {
                unify(a, b, env)?;
            }
            Ok(())
        }

        (Type::AnonStruct { elems: elems1, .. }, Type::AnonStruct { elems: elems2, .. })
            if elems1.len() == elems2.len() =>
        {
            for (a, b) in elems1.iter().zip(elems2) {
                unify(a, b, env)?;
            }
            Ok(())
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
        ) if args1.len() == args2.len() => {
            for (a, b) in args1.iter().zip(args2) {
                unify(a, b, env)?;
            }
            unify(retrn1, retrn2, env)
        }

        (_, _) => Err(UnifyError::CouldNotUnify {
            expected: (*t1).clone(),
            given: (*t2).clone(),
        }),
    }
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

        Type::Fn { args, retrn } => {
            for arg in args.iter() {
                update_levels(arg, own_level, own_id)?;
            }
            update_levels(retrn, own_level, own_id)
        }

        Type::AnonStruct { elems, .. } => {
            for elem in elems.iter() {
                update_levels(elem, own_level, own_id)?
            }
            Ok(())
        }

        Type::Var { .. } => unreachable!(),
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

#[test]
fn infer_test() {
    macro_rules! assert_infer {
        ($src:expr, $typ:expr $(,)?) => {
            let ast = crate::grammar::ExprParser::new()
                .parse($src)
                .expect("syntax error");
            let result =
                infer(ast, 1, &mut Env::new(&HashMap::new())).expect("should successfully infer");
            assert_eq!(
                (
                    $src,
                    result
                        .typ()
                        .to_gleam_doc(&mut hashmap![], &mut 0)
                        .format(80)
                ),
                ($src, $typ.to_string()),
            );
        };
    }

    assert_infer!("True", "Bool");
    assert_infer!("False", "Bool");
    assert_infer!("1", "Int");
    assert_infer!("-2", "Int");
    assert_infer!("1.0", "Float");
    assert_infer!("-8.0", "Float");
    assert_infer!("\"ok\"", "String");
    assert_infer!("\"ok\"", "String");
    assert_infer!("[]", "List(a)");
    assert_infer!("4 % 1", "Int");
    assert_infer!("4 > 1", "Bool");
    assert_infer!("4 >= 1", "Bool");
    assert_infer!("4 <= 1", "Bool");
    assert_infer!("4 < 1", "Bool");

    // let
    assert_infer!("let x = 1 2", "Int");
    assert_infer!("let x = 1 x", "Int");
    assert_infer!("let x = 2.0 x", "Float");
    assert_infer!("let x = 2 let y = x y", "Int");

    // list
    assert_infer!("[]", "List(a)");
    assert_infer!("[1]", "List(Int)");
    assert_infer!("[1, 2, 3]", "List(Int)");
    assert_infer!("[[]]", "List(List(a))");
    assert_infer!("[[1.0, 2.0]]", "List(List(Float))");
    assert_infer!("[fn(x) { x }]", "List(fn(a) -> a)");
    assert_infer!("[fn(x) { x + 1 }]", "List(fn(Int) -> Int)");
    assert_infer!("[fn(x) { x }, fn(x) { x + 1 }]", "List(fn(Int) -> Int)");
    assert_infer!("[fn(x) { x + 1 }, fn(x) { x }]", "List(fn(Int) -> Int)");
    assert_infer!("[[], []]", "List(List(a))");
    assert_infer!("[[], [1]]", "List(List(Int))");
    assert_infer!("[1 | [2 | []]]", "List(Int)");
    assert_infer!("[fn(x) { x } | []]", "List(fn(a) -> a)");
    assert_infer!("let f = fn(x) { x } [f, f]", "List(fn(a) -> a)");
    assert_infer!("let x = [1 | []] [2 | x]", "List(Int)");
    assert_infer!("[struct([], [])]", "List(struct(List(a), List(b)))");

    // anon structs
    assert_infer!("struct(1)", "struct(Int)");
    assert_infer!("struct(1, 2.0)", "struct(Int, Float)");
    assert_infer!("struct(1, 2.0, 3)", "struct(Int, Float, Int)");
    assert_infer!(
        "struct(1, 2.0, struct(1, 1))",
        "struct(Int, Float, struct(Int, Int))",
    );

    // fn
    assert_infer!("fn(x) { x }", "fn(a) -> a");
    assert_infer!("fn(x) { x }", "fn(a) -> a");
    assert_infer!("fn(x, y) { x }", "fn(a, b) -> a");
    assert_infer!("fn(x, y) { [] }", "fn(a, b) -> List(c)");
    assert_infer!("let x = 1.0 1", "Int");
    assert_infer!("let id = fn(x) { x } id(1)", "Int");
    assert_infer!("let x = fn() { 1.0 } x()", "Float");
    assert_infer!("fn(x) { x }(1)", "Int");
    assert_infer!("fn() { 1 }", "fn() -> Int");
    assert_infer!("fn() { 1.1 }", "fn() -> Float");
    assert_infer!("fn(x) { 1.1 }", "fn(a) -> Float");
    assert_infer!("fn(x) { x }", "fn(a) -> a");
    assert_infer!("let x = fn(x) { 1.1 } x", "fn(a) -> Float");
    assert_infer!("fn(x, y, z) { 1 }", "fn(a, b, c) -> Int");
    assert_infer!("fn(x) { let y = x y }", "fn(a) -> a");
    assert_infer!("let id = fn(x) { x } id(1)", "Int");
    assert_infer!(
        "let constant = fn(x) { fn(y) { x } } let one = constant(1) one(2.0)",
        "Int",
    );
    assert_infer!("fn(f) { f(1) }", "fn(fn(Int) -> a) -> a");
    assert_infer!("fn(f, x) { f(x) }", "fn(fn(a) -> b, a) -> b");
    assert_infer!("fn(f) { fn(x) { f(x) } }", "fn(fn(a) -> b) -> fn(a) -> b");
    assert_infer!(
        "fn(f) { fn(x) { fn(y) { f(x, y) } } }",
        "fn(fn(a, b) -> c) -> fn(a) -> fn(b) -> c",
    );
    assert_infer!(
        "fn(f) { fn(x, y) { f(x)(y) } }",
        "fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c",
    );
    assert_infer!(
        "fn(f) { fn(x) { let ff = f ff(x) } }",
        "fn(fn(a) -> b) -> fn(a) -> b",
    );
    assert_infer!(
        "fn(f) { fn(x, y) { let ff = f(x) ff(y) } }",
        "fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c",
    );
    assert_infer!("fn(x) { fn(y) { x } }", "fn(a) -> fn(b) -> a");
    assert_infer!("fn(f) { f() }", "fn(fn() -> a) -> a");
    assert_infer!("fn(f, x) { f(f(x)) }", "fn(fn(a) -> a, a) -> a");
    assert_infer!(
        "let id = fn(a) { a } fn(x) { x(id) }",
        "fn(fn(fn(a) -> a) -> b) -> b",
    );
    assert_infer!("let add = fn(x, y) { x + y } add(_, 2)", "fn(Int) -> Int");
    assert_infer!("fn(x) { struct(1, x) }", "fn(a) -> struct(Int, a)");
    assert_infer!("fn(x, y) { struct(x, y) }", "fn(a, b) -> struct(a, b)");
    assert_infer!("fn(x) { struct(x, x) }", "fn(a) -> struct(a, a)");

    // case
    assert_infer!("case 1 { a -> 1 }", "Int");
    assert_infer!("case 1 { a -> 1.0 b -> 2.0 c -> 3.0 }", "Float");
    assert_infer!("case 1 { a -> a }", "Int");
    assert_infer!("case 1 { 1 -> 10 2 -> 20 x -> x * 10 }", "Int");
    assert_infer!("case 2.0 { 2.0 -> 1 x -> 0 }", "Int");
    assert_infer!(r#"case "ok" { "ko" -> 1 x -> 0 }"#, "Int");

    // Multiple subject case
    assert_infer!("case 1, 2.0 { a, b -> a }", "Int");
    assert_infer!("case 1, 2.0 { a, b -> b }", "Float");
    assert_infer!("case 1, 2.0, 3 { a, b, c -> a + c }", "Int");

    // let
    assert_infer!("let [] = [] 1", "Int");
    assert_infer!("let [a] = [1] a", "Int");
    assert_infer!("let [a, 2] = [1] a", "Int");
    assert_infer!("let [a | [b | []]] = [1] a", "Int");
    assert_infer!("fn(x) { let [a] = x a }", "fn(List(a)) -> a");
    assert_infer!("fn(x) { let [a] = x a + 1 }", "fn(List(Int)) -> Int");
    assert_infer!("let _x = 1 2.0", "Float");
    assert_infer!("let _ = 1 2.0", "Float");
    assert_infer!("let struct(tag, x) = struct(1.0, 1) x", "Int");
    assert_infer!("fn(x) { let struct(a, b) = x a }", "fn(struct(a, b)) -> a");
}

#[test]
fn infer_error_test() {
    macro_rules! assert_error {
        ($src:expr, $error:expr $(,)?) => {
            let ast = crate::grammar::ExprParser::new()
                .parse($src)
                .expect("syntax error");
            let result =
                infer(ast, 1, &mut Env::new(&HashMap::new())).expect_err("should infer an error");
            assert_eq!(($src, $error), ($src, result));
        };
    }

    assert_error!(
        "1 + 1.0",
        Error::CouldNotUnify {
            meta: Meta { start: 4, end: 7 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "1 +. 1.0",
        Error::CouldNotUnify {
            meta: Meta { start: 0, end: 1 },
            expected: float(),
            given: int(),
        },
    );

    assert_error!(
        "1 == 1.0",
        Error::CouldNotUnify {
            meta: Meta { start: 5, end: 8 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "1 > 1.0",
        Error::CouldNotUnify {
            meta: Meta { start: 4, end: 7 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "1.0 >. 1",
        Error::CouldNotUnify {
            meta: Meta { start: 7, end: 8 },
            expected: float(),
            given: int(),
        },
    );

    assert_error!(
        "x",
        Error::UnknownVariable {
            meta: Meta { start: 0, end: 1 },
            name: "x".to_string(),
            variables: Env::new(&HashMap::new()).local_values,
        },
    );

    assert_error!(
        "x",
        Error::UnknownVariable {
            meta: Meta { start: 0, end: 1 },
            name: "x".to_string(),
            variables: Env::new(&HashMap::new()).local_values,
        },
    );

    assert_error!(
        "let id = fn(x) { x } id()",
        Error::IncorrectArity {
            meta: Meta { start: 21, end: 25 },
            expected: 1,
            given: 0,
        },
    );

    assert_error!(
        "let id = fn(x) { x } id(1, 2)",
        Error::IncorrectArity {
            meta: Meta { start: 21, end: 29 },
            expected: 1,
            given: 2,
        },
    );

    assert_error!(
        "case 1 { a -> 1 b -> 2.0 }",
        Error::CouldNotUnify {
            meta: Meta { start: 21, end: 24 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case 1.0 { 1 -> 1 }",
        Error::CouldNotUnify {
            meta: Meta { start: 11, end: 12 },
            expected: float(),
            given: int(),
        },
    );

    assert_error!(
        "case 1 { 1.0 -> 1 }",
        Error::CouldNotUnify {
            meta: Meta { start: 9, end: 12 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case 1, 2.0 { a, b -> a + b }",
        Error::CouldNotUnify {
            meta: Meta { start: 26, end: 27 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case 1, 2.0 { a, b -> a 1, 2 -> 0 }",
        Error::CouldNotUnify {
            meta: Meta { start: 27, end: 28 },
            expected: float(),
            given: int(),
        },
    );

    assert_error!(
        "fn() { 1 } == fn(x) { x + 1 }",
        Error::CouldNotUnify {
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
    );

    assert_error!(
        "let f = fn(x: Int) { x } f(1.0)",
        Error::CouldNotUnify {
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
    );

    assert_error!(
        "case 1 { x -> 1 1 -> x }",
        Error::UnknownVariable {
            meta: Meta { start: 21, end: 22 },
            name: "x".to_string(),
            variables: Env::new(&HashMap::new()).local_values,
        },
    );

    assert_error!(
        "let id = fn(x) { x(x) } 1",
        Error::RecursiveType {
            meta: Meta { start: 19, end: 20 },
        },
    );

    assert_error!(
        "let True(x) = 1 x",
        Error::IncorrectArity {
            meta: Meta { start: 4, end: 11 },
            expected: 0,
            given: 1,
        },
    );

    assert_error!(
        "let Ok(1, x) = 1 x",
        Error::IncorrectArity {
            meta: Meta { start: 4, end: 12 },
            expected: 1,
            given: 2,
        },
    );

    assert_error!(
        "let x = 1 x.whatever",
        Error::NotModule {
            meta: Meta { start: 10, end: 11 },
            typ: int(),
        },
    );

    assert_error!(
        "struct(1, 2) == struct(1, 2, 3)",
        Error::CouldNotUnify {
            meta: Meta { start: 16, end: 31 },
            expected: Type::AnonStruct {
                elems: vec![int(), int()],
            },
            given: Type::AnonStruct {
                elems: vec![int(), int(), int()],
            },
        },
    );
}

#[test]
fn infer_module_test() {
    macro_rules! assert_infer {
        ($src:expr, $module:expr $(,)?) => {
            let ast = crate::grammar::ModuleParser::new()
                .parse($src)
                .expect("syntax error");
            let result = infer_module(ast, &HashMap::new()).expect("should successfully infer");
            let mut constructors: Vec<(_, _)> = result
                .type_info
                .values
                .iter()
                .map(|(k, v)| (k.clone(), v.typ.pretty_print(0)))
                .collect();
            constructors.sort();
            let expected: Vec<_> = $module
                .into_iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect();
            assert_eq!(($src, constructors), ($src, expected));
        };
    }

    assert_infer!(
        "pub fn repeat(i, x) {
           case i {
             0 -> []
             i -> [x | repeat(i - 1, x)]
           }
         }",
        vec![("repeat", "fn(Int, a) -> List(a)")],
    );

    assert_infer!(
        "fn private() { 1 }
         pub fn public() { 1 }",
        vec![("public", "fn() -> Int")],
    );

    assert_infer!(
        "pub enum Is { Yes No }
         pub fn yes() { Yes }
         pub fn no() { No }",
        vec![
            ("No", "Is"),
            ("Yes", "Is"),
            ("no", "fn() -> Is"),
            ("yes", "fn() -> Is"),
        ],
    );

    assert_infer!(
        "pub enum Num { I(Int) }
         pub fn one() { I(1) }",
        vec![("I", "fn(Int) -> Num"), ("one", "fn() -> Num")],
    );

    assert_infer!(
        "pub fn id(x) { x }
         pub fn float() { id(1.0) }
         pub fn int() { id(1) }",
        vec![
            ("float", "fn() -> Float"),
            ("id", "fn(a) -> a"),
            ("int", "fn() -> Int"),
        ],
    );

    assert_infer!(
        "pub enum Box(a) { Box(a) }
        pub fn int() { Box(1) }
        pub fn float() { Box(1.0) }",
        vec![
            ("Box", "fn(a) -> Box(a)"),
            ("float", "fn() -> Box(Float)"),
            ("int", "fn() -> Box(Int)"),
        ],
    );

    assert_infer!(
        "pub enum Singleton { Singleton }
        pub fn go(x) { let Singleton = x 1 }",
        vec![("Singleton", "Singleton"), ("go", "fn(Singleton) -> Int")],
    );

    assert_infer!(
        "pub enum Box(a) { Box(a) }
        pub fn unbox(x) { let Box(a) = x a }",
        vec![("Box", "fn(a) -> Box(a)"), ("unbox", "fn(Box(a)) -> a")],
    );

    assert_infer!(
        "pub enum I { I(Int) }
        pub fn open(x) { case x { I(i) -> i  } }",
        vec![("I", "fn(Int) -> I"), ("open", "fn(I) -> Int")],
    );

    assert_infer!(
        "pub fn status() { 1 } pub fn list_of(x) { [x] }",
        vec![("list_of", "fn(a) -> List(a)"), ("status", "fn() -> Int")],
    );

    assert_infer!(
        "pub external fn go(String) -> String = \"\" \"\"",
        vec![("go", "fn(String) -> String")],
    );

    assert_infer!(
        "pub external fn go(Int) -> Float = \"\" \"\"",
        vec![("go", "fn(Int) -> Float")],
    );

    assert_infer!(
        "pub external fn go(Int) -> Int = \"\" \"\"",
        vec![("go", "fn(Int) -> Int")],
    );

    assert_infer!(
        "pub external fn ok() -> fn(Int) -> Int = \"\" \"\"",
        vec![("ok", "fn() -> fn(Int) -> Int")],
    );

    assert_infer!(
        "pub external fn go(Int) -> b = \"\" \"\"",
        vec![("go", "fn(Int) -> a")],
    );

    assert_infer!(
        "pub external fn go(Bool) -> b = \"\" \"\"",
        vec![("go", "fn(Bool) -> a")],
    );

    assert_infer!(
        "pub external fn go(List(a)) -> a = \"\" \"\"",
        vec![("go", "fn(List(a)) -> a")],
    );

    assert_infer!(
        "external fn go(Int) -> b = \"\" \"\"
        pub fn x() { go(1) }",
        vec![("x", "fn() -> a")],
    );

    assert_infer!(
        "external fn id(a) -> a = \"\" \"\"
        pub fn i(x) { id(x) }
        pub fn a() { id(1) }
        pub fn b() { id(1.0) }",
        vec![
            ("a", "fn() -> Int"),
            ("b", "fn() -> Float"),
            ("i", "fn(a) -> a"),
        ],
    );

    assert_infer!(
        "pub external fn len(List(a)) -> Int = \"\" \"\"",
        vec![("len", "fn(List(a)) -> Int")],
    );

    assert_infer!(
        "pub external type Connection\n
         pub external fn is_open(Connection) -> Bool = \"\" \"\"",
        vec![("is_open", "fn(Connection) -> Bool")],
    );

    assert_infer!(
        "pub external type Pair(thing, thing)\n
         pub external fn pair(a) -> Pair(a, a) = \"\" \"\"",
        vec![("pair", "fn(a) -> Pair(a, a)")],
    );

    assert_infer!(
        "pub fn one() { 1 }
         pub fn zero() { one() - 1 }
         pub fn two() { one() + zero() }",
        vec![
            ("one", "fn() -> Int"),
            ("two", "fn() -> Int"),
            ("zero", "fn() -> Int"),
        ],
    );

    assert_infer!(
        "pub fn one() { 1 }
         pub fn zero() { one() - 1 }
         pub fn two() { one() + zero() }",
        vec![
            ("one", "fn() -> Int"),
            ("two", "fn() -> Int"),
            ("zero", "fn() -> Int"),
        ],
    );

    // Type annotations
    assert_infer!("pub fn go(x: Int) { x }", vec![("go", "fn(Int) -> Int")],);
    assert_infer!("pub fn go(x: b) -> b { x }", vec![("go", "fn(a) -> a")],);
    assert_infer!("pub fn go(x) -> b { x }", vec![("go", "fn(a) -> a")],);
    assert_infer!("pub fn go(x: b) { x }", vec![("go", "fn(a) -> a")],);
    assert_infer!(
        "pub fn go(x: List(b)) -> List(b) { x }",
        vec![("go", "fn(List(a)) -> List(a)")],
    );
    assert_infer!(
        "pub fn go(x: List(b)) { x }",
        vec![("go", "fn(List(a)) -> List(a)")],
    );
    assert_infer!(
        "pub fn go(x: List(String)) { x }",
        vec![("go", "fn(List(String)) -> List(String)")],
    );
    assert_infer!("pub fn go(x: b, y: c) { x }", vec![("go", "fn(a, b) -> a")],);
    assert_infer!("pub fn go(x) -> Int { x }", vec![("go", "fn(Int) -> Int")],);

    // // Type aliases
    // assert_infer!(     src: "
    // type Html = String
    // pub fn go() { 1 }",
    //     vec![("go", "fn() -> Int")],
    // );
    assert_infer!(
        "pub fn length(list) {
           case list {
           [] -> 0
           [x | xs] -> length(xs) + 1
           }
        }",
        vec![("length", "fn(List(a)) -> Int")],
    );
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

    // Structs
    assert_infer!(
        "pub struct Box { boxed: Int }",
        vec![("Box", "fn(Int) -> Box")]
    );
    assert_infer!(
        "pub struct Tup(a, b) { first: a second: b }",
        vec![("Tup", "fn(a, b) -> Tup(a, b)")]
    );
    assert_infer!(
        "pub struct Tup(a, b, c) { first: a second: b third: c }
         pub fn third(t) { let Tup(_, third: a, _) = t a }",
        vec![
            ("Tup", "fn(a, b, c) -> Tup(a, b, c)"),
            ("third", "fn(Tup(a, b, c)) -> c"),
        ],
    );
    assert_infer!(
        "pub struct Box(x) { label: String contents: x }
         pub fn id(x: Box(y)) { x }",
        vec![
            ("Box", "fn(String, a) -> Box(a)"),
            ("id", "fn(Box(a)) -> Box(a)"),
        ],
    );
    // assert_infer!(     src: "pub struct Box(a) { boxed: a }
    //           pub fn unbox_int(b: Box(Int)) { b.boxed }",
    //     vec![
    //         ("Box", "fn(a) -> Box(a)"),
    //         ("unbox_int", "fn(Box(Int)) -> Int"),
    //     ],
    // );
    // assert_infer!(     src: "pub struct Box(a) { boxed: a }
    //           pub fn unbox_pattern(b: Box(a)) { let Box(x) = b x }
    //           pub fn unbox(b: Box(a)) { b.boxed }
    //           pub fn unbox_int(b: Box(Int)) { b.boxed }",
    //     vec![
    //         ("Box", "fn(a) -> Box(a)"),
    //         ("unbox", "fn(Box(a)) -> a"),
    //         ("unbox_int", "fn(Box(Int)) -> Int"),
    //         ("unbox_pattern", "fn(Box(a)) -> a"),
    //     ],
    // );

    // Anon structs
    assert_infer!(
        "pub fn ok(x) { struct(1, x) }",
        vec![("ok", "fn(a) -> struct(Int, a)")],
    );

    assert_infer!(
        "pub external fn ok(Int) -> struct(Int, Int) = \"\" \"\"",
        vec![("ok", "fn(Int) -> struct(Int, Int)")],
    );

    assert_infer!(
        "pub external fn go(struct(a, c)) -> c = \"\" \"\"",
        vec![("go", "fn(struct(a, b)) -> b")],
    );
}

#[test]
fn infer_module_error_test() {
    macro_rules! assert_error {
        ($src:expr, $error:expr $(,)?) => {
            let ast = crate::grammar::ModuleParser::new()
                .parse($src)
                .expect("syntax error");
            let result = infer_module(ast, &HashMap::new()).expect_err("should infer an error");
            assert_eq!(($src, $error), ($src, result));
        };

        ($src:expr) => {
            let ast = crate::grammar::ModuleParser::new()
                .parse($src)
                .expect("syntax error");
            infer_module(ast, &HashMap::new()).expect_err("should infer an error");
        };
    }

    assert_error!(
        "fn go() { 1 + 2.0 }",
        Error::CouldNotUnify {
            meta: Meta { start: 14, end: 17 },
            expected: int(),
            given: float(),
        }
    );

    assert_error!(
        "fn go() { 1 + 2.0 }",
        Error::CouldNotUnify {
            meta: Meta { start: 14, end: 17 },
            expected: int(),
            given: float(),
        }
    );

    assert_error!(
        "
fn id(x: a, y: a) { x }
pub fn x() { id(1, 1.0) }
                ",
        Error::CouldNotUnify {
            meta: Meta { start: 44, end: 47 },
            expected: int(),
            given: float(),
        }
    );

    assert_error!(
        "external fn go(List(a, b)) -> a = \"\" \"\"",
        Error::IncorrectTypeArity {
            meta: Meta { start: 15, end: 25 },
            name: "List".to_string(),
            expected: 1,
            given: 2,
        }
    );

    assert_error!(
        "fn dupe() { 1 }
         fn dupe() { 2 }",
        Error::DuplicateName {
            meta: Meta { start: 25, end: 40 },
            name: "dupe".to_string(),
        }
    );

    assert_error!(
        "fn dupe() { 1 }
         fn dupe(x) { x }",
        Error::DuplicateName {
            meta: Meta { start: 25, end: 41 },
            name: "dupe".to_string(),
        }
    );

    assert_error!(
        r#"external type PrivateType
           pub external fn leak_type() -> PrivateType = "" """#,
        Error::PrivateTypeLeak {
            meta: Meta { start: 37, end: 87 },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec![],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_error!(
        r#"external type PrivateType
           external fn go() -> PrivateType = "" ""
           pub fn leak_type() { go() }"#,
        Error::PrivateTypeLeak {
            meta: Meta {
                start: 88,
                end: 115,
            },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec![],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_error!(
        r#"external type PrivateType
           external fn go() -> PrivateType = "" ""
           pub fn leak_type() { [go()] }"#,
        Error::PrivateTypeLeak {
            meta: Meta {
                start: 88,
                end: 117,
            },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec![],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_error!(
        r#"external type PrivateType
                    pub external fn go(PrivateType) -> Int = "" """#,
        Error::PrivateTypeLeak {
            meta: Meta { start: 46, end: 92 },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec![],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_error!(
        r#"external type PrivateType
           pub enum LeakType { Variant(PrivateType) }"#,
        Error::PrivateTypeLeak {
            meta: Meta { start: 57, end: 77 },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec![],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_error!(
        r#"fn id(x) { x } fn y() { id(x: 4) }"#,
        Error::UnexpectedLabelledArg {
            label: "x".to_string(),
            meta: Meta { start: 27, end: 31 },
        }
    );

    assert_error!(
        r#"struct X { a: Int b: Int c: Int }
                    fn x() { X(b: 1, a: 1, 1) }"#,
        Error::PositionalArgumentAfterLabelled {
            meta: Meta { start: 77, end: 78 },
        }
    );

    assert_error!(
        r#"struct Thing { unknown: x }"#,
        Error::UnknownType {
            meta: Meta { start: 24, end: 25 },
            name: "x".to_string(),
            types: {
                let mut types = Env::new(&mut HashMap::new()).module_types;
                types.insert(
                    "Thing".to_string(),
                    TypeConstructor {
                        public: false,
                        module: vec![],
                        arity: 0,
                    },
                );
                types
            },
        }
    );

    // Cases were we can't so easily check for equality-
    // i.e. because the contents of the error are non-deterministic.
    assert_error!("fn inc(x: a) { x + 1 }");
}
