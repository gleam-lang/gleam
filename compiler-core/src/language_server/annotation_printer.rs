#![allow(dead_code)]

use ecow::EcoString;
use im::{HashMap, HashSet};
use std::sync::Arc;

use crate::{
    ast::{TypeAst, UnqualifiedImport},
    type_::{Type, TypeVar, PRELUDE_MODULE_NAME},
};

#[derive(Debug)]
pub struct AnnotationPrinter<'a> {
    names: HashMap<u64, EcoString>,
    uid: u64,

    /// Type aliases that are defined in the current module.
    ///
    /// key:   (Defining module name, type name)
    /// value: Alias name
    ///
    /// # Example 1
    ///
    /// ```gleam
    /// type Wibble = wobble.Woo
    /// ```
    /// would result in
    /// - key:   `("wibble", "Woo")`
    /// - value: `"Wibble"`
    ///
    type_aliases: &'a HashMap<(EcoString, EcoString), EcoString>,

    /// Mapping of imported modules to their aliased named
    ///
    /// key:   The name of the module
    /// value: The name the module is aliased to
    ///
    /// # Example 1
    ///
    /// ```gleam
    /// import mod1 as my_mod
    /// ```
    /// would result in:
    /// - key:   "mod1"
    /// - value: "my_mod"
    ///
    module_aliases: &'a HashMap<EcoString, EcoString>,

    /// Generic type parameters that have been annotated in the current
    /// function.
    ///
    /// key:   The id of generic type that was annotated
    /// value: The name that is used for the generic type in the annotation.
    ///
    /// # Example 1
    ///
    /// ```gleam
    /// fn equal(x: something, y: something) -> Bool {
    ///   arg1 == arg2
    /// }
    /// ```
    ///
    /// key:   <some id int>
    /// value: `"something"`
    ///
    generic_annotations: &'a HashMap<u64, TypeAst>,

    /// These are unqualified types that are imported in the current module.
    ///
    /// key:   (Defining module name, type name)
    /// value: The name that was used, either its original name or an alias
    ///
    /// # Example 1
    ///
    /// ```gleam
    /// import some/module.{type Wibble}
    /// ```
    /// would result in
    /// - key:   `("some/module", "Wibble")`
    /// - value: `"Wibble"`
    ///
    /// # Example 2
    ///
    /// ```gleam
    /// import some/module.{type Wibble as Wobble}
    /// ```
    /// would result in
    /// - key:   `("some/module", "Wibble")`
    /// - value: `"Wobble"`
    ///
    unqualified_imports: &'a HashMap<(EcoString, EcoString), UnqualifiedImport>,

    /// A set of the names of types that are defined in this module. Used to
    /// identify any prelude types that have been shadowed.
    ///
    locally_defined_types: &'a HashSet<EcoString>,

    current_module: EcoString,
}

impl<'a> AnnotationPrinter<'a> {
    pub fn new(
        type_aliases: &'a HashMap<(EcoString, EcoString), EcoString>,
        module_aliases: &'a HashMap<EcoString, EcoString>,
        generic_annotations: &'a HashMap<u64, TypeAst>,
        unqualified_imports: &'a HashMap<(EcoString, EcoString), UnqualifiedImport>,
        unqualified_type_names: &'a HashSet<EcoString>,
        current_module: EcoString,
    ) -> Self {
        AnnotationPrinter {
            names: HashMap::new(),
            uid: u64::default(),
            type_aliases,
            module_aliases,
            generic_annotations,
            unqualified_imports,
            locally_defined_types: unqualified_type_names,
            current_module,
        }
    }

    pub fn print_type(&mut self, type_: &Type) -> EcoString {
        let mut buffer = EcoString::new();
        self.print(type_, &mut buffer);
        buffer
    }

    fn print(&mut self, type_: &Type, buffer: &mut EcoString) {
        match type_ {
            Type::Named {
                name, args, module, ..
            } => {
                let key = (module.clone(), name.clone());

                if let Some(typ_alias) = self.type_aliases.get(&key) {
                    buffer.push_str(typ_alias.as_str());
                } else if module == PRELUDE_MODULE_NAME
                    && !self.locally_defined_types.contains(name)
                {
                    buffer.push_str(name.as_str());
                } else if let Some(unqualified_import) = self.unqualified_imports.get(&key) {
                    if let Some(as_name) = &unqualified_import.as_name {
                        buffer.push_str(as_name.as_str());
                    } else {
                        buffer.push_str(name.as_str());
                    }
                } else {
                    if let Some(module_alias) = self.module_aliases.get(module) {
                        buffer.push_str(module_alias);
                        buffer.push('.');
                    } else if module != &self.current_module {
                        buffer.push_str(module);
                        buffer.push('.');
                    }
                    buffer.push_str(name);
                }

                if !args.is_empty() {
                    buffer.push('(');
                    self.print_arguments(args, buffer);
                    buffer.push(')');
                }
            }

            Type::Fn { args, retrn } => {
                buffer.push_str("fn(");
                self.print_arguments(args, buffer);
                buffer.push_str(") -> ");
                self.print(retrn, buffer);
            }

            Type::Var { type_: typ, .. } => match *typ.borrow() {
                TypeVar::Link { type_: ref typ, .. } => self.print(typ, buffer),
                TypeVar::Unbound { id, .. } | TypeVar::Generic { id, .. } => {
                    if let Some(TypeAst::Var(type_var)) = self.generic_annotations.get(&id) {
                        buffer.push_str(&type_var.name);
                        return;
                    }
                    self.print_generic_type_var(id, buffer);
                }
            },

            Type::Tuple { elems, .. } => {
                buffer.push_str("#(");
                self.print_arguments(elems, buffer);
                buffer.push(')');
            }
        }
    }

    fn print_arguments(&mut self, args: &[Arc<Type>], typ_str: &mut EcoString) {
        for (i, arg) in args.iter().enumerate() {
            self.print(arg, typ_str);
            if i < args.len() - 1 {
                typ_str.push_str(", ");
            }
        }
    }

    fn print_generic_type_var(&mut self, id: u64, typ_str: &mut EcoString) {
        match self.names.get(&id) {
            Some(n) => {
                typ_str.push_str(n);
            }
            None => {
                let n = self.next_letter();
                let _ = self.names.insert(id, n.clone());
                typ_str.push_str(&n)
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
}

#[test]
fn test_type_alias() {
    let mut type_aliases = HashMap::new();
    let module_aliases = HashMap::new();
    let generic_annotations = HashMap::new();
    let unqualified_imports = HashMap::new();
    let unqualified_type_names = HashSet::new();

    let _ = type_aliases.insert(
        (EcoString::from("mod"), EcoString::from("Tiger")),
        EcoString::from("Cat"),
    );

    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Named {
        name: EcoString::from("Tiger"),
        args: vec![],
        module: EcoString::from("mod"),
        publicity: crate::ast::Publicity::Public,
        package: EcoString::from(""),
    };

    assert_eq!(printer.print_type(&typ), "Cat");
}

#[test]
fn test_generic_type_annotation() {
    let type_aliases = HashMap::new();
    let module_aliases = HashMap::new();
    let unqualified_imports = HashMap::new();
    let unqualified_type_names = HashSet::new();
    let mut generic_annotations = HashMap::new();

    let type_var = TypeAst::Var(crate::ast::TypeAstVar {
        name: EcoString::from("foo"),
        location: crate::ast::SrcSpan::default(),
    });

    let _ = generic_annotations.insert(0, type_var);
    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Var {
        type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 0 })),
    };

    assert_eq!(printer.print_type(&typ), "foo");
}

#[test]
fn test_unqualified_import() {
    let type_aliases = HashMap::new();
    let module_aliases = HashMap::new();
    let generic_annotations = HashMap::new();
    let mut unqualified_imports = HashMap::new();
    let unqualified_type_names = HashSet::new();

    let unqualified_import = UnqualifiedImport {
        name: EcoString::from("Cat"),
        as_name: Some(EcoString::from("C")),
        location: crate::ast::SrcSpan::default(),
    };

    let _ = unqualified_imports.insert(
        (EcoString::from("mod"), EcoString::from("Cat")),
        unqualified_import,
    );

    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Named {
        name: EcoString::from("Cat"),
        args: vec![],
        module: EcoString::from("mod"),
        publicity: crate::ast::Publicity::Public,
        package: EcoString::from(""),
    };

    assert_eq!(printer.print_type(&typ), "C");
}

#[test]
fn test_prelude_type_shadowed() {
    let type_aliases = HashMap::new();
    let module_aliases = HashMap::new();
    let generic_annotations = HashMap::new();
    let unqualified_imports = HashMap::new();
    let mut unqualified_type_names = HashSet::new();

    let _ = unqualified_type_names.insert(EcoString::from("Int"));

    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Named {
        name: EcoString::from("Int"),
        args: vec![],
        module: PRELUDE_MODULE_NAME.into(),
        publicity: crate::ast::Publicity::Public,
        package: EcoString::from(""),
    };

    assert_eq!(printer.print_type(&typ), "gleam.Int");
}

#[test]
fn test_prelude_type_not_shadowed() {
    let type_aliases = HashMap::new();
    let module_aliases = HashMap::new();
    let generic_annotations = HashMap::new();
    let unqualified_imports = HashMap::new();
    let unqualified_type_names = HashSet::new();

    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Named {
        name: EcoString::from("Int"),
        args: vec![],
        module: PRELUDE_MODULE_NAME.into(),
        publicity: crate::ast::Publicity::Public,
        package: EcoString::from(""),
    };

    assert_eq!(printer.print_type(&typ), "Int");
}

#[test]
fn test_generic_type_var() {
    let type_aliases = HashMap::new();
    let module_aliases = HashMap::new();
    let generic_annotations = HashMap::new();
    let unqualified_imports = HashMap::new();
    let unqualified_type_names = HashSet::new();

    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Var {
        type_: Arc::new(std::cell::RefCell::new(TypeVar::Unbound { id: 0 })),
    };

    let typ2 = Type::Var {
        type_: Arc::new(std::cell::RefCell::new(TypeVar::Unbound { id: 1 })),
    };

    assert_eq!(printer.print_type(&typ), "a");
    assert_eq!(printer.print_type(&typ2), "b");
}

#[test]
fn test_tuple_type() {
    let type_aliases = HashMap::new();
    let module_aliases = HashMap::new();
    let generic_annotations = HashMap::new();
    let unqualified_imports = HashMap::new();
    let unqualified_type_names = HashSet::new();

    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Tuple {
        elems: vec![
            Arc::new(Type::Named {
                name: EcoString::from("Int"),
                args: vec![],
                module: PRELUDE_MODULE_NAME.into(),
                publicity: crate::ast::Publicity::Public,
                package: EcoString::from(""),
            }),
            Arc::new(Type::Named {
                name: EcoString::from("String"),
                args: vec![],
                module: PRELUDE_MODULE_NAME.into(),
                publicity: crate::ast::Publicity::Public,
                package: EcoString::from(""),
            }),
        ],
    };

    assert_eq!(printer.print_type(&typ), "#(Int, String)");
}

#[test]
fn test_fn_type() {
    let type_aliases = HashMap::new();
    let module_aliases = HashMap::new();
    let generic_annotations = HashMap::new();
    let unqualified_imports = HashMap::new();
    let mut unqualified_type_names = HashSet::new();

    let _ = unqualified_type_names.insert(EcoString::from("String"));

    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Fn {
        args: vec![
            Arc::new(Type::Named {
                name: EcoString::from("Int"),
                args: vec![],
                module: PRELUDE_MODULE_NAME.into(),
                publicity: crate::ast::Publicity::Public,
                package: EcoString::from(""),
            }),
            Arc::new(Type::Named {
                name: EcoString::from("String"),
                args: vec![],
                module: PRELUDE_MODULE_NAME.into(),
                publicity: crate::ast::Publicity::Public,
                package: EcoString::from(""),
            }),
        ],
        retrn: Arc::new(Type::Named {
            name: EcoString::from("Bool"),
            args: vec![],
            module: PRELUDE_MODULE_NAME.into(),
            publicity: crate::ast::Publicity::Public,
            package: EcoString::from(""),
        }),
    };

    assert_eq!(printer.print_type(&typ), "fn(Int, gleam.String) -> Bool");
}

#[test]
fn test_module_alias() {
    let type_aliases = HashMap::new();
    let mut module_aliases = HashMap::new();
    let generic_annotations = HashMap::new();
    let unqualified_imports = HashMap::new();
    let unqualified_type_names = HashSet::new();

    let _ = module_aliases.insert(EcoString::from("mod1"), EcoString::from("animals"));

    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Named {
        name: EcoString::from("Cat"),
        args: vec![],
        module: EcoString::from("mod1"),
        publicity: crate::ast::Publicity::Public,
        package: EcoString::from(""),
    };

    assert_eq!(printer.print_type(&typ), "animals.Cat");
}

#[test]
fn test_type_alias_and_generics() {
    let mut type_aliases = HashMap::new();
    let module_aliases = HashMap::new();
    let mut generic_annotations = HashMap::new();
    let unqualified_imports = HashMap::new();
    let unqualified_type_names = HashSet::new();

    let _ = type_aliases.insert(
        (EcoString::from("mod"), EcoString::from("Tiger")),
        EcoString::from("Cat"),
    );

    let type_var = TypeAst::Var(crate::ast::TypeAstVar {
        name: EcoString::from("foo"),
        location: crate::ast::SrcSpan::default(),
    });

    let _ = generic_annotations.insert(0, type_var);

    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Named {
        name: EcoString::from("Tiger"),
        args: vec![Arc::new(Type::Var {
            type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 0 })),
        })],
        module: EcoString::from("mod"),
        publicity: crate::ast::Publicity::Public,
        package: EcoString::from(""),
    };

    assert_eq!(printer.print_type(&typ), "Cat(foo)");
}

#[test]
fn test_unqualified_import_and_generic() {
    let type_aliases = HashMap::new();
    let module_aliases = HashMap::new();
    let mut unqualified_imports = HashMap::new();
    let mut generic_annotations = HashMap::new();
    let unqualified_type_names = HashSet::new();

    let unqualified_import = UnqualifiedImport {
        name: EcoString::from("Cat"),
        as_name: Some(EcoString::from("C")),
        location: crate::ast::SrcSpan::default(),
    };

    let _ = unqualified_imports.insert(
        (EcoString::from("mod"), EcoString::from("Cat")),
        unqualified_import,
    );

    let type_var = TypeAst::Var(crate::ast::TypeAstVar {
        name: EcoString::from("foo"),
        location: crate::ast::SrcSpan::default(),
    });

    let _ = generic_annotations.insert(0, type_var);

    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Named {
        name: EcoString::from("Cat"),
        args: vec![Arc::new(Type::Var {
            type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 0 })),
        })],
        module: EcoString::from("mod"),
        publicity: crate::ast::Publicity::Public,
        package: EcoString::from(""),
    };

    assert_eq!(printer.print_type(&typ), "C(foo)");
}

#[test]
fn test_unqualified_import_and_module_alias() {
    let type_aliases = HashMap::new();
    let mut module_aliases = HashMap::new();
    let mut unqualified_imports = HashMap::new();
    let unqualified_type_names = HashSet::new();
    let generic_annotations = HashMap::new();

    let _ = module_aliases.insert(EcoString::from("mod1"), EcoString::from("animals"));

    let unqualified_import = UnqualifiedImport {
        name: EcoString::from("Cat"),
        as_name: Some(EcoString::from("C")),
        location: crate::ast::SrcSpan::default(),
    };

    let _ = unqualified_imports.insert(
        (EcoString::from("mod1"), EcoString::from("Cat")),
        unqualified_import,
    );

    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Named {
        name: EcoString::from("Cat"),
        args: vec![],
        module: EcoString::from("mod1"),
        publicity: crate::ast::Publicity::Public,
        package: EcoString::from(""),
    };

    assert_eq!(printer.print_type(&typ), "C");
}

#[test]
fn test_module_imports() {
    let type_aliases = HashMap::new();
    let mut module_aliases = HashMap::new();
    let generic_annotations = HashMap::new();
    let unqualified_imports = HashMap::new();
    let mut unqualified_type_names = HashSet::new();

    let _ = module_aliases.insert(EcoString::from("mod"), EcoString::from("animals"));
    let _ = unqualified_type_names.insert(EcoString::from("Cat"));

    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Named {
        name: EcoString::from("Cat"),
        args: vec![],
        module: EcoString::from("mod"),
        publicity: crate::ast::Publicity::Public,
        package: EcoString::from(""),
    };

    let typ1 = Type::Named {
        name: EcoString::from("Cat"),
        args: vec![],
        module: EcoString::from(""),
        publicity: crate::ast::Publicity::Public,
        package: EcoString::from(""),
    };

    assert_eq!(printer.print_type(&typ), "animals.Cat");
    assert_eq!(printer.print_type(&typ1), "Cat");
}

#[test]
fn test_multiple_generic_annotations() {
    let type_aliases = HashMap::new();
    let module_aliases = HashMap::new();
    let mut generic_annotations = HashMap::new();
    let unqualified_imports = HashMap::new();
    let unqualified_type_names = HashSet::new();

    let type_var = TypeAst::Var(crate::ast::TypeAstVar {
        name: EcoString::from("foo"),
        location: crate::ast::SrcSpan::default(),
    });

    let type_var2 = TypeAst::Var(crate::ast::TypeAstVar {
        name: EcoString::from("bar"),
        location: crate::ast::SrcSpan::default(),
    });

    let _ = generic_annotations.insert(0, type_var);
    let _ = generic_annotations.insert(1, type_var2);

    let mut printer = AnnotationPrinter::new(
        &type_aliases,
        &module_aliases,
        &generic_annotations,
        &unqualified_imports,
        &unqualified_type_names,
        EcoString::from(""),
    );

    let typ = Type::Named {
        name: EcoString::from("Tiger"),
        args: vec![
            Arc::new(Type::Var {
                type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 0 })),
            }),
            Arc::new(Type::Var {
                type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 1 })),
            }),
        ],
        module: EcoString::from(""),
        publicity: crate::ast::Publicity::Public,
        package: EcoString::from(""),
    };

    let typ1 = Type::Var {
        type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 2 })),
    };

    assert_eq!(printer.print_type(&typ), "Tiger(foo, bar)");
    assert_eq!(printer.print_type(&typ1), "a");
}
