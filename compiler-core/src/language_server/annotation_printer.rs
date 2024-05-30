#![allow(dead_code)]

use ecow::EcoString;
use im::{HashMap, HashSet};
use std::sync::Arc;

use crate::{
    ast::{TypeAst, UnqualifiedImport},
    type_::{Type, TypeVar, PRELUDE_MODULE_NAME},
};

#[derive(Debug, Default)]
pub struct TypeNames {
    pub current_module: EcoString,

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
    pub type_aliases: HashMap<(EcoString, EcoString), EcoString>,

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
    pub module_aliases: HashMap<EcoString, EcoString>,

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
    pub generic_annotations: HashMap<u64, TypeAst>,

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
    pub unqualified_imports: HashMap<(EcoString, EcoString), UnqualifiedImport>,

    /// A set of the names of types that are defined in this module. Used to
    /// identify any prelude types that have been shadowed.
    ///
    pub locally_defined_types: HashSet<EcoString>,
}

impl TypeNames {
    fn named_type<'a>(
        &'a self,
        module: &'a EcoString,
        name: &'a EcoString,
    ) -> (Option<&'a str>, &'a str) {
        let key = (module.clone(), name.clone());

        // There is a local type alias for this type, use that.
        if let Some(type_alias) = self.type_aliases.get(&key) {
            return (None, type_alias.as_str());
        }

        // This is a prelude type and it has not been shadowed.
        if module == PRELUDE_MODULE_NAME && !self.locally_defined_types.contains(name) {
            return (None, name.as_str());
        }

        // This is a type that has been imported in an unqualified fashion
        if let Some(unqualified_import) = self.unqualified_imports.get(&key) {
            if let Some(as_name) = &unqualified_import.as_name {
                return (None, as_name.as_str());
            } else {
                return (None, name.as_str());
            }
        }

        // This type is from a module that has been imported
        if let Some(module_alias) = self.module_aliases.get(module) {
            return (Some(module_alias), name.as_str());
        };

        if module != &self.current_module {
            return (Some(module), name.as_str());
        }

        // TODO: handle the module having not been imported. I guess it should
        // be qualified?
        return (None, name.as_str());
    }
}

#[derive(Debug)]
pub struct AnnotationPrinter<'a> {
    generated_names: HashMap<u64, EcoString>,
    uid: u64,
    names: &'a TypeNames,
}

impl<'a> AnnotationPrinter<'a> {
    pub fn new(names: &'a TypeNames) -> Self {
        AnnotationPrinter {
            generated_names: HashMap::new(),
            uid: u64::default(),
            names,
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
                let (module, name) = self.names.named_type(module, name);

                if let Some(module) = module {
                    buffer.push_str(module);
                    buffer.push('.');
                }
                buffer.push_str(name);

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
                    if let Some(TypeAst::Var(type_var)) = self.names.generic_annotations.get(&id) {
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
        match self.generated_names.get(&id) {
            Some(n) => {
                typ_str.push_str(n);
            }
            None => {
                let n = self.next_letter();
                let _ = self.generated_names.insert(id, n.clone());
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
    let mut names = TypeNames::default();
    let _ = names.type_aliases.insert(
        (EcoString::from("mod"), EcoString::from("Tiger")),
        EcoString::from("Cat"),
    );
    let mut printer = AnnotationPrinter::new(&names);

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
    let mut names = TypeNames::default();
    let _ = names.generic_annotations.insert(
        0,
        TypeAst::Var(crate::ast::TypeAstVar {
            name: EcoString::from("foo"),
            location: crate::ast::SrcSpan::default(),
        }),
    );
    let mut printer = AnnotationPrinter::new(&names);

    let typ = Type::Var {
        type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 0 })),
    };

    assert_eq!(printer.print_type(&typ), "foo");
}

#[test]
fn test_unqualified_import() {
    let mut names = TypeNames::default();
    let _ = names.unqualified_imports.insert(
        (EcoString::from("mod"), EcoString::from("Cat")),
        UnqualifiedImport {
            name: EcoString::from("Cat"),
            as_name: Some(EcoString::from("C")),
            location: crate::ast::SrcSpan::default(),
        },
    );

    let mut printer = AnnotationPrinter::new(&names);

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
    let mut names = TypeNames::default();
    let _ = names.locally_defined_types.insert(EcoString::from("Int"));

    let mut printer = AnnotationPrinter::new(&names);

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
    let names = TypeNames::default();
    let mut printer = AnnotationPrinter::new(&names);

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
    let names = TypeNames::default();
    let mut printer = AnnotationPrinter::new(&names);

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
    let names = TypeNames::default();
    let mut printer = AnnotationPrinter::new(&names);

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
    let mut names = TypeNames::default();
    let _ = names
        .locally_defined_types
        .insert(EcoString::from("String"));
    let mut printer = AnnotationPrinter::new(&names);

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
    let mut names = TypeNames::default();
    let _ = names
        .module_aliases
        .insert(EcoString::from("mod1"), EcoString::from("animals"));
    let mut printer = AnnotationPrinter::new(&names);

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
    let mut names = TypeNames::default();

    let _ = names.type_aliases.insert(
        (EcoString::from("mod"), EcoString::from("Tiger")),
        EcoString::from("Cat"),
    );

    let type_var = TypeAst::Var(crate::ast::TypeAstVar {
        name: EcoString::from("foo"),
        location: crate::ast::SrcSpan::default(),
    });

    let _ = names.generic_annotations.insert(0, type_var);

    let mut printer = AnnotationPrinter::new(&names);

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
    let mut names = TypeNames::default();

    let unqualified_import = UnqualifiedImport {
        name: EcoString::from("Cat"),
        as_name: Some(EcoString::from("C")),
        location: crate::ast::SrcSpan::default(),
    };

    let _ = names.unqualified_imports.insert(
        (EcoString::from("mod"), EcoString::from("Cat")),
        unqualified_import,
    );

    let type_var = TypeAst::Var(crate::ast::TypeAstVar {
        name: EcoString::from("foo"),
        location: crate::ast::SrcSpan::default(),
    });

    let _ = names.generic_annotations.insert(0, type_var);

    let mut printer = AnnotationPrinter::new(&names);

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
    let mut names = TypeNames::default();

    let _ = names
        .module_aliases
        .insert(EcoString::from("mod1"), EcoString::from("animals"));

    let unqualified_import = UnqualifiedImport {
        name: EcoString::from("Cat"),
        as_name: Some(EcoString::from("C")),
        location: crate::ast::SrcSpan::default(),
    };

    let _ = names.unqualified_imports.insert(
        (EcoString::from("mod1"), EcoString::from("Cat")),
        unqualified_import,
    );

    let mut printer = AnnotationPrinter::new(&names);

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
    let mut names = TypeNames::default();
    let _ = names
        .module_aliases
        .insert(EcoString::from("mod"), EcoString::from("animals"));
    let _ = names.locally_defined_types.insert(EcoString::from("Cat"));

    let mut printer = AnnotationPrinter::new(&names);

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
    let mut names = TypeNames::default();

    let type_var = TypeAst::Var(crate::ast::TypeAstVar {
        name: EcoString::from("foo"),
        location: crate::ast::SrcSpan::default(),
    });

    let type_var2 = TypeAst::Var(crate::ast::TypeAstVar {
        name: EcoString::from("bar"),
        location: crate::ast::SrcSpan::default(),
    });

    let _ = names.generic_annotations.insert(0, type_var);
    let _ = names.generic_annotations.insert(1, type_var2);

    let mut printer = AnnotationPrinter::new(&names);

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
