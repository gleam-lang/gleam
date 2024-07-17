#![allow(dead_code)]

use ecow::EcoString;
use im::{HashMap, HashSet};
use std::sync::Arc;

use crate::type_::{Type, TypeVar};

/// This class keeps track of what names are used for modules in the current
/// scope, so they can be printed in errors, etc.
///
#[derive(Debug)]
pub struct TypeNames {
    uid: u64,
    current_module: EcoString,

    /// Types that exist in the current module, either defined or imported in an
    /// unqualified fashion.
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
    /// # Example 2
    ///
    /// ```gleam
    /// import some/module.{type Wibble}
    /// ```
    /// would result in
    /// - key:   `("some/module", "Wibble")`
    /// - value: `"Wibble"`
    ///
    /// # Example 3
    ///
    /// ```gleam
    /// import some/module.{type Wibble as Wobble}
    /// ```
    /// would result in
    /// - key:   `("some/module", "Wibble")`
    /// - value: `"Wobble"`
    ///
    local_types: HashMap<(EcoString, EcoString), EcoString>,

    /// Values which are imported in the current module in an
    /// unqualified fashion.
    ///
    /// key:   (Defining module name, type name)
    /// value: Alias name
    ///
    /// # Example 1
    ///
    /// ```gleam
    /// import wibble.{Wobble}
    /// ```
    /// would result in
    /// - key:   `("wibble", "Wobble")`
    /// - value: `"Wobble"`
    ///
    /// # Example 2
    ///
    /// ```gleam
    /// import wibble.{Wobble as Woo}
    /// ```
    /// would result in
    /// - key:   `("wibble", "Wobble")`
    /// - value: `"Woo"`
    ///
    local_values: HashMap<(EcoString, EcoString), EcoString>,

    /// Mapping of imported modules to their locally used named
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
    /// # Example 2
    ///
    /// ```gleam
    /// import mod1
    /// ```
    /// would result in:
    /// - key:   "mod1"
    /// - value: "mod1"
    ///
    imported_modules: HashMap<EcoString, EcoString>,

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
    type_variables: HashMap<u64, EcoString>,
    type_variable_names: HashSet<EcoString>,
}

impl TypeNames {
    pub fn new(current_module: EcoString) -> Self {
        Self {
            uid: Default::default(),
            current_module,
            local_types: Default::default(),
            local_values: Default::default(),
            imported_modules: Default::default(),
            type_variables: Default::default(),
            type_variable_names: Default::default(),
        }
    }

    /// Record a named type in this module.
    pub fn named_type_in_scope(
        &mut self,
        module_name: EcoString,
        type_name: EcoString,
        local_alias: EcoString,
    ) {
        _ = self
            .local_types
            .insert((module_name, type_name), local_alias);
    }

    /// Record a named value in this module.
    pub fn named_value_in_scope(
        &mut self,
        module_name: EcoString,
        value_name: EcoString,
        local_alias: EcoString,
    ) {
        _ = self
            .local_values
            .insert((module_name, value_name), local_alias);
    }

    /// Record a type variable in this module.
    pub fn type_variable_in_scope(&mut self, id: u64, local_alias: EcoString) {
        _ = self.type_variables.insert(id, local_alias.clone());
        _ = self.type_variable_names.insert(local_alias);
    }

    /// Record an imported module in this module.
    pub fn imported_module(&mut self, module_name: EcoString, module_alias: EcoString) {
        _ = self.imported_modules.insert(module_name, module_alias)
    }

    /// Get the name and optional module qualifier for a named type.
    pub fn named_type<'a>(
        &'a self,
        module: &'a EcoString,
        name: &'a EcoString,
    ) -> NamedTypeNames<'a> {
        let key = (module.clone(), name.clone());

        // There is a local name for this type, use that.
        if let Some(name) = self.local_types.get(&key) {
            return NamedTypeNames::Unqualified(name.as_str());
        }

        // This type is from a module that has been imported
        if let Some(module) = self.imported_modules.get(module) {
            return NamedTypeNames::Qualified(module, name.as_str());
        };

        return NamedTypeNames::Unimported(name.as_str());
    }

    /// Get the name and optional module qualifier for a named type.
    pub fn named_value<'a>(
        &'a self,
        module: &'a EcoString,
        name: &'a EcoString,
    ) -> NamedTypeNames<'a> {
        let key: (EcoString, EcoString) = (module.clone(), name.clone());

        // There is a local name for this type, use that.
        if let Some(name) = self.local_values.get(&key) {
            return NamedTypeNames::Unqualified(name.as_str());
        }

        // This type is from a module that has been imported
        if let Some(module) = self.imported_modules.get(module) {
            return NamedTypeNames::Qualified(module, name.as_str());
        };

        return NamedTypeNames::Unimported(name.as_str());
    }

    /// A suitable name of a type variable.
    pub fn type_variable(&mut self, id: u64) -> EcoString {
        if let Some(name) = self.type_variables.get(&id) {
            return name.clone();
        }

        loop {
            let name = self.next_letter();
            if !self.type_variable_names.contains(&name) {
                _ = self.type_variable_names.insert(name.clone());
                _ = self.type_variables.insert(id, name.clone());
                return name;
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

#[derive(Debug)]
pub enum NamedTypeNames<'a> {
    /// This type is from a module that has not been imported in this module.
    Unimported(&'a str),
    /// This type has been imported in an unqualifid fashion in this module.
    Unqualified(&'a str),
    /// This type is from a module that has been imported.
    Qualified(&'a str, &'a str),
}

/// A type printer that does not wrap and indent, but does take into account the
/// names that types and modules have been aliased with in the current module.
#[derive(Debug)]
pub struct Printer<'a> {
    names: &'a mut TypeNames,
}

impl<'a> Printer<'a> {
    pub fn new(names: &'a mut TypeNames) -> Self {
        Printer { names }
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
                let (module, name) = match self.names.named_type(module, name) {
                    NamedTypeNames::Qualified(m, n) => (Some(m), n),
                    NamedTypeNames::Unqualified(n) => (None, n),
                    // TODO: indicate that the module is not import and as such
                    // needs to be, as well as how.
                    NamedTypeNames::Unimported(n) => {
                        (Some(module.split('/').last().unwrap_or(module)), n)
                    }
                };

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
                    buffer.push_str(&self.names.type_variable(id))
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
}

#[test]
fn test_local_type() {
    let mut names = TypeNames::new("module".into());
    names.named_type_in_scope("mod".into(), "Tiger".into(), "Cat".into());
    let mut printer = Printer::new(&mut names);

    let typ = Type::Named {
        name: "Tiger".into(),
        args: vec![],
        module: "mod".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
    };

    assert_eq!(printer.print_type(&typ), "Cat");
}

#[test]
fn test_generic_type_annotation() {
    let mut names = TypeNames::new("module".into());
    names.type_variable_in_scope(0, "one".into());
    let mut printer = Printer::new(&mut names);

    let typ = Type::Var {
        type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 0 })),
    };

    assert_eq!(printer.print_type(&typ), "one");
}

#[test]
fn test_generic_type_var() {
    let mut names = TypeNames::new("module".into());
    let mut printer = Printer::new(&mut names);

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
    let mut names = TypeNames::new("module".into());
    let mut printer = Printer::new(&mut names);

    let typ = Type::Tuple {
        elems: vec![
            Arc::new(Type::Named {
                name: "Int".into(),
                args: vec![],
                module: "gleam".into(),
                publicity: crate::ast::Publicity::Public,
                package: "".into(),
            }),
            Arc::new(Type::Named {
                name: "String".into(),
                args: vec![],
                module: "gleam".into(),
                publicity: crate::ast::Publicity::Public,
                package: "".into(),
            }),
        ],
    };

    assert_eq!(printer.print_type(&typ), "#(gleam.Int, gleam.String)");
}

#[test]
fn test_fn_type() {
    let mut names = TypeNames::new("module".into());
    names.named_type_in_scope("gleam".into(), "Int".into(), "Int".into());
    names.named_type_in_scope("gleam".into(), "Bool".into(), "Bool".into());
    let mut printer = Printer::new(&mut names);

    let typ = Type::Fn {
        args: vec![
            Arc::new(Type::Named {
                name: "Int".into(),
                args: vec![],
                module: "gleam".into(),
                publicity: crate::ast::Publicity::Public,
                package: "".into(),
            }),
            Arc::new(Type::Named {
                name: "String".into(),
                args: vec![],
                module: "gleam".into(),
                publicity: crate::ast::Publicity::Public,
                package: "".into(),
            }),
        ],
        retrn: Arc::new(Type::Named {
            name: "Bool".into(),
            args: vec![],
            module: "gleam".into(),
            publicity: crate::ast::Publicity::Public,
            package: "".into(),
        }),
    };

    assert_eq!(printer.print_type(&typ), "fn(Int, gleam.String) -> Bool");
}

#[test]
fn test_module_alias() {
    let mut names = TypeNames::new("module".into());
    names.imported_module("mod1".into(), "animals".into());
    let mut printer = Printer::new(&mut names);

    let typ = Type::Named {
        name: "Cat".into(),
        args: vec![],
        module: "mod1".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
    };

    assert_eq!(printer.print_type(&typ), "animals.Cat");
}

#[test]
fn test_type_alias_and_generics() {
    let mut names = TypeNames::new("module".into());

    names.named_type_in_scope("mod".into(), "Tiger".into(), "Cat".into());

    names.type_variable_in_scope(0, "one".into());

    let mut printer = Printer::new(&mut names);

    let typ = Type::Named {
        name: "Tiger".into(),
        args: vec![Arc::new(Type::Var {
            type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 0 })),
        })],
        module: "mod".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
    };

    assert_eq!(printer.print_type(&typ), "Cat(one)");
}

#[test]
fn test_unqualified_import_and_generic() {
    let mut names = TypeNames::new("module".into());

    names.named_type_in_scope("mod".into(), "Cat".into(), "C".into());

    names.type_variable_in_scope(0, "one".into());

    let mut printer = Printer::new(&mut names);

    let typ = Type::Named {
        name: "Cat".into(),
        args: vec![Arc::new(Type::Var {
            type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 0 })),
        })],
        module: "mod".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
    };

    assert_eq!(printer.print_type(&typ), "C(one)");
}

#[test]
fn nested_module() {
    let mut names = TypeNames::new("module".into());
    let mut printer = Printer::new(&mut names);
    let typ = Type::Named {
        name: "Cat".into(),
        args: vec![],
        module: "one/two/three".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
    };

    assert_eq!(printer.print_type(&typ), "three.Cat");
}

#[test]
fn test_unqualified_import_and_module_alias() {
    let mut names = TypeNames::new("module".into());

    names.imported_module("mod1".into(), "animals".into());

    let _ = names
        .local_types
        .insert(("mod1".into(), "Cat".into()), "C".into());

    let mut printer = Printer::new(&mut names);

    let typ = Type::Named {
        name: "Cat".into(),
        args: vec![],
        module: "mod1".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
    };

    assert_eq!(printer.print_type(&typ), "C");
}

#[test]
fn test_module_imports() {
    let mut names = TypeNames::new("module".into());
    names.imported_module("mod".into(), "animals".into());
    let _ = names
        .local_types
        .insert(("mod2".into(), "Cat".into()), "Cat".into());

    let mut printer = Printer::new(&mut names);

    let typ = Type::Named {
        name: "Cat".into(),
        args: vec![],
        module: "mod".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
    };

    let typ1 = Type::Named {
        name: "Cat".into(),
        args: vec![],
        module: "mod2".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
    };

    assert_eq!(printer.print_type(&typ), "animals.Cat");
    assert_eq!(printer.print_type(&typ1), "Cat");
}

#[test]
fn test_multiple_generic_annotations() {
    let mut names = TypeNames::new("module".into());

    names.type_variable_in_scope(0, "one".into());
    names.type_variable_in_scope(1, "two".into());

    let mut printer = Printer::new(&mut names);

    let typ = Type::Named {
        name: "Tiger".into(),
        args: vec![
            Arc::new(Type::Var {
                type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 0 })),
            }),
            Arc::new(Type::Var {
                type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 1 })),
            }),
        ],
        module: "tigermodule".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
    };

    let typ1 = Type::Var {
        type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 2 })),
    };

    assert_eq!(printer.print_type(&typ), "tigermodule.Tiger(one, two)");
    assert_eq!(printer.print_type(&typ1), "a");
}

#[test]
fn test_variable_name_already_in_scope() {
    let mut names = TypeNames::new("module".into());

    names.type_variable_in_scope(1, "a".into());
    names.type_variable_in_scope(2, "b".into());

    let mut printer = Printer::new(&mut names);

    let type_ = |id| Type::Var {
        type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id })),
    };

    assert_eq!(printer.print_type(&type_(0)), "c");
    assert_eq!(printer.print_type(&type_(1)), "a");
    assert_eq!(printer.print_type(&type_(2)), "b");
    assert_eq!(printer.print_type(&type_(3)), "d");
}
