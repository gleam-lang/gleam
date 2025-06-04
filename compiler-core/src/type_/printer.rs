use bimap::BiMap;
use ecow::{EcoString, eco_format};
use im::HashMap;
use std::{collections::HashSet, sync::Arc};

use crate::{
    ast::SrcSpan,
    type_::{Type, TypeVar},
};

/// This class keeps track of what names are used for modules in the current
/// scope, so they can be printed in errors, etc.
///
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Names {
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
    local_types: BiMap<(EcoString, EcoString), EcoString>,

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
    imported_modules: HashMap<EcoString, (EcoString, SrcSpan)>,

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

    /// Constructors which are imported in the current module in an
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
    local_value_constructors: BiMap<(EcoString, EcoString), EcoString>,
}

/// The `PartialEq` implementation for `Type` doesn't account for `TypeVar::Link`,
/// so we implement an equality check that does account for it here.
fn compare_arguments(arguments: &[Arc<Type>], parameters: &[Arc<Type>]) -> bool {
    if arguments.len() != parameters.len() {
        return false;
    }

    arguments
        .iter()
        .zip(parameters)
        .all(|(argument, parameter)| argument.same_as(parameter))
}

impl Names {
    pub fn new() -> Self {
        Self {
            local_types: Default::default(),
            imported_modules: Default::default(),
            type_variables: Default::default(),
            local_value_constructors: Default::default(),
        }
    }

    /// Record a named type in this module.
    pub fn named_type_in_scope(
        &mut self,
        module_name: EcoString,
        type_name: EcoString,
        local_alias: EcoString,
    ) {
        _ = self.local_types.remove_by_right(&local_alias);
        _ = self
            .local_types
            .insert((module_name, type_name), local_alias);
    }

    pub fn type_in_scope(
        &mut self,
        local_alias: EcoString,
        type_: &Type,
        parameters: &[Arc<Type>],
    ) {
        match type_ {
            Type::Named {
                module, name, args, ..
            } if compare_arguments(args, parameters) => {
                self.named_type_in_scope(module.clone(), name.clone(), local_alias);
            }
            Type::Named { .. } | Type::Fn { .. } | Type::Var { .. } | Type::Tuple { .. } => {
                _ = self.local_types.remove_by_right(&local_alias);
            }
        }
    }

    /// Record a type variable in this module.
    pub fn type_variable_in_scope(&mut self, id: u64, local_alias: EcoString) {
        _ = self.type_variables.insert(id, local_alias.clone());
    }

    /// Record an imported module in this module.
    ///
    /// Returns the location of the previous time this module was imported, if there was one.
    pub fn imported_module(
        &mut self,
        module_name: EcoString,
        module_alias: EcoString,
        location: SrcSpan,
    ) -> Option<SrcSpan> {
        self.imported_modules
            .insert(module_name, (module_alias, location))
            .map(|(_, location)| location)
    }

    /// Get the name and optional module qualifier for a named type.
    pub fn named_type<'a>(
        &'a self,
        module: &'a EcoString,
        name: &'a EcoString,
        print_mode: PrintMode,
    ) -> NameContextInformation<'a> {
        if print_mode == PrintMode::ExpandAliases {
            if let Some((module, _)) = self.imported_modules.get(module) {
                return NameContextInformation::Qualified(module, name.as_str());
            };

            return NameContextInformation::Unimported(name.as_str());
        }

        let key = (module.clone(), name.clone());

        // Only check for local aliases if we want to print aliases
        // There is a local name for this type, use that.
        if let Some(name) = self.local_types.get_by_left(&key) {
            return NameContextInformation::Unqualified(name.as_str());
        }

        // This type is from a module that has been imported
        if let Some((module, _)) = self.imported_modules.get(module) {
            return NameContextInformation::Qualified(module, name.as_str());
        };

        NameContextInformation::Unimported(name.as_str())
    }

    /// Record a named value in this module.
    pub fn named_constructor_in_scope(
        &mut self,
        module_name: EcoString,
        value_name: EcoString,
        local_alias: EcoString,
    ) {
        _ = self.local_value_constructors.remove_by_right(&local_alias);
        _ = self
            .local_value_constructors
            .insert((module_name.clone(), value_name), local_alias.clone());
    }

    /// Get the name and optional module qualifier for a named constructor.
    pub fn named_constructor<'a>(
        &'a self,
        module: &'a EcoString,
        name: &'a EcoString,
    ) -> NameContextInformation<'a> {
        let key = (module.clone(), name.clone());

        // There is a local name for this value, use that.
        if let Some(name) = self.local_value_constructors.get_by_left(&key) {
            return NameContextInformation::Unqualified(name.as_str());
        }

        // This value is from a module that has been imported
        if let Some((module, _)) = self.imported_modules.get(module) {
            return NameContextInformation::Qualified(module, name.as_str());
        };

        NameContextInformation::Unimported(name.as_str())
    }

    pub fn is_imported(&self, module: &str) -> bool {
        self.imported_modules.contains_key(module)
    }

    pub fn get_type_variable(&self, id: u64) -> Option<&EcoString> {
        self.type_variables.get(&id)
    }
}

#[derive(Debug)]
pub enum NameContextInformation<'a> {
    /// This type is from a module that has not been imported in this module.
    Unimported(&'a str),
    /// This type has been imported in an unqualifid fashion in this module.
    Unqualified(&'a str),
    /// This type is from a module that has been imported.
    Qualified(&'a str, &'a str),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrintMode {
    /// Prints the context-specific representation of a type.
    Normal,
    /// Prints full detail of the given type, always qualified.
    /// Useful for providing more detail to the user.
    ///
    /// For example, with this code:
    /// ```gleam
    /// type A = Int
    /// ```
    /// If the type `gleam.Int` were printed using the `Normal` mode,
    /// we would print `A`, since that is the local alias for the `Int` type.
    ///
    /// However, if the user were hovering over the type `A` itself, it wouldn't be
    /// particularly helpful to print `A`.
    /// So with `ExpandAliases`, it would print `gleam.Int`,
    /// which tells the user exactly what type `A` represents.
    ///
    ExpandAliases,
}

/// A type printer that does not wrap and indent, but does take into account the
/// names that types and modules have been aliased with in the current module.
#[derive(Debug)]
pub struct Printer<'a> {
    names: &'a Names,
    uid: u64,

    /// Some type variables aren't bound to names, so when trying to print those,
    /// we need to create our own names which don't overlap with existing type variables.
    /// These two data structures store a mapping of IDs to created type-variable names,
    /// to ensure consistent printing, and the set of all printed names so that we don't
    /// create a type variable name which matches an existing one.
    ///
    /// Note: These are stored per printer, not per TypeNames struct, because:
    /// - It doesn't really matter what these are, as long as they are consistent.
    /// - We would need mutable access to the names struct, which isn't really possible
    ///   in many contexts.
    ///
    printed_type_variables: HashMap<u64, EcoString>,
    printed_type_variable_names: HashSet<EcoString>,
}

impl<'a> Printer<'a> {
    pub fn new(names: &'a Names) -> Self {
        Printer {
            names,
            uid: Default::default(),
            printed_type_variables: Default::default(),
            printed_type_variable_names: names.type_variables.values().cloned().collect(),
        }
    }

    pub fn print_type(&mut self, type_: &Type) -> EcoString {
        let mut buffer = EcoString::new();
        self.print(type_, &mut buffer, PrintMode::Normal);
        buffer
    }

    pub fn print_module(&self, module: &str) -> EcoString {
        match self.names.imported_modules.get(module) {
            Some((module, _)) => module.clone(),
            _ => module.split("/").last().unwrap_or(module).into(),
        }
    }

    pub fn print_type_without_aliases(&mut self, type_: &Type) -> EcoString {
        let mut buffer = EcoString::new();
        self.print(type_, &mut buffer, PrintMode::ExpandAliases);
        buffer
    }

    fn print(&mut self, type_: &Type, buffer: &mut EcoString, print_mode: PrintMode) {
        match type_ {
            Type::Named {
                name, args, module, ..
            } => {
                let (module, name) = match self.names.named_type(module, name, print_mode) {
                    NameContextInformation::Qualified(m, n) => (Some(m), n),
                    NameContextInformation::Unqualified(n) => (None, n),
                    // TODO: indicate that the module is not import and as such
                    // needs to be, as well as how.
                    NameContextInformation::Unimported(n) => {
                        (Some(module.split('/').next_back().unwrap_or(module)), n)
                    }
                };

                if let Some(module) = module {
                    buffer.push_str(module);
                    buffer.push('.');
                }
                buffer.push_str(name);

                if !args.is_empty() {
                    buffer.push('(');
                    self.print_arguments(args, buffer, print_mode);
                    buffer.push(')');
                }
            }

            Type::Fn { args, return_ } => {
                buffer.push_str("fn(");
                self.print_arguments(args, buffer, print_mode);
                buffer.push_str(") -> ");
                self.print(return_, buffer, print_mode);
            }

            Type::Var { type_, .. } => match *type_.borrow() {
                TypeVar::Link { ref type_, .. } => self.print(type_, buffer, print_mode),
                TypeVar::Unbound { id, .. } | TypeVar::Generic { id, .. } => {
                    buffer.push_str(&self.type_variable(id))
                }
            },

            Type::Tuple { elements, .. } => {
                buffer.push_str("#(");
                self.print_arguments(elements, buffer, print_mode);
                buffer.push(')');
            }
        }
    }

    pub fn print_constructor(&mut self, module: &EcoString, name: &EcoString) -> EcoString {
        let (module, name) = match self.names.named_constructor(module, name) {
            NameContextInformation::Qualified(module, name) => (Some(module), name),
            NameContextInformation::Unqualified(name) => (None, name),
            NameContextInformation::Unimported(name) => {
                (Some(module.split('/').next_back().unwrap_or(module)), name)
            }
        };

        match module {
            Some(module) => eco_format!("{module}.{name}"),
            None => name.into(),
        }
    }

    fn print_arguments(
        &mut self,
        args: &[Arc<Type>],
        type_str: &mut EcoString,
        print_mode: PrintMode,
    ) {
        for (i, arg) in args.iter().enumerate() {
            self.print(arg, type_str, print_mode);
            if i < args.len() - 1 {
                type_str.push_str(", ");
            }
        }
    }

    /// A suitable name of a type variable.
    pub fn type_variable(&mut self, id: u64) -> EcoString {
        if let Some(name) = self.names.type_variables.get(&id) {
            return name.clone();
        }

        if let Some(name) = self.printed_type_variables.get(&id) {
            return name.clone();
        }

        loop {
            let name = self.next_letter();
            if !self.printed_type_variable_names.contains(&name) {
                _ = self.printed_type_variable_names.insert(name.clone());
                _ = self.printed_type_variables.insert(id, name.clone());
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

#[test]
fn test_local_type() {
    let mut names = Names::new();
    names.named_type_in_scope("mod".into(), "Tiger".into(), "Cat".into());
    let mut printer = Printer::new(&names);

    let type_ = Type::Named {
        name: "Tiger".into(),
        args: vec![],
        module: "mod".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
        inferred_variant: None,
    };

    assert_eq!(printer.print_type(&type_), "Cat");
}

#[test]
fn test_prelude_type() {
    let mut names = Names::new();
    names.named_type_in_scope("gleam".into(), "Int".into(), "Int".into());
    let mut printer = Printer::new(&names);

    let type_ = Type::Named {
        name: "Int".into(),
        args: vec![],
        module: "gleam".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
        inferred_variant: None,
    };

    assert_eq!(printer.print_type(&type_), "Int");
}

#[test]
fn test_shadowed_prelude_type() {
    let mut names = Names::new();

    names.named_type_in_scope("gleam".into(), "Int".into(), "Int".into());
    names.named_type_in_scope("mod".into(), "Int".into(), "Int".into());

    let mut printer = Printer::new(&names);

    let type_ = Type::Named {
        name: "Int".into(),
        args: vec![],
        module: "gleam".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
        inferred_variant: None,
    };

    assert_eq!(printer.print_type(&type_), "gleam.Int");
}

#[test]
fn test_generic_type_annotation() {
    let mut names = Names::new();
    names.type_variable_in_scope(0, "one".into());
    let mut printer = Printer::new(&names);

    let type_ = Type::Var {
        type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 0 })),
    };

    assert_eq!(printer.print_type(&type_), "one");
}

#[test]
fn test_generic_type_var() {
    let names = Names::new();
    let mut printer = Printer::new(&names);

    let type_ = Type::Var {
        type_: Arc::new(std::cell::RefCell::new(TypeVar::Unbound { id: 0 })),
    };

    let typ2 = Type::Var {
        type_: Arc::new(std::cell::RefCell::new(TypeVar::Unbound { id: 1 })),
    };

    assert_eq!(printer.print_type(&type_), "a");
    assert_eq!(printer.print_type(&typ2), "b");
}

#[test]
fn test_tuple_type() {
    let names = Names::new();
    let mut printer = Printer::new(&names);

    let type_ = Type::Tuple {
        elements: vec![
            Arc::new(Type::Named {
                name: "Int".into(),
                args: vec![],
                module: "gleam".into(),
                publicity: crate::ast::Publicity::Public,
                package: "".into(),
                inferred_variant: None,
            }),
            Arc::new(Type::Named {
                name: "String".into(),
                args: vec![],
                module: "gleam".into(),
                publicity: crate::ast::Publicity::Public,
                package: "".into(),
                inferred_variant: None,
            }),
        ],
    };

    assert_eq!(printer.print_type(&type_), "#(gleam.Int, gleam.String)");
}

#[test]
fn test_fn_type() {
    let mut names = Names::new();
    names.named_type_in_scope("gleam".into(), "Int".into(), "Int".into());
    names.named_type_in_scope("gleam".into(), "Bool".into(), "Bool".into());
    let mut printer = Printer::new(&names);

    let type_ = Type::Fn {
        args: vec![
            Arc::new(Type::Named {
                name: "Int".into(),
                args: vec![],
                module: "gleam".into(),
                publicity: crate::ast::Publicity::Public,
                package: "".into(),
                inferred_variant: None,
            }),
            Arc::new(Type::Named {
                name: "String".into(),
                args: vec![],
                module: "gleam".into(),
                publicity: crate::ast::Publicity::Public,
                package: "".into(),
                inferred_variant: None,
            }),
        ],
        return_: Arc::new(Type::Named {
            name: "Bool".into(),
            args: vec![],
            module: "gleam".into(),
            publicity: crate::ast::Publicity::Public,
            package: "".into(),
            inferred_variant: None,
        }),
    };

    assert_eq!(printer.print_type(&type_), "fn(Int, gleam.String) -> Bool");
}

#[test]
fn test_module_alias() {
    let mut names = Names::new();

    assert!(
        names
            .imported_module("mod1".into(), "animals".into(), SrcSpan::new(50, 63))
            .is_none()
    );

    let mut printer = Printer::new(&names);

    let type_ = Type::Named {
        name: "Cat".into(),
        args: vec![],
        module: "mod1".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
        inferred_variant: None,
    };

    assert_eq!(printer.print_type(&type_), "animals.Cat");
}

#[test]
fn test_type_alias_and_generics() {
    let mut names = Names::new();

    names.named_type_in_scope("mod".into(), "Tiger".into(), "Cat".into());

    names.type_variable_in_scope(0, "one".into());

    let mut printer = Printer::new(&names);

    let type_ = Type::Named {
        name: "Tiger".into(),
        args: vec![Arc::new(Type::Var {
            type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 0 })),
        })],
        module: "mod".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
        inferred_variant: None,
    };

    assert_eq!(printer.print_type(&type_), "Cat(one)");
}

#[test]
fn test_unqualified_import_and_generic() {
    let mut names = Names::new();

    names.named_type_in_scope("mod".into(), "Cat".into(), "C".into());

    names.type_variable_in_scope(0, "one".into());

    let mut printer = Printer::new(&names);

    let type_ = Type::Named {
        name: "Cat".into(),
        args: vec![Arc::new(Type::Var {
            type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 0 })),
        })],
        module: "mod".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
        inferred_variant: None,
    };

    assert_eq!(printer.print_type(&type_), "C(one)");
}

#[test]
fn nested_module() {
    let names = Names::new();
    let mut printer = Printer::new(&names);
    let type_ = Type::Named {
        name: "Cat".into(),
        args: vec![],
        module: "one/two/three".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
        inferred_variant: None,
    };

    assert_eq!(printer.print_type(&type_), "three.Cat");
}

#[test]
fn test_unqualified_import_and_module_alias() {
    let mut names = Names::new();

    assert!(
        names
            .imported_module("mod1".into(), "animals".into(), SrcSpan::new(76, 93))
            .is_none()
    );

    let _ = names
        .local_types
        .insert(("mod1".into(), "Cat".into()), "C".into());

    let mut printer = Printer::new(&names);

    let type_ = Type::Named {
        name: "Cat".into(),
        args: vec![],
        module: "mod1".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
        inferred_variant: None,
    };

    assert_eq!(printer.print_type(&type_), "C");
}

#[test]
fn test_module_imports() {
    let mut names = Names::new();

    assert!(
        names
            .imported_module("mod".into(), "animals".into(), SrcSpan::new(76, 93))
            .is_none()
    );

    let _ = names
        .local_types
        .insert(("mod2".into(), "Cat".into()), "Cat".into());

    let mut printer = Printer::new(&names);

    let type_ = Type::Named {
        name: "Cat".into(),
        args: vec![],
        module: "mod".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
        inferred_variant: None,
    };

    let typ1 = Type::Named {
        name: "Cat".into(),
        args: vec![],
        module: "mod2".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
        inferred_variant: None,
    };

    assert_eq!(printer.print_type(&type_), "animals.Cat");
    assert_eq!(printer.print_type(&typ1), "Cat");
}

#[test]
fn test_multiple_generic_annotations() {
    let mut names = Names::new();

    names.type_variable_in_scope(0, "one".into());
    names.type_variable_in_scope(1, "two".into());

    let mut printer = Printer::new(&names);

    let type_ = Type::Named {
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
        inferred_variant: None,
    };

    let typ1 = Type::Var {
        type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 2 })),
    };

    assert_eq!(printer.print_type(&type_), "tigermodule.Tiger(one, two)");
    assert_eq!(printer.print_type(&typ1), "a");
}

#[test]
fn test_variable_name_already_in_scope() {
    let mut names = Names::new();

    names.type_variable_in_scope(1, "a".into());
    names.type_variable_in_scope(2, "b".into());

    let mut printer = Printer::new(&names);

    let type_ = |id| Type::Var {
        type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id })),
    };

    assert_eq!(printer.print_type(&type_(0)), "c");
    assert_eq!(printer.print_type(&type_(1)), "a");
    assert_eq!(printer.print_type(&type_(2)), "b");
    assert_eq!(printer.print_type(&type_(3)), "d");
}
