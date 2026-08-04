// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 The Gleam contributors

use bimap::{BiHashMap, BiMap};
use ecow::{EcoString, eco_format};
use im::HashMap;
use itertools::Itertools;
use std::{collections::HashSet, sync::Arc};

use crate::{
    ast::SrcSpan,
    type_::{
        Type, TypeAliasConstructor, TypeVar, collapse_links, fn_, generalise, tuple, unbound_var,
        unify,
    },
};

/// This class keeps track of what names are used for modules in the current
/// scope, so they can be printed in errors, etc.
///
#[derive(Debug, Clone, PartialEq, Eq, Default, serde::Serialize, serde::Deserialize)]
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

    /// A map containing information about type aliases available to the printer.
    /// This includes all aliases in the current module, as well as public
    /// aliases of internal types discovered in other modules. Reexporting an
    /// internal type without exposing its implementation details is a common
    /// pattern in Gleam, so we want to use the public alias rather than the
    /// internal underlying type. Since Gleam type aliases are not part of the
    /// type system, the printer has to track them separately here.
    ///
    /// For example, if we had the following code:
    ///
    /// ```gleam
    /// // lustre/element.gleam
    /// import lustre/internal
    ///
    /// pub type Element(a) = internal.Element(a)
    /// ```
    ///
    /// This map would contain a named key for `("lustre/internal", "Element")`
    /// with a value for `("lustre/element", "Element")`. This can then be used
    /// to look up the alias we want to print based on the type we are printing.
    ///
    type_aliases: HashMap<TypeAliasKey, Vec<TypeAlias>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
enum TypeAliasKey {
    /// Named types are grouped by their module and name
    Named(EcoString, EcoString),
    /// Functions are grouped by their arity
    Function { arity: usize },
    /// Tuples are grouped by their arity
    Tuple { size: usize },
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
struct TypeAlias {
    // This is the right-hand side of the alias declaration. It is stored as a
    // generic pattern so it can be instantiated and matched against types that
    // are being printed.
    underlying_type_pattern: Arc<Type>,
    // The alias itself might also be generic
    alias_parameter_ids: Vec<u64>,
    // This is information about the alias type, used to print the type if it matches.
    alias_package: EcoString,
    alias_module: EcoString,
    alias_name: EcoString,
    availability: TypeAliasAvailability,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
enum TypeAliasAvailability {
    // Private local aliases and ordinary unqualified imports can be printed in
    // errors and the language server, but not in generated documentation.
    InScopeOnly,
    // Public local aliases and reexports can also be used in documentation.
    Public,
}

#[derive(Debug)]
pub struct ResolvedTypeAlias {
    pub package: EcoString,
    pub module: EcoString,
    pub name: EcoString,
    pub arguments: Vec<Arc<Type>>,
}

#[derive(Debug, Clone, Copy)]
pub enum AliasResolutionMode<'a> {
    /// Errors and the language server use aliases available in the current
    /// scope, and never discover aliases globally.
    InScope,
    /// Documentation uses public aliases, including a unique global reexport.
    Public,
    /// Alias declarations use public aliases, but exclude aliases from the
    /// module being printed to avoid resolving an alias back to itself.
    PublicExcludingModule {
        package: &'a EcoString,
        module: &'a EcoString,
    },
}

impl AliasResolutionMode<'_> {
    fn excludes(&self, alias: &TypeAlias) -> bool {
        matches!(
            self,
            Self::PublicExcludingModule { package, module }
                if (*package, *module) == (&alias.alias_package, &alias.alias_module)
        )
    }
}

impl TypeAliasKey {
    fn new(type_: &Type) -> Option<Self> {
        match type_ {
            Type::Named { module, name, .. } => Some(Self::Named(module.clone(), name.clone())),
            Type::Fn { arguments, .. } => Some(Self::Function {
                arity: arguments.len(),
            }),
            Type::Tuple { elements } => Some(Self::Tuple {
                size: elements.len(),
            }),
            Type::Var { type_ } => match &*type_.borrow() {
                TypeVar::Link { type_ } => Self::new(type_),
                TypeVar::Unbound { .. } | TypeVar::Generic { .. } => None,
            },
        }
    }
}

impl TypeAlias {
    /// Try to make a `TypeAlias` from a discovered type alias declaration.
    /// This function may fail if its type parameters cannot be matched.
    fn new(
        alias_package: &EcoString,
        alias_name: &EcoString,
        alias: &TypeAliasConstructor,
        availability: TypeAliasAvailability,
    ) -> Option<Self> {
        // we need to preserve the order in which type variables go in the
        // type alias so when printing the final type, we can print the
        // generics in the correct order.
        let alias_parameter_ids = alias
            .parameters
            .iter()
            .map(|parameter| parameter.type_variable_id())
            .collect::<Option<Vec<_>>>()?;

        Some(Self {
            underlying_type_pattern: generalise(alias.type_.clone()),
            alias_parameter_ids,
            alias_package: alias_package.clone(),
            alias_module: alias.module.clone(),
            alias_name: alias_name.clone(),
            availability,
        })
    }

    /// Clone the stored type into a disposable pattern for alias matching.
    ///
    /// This is similar to a combination of generalise and instantiate.
    /// Unbound and generic variables become new unbound variables with the
    /// same IDs.
    fn instantiate_pattern(type_: Arc<Type>, variables: &mut HashMap<u64, Arc<Type>>) -> Arc<Type> {
        match type_.as_ref() {
            Type::Named {
                publicity,
                package,
                module,
                name,
                arguments,
                inferred_variant: _,
            } => Arc::new(Type::Named {
                publicity: *publicity,
                package: package.clone(),
                module: module.clone(),
                name: name.clone(),
                arguments: arguments
                    .iter()
                    .map(|type_| Self::instantiate_pattern(type_.clone(), variables))
                    .collect(),
                inferred_variant: None,
            }),
            Type::Fn { arguments, return_ } => fn_(
                arguments
                    .iter()
                    .map(|type_| Self::instantiate_pattern(type_.clone(), variables))
                    .collect(),
                Self::instantiate_pattern(return_.clone(), variables),
            ),
            Type::Tuple { elements } => tuple(
                elements
                    .iter()
                    .map(|type_| Self::instantiate_pattern(type_.clone(), variables))
                    .collect(),
            ),
            Type::Var { type_ } => match &*type_.borrow() {
                TypeVar::Link { type_ } => Self::instantiate_pattern(type_.clone(), variables),
                TypeVar::Unbound { id } | TypeVar::Generic { id } => {
                    if let Some(type_) = variables.get(id) {
                        return type_.clone();
                    }

                    let type_ = unbound_var(*id);
                    let _ = variables.insert(*id, type_.clone());
                    type_
                }
            },
        }
    }

    /// Try to match a type to the underlying type this `TypeAlias` specifies.
    fn match_type(&self, type_: &Type) -> Option<Vec<Arc<Type>>> {
        let mut pattern_variables = HashMap::new();

        // The stored pattern is generic and reused for every match attempt, so
        // instantiate it with fresh unbound variables which `unify` can
        // specialise.
        let pattern =
            Self::instantiate_pattern(self.underlying_type_pattern.clone(), &mut pattern_variables);

        // Make the variables in the type being printed rigid to ensure
        // unify only specialises our disposable pattern variables.
        let type_ = generalise(Arc::new(type_.clone()));

        // The alias can only represent this type if the complete underlying
        // type pattern matches.
        unify(pattern, type_).ok()?;

        // here, we now know we found a match, but we have to re-order the
        // types in the pattern according to the order the outer alias uses.
        let mut alias_arguments = Vec::with_capacity(self.alias_parameter_ids.len());
        for id in &self.alias_parameter_ids {
            let variable = pattern_variables.get(id)?;
            alias_arguments.push(collapse_links(variable.clone()));
        }

        Some(alias_arguments)
    }

    fn resolved(&self, arguments: Vec<Arc<Type>>) -> ResolvedTypeAlias {
        ResolvedTypeAlias {
            package: self.alias_package.clone(),
            module: self.alias_module.clone(),
            name: self.alias_name.clone(),
            arguments,
        }
    }
}

impl Names {
    pub fn new() -> Self {
        Self {
            local_types: BiHashMap::new(),
            imported_modules: HashMap::new(),
            type_variables: HashMap::new(),
            local_value_constructors: BiHashMap::new(),
            type_aliases: HashMap::new(),
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
        package: &EcoString,
        alias_name: &EcoString,
        alias: &TypeAliasConstructor,
    ) {
        let availability = if alias.publicity.is_public() {
            TypeAliasAvailability::Public
        } else {
            TypeAliasAvailability::InScopeOnly
        };
        if self.register_type_alias(package, alias_name, alias, availability) {
            self.named_type_in_scope(alias.module.clone(), alias_name.clone(), alias_name.clone());
        } else {
            _ = self.local_types.remove_by_right(alias_name);
        }
    }

    pub fn imported_type_alias_in_scope(
        &mut self,
        package: &EcoString,
        alias_name: &EcoString,
        local_name: &EcoString,
        alias: &TypeAliasConstructor,
    ) {
        if self.register_type_alias(
            package,
            alias_name,
            alias,
            TypeAliasAvailability::InScopeOnly,
        ) {
            self.named_type_in_scope(alias.module.clone(), alias_name.clone(), local_name.clone());
        } else {
            _ = self.local_types.remove_by_right(local_name);
        }
    }

    fn register_type_alias(
        &mut self,
        package: &EcoString,
        alias_name: &EcoString,
        alias: &TypeAliasConstructor,
        availability: TypeAliasAvailability,
    ) -> bool {
        let Some(alias) = TypeAlias::new(package, alias_name, alias, availability) else {
            return false;
        };
        let Some(key) = TypeAliasKey::new(&alias.underlying_type_pattern) else {
            return false;
        };

        self.type_aliases.entry(key).or_default().push(alias);
        true
    }

    /// Record a type variable in this module.
    pub fn type_variable_in_scope(&mut self, id: u64, local_alias: EcoString) {
        _ = self.type_variables.insert(id, local_alias);
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

    /// Check whether a particular type alias is reexporting an internal type,
    /// and if so register it so we can print it correctly.
    pub fn maybe_register_reexport_alias(
        &mut self,
        package: &EcoString,
        alias_name: &EcoString,
        alias: &TypeAliasConstructor,
    ) {
        //  We only count this alias as a reexport if it is:
        // - a public alias
        // - exposes an internal type
        if !alias.publicity.is_public() || alias.type_.find_internal_type().is_none() {
            return;
        }

        _ = self.register_type_alias(package, alias_name, alias, TypeAliasAvailability::Public);
    }

    /// Get the name and optional module qualifier for a named type.
    fn named_type<'a>(
        &'a self,
        module: &'a EcoString,
        name: &'a EcoString,
        print_mode: PrintMode,
    ) -> NameContextInformation<'a> {
        match print_mode {
            PrintMode::ExpandAliases => match self.imported_modules.get(module) {
                Some((module, _)) => NameContextInformation::Qualified(module, name.as_str()),
                None => NameContextInformation::Unimported(module, name),
            },

            PrintMode::Normal => {
                let key = (module.clone(), name.clone());

                // There is a local name for this type, use that.
                if let Some(name) = self.local_types.get_by_left(&key) {
                    NameContextInformation::Unqualified(name.as_str())
                // This type is from a module that has been imported.
                } else if let Some((module, _)) = self.imported_modules.get(module) {
                    NameContextInformation::Qualified(module, name.as_str())
                } else {
                    NameContextInformation::Unimported(module, name)
                }
            }
        }
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
            .insert((module_name, value_name), local_alias.clone());
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
        }

        NameContextInformation::Unimported(module, name)
    }

    pub fn is_imported(&self, module: &str) -> bool {
        self.imported_modules.contains_key(module)
    }

    pub fn get_type_variable(&self, id: u64) -> Option<&EcoString> {
        self.type_variables.get(&id)
    }

    pub fn type_alias(
        &self,
        type_: &Type,
        mode: AliasResolutionMode<'_>,
    ) -> Option<ResolvedTypeAlias> {
        let scoped_alias = match mode {
            AliasResolutionMode::InScope => {
                // Errors and the language server first prefer an unqualified
                // alias, including private aliases in the current module.
                self.find_matching_type_alias(type_, |alias| {
                    self.unqualified_alias_is_in_scope(alias)
                })
                .or_else(|| {
                    self.find_matching_type_alias(type_, |alias| {
                        alias.availability == TypeAliasAvailability::Public
                            && self.type_alias_is_in_scope(alias)
                    })
                })
            }
            AliasResolutionMode::Public | AliasResolutionMode::PublicExcludingModule { .. } => self
                .find_matching_type_alias(type_, |alias| {
                    alias.availability == TypeAliasAvailability::Public
                        && self.type_alias_is_in_scope(alias)
                        && !mode.excludes(alias)
                }),
        };

        let (alias, arguments) = match scoped_alias {
            Some(alias) => alias,
            None if matches!(mode, AliasResolutionMode::InScope) => return None,
            None => self.find_unique_global_reexport(type_, mode)?,
        };

        Some(alias.resolved(arguments))
    }

    fn find_matching_type_alias<'a>(
        &'a self,
        type_: &Type,
        is_candidate: impl Fn(&TypeAlias) -> bool,
    ) -> Option<(&'a TypeAlias, Vec<Arc<Type>>)> {
        let aliases = self.type_aliases.get(&TypeAliasKey::new(type_)?)?;
        aliases
            .iter()
            .rev()
            .filter(|alias| is_candidate(alias))
            .find_map(|alias| alias.match_type(type_).map(|arguments| (alias, arguments)))
    }

    fn find_unique_global_reexport<'a>(
        &'a self,
        type_: &Type,
        mode: AliasResolutionMode<'_>,
    ) -> Option<(&'a TypeAlias, Vec<Arc<Type>>)> {
        let aliases = self.type_aliases.get(&TypeAliasKey::new(type_)?)?;

        // An out-of-scope reexport is safe to use in documentation only when
        // it is public, is not excluded by alias expansion, and is the sole
        // matching global reexport.
        aliases
            .iter()
            .filter(|alias| {
                alias.availability == TypeAliasAvailability::Public
                    && !self.type_alias_is_in_scope(alias)
                    && !mode.excludes(alias)
            })
            .filter_map(|alias| alias.match_type(type_).map(|arguments| (alias, arguments)))
            .exactly_one()
            .ok()
    }

    fn unqualified_alias_is_in_scope(&self, alias: &TypeAlias) -> bool {
        self.local_types
            .contains_left(&(alias.alias_module.clone(), alias.alias_name.clone()))
    }

    fn type_alias_is_in_scope(&self, alias: &TypeAlias) -> bool {
        self.imported_modules.contains_key(&alias.alias_module)
            || self.unqualified_alias_is_in_scope(alias)
    }
}

#[derive(Debug)]
pub enum NameContextInformation<'a> {
    /// This type is from a module that has not been imported in this module.
    Unimported(&'a str, &'a str),
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
            uid: 0,
            printed_type_variables: HashMap::new(),
            printed_type_variable_names: names.type_variables.values().cloned().collect(),
        }
    }

    /// In the AST, type variables are represented by their IDs, not their names.
    /// This means that when we are printing a type variable, we either need to
    /// find its name that was given by the programmer, or generate a new one.
    /// Type variable names are local to functions, meaning there can be one
    /// named `a` in one function, and a different one named `a` in another
    /// function. However, there can't be two named `a` in the same function.
    ///
    /// By default, the printer avoids duplicating type variable names entirely.
    /// This is because we don't have easy access to information about which type
    /// variables belong to this function. In order to ensure no accidental,
    /// collisions, we treat all type variables from the module as in scope, even
    /// though this isn't the case.
    ///
    /// When sufficient information is present to ensure type variables are not
    /// duplicated, `new_without_type_variables` can be used, in combination with
    /// `register_type_variables` in order to precisely control which variables
    /// are in scope.
    ///
    pub fn new_without_type_variables(names: &'a Names) -> Self {
        Printer {
            names,
            uid: 0,
            printed_type_variables: HashMap::new(),
            printed_type_variable_names: HashSet::new(),
        }
    }

    /// Clear the registered type variable names. This allows the same `Printer`
    /// to be used in multiple different scopes, which have different sets of
    /// type variables. After clearing, the correct variables from the desired
    /// scope can be registered using `register_type_variable`.
    pub fn clear_type_variables(&mut self) {
        self.printed_type_variable_names.clear();
    }

    /// As explained in the documentation for `new_without_type_variables`, it
    /// it not always possible to determine which type variables are in scope.
    /// However, when it is possible, this function can be used to manually
    /// register which type variable names are in scope and cannot be used.
    pub fn register_type_variable(&mut self, name: EcoString) {
        _ = self.printed_type_variable_names.insert(name);
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
        // If this type can be represented as an alias,
        // replace the complete type before printing its underlying structure.
        if print_mode == PrintMode::Normal
            && let Some(type_alias) = self.names.type_alias(type_, AliasResolutionMode::InScope)
        {
            let name = self
                .names
                .named_type(&type_alias.module, &type_alias.name, print_mode);
            self.print_named_type(name, &type_alias.arguments, buffer, print_mode);
            return;
        }

        match type_ {
            Type::Named {
                name,
                arguments,
                module,
                ..
            } => {
                // No alias represented this type, so resolve how the original
                // named type should be qualified and print it.
                let name = self.names.named_type(module, name, print_mode);
                self.print_named_type(name, arguments, buffer, print_mode);
            }

            Type::Fn { arguments, return_ } => {
                buffer.push_str("fn(");
                self.print_arguments(arguments, buffer, print_mode);
                buffer.push_str(") -> ");
                self.print(return_, buffer, print_mode);
            }

            Type::Var { type_, .. } => match *type_.borrow() {
                TypeVar::Link { ref type_, .. } => self.print(type_, buffer, print_mode),
                TypeVar::Unbound { id, .. } | TypeVar::Generic { id, .. } => {
                    buffer.push_str(&self.type_variable(id));
                }
            },

            Type::Tuple { elements, .. } => {
                buffer.push_str("#(");
                self.print_arguments(elements, buffer, print_mode);
                buffer.push(')');
            }
        }
    }

    fn print_named_type(
        &mut self,
        name: NameContextInformation<'_>,
        arguments: &[Arc<Type>],
        buffer: &mut EcoString,
        print_mode: PrintMode,
    ) {
        let (module, name) = match name {
            NameContextInformation::Qualified(module, name) => (Some(module), name),
            NameContextInformation::Unqualified(name) => (None, name),
            // TODO: indicate that the module is not import and as such
            // needs to be, as well as how.
            NameContextInformation::Unimported(module, name) => {
                (module.split('/').next_back(), name)
            }
        };

        if let Some(module) = module {
            buffer.push_str(module);
            buffer.push('.');
        }
        buffer.push_str(name);

        if !arguments.is_empty() {
            buffer.push('(');
            self.print_arguments(arguments, buffer, print_mode);
            buffer.push(')');
        }
    }

    pub fn print_constructor(&mut self, module: &EcoString, name: &EcoString) -> EcoString {
        let (module, name) = match self.names.named_constructor(module, name) {
            NameContextInformation::Qualified(module, name) => (Some(module), name),
            NameContextInformation::Unqualified(name) => (None, name),
            NameContextInformation::Unimported(module, name) => {
                (module.split('/').next_back(), name)
            }
        };

        match module {
            Some(module) => eco_format!("{module}.{name}"),
            None => name.into(),
        }
    }

    fn print_arguments(
        &mut self,
        arguments: &[Arc<Type>],
        type_str: &mut EcoString,
        print_mode: PrintMode,
    ) {
        for (i, argument) in arguments.iter().enumerate() {
            self.print(argument, type_str, print_mode);
            if i < arguments.len() - 1 {
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
            rest -= 1;
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
        arguments: vec![],
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
        arguments: vec![],
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
        arguments: vec![],
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
                arguments: vec![],
                module: "gleam".into(),
                publicity: crate::ast::Publicity::Public,
                package: "".into(),
                inferred_variant: None,
            }),
            Arc::new(Type::Named {
                name: "String".into(),
                arguments: vec![],
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
        arguments: vec![
            Arc::new(Type::Named {
                name: "Int".into(),
                arguments: vec![],
                module: "gleam".into(),
                publicity: crate::ast::Publicity::Public,
                package: "".into(),
                inferred_variant: None,
            }),
            Arc::new(Type::Named {
                name: "String".into(),
                arguments: vec![],
                module: "gleam".into(),
                publicity: crate::ast::Publicity::Public,
                package: "".into(),
                inferred_variant: None,
            }),
        ],
        return_: Arc::new(Type::Named {
            name: "Bool".into(),
            arguments: vec![],
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
        arguments: vec![],
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

    let parameter = crate::type_::generic_var(0);
    names.type_in_scope(
        &"package".into(),
        &"Cat".into(),
        &TypeAliasConstructor {
            publicity: crate::ast::Publicity::Private,
            module: "local".into(),
            type_: Arc::new(Type::Named {
                name: "Tiger".into(),
                arguments: vec![parameter.clone()],
                module: "mod".into(),
                publicity: crate::ast::Publicity::Public,
                package: "package".into(),
                inferred_variant: None,
            }),
            arity: 1,
            deprecation: crate::type_::Deprecation::NotDeprecated,
            documentation: None,
            origin: SrcSpan::new(0, 0),
            parameters: vec![parameter],
        },
    );

    names.type_variable_in_scope(0, "one".into());

    let mut printer = Printer::new(&names);

    let type_ = Type::Named {
        name: "Tiger".into(),
        arguments: vec![Arc::new(Type::Var {
            type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id: 0 })),
        })],
        module: "mod".into(),
        publicity: crate::ast::Publicity::Public,
        package: "package".into(),
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
        arguments: vec![Arc::new(Type::Var {
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
        arguments: vec![],
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
        arguments: vec![],
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
        arguments: vec![],
        module: "mod".into(),
        publicity: crate::ast::Publicity::Public,
        package: "".into(),
        inferred_variant: None,
    };

    let typ1 = Type::Named {
        name: "Cat".into(),
        arguments: vec![],
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
        arguments: vec![
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
