// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

#[cfg(test)]
mod tests;

use crate::{
    package_interface::{
        ImplementationsInterface, PackageInterface, TypeDefinitionInterface, TypeInterface,
    },
    type_::{
        ModuleInterface, Type, TypeConstructor, TypeVar, TypeVariantConstructors,
        ValueConstructorVariant, expression::Implementations,
    },
};
use ecow::EcoString;
use std::{collections::HashMap, sync::Arc};

/// Represents a change in the public API of a package, module, type, or value
/// between two versions of a package.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VersionBump {
    /// A breaking change, meaning old code may break in this version
    Major,
    /// A non-breaking change, meaning old code will continue to function but some
    /// new features was introduced
    Minor,
    /// A patch change, meaning nothing has been added or removed from the public
    /// API, though bug-fixing behavioural changes may have occurred
    Patch,
}

impl VersionBump {
    pub fn to_string(&self) -> &'static str {
        match self {
            VersionBump::Major => "major",
            VersionBump::Minor => "minor",
            VersionBump::Patch => "patch",
        }
    }
}

/// Detect the changes to the public API of a package between two versions.
pub fn detect_changes(
    package_interface: PackageInterface,
    modules: &im::HashMap<EcoString, ModuleInterface>,
) -> VersionBump {
    VersionChecker::new(modules).detect_changes(package_interface)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
struct TypeNames {
    module: EcoString,
    name: EcoString,
}

struct VersionChecker<'a> {
    bump: VersionBump,
    modules: &'a im::HashMap<EcoString, ModuleInterface>,
    moved_types: HashMap<TypeNames, TypeNames>,
}

impl<'a> VersionChecker<'a> {
    fn new(modules: &'a im::HashMap<EcoString, ModuleInterface>) -> Self {
        Self {
            bump: VersionBump::Patch,
            modules,
            moved_types: HashMap::new(),
        }
    }

    fn record_minor_change(&mut self) {
        // If we've already recorded a major bump then there's nothing a minor bump changes here; if
        // we've already recorded a minor bump, then another one also doesn't change anything.
        if self.bump == VersionBump::Patch {
            self.bump = VersionBump::Minor;
        }
    }

    fn record_major_change(&mut self) {
        // There's no higher bump than a major one, so we can safely reassign it here no matter what
        // the value was before.
        self.bump = VersionBump::Major;
    }

    fn detect_changes(mut self, mut package_interface: PackageInterface) -> VersionBump {
        self.find_moved_types(&package_interface);

        for module in self.modules.values() {
            // The list of modules contains all compiled modules including dependencies.
            // We only want to check modules in the current package though, so
            // we skip the others.
            if module.package != package_interface.name {
                continue;
            }

            // Internal modules aren't part of the public API
            if module.is_internal {
                continue;
            }

            // For each module we find in the newer version, we remove it from
            // the list of modules in the old version. That way, if we have any
            // modules left over in the old version at the end, we know it has
            // been removed (or made internal) in the newer version, which makes
            // a breaking change.
            if let Some(mut previous_module) = package_interface.modules.remove(&module.name) {
                for (name, type_) in module.types.iter() {
                    // Private and internal types aren't part of the public API
                    if !type_.publicity.is_public() {
                        continue;
                    }

                    // Like with modules, we remove the type aliases from the old
                    // interface so we know that any left over at the end have
                    // been removed and are a major change.
                    if let Some(alias) = previous_module.type_aliases.remove(name) {
                        // Changing the number of type parameters breaks any type
                        // annotations.
                        if type_.parameters.len() != alias.parameters {
                            return VersionBump::Major;
                        }

                        let remapped_ids = remap_type_ids(type_);

                        // Now we actually need to compare the type that is being
                        // aliased between the two versions.
                        self.compare_types(
                            &alias.alias,
                            &type_.type_,
                            &mut TypeComparisonContext::Constructor(remapped_ids),
                        );
                    } else if let Some(custom_type) = previous_module.types.remove(name) {
                        self.compare_custom_type(module, name, &custom_type, type_);
                    } else {
                        // If the type was neither a type alias nor a custom type
                        // in the previous version, it has been added this version,
                        // which constitutes a minor change.
                        self.record_minor_change();
                    }
                }

                // As with modules and types, we remove the values from the
                // old interface so we know that any left over at the end have
                // been removed and are a major change.
                for (name, value) in module.values.iter() {
                    // Private and internal values aren't part of the public API
                    if !value.publicity.is_public() {
                        continue;
                    }

                    // Custom type variants are handled along with custom types,
                    // so we can skip over them here.
                    if let ValueConstructorVariant::Record { .. } = &value.variant {
                        continue;
                    }

                    if let Some(function) = previous_module.functions.remove(name) {
                        let old_labels: HashMap<_, _> = function
                            .parameters
                            .iter()
                            .enumerate()
                            .filter_map(|(index, parameter)| match &parameter.label {
                                Some(label) => Some((index as u32, label.clone())),
                                None => None,
                            })
                            .collect();

                        if let ValueConstructorVariant::ModuleFn {
                            field_map: Some(field_map),
                            ..
                        } = &value.variant
                            && !field_map.fields.is_empty()
                        {
                            let new_labels = field_map.indices_to_labels();
                            for i in 0..function.parameters.len() as u32 {
                                match (old_labels.get(&i), new_labels.get(&i)) {
                                    // If the old function didn't have a label for the parameter and
                                    // the new one does, it is a minor change.
                                    (None, Some(_)) => self.record_minor_change(),
                                    // If the label has been removed, it's a major change.
                                    (Some(_), None) => return VersionBump::Major,
                                    // If there is still a label but it's different, it's also a major
                                    // change.
                                    (Some(old_label), Some(new_label))
                                        if old_label != *new_label =>
                                    {
                                        return VersionBump::Major;
                                    }
                                    (None, None) | (Some(_), Some(_)) => {}
                                }
                            }
                        } else if !old_labels.is_empty() {
                            // If there are no labels now but there were some in the older version,
                            // they've all been removed and it's a major change.
                            return VersionBump::Major;
                        }

                        // The `FunctionInterface` doesn't directly contain the
                        // type of the function value, so we need to construct
                        // that so we can compare it with the new value.
                        let old_type = TypeInterface::Fn {
                            parameters: function
                                .parameters
                                .into_iter()
                                .map(|parameter| parameter.type_)
                                .collect(),
                            return_: Box::new(function.return_),
                        };

                        self.compare_types(
                            &old_type,
                            &value.type_,
                            &mut TypeComparisonContext::FunctionOrConstant(HashMap::new()),
                        );

                        let new_implementations = match &value.variant {
                            ValueConstructorVariant::LocalVariable { .. }
                            | ValueConstructorVariant::Record { .. } => continue,
                            ValueConstructorVariant::ModuleConstant {
                                implementations, ..
                            } => implementations,
                            ValueConstructorVariant::ModuleFn {
                                implementations, ..
                            } => implementations,
                        };
                        self.compare_implementations(
                            &function.implementations,
                            &new_implementations,
                        );
                    } else if let Some(constant) = previous_module.constants.remove(name) {
                        self.compare_types(
                            &constant.type_,
                            &value.type_,
                            &mut TypeComparisonContext::FunctionOrConstant(HashMap::new()),
                        );

                        let new_implementations = match &value.variant {
                            ValueConstructorVariant::LocalVariable { .. }
                            | ValueConstructorVariant::Record { .. } => continue,
                            ValueConstructorVariant::ModuleConstant {
                                implementations, ..
                            } => implementations,
                            ValueConstructorVariant::ModuleFn {
                                implementations, ..
                            } => implementations,
                        };
                        self.compare_implementations(
                            &constant.implementations,
                            &new_implementations,
                        );

                        // If what was a constant in the old version is now a function with labels,
                        // and they are the same type (checked above), this is effectively adding
                        // labels to a label-less function, which is a minor change.
                        if let ValueConstructorVariant::ModuleFn {
                            field_map: Some(field_map),
                            ..
                        } = &value.variant
                            && !field_map.fields.is_empty()
                        {
                            self.record_minor_change();
                        }
                    } else {
                        // If the value was neither a function nor a constant in
                        // the previous version, it is new in this version, which
                        // is a minor change.
                        self.record_minor_change();
                    }
                }

                // Since we remove types and values as we compare them, if there
                // are any left over at the end, it means they were removed in the
                // new version, which is a major change.
                if !previous_module.is_empty() {
                    return VersionBump::Major;
                }
            } else {
                // This module is new in this version, which constitutes a minor change.
                // We don't care about what types/values are in this module, since they
                // are all new.
                self.record_minor_change();
            }
        }

        // We remove modules as we go, meaning any left over at the end have been
        // removed in the new version, which is a major change.
        if !package_interface.modules.is_empty() {
            return VersionBump::Major;
        }

        self.bump
    }

    /// Detect types which have been "moved", that is, the name of the type has
    /// changed but an alias with the old type name has remained, which is only
    /// a minor change, since any type annotations still work.
    fn find_moved_types(&mut self, package_interface: &PackageInterface) {
        for module in self.modules.values() {
            // Types moved to/from another package are still a breaking change
            if module.package != package_interface.name {
                continue;
            }

            // Internal modules are not part of the public API
            if module.is_internal {
                continue;
            }

            let Some(previous_module) = package_interface.modules.get(&module.name) else {
                continue;
            };

            // Iterate over all the type aliases to see if they point to a type
            // that was at the alias' name in the previous version.
            for (name, alias) in module.type_aliases.iter() {
                // The aliases have to be public for it to not be a breaking change
                if !alias.publicity.is_public() {
                    continue;
                }
                if let Some(custom_type) = previous_module.types.get(name)
                    && let Some(type_) = module.types.get(name)
                    && let Some((new_module, new_name)) = alias.type_.named_type_name()
                {
                    // Changing the type parameters is a breaking change, since it
                    // will break type annotations.
                    if type_.parameters.len() != custom_type.parameters {
                        continue;
                    }
                    // We want to check if this type specifically is compatible, so we need to isolate
                    // the bump from this type. We set our bump to `Patch` temporarily to ensure we
                    // don't detect major changes from other types.
                    // We aren't losing any information here since these types will be checked again
                    // later and this is run before any other checking is performed.
                    self.bump = VersionBump::Patch;
                    self.compare_custom_type(module, name, custom_type, type_);
                    // If it's compatible, record it as a moved type. This type is now effectively
                    // interchangeable with the one it aliases, even though they are different types.
                    if self.bump != VersionBump::Major {
                        _ = self.moved_types.insert(
                            TypeNames {
                                module: new_module,
                                name: new_name,
                            },
                            TypeNames {
                                module: module.name.clone(),
                                name: name.clone(),
                            },
                        );
                    }
                }
            }
        }
    }

    /// Compares a custom type in the old version of the package to the new type.
    /// Even if the new type is a type alias and not a custom type, it may only
    /// be a minor change if the alias points to a compatible custom type.
    fn compare_custom_type(
        &mut self,
        module: &ModuleInterface,
        name: &EcoString,
        custom_type: &TypeDefinitionInterface,
        new_type: &TypeConstructor,
    ) {
        // Changing the number of type parameters breaks any type
        // annotations.
        if new_type.parameters.len() != custom_type.parameters {
            self.record_major_change();
        }

        let remapped_ids = remap_type_ids(new_type);

        // If there are constructors, it's still a custom type, and we can compare
        // the constructors directly to see if any breaking changes have occurred
        // there.
        if let Some(new_constructors) = module.types_value_constructors.get(name) {
            self.do_compare_custom_type(
                custom_type,
                new_constructors,
                remapped_ids,
                &HashMap::new(),
            );
        } else if let Type::Named {
            module: type_module,
            name,
            arguments,
            ..
        } = new_type.type_.as_ref()
            && let Some(type_module) = self.modules.get(type_module)
            && let Some(type_) = type_module.types.get(name)
            && let Some(new_constructors) = type_module.types_value_constructors.get(name)
        {
            // The new type is a type alias to a custom type. We need to check if the custom type is
            // compatible with the old type; if it is, the type is "moved", and it is a minor change,
            // since all type annotations still work correctly.

            // If the old type had public constructors, and the new type is either being made internal
            // or being moved to a different module, that is a major change, since the constructors
            // are no longer available as the public API of this module (even if they are still
            // accessible elsewhere in the package).
            if !custom_type.constructors.is_empty()
                && (type_.publicity.is_internal() || type_module.name != module.name)
            {
                self.record_major_change();
            }

            // Even if the custom type changes its type parameters, the alias can still be made in a
            // way to avoid breaking changes. For example, if we have a type like this:
            //
            // ```gleam
            // pub type Wibble(a, b) {
            //   A(a)
            //   B(b)
            //   C(Int)
            // }
            // ```
            //
            // Then, it is moved to:
            //
            // ```gleam
            // pub type Wobble(a, b, c) {
            //   A(a)
            //   B(b)
            //   C(c)
            // }
            // ```
            //
            // The new type isn't directly compatible with the old type. However, if an aliases is
            // added like this:
            //
            // ```gleam
            // pub type Wibble(a, b) = Wobble(a, b, Int)
            // ```
            //
            // All times that `Wibble` is referenced in type annotations, it refers to the same set
            // of values, which means it's a minor change.
            //
            // In order to correctly compute this, we need to create a list of the type parameters
            // of the custom type and what the type alias assigns to each one, so that we can use
            // that to compare instead of the generic.
            //
            let remapped_parameters = type_
                .parameters
                .iter()
                .filter_map(|parameter| parameter.variable_id())
                .zip(arguments.iter().cloned())
                .collect();

            // Moving a custom type is a minor change, even if no changes are made to the type itself.
            self.record_minor_change();

            // Compare the aliased custom type with the old type to see if they match.
            return self.do_compare_custom_type(
                custom_type,
                new_constructors,
                remapped_ids,
                &remapped_parameters,
            );
        } else {
            // If the new type is an alias to something other than a custom type, it's a major change.
            self.record_major_change();
        }
    }

    /// Compares the actual constructors of two custom types, checking if they are compatible.
    fn do_compare_custom_type(
        &mut self,
        custom_type: &TypeDefinitionInterface,
        new_constructors: &TypeVariantConstructors,
        remapped_ids: HashMap<u64, u64>,
        remapped_parameters: &HashMap<u64, Arc<Type>>,
    ) {
        // If there were previously no constructors (the type was an external type or was opaque)
        // and we have added non-opaque constructors, it's a minor change.
        if custom_type.constructors.is_empty() {
            if !new_constructors.variants.is_empty() && !new_constructors.opaque.is_opaque() {
                self.record_minor_change();
            }
        } else {
            // If there were previously public constructors and we made them opaque, it's a
            // major change.
            if new_constructors.opaque.is_opaque() {
                self.record_major_change();
            }
            // Adding a constructor is a major change as well as removing one, as it breaks any
            // pattern matching on the type.
            if custom_type.constructors.len() != new_constructors.variants.len() {
                self.record_major_change();
            }

            let old_constructors: HashMap<_, _> = custom_type
                .constructors
                .iter()
                .map(|constructor| (&constructor.name, &constructor.parameters))
                .collect();

            let mut context = TypeComparisonContext::Constructor(remapped_ids);

            for new_constructor in new_constructors.variants.iter() {
                // If a constructor has been added or renamed, it's a major change.
                let Some(old_parameters) = old_constructors.get(&new_constructor.name) else {
                    self.record_major_change();
                    return;
                };

                if old_parameters.len() != new_constructor.parameters.len() {
                    self.record_major_change();
                }

                for (old, new) in old_parameters.iter().zip(new_constructor.parameters.iter()) {
                    match (&old.label, &new.label) {
                        (None, None) => {}
                        (Some(old_label), Some(new_label)) if old_label == new_label => {}
                        // Adding a label is a minor change
                        (None, Some(_)) => self.record_minor_change(),
                        // Changing or removing a label is a major change
                        (Some(_), Some(_) | None) => self.record_major_change(),
                    }

                    let new_type = if let Some(id) = new.type_.variable_id()
                        && let Some(remapped) = remapped_parameters.get(&id)
                    {
                        remapped
                    } else {
                        &new.type_
                    };

                    self.compare_types(&old.type_, new_type, &mut context);
                }
            }
        }
    }

    /// Compares two types to see if they are compatible. Has slightly different behaviour for types
    /// and values, as generics are treated differently, meaning some changes in a constructor's type
    /// is a breaking change, whereas it isn't for a function's type signature.
    fn compare_types<'b>(
        &mut self,
        old_type: &'b TypeInterface,
        new_type: &Type,
        context: &mut TypeComparisonContext<'b>,
    ) {
        match (old_type, new_type) {
            (old_type, Type::Var { type_ }) => match (&*type_.borrow(), context) {
                (TypeVar::Link { type_ }, context) => {
                    self.compare_types(old_type, type_, context);
                }
                // We treat this case differently depending on whether this we are comparing
                // this type inside a custom type (constructor), or constant/function. In a
                // constructor, changing a concrete to a generic type is always a breaking
                // change, since the type annotation would affect the constructor differently.
                // However, for constants and functions, since generic parameters are implicit,
                // a concrete type can be turned generic without breaking usage.
                (
                    TypeVar::Unbound { id } | TypeVar::Generic { id },
                    TypeComparisonContext::Constructor(remapped_ids),
                ) => {
                    match old_type {
                        // If the old type was also a generic type, we need to find what the remapped ID
                        // of the new generic is, and compare those.
                        TypeInterface::Variable { id: old_id } => {
                            let new_id = if let Some(id) = remapped_ids.get(id) {
                                *id
                            } else {
                                self.record_major_change();
                                return;
                            };
                            if new_id != *old_id {
                                self.record_major_change();
                            }
                        }
                        // Otherwise, since we're in a constructor, it's a major change.
                        TypeInterface::Fn { .. }
                        | TypeInterface::Tuple { .. }
                        | TypeInterface::Named { .. } => {
                            self.record_major_change();
                        }
                    }
                }
                (
                    TypeVar::Unbound { id } | TypeVar::Generic { id },
                    TypeComparisonContext::FunctionOrConstant(generic_replacements),
                ) => {
                    // Generics are a little more complex with functions/constants than constructors.
                    // Here, not every concrete to generic type is a minor change. If a single
                    // generic replaces what used to be two different types, that is a major
                    // change.
                    //
                    // For example, going from `fn(Int, Int) -> Int` to `fn(a, a) -> a` is
                    // fine, but `fn(Int, Float) -> String` to `fn(a, a) -> a` is not. The
                    // types must all be the same for the change to be minor.
                    //
                    // Generic IDs are also not stable, since there is no explicit type parameter list,
                    // so introducing a new non-breaking generic shifts the IDs of all the other type
                    // variables, meaning we need to treat generics the same as any other type here.
                    if let Some(replacement) = generic_replacements.get(id) {
                        // If this generic is replacing a two different types, it's a major change.
                        if !replacement.same_as(old_type) {
                            self.record_major_change();
                        }
                    } else {
                        // If this is the first usage of the generic, keep track of what it was
                        // previously so we can detect major changes as above.
                        _ = generic_replacements.insert(*id, old_type);
                        match old_type {
                            // If we are changing a generic to a generic, that is not actually a
                            // minor change so we don't record it. Since we can't keep track of
                            // generic IDs, we can't handle this case separately so it must be done
                            // here.
                            TypeInterface::Variable { .. } => {}
                            TypeInterface::Tuple { .. }
                            | TypeInterface::Fn { .. }
                            | TypeInterface::Named { .. } => self.record_minor_change(),
                        }
                    }
                }
            },

            (
                TypeInterface::Named {
                    name: old_name,
                    package: old_package,
                    module: old_module,
                    parameters: old_arguments,
                },
                Type::Named {
                    package: new_package,
                    module: new_module,
                    name: new_name,
                    arguments: new_arguments,
                    ..
                },
            ) => {
                // Two named types are compatible if they are the same type by name, or one of them
                // has been "moved", and all of their generic arguments are compatible.
                if old_package != new_package
                    || !self.compatible_type(old_module, old_name, new_module, new_name)
                    || old_arguments.len() != new_arguments.len()
                {
                    self.record_major_change();
                } else {
                    for (old_type, new_type) in old_arguments.iter().zip(new_arguments.iter()) {
                        self.compare_types(old_type, new_type, context);
                    }
                }
            }

            (
                TypeInterface::Tuple {
                    elements: old_elements,
                },
                Type::Tuple {
                    elements: new_elements,
                },
            ) if old_elements.len() == new_elements.len() => {
                // Two tuples are compatible if they are the same length and all of their elements are
                // compatible.
                for (old_type, new_type) in old_elements.iter().zip(new_elements.iter()) {
                    self.compare_types(old_type, new_type, context);
                }
            }

            (
                TypeInterface::Fn {
                    parameters: old_parameters,
                    return_: old_return,
                },
                Type::Fn {
                    arguments: new_parameters,
                    return_: new_return,
                },
            ) if old_parameters.len() == new_parameters.len() => {
                // Two function types are compatible if they have the same number of parameters and all
                // of their parameters and their return types are compatible.
                for (old_type, new_type) in old_parameters.iter().zip(new_parameters.iter()) {
                    self.compare_types(old_type, new_type, context);
                }
                self.compare_types(old_return, new_return, context);
            }

            // Any other combination of types is a major change.
            (TypeInterface::Variable { .. }, _)
            | (TypeInterface::Named { .. }, _)
            | (TypeInterface::Tuple { .. }, _)
            | (TypeInterface::Fn { .. }, _) => self.record_major_change(),
        }
    }

    /// Checks whether two types are "compatible", that is, if they are either the same type, or one
    /// is an alias which points to the type, which has been moved.
    fn compatible_type(
        &self,
        old_module: &EcoString,
        old_name: &EcoString,
        new_module: &EcoString,
        new_name: &EcoString,
    ) -> bool {
        let key = TypeNames {
            module: new_module.clone(),
            name: new_name.clone(),
        };
        // This type has been moved
        if let Some(moved) = self.moved_types.get(&key)
            && moved.module == *old_module
            && moved.name == *old_name
        {
            true
        } else {
            // It hasn't been moved, so is it the same type?
            old_module == new_module && old_name == new_name
        }
    }

    /// Compares the `Implementations` of two values (their target support) to see if any minor or
    /// major changes have been made.
    fn compare_implementations(
        &mut self,
        old_implementations: &ImplementationsInterface,
        new_implementations: &Implementations,
    ) {
        // Removing support for any target is a major change
        if (old_implementations.can_run_on_javascript && !new_implementations.can_run_on_javascript)
            || (old_implementations.can_run_on_erlang && !new_implementations.can_run_on_erlang)
        {
            self.record_major_change();
        } else if (new_implementations.can_run_on_javascript
            && !old_implementations.can_run_on_javascript)
            || (new_implementations.can_run_on_erlang && !old_implementations.can_run_on_erlang)
        {
            // Adding support for a new target is a minor change
            self.record_minor_change();
        }
    }
}

/// Creates a mapping between the type variable IDs used in the Gleam compiler and those used in the
/// package interface JSON.
fn remap_type_ids(type_: &TypeConstructor) -> HashMap<u64, u64> {
    // The JSON file uses incrementing IDs in the order they appear in the type parameter list. They
    // are only unique to a specific type rather than the whole module, and always start from 0.
    type_
        .parameters
        .iter()
        .enumerate()
        .filter_map(|(i, parameter)| parameter.variable_id().map(|id| (id, i as u64)))
        .collect()
}

/// The place where we are comparing two different types to see if any minor or major changes have
/// been made.
enum TypeComparisonContext<'a> {
    /// In a constructor, the comparison is more rigid. We keep track of a mapping of new type variable
    /// IDs to old ones to ensure all generic remain the same.
    Constructor(HashMap<u64, u64>),
    /// In a function or constant, generics are more flexible. Since there is no explicit generic list
    /// given, concrete types can become generic without a breaking change. We need to keep track of
    /// which generics replace which types, since a single generic replacing multiple different types
    /// is still a breaking change.
    FunctionOrConstant(HashMap<u64, &'a TypeInterface>),
}
