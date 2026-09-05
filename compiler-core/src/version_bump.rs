// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

#[cfg(test)]
mod tests;

use crate::{
    ast::Layer,
    package_interface::{
        self, ImplementationsInterface, PackageInterface, TypeDefinitionInterface, TypeInterface,
    },
    strings::number_to_letters,
    type_::{
        Deprecation, ModuleInterface, Type, TypeConstructor, TypeVar, TypeVariantConstructors,
        ValueConstructorVariant, expression::Implementations,
    },
};
use ecow::{EcoString, eco_format};
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VersionBump {
    Major,
    Minor,
    Patch,
}

impl VersionBump {
    /// Combines two bumps, returning the stronger of the two. (Major is stronger than minor which is
    /// stronger than patch).
    fn combine(self, other: VersionBump) -> Self {
        match (self, other) {
            (VersionBump::Major, _) | (_, VersionBump::Major) => VersionBump::Major,
            (VersionBump::Minor, _) | (_, VersionBump::Minor) => VersionBump::Minor,
            (VersionBump::Patch, VersionBump::Patch) => VersionBump::Patch,
        }
    }

    pub fn to_string(&self) -> &'static str {
        match self {
            VersionBump::Major => "major",
            VersionBump::Minor => "minor",
            VersionBump::Patch => "patch",
        }
    }
}

/// Represents a change in the public API of a package, module, type, or value
/// between two versions of a package.
#[derive(Debug, Clone)]
pub enum VersionChanges {
    /// A breaking change, meaning old code may break in this version
    Major(HashSet<Change>),
    /// A non-breaking change, meaning old code will continue to function but some
    /// new features was introduced
    Minor(HashSet<Change>),
    /// A patch change, meaning nothing has been added or removed from the public
    /// API, though bug-fixing behavioural changes may have occurred
    Patch,
}

/// A single change that constitutes either a minor or major version bump. The
/// changes are only granular enough to know which part of a type or value's
/// signature to display in order to communicate this to the user. Further details
/// are not needed, due to how changes are printed.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Change {
    /// A change in a function or constant's type signature
    Signature { module: EcoString, name: EcoString },
    /// A change in a function or constant's target support
    TargetSupport { module: EcoString, name: EcoString },
    /// A change in a custom type or type alias's type parameters
    TypeParameters { module: EcoString, name: EcoString },
    /// A change in a custom type's constructor or a type alias's aliased type
    TypeBody { module: EcoString, name: EcoString },
    /// A type or value is deprecated or the deprecation is removed. This doesn't
    /// include changes in the deprecation message, as that's purely a change in
    /// documentation rather than API.
    Deprecation {
        module: EcoString,
        name: EcoString,
        layer: Layer,
    },
    /// A type or value is added, or made public where it was previously private
    /// or internal
    Added {
        module: EcoString,
        name: EcoString,
        layer: Layer,
    },
    /// A public type or value is removed, or made private or internal
    Removed {
        module: EcoString,
        name: EcoString,
        layer: Layer,
    },
    /// A new module is added, or made public where it was previously internal
    ModuleAdded { name: EcoString },
    /// A public module is removed or made internal
    ModuleRemoved { name: EcoString },
}

/// Detect the changes to the public API of a package between two versions.
pub fn detect_changes(
    package_interface: &PackageInterface,
    modules: &im::HashMap<EcoString, ModuleInterface>,
) -> VersionChanges {
    VersionChecker::new(modules).detect_changes(package_interface)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
struct TypeNames {
    module: EcoString,
    name: EcoString,
}

struct VersionChecker<'a> {
    changes: VersionChanges,
    modules: &'a im::HashMap<EcoString, ModuleInterface>,
    moved_types: HashMap<TypeNames, TypeNames>,
}

impl<'a> VersionChecker<'a> {
    fn new(modules: &'a im::HashMap<EcoString, ModuleInterface>) -> Self {
        Self {
            changes: VersionChanges::Patch,
            modules,
            moved_types: HashMap::new(),
        }
    }

    fn minor_change(&mut self, change: Change) {
        match &mut self.changes {
            VersionChanges::Major(_) => {}
            VersionChanges::Minor(changes) => _ = changes.insert(change),
            VersionChanges::Patch => self.changes = VersionChanges::Minor(HashSet::from([change])),
        }
    }

    fn major_change(&mut self, change: Change) {
        match &mut self.changes {
            VersionChanges::Major(changes) => _ = changes.insert(change),
            VersionChanges::Minor(_) | VersionChanges::Patch => {
                self.changes = VersionChanges::Major(HashSet::from([change]))
            }
        }
    }

    fn detect_changes(mut self, package_interface: &PackageInterface) -> VersionChanges {
        self.find_moved_types(package_interface);

        for module in self.modules.values() {
            // Internal modules aren't part of the public API
            if module.is_internal {
                continue;
            }

            if let Some(previous_module) = package_interface.modules.get(&module.name) {
                for (name, type_) in module.types.iter() {
                    // Private and internal types aren't part of the public API
                    if !type_.publicity.is_public() {
                        continue;
                    }

                    // Like with modules, we remove the type aliases from the old
                    // interface so we know that any left over at the end have
                    // been removed and are a major change.
                    if let Some(alias) = previous_module.type_aliases.get(name) {
                        // Changing the number of type parameters breaks any type
                        // annotations.
                        if type_.parameters.len() != alias.parameters {
                            self.major_change(Change::TypeParameters {
                                module: module.name.clone(),
                                name: name.clone(),
                            });
                            continue;
                        }

                        let remapped_ids = remap_type_ids(type_);

                        match (&alias.deprecation, &type_.deprecation) {
                            (None, Deprecation::NotDeprecated)
                            | (Some(_), Deprecation::Deprecated { .. }) => {}
                            // Changing the deprecation of something is a minor change.
                            (None, Deprecation::Deprecated { .. })
                            | (Some(_), Deprecation::NotDeprecated) => {
                                self.minor_change(Change::Deprecation {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                    layer: Layer::Type,
                                });
                            }
                        }

                        // Now we actually need to compare the type that is being
                        // aliased between the two versions.
                        match self.compare_types(
                            &alias.alias,
                            &type_.type_,
                            &mut TypeComparisonContext::Constructor(remapped_ids),
                        ) {
                            VersionBump::Major => {
                                self.major_change(Change::TypeBody {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                });
                            }
                            VersionBump::Minor => {
                                self.minor_change(Change::TypeBody {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                });
                            }
                            VersionBump::Patch => {}
                        }
                    } else if let Some(custom_type) = previous_module.types.get(name) {
                        match (&custom_type.deprecation, &type_.deprecation) {
                            (None, Deprecation::NotDeprecated)
                            | (Some(_), Deprecation::Deprecated { .. }) => {}
                            // Changing the deprecation of something is a minor change.
                            (None, Deprecation::Deprecated { .. })
                            | (Some(_), Deprecation::NotDeprecated) => {
                                self.minor_change(Change::Deprecation {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                    layer: Layer::Type,
                                });
                            }
                        }

                        match self.compare_custom_type(module, name, custom_type, type_) {
                            CustomTypeChanges::TypeParameters => {
                                self.major_change(Change::TypeParameters {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                });
                            }
                            CustomTypeChanges::MajorBody => {
                                self.major_change(Change::TypeBody {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                });
                            }
                            CustomTypeChanges::MinorBody => {
                                self.minor_change(Change::TypeBody {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                });
                            }
                            CustomTypeChanges::Patch => {}
                        }
                    } else {
                        // If the type was neither a type alias nor a custom type
                        // in the previous version, it has been added this version,
                        // which constitutes a minor change.
                        self.minor_change(Change::Added {
                            module: module.name.clone(),
                            name: name.clone(),
                            layer: Layer::Type,
                        });
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

                    if let Some(function) = previous_module.functions.get(name) {
                        match (&function.deprecation, &value.deprecation) {
                            (None, Deprecation::NotDeprecated)
                            | (Some(_), Deprecation::Deprecated { .. }) => {}
                            // Changing the deprecation of something is a minor change.
                            (None, Deprecation::Deprecated { .. })
                            | (Some(_), Deprecation::NotDeprecated) => {
                                self.minor_change(Change::Deprecation {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                    layer: Layer::Value,
                                });
                            }
                        }

                        let old_labels: HashMap<_, _> = function
                            .parameters
                            .iter()
                            .enumerate()
                            .filter_map(|(index, parameter)| {
                                parameter
                                    .label
                                    .as_ref()
                                    .map(|label| (index as u32, label.clone()))
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
                                    (None, Some(_)) => self.minor_change(Change::Signature {
                                        module: module.name.clone(),
                                        name: name.clone(),
                                    }),
                                    // If the label has been removed, it's a major change.
                                    (Some(_), None) => {
                                        self.major_change(Change::Signature {
                                            module: module.name.clone(),
                                            name: name.clone(),
                                        });
                                        break;
                                    }
                                    // If there is still a label but it's different, it's also a major
                                    // change.
                                    (Some(old_label), Some(new_label))
                                        if old_label != *new_label =>
                                    {
                                        self.major_change(Change::Signature {
                                            module: module.name.clone(),
                                            name: name.clone(),
                                        });
                                        break;
                                    }
                                    (None, None) | (Some(_), Some(_)) => {}
                                }
                            }
                        } else if !old_labels.is_empty() {
                            // If there are no labels now but there were some in the older version,
                            // they've all been removed and it's a major change.
                            self.major_change(Change::Signature {
                                module: module.name.clone(),
                                name: name.clone(),
                            });
                        }

                        // The `FunctionInterface` doesn't directly contain the
                        // type of the function value, so we need to construct
                        // that so we can compare it with the new value.
                        let old_type = TypeInterface::Fn {
                            parameters: function
                                .parameters
                                .iter()
                                .map(|parameter| parameter.type_.clone())
                                .collect(),
                            return_: Box::new(function.return_.clone()),
                        };

                        match self.compare_types(
                            &old_type,
                            &value.type_,
                            &mut TypeComparisonContext::FunctionOrConstant(HashMap::new()),
                        ) {
                            VersionBump::Major => {
                                self.major_change(Change::Signature {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                });
                                continue;
                            }
                            VersionBump::Minor => {
                                self.minor_change(Change::Signature {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                });
                            }
                            VersionBump::Patch => {}
                        }

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
                        match self
                            .compare_implementations(&function.implementations, new_implementations)
                        {
                            VersionBump::Major => {
                                self.major_change(Change::TargetSupport {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                });
                            }
                            VersionBump::Minor => {
                                self.minor_change(Change::TargetSupport {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                });
                            }
                            VersionBump::Patch => {}
                        }
                    } else if let Some(constant) = previous_module.constants.get(name) {
                        match (&constant.deprecation, &value.deprecation) {
                            (None, Deprecation::NotDeprecated)
                            | (Some(_), Deprecation::Deprecated { .. }) => {}
                            // Changing the deprecation of something is a minor change.
                            (None, Deprecation::Deprecated { .. })
                            | (Some(_), Deprecation::NotDeprecated) => {
                                self.minor_change(Change::Deprecation {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                    layer: Layer::Value,
                                });
                            }
                        }

                        match self.compare_types(
                            &constant.type_,
                            &value.type_,
                            &mut TypeComparisonContext::FunctionOrConstant(HashMap::new()),
                        ) {
                            VersionBump::Major => {
                                self.major_change(Change::Signature {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                });
                            }
                            VersionBump::Minor => {
                                self.minor_change(Change::Signature {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                });
                            }
                            VersionBump::Patch => {}
                        }

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
                        match self
                            .compare_implementations(&constant.implementations, new_implementations)
                        {
                            VersionBump::Major => {
                                self.major_change(Change::TargetSupport {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                });
                            }
                            VersionBump::Minor => {
                                self.minor_change(Change::TargetSupport {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                });
                            }
                            VersionBump::Patch => {}
                        }

                        // If what was a constant in the old version is now a function with labels,
                        // and they are the same type (checked above), this is effectively adding
                        // labels to a label-less function, which is a minor change.
                        if let ValueConstructorVariant::ModuleFn {
                            field_map: Some(field_map),
                            ..
                        } = &value.variant
                            && !field_map.fields.is_empty()
                        {
                            self.minor_change(Change::Signature {
                                module: module.name.clone(),
                                name: name.clone(),
                            });
                        }
                    } else {
                        // If the value was neither a function nor a constant in
                        // the previous version, it is new in this version, which
                        // is a minor change.
                        self.minor_change(Change::Added {
                            module: module.name.clone(),
                            name: name.clone(),
                            layer: Layer::Value,
                        });
                    }
                }

                // Iterate over all of the types and values from the old module and check if any of
                // them are missing or non-public in the new version. If they are, they have been
                // removed from the public API, which is a major change.
                for type_ in previous_module
                    .types
                    .keys()
                    .chain(previous_module.type_aliases.keys())
                {
                    if module
                        .types
                        .get(type_)
                        .is_none_or(|type_| !type_.publicity.is_public())
                    {
                        self.major_change(Change::Removed {
                            module: module.name.clone(),
                            name: type_.clone(),
                            layer: Layer::Type,
                        });
                    }
                }

                for value in previous_module
                    .functions
                    .keys()
                    .chain(previous_module.constants.keys())
                {
                    if module
                        .values
                        .get(value)
                        .is_none_or(|value| !value.publicity.is_public())
                    {
                        self.major_change(Change::Removed {
                            module: module.name.clone(),
                            name: value.clone(),
                            layer: Layer::Value,
                        });
                    }
                }
            } else {
                // This module is new in this version, which constitutes a minor change.
                // We don't care about what types/values are in this module, since they
                // are all new.
                self.minor_change(Change::ModuleAdded {
                    name: module.name.clone(),
                });
            }
        }

        // Iterate through all of the modules in the old version and check if any are absent or
        // internal in the new version. If they are, they have been removed from the public API,
        // which is a major change.
        for module in package_interface.modules.keys() {
            if self
                .modules
                .get(module)
                // Since `self.modules` contains all importable modules in the project, we need to
                // check that the module is in this package, as if it isn't, it's still a major
                // change.
                .is_none_or(|module| module.is_internal || module.package != package_interface.name)
            {
                self.major_change(Change::ModuleRemoved {
                    name: module.clone(),
                });
            }
        }

        self.changes
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
                    match self.compare_custom_type(module, name, custom_type, type_) {
                        CustomTypeChanges::MajorBody | CustomTypeChanges::TypeParameters => {}

                        // If it's compatible, record it as a moved type. This type is now effectively
                        // interchangeable with the one it aliases, even though they are different types.
                        CustomTypeChanges::Patch | CustomTypeChanges::MinorBody => {
                            _ = self.moved_types.insert(
                                TypeNames {
                                    module: new_module,
                                    name: new_name,
                                },
                                TypeNames {
                                    module: module.name.clone(),
                                    name: name.clone(),
                                },
                            )
                        }
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
    ) -> CustomTypeChanges {
        // Changing the number of type parameters breaks any type
        // annotations.
        if new_type.parameters.len() != custom_type.parameters {
            return CustomTypeChanges::TypeParameters;
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
            )
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
                return CustomTypeChanges::MajorBody;
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

            // Compare the aliased custom type with the old type to see if they match.
            match self.do_compare_custom_type(
                custom_type,
                new_constructors,
                remapped_ids,
                &remapped_parameters,
            ) {
                // Moving a custom type is a minor change, even if no changes are made to the type itself.
                CustomTypeChanges::Patch => CustomTypeChanges::MinorBody,
                changes @ (CustomTypeChanges::MinorBody
                | CustomTypeChanges::MajorBody
                | CustomTypeChanges::TypeParameters) => changes,
            }
        } else {
            // If the new type is an alias to something other than a custom type, it's a major change.
            CustomTypeChanges::MajorBody
        }
    }

    /// Compares the actual constructors of two custom types, checking if they are compatible.
    fn do_compare_custom_type(
        &mut self,
        custom_type: &TypeDefinitionInterface,
        new_constructors: &TypeVariantConstructors,
        remapped_ids: HashMap<u64, u64>,
        remapped_parameters: &HashMap<u64, Arc<Type>>,
    ) -> CustomTypeChanges {
        let mut bump = CustomTypeChanges::Patch;

        // If there were previously no constructors (the type was an external type or was opaque)
        // and we have added non-opaque constructors, it's a minor change.
        if custom_type.constructors.is_empty() {
            if !new_constructors.variants.is_empty() && !new_constructors.opaque.is_opaque() {
                bump = CustomTypeChanges::MinorBody;
            }
        } else {
            // If there were previously public constructors and we made them opaque, it's a
            // major change.
            if new_constructors.opaque.is_opaque() {
                return CustomTypeChanges::MajorBody;
            }
            // Adding a constructor is a major change as well as removing one, as it breaks any
            // pattern matching on the type.
            if custom_type.constructors.len() != new_constructors.variants.len() {
                return CustomTypeChanges::MajorBody;
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
                    return CustomTypeChanges::MajorBody;
                };

                if old_parameters.len() != new_constructor.parameters.len() {
                    return CustomTypeChanges::MajorBody;
                }

                for (old, new) in old_parameters.iter().zip(new_constructor.parameters.iter()) {
                    match (&old.label, &new.label) {
                        (None, None) => {}
                        (Some(old_label), Some(new_label)) if old_label == new_label => {}
                        // Adding a label is a minor change
                        (None, Some(_)) => bump = CustomTypeChanges::MinorBody,
                        // Changing or removing a label is a major change
                        (Some(_), Some(_) | None) => return CustomTypeChanges::MajorBody,
                    }

                    let new_type = if let Some(id) = new.type_.variable_id()
                        && let Some(remapped) = remapped_parameters.get(&id)
                    {
                        remapped
                    } else {
                        &new.type_
                    };

                    match self.compare_types(&old.type_, new_type, &mut context) {
                        // Once we've found a major bump there's no use continuing to check, since we
                        // know that the whole custom type has had a major bump.
                        VersionBump::Major => return CustomTypeChanges::MajorBody,
                        // Since we always return as soon as we encounter a major bump, we can assign a
                        // minor bump to `bump`, as it will only ever be `Minor` or `Patch`.
                        VersionBump::Minor => bump = CustomTypeChanges::MinorBody,
                        VersionBump::Patch => {}
                    }
                }
            }
        }

        bump
    }

    /// Compares two types to see if they are compatible. Has slightly different behaviour for types
    /// and values, as generics are treated differently, meaning some changes in a constructor's type
    /// is a breaking change, whereas it isn't for a function's type signature.
    fn compare_types<'b>(
        &mut self,
        old_type: &'b TypeInterface,
        new_type: &Type,
        context: &mut TypeComparisonContext<'b>,
    ) -> VersionBump {
        match (old_type, new_type) {
            (old_type, Type::Var { type_ }) => match (&*type_.borrow(), context) {
                (TypeVar::Link { type_ }, context) => self.compare_types(old_type, type_, context),
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
                                return VersionBump::Major;
                            };
                            if new_id == *old_id {
                                VersionBump::Patch
                            } else {
                                VersionBump::Major
                            }
                        }
                        // Otherwise, since we're in a constructor, it's a major change.
                        TypeInterface::Fn { .. }
                        | TypeInterface::Tuple { .. }
                        | TypeInterface::Named { .. } => VersionBump::Major,
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
                        if replacement.same_as(old_type) {
                            VersionBump::Patch
                        } else {
                            // If this generic is replacing a two different types, it's a major change.
                            VersionBump::Major
                        }
                    } else {
                        // If this is the first usage of the generic, keep track of what it was
                        // previously so we can detect major changes as above.
                        _ = generic_replacements.insert(*id, old_type);
                        match old_type {
                            // If we are changing a generic to a generic, that is not actually a
                            // minor change. Since we can't keep track of generic IDs, we can't
                            // handle this case separately so it must be done here.
                            TypeInterface::Variable { .. } => VersionBump::Patch,
                            TypeInterface::Tuple { .. }
                            | TypeInterface::Fn { .. }
                            | TypeInterface::Named { .. } => VersionBump::Minor,
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
                    VersionBump::Major
                } else {
                    old_arguments.iter().zip(new_arguments.iter()).fold(
                        VersionBump::Patch,
                        |bump, (old_type, new_type)| {
                            self.compare_types(old_type, new_type, context)
                                .combine(bump)
                        },
                    )
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
                old_elements.iter().zip(new_elements.iter()).fold(
                    VersionBump::Patch,
                    |bump, (old_type, new_type)| {
                        self.compare_types(old_type, new_type, context)
                            .combine(bump)
                    },
                )
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
                let bump = self.compare_types(old_return, new_return, context);
                old_parameters.iter().zip(new_parameters.iter()).fold(
                    bump,
                    |bump, (old_type, new_type)| {
                        self.compare_types(old_type, new_type, context)
                            .combine(bump)
                    },
                )
            }

            // Any other combination of types is a major change.
            (
                TypeInterface::Tuple { .. }
                | TypeInterface::Fn { .. }
                | TypeInterface::Variable { .. }
                | TypeInterface::Named { .. },
                _,
            ) => VersionBump::Major,
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
    ) -> VersionBump {
        // Removing support for any target is a major change
        if (old_implementations.can_run_on_javascript && !new_implementations.can_run_on_javascript)
            || (old_implementations.can_run_on_erlang && !new_implementations.can_run_on_erlang)
        {
            VersionBump::Major
        } else if (new_implementations.can_run_on_javascript
            && !old_implementations.can_run_on_javascript)
            || (new_implementations.can_run_on_erlang && !old_implementations.can_run_on_erlang)
        {
            // Adding support for a new target is a minor change
            VersionBump::Minor
        } else {
            VersionBump::Patch
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum CustomTypeChanges {
    Patch,
    MinorBody,
    MajorBody,
    TypeParameters,
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

/// Pretty-print the changes between two versions.
pub fn show_diffs(
    changes: HashSet<Change>,
    package_interface: &PackageInterface,
    modules: &im::HashMap<EcoString, ModuleInterface>,
) -> EcoString {
    let mut module_changes: HashMap<_, HashMap<_, Vec<_>>> = HashMap::new();
    let mut added_modules = Vec::new();
    let mut removed_modules = Vec::new();

    // Group changes by module and type/value, so that they can be printed in an
    // organised, coherent way rather than randomly.
    for change in changes {
        let (module, name, layer) = match &change {
            Change::Signature { module, name } => (module, name, Layer::Value),
            Change::TargetSupport { module, name } => (module, name, Layer::Value),
            Change::TypeParameters { module, name } => (module, name, Layer::Type),
            Change::TypeBody { module, name } => (module, name, Layer::Type),
            Change::Deprecation {
                module,
                name,
                layer,
            } => (module, name, *layer),
            Change::Added {
                module,
                name,
                layer,
            } => (module, name, *layer),
            Change::Removed {
                module,
                name,
                layer,
            } => (module, name, *layer),
            Change::ModuleAdded { name } => {
                added_modules.push(name.clone());
                continue;
            }
            Change::ModuleRemoved { name } => {
                removed_modules.push(name.clone());
                continue;
            }
        };

        module_changes
            .entry(module.clone())
            .or_default()
            .entry((name.clone(), layer))
            .or_default()
            .push(change);
    }

    let mut out = EcoString::new();

    // Sort modules so that output is deterministic
    added_modules.sort();
    removed_modules.sort();
    for module in added_modules {
        out.push_str(&eco_format!("+ {module}\n\n"));
    }
    for module in removed_modules {
        out.push_str(&eco_format!("- {module}\n\n"));
    }

    // Sort by module and name to ensure output is deterministic
    for (module, changed) in module_changes.iter().sorted_by_key(|(name, _)| *name) {
        out.push_str(&eco_format!("== {module} ==\n"));

        let (Some(old_module), Some(new_module)) =
            (package_interface.modules.get(module), modules.get(module))
        else {
            continue;
        };

        for ((name, layer), changes) in changed.iter().sorted_by_key(|((name, _), _)| name) {
            let diff = match layer {
                Layer::Value => diff_value(
                    ValueChanges::from_changes(changes),
                    old_module,
                    new_module,
                    name,
                ),
                Layer::Type => diff_type(
                    TypeChanges::from_changes(changes),
                    old_module,
                    new_module,
                    name,
                ),
            };
            if let Some(diff) = diff {
                out.push_str(&diff);
                out.push_str("\n\n");
            }
        }
    }

    out
}

/// Stores the possible changes that can happen to a function or constant that
/// may need to be displayed for a version bump.
#[derive(Debug, Clone, Copy, Default)]
struct ValueChanges {
    signature: bool,
    target_support: bool,
    deprecation: bool,
    added: bool,
    removed: bool,
}

impl ValueChanges {
    fn from_changes(changes: &[Change]) -> Self {
        let mut value_changes = Self::default();
        for change in changes {
            match change {
                Change::Signature { .. } => value_changes.signature = true,
                Change::TargetSupport { .. } => value_changes.target_support = true,
                Change::Deprecation { .. } => value_changes.deprecation = true,
                Change::Added { .. } => value_changes.added = true,
                Change::Removed { .. } => value_changes.removed = true,
                Change::TypeParameters { .. }
                | Change::TypeBody { .. }
                | Change::ModuleAdded { .. }
                | Change::ModuleRemoved { .. } => {}
            }
        }
        value_changes
    }
}

/// Stores the possible changes that can happen to a custom type or alias that
/// may need to be displayed for a version bump.
#[derive(Debug, Clone, Copy, Default)]
struct TypeChanges {
    type_parameters: bool,
    type_body: bool,
    deprecation: bool,
    added: bool,
    removed: bool,
}

impl TypeChanges {
    fn from_changes(changes: &[Change]) -> Self {
        let mut type_changes = Self::default();
        for change in changes {
            match change {
                Change::TypeParameters { .. } => type_changes.type_parameters = true,
                Change::TypeBody { .. } => type_changes.type_body = true,
                Change::Deprecation { .. } => type_changes.deprecation = true,
                Change::Added { .. } => type_changes.added = true,
                Change::Removed { .. } => type_changes.removed = true,
                Change::Signature { .. }
                | Change::TargetSupport { .. }
                | Change::ModuleAdded { .. }
                | Change::ModuleRemoved { .. } => {}
            }
        }
        type_changes
    }
}

/// Shows the difference between two versions of a custom type or type alias.
/// Only relevant details are shown, so if just the deprecation changes, we don't
/// bother showing all of the constructors.
fn diff_type(
    changes: TypeChanges,
    old_module: &package_interface::ModuleInterface,
    new_module: &ModuleInterface,
    name: &str,
) -> Option<EcoString> {
    if changes.removed {
        return Some(eco_format!("- pub type {name}"));
    } else if changes.added {
        return Some(eco_format!("+ pub type {name}"));
    }

    let mut out = EcoString::new();

    if let Some(alias) = old_module.type_aliases.get(name) {
        if changes.deprecation && alias.deprecation.is_some() {
            out.push_str("- @deprecated(...)\n");
        }

        out.push_str(&eco_format!("- pub type {name}"));

        // We need to print the type parameters if either the parameters themselves have changed, or
        // if we're printing the type body so the user has a reference to the parameters to be able
        // to see any difference in the body relating to them.
        //
        // For example, changing:
        // ```gleam
        // pub type Output(a, b) = Result(a, b)
        // ```
        // to:
        // ```gleam
        // pub type Output(b, a) = Result(b, a)
        // ```
        // Is not a change in the API, however, changing it to:
        // ```gleam
        // pub type Output(a, b) = Result(b, a)
        // ```
        // Is. The type parameters must be present to infer the context of the change.
        if alias.parameters > 0 && (changes.type_parameters || changes.type_body) {
            out.push_str(&eco_format!(
                "({})",
                (0..alias.parameters as u64)
                    .into_iter()
                    .map(number_to_letters)
                    .join(", ")
            ))
        }

        if changes.type_body {
            out.push_str(&eco_format!(" = {}", print_type_interface(&alias.alias)))
        }
    } else {
        let custom_type = old_module.types.get(name)?;
        if changes.deprecation && custom_type.deprecation.is_some() {
            out.push_str("- @deprecated(...)\n");
        }

        out.push_str(&eco_format!("- pub type {name}"));

        if custom_type.parameters > 0 && (changes.type_parameters || changes.type_body) {
            out.push_str(&eco_format!(
                "({})",
                (0..custom_type.parameters as u64)
                    .into_iter()
                    .map(number_to_letters)
                    .join(", ")
            ))
        }

        if changes.type_body && !custom_type.constructors.is_empty() {
            out.push_str(" {");
            for constructor in custom_type.constructors.iter() {
                out.push_str("\n-   ");
                out.push_str(&constructor.name);
                if !constructor.parameters.is_empty() {
                    out.push('(');
                    out.push_str(
                        &constructor
                            .parameters
                            .iter()
                            .map(|parameter| {
                                if let Some(label) = &parameter.label {
                                    eco_format!(
                                        "{label}: {}",
                                        print_type_interface(&parameter.type_)
                                    )
                                } else {
                                    print_type_interface(&parameter.type_)
                                }
                            })
                            .join(", "),
                    );
                    out.push(')');
                }
            }
            out.push_str("\n- }");
        }
    };

    out.push('\n');

    let mut type_variables = HashMap::new();
    let mut next_id = 0;

    let type_ = new_module.types.get(name)?;
    if changes.deprecation && type_.deprecation.is_deprecated() {
        out.push_str("+ @deprecated(...)\n");
    }

    out.push_str(&eco_format!("+ pub type {name}"));

    if !type_.parameters.is_empty() && (changes.type_parameters || changes.type_body) {
        out.push_str(&eco_format!(
            "({})",
            type_
                .parameters
                .iter()
                .map(|type_| print_type(type_, &mut next_id, &mut type_variables))
                .join(", ")
        ))
    }

    if changes.type_body {
        if let Some(constructors) = new_module.types_value_constructors.get(name) {
            // Opaque type's constructors are not part of the public API.
            if !constructors.variants.is_empty() && !constructors.opaque.is_opaque() {
                out.push_str(" {");
                for constructor in constructors.variants.iter() {
                    out.push_str("\n+   ");
                    out.push_str(&constructor.name);
                    if !constructor.parameters.is_empty() {
                        out.push('(');
                        out.push_str(
                            &constructor
                                .parameters
                                .iter()
                                .map(|parameter| {
                                    if let Some(label) = &parameter.label {
                                        eco_format!(
                                            "{label}: {}",
                                            print_type(
                                                &parameter.type_,
                                                &mut next_id,
                                                &mut type_variables
                                            )
                                        )
                                    } else {
                                        print_type(
                                            &parameter.type_,
                                            &mut next_id,
                                            &mut type_variables,
                                        )
                                    }
                                })
                                .join(", "),
                        );
                        out.push(')');
                    }
                }
                out.push_str("\n+ }");
            }
        } else {
            out.push_str(&eco_format!(
                " = {}",
                print_type(&type_.type_, &mut next_id, &mut type_variables)
            ))
        }
    }

    Some(out)
}

/// Shows the difference between two versions of a constant or function.
/// Only relevant details are shown, so if just the target support changes,
/// we don't bother showing a function's signature.
fn diff_value(
    changes: ValueChanges,
    old_module: &package_interface::ModuleInterface,
    new_module: &ModuleInterface,
    name: &str,
) -> Option<EcoString> {
    if changes.removed {
        if old_module.constants.contains_key(name) {
            return Some(eco_format!("- pub const {name}"));
        } else if old_module.functions.contains_key(name) {
            return Some(eco_format!("- pub fn {name}"));
        }
    } else if changes.added
        && let Some(value) = new_module.values.get(name)
    {
        return match &value.variant {
            ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::Record { .. } => None,
            ValueConstructorVariant::ModuleConstant { .. } => {
                Some(eco_format!("+ pub const {name}"))
            }
            ValueConstructorVariant::ModuleFn { .. } => Some(eco_format!("+ pub fn {name}")),
        };
    }

    // We don't have any syntax for changing target support, so we need to use words to show it.
    // Because of this, it must be displayed separately from the main function/constant signature.
    // If target support is the only change, we don't need to display any other information and can
    // just print the text instead.
    if changes.target_support && !changes.signature && !changes.deprecation {
        return diff_target_support(old_module, new_module, name);
    }

    let mut out = EcoString::new();

    if let Some(function) = old_module.functions.get(name) {
        if changes.deprecation && function.deprecation.is_some() {
            out.push_str("- @deprecated(...)\n");
        }

        if changes.signature {
            let parameters = function
                .parameters
                .iter()
                .map(|parameter| match &parameter.label {
                    Some(label) => {
                        eco_format!("{label}: {}", print_type_interface(&parameter.type_))
                    }
                    None => print_type_interface(&parameter.type_),
                })
                .join(", ");

            out.push_str(&eco_format!(
                "- pub fn {name}({parameters}) -> {}",
                print_type_interface(&function.return_)
            ))
        } else {
            out.push_str(&eco_format!("- pub fn {name}"));
        }
    } else {
        let constant = old_module.constants.get(name)?;
        if changes.deprecation && constant.deprecation.is_some() {
            out.push_str("- @deprecated(...)\n");
        }
        if changes.signature {
            out.push_str(&eco_format!(
                "- pub const {name}: {}",
                print_type_interface(&constant.type_)
            ));
        } else {
            out.push_str(&eco_format!("- pub const {name}"));
        }
    }

    out.push('\n');

    let mut type_variables = HashMap::new();
    let mut next_id = 0;

    let value = new_module.values.get(name)?;
    if changes.deprecation && value.deprecation.is_deprecated() {
        out.push_str("+ @deprecated(...)\n");
    }

    match &value.variant {
        ValueConstructorVariant::LocalVariable { .. } | ValueConstructorVariant::Record { .. } => {
            return None;
        }
        ValueConstructorVariant::ModuleConstant { .. } if changes.signature => {
            out.push_str(&eco_format!(
                "+ pub const {name}: {}",
                print_type(&value.type_, &mut next_id, &mut type_variables)
            ));
        }
        ValueConstructorVariant::ModuleConstant { .. } => {
            out.push_str(&eco_format!("+ pub const {name}"));
        }

        ValueConstructorVariant::ModuleFn { field_map, .. } if changes.signature => {
            let labels = field_map
                .as_ref()
                .map(|field_map| field_map.indices_to_labels())
                .unwrap_or_default();

            if let Some((parameters, return_type)) = value.type_.fn_types() {
                let parameters = parameters
                    .iter()
                    .enumerate()
                    .map(|(index, type_)| match labels.get(&(index as u32)) {
                        Some(label) => eco_format!(
                            "{label}: {}",
                            print_type(type_, &mut next_id, &mut type_variables)
                        ),
                        None => print_type(type_, &mut next_id, &mut type_variables),
                    })
                    .join(", ");

                out.push_str(&eco_format!(
                    "+ pub fn {name}({parameters}) -> {}",
                    print_type(&return_type, &mut next_id, &mut type_variables)
                ));
            } else {
                return None;
            }
        }
        ValueConstructorVariant::ModuleFn { .. } => {
            out.push_str(&eco_format!("+ pub fn {name}"));
        }
    }

    if changes.target_support {
        out.push_str("\n\n");
        out.push_str(&diff_target_support(old_module, new_module, name)?);
    }

    Some(out)
}

/// Shows the difference in target support between two versions of a function or
/// constant. Since there is no direct Gleam syntax for that, it must be shown
/// in plain language instead.
fn diff_target_support(
    old_module: &package_interface::ModuleInterface,
    new_module: &ModuleInterface,
    name: &str,
) -> Option<EcoString> {
    let old_implementations = if let Some(function) = old_module.functions.get(name) {
        &function.implementations
    } else {
        &old_module.constants.get(name)?.implementations
    };

    let new_implementations = match &new_module.values.get(name)?.variant {
        ValueConstructorVariant::ModuleConstant {
            implementations, ..
        }
        | ValueConstructorVariant::ModuleFn {
            implementations, ..
        } => implementations,
        ValueConstructorVariant::LocalVariable { .. } | ValueConstructorVariant::Record { .. } => {
            return None;
        }
    };

    let old_targets =
        if old_implementations.can_run_on_erlang && old_implementations.can_run_on_javascript {
            "Erlang, JavaScript"
        } else if old_implementations.can_run_on_erlang {
            "Erlang"
        } else if old_implementations.can_run_on_javascript {
            "JavaScript"
        } else {
            return None;
        };

    let new_targets =
        if new_implementations.can_run_on_erlang && new_implementations.can_run_on_javascript {
            "Erlang, JavaScript"
        } else if new_implementations.can_run_on_erlang {
            "Erlang"
        } else if new_implementations.can_run_on_javascript {
            "JavaScript"
        } else {
            return None;
        };

    Some(eco_format!(
        "The target support for {name} has changed:
- {old_targets}
+ {new_targets}"
    ))
}

/// Prints a `TypeInterface` into Gleam syntax.
fn print_type_interface(type_: &TypeInterface) -> EcoString {
    match type_ {
        TypeInterface::Tuple { elements } => {
            eco_format!(
                "#({})",
                elements.iter().map(print_type_interface).join(", ")
            )
        }
        TypeInterface::Fn {
            parameters,
            return_,
        } => eco_format!(
            "fn({}) -> {}",
            parameters.iter().map(print_type_interface).join(", "),
            print_type_interface(return_)
        ),
        TypeInterface::Variable { id } => number_to_letters(*id),
        TypeInterface::Named {
            name, parameters, ..
        } => {
            if parameters.is_empty() {
                name.clone()
            } else {
                eco_format!(
                    "{name}({})",
                    parameters.iter().map(print_type_interface).join(", ")
                )
            }
        }
    }
}

/// Prints a `Type` into Gleam syntax.
fn print_type(
    type_: &Type,
    next_id: &mut u64,
    type_variables: &mut HashMap<u64, EcoString>,
) -> EcoString {
    match type_ {
        Type::Named {
            name, arguments, ..
        } => {
            if arguments.is_empty() {
                name.clone()
            } else {
                eco_format!(
                    "{name}({})",
                    arguments
                        .iter()
                        .map(|type_| print_type(type_, next_id, type_variables))
                        .join(", ")
                )
            }
        }
        Type::Fn { arguments, return_ } => eco_format!(
            "fn({}) -> {}",
            arguments
                .iter()
                .map(|type_| print_type(type_, next_id, type_variables))
                .join(", "),
            print_type(return_, next_id, type_variables)
        ),
        Type::Var { type_ } => match &*type_.borrow() {
            TypeVar::Unbound { id } | TypeVar::Generic { id } => {
                if let Some(name) = type_variables.get(id) {
                    name.clone()
                } else {
                    let name = number_to_letters(*next_id);
                    _ = type_variables.insert(*id, name.clone());
                    *next_id += 1;
                    name
                }
            }
            TypeVar::Link { type_ } => print_type(type_, next_id, type_variables),
        },
        Type::Tuple { elements } => eco_format!(
            "#({})",
            elements
                .iter()
                .map(|type_| print_type(type_, next_id, type_variables))
                .join(", ")
        ),
    }
}
