// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

use std::collections::{HashMap, HashSet};

use crate::ast::{Publicity, SrcSpan};
use bimap::{BiMap, Overwritten};
use ecow::EcoString;
use petgraph::{
    Directed, Direction,
    stable_graph::{NodeIndex, StableGraph},
};

/// Describes one of a number of situations where references can be generated.
/// See each variant for an explanation.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ReferenceKind {
    /// A type or value which is referenced using the qualified syntax, along with
    /// some information about the qualifier which is used for tracking module name
    /// references. For example:
    /// ```gleam
    /// import gleam/option.{Some}
    ///
    /// pub fn main() -> option.Option(Int) {
    /// //               ^^^^^^ `module_location` covers this
    ///   Some(1)
    /// }
    /// ```
    ///
    /// Here, `module_alias` is `option`, as that's what's being used as a
    /// qualifier.
    ///
    ///
    /// ```gleam
    /// import gleam/int as integer
    ///
    /// pub fn main() {
    ///   integer.add(1, 2)
    /// //^^^^^^^ `module_location` covers this
    /// }
    /// ```
    ///
    /// In this case, `module_alias` is `integer`, due to the aliased import.
    ///
    Qualified {
        module_alias: EcoString,
        module_location: SrcSpan,
    },
    /// A type or value is being referenced using unqualified syntax. This may
    /// be due to being imported unqualified, or because it's from the same
    /// module. For example:
    ///
    /// ```gleam
    /// import gleam/option.{None}
    ///
    /// pub fn main() {
    ///   none()
    /// //^^^^ Unqualified
    /// }
    ///
    /// fn none() {
    ///   None
    /// //^^^^ Unqualified
    /// }
    /// ```
    ///
    Unqualified,
    /// A value or type is being referenced inside an unqualified import. For
    /// example:
    /// ```gleam
    /// import gleam/option.{None}
    /// //                   ^^^^ Import
    /// import gleam/dynamic/decode.{type Dynamic}
    /// //                                ^^^^^^^ Import
    /// ```
    /// The contained span is the location of `as ...` part, if it exists,
    /// otherwise it will just be empty. For example:
    /// ```gleam
    /// import gleam/option.{None as Nothing}
    /// //                       ^^^^^^^^^^^ Alias span
    /// ```
    Import(SrcSpan),
    /// The original definition location of a type or value. This also counts as
    /// a reference for renaming and "find references" purposes. For example:
    ///
    /// ```gleam
    /// pub type Wibble {
    /// //       ^^^^^^ Definition
    ///   Wibble(Int)
    /// //^^^^^^ Definition
    /// }
    ///
    /// pub fn extract(w: Wibble) {
    /// //     ^^^^^^^ Definition
    ///   let Wibble(x) = w
    ///   x
    /// }
    /// ```
    Definition,
    /// A value or type is being referenced using unqualified syntax, with a
    /// name other than its original definition. This can be due to importing it
    /// using an alias, or due to referencing it through a type alias. For example:
    ///
    /// ```gleam
    /// import gleam/option.{None as Nothing, type Option as Maybe}
    ///
    /// pub fn nothing() -> Maybe(_) {
    /// //                  ^^^^^ Alias
    ///   Nothing
    /// //^^^^^^^ Alias
    /// }
    /// ```
    ///
    Alias,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Reference {
    pub location: SrcSpan,
    pub kind: ReferenceKind,
}

/// A reference to a module name. This is similar to a `Reference`, which covers
/// types and values, but it is separate because we care about slightly different
/// pieces of information when, for example, renaming modules vs. renaming types
/// or values.
///
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ModuleNameReference {
    /// The location of a module name in a `ModuleSelect`, when the module name
    /// is not aliased. For example:
    /// ```gleam
    /// import gleam/option
    ///
    /// pub fn main() -> option.Option(_) {
    /// //               ^^^^^^ ModuleSelect
    ///   option.None
    /// //^^^^^^ ModuleSelect
    /// }
    /// ```
    ///
    ModuleSelect(SrcSpan),
    /// The location of a module name in a `ModuleSelect`, when the module name
    /// *is* aliased. For example:
    /// ```gleam
    /// import gleam/option as maybe
    ///
    /// pub fn main() -> maybe.Option(_) {
    /// //               ^^^^^ AliasedModuleSelect
    ///   maybe.None
    /// //^^^^^ AliasedModuleSelect
    /// }
    /// ```
    ///
    AliasedModuleSelect(SrcSpan),
    /// The location of a module name in an `import` statement, when the module
    /// name is not aliased. For example:
    /// ```gleam
    /// import gleam/option.{None, Some}
    /// //     ^^^^^^^^^^^^ Import     ^ `import_end`
    /// ```
    ///
    Import {
        module_location: SrcSpan,
        import_end: u32,
    },
    /// The location of a module name in an `import` statement, when the module
    /// name *is* aliased. Also stores the location of the alias (including the
    /// `as` keyword), and what the alias is. For example:
    /// ```gleam
    /// import gleam/option.{None, Some} as maybe
    /// //     ^^^^^^^^^^^^ `module_location`
    /// //                               ^^^^^^^^ `alias_location`
    /// ```
    /// In this example, `alias` would be `maybe`.
    ///
    AliasedImport {
        module_location: SrcSpan,
        alias_location: SrcSpan,
        alias: EcoString,
    },
}

pub type ReferenceMap = HashMap<(EcoString, EcoString), Vec<Reference>>;

/// A use of a record field label: a labelled argument in a record constructor
/// call or pattern, a record update argument, or a `record.field` access.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct LabelReference {
    /// The location of the label. For a shorthand (`label:`) this spans the
    /// whole `label:`, mirroring how variable references record label
    /// shorthands. For everything else it is just the label itself.
    pub location: SrcSpan,
    pub syntax: LabelSyntax,
}

/// The syntax used to write a record field label where it is used.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum LabelSyntax {
    /// The label is written out on its own, as in `Wibble(label: value)` or
    /// `record.label`, so renaming it is a simple replacement.
    Longhand,
    /// The label shorthand syntax (`label:`), which stands for both the label
    /// and a variable with the same name. Renaming the field then has to
    /// expand it so the value keeps its name: `wibble:` becomes
    /// `wobble: wibble`.
    Shorthand,
}

/// The location at which a record field label is defined in one of the
/// variants of a custom type.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct LabelDefinition {
    pub location: SrcSpan,
    /// The name of the variant this label is defined in.
    pub variant: EcoString,
}

/// Identifies a record field label within a custom type, used to look up the
/// references to that label.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct RecordLabel {
    /// The module the type this label belongs to is defined in.
    pub type_module: EcoString,
    /// The name of the type this label belongs to.
    pub type_name: EcoString,
    pub label: EcoString,
}

#[derive(Debug, Clone)]
pub struct EntityInformation {
    pub origin: SrcSpan,
    pub kind: EntityKind,
}

/// Information about an "Entity". This determines how we warn about an entity
/// being unused.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum EntityKind {
    Function,
    Constant,
    Constructor,
    Type,
    ImportedModule { module_name: EcoString },
    ModuleAlias { module: EcoString },
    ImportedConstructor { module: EcoString },
    ImportedType { module: EcoString },
    ImportedValue { module: EcoString },
}

/// Like `ast::Layer`, this type differentiates between different scopes. For example,
/// there can be a `wibble` value, a `wibble` module and a `wibble` type in the same
/// scope all at once!
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum EntityLayer {
    /// An entity which exists in the type layer: a custom type, type variable
    /// or type alias.
    Type,
    /// An entity which exists in the value layer: a constant, function or
    /// custom type variant constructor.
    Value,
    /// An entity which has been shadowed. This allows us to keep track of unused
    /// imports even if they have been shadowed by another value in the current
    /// module.
    /// This extra variant is needed because we used `Entity` as a key in a hashmap,
    /// and so a duplicate key would not be able to exist.
    /// We also would not want to get this shadowed entity when performing a lookup
    /// of a named entity; we only want it to register it as an entity in the
    /// `unused` function.
    Shadowed,
    /// The name of an imported module. Modules are separate to values!
    Module,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Entity {
    pub name: EcoString,
    layer: EntityLayer,
}

#[derive(Debug, Default)]
pub struct ReferenceTracker {
    /// A call-graph which tracks which values are referenced by which other value,
    /// used for dead code detection.
    graph: StableGraph<(), (), Directed>,
    entities: BiMap<Entity, NodeIndex>,
    current_node: NodeIndex,
    public_entities: HashSet<Entity>,
    entity_information: HashMap<Entity, EntityInformation>,

    /// The locations of the references to each value in this module, used for
    /// renaming and go-to reference.
    pub value_references: ReferenceMap,
    /// The locations of the references to each type in this module, used for
    /// renaming and go-to reference.
    pub type_references: ReferenceMap,
    /// The locations of the references to each imported module, used for
    /// renaming and go-to reference.
    pub module_references: HashMap<EcoString, Vec<ModuleNameReference>>,
    /// The locations of the references to each record field label in this
    /// module, used for renaming and go-to reference.
    pub label_references: HashMap<RecordLabel, Vec<LabelReference>>,
    /// The locations at which each record field label is defined, one per
    /// variant that defines it, used for renaming and go-to definition.
    pub label_definitions: HashMap<RecordLabel, Vec<LabelDefinition>>,

    /// Maps a module's canonical name to the node of the import it was brought
    /// in by. Every import is inserted here (aliased or not), keyed by its full
    /// module name, _except_ imports with a discarded alias (`as _name`), which
    /// have no reachable node and so are absent.
    ///
    /// We need this to keep track of references made to imports by unqualified
    /// values/types: when an unqualified item is used we want to add an edge
    /// pointing to the import it comes from, so that if the item is used the
    /// import won't be marked as unused:
    ///
    /// ```gleam
    /// import wibble/wobble.{used}
    ///
    /// pub fn main() {
    ///   used
    /// }
    /// ```
    ///
    /// And each imported entity carries around the _name of the module_ and not
    /// just the alias (here it would be `wibble/wobble` and not just `wobble`),
    /// so this map is keyed by that full name rather than the local name.
    ///
    module_name_to_node: HashMap<EcoString, NodeIndex>,
}

impl ReferenceTracker {
    pub fn new() -> Self {
        Self::default()
    }

    fn get_or_create_node(&mut self, name: EcoString, layer: EntityLayer) -> NodeIndex {
        let entity = Entity { name, layer };

        match self.entities.get_by_left(&entity) {
            Some(index) => *index,
            None => {
                let index = self.graph.add_node(());
                _ = self.entities.insert(entity, index);
                index
            }
        }
    }

    fn create_node(&mut self, name: EcoString, layer: EntityLayer) -> NodeIndex {
        let entity = Entity { name, layer };
        let index = self.graph.add_node(());

        self.create_node_and_maybe_shadow(entity, index);

        index
    }

    fn create_node_and_maybe_shadow(&mut self, entity: Entity, index: NodeIndex) {
        match self.entities.insert(entity, index) {
            Overwritten::Neither => {}
            Overwritten::Left(mut entity, index)
            | Overwritten::Right(mut entity, index)
            | Overwritten::Pair(mut entity, index)
            | Overwritten::Both((mut entity, index), _) => {
                // If an entity with the same name as this already exists,
                // we still need to keep track of its usage! Thought it cannot
                // be referenced anymore, it still might have been used before this
                // point, or need to be marked as unused.
                // To do this, we keep track of a "Shadowed" entity in `entity_information`.
                if let Some(information) = self.entity_information.get(&entity) {
                    entity.layer = EntityLayer::Shadowed;
                    _ = self
                        .entity_information
                        .insert(entity.clone(), information.clone());
                    _ = self.entities.insert(entity, index);
                }
            }
        }
    }

    /// Add a call-graph edge from the current node to the entity with the given
    /// name and layer, creating its node if it doesn't exist yet. If the target
    /// is used then the current node counts as used too.
    ///
    fn add_reference_edge(&mut self, name: EcoString, layer: EntityLayer) {
        let target = self.get_or_create_node(name, layer);
        _ = self.graph.add_edge(self.current_node, target, ());
    }

    /// This function exists because of a specific edge-case where constants
    /// can shadow imported values. For example:
    /// ```gleam
    /// import math.{pi}
    ///
    /// pub const pi = pi
    /// ```
    /// Here, the new `pi` constant shadows the imported `pi` value, but it still
    /// references it, so it should not be marked as unused.
    /// In order for this to work, we must first set the `current_function` field
    /// so that the `pi` value is referenced by the public `pi` constant.
    /// However, we can't insert the `pi` constant into the name scope yet, since
    /// then it would count as referencing itself. We first need to set `current_function`,
    /// then once we have analysed the right-hand-side of the constant, we can
    /// register it in the scope using `register_constant`.
    ///
    pub fn begin_constant(&mut self) {
        self.current_node = self.graph.add_node(());
    }

    pub fn register_constant(&mut self, name: EcoString, location: SrcSpan, publicity: Publicity) {
        let entity = Entity {
            name,
            layer: EntityLayer::Value,
        };
        self.create_node_and_maybe_shadow(entity.clone(), self.current_node);
        match publicity {
            Publicity::Public | Publicity::Internal { .. } => {
                let _ = self.public_entities.insert(entity.clone());
            }
            Publicity::Private => {}
        }

        _ = self.entity_information.insert(
            entity,
            EntityInformation {
                kind: EntityKind::Constant,
                origin: location,
            },
        );
    }

    pub fn register_value(
        &mut self,
        name: EcoString,
        kind: EntityKind,
        location: SrcSpan,
        publicity: Publicity,
    ) {
        self.current_node = self.create_node(name.clone(), EntityLayer::Value);
        self.register_module_reference_from_imported_entity(&kind);

        let entity = Entity {
            name,
            layer: EntityLayer::Value,
        };
        match publicity {
            Publicity::Public | Publicity::Internal { .. } => {
                let _ = self.public_entities.insert(entity.clone());
            }
            Publicity::Private => {}
        }

        _ = self.entity_information.insert(
            entity,
            EntityInformation {
                kind,
                origin: location,
            },
        );
    }

    pub fn set_current_node(&mut self, name: EcoString) {
        self.current_node = self.get_or_create_node(name, EntityLayer::Value);
    }

    pub fn register_type(
        &mut self,
        name: EcoString,
        kind: EntityKind,
        location: SrcSpan,
        publicity: Publicity,
    ) {
        self.current_node = self.create_node(name.clone(), EntityLayer::Type);
        self.register_module_reference_from_imported_entity(&kind);

        let entity = Entity {
            name,
            layer: EntityLayer::Type,
        };
        match publicity {
            Publicity::Public | Publicity::Internal { .. } => {
                let _ = self.public_entities.insert(entity.clone());
            }
            Publicity::Private => {}
        }

        _ = self.entity_information.insert(
            entity,
            EntityInformation {
                kind,
                origin: location,
            },
        );
    }

    pub fn register_aliased_module(
        &mut self,
        used_name: EcoString,
        module_name: EcoString,
        alias_location: SrcSpan,
        import_location: SrcSpan,
        module_location: SrcSpan,
    ) {
        // We first record a node for the module being aliased.
        self.register_module(
            used_name.clone(),
            module_name.clone(),
            import_location,
            module_location,
            Some(alias_location),
        );

        // Then we create a node for the alias, as the alias itself might be
        // unused!
        self.current_node = self.create_node(used_name.clone(), EntityLayer::Module);
        // Also we want to register the fact that if this alias is used then the
        // import is used: so we add a reference from the alias to the import
        // we've just added.
        self.register_module_reference_by_module_name(module_name.clone());

        // Finally we can add information for this alias:
        let entity = Entity {
            name: used_name,
            layer: EntityLayer::Module,
        };
        _ = self.entity_information.insert(
            entity,
            EntityInformation {
                kind: EntityKind::ModuleAlias {
                    module: module_name,
                },
                origin: alias_location,
            },
        );
    }

    pub fn register_module(
        &mut self,
        used_name: EcoString,
        module_name: EcoString,
        location: SrcSpan,
        module_location: SrcSpan,
        alias_location: Option<SrcSpan>,
    ) {
        self.current_node = self.create_node(used_name.clone(), EntityLayer::Module);
        let _ = self
            .module_name_to_node
            .insert(module_name.clone(), self.current_node);

        let reference = if let Some(alias_location) = alias_location {
            ModuleNameReference::AliasedImport {
                module_location,
                alias_location,
                alias: used_name.clone(),
            }
        } else {
            ModuleNameReference::Import {
                module_location,
                import_end: location.end,
            }
        };

        self.register_module_name_reference(module_name.clone(), reference);

        let entity = Entity {
            name: used_name,
            layer: EntityLayer::Module,
        };

        _ = self.entity_information.insert(
            entity,
            EntityInformation {
                kind: EntityKind::ImportedModule { module_name },
                origin: location,
            },
        );
    }

    fn register_module_reference_from_imported_entity(&mut self, entity_kind: &EntityKind) {
        match entity_kind {
            EntityKind::Function
            | EntityKind::Constant
            | EntityKind::Constructor
            | EntityKind::Type
            | EntityKind::ImportedModule { .. }
            | EntityKind::ModuleAlias { .. } => (),

            EntityKind::ImportedConstructor { module }
            | EntityKind::ImportedType { module }
            | EntityKind::ImportedValue { module } => {
                self.register_module_reference_by_module_name(module.clone());
            }
        }
    }

    pub fn register_value_reference(
        &mut self,
        module: EcoString,
        name: EcoString,
        referenced_name: &EcoString,
        location: SrcSpan,
        kind: ReferenceKind,
    ) {
        match &kind {
            ReferenceKind::Qualified {
                module_alias,
                module_location,
            } => {
                let last_module_segment = module.split('/').next_back().unwrap_or(&module);
                let reference = if last_module_segment == module_alias {
                    ModuleNameReference::ModuleSelect(*module_location)
                } else {
                    ModuleNameReference::AliasedModuleSelect(*module_location)
                };
                self.register_module_name_reference(module.clone(), reference);
            }
            ReferenceKind::Import(_) | ReferenceKind::Definition => {}
            ReferenceKind::Alias | ReferenceKind::Unqualified => {
                self.add_reference_edge(referenced_name.clone(), EntityLayer::Value);
            }
        }

        self.value_references
            .entry((module, name))
            .or_default()
            .push(Reference { location, kind });
    }

    pub fn register_type_reference(
        &mut self,
        module: EcoString,
        name: EcoString,
        referenced_name: &EcoString,
        location: SrcSpan,
        kind: ReferenceKind,
    ) {
        match &kind {
            ReferenceKind::Qualified {
                module_alias,
                module_location,
            } => {
                let last_module_segment = module.split('/').next_back().unwrap_or(&module);
                let reference = if last_module_segment == module_alias {
                    ModuleNameReference::ModuleSelect(*module_location)
                } else {
                    ModuleNameReference::AliasedModuleSelect(*module_location)
                };
                self.register_module_name_reference(module.clone(), reference);
            }
            ReferenceKind::Import(_) | ReferenceKind::Definition => {}
            ReferenceKind::Alias | ReferenceKind::Unqualified => {
                self.register_type_reference_in_call_graph(referenced_name.clone());
            }
        }

        self.type_references
            .entry((module, name))
            .or_default()
            .push(Reference { location, kind });
    }

    /// Register a reference to a module name written explicitly in the source
    /// code, when the module name or local alias appears. This is separate from
    /// the call-graph edges added by `register_module_reference_by_alias`
    /// and `register_module_reference_by_module_name`, which track references
    /// created implicitly, for example when using unqualified imports.
    ///
    pub fn register_module_name_reference(
        &mut self,
        module: EcoString,
        reference: ModuleNameReference,
    ) {
        self.module_references
            .entry(module)
            .or_default()
            .push(reference);
    }

    /// Registers a use of a record field label.
    ///
    /// `type_` is the `(module, name)` of the type the label belongs to, as
    /// returned by [`Type::named_type_name`].
    pub fn register_label_reference(
        &mut self,
        type_: (EcoString, EcoString),
        label: EcoString,
        location: SrcSpan,
        syntax: LabelSyntax,
    ) {
        let (type_module, type_name) = type_;
        self.label_references
            .entry(RecordLabel {
                type_module,
                type_name,
                label,
            })
            .or_default()
            .push(LabelReference { location, syntax });
    }

    /// Registers the definition of a record field label in one of the
    /// variants of a custom type.
    ///
    /// `type_` is the `(module, name)` of the type being defined, as returned
    /// by [`Type::named_type_name`].
    pub fn register_label_definition(
        &mut self,
        type_: (EcoString, EcoString),
        label: EcoString,
        location: SrcSpan,
        variant: EcoString,
    ) {
        let (type_module, type_name) = type_;
        self.label_definitions
            .entry(RecordLabel {
                type_module,
                type_name,
                label,
            })
            .or_default()
            .push(LabelDefinition { location, variant });
    }

    /// Like `register_type_reference`, but doesn't modify `self.type_references`.
    /// This is used when we define a constructor for a custom type. The constructor
    /// doesn't actually "reference" its type, but if the constructor is used, the
    /// type should also be considered used. The best way to represent this relationship
    /// is to make a connection between them in the call graph.
    ///
    pub fn register_type_reference_in_call_graph(&mut self, name: EcoString) {
        self.add_reference_edge(name, EntityLayer::Type);
    }

    /// Add a call-graph edge to a module resolved by its local name: the
    /// alias, or the last segment of the module path when there is no alias.
    /// Use this for references that name the module as written at the use site,
    /// such as a qualified `wobble.thing`.
    ///
    pub fn register_module_reference_by_alias(&mut self, name: EcoString) {
        self.add_reference_edge(name, EntityLayer::Module);
    }

    /// Add a call-graph edge to an import resolved by its canonical module name
    /// via `module_name_to_node`. Use this for references that carry the full
    /// module name rather than the local alias, such as an unqualified item
    /// that remembers which module it came from.
    ///
    fn register_module_reference_by_module_name(&mut self, name: EcoString) {
        // If the module has no node then it was imported with a discarded
        // alias, meaning there's no import for the reference to point at.
        let Some(target) = self.module_name_to_node.get(&name).copied() else {
            return;
        };
        _ = self.graph.add_edge(self.current_node, target, ());
    }

    pub fn unused(&self) -> HashMap<Entity, EntityInformation> {
        let mut unused_values = HashMap::with_capacity(self.entities.len());

        for (entity, information) in self.entity_information.iter() {
            _ = unused_values.insert(entity.clone(), information.clone());
        }
        for entity in self.public_entities.iter() {
            if let Some(index) = self.entities.get_by_left(entity) {
                self.mark_entity_as_used(&mut unused_values, entity, *index);
            }
        }

        for (entity, _) in self.entities.iter() {
            let Some(index) = self.entities.get_by_left(entity) else {
                continue;
            };

            if self.public_entities.contains(entity) {
                self.mark_entity_as_used(&mut unused_values, entity, *index);
            } else {
                // If the entity is not public, we still want to mark referenced
                // imports as used.
                self.mark_referenced_imports_as_used(&mut unused_values, entity, *index);
            }
        }

        unused_values
    }

    fn mark_entity_as_used(
        &self,
        unused: &mut HashMap<Entity, EntityInformation>,
        entity: &Entity,
        index: NodeIndex,
    ) {
        if unused.remove(entity).is_some() {
            for node in self.graph.neighbors_directed(index, Direction::Outgoing) {
                if let Some(entity) = self.entities.get_by_right(&node) {
                    self.mark_entity_as_used(unused, entity, node);
                }
            }
        }
    }

    fn mark_referenced_imports_as_used(
        &self,
        unused: &mut HashMap<Entity, EntityInformation>,
        entity: &Entity,
        index: NodeIndex,
    ) {
        // If the entity is a module there's no way it can reference other
        // modules so we just ignore it.
        // This also means that module aliases do not count as using a module!
        if entity.layer == EntityLayer::Module {
            return;
        }

        for node in self.graph.neighbors_directed(index, Direction::Outgoing) {
            // We only want to mark referenced modules as used, so if the node
            // is not a module we just skip it.
            let Some(
                module @ Entity {
                    layer: EntityLayer::Module,
                    ..
                },
            ) = self.entities.get_by_right(&node)
            else {
                continue;
            };

            // If the value appears in the module import list, it doesn't count
            // as using it!
            let is_imported_type = self
                .type_references
                .contains_key(&(module.name.clone(), entity.name.clone()));
            let is_imported_value = self
                .value_references
                .contains_key(&(module.name.clone(), entity.name.clone()));
            let appears_in_module_import_list = is_imported_type || is_imported_value;
            if !(appears_in_module_import_list) {
                self.mark_entity_as_used(unused, module, node);
            }
        }
    }
}
