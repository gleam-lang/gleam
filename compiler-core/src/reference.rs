use std::collections::{HashMap, HashSet};

use crate::ast::{Publicity, SrcSpan};
use bimap::{BiMap, Overwritten};
use ecow::EcoString;
use petgraph::{
    Directed, Direction,
    stable_graph::{NodeIndex, StableGraph},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceKind {
    /// We're referencing a type/value in a qualified manner. For example:
    ///
    /// ```gleam
    /// import wibble
    ///
    /// pub fn main() -> wibble.Wobble
    /// //               ^^^^^^^^^^^^^ This is a qualified reference
    /// ```
    ///
    Qualified,

    /// We're referencing a type/value that has been brought in scope through an
    /// unqualified import or that is defined in the same module (and so is only
    /// accessed unqualified).
    /// For example:
    ///
    /// ```gleam
    /// import wibble.{type Wobble}
    ///
    /// pub fn main() -> Wobble { todo }
    /// //               ^^^^^^ This is an unqualified reference
    /// ```
    ///
    Unqualified,

    /// We're referencing a type/value through an import.
    /// For example:
    ///
    /// ```gleam
    /// import wibble.{type Wobble}
    /// //                  ^^^^^^ This is a reference to the `Wobble` type
    /// ```
    ///
    Import,

    /// This is to keep track of definitions themselves (as far as the tracking
    /// code is concerned a definition references itself). For example:
    ///
    /// ```gleam
    /// fn wibble() {}
    /// // ^^^^^^ This!
    /// ```
    ///
    Definition,

    /// This happens when we're referencing a value that has been imported and
    /// aliased.
    ///
    /// For example:
    ///
    /// ```gleam
    /// import wibble.{Wibble as Wobble}
    ///
    /// pub fn make_wobble() -> Wobble
    /// //                      ^^^^^^ This is an alias reference
    /// ```
    ///
    Alias,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Reference {
    pub location: SrcSpan,
    pub kind: ReferenceKind,
}

pub type ReferenceMap = HashMap<(EcoString, EcoString), Vec<Reference>>;

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

    /// This map is used to access the nodes of modules that were not
    /// aliased, given their name.
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
    /// just the alias (here it would be `wibble/wobble` and not just `wobble`).
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
    ) {
        // We first record a node for the module being aliased. We use its entire
        // name to identify it in this case and keep track of the node it's
        // associated with.
        self.register_module(module_name.clone(), module_name.clone(), import_location);

        // Then we create a node for the alias, as the alias itself might be
        // unused!
        self.current_node = self.create_node(used_name.clone(), EntityLayer::Module);
        // Also we want to register the fact that if this alias is used then the
        // import is used: so we add a reference from the alias to the import
        // we've just added.
        self.register_module_reference(module_name.clone());

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
    ) {
        self.current_node = self.create_node(used_name.clone(), EntityLayer::Module);
        let _ = self
            .module_name_to_node
            .insert(module_name.clone(), self.current_node);

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
                self.register_module_reference(module.clone())
            }
        }
    }

    /// Registers a reference of the given kind to a value that has the given
    /// name.
    ///
    pub fn register_value_reference(
        &mut self,
        module: EcoString,
        // The name of the value that is being referenced.
        name: EcoString,
        // The name with which we are referring to the value.
        referenced_name: &EcoString,
        location: SrcSpan,
        // How we are referencing the value.
        kind: ReferenceKind,
    ) {
        match kind {
            ReferenceKind::Qualified | ReferenceKind::Import | ReferenceKind::Definition => {}
            ReferenceKind::Alias | ReferenceKind::Unqualified => {
                let target = self.get_or_create_node(referenced_name.clone(), EntityLayer::Value);
                _ = self.graph.add_edge(self.current_node, target, ());
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
        match kind {
            ReferenceKind::Qualified | ReferenceKind::Import | ReferenceKind::Definition => {}
            ReferenceKind::Alias | ReferenceKind::Unqualified => {
                self.register_type_reference_in_call_graph(referenced_name.clone())
            }
        }

        self.type_references
            .entry((module, name))
            .or_default()
            .push(Reference { location, kind });
    }

    /// Like `register_type_reference`, but doesn't modify `self.type_references`.
    /// This is used when we define a constructor for a custom type. The constructor
    /// doesn't actually "reference" its type, but if the constructor is used, the
    /// type should also be considered used. The best way to represent this relationship
    /// is to make a connection between them in the call graph.
    ///
    pub fn register_type_reference_in_call_graph(&mut self, name: EcoString) {
        let target = self.get_or_create_node(name, EntityLayer::Type);
        _ = self.graph.add_edge(self.current_node, target, ());
    }

    pub fn register_module_reference(&mut self, name: EcoString) {
        let target = match self.module_name_to_node.get(&name) {
            Some(target) => *target,
            None => self.get_or_create_node(name, EntityLayer::Module),
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
