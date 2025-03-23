use std::collections::HashMap;

use crate::ast::{Publicity, SrcSpan};
use bimap::{BiMap, Overwritten};
use ecow::EcoString;
use petgraph::{
    Directed, Direction,
    stable_graph::{NodeIndex, StableGraph},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceKind {
    Qualified,
    Unqualified,
    Import,
    Definition,
    Alias,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Reference {
    pub location: SrcSpan,
    pub kind: ReferenceKind,
}

pub type ReferenceMap = HashMap<(EcoString, EcoString), Vec<Reference>>;

#[derive(Debug, Clone, Copy)]
pub struct EntityInformation {
    pub origin: SrcSpan,
    pub kind: EntityKind,
}

/// Information about an "Entity". This determines how we warn about an entity
/// being unused.
#[derive(Debug, Clone, Copy)]
pub enum EntityKind {
    Function,
    Constant,
    Constructor,
    Type,
    ImportedConstructor,
    ImportedType,
    ImportedValue,
    ImportedModule,
    ModuleAlias,
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
    public_entities: Vec<Entity>,
    entity_information: HashMap<Entity, EntityInformation>,

    /// The locations of the references to each value in this module, used for
    /// renaming and go-to reference.
    pub value_references: ReferenceMap,
    /// The locations of the references to each type in this module, used for
    /// renaming and go-to reference.
    pub type_references: ReferenceMap,
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
                    _ = self.entity_information.insert(entity.clone(), *information);
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
                self.public_entities.push(entity.clone());
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
        let entity = Entity {
            name,
            layer: EntityLayer::Value,
        };
        match publicity {
            Publicity::Public | Publicity::Internal { .. } => {
                self.public_entities.push(entity.clone());
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
        let entity = Entity {
            name,
            layer: EntityLayer::Type,
        };
        match publicity {
            Publicity::Public | Publicity::Internal { .. } => {
                self.public_entities.push(entity.clone());
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

    pub fn register_module(&mut self, name: EcoString, kind: EntityKind, location: SrcSpan) {
        self.current_node = self.create_node(name.clone(), EntityLayer::Module);
        let entity = Entity {
            name,
            layer: EntityLayer::Module,
        };

        _ = self.entity_information.insert(
            entity,
            EntityInformation {
                kind,
                origin: location,
            },
        );
    }

    pub fn register_value_reference(
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
        let target = self.get_or_create_node(name, EntityLayer::Module);
        _ = self.graph.add_edge(self.current_node, target, ());
    }

    pub fn unused(&self) -> HashMap<Entity, EntityInformation> {
        let mut unused_values = HashMap::with_capacity(self.entities.len());

        for (entity, information) in self.entity_information.iter() {
            _ = unused_values.insert(entity.clone(), *information);
        }
        for entity in self.public_entities.iter() {
            if let Some(index) = self.entities.get_by_left(entity) {
                self.mark_entity_as_used(&mut unused_values, entity, *index);
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
}
