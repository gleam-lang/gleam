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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum EntityLayer {
    Type,
    Value,
    Shadowed,
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
    current_function: NodeIndex,
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
        self.current_function = self.graph.add_node(());
    }

    pub fn register_constant(&mut self, name: EcoString, location: SrcSpan, publicity: Publicity) {
        let entity = Entity {
            name,
            layer: EntityLayer::Value,
        };
        self.create_node_and_maybe_shadow(entity.clone(), self.current_function);
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
        self.current_function = self.create_node(name.clone(), EntityLayer::Value);
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

    pub fn set_current_function(&mut self, name: EcoString) {
        self.current_function = self.get_or_create_node(name, EntityLayer::Value);
    }

    pub fn register_type(
        &mut self,
        name: EcoString,
        kind: EntityKind,
        location: SrcSpan,
        publicity: Publicity,
    ) {
        self.current_function = self.create_node(name.clone(), EntityLayer::Type);
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
        self.current_function = self.create_node(name.clone(), EntityLayer::Module);
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
                _ = self.graph.add_edge(self.current_function, target, ());
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

    pub fn register_type_reference_in_call_graph(&mut self, name: EcoString) {
        let target = self.get_or_create_node(name, EntityLayer::Type);
        _ = self.graph.add_edge(self.current_function, target, ());
    }

    pub fn register_module_reference(&mut self, name: EcoString) {
        let target = self.get_or_create_node(name, EntityLayer::Module);
        _ = self.graph.add_edge(self.current_function, target, ());
    }

    pub fn unused_values(&self) -> HashMap<Entity, EntityInformation> {
        let mut unused_values = HashMap::with_capacity(self.entities.len());

        for (entity, information) in self.entity_information.iter() {
            _ = unused_values.insert(entity.clone(), *information);
        }
        for entity in self.public_entities.iter() {
            if let Some(index) = self.entities.get_by_left(entity) {
                self.mark_value_as_used(&mut unused_values, entity, *index);
            }
        }

        unused_values
    }

    fn mark_value_as_used(
        &self,
        unused: &mut HashMap<Entity, EntityInformation>,
        entity: &Entity,
        index: NodeIndex,
    ) {
        if unused.remove(entity).is_some() {
            for node in self.graph.neighbors_directed(index, Direction::Outgoing) {
                if let Some(entity) = self.entities.get_by_right(&node) {
                    self.mark_value_as_used(unused, entity, node);
                }
            }
        }
    }
}
