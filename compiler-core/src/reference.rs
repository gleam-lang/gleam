use std::collections::HashMap;

use crate::ast::{self, Publicity, SrcSpan};
use bimap::BiMap;
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

#[derive(Debug, Clone)]
pub struct EntityInformation {
    pub name: EcoString,
    pub layer: ast::Layer,
    pub origin: SrcSpan,
    pub kind: EntityKind,
}

#[derive(Debug, Clone, Copy)]
pub enum EntityKind {
    Function,
    Constant,
    Constructor,
    PrivateType,
    ImportedConstructor,
    ImportedType,
    ImportedValue,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Entity {
    pub name: EcoString,
    pub layer: ast::Layer,
}

#[derive(Debug, Default)]
pub struct ReferenceTracker {
    /// A call-graph which tracks which values are referenced by which other value,
    /// used for dead code detection.
    graph: StableGraph<(), (), Directed>,
    entities: BiMap<Entity, NodeIndex>,
    current_function: NodeIndex,
    public_entities: Vec<Entity>,
    entity_information: Vec<EntityInformation>,

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

    fn get_or_create_node(&mut self, name: EcoString, layer: ast::Layer) -> NodeIndex {
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

    pub fn register_value(
        &mut self,
        name: EcoString,
        kind: EntityKind,
        location: SrcSpan,
        publicity: Publicity,
    ) {
        self.current_function = self.get_or_create_node(name.clone(), ast::Layer::Value);
        let entity = Entity {
            name,
            layer: ast::Layer::Value,
        };
        match publicity {
            Publicity::Public | Publicity::Internal { .. } => {
                _ = self.public_entities.push(entity.clone());
            }
            Publicity::Private => {}
        }

        _ = self.entity_information.push(EntityInformation {
            name: entity.name,
            layer: ast::Layer::Value,
            kind,
            origin: location,
        });
    }

    pub fn set_current_function(&mut self, name: EcoString) {
        self.current_function = self.get_or_create_node(name, ast::Layer::Value);
    }

    pub fn register_type(
        &mut self,
        name: EcoString,
        kind: EntityKind,
        location: SrcSpan,
        publicity: Publicity,
    ) {
        self.current_function = self.get_or_create_node(name.clone(), ast::Layer::Type);
        let entity = Entity {
            name,
            layer: ast::Layer::Type,
        };
        match publicity {
            Publicity::Public | Publicity::Internal { .. } => {
                _ = self.public_entities.push(entity.clone());
            }
            Publicity::Private => {}
        }

        _ = self.entity_information.push(EntityInformation {
            name: entity.name,
            layer: ast::Layer::Type,
            kind,
            origin: location,
        });
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
                let target = self.get_or_create_node(referenced_name.clone(), ast::Layer::Value);
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
        let target = self.get_or_create_node(name, ast::Layer::Type);
        _ = self.graph.add_edge(self.current_function, target, ());
    }

    pub fn unused_values(&self) -> HashMap<Entity, EntityInformation> {
        let mut unused_values = HashMap::with_capacity(self.entities.len());

        for information in self.entity_information.iter() {
            _ = unused_values.insert(
                Entity {
                    name: information.name.clone(),
                    layer: information.layer,
                },
                information.clone(),
            );
        }
        for entity in self.public_entities.iter() {
            let index = self.entities.get_by_left(entity).expect("Entity exists");
            self.mark_value_as_used(&mut unused_values, entity, *index);
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
                let entity = self.entities.get_by_right(&node).expect("Value exists");
                self.mark_value_as_used(unused, entity, node);
            }
        }
    }
}
