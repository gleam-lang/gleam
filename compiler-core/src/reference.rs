use std::collections::{HashMap, HashSet};

use crate::{
    ast::{self, Publicity, SrcSpan},
    type_::Type,
};
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
    PrivateType,
    ImportedConstructor,
    ImportedType,
    ImportedValue,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Entity {
    ModuleValue { name: EcoString },
    ModuleType { name: EcoString },
    TypeAlias { name: EcoString },
    ImportedValue { module: EcoString, name: EcoString },
    ImportedType { module: EcoString, name: EcoString },
}

#[derive(Debug, Clone, Copy)]
pub enum Qualification {
    Qualified,
    Unqualified,
}

#[derive(Debug, Default)]
pub struct ReferenceTracker {
    /// A call-graph which tracks which values are referenced by which other value,
    /// used for dead code detection.
    graph: StableGraph<(), (), Directed>,
    entities: BiMap<Entity, NodeIndex>,
    current_function: NodeIndex,
    public_entities: HashMap<EcoString, ast::Layer>,
    entity_information: HashMap<Entity, EntityInformation>,
    current_module: EcoString,
    type_aliases: HashSet<EcoString>,

    /// The locations of the references to each value in this module, used for
    /// renaming and go-to reference.
    pub value_references: ReferenceMap,
    /// The locations of the references to each type in this module, used for
    /// renaming and go-to reference.
    pub type_references: ReferenceMap,
}

impl ReferenceTracker {
    pub fn new(current_module: EcoString) -> Self {
        let mut default = Self::default();
        default.current_module = current_module;
        default
    }

    fn entity(&self, module: &EcoString, name: &EcoString, layer: ast::Layer) -> Entity {
        if module == &self.current_module {
            match layer {
                ast::Layer::Value => Entity::ModuleValue { name: name.clone() },
                ast::Layer::Type if self.type_aliases.contains(name) => {
                    Entity::TypeAlias { name: name.clone() }
                }
                ast::Layer::Type => Entity::ModuleType { name: name.clone() },
            }
        } else {
            match layer {
                ast::Layer::Value => Entity::ImportedValue {
                    module: module.clone(),
                    name: name.clone(),
                },
                ast::Layer::Type => Entity::ImportedType {
                    module: module.clone(),
                    name: name.clone(),
                },
            }
        }
    }

    fn get_or_create_node(
        &mut self,
        module: &EcoString,
        name: &EcoString,
        layer: ast::Layer,
    ) -> NodeIndex {
        let entity = self.entity(module, name, layer);

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
        module: &EcoString,
        name: &EcoString,
        kind: EntityKind,
        location: SrcSpan,
        publicity: Publicity,
    ) {
        self.current_function = self.get_or_create_node(module, name, ast::Layer::Value);
        match publicity {
            Publicity::Public | Publicity::Internal { .. } => {
                _ = self.public_entities.insert(name.clone(), ast::Layer::Value);
            }
            Publicity::Private => {}
        }
        _ = self.entity_information.insert(
            self.entity(module, name, ast::Layer::Value),
            EntityInformation {
                kind,
                origin: location,
            },
        );
    }

    pub fn set_current_function(&mut self, module: &EcoString, name: &EcoString) {
        self.current_function = self.get_or_create_node(module, name, ast::Layer::Value);
    }

    pub fn register_type(
        &mut self,
        module: &EcoString,
        name: &EcoString,
        kind: EntityKind,
        location: SrcSpan,
    ) {
        _ = self.entity_information.insert(
            self.entity(module, name, ast::Layer::Type),
            EntityInformation {
                kind,
                origin: location,
            },
        );
    }

    pub fn register_type_alias(
        &mut self,
        name: EcoString,
        location: SrcSpan,
        publicity: Publicity,
        kind: EntityKind,
    ) {
        _ = self.type_aliases.insert(name.clone());

        let current_module = self.current_module.clone();
        self.current_function = self.get_or_create_node(&current_module, &name, ast::Layer::Type);
        match publicity {
            Publicity::Public | Publicity::Internal { .. } => {
                _ = self.public_entities.insert(name.clone(), ast::Layer::Type);
            }
            Publicity::Private => {}
        }

        let entity = Entity::TypeAlias { name };
        _ = self.entity_information.insert(
            entity.clone(),
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
        location: SrcSpan,
        kind: ReferenceKind,
    ) {
        match kind {
            ReferenceKind::Qualified | ReferenceKind::Import | ReferenceKind::Definition => {}
            ReferenceKind::Alias | ReferenceKind::Unqualified => {
                let target = self.get_or_create_node(&module, &name, ast::Layer::Value);
                _ = self.graph.add_edge(self.current_function, target, ());
            }
        }

        self.value_references
            .entry((module, name))
            .or_default()
            .push(Reference { location, kind });
    }

    pub fn register_type_or_type_alias_reference(
        &mut self,
        name: &EcoString,
        type_: &Type,
        location: SrcSpan,
        qualification: Qualification,
    ) {
        if let Some((module, type_name)) = type_.named_type_name() {
            let kind = match qualification {
                Qualification::Qualified => ReferenceKind::Qualified,
                Qualification::Unqualified if name != &type_name => ReferenceKind::Alias,
                Qualification::Unqualified => ReferenceKind::Unqualified,
            };

            self.type_references
                .entry((module, type_name))
                .or_default()
                .push(Reference { location, kind });
        }

        match qualification {
            Qualification::Qualified => {}
            Qualification::Unqualified => {
                if self.type_aliases.contains(name) {
                    let current_module = self.current_module.clone();
                    let index = self.get_or_create_node(&current_module, &name, ast::Layer::Type);
                    _ = self.graph.add_edge(self.current_function, index, ());
                } else if let Some((module, name)) = type_.named_type_name() {
                    let index = self.get_or_create_node(&module, &name, ast::Layer::Type);
                    _ = self.graph.add_edge(self.current_function, index, ());
                }
            }
        }
    }

    pub fn register_type_reference(
        &mut self,
        module: EcoString,
        name: EcoString,
        location: SrcSpan,
        kind: ReferenceKind,
    ) {
        match kind {
            ReferenceKind::Qualified | ReferenceKind::Import | ReferenceKind::Definition => {}
            ReferenceKind::Alias | ReferenceKind::Unqualified => {
                self.register_type_reference_in_call_graph(&module, &name)
            }
        }

        self.type_references
            .entry((module, name))
            .or_default()
            .push(Reference { location, kind });
    }

    pub fn register_type_reference_in_call_graph(&mut self, module: &EcoString, name: &EcoString) {
        let target = self.get_or_create_node(module, name, ast::Layer::Type);
        _ = self.graph.add_edge(self.current_function, target, ());
    }

    pub fn unused_values(&self) -> HashMap<Entity, EntityInformation> {
        let mut unused_values = HashMap::with_capacity(self.entities.len());

        for (entity, information) in self.entity_information.iter() {
            _ = unused_values.insert(entity.clone(), *information);
        }
        for (name, layer) in self.public_entities.iter() {
            let entity = self.entity(&self.current_module, name, *layer);
            let index = self.entities.get_by_left(&entity).expect("Entity exists");
            self.mark_value_as_used(&mut unused_values, &entity, *index);
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
