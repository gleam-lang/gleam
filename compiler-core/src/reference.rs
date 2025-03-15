use std::collections::{HashMap, HashSet};

use crate::ast::{Publicity, SrcSpan};
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
pub struct ValueInformation {
    pub origin: SrcSpan,
    pub kind: ValueKind,
}

#[derive(Debug, Clone, Copy)]
pub enum ValueKind {
    Function,
    Constant,
    TypeVariant,
}

#[derive(Debug, Default)]
pub struct ReferenceTracker {
    /// A call-graph which tracks which values are referenced by which other value,
    /// used for dead code detection.
    graph: StableGraph<(), (), Directed>,
    names: BiMap<EcoString, NodeIndex>,
    current_function: NodeIndex,
    public_values: HashSet<EcoString>,
    value_information: HashMap<EcoString, ValueInformation>,

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
}

impl ReferenceTracker {
    fn get_or_create_node(&mut self, name: &EcoString) -> NodeIndex {
        match self.names.get_by_left(name) {
            Some(index) => *index,
            None => {
                let index = self.graph.add_node(());
                _ = self.names.insert(name.clone(), index);
                index
            }
        }
    }

    pub fn register_value(
        &mut self,
        name: &EcoString,
        kind: ValueKind,
        location: SrcSpan,
        publicity: Publicity,
    ) {
        self.current_function = self.get_or_create_node(name);
        match publicity {
            Publicity::Public | Publicity::Internal { .. } => {
                _ = self.public_values.insert(name.clone());
            }
            Publicity::Private => {}
        }
        _ = self.value_information.insert(
            name.clone(),
            ValueInformation {
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
        let target = self.get_or_create_node(&name);
        self.value_references
            .entry((module, name))
            .or_default()
            .push(Reference { location, kind });

        _ = self.graph.add_edge(self.current_function, target, ());
    }

    pub fn register_type_reference(
        &mut self,
        module: EcoString,
        name: EcoString,
        location: SrcSpan,
        kind: ReferenceKind,
    ) {
        self.type_references
            .entry((module, name))
            .or_default()
            .push(Reference { location, kind });
    }

    pub fn unused_values(&self) -> HashMap<EcoString, ValueInformation> {
        let mut unused_values = HashMap::with_capacity(self.names.len());

        for (name, information) in self.value_information.iter() {
            _ = unused_values.insert(name.clone(), *information);
        }
        for value in self.public_values.iter() {
            let index = self.names.get_by_left(value).expect("Value exists");
            self.mark_value_as_used(&mut unused_values, value, *index);
        }

        unused_values
    }

    fn mark_value_as_used(
        &self,
        unused: &mut HashMap<EcoString, ValueInformation>,
        name: &EcoString,
        index: NodeIndex,
    ) {
        if unused.remove(name).is_some() {
            for node in self.graph.neighbors_directed(index, Direction::Outgoing) {
                let name = self.names.get_by_right(&node).expect("Value exists");
                self.mark_value_as_used(unused, name, node);
            }
        }
    }
}
