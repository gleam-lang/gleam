use std::collections::HashMap;

use crate::ast::SrcSpan;
use ecow::EcoString;
use petgraph::{
    Directed,
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

#[derive(Debug, Default)]
pub struct ReferenceTracker {
    /// A call-graph which tracks which values are referenced by which other value,
    /// used for dead code detection.
    graph: StableGraph<(), (), Directed>,
    names: HashMap<(EcoString, EcoString), NodeIndex>,
    current_function: NodeIndex,

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
    fn get_or_create_node(&mut self, module: EcoString, name: EcoString) -> NodeIndex {
        let key: (EcoString, EcoString) = (module, name);
        match self.names.get(&key) {
            Some(index) => *index,
            None => {
                let index = self.graph.add_node(());
                _ = self.names.insert(key, index);
                index
            }
        }
    }

    pub fn enter_function(&mut self, module: EcoString, name: EcoString) {
        self.current_function = self.get_or_create_node(module, name);
    }

    pub fn register_value_reference(
        &mut self,
        module: EcoString,
        name: EcoString,
        location: SrcSpan,
        kind: ReferenceKind,
    ) {
        let target = self.get_or_create_node(module.clone(), name.clone());
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
}
