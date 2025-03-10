use std::collections::HashMap;

use crate::ast::SrcSpan;
use ecow::EcoString;
use petgraph::{
    Directed,
    stable_graph::{NodeIndex, StableGraph},
};

#[derive(Debug, Default)]
pub struct ReferenceTracker {
    /// A call-graph which tracks which values are referenced by which other value,
    /// used for dead code detection.
    graph: StableGraph<(), (), Directed>,
    names: HashMap<(EcoString, EcoString), NodeIndex>,
    current_function: NodeIndex,

    /// The locations of the references to each value in this module, used for
    /// renaming and go-to reference.
    reference_locations: HashMap<(EcoString, EcoString), Vec<SrcSpan>>,
}

impl ReferenceTracker {
    pub fn new() -> Self {
        Self::default()
    }
}

impl ReferenceTracker {
    pub fn into_locations(self) -> HashMap<(EcoString, EcoString), Vec<SrcSpan>> {
        self.reference_locations
    }

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

    pub fn register_reference(&mut self, module: EcoString, name: EcoString, location: SrcSpan) {
        let target = self.get_or_create_node(module.clone(), name.clone());
        self.reference_locations
            .entry((module, name))
            .or_default()
            .push(location);

        _ = self.graph.add_edge(self.current_function, target, ());
    }
}
