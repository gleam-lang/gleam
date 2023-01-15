//! Graphs that represent the relationships between entities in a Gleam module,
//! such as module functions or constants.

use crate::ast::SrcSpan;
use crate::{
    ast::{ExternalFunction, Function as ModuleFunction, UntypedExpr},
    Result,
};
use itertools::Itertools;
use petgraph::stable_graph::NodeIndex;
use petgraph::{stable_graph::StableGraph, Directed};

#[derive(Debug)]
pub enum Function {
    Module(ModuleFunction<(), UntypedExpr>),
    External(ExternalFunction<()>),
}

impl Function {
    pub fn name(&self) -> &str {
        match self {
            Function::Module(f) => &f.name,
            Function::External(f) => &f.name,
        }
    }

    pub fn location(&self) -> SrcSpan {
        match self {
            Function::Module(f) => f.location,
            Function::External(f) => f.location,
        }
    }

    fn register_references(
        &self,
        graph: &mut StableGraph<(), ()>,
        indicies: &mut im::HashMap<&str, (NodeIndex, SrcSpan)>,
    ) {
        todo!()
    }
}

/// Determine the order in which functions should be compiled and if any
/// mutually recursive functions need to be compiled together.
///
pub fn into_dependency_order(functions: Vec<Function>) -> Result<Vec<Vec<Function>>> {
    let mut name_map = im::HashMap::new();
    let mut graph: StableGraph<(), (), Directed> = StableGraph::new();

    // Add each function to the graph, storing the index of the node under the
    // name of the function.
    for function in &functions {
        let index = graph.add_node(());
        let previous = name_map.insert(function.name(), (index, function.location()));

        // TODO: return an error if there are duplicate function names
        if let Some((_, previous_location)) = previous {
            panic!(
                "Duplicate function name {} found at {:?} and {:?}",
                function.name(),
                function.location(),
                previous_location
            );
        }
    }

    // Build the call graph between the module functions.
    for function in &functions {
        function.register_references(&mut graph, &mut name_map);
    }

    // Determine the order in which the functions should be compiled by looking
    // at which other functions they depend on.
    let indicies = crate::graph::into_dependency_order(graph);

    // Drop these transient values so we can consume the values they reference.
    drop(name_map);

    // We got node indicies back, so we need to map them back to the functions
    // they represent.
    let mut functions = functions.into_iter().map(Some).collect_vec();
    let ordered = indicies
        .into_iter()
        .map(|level| {
            level
                .into_iter()
                .map(|index| {
                    functions[index.index()]
                        .take()
                        .expect("Function already taken")
                })
                .collect_vec()
        })
        .collect_vec();

    Ok(ordered)
}

// TODO: test it!
