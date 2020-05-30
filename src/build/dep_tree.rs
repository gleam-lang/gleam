use crate::error::GleamExpect;
use petgraph::{algo::Cycle, graph::NodeIndex, Direction};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug, Default)]
pub struct DependencyTree<T> {
    graph: petgraph::Graph<T, ()>,
    indexes: HashMap<T, NodeIndex>,
    values: HashMap<NodeIndex, T>,
}

/// Take a sequence of values and their deps, and return the values in
/// order so that deps come before the dependants.
///
/// Errors if there are duplicate values, unknown deps, or cycles.
///
pub fn toposort_deps<'a>(inputs: &'a [(&'a str, Vec<&'a str>)]) -> Result<Vec<&'a str>, Error<'a>> {
    let mut indexes = Indexes::new();
    let mut edges = Vec::with_capacity(inputs.len());

    for (value, deps) in inputs {
        let index = indexes.get_index(value);
        for dep in deps.into_iter() {
            edges.push((indexes.get_index(value), indexes.get_index(dep)))
        }
    }

    let mut graph = petgraph::Graph::<(), ()>::from_edges(edges.into_iter());

    let sequence: Vec<_> = petgraph::algo::toposort(&graph, None)
        // .map_err(import_cycle)?
        .unwrap() // TODO
        .into_iter()
        .map(move |i| indexes.pop_value(&i))
        .rev()
        .collect();

    Ok(sequence)
}

struct Indexes<'a> {
    next: usize,
    indexes: HashMap<&'a str, NodeIndex<u32>>,
    values: HashMap<NodeIndex<u32>, &'a str>,
}

impl<'a> Indexes<'a> {
    pub fn new() -> Self {
        Self {
            next: 0,
            indexes: Default::default(),
            values: Default::default(),
        }
    }

    pub fn get_index(&mut self, value: &'a str) -> NodeIndex<u32> {
        match self.indexes.get(value) {
            Some(i) => *i,
            None => {
                let index = NodeIndex::new(self.next);
                self.next += 1;
                self.indexes.insert(value, index);
                self.values.insert(index, value);
                index
            }
        }
    }

    pub fn pop_value(&mut self, index: &NodeIndex<u32>) -> &'a str {
        self.values
            .get(index)
            .gleam_expect("DependencyTree.pop_value(): Unknown graph index")
    }
}

//     // fn calculate_dependencies(&mut self) -> Result<(), CreateError<T>> {
//     //     for value in self.value.values() {
//     //         let module_index = self.indexes.get(&module_name).gleam_expect(
//     //             "DependencyTree.calculate_dependencies(): Unable to find module index for name",
//     //         );
//     //         let module = self.modules.get(&module_index).gleam_expect(
//     //             "DependencyTree.calculate_dependencies(): Unable to find module for index",
//     //         );

//     //         for (dep, location) in deps {
//     //             let dep_index = self.indexes.get(&dep).ok_or_else(|| NodeNotFound(dep))?;

//     //             self.graph
//     //                 .add_edge(dep_index.clone(), module_index.clone(), ());
//     //         }
//     //     }
//     //     Ok(())
//     // }

//     fn insert(&mut self, value: T, deps: &[T]) -> Result<(), DuplicateNode<T>> {
//         // Check to see if we already have a module with this name
//         if let Some(_) = self.indexes.get(&value).and_then(|i| self.modules.get(i)) {
//             return Err(DuplicateNode(value));
//         }

//         // Register the module
//         let index = self.graph.add_node(value.clone());
//         self.indexes.insert(value, index);
//         self.values.insert(index, value);
//         Ok(())
//     }
// }

//     pub fn consume(&mut self) -> Result<impl Iterator<Item = T> + '_, ImportCycle<T>> {
//         let iter = petgraph::algo::toposort(&self.graph, None)
//             .map_err(|e| self.import_cycle(e))?
//             .into_iter()
//             .map(move |i| {
//                 self.values
//                     .remove(&i)
//                     .gleam_expect("DependencyTree.consume(): Unknown graph index")
//             });
//         Ok(iter)
//     }

//     fn import_cycle(&mut self, cycle: Cycle<NodeIndex>) -> ImportCycle<T> {
//         let origin = cycle.node_id();
//         let mut path = vec![];
//         self.find_cycle(origin, origin, &mut path, &mut HashSet::new());
//         let chain: Vec<_> = path
//             .iter()
//             .map(|index| {
//                 self.values
//                     .remove(index)
//                     .gleam_expect("DependencyTree.import_cycle(): cannot find values for index")
//             })
//             .collect();
//         ImportCycle(chain)
//     }

//     fn find_cycle(
//         &self,
//         origin: NodeIndex,
//         parent: NodeIndex,
//         path: &mut Vec<NodeIndex>,
//         seen: &mut HashSet<NodeIndex>,
//     ) -> bool {
//         seen.insert(parent);
//         for node in self.graph.neighbors_directed(parent, Direction::Outgoing) {
//             if node == origin {
//                 path.push(node);
//                 return true;
//             }
//             if seen.contains(&node) {
//                 continue;
//             }
//             if self.find_cycle(origin, node, path, seen) {
//                 path.push(node);
//                 return true;
//             }
//         }
//         false
//     }

#[derive(Debug, PartialEq)]
pub enum Error<'a> {
    DuplicateNode(&'a str),
    NodeNotFound(&'a str),
    Cycle(Vec<&'a str>),
}
