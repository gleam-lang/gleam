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
/// Any deps that are not nodes are ignored and presumed to be nodes
/// that do not need processing.
///
/// Errors if there are duplicate values, unknown deps, or cycles.
///
pub fn toposort_deps(inputs: Vec<(String, Vec<String>)>) -> Result<Vec<String>, Error> {
    let mut graph = petgraph::Graph::<(), ()>::with_capacity(inputs.len(), inputs.len() * 5);
    let mut values = HashMap::with_capacity(inputs.len());
    let mut indexes = HashMap::with_capacity(inputs.len());

    for (value, _deps) in inputs.iter() {
        let index = graph.add_node(());
        indexes.insert(value.clone(), index);
        values.insert(index, value.clone());
    }

    for (value, deps) in inputs {
        let from_index = indexes
            .get(value.as_str())
            .gleam_expect("Finding index for value");
        for dep in deps.into_iter() {
            if let Some(to_index) = indexes.get(dep.as_str()) {
                graph.add_edge(*from_index, *to_index, ());
            }
        }
    }

    match petgraph::algo::toposort(&graph, None) {
        Err(e) => Err(Error::Cycle(import_cycle(e, &graph, values))),

        Ok(seq) => Ok(seq
            .into_iter()
            .map(|i| values.remove(&i).gleam_expect("Finding value for index"))
            .rev()
            .collect()),
    }
}

// TODO: test
fn import_cycle(
    cycle: Cycle<NodeIndex>,
    graph: &petgraph::Graph<(), ()>,
    mut values: HashMap<NodeIndex, String>,
) -> Vec<String> {
    let origin = cycle.node_id();
    let mut path = vec![];
    find_cycle(origin, origin, &graph, &mut path, &mut HashSet::new());
    path.iter()
        .map(|index| {
            values
                .remove(index)
                .gleam_expect("dep_tree::import_cycle(): cannot find values for index")
        })
        .collect()
}

fn find_cycle(
    origin: NodeIndex,
    parent: NodeIndex,
    graph: &petgraph::Graph<(), ()>,
    path: &mut Vec<NodeIndex>,
    seen: &mut HashSet<NodeIndex>,
) -> bool {
    seen.insert(parent);
    for node in graph.neighbors_directed(parent, Direction::Outgoing) {
        if node == origin {
            path.push(node);
            return true;
        }
        if seen.contains(&node) {
            continue;
        }
        if find_cycle(origin, node, graph, path, seen) {
            path.push(node);
            return true;
        }
    }
    false
}

#[test]
fn toposort_deps_test() {
    // All deps are nodes
    assert_eq!(
        toposort_deps(vec![
            ("a".to_string(), vec!["b".to_string()]),
            ("c".to_string(), vec![]),
            ("b".to_string(), vec!["c".to_string()])
        ]),
        Ok(vec![
            "c".to_string().to_string(),
            "b".to_string().to_string(),
            "a".to_string().to_string()
        ])
    );

    // No deps
    assert_eq!(
        toposort_deps(vec![
            ("no-deps-1".to_string(), vec![]),
            ("no-deps-2".to_string(), vec![])
        ]),
        Ok(vec!["no-deps-1".to_string(), "no-deps-2".to_string(),])
    );

    // Some deps are not nodes (and thus are ignored)
    assert_eq!(
        toposort_deps(vec![
            ("a".to_string(), vec!["b".to_string(), "z".to_string()]),
            ("b".to_string(), vec!["x".to_string()])
        ]),
        Ok(vec!["b".to_string(), "a".to_string()])
    );
}

#[derive(Debug, PartialEq)]
pub enum Error {
    Cycle(Vec<String>),
}
