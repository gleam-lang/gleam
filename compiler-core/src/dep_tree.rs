use ecow::EcoString;
use petgraph::{algo::Cycle, graph::NodeIndex, Direction};
use std::collections::{HashMap, HashSet};

#[cfg(test)]
use pretty_assertions::assert_eq;

/// Take a sequence of values and their deps, and return the values in
/// order so that deps come before the dependants.
///
/// Any deps that are not nodes are ignored and presumed to be nodes
/// that do not need processing.
///
/// Errors if there are duplicate values, unknown deps, or cycles.
///
pub fn toposort_deps(inputs: Vec<(EcoString, Vec<EcoString>)>) -> Result<Vec<EcoString>, Error> {
    let mut graph = petgraph::Graph::<(), ()>::with_capacity(inputs.len(), inputs.len() * 5);
    let mut values = HashMap::with_capacity(inputs.len());
    let mut indexes = HashMap::with_capacity(inputs.len());

    for (value, _deps) in &inputs {
        let index = graph.add_node(());
        let _ = indexes.insert(value.clone(), index);
        let _ = values.insert(index, value.clone());
    }

    for (value, deps) in inputs {
        let &from_index = indexes.get(&value).expect("Finding index for value");
        for &to_index in deps.into_iter().filter_map(|dep| indexes.get(&dep)) {
            let _ = graph.add_edge(from_index, to_index, ());
        }
    }

    match petgraph::algo::toposort(&graph, None) {
        Err(e) => Err(Error::Cycle(import_cycle(e, &graph, values))),

        Ok(seq) => Ok(seq
            .into_iter()
            .map(|i| values.remove(&i).expect("Finding value for index"))
            .rev()
            .collect()),
    }
}

fn import_cycle(
    cycle: Cycle<NodeIndex>,
    graph: &petgraph::Graph<(), ()>,
    mut values: HashMap<NodeIndex, EcoString>,
) -> Vec<EcoString> {
    let origin = cycle.node_id();
    let mut path = vec![];
    let _ = find_cycle(origin, origin, graph, &mut path, &mut HashSet::new());
    path.iter()
        .map(|index| {
            values
                .remove(index)
                .expect("dep_tree::import_cycle(): cannot find values for index")
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
    let _ = seen.insert(parent);
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

#[derive(Debug, PartialEq)]
pub enum Error {
    Cycle(Vec<EcoString>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn toposort_deps_test() {
        // All deps are nodes
        assert_eq!(
            toposort_deps(vec![
                ("a".into(), vec!["b".into()]),
                ("c".into(), vec![]),
                ("b".into(), vec!["c".into()])
            ]),
            Ok(vec!["c".into(), "b".into(), "a".into()])
        );

        // No deps
        assert_eq!(
            toposort_deps(vec![
                ("no-deps-1".into(), vec![]),
                ("no-deps-2".into(), vec![])
            ]),
            Ok(vec!["no-deps-1".into(), "no-deps-2".into(),])
        );

        // Some deps are not nodes (and thus are ignored)
        assert_eq!(
            toposort_deps(vec![
                ("a".into(), vec!["b".into(), "z".into()]),
                ("b".into(), vec!["x".into()])
            ]),
            Ok(vec!["b".into(), "a".into()])
        );
    }

    #[test]
    fn cycle_detection() {
        // a ---+
        // ^    |
        // |    v
        // +----+
        assert_eq!(
            toposort_deps(vec![("a".into(), vec!["a".into()])]),
            Err(Error::Cycle(vec!["a".into()]))
        );

        // a -> b -> c
        // ^         v
        // |         |
        // +---------+
        assert_eq!(
            toposort_deps(vec![
                ("a".into(), vec!["b".into()]),
                ("b".into(), vec!["c".into()]),
                ("c".into(), vec!["a".into()]),
            ]),
            Err(Error::Cycle(vec!["c".into(), "b".into(), "a".into()]))
        );

        // a -> b <- e
        // |    |    ^
        // v    v    |
        // f    c -> d
        assert_eq!(
            toposort_deps(vec![
                ("a".into(), vec!["b".into()]),
                ("b".into(), vec!["c".into()]),
                ("c".into(), vec!["d".into()]),
                ("d".into(), vec!["e".into()]),
                ("e".into(), vec!["b".into()]),
                ("a".into(), vec!["f".into()]),
            ]),
            Err(Error::Cycle(vec![
                "e".into(),
                "d".into(),
                "c".into(),
                "b".into(),
            ]))
        );
    }
}
