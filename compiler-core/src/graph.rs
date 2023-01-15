//! General functions for working with graphs.

use std::collections::HashSet;

use petgraph::{prelude::NodeIndex, stable_graph::StableGraph, Direction};

/// Sort a graph into a sequence from the leaves to the roots.
///
/// Nodes are returned in their smallest possible groups, which is either a leaf
/// or a cycle.
///
/// This function is implemented using `pop_leaf_or_cycle`.
///
pub fn into_dependency_order<N, E>(mut graph: StableGraph<N, E>) -> Vec<Vec<NodeIndex>> {
    let mut items = vec![];

    loop {
        let current = pop_leaf_or_cycle(&mut graph);
        if current.is_empty() {
            return items;
        } else {
            items.push(current);
        }
    }
}

/// The same as `leaf_or_cycle` but removes the nodes from the graph.
/// See the docs there for more details.
///
pub fn pop_leaf_or_cycle<N, E>(graph: &mut StableGraph<N, E>) -> Vec<NodeIndex> {
    let nodes = leaf_or_cycle(graph);
    for node in &nodes {
        _ = graph.remove_node(*node);
    }
    nodes
}

/// Return a leaf from the graph. If there are no leaves then a cycle is
/// returned instead.
///
/// The nodes returned are not removed from the graph.
///
/// If there are no leaves or cycles then an empty vector is returned.
///
pub fn leaf_or_cycle<N, E>(graph: &StableGraph<N, E>) -> Vec<NodeIndex> {
    if graph.node_count() == 0 {
        return vec![];
    }

    // Find a leaf, returning one if found.
    for node in graph.node_indices() {
        if graph.neighbors_directed(node, Direction::Outgoing).count() == 0 {
            return vec![node];
        }
    }

    // No leaves were found, so find a cycle.
    // We use a toposort to find the start of the cycle.
    let start = petgraph::algo::toposort(&graph, None)
        .expect_err("Non-empty graph has no leaves or cycles")
        .node_id();

    // Then traverse the graph to find nodes in the cycle
    enum Step {
        Backtrack,
        Next(NodeIndex),
    }
    let mut path = vec![];
    let mut stack = vec![Step::Next(start)];
    let mut seen = HashSet::new();

    while let Some(step) = stack.pop() {
        let node = match step {
            // We have processed all the nodes in the branch so backtrack,
            // popping the node off the path.
            Step::Backtrack => {
                _ = path.pop();
                continue;
            }
            Step::Next(node) => node,
        };

        // Record the point at which we need to backtrack in order to go back up
        // the tree.
        stack.push(Step::Backtrack);

        if !seen.insert(node) {
            continue;
        }

        // Check each child & add them to the stack if they are not the target.
        for neighbour in graph.neighbors_directed(node, Direction::Outgoing) {
            path.push(neighbour);

            if neighbour == start {
                return path;
            }

            stack.push(Step::Next(neighbour));
        }
    }

    unreachable!("Could not find cycle for toposort returned start node")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn leaf_or_cycle_empty() {
        let mut graph: StableGraph<(), ()> = StableGraph::new();
        assert!(pop_leaf_or_cycle(&mut graph).is_empty());
    }

    #[test]
    fn leaf_or_cycle_1() {
        let mut graph: StableGraph<(), ()> = StableGraph::new();
        let a = graph.add_node(());
        assert_eq!(
            [pop_leaf_or_cycle(&mut graph), pop_leaf_or_cycle(&mut graph),],
            [vec![a], vec![]]
        );
    }

    #[test]
    fn leaf_or_cycle_2() {
        let mut graph: StableGraph<(), ()> = StableGraph::new();
        let a = graph.add_node(());
        let b = graph.add_node(());

        assert_eq!(
            [
                pop_leaf_or_cycle(&mut graph),
                pop_leaf_or_cycle(&mut graph),
                pop_leaf_or_cycle(&mut graph),
            ],
            [vec![a], vec![b], vec![]]
        );
    }

    #[test]
    fn leaf_or_cycle_3() {
        let mut graph: StableGraph<(), ()> = StableGraph::new();
        // Here a depends on b so b must come before a
        let a = graph.add_node(());
        let b = graph.add_node(());
        let c = graph.add_node(());
        _ = graph.add_edge(a, b, ());

        assert_eq!(
            [
                pop_leaf_or_cycle(&mut graph),
                pop_leaf_or_cycle(&mut graph),
                pop_leaf_or_cycle(&mut graph),
                pop_leaf_or_cycle(&mut graph),
            ],
            [vec![b], vec![a], vec![c], vec![]]
        );
    }

    #[test]
    fn leaf_or_cycle_4() {
        let mut graph: StableGraph<(), ()> = StableGraph::new();
        let a = graph.add_node(());
        let b = graph.add_node(());
        let c = graph.add_node(());
        _ = graph.add_edge(a, b, ());
        _ = graph.add_edge(a, c, ());

        assert_eq!(
            [
                pop_leaf_or_cycle(&mut graph),
                pop_leaf_or_cycle(&mut graph),
                pop_leaf_or_cycle(&mut graph),
                pop_leaf_or_cycle(&mut graph),
            ],
            [vec![b], vec![c], vec![a], vec![]]
        );
    }

    #[test]
    fn leaf_or_cycle_5() {
        let mut graph: StableGraph<(), ()> = StableGraph::new();
        let a = graph.add_node(());
        let b = graph.add_node(());
        let c = graph.add_node(());
        _ = graph.add_edge(a, b, ());
        _ = graph.add_edge(b, a, ());

        assert_eq!(
            [
                pop_leaf_or_cycle(&mut graph),
                pop_leaf_or_cycle(&mut graph),
                pop_leaf_or_cycle(&mut graph),
            ],
            [vec![c], vec![a, b], vec![]]
        );
    }

    #[test]
    fn leaf_or_cycle_6() {
        let mut graph: StableGraph<(), ()> = StableGraph::new();
        let a = graph.add_node(());
        let b = graph.add_node(());
        let c = graph.add_node(());
        let d = graph.add_node(());
        _ = graph.add_edge(a, b, ());
        _ = graph.add_edge(b, c, ());
        _ = graph.add_edge(c, a, ());
        _ = graph.add_edge(d, a, ());

        assert_eq!(
            [
                pop_leaf_or_cycle(&mut graph),
                pop_leaf_or_cycle(&mut graph),
                pop_leaf_or_cycle(&mut graph),
            ],
            [vec![a, b, c], vec![d], vec![]]
        );
    }
}
