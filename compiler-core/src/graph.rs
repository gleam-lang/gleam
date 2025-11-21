//! General functions for working with graphs.

use petgraph::{Direction, prelude::NodeIndex, stable_graph::StableGraph};

/// Sort a graph into a sequence from the leaves to the roots.
///
/// Nodes are returned in their smallest possible groups, which is either a leaf
/// or a cycle.
///
/// This function is implemented using `pop_leaf_or_cycle`.
///
pub fn into_dependency_order<N, E>(mut graph: StableGraph<N, E>) -> Vec<Vec<NodeIndex>> {
    let mut items = vec![];

    // Remove all self-edges from the graph.
    graph.retain_edges(|graph, edge| match graph.edge_endpoints(edge) {
        Some((a, b)) => a != b,
        None => false,
    });

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
/// # Panics
///
/// Panics if the graph contains a self-edge.
///
fn pop_leaf_or_cycle<N, E>(graph: &mut StableGraph<N, E>) -> Vec<NodeIndex> {
    let nodes = leaf_or_cycle(graph);
    for node in &nodes {
        _ = graph.remove_node(*node);
    }
    nodes
}

/// Return a leaf from the graph. If there are no leaves then the largest cycle
/// is returned instead.
///
/// If there are no leaves or cycles then an empty vector is returned.
///
/// The nodes returned are not removed from the graph.
///
/// # Panics
///
/// Panics if the graph contains a self-edge.
///
fn leaf_or_cycle<N, E>(graph: &StableGraph<N, E>) -> Vec<NodeIndex> {
    if graph.node_count() == 0 {
        return vec![];
    }

    // Find a leaf, returning one if found.
    for node in graph.node_indices() {
        let mut outgoing = graph.neighbors_directed(node, Direction::Outgoing);
        let referenced = outgoing.next();

        if referenced == Some(node) {
            panic!("Self edge found in graph");
        }

        // This is a leaf.
        if referenced.is_none() {
            return vec![node];
        }
    }

    // No leaves were found, so find a cycle.
    // We use a toposort to find the start of the cycle.
    let start = petgraph::algo::toposort(&graph, None)
        .expect_err("Non-empty graph has no leaves or cycles")
        .node_id();

    // Then traverse the graph to find nodes in the cycle.
    // This traverses all possible paths to find a cycle, this can likely be
    // optimised. There's not a large number of functions in a module however so
    // this is tolerable in this specific instance.
    #[derive(Debug)]
    enum Step {
        Backtrack,
        Next(NodeIndex),
    }
    let mut path = vec![];
    let mut stack = vec![Step::Next(start)];
    let mut cycles = vec![];

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

        if path.contains(&node) {
            continue;
        }

        // Add this node to the path and record the point at which we need to
        // backtrack in order to go back up the tree.
        stack.push(Step::Backtrack);
        path.push(node);

        // Check each child & add them to the stack if they are not the target.
        for node in graph.neighbors_directed(node, Direction::Outgoing) {
            if node == start {
                cycles.push(path.clone());
            } else {
                stack.push(Step::Next(node));
            }
        }
    }

    cycles
        .into_iter()
        .max_by_key(|x| x.len())
        .expect("Could not find cycle for toposort returned start node")
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
        assert_eq!(into_dependency_order(graph), vec![vec![a]]);
    }

    #[test]
    fn leaf_or_cycle_2() {
        let mut graph: StableGraph<(), ()> = StableGraph::new();
        let a = graph.add_node(());
        let b = graph.add_node(());

        assert_eq!(into_dependency_order(graph), vec![vec![a], vec![b]]);
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
            into_dependency_order(graph),
            vec![vec![b], vec![a], vec![c]]
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
            into_dependency_order(graph),
            vec![vec![b], vec![c], vec![a]]
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

        assert_eq!(into_dependency_order(graph), vec![vec![c], vec![b, a]]);
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

        assert_eq!(into_dependency_order(graph), vec![vec![c, a, b], vec![d]]);
    }

    #[test]
    fn leaf_or_cycle_7() {
        let mut graph: StableGraph<(), ()> = StableGraph::new();
        let a = graph.add_node(());
        let b = graph.add_node(());
        _ = graph.add_edge(a, a, ());
        _ = graph.add_edge(a, b, ());
        _ = graph.add_edge(b, b, ());

        // Here there are no true leafs, only cycles. However, b is in a loop
        // with itself so counts as a leaf as far as we are concerned.

        assert_eq!(into_dependency_order(graph), vec![vec![b], vec![a]]);
    }

    #[test]
    fn leaf_or_cycle_8() {
        let mut graph: StableGraph<(), ()> = StableGraph::new();
        let a = graph.add_node(());
        let b = graph.add_node(());
        _ = graph.add_edge(a, a, ());
        _ = graph.add_edge(a, b, ());
        _ = graph.add_edge(b, b, ());
        _ = graph.add_edge(b, b, ());
        _ = graph.add_edge(b, b, ());

        // Here there are no true leafs, only cycles. However, b is in a loop
        // with itself so counts as a leaf as far as we are concerned.
        // This is different from the previous test as there are multiple self
        // references for node b.

        assert_eq!(into_dependency_order(graph), vec![vec![b], vec![a]]);
    }

    #[test]
    fn leaf_or_cycle_9() {
        let mut graph: StableGraph<(), ()> = StableGraph::new();
        let a = graph.add_node(());
        let b = graph.add_node(());
        let c = graph.add_node(());

        _ = graph.add_edge(a, a, ());
        _ = graph.add_edge(a, b, ());

        _ = graph.add_edge(b, b, ());
        _ = graph.add_edge(b, c, ());

        _ = graph.add_edge(c, b, ());
        _ = graph.add_edge(c, c, ());

        // Here there are no true leafs, only cycles. However, b is in a loop
        // with itself so counts as a leaf as far as we are concerned.
        // This is different from the previous test as there are multiple self
        // references for node b.

        assert_eq!(into_dependency_order(graph), vec![vec![c, b], vec![a]]);
    }
}
