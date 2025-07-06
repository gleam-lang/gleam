use crate::error::wrap;
use ecow::EcoString;
use hexpm::version::Version;
use im::HashSet;
use itertools::Itertools;
use petgraph::Direction;
use petgraph::algo::all_simple_paths;
use petgraph::graph::NodeIndex;
use petgraph::prelude::StableGraph;
use pubgrub::External;
use pubgrub::{DerivationTree, Derived, Ranges};
use std::collections::HashMap;
use std::hash::RandomState;
use std::ops::Bound::{Excluded, Included, Unbounded};
use std::sync::Arc;

macro_rules! wrap_format {
    ($($tts:tt)*) => {
        wrap(&format!($($tts)*))
    }
}

/// Makes a best effort at turning a derivation tree into a nice readable error
/// message.
///
pub struct DerivationTreePrinter {
    derivation_tree: DerivationTree<String, Ranges<Version>, String>,

    /// The name of the root package for which we're trying to add new
    /// dependencies. This is the starting point we use to find and report
    /// dependency conflicts!
    root_package_name: EcoString,

    /// The graph of dependencies built from the derivation tree. The nodes are
    /// packages and the arcs connecting them represent a dependency:
    ///
    /// ```txt
    /// wibble ---- (range1, range2) ---> wobble
    /// ```
    ///
    /// Means "package wibble with version `range1` requires package wobble
    /// with version `range2`".
    ///
    dependencies: StableGraph<String, (Ranges<Version>, Ranges<Version>)>,

    /// A map going from package name to its index in the dependencies graph.
    ///
    nodes: HashMap<String, NodeIndex>,
}

impl DerivationTreePrinter {
    pub fn new(
        root_package_name: EcoString,
        mut derivation_tree: DerivationTree<String, Ranges<Version>, String>,
    ) -> Self {
        // We start by trying to simplify the derivation tree as much as
        // possible.
        derivation_tree.collapse_no_versions();
        simplify_derivation_tree(&mut derivation_tree);

        let mut dependencies = StableGraph::new();
        let mut nodes = HashMap::new();
        build_dependencies_graph(&derivation_tree, &mut dependencies, &mut nodes);

        DerivationTreePrinter {
            root_package_name,
            derivation_tree,
            dependencies,
            nodes,
        }
    }

    pub fn print(&self) -> String {
        self.pretty_explanation()
            .unwrap_or_else(|| self.fallback_explanation())
    }

    /// Tries and print a pretty explanation for the given resolution tree.
    /// If for some reason our heuristic to produce a nice error message fails
    /// we return `None` se we can still produce a good enough error message!
    ///
    fn pretty_explanation(&self) -> Option<String> {
        let root_package_index = self.nodes.get(self.root_package_name.as_str())?;
        let unresolvable_nodes = self.find_unresolvable_nodes();
        if unresolvable_nodes.is_empty() {
            return None;
        }

        let mut unresolvable = vec![];
        for unresolvable_node in unresolvable_nodes {
            let paths = all_simple_paths::<Vec<_>, _, RandomState>(
                &self.dependencies,
                *root_package_index,
                unresolvable_node,
                0,
                None,
            );

            let package = self
                .dependencies
                .node_weight(unresolvable_node)
                .expect("package is in the graph");

            let heading = format!("There's no compatible version of `{package}`:");
            let explanation = paths.sorted().map(|path| self.pretty_path(path)).join("\n");
            unresolvable.push(format!("{heading}\n{explanation}"));
        }
        Some(unresolvable.join("\n\n"))
    }

    fn pretty_path(&self, path: Vec<NodeIndex>) -> String {
        let (you, dependee, rest) = match path.as_slice() {
            [you, dependee, rest @ ..] => (you, dependee, rest),
            _ => panic!("path with less than two nodes"),
        };

        let dependee_name = self
            .dependencies
            .node_weight(*dependee)
            .expect("path node is in the graph");
        let (_, dependee_range) = self
            .ranges_between(you, dependee)
            .expect("path edge is in the graph");

        let mut message = format!(
            "  - You require {dependee_name} {}",
            pretty_range(dependee_range)
        );

        let mut previous = dependee;
        for next in rest {
            let previous_name = self
                .dependencies
                .node_weight(*previous)
                .expect("path node is in the graph");
            let next_name = self
                .dependencies
                .node_weight(*next)
                .expect("path node is in the graph");
            let (_, next_range) = self
                .ranges_between(previous, next)
                .expect("path edge is in the graph");

            message.push_str(&format!(
                "\n    - {previous_name} requires {next_name} {}",
                pretty_range(next_range)
            ));
            previous = next;
        }
        message
    }

    fn find_unresolvable_nodes(&self) -> Vec<NodeIndex> {
        self.dependencies
            .node_indices()
            .filter(|node_index| {
                self.dependencies
                    .neighbors_directed(*node_index, Direction::Incoming)
                    .count()
                    > 1
            })
            .sorted()
            .collect_vec()
    }

    fn ranges_between(
        &self,
        one: &NodeIndex,
        other: &NodeIndex,
    ) -> Option<(&Ranges<Version>, &Ranges<Version>)> {
        let edge = self.dependencies.find_edge(*one, *other)?;
        self.dependencies
            .edge_weight(edge)
            .map(|(one, other)| (one, other))
    }

    /// A good enough explanation in case we're not able to produce anything
    /// nicer.
    fn fallback_explanation(&self) -> String {
        let mut conflicting_packages = HashSet::new();
        collect_conflicting_packages(&self.derivation_tree, &mut conflicting_packages);

        wrap_format!(
            "Unable to find compatible versions for \
the version constraints in your gleam.toml. \
The conflicting packages are:

{}
",
            conflicting_packages
                .into_iter()
                .map(|s| format!("- {s}"))
                .join("\n")
        )
    }
}

fn build_dependencies_graph(
    derivation_tree: &DerivationTree<String, Ranges<Version>, String>,
    graph: &mut StableGraph<String, (Ranges<Version>, Ranges<Version>)>,
    nodes: &mut HashMap<String, NodeIndex<u32>>,
) {
    match derivation_tree {
        DerivationTree::External(External::FromDependencyOf(
            one,
            range_one,
            other,
            range_other,
        )) => {
            let one_index = match nodes.get(one) {
                Some(index) => *index,
                None => {
                    let index = graph.add_node(one.clone());
                    let _ = nodes.insert(one.clone(), index);
                    index
                }
            };

            let other_index = match nodes.get(other) {
                Some(index) => *index,
                None => {
                    let index = graph.add_node(other.clone());
                    let _ = nodes.insert(other.clone(), index);
                    index
                }
            };

            let edges = graph.edges_connecting(one_index, other_index);
            let edge_weight = match edges.peekable().peek() {
                Some(edge) => {
                    let (old_range_one, old_range_other) = edge.weight();
                    (
                        range_one.union(old_range_one),
                        range_other.union(old_range_other),
                    )
                }
                None => (range_one.clone(), range_other.clone()),
            };

            let _ = graph.update_edge(one_index, other_index, edge_weight);
        }
        DerivationTree::External(_) => (),
        DerivationTree::Derived(Derived { cause1, cause2, .. }) => {
            build_dependencies_graph(cause1, graph, nodes);
            build_dependencies_graph(cause2, graph, nodes);
        }
    }
}

/// This function collapses adjacent levels of a derivation tree that are all
/// relative to the same dependency.
///
/// By default a derivation tree might have many nodes for a specific package,
/// each node referring to a specific version range. For example:
///
///   - package_wibble `>= 1.0.0 and < 1.1.0` requires package_wobble `>= 1.1.0`
///   - package_wibble `>= 1.1.0 and < 1.2.0` requires package_wobble `>= 1.2.0`
///   - package_wibble `1.1.0` requires package_wobble `>= 1.1.0`
///
/// This level of fine-grained detail would be quite overwhelming in the vast
/// majority of cases so we're fine with collapsing all these details into a
/// single node taking the union of all the ranges that are there:
///
///   - package_wibble `>= 1.0.0 and < 1.2.0` requires package_wobble `>= 1.1.0`
///
/// This way we can print an error message that is way more concise and still
/// informative about what went wrong, at the cost of
///
fn simplify_derivation_tree(derivation_tree: &mut DerivationTree<String, Ranges<Version>, String>) {
    match derivation_tree {
        DerivationTree::External(_) => {}
        DerivationTree::Derived(derived) => {
            simplify_derivation_tree(Arc::make_mut(&mut derived.cause1));
            simplify_derivation_tree(Arc::make_mut(&mut derived.cause2));
            simplify_derivation_tree_outer(derivation_tree);
        }
    }
}

fn simplify_derivation_tree_outer(
    derivation_tree: &mut DerivationTree<String, Ranges<Version>, String>,
) {
    match derivation_tree {
        DerivationTree::External(_) => {}
        DerivationTree::Derived(derived) => {
            match (
                Arc::make_mut(&mut derived.cause1),
                Arc::make_mut(&mut derived.cause2),
            ) {
                (
                    DerivationTree::External(External::FromDependencyOf(
                        package,
                        package_range,
                        required_package,
                        required_package_range,
                    )),
                    DerivationTree::External(External::FromDependencyOf(
                        maybe_package,
                        other_package_range,
                        maybe_required_package,
                        other_required_package_range,
                    )),
                ) if package == maybe_package && required_package == maybe_required_package => {
                    *derivation_tree = DerivationTree::External(External::FromDependencyOf(
                        package.clone(),
                        package_range.union(other_package_range),
                        required_package.clone(),
                        required_package_range.union(other_required_package_range),
                    ))
                }

                _ => {}
            }
        }
    }
}

fn collect_conflicting_packages<'dt>(
    derivation_tree: &'dt DerivationTree<String, Ranges<Version>, String>,
    conflicting_packages: &mut HashSet<&'dt String>,
) {
    match derivation_tree {
        DerivationTree::External(external) => match external {
            External::NotRoot(package, _)
            | External::NoVersions(package, _)
            | External::Custom(package, _, _) => {
                let _ = conflicting_packages.insert(package);
            }
            External::FromDependencyOf(package, _, dep_package, _) => {
                let _ = conflicting_packages.insert(package);
                let _ = conflicting_packages.insert(dep_package);
            }
        },
        DerivationTree::Derived(derived) => {
            collect_conflicting_packages(&derived.cause1, conflicting_packages);
            collect_conflicting_packages(&derived.cause2, conflicting_packages);
        }
    }
}

fn pretty_range(range: &Ranges<Version>) -> String {
    range
        .iter()
        .map(|(lower, upper)| match (lower, upper) {
            (Included(lower), Included(upper)) if lower == upper => format!("{lower}"),
            (Included(lower), Included(upper)) => format!(">= {lower} and <= {upper}"),
            (Included(lower), Excluded(upper)) => format!(">= {lower} and < {upper}"),
            (Excluded(lower), Included(upper)) => format!("> {lower} and <= {upper}"),
            (Excluded(lower), Excluded(upper)) => format!("> {lower} and < {upper}"),

            (Included(version), Unbounded) => format!(">= {version}"),
            (Excluded(version), Unbounded) => format!("> {version}"),
            (Unbounded, Included(version)) => format!("<= {version}"),
            (Unbounded, Excluded(version)) => format!("< {version}"),

            (Unbounded, Unbounded) => "".into(),
        })
        .join(" or ")
}
