use super::{GleamExpect, Input, Module, ModuleOrigin};
use crate::error::Error;
use petgraph::{algo::Cycle, graph::NodeIndex, Direction};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Default)]
pub struct SourceTree {
    graph: petgraph::Graph<String, ()>,
    indexes: HashMap<String, NodeIndex>,
    modules: HashMap<NodeIndex, Module>,
}

impl SourceTree {
    pub fn new(inputs: Vec<Input>) -> Result<Self, Error> {
        let mut graph: SourceTree = Default::default();
        for input in inputs.into_iter() {
            graph.insert(input)?;
        }
        graph.calculate_dependencies()?;
        Ok(graph)
    }

    pub fn consume(&mut self) -> Result<impl Iterator<Item = Module> + '_, Error> {
        let iter = petgraph::algo::toposort(&self.graph, None)
            .map_err(|e| self.dependency_cycle(e))?
            .into_iter()
            .map(move |i| {
                self.modules
                    .remove(&i)
                    .gleam_expect("SourceTree.consume(): Unknown graph index")
            });
        Ok(iter)
    }

    // TODO: unit tests
    fn dependency_cycle(&mut self, cycle: Cycle<NodeIndex>) -> Error {
        let origin = cycle.node_id();
        let mut path = vec![];
        self.find_cycle(origin, origin, &mut path, &mut HashSet::new());
        let modules: Vec<_> = path
            .iter()
            .map(|index| {
                self.modules
                    .remove(index)
                    .gleam_expect("SourceTree.dependency_cycle(): cannot find module for index")
                    .module
                    .name
            })
            .collect();
        Error::DependencyCycle { modules }
    }

    fn find_cycle(
        &self,
        origin: NodeIndex,
        parent: NodeIndex,
        path: &mut Vec<NodeIndex>,
        seen: &mut HashSet<NodeIndex>,
    ) -> bool {
        seen.insert(parent);
        for node in self.graph.neighbors_directed(parent, Direction::Outgoing) {
            if node == origin {
                path.push(node);
                return true;
            }
            if seen.contains(&node) {
                continue;
            }
            if self.find_cycle(origin, node, path, seen) {
                path.push(node);
                return true;
            }
        }
        false
    }

    fn calculate_dependencies(&mut self) -> Result<(), Error> {
        for module in self.modules.values() {
            let module_name = module.module.name_string();
            let src = module.src.clone();
            let path = module.path.clone();
            let deps = module.module.dependencies();
            let module_index = self.indexes.get(&module_name).gleam_expect(
                "SourceTree.calculate_dependencies(): Unable to find module index for name",
            );
            let module = self.modules.get(&module_index).gleam_expect(
                "SourceTree.calculate_dependencies(): Unable to find module for index",
            );

            for (dep, meta) in deps {
                let dep_index = self.indexes.get(&dep).ok_or_else(|| Error::UnknownImport {
                    module: module_name.clone(),
                    import: dep.clone(),
                    src: src.clone(),
                    path: path.clone(),
                    modules: self
                        .modules
                        .values()
                        .map(|m| m.module.name_string())
                        .collect(),
                    meta: meta.clone(),
                })?;

                if module.origin == ModuleOrigin::Src
                    && self
                        .modules
                        .get(&dep_index)
                        .gleam_expect("SourceTree.calculate_dependencies(): Unable to find module for dep index")
                        .origin
                        == ModuleOrigin::Test
                {
                    return Err(Error::SrcImportingTest {
                        path: path.clone(),
                        src: src.clone(),
                        meta,
                        src_module: module_name,
                        test_module: dep,
                    });
                }

                self.graph
                    .add_edge(dep_index.clone(), module_index.clone(), ());
            }
        }
        Ok(())
    }

    fn insert(&mut self, input: Input) -> Result<(), Error> {
        // Determine the module name
        let name = input
            .path
            .strip_prefix(input.source_base_path.clone())
            .unwrap()
            .parent()
            .unwrap()
            .join(input.path.file_stem().unwrap())
            .to_str()
            .unwrap()
            .to_string()
            .replace("\\", "/");

        // Parse the source
        let mut module = crate::grammar::ModuleParser::new()
            .parse(&crate::parser::strip_extra(&input.src))
            .map_err(|e| Error::Parse {
                path: input.path.clone(),
                src: input.src.clone(),
                error: e.map_token(|crate::grammar::Token(a, b)| (a, b.to_string())),
            })?;

        // Store the name
        module.name = name.split('/').map(|s| s.to_string()).collect();

        // Check to see if we already have a module with this name
        if let Some(Module { path, .. }) = self.indexes.get(&name).and_then(|i| self.modules.get(i))
        {
            return Err(Error::DuplicateModule {
                module: name.clone(),
                first: path.clone(),
                second: input.path,
            });
        }

        // Register the module
        let index = self.graph.add_node(name.clone());
        self.indexes.insert(name, index);
        self.modules.insert(
            index,
            Module {
                src: input.src,
                path: input.path,
                origin: input.origin,
                source_base_path: input.source_base_path,
                module,
            },
        );
        Ok(())
    }
}
