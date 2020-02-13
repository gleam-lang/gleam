#[cfg(test)]
mod tests;

use crate::error::{Error, FileIOAction, FileKind};
use crate::typ;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, PartialEq)]
pub struct Input {
    pub source_base_path: PathBuf,
    pub path: PathBuf,
    pub src: String,
    pub origin: ModuleOrigin,
}

#[derive(Debug, PartialEq)]
pub struct Compiled {
    pub name: Vec<String>,
    pub origin: ModuleOrigin,
    pub files: Vec<OutputFile>,
    pub type_info: typ::Module,
}

#[derive(Debug, PartialEq)]
pub struct OutputFile {
    pub text: String,
    pub path: PathBuf,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ModuleOrigin {
    Src,
    Test,
    Dependency,
}

impl ModuleOrigin {
    pub fn dir_name(&self) -> &'static str {
        match self {
            ModuleOrigin::Src | ModuleOrigin::Dependency => "src",
            ModuleOrigin::Test => "test",
        }
    }
}

#[derive(Debug)]
struct Module {
    src: String,
    path: PathBuf,
    source_base_path: PathBuf,
    origin: ModuleOrigin,
    module: crate::ast::UntypedModule,
}

#[derive(Debug, Default)]
struct SourceTree {
    graph: petgraph::Graph<String, ()>,
    indexes: HashMap<String, petgraph::graph::NodeIndex>,
    modules: HashMap<petgraph::graph::NodeIndex, Module>,
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
            .map_err(|_| Error::DependencyCycle)?
            .into_iter()
            .map(move |i| self.modules.remove(&i).expect("Unknown graph index"));
        Ok(iter)
    }

    fn calculate_dependencies(&mut self) -> Result<(), Error> {
        for module in self.modules.values() {
            let module_name = module.module.name_string();
            let src = module.src.clone();
            let path = module.path.clone();
            let deps = module.module.dependencies();
            let module_index = self
                .indexes
                .get(&module_name)
                .expect("Unable to find module index");
            let module = self
                .modules
                .get(&module_index)
                .expect("Unable to find module for index");

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
                        .expect("Unable to find module for dep index")
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
        self.indexes.insert(name.clone(), index);
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

pub fn compile(inputs: Vec<Input>) -> Result<Vec<Compiled>, Error> {
    let module_count = inputs.len();
    let mut source_tree = SourceTree::new(inputs)?;
    let mut modules_type_infos = HashMap::new();
    let mut compiled_modules = Vec::with_capacity(module_count);

    struct Out {
        name_string: String,
        name: Vec<String>,
        origin: ModuleOrigin,
        files: Vec<OutputFile>,
    }

    for Module {
        src,
        path,
        module,
        origin,
        source_base_path,
    } in source_tree.consume()?
    {
        let name = module.name.clone();
        let name_string = module.name_string();

        println!("Compiling {}", name_string);

        let module = crate::typ::infer_module(module, &modules_type_infos)
            .map_err(|error| Error::Type { path, src, error })?;

        modules_type_infos.insert(name_string.clone(), module.type_info.clone());

        let gen_dir = source_base_path
            .parent()
            .unwrap()
            .join("gen")
            .join(origin.dir_name());
        let erl_module_name = module.name.join("@");

        let mut files: Vec<_> = crate::erl::records(&module)
            .into_iter()
            .map(|(name, text)| OutputFile {
                path: gen_dir.join(format!("{}_{}.hrl", erl_module_name, name)),
                text,
            })
            .collect();

        files.push(OutputFile {
            path: gen_dir.join(format!("{}.erl", erl_module_name)),
            text: crate::erl::module(module),
        });

        compiled_modules.push(Out {
            name,
            name_string,
            origin,
            files,
        });
    }

    Ok(compiled_modules
        .into_iter()
        .map(
            |Out {
                 name,
                 name_string,
                 origin,
                 files,
             }| Compiled {
                name,
                files,
                origin,
                type_info: modules_type_infos
                    .remove(&name_string)
                    .expect("merging module type info"),
            },
        )
        .collect())
}

pub fn collect_source(
    src_dir: PathBuf,
    origin: ModuleOrigin,
    srcs: &mut Vec<Input>,
) -> Result<(), Error> {
    let src_dir = match src_dir.canonicalize() {
        Ok(d) => d,
        Err(_) => return Ok(()),
    };
    let is_gleam_path = |e: &walkdir::DirEntry| {
        use regex::Regex;
        lazy_static! {
            static ref RE: Regex =
                Regex::new("^([a-z_]+(/|\\\\))*[a-z_]+\\.gleam$").expect("collect_source RE regex");
        }

        RE.is_match(
            e.path()
                .strip_prefix(&*src_dir)
                .expect("collect_source strip_prefix")
                .to_str()
                .unwrap_or(""),
        )
    };

    for dir_entry in walkdir::WalkDir::new(src_dir.clone())
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .filter(is_gleam_path)
    {
        let src = std::fs::read_to_string(dir_entry.path()).map_err(|err| Error::FileIO {
            action: FileIOAction::Read,
            kind: FileKind::File,
            path: dir_entry.path().to_path_buf(),
            err: Some(err.to_string()),
        })?;

        srcs.push(Input {
            path: dir_entry
                .path()
                .canonicalize()
                .expect("collect_source path canonicalize"),
            source_base_path: src_dir.clone(),
            origin: origin.clone(),
            src,
        })
    }
    Ok(())
}
