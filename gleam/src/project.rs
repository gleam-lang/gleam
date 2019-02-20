use petgraph::Graph;
use std::collections::HashMap;

pub type Name = String;
pub type Src = String;

#[derive(Debug, PartialEq)]
pub struct Compiled {
    pub name: String,
    pub out: String,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    Parse {
        name: Name,
        src: Src,
        error: lalrpop_util::ParseError<usize, (usize, String), String>,
    },

    Type {
        name: Name,
        src: Src,
        error: crate::typ::Error,
    },

    DependencyCycle,
}

pub fn compile(srcs: Vec<(Name, Src)>) -> Result<Vec<Compiled>, Error> {
    let mut deps_graph = Graph::new();
    let mut deps_vec = Vec::with_capacity(srcs.len());
    let mut indexes = HashMap::new();
    let mut modules = HashMap::new();

    for (name, src) in srcs {
        let mut module = crate::grammar::ModuleParser::new()
            .parse(&crate::parser::strip_extra(&src))
            .map_err(|e| Error::Parse {
                name: name.clone(),
                src: src.clone(),
                error: e
                    .map_error(|s| s.to_string())
                    .map_token(|crate::grammar::Token(a, b)| (a, b.to_string())),
            })?;

        module.name = name.clone();

        let index = deps_graph.add_node(name.clone());
        deps_vec.push((name.clone(), module.dependancies()));
        indexes.insert(name.clone(), index.clone());
        modules.insert(index, (src, module));
    }

    // Register each module's deps so that we can determine a correct order to compile the modules.
    for (module, deps) in deps_vec {
        let module_index = indexes.get(&module).expect("Unable to find module index");
        for dep in deps {
            let dep_index = indexes.get(&dep).expect("Unable to find module index");
            deps_graph.add_edge(dep_index.clone(), module_index.clone(), ());
        }
    }

    let mut module_types = HashMap::new();

    petgraph::algo::toposort(&deps_graph, None)
        .map_err(|_| Error::DependencyCycle)?
        .into_iter()
        .map(|i| {
            let (src, module) = modules.remove(&i).expect("Unknown graph index");
            let name = module.name.clone();

            println!("Compiling {}", name);

            let module =
                crate::typ::infer_module(module, &module_types).map_err(|error| Error::Type {
                    name: name.clone(),
                    src,
                    error,
                })?;

            module_types.insert(name.clone(), module.typ.clone());

            let out = crate::erl::module(module);

            Ok(Compiled { name, out })
        })
        .collect()
}

#[test]
fn compile_test() {
    struct Case {
        input: Vec<(Name, Src)>,
        expected: Result<Vec<Compiled>, Error>,
    }

    let cases = vec![
        Case {
            input: vec![],
            expected: Ok(vec![]),
        },
        Case {
            input: vec![
                ("one".to_string(), "".to_string()),
                ("two".to_string(), "".to_string()),
            ],
            expected: Ok(vec![
                Compiled {
                    name: "two".to_string(),
                    out: "-module(gleam_two).\n\n-export([]).\n\n\n".to_string(),
                },
                Compiled {
                    name: "one".to_string(),
                    out: "-module(gleam_one).\n\n-export([]).\n\n\n".to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                ("one".to_string(), "import two".to_string()),
                ("two".to_string(), "".to_string()),
            ],
            expected: Ok(vec![
                Compiled {
                    name: "two".to_string(),
                    out: "-module(gleam_two).\n\n-export([]).\n\n\n".to_string(),
                },
                Compiled {
                    name: "one".to_string(),
                    out: "-module(gleam_one).\n\n-export([]).\n\n\n".to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                ("one".to_string(), "".to_string()),
                ("two".to_string(), "import one".to_string()),
            ],
            expected: Ok(vec![
                Compiled {
                    name: "one".to_string(),
                    out: "-module(gleam_one).\n\n-export([]).\n\n\n".to_string(),
                },
                Compiled {
                    name: "two".to_string(),
                    out: "-module(gleam_two).\n\n-export([]).\n\n\n".to_string(),
                },
            ]),
        },
    ];

    for Case { input, expected } in cases.into_iter() {
        assert_eq!(expected, compile(input));
    }
}
