use crate::error::Error;
use crate::typ::ModuleTypeInfo;
use petgraph::Graph;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, PartialEq)]
pub struct Input {
    pub base_path: PathBuf,
    pub path: PathBuf,
    pub src: String,
    pub origin: ModuleOrigin,
}

#[derive(Debug, PartialEq)]
pub struct Compiled {
    pub name: Vec<String>,
    pub code: String,
    pub path: PathBuf,
    pub origin: ModuleOrigin,
    pub type_info: ModuleTypeInfo,
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

pub fn compile(srcs: Vec<Input>) -> Result<Vec<Compiled>, Error> {
    struct Module {
        src: String,
        path: PathBuf,
        base_path: PathBuf,
        origin: ModuleOrigin,
        module: crate::ast::UntypedModule,
    }
    let module_count = srcs.len();
    let mut deps_graph = Graph::new();
    let mut indexes = HashMap::new();
    let mut modules: HashMap<_, Module> = HashMap::new();

    for Input {
        base_path,
        path,
        src,
        origin,
    } in srcs
    {
        let name = path
            .strip_prefix(base_path.clone())
            .unwrap()
            .parent()
            .unwrap()
            .join(path.file_stem().unwrap())
            .to_str()
            .unwrap()
            .to_string();
        let mut module = crate::grammar::ModuleParser::new()
            .parse(&crate::parser::strip_extra(&src))
            .map_err(|e| Error::Parse {
                path: path.clone(),
                src: src.clone(),
                error: e.map_token(|crate::grammar::Token(a, b)| (a, b.to_string())),
            })?;

        if let Some(Module {
            path: first_path, ..
        }) = indexes.get(&name).and_then(|i| modules.get(i))
        {
            return Err(Error::DuplicateModule {
                module: name,
                first: first_path.clone(),
                second: path,
            });
        }

        module.name = name.split('/').map(|s| s.to_string()).collect();

        let index = deps_graph.add_node(name.clone());
        indexes.insert(name.clone(), index);
        modules.insert(
            index,
            Module {
                src,
                path,
                module,
                origin,
                base_path,
            },
        );
    }

    // Register each module's deps so that we can determine a correct order to compile the modules.
    for module in modules.values() {
        let module_name = module.module.name_string();
        let src = module.src.clone();
        let path = module.path.clone();
        let deps = module.module.dependencies();
        let module_index = indexes
            .get(&module_name)
            .expect("Unable to find module index");
        let module = modules
            .get(&module_index)
            .expect("Unable to find module for index");

        for (dep, meta) in deps {
            let dep_index = indexes.get(&dep).ok_or_else(|| Error::UnknownImport {
                module: module_name.clone(),
                import: dep.clone(),
                src: src.clone(),
                path: path.clone(),
                modules: modules.values().map(|m| m.module.name_string()).collect(),
                meta: meta.clone(),
            })?;

            if module.origin == ModuleOrigin::Src
                && modules
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

            deps_graph.add_edge(dep_index.clone(), module_index.clone(), ());
        }
    }

    let mut modules_type_infos = HashMap::new();
    let mut compiled_modules = Vec::with_capacity(module_count);

    for i in petgraph::algo::toposort(&deps_graph, None)
        .map_err(|_| Error::DependencyCycle)?
        .into_iter()
    {
        let Module {
            src,
            path,
            module,
            origin,
            base_path,
        } = modules.remove(&i).expect("Unknown graph index");
        let name = module.name.clone();
        let name_string = module.name_string();

        println!("Compiling {}", name_string);

        let module = crate::typ::infer_module(module, &modules_type_infos)
            .map_err(|error| Error::Type { path, src, error })?;

        modules_type_infos.insert(name_string.clone(), module.type_info.clone());

        let path = base_path
            .parent()
            .unwrap()
            .join("gen")
            .join(origin.dir_name())
            .join(format!("{}.erl", module.name.join("@")));
        let code = crate::erl::module(module);

        compiled_modules.push((name, name_string, code, path, origin));
    }

    Ok(compiled_modules
        .into_iter()
        .map(|(name, name_string, code, path, origin)| Compiled {
            name,
            code,
            path,
            origin,
            type_info: modules_type_infos
                .remove(&name_string)
                .expect("merging module type info"),
        })
        .collect())
}

pub fn collect_source(src_dir: PathBuf, origin: ModuleOrigin, srcs: &mut Vec<Input>) {
    let src_dir = match src_dir.canonicalize() {
        Ok(d) => d,
        Err(_) => return,
    };
    let is_gleam_path = |e: &walkdir::DirEntry| {
        use regex::Regex;
        lazy_static! {
            static ref RE: Regex =
                Regex::new("^([a-z_]+/)*[a-z_]+\\.gleam$").expect("collect_source RE regex");
        }

        RE.is_match(
            e.path()
                .strip_prefix(&*src_dir)
                .expect("collect_source strip_prefix")
                .to_str()
                .unwrap_or(""),
        )
    };

    walkdir::WalkDir::new(src_dir.clone())
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .filter(is_gleam_path)
        .for_each(|dir_entry| {
            let src = std::fs::read_to_string(dir_entry.path())
                .unwrap_or_else(|_| panic!("Unable to read {:?}", dir_entry.path()));

            srcs.push(Input {
                path: dir_entry
                    .path()
                    .canonicalize()
                    .expect("collect_source path canonicalize"),
                base_path: src_dir.clone(),
                origin: origin.clone(),
                src,
            })
        });
}

#[test]
fn compile_test() {
    struct Case {
        input: Vec<Input>,
        expected: Result<Vec<Output>, Error>,
    }
    #[derive(Debug, PartialEq)]
    struct Output {
        name: Vec<String>,
        code: String,
        path: PathBuf,
        origin: ModuleOrigin,
    }

    let cases = vec![
        Case {
            input: vec![],
            expected: Ok(vec![]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    base_path: PathBuf::from("/src"),
                    path: PathBuf::from("/src/one.gleam"),
                    src: "".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    base_path: PathBuf::from("/src"),
                    path: PathBuf::from("/src/two.gleam"),
                    src: "".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
            ]),
        },
        Case {
            input: vec![Input {
                origin: ModuleOrigin::Test,
                base_path: PathBuf::from("/test"),
                path: PathBuf::from("/test/one.gleam"),
                src: "".to_string(),
            }],
            expected: Ok(vec![Output {
                    origin: ModuleOrigin::Test,
                name: vec!["one".to_string()],
                path: PathBuf::from("/gen/test/one.erl"),
                code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            }]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Test,
                    base_path: PathBuf::from("/test"),
                    path: PathBuf::from("/test/two.gleam"),
                    src: "".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    base_path: PathBuf::from("/src"),
                    path: PathBuf::from("/src/one.gleam"),
                    src: "import two".to_string(),
                },
            ],
            expected: Err(Error::SrcImportingTest {
                path: PathBuf::from("/src/one.gleam"),
                src: "import two".to_string(),
                meta: crate::ast::Meta { start: 7, end: 10 },
                src_module: "one".to_string(),
                test_module: "two".to_string(),
            }),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import two".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import one".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "pub enum Box { Box(Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import one pub fn unbox(x) { let one.Box(i) = x i }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\n-export([unbox/1]).\n
unbox(X) ->\n    {box, I} = X,\n    I.\n"
                        .to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Dependency,
                    path: PathBuf::from("/src/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "pub enum Box { Box(Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Dependency,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import one pub fn box(x) { one.Box(x) }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Dependency,
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Output {
                    origin: ModuleOrigin::Dependency,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\n-export([box/1]).\n
box(X) ->\n    {box, X}.\n"
                        .to_string(),
                },
            ]),
        },
        Case {
            input: vec![Input {
                origin: ModuleOrigin::Src,
                path: PathBuf::from("/src/one/two.gleam"),
                base_path: PathBuf::from("/src"),
                src: "pub enum Box { Box }".to_string(),
            }],
            expected: Ok(vec![Output {
                    origin: ModuleOrigin::Src,
                name: vec!["one".to_string(), "two".to_string()],
                path: PathBuf::from("/gen/src/one@two.erl"),
                code: "-module(one@two).\n-compile(no_auto_import).\n\n\n".to_string(),
            }]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "pub enum Box { Box }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import one pub fn box() { one.Box }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\n-export([box/0]).\n
box() ->\n    box.\n"
                        .to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "pub fn go() { 1 }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import one as thingy       pub fn call() { thingy.go() }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n-export([go/0]).\n
go() ->
    1.\n"
                        .to_string(),
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\n-export([call/0]).\n
call() ->
    one:go().\n"
                        .to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/nested/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "pub enum Box { Box(Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import nested/one\npub fn go(x) { let one.Box(y) = x y }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["nested".to_string(), "one".to_string()],
                    path: PathBuf::from("/gen/src/nested@one.erl"),
                    code: "-module(nested@one).\n-compile(no_auto_import).\n\n\n"
                        .to_string(),
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\n-export([go/1]).\n\ngo(X) ->\n    {box, Y} = X,\n    Y.\n"
                        .to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/nested/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "pub enum Box { Box(Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import nested/one as thingy\npub fn go(x) { let thingy.Box(y) = x y }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["nested".to_string(), "one".to_string()],
                    path: PathBuf::from("/gen/src/nested@one.erl"),
                    code: "-module(nested@one).\n-compile(no_auto_import).\n\n\n"
                        .to_string(),
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\n-export([go/1]).\n\ngo(X) ->\n    {box, Y} = X,\n    Y.\n"
                        .to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/nested/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "pub external type Thing pub fn go() { 1 }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import nested/one
                        pub fn go() { one.go() }
                        pub external fn thing() -> one.Thing = \"thing\" \"new\""
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["nested".to_string(), "one".to_string()],
                    path: PathBuf::from("/gen/src/nested@one.erl"),
                    code: "-module(nested@one).\n-compile(no_auto_import).\n\n-export([go/0]).\n
go() ->\n    1.\n"
                        .to_string(),
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\n-export([go/0, thing/0]).\n
go() ->\n    nested@one:go().\n
thing() ->\n    thing:new().\n"
                        .to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/other/src/one.gleam"),
                    base_path: PathBuf::from("/other/src"),
                    src: "".to_string(),
                },
            ],
            expected: Err(Error::DuplicateModule {
                module: "one".to_string(),
                first: PathBuf::from("/src/one.gleam"),
                second: PathBuf::from("/other/src/one.gleam"),
            }),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "pub struct Point { x: Int y: Int }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import one
                        fn make() { one.Point(1, 4) }
                        fn x(p) { let one.Point(x, _) = p x }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n
make() ->\n    {point, 1, 4}.\n
x(P) ->\n    {point, X, _} = P,\n    X.\n".to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "pub struct Empty {}".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import one
                        fn make() { one.Empty }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\nmake() ->\n    empty.\n".to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "pub fn id(x) { x } pub struct Empty {}".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import one.{Empty, id} fn make() { id(Empty) }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n-export([id/1]).\n\nid(X) ->\n    X.\n".to_string(),
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\nmake() ->\n    one:id(empty).\n".to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "pub fn receive() { 1 }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import one fn funky() { one.receive }".to_string(),
                },
            ],
            expected: Ok(vec![
                Compiled {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n-export(['receive'/0]).\n\n'receive'() ->\n    1.\n".to_string(),
                },
                Compiled {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\nfunky() ->\n    fun one:'receive'/0.\n".to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "pub fn receive() { 1 }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import one.{receive} fn funky() { receive }".to_string(),
                },
            ],
            expected: Ok(vec![
                Compiled {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n-export(['receive'/0]).\n\n'receive'() ->\n    1.\n".to_string(),
                },
                Compiled {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\nfunky() ->\n    fun one:'receive'/0.\n".to_string(),
                },
            ]),
        },
    ];

    for Case { input, expected } in cases.into_iter() {
        let output = compile(input).map(|mods| {
            mods.into_iter()
                .map(|compiled| Output {
                    name: compiled.name,
                    code: compiled.code,
                    path: compiled.path,
                    origin: compiled.origin,
                })
                .collect::<Vec<_>>()
        });
        assert_eq!(expected, output);
    }
}
