use petgraph::Graph;
use std::collections::HashMap;
use std::path::PathBuf;
use termcolor::Buffer;

pub type Name = String;
pub type Src = String;

#[derive(Debug, PartialEq)]
pub struct Input {
    pub path: PathBuf,
    pub src: String,
}

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

    UnknownImport {
        module: Name,
        import: Name,
    },

    DependencyCycle,
}

impl Error {
    // TODO: Tests.
    pub fn pretty(&self, buffer: &mut Buffer) {
        use crate::typ::Error::*;
        use std::io::Write;

        buffer
            .write_all(b"\n")
            .expect("error pretty buffer write space before");

        match self {
            Error::Type { src, name, error } => match error {
                DuplicateName { meta, name: fun } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Duplicate name".to_string(),
                        label: format!("`{}` redefined here", fun),
                        file: name.clone(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                }

                RecursiveType { meta } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Recursive type".to_string(),
                        label: "".to_string(),
                        file: name.clone(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                }

                CouldNotUnify {
                    meta,
                    expected,
                    given,
                } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Type mismatch".to_string(),
                        label: "".to_string(),
                        file: name.clone(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);

                    write!(
                        buffer,
                        "
Expected type:

{}

Found type:

{}
",
                        expected.pretty_print(4),
                        given.pretty_print(4)
                    )
                    .unwrap();
                }

                IncorrectTypeArity {
                    meta,
                    expected,
                    given,
                    ..
                } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Incorrect arity".to_string(),
                        label: format!("Expected {} arguments, got {}", expected, given),
                        file: name.clone(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                }

                IncorrectArity {
                    meta,
                    expected,
                    given,
                } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Incorrect arity".to_string(),
                        label: format!("Expected {} arguments, got {}", expected, given),
                        file: name.clone(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                }

                UnknownType { meta, .. } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Unknown type".to_string(),
                        label: "".to_string(),
                        file: name.clone(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                }

                UnknownVariable { meta, .. } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Unknown variable".to_string(),
                        label: "".to_string(),
                        file: name.clone(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                }

                PrivateTypeLeak { .. } => {
                    // let diagnostic = ErrorDiagnostic {
                    //     title: "Unknown variable".to_string(),
                    //     label: "".to_string(),
                    //     file: name.clone(),
                    //     src: src.to_string(),
                    //     meta: meta.clone(),
                    // };
                    // write(buffer, diagnostic);
                    // TODO
                    unimplemented!()
                }
            },

            Error::Parse { name, src, error } => {
                use lalrpop_util::ParseError::*;

                match error {
                    UnrecognizedToken {
                        token: Some((start, _, end)),
                        expected,
                    } => {
                        let diagnostic = ErrorDiagnostic {
                            title: "Syntax error".to_string(),
                            label: "Unexpected token".to_string(),
                            file: name.clone(),
                            src: src.to_string(),
                            meta: crate::ast::Meta {
                                start: *start,
                                end: *end,
                            },
                        };
                        write(buffer, diagnostic);
                        write!(buffer, "\nExpected one of {}", expected.join(", "))
                            .expect("error pretty buffer write");
                    }

                    UnrecognizedToken { token: None, .. } => {
                        let diagnostic = ErrorDiagnostic {
                            title: "Syntax error".to_string(),
                            label: "Unexpected end of file".to_string(),
                            file: name.clone(),
                            src: src.to_string(),
                            meta: crate::ast::Meta {
                                start: src.len() - 2,
                                end: src.len() - 1,
                            },
                        };
                        write(buffer, diagnostic);
                    }

                    InvalidToken { location } => {
                        let diagnostic = ErrorDiagnostic {
                            title: "Syntax error".to_string(),
                            label: "Unknown token".to_string(),
                            file: name.clone(),
                            src: src.to_string(),
                            meta: crate::ast::Meta {
                                start: *location - 1,
                                end: *location,
                            },
                        };
                        write(buffer, diagnostic);
                    }

                    ExtraToken { .. } => unimplemented!(),

                    User { .. } => unimplemented!(),
                }
            }

            Error::DependencyCycle => {
                println!("{:?}", self);
                unimplemented!();
            }

            Error::UnknownImport { module, import } => {
                // TODO: source code preview
                write!(
                    buffer,
                    "error: Unknown import

The module `{}` is trying to import the module `{}`, but it cannot be found.
",
                    module, import
                )
                .expect("error pretty buffer write");
            }
        }

        buffer
            .write_all(b"\n")
            .expect("error pretty buffer write space after");
    }

    pub fn pretty_print(&self) {
        let buffer_writer = termcolor::BufferWriter::stderr(termcolor::ColorChoice::Always);
        let mut buffer = buffer_writer.buffer();
        self.pretty(&mut buffer);
        buffer_writer.print(&buffer).unwrap();
    }
}

struct ErrorDiagnostic {
    file: String,
    meta: crate::ast::Meta,
    src: String,
    title: String,
    label: String,
}

fn write(mut buffer: &mut Buffer, d: ErrorDiagnostic) {
    use codespan::{CodeMap, Span};
    use codespan_reporting::{Diagnostic, Label};

    let mut code_map = CodeMap::new();
    code_map.add_filemap(d.file.into(), d.src);
    let diagnostic = Diagnostic::new_error(d.title).with_label(
        Label::new_primary(Span::from_offset(
            ((d.meta.start + 1) as u32).into(),
            ((d.meta.end - d.meta.start) as i64).into(),
        ))
        .with_message(d.label),
    );
    codespan_reporting::emit(&mut buffer, &code_map, &diagnostic).unwrap();
}

pub fn compile(srcs: Vec<Input>) -> Result<Vec<Compiled>, Error> {
    let mut deps_graph = Graph::new();
    let mut deps_vec = Vec::with_capacity(srcs.len());
    let mut indexes = HashMap::new();
    let mut modules = HashMap::new();

    for Input { path, src } in srcs {
        let name = path.file_stem().unwrap().to_str().unwrap().to_string();
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
        indexes.insert(name.clone(), index);
        modules.insert(index, (src, module));
    }

    // Register each module's deps so that we can determine a correct order to compile the modules.
    for (module, deps) in deps_vec {
        let module_index = indexes.get(&module).expect("Unable to find module index");
        for dep in deps {
            let dep_index = indexes.get(&dep).ok_or_else(|| Error::UnknownImport {
                module: module.clone(),
                import: dep,
            })?;
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
        input: Vec<Input>,
        expected: Result<Vec<Compiled>, Error>,
    }

    let cases = vec![
        Case {
            input: vec![],
            expected: Ok(vec![]),
        },
        Case {
            input: vec![
                Input {
                    path: PathBuf::from("/src/one"),
                    src: "".to_string(),
                },
                Input {
                    path: PathBuf::from("/src/two"),
                    src: "".to_string(),
                },
            ],
            expected: Ok(vec![
                Compiled {
                    name: "two".to_string(),
                    out: "-module(two).\n-compile(no_auto_import).\n\n-export([]).\n\n\n"
                        .to_string(),
                },
                Compiled {
                    name: "one".to_string(),
                    out: "-module(one).\n-compile(no_auto_import).\n\n-export([]).\n\n\n"
                        .to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    path: PathBuf::from("/src/one"),
                    src: "import two".to_string(),
                },
                Input {
                    path: PathBuf::from("/src/two"),
                    src: "".to_string(),
                },
            ],
            expected: Ok(vec![
                Compiled {
                    name: "two".to_string(),
                    out: "-module(two).\n-compile(no_auto_import).\n\n-export([]).\n\n\n"
                        .to_string(),
                },
                Compiled {
                    name: "one".to_string(),
                    out: "-module(one).\n-compile(no_auto_import).\n\n-export([]).\n\n\n"
                        .to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    path: PathBuf::from("/src/one"),
                    src: "".to_string(),
                },
                Input {
                    path: PathBuf::from("/src/two"),
                    src: "import one".to_string(),
                },
            ],
            expected: Ok(vec![
                Compiled {
                    name: "one".to_string(),
                    out: "-module(one).\n-compile(no_auto_import).\n\n-export([]).\n\n\n"
                        .to_string(),
                },
                Compiled {
                    name: "two".to_string(),
                    out: "-module(two).\n-compile(no_auto_import).\n\n-export([]).\n\n\n"
                        .to_string(),
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    path: PathBuf::from("/src/one"),
                    src: "pub enum Box = | Box(Int)".to_string(),
                },
                Input {
                    path: PathBuf::from("/src/two"),
                    src: "import one pub fn unbox(x) { let one:Box(i) = x i }".to_string(),
                },
            ],
            expected: Ok(vec![
                Compiled {
                    name: "one".to_string(),
                    out: "-module(one).\n-compile(no_auto_import).\n\n-export([]).\n\n\n"
                        .to_string(),
                },
                Compiled {
                    name: "two".to_string(),
                    out: "-module(two).\n-compile(no_auto_import).\n\n-export([unbox/1]).\n
unbox(X) ->\n    {box, I} = X,\n    I.\n"
                        .to_string(),
                },
            ]),
        },
    ];

    for Case { input, expected } in cases.into_iter() {
        assert_eq!(expected, compile(input));
    }
}
