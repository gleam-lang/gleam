use itertools::Itertools;
use petgraph::Graph;
use std::collections::HashMap;
use std::path::PathBuf;
use termcolor::Buffer;

pub type Name = String;
pub type Src = String;

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
}

#[derive(Debug, PartialEq, Clone)]
pub enum ModuleOrigin {
    Src,
    Test,
}

impl ModuleOrigin {
    pub fn dir_name(&self) -> &'static str {
        match self {
            ModuleOrigin::Src => "src",
            ModuleOrigin::Test => "test",
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    Parse {
        path: PathBuf,
        src: Src,
        error: lalrpop_util::ParseError<usize, (usize, String), crate::parser::Error>,
    },

    Type {
        path: PathBuf,
        src: Src,
        error: crate::typ::Error,
    },

    UnknownImport {
        module: Name,
        import: Name,
        meta: crate::ast::Meta,
        path: PathBuf,
        src: String,
        modules: Vec<String>,
    },

    DuplicateModule {
        module: Name,
        first: PathBuf,
        second: PathBuf,
    },

    SrcImportingTest {
        path: PathBuf,
        src: Src,
        meta: crate::ast::Meta,
        src_module: Name,
        test_module: Name,
    },

    DependencyCycle,
}

fn did_you_mean(name: &str, options: &mut Vec<&String>, alt: &'static str) -> String {
    options.sort_by(|a, b| {
        strsim::levenshtein(a, name)
            .partial_cmp(&strsim::levenshtein(b, name))
            .unwrap()
    });

    match options.get(0) {
        Some(option) => format!("Did you mean `{}`?", option),
        None => alt.to_string(),
    }
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
            Error::SrcImportingTest {
                path,
                src,
                meta,
                src_module,
                test_module,
            } => {
                let diagnostic = ErrorDiagnostic {
                    title: "App importing test module".to_string(),
                    label: "Imported here".to_string(),
                    file: path.to_str().unwrap().to_string(),
                    src: src.to_string(),
                    meta: meta.clone(),
                };
                write(buffer, diagnostic);
                write!(
                    buffer,
                    "
The application module `{}` is importing the test module `{}`.

Test modules are not included in production builds so test modules
cannot import them. Perhaps move the `{}` module to the src directory.",
                    src_module, test_module, test_module,
                )
                .unwrap();
            }
            Error::DuplicateModule {
                module,
                first,
                second,
            } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Duplicate module".to_string(),
                    label: format!(
                        "The module `{}` is defined multiple times.

First:  {}
Second: {}",
                        module,
                        first.to_str().expect("pretty error print PathBuf to_str"),
                        second.to_str().expect("pretty error print PathBuf to_str"),
                    ),
                };
                write_project(buffer, diagnostic);
            }

            Error::Type { path, src, error } => match error {
                UnknownLabel {
                    label,
                    meta,
                    labels,
                } => {
                    let mut options: Vec<_> = labels.keys().collect();
                    let diagnostic = ErrorDiagnostic {
                        title: "Unknown label".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                        label: did_you_mean(label, &mut options, "Unexpected label"),
                    };
                    write(buffer, diagnostic);
                    if options.len() > 0 {
                        write!(
                            buffer,
                            "
This constructor does not accept the label `{}`.
Expected one of `{}`.
",
                            label,
                            options.iter().join("`, `")
                        )
                        .unwrap();
                    } else {
                        write!(
                            buffer,
                            "\n This constructor does not accept any labelled arguments.\n "
                        )
                        .unwrap();
                    }

                    if options.len() > 0 {}
                }

                UnexpectedLabelledArg { meta, label } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Unexpected labelled argument".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                    write!(
                        buffer,
                        "
This argument has been given a label but the constructor does not expect any.
Please remove the label `{}`.
",
                        label
                    )
                    .unwrap();
                }

                PositionalArgumentAfterLabelled { meta } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Unexpected positional argument".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                    write!(
                        buffer,
                        "
This unlablled argument has been supplied after a labelled argument.
Once a labelled argument has been supplied all following arguments must
also be labelled.
",
                    )
                    .unwrap();
                }

                DuplicateName { meta, name: fun } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Duplicate name".to_string(),
                        label: "Redefined here".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                    write!(
                        buffer,
                        "
A function has already been defined with the name
`{}` in this module.
",
                        fun
                    )
                    .unwrap();
                }

                DuplicateField { meta, label } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Duplicate field".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                    write!(
                        buffer,
                        "
The field `{}` has already been defined. Rename this field.
",
                        label
                    )
                    .unwrap();
                }

                DuplicateArgument { meta, label } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Duplicate argument".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                    write!(
                        buffer,
                        "
The labelled argument `{}` has already been supplied.
",
                        label
                    )
                    .unwrap();
                }

                RecursiveType { meta } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Recursive type".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                }

                NotFn { meta, typ } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Type mismatch".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);

                    write!(
                        buffer,
                        "
This value is being called as a function but its type is:

{}
",
                        typ.pretty_print(4)
                    )
                    .unwrap();
                }

                NotModule { meta, typ } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Not a module".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);

                    write!(
                        buffer,
                        "
Fields can only be accessed on modules. This is not a module, it is
a value with this type:

{}
",
                        typ.pretty_print(4)
                    )
                    .unwrap();
                }

                CouldNotUnify {
                    meta,
                    expected,
                    given,
                } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Type mismatch".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
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
                        file: path.to_str().unwrap().to_string(),
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
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                }

                UnknownType { meta, name, types } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Unknown type".to_string(),
                        label: did_you_mean(name, &mut types.keys().collect::<Vec<_>>(), ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                    write!(
                        buffer,
                        "
The type `{}` is not defined or imported in this module.
",
                        name
                    )
                    .unwrap();
                }

                UnknownVariable {
                    meta,
                    variables,
                    name,
                } => {
                    let mut options: Vec<_> = variables.keys().collect();
                    let diagnostic = ErrorDiagnostic {
                        title: "Unknown variable".to_string(),
                        label: did_you_mean(name, &mut options, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                    write!(
                        buffer,
                        "
The name `{}` is not in scope here.
",
                        name
                    )
                    .unwrap();
                }

                PrivateTypeLeak { meta, leaked } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Private type used in public interface".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);

                    // TODO: be more precise.
                    // - is being returned by this public function
                    // - is taken as an argument by this public function
                    // - is taken as an argument by this public enum constructor
                    // etc
                    write!(
                        buffer,
                        "
The following type is private, but is being used by this public export.

{}

Private types can only be used within the module that defines them.
",
                        leaked.pretty_print(4),
                    )
                    .unwrap();
                }

                UnknownModule {
                    meta,
                    name,
                    imported_modules,
                } => {
                    let mut options: Vec<_> = imported_modules.keys().collect();
                    let diagnostic = ErrorDiagnostic {
                        title: "Unknown module".to_string(),
                        label: did_you_mean(name, &mut options, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                    write!(
                        buffer,
                        "
No module has been imported with the name `{}`.
",
                        name
                    )
                    .unwrap();
                }

                UnknownModuleType {
                    meta,
                    name,
                    module_name,
                    type_constructors,
                } => {
                    let mut options: Vec<_> = type_constructors.keys().collect();
                    let diagnostic = ErrorDiagnostic {
                        title: "Unknown module type".to_string(),
                        label: did_you_mean(name, &mut options, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                    write!(
                        buffer,
                        "\nThe module `{}` does not have a `{}` type.\n",
                        module_name.join("/"),
                        name
                    )
                    .unwrap();
                }

                UnknownModuleValue {
                    meta,
                    name,
                    module_name,
                    value_constructors,
                } => {
                    let mut options: Vec<_> = value_constructors.keys().collect();
                    let diagnostic = ErrorDiagnostic {
                        title: "Unknown module value".to_string(),
                        label: did_you_mean(name, &mut options, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                    write!(
                        buffer,
                        "\nThe module `{}` does not have a `{}` value.\n",
                        module_name.join("/"),
                        name
                    )
                    .unwrap();
                }

                UnknownModuleField {
                    meta,
                    name,
                    module_name,
                    type_constructors,
                    value_constructors,
                } => {
                    let mut options: Vec<_> = type_constructors
                        .keys()
                        .chain(value_constructors.keys())
                        .collect();
                    let diagnostic = ErrorDiagnostic {
                        title: "Unknown module field".to_string(),
                        label: did_you_mean(name, &mut options, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                    write!(
                        buffer,
                        "\nThe module `{}` does not have a `{}` field.\n",
                        module_name.join("/"),
                        name
                    )
                    .unwrap();
                }
            },

            Error::Parse { path, src, error } => {
                use lalrpop_util::ParseError::*;

                match error {
                    UnrecognizedToken {
                        token: Some((start, _, end)),
                        expected,
                    } => {
                        let diagnostic = ErrorDiagnostic {
                            title: "Syntax error".to_string(),
                            label: "Unexpected token".to_string(),
                            file: path.to_str().unwrap().to_string(),
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
                            file: path.to_str().unwrap().to_string(),
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
                            file: path.to_str().unwrap().to_string(),
                            src: src.to_string(),
                            meta: crate::ast::Meta {
                                start: *location,
                                end: *location + 1,
                            },
                        };
                        write(buffer, diagnostic);
                        write!(
                            buffer,
                            "\nI don't know what this character means. Is it a typo?\n"
                        )
                        .expect("error pretty buffer write");
                    }

                    ExtraToken { .. } => unimplemented!(),

                    User { error } => {
                        use crate::parser::Error;
                        match error {
                            Error::TooManyHolesInCapture { meta, count } => {
                                let diagnostic = ErrorDiagnostic {
                                    title: "Invalid capture".to_string(),
                                    label: "".to_string(),
                                    file: path.to_str().unwrap().to_string(),
                                    src: src.to_string(),
                                    meta: meta.clone(),
                                };
                                write(buffer, diagnostic);
                                let chars: String = (97..(97 + count))
                                    .map(|x| x as u8 as char)
                                    .map(|c| c.to_string())
                                    .intersperse(", ".to_string())
                                    .collect();
                                write!(
                                    buffer,
                                    "
The function capture syntax can only be used with a single _ argument,
but this one uses {}. Rewrite this using the fn({}) {{ ... }} syntax.",
                                    count, chars
                                )
                                .expect("error pretty buffer write");
                            }
                        }
                    }
                }
            }

            Error::DependencyCycle => {
                println!("{:?}", self);
                unimplemented!();
            }

            Error::UnknownImport {
                module,
                import,
                meta,
                path,
                src,
                modules,
            } => {
                let mut modules: Vec<&String> = modules.iter().collect();
                let diagnostic = ErrorDiagnostic {
                    title: "Unknown import".to_string(),
                    label: did_you_mean(import, &mut modules, ""),
                    file: path.to_str().unwrap().to_string(),
                    src: src.to_string(),
                    meta: meta.clone(),
                };
                write(buffer, diagnostic);
                write!(
                    buffer,
                    "
The module `{}` is trying to import the module `{}`,
but it cannot be found.
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

/// Describes an error encountered while compiling the project (eg. a name collision
/// between files).
///
struct ProjectErrorDiagnostic {
    title: String,
    label: String,
}

fn write_project(mut buffer: &mut Buffer, d: ProjectErrorDiagnostic) {
    use codespan::{CodeMap, Span};
    use codespan_reporting::{Diagnostic, Label};

    let code_map: CodeMap<String> = CodeMap::new();
    let diagnostic = Diagnostic::new_error(d.title)
        .with_label(Label::new_primary(Span::new(0.into(), 0.into())).with_message(d.label));
    codespan_reporting::emit(&mut buffer, &code_map, &diagnostic).unwrap();
}

pub fn compile(srcs: Vec<Input>) -> Result<Vec<Compiled>, Error> {
    struct Module {
        src: String,
        path: PathBuf,
        base_path: PathBuf,
        origin: ModuleOrigin,
        module: crate::ast::UntypedModule,
    }

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

        module.name = name.split("/").map(|s| s.to_string()).collect();

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

    petgraph::algo::toposort(&deps_graph, None)
        .map_err(|_| Error::DependencyCycle)?
        .into_iter()
        .map(|i| {
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

            modules_type_infos.insert(name_string, module.type_info.clone());

            let path = base_path
                .parent()
                .unwrap()
                .join("gen")
                .join(origin.dir_name())
                .join(format!("{}.erl", module.name.join("@")));
            let code = crate::erl::module(module);

            Ok(Compiled { name, code, path })
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
                Compiled {
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Compiled {
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
            expected: Ok(vec![Compiled {
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
                Compiled {
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Compiled {
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
                Compiled {
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Compiled {
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
                Compiled {
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Compiled {
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
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "pub enum Box { Box(Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    base_path: PathBuf::from("/src"),
                    src: "import one pub fn box(x) { one.Box(x) }".to_string(),
                },
            ],
            expected: Ok(vec![
                Compiled {
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Compiled {
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
            expected: Ok(vec![Compiled {
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
                Compiled {
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Compiled {
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
                Compiled {
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n-export([go/0]).\n
go() ->
    1.\n"
                        .to_string(),
                },
                Compiled {
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
                Compiled {
                    name: vec!["nested".to_string(), "one".to_string()],
                    path: PathBuf::from("/gen/src/nested@one.erl"),
                    code: "-module(nested@one).\n-compile(no_auto_import).\n\n\n"
                        .to_string(),
                },
                Compiled {
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
                Compiled {
                    name: vec!["nested".to_string(), "one".to_string()],
                    path: PathBuf::from("/gen/src/nested@one.erl"),
                    code: "-module(nested@one).\n-compile(no_auto_import).\n\n\n"
                        .to_string(),
                },
                Compiled {
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
                Compiled {
                    name: vec!["nested".to_string(), "one".to_string()],
                    path: PathBuf::from("/gen/src/nested@one.erl"),
                    code: "-module(nested@one).\n-compile(no_auto_import).\n\n-export([go/0]).\n
go() ->\n    1.\n"
                        .to_string(),
                },
                Compiled {
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
                Compiled {
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Compiled {
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
                Compiled {
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                Compiled {
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
                Compiled {
                    name: vec!["one".to_string()],
                    path: PathBuf::from("/gen/src/one.erl"),
                    code: "-module(one).\n-compile(no_auto_import).\n\n-export([id/1]).\n\nid(X) ->\n    X.\n".to_string(),
                },
                Compiled {
                    name: vec!["two".to_string()],
                    path: PathBuf::from("/gen/src/two.erl"),
                    code: "-module(two).\n-compile(no_auto_import).\n\nmake() ->\n    one:id(empty).\n".to_string(),
                },
            ]),
        },
    ];

    for Case { input, expected } in cases.into_iter() {
        assert_eq!(expected, compile(input));
    }
}
