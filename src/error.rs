use crate::typ::pretty::Printer;
use itertools::Itertools;
use std::path::PathBuf;
use termcolor::Buffer;

pub type Src = String;
pub type Name = String;

pub fn fatal_compiler_bug(msg: &str) -> ! {
    let buffer_writer = termcolor::BufferWriter::stderr(termcolor::ColorChoice::Always);
    let mut buffer = buffer_writer.buffer();
    use std::io::Write;
    use termcolor::{Color, ColorSpec, WriteColor};
    buffer
        .set_color(ColorSpec::new().set_bold(true).set_fg(Some(Color::Red)))
        .unwrap();
    write!(buffer, "error").unwrap();
    buffer.set_color(ColorSpec::new().set_bold(true)).unwrap();
    write!(buffer, ": Fatal compiler bug!\n\n").unwrap();
    buffer.set_color(&ColorSpec::new()).unwrap();
    writeln!(
        buffer,
        "This is a bug in the Gleam compiler, sorry!

Please report this crash to https://github.com/gleam-lang/gleam/issues/new
with this information and the code that produces the crash:

{}",
        msg
    )
    .unwrap();
    buffer_writer.print(&buffer).unwrap();
    std::process::exit(1);
}

pub trait GleamExpect<T> {
    fn gleam_expect(self, msg: &str) -> T;
}

impl<T> GleamExpect<T> for Option<T> {
    fn gleam_expect(self, msg: &str) -> T {
        match self {
            None => fatal_compiler_bug(msg),
            Some(x) => x,
        }
    }
}

impl<T, E> GleamExpect<T> for Result<T, E> {
    fn gleam_expect(self, msg: &str) -> T {
        match self {
            Err(_) => fatal_compiler_bug(msg),
            Ok(x) => x,
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

    ImportCycle {
        modules: Vec<Vec<String>>,
    },

    FileIO {
        kind: FileKind,
        action: FileIOAction,
        path: PathBuf,
        err: Option<String>,
    },
}

#[derive(Debug, PartialEq)]
pub enum FileIOAction {
    Open,
    Read,
    Parse,
    Delete,
    Create,
    WriteTo,
    FindParent,
}

impl FileIOAction {
    fn text(&self) -> &'static str {
        match self {
            FileIOAction::Open => "open",
            FileIOAction::Read => "read",
            FileIOAction::Parse => "parse",
            FileIOAction::Delete => "delete",
            FileIOAction::Create => "create",
            FileIOAction::WriteTo => "write to",
            FileIOAction::FindParent => "find the parent of",
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum FileKind {
    File,
    Directory,
}

impl FileKind {
    fn text(&self) -> &'static str {
        match self {
            FileKind::File => "file",
            FileKind::Directory => "directory",
        }
    }
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
                writeln!(
                    buffer,
                    "The application module `{}` is importing the test module `{}`.

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
Second: {}
",
                        module,
                        first.to_str().expect("pretty error print PathBuf to_str"),
                        second.to_str().expect("pretty error print PathBuf to_str"),
                    ),
                };
                write_project(buffer, diagnostic);
            }

            Error::FileIO {
                kind,
                action,
                path,
                err,
            } => {
                let err = match err {
                    Some(e) => format!(
                        "\nThe error message from the file IO library was:\n\n    {}\n",
                        e
                    ),
                    None => "".to_string(),
                };
                let diagnostic = ProjectErrorDiagnostic {
                    title: "File IO failure".to_string(),
                    label: format!(
                        "An error occurred while trying to {} this {}:

    {}
{}
",
                        action.text(),
                        kind.text(),
                        path.to_string_lossy(),
                        err,
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
                    if !options.is_empty() {
                        writeln!(
                            buffer,
                            "This constructor does not accept the label `{}`.
Expected one of `{}`.",
                            label,
                            options.iter().join("`, `")
                        )
                        .unwrap();
                    } else {
                        writeln!(
                            buffer,
                            "This constructor does not accept any labelled arguments."
                        )
                        .unwrap();
                    }
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
                    writeln!(
                        buffer,
                        "
This argument has been given a label but the constructor does not expect any.
Please remove the label `{}`.",
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
                    writeln!(
                        buffer,
                        "This unlablled argument has been supplied after a labelled argument.
Once a labelled argument has been supplied all following arguments must
also be labelled.",
                    )
                    .unwrap();
                }

                DuplicateName {
                    location,
                    name: fun,
                    // TODO: show previous location
                    // previous_location,
                    ..
                } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Duplicate name".to_string(),
                        label: "Redefined here".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: location.clone(),
                    };
                    write(buffer, diagnostic);
                    writeln!(
                        buffer,
                        "A function has already been defined with the name
`{}` in this module.",
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
                    writeln!(
                        buffer,
                        "The field `{}` has already been defined. Rename this field.",
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
                    writeln!(
                        buffer,
                        "The labelled argument `{}` has already been supplied.",
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
                    let mut printer = Printer::new();

                    writeln!(
                        buffer,
                        "This value is being called as a function but its type is:\n\n{}",
                        printer.pretty_print(typ, 4)
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
                    let mut printer = Printer::new();

                    writeln!(
                        buffer,
                        "Fields can only be accessed on modules. This is not a module, it is
a value with this type:

{}",
                        printer.pretty_print(typ, 4)
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
                    let mut printer = Printer::new();

                    writeln!(
                        buffer,
                        "Expected type:

{}

Found type:

{}",
                        printer.pretty_print(expected, 4),
                        printer.pretty_print(given, 4),
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
                    writeln!(
                        buffer,
                        "The type `{}` is not defined or imported in this module.",
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
                    writeln!(buffer, "The name `{}` is not in scope here.", name).unwrap();
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
                    let mut printer = Printer::new();

                    // TODO: be more precise.
                    // - is being returned by this public function
                    // - is taken as an argument by this public function
                    // - is taken as an argument by this public enum constructor
                    // etc
                    writeln!(
                        buffer,
                        "The following type is private, but is being used by this public export.

{}

Private types can only be used within the module that defines them.",
                        printer.pretty_print(leaked, 4),
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
                    writeln!(
                        buffer,
                        "No module has been imported with the name `{}`.",
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
                    writeln!(
                        buffer,
                        "The module `{}` does not have a `{}` type.",
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
                        title: "Unknown module field".to_string(),
                        label: did_you_mean(name, &mut options, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                    writeln!(
                        buffer,
                        "The module `{}` does not have a `{}` field.",
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
                    writeln!(
                        buffer,
                        "The module `{}` does not have a `{}` field.",
                        module_name.join("/"),
                        name
                    )
                    .unwrap();
                }

                IncorrectNumClausePatterns {
                    meta,
                    expected,
                    given,
                } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Incorrect number of pattern".to_string(),
                        label: format!("Expected {} patterns, got {}", expected, given),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                }

                NonLocalClauseGuardVariable { meta, name } => {
                    let diagnostic = ErrorDiagnostic {
                        title: "Invalid guard varible".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        meta: meta.clone(),
                    };
                    write(buffer, diagnostic);
                    writeln!(
                        buffer,
                        "Variables used in guards must be either defined in the function, or be an
argument to the function. The variable `{}` is not defined locally.",
                        name
                    )
                    .unwrap();
                }
            },

            Error::Parse { path, src, error } => {
                use lalrpop_util::ParseError::*;

                match error {
                    UnrecognizedToken {
                        token: (start, _, end),
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
                        writeln!(buffer, "Expected one of {}", expected.join(", "))
                            .expect("error pretty buffer write");
                    }

                    UnrecognizedEOF { .. } => {
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
                        writeln!(
                            buffer,
                            "I don't know what this character means. Is it a typo?"
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
                                writeln!(
                                    buffer,
                                    "The function capture syntax can only be used with a single _ argument,
but this one uses {}. Rewrite this using the fn({}) {{ ... }} syntax.",
                                    count, chars
                                )
                                .expect("error pretty buffer write");
                            }
                        }
                    }
                }
            }

            Error::ImportCycle { modules } => import_cycle(buffer, modules.as_ref()),

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
                writeln!(
                    buffer,
                    "The module `{}` is trying to import the module `{}`,
but it cannot be found.",
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

fn import_cycle(buffer: &mut Buffer, modules: &[Vec<String>]) {
    use std::io::Write;
    use termcolor::{Color, ColorSpec, WriteColor};
    write_title(buffer, "Import cycle");
    writeln!(
        buffer,
        "The import statements for these modules form a cycle:

    ┌─────┐"
    )
    .unwrap();
    for (index, name) in modules.iter().enumerate() {
        if index != 0 {
            writeln!(buffer, "    │     ↓").unwrap();
        }
        write!(buffer, "    │    ").unwrap();
        buffer
            .set_color(ColorSpec::new().set_fg(Some(Color::Cyan)))
            .unwrap();
        writeln!(buffer, "{}", name.join("/")).unwrap();
        buffer.set_color(&ColorSpec::new()).unwrap();
    }
    writeln!(
        buffer,
        "    └─────┘

Gleam doesn't support import cycles like these, please break the
cycle to continue."
    )
    .unwrap();
}

struct ErrorDiagnostic {
    file: String,
    meta: crate::ast::Meta,
    src: String,
    title: String,
    label: String,
}

fn write(mut buffer: &mut Buffer, d: ErrorDiagnostic) {
    use codespan::Files;
    use codespan_reporting::diagnostic::{Diagnostic, Label};
    use codespan_reporting::term::emit;

    let mut files = Files::new();
    let file_id = files.add(d.file, d.src);

    let diagnostic = Diagnostic::new_error(
        d.title,
        Label::new(file_id, (d.meta.start as u32)..(d.meta.end as u32), d.label),
    );

    let config = codespan_reporting::term::Config::default();
    emit(&mut buffer, &config, &files, &diagnostic).unwrap();
}

/// Describes an error encountered while compiling the project (eg. a name collision
/// between files).
///
struct ProjectErrorDiagnostic {
    title: String,
    label: String,
}

fn write_title(buffer: &mut Buffer, title: &str) {
    use std::io::Write;
    use termcolor::{Color, ColorSpec, WriteColor};
    buffer
        .set_color(ColorSpec::new().set_bold(true).set_fg(Some(Color::Red)))
        .unwrap();
    write!(buffer, "error").unwrap();
    buffer.set_color(ColorSpec::new().set_bold(true)).unwrap();
    write!(buffer, ": {}\n\n", title).unwrap();
    buffer.set_color(&ColorSpec::new()).unwrap();
}

fn write_project(buffer: &mut Buffer, d: ProjectErrorDiagnostic) {
    use std::io::Write;
    use termcolor::{ColorSpec, WriteColor};
    write_title(buffer, d.title.as_ref());
    buffer.set_color(&ColorSpec::new()).unwrap();
    write!(buffer, "{}", d.label).unwrap();
}
