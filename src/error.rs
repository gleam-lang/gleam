use crate::diagnostic::{
    buffer_writer, write, write_project, Diagnostic, ProjectErrorDiagnostic, Severity,
};
use crate::typ::pretty::Printer;
use itertools::Itertools;
use std::fmt::Debug;
use std::path::PathBuf;
use termcolor::Buffer;

pub type Src = String;
pub type Name = String;

pub fn fatal_compiler_bug(msg: &str) -> ! {
    let buffer_writer = buffer_writer();
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

impl<T, E: Debug> GleamExpect<T> for Result<T, E> {
    fn gleam_expect(self, msg: &str) -> T {
        match self {
            Err(e) => fatal_compiler_bug(format!("{}\n\n{:?}", msg, e).as_str()),
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
        location: crate::ast::SrcSpan,
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
        location: crate::ast::SrcSpan,
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

    StandardIO {
        action: StandardIOAction,
        err: Option<std::io::ErrorKind>,
    },

    Format {
        problem_files: Vec<crate::format::command::Formatted>,
    },

    Hex(String),

    Tar {
        path: PathBuf,
        err: String,
    },

    TarFinish(String),

    Gzip(String),
}

#[derive(Debug, PartialEq)]
pub enum StandardIOAction {
    Read,
}

impl StandardIOAction {
    fn text(&self) -> &'static str {
        match self {
            StandardIOAction::Read => "read from",
        }
    }
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

fn did_you_mean(name: &str, options: &mut Vec<String>, alt: &'static str) -> String {
    // Remove magic variables
    options.retain(|option| option != crate::ast::CAPTURE_VARIABLE);

    // Find best match
    options.sort_by(|a, b| {
        strsim::levenshtein(a, name)
            .partial_cmp(&strsim::levenshtein(b, name))
            .unwrap()
    });

    match options.get(0) {
        Some(option) => format!("did you mean `{}`?", option),
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
            Error::Gzip(detail) => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Gzip compression failure".to_string(),
                    label: format!(
                        "There was a problem when applying gzip compression.

This was error from the gzip library:

    {}",
                        detail
                    ),
                };
                write_project(buffer, diagnostic);
            }

            Error::Tar { path, err } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Failure creating tar archive".to_string(),
                    label: format!(
                        "There was a problem when attempting to add the file {}
to a tar archive.

This was error from the tar library:

    {}",
                        path.to_str().unwrap(),
                        err.to_string()
                    ),
                };
                write_project(buffer, diagnostic);
            }

            Error::TarFinish(detail) => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Failure creating tar archive".to_string(),
                    label: format!(
                        "There was a problem when creating a tar archive.

This was error from the tar library:

    {}",
                        detail
                    ),
                };
                write_project(buffer, diagnostic);
            }

            Error::Hex(detail) => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Hex API failure".to_string(),
                    label: format!(
                        "There was a problem when using the Hex API.

This was error from the Hex client library:

    {}",
                        detail
                    ),
                };
                write_project(buffer, diagnostic);
            }

            Error::SrcImportingTest {
                path,
                src,
                location,
                src_module,
                test_module,
            } => {
                let diagnostic = Diagnostic {
                    title: "App importing test module".to_string(),
                    label: "Imported here".to_string(),
                    file: path.to_str().unwrap().to_string(),
                    src: src.to_string(),
                    location: location.clone(),
                };
                write(buffer, diagnostic, Severity::Error);
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
Second: {}",
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
{}",
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
                    location,
                    labels,
                } => {
                    let mut labels = labels.clone();
                    let diagnostic = Diagnostic {
                        title: "Unknown label".to_string(),
                        label: did_you_mean(label, &mut labels, "Unexpected label"),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    if !labels.is_empty() {
                        writeln!(
                            buffer,
                            "This constructor does not accept the label `{}`.
Expected one of `{}`.",
                            label,
                            labels.iter().join("`, `")
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

                UnexpectedLabelledArg { location, label } => {
                    let diagnostic = Diagnostic {
                        title: "Unexpected labelled argument".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "
This argument has been given a label but the constructor does not expect any.
Please remove the label `{}`.",
                        label
                    )
                    .unwrap();
                }

                PositionalArgumentAfterLabelled { location } => {
                    let diagnostic = Diagnostic {
                        title: "Unexpected positional argument".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
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
                    let diagnostic = Diagnostic {
                        title: "Duplicate name".to_string(),
                        label: "redefined here".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "A function has already been defined with the name
`{}` in this module.",
                        fun
                    )
                    .unwrap();
                }

                DuplicateTypeName {
                    name,
                    location,
                    // TODO: show previous location
                    // previous_location,
                    ..
                } => {
                    let diagnostic = Diagnostic {
                        title: "Duplicate name".to_string(),
                        label: "redefined here".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "A type has already been defined with the name
`{}` in this module.",
                        name
                    )
                    .unwrap();
                }

                DuplicateField { location, label } => {
                    let diagnostic = Diagnostic {
                        title: "Duplicate field".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "The field `{}` has already been defined. Rename this field.",
                        label
                    )
                    .unwrap();
                }

                DuplicateArgument { location, label } => {
                    let diagnostic = Diagnostic {
                        title: "Duplicate argument".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "The labelled argument `{}` has already been supplied.",
                        label
                    )
                    .unwrap();
                }

                RecursiveType { location } => {
                    let diagnostic = Diagnostic {
                        title: "Recursive type".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                }

                NotFn { location, typ } => {
                    let diagnostic = Diagnostic {
                        title: "Type mismatch".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    let mut printer = Printer::new();

                    writeln!(
                        buffer,
                        "This value is being called as a function but its type is:\n\n{}",
                        printer.pretty_print(typ, 4)
                    )
                    .unwrap();
                }

                UnknownField {
                    location,
                    typ,
                    label,
                    fields,
                } => {
                    let mut fields = fields.clone();
                    let diagnostic = Diagnostic {
                        title: "Unknown field".to_string(),
                        label: did_you_mean(
                            label.as_ref(),
                            &mut fields,
                            "This field does not exist",
                        ),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    let mut printer = Printer::new();

                    writeln!(
                        buffer,
                        "The value has this type:

{}
",
                        printer.pretty_print(typ, 4)
                    )
                    .unwrap();

                    if fields.is_empty() {
                        writeln!(buffer, "It does not have any fields.",).unwrap();
                    } else {
                        write!(buffer, "It has these fields:\n\n").unwrap();
                        for field in fields {
                            writeln!(buffer, "    .{}", field).unwrap();
                        }
                    }
                }

                CouldNotUnify {
                    location,
                    expected,
                    given,
                } => {
                    let diagnostic = Diagnostic {
                        title: "Type mismatch".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
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
                    location,
                    expected,
                    given,
                    ..
                } => {
                    let diagnostic = Diagnostic {
                        title: "Incorrect arity".to_string(),
                        label: format!("expected {} arguments, got {}", expected, given),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                }

                IncorrectArity {
                    location,
                    expected,
                    given,
                } => {
                    let diagnostic = Diagnostic {
                        title: "Incorrect arity".to_string(),
                        label: format!("expected {} arguments, got {}", expected, given),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                }

                UnnecessarySpreadOperator { location, arity } => {
                    let diagnostic = Diagnostic {
                        title: "Unnecessary spread operator".to_string(),
                        label: format!(""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);

                    writeln!(
                        buffer,
                        "This record has {} fields and you have already assigned variables to all of them.",
                        arity
                    )
                    .unwrap();
                }

                UnknownType {
                    location,
                    name,
                    types,
                } => {
                    let mut types = types.clone();
                    let diagnostic = Diagnostic {
                        title: "Unknown type".to_string(),
                        label: did_you_mean(name, &mut types, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "The type `{}` is not defined or imported in this module.",
                        name
                    )
                    .unwrap();
                }

                UnknownVariable {
                    location,
                    variables,
                    name,
                } => {
                    let mut variables = variables.clone();
                    let diagnostic = Diagnostic {
                        title: "Unknown variable".to_string(),
                        label: did_you_mean(name, &mut variables, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(buffer, "The name `{}` is not in scope here.", name).unwrap();
                }

                PrivateTypeLeak { location, leaked } => {
                    let diagnostic = Diagnostic {
                        title: "Private type used in public interface".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
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
                    location,
                    name,
                    imported_modules,
                } => {
                    let mut imported_modules = imported_modules.clone();
                    let diagnostic = Diagnostic {
                        title: "Unknown module".to_string(),
                        label: did_you_mean(name, &mut imported_modules, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "No module has been imported with the name `{}`.",
                        name
                    )
                    .unwrap();
                }

                UnknownModuleType {
                    location,
                    name,
                    module_name,
                    type_constructors,
                } => {
                    let mut type_constructors = type_constructors.clone();
                    let diagnostic = Diagnostic {
                        title: "Unknown module type".to_string(),
                        label: did_you_mean(name, &mut type_constructors, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "The module `{}` does not have a `{}` type.",
                        module_name.join("/"),
                        name
                    )
                    .unwrap();
                }

                UnknownModuleValue {
                    location,
                    name,
                    module_name,
                    value_constructors,
                } => {
                    let mut value_constructors = value_constructors.clone();
                    let diagnostic = Diagnostic {
                        title: "Unknown module field".to_string(),
                        label: did_you_mean(name, &mut value_constructors, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "The module `{}` does not have a `{}` field.",
                        module_name.join("/"),
                        name
                    )
                    .unwrap();
                }

                UnknownModuleField {
                    location,
                    name,
                    module_name,
                    type_constructors,
                    value_constructors,
                } => {
                    let mut options: Vec<String> = type_constructors
                        .iter()
                        .chain(value_constructors.iter())
                        .map(|s| s.to_string())
                        .collect();
                    let diagnostic = Diagnostic {
                        title: "Unknown module field".to_string(),
                        label: did_you_mean(name, &mut options, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "The module `{}` does not have a `{}` field.",
                        module_name.join("/"),
                        name
                    )
                    .unwrap();
                }

                IncorrectNumClausePatterns {
                    location,
                    expected,
                    given,
                } => {
                    let diagnostic = Diagnostic {
                        title: "Incorrect number of patterns".to_string(),
                        label: format!("expected {} patterns, got {}", expected, given),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "This case expression has {} subjects, but this pattern matches {}.
Each clause must have a pattern for every subject value.",
                        expected, given
                    )
                    .unwrap();
                }

                NonLocalClauseGuardVariable { location, name } => {
                    let diagnostic = Diagnostic {
                        title: "Invalid guard variable".to_string(),
                        label: "is not locally defined".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "Variables used in guards must be either defined in the function, or be an
argument to the function. The variable `{}` is not defined locally.",
                        name
                    )
                    .unwrap();
                }

                ExtraVarInAlternativePattern { location, name } => {
                    let diagnostic = Diagnostic {
                        title: "Extra alternative pattern variable".to_string(),
                        label: "has not been previously defined".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "All alternative patterns must define the same variables as the initial
pattern. This variable `{}` has not been previously defined.",
                        name
                    )
                    .unwrap();
                }

                DuplicateVarInPattern { location, name } => {
                    let diagnostic = Diagnostic {
                        title: "Duplicate variable in pattern".to_string(),
                        label: "has already been used".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);

                    writeln!(
                        buffer,
                        "Variables can only be used once per pattern. This variable {} appears multiple times.
If you used the same variable twice deliberately in order to check for equality
please use a guard clause instead e.g. (x, y) if x == y -> ...",
                        name
                    )
                    .unwrap();
                }

                OutOfBoundsTupleIndex {
                    location, size: 0, ..
                } => {
                    let diagnostic = Diagnostic {
                        title: "Out of bounds tuple index".to_string(),
                        label: "this index is too large".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "This tuple has no elements so it cannot be indexed at all!"
                    )
                    .unwrap();
                }

                OutOfBoundsTupleIndex {
                    location,
                    index,
                    size,
                } => {
                    let diagnostic = Diagnostic {
                        title: "Out of bounds tuple index".to_string(),
                        label: "this index is too large".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "The index being accessed for this tuple is {}, but this tuple has
{} elements so the highest valid index is {}.",
                        index,
                        size,
                        size - 1,
                    )
                    .unwrap();
                }

                NotATuple { location, given } => {
                    let diagnostic = Diagnostic {
                        title: "Type mismatch".to_string(),
                        label: "is not a tuple".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);
                    let mut printer = Printer::new();

                    writeln!(
                        buffer,
                        "To index into this value it needs to be a tuple, however it has this type:

{}",
                        printer.pretty_print(given, 4),
                    )
                    .unwrap();
                }

                NotATupleUnbound { location } => {
                    let diagnostic = Diagnostic {
                        title: "Type mismatch".to_string(),
                        label: "what type is this?".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);

                    writeln!(
                        buffer,
                        "To index into a tuple we need to know it size, but we don't know anything
about this type yet. Please add some type annotations so we can continue.",
                    )
                    .unwrap();
                }

                RecordAccessUnknownType { location } => {
                    let diagnostic = Diagnostic {
                        title: "Unknown type for record access".to_string(),
                        label: "I don't know what type this is".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Error);

                    writeln!(
                        buffer,
                        "In order to access a record field we need to know what type it is, but
I can't tell the type here. Try adding type annotations to your function
and try again.
",
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
                        let diagnostic = Diagnostic {
                            title: "Syntax error".to_string(),
                            label: "Unexpected token".to_string(),
                            file: path.to_str().unwrap().to_string(),
                            src: src.to_string(),
                            location: crate::ast::SrcSpan {
                                start: *start,
                                end: *end,
                            },
                        };
                        write(buffer, diagnostic, Severity::Error);
                        writeln!(buffer, "Expected one of {}", expected.join(", "))
                            .expect("error pretty buffer write");
                    }

                    UnrecognizedEOF { .. } => {
                        let diagnostic = Diagnostic {
                            title: "Syntax error".to_string(),
                            label: "Unexpected end of file".to_string(),
                            file: path.to_str().unwrap().to_string(),
                            src: src.to_string(),
                            location: crate::ast::SrcSpan {
                                start: src.len() - 2,
                                end: src.len() - 1,
                            },
                        };
                        write(buffer, diagnostic, Severity::Error);
                    }

                    InvalidToken { location } => {
                        let diagnostic = Diagnostic {
                            title: "Syntax error".to_string(),
                            label: "Unknown token".to_string(),
                            file: path.to_str().unwrap().to_string(),
                            src: src.to_string(),
                            location: crate::ast::SrcSpan {
                                start: *location,
                                end: *location + 1,
                            },
                        };
                        write(buffer, diagnostic, Severity::Error);
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
                            Error::TooManyHolesInCapture { location, count } => {
                                let diagnostic = Diagnostic {
                                    title: "Invalid capture".to_string(),
                                    label: "".to_string(),
                                    file: path.to_str().unwrap().to_string(),
                                    src: src.to_string(),
                                    location: location.clone(),
                                };
                                write(buffer, diagnostic, Severity::Error);
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
                location,
                path,
                src,
                modules,
            } => {
                let mut modules = modules.clone();
                let diagnostic = Diagnostic {
                    title: "Unknown import".to_string(),
                    label: did_you_mean(import, &mut modules, ""),
                    file: path.to_str().unwrap().to_string(),
                    src: src.to_string(),
                    location: location.clone(),
                };
                write(buffer, diagnostic, Severity::Error);
                writeln!(
                    buffer,
                    "The module `{}` is trying to import the module `{}`,
but it cannot be found.",
                    module, import
                )
                .expect("error pretty buffer write");
            }

            Error::StandardIO { action, err } => {
                let err = match err {
                    Some(e) => format!(
                        "\nThe error message from the stdio library was:\n\n    {}\n",
                        std_io_error_kind_text(e)
                    ),
                    None => "".to_string(),
                };
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Standard IO failure".to_string(),
                    label: format!(
                        "An error occurred while trying to {}:

{}",
                        action.text(),
                        err,
                    ),
                };
                write_project(buffer, diagnostic);
            }
            Error::Format { problem_files } => {
                let mut files: Vec<_> = problem_files
                    .iter()
                    .flat_map(|formatted| formatted.source.to_str())
                    .map(|p| format!("  - {}", p))
                    .collect();
                files.sort();
                let mut label = files.iter().join("\n");
                label.push('\n');
                let diagnostic = ProjectErrorDiagnostic {
                    title: "These files have not been formatted".to_string(),
                    label,
                };

                write_project(buffer, diagnostic);
            }
        }
    }

    pub fn pretty_print(&self) {
        let buffer_writer = buffer_writer();
        let mut buffer = buffer_writer.buffer();
        self.pretty(&mut buffer);
        buffer_writer.print(&buffer).unwrap();
    }
}

fn std_io_error_kind_text(kind: &std::io::ErrorKind) -> String {
    use std::io::ErrorKind::*;
    match kind {
        NotFound => "Could not find the stdio stream".to_string(),
        PermissionDenied => "Permission was denied".to_string(),
        ConnectionRefused => "Connection was refused".to_string(),
        ConnectionReset => "Connection was reset".to_string(),
        ConnectionAborted => "Connection was aborted".to_string(),
        NotConnected => "Was not connected".to_string(),
        AddrInUse => "The stream was already in use".to_string(),
        AddrNotAvailable => "The stream was not available".to_string(),
        BrokenPipe => "The pipe was broken".to_string(),
        AlreadyExists => "A handle to the stream already exists".to_string(),
        WouldBlock => "This operation would block when it was requested not to".to_string(),
        InvalidInput => "Some parameter was invalid".to_string(),
        InvalidData => "The data was invalid.  Check that the encoding is UTF-8".to_string(),
        TimedOut => "The operation timed out".to_string(),
        WriteZero => "An attempt was made to write, but all bytes could not be written".to_string(),
        Interrupted => "The operation was interrupted".to_string(),
        UnexpectedEof => "The end of file was reached before it was expected".to_string(),
        _ => "An unknown error occurred".to_string(),
    }
}

fn import_cycle(buffer: &mut Buffer, modules: &[Vec<String>]) {
    use std::io::Write;
    use termcolor::{Color, ColorSpec, WriteColor};
    crate::diagnostic::write_title(buffer, "Import cycle");
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
