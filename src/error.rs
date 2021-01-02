use crate::parse::error::ParseErrorType;
use crate::{
    cli,
    diagnostic::{
        write, write_diagnostic, write_project, Diagnostic, DiagnosticLabel, LabelStyle,
        MultiLineDiagnostic, ProjectErrorDiagnostic, Severity,
    },
    typ::pretty::Printer,
};
use itertools::Itertools;
use std::fmt::Debug;
use std::path::PathBuf;
use termcolor::Buffer;

pub type Src = String;
pub type Name = String;

pub fn fatal_compiler_bug(msg: &str) -> ! {
    let buffer_writer = cli::stderr_buffer_writer();
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

pub type Result<Ok, Err = Error> = std::result::Result<Ok, Err>;

#[derive(Debug, PartialEq)]
pub enum Error {
    Parse {
        path: PathBuf,
        src: Src,
        error: crate::parse::error::ParseError,
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
        modules: Vec<String>,
    },

    PackageCycle {
        packages: Vec<String>,
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
        problem_files: Vec<crate::format::command::Unformatted>,
    },

    Hex(String),

    Tar {
        path: PathBuf,
        err: String,
    },

    TarFinish(String),

    Gzip(String),

    ShellCommand {
        command: String,
        err: Option<std::io::ErrorKind>,
    },

    InvalidProjectName {
        name: String,
        reason: InvalidProjectNameReason,
    },

    UnableToFindProjectRoot {
        path: String,
    },

    VersionDoesNotMatch {
        toml_ver: String,
        app_ver: String,
    },
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum InvalidProjectNameReason {
    Format,
    ErlangReservedWord,
    ErlangStandardLibraryModule,
    GleamReservedWord,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FileIOAction {
    Open,
    Copy,
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
            FileIOAction::Copy => "copy",
            FileIOAction::Read => "read",
            FileIOAction::Parse => "parse",
            FileIOAction::Delete => "delete",
            FileIOAction::Create => "create",
            FileIOAction::WriteTo => "write to",
            FileIOAction::FindParent => "find the parent of",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
        use crate::typ::Error as TypeError;
        use std::io::Write;

        buffer
            .write_all(b"\n")
            .expect("error pretty buffer write space before");

        match self {
            Error::InvalidProjectName { name, reason } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Invalid project name".to_string(),
                    label: format!(
                        "We were not able to create your project as `{}` is
{}

Please try again with a different project name.",
                        name,
                        match reason {
                            InvalidProjectNameReason::ErlangReservedWord =>
                                "a reserved word in Erlang.",
                            InvalidProjectNameReason::ErlangStandardLibraryModule =>
                                "a standard library module in Erlang.",
                            InvalidProjectNameReason::GleamReservedWord =>
                                "a reserved word in Gleam.",
                            InvalidProjectNameReason::Format =>
                                "does not have the correct format. Project names must start
with a lowercase letter and may only contain lowercase letters
and underscores.",
                        }
                    ),
                };
                write_project(buffer, diagnostic);
            }
            Error::UnableToFindProjectRoot { path } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Invalid project root".to_string(),
                    label: format!("We were unable to find the project root:\n\n  {}", path),
                };
                write_project(buffer, diagnostic);
            }
            Error::VersionDoesNotMatch { toml_ver, app_ver } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Version does not match".to_string(),
                    label:
                        format!("The version in gleam.toml \"{}\" does not match the version in your app.src file \"{}\"", toml_ver, app_ver),
                };
                write_project(buffer, diagnostic);
            }
            Error::ShellCommand { command, err: None } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Shell command failure".to_string(),
                    label: format!(
                        "There was a problem when running the shell command `{}`.",
                        command
                    ),
                };
                write_project(buffer, diagnostic);
            }

            Error::ShellCommand {
                command,
                err: Some(err),
            } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Shell command failure".to_string(),
                    label: format!(
                        "There was a problem when running the shell command `{}`.

The error from the shell command library was:

    {}",
                        command,
                        std_io_error_kind_text(err)
                    ),
                };
                write_project(buffer, diagnostic);
            }

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
                    location: *location,
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
                TypeError::UnknownLabels {
                    unknown,
                    valid,
                    supplied,
                } => {
                    let mut other_labels = valid
                        .iter()
                        .cloned()
                        .filter(|label| !supplied.contains(label))
                        .collect();

                    let title = if unknown.len() > 1 {
                        "Unknown labels"
                    } else {
                        "Unknown label"
                    };

                    let diagnostic = MultiLineDiagnostic {
                        title: title.to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        labels: unknown
                            .iter()
                            .map(|(label, location)| DiagnosticLabel {
                                label: did_you_mean(label, &mut other_labels, "Unexpected label"),
                                location: *location,
                                style: LabelStyle::Primary,
                            })
                            .collect(),
                    };
                    write_diagnostic(buffer, diagnostic, Severity::Error);

                    if valid.is_empty() {
                        writeln!(
                            buffer,
                            "This constructor does not accept any labelled arguments."
                        )
                        .unwrap();
                    } else if other_labels.is_empty() {
                        writeln!(
                                buffer,
                                "You have already supplied all the labelled arguments that this constructor accepts."
                            )
                            .unwrap();
                    } else {
                        writeln!(
                            buffer,
                            "The other labelled arguments that this constructor accepts are `{}`.",
                            other_labels.iter().join("`, `")
                        )
                        .unwrap();
                    }
                }

                TypeError::UnexpectedLabelledArg { location, label } => {
                    let diagnostic = Diagnostic {
                        title: "Unexpected labelled argument".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
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

                TypeError::PositionalArgumentAfterLabelled { location } => {
                    let diagnostic = Diagnostic {
                        title: "Unexpected positional argument".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
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

                TypeError::DuplicateName {
                    location,
                    name: fun,
                    previous_location,
                    ..
                } => {
                    let diagnostic = MultiLineDiagnostic {
                        title: format!("Duplicate function definition with name `{}`", fun),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        labels: vec![
                            DiagnosticLabel {
                                label: "redefined here".to_string(),
                                location: *location,
                                style: LabelStyle::Primary,
                            },
                            DiagnosticLabel {
                                label: "previously defined here".to_string(),
                                location: previous_location.clone(),
                                style: LabelStyle::Secondary,
                            },
                        ],
                    };
                    write_diagnostic(buffer, diagnostic, Severity::Error);
                }

                TypeError::DuplicateTypeName {
                    name,
                    location,
                    previous_location,
                    ..
                } => {
                    let diagnostic = MultiLineDiagnostic {
                        title: format!("Duplicate type definition with name `{}`", name),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        labels: vec![
                            DiagnosticLabel {
                                label: "redefined here".to_string(),
                                location: *location,
                                style: LabelStyle::Primary,
                            },
                            DiagnosticLabel {
                                label: "previously defined here".to_string(),
                                location: previous_location.clone(),
                                style: LabelStyle::Secondary,
                            },
                        ],
                    };
                    write_diagnostic(buffer, diagnostic, Severity::Error);
                }

                TypeError::DuplicateField { location, label } => {
                    let diagnostic = Diagnostic {
                        title: "Duplicate field".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "The field `{}` has already been defined. Rename this field.",
                        label
                    )
                    .unwrap();
                }

                TypeError::DuplicateArgument { location, label } => {
                    let diagnostic = Diagnostic {
                        title: "Duplicate argument".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "The labelled argument `{}` has already been supplied.",
                        label
                    )
                    .unwrap();
                }

                TypeError::RecursiveType { location } => {
                    let diagnostic = Diagnostic {
                        title: "Recursive type".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                }

                TypeError::NotFn { location, typ } => {
                    let diagnostic = Diagnostic {
                        title: "Type mismatch".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
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

                TypeError::UnknownField {
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
                        location: *location,
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

                TypeError::CouldNotUnify {
                    location,
                    expected,
                    given,
                } => {
                    let diagnostic = Diagnostic {
                        title: "Type mismatch".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
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

                TypeError::IncorrectTypeArity {
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
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                }

                TypeError::IncorrectArity {
                    labels,
                    location,
                    expected,
                    given,
                } => {
                    let diagnostic = Diagnostic {
                        title: "Incorrect arity".to_string(),
                        label: format!("expected {} arguments, got {}", expected, given),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    if !labels.is_empty() {
                        let labels = labels
                            .iter()
                            .map(|p| format!("  - {}", p))
                            .sorted()
                            .join("\n");
                        writeln!(
                            buffer,
                            "This call accepts these additional labelled arguments:\n\n{}\n",
                            labels,
                        )
                        .unwrap();
                    }
                }

                TypeError::UnnecessarySpreadOperator { location, arity } => {
                    let diagnostic = Diagnostic {
                        title: "Unnecessary spread operator".to_string(),
                        label: format!(""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);

                    writeln!(
                        buffer,
                        "This record has {} fields and you have already assigned variables to all of them.",
                        arity
                    )
                    .unwrap();
                }

                TypeError::UnknownType {
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
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "The type `{}` is not defined or imported in this module.",
                        name
                    )
                    .unwrap();
                }

                TypeError::UnknownVariable {
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
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(buffer, "The name `{}` is not in scope here.", name).unwrap();
                }

                TypeError::PrivateTypeLeak { location, leaked } => {
                    let diagnostic = Diagnostic {
                        title: "Private type used in public interface".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
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

                TypeError::UnknownModule {
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
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "No module has been imported with the name `{}`.",
                        name
                    )
                    .unwrap();
                }

                TypeError::UnknownModuleType {
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
                        location: *location,
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

                TypeError::UnknownModuleValue {
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
                        location: *location,
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

                TypeError::UnknownModuleField {
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
                        location: *location,
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

                TypeError::IncorrectNumClausePatterns {
                    location,
                    expected,
                    given,
                } => {
                    let diagnostic = Diagnostic {
                        title: "Incorrect number of patterns".to_string(),
                        label: format!("expected {} patterns, got {}", expected, given),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
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

                TypeError::NonLocalClauseGuardVariable { location, name } => {
                    let diagnostic = Diagnostic {
                        title: "Invalid guard variable".to_string(),
                        label: "is not locally defined".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
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

                TypeError::ExtraVarInAlternativePattern { location, name } => {
                    let diagnostic = Diagnostic {
                        title: "Extra alternative pattern variable".to_string(),
                        label: "has not been previously defined".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
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

                TypeError::DuplicateVarInPattern { location, name } => {
                    let diagnostic = Diagnostic {
                        title: "Duplicate variable in pattern".to_string(),
                        label: "has already been used".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
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

                TypeError::OutOfBoundsTupleIndex {
                    location, size: 0, ..
                } => {
                    let diagnostic = Diagnostic {
                        title: "Out of bounds tuple index".to_string(),
                        label: "this index is too large".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "This tuple has no elements so it cannot be indexed at all!"
                    )
                    .unwrap();
                }

                TypeError::OutOfBoundsTupleIndex {
                    location,
                    index,
                    size,
                } => {
                    let diagnostic = Diagnostic {
                        title: "Out of bounds tuple index".to_string(),
                        label: "this index is too large".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
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

                TypeError::NotATuple { location, given } => {
                    let diagnostic = Diagnostic {
                        title: "Type mismatch".to_string(),
                        label: "is not a tuple".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
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

                TypeError::NotATupleUnbound { location } => {
                    let diagnostic = Diagnostic {
                        title: "Type mismatch".to_string(),
                        label: "what type is this?".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);

                    writeln!(
                        buffer,
                        "To index into a tuple we need to know it size, but we don't know anything
about this type yet. Please add some type annotations so we can continue.",
                    )
                    .unwrap();
                }

                TypeError::RecordAccessUnknownType { location } => {
                    let diagnostic = Diagnostic {
                        title: "Unknown type for record access".to_string(),
                        label: "I don't know what type this is".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
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

                TypeError::ConflictingBinaryTypeOptions { location, name, .. } => {
                    let diagnostic = Diagnostic {
                        title: "Duplicate bit string type option".to_string(),
                        label: "given here".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(buffer, "This segment already has the type {}", name).unwrap();
                }

                TypeError::ConflictingBinarySignednessOptions { location, name, .. } => {
                    let diagnostic = Diagnostic {
                        title: "Duplicate bit string signedness".to_string(),
                        label: "redefined here".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(buffer, "This segment already has a signedness of {}", name).unwrap();
                }

                TypeError::ConflictingBinaryEndiannessOptions { location, name, .. } => {
                    let diagnostic = Diagnostic {
                        title: "Duplicate bit string endianness".to_string(),
                        label: "redefined here".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(buffer, "This segment already has an endianness of {}", name).unwrap();
                }

                TypeError::ConflictingBinarySizeOptions { location, .. } => {
                    let diagnostic = Diagnostic {
                        title: "Duplicate bit string size".to_string(),
                        label: "redefined here".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(buffer, "This segment already has a size",).unwrap();
                }

                TypeError::ConflictingBinaryUnitOptions { location, .. } => {
                    let diagnostic = Diagnostic {
                        title: "Duplicate bit string unit".to_string(),
                        label: "redefined here".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(buffer, "This segment already has a unit",).unwrap();
                }

                TypeError::BinaryTypeDoesNotAllowUnit { location, typ, .. } => {
                    let diagnostic = Diagnostic {
                        title: "Unit cannot be specified for given type".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "No unit specifier must be given for the types utf8, utf16, and utf32.
This segment has a type of {}.",
                        typ
                    )
                    .unwrap();
                }

                TypeError::BinarySegmentMustHaveSize { location, .. } => {
                    let diagnostic = Diagnostic {
                        title: "Bit string segment without required size".to_string(),
                        label: "specified here".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "Bit string segments without a size are only allowed
at the end of a bin pattern",
                    )
                    .unwrap();
                }

                TypeError::InvalidBinarySegmentOption { label, location } => {
                    let diagnostic = Diagnostic {
                        title: "Invalid bit string segment option".to_string(),
                        label: "specified here".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);
                    writeln!(
                        buffer,
                        "{} is not a valid option for a bit string segment.
Valid options are: binary, int, float, bit_string,
utf8, utf16, utf32, utf8_codepoint, utf16_codepoint, utf32_codepoint,
signed, unsigned, big, little, native, size, unit",
                        label
                    )
                    .unwrap();
                }

                TypeError::RecordUpdateInvalidConstructor { location } => {
                    let diagnostic = Diagnostic {
                        title: "Invalid record constructor".to_string(),
                        label: "This is not a record constructor".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);

                    writeln!(
                        buffer,
                        "You are attempting to update a record using a value that is not a record constructor",
                    )
                    .unwrap();
                }

                TypeError::UnexpectedTypeHole { location } => {
                    let diagnostic = Diagnostic {
                        title: "Unexpected type hole".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);

                    writeln!(
                        buffer,
                        "We need to know the exact type here so type holes are not permitted.",
                    )
                    .unwrap();
                }

                TypeError::UTFVarInBitStringSegment { location, option } => {
                    let diagnostic = Diagnostic {
                        title: "Incorrect type specifier in bit string segment".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buffer, diagnostic, Severity::Error);

                    writeln!(
                        buffer,
                        "The `{}` type specifier can only be used for construction e.g. <<\"hello\":{}>>.
When matching you need to use the `{}_codepoint` specifier instead.",
                        option,
                        option,
                        option,
                    )
                    .unwrap();
                }
            },

            Error::Parse { path, src, error } => {
                let crate::parse::error::ParseError { location, error } = error;

                let (label, extra) = match error {
                    ParseErrorType::ExpectedExpr => (
                        "I was expecting an expression after this.",
                        vec![]
                    ),
                    ParseErrorType::ExpectedName => (
                        "I was expecting a name here.",
                        vec![]
                    ),
                    ParseErrorType::ExpectedPattern => (
                        "I was expecting a pattern after this.",
                        vec!["See: https://gleam.run/book/tour/patterns".to_string()]
                    ),
                    ParseErrorType::ExpectedType => (
                        "I was expecting a type after this.",
                        vec!["See: https://gleam.run/book/tour/type-annotations".to_string()]
                    ),
                    ParseErrorType::ExpectedUpName => (
                        "I was expecting a type name here.",
                        vec![]
                    ),
                    ParseErrorType::ExpectedValue => (
                        "I was expecting a value after this.",
                        vec!["See: https://gleam.run/book/tour/patterns".to_string()]
                    ),
                    ParseErrorType::ExtraSeparator => (
                        "This is an extra delimiter.",
                        vec!["Hint: Try removing it?".to_string()]
                    ),
                    ParseErrorType::ExprLparStart=> (
                        "This paren cannot be understood here.",
                        vec!["Hint: To group expressions in gleam use \"{\" and \"}\"".to_string()]
                    ),
                    ParseErrorType::ExprTailBinding=> (
                        "A variable binding cannot be the last expression.",
                        vec!["Hint: Try using the value?".to_string()]
                    ),
                    ParseErrorType::IncorrectName => (
                        "I'm expecting a lowercase name here.",
                        vec![ "Hint: Variable and module names start with a lowercase letter, and can contain a-z, 0-9, or _.".to_string()]
                    ),
                    ParseErrorType::IncorrectUpName => (
                        "I'm expecting a type name here.",
                        vec![ "Hint: Type names start with a uppercase letter, and can contain a-z, A-Z, or 0-9.".to_string()]
                    ),
                    ParseErrorType::InvalidBitStringSegment => (
                        "This is not a valid BitString segment option.",
                        vec![ "Hint: Valid BitString segment options are:".to_string(),
                        "binary, int, float, bit_string, utf8, utf16, utf32, utf8_codepoint, utf16_codepoint".to_string(),
                        "utf32_codepoint, signed, unsigned, big, little, native, size, unit".to_string(),
                        "See: https://gleam.run/book/tour/bit-strings".to_string()]
                    ),
                    ParseErrorType::InvalidTailPattern => (
                        "This part of a list pattern can only be a name or a discard.",
                        vec!["See: https://gleam.run/book/tour/patterns".to_string()]
                    ),
                    ParseErrorType::InvalidTupleAccess => (
                        "This integer is not valid for tuple access.",
                        vec!["Hint: Only non negative integer literals like 0, or 1_000 can be used.".to_string()]
                    ),
                    ParseErrorType::LexError { error: lex_err } => lex_err.to_parse_error_info(),
                    ParseErrorType::ListNilNotAllowed=> (
                        "Hint: Empty list is not allowed here.",
                        vec![]
                    ),
                    ParseErrorType::NoCaseClause => (
                        "This case expression has no clauses.",
                        vec!["See: https://gleam.run/book/tour/case-expressions".to_string()]
                    ),
                    ParseErrorType::NoConstructors => (
                        "Custom types must have at least 1 constructor.",
                        vec!["See: https://gleam.run/book/tour/custom-types".to_string()]
                    ),
                    ParseErrorType::NotConstType => (
                        "This type is not allowed in module constants.",
                        vec!["See: https://gleam.run/book/tour/constants".to_string()]
                    ),
                    ParseErrorType::NoExpression=> (
                        "There must be an expression in here.",
                        vec!["Hint: Put an expression in there or remove the brackets.".to_string()]
                    ),
                    ParseErrorType::NoValueAfterEqual => (
                        "I was expecting to see a value after this equals sign.",
                        vec![]
                    ),
                    ParseErrorType::OpaqueTypeAlias => (
                        "Type Aliases cannot be opaque",
                        vec!["See: https://gleam.run/book/tour/type-aliases".to_string()]
                    ),
                    ParseErrorType::OpNakedRight => (
                        "This operator has no value on its right side.",
                        vec!["Hint: Remove it or put a value after it.".to_string()]
                    ),
                    ParseErrorType::TooManyArgHoles => (
                        "There is more than 1 argument hole in this function call.",
                        vec!["Hint: Function calls can have at most one argument hole.".to_string(),
                             "See: https://gleam.run/book/tour/functions".to_string()
                            ]
                    ),
                    ParseErrorType::UnexpectedEOF => (
                        "The module ended unexpectedly.",
                        vec![]
                    ),
                    ParseErrorType::UnexpectedReservedWord => (
                        "This is a reserved word.",
                        vec!["Hint: I was expecting to see a name here.".to_string(), "See: https://gleam.run/book/tour/reserved-words".to_string()]
                    ),
                    ParseErrorType::UnexpectedToken { expected } => {
                        let mut messages = expected.clone();
                        messages
                            .first_mut()
                            .map(|s| *s = format!("Expected one of: {}", *s));

                        ( "I was not expecting this.",
                          messages
                        )
                    }
                };

                let adjusted_location = if error == &ParseErrorType::UnexpectedEOF {
                    crate::ast::SrcSpan {
                        start: src.len() - 1,
                        end: src.len() - 1,
                    }
                } else {
                    *location
                };

                let diagnostic = Diagnostic {
                    title: "Syntax error".to_string(),
                    label: label.to_string(),
                    location: adjusted_location,
                    file: path.to_str().unwrap().to_string(),
                    src: src.to_string(),
                };
                write(buffer, diagnostic, Severity::Error);
                if !extra.is_empty() {
                    writeln!(buffer, "{}", extra.join("\n")).expect("error pretty buffer write");
                }
            }

            Error::ImportCycle { modules } => {
                crate::diagnostic::write_title(buffer, "Import cycle");
                writeln!(
                    buffer,
                    "The import statements for these modules form a cycle:\n"
                )
                .unwrap();
                import_cycle(buffer, modules.as_ref());

                writeln!(
                    buffer,
                    "Gleam doesn't support import cycles like these, please break the
cycle to continue."
                )
                .unwrap();
            }

            Error::PackageCycle { packages } => {
                crate::diagnostic::write_title(buffer, "Dependency cycle");
                writeln!(
                    buffer,
                    "The dependencies for these packages form a cycle:\n"
                )
                .unwrap();
                import_cycle(buffer, packages.as_ref());
                writeln!(
                    buffer,
                    "Gleam doesn't support dependency cycles like these, please break the
cycle to continue."
                )
                .unwrap();
            }

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
                    location: *location,
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
                let files: Vec<_> = problem_files
                    .iter()
                    .flat_map(|formatted| formatted.source.to_str())
                    .map(|p| format!("  - {}", p))
                    .sorted()
                    .collect();
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
        let buffer_writer = cli::stderr_buffer_writer();
        let mut buffer = buffer_writer.buffer();
        self.pretty(&mut buffer);
        buffer_writer.print(&buffer).unwrap();
    }
}

fn std_io_error_kind_text(kind: &std::io::ErrorKind) -> String {
    use std::io::ErrorKind;
    match kind {
        ErrorKind::NotFound => "Could not find the stdio stream".to_string(),
        ErrorKind::PermissionDenied => "Permission was denied".to_string(),
        ErrorKind::ConnectionRefused => "Connection was refused".to_string(),
        ErrorKind::ConnectionReset => "Connection was reset".to_string(),
        ErrorKind::ConnectionAborted => "Connection was aborted".to_string(),
        ErrorKind::NotConnected => "Was not connected".to_string(),
        ErrorKind::AddrInUse => "The stream was already in use".to_string(),
        ErrorKind::AddrNotAvailable => "The stream was not available".to_string(),
        ErrorKind::BrokenPipe => "The pipe was broken".to_string(),
        ErrorKind::AlreadyExists => "A handle to the stream already exists".to_string(),
        ErrorKind::WouldBlock => {
            "This operation would block when it was requested not to".to_string()
        }
        ErrorKind::InvalidInput => "Some parameter was invalid".to_string(),
        ErrorKind::InvalidData => {
            "The data was invalid.  Check that the encoding is UTF-8".to_string()
        }
        ErrorKind::TimedOut => "The operation timed out".to_string(),
        ErrorKind::WriteZero => {
            "An attempt was made to write, but all bytes could not be written".to_string()
        }
        ErrorKind::Interrupted => "The operation was interrupted".to_string(),
        ErrorKind::UnexpectedEof => {
            "The end of file was reached before it was expected".to_string()
        }
        _ => "An unknown error occurred".to_string(),
    }
}

fn import_cycle(buffer: &mut Buffer, modules: &[String]) {
    use std::io::Write;
    use termcolor::{Color, ColorSpec, WriteColor};

    writeln!(
        buffer,
        "
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
        writeln!(buffer, "{}", name).unwrap();
        buffer.set_color(&ColorSpec::new()).unwrap();
    }
    writeln!(buffer, "    └─────┘\n").unwrap();
}
