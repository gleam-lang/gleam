#![allow(clippy::unwrap_used, clippy::expect_used)]
use crate::{ast::BinOp, parse::error::ParseErrorType, type_::Type};
use crate::{
    bit_string,
    diagnostic::{
        write, write_diagnostic, write_project, Diagnostic, DiagnosticLabel, LabelStyle,
        MultiLineDiagnostic, ProjectErrorDiagnostic, Severity,
    },
    javascript,
    type_::{pretty::Printer, UnifyErrorSituation},
};
use itertools::Itertools;
use std::fmt::Debug;
use std::path::PathBuf;
use termcolor::Buffer;

pub type Src = String;
pub type Name = String;

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
        error: crate::type_::Error,
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

    FileIo {
        kind: FileKind,
        action: FileIoAction,
        path: PathBuf,
        err: Option<String>,
    },

    StandardIo {
        action: StandardIoAction,
        err: Option<std::io::ErrorKind>,
    },

    Format {
        problem_files: Vec<Unformatted>,
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

    InvalidVersionFormat {
        input: String,
        error: String,
    },

    ProjectRootAlreadyExist {
        path: String,
    },

    UnableToFindProjectRoot {
        path: String,
    },

    VersionDoesNotMatch {
        toml_ver: String,
        app_ver: String,
    },

    MetadataDecodeError {
        error: Option<String>,
    },

    ForbiddenWarnings {
        count: usize,
    },

    JavaScript {
        path: PathBuf,
        src: Src,
        error: crate::javascript::Error,
    },

    DownloadPackageError {
        package_name: String,
        package_version: String,
        error: String,
    },

    Http(String),
}

impl Error {
    pub fn http<E>(error: E) -> Error
    where
        E: std::error::Error,
    {
        Self::Http(error.to_string())
    }

    pub fn hex<E>(error: E) -> Error
    where
        E: std::error::Error,
    {
        Self::Hex(error.to_string())
    }
}

impl From<capnp::Error> for Error {
    fn from(error: capnp::Error) -> Self {
        Error::MetadataDecodeError {
            error: Some(error.to_string()),
        }
    }
}

impl From<capnp::NotInSchema> for Error {
    fn from(error: capnp::NotInSchema) -> Self {
        Error::MetadataDecodeError {
            error: Some(error.to_string()),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum InvalidProjectNameReason {
    Format,
    ErlangReservedWord,
    ErlangStandardLibraryModule,
    GleamReservedWord,
    GleamReservedModule,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum StandardIoAction {
    Read,
}

impl StandardIoAction {
    fn text(&self) -> &'static str {
        match self {
            StandardIoAction::Read => "read from",
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FileIoAction {
    Open,
    Copy,
    Read,
    Parse,
    Delete,
    Create,
    WriteTo,
    FindParent,
}

impl FileIoAction {
    fn text(&self) -> &'static str {
        match self {
            FileIoAction::Open => "open",
            FileIoAction::Copy => "copy",
            FileIoAction::Read => "read",
            FileIoAction::Parse => "parse",
            FileIoAction::Delete => "delete",
            FileIoAction::Create => "create",
            FileIoAction::WriteTo => "write to",
            FileIoAction::FindParent => "find the parent of",
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

fn did_you_mean(name: &str, options: &[String], alt: &'static str) -> String {
    // Find best match
    options
        .iter()
        .filter(|&option| option != crate::ast::CAPTURE_VARIABLE)
        .sorted()
        .min_by_key(|option| strsim::levenshtein(option, name))
        .map(|option| format!("did you mean `{}`?", option))
        .unwrap_or_else(|| alt.to_string())
}

impl Error {
    pub fn pretty_string(&self) -> String {
        let mut nocolor = Buffer::no_color();
        self.pretty(&mut nocolor);
        String::from_utf8(nocolor.into_inner()).expect("Error printing produced invalid utf8")
    }
    pub fn pretty(&self, buf: &mut Buffer) {
        use crate::type_::Error as TypeError;
        use std::io::Write;

        buf.write_all(b"\n")
            .expect("error pretty buffer write space before");

        match self {
            Error::MetadataDecodeError { error } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Failed to decode module metadata".to_string(),
                    label: "A problem was encountered when decoding the metadata for one 
of the Gleam dependency modules."
                        .to_string(),
                };
                write_project(buf, diagnostic);
                if let Some(error) = error {
                    writeln!(
                        buf,
                        "\nThe error from the decoder library was:

    {}",
                        error
                    )
                    .unwrap();
                }
            }

            Error::InvalidProjectName { name, reason } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Invalid project name".to_string(),
                    label: format!(
                        "We were not able to create your project as `{}`
{}

Please try again with a different project name.",
                        name,
                        match reason {
                            InvalidProjectNameReason::ErlangReservedWord =>
                                "is a reserved word in Erlang.",
                            InvalidProjectNameReason::ErlangStandardLibraryModule =>
                                "is a standard library module in Erlang.",
                            InvalidProjectNameReason::GleamReservedWord =>
                                "is a reserved word in Gleam.",
                            InvalidProjectNameReason::GleamReservedModule =>
                                "is a reserved module name in Gleam.",
                            InvalidProjectNameReason::Format =>
                                "does not have the correct format. Project names must start
with a lowercase letter and may only contain lowercase letters,
numbers and underscores.",
                        }
                    ),
                };
                write_project(buf, diagnostic);
            }

            Error::ProjectRootAlreadyExist { path } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Project folder already exists".to_string(),
                    label: format!("Project folder root:\n\n  {}", path),
                };
                write_project(buf, diagnostic);
            }

            Error::UnableToFindProjectRoot { path } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Invalid project root".to_string(),
                    label: format!("We were unable to find the project root:\n\n  {}", path),
                };
                write_project(buf, diagnostic);
            }

            Error::VersionDoesNotMatch { toml_ver, app_ver } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Version does not match".to_string(),
                    label: format!(
                        "The version in gleam.toml \"{}\" does not match the version 
in your app.src file \"{}\"",
                        toml_ver, app_ver
                    ),
                };
                write_project(buf, diagnostic);
            }

            Error::ShellCommand { command, err: None } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Shell command failure".to_string(),
                    label: format!(
                        "There was a problem when running the shell command `{}`.",
                        command
                    ),
                };
                write_project(buf, diagnostic);
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
                write_project(buf, diagnostic);
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
                write_project(buf, diagnostic);
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
                write_project(buf, diagnostic);
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
                write_project(buf, diagnostic);
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
                write_project(buf, diagnostic);
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
                write(buf, diagnostic, Severity::Error);
                writeln!(
                    buf,
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
                write_project(buf, diagnostic);
            }

            Error::FileIo {
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
                write_project(buf, diagnostic);
            }

            Error::Type { path, src, error } => match error {
                TypeError::UnknownLabels {
                    unknown,
                    valid,
                    supplied,
                } => {
                    let other_labels: Vec<String> = valid
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
                                label: did_you_mean(label, &other_labels, "Unexpected label"),
                                location: *location,
                                style: LabelStyle::Primary,
                            })
                            .collect(),
                    };
                    write_diagnostic(buf, diagnostic, Severity::Error);

                    if valid.is_empty() {
                        writeln!(
                            buf,
                            "This constructor does not accept any labelled arguments."
                        )
                        .unwrap();
                    } else if other_labels.is_empty() {
                        writeln!(
                                buf,
                                "You have already supplied all the labelled arguments that this constructor accepts."
                            )
                            .unwrap();
                    } else {
                        writeln!(
                            buf,
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
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
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
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
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
                                location: *previous_location,
                                style: LabelStyle::Secondary,
                            },
                        ],
                    };
                    write_diagnostic(buf, diagnostic, Severity::Error);
                }

                TypeError::DuplicateImport {
                    location,
                    previous_location,
                    name,
                    ..
                } => {
                    let diagnostic = MultiLineDiagnostic {
                        title: format!("Duplicate import with name `{}`", name),
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
                                location: *previous_location,
                                style: LabelStyle::Secondary,
                            },
                        ],
                    };
                    write_diagnostic(buf, diagnostic, Severity::Error);
                }

                TypeError::DuplicateConstName {
                    location,
                    name,
                    previous_location,
                    ..
                } => {
                    let diagnostic = MultiLineDiagnostic {
                        title: format!("Duplicate const definition with name `{}`", name),
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
                                location: *previous_location,
                                style: LabelStyle::Secondary,
                            },
                        ],
                    };
                    write_diagnostic(buf, diagnostic, Severity::Error);
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
                                location: *previous_location,
                                style: LabelStyle::Secondary,
                            },
                        ],
                    };
                    write_diagnostic(buf, diagnostic, Severity::Error);
                }

                TypeError::DuplicateField { location, label } => {
                    let diagnostic = Diagnostic {
                        title: "Duplicate field".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
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
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
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
                    write(buf, diagnostic, Severity::Error);
                }

                TypeError::NotFn { location, typ } => {
                    let diagnostic = Diagnostic {
                        title: "Type mismatch".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                    let mut printer = Printer::new();

                    writeln!(
                        buf,
                        "This value is being called as a function but its type is:\n\n{}",
                        printer.pretty_print(typ, 4)
                    )
                    .unwrap();
                }

                TypeError::UnknownRecordField {
                    location,
                    typ,
                    label,
                    fields,
                } => {
                    let diagnostic = Diagnostic {
                        title: "Unknown record field".to_string(),
                        label: did_you_mean(label, fields, "This field does not exist"),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                    let mut printer = Printer::new();

                    writeln!(
                        buf,
                        "The record being updated has this type:

{}
",
                        printer.pretty_print(typ, 4)
                    )
                    .unwrap();

                    if fields.is_empty() {
                        writeln!(buf, "It does not have any fields.",).unwrap();
                    } else {
                        write!(buf, "It has these fields:\n\n").unwrap();
                        for field in fields.iter().sorted() {
                            writeln!(buf, "    .{}", field).unwrap();
                        }
                    }
                }

                TypeError::CouldNotUnify {
                    location,
                    expected,
                    given,
                    situation: Some(UnifyErrorSituation::Operator(op)),
                } => {
                    let diagnostic = Diagnostic {
                        title: "Type mismatch".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                    let mut printer = Printer::new();
                    writeln!(
                        buf,
                        "The {op} operator expects arguments of this type:

{expected}

But this argument has this type:

{given}\n",
                        op = op.name(),
                        expected = printer.pretty_print(expected, 4),
                        given = printer.pretty_print(given, 4),
                    )
                    .unwrap();
                    if let Some(t) = hint_alternative_operator(op, given) {
                        writeln!(buf, "Hint: {}\n", t).unwrap();
                    }
                }

                TypeError::CouldNotUnify {
                    location,
                    expected,
                    given,
                    situation: Some(UnifyErrorSituation::PipeTypeMismatch),
                } => {
                    let diagnostic = Diagnostic {
                        title:
                            "This function cannot handle the argument sent through the (|>) pipe:"
                                .to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };

                    // Remap the pipe function type into just the type expected by the pipe.
                    let expected = expected
                        .fn_types()
                        .and_then(|(args, _)| args.get(0).cloned());

                    // Remap the argument as well, if it's a function.
                    let given = given
                        .fn_types()
                        .and_then(|(args, _)| args.get(0).cloned())
                        .unwrap_or_else(|| given.clone());

                    write(buf, diagnostic, Severity::Error);
                    let mut printer = Printer::new();
                    writeln!(
                        buf,
                        "The argument is:

{given}

But (|>) is piping it to a function that expects:

{expected}

\n",
                        expected = expected
                            .map(|v| printer.pretty_print(&v, 4))
                            .unwrap_or_else(|| "    No arguments".to_string()),
                        given = printer.pretty_print(&given, 4)
                    )
                    .unwrap();
                }

                TypeError::CouldNotUnify {
                    location,
                    expected,
                    given,
                    situation,
                } => {
                    let diagnostic = Diagnostic {
                        title: "Type mismatch".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                    if let Some(description) = situation.and_then(|s| s.description()) {
                        writeln!(buf, "{}\n", description).unwrap();
                    }
                    let mut printer = Printer::new();
                    writeln!(
                        buf,
                        "Expected type:

{}

Found type:

{}\n",
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
                    write(buf, diagnostic, Severity::Error);
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
                    write(buf, diagnostic, Severity::Error);
                    if !labels.is_empty() {
                        let labels = labels
                            .iter()
                            .map(|p| format!("  - {}", p))
                            .sorted()
                            .join("\n");
                        writeln!(
                            buf,
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
                    write(buf, diagnostic, Severity::Error);

                    writeln!(
                        buf,
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
                    let diagnostic = Diagnostic {
                        title: "Unknown type".to_string(),
                        label: did_you_mean(name, types, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
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
                    let diagnostic = Diagnostic {
                        title: "Unknown variable".to_string(),
                        label: did_you_mean(name, variables, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                    writeln!(buf, "The name `{}` is not in scope here.", name).unwrap();
                }

                TypeError::PrivateTypeLeak { location, leaked } => {
                    let diagnostic = Diagnostic {
                        title: "Private type used in public interface".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                    let mut printer = Printer::new();

                    // TODO: be more precise.
                    // - is being returned by this public function
                    // - is taken as an argument by this public function
                    // - is taken as an argument by this public enum constructor
                    // etc
                    writeln!(
                        buf,
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
                    let diagnostic = Diagnostic {
                        title: "Unknown module".to_string(),
                        label: did_you_mean(name, imported_modules, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                    writeln!(buf, "No module has been found with the name `{}`.", name).unwrap();
                }

                TypeError::UnknownModuleType {
                    location,
                    name,
                    module_name,
                    type_constructors,
                } => {
                    let diagnostic = Diagnostic {
                        title: "Unknown module type".to_string(),
                        label: did_you_mean(name, type_constructors, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
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
                    let diagnostic = Diagnostic {
                        title: "Unknown module field".to_string(),
                        label: did_you_mean(name, value_constructors, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
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
                    let options: Vec<String> = type_constructors
                        .iter()
                        .chain(value_constructors)
                        .map(|s| s.to_string())
                        .collect();
                    let diagnostic = Diagnostic {
                        title: "Unknown module field".to_string(),
                        label: did_you_mean(name, &options, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
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
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
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
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
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
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
                        "All alternative patterns must define the same variables as the initial
pattern. This variable `{}` has not been previously defined.",
                        name
                    )
                    .unwrap();
                }

                TypeError::MissingVarInAlternativePattern { location, name } => {
                    let diagnostic = Diagnostic {
                        title: "Missing alternative pattern variable".to_string(),
                        label: "does not define all required variables".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
                        "All alternative patterns must define the same variables as the initial
pattern, but the `{}` variable is missing.",
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
                    write(buf, diagnostic, Severity::Error);

                    writeln!(
                        buf,
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
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
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
                    write(buf, diagnostic, Severity::Error);
                    writeln!(
                        buf,
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
                    write(buf, diagnostic, Severity::Error);
                    let mut printer = Printer::new();

                    writeln!(
                        buf,
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
                    write(buf, diagnostic, Severity::Error);

                    writeln!(
                        buf,
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
                    write(buf, diagnostic, Severity::Error);

                    writeln!(
                        buf,
                        "In order to access a record field we need to know what type it is, but
I can't tell the type here. Try adding type annotations to your function
and try again.
",
                    )
                    .unwrap();
                }

                TypeError::BitStringSegmentError { error, location } => {
                    let (label, mut extra) = match error {
                        bit_string::ErrorType::ConflictingTypeOptions { existing_type } => (
                            "This is an extra type specifier.",
                            vec![format!("Hint: This segment already has the type {}.", existing_type)],
                        ),

                        bit_string::ErrorType::ConflictingSignednessOptions {
                            existing_signed
                        } => (
                            "This is an extra signedness specifier.",
                            vec![format!(
                                "Hint: This segment already has a signedness of {}.",
                                existing_signed
                            )],
                        ),

                        bit_string::ErrorType::ConflictingEndiannessOptions {
                            existing_endianness
                        } => (
                            "This is an extra endianness specifier.",
                            vec![format!(
                                "Hint: This segment already has an endianness of {}.",
                                existing_endianness
                            )],
                        ),

                        bit_string::ErrorType::ConflictingSizeOptions => (
                            "This is an extra size specifier.",
                            vec!["Hint: This segment already has a size.".to_string()],
                        ),

                        bit_string::ErrorType::ConflictingUnitOptions => (
                            "This is an extra unit specifier.",
                            vec!["Hint: A BitString segment can have at most 1 unit.".to_string()],
                        ),

                        bit_string::ErrorType::FloatWithSize => (
                            "Size cannot be used with float.",
                            vec!["Hint: floats have an exact size of 64 bits.".to_string()],
                        ),

                        bit_string::ErrorType::InvalidEndianness => (
                            "this option is invalid here.",
                            vec!["Hint: signed and unsigned can only be used with int, float, utf16 and utf32 types.".to_string()],
                        ),

                        bit_string::ErrorType::OptionNotAllowedInValue => (
                            "This option is only allowed in BitString patterns.",
                            vec!["Hint: This option has no effect in BitString values.".to_string()],
                        ),

                        bit_string::ErrorType::SignednessUsedOnNonInt { typ } => (
                            "signedness is only valid with int types.",
                            vec![format!("Hint: This segment has a type of {}", typ)],
                        ),
                        bit_string::ErrorType::TypeDoesNotAllowSize { typ } => (
                            "size cannot be specified here",
                            vec![format!("Hint: {} segments have an autoatic size.", typ)],
                        ),
                        bit_string::ErrorType::TypeDoesNotAllowUnit { typ } => (
                            "unit cannot be specified here",
                            vec![format!("Hint: {} segments are sized based on their value and cannot have a unit.", typ)],
                        ),
                        bit_string::ErrorType::VariableUtfSegmentInPattern => (
                            "this cannot be a variable",
                            vec!["Hint: in patterns utf8, utf16, and utf32  must be an exact string.".to_string()],
                        ),
                        bit_string::ErrorType::SegmentMustHaveSize => (
                            "This segment has no size",
                            vec!["Hint: Bit string segments without a size are only allowed at the end of a bin pattern.".to_string()],
                        ),
                        bit_string::ErrorType::UnitMustHaveSize => (
                            "This needs an explicit size",
                            vec!["Hint: If you specify unit() you must also specify size().".to_string()],
                        ),
                    };
                    let diagnostic = Diagnostic {
                        title: "BitString Segment Error".to_string(),
                        label: label.to_string(),
                        location: *location,
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                    };
                    extra.push("See: https://gleam.run/book/tour/bit-strings.html".to_string());
                    write(buf, diagnostic, Severity::Error);
                    if !extra.is_empty() {
                        writeln!(buf, "{}", extra.join("\n")).expect("error pretty buffer write");
                    }
                }

                TypeError::RecordUpdateInvalidConstructor { location } => {
                    let diagnostic = Diagnostic {
                        title: "Invalid record constructor".to_string(),
                        label: "This is not a record constructor".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);

                    writeln!(
                        buf,
                        "Only record constructors can be used with the update syntax.",
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
                    write(buf, diagnostic, Severity::Error);

                    writeln!(
                        buf,
                        "We need to know the exact type here so type holes are not permitted.",
                    )
                    .unwrap();
                }

                TypeError::ReservedModuleName { name } => {
                    let diagnostic = ProjectErrorDiagnostic {
                        title: "Reserved module name".to_string(),
                        label: format!(
                            "The module name `{}` is reserved.
Try a different name for this module.",
                            name
                        ),
                    };
                    write_project(buf, diagnostic);
                }

                TypeError::KeywordInModuleName { name, keyword } => {
                    let diagnostic = ProjectErrorDiagnostic {
                        title: "Invalid module name".to_string(),
                        label: format!(
                            "The module name `{}` contains the
keyword `{}`, so importing it would be a syntax error.
Try a different name for this module.",
                            name, keyword
                        ),
                    };
                    write_project(buf, diagnostic);
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
                    ParseErrorType::ExprThenlessTry=> (
                        "A `try` cannot be the last expression.",
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
                    ParseErrorType::InvalidBitStringUnit => (
                        "This is not a valid BitString unit value.",
                        vec!["Hint: unit must be an integer literal >= 1 and <= 256".to_string(),
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
                    ParseErrorType::NestedBitStringPattern => (
                        "BitString patterns cannot be nested.",
                        vec!["See: https://gleam.run/book/tour/patterns".to_string()]
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
                    ParseErrorType::UnexpectedEof => (
                        "The module ended unexpectedly.",
                        vec![]
                    ),
                    ParseErrorType::ListSpreadWithoutElements   => (
                        "This spread does nothing",
                        vec!["Hint: Try prepending some elements [1, 2, ..list].".to_string(),
"See: https://gleam.run/book/tour/lists.html".to_string()
                        ]
                    ),
                    ParseErrorType::UnexpectedReservedWord => (
                        "This is a reserved word.",
                        vec!["Hint: I was expecting to see a name here.".to_string(), "See: https://gleam.run/book/tour/reserved-words".to_string()]
                    ),
                    ParseErrorType::UnexpectedToken { expected } => {
                        let mut messages = expected.clone();
                        if let Some(s) = messages.first_mut() {
                            *s = format!("Expected one of: {}", s);
                        }

                        ( "I was not expecting this.",
                          messages
                        )
                    }
                };

                let adjusted_location = if error == &ParseErrorType::UnexpectedEof {
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
                write(buf, diagnostic, Severity::Error);
                if !extra.is_empty() {
                    writeln!(buf, "{}", extra.join("\n")).expect("error pretty buffer write");
                }
            }

            Error::ImportCycle { modules } => {
                crate::diagnostic::write_title(buf, "Import cycle");
                writeln!(
                    buf,
                    "The import statements for these modules form a cycle:\n"
                )
                .unwrap();
                import_cycle(buf, modules);

                writeln!(
                    buf,
                    "Gleam doesn't support import cycles like these, please break the
cycle to continue."
                )
                .unwrap();
            }

            Error::PackageCycle { packages } => {
                crate::diagnostic::write_title(buf, "Dependency cycle");
                writeln!(buf, "The dependencies for these packages form a cycle:\n").unwrap();
                import_cycle(buf, packages);
                writeln!(
                    buf,
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
                let diagnostic = Diagnostic {
                    title: "Unknown import".to_string(),
                    label: did_you_mean(import, modules, ""),
                    file: path.to_str().unwrap().to_string(),
                    src: src.to_string(),
                    location: *location,
                };
                write(buf, diagnostic, Severity::Error);
                writeln!(
                    buf,
                    "The module `{}` is trying to import the module `{}`,
but it cannot be found.",
                    module, import
                )
                .expect("error pretty buffer write");
            }

            Error::StandardIo { action, err } => {
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
                write_project(buf, diagnostic);
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

                write_project(buf, diagnostic);
            }

            Error::ForbiddenWarnings { count } => {
                let word_warning = match count {
                    1 => "warning",
                    _ => "warnings",
                };
                let diagnostic = ProjectErrorDiagnostic {
                    title: format!("{} {} generated.", count, word_warning),
                    label: "Your project was compiled with the `--warnings-as-errors` flag.
Fix the warnings and try again!"
                        .to_string(),
                };
                write_project(buf, diagnostic);
            }

            Error::JavaScript { src, path, error } => match error {
                javascript::Error::Unsupported { feature, location } => {
                    let diagnostic = Diagnostic {
                        title: "Unsupported feature for compilation target".to_string(),
                        label: format!("{} is not supported for JavaScript compilation", feature),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write(buf, diagnostic, Severity::Error);
                }
            },
            Error::DownloadPackageError {
                package_name,
                package_version,
                error,
            } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Failed to download package".to_string(),
                    label: format!(
                        "A problem was encountered when downloading {} {}.",
                        package_name, package_version
                    ),
                };
                write_project(buf, diagnostic);
                writeln!(
                    buf,
                    "\nThe error from the package manager client was:

    {}",
                    error
                )
                .unwrap();
            }

            Error::Http(error) => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "HTTP error".to_string(),
                    label: "A HTTP request failed".to_string(),
                };
                write_project(buf, diagnostic);
                writeln!(
                    buf,
                    "\nThe error from the HTTP client was:

    {}",
                    error
                )
                .unwrap();
            }

            Error::InvalidVersionFormat { input, error } => {
                let diagnostic = ProjectErrorDiagnostic {
                    title: "Invalid version format".to_string(),
                    label: format!("I was unable to parse the version {}.", input),
                };
                write_project(buf, diagnostic);
                writeln!(
                    buf,
                    "\nThe error from the parser was:

    {}",
                    error
                )
                .unwrap();
            }
        }
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

fn hint_alternative_operator(op: &BinOp, given: &Type) -> Option<String> {
    match op {
        BinOp::AddInt if given.is_float() => Some(hint_numeric_message("+.", "Float")),
        BinOp::DivInt if given.is_float() => Some(hint_numeric_message("/.", "Float")),
        BinOp::GtEqInt if given.is_float() => Some(hint_numeric_message(">=", "Float")),
        BinOp::GtInt if given.is_float() => Some(hint_numeric_message(">", "Float")),
        BinOp::LtEqInt if given.is_float() => Some(hint_numeric_message("<=.", "Float")),
        BinOp::LtInt if given.is_float() => Some(hint_numeric_message("<.", "Float")),
        BinOp::MultInt if given.is_float() => Some(hint_numeric_message("*.", "Float")),
        BinOp::SubInt if given.is_float() => Some(hint_numeric_message("-.", "Float")),

        BinOp::AddFloat if given.is_int() => Some(hint_numeric_message("+", "Int")),
        BinOp::DivFloat if given.is_int() => Some(hint_numeric_message("/", "Int")),
        BinOp::GtEqFloat if given.is_int() => Some(hint_numeric_message(">=.", "Int")),
        BinOp::GtFloat if given.is_int() => Some(hint_numeric_message(">.", "Int")),
        BinOp::LtEqFloat if given.is_int() => Some(hint_numeric_message("<=", "Int")),
        BinOp::LtFloat if given.is_int() => Some(hint_numeric_message("<", "Int")),
        BinOp::MultFloat if given.is_int() => Some(hint_numeric_message("*", "Int")),
        BinOp::SubFloat if given.is_int() => Some(hint_numeric_message("-", "Int")),

        BinOp::AddInt if given.is_string() => Some(hint_string_message()),
        BinOp::AddFloat if given.is_string() => Some(hint_string_message()),

        _ => None,
    }
}

fn hint_numeric_message(alt: &str, type_: &str) -> String {
    format!("the {} operator can be used with {}s\n", alt, type_)
}

fn hint_string_message() -> String {
    "Strings can be joined using the `append` or `concat` functions from
the `gleam/string` module"
        .to_string()
}

#[derive(Debug, PartialEq)]
pub struct Unformatted {
    pub source: PathBuf,
    pub destination: PathBuf,
    pub input: String,
    pub output: String,
}
