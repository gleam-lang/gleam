#![allow(clippy::unwrap_used, clippy::expect_used)]
use crate::diagnostic::{Diagnostic, Label, Location};
use crate::{ast::BinOp, parse::error::ParseErrorType, type_::Type};
use crate::{
    bit_string,
    diagnostic::{
        write_diagnostic_old, write_old, write_project, DiagnosticLabel, LabelStyle, Level,
        OldDiagnostic, OldMultiLineDiagnostic, ProjectErrorDiagnostic, Severity,
    },
    javascript,
    type_::{pretty::Printer, UnifyErrorSituation},
};
use hexpm::version::pubgrub_report::{DefaultStringReporter, Reporter};
use hexpm::version::ResolutionError;
use itertools::Itertools;
use std::fmt::Debug;
use std::path::{Path, PathBuf};
use termcolor::Buffer;
use thiserror::Error;

pub type Src = String;
pub type Name = String;

pub type Result<Ok, Err = Error> = std::result::Result<Ok, Err>;

macro_rules! wrap_writeln {
    ($buf:expr, $($tts:tt)*) => {
        writeln!($buf, "{}", wrap(&format!($($tts)*)))
    }
}

macro_rules! wrap_format {
    ($($tts:tt)*) => {
        wrap(&format!($($tts)*))
    }
}

#[derive(Debug, PartialEq, Error)]
pub enum Error {
    #[error("failed to parse Gleam source code")]
    Parse {
        path: PathBuf,
        src: Src,
        error: crate::parse::error::ParseError,
    },

    #[error("type checking failed")]
    Type {
        path: PathBuf,
        src: Src,
        error: crate::type_::Error,
    },

    #[error("unknown import {import} in {module}")]
    UnknownImport {
        module: Name,
        import: Name,
        location: crate::ast::SrcSpan,
        path: PathBuf,
        src: String,
        modules: Vec<String>,
    },

    #[error("duplicate module {module}")]
    DuplicateModule {
        module: Name,
        first: PathBuf,
        second: PathBuf,
    },

    #[error("duplicate source file {file}")]
    DuplicateSourceFile { file: String },

    #[error("test module {test_module} imported into application module {src_module}")]
    SrcImportingTest {
        path: PathBuf,
        src: Src,
        location: crate::ast::SrcSpan,
        src_module: Name,
        test_module: Name,
    },

    #[error("cyclical module imports")]
    ImportCycle { modules: Vec<String> },

    #[error("cyclical package dependencies")]
    PackageCycle { packages: Vec<String> },

    #[error("file operation failed")]
    FileIo {
        kind: FileKind,
        action: FileIoAction,
        path: PathBuf,
        err: Option<String>,
    },

    #[error("{error}")]
    GitInitialization { error: String },

    #[error("io operation failed")]
    StandardIo {
        action: StandardIoAction,
        err: Option<std::io::ErrorKind>,
    },

    #[error("source code incorrectly formatted")]
    Format { problem_files: Vec<Unformatted> },

    #[error("Hex error: {0}")]
    Hex(String),

    #[error("{error}")]
    ExpandTar { error: String },

    #[error("{err}")]
    AddTar { path: PathBuf, err: String },

    #[error("{0}")]
    TarFinish(String),

    #[error("{0}")]
    Gzip(String),

    #[error("shell program `{program}` not found")]
    ShellProgramNotFound { program: String },

    #[error("shell program `{program}` failed")]
    ShellCommand {
        program: String,
        err: Option<std::io::ErrorKind>,
    },

    #[error("{name} is not a valid project name")]
    InvalidProjectName {
        name: String,
        reason: InvalidProjectNameReason,
    },

    #[error("{input} is not a valid version. {error}")]
    InvalidVersionFormat { input: String, error: String },

    #[error("project root already exists")]
    ProjectRootAlreadyExist { path: String },

    #[error("unable to find project root")]
    UnableToFindProjectRoot { path: String },

    #[error("gleam.toml version {toml_ver} does not match .app version {app_ver}")]
    VersionDoesNotMatch { toml_ver: String, app_ver: String },

    #[error("metadata decoding failed")]
    MetadataDecodeError { error: Option<String> },

    #[error("warnings are not permitted")]
    ForbiddenWarnings { count: usize },

    #[error("javascript codegen failed")]
    JavaScript {
        path: PathBuf,
        src: Src,
        error: crate::javascript::Error,
    },

    #[error("package downloading failed: {error}")]
    DownloadPackageError {
        package_name: String,
        package_version: String,
        error: String,
    },

    #[error("{0}")]
    Http(String),

    #[error("Dependency tree resolution failed: {0}")]
    DependencyResolutionFailed(String),

    #[error("The package {0} is listed in dependencies and dev-dependencies")]
    DuplicateDependency(String),

    #[error("The package was missing required fields for publishing")]
    MissingHexPublishFields {
        description_missing: bool,
        licence_missing: bool,
    },

    #[error("The package {package} uses unsupported build tools {build_tools:?}")]
    UnsupportedBuildTool {
        package: String,
        build_tools: Vec<String>,
    },
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

    pub fn add_tar<P, E>(path: P, error: E) -> Error
    where
        P: AsRef<Path>,
        E: std::error::Error,
    {
        Self::AddTar {
            path: path.as_ref().to_path_buf(),
            err: error.to_string(),
        }
    }

    pub fn finish_tar<E>(error: E) -> Error
    where
        E: std::error::Error,
    {
        Self::TarFinish(error.to_string())
    }

    pub fn dependency_resolution_failed(error: ResolutionError) -> Error {
        Self::DependencyResolutionFailed(match error {
            ResolutionError::NoSolution(mut derivation_tree) => {
                derivation_tree.collapse_no_versions();
                let report = DefaultStringReporter::report(&derivation_tree);
                wrap(&report)
            }

            // TODO: Custom error here
            // ResolutionError::ErrorRetrievingDependencies {
            //     package,
            //     version,
            //     source,
            // } => Use the source, it'll provide a better error message
            error => error.to_string(),
        })
    }

    pub fn expand_tar<E>(error: E) -> Error
    where
        E: std::error::Error,
    {
        Self::ExpandTar {
            error: error.to_string(),
        }
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
    Write,
}

impl StandardIoAction {
    fn text(&self) -> &'static str {
        match self {
            StandardIoAction::Read => "read from",
            StandardIoAction::Write => "write to",
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FileIoAction {
    Link,
    Open,
    Copy,
    Read,
    Parse,
    Delete,
    Create,
    WriteTo,
    Canonicalise,
    FindParent,
}

impl FileIoAction {
    fn text(&self) -> &'static str {
        match self {
            FileIoAction::Link => "link",
            FileIoAction::Open => "open",
            FileIoAction::Copy => "copy",
            FileIoAction::Read => "read",
            FileIoAction::Parse => "parse",
            FileIoAction::Delete => "delete",
            FileIoAction::Create => "create",
            FileIoAction::WriteTo => "write to",
            FileIoAction::FindParent => "find the parent of",
            FileIoAction::Canonicalise => "Canonicalise",
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

    pub fn pretty(&self, buffer: &mut Buffer) {
        write_diagnostic(buffer, &self.to_diagnostic());
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        use crate::type_::Error as TypeError;
        match self {
            Error::MetadataDecodeError { error } => {
                let mut text = "A problem was encountered when decoding the metadata for one \
of the Gleam dependency modules."
                    .to_string();
                if let Some(error) = error {
                    text.push_str("\nThe error from the decoder library was:\n\n");
                    text.push_str(error);
                }

                Diagnostic {
                    title: "Failed to decode module metadata".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::InvalidProjectName { name, reason } => {
                let text = format!(
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
                );

                Diagnostic {
                    title: "Invalid project name".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::ProjectRootAlreadyExist { path } => Diagnostic {
                title: "Project folder already exists".into(),
                text: format!("Project folder root:\n\n  {}", path),
                level: Level::Error,
                location: None,
            },

            Error::UnableToFindProjectRoot { path } => Diagnostic {
                title: "Invalid project root".into(),
                text: format!("We were unable to find the project root:\n\n  {}", path),
                level: Level::Error,
                location: None,
            },

            Error::VersionDoesNotMatch { toml_ver, app_ver } => {
                let text = format!(
                    "The version in gleam.toml \"{}\" does not match the version in
your app.src file \"{}\"",
                    toml_ver, app_ver
                );
                Diagnostic {
                    title: "Version does not match".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::ShellProgramNotFound { program } => {
                let mut text = format!("The program `{}` was not found. Is it installed?", program);

                match program.as_str() {
                    "erl" | "erlc" | "escript" => text.push_str(
                        "
Documentation for installing Erlang can be viewed here:
https://gleam.run/getting-started/",
                    ),
                    _ => (),
                };

                Diagnostic {
                    title: "Program not found".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::ShellCommand {
                program: command,
                err: None,
            } => {
                let text = format!(
                    "There was a problem when running the shell command `{}`.",
                    command
                );
                Diagnostic {
                    title: "Shell command failure".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::ShellCommand {
                program: command,
                err: Some(err),
            } => {
                let text = format!(
                    "There was a problem when running the shell command `{}`.

The error from the shell command library was:

    {}",
                    command,
                    std_io_error_kind_text(err)
                );
                Diagnostic {
                    title: "Shell command failure".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::Gzip(detail) => {
                let text = format!(
                    "There was a problem when applying gzip compression.

This was error from the gzip library:

    {}",
                    detail
                );
                Diagnostic {
                    title: "Gzip compression failure".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::AddTar { path, err } => {
                let text = format!(
                    "There was a problem when attempting to add the file {}
to a tar archive.

This was error from the tar library:

    {}",
                    path.to_str().unwrap(),
                    err
                );
                Diagnostic {
                    title: "Failure creating tar archive".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::ExpandTar { error } => {
                let text = format!(
                    "There was a problem when attempting to expand a to a tar archive.

This was error from the tar library:

    {}",
                    error
                );
                Diagnostic {
                    title: "Failure opening tar archive".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::TarFinish(detail) => {
                let text = format!(
                    "There was a problem when creating a tar archive.

This was error from the tar library:

    {}",
                    detail
                );
                Diagnostic {
                    title: "Failure creating tar archive".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::Hex(detail) => {
                let text = format!(
                    "There was a problem when using the Hex API.

This was error from the Hex client library:

    {}",
                    detail
                );
                Diagnostic {
                    title: "Hex API failure".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::SrcImportingTest {
                path,
                src,
                location,
                src_module,
                test_module,
            } => {
                let text = wrap_format!(
                    "The application module `{}` is importing the test module `{}`.

Test modules are not included in production builds so test modules cannot import them. Perhaps move the `{}` module to the src directory.",
                    src_module, test_module, test_module,
                );

                Diagnostic {
                    title: "App importing test module".into(),
                    text,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: Some("Imported here".into()),
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.into(),
                        extra_labels: vec![],
                    }),
                }
            }

            Error::DuplicateModule {
                module,
                first,
                second,
            } => {
                let text = format!(
                    "The module `{}` is defined multiple times.

First:  {}
Second: {}",
                    module,
                    first.to_str().expect("pretty error print PathBuf to_str"),
                    second.to_str().expect("pretty error print PathBuf to_str"),
                );

                Diagnostic {
                    title: "Duplicate module".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::DuplicateSourceFile { file } => Diagnostic {
                title: "Duplicate Source file".into(),
                text: format!("The file `{}` is defined multiple times.", file),
                level: Level::Error,
                location: None,
            },

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
                    None => "".into(),
                };
                let text = format!(
                    "An error occurred while trying to {} this {}:

    {}
{}",
                    action.text(),
                    kind.text(),
                    path.to_string_lossy(),
                    err,
                );
                Diagnostic {
                    title: "File IO failure".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::GitInitialization { error } => {
                let text = format!(
                    "An error occurred while trying make a git repository for this project:

    {}",
                    error
                );
                Diagnostic {
                    title: "Failed to initialize git repository".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
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

                    OldMultiLineDiagnostic {
                        title: title.to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        labels: unknown
                            .iter()
                            .map(|(label, location)| DiagnosticLabel {
                                text: did_you_mean(label, &other_labels, "Unexpected label"),
                                location: *location,
                                style: LabelStyle::Primary,
                            })
                            .collect(),
                    };
                    write_diagnostic_old(buf, diagnostic, Severity::Error);

                    if valid.is_empty() {
                        writeln!(
                            buf,
                            "This constructor does not accept any labelled arguments."
                        )
                        .unwrap();
                    } else if other_labels.is_empty() {
                        wrap_writeln!(
                                buf,
                                "You have already supplied all the labelled arguments that this constructor accepts."
                            )
                            .unwrap();
                    } else {
                        wrap_writeln!(
                            buf,
                            "The other labelled arguments that this constructor accepts are `{}`.",
                            other_labels.iter().join("`, `")
                        )
                        .unwrap();
                    }
                }

                TypeError::UnexpectedLabelledArg { location, label } => {
                    OldDiagnostic {
                        title: "Unexpected labelled argument".into(),
                        text: "".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    wrap_writeln!(
                        buf,
                        "
This argument has been given a label but the constructor does not expect any.
Please remove the label `{}`.",
                        label
                    )
                    .unwrap();
                }

                TypeError::PositionalArgumentAfterLabelled { location } => {
                    OldDiagnostic {
                        title: "Unexpected positional argument".into(),
                        text: "".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    wrap_writeln!(
                        buf,
                        "This unlablled argument has been supplied after a labelled argument.
Once a labelled argument has been supplied all following arguments must also be labelled.",
                    )
                    .unwrap();
                }

                TypeError::DuplicateName {
                    location,
                    name: fun,
                    previous_location,
                    ..
                } => {
                    OldMultiLineDiagnostic {
                        title: format!("Duplicate function definition with name `{}`", fun),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        labels: vec![
                            DiagnosticLabel {
                                text: "redefined here".into(),
                                location: *location,
                                style: LabelStyle::Primary,
                            },
                            DiagnosticLabel {
                                text: "previously defined here".into(),
                                location: *previous_location,
                                style: LabelStyle::Secondary,
                            },
                        ],
                    };
                    write_diagnostic_old(buf, diagnostic, Severity::Error);
                }

                TypeError::DuplicateImport {
                    location,
                    previous_location,
                    name,
                    ..
                } => {
                    OldMultiLineDiagnostic {
                        title: format!("Duplicate import with name `{}`", name),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        labels: vec![
                            DiagnosticLabel {
                                text: "redefined here".into(),
                                location: *location,
                                style: LabelStyle::Primary,
                            },
                            DiagnosticLabel {
                                text: "previously defined here".into(),
                                location: *previous_location,
                                style: LabelStyle::Secondary,
                            },
                        ],
                    };
                    write_diagnostic_old(buf, diagnostic, Severity::Error);
                }

                TypeError::DuplicateConstName {
                    location,
                    name,
                    previous_location,
                    ..
                } => {
                    OldMultiLineDiagnostic {
                        title: format!("Duplicate const definition with name `{}`", name),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        labels: vec![
                            DiagnosticLabel {
                                text: "redefined here".into(),
                                location: *location,
                                style: LabelStyle::Primary,
                            },
                            DiagnosticLabel {
                                text: "previously defined here".into(),
                                location: *previous_location,
                                style: LabelStyle::Secondary,
                            },
                        ],
                    };
                    write_diagnostic_old(buf, diagnostic, Severity::Error);
                }

                TypeError::DuplicateTypeName {
                    name,
                    location,
                    previous_location,
                    ..
                } => {
                    OldMultiLineDiagnostic {
                        title: format!("Duplicate type definition with name `{}`", name),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        labels: vec![
                            DiagnosticLabel {
                                text: "redefined here".into(),
                                location: *location,
                                style: LabelStyle::Primary,
                            },
                            DiagnosticLabel {
                                text: "previously defined here".into(),
                                location: *previous_location,
                                style: LabelStyle::Secondary,
                            },
                        ],
                    };
                    write_diagnostic_old(buf, diagnostic, Severity::Error);
                }

                TypeError::DuplicateField { location, label } => {
                    OldDiagnostic {
                        title: "Duplicate field".into(),
                        text: "".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    wrap_writeln!(
                        buf,
                        "The field `{}` has already been defined. Rename this field.",
                        label
                    )
                    .unwrap();
                }

                TypeError::DuplicateArgument { location, label } => {
                    OldDiagnostic {
                        title: "Duplicate argument".into(),
                        text: "".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    wrap_writeln!(
                        buf,
                        "The labelled argument `{}` has already been supplied.",
                        label
                    )
                    .unwrap();
                }

                TypeError::RecursiveType { location } => {
                    OldDiagnostic {
                        title: "Recursive type".into(),
                        text: "".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                }

                TypeError::NotFn { location, typ } => {
                    OldDiagnostic {
                        title: "Type mismatch".into(),
                        text: "".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
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
                    OldDiagnostic {
                        title: "Unknown record field".into(),
                        text: did_you_mean(label, fields, "This field does not exist"),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    let mut printer = Printer::new();

                    writeln!(
                        buf,
                        "The value being accessed has this type:

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
                    rigid_type_names: annotated_names,
                } => {
                    OldDiagnostic {
                        title: "Type mismatch".into(),
                        text: "".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    let mut printer = Printer::new();
                    printer.with_names(annotated_names.clone());
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
                    rigid_type_names: annotated_names,
                } => {
                    OldDiagnostic {
                        title: "Type mismatch".into(),
                        text: "This function does not accept the piped type".into(),
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

                    write_old(buf, diagnostic, Severity::Error);
                    let mut printer = Printer::new();
                    printer.with_names(annotated_names.clone());
                    writeln!(
                        buf,
                        "The argument is:

{given}

But function expects:

{expected}

",
                        expected = expected
                            .map(|v| printer.pretty_print(&v, 4))
                            .unwrap_or_else(|| "    No arguments".into()),
                        given = printer.pretty_print(&given, 4)
                    )
                    .unwrap();
                }

                TypeError::CouldNotUnify {
                    location,
                    expected,
                    given,
                    situation,
                    rigid_type_names: annotated_names,
                } => {
                    OldDiagnostic {
                        title: "Type mismatch".into(),
                        text: "".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    if let Some(description) = situation.and_then(|s| s.description()) {
                        writeln!(buf, "{}\n", description).unwrap();
                    }
                    let mut printer = Printer::new();
                    printer.with_names(annotated_names.clone());
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
                    OldDiagnostic {
                        title: "Incorrect arity".into(),
                        text: format!("expected {} arguments, got {}", expected, given),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                }

                TypeError::IncorrectArity {
                    labels,
                    location,
                    expected,
                    given,
                } => {
                    OldDiagnostic {
                        title: "Incorrect arity".into(),
                        text: format!("expected {} arguments, got {}", expected, given),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
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
                    OldDiagnostic {
                        title: "Unnecessary spread operator".into(),
                        text: String::new(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);

                    wrap_writeln!(
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
                    OldDiagnostic {
                        title: "Unknown type".into(),
                        text: did_you_mean(name, types, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    wrap_writeln!(
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
                    OldDiagnostic {
                        title: "Unknown variable".into(),
                        text: did_you_mean(name, variables, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    wrap_writeln!(buf, "The name `{}` is not in scope here.", name).unwrap();
                }

                TypeError::PrivateTypeLeak { location, leaked } => {
                    OldDiagnostic {
                        title: "Private type used in public interface".into(),
                        text: "".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
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
                    OldDiagnostic {
                        title: "Unknown module".into(),
                        text: did_you_mean(name, imported_modules, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    writeln!(buf, "No module has been found with the name `{}`.", name).unwrap();
                }

                TypeError::UnknownModuleType {
                    location,
                    name,
                    module_name,
                    type_constructors,
                } => {
                    OldDiagnostic {
                        title: "Unknown module type".into(),
                        text: did_you_mean(name, type_constructors, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
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
                    OldDiagnostic {
                        title: "Unknown module field".into(),
                        text: did_you_mean(name, value_constructors, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
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
                    OldDiagnostic {
                        title: "Unknown module field".into(),
                        text: did_you_mean(name, &options, ""),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
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
                    OldDiagnostic {
                        title: "Incorrect number of patterns".into(),
                        text: format!("expected {} patterns, got {}", expected, given),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    wrap_writeln!(
                        buf,
                        "This case expression has {} subjects, but this pattern matches {}.
Each clause must have a pattern for every subject value.",
                        expected,
                        given
                    )
                    .unwrap();
                }

                TypeError::NonLocalClauseGuardVariable { location, name } => {
                    OldDiagnostic {
                        title: "Invalid guard variable".into(),
                        text: "is not locally defined".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    wrap_writeln!(
                        buf,
                        "Variables used in guards must be either defined in the function, or be an argument to the function. The variable `{}` is not defined locally.",
                        name
                    )
                    .unwrap();
                }

                TypeError::ExtraVarInAlternativePattern { location, name } => {
                    OldDiagnostic {
                        title: "Extra alternative pattern variable".into(),
                        text: "has not been previously defined".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    wrap_writeln!(
                        buf,
                        "All alternative patterns must define the same variables as the initial pattern. This variable `{}` has not been previously defined.",
                        name
                    )
                    .unwrap();
                }

                TypeError::MissingVarInAlternativePattern { location, name } => {
                    OldDiagnostic {
                        title: "Missing alternative pattern variable".into(),
                        text: "does not define all required variables".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    buf.write_all(wrap(&format!(
                        "All alternative patterns must define the same variables as the initial pattern, but the `{}` variable is missing.",
                        name
                    )).as_bytes())
                    .unwrap();
                }

                TypeError::DuplicateVarInPattern { location, name } => {
                    OldDiagnostic {
                        title: "Duplicate variable in pattern".into(),
                        text: "has already been used".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);

                    writeln!(
                        buf,
                        "{}",
                        wrap(&format!(
                            "Variables can only be used once per pattern. This variable {} appears multiple times.
If you used the same variable twice deliberately in order to check for equality please use a guard clause instead.
e.g. (x, y) if x == y -> ...",
                            name
                        ))
                    )
                    .unwrap();
                }

                TypeError::OutOfBoundsTupleIndex {
                    location, size: 0, ..
                } => {
                    OldDiagnostic {
                        title: "Out of bounds tuple index".into(),
                        text: "this index is too large".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    wrap_writeln!(
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
                    OldDiagnostic {
                        title: "Out of bounds tuple index".into(),
                        text: "this index is too large".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    wrap_writeln!(
                        buf,
                        "The index being accessed for this tuple is {}, but this tuple has {} elements so the highest valid index is {}.",
                        index,
                        size,
                        size - 1,
                    )
                    .unwrap();
                }

                TypeError::NotATuple { location, given } => {
                    OldDiagnostic {
                        title: "Type mismatch".into(),
                        text: "is not a tuple".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
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
                    OldDiagnostic {
                        title: "Type mismatch".into(),
                        text: "what type is this?".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    wrap_writeln!(
                        buf,
                        "To index into a tuple we need to know it size, but we don't know anything about this type yet. Please add some type annotations so we can continue.",
                    )
                    .unwrap();
                }

                TypeError::RecordAccessUnknownType { location } => {
                    OldDiagnostic {
                        title: "Unknown type for record access".into(),
                        text: "I don't know what type this is".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);

                    wrap_writeln!(
                        buf,
                        "In order to access a record field we need to know what type it is, but I can't tell the type here. Try adding type annotations to your function and try again.",
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
                            vec!["Hint: This segment already has a size.".into()],
                        ),

                        bit_string::ErrorType::ConflictingUnitOptions => (
                            "This is an extra unit specifier.",
                            vec!["Hint: A BitString segment can have at most 1 unit.".into()],
                        ),

                        bit_string::ErrorType::FloatWithSize => (
                            "Size cannot be used with float.",
                            vec!["Hint: floats have an exact size of 64 bits.".into()],
                        ),

                        bit_string::ErrorType::InvalidEndianness => (
                            "this option is invalid here.",
                            vec![wrap("Hint: signed and unsigned can only be used with int, float, utf16 and utf32 types.")],
                        ),

                        bit_string::ErrorType::OptionNotAllowedInValue => (
                            "This option is only allowed in BitString patterns.",
                            vec!["Hint: This option has no effect in BitString values.".into()],
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
                            vec![wrap(&format!("Hint: {} segments are sized based on their value and cannot have a unit.", typ))],
                        ),
                        bit_string::ErrorType::VariableUtfSegmentInPattern => (
                            "this cannot be a variable",
                            vec![wrap("Hint: in patterns utf8, utf16, and utf32  must be an exact string.")],
                        ),
                        bit_string::ErrorType::SegmentMustHaveSize => (
                            "This segment has no size",
                            vec![wrap("Hint: Bit string segments without a size are only allowed at the end of a bin pattern.")],
                        ),
                        bit_string::ErrorType::UnitMustHaveSize => (
                            "This needs an explicit size",
                            vec!["Hint: If you specify unit() you must also specify size().".into()],
                        ),
                    };
                    OldDiagnostic {
                        title: "BitString Segment Error".into(),
                        text: label.to_string(),
                        location: *location,
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                    };
                    extra.push("See: https://gleam.run/book/tour/bit-strings.html".into());
                    write_old(buf, diagnostic, Severity::Error);
                    if !extra.is_empty() {
                        writeln!(buf, "{}", extra.join("\n")).expect("error pretty buffer write");
                    }
                }

                TypeError::RecordUpdateInvalidConstructor { location } => {
                    OldDiagnostic {
                        title: "Invalid record constructor".into(),
                        text: "This is not a record constructor".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);

                    writeln!(
                        buf,
                        "Only record constructors can be used with the update syntax.",
                    )
                    .unwrap();
                }

                TypeError::UnexpectedTypeHole { location } => {
                    OldDiagnostic {
                        title: "Unexpected type hole".into(),
                        text: "".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);

                    writeln!(
                        buf,
                        "We need to know the exact type here so type holes are not permitted.",
                    )
                    .unwrap();
                }

                TypeError::ReservedModuleName { name } => {
                    Diagnostic {
                        title: "Reserved module name".into(),
                        text: format!(
                            "The module name `{}` is reserved.
Try a different name for this module.",
                            name
                        ),
                        location: None,
                        level: Level::Error,
                    };
                }

                TypeError::KeywordInModuleName { name, keyword } => {
                    Diagnostic {
                        title: "Invalid module name".into(),
                        text: wrap(&format!(
                            "The module name `{}` contains the keyword `{}`, so importing it would be a syntax error.
Try a different name for this module.",
                            name, keyword
                        )),
                    location: None,
                    level: Level::Error,
                    };
                }

                TypeError::NotExhaustivePatternMatch {
                    location,
                    unmatched,
                } => {
                    OldDiagnostic {
                        title: "Not exhaustive pattern match".into(),
                        text: "".into(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    };
                    write_old(buf, diagnostic, Severity::Error);
                    wrap_writeln!(
                        buf,
                        "This case expression does not match all possibilities. \
Each constructor must have a pattern that matches it or else it could crash.

These values are not matched:

  - {}",
                        unmatched.join("\n  - "),
                    )
                    .unwrap();
                }
            },

            Error::Parse { path, src, error } => {
                let (label, extra) = error.details();

                let adjusted_location = if error.error == ParseErrorType::UnexpectedEof {
                    crate::ast::SrcSpan {
                        start: src.len() - 1,
                        end: src.len() - 1,
                    }
                } else {
                    error.location
                };

                OldDiagnostic {
                    title: "Syntax error".into(),
                    text: label.to_string(),
                    location: adjusted_location,
                    file: path.to_str().unwrap().to_string(),
                    src: src.to_string(),
                };
                write_old(buf, diagnostic, Severity::Error);
                if !extra.is_empty() {
                    writeln!(buf, "{}", extra.join("\n")).expect("error pretty buffer write");
                }
            }

            Error::ImportCycle { modules } => {
                crate::diagnostic::write_title(buf, "Import cycle", Severity::Error);
                writeln!(
                    buf,
                    "The import statements for these modules form a cycle:\n"
                )
                .unwrap();
                let mut buffer = Buffer::ansi();
                import_cycle(buf, modules);

                wrap_writeln!(
                    buf,
                    "Gleam doesn't support import cycles like these, please break the cycle to continue."
                )
                .unwrap();
            }

            Error::PackageCycle { packages } => {
                let mut text = "The dependencies for these packages form a cycle:
"
                .to_string();
                import_cycle(&mut text, packages);
                text.push_str(
                    "Gleam doesn't support dependency cycles like these, please break the
cycle to continue.",
                );
                Diagnostic {
                    title: "Dependency cycle".into(),
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::UnknownImport {
                module,
                import,
                location,
                path,
                src,
                modules,
            } => {
                let text = wrap(&format!(
                    "The module `{}` is trying to import the module `{}`, but it cannot be found.",
                    module, import
                ));
                Diagnostic {
                    title: "Unknown import".into(),
                    text,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: Some(did_you_mean(import, modules, "")),
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.into(),
                        extra_labels: vec![],
                    }),
                }
            }

            Error::StandardIo { action, err } => {
                let err = match err {
                    Some(e) => format!(
                        "\nThe error message from the stdio library was:\n\n    {}\n",
                        std_io_error_kind_text(e)
                    ),
                    None => "".into(),
                };
                Diagnostic {
                    title: "Standard IO failure".into(),
                    text: format!(
                        "An error occurred while trying to {}:

{}",
                        action.text(),
                        err,
                    ),
                    location: None,
                    level: Level::Error,
                }
            }

            Error::Format { problem_files } => {
                let files: Vec<_> = problem_files
                    .iter()
                    .flat_map(|formatted| formatted.source.to_str())
                    .map(|p| format!("  - {}", p))
                    .sorted()
                    .collect();
                let mut text = files.iter().join("\n");
                text.push('\n');
                Diagnostic {
                    title: "These files have not been formatted".into(),
                    text,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::ForbiddenWarnings { count } => {
                let word_warning = match count {
                    1 => "warning",
                    _ => "warnings",
                };
                let text = "Your project was compiled with the `--warnings-as-errors` flag.
Fix the warnings and try again."
                    .into();
                Diagnostic {
                    title: format!("{} {} generated.", count, word_warning),
                    text,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::JavaScript { src, path, error } => match error {
                javascript::Error::Unsupported { feature, location } => Diagnostic {
                    title: "Unsupported feature for compilation target".into(),
                    text: format!("{} is not supported for JavaScript compilation", feature),
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: None,
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.into(),
                        extra_labels: vec![],
                    }),
                },
            },

            Error::DownloadPackageError {
                package_name,
                package_version,
                error,
            } => {
                let text = format!(
                    "A problem was encountered when downloading {} {}.
The error from the package manager client was:

    {}",
                    package_name, package_version, error
                );
                Diagnostic {
                    title: "Failed to download package".into(),
                    text,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::Http(error) => {
                let text = format!(
                    "A HTTP request failed.
The error from the HTTP client was:

    {}",
                    error
                );
                Diagnostic {
                    title: "HTTP error".into(),
                    text,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::InvalidVersionFormat { input, error } => {
                let text = format!(
                    "I was unable to parse the version {}.
The error from the parser was:

    {}",
                    input, error
                );
                Diagnostic {
                    title: "Invalid version format".into(),
                    text,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::DependencyResolutionFailed(error) => {
                let text = format!(
                    "An error occurred while determining what dependency packages and versions
should be downloaded.
The error from the version resolver library was:

{}",
                    wrap(error)
                );
                Diagnostic {
                    title: "Dependency resolution failed".into(),
                    text,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::DuplicateDependency(name) => {
                let text = format!(
                    "The package {} is specified in both the dependencies and
dev-dependencies sections of the gleam.toml file.",
                    name
                );
                Diagnostic {
                    title: "Dependency duplicated".into(),
                    text,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::MissingHexPublishFields {
                description_missing,
                licence_missing,
            } => {
                let mut text =
                    "Licence information and package description are required to publish a
package to Hex.\n"
                        .into();
                text.push_str(if *description_missing && *licence_missing {
                    r#"Add the licences and description fields to your gleam.toml file.
                
description = "A Gleam library"
licences = ["Apache-2.0"]"#
                } else if *description_missing {
                    r#"Add the description field to your gleam.toml file.
                
description = "A Gleam library""#
                } else {
                    r#"Add the licences field to your gleam.toml file.
                
licences = ["Apache-2.0"]"#
                });
                Diagnostic {
                    title: "Missing required package fields".into(),
                    text,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::UnsupportedBuildTool {
                package,
                build_tools,
            } => {
                let text = wrap_format!(
                    "The package {} cannot be built as it does not use \
a build tool supported by Gleam. It uses {:?}.

If you would like us to support this package please let us know by opening an \
issue in our tracker: https://github.com/gleam-lang/gleam/issues",
                    package,
                    build_tools
                );
                Diagnostic {
                    title: "Unsupported build tool".into(),
                    text,
                    location: None,
                    level: Level::Error,
                }
            }
        }
    }
}

fn std_io_error_kind_text(kind: &std::io::ErrorKind) -> String {
    use std::io::ErrorKind;
    match kind {
        ErrorKind::NotFound => "Could not find the stdio stream".into(),
        ErrorKind::PermissionDenied => "Permission was denied".into(),
        ErrorKind::ConnectionRefused => "Connection was refused".into(),
        ErrorKind::ConnectionReset => "Connection was reset".into(),
        ErrorKind::ConnectionAborted => "Connection was aborted".into(),
        ErrorKind::NotConnected => "Was not connected".into(),
        ErrorKind::AddrInUse => "The stream was already in use".into(),
        ErrorKind::AddrNotAvailable => "The stream was not available".into(),
        ErrorKind::BrokenPipe => "The pipe was broken".into(),
        ErrorKind::AlreadyExists => "A handle to the stream already exists".into(),
        ErrorKind::WouldBlock => "This operation would block when it was requested not to".into(),
        ErrorKind::InvalidInput => "Some parameter was invalid".into(),
        ErrorKind::InvalidData => "The data was invalid.  Check that the encoding is UTF-8".into(),
        ErrorKind::TimedOut => "The operation timed out".into(),
        ErrorKind::WriteZero => {
            "An attempt was made to write, but all bytes could not be written".into()
        }
        ErrorKind::Interrupted => "The operation was interrupted".into(),
        ErrorKind::UnexpectedEof => "The end of file was reached before it was expected".into(),
        _ => "An unknown error occurred".into(),
    }
}

fn import_cycle(buffer: &mut String, modules: &[String]) {
    buffer.push_str(
        "
    ┌─────┐\n",
    );
    for (index, name) in modules.iter().enumerate() {
        if index != 0 {
            buffer.push_str("    │     ↓\n");
        }
        buffer.push_str("    │    ");
        buffer.push_str(name);
        buffer.push('\n');
    }
    buffer.push_str("    └─────┘\n");
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
    wrap("Strings can be joined using the `append` or `concat` functions from the `gleam/string` module")
}

#[derive(Debug, PartialEq)]
pub struct Unformatted {
    pub source: PathBuf,
    pub destination: PathBuf,
    pub input: String,
    pub output: String,
}

pub fn wrap(text: &str) -> String {
    textwrap::fill(text, std::cmp::min(75, textwrap::termwidth()))
}

pub fn write_diagnostic(buffer: &mut Buffer, diagnostic: &Diagnostic) -> () {
    todo!()
}
