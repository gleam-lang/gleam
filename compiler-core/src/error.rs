#![allow(clippy::unwrap_used, clippy::expect_used)]
use crate::build::{Runtime, Target};
use crate::diagnostic::{Diagnostic, Label, Location};
use crate::type_::error::{MissingAnnotation, UnknownTypeHint};
use crate::type_::{error::PatternMatchKind, FieldAccessUsage};
use crate::{ast::BinOp, parse::error::ParseErrorType, type_::Type};
use crate::{
    bit_array,
    diagnostic::Level,
    javascript,
    type_::{pretty::Printer, UnifyErrorSituation},
};
use ecow::EcoString;
use hexpm::version::pubgrub_report::{DefaultStringReporter, Reporter};
use hexpm::version::ResolutionError;
use itertools::Itertools;
use std::env;
use std::fmt::Debug;
use std::path::PathBuf;
use termcolor::Buffer;
use thiserror::Error;

use camino::{Utf8Path, Utf8PathBuf};

pub type Name = EcoString;

pub type Result<Ok, Err = Error> = std::result::Result<Ok, Err>;

macro_rules! wrap_format {
    ($($tts:tt)*) => {
        wrap(&format!($($tts)*))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnknownImportDetails {
    pub module: Name,
    pub location: crate::ast::SrcSpan,
    pub path: Utf8PathBuf,
    pub src: EcoString,
    pub modules: Vec<EcoString>,
}

#[derive(Debug, Eq, PartialEq, Error, Clone)]
pub enum Error {
    #[error("failed to parse Gleam source code")]
    Parse {
        path: Utf8PathBuf,
        src: EcoString,
        error: crate::parse::error::ParseError,
    },

    #[error("type checking failed")]
    Type {
        path: Utf8PathBuf,
        src: EcoString,
        error: crate::type_::Error,
    },

    #[error("unknown import {import}")]
    UnknownImport {
        import: EcoString,
        // Boxed to prevent this variant from being overly large
        details: Box<UnknownImportDetails>,
    },

    #[error("duplicate module {module}")]
    DuplicateModule {
        module: Name,
        first: Utf8PathBuf,
        second: Utf8PathBuf,
    },

    #[error("duplicate source file {file}")]
    DuplicateSourceFile { file: String },

    #[error("cyclical module imports")]
    ImportCycle { modules: Vec<EcoString> },

    #[error("cyclical package dependencies")]
    PackageCycle { packages: Vec<EcoString> },

    #[error("file operation failed")]
    FileIo {
        kind: FileKind,
        action: FileIoAction,
        path: Utf8PathBuf,
        err: Option<String>,
    },

    #[error("Non Utf-8 Path: {path}")]
    NonUtf8Path { path: PathBuf },

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
    AddTar { path: Utf8PathBuf, err: String },

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

    #[error("{module} is not a valid module name")]
    InvalidModuleName { module: String },

    #[error("{module} is not module")]
    ModuleDoesNotExist {
        module: EcoString,
        suggestion: Option<EcoString>,
    },

    #[error("{module} does not have a main function")]
    ModuleDoesNotHaveMainFunction { module: EcoString },

    #[error("{module}'s main function has the wrong arity so it can not be run")]
    MainFunctionHasWrongArity { module: EcoString, arity: usize },

    #[error("{module}'s main function does not support the current target")]
    MainFunctionDoesNotSupportTarget { module: EcoString, target: Target },

    #[error("{input} is not a valid version. {error}")]
    InvalidVersionFormat { input: String, error: String },

    #[error("project root already exists")]
    ProjectRootAlreadyExist { path: String },

    #[error("File(s) already exist in {}",
file_names.iter().map(|x| x.as_str()).join(", "))]
    OutputFilesAlreadyExist { file_names: Vec<Utf8PathBuf> },

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
        path: Utf8PathBuf,
        src: EcoString,
        error: crate::javascript::Error,
    },

    #[error("Invalid runtime for {target} target: {invalid_runtime}")]
    InvalidRuntime {
        target: Target,
        invalid_runtime: Runtime,
    },

    #[error("package downloading failed: {error}")]
    DownloadPackageError {
        package_name: String,
        package_version: String,
        error: String,
    },

    #[error("{0}")]
    Http(String),

    #[error("Git dependencies are currently unsupported")]
    GitDependencyUnsupported,

    #[error("Failed to create canonical path for package {0}")]
    DependencyCanonicalizationFailed(String),

    #[error("Dependency tree resolution failed: {0}")]
    DependencyResolutionFailed(String),

    #[error("The package {0} is listed in dependencies and dev-dependencies")]
    DuplicateDependency(EcoString),

    #[error("Expected package {expected} at path {path} but found {found} instead")]
    WrongDependencyProvided {
        path: Utf8PathBuf,
        expected: String,
        found: String,
    },

    #[error("The package {package} is provided multiple times, as {source_1} and {source_2}")]
    ProvidedDependencyConflict {
        package: String,
        source_1: String,
        source_2: String,
    },

    #[error("The package was missing required fields for publishing")]
    MissingHexPublishFields {
        description_missing: bool,
        licence_missing: bool,
    },

    #[error("Dependency {package:?} has not been published to Hex")]
    PublishNonHexDependencies { package: String },

    #[error("The package {package} uses unsupported build tools {build_tools:?}")]
    UnsupportedBuildTool {
        package: String,
        build_tools: Vec<EcoString>,
    },

    #[error("Opening docs at {path} failed: {error}")]
    FailedToOpenDocs { path: Utf8PathBuf, error: String },

    #[error("The package {package} requires a Gleam version satisfying {required_version} and you are using v{gleam_version}")]
    IncompatibleCompilerVersion {
        package: String,
        required_version: String,
        gleam_version: String,
    },

    #[error("The --javascript-prelude flag must be given when compiling to JavaScript")]
    JavaScriptPreludeRequired,

    #[error("The modules {unfinished:?} contain todo expressions and so cannot be published")]
    CannotPublishTodo { unfinished: Vec<EcoString> },
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
        P: AsRef<Utf8Path>,
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

            ResolutionError::ErrorRetrievingDependencies {
                package,
                version,
                source,
            } => format!(
                "An error occured while trying to retrieve dependencies of {package}@{version}: {source}",
            ),

            ResolutionError::DependencyOnTheEmptySet {
                package,
                version,
                dependent,
            } => format!(
                "{package}@{version} has an impossible dependency on {dependent}",
            ),

            ResolutionError::SelfDependency { package, version } => {
                format!("{package}@{version} somehow depends on itself.")
            }

            ResolutionError::ErrorChoosingPackageVersion(err) => {
                format!("Unable to determine package versions: {err}")
            }

            ResolutionError::ErrorInShouldCancel(err) => {
                format!("Dependency resolution was cancelled. {err}")
            }

            ResolutionError::Failure(err) => format!(
                "An unrecoverable error happened while solving dependencies: {err}"
            ),
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum InvalidProjectNameReason {
    Format,
    GleamPrefix,
    ErlangReservedWord,
    ErlangStandardLibraryModule,
    GleamReservedWord,
    GleamReservedModule,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FileIoAction {
    Link,
    Open,
    Copy,
    Read,
    Parse,
    Delete,
    // Rename,
    Create,
    WriteTo,
    Canonicalise,
    UpdatePermissions,
    FindParent,
    ReadMetadata,
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
            // FileIoAction::Rename => "rename",
            FileIoAction::Create => "create",
            FileIoAction::WriteTo => "write to",
            FileIoAction::FindParent => "find the parent of",
            FileIoAction::Canonicalise => "canonicalise",
            FileIoAction::UpdatePermissions => "update permissions of",
            FileIoAction::ReadMetadata => "read metadata of",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

fn did_you_mean(name: &str, options: &[EcoString]) -> Option<String> {
    // Find best match
    options
        .iter()
        .filter(|&option| option != crate::ast::CAPTURE_VARIABLE)
        .sorted()
        .min_by_key(|option| strsim::levenshtein(option, name))
        .map(|option| format!("Did you mean `{option}`?"))
}

impl Error {
    pub fn pretty_string(&self) -> String {
        self.to_diagnostic().pretty_string()
    }

    pub fn pretty(&self, buffer: &mut Buffer) {
        self.to_diagnostic().write(buffer)
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
                    hint: None,
                }
            }

            Error::InvalidProjectName { name, reason } => {
                let text = wrap_format!(
                    "We were not able to create your project as `{}` {}

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
                            "does not have the correct format. Project names \
must start with a lowercase letter and may only contain lowercase letters, \
numbers and underscores.",
                        InvalidProjectNameReason::GleamPrefix =>
                            "has the reserved prefix `gleam_`. \
This prefix is intended for official Gleam packages only.",
                    }
                );

                Diagnostic {
                    title: "Invalid project name".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::InvalidModuleName { module } => Diagnostic {
                title: "Invalid module name".into(),
                text: format!(
                    "`{module}` is not a valid module name.
Module names can only contain lowercase letters, underscore, and
forward slash and must not end with a slash."
                ),
                level: Level::Error,
                location: None,
                hint: None,
            },

            Error::ModuleDoesNotExist { module, suggestion } => {
                let hint = match suggestion {
                    Some(suggestion) => format!("Did you mean `{suggestion}`?"),
                    None => format!("Try creating the file `src/{module}.gleam`."),
                };
                Diagnostic {
                    title: "Module does not exist".into(),
                    text: format!("Module `{module}` was not found."),
                    level: Level::Error,
                    location: None,
                    hint: Some(hint),
                }
            }

            Error::ModuleDoesNotHaveMainFunction { module } => Diagnostic {
                title: "Module does not have a main function".into(),
                text: format!(
                    "`{module}` does not have a main function so the module can not be run."
                ),
                level: Level::Error,
                location: None,
                hint: Some(format!(
                    "Add a public `main` function to \
to `src/{module}.gleam`."
                )),
            },

            Error::MainFunctionDoesNotSupportTarget { module, target } => Diagnostic {
                title: "Target not supported".into(),
                text: wrap_format!(
                    "`{module}` has a main function, but it does not support the {target} \
target, so it cannot be run."
                ),
                level: Level::Error,
                location: None,
                hint: None,
            },

            Error::MainFunctionHasWrongArity { module, arity } => Diagnostic {
                title: "Main function has wrong arity".into(),
                text: format!(
                    "`{module}:main` should have an arity of 0 to be run but its arity is {arity}."
                ),
                level: Level::Error,
                location: None,
                hint: Some("Change the function signature of main to `pub fn main() {}`.".into()),
            },

            Error::ProjectRootAlreadyExist { path } => Diagnostic {
                title: "Project folder already exists".into(),
                text: format!("Project folder root:\n\n  {path}"),
                level: Level::Error,
                hint: None,
                location: None,
            },

            Error::OutputFilesAlreadyExist { file_names } => Diagnostic {
                title: format!(
                    "{} already exist{} in target directory",
                    if file_names.len() == 1 {
                        "File"
                    } else {
                        "Files"
                    },
                    if file_names.len() == 1 { "" } else { "s" }
                ),
                text: format!(
                    "{}
If you want to overwrite these files, delete them and run the command again.
",
                    file_names
                        .iter()
                        .map(|name| format!("  - {}", name.as_str()))
                        .join("\n")
                ),
                level: Level::Error,
                hint: None,
                location: None,
            },

            Error::CannotPublishTodo { unfinished } => Diagnostic {
                title: "Cannot publish unfinished code".into(),
                text: format!(
                    "These modules contain todo expressions and cannot be published:

{}

Please remove them and try again.
",
                    unfinished
                        .iter()
                        .map(|name| format!("  - {}", name.as_str()))
                        .join("\n")
                ),
                level: Level::Error,
                hint: None,
                location: None,
            },

            Error::UnableToFindProjectRoot { path } => Diagnostic {
                title: "Invalid project root".into(),
                text: format!("We were unable to find the project root:\n\n  {path}"),
                hint: None,
                level: Level::Error,
                location: None,
            },

            Error::VersionDoesNotMatch { toml_ver, app_ver } => {
                let text = format!(
                    "The version in gleam.toml \"{toml_ver}\" does not match the version in
your app.src file \"{app_ver}\"."
                );
                Diagnostic {
                    title: "Version does not match".into(),
                    hint: None,
                    text,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::ShellProgramNotFound { program } => {
                let mut text = format!("The program `{program}` was not found. Is it installed?");

                match program.as_str() {
                    "erl" | "erlc" | "escript" => text.push_str(
                        "
Documentation for installing Erlang can be viewed here:
https://gleam.run/getting-started/installing/",
                    ),
                    "rebar3" => text.push_str(
                        "
Documentation for installing rebar3 can be viewed here:
https://gleam.run/getting-started/installing/",
                    ),
                    _ => (),
                }
                match (program.as_str(), env::consts::OS) {
                    // TODO: Further suggestions for other OSes?
                    ("erl" | "erlc" | "escript", "macos") => text.push_str(
                        "
You can also install Erlang via homebrew using \"brew install erlang\"",
                    ),
                    ("rebar3", "macos") => text.push_str(
                        "
You can also install rebar3 via homebrew using \"brew install rebar3\"",
                    ),
                    _ => (),
                };

                Diagnostic {
                    title: "Program not found".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::ShellCommand {
                program: command,
                err: None,
            } => {
                let text =
                    format!("There was a problem when running the shell command `{command}`.");
                Diagnostic {
                    title: "Shell command failure".into(),
                    text,
                    hint: None,
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
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::Gzip(detail) => {
                let text = format!(
                    "There was a problem when applying gzip compression.

This was error from the gzip library:

    {detail}"
                );
                Diagnostic {
                    title: "Gzip compression failure".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::AddTar { path, err } => {
                let text = format!(
                    "There was a problem when attempting to add the file {path}
to a tar archive.

This was error from the tar library:

    {err}"
                );
                Diagnostic {
                    title: "Failure creating tar archive".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::ExpandTar { error } => {
                let text = format!(
                    "There was a problem when attempting to expand a to a tar archive.

This was error from the tar library:

    {error}"
                );
                Diagnostic {
                    title: "Failure opening tar archive".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::TarFinish(detail) => {
                let text = format!(
                    "There was a problem when creating a tar archive.

This was error from the tar library:

    {detail}"
                );
                Diagnostic {
                    title: "Failure creating tar archive".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::Hex(detail) => {
                let text = format!(
                    "There was a problem when using the Hex API.

This was error from the Hex client library:

    {detail}"
                );
                Diagnostic {
                    title: "Hex API failure".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::DuplicateModule {
                module,
                first,
                second,
            } => {
                let text = format!(
                    "The module `{module}` is defined multiple times.

First:  {first}
Second: {second}"
                );

                Diagnostic {
                    title: "Duplicate module".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::DuplicateSourceFile { file } => Diagnostic {
                title: "Duplicate Source file".into(),
                text: format!("The file `{file}` is defined multiple times."),
                hint: None,
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
                    Some(e) => {
                        format!("\nThe error message from the file IO library was:\n\n    {e}\n")
                    }
                    None => "".into(),
                };
                let text = format!(
                    "An error occurred while trying to {} this {}:

    {}
{}",
                    action.text(),
                    kind.text(),
                    path,
                    err,
                );
                Diagnostic {
                    title: "File IO failure".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::NonUtf8Path { path: _ } => {
                let text =
                    "Encountered a non UTF-8 path, but only UTF-8 paths are supported.".to_owned();
                Diagnostic {
                    title: "Non UTF-8 Path Encountered".into(),
                    text,
                    level: Level::Error,
                    location: None,
                    hint: None,
                }
            }

            Error::GitInitialization { error } => {
                let text = format!(
                    "An error occurred while trying make a git repository for this project:

    {error}"
                );
                Diagnostic {
                    title: "Failed to initialize git repository".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::Type { path, src, error } => match error {
                TypeError::SrcImportingTest {
                    location,
                    src_module,
                    test_module,
                } => {
                    let text = wrap_format!(
                        "The application module `{src_module}` is importing the test module `{test_module}`.

Test modules are not included in production builds so test \
modules cannot import them. Perhaps move the `{test_module}` module to the src directory.",
                    );

                    Diagnostic {
                        title: "App importing test module".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("Imported here".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::UnknownLabels {
                    unknown,
                    valid,
                    supplied,
                } => {
                    let other_labels: Vec<_> = valid
                        .iter()
                        .filter(|label| !supplied.contains(label))
                        .cloned()
                        .collect();

                    let title = if unknown.len() > 1 {
                        "Unknown labels"
                    } else {
                        "Unknown label"
                    }
                    .into();

                    let mut labels = unknown.iter().map(|(label, location)| {
                        let text = did_you_mean(label, &other_labels)
                            .unwrap_or_else(|| "Unexpected label".into());
                        Label {
                            text: Some(text),
                            span: *location,
                        }
                    });
                    let label = labels.next().expect("Unknown labels first label");
                    let extra_labels = labels.collect();
                    let text = if valid.is_empty() {
                        "This constructor does not accept any labelled arguments.".into()
                    } else if other_labels.is_empty() {
                        "You have already supplied all the labelled arguments that this
constructor accepts."
                            .into()
                    } else {
                        let mut label_text = String::from("It accepts these labels:\n");
                        for label in other_labels.iter().sorted() {
                            label_text.push_str("\n    ");
                            label_text.push_str(label);
                        }
                        label_text
                    };
                    Diagnostic {
                        title,
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label,
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels,
                        }),
                    }
                }

                TypeError::UnexpectedLabelledArg { location, label } => {
                    let text = format!(
                        "This argument has been given a label but the constructor does
not expect any. Please remove the label `{label}`."
                    );
                    Diagnostic {
                        title: "Unexpected labelled argument".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::PositionalArgumentAfterLabelled { location } => {
                    let text =
                        "This unlabeled argument has been supplied after a labelled argument.
Once a labelled argument has been supplied all following arguments must
also be labelled."
                            .into();
                    Diagnostic {
                        title: "Unexpected positional argument".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::DuplicateImport {
                    location,
                    previous_location,
                    name,
                } => {
                    let text = format!(
                        "`{name}` has been imported multiple times.
Names in a Gleam module must be unique so one will need to be renamed."
                    );
                    Diagnostic {
                        title: "Duplicate import".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("Reimported here".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![Label {
                                text: Some("First imported here".into()),
                                span: *previous_location,
                            }],
                        }),
                    }
                }

                TypeError::DuplicateName {
                    location_a,
                    location_b,
                    name,
                    ..
                } => {
                    let (first_location, second_location) = if location_a.start < location_b.start {
                        (location_a, location_b)
                    } else {
                        (location_b, location_a)
                    };
                    let text = format!(
                        "`{name}` has been defined multiple times.
Names in a Gleam module must be unique so one will need to be renamed."
                    );
                    Diagnostic {
                        title: "Duplicate definition".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("Redefined here".into()),
                                span: *second_location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![Label {
                                text: Some("First defined here".into()),
                                span: *first_location,
                            }],
                        }),
                    }
                }

                TypeError::DuplicateTypeName {
                    name,
                    location,
                    previous_location,
                    ..
                } => {
                    let text = format!(
                        "The type `{name}` has been defined multiple times.
Names in a Gleam module must be unique so one will need to be renamed."
                    );
                    Diagnostic {
                        title: "Duplicate type definition".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("Redefined here".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![Label {
                                text: Some("First defined here".into()),
                                span: *previous_location,
                            }],
                        }),
                    }
                }

                TypeError::DuplicateField { location, label } => {
                    let text =
                        format!("The field `{label}` has already been defined. Rename this field.");
                    Diagnostic {
                        title: "Duplicate field".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::DuplicateArgument { location, label } => {
                    let text =
                        format!("The labelled argument `{label}` has already been supplied.");
                    Diagnostic {
                        title: "Duplicate argument".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::RecursiveType { location } => {
                    let text = "I don't know how to work out what type this value has. It seems
to be defined in terms of itself.

Hint: Add some type annotations and try again."
                        .into();
                    Diagnostic {
                        title: "Recursive type".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::NotFn { location, typ } => {
                    let mut printer = Printer::new();
                    let text = format!(
                        "This value is being called as a function but its type is:\n\n{}",
                        printer.pretty_print(typ, 4)
                    );
                    Diagnostic {
                        title: "Type mismatch".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::UnknownRecordField {
                    usage,
                    location,
                    typ,
                    label,
                    fields,
                } => {
                    let mut printer = Printer::new();

                    // Give a hint about what type this value has.
                    let mut text = format!(
                        "The value being accessed has this type:\n\n{}\n",
                        printer.pretty_print(typ, 4)
                    );

                    // Give a hint about what record fields this value has, if any.
                    if fields.is_empty() {
                        text.push_str("\nIt does not have any fields.");
                    } else {
                        text.push_str("\nIt has these fields:\n");
                    }
                    for field in fields.iter().sorted() {
                        text.push_str("\n    .");
                        text.push_str(field);
                    }

                    // Give a hint about Gleam not having OOP methods if it
                    // looks like they might be trying to call one.
                    match usage {
                        FieldAccessUsage::MethodCall => {
                            let msg = wrap(
                                "Gleam is not object oriented, so if you are trying \
to call a method on this value you may want to use the function syntax instead.",
                            );
                            text.push_str("\n\n");
                            text.push_str(&msg);
                            text.push_str("\n\n    ");
                            text.push_str(label);
                            text.push_str("(value)");
                        }
                        FieldAccessUsage::Other => (),
                    }

                    let label = did_you_mean(label, fields)
                        .unwrap_or_else(|| "This field does not exist".into());
                    Diagnostic {
                        title: "Unknown record field".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some(label),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::CouldNotUnify {
                    location,
                    expected,
                    given,
                    situation: Some(UnifyErrorSituation::Operator(op)),
                    rigid_type_names: annotated_names,
                } => {
                    let mut printer = Printer::new();
                    printer.with_names(annotated_names.clone());
                    let mut text = format!(
                        "The {op} operator expects arguments of this type:

{expected}

But this argument has this type:

{given}\n",
                        op = op.name(),
                        expected = printer.pretty_print(expected, 4),
                        given = printer.pretty_print(given, 4),
                    );
                    if let Some(hint) = hint_alternative_operator(op, given) {
                        text.push('\n');
                        text.push_str("Hint: ");
                        text.push_str(&hint);
                    }
                    Diagnostic {
                        title: "Type mismatch".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::CouldNotUnify {
                    location,
                    expected,
                    given,
                    situation: Some(UnifyErrorSituation::PipeTypeMismatch),
                    rigid_type_names: annotated_names,
                } => {
                    // Remap the pipe function type into just the type expected by the pipe.
                    let expected = expected
                        .fn_types()
                        .and_then(|(args, _)| args.first().cloned());

                    // Remap the argument as well, if it's a function.
                    let given = given
                        .fn_types()
                        .and_then(|(args, _)| args.first().cloned())
                        .unwrap_or_else(|| given.clone());

                    let mut printer = Printer::new();
                    printer.with_names(annotated_names.clone());
                    let text = format!(
                        "The argument is:

{given}

But function expects:

{expected}",
                        expected = expected
                            .map(|v| printer.pretty_print(&v, 4))
                            .unwrap_or_else(|| "    No arguments".into()),
                        given = printer.pretty_print(&given, 4)
                    );

                    Diagnostic {
                        title: "Type mismatch".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("This function does not accept the piped type".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::CouldNotUnify {
                    location,
                    expected,
                    given,
                    situation,
                    rigid_type_names: annotated_names,
                } => {
                    let mut printer = Printer::new();
                    printer.with_names(annotated_names.clone());
                    let mut text =
                        if let Some(description) = situation.and_then(|s| s.description()) {
                            let mut text = description.to_string();
                            text.push('\n');
                            text.push('\n');
                            text
                        } else {
                            "".into()
                        };
                    text.push_str("Expected type:\n\n");
                    text.push_str(&printer.pretty_print(expected, 4));
                    text.push_str("\n\nFound type:\n\n");
                    text.push_str(&printer.pretty_print(given, 4));
                    Diagnostic {
                        title: "Type mismatch".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::IncorrectTypeArity {
                    location,
                    expected,
                    given,
                    ..
                } => {
                    let text = "Functions and constructors have to be called with their expected
number of arguments."
                        .into();
                    let expected = match expected {
                        0 => "no arguments".into(),
                        1 => "1 argument".into(),
                        _ => format!("{expected} arguments"),
                    };
                    Diagnostic {
                        title: "Incorrect arity".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some(format!("Expected {expected}, got {given}")),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::IncorrectArity {
                    labels,
                    location,
                    expected,
                    given,
                } => {
                    let text = if labels.is_empty() {
                        "".into()
                    } else {
                        let labels = labels
                            .iter()
                            .map(|p| format!("  - {p}"))
                            .sorted()
                            .join("\n");
                        format!(
                            "This call accepts these additional labelled arguments:\n\n{labels}",
                        )
                    };
                    let expected = match expected {
                        0 => "no arguments".into(),
                        1 => "1 argument".into(),
                        _ => format!("{expected} arguments"),
                    };
                    let label = format!("Expected {expected}, got {given}");
                    Diagnostic {
                        title: "Incorrect arity".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some(label),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::UnnecessarySpreadOperator { location, arity } => {
                    let text = wrap_format!(
                        "This record has {arity} fields and you have already \
assigned variables to all of them."
                    );
                    Diagnostic {
                        title: "Unnecessary spread operator".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::UpdateMultiConstructorType { location } => {
                    let text = "This type has multiple constructors so it cannot be safely updated.
If this value was one of the other variants then the update would be
produce incorrect results.

Consider pattern matching on it with a case expression and then
constructing a new record with its values."
                        .into();

                    Diagnostic {
                        title: "Unsafe record update".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some(
                                    "I can't tell this is always the right constructor".into(),
                                ),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::UnknownType {
                    location,
                    name,
                    hint,
                } => {
                    let label_text = match hint {
                        UnknownTypeHint::AlternativeTypes(types) => did_you_mean(name, types),
                        UnknownTypeHint::ValueInScopeWithSameName => None,
                    };

                    let mut text = wrap_format!(
                        "The type `{name}` is not defined or imported in this module."
                    );

                    match hint {
                        UnknownTypeHint::ValueInScopeWithSameName => {
                            let hint = wrap_format!(
                                "There is a value in scope with the name `{name}`, but no type in scope with that name."
                            );
                            text.push('\n');
                            text.push_str(hint.as_str());
                        }
                        UnknownTypeHint::AlternativeTypes(_) => {}
                    };

                    Diagnostic {
                        title: "Unknown type".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: label_text,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::UnknownVariable {
                    location,
                    variables,
                    name,
                } => Diagnostic {
                    title: "Unknown variable".into(),
                    text: wrap_format!("The name `{name}` is not in scope here."),
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: did_you_mean(name, variables),
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                },

                TypeError::PrivateTypeLeak { location, leaked } => {
                    let mut printer = Printer::new();

                    // TODO: be more precise.
                    // - is being returned by this public function
                    // - is taken as an argument by this public function
                    // - is taken as an argument by this public enum constructor
                    // etc
                    let text = format!(
                        "The following type is private, but is being used by this public export.

{}

Private types can only be used within the module that defines them.",
                        printer.pretty_print(leaked, 4),
                    );
                    Diagnostic {
                        title: "Private type used in public interface".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::UnknownModule {
                    location,
                    name,
                    imported_modules,
                } => Diagnostic {
                    title: "Unknown module".into(),
                    text: format!("No module has been found with the name `{name}`."),
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: did_you_mean(name, imported_modules),
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                },

                TypeError::UnknownModuleType {
                    location,
                    name,
                    module_name,
                    type_constructors,
                } => {
                    let text =
                        format!("The module `{module_name}` does not have a `{name}` type.",);
                    Diagnostic {
                        title: "Unknown module type".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: did_you_mean(name, type_constructors),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::UnknownModuleValue {
                    location,
                    name,
                    module_name,
                    value_constructors,
                } => {
                    let text =
                        format!("The module `{module_name}` does not have a `{name}` value.",);
                    Diagnostic {
                        title: "Unknown module field".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: did_you_mean(name, value_constructors),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::UnknownModuleField {
                    location,
                    name,
                    module_name,
                    type_constructors,
                    value_constructors,
                } => {
                    let options: Vec<_> = type_constructors
                        .iter()
                        .chain(value_constructors)
                        .cloned()
                        .collect();
                    let text =
                        format!("The module `{module_name}` does not have a `{name}` field.",);
                    Diagnostic {
                        title: "Unknown module field".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: did_you_mean(name, &options),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::IncorrectNumClausePatterns {
                    location,
                    expected,
                    given,
                } => {
                    let text = wrap_format!(
                        "This case expression has {expected} subjects, but this pattern matches {given}.
Each clause must have a pattern for every subject value.",
                    );
                    Diagnostic {
                        title: "Incorrect number of patterns".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some(format!("Expected {expected} patterns, got {given}")),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::NonLocalClauseGuardVariable { location, name } => {
                    let text = wrap_format!(
                        "Variables used in guards must be either defined in the \
function, or be an argument to the function. The variable `{name}` is not defined locally.",
                    );
                    Diagnostic {
                        title: "Invalid guard variable".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("Is not locally defined".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::ExtraVarInAlternativePattern { location, name } => {
                    let text = wrap_format!(
"All alternative patterns must define the same variables as the initial pattern. \
This variable `{name}` has not been previously defined.",
                    );
                    Diagnostic {
                        title: "Extra alternative pattern variable".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("Has not been previously defined".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::MissingVarInAlternativePattern { location, name } => {
                    let text = wrap_format!(
                        "All alternative patterns must define the same variables \
as the initial pattern, but the `{name}` variable is missing.",
                    );
                    Diagnostic {
                        title: "Missing alternative pattern variable".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("This does not define all required variables".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::DuplicateVarInPattern { location, name } => {
                    let text = wrap_format!(
                        "Variables can only be used once per pattern. This \
variable `{name}` appears multiple times.
If you used the same variable twice deliberately in order to check for equality \
please use a guard clause instead.
e.g. (x, y) if x == y -> ...",
                    );
                    Diagnostic {
                        title: "Duplicate variable in pattern".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("This has already been used".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::OutOfBoundsTupleIndex {
                    location, size: 0, ..
                } => Diagnostic {
                    title: "Out of bounds tuple index".into(),
                    text: "This tuple has no elements so it cannot be indexed at all.".into(),
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: None,
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                },

                TypeError::OutOfBoundsTupleIndex {
                    location,
                    index,
                    size,
                } => {
                    let text = wrap_format!(
                        "The index being accessed for this tuple is {}, but this \
tuple has {} elements so the highest valid index is {}.",
                        index,
                        size,
                        size - 1,
                    );
                    Diagnostic {
                        title: "Out of bounds tuple index".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("This index is too large".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::NotATuple { location, given } => {
                    let mut printer = Printer::new();
                    let text = format!(
                        "To index into this value it needs to be a tuple, however it has this type:

{}",
                        printer.pretty_print(given, 4),
                    );
                    Diagnostic {
                        title: "Type mismatch".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("This is not a tuple".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::NotATupleUnbound { location } => {
                    let text = "To index into a tuple we need to know it size, but we don't know
anything about this type yet. Please add some type annotations so
we can continue."
                        .into();
                    Diagnostic {
                        title: "Type mismatch".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("What type is this?".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::RecordAccessUnknownType { location } => {
                    let text = "In order to access a record field we need to know what type it is,
but I can't tell the type here. Try adding type annotations to your
function and try again."
                        .into();
                    Diagnostic {
                        title: "Unknown type for record access".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("I don't know what type this is".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::BitArraySegmentError { error, location } => {
                    let (label, mut extra) = match error {
                        bit_array::ErrorType::ConflictingTypeOptions { existing_type } => (
                            "This is an extra type specifier",
                            vec![format!("Hint: This segment already has the type {existing_type}.")],
                        ),

                        bit_array::ErrorType::ConflictingSignednessOptions {
                            existing_signed
                        } => (
                            "This is an extra signedness specifier",
                            vec![format!(
                                "Hint: This segment already has a signedness of {existing_signed}."
                            )],
                        ),

                        bit_array::ErrorType::ConflictingEndiannessOptions {
                            existing_endianness
                        } => (
                            "This is an extra endianness specifier",
                            vec![format!(
                                "Hint: This segment already has an endianness of {existing_endianness}."
                            )],
                        ),

                        bit_array::ErrorType::ConflictingSizeOptions => (
                            "This is an extra size specifier",
                            vec!["Hint: This segment already has a size.".into()],
                        ),

                        bit_array::ErrorType::ConflictingUnitOptions => (
                            "This is an extra unit specifier",
                            vec!["Hint: A BitArray segment can have at most 1 unit.".into()],
                        ),

                        bit_array::ErrorType::FloatWithSize => (
                            "Invalid float size",
                            vec!["Hint: floats have an exact size of 16/32/64 bits.".into()],
                        ),

                        bit_array::ErrorType::InvalidEndianness => (
                            "This option is invalid here",
                                vec![wrap("Hint: signed and unsigned can only be used with \
int, float, utf16 and utf32 types.")],
                        ),

                        bit_array::ErrorType::OptionNotAllowedInValue => (
                            "This option is only allowed in BitArray patterns",
                            vec!["Hint: This option has no effect in BitArray values.".into()],
                        ),

                        bit_array::ErrorType::SignednessUsedOnNonInt { typ } => (
                            "Signedness is only valid with int types",
                            vec![format!("Hint: This segment has a type of {typ}")],
                        ),
                        bit_array::ErrorType::TypeDoesNotAllowSize { typ } => (
                            "Size cannot be specified here",
                            vec![format!("Hint: {typ} segments have an autoatic size.")],
                        ),
                        bit_array::ErrorType::TypeDoesNotAllowUnit { typ } => (
                            "Unit cannot be specified here",
                            vec![wrap(&format!("Hint: {typ} segments are sized based on their value \
and cannot have a unit."))],
                        ),
                        bit_array::ErrorType::VariableUtfSegmentInPattern => (
                            "This cannot be a variable",
                            vec![wrap("Hint: in patterns utf8, utf16, and utf32  must be an exact string.")],
                        ),
                        bit_array::ErrorType::SegmentMustHaveSize => (
                            "This segment has no size",
                            vec![wrap("Hint: Bit array segments without a size are only \
allowed at the end of a bin pattern.")],
                        ),
                        bit_array::ErrorType::UnitMustHaveSize => (
                            "This needs an explicit size",
                            vec!["Hint: If you specify unit() you must also specify size().".into()],
                        ),
                    };
                    extra.push("See: https://gleam.run/book/tour/bit-arrays.html".into());
                    let text = extra.join("\n");
                    Diagnostic {
                        title: "Invalid bit array segment".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some(label.into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::RecordUpdateInvalidConstructor { location } => Diagnostic {
                    title: "Invalid record constructor".into(),
                    text: "Only record constructors can be used with the update syntax.".into(),
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: Some("This is not a record constructor".into()),
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                },

                TypeError::UnexpectedTypeHole { location } => Diagnostic {
                    title: "Unexpected type hole".into(),
                    text: "We need to know the exact type here so type holes cannot be used."
                        .into(),
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: Some("I need to know what this is".into()),
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                },

                TypeError::ReservedModuleName { name } => {
                    let text = format!(
                        "The module name `{name}` is reserved.
Try a different name for this module."
                    );
                    Diagnostic {
                        title: "Reserved module name".into(),
                        text,
                        hint: None,
                        location: None,
                        level: Level::Error,
                    }
                }

                TypeError::KeywordInModuleName { name, keyword } => {
                    let text = wrap(&format!(
                        "The module name `{name}` contains the keyword `{keyword}`, so importing \
it would be a syntax error.
Try a different name for this module."
                    ));
                    Diagnostic {
                        title: "Invalid module name".into(),
                        text,
                        hint: None,
                        location: None,
                        level: Level::Error,
                    }
                }

                TypeError::NotExhaustivePatternMatch {
                    location,
                    unmatched,
                    kind,
                } => {
                    let mut text = match kind {
                        PatternMatchKind::Case => {
                            "This case expression does not match all possibilities.
Each constructor must have a pattern that matches it or
else it could crash."
                        }
                        PatternMatchKind::Assignment => {
                            "This assignment does not match all possibilities.
Either use a case expression with patterns for each possible
value, or use `let assert` rather than `let`."
                        }
                    }
                    .to_string();

                    text.push_str("\n\nThese values are not matched:\n\n");
                    for unmatched in unmatched {
                        text.push_str("  - ");
                        text.push_str(unmatched);
                        text.push('\n');
                    }
                    Diagnostic {
                        title: "Not exhaustive pattern match".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::ArgumentNameAlreadyUsed { location, name } => Diagnostic {
                    title: "Argument name already used".into(),
                    text: format!("Two `{name}` arguments have been defined for this function."),
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: None,
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                },

                TypeError::UnlabelledAfterlabelled { location } => Diagnostic {
                    title: "Unlabelled argument after labelled argument".into(),
                    text: wrap("All unlabelled arguments must come before any labelled arguments."),
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: None,
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                },

                TypeError::RecursiveTypeAlias { location, cycle } => {
                    let mut text = "This type alias is defined in terms of itself.\n".into();
                    write_cycle(&mut text, cycle);
                    text.push_str(
                        "If we tried to compile this recursive type it would expand
forever in a loop, and we'd never get the final type.",
                    );
                    Diagnostic {
                        title: "Type cycle".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::ExternalMissingAnnotation { location, kind } => {
                    let kind = match kind {
                        MissingAnnotation::Parameter => "parameter",
                        MissingAnnotation::Return => "return",
                    };
                    let text = format!(
                        "A {kind} annotation is missing from this function.

Functions with external implementations must have type annotations
so we can tell what type of values they accept and return.",
                    );
                    Diagnostic {
                        title: "Missing type annotation".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::NoImplementation { location } => {
                    let text = "We can't compile this function as it doesn't have an
implementation. Add a body or an external implementation
using the `@external` attribute."
                        .into();
                    Diagnostic {
                        title: "Function without an implementation".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::InvalidExternalJavascriptModule {
                    location,
                    name,
                    module,
                } => {
                    let text = wrap_format!(
                        "The function `{name}` has an external JavaScript \
implementation but the module path `{module}` is not valid."
                    );
                    Diagnostic {
                        title: "Invalid JavaScript module".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::InvalidExternalJavascriptFunction {
                    location,
                    name,
                    function,
                } => {
                    let text = wrap_format!(
                        "The function `{name}` has an external JavaScript \
implementation but the function name `{function}` is not valid."
                    );
                    Diagnostic {
                        title: "Invalid JavaScript function".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::InexhaustiveLetAssignment { location, missing } => {
                    let mut text: String =
                        "This assignment uses a pattern that does not match all possible
values. If one of the other values is used then the assignment
will crash.

The missing patterns are:\n"
                            .into();
                    for missing in missing {
                        text.push_str("\n    ");
                        text.push_str(missing);
                    }
                    text.push('\n');

                    Diagnostic {
                        title: "Inexhaustive pattern".into(),
                        text,
                        hint: Some(
                            "Use a more general pattern or use `let assert` instead.".into(),
                        ),
                        level: Level::Error,
                        location: Some(Location {
                            src: src.clone(),
                            path: path.to_path_buf(),
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                TypeError::InexhaustiveCaseExpression { location, missing } => {
                    let mut text: String =
                        "This case expression does not have a pattern for all possible values.
If is run on one of the values without a pattern then it will crash.

The missing patterns are:\n"
                            .into();
                    for missing in missing {
                        text.push_str("\n    ");
                        text.push_str(missing);
                    }
                    Diagnostic {
                        title: "Inexhaustive patterns".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            src: src.clone(),
                            path: path.to_path_buf(),
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                TypeError::UnsupportedExpressionTarget {
                    location,
                    target: current_target,
                } => {
                    let text = wrap_format!(
                        "This value is not available as it is defined using externals, \
and there is no implementation for the {} target.",
                        match current_target {
                            Target::Erlang => "Erlang",
                            Target::JavaScript => "JavaScript",
                        }
                    );
                    Diagnostic {
                        title: "Unsupported target".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            path: path.clone(),
                            src: src.clone(),
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::UnsupportedPublicFunctionTarget {
                    location,
                    name,
                    target,
                } => {
                    let target = match target {
                        Target::Erlang => "Erlang",
                        Target::JavaScript => "JavaScript",
                    };
                    let text = wrap_format!(
                        "The `{name}` function is public but doesn't have an \
implementation for the {target} target. All public functions of a package \
must be able to compile for a module to be valid."
                    );
                    Diagnostic {
                        title: "Unsupported target".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            path: path.clone(),
                            src: src.clone(),
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::UnusedTypeAliasParameter { location, name } => {
                    let text = wrap_format!(
                        "The type variable `{name}` is unused. It can be safely removed.",
                    );
                    Diagnostic {
                        title: "Unused type parameter".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            path: path.clone(),
                            src: src.clone(),
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: vec![],
                        }),
                    }
                }

                TypeError::DuplicateTypeParameter { location, name } => {
                    let text = wrap_format!(
                        "This definition has multiple type parameters named `{name}`.
Rename or remove one of them.",
                    );
                    Diagnostic {
                        title: "Duplicate type parameter".into(),
                        text,
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            path: path.clone(),
                            src: src.clone(),
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: vec![],
                        }),
                    }
                }
            },

            Error::Parse { path, src, error } => {
                let (label, extra) = error.details();
                let text = extra.join("\n");

                let adjusted_location = if error.error == ParseErrorType::UnexpectedEof {
                    crate::ast::SrcSpan {
                        start: (src.len() - 1) as u32,
                        end: (src.len() - 1) as u32,
                    }
                } else {
                    error.location
                };

                Diagnostic {
                    title: "Syntax error".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: Some(label.to_string()),
                            span: adjusted_location,
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                }
            }

            Error::ImportCycle { modules } => {
                let mut text = "The import statements for these modules form a cycle:
"
                .into();
                write_cycle(&mut text, modules);
                text.push_str(
                    "Gleam doesn't support dependency cycles like these, please break the
cycle to continue.",
                );
                Diagnostic {
                    title: "Import cycle".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::PackageCycle { packages } => {
                let mut text = "The dependencies for these packages form a cycle:
"
                .into();
                write_cycle(&mut text, packages);
                text.push_str(
                    "Gleam doesn't support dependency cycles like these, please break the
cycle to continue.",
                );
                Diagnostic {
                    title: "Dependency cycle".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::UnknownImport { import, details } => {
                let UnknownImportDetails {
                    module,
                    location,
                    path,
                    src,
                    modules,
                } = details.as_ref();
                let text = wrap(&format!(
                    "The module `{module}` is trying to import the module `{import}`, \
but it cannot be found."
                ));
                Diagnostic {
                    title: "Unknown import".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: did_you_mean(import, modules),
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.clone(),
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
                    hint: None,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::Format { problem_files } => {
                let files: Vec<_> = problem_files
                    .iter()
                    .map(|formatted| formatted.source.as_str())
                    .map(|p| format!("  - {p}"))
                    .sorted()
                    .collect();
                let mut text = files.iter().join("\n");
                text.push('\n');
                Diagnostic {
                    title: "These files have not been formatted".into(),
                    text,
                    hint: None,
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
                    title: format!("{count} {word_warning} generated."),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::JavaScript { src, path, error } => match error {
                javascript::Error::Unsupported { feature, location } => Diagnostic {
                    title: "Unsupported feature for compilation target".into(),
                    text: format!("{feature} is not supported for JavaScript compilation."),
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: None,
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.clone(),
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
                    "A problem was encountered when downloading `{package_name}` {package_version}.
The error from the package manager client was:

    {error}"
                );
                Diagnostic {
                    title: "Failed to download package".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::Http(error) => {
                let text = format!(
                    "A HTTP request failed.
The error from the HTTP client was:

    {error}"
                );
                Diagnostic {
                    title: "HTTP error".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::InvalidVersionFormat { input, error } => {
                let text = format!(
                    "I was unable to parse the version \"{input}\".
The error from the parser was:

    {error}"
                );
                Diagnostic {
                    title: "Invalid version format".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::DependencyCanonicalizationFailed(package) => {
                let text = format!("Local package `{package}` has no canonical path");

                Diagnostic {
                    title: "Failed to create canonical path".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::DependencyResolutionFailed(error) => {
                let text = format!(
                    "An error occurred while determining what dependency packages and
versions should be downloaded.
The error from the version resolver library was:

{}",
                    wrap(error)
                );
                Diagnostic {
                    title: "Dependency resolution failed".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::GitDependencyUnsupported => Diagnostic {
                title: "Git dependencies are not currently supported".into(),
                text: "Please remove all git dependencies from the gleam.toml file".into(),
                hint: None,
                location: None,
                level: Level::Error,
            },

            Error::WrongDependencyProvided {
                path,
                expected,
                found,
            } => {
                let text = format!(
                    "Expected package `{expected}` at path `{path}` but found `{found}` instead.",
                );

                Diagnostic {
                    title: "Wrong dependency provided".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::ProvidedDependencyConflict {
                package,
                source_1,
                source_2,
            } => {
                let text = format!(
                    "The package `{package}` is provided as both `{source_1}` and `{source_2}`.",
                );

                Diagnostic {
                    title: "Conflicting provided dependencies".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::DuplicateDependency(name) => {
                let text = format!(
                    "The package `{name}` is specified in both the dependencies and
dev-dependencies sections of the gleam.toml file."
                );
                Diagnostic {
                    title: "Dependency duplicated".into(),
                    text,
                    hint: None,
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
                        .to_string();
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
                    hint: None,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::PublishNonHexDependencies { package } => Diagnostic {
                title: "Unblished dependencies".into(),
                text: wrap_format!(
                    "The package cannot be published to Hex \
because dependency `{package}` is not a Hex dependency.",
                ),
                hint: None,
                location: None,
                level: Level::Error,
            },

            Error::UnsupportedBuildTool {
                package,
                build_tools,
            } => {
                let text = wrap_format!(
                    "The package `{}` cannot be built as it does not use \
a build tool supported by Gleam. It uses {:?}.

If you would like us to support this package please let us know by opening an \
issue in our tracker: https://github.com/gleam-lang/gleam/issues",
                    package,
                    build_tools
                );
                Diagnostic {
                    title: "Unsupported build tool".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::FailedToOpenDocs { path, error } => {
                let error = format!("\nThe error message from the library was:\n\n    {error}\n");
                let text = format!(
                    "An error occurred while trying to open the docs:

    {path}
{error}",
                );
                Diagnostic {
                    title: "Failed to open docs".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }
            }

            Error::IncompatibleCompilerVersion {
                package,
                required_version,
                gleam_version,
            } => {
                let text = format!(
                    "The package `{package}` requires a Gleam version satisfying {required_version} \
but you are using v{gleam_version}.",
                );
                Diagnostic {
                    title: "Incompatible Gleam version".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::InvalidRuntime {
                target,
                invalid_runtime,
            } => {
                let text = format!("Invalid runtime for {target} target: {invalid_runtime}");

                let hint = match target {
                    Target::JavaScript => {
                        Some("available runtimes for JavaScript are: node, deno.".into())
                    }
                    Target::Erlang => Some(
                        "You can not set a runtime for Erlang. Did you mean to target JavaScript?"
                            .into(),
                    ),
                };

                Diagnostic {
                    title: format!("Invalid runtime for {target}"),
                    text,
                    hint,
                    location: None,
                    level: Level::Error,
                }
            }

            Error::JavaScriptPreludeRequired => Diagnostic {
                title: "JavaScript prelude required".into(),
                text: "The --javascript-prelude flag must be given when compiling to JavaScript."
                    .into(),
                level: Level::Error,
                location: None,
                hint: None,
            },
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

fn write_cycle(buffer: &mut String, cycle: &[EcoString]) {
    buffer.push_str(
        "
    ┌─────┐\n",
    );
    for (index, name) in cycle.iter().enumerate() {
        if index != 0 {
            buffer.push_str("    │     ↓\n");
        }
        buffer.push_str("    │     ");
        buffer.push_str(name);
        buffer.push('\n');
    }
    buffer.push_str("    └─────┘\n");
}

fn hint_alternative_operator(op: &BinOp, given: &Type) -> Option<String> {
    match op {
        BinOp::AddInt if given.is_float() => Some(hint_numeric_message("+.", "Float")),
        BinOp::DivInt if given.is_float() => Some(hint_numeric_message("/.", "Float")),
        BinOp::GtEqInt if given.is_float() => Some(hint_numeric_message(">=.", "Float")),
        BinOp::GtInt if given.is_float() => Some(hint_numeric_message(">.", "Float")),
        BinOp::LtEqInt if given.is_float() => Some(hint_numeric_message("<=.", "Float")),
        BinOp::LtInt if given.is_float() => Some(hint_numeric_message("<.", "Float")),
        BinOp::MultInt if given.is_float() => Some(hint_numeric_message("*.", "Float")),
        BinOp::SubInt if given.is_float() => Some(hint_numeric_message("-.", "Float")),

        BinOp::AddFloat if given.is_int() => Some(hint_numeric_message("+", "Int")),
        BinOp::DivFloat if given.is_int() => Some(hint_numeric_message("/", "Int")),
        BinOp::GtEqFloat if given.is_int() => Some(hint_numeric_message(">=", "Int")),
        BinOp::GtFloat if given.is_int() => Some(hint_numeric_message(">", "Int")),
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
    format!("the {alt} operator can be used with {type_}s\n")
}

fn hint_string_message() -> String {
    wrap(
        "Strings can be joined using the `append` or `concat` \
functions from the `gleam/string` module.",
    )
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unformatted {
    pub source: Utf8PathBuf,
    pub destination: Utf8PathBuf,
    pub input: EcoString,
    pub output: String,
}

pub fn wrap(text: &str) -> String {
    textwrap::fill(text, std::cmp::min(75, textwrap::termwidth()))
}
