#![allow(clippy::unwrap_used, clippy::expect_used)]
use crate::bit_array::UnsupportedOption;
use crate::build::{Origin, Outcome, Runtime, Target};
use crate::dependency::{PackageFetcher, ResolutionError};
use crate::diagnostic::{Diagnostic, ExtraLabel, Label, Location};

use crate::derivation_tree::DerivationTreePrinter;
use crate::parse::error::ParseErrorDetails;
use crate::strings::{to_snake_case, to_upper_camel_case};
use crate::type_::collapse_links;
use crate::type_::error::{
    IncorrectArityContext, InvalidImportKind, MissingAnnotation, ModuleValueUsageContext, Named,
    RecordField, UnknownField, UnknownTypeHint, UnsafeRecordUpdateReason,
};
use crate::type_::printer::{Names, Printer};
use crate::type_::{FieldAccessUsage, error::PatternMatchKind};
use crate::{ast::BinOp, parse::error::ParseErrorType, type_::Type};
use crate::{bit_array, diagnostic::Level, type_::UnifyErrorSituation};
use ecow::EcoString;
use hexpm::version::Version;
use itertools::Itertools;
use std::borrow::Cow;
use std::fmt::{Debug, Display};
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use termcolor::Buffer;
use thiserror::Error;
use vec1::Vec1;

use camino::{Utf8Path, Utf8PathBuf};

pub type Name = EcoString;

pub type Result<Ok, Err = Error> = std::result::Result<Ok, Err>;

#[cfg(test)]
pub mod tests;

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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ImportCycleLocationDetails {
    pub location: crate::ast::SrcSpan,
    pub path: Utf8PathBuf,
    pub src: EcoString,
}

#[derive(Debug, Eq, PartialEq, Error, Clone)]
pub enum Error {
    #[error("failed to parse Gleam source code")]
    Parse {
        path: Utf8PathBuf,
        src: EcoString,
        error: Box<crate::parse::error::ParseError>,
    },

    #[error("type checking failed")]
    Type {
        path: Utf8PathBuf,
        src: EcoString,
        errors: Vec1<crate::type_::Error>,
        names: Box<Names>,
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

    #[error("duplicate native Erlang module {module}")]
    DuplicateNativeErlangModule {
        module: Name,
        first: Utf8PathBuf,
        second: Utf8PathBuf,
    },

    #[error("gleam module {module} clashes with native file of same name")]
    ClashingGleamModuleAndNativeFileName {
        module: Name,
        gleam_file: Utf8PathBuf,
        native_file: Utf8PathBuf,
    },

    #[error("cyclical module imports")]
    ImportCycle {
        modules: Vec1<(EcoString, ImportCycleLocationDetails)>,
    },

    #[error("cyclical package dependencies")]
    PackageCycle { packages: Vec<EcoString> },

    #[error("{action:?} {path:?} failed: {err:?}")]
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
    ShellProgramNotFound { program: String, os: OS },

    #[error("shell program `{program}` failed")]
    ShellCommand {
        program: String,
        reason: ShellCommandFailureReason,
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
    ModuleDoesNotHaveMainFunction { module: EcoString, origin: Origin },

    #[error("{module} does not have a public main function")]
    MainFunctionIsPrivate { module: EcoString },

    #[error("{module}'s main function has the wrong arity so it can not be run")]
    MainFunctionHasWrongArity { module: EcoString, arity: usize },

    #[error("{module}'s main function does not support the current target")]
    MainFunctionDoesNotSupportTarget { module: EcoString, target: Target },

    #[error("{input} is not a valid version. {error}")]
    InvalidVersionFormat { input: String, error: String },

    #[error("incompatible locked version. {error}")]
    IncompatibleLockedVersion { error: String },

    #[error("project root already exists")]
    ProjectRootAlreadyExist { path: String },

    #[error("File(s) already exist in {}",
file_names.iter().map(|x| x.as_str()).join(", "))]
    OutputFilesAlreadyExist { file_names: Vec<Utf8PathBuf> },

    #[error("Packages not exist: {}", packages.iter().join(", "))]
    RemovedPackagesNotExist { packages: Vec<String> },

    #[error("Packages to update not exist: {}", packages.iter().join(", "))]
    PackagesToUpdateNotExist { packages: Vec<EcoString> },

    #[error("unable to find project root")]
    UnableToFindProjectRoot { path: String },

    #[error("gleam.toml version {toml_ver} does not match .app version {app_ver}")]
    VersionDoesNotMatch { toml_ver: String, app_ver: String },

    #[error("metadata decoding failed")]
    MetadataDecodeError { error: Option<String> },

    #[error("warnings are not permitted")]
    ForbiddenWarnings { count: usize },

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

    #[error("Failed to create canonical path for package {0}")]
    DependencyCanonicalizationFailed(String),

    #[error("Could not find versions that satisfy dependency requirements")]
    DependencyResolutionNoSolution {
        root_package_name: EcoString,
        derivation_tree:
            Box<NeverEqual<pubgrub::DerivationTree<String, pubgrub::Ranges<Version>, String>>>,
    },

    #[error("Dependency resolution failed: {0}")]
    DependencyResolutionError(String),

    #[error("The package {0} is listed in dependencies and dev_dependencies")]
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

    #[error(
        "The package {package} requires a Gleam version satisfying \
{required_version} and you are using v{gleam_version}"
    )]
    IncompatibleCompilerVersion {
        package: String,
        required_version: String,
        gleam_version: String,
    },

    #[error("The --javascript-prelude flag must be given when compiling to JavaScript")]
    JavaScriptPreludeRequired,

    #[error("The modules {unfinished:?} contain todo expressions and so cannot be published")]
    CannotPublishTodo { unfinished: Vec<EcoString> },

    #[error("The modules {unfinished:?} contain todo expressions and so cannot be published")]
    CannotPublishEcho { unfinished: Vec<EcoString> },

    #[error(
        "The modules {unfinished:?} contain internal types in their public API so cannot be published"
    )]
    CannotPublishLeakedInternalType { unfinished: Vec<EcoString> },

    #[error("The modules {unfinished:?} are empty and so cannot be published")]
    CannotPublishEmptyModules { unfinished: Vec<EcoString> },

    #[error("Publishing packages to reserve names is not permitted")]
    HexPackageSquatting,

    #[error("The package includes the default main function so cannot be published")]
    CannotPublishWithDefaultMain { package_name: EcoString },

    #[error("Corrupt manifest.toml")]
    CorruptManifest,

    #[error("The Gleam module {path} would overwrite the Erlang module {name}")]
    GleamModuleWouldOverwriteStandardErlangModule { name: EcoString, path: Utf8PathBuf },

    #[error("Version already published")]
    HexPublishReplaceRequired { version: String },

    #[error("Package is already exists ")]
    HexPackageAlreadyExists { name: String, version: String },

    #[error("The gleam version constraint is wrong and so cannot be published")]
    CannotPublishWrongVersion {
        minimum_required_version: SmallVersion,
        wrongfully_allowed_version: SmallVersion,
    },

    #[error("Failed to encrypt local Hex API key")]
    FailedToEncryptLocalHexApiKey { detail: String },

    #[error("Failed to decrypt local Hex API key")]
    FailedToDecryptLocalHexApiKey { detail: String },

    #[error("Cannot add a package with the same name as a dependency")]
    CannotAddSelfAsDependency { name: EcoString },
}

// A wrapper that ignores the inner value for equality:
#[derive(Debug, Clone)]
pub struct NeverEqual<T>(pub T);

impl<T> PartialEq for NeverEqual<T> {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}
impl<T> Eq for NeverEqual<T> {}

/// This is to make clippy happy and not make the error variant too big by
/// storing an entire `hexpm::version::Version` in the error.
///
/// This is enough to report wrong Gleam compiler versions.
///
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SmallVersion {
    major: u8,
    minor: u8,
    patch: u8,
}

impl Display for SmallVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}.{}.{}", self.major, self.minor, self.patch))
    }
}

impl SmallVersion {
    pub fn from_hexpm(version: Version) -> Self {
        Self {
            major: version.major as u8,
            minor: version.minor as u8,
            patch: version.patch as u8,
        }
    }
}
#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub enum OS {
    Linux(Distro),
    MacOS,
    Windows,
    Other,
}

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub enum Distro {
    Ubuntu,
    Debian,
    Other,
}

pub fn parse_os(os: &str, distro: &str) -> OS {
    match os {
        "macos" => OS::MacOS,
        "windows" => OS::Windows,
        "linux" => OS::Linux(parse_linux_distribution(distro)),
        _ => OS::Other,
    }
}

pub fn parse_linux_distribution(distro: &str) -> Distro {
    match distro {
        "ubuntu" => Distro::Ubuntu,
        "debian" => Distro::Debian,
        _ => Distro::Other,
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ShellCommandFailureReason {
    /// When we don't have any context about the failure
    Unknown,
    /// When the actual running of the command failed for some reason.
    IoError(std::io::ErrorKind),
    /// When the shell command returned an error status
    ShellCommandError(String),
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

    pub fn dependency_resolution_failed<T: PackageFetcher>(
        error: ResolutionError<'_, T>,
        root_package_name: EcoString,
    ) -> Error {
        match error {
            ResolutionError::NoSolution(derivation_tree) => Self::DependencyResolutionNoSolution {
                root_package_name,
                derivation_tree: Box::new(NeverEqual(derivation_tree)),
            },

            ResolutionError::ErrorRetrievingDependencies {
                package,
                version,
                source,
            } => Self::DependencyResolutionError(format!(
                "An error occurred while trying to retrieve dependencies of {package}@{version}: {source}",
            )),

            ResolutionError::ErrorChoosingVersion { package, source } => {
                Self::DependencyResolutionError(format!(
                    "An error occurred while choosing the version of {package}: {source}",
                ))
            }

            ResolutionError::ErrorInShouldCancel(err) => Self::DependencyResolutionError(format!(
                "Dependency resolution was cancelled. {err}"
            )),
        }
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

impl<T> From<Error> for Outcome<T, Error> {
    fn from(error: Error) -> Self {
        Outcome::TotalFailure(error)
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
    FormatNotLowercase,
    GleamPrefix,
    ErlangReservedWord,
    ErlangStandardLibraryModule,
    GleamReservedWord,
    GleamReservedModule,
}

pub fn format_invalid_project_name_error(
    name: &str,
    reason: &InvalidProjectNameReason,
    with_suggestion: &Option<String>,
) -> String {
    let reason_message = match reason {
        InvalidProjectNameReason::ErlangReservedWord => "is a reserved word in Erlang.",
        InvalidProjectNameReason::ErlangStandardLibraryModule => {
            "is a standard library module in Erlang."
        }
        InvalidProjectNameReason::GleamReservedWord => "is a reserved word in Gleam.",
        InvalidProjectNameReason::GleamReservedModule => "is a reserved module name in Gleam.",
        InvalidProjectNameReason::FormatNotLowercase => {
            "does not have the correct format. Project names \
may only contain lowercase letters."
        }
        InvalidProjectNameReason::Format => {
            "does not have the correct format. Project names \
must start with a lowercase letter and may only contain lowercase letters, \
numbers and underscores."
        }
        InvalidProjectNameReason::GleamPrefix => {
            "has the reserved prefix `gleam_`. \
This prefix is intended for official Gleam packages only."
        }
    };

    match with_suggestion {
        Some(suggested_name) => wrap_format!(
            "We were not able to create your project as `{}` {}

Would you like to name your project '{}' instead?",
            name,
            reason_message,
            suggested_name
        ),
        None => wrap_format!(
            "We were not able to create your project as `{}` {}

Please try again with a different project name.",
            name,
            reason_message
        ),
    }
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

// https://github.com/rust-lang/rust/blob/03994e498df79aa1f97f7bbcfd52d57c8e865049/compiler/rustc_span/src/edit_distance.rs
pub fn edit_distance(a: &str, b: &str, limit: usize) -> Option<usize> {
    let mut a = &a.chars().collect::<Vec<_>>()[..];
    let mut b = &b.chars().collect::<Vec<_>>()[..];

    if a.len() < b.len() {
        std::mem::swap(&mut a, &mut b);
    }

    let min_dist = a.len() - b.len();
    // If we know the limit will be exceeded, we can return early.
    if min_dist > limit {
        return None;
    }

    // Strip common prefix.
    while !b.is_empty() && !a.is_empty() {
        let (b_first, b_rest) = b.split_last().expect("Failed to split 'b' slice");
        let (a_first, a_rest) = a.split_last().expect("Failed to split 'a' slice");

        if b_first == a_first {
            a = a_rest;
            b = b_rest;
        } else {
            break;
        }
    }

    // If either string is empty, the distance is the length of the other.
    // We know that `b` is the shorter string, so we don't need to check `a`.
    if b.is_empty() {
        return Some(min_dist);
    }

    let mut prev_prev = vec![usize::MAX; b.len() + 1];
    let mut prev = (0..=b.len()).collect::<Vec<_>>();
    let mut current = vec![0; b.len() + 1];

    // row by row
    for i in 1..=a.len() {
        if let Some(element) = current.get_mut(0) {
            *element = i;
        }
        let a_idx = i - 1;

        // column by column
        for j in 1..=b.len() {
            let b_idx = j - 1;

            // There is no cost to substitute a character with itself.
            let substitution_cost = match (a.get(a_idx), b.get(b_idx)) {
                (Some(&a_char), Some(&b_char)) => {
                    if a_char == b_char {
                        0
                    } else {
                        1
                    }
                }
                _ => panic!("Index out of bounds"),
            };

            let insertion = current.get(j - 1).map_or(usize::MAX, |&x| x + 1);

            if let Some(value) = current.get_mut(j) {
                *value = std::cmp::min(
                    // deletion
                    prev.get(j).map_or(usize::MAX, |&x| x + 1),
                    std::cmp::min(
                        // insertion
                        insertion,
                        // substitution
                        prev.get(j - 1)
                            .map_or(usize::MAX, |&x| x + substitution_cost),
                    ),
                );
            }

            if (i > 1)
                && (j > 1)
                && let (Some(&a_val), Some(&b_val_prev), Some(&a_val_prev), Some(&b_val)) = (
                    a.get(a_idx),
                    b.get(b_idx - 1),
                    a.get(a_idx - 1),
                    b.get(b_idx),
                )
                && (a_val == b_val_prev)
                && (a_val_prev == b_val)
            {
                // transposition
                if let Some(curr) = current.get_mut(j)
                    && let Some(&prev_prev_val) = prev_prev.get(j - 2)
                {
                    *curr = std::cmp::min(*curr, prev_prev_val + 1);
                }
            }
        }

        // Rotate the buffers, reusing the memory.
        [prev_prev, prev, current] = [prev, current, prev_prev];
    }

    // `prev` because we already rotated the buffers.
    let distance = match prev.get(b.len()) {
        Some(&d) => d,
        None => usize::MAX,
    };
    (distance <= limit).then_some(distance)
}

fn edit_distance_with_substrings(a: &str, b: &str, limit: usize) -> Option<usize> {
    let n = a.chars().count();
    let m = b.chars().count();

    // Check one isn't less than half the length of the other. If this is true then there is a
    // big difference in length.
    let big_len_diff = (n * 2) < m || (m * 2) < n;
    let len_diff = m.abs_diff(n);
    let distance = edit_distance(a, b, limit + len_diff)?;

    // This is the crux, subtracting length difference means exact substring matches will now be 0
    let score = distance - len_diff;

    // If the score is 0 but the words have different lengths then it's a substring match not a full
    // word match
    let score = if score == 0 && len_diff > 0 && !big_len_diff {
        1 // Exact substring match, but not a total word match so return non-zero
    } else if !big_len_diff {
        // Not a big difference in length, discount cost of length difference
        score + len_diff.div_ceil(2)
    } else {
        // A big difference in length, add back the difference in length to the score
        score + len_diff
    };

    (score <= limit).then_some(score)
}

fn did_you_mean(name: &str, options: &[EcoString]) -> Option<String> {
    // If only one option is given, return that option.
    // This seems to solve the `unknown_variable_3` test.
    if options.len() == 1 {
        return options
            .first()
            .map(|option| format!("Did you mean `{option}`?"));
    }

    // Check for case-insensitive matches.
    // This solves the comparison to small and single character terms,
    // such as the test on `type_vars_must_be_declared`.
    if let Some(exact_match) = options
        .iter()
        .find(|&option| option.eq_ignore_ascii_case(name))
    {
        return Some(format!("Did you mean `{exact_match}`?"));
    }

    // Calculate the threshold as one third of the name's length, with a minimum of 1.
    let threshold = std::cmp::max(name.chars().count() / 3, 1);

    // Filter and sort options based on edit distance.
    options
        .iter()
        .filter(|&option| option != crate::ast::CAPTURE_VARIABLE)
        .sorted()
        .filter_map(|option| {
            edit_distance_with_substrings(option, name, threshold)
                .map(|distance| (option, distance))
        })
        .min_by_key(|&(_, distance)| distance)
        .map(|(option, _)| format!("Did you mean `{option}`?"))
}

fn to_ordinal(value: u32) -> String {
    match value % 10 {
        // All numbers starting with 1 end in `th` (11th, 12th, 13th, etc.)
        _ if value / 10 == 1 => format!("{value}th"),
        1 => format!("{value}st"),
        2 => format!("{value}nd"),
        3 => format!("{value}rd"),
        _ => format!("{value}th"),
    }
}

impl Error {
    pub fn pretty_string(&self) -> String {
        let mut nocolor = Buffer::no_color();
        self.pretty(&mut nocolor);
        String::from_utf8(nocolor.into_inner()).expect("Error printing produced invalid utf8")
    }

    pub fn pretty(&self, buffer: &mut Buffer) {
        for diagnostic in self.to_diagnostics() {
            diagnostic.write(buffer);
            writeln!(buffer).expect("write new line after diagnostic");
        }
    }

    pub fn to_diagnostics(&self) -> Vec<Diagnostic> {
        use crate::type_::Error as TypeError;
        match self {
            Error::HexPackageSquatting => {
                let text =
                    "You appear to be attempting to reserve a name on Hex rather than publishing a
working package. This is against the Hex terms of service and can result in
package deletion or account suspension.
"
                    .into();

                vec![Diagnostic {
                    title: "Invalid Hex package".into(),
                    text,
                    level: Level::Error,
                    location: None,
                    hint: None,
                }]
            }

            Error::CannotPublishWithDefaultMain { package_name } => {
                let text = wrap_format!(
                    "Packages with the default main function cannot be published

Remove or modify the main function that contains only:
    `io.println(\"Hello from {package_name}!\")`"
                );

                vec![Diagnostic {
                    title: "Cannot publish with default main function".into(),
                    text,
                    level: Level::Error,
                    location: None,
                    hint: None,
                }]
            }

            Error::MetadataDecodeError { error } => {
                let mut text = "A problem was encountered when decoding the metadata for one \
of the Gleam dependency modules."
                    .to_string();
                if let Some(error) = error {
                    text.push_str("\nThe error from the decoder library was:\n\n");
                    text.push_str(error);
                }

                vec![Diagnostic {
                    title: "Failed to decode module metadata".into(),
                    text,
                    level: Level::Error,
                    location: None,
                    hint: None,
                }]
            }

            Error::InvalidProjectName { name, reason } => {
                let text = format_invalid_project_name_error(name, reason, &None);

                vec![Diagnostic {
                    title: "Invalid project name".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::InvalidModuleName { module } => vec![Diagnostic {
                title: "Invalid module name".into(),
                text: format!(
                    "`{module}` is not a valid module name.
Module names can only contain lowercase letters, underscore, and
forward slash and must not end with a slash."
                ),
                level: Level::Error,
                location: None,
                hint: None,
            }],

            Error::ModuleDoesNotExist { module, suggestion } => {
                let hint = match suggestion {
                    Some(suggestion) => format!("Did you mean `{suggestion}`?"),
                    None => format!("Try creating the file `src/{module}.gleam`."),
                };
                vec![Diagnostic {
                    title: "Module does not exist".into(),
                    text: format!("Module `{module}` was not found."),
                    level: Level::Error,
                    location: None,
                    hint: Some(hint),
                }]
            }

            Error::ModuleDoesNotHaveMainFunction { module, origin } => vec![Diagnostic {
                title: "Module does not have a main function".into(),
                text: format!(
                    "`{module}` does not have a main function so the module can not be run."
                ),
                level: Level::Error,
                location: None,
                hint: Some(format!(
                    "Add a public `main` function to `{}/{module}.gleam`.",
                    origin.folder_name()
                )),
            }],

            Error::MainFunctionIsPrivate { module } => vec![Diagnostic {
                title: "Module does not have a public main function".into(),
                text: wrap_format!(
                    "`{module}` has a main function, but it is private, so it cannot be run."
                ),
                level: Level::Error,
                location: None,
                hint: Some(wrap_format!(
                    "Make the `main` function in the `{module}` module public."
                )),
            }],

            Error::MainFunctionDoesNotSupportTarget { module, target } => vec![Diagnostic {
                title: "Target not supported".into(),
                text: wrap_format!(
                    "`{module}` has a main function, but it does not support the {target} \
target, so it cannot be run."
                ),
                level: Level::Error,
                location: None,
                hint: None,
            }],

            Error::MainFunctionHasWrongArity { module, arity } => vec![Diagnostic {
                title: "Main function has wrong arity".into(),
                text: wrap_format!(
                    "`{module}:main` should have an arity of 0 to be run but its arity is {arity}."
                ),
                level: Level::Error,
                location: None,
                hint: Some("Change the function signature of main to `pub fn main() {}`.".into()),
            }],

            Error::ProjectRootAlreadyExist { path } => vec![Diagnostic {
                title: "Project folder already exists".into(),
                text: format!("Project folder root:\n\n  {path}"),
                level: Level::Error,
                hint: None,
                location: None,
            }],

            Error::OutputFilesAlreadyExist { file_names } => vec![Diagnostic {
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
            }],

            Error::RemovedPackagesNotExist { packages } => vec![Diagnostic {
                title: "Package not found".into(),
                text: format!(
                    "These packages are not dependencies of your package so they could not
be removed.

{}
",
                    packages
                        .iter()
                        .map(|p| format!("  - {}", p.as_str()))
                        .join("\n")
                ),
                level: Level::Error,
                hint: None,
                location: None,
            }],

            Error::PackagesToUpdateNotExist { packages } => vec![Diagnostic {
                title: "Packages to update not found".into(),
                text: format!(
                    "These packages are not dependencies of your package so they could not
be updated.

{}
",
                    packages
                        .iter()
                        .map(|p| format!("  - {}", p.as_str()))
                        .join("\n")
                ),
                level: Level::Error,
                hint: None,
                location: None,
            }],

            Error::CannotPublishTodo { unfinished } => vec![Diagnostic {
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
            }],

            Error::CannotPublishEcho { unfinished } => vec![Diagnostic {
                title: "Cannot publish unfinished code".into(),
                text: format!(
                    "These modules contain echo expressions and cannot be published:

{}

`echo` is only meant for debug printing, please remove them and try again.
",
                    unfinished
                        .iter()
                        .map(|name| format!("  - {}", name.as_str()))
                        .join("\n")
                ),
                level: Level::Error,
                hint: None,
                location: None,
            }],

            Error::CannotPublishWrongVersion {
                minimum_required_version,
                wrongfully_allowed_version,
            } => vec![Diagnostic {
                title: "Cannot publish package with wrong Gleam version range".into(),
                text: wrap(&format!(
                    "Your package uses features that require at least v{minimum_required_version}.
But the Gleam version range specified in your `gleam.toml` would allow this \
code to run on an earlier version like v{wrongfully_allowed_version}, \
resulting in compilation errors!"
                )),
                level: Level::Error,
                hint: Some(format!(
                    "Remove the version constraint from your `gleam.toml` or update it to be:

    gleam = \">= {minimum_required_version}\""
                )),
                location: None,
            }],

            Error::CannotPublishLeakedInternalType { unfinished } => vec![Diagnostic {
                title: "Cannot publish unfinished code".into(),
                text: format!(
                    "These modules leak internal types in their public API and cannot be published:

{}

Please make sure internal types do not appear in public functions and try again.
",
                    unfinished
                        .iter()
                        .map(|name| format!("  - {}", name.as_str()))
                        .join("\n")
                ),
                level: Level::Error,
                hint: None,
                location: None,
            }],

            Error::CannotPublishEmptyModules { unfinished } => vec![Diagnostic {
                title: "Cannot publish empty modules".into(),
                text: wrap_format!(
                    "These modules contain no public definitions and cannot be published:

{}

Please add public functions, types, or constants to these modules, or remove them and try again.",
                    unfinished
                        .iter()
                        .map(|name| format!("  - {}", name.as_str()))
                        .join("\n")
                ),
                level: Level::Error,
                hint: None,
                location: None,
            }],

            Error::UnableToFindProjectRoot { path } => {
                let text = wrap_format!(
                    "We were unable to find gleam.toml.

We searched in {path} and all parent directories."
                );
                vec![Diagnostic {
                    title: "Project not found".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::VersionDoesNotMatch { toml_ver, app_ver } => {
                let text = format!(
                    "The version in gleam.toml \"{toml_ver}\" does not match the version in
your app.src file \"{app_ver}\"."
                );
                vec![Diagnostic {
                    title: "Version does not match".into(),
                    hint: None,
                    text,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::ShellProgramNotFound { program, os } => {
                let mut text = format!("The program `{program}` was not found. Is it installed?");

                match os {
                    OS::MacOS => {
                        fn brew_install(name: &str, pkg: &str) -> String {
                            format!("\n\nYou can install {name} via homebrew: brew install {pkg}",)
                        }
                        match program.as_str() {
                            "erl" | "erlc" | "escript" => {
                                text.push_str(&brew_install("Erlang", "erlang"))
                            }
                            "rebar3" => text.push_str(&brew_install("Rebar3", "rebar3")),
                            "deno" => text.push_str(&brew_install("Deno", "deno")),
                            "elixir" => text.push_str(&brew_install("Elixir", "elixir")),
                            "node" => text.push_str(&brew_install("Node.js", "node")),
                            "bun" => text.push_str(&brew_install("Bun", "oven-sh/bun/bun")),
                            "git" => text.push_str(&brew_install("Git", "git")),
                            _ => (),
                        }
                    }
                    OS::Linux(distro) => {
                        fn apt_install(name: &str, pkg: &str) -> String {
                            format!("\n\nYou can install {name} via apt: sudo apt install {pkg}")
                        }
                        match distro {
                            Distro::Ubuntu | Distro::Debian => match program.as_str() {
                                "elixir" => text.push_str(&apt_install("Elixir", "elixir")),
                                "git" => text.push_str(&apt_install("Git", "git")),
                                _ => (),
                            },
                            Distro::Other => (),
                        }
                    }
                    OS::Windows | OS::Other => (),
                }

                text.push('\n');

                match program.as_str() {
                    "erl" | "erlc" | "escript" => text.push_str(
                        "
Documentation for installing Erlang can be viewed here:
https://gleam.run/getting-started/installing/",
                    ),
                    "rebar3" => text.push_str(
                        "
Documentation for installing Rebar3 can be viewed here:
https://rebar3.org/docs/getting-started/",
                    ),
                    "deno" => text.push_str(
                        "
Documentation for installing Deno can be viewed here:
https://docs.deno.com/runtime/getting_started/installation/",
                    ),
                    "elixir" => text.push_str(
                        "
Documentation for installing Elixir can be viewed here:
https://elixir-lang.org/install.html",
                    ),
                    "node" => text.push_str(
                        "
Documentation for installing Node.js via package manager can be viewed here:
https://nodejs.org/en/download/package-manager/all/",
                    ),
                    "bun" => text.push_str(
                        "
Documentation for installing Bun can be viewed here:
https://bun.sh/docs/installation/",
                    ),
                    "git" => text.push_str(
                        "
Documentation for installing Git can be viewed here:
https://git-scm.com/book/en/v2/Getting-Started-Installing-Git",
                    ),
                    _ => (),
                }

                vec![Diagnostic {
                    title: "Program not found".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::ShellCommand {
                program: command,
                reason: ShellCommandFailureReason::Unknown,
            } => {
                let text =
                    format!("There was a problem when running the shell command `{command}`.");
                vec![Diagnostic {
                    title: "Shell command failure".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::ShellCommand {
                program: command,
                reason: ShellCommandFailureReason::IoError(err),
            } => {
                let text = format!(
                    "There was a problem when running the shell command `{}`.

The error from the shell command library was:

    {}",
                    command,
                    std_io_error_kind_text(err)
                );
                vec![Diagnostic {
                    title: "Shell command failure".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::ShellCommand {
                program: command,
                reason: ShellCommandFailureReason::ShellCommandError(err),
            } => {
                let text = format!(
                    "There was a problem when running the shell command `{command}`.

The error from the shell command was:

    {err}"
                );
                vec![Diagnostic {
                    title: "Shell command failure".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::Gzip(detail) => {
                let text = format!(
                    "There was a problem when applying gzip compression.

This was error from the gzip library:

    {detail}"
                );
                vec![Diagnostic {
                    title: "Gzip compression failure".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::AddTar { path, err } => {
                let text = format!(
                    "There was a problem when attempting to add the file {path}
to a tar archive.

This was error from the tar library:

    {err}"
                );
                vec![Diagnostic {
                    title: "Failure creating tar archive".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::ExpandTar { error } => {
                let text = format!(
                    "There was a problem when attempting to expand a to a tar archive.

This was error from the tar library:

    {error}"
                );
                vec![Diagnostic {
                    title: "Failure opening tar archive".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::TarFinish(detail) => {
                let text = format!(
                    "There was a problem when creating a tar archive.

This was error from the tar library:

    {detail}"
                );
                vec![Diagnostic {
                    title: "Failure creating tar archive".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::Hex(detail) => {
                let text = format!(
                    "There was a problem when using the Hex API.

This was error from the Hex client library:

    {detail}"
                );
                vec![Diagnostic {
                    title: "Hex API failure".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
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

                vec![Diagnostic {
                    title: "Duplicate module".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::ClashingGleamModuleAndNativeFileName {
                module,
                gleam_file,
                native_file,
            } => {
                let text = format!(
                    "The Gleam module `{module}` is clashing with a native file
with the same name:

    Gleam module: {gleam_file}
    Native file:  {native_file}

This is a problem because the Gleam module would be compiled to a file with the
same name and extension, unintentionally overwriting the native file."
                );

                vec![Diagnostic {
                    title: "Gleam module clashes with native file".into(),
                    text,
                    hint: Some(
                        "Consider renaming one of the files, such as by \
adding an `_ffi` suffix to the native file's name, and trying again."
                            .into(),
                    ),
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::DuplicateSourceFile { file } => vec![Diagnostic {
                title: "Duplicate Source file".into(),
                text: format!("The file `{file}` is defined multiple times."),
                hint: None,
                level: Level::Error,
                location: None,
            }],

            Error::DuplicateNativeErlangModule {
                module,
                first,
                second,
            } => {
                let text = format!(
                    "The native Erlang module `{module}` is defined multiple times.

First:  {first}
Second: {second}

Erlang modules must have unique names regardless of the subfolders where their
`.erl` files are located."
                );

                vec![Diagnostic {
                    title: "Duplicate native Erlang module".into(),
                    text,
                    hint: Some("Rename one of the native Erlang modules and try again.".into()),
                    level: Level::Error,
                    location: None,
                }]
            }

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
                let mut text = format!(
                    "An error occurred while trying to {} this {}:

    {}
{}",
                    action.text(),
                    kind.text(),
                    path,
                    err,
                );
                if cfg!(target_family = "windows") && action == &FileIoAction::Link {
                    text.push_str("

Windows does not support symbolic links without developer mode
or admin privileges. Please enable developer mode and try again.

https://learn.microsoft.com/en-us/windows/apps/get-started/enable-your-device-for-development#activate-developer-mode");
                }
                vec![Diagnostic {
                    title: "File IO failure".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::FailedToEncryptLocalHexApiKey { detail } => {
                let text = wrap_format!(
                    "A problem was encountered \
encrypting the local Hex API key with the given password.
The error from the encryption library was:

    {detail}"
                );
                vec![Diagnostic {
                    title: "Failed to encrypt data".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::FailedToDecryptLocalHexApiKey { detail } => {
                let text = wrap_format!(
                    "Unable to decrypt the local Hex API key with the given password.
The error from the encryption library was:

    {detail}"
                );
                vec![Diagnostic {
                    title: "Failed to decrypt local Hex API key".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::NonUtf8Path { path } => {
                let text = format!(
                    "Encountered a non UTF-8 path '{}', but only UTF-8 paths are supported.",
                    path.to_string_lossy()
                );
                vec![Diagnostic {
                    title: "Non UTF-8 Path Encountered".into(),
                    text,
                    level: Level::Error,
                    location: None,
                    hint: None,
                }]
            }

            Error::GitInitialization { error } => {
                let text = format!(
                    "An error occurred while trying make a git repository for this project:

    {error}"
                );
                vec![Diagnostic {
                    title: "Failed to initialize git repository".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::Type {
                path,
                src,
                errors: error,
                names,
            } => error
                .iter()
                .map(|error| match error {
                    TypeError::LiteralFloatOutOfRange { location, .. } => Diagnostic {
                        title: "Float outside of valid range".into(),
                        text: wrap(
                            "This float value is too large to be represented by \
a floating point type: float values must be in the range -1.7976931348623157e308 \
- 1.7976931348623157e308.",
                        ),
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

                    TypeError::InvalidImport {
                        location,
                        importing_module,
                        imported_module,
                        kind: InvalidImportKind::SrcImportingTest,
                    } => {
                        let text = wrap_format!(
                            "The application module `{importing_module}` \
is importing the test module `{imported_module}`.

Test modules are not included in production builds so application \
modules cannot import them. Perhaps move the `{imported_module}` \
module to the src directory.",
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

                    TypeError::InvalidImport {
                        location,
                        importing_module,
                        imported_module,
                        kind: InvalidImportKind::SrcImportingDev,
                    } => {
                        let text = wrap_format!(
                            "The application module `{importing_module}` \
is importing the development module `{imported_module}`.

Development modules are not included in production builds so application \
modules cannot import them. Perhaps move the `{imported_module}` \
module to the src directory.",
                        );

                        Diagnostic {
                            title: "App importing dev module".into(),
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

                    TypeError::InvalidImport {
                        location,
                        importing_module,
                        imported_module,
                        kind: InvalidImportKind::DevImportingTest,
                    } => {
                        let text = wrap_format!(
                            "The development module `{importing_module}` \
is importing the test module `{imported_module}`.

Test modules should only contain test-related code, and not general development \
code. Perhaps move the `{imported_module}` module to the dev directory.",
                        );

                        Diagnostic {
                            title: "Dev importing test module".into(),
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
                        let extra_labels = labels
                            .map(|label| ExtraLabel {
                                src_info: None,
                                label,
                            })
                            .collect();
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
                        let text = wrap(
                            "This unlabeled argument has been \
supplied after a labelled argument.
Once a labelled argument has been supplied all following arguments must
also be labelled.",
                        );

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
                                extra_labels: vec![ExtraLabel {
                                    src_info: None,
                                    label: Label {
                                        text: Some("First imported here".into()),
                                        span: *previous_location,
                                    },
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
                        let (first_location, second_location) =
                            if location_a.start < location_b.start {
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
                                extra_labels: vec![ExtraLabel {
                                    src_info: None,
                                    label: Label {
                                        text: Some("First defined here".into()),
                                        span: *first_location,
                                    },
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
                                extra_labels: vec![ExtraLabel {
                                    src_info: None,
                                    label: Label {
                                        text: Some("First defined here".into()),
                                        span: *previous_location,
                                    },
                                }],
                            }),
                        }
                    }

                    TypeError::DuplicateField { location, label } => {
                        let text = format!(
                            "The label `{label}` has already been defined. Rename this label."
                        );
                        Diagnostic {
                            title: "Duplicate label".into(),
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
                        let text = wrap(
                            "I don't know how to work out what type this \
value has. It seems to be defined in terms of itself.",
                        );
                        Diagnostic {
                            title: "Recursive type".into(),
                            text,
                            hint: Some("Add some type annotations and try again.".into()),
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

                    TypeError::NotFn { location, type_ } => {
                        let mut printer = Printer::new(names);
                        let text = format!(
                            "This value is being called as a function but its type is:\n\n    {}",
                            printer.print_type(type_)
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
                        type_,
                        label,
                        fields,
                        unknown_field: variants,
                    } => {
                        let mut printer = Printer::new(names);

                        // Give a hint about what type this value has.
                        let mut text = format!(
                            "The value being accessed has this type:\n\n    {}\n",
                            printer.print_type(type_)
                        );

                        // Give a hint about what record fields this value has, if any.
                        if fields.is_empty() {
                            if variants == &UnknownField::NoFields {
                                text.push_str("\nIt does not have any fields.");
                            } else {
                                text.push_str(
                                    "\nIt does not have fields that are common \
across all variants.",
                                );
                            }
                        } else {
                            text.push_str("\nIt has these accessible fields:\n");
                        }
                        for field in fields.iter().sorted() {
                            text.push_str("\n    .");
                            text.push_str(field);
                        }

                        match variants {
                            UnknownField::AppearsInAVariant => {
                                let msg = wrap(
                                    "Note: The field you are trying to access is \
not defined consistently across all variants of this custom type. To fix this, \
ensure that all variants include the field with the same name, position, and \
type.",
                                );
                                text.push_str("\n\n");
                                text.push_str(&msg);
                            }
                            UnknownField::AppearsInAnImpossibleVariant => {
                                let msg = wrap(
                                    "Note: The field exists in this custom type \
but is not defined for the current variant. Ensure that you are accessing the \
field on a variant where it is valid.",
                                );
                                text.push_str("\n\n");
                                text.push_str(&msg);
                            }
                            UnknownField::TrulyUnknown => (),
                            UnknownField::NoFields => (),
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
                            FieldAccessUsage::Other | FieldAccessUsage::RecordUpdate => (),
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
                    } => {
                        let mut printer = Printer::new(names);
                        let mut text = format!(
                            "The {op} operator expects arguments of this type:

    {expected}

But this argument has this type:

    {given}\n",
                            op = op.name(),
                            expected = printer.print_type(expected),
                            given = printer.print_type(given),
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
                    } => {
                        // Remap the pipe function type into just the type expected by the pipe.
                        let expected = expected
                            .fn_types()
                            .and_then(|(arguments, _)| arguments.first().cloned());

                        // Remap the argument as well, if it's a function.
                        let given = given
                            .fn_types()
                            .and_then(|(arguments, _)| arguments.first().cloned())
                            .unwrap_or_else(|| given.clone());

                        let mut printer = Printer::new(names);
                        let text = format!(
                            "The argument is:

    {given}

But function expects:

    {expected}",
                            expected = expected
                                .map(|v| printer.print_type(&v))
                                .unwrap_or_else(|| "    No arguments".into()),
                            given = printer.print_type(&given)
                        );

                        Diagnostic {
                            title: "Type mismatch".into(),
                            text,
                            hint: None,
                            level: Level::Error,
                            location: Some(Location {
                                label: Label {
                                    text: Some(
                                        "This function does not accept the piped type".into(),
                                    ),
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
                    } => {
                        let mut printer = Printer::new(names);
                        let mut text = if let Some(description) =
                            situation.as_ref().and_then(|s| s.description())
                        {
                            let mut text = description.to_string();
                            text.push('\n');
                            text.push('\n');
                            text
                        } else {
                            "".into()
                        };
                        text.push_str("Expected type:\n\n    ");
                        text.push_str(&printer.print_type(expected));
                        text.push_str("\n\nFound type:\n\n    ");
                        text.push_str(&printer.print_type(given));

                        let (main_message_location, main_message_text, extra_labels) =
                            match situation {
                                // When the mismatch error comes from a case clause we want to highlight the
                                // entire branch (pattern included) when reporting the error; in addition,
                                // if the error could be resolved just by wrapping the value in an `Ok`
                                // or `Error` we want to add an additional label with this hint below the
                                // offending value.
                                Some(UnifyErrorSituation::CaseClauseMismatch {
                                    clause_location,
                                }) => (clause_location, None, vec![]),
                                // In all other cases we just highlight the offending expression, optionally
                                // adding the wrapping hint if it makes sense.
                                Some(_) | None => {
                                    (location, hint_wrap_value_in_result(expected, given), vec![])
                                }
                            };

                        Diagnostic {
                            title: "Type mismatch".into(),
                            text,
                            hint: None,
                            level: Level::Error,
                            location: Some(Location {
                                label: Label {
                                    text: main_message_text,
                                    span: *main_message_location,
                                },
                                path: path.clone(),
                                src: src.clone(),
                                extra_labels,
                            }),
                        }
                    }

                    TypeError::IncorrectTypeArity {
                        location,
                        expected,
                        given: given_number,
                        name,
                    } => {
                        let expected = match expected {
                            0 => "no type arguments".into(),
                            1 => "1 type argument".into(),
                            _ => format!("{expected} type arguments"),
                        };
                        let given = match given_number {
                            0 => "none",
                            _ => &format!("{given_number}"),
                        };
                        let text = wrap_format!(
                            "`{name}` requires {expected} \
but {given} where provided."
                        );
                        Diagnostic {
                            title: "Incorrect arity".into(),
                            text,
                            hint: None,
                            level: Level::Error,
                            location: Some(Location {
                                label: Label {
                                    text: Some(format!("Expected {expected}, got {given_number}")),
                                    span: *location,
                                },
                                path: path.clone(),
                                src: src.clone(),
                                extra_labels: vec![],
                            }),
                        }
                    }

                    TypeError::TypeUsedAsAConstructor { location, name } => {
                        let text = wrap_format!(
                            "`{name}` is a type with no parameters, but here it's \
being used as a type constructor."
                        );

                        Diagnostic {
                            title: "Type used as a type constructor".into(),
                            text,
                            hint: None,
                            level: Level::Error,
                            location: Some(Location {
                                label: Label {
                                    text: Some("You can remove this".into()),
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
                        context,
                        expected,
                        given,
                    } => {
                        let text = if labels.is_empty() {
                            "".into()
                        } else {
                            let subject = match context {
                                IncorrectArityContext::Pattern => "pattern",
                                IncorrectArityContext::Function => "call",
                            };
                            let labels = labels
                                .iter()
                                .map(|p| format!("  - {p}"))
                                .sorted()
                                .join("\n");
                            format!(
                                "This {subject} accepts these additional labelled \
                                arguments:\n\n{labels}",
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

                    TypeError::UnsafeRecordUpdate { location, reason } => match reason {
                        UnsafeRecordUpdateReason::UnknownVariant {
                            constructed_variant,
                        } => {
                            let text = wrap_format!(
                                "This value cannot be used to build an updated \
`{constructed_variant}` as it could be some other variant.

Consider pattern matching on it with a case expression and then \
constructing a new record with its values."
                            );

                            Diagnostic {
                                title: "Unsafe record update".into(),
                                text,
                                hint: None,
                                level: Level::Error,
                                location: Some(Location {
                                    label: Label {
                                        text: Some(format!(
                                            "I'm not sure this is always a `{constructed_variant}`"
                                        )),
                                        span: *location,
                                    },
                                    path: path.clone(),
                                    src: src.clone(),
                                    extra_labels: vec![],
                                }),
                            }
                        }
                        UnsafeRecordUpdateReason::WrongVariant {
                            constructed_variant,
                            spread_variant,
                        } => {
                            let text = wrap_format!(
                                "This value is a `{spread_variant}` so \
it cannot be used to build a `{constructed_variant}`, even if they share some fields.

Note: If you want to change one variant of a type into another, you should \
specify all fields explicitly instead of using the record update syntax."
                            );

                            Diagnostic {
                                title: "Incorrect record update".into(),
                                text,
                                hint: None,
                                level: Level::Error,
                                location: Some(Location {
                                    label: Label {
                                        text: Some(format!("This is a `{spread_variant}`")),
                                        span: *location,
                                    },
                                    path: path.clone(),
                                    src: src.clone(),
                                    extra_labels: vec![],
                                }),
                            }
                        }
                        UnsafeRecordUpdateReason::IncompatibleFieldTypes {
                            expected_field_type,
                            record_field_type,
                            record_variant,
                            field,
                            ..
                        } => {
                            let mut printer = Printer::new(names);
                            let expected_field_type = printer.print_type(expected_field_type);
                            let record_field_type = printer.print_type(record_field_type);
                            let record_variant = printer.print_type(record_variant);
                            let text = match field {
                                RecordField::Labelled(label) => wrap_format!(
                                    "The `{label}` field \
of this value is a `{record_field_type}`, but the arguments given to the record \
update indicate that it should be a `{expected_field_type}`.

Note: If the same type variable is used for multiple fields, all those fields \
need to be updated at the same time if their type changes."
                                ),
                                RecordField::Unlabelled(index) => wrap_format!(
                                    "The {} field \
of this value is a `{record_field_type}`, but the arguments given to the record \
update indicate that it should be a `{expected_field_type}`.

Note: Unlabelled fields cannot be updated in a record update, so either add \
a label or use a record constructor.",
                                    to_ordinal(*index + 1),
                                ),
                            };

                            Diagnostic {
                                title: "Incomplete record update".into(),
                                text,
                                hint: None,
                                level: Level::Error,
                                location: Some(Location {
                                    label: Label {
                                        text: Some(format!("This is a `{record_variant}`")),
                                        span: *location,
                                    },
                                    path: path.clone(),
                                    src: src.clone(),
                                    extra_labels: vec![],
                                }),
                            }
                        }
                    },

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
                                    "There is a value in scope with the name `{name}`, \
but no type in scope with that name."
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
                        discarded_location,
                        name,
                        type_with_name_in_scope,
                    } => {
                        let title = String::from("Unknown variable");

                        if let Some(ignored_location) = discarded_location {
                            let location = Location {
                                label: Label {
                                    text: Some("So this is not in scope".into()),
                                    span: *location,
                                },
                                path: path.clone(),
                                src: src.clone(),
                                extra_labels: vec![ExtraLabel {
                                    src_info: None,
                                    label: Label {
                                        text: Some("This value is discarded".into()),
                                        span: *ignored_location,
                                    },
                                }],
                            };
                            Diagnostic {
                                title,
                                text: "".into(),
                                hint: Some(wrap_format!(
                                    "Change `_{name}` to `{name}` or reference another variable",
                                )),
                                level: Level::Error,
                                location: Some(location),
                            }
                        } else {
                            let text = if *type_with_name_in_scope {
                                wrap_format!("`{name}` is a type, it cannot be used as a value.")
                            } else {
                                let is_first_char_uppercase =
                                    name.chars().next().is_some_and(char::is_uppercase);

                                if is_first_char_uppercase {
                                    wrap_format!(
                                        "The custom type variant constructor \
`{name}` is not in scope here."
                                    )
                                } else {
                                    wrap_format!("The name `{name}` is not in scope here.")
                                }
                            };

                            Diagnostic {
                                title,
                                text,
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
                            }
                        }
                    }

                    TypeError::PrivateTypeLeak { location, leaked } => {
                        let mut printer = Printer::new(names);

                        // TODO: be more precise.
                        // - is being returned by this public function
                        // - is taken as an argument by this public function
                        // - is taken as an argument by this public enum constructor
                        // etc
                        let text = wrap_format!(
                            "The following type is private, but is \
being used by this public export.

    {}

Private types can only be used within the module that defines them.",
                            printer.print_type(leaked),
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
                        suggestions,
                    } => Diagnostic {
                        title: "Unknown module".into(),
                        text: format!("No module has been found with the name `{name}`."),
                        hint: suggestions
                            .first()
                            .map(|suggestion| suggestion.suggestion(name)),
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

                    TypeError::UnknownModuleType {
                        location,
                        name,
                        module_name,
                        type_constructors,
                        value_with_same_name: imported_type_as_value,
                    } => {
                        let text = if *imported_type_as_value {
                            format!("`{name}` is only a value, it cannot be imported as a type.")
                        } else {
                            format!("The module `{module_name}` does not have a `{name}` type.")
                        };
                        Diagnostic {
                            title: "Unknown module type".into(),
                            text,
                            hint: None,
                            level: Level::Error,
                            location: Some(Location {
                                label: Label {
                                    text: if *imported_type_as_value {
                                        Some(format!("Did you mean `{name}`?"))
                                    } else {
                                        did_you_mean(name, type_constructors)
                                    },
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
                        type_with_same_name: imported_value_as_type,
                        context,
                    } => {
                        let text = if *imported_value_as_type {
                            match context {
                                ModuleValueUsageContext::UnqualifiedImport => wrap_format!(
                                    "`{name}` is only a type, it cannot be imported as a value."
                                ),
                                ModuleValueUsageContext::ModuleAccess => wrap_format!(
                                    "{module_name}.{name} is a type constructor, \
it cannot be used as a value"
                                ),
                            }
                        } else {
                            wrap_format!(
                                "The module `{module_name}` does not have a `{name}` value."
                            )
                        };
                        Diagnostic {
                            title: "Unknown module value".into(),
                            text,
                            hint: None,
                            level: Level::Error,
                            location: Some(Location {
                                label: Label {
                                    text: if *imported_value_as_type
                                        && matches!(
                                            context,
                                            ModuleValueUsageContext::UnqualifiedImport
                                        ) {
                                        Some(format!("Did you mean `type {name}`?"))
                                    } else {
                                        did_you_mean(name, value_constructors)
                                    },
                                    span: *location,
                                },
                                path: path.clone(),
                                src: src.clone(),
                                extra_labels: vec![],
                            }),
                        }
                    }

                    TypeError::ModuleAliasUsedAsName { location, name } => {
                        let text = wrap(
                            "Modules are not values, so you cannot assign them \
to variables, pass them to functions, or anything else that you would do with a value.",
                        );
                        Diagnostic {
                            title: format!("Module `{name}` used as a value"),
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

                    TypeError::IncorrectNumClausePatterns {
                        location,
                        expected,
                        given,
                    } => {
                        let subject = if *expected == 1 {
                            "subject"
                        } else {
                            "subjects"
                        };
                        let pattern = if *expected == 1 {
                            "pattern"
                        } else {
                            "patterns"
                        };
                        let text = wrap_format!(
                            "This case expression has {expected} {subject}, \
but this pattern matches {given}.
Each clause must have a pattern for every subject value.",
                        );
                        Diagnostic {
                            title: "Incorrect number of patterns".into(),
                            text,
                            hint: None,
                            level: Level::Error,
                            location: Some(Location {
                                label: Label {
                                    text: Some(format!(
                                        "Expected {expected} {pattern}, got {given}"
                                    )),
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
function, or be an argument to the function. The variable \
`{name}` is not defined locally.",
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
                            "All alternative patterns must define the same variables as \
the initial pattern. This variable `{name}` has not been previously defined.",
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
                                    text: Some(
                                        "This does not define all required variables".into(),
                                    ),
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
                        let mut printer = Printer::new(names);
                        let text = format!(
                            "To index into this value it needs to be a tuple, \
however it has this type:

    {}",
                            printer.print_type(given),
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
                        let text = wrap(
                            "To index into a tuple we need to \
know its size, but we don't know anything about this type yet. \
Please add some type annotations so we can continue.",
                        );
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
                        let text = wrap(
                            "In order to access a record field \
we need to know what type it is, but I can't tell \
the type here. Try adding type annotations to your \
function and try again.",
                        );
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
                                vec![format!(
                                    "Hint: This segment already has the type {existing_type}."
                                )],
                            ),

                            bit_array::ErrorType::ConflictingSignednessOptions {
                                existing_signed,
                            } => (
                                "This is an extra signedness specifier",
                                vec![format!(
                                    "Hint: This segment already has a \
signedness of {existing_signed}."
                                )],
                            ),

                            bit_array::ErrorType::ConflictingEndiannessOptions {
                                existing_endianness,
                            } => (
                                "This is an extra endianness specifier",
                                vec![format!(
                                    "Hint: This segment already has an \
endianness of {existing_endianness}."
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
                                vec![wrap(
                                    "Hint: signed and unsigned \
can only be used with int, float, utf16 and utf32 types.",
                                )],
                            ),

                            bit_array::ErrorType::OptionNotAllowedInValue => (
                                "This option is only allowed in BitArray patterns",
                                vec!["Hint: This option has no effect in BitArray values.".into()],
                            ),

                            bit_array::ErrorType::SignednessUsedOnNonInt { type_ } => (
                                "Signedness is only valid with int types",
                                vec![format!("Hint: This segment has a type of {type_}")],
                            ),
                            bit_array::ErrorType::TypeDoesNotAllowSize { type_ } => (
                                "Size cannot be specified here",
                                vec![format!("Hint: {type_} segments have an automatic size.")],
                            ),
                            bit_array::ErrorType::TypeDoesNotAllowUnit { type_ } => (
                                "Unit cannot be specified here",
                                vec![wrap(&format!(
                                    "Hint: {type_} segments \
are sized based on their value and cannot have a unit."
                                ))],
                            ),
                            bit_array::ErrorType::VariableUtfSegmentInPattern => (
                                "This cannot be a variable",
                                vec![wrap(
                                    "Hint: in patterns utf8, utf16, and \
utf32  must be an exact string.",
                                )],
                            ),
                            bit_array::ErrorType::SegmentMustHaveSize => (
                                "This segment has no size",
                                vec![wrap(
                                    "Hint: Bit array segments without \
a size are only allowed at the end of a bin pattern.",
                                )],
                            ),
                            bit_array::ErrorType::UnitMustHaveSize => (
                                "This needs an explicit size",
                                vec![
                                    "Hint: If you specify unit() you must also specify size()."
                                        .into(),
                                ],
                            ),
                            bit_array::ErrorType::ConstantSizeNotPositive => {
                                ("A constant size must be a positive number", vec![])
                            }
                            bit_array::ErrorType::OptionNotSupportedForTarget {
                                target,
                                option: UnsupportedOption::NativeEndianness,
                            } => (
                                "Unsupported endianness",
                                vec![wrap_format!(
                                    "The {target} target does not support the `native` \
endianness option."
                                )],
                            ),
                            bit_array::ErrorType::OptionNotSupportedForTarget {
                                target,
                                option: UnsupportedOption::UtfCodepointPattern,
                            } => (
                                "UTF-codepoint pattern matching is not supported",
                                vec![wrap_format!(
                                    "The {target} target does not support \
UTF-codepoint pattern matching."
                                )],
                            ),
                        };
                        extra.push("See: https://tour.gleam.run/data-types/bit-arrays/".into());
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
                        let text = wrap_format!(
                            "The module name `{name}` contains the keyword `{keyword}`, \
so importing it would be a syntax error.
Try a different name for this module."
                        );
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
                        text: format!(
                            "Two `{name}` arguments have been defined for this function."
                        ),
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
                        text: wrap(
                            "All unlabelled arguments must come before any labelled arguments.",
                        ),
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
                        let mut text = wrap(
                            "This assignment uses a pattern that does not \
match all possible values. If one of the other values \
is used then the assignment will crash.

The missing patterns are:\n",
                        );
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
                        let mut text = wrap(
                            "This case expression does not have a pattern \
for all possible values. If it is run on one of the \
values without a pattern then it will crash.

The missing patterns are:\n",
                        );
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

                    TypeError::MissingCaseBody { location } => {
                        let text = wrap("This case expression is missing its body.");
                        Diagnostic {
                            title: "Missing case body".into(),
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
and there is no implementation for the {} target.\n",
                            match current_target {
                                Target::Erlang => "Erlang",
                                Target::JavaScript => "JavaScript",
                            }
                        );
                        let hint = wrap("Did you mean to build for a different target?");
                        Diagnostic {
                            title: "Unsupported target".into(),
                            text,
                            hint: Some(hint),
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

                    TypeError::NotFnInUse { location, type_ } => {
                        let mut printer = Printer::new(names);
                        let text = wrap_format!(
                            "In a use expression, there should be a function on \
the right hand side of `<-`, but this value has type:

    {}

See: https://tour.gleam.run/advanced-features/use/",
                            printer.print_type(type_)
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

                    TypeError::UseFnDoesntTakeCallback {
                        location,
                        actual_type: None,
                    }
                    | TypeError::UseFnIncorrectArity {
                        location,
                        expected: 0,
                        given: 1,
                    } => {
                        let text = wrap(
                            "The function on the right of `<-` here \
takes no arguments, but it has to take at least \
one argument, a callback function.

See: https://tour.gleam.run/advanced-features/use/",
                        );
                        Diagnostic {
                            title: "Incorrect arity".into(),
                            text,
                            hint: None,
                            level: Level::Error,
                            location: Some(Location {
                                label: Label {
                                    text: Some("Expected no arguments, got 1".into()),
                                    span: *location,
                                },
                                path: path.clone(),
                                src: src.clone(),
                                extra_labels: vec![],
                            }),
                        }
                    }

                    TypeError::UseFnIncorrectArity {
                        location,
                        expected,
                        given,
                    } => {
                        let expected_string = match expected {
                            0 => "no arguments".into(),
                            1 => "1 argument".into(),
                            _ => format!("{expected} arguments"),
                        };
                        let supplied_arguments = given - 1;
                        let supplied_arguments_string = match supplied_arguments {
                            0 => "no arguments".into(),
                            1 => "1 argument".into(),
                            _ => format!("{given} arguments"),
                        };
                        let label = format!("Expected {expected_string}, got {given}");
                        let mut text: String = format!(
                            "The function on the right of `<-` \
here takes {expected_string}.\n"
                        );

                        if expected > given {
                            if supplied_arguments == 0 {
                                text.push_str(
                                    "The only argument that was supplied is \
the `use` callback function.\n",
                                )
                            } else {
                                text.push_str(&format!(
                                    "You supplied {supplied_arguments_string} \
and the final one is the `use` callback function.\n"
                                ));
                            }
                        } else {
                            text.push_str(
                                "All the arguments have already been supplied, \
so it cannot take the `use` callback function as a final argument.\n",
                            )
                        };

                        text.push_str("\nSee: https://tour.gleam.run/advanced-features/use/");

                        Diagnostic {
                            title: "Incorrect arity".into(),
                            text: wrap(&text),
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

                    TypeError::UseFnDoesntTakeCallback {
                        location,
                        actual_type: Some(actual),
                    } => {
                        let mut printer = Printer::new(names);
                        let text = wrap_format!(
                            "The function on the right hand side of `<-` \
has to take a callback function as its last argument. \
But the last argument of this function has type:

    {}

See: https://tour.gleam.run/advanced-features/use/",
                            printer.print_type(actual)
                        );
                        Diagnostic {
                            title: "Type mismatch".into(),
                            text: wrap(&text),
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

                    TypeError::UseCallbackIncorrectArity {
                        pattern_location,
                        call_location,
                        expected,
                        given,
                    } => {
                        let expected = match expected {
                            0 => "no arguments".into(),
                            1 => "1 argument".into(),
                            _ => format!("{expected} arguments"),
                        };

                        let specified = match given {
                            0 => "none were provided".into(),
                            1 => "1 was provided".into(),
                            _ => format!("{given} were provided"),
                        };

                        let text = wrap_format!(
                            "This function takes a callback that expects {expected}. \
But {specified} on the left hand side of `<-`.

See: https://tour.gleam.run/advanced-features/use/"
                        );
                        Diagnostic {
                            title: "Incorrect arity".into(),
                            text,
                            hint: None,
                            level: Level::Error,
                            location: Some(Location {
                                label: Label {
                                    text: None,
                                    span: *call_location,
                                },
                                path: path.clone(),
                                src: src.clone(),
                                extra_labels: vec![ExtraLabel {
                                    src_info: None,
                                    label: Label {
                                        text: Some(format!("Expected {expected}, got {given}")),
                                        span: *pattern_location,
                                    },
                                }],
                            }),
                        }
                    }

                    TypeError::BadName {
                        location,
                        name,
                        kind,
                    } => {
                        let kind_str = kind.as_str();
                        let label = format!("This is not a valid {} name", kind_str.to_lowercase());
                        let text = match kind {
                            Named::Type | Named::TypeAlias | Named::CustomTypeVariant => {
                                wrap_format!(
                                    "Hint: {} names start with an uppercase \
letter and contain only lowercase letters, numbers, \
and uppercase letters.
Try: {}",
                                    kind_str,
                                    to_upper_camel_case(name)
                                )
                            }
                            Named::Variable
                            | Named::TypeVariable
                            | Named::Argument
                            | Named::Label
                            | Named::Constant
                            | Named::Function => wrap_format!(
                                "Hint: {} names start with a lowercase letter \
and contain a-z, 0-9, or _.
Try: {}",
                                kind_str,
                                to_snake_case(name)
                            ),
                            Named::Discard => wrap_format!(
                                "Hint: {} names start with _ and contain \
a-z, 0-9, or _.
Try: _{}",
                                kind_str,
                                to_snake_case(name)
                            ),
                        };

                        Diagnostic {
                            title: format!("Invalid {} name", kind_str.to_lowercase()),
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

                    TypeError::AllVariantsDeprecated { location } => {
                        let text = String::from(
                            "Consider deprecating the type as a whole.

  @deprecated(\"message\")
  type Wibble {
    Wobble1
    Wobble2
  }
",
                        );
                        Diagnostic {
                            title: "All variants of custom type deprecated.".into(),
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
                    TypeError::DeprecatedVariantOnDeprecatedType { location } => {
                        let text = wrap(
                            "This custom type has already been deprecated, so deprecating \
one of its variants does nothing.
Consider removing the deprecation attribute on the variant.",
                        );

                        Diagnostic {
                            title: "Custom type already deprecated".into(),
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

                    TypeError::EchoWithNoFollowingExpression { location } => Diagnostic {
                        title: "Invalid echo use".to_string(),
                        text: wrap("The `echo` keyword should be followed by a value to print."),
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("I was expecting a value after this".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    },

                    TypeError::StringConcatenationWithAddInt { location } => Diagnostic {
                        title: "Type mismatch".to_string(),
                        text: wrap(
                            "The + operator can only be used on Ints.
To join two strings together you can use the <> operator.",
                        ),
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("Use <> instead".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    },

                    TypeError::IntOperatorOnFloats { location, operator } => Diagnostic {
                        title: "Type mismatch".to_string(),
                        text: wrap_format!(
                            "The {} operator can only be used on Ints.",
                            operator.name()
                        ),
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: operator
                                    .float_equivalent()
                                    .map(|operator| format!("Use {} instead", operator.name())),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    },

                    TypeError::FloatOperatorOnInts { location, operator } => Diagnostic {
                        title: "Type mismatch".to_string(),
                        text: wrap_format!(
                            "The {} operator can only be used on Floats.",
                            operator.name()
                        ),
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: operator
                                    .int_equivalent()
                                    .map(|operator| format!("Use {} instead", operator.name())),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    },

                    TypeError::DoubleVariableAssignmentInBitArray { location } => Diagnostic {
                        title: "Double variable assignment".to_string(),
                        text: wrap(
                            "This pattern assigns to two different variables \
at once, which is not possible in bit arrays.",
                        ),
                        hint: Some(wrap("Remove the `as` assignment.")),
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

                    TypeError::NonUtf8StringAssignmentInBitArray { location } => Diagnostic {
                        title: "Non UTF-8 string assignment".to_string(),
                        text: wrap(
                            "This pattern assigns a non UTF-8 string to a \
variable in a bit array. This is planned to be supported in the future, but we are \
unsure of the desired behaviour. Please go to https://github.com/gleam-lang/gleam/issues/4566 \
and explain your usecase for this pattern, and how you would expect it to behave.",
                        ),
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

                    TypeError::PrivateOpaqueType { location } => Diagnostic {
                        title: "Private opaque type".to_string(),
                        text: wrap("Only a public type can be opaque."),
                        hint: None,
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("You can safely remove this.".to_string()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    },

                    TypeError::SrcImportingDevDependency {
                        location,
                        importing_module,
                        imported_module,
                        package,
                    } => Diagnostic {
                        title: "App importing dev dependency".to_string(),
                        text: wrap_format!(
                            "The application module `{importing_module}` is \
importing the module `{imported_module}`, but `{package}`, the package it \
belongs to, is a dev dependency.

Dev dependencies are not included in production builds so application \
modules should not import them. Perhaps change `{package}` to a regular dependency."
                        ),
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

                    TypeError::ExternalTypeWithConstructors { location } => Diagnostic {
                        title: "External type with constructors".to_string(),
                        text: wrap_format!(
                            "This type is annotated with the `@external` annotation, \
but it has constructors. The `@external` annotation is only for external types \
with no constructors."
                        ),
                        hint: Some("Remove the `@external` annotation".into()),
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

                    TypeError::LowercaseBoolPattern { location } => Diagnostic {
                        title: "Lowercase bool pattern".to_string(),
                        text: "See: https://tour.gleam.run/basics/bools/".into(),
                        hint: Some("In Gleam bool literals are `True` and `False`.".into()),
                        level: Level::Error,
                        location: Some(Location {
                            label: Label {
                                text: Some("This is not a bool".into()),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    },
                })
                .collect_vec(),

            Error::Parse { path, src, error } => {
                let location = if error.error == ParseErrorType::UnexpectedEof {
                    crate::ast::SrcSpan {
                        start: (src.len() - 1) as u32,
                        end: (src.len() - 1) as u32,
                    }
                } else {
                    error.location
                };

                let title = String::from("Syntax error");
                let ParseErrorDetails {
                    text,
                    label_text,
                    extra_labels,
                    hint,
                } = error.error.details();
                vec![Diagnostic {
                    title,
                    text,
                    level: Level::Error,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.clone(),
                        label: Label {
                            text: Some(label_text.into()),
                            span: location,
                        },
                        extra_labels,
                    }),
                    hint,
                }]
            }

            Error::ImportCycle { modules } => {
                let first_location = &modules.first().1;
                let rest_locations = modules
                    .iter()
                    .skip(1)
                    .map(|(_, l)| ExtraLabel {
                        label: Label {
                            text: Some("Imported here".into()),
                            span: l.location,
                        },
                        src_info: Some((l.src.clone(), l.path.clone())),
                    })
                    .collect_vec();
                let mut text = "The import statements for these modules form a cycle:
"
                .into();
                let mod_names = modules.iter().map(|m| m.0.clone()).collect_vec();
                write_cycle(&mut text, &mod_names);
                text.push_str(
                    "Gleam doesn't support dependency cycles like these, please break the
cycle to continue.",
                );
                vec![Diagnostic {
                    title: "Import cycle".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: Some("Imported here".into()),
                            span: first_location.location,
                        },
                        path: first_location.path.clone(),
                        src: first_location.src.clone(),
                        extra_labels: rest_locations,
                    }),
                }]
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
                vec![Diagnostic {
                    title: "Dependency cycle".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
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
                vec![Diagnostic {
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
                }]
            }

            Error::StandardIo { action, err } => {
                let err = match err {
                    Some(e) => format!(
                        "\nThe error message from the stdio library was:\n\n    {}\n",
                        std_io_error_kind_text(e)
                    ),
                    None => "".into(),
                };
                vec![Diagnostic {
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
                }]
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
                vec![Diagnostic {
                    title: "These files have not been formatted".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
            }

            Error::ForbiddenWarnings { count } => {
                let word_warning = match count {
                    1 => "warning",
                    _ => "warnings",
                };
                let text = "Your project was compiled with the `--warnings-as-errors` flag.
Fix the warnings and try again."
                    .into();
                vec![Diagnostic {
                    title: format!("{count} {word_warning} generated."),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
            }

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
                vec![Diagnostic {
                    title: "Failed to download package".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
            }

            Error::Http(error) => {
                let text = format!(
                    "A HTTP request failed.
The error from the HTTP client was:

    {error}"
                );
                vec![Diagnostic {
                    title: "HTTP error".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
            }

            Error::InvalidVersionFormat { input, error } => {
                let text = format!(
                    "I was unable to parse the version \"{input}\".
The error from the parser was:

    {error}"
                );
                vec![Diagnostic {
                    title: "Invalid version format".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
            }

            Error::IncompatibleLockedVersion { error } => {
                let text = format!(
                    "There is an incompatiblity between a version specified in
manifest.toml and a version range specified in gleam.toml:

    {error}"
                );
                vec![Diagnostic {
                    title: "Incompatible locked version".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
            }

            Error::DependencyCanonicalizationFailed(package) => {
                let text = format!("Local package `{package}` has no canonical path");

                vec![Diagnostic {
                    title: "Failed to create canonical path".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
            }

            Error::DependencyResolutionError(error) => vec![Diagnostic {
                title: "Dependency resolution failed".into(),
                text: wrap(error),
                hint: None,
                location: None,
                level: Level::Error,
            }],

            Error::DependencyResolutionNoSolution {
                root_package_name,
                derivation_tree,
            } => {
                let text = wrap(
                    &DerivationTreePrinter::new(
                        root_package_name.clone(),
                        derivation_tree.0.clone(),
                    )
                    .print(),
                );
                vec![Diagnostic {
                    title: "Dependency resolution failed".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
            }

            Error::WrongDependencyProvided {
                path,
                expected,
                found,
            } => {
                let text = format!(
                    "Expected package `{expected}` at path `{path}` but found `{found}` instead.",
                );

                vec![Diagnostic {
                    title: "Wrong dependency provided".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
            }

            Error::ProvidedDependencyConflict {
                package,
                source_1,
                source_2,
            } => {
                let text = format!(
                    "The package `{package}` is provided as both `{source_1}` and `{source_2}`.",
                );

                vec![Diagnostic {
                    title: "Conflicting provided dependencies".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
            }

            Error::DuplicateDependency(name) => {
                let text = format!(
                    "The package `{name}` is specified in both the dependencies and
dev_dependencies sections of the gleam.toml file."
                );
                vec![Diagnostic {
                    title: "Dependency duplicated".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
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

description = ""
licences = ["Apache-2.0"]"#
                } else if *description_missing {
                    r#"Add the description field to your gleam.toml file.

description = """#
                } else {
                    r#"Add the licences field to your gleam.toml file.

licences = ["Apache-2.0"]"#
                });
                vec![Diagnostic {
                    title: "Missing required package fields".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
            }

            Error::PublishNonHexDependencies { package } => vec![Diagnostic {
                title: "Unpublished dependencies".into(),
                text: wrap_format!(
                    "The package cannot be published to Hex \
because dependency `{package}` is not a Hex dependency.",
                ),
                hint: None,
                location: None,
                level: Level::Error,
            }],

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
                vec![Diagnostic {
                    title: "Unsupported build tool".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
            }

            Error::FailedToOpenDocs { path, error } => {
                let error = format!("\nThe error message from the library was:\n\n    {error}\n");
                let text = format!(
                    "An error occurred while trying to open the docs:

    {path}
{error}",
                );
                vec![Diagnostic {
                    title: "Failed to open docs".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: None,
                }]
            }

            Error::IncompatibleCompilerVersion {
                package,
                required_version,
                gleam_version,
            } => {
                let text = format!(
                    "The package `{package}` requires a Gleam version \
satisfying {required_version} but you are using v{gleam_version}.",
                );
                vec![Diagnostic {
                    title: "Incompatible Gleam version".into(),
                    text,
                    hint: None,
                    location: None,
                    level: Level::Error,
                }]
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

                vec![Diagnostic {
                    title: format!("Invalid runtime for {target}"),
                    text,
                    hint,
                    location: None,
                    level: Level::Error,
                }]
            }

            Error::JavaScriptPreludeRequired => vec![Diagnostic {
                title: "JavaScript prelude required".into(),
                text: "The --javascript-prelude flag must be given when compiling to JavaScript."
                    .into(),
                level: Level::Error,
                location: None,
                hint: None,
            }],
            Error::CorruptManifest => vec![Diagnostic {
                title: "Corrupt manifest.toml".into(),
                text: "The `manifest.toml` file is corrupt.".into(),
                level: Level::Error,
                location: None,
                hint: Some("Please run `gleam update` to fix it.".into()),
            }],

            Error::GleamModuleWouldOverwriteStandardErlangModule { name, path } => {
                vec![Diagnostic {
                    title: "Erlang module name collision".into(),
                    text: wrap_format!(
                        "The module `{path}` compiles to an Erlang module \
named `{name}`.

By default Erlang includes a module with the same name so if we were \
to compile and load your module it would overwrite the Erlang one, potentially \
causing confusing errors and crashes.
"
                    ),
                    level: Level::Error,
                    location: None,
                    hint: Some("Rename this module and try again.".into()),
                }]
            }

            Error::HexPublishReplaceRequired { version } => vec![Diagnostic {
                title: "Version already published".into(),
                text: wrap_format!(
                    "Version v{version} has already been published.
This release has been recently published so you can replace it \
or you can publish it using a different version number"
                ),
                level: Level::Error,
                location: None,
                hint: Some(
                    "Please add the --replace flag if you want to replace the release.".into(),
                ),
            }],
            Error::HexPackageAlreadyExists { name, version } => vec![Diagnostic {
                title: format!("Package name {name} is already taken"),
                text: wrap_format!(
                    "I couldn't publish v{version} of {name}, it looks like that name is taken
on hex and you don't have permission to publish it!
"
                ),
                level: Level::Error,
                location: None,
                hint: Some(format!(
                    "Choose a new name or make sure you are authorised as one of the
hex users that can publish {name}"
                )),
            }],

            Error::CannotAddSelfAsDependency { name } => vec![Diagnostic {
                title: "Dependency cycle".into(),
                text: wrap_format!(
                    "A package cannot depend on itself, so you cannot \
add `gleam add {name}` in this project."
                ),
                level: Level::Error,
                location: None,
                hint: None,
            }],
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
        ErrorKind::HostUnreachable
        | ErrorKind::NetworkUnreachable
        | ErrorKind::NetworkDown
        | ErrorKind::NotADirectory
        | ErrorKind::IsADirectory
        | ErrorKind::DirectoryNotEmpty
        | ErrorKind::ReadOnlyFilesystem
        | ErrorKind::StaleNetworkFileHandle
        | ErrorKind::StorageFull
        | ErrorKind::NotSeekable
        | ErrorKind::QuotaExceeded
        | ErrorKind::FileTooLarge
        | ErrorKind::ResourceBusy
        | ErrorKind::ExecutableFileBusy
        | ErrorKind::Deadlock
        | ErrorKind::CrossesDevices
        | ErrorKind::TooManyLinks
        | ErrorKind::InvalidFilename
        | ErrorKind::ArgumentListTooLong
        | ErrorKind::Unsupported
        | ErrorKind::OutOfMemory
        | ErrorKind::Other
        | _ => "An unknown error occurred".into(),
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

        BinOp::And
        | BinOp::Or
        | BinOp::Eq
        | BinOp::NotEq
        | BinOp::LtInt
        | BinOp::LtEqInt
        | BinOp::LtFloat
        | BinOp::LtEqFloat
        | BinOp::GtEqInt
        | BinOp::GtInt
        | BinOp::GtEqFloat
        | BinOp::GtFloat
        | BinOp::AddInt
        | BinOp::AddFloat
        | BinOp::SubInt
        | BinOp::SubFloat
        | BinOp::MultInt
        | BinOp::MultFloat
        | BinOp::DivInt
        | BinOp::DivFloat
        | BinOp::RemainderInt
        | BinOp::Concatenate => None,
    }
}

fn hint_wrap_value_in_result(expected: &Arc<Type>, given: &Arc<Type>) -> Option<String> {
    let expected = collapse_links(expected.clone());
    let (expected_ok_type, expected_error_type) = expected.result_types()?;

    if given.same_as(expected_ok_type.as_ref()) {
        Some("Did you mean to wrap this in an `Ok`?".into())
    } else if given.same_as(expected_error_type.as_ref()) {
        Some("Did you mean to wrap this in an `Error`?".into())
    } else {
        None
    }
}

fn hint_numeric_message(alt: &str, type_: &str) -> String {
    format!("the {alt} operator can be used with {type_}s\n")
}

fn hint_string_message() -> String {
    wrap("Strings can be joined using the `<>` operator.")
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unformatted {
    pub source: Utf8PathBuf,
    pub destination: Utf8PathBuf,
    pub input: EcoString,
    pub output: String,
}

pub fn wrap(text: &str) -> String {
    let mut result = String::with_capacity(text.len());

    for (i, line) in wrap_text(text, 75).iter().enumerate() {
        if i > 0 {
            result.push('\n');
        }
        result.push_str(line);
    }

    result
}

fn wrap_text(text: &str, width: usize) -> Vec<Cow<'_, str>> {
    let mut lines: Vec<Cow<'_, str>> = Vec::new();
    for line in text.split('\n') {
        // check if line needs to be broken
        match line.len() > width {
            false => lines.push(Cow::from(line)),
            true => {
                let mut new_lines = break_line(line, width);
                lines.append(&mut new_lines);
            }
        };
    }

    lines
}

fn break_line(line: &str, width: usize) -> Vec<Cow<'_, str>> {
    let mut lines: Vec<Cow<'_, str>> = Vec::new();
    let mut newline = String::from("");

    // split line by spaces
    for (i, word) in line.split(' ').enumerate() {
        let is_new_line = i < 1 || newline.is_empty();

        let can_add_word = match is_new_line {
            true => newline.len() + word.len() <= width,
            // +1 accounts for space added before word
            false => newline.len() + (word.len() + 1) <= width,
        };

        if can_add_word {
            if !is_new_line {
                newline.push(' ');
            }
            newline.push_str(word);
        } else {
            // word too big, save existing line if present
            if !newline.is_empty() {
                // save current line and reset it
                lines.push(Cow::from(newline.to_owned()));
                newline.clear();
            }

            // then save word to a new line or break it
            match word.len() > width {
                false => newline.push_str(word),
                true => {
                    let (mut newlines, remainder) = break_word(word, width);
                    lines.append(&mut newlines);
                    newline.push_str(remainder);
                }
            }
        }
    }

    // save last line after loop finishes
    if !newline.is_empty() {
        lines.push(Cow::from(newline));
    }

    lines
}

// breaks word into n lines based on width. Returns list of new lines and remainder
fn break_word(word: &str, width: usize) -> (Vec<Cow<'_, str>>, &str) {
    let mut new_lines: Vec<Cow<'_, str>> = Vec::new();
    let (first, mut remainder) = word.split_at(width);
    new_lines.push(Cow::from(first));

    // split remainder until it's small enough
    while remainder.len() > width {
        let (first, second) = remainder.split_at(width);
        new_lines.push(Cow::from(first));
        remainder = second;
    }

    (new_lines, remainder)
}
