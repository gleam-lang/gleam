use std::sync::Arc;

use camino::{Utf8Path, Utf8PathBuf};
use gleam_core::{
    analyse::TargetSupport,
    build::{Codegen, Compile, Mode, Options},
    error::{FileIoAction, FileKind},
    type_,
    warning::VectorWarningEmitterIO,
    Error, Result, Warning,
};
use hexpm::version::Version;

use crate::{build, cli};

enum FixResult {
    FixedSomething,
    NothingToFix,
}

pub fn run() -> Result<()> {
    // When running gleam fix we want all the compilation warnings to be hidden,
    // at the same time we need to access those to apply the fixes: so we
    // accumulate those into a vector.
    let warnings = Arc::new(VectorWarningEmitterIO::new());
    let _built = build::main_with_warnings(
        Options {
            root_target_support: TargetSupport::Enforced,
            warnings_as_errors: false,
            codegen: Codegen::DepsOnly,
            compile: Compile::All,
            mode: Mode::Dev,
            target: None,
            no_print_progress: false,
        },
        build::download_dependencies(cli::Reporter::new())?,
        warnings.clone(),
    )?;
    let warnings = warnings.take();

    let result = fix_minimum_required_version(warnings)?;

    match result {
        FixResult::FixedSomething => println!("Your Gleam code has been fixed!"),
        FixResult::NothingToFix => println!("Nothing to fix."),
    };

    Ok(())
}

fn fix_minimum_required_version(warnings: Vec<Warning>) -> Result<FixResult> {
    let Some(minimum_required_version) = minimum_required_version_from_warnings(warnings) else {
        return Ok(FixResult::NothingToFix);
    };

    // Set the version requirement in gleam.toml
    let mut toml = crate::fs::read("gleam.toml")?
        .parse::<toml_edit::DocumentMut>()
        .map_err(|e| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Parse,
            path: Utf8PathBuf::from("gleam.toml"),
            err: Some(e.to_string()),
        })?;

    #[allow(clippy::indexing_slicing)]
    {
        toml["gleam"] = toml_edit::value(format!(">= {minimum_required_version}"));
    }

    // Write the updated config
    crate::fs::write(Utf8Path::new("gleam.toml"), &toml.to_string())?;
    Ok(FixResult::FixedSomething)
}

/// Returns the highest minimum required version among all warnings requiring a
/// specific Gleam version that is not allowed by the `gleam` version contraint
/// in the `gleam.toml`.
fn minimum_required_version_from_warnings(warnings: Vec<Warning>) -> Option<Version> {
    warnings
        .iter()
        .filter_map(|warning| match warning {
            Warning::Type {
                warning:
                    type_::Warning::FeatureRequiresHigherGleamVersion {
                        minimum_required_version,
                        ..
                    },
                ..
            } => Some(minimum_required_version),
            _ => None,
        })
        .reduce(std::cmp::max)
        .map(|version| version.clone())
}
