// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2020 The Gleam contributors

use crate::{
    CompilePackage, config,
    fs::{self, ConsoleWarningEmitter, ProjectIO},
};
use camino::Utf8Path;
use ecow::EcoString;
use gleam_core::{
    Error, Result,
    build::{
        ErlangAppCodegenConfiguration, ErlangOutput, Mode, NullTelemetry, PackageCompiler,
        StaleTracker, Target, TargetCodegenConfiguration,
    },
    error::{FileIoAction, FileIoCause, FileKind},
    metadata,
    paths::{self, ProjectPaths},
    type_::ModuleInterface,
    uid::UniqueIdGenerator,
    warning::WarningEmitter,
};
use std::{collections::HashSet, rc::Rc};

pub fn command(options: CompilePackage) -> Result<()> {
    let ids = UniqueIdGenerator::new();
    let mut type_manifests = load_libraries(&ids, &options.libraries_directory)?;
    let mut defined_modules = imbl::HashMap::new();
    let warnings = WarningEmitter::new(Rc::new(ConsoleWarningEmitter));
    let paths = ProjectPaths::new(options.package_directory.clone());
    let config = config::read(paths.root_config())?;

    let io = ProjectIO::new();
    // Initialise the BEAM compiler instance eagerly, so we don't have to wait
    // for it to boot when we come to use it for the first time.
    if options.target.is_erlang() && !options.skip_beam_compilation {
        io.initialise_beam_compiler()?;
    }
    let mode = compilation_mode(&options);
    let target = match options.target {
        Target::Erlang => TargetCodegenConfiguration::Erlang {
            app_file: app_file_configuration(&options, mode),
            output: ErlangOutput::Textual,
        },
        Target::JavaScript => TargetCodegenConfiguration::JavaScript {
            emit_typescript_definitions: false,
            emit_source_maps: false,
            prelude_location: options
                .javascript_prelude
                .ok_or_else(|| Error::JavaScriptPreludeRequired)?,
        },
    };

    tracing::info!("Compiling package");

    let mut compiler = PackageCompiler::new(
        &config,
        mode,
        &options.package_directory,
        &options.output_directory,
        &options.libraries_directory,
        &target,
        ids,
        io,
    );
    compiler.write_entrypoint = false;
    compiler.write_metadata = true;
    compiler
        .compile(
            &warnings,
            &mut type_manifests,
            &mut defined_modules,
            &mut StaleTracker::default(),
            &mut HashSet::new(),
            &NullTelemetry,
        )
        .into_result()
        .map(|_| ())
}

fn compilation_mode(options: &CompilePackage) -> Mode {
    if options.no_dev {
        Mode::Prod
    } else {
        Mode::Dev
    }
}

fn app_file_configuration(
    options: &CompilePackage,
    mode: Mode,
) -> Option<ErlangAppCodegenConfiguration> {
    if options.skip_beam_compilation {
        return None;
    }
    Some(ErlangAppCodegenConfiguration {
        include_dev_deps: mode.includes_dev_dependencies(),
        package_name_overrides: options.otp_app_names.clone(),
    })
}

fn load_libraries(
    ids: &UniqueIdGenerator,
    lib: &Utf8Path,
) -> Result<imbl::HashMap<EcoString, ModuleInterface>> {
    tracing::info!("Reading precompiled module metadata files");
    let mut manifests = imbl::HashMap::new();
    for lib in fs::read_dir(lib)?.filter_map(Result::ok) {
        let path = lib.path().join(paths::ARTEFACT_DIRECTORY_NAME);
        if !path.is_dir() {
            continue;
        }
        for module in fs::module_caches_paths(path)? {
            let bytes = fs::read_bytes(module.clone())?;
            let module = match metadata::decode(&bytes, ids.clone()) {
                Some(module) => module,
                None => {
                    return Err(Error::FileIo {
                        kind: FileKind::File,
                        action: FileIoAction::Parse,
                        path: module,
                        cause: FileIoCause::CacheMetadataFormatIncorrect,
                    });
                }
            };
            let _ = manifests.insert(module.name.clone(), module);
        }
    }

    Ok(manifests)
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::Parser;

    #[derive(Parser)]
    struct Cli {
        #[command(flatten)]
        options: CompilePackage,
    }

    fn parse(extra_arguments: &[&str]) -> CompilePackage {
        let arguments = [
            "gleam",
            "--target",
            "erlang",
            "--package",
            ".",
            "--out",
            "out",
        ];
        let arguments = arguments.iter().chain(["--lib", "lib"].iter());
        Cli::parse_from(arguments.chain(extra_arguments)).options
    }

    fn includes_dev_deps(options: &CompilePackage) -> bool {
        app_file_configuration(options, compilation_mode(options))
            .expect("app file configuration")
            .include_dev_deps
    }

    #[test]
    fn app_file_includes_dev_dependencies() {
        // https://github.com/gleam-lang/gleam/issues/6339
        assert!(includes_dev_deps(&parse(&[])));
    }

    #[test]
    fn app_file_excludes_dev_dependencies_with_no_dev() {
        assert!(!includes_dev_deps(&parse(&["--no-dev"])));
    }

    #[test]
    fn no_app_file_with_no_beam() {
        let options = parse(&["--no-beam"]);
        assert!(app_file_configuration(&options, compilation_mode(&options)).is_none());
    }
}
