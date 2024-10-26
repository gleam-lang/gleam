#[cfg(test)]
mod generated_tests;

use camino::Utf8PathBuf;
use gleam_core::{
    build::{
        ErlangAppCodegenConfiguration, Mode, NullTelemetry, Outcome, StaleTracker, Target,
        TargetCodegenConfiguration,
    },
    config::PackageConfig,
    io::FileSystemWriter,
    warning::{VectorWarningEmitterIO, WarningEmitter},
};
use std::{
    collections::{HashMap, HashSet},
    fs,
    io::Read,
    process::{Command, Stdio},
    rc::Rc,
};

pub fn prepare(path: &str) -> String {
    let root = Utf8PathBuf::from(path).canonicalize_utf8().unwrap();
    let code = fs::read_to_string(root.as_path().join("src").join("main.gleam")).expect("main");
    fs::remove_dir_all(root.as_path().join("build")).expect("clean build dir");

    let mut cmd = Command::new("gleam")
        .arg("run")
        .arg("-mmain")
        .arg("--no-print-progress")
        .current_dir(&root)
        .stderr(Stdio::piped())
        .spawn()
        .expect("gleam run -terl");

    let mut output = format!("--- CODE\n{code}\n--- OUTPUT ON ERLANG TARGET\n");

    cmd.stderr
        .take()
        .expect("erl stderr")
        .read_to_string(&mut output)
        .expect("erl stderr to string");

    output.push_str("\n--- OUTPUT ON JAVASCRIPT TARGET\n");

    let mut cmd = Command::new("gleam")
        .arg("run")
        .arg("-mmain")
        .arg("-tjs")
        .arg("--no-print-progress")
        .current_dir(&root)
        .stderr(Stdio::piped())
        .spawn()
        .expect("gleam run -tjs");

    cmd.stderr
        .take()
        .expect("js stderr")
        .read_to_string(&mut output)
        .expect("js stderr to string");

    output

    //let root = Utf8PathBuf::from(path).canonicalize_utf8().unwrap();

    //let toml = std::fs::read_to_string(root.join("gleam.toml")).unwrap();
    //let config: PackageConfig = toml::from_str(&toml).unwrap();

    //let target = match config.target {
    //    Target::Erlang => TargetCodegenConfiguration::Erlang {
    //        app_file: Some(ErlangAppCodegenConfiguration {
    //            include_dev_deps: true,
    //            package_name_overrides: HashMap::new(),
    //        }),
    //    },
    //    Target::JavaScript => TargetCodegenConfiguration::JavaScript {
    //        emit_typescript_definitions: config.javascript.typescript_declarations,
    //        prelude_location: Utf8PathBuf::from("../prelude.mjs"),
    //    },
    //};

    //let ids = gleam_core::uid::UniqueIdGenerator::new();
    //let mut modules = im::HashMap::new();
    //let warnings = VectorWarningEmitterIO::default();
    //let warning_emitter = WarningEmitter::new(Rc::new(warnings.clone()));
    //let filesystem = test_helpers_rs::to_in_memory_filesystem(&root);
    //let initial_files = filesystem.paths();
    //let root = Utf8PathBuf::from("");
    //let out = Utf8PathBuf::from("/out/lib/the_package");
    //let lib = Utf8PathBuf::from("/out/lib");
    //let mut compiler = gleam_core::build::PackageCompiler::new(
    //    &config,
    //    Mode::Dev,
    //    &root,
    //    &out,
    //    &lib,
    //    &target,
    //    ids,
    //    filesystem.clone(),
    //);
    //compiler.write_entrypoint = false;
    //compiler.write_metadata = true;
    //compiler.compile_beam_bytecode = false;
    //compiler.copy_native_files = false;
    //let result = compiler.compile(
    //    &warning_emitter,
    //    &mut modules,
    //    &mut im::HashMap::new(),
    //    &mut StaleTracker::default(),
    //    &mut HashSet::new(),
    //    &NullTelemetry,
    //);
    //match result {
    //    Outcome::Ok(_) => {
    //        for path in initial_files {
    //            filesystem.delete_file(&path).unwrap();
    //        }
    //        let files = filesystem.into_contents();
    //        let warnings = warnings.take();
    //        test_helpers_rs::TestCompileOutput { files, warnings }.as_overview_text()
    //    }
    //    Outcome::TotalFailure(error) | Outcome::PartialFailure(_, error) => {
    //        test_helpers_rs::normalise_diagnostic(&error.pretty_string())
    //    }
    //}
}
