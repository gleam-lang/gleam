// TODO: move TestCompileOutput to a test helper crate
#![allow(
    // TODO: fix
    clippy::arc_with_non_send_sync,
)]

#[cfg(test)]
mod generated_tests;

use gleam_core::{
    build::{
        ErlangAppCodegenConfiguration, Mode, StaleTracker, Target, TargetCodegenConfiguration,
    },
    config::PackageConfig,
    io::{memory::InMemoryFileSystem, Content, FileSystemWriter},
    warning::{VectorWarningEmitterIO, WarningEmitter},
};
use itertools::Itertools;
use regex::Regex;
use std::{collections::HashMap, fmt::Write, sync::Arc};

use camino::{Utf8Path, Utf8PathBuf};

pub fn prepare(path: &str) -> String {
    let root = Utf8PathBuf::from(path).canonicalize_utf8().unwrap();

    let toml = std::fs::read_to_string(root.join("gleam.toml")).unwrap();
    let config: PackageConfig = toml::from_str(&toml).unwrap();

    let target = match config.target {
        Target::Erlang => TargetCodegenConfiguration::Erlang {
            app_file: Some(ErlangAppCodegenConfiguration {
                include_dev_deps: true,
            }),
        },
        Target::JavaScript => TargetCodegenConfiguration::JavaScript {
            emit_typescript_definitions: config.javascript.typescript_declarations,
        },
    };

    let ids = gleam_core::uid::UniqueIdGenerator::new();
    let mut modules = im::HashMap::new();
    let warnings = VectorWarningEmitterIO::default();
    let warning_emitter = WarningEmitter::new(Arc::new(warnings.clone()));
    let filesystem = to_in_memory_filesystem(&root);
    let initial_files = filesystem.paths();
    let root = Utf8PathBuf::from("");
    let out = Utf8PathBuf::from("/out/lib/the_package");
    let lib = Utf8PathBuf::from("/out/lib");
    let mut compiler = gleam_core::build::PackageCompiler::new(
        &config,
        Mode::Dev,
        &root,
        &out,
        &lib,
        &target,
        ids,
        filesystem.clone(),
    );
    compiler.write_entrypoint = false;
    compiler.write_metadata = true;
    compiler.compile_beam_bytecode = false;
    compiler.copy_native_files = false;
    let result = compiler.compile(
        &warning_emitter,
        &mut modules,
        &mut im::HashMap::new(),
        &mut StaleTracker::default(),
    );
    match result {
        Ok(_) => {
            for path in initial_files {
                filesystem.delete_file(&path).unwrap();
            }
            let files = filesystem.into_contents();
            let warnings = warnings.take();
            TestCompileOutput { files, warnings }.as_overview_text()
        }
        Err(error) => normalise_diagnostic(&error.pretty_string()),
    }
}

fn normalise_diagnostic(text: &str) -> String {
    // There is an extra ^ on Windows in some error messages' code
    // snippets.
    // I've not managed to determine why this is yet (it is especially
    // tricky without a Windows computer) so for now we just squash them
    // in these cross-platform tests.
    Regex::new(r"\^+")
        .expect("^ sequence regex")
        .replace_all(text, "^")
        .replace('\\', "/")
}

#[derive(Debug)]
pub struct TestCompileOutput {
    files: HashMap<Utf8PathBuf, Content>,
    warnings: Vec<gleam_core::Warning>,
}

impl TestCompileOutput {
    pub fn as_overview_text(&self) -> String {
        let mut buffer = String::new();
        for (path, content) in self.files.iter().sorted_by(|a, b| a.0.cmp(b.0)) {
            buffer.push_str("//// ");
            buffer.push_str(&path.as_str().replace('\\', "/"));
            buffer.push('\n');

            let extension = path.extension();
            match content {
                _ if extension == Some("cache") => buffer.push_str("<.cache binary>"),

                _ if path.ends_with("gleam.mjs") || path.ends_with("gleam.d.ts") => {
                    buffer.push_str("<prelude>")
                }

                Content::Binary(data) => write!(buffer, "<{} byte binary>", data.len()).unwrap(),

                Content::Text(text) => buffer.push_str(text),
            };
            buffer.push('\n');
            buffer.push('\n');
        }

        for warning in self.warnings.iter().map(|w| w.to_pretty_string()).sorted() {
            write!(buffer, "//// Warning\n{}", normalise_diagnostic(&warning)).unwrap();
            buffer.push('\n');
            buffer.push('\n');
        }

        buffer
    }
}

fn to_in_memory_filesystem(path: &Utf8Path) -> InMemoryFileSystem {
    let fs = InMemoryFileSystem::new();

    let files = walkdir::WalkDir::new(path)
        .follow_links(true)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .map(|d| d.into_path());

    for fullpath in files {
        let content = std::fs::read(&fullpath).unwrap();
        let path = fullpath.strip_prefix(path).unwrap();
        fs.write_bytes(Utf8Path::from_path(path).unwrap(), &content)
            .unwrap();
    }

    fs
}
