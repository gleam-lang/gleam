// TODO: move TestCompileOutput to a test helper crate

#[cfg(test)]
mod generated_tests;

use gleam_core::{
    build::{ErlangAppCodegenConfiguration, Mode, Target, TargetCodegenConfiguration},
    config::PackageConfig,
    io::{memory::InMemoryFileSystem, Content, FileSystemWriter},
};
use itertools::Itertools;
use std::{
    collections::HashMap,
    ffi::OsStr,
    fmt::Write,
    path::{Path, PathBuf},
};

pub fn prepare(path: &str) -> String {
    let root = PathBuf::from(path).canonicalize().unwrap();

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
    let mut warnings = Vec::new();
    let filesystem = to_in_memory_filesystem(&root);
    let initial_files = filesystem.paths();
    let root = PathBuf::from("");
    let out = PathBuf::from("/out/lib/the_package");
    let lib = PathBuf::from("/out/lib");
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
    let result = compiler.compile(&mut warnings, &mut modules, &mut im::HashMap::new());
    match result {
        Ok(_) => {
            for path in initial_files {
                filesystem.delete_file(&path).unwrap();
            }
            let files = filesystem.into_contents();
            TestCompileOutput { files, warnings }.as_overview_text()
        }
        Err(error) => error.pretty_string(),
    }
}

#[derive(Debug)]
pub struct TestCompileOutput {
    files: HashMap<PathBuf, Content>,
    warnings: Vec<gleam_core::Warning>,
}

impl TestCompileOutput {
    pub fn as_overview_text(&self) -> String {
        let mut buffer = String::new();
        for (path, content) in self.files.iter().sorted_by(|a, b| a.0.cmp(b.0)) {
            buffer.push_str("//// ");
            buffer.push_str(path.to_str().unwrap());
            buffer.push('\n');

            let extension = path.extension().and_then(OsStr::to_str);
            match content {
                _ if extension == Some("gleam_module") => buffer.push_str("<.gleam_module binary>"),

                _ if path.ends_with("gleam.mjs") || path.ends_with("gleam.d.ts") => {
                    buffer.push_str("<prelude>")
                }

                Content::Binary(data) => write!(buffer, "<{} byte binary>", data.len()).unwrap(),

                Content::Text(text) => buffer.push_str(text),
            };
            buffer.push('\n');
            buffer.push('\n');
        }

        for warning in self.warnings.iter() {
            write!(buffer, "//// Warning\n{:#?}", warning).unwrap();
            buffer.push('\n');
            buffer.push('\n');
        }

        buffer
    }
}

fn to_in_memory_filesystem(path: &Path) -> InMemoryFileSystem {
    let fs = InMemoryFileSystem::new();

    let files = walkdir::WalkDir::new(path)
        .follow_links(true)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .map(|d| d.into_path());

    for fullpath in files {
        let content = std::fs::read_to_string(&fullpath).unwrap();
        let path = fullpath.strip_prefix(path).unwrap();
        fs.write(path, &content).unwrap();
    }

    fs
}
