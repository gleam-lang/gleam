// TODO: move TestCompileOutput to a test helper crate

use gleam_core::{
    build::{
        package_compiler::Source, ErlangAppCodegenConfiguration, Target, TargetCodegenConfiguration,
    },
    config::PackageConfig,
    io::Content,
};
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
    path::PathBuf,
};

pub fn prepare(path: &str) -> String {
    let path = PathBuf::from(path);

    let toml = std::fs::read_to_string(path.join("gleam.toml")).unwrap();
    let config: PackageConfig = toml::from_str(&toml).unwrap();

    let sources = path
        .join("src")
        .read_dir()
        .unwrap()
        .map(|entry| {
            let entry = entry.unwrap();
            let localpath = entry.path();
            let filename = localpath.file_name().unwrap();
            let path = PathBuf::from("src").join(filename);
            let name = localpath.file_stem().unwrap().to_string_lossy().to_string();
            Source {
                code: std::fs::read_to_string(entry.path()).unwrap(),
                origin: gleam_core::build::Origin::Src,
                path,
                name,
            }
        })
        .collect();

    let target = match config.target {
        Target::Erlang => TargetCodegenConfiguration::Erlang {
            app_file: Some(ErlangAppCodegenConfiguration {
                include_dev_deps: false,
            }),
        },
        Target::JavaScript => TargetCodegenConfiguration::JavaScript {
            emit_typescript_definitions: false,
        },
    };

    let ids = gleam_core::uid::UniqueIdGenerator::new();
    let mut modules = im::HashMap::new();
    let mut warnings = Vec::new();
    let filesystem = gleam_core::io::memory::InMemoryFileSystem::new();
    let root = PathBuf::from("/");
    let out = PathBuf::from("/out/lib/the_package");
    let lib = PathBuf::from("/out/lib");
    let mut build_journal = HashSet::new();
    let mut compiler = gleam_core::build::PackageCompiler::new(
        &config,
        &root,
        &out,
        &lib,
        &target,
        ids,
        filesystem.clone(),
        Some(&mut build_journal),
    );
    compiler.write_entrypoint = false;
    compiler.write_metadata = false;
    compiler.compile_beam_bytecode = false;
    compiler.copy_native_files = false;
    compiler.sources = sources;
    compiler
        .compile(&mut warnings, &mut modules, &mut im::HashMap::new())
        .unwrap();
    let files = filesystem.into_contents();
    TestCompileOutput { files, warnings }.as_overview_text()
}

// TODO: move this to a test helper crate
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

            match content {
                Content::Text(text) => buffer.push_str(text),
                Content::Binary(data) => write!(buffer, "{:#?}", data).unwrap(),
            };
            buffer.push('\n');
            buffer.push('\n');
        }

        for warning in self.warnings.iter() {
            write!(buffer, "{:#?}", warning).unwrap();
            buffer.push('\n');
            buffer.push('\n');
        }

        buffer
    }
}

#[cfg(test)]
mod tests {
    include!(concat!(env!("OUT_DIR"), "/tests.rs"));
}
