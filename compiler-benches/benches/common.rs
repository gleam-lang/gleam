// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use camino::Utf8PathBuf;
use gleam_cli::fs::ProjectIO;
use gleam_core::{io::FileSystemReader, parse::lexer, paths::ProjectPaths};

pub const PACKAGES: [&str; 3] = ["gleam_stdlib", "emojindex", "squirrel"];

const CORPUS: &str = "cases/corpus";

pub fn package_modules(package: &str) -> Vec<String> {
    let paths = ProjectPaths::new(Utf8PathBuf::from(CORPUS));
    let source = paths.build_packages_package(package).join("src");
    let io = ProjectIO::new();

    if !source.is_dir() {
        panic!("{source} not found. Run `gleam deps download` in {CORPUS} first.");
    }

    let mut modules: Vec<_> = gleam_core::io::files_with_extension(&io, &source, "gleam")
        .map(|path| io.read(&path).expect("read module source"))
        .collect();
    modules.sort();
    modules
}

pub fn lex_all(modules: &[String]) {
    for module in modules {
        for token in lexer::make_tokenizer(module) {
            std::hint::black_box(token.expect("source should lex without errors"));
        }
    }
}
