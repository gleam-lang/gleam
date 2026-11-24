// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::*;
use crate::io::{FileSystemReader, memory::InMemoryFileSystem};
use std::collections::HashMap;

#[test]
fn app_file_includes_cached_modules() {
    // A warm rebuild recompiles nothing, so every module comes from the
    // `ebin` scan rather than being freshly compiled; they must still be
    // listed in the .app (https://github.com/gleam-lang/gleam/issues/5834).
    let fs = InMemoryFileSystem::new();
    let mut config = PackageConfig::default();
    config.name = "my_app".into();
    let codegen_config = ErlangAppCodegenConfiguration {
        include_dev_deps: false,
        package_name_overrides: HashMap::new(),
    };

    // Module names here are already in Erlang atom form (`@`, not `/`),
    // matching what a real `ebin` scan of `.beam` filenames would yield.
    ErlangApp::new(Utf8Path::new("/ebin"), &codegen_config)
        .render(
            fs.clone(),
            &config,
            vec!["my_app@one".into(), "my_app@two".into()],
        )
        .expect("render .app");

    let app = fs
        .read(Utf8Path::new("/ebin/my_app.app"))
        .expect("read .app");
    insta::assert_snapshot!(app);
}

#[test]
fn app_file_escapes_descriptions() {
    let fs = InMemoryFileSystem::new();
    let mut config = PackageConfig::default();
    config.name = "my_app".into();
    config.description = "This is a quote \" and a slash \\".into();
    let codegen_config = ErlangAppCodegenConfiguration {
        include_dev_deps: false,
        package_name_overrides: HashMap::new(),
    };

    ErlangApp::new(Utf8Path::new("/ebin"), &codegen_config)
        .render(
            fs.clone(),
            &config,
            vec!["my_app@one".into(), "my_app@two".into()],
        )
        .expect("render .app");

    let app = fs
        .read(Utf8Path::new("/ebin/my_app.app"))
        .expect("read .app");
    insta::assert_snapshot!(app);
}
