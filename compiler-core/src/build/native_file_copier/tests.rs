use super::NativeFileCopier;
use crate::{
    build::native_file_copier::CopiedNativeFiles,
    io::{memory::InMemoryFileSystem, FileSystemWriter},
};
use std::{
    collections::HashMap,
    sync::OnceLock,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use camino::{Utf8Path, Utf8PathBuf};

fn root() -> &'static Utf8PathBuf {
    static ROOT: OnceLock<Utf8PathBuf> = OnceLock::new();
    ROOT.get_or_init(|| Utf8PathBuf::from("/").to_owned())
}

fn root_out() -> &'static Utf8PathBuf {
    static OUT: OnceLock<Utf8PathBuf> = OnceLock::new();
    OUT.get_or_init(|| Utf8PathBuf::from("/out"))
}

#[test]
fn javascript_files_are_copied_from_src() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/src/wibble.js"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/src/wibble.js"), "1".into()),
            (Utf8PathBuf::from("/out/wibble.js"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn javascript_files_are_copied_from_test() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/test/wibble.js"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/test/wibble.js"), "1".into()),
            (Utf8PathBuf::from("/out/wibble.js"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn mjavascript_files_are_copied_from_src() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/src/wibble.mjs"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/src/wibble.mjs"), "1".into()),
            (Utf8PathBuf::from("/out/wibble.mjs"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn mjavascript_files_are_copied_from_test() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/test/wibble.mjs"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/test/wibble.mjs"), "1".into()),
            (Utf8PathBuf::from("/out/wibble.mjs"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn typescript_files_are_copied_from_src() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/src/wibble.ts"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/src/wibble.ts"), "1".into()),
            (Utf8PathBuf::from("/out/wibble.ts"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn typescript_files_are_copied_from_test() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/test/wibble.ts"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/test/wibble.ts"), "1".into()),
            (Utf8PathBuf::from("/out/wibble.ts"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn all_javascript_files_are_copied_from_src_subfolders() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/src/abc/def/wibble.mjs"), "1")
        .unwrap();
    fs.write(&Utf8Path::new("/src/abc/ghi/wibble.js"), "2")
        .unwrap();
    fs.write(&Utf8Path::new("/src/def/wobble.ts"), "3").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/src/abc/def/wibble.mjs"), "1".into()),
            (Utf8PathBuf::from("/out/abc/def/wibble.mjs"), "1".into()),
            (Utf8PathBuf::from("/src/abc/ghi/wibble.js"), "2".into()),
            (Utf8PathBuf::from("/out/abc/ghi/wibble.js"), "2".into()),
            (Utf8PathBuf::from("/src/def/wobble.ts"), "3".into()),
            (Utf8PathBuf::from("/out/def/wobble.ts"), "3".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn all_javascript_files_are_copied_from_test_subfolders() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/test/abc/def/wibble.mjs"), "1")
        .unwrap();
    fs.write(&Utf8Path::new("/test/abc/ghi/wibble.js"), "2")
        .unwrap();
    fs.write(&Utf8Path::new("/test/def/wobble.ts"), "3")
        .unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/test/abc/def/wibble.mjs"), "1".into()),
            (Utf8PathBuf::from("/out/abc/def/wibble.mjs"), "1".into()),
            (Utf8PathBuf::from("/test/abc/ghi/wibble.js"), "2".into()),
            (Utf8PathBuf::from("/out/abc/ghi/wibble.js"), "2".into()),
            (Utf8PathBuf::from("/test/def/wobble.ts"), "3".into()),
            (Utf8PathBuf::from("/out/def/wobble.ts"), "3".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn erlang_header_files_are_copied_from_src() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/src/wibble.hrl"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/src/wibble.hrl"), "1".into()),
            (Utf8PathBuf::from("/out/wibble.hrl"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn erlang_header_files_are_copied_from_test() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/test/wibble.hrl"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/test/wibble.hrl"), "1".into()),
            (Utf8PathBuf::from("/out/wibble.hrl"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn erlang_files_are_copied_from_src() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/src/wibble.erl"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert_eq!(copied.to_compile, vec![Utf8PathBuf::from("wibble.erl")]);
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/src/wibble.erl"), "1".into()),
            (Utf8PathBuf::from("/out/wibble.erl"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn erlang_files_are_copied_from_test() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/test/wibble.erl"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert_eq!(copied.to_compile, vec![Utf8PathBuf::from("wibble.erl")]);
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/test/wibble.erl"), "1".into()),
            (Utf8PathBuf::from("/out/wibble.erl"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn elixir_files_are_copied_from_src() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/src/wibble.ex"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(copied.any_elixir);
    assert_eq!(copied.to_compile, vec![Utf8PathBuf::from("wibble.ex")]);
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/src/wibble.ex"), "1".into()),
            (Utf8PathBuf::from("/out/wibble.ex"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn elixir_files_are_copied_from_test() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/test/wibble.ex"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(copied.any_elixir);
    assert_eq!(copied.to_compile, vec![Utf8PathBuf::from("wibble.ex")]);
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/test/wibble.ex"), "1".into()),
            (Utf8PathBuf::from("/out/wibble.ex"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn other_files_are_ignored() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/src/wibble.cpp"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([(Utf8PathBuf::from("/src/wibble.cpp"), "1".into())]),
        fs.into_contents(),
    );
}

#[test]
fn files_do_not_get_copied_if_there_already_is_a_new_version() {
    let fs = InMemoryFileSystem::new();
    let out = Utf8Path::new("/out/wibble.mjs");
    let src = Utf8Path::new("/src/wibble.mjs");
    fs.write(&out, "in-out").unwrap();
    fs.write(&src, "in-src").unwrap();
    fs.set_modification_time(&out, UNIX_EPOCH + Duration::from_secs(1));
    fs.set_modification_time(&src, UNIX_EPOCH);

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/src/wibble.mjs"), "in-src".into()),
            (Utf8PathBuf::from("/out/wibble.mjs"), "in-out".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn files_get_copied_if_the_previously_copied_version_is_older() {
    let fs = InMemoryFileSystem::new();
    let out = Utf8Path::new("/out/wibble.mjs");
    let src = Utf8Path::new("/src/wibble.mjs");
    fs.write(&out, "in-out").unwrap();
    fs.write(&src, "in-src").unwrap();
    fs.set_modification_time(&out, UNIX_EPOCH);
    fs.set_modification_time(&src, UNIX_EPOCH + Duration::from_secs(1));

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (Utf8PathBuf::from("/src/wibble.mjs"), "in-src".into()),
            (Utf8PathBuf::from("/out/wibble.mjs"), "in-src".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn duplicate_native_files_result_in_an_error() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/src/wibble.mjs"), "1").unwrap();
    fs.write(&Utf8Path::new("/test/wibble.mjs"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    assert!(copier.run().is_err());
}

#[test]
fn conflicting_erlang_modules_result_in_an_error() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/src/a/b/c/wibble.erl"), "1")
        .unwrap();
    fs.write(&Utf8Path::new("/src/e/f/wibble.erl"), "1")
        .unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    assert!(copier.run().is_err());
}

#[test]
fn conflicting_gleam_and_javascript_modules_result_in_an_error() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/src/wibble.gleam"), "1").unwrap();
    fs.write(&Utf8Path::new("/src/wibble.mjs"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    assert!(copier.run().is_err());
}

#[test]
fn differently_nested_gleam_and_javascript_modules_with_same_name_are_ok() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Utf8Path::new("/src/a/b/c/wibble.gleam"), "1")
        .unwrap();
    fs.write(&Utf8Path::new("/src/d/e/wibble.mjs"), "1")
        .unwrap();

    let copier = NativeFileCopier::new(fs.clone(), root(), root_out());
    assert!(copier.run().is_ok());
}
