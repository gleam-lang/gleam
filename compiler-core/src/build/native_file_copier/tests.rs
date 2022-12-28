use super::NativeFileCopier;
use crate::{
    build::native_file_copier::CopiedNativeFiles,
    io::{memory::InMemoryFileSystem, FileSystemWriter},
};
use lazy_static::lazy_static;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

lazy_static! {
    static ref ROOT: PathBuf = PathBuf::from("/");
    static ref OUT: PathBuf = PathBuf::from("/out");
}

#[test]
fn javascript_files_are_copied_from_src() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/src/wibble.js"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/src/wibble.js"), "1".into()),
            (PathBuf::from("/out/wibble.js"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn javascript_files_are_copied_from_test() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/test/wibble.js"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/test/wibble.js"), "1".into()),
            (PathBuf::from("/out/wibble.js"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn mjavascript_files_are_copied_from_src() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/src/wibble.mjs"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/src/wibble.mjs"), "1".into()),
            (PathBuf::from("/out/wibble.mjs"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn mjavascript_files_are_copied_from_test() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/test/wibble.mjs"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/test/wibble.mjs"), "1".into()),
            (PathBuf::from("/out/wibble.mjs"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn typescript_files_are_copied_from_src() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/src/wibble.ts"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/src/wibble.ts"), "1".into()),
            (PathBuf::from("/out/wibble.ts"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn typescript_files_are_copied_from_test() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/test/wibble.ts"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/test/wibble.ts"), "1".into()),
            (PathBuf::from("/out/wibble.ts"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn erlang_header_files_are_copied_from_src() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/src/wibble.hrl"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/src/wibble.hrl"), "1".into()),
            (PathBuf::from("/out/wibble.hrl"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn erlang_header_files_are_copied_from_test() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/test/wibble.hrl"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/test/wibble.hrl"), "1".into()),
            (PathBuf::from("/out/wibble.hrl"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn erlang_files_are_copied_from_src() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/src/wibble.erl"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert_eq!(copied.to_compile, vec![PathBuf::from("wibble.erl")]);
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/src/wibble.erl"), "1".into()),
            (PathBuf::from("/out/wibble.erl"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn erlang_files_are_copied_from_test() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/test/wibble.erl"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert_eq!(copied.to_compile, vec![PathBuf::from("wibble.erl")]);
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/test/wibble.erl"), "1".into()),
            (PathBuf::from("/out/wibble.erl"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn elixir_files_are_copied_from_src() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/src/wibble.ex"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(copied.any_elixir);
    assert_eq!(copied.to_compile, vec![PathBuf::from("wibble.ex")]);
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/src/wibble.ex"), "1".into()),
            (PathBuf::from("/out/wibble.ex"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn elixir_files_are_copied_from_test() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/test/wibble.ex"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(copied.any_elixir);
    assert_eq!(copied.to_compile, vec![PathBuf::from("wibble.ex")]);
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/test/wibble.ex"), "1".into()),
            (PathBuf::from("/out/wibble.ex"), "1".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn other_files_are_ignored() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/src/wibble.cpp"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([(PathBuf::from("/src/wibble.cpp"), "1".into())]),
        fs.into_contents(),
    );
}

#[test]
fn files_do_not_get_copied_if_there_already_is_a_new_version() {
    let fs = InMemoryFileSystem::new();
    let out = Path::new("/out/wibble.mjs");
    let src = Path::new("/src/wibble.mjs");
    fs.write(&out, "in-out").unwrap();
    fs.write(&src, "in-src").unwrap();
    fs.set_modification_time(&out, UNIX_EPOCH + Duration::from_secs(1));
    fs.set_modification_time(&src, UNIX_EPOCH);

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/src/wibble.mjs"), "in-src".into()),
            (PathBuf::from("/out/wibble.mjs"), "in-out".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn files_get_copied_if_the_previously_copied_vesion_is_older() {
    let fs = InMemoryFileSystem::new();
    let out = Path::new("/out/wibble.mjs");
    let src = Path::new("/src/wibble.mjs");
    fs.write(&out, "in-out").unwrap();
    fs.write(&src, "in-src").unwrap();
    fs.set_modification_time(&out, UNIX_EPOCH);
    fs.set_modification_time(&src, UNIX_EPOCH + Duration::from_secs(1));

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    let copied = copier.run().unwrap();

    assert!(!copied.any_elixir);
    assert!(copied.to_compile.is_empty());
    assert_eq!(
        HashMap::from([
            (PathBuf::from("/src/wibble.mjs"), "in-src".into()),
            (PathBuf::from("/out/wibble.mjs"), "in-src".into())
        ]),
        fs.into_contents(),
    );
}

#[test]
fn duplicate_native_files_result_in_an_error() {
    let fs = InMemoryFileSystem::new();
    fs.write(&Path::new("/src/wibble.mjs"), "1").unwrap();
    fs.write(&Path::new("/test/wibble.mjs"), "1").unwrap();

    let copier = NativeFileCopier::new(fs.clone(), &ROOT, &OUT);
    assert!(copier.run().is_err());
}
