use super::*;
use crate::{
    build::SourceFingerprint,
    io::{FileSystemWriter, memory::InMemoryFileSystem},
    line_numbers::LineNumbers,
};
use std::time::Duration;

#[test]
fn no_cache_present() {
    let name = "package".into();
    let artefact = Utf8Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let warnings = WarningEmitter::null();
    let incomplete_modules = HashSet::new();
    let loader = make_loader(&warnings, &name, &fs, artefact, &incomplete_modules);

    fs.write(&Utf8Path::new("/src/main.gleam"), "const x = 1")
        .unwrap();

    let file = GleamFile::new("/src".into(), "/src/main.gleam".into());
    let result = loader.load(file).unwrap();

    assert!(result.is_new());
}

#[test]
fn cache_present_and_fresh() {
    let name = "package".into();
    let artefact = Utf8Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let warnings = WarningEmitter::null();
    let incomplete_modules = HashSet::new();
    let loader = make_loader(&warnings, &name, &fs, artefact, &incomplete_modules);

    // The mtime of the source is older than that of the cache
    write_src(&fs, TEST_SOURCE_1, "/src/main.gleam", 0);
    write_cache(&fs, TEST_SOURCE_1, "/artefact/main.cache_meta", 1, false);

    let file = GleamFile::new("/src".into(), "/src/main.gleam".into());
    let result = loader.load(file).unwrap();

    assert!(result.is_cached());
}

#[test]
fn cache_present_and_stale() {
    let name = "package".into();
    let artefact = Utf8Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let warnings = WarningEmitter::null();
    let incomplete_modules = HashSet::new();
    let loader = make_loader(&warnings, &name, &fs, artefact, &incomplete_modules);

    // The mtime of the source is newer than that of the cache
    write_src(&fs, TEST_SOURCE_2, "/src/main.gleam", 2);
    write_cache(&fs, TEST_SOURCE_1, "/artefact/main.cache_meta", 1, false);

    let file = GleamFile::new("/src".into(), "/src/main.gleam".into());
    let result = loader.load(file).unwrap();

    assert!(result.is_new());
}

#[test]
fn cache_present_and_stale_but_source_is_the_same() {
    let name = "package".into();
    let artefact = Utf8Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let warnings = WarningEmitter::null();
    let incomplete_modules = HashSet::new();
    let loader = make_loader(&warnings, &name, &fs, artefact, &incomplete_modules);

    // The mtime of the source is newer than that of the cache
    write_src(&fs, TEST_SOURCE_1, "/src/main.gleam", 2);
    write_cache(&fs, TEST_SOURCE_1, "/artefact/main.cache_meta", 1, false);

    let file = GleamFile::new("/src".into(), "/src/main.gleam".into());
    let result = loader.load(file).unwrap();

    assert!(result.is_cached());
}

#[test]
fn cache_present_and_stale_source_is_the_same_lsp_mode() {
    let name = "package".into();
    let artefact = Utf8Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let warnings = WarningEmitter::null();
    let incomplete_modules = HashSet::new();
    let mut loader = make_loader(&warnings, &name, &fs, artefact, &incomplete_modules);
    loader.mode = Mode::Lsp;

    // The mtime of the source is newer than that of the cache
    write_src(&fs, TEST_SOURCE_1, "/src/main.gleam", 2);
    write_cache(&fs, TEST_SOURCE_1, "/artefact/main.cache_meta", 1, false);

    let file = GleamFile::new("/src".into(), "/src/main.gleam".into());
    let result = loader.load(file).unwrap();

    assert!(result.is_cached());
}

#[test]
fn cache_present_and_stale_source_is_the_same_lsp_mode_and_invalidated() {
    let name = "package".into();
    let artefact = Utf8Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let warnings = WarningEmitter::null();
    let mut incomplete_modules = HashSet::new();
    let _ = incomplete_modules.insert("main".into());
    let mut loader = make_loader(&warnings, &name, &fs, artefact, &incomplete_modules);
    loader.mode = Mode::Lsp;

    // The mtime of the source is newer than that of the cache
    write_src(&fs, TEST_SOURCE_1, "/src/main.gleam", 2);
    write_cache(&fs, TEST_SOURCE_1, "/artefact/main.cache_meta", 1, false);

    let file = GleamFile::new("/src".into(), "/src/main.gleam".into());
    let result = loader.load(file).unwrap();

    assert!(result.is_new());
}

#[test]
fn cache_present_without_codegen_when_required() {
    let name = "package".into();
    let artefact = Utf8Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let warnings = WarningEmitter::null();
    let incomplete_modules = HashSet::new();
    let mut loader = make_loader(&warnings, &name, &fs, artefact, &incomplete_modules);
    loader.codegen = CodegenRequired::Yes;

    // The mtime of the cache is newer than that of the source
    write_src(&fs, TEST_SOURCE_1, "/src/main.gleam", 0);
    write_cache(&fs, TEST_SOURCE_1, "/artefact/main.cache_meta", 1, false);

    let file = GleamFile::new("/src".into(), "/src/main.gleam".into());
    let result = loader.load(file).unwrap();

    assert!(result.is_new());
}

#[test]
fn cache_present_with_codegen_when_required() {
    let name = "package".into();
    let artefact = Utf8Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let warnings = WarningEmitter::null();
    let incomplete_modules = HashSet::new();
    let mut loader = make_loader(&warnings, &name, &fs, artefact, &incomplete_modules);
    loader.codegen = CodegenRequired::Yes;

    // The mtime of the cache is newer than that of the source
    write_src(&fs, TEST_SOURCE_1, "/src/main.gleam", 0);
    write_cache(&fs, TEST_SOURCE_1, "/artefact/main.cache_meta", 1, true);

    let file = GleamFile::new("/src".into(), "/src/main.gleam".into());
    let result = loader.load(file).unwrap();

    assert!(result.is_cached());
}

#[test]
fn cache_present_without_codegen_when_not_required() {
    let name = "package".into();
    let artefact = Utf8Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let warnings = WarningEmitter::null();
    let incomplete_modules = HashSet::new();
    let mut loader = make_loader(&warnings, &name, &fs, artefact, &incomplete_modules);
    loader.codegen = CodegenRequired::No;

    // The mtime of the cache is newer than that of the source
    write_src(&fs, TEST_SOURCE_1, "/src/main.gleam", 0);
    write_cache(&fs, TEST_SOURCE_1, "/artefact/main.cache_meta", 1, false);

    let file = GleamFile::new("/src".into(), "/src/main.gleam".into());
    let result = loader.load(file).unwrap();

    assert!(result.is_cached());
}

const TEST_SOURCE_1: &'static str = "const x = 1";
const TEST_SOURCE_2: &'static str = "const x = 2";

fn write_cache(
    fs: &InMemoryFileSystem,
    source: &str,
    path: &str,
    seconds: u64,
    codegen_performed: bool,
) {
    let line_numbers = LineNumbers::new(source);
    let cache_metadata = CacheMetadata {
        mtime: SystemTime::UNIX_EPOCH + Duration::from_secs(seconds),
        codegen_performed,
        dependencies: vec![],
        fingerprint: SourceFingerprint::new(source),
        line_numbers,
    };
    let path = Utf8Path::new(path);
    fs.write_bytes(&path, &cache_metadata.to_binary()).unwrap();
}

fn write_src(fs: &InMemoryFileSystem, source: &str, path: &str, seconds: u64) {
    let path = Utf8Path::new(path);
    fs.write(&path, source).unwrap();
    fs.set_modification_time(&path, SystemTime::UNIX_EPOCH + Duration::from_secs(seconds));
}

fn make_loader<'a>(
    warnings: &'a WarningEmitter,
    package_name: &'a EcoString,
    fs: &InMemoryFileSystem,
    artefact: &'a Utf8Path,
    incomplete_modules: &'a HashSet<EcoString>,
) -> ModuleLoader<'a, InMemoryFileSystem> {
    ModuleLoader {
        warnings,
        io: fs.clone(),
        mode: Mode::Dev,
        target: Target::Erlang,
        codegen: CodegenRequired::No,
        package_name,
        artefact_directory: &artefact,
        origin: Origin::Src,
        incomplete_modules,
    }
}
