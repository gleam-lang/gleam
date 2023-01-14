use super::*;
use crate::io::{memory::InMemoryFileSystem, FileSystemWriter};
use std::time::Duration;

#[test]
fn no_cache_present() {
    let name = "package".into();
    let src = Path::new("/src");
    let artefact = Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let loader = make_loader(&name, &fs, src, artefact);

    fs.write(&Path::new("/src/main.gleam"), "const x = 1")
        .unwrap();

    let result = loader
        .load(Path::new("/src/main.gleam").to_path_buf())
        .unwrap();

    assert!(result.is_new());
}

#[test]
fn cache_present_and_fresh() {
    let name = "package".into();
    let src = Path::new("/src");
    let artefact = Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let loader = make_loader(&name, &fs, src, artefact);

    // The mtime of the source is older than that of the cache
    write_src(&fs, "/src/main.gleam", 0);
    write_cache(&fs, "/artefact/main.cache_meta", 1, false);

    let result = loader
        .load(Path::new("/src/main.gleam").to_path_buf())
        .unwrap();

    assert!(result.is_cached());
}

#[test]
fn cache_present_and_stale() {
    let name = "package".into();
    let src = Path::new("/src");
    let artefact = Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let loader = make_loader(&name, &fs, src, artefact);

    // The mtime of the source is newer than that of the cache
    write_src(&fs, "/src/main.gleam", 2);
    write_cache(&fs, "/artefact/main.cache_meta", 1, false);

    let result = loader
        .load(Path::new("/src/main.gleam").to_path_buf())
        .unwrap();

    assert!(result.is_new());
}

#[test]
fn cache_present_without_codegen_when_required() {
    let name = "package".into();
    let src = Path::new("/src");
    let artefact = Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let mut loader = make_loader(&name, &fs, src, artefact);
    loader.codegen = CodegenRequired::Yes;

    // The mtime of the cache is newer than that of the source
    write_src(&fs, "/src/main.gleam", 0);
    write_cache(&fs, "/artefact/main.cache_meta", 1, false);

    let result = loader
        .load(Path::new("/src/main.gleam").to_path_buf())
        .unwrap();

    assert!(result.is_new());
}

#[test]
fn cache_present_with_codegen_when_required() {
    let name = "package".into();
    let src = Path::new("/src");
    let artefact = Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let mut loader = make_loader(&name, &fs, src, artefact);
    loader.codegen = CodegenRequired::Yes;

    // The mtime of the cache is newer than that of the source
    write_src(&fs, "/src/main.gleam", 0);
    write_cache(&fs, "/artefact/main.cache_meta", 1, true);

    let result = loader
        .load(Path::new("/src/main.gleam").to_path_buf())
        .unwrap();

    assert!(result.is_cached());
}

#[test]
fn cache_present_without_codegen_when_not_required() {
    let name = "package".into();
    let src = Path::new("/src");
    let artefact = Path::new("/artefact");
    let fs = InMemoryFileSystem::new();
    let mut loader = make_loader(&name, &fs, src, artefact);
    loader.codegen = CodegenRequired::No;

    // The mtime of the cache is newer than that of the source
    write_src(&fs, "/src/main.gleam", 0);
    write_cache(&fs, "/artefact/main.cache_meta", 1, false);

    let result = loader
        .load(Path::new("/src/main.gleam").to_path_buf())
        .unwrap();

    assert!(result.is_cached());
}

fn write_cache(fs: &InMemoryFileSystem, path: &str, seconds: u64, codegen_performed: bool) {
    let path = Path::new(path);
    let cache_metadata = CacheMetadata {
        mtime: SystemTime::UNIX_EPOCH + Duration::from_secs(seconds),
        codegen_performed,
        dependencies: vec![],
    };
    fs.write_bytes(&path, &cache_metadata.to_binary()).unwrap();
}

fn write_src(fs: &InMemoryFileSystem, path: &str, seconds: u64) {
    let path = Path::new(path);
    fs.write(&path, "const x = 1").unwrap();
    fs.set_modification_time(&path, SystemTime::UNIX_EPOCH + Duration::from_secs(seconds));
}

fn make_loader<'a>(
    package_name: &'a SmolStr,
    fs: &InMemoryFileSystem,
    src: &'a Path,
    artefact: &'a Path,
) -> ModuleLoader<'a, InMemoryFileSystem> {
    ModuleLoader {
        io: fs.clone(),
        mode: Mode::Dev,
        target: Target::Erlang,
        codegen: CodegenRequired::No,
        package_name,
        source_directory: &src,
        artefact_directory: &artefact,
        origin: Origin::Src,
    }
}
