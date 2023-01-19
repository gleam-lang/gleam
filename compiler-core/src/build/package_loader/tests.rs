use smol_str::SmolStr;

use super::*;
use crate::{
    io::{memory::InMemoryFileSystem, FileSystemWriter},
    parse::extra::ModuleExtra,
};
use std::time::Duration;

#[derive(Debug)]
struct LoaderTestOutput {
    to_compile: Vec<SmolStr>,
    cached: Vec<SmolStr>,
}

fn write_src(fs: &InMemoryFileSystem, path: &str, seconds: u64, src: &str) {
    let path = Path::new(path);
    fs.write(&path, src).unwrap();
    fs.set_modification_time(&path, SystemTime::UNIX_EPOCH + Duration::from_secs(seconds));
}

fn write_cache(fs: &InMemoryFileSystem, name: &str, seconds: u64, deps: Vec<SmolStr>) {
    let mtime = SystemTime::UNIX_EPOCH + Duration::from_secs(seconds);
    let cache_metadata = CacheMetadata {
        mtime,
        codegen_performed: true,
        dependencies: deps,
    };
    let path = Path::new("/artefact").join(format!("{name}.cache_meta"));
    fs.write_bytes(&path, &cache_metadata.to_binary()).unwrap();

    let cache = crate::type_::Module {
        name: name.into(),
        origin: Origin::Src,
        package: "my_package".into(),
        types: Default::default(),
        types_constructors: Default::default(),
        values: Default::default(),
        accessors: Default::default(),
    };
    let path = Path::new("/artefact").join(format!("{name}.cache"));
    fs.write_bytes(
        &path,
        &metadata::ModuleEncoder::new(&cache).encode().unwrap(),
    )
    .unwrap();
}

fn run_loader(fs: InMemoryFileSystem, root: &Path, artefact: &Path) -> LoaderTestOutput {
    let mut defined = im::HashMap::new();
    let ids = UniqueIdGenerator::new();
    let loader = PackageLoader {
        io: fs.clone(),
        ids,
        mode: Mode::Dev,
        root: &root,
        codegen: CodegenRequired::Yes,
        artefact_directory: &artefact,
        package_name: &"my_package".into(),
        target: Target::JavaScript,
        already_defined_modules: &mut defined,
    };
    let loaded = loader.run().unwrap();

    LoaderTestOutput {
        to_compile: loaded.to_compile.into_iter().map(|m| m.name).collect(),
        cached: loaded.cached.into_iter().map(|m| m.name).collect(),
    }
}

#[test]
fn no_modules() {
    let fs = InMemoryFileSystem::new();
    let root = Path::new("/");
    let artefact = Path::new("/artefact");

    let loaded = run_loader(fs, root, artefact);
    assert!(loaded.to_compile.is_empty());
    assert!(loaded.cached.is_empty());
}

#[test]
fn one_src_module() {
    let fs = InMemoryFileSystem::new();
    let root = Path::new("/");
    let artefact = Path::new("/artefact");

    write_src(&fs, "/src/main.gleam", 0, "const x = 1");

    let loaded = run_loader(fs, root, artefact);
    assert_eq!(loaded.to_compile, vec![SmolStr::new("main")]);
    assert!(loaded.cached.is_empty());
}

#[test]
fn one_test_module() {
    let fs = InMemoryFileSystem::new();
    let root = Path::new("/");
    let artefact = Path::new("/artefact");

    write_src(&fs, "/test/main.gleam", 0, "const x = 1");

    let loaded = run_loader(fs, root, artefact);
    assert_eq!(loaded.to_compile, vec![SmolStr::new("main")]);
    assert!(loaded.cached.is_empty());
}

#[test]
fn importing() {
    let fs = InMemoryFileSystem::new();
    let root = Path::new("/");
    let artefact = Path::new("/artefact");

    write_src(&fs, "/src/three.gleam", 0, "import two");
    write_src(&fs, "/src/one.gleam", 0, "");
    write_src(&fs, "/src/two.gleam", 0, "import one");

    let loaded = run_loader(fs, root, artefact);
    assert_eq!(
        loaded.to_compile,
        vec![
            SmolStr::new("one"),
            SmolStr::new("two"),
            SmolStr::new("three")
        ]
    );
    assert!(loaded.cached.is_empty());
}

#[test]
fn reading_cache() {
    let fs = InMemoryFileSystem::new();
    let root = Path::new("/");
    let artefact = Path::new("/artefact");

    write_src(&fs, "/src/one.gleam", 0, "");
    write_cache(&fs, "one", 0, vec![]);

    let loaded = run_loader(fs, root, artefact);
    assert!(loaded.to_compile.is_empty());
    assert_eq!(loaded.cached, vec![SmolStr::new("one")]);
}

#[test]
fn module_is_stale_if_cache_older() {
    let fs = InMemoryFileSystem::new();
    let root = Path::new("/");
    let artefact = Path::new("/artefact");

    write_src(&fs, "/src/one.gleam", 1, "");
    write_cache(&fs, "one", 0, vec![]);

    let loaded = run_loader(fs, root, artefact);
    assert_eq!(loaded.to_compile, vec![SmolStr::new("one")]);
    assert!(loaded.cached.is_empty());
}

#[test]
fn module_is_stale_if_deps_are_stale() {
    let fs = InMemoryFileSystem::new();
    let root = Path::new("/");
    let artefact = Path::new("/artefact");

    // Cache is stale
    write_src(&fs, "/src/one.gleam", 1, "");
    write_cache(&fs, "one", 0, vec![]);

    // Cache is fresh but dep is stale
    write_src(&fs, "/src/two.gleam", 1, "import one");
    write_cache(&fs, "two", 2, vec![SmolStr::new("one")]);

    // Cache is fresh
    write_src(&fs, "/src/three.gleam", 1, "");
    write_cache(&fs, "three", 2, vec![]);

    let loaded = run_loader(fs, root, artefact);
    assert_eq!(
        loaded.to_compile,
        vec![SmolStr::new("one"), SmolStr::new("two")]
    );
    assert_eq!(loaded.cached, vec![SmolStr::new("three")]);
}
