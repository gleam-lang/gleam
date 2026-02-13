use ecow::{EcoString, eco_format};
use hexpm::version::Version;

use super::*;
use crate::{
    Warning,
    build::SourceFingerprint,
    io::{FileSystemWriter, memory::InMemoryFileSystem},
    line_numbers,
    parse::extra::ModuleExtra,
    warning::NullWarningEmitterIO,
};

use std::time::Duration;

#[derive(Debug)]
struct LoaderTestOutput {
    to_compile: Vec<EcoString>,
    cached: Vec<EcoString>,
    warnings: Vec<Warning>,
}

const TEST_SOURCE_1: &'static str = "const x = 1";
const TEST_SOURCE_2: &'static str = "const x = 2";

fn write_src(fs: &InMemoryFileSystem, path: &str, seconds: u64, src: &str) {
    let path = Utf8Path::new(path);
    fs.write(&path, src).unwrap();
    fs.set_modification_time(&path, SystemTime::UNIX_EPOCH + Duration::from_secs(seconds));
}

fn write_cache(
    fs: &InMemoryFileSystem,
    name: &str,
    seconds: u64,
    deps: Vec<(EcoString, SrcSpan)>,
    src: &str,
) {
    let line_numbers = line_numbers::LineNumbers::new(src);
    let mtime = SystemTime::UNIX_EPOCH + Duration::from_secs(seconds);
    let cache_metadata = CacheMetadata {
        mtime,
        codegen_performed: true,
        dependencies: deps,
        fingerprint: SourceFingerprint::new(src),
        line_numbers: line_numbers.clone(),
    };

    let artefact_name = name.replace("/", "@");
    let path = Utf8Path::new("/artefact").join(format!("{artefact_name}.cache_meta"));
    fs.write_bytes(&path, &cache_metadata.to_binary()).unwrap();

    let cache = crate::type_::ModuleInterface {
        name: name.into(),
        origin: Origin::Src,
        package: "my_package".into(),
        types: Default::default(),
        types_value_constructors: Default::default(),
        values: Default::default(),
        accessors: Default::default(),
        line_numbers: line_numbers.clone(),
        is_internal: false,
        src_path: Utf8PathBuf::from(format!("/src/{}.gleam", name)),
        warnings: vec![],
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: Default::default(),
        documentation: Default::default(),
        contains_echo: false,
        references: Default::default(),
        inline_functions: Default::default(),
    };
    let path = Utf8Path::new("/artefact").join(format!("{artefact_name}.cache"));
    fs.write_bytes(
        &path,
        &metadata::ModuleEncoder::new(&cache).encode().unwrap(),
    )
    .unwrap();
}

fn run_loader(fs: InMemoryFileSystem, root: &Utf8Path, artefact: &Utf8Path) -> LoaderTestOutput {
    let mut defined = im::HashMap::new();
    let ids = UniqueIdGenerator::new();
    let (emitter, warnings) = WarningEmitter::vector();

    let loader = PackageLoader {
        io: fs.clone(),
        ids,
        mode: Mode::Dev,
        paths: ProjectPaths::new(root.into()),
        warnings: &emitter,
        codegen: CodegenRequired::Yes,
        artefact_directory: &artefact,
        package_name: &"my_package".into(),
        target: Target::JavaScript,
        stale_modules: &mut StaleTracker::default(),
        already_defined_modules: &mut defined,
        incomplete_modules: &mut HashSet::new(),
        cached_warnings: CachedWarnings::Ignore,
    };
    let loaded = loader.run().unwrap();

    LoaderTestOutput {
        to_compile: loaded.to_compile.into_iter().map(|m| m.name).collect(),
        cached: loaded.cached.into_iter().map(|m| m.name).collect(),
        warnings: warnings.take(),
    }
}

#[test]
fn no_modules() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    let loaded = run_loader(fs, root, artefact);
    assert!(loaded.to_compile.is_empty());
    assert!(loaded.cached.is_empty());
}

#[test]
fn one_src_module() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    write_src(&fs, "/src/main.gleam", 0, "const x = 1");

    let loaded = run_loader(fs, root, artefact);
    assert_eq!(loaded.to_compile, vec![EcoString::from("main")]);
    assert!(loaded.cached.is_empty());
}

#[test]
fn one_test_module() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    write_src(&fs, "/test/main.gleam", 0, "const x = 1");

    let loaded = run_loader(fs, root, artefact);
    assert_eq!(loaded.to_compile, vec![EcoString::from("main")]);
    assert!(loaded.cached.is_empty());
}

#[test]
fn one_dev_module() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    write_src(&fs, "/dev/main.gleam", 0, "const x = 1");

    let loaded = run_loader(fs, root, artefact);
    assert_eq!(loaded.to_compile, vec![EcoString::from("main")]);
    assert!(loaded.cached.is_empty());
}

#[test]
fn importing() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    write_src(&fs, "/src/three.gleam", 0, "import two");
    write_src(&fs, "/src/one.gleam", 0, "");
    write_src(&fs, "/src/two.gleam", 0, "import one");

    let loaded = run_loader(fs, root, artefact);
    assert_eq!(
        loaded.to_compile,
        vec![
            EcoString::from("one"),
            EcoString::from("two"),
            EcoString::from("three")
        ]
    );
    assert!(loaded.cached.is_empty());
}

#[test]
fn reading_cache() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    write_src(&fs, "/src/one.gleam", 0, TEST_SOURCE_1);
    write_cache(&fs, "one", 0, vec![], TEST_SOURCE_1);

    let loaded = run_loader(fs, root, artefact);
    assert!(loaded.to_compile.is_empty());
    assert_eq!(loaded.cached, vec![EcoString::from("one")]);
}

#[test]
fn module_is_stale_if_cache_older() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    write_src(&fs, "/src/one.gleam", 1, TEST_SOURCE_2);
    write_cache(&fs, "one", 0, vec![], TEST_SOURCE_1);

    let loaded = run_loader(fs, root, artefact);
    assert_eq!(loaded.to_compile, vec![EcoString::from("one")]);
    assert!(loaded.cached.is_empty());
}

#[test]
fn module_is_stale_if_deps_are_stale() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    // Cache is stale
    write_src(&fs, "/src/one.gleam", 1, TEST_SOURCE_2);
    write_cache(&fs, "one", 0, vec![], TEST_SOURCE_1);

    // Cache is fresh but dep is stale
    write_src(&fs, "/src/two.gleam", 1, "import one");
    write_cache(
        &fs,
        "two",
        2,
        vec![(EcoString::from("one"), SrcSpan { start: 0, end: 0 })],
        "import one",
    );

    // Cache is fresh
    write_src(&fs, "/src/three.gleam", 1, TEST_SOURCE_1);
    write_cache(&fs, "three", 2, vec![], TEST_SOURCE_1);

    let loaded = run_loader(fs, root, artefact);
    assert_eq!(
        loaded.to_compile,
        vec![EcoString::from("one"), EcoString::from("two")]
    );
    assert_eq!(loaded.cached, vec![EcoString::from("three")]);
}

#[test]
fn module_is_stale_if_deps_removed() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    // Source is removed, cache is present
    write_cache(&fs, "nested/one", 0, vec![], TEST_SOURCE_1);

    // Cache is fresh but dep is removed
    write_src(&fs, "/src/two.gleam", 1, "import one");
    write_cache(
        &fs,
        "two",
        2,
        vec![(EcoString::from("nested/one"), SrcSpan { start: 0, end: 0 })],
        "import nested/one",
    );

    let loaded = run_loader(fs, root, artefact);
    assert_eq!(loaded.to_compile, vec![EcoString::from("two")]);
}

#[test]
fn module_continues_to_be_stale_if_deps_get_updated() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    // Cache is stale
    write_src(&fs, "/src/one.gleam", 1, TEST_SOURCE_2);
    write_cache(&fs, "one", 0, vec![], TEST_SOURCE_1);

    // Cache is fresh but dep is stale
    write_src(&fs, "/src/two.gleam", 1, "import one");
    write_cache(
        &fs,
        "two",
        2,
        vec![(EcoString::from("one"), SrcSpan { start: 0, end: 0 })],
        "import one",
    );

    // Cache is fresh
    write_src(&fs, "/src/three.gleam", 1, TEST_SOURCE_1);
    write_cache(&fs, "three", 2, vec![], TEST_SOURCE_1);

    let _loaded1 = run_loader(fs.clone(), root, artefact);

    // update the dependency
    write_cache(&fs, "one", 3, vec![], TEST_SOURCE_2);
    let loaded2 = run_loader(fs, root, artefact);

    assert_eq!(loaded2.to_compile, vec![EcoString::from("two")]);
    assert_eq!(
        loaded2.cached,
        vec![EcoString::from("one"), EcoString::from("three")]
    );
}

#[test]
fn invalid_module_name() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    // Cache is stale
    write_src(&fs, "/src/One.gleam", 1, TEST_SOURCE_2);

    let loaded = run_loader(fs, root, artefact);
    assert!(loaded.to_compile.is_empty());
    assert!(loaded.cached.is_empty());
    assert_eq!(
        loaded.warnings,
        vec![Warning::InvalidSource {
            path: Utf8PathBuf::from("/src/One.gleam"),
        }],
    );
}

#[test]
fn invalid_nested_module_name() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    // Cache is stale
    write_src(&fs, "/src/1/one.gleam", 1, TEST_SOURCE_2);

    let loaded = run_loader(fs, root, artefact);
    assert!(loaded.to_compile.is_empty());
    assert!(loaded.cached.is_empty());
    assert_eq!(
        loaded.warnings,
        vec![Warning::InvalidSource {
            path: Utf8PathBuf::from("/src/1/one.gleam"),
        }],
    );
}

#[test]
fn invalid_module_name_in_test() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    // Cache is stale
    write_src(&fs, "/test/One.gleam", 1, TEST_SOURCE_2);

    let loaded = run_loader(fs, root, artefact);
    assert!(loaded.to_compile.is_empty());
    assert!(loaded.cached.is_empty());
    assert_eq!(
        loaded.warnings,
        vec![Warning::InvalidSource {
            path: Utf8PathBuf::from("/test/One.gleam"),
        }],
    );
}

#[test]
fn invalid_nested_module_name_in_test() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    // Cache is stale
    write_src(&fs, "/test/1/one.gleam", 1, TEST_SOURCE_2);

    let loaded = run_loader(fs, root, artefact);
    assert!(loaded.to_compile.is_empty());
    assert!(loaded.cached.is_empty());
    assert_eq!(
        loaded.warnings,
        vec![Warning::InvalidSource {
            path: Utf8PathBuf::from("/test/1/one.gleam"),
        }],
    );
}

#[test]
fn invalid_module_name_in_dev() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    // Cache is stale
    write_src(&fs, "/dev/One.gleam", 1, TEST_SOURCE_2);

    let loaded = run_loader(fs, root, artefact);
    assert!(loaded.to_compile.is_empty());
    assert!(loaded.cached.is_empty());
    assert_eq!(
        loaded.warnings,
        vec![Warning::InvalidSource {
            path: Utf8PathBuf::from("/dev/One.gleam"),
        }],
    );
}

#[test]
fn invalid_nested_module_name_in_dev() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    // Cache is stale
    write_src(&fs, "/dev/1/one.gleam", 1, TEST_SOURCE_2);

    let loaded = run_loader(fs, root, artefact);
    assert!(loaded.to_compile.is_empty());
    assert!(loaded.cached.is_empty());
    assert_eq!(
        loaded.warnings,
        vec![Warning::InvalidSource {
            path: Utf8PathBuf::from("/dev/1/one.gleam"),
        }],
    );
}

#[test]
fn cache_files_are_removed_when_source_removed() {
    let fs = InMemoryFileSystem::new();
    let root = Utf8Path::new("/");
    let artefact = Utf8Path::new("/artefact");

    // Source is removed, cache is present
    write_cache(&fs, "nested/one", 0, vec![], TEST_SOURCE_1);

    _ = run_loader(fs.clone(), root, artefact);

    assert_eq!(fs.files().len(), 0);
}
