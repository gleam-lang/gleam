mod stale_package_remover;
use crate::error::{FileIoAction, FileKind};
use crate::io::FileSystemReader;
use crate::io::ordered_map;
use crate::manifest::Manifest;
use crate::requirement::Requirement;
use crate::version::COMPILER_VERSION;
use crate::{Error, Result};
use camino::{Utf8Path, Utf8PathBuf};
use ecow::EcoString;
use globset::{Glob, GlobSetBuilder};
use hexpm::version::{self, LowestVersion, Version};
use http::Uri;
use serde::ser::SerializeSeq;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{self};
use std::marker::PhantomData;

#[cfg(test)]
use crate::manifest::ManifestPackage;

use crate::build::{Mode, Runtime, Target};

fn default_version() -> Version {
    Version::parse("0.1.0").expect("default version")
}

fn erlang_target() -> Target {
    Target::Erlang
}

fn default_javascript_runtime() -> Runtime {
    Runtime::NodeJs
}

pub type Dependencies = HashMap<EcoString, Requirement>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SpdxLicense {
    pub licence: String,
}

impl ToString for SpdxLicense {
    fn to_string(&self) -> String {
        String::from(&self.licence)
    }
}

impl<'de> Deserialize<'de> for SpdxLicense {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_str(SpdxLicenseVisitor)
    }
}

struct SpdxLicenseVisitor;

impl<'de> serde::de::Visitor<'de> for SpdxLicenseVisitor {
    type Value = SpdxLicense;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("a SPDX License ID")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        match spdx::license_id(value) {
            None => Err(serde::de::Error::custom(format!(
                "{value} is not a valid SPDX License ID"
            ))),
            Some(_) => Ok(SpdxLicense {
                licence: value.to_string(),
            }),
        }
    }
}

impl Serialize for SpdxLicense {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.licence)
    }
}

impl AsRef<str> for SpdxLicense {
    fn as_ref(&self) -> &str {
        self.licence.as_str()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct GleamVersion(version::Range);
impl From<version::Range> for GleamVersion {
    fn from(range: version::Range) -> Self {
        Self(range)
    }
}

impl From<GleamVersion> for version::Range {
    fn from(gleam_version: GleamVersion) -> Self {
        gleam_version.0
    }
}

impl From<GleamVersion> for pubgrub::Range<Version> {
    fn from(gleam_version: GleamVersion) -> Self {
        gleam_version.0.into()
    }
}

impl GleamVersion {
    pub fn from_pubgrub(range: pubgrub::Range<Version>) -> Self {
        let range: version::Range = range.into();
        range.into()
    }

    pub fn as_pubgrub(&self) -> &pubgrub::Range<Version> {
        self.0.to_pubgrub()
    }

    pub fn new(spec: String) -> Result<GleamVersion> {
        let hex =
            version::Range::new(spec.to_string()).map_err(|e| Error::InvalidVersionFormat {
                input: spec,
                error: e.to_string(),
            })?;
        Ok(hex.into())
    }

    pub fn lowest_version(&self) -> Option<Version> {
        self.as_pubgrub().lowest_version()
    }

    pub fn hex(&self) -> &version::Range {
        &self.0
    }
}

#[derive(Deserialize, Serialize, Debug, PartialEq, Clone)]
pub struct PackageConfig {
    #[serde(deserialize_with = "package_name::deserialize")]
    pub name: EcoString,
    #[serde(default = "default_version")]
    pub version: Version,

    #[serde(
        default,
        rename = "gleam",
        deserialize_with = "deserialise_gleam_version",
        serialize_with = "serialise_gleam_version"
    )]
    pub gleam_version: Option<GleamVersion>,
    #[serde(default, alias = "licenses")]
    pub licences: Vec<SpdxLicense>,
    #[serde(default)]
    pub description: EcoString,
    #[serde(default, alias = "docs")]
    pub documentation: Docs,
    #[serde(default, serialize_with = "ordered_map")]
    pub dependencies: Dependencies,
    #[serde(default, alias = "dev-dependencies", serialize_with = "ordered_map")]
    pub dev_dependencies: Dependencies,
    #[serde(default)]
    pub repository: Option<Repository>,
    #[serde(default)]
    pub links: Vec<Link>,
    #[serde(default)]
    pub erlang: ErlangConfig,
    #[serde(default)]
    pub javascript: JavaScriptConfig,
    #[serde(default = "erlang_target")]
    pub target: Target,
    #[serde(default)]
    pub internal_modules: Option<Vec<Glob>>,
}

pub fn serialise_gleam_version<S>(
    gleam_gersion: &Option<GleamVersion>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    match gleam_gersion {
        Some(version) => serializer.serialize_str(&version.hex().to_string()),
        None => serializer.serialize_none(),
    }
}

pub fn deserialise_gleam_version<'de, D>(deserialiser: D) -> Result<Option<GleamVersion>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    match Deserialize::deserialize(deserialiser)? {
        Some(range_string) => {
            let hex = version::Range::new(range_string).map_err(serde::de::Error::custom)?;
            Ok(Some(hex.into()))
        }
        None => Ok(None),
    }
}

impl PackageConfig {
    pub fn dependencies_for(&self, mode: Mode) -> Result<Dependencies> {
        match mode {
            Mode::Dev | Mode::Lsp => self.all_direct_dependencies(),
            Mode::Prod => Ok(self.dependencies.clone()),
        }
    }

    // Return all the dependencies listed in the configuration, that is, all the
    // direct dependencies, both in the `dependencies` and `dev_dependencies`.
    pub fn all_direct_dependencies(&self) -> Result<Dependencies> {
        let mut deps =
            HashMap::with_capacity(self.dependencies.len() + self.dev_dependencies.len());
        for (name, requirement) in self.dependencies.iter().chain(&self.dev_dependencies) {
            let already_inserted = deps.insert(name.clone(), requirement.clone()).is_some();
            if already_inserted {
                return Err(Error::DuplicateDependency(name.clone()));
            }
        }
        Ok(deps)
    }

    pub fn read<FS: FileSystemReader, P: AsRef<Utf8Path>>(
        path: P,
        fs: &FS,
    ) -> Result<PackageConfig, Error> {
        let toml = fs.read(path.as_ref())?;
        deserialise_config(path, toml)
    }

    /// Get the locked packages for the current config and a given (optional)
    /// manifest of previously locked packages.
    ///
    /// If a package is removed or the specified required version range for it
    /// changes then it is not considered locked. This also goes for any child
    /// packages of the package which have no other parents.
    ///
    /// This function should be used each time resolution is performed so that
    /// outdated deps are removed from the manifest and not locked to the
    /// previously selected versions.
    ///
    pub fn locked(&self, manifest: Option<&Manifest>) -> Result<HashMap<EcoString, Version>> {
        match manifest {
            None => Ok(HashMap::new()),
            Some(manifest) => {
                let requirements = self.all_direct_dependencies()?;
                let fresh_and_locked = stale_package_remover::StalePackageRemover::fresh_and_locked(
                    &requirements,
                    manifest,
                );
                Ok(fresh_and_locked)
            }
        }
    }

    /// Determines whether the given module should be hidden in the docs or not
    ///
    /// The developer can specify a list of glob patterns in the gleam.toml file
    /// to determine modules that should not be shown in the package's documentation
    pub fn is_internal_module(&self, module: &str) -> bool {
        let package = &self.name;
        match &self.internal_modules {
            Some(globs) => {
                let mut builder = GlobSetBuilder::new();
                for glob in globs {
                    _ = builder.add(glob.clone());
                }
                builder.build()
            }

            // If no patterns were specified in the config then we use a default value
            None => GlobSetBuilder::new()
                .add(Glob::new(&format!("{package}/internal")).expect("internal module glob"))
                .add(Glob::new(&format!("{package}/internal/*")).expect("internal module glob"))
                .build(),
        }
        .expect("internal module globs")
        .is_match(module)
    }

    // Checks to see if the gleam version specified in the config is compatible
    // with the current compiler version
    pub fn check_gleam_compatibility(&self) -> Result<(), Error> {
        if let Some(version) = &self.gleam_version {
            let range = version.as_pubgrub();
            let compiler_version =
                Version::parse(COMPILER_VERSION).expect("Parse compiler semantic version");

            // We ignore the pre-release and build metadata when checking compatibility
            let mut version_without_pre = compiler_version.clone();
            version_without_pre.pre = vec![];
            version_without_pre.build = None;
            if !range.contains(&version_without_pre) {
                return Err(Error::IncompatibleCompilerVersion {
                    package: self.name.to_string(),
                    required_version: range.to_string(),
                    gleam_version: COMPILER_VERSION.to_string(),
                });
            }
        }
        Ok(())
    }

    pub fn tag_for_version(&self, version: &Version) -> String {
        let prefix = match self.repository.as_ref() {
            Some(
                Repository::GitHub { tag_prefix, .. }
                | Repository::GitLab { tag_prefix, .. }
                | Repository::BitBucket { tag_prefix, .. }
                | Repository::Codeberg { tag_prefix, .. }
                | Repository::SourceHut { tag_prefix, .. }
                | Repository::Gitea { tag_prefix, .. }
                | Repository::Forgejo { tag_prefix, .. }
                | Repository::Tangled { tag_prefix, .. },
            ) => tag_prefix.as_ref(),

            Some(Repository::Custom { .. }) | None => None,
        };

        match prefix {
            Some(prefix) => format!("{prefix}v{version}"),
            None => format!("v{version}"),
        }
    }
}

fn deserialise_config<P: AsRef<Utf8Path>>(
    path: P,
    toml: String,
) -> std::result::Result<PackageConfig, Error> {
    let config: PackageConfig = toml::from_str(&toml).map_err(|e| Error::FileIo {
        action: FileIoAction::Parse,
        kind: FileKind::File,
        path: path.as_ref().to_path_buf(),
        err: Some(e.to_string()),
    })?;
    Ok(config)
}

// https://github.com/gleam-lang/gleam/issues/4867
#[test]
fn deny_extra_deps_properties() {
    let toml = r#"
name = "wibble"
version = "1.0.0"

[dependencies]
aide_generator = { git = "git@github.com:crowdhailer/aide.git", ref = "f559c5bc", extra = "idk what this is" }
"#;
    let error = deserialise_config("gleam.toml", toml.into())
        .expect_err("should fail to deserialise because of additional path");

    insta::assert_snapshot!(insta::internals::AutoName, error.pretty_string());
}

#[test]
fn locked_no_manifest() {
    let mut config = PackageConfig::default();
    config.dependencies = [
        ("prod1".into(), Requirement::hex("~> 1.0").unwrap()),
        ("prod2".into(), Requirement::hex("~> 2.0").unwrap()),
    ]
    .into();
    config.dev_dependencies = [
        ("dev1".into(), Requirement::hex("~> 1.0").unwrap()),
        ("dev2".into(), Requirement::hex("~> 2.0").unwrap()),
    ]
    .into();
    assert_eq!(config.locked(None).unwrap(), [].into());
}

#[test]
fn locked_no_changes() {
    let mut config = PackageConfig::default();
    config.dependencies = [
        ("prod1".into(), Requirement::hex("~> 1.0").unwrap()),
        ("prod2".into(), Requirement::hex("~> 2.0").unwrap()),
    ]
    .into();
    config.dev_dependencies = [
        ("dev1".into(), Requirement::hex("~> 1.0").unwrap()),
        ("dev2".into(), Requirement::hex("~> 2.0").unwrap()),
    ]
    .into();
    let manifest = Manifest {
        requirements: config.all_direct_dependencies().unwrap(),
        packages: vec![
            manifest_package("prod1", "1.1.0", &[]),
            manifest_package("prod2", "1.2.0", &[]),
            manifest_package("dev1", "1.1.0", &[]),
            manifest_package("dev2", "1.2.0", &[]),
        ],
    };
    assert_eq!(
        config.locked(Some(&manifest)).unwrap(),
        [
            locked_version("prod1", "1.1.0"),
            locked_version("prod2", "1.2.0"),
            locked_version("dev1", "1.1.0"),
            locked_version("dev2", "1.2.0"),
        ]
        .into()
    );
}

#[test]
fn locked_some_removed() {
    let mut config = PackageConfig::default();
    config.dependencies = [("prod1".into(), Requirement::hex("~> 1.0").unwrap())].into();
    config.dev_dependencies = [("dev2".into(), Requirement::hex("~> 2.0").unwrap())].into();
    let manifest = Manifest {
        requirements: config.all_direct_dependencies().unwrap(),
        packages: vec![
            manifest_package("prod1", "1.1.0", &[]),
            manifest_package("prod2", "1.2.0", &[]), // Not in config
            manifest_package("dev1", "1.1.0", &[]),  // Not in config
            manifest_package("dev2", "1.2.0", &[]),
        ],
    };
    assert_eq!(
        config.locked(Some(&manifest)).unwrap(),
        [
            // prod2 removed
            // dev1 removed
            locked_version("prod1", "1.1.0"),
            locked_version("dev2", "1.2.0"),
        ]
        .into()
    );
}

#[test]
fn locked_some_changed() {
    let mut config = PackageConfig::default();
    config.dependencies = [
        ("prod1".into(), Requirement::hex("~> 3.0").unwrap()), // Does not match manifest
        ("prod2".into(), Requirement::hex("~> 2.0").unwrap()),
    ]
    .into();
    config.dev_dependencies = [
        ("dev1".into(), Requirement::hex("~> 3.0").unwrap()), // Does not match manifest
        ("dev2".into(), Requirement::hex("~> 2.0").unwrap()),
    ]
    .into();
    let manifest = Manifest {
        requirements: [
            ("prod1".into(), Requirement::hex("~> 1.0").unwrap()),
            ("prod2".into(), Requirement::hex("~> 2.0").unwrap()),
            ("dev1".into(), Requirement::hex("~> 1.0").unwrap()),
            ("dev2".into(), Requirement::hex("~> 2.0").unwrap()),
        ]
        .into(),
        packages: vec![
            manifest_package("prod1", "1.1.0", &[]),
            manifest_package("prod2", "1.2.0", &[]),
            manifest_package("dev1", "1.1.0", &[]),
            manifest_package("dev2", "1.2.0", &[]),
        ],
    };
    assert_eq!(
        config.locked(Some(&manifest)).unwrap(),
        [
            // prod1 removed
            // dev1 removed
            locked_version("prod2", "1.2.0"),
            locked_version("dev2", "1.2.0"),
        ]
        .into()
    );
}

#[test]
fn locked_nested_are_removed_too() {
    let mut config = PackageConfig::default();
    config.dependencies = [
        ("1".into(), Requirement::hex("~> 2.0").unwrap()), // Does not match manifest
        ("2".into(), Requirement::hex("~> 1.0").unwrap()),
    ]
    .into();
    config.dev_dependencies = [].into();
    let manifest = Manifest {
        requirements: [
            ("1".into(), Requirement::hex("~> 1.0").unwrap()),
            ("2".into(), Requirement::hex("~> 1.0").unwrap()),
        ]
        .into(),
        packages: vec![
            manifest_package("1", "1.1.0", &["1.1", "1.2"]),
            manifest_package("1.1", "1.1.0", &["1.1.1", "1.1.2"]),
            manifest_package("1.1.1", "1.1.0", &["shared"]),
            manifest_package("1.1.2", "1.1.0", &[]),
            manifest_package("1.2", "1.1.0", &["1.2.1", "1.2.2"]),
            manifest_package("1.2.1", "1.1.0", &[]),
            manifest_package("1.2.2", "1.1.0", &[]),
            manifest_package("2", "2.1.0", &["2.1", "2.2"]),
            manifest_package("2.1", "2.1.0", &["2.1.1", "2.1.2"]),
            manifest_package("2.1.1", "2.1.0", &[]),
            manifest_package("2.1.2", "2.1.0", &[]),
            manifest_package("2.2", "2.1.0", &["2.2.1", "2.2.2", "shared"]),
            manifest_package("2.2.1", "2.1.0", &[]),
            manifest_package("2.2.2", "2.1.0", &[]),
            manifest_package("shared", "2.1.0", &[]),
        ],
    };
    assert_eq!(
        config.locked(Some(&manifest)).unwrap(),
        [
            // 1* removed
            locked_version("2", "2.1.0"),
            locked_version("2.1", "2.1.0"),
            locked_version("2.1.1", "2.1.0"),
            locked_version("2.1.2", "2.1.0"),
            locked_version("2.2", "2.1.0"),
            locked_version("2.2.1", "2.1.0"),
            locked_version("2.2.2", "2.1.0"),
            locked_version("shared", "2.1.0"),
        ]
        .into()
    );
}

// https://github.com/gleam-lang/gleam/issues/1754
#[test]
fn locked_unlock_new() {
    let mut config = PackageConfig::default();
    config.dependencies = [
        ("1".into(), Requirement::hex("~> 1.0").unwrap()),
        ("2".into(), Requirement::hex("~> 1.0").unwrap()),
        ("3".into(), Requirement::hex("~> 3.0").unwrap()), // Does not match manifest
    ]
    .into();
    config.dev_dependencies = [].into();
    let manifest = Manifest {
        requirements: [
            ("1".into(), Requirement::hex("~> 1.0").unwrap()),
            ("2".into(), Requirement::hex("~> 1.0").unwrap()),
        ]
        .into(),
        packages: vec![
            manifest_package("1", "1.1.0", &["3"]),
            manifest_package("2", "1.1.0", &["3"]),
            manifest_package("3", "1.1.0", &[]),
        ],
    };
    assert_eq!(
        config.locked(Some(&manifest)).unwrap(),
        [locked_version("1", "1.1.0"), locked_version("2", "1.1.0"),].into()
    )
}

#[test]
fn default_internal_modules() {
    // When no internal modules are specified then we default to
    // `["$package/internal", "$package/internal/*"]`
    let mut config = PackageConfig::default();
    config.name = "my_package".into();
    config.internal_modules = None;

    assert!(config.is_internal_module("my_package/internal"));
    assert!(config.is_internal_module("my_package/internal/wibble"));
    assert!(config.is_internal_module("my_package/internal/wibble/wobble"));
    assert!(!config.is_internal_module("my_package/internallll"));
    assert!(!config.is_internal_module("my_package/other"));
    assert!(!config.is_internal_module("my_package/other/wibble"));
    assert!(!config.is_internal_module("other/internal"));
}

#[test]
fn no_internal_modules() {
    // When no internal modules are specified then we default to
    // `["$package/internal", "$package/internal/*"]`
    let mut config = PackageConfig::default();
    config.name = "my_package".into();
    config.internal_modules = Some(vec![]);

    assert!(!config.is_internal_module("my_package/internal"));
    assert!(!config.is_internal_module("my_package/internal/wibble"));
    assert!(!config.is_internal_module("my_package/internal/wibble/wobble"));
    assert!(!config.is_internal_module("my_package/internallll"));
    assert!(!config.is_internal_module("my_package/other"));
    assert!(!config.is_internal_module("my_package/other/wibble"));
    assert!(!config.is_internal_module("other/internal"));
}

#[test]
fn hidden_a_directory_from_docs() {
    let mut config = PackageConfig::default();
    config.internal_modules = Some(vec![Glob::new("package/internal/*").expect("")]);

    let mod1 = "package/internal";
    let mod2 = "package/internal/module";

    assert_eq!(config.is_internal_module(mod1), false);
    assert_eq!(config.is_internal_module(mod2), true);
}

#[test]
fn hidden_two_directories_from_docs() {
    let mut config = PackageConfig::default();
    config.internal_modules = Some(vec![
        Glob::new("package/internal1/*").expect(""),
        Glob::new("package/internal2/*").expect(""),
    ]);

    let mod1 = "package/internal1";
    let mod2 = "package/internal1/module";
    let mod3 = "package/internal2";
    let mod4 = "package/internal2/module";

    assert_eq!(config.is_internal_module(mod1), false);
    assert_eq!(config.is_internal_module(mod2), true);
    assert_eq!(config.is_internal_module(mod3), false);
    assert_eq!(config.is_internal_module(mod4), true);
}

#[test]
fn hidden_a_directory_and_a_file_from_docs() {
    let mut config = PackageConfig::default();
    config.internal_modules = Some(vec![
        Glob::new("package/internal1/*").expect(""),
        Glob::new("package/module").expect(""),
    ]);

    let mod1 = "package/internal1";
    let mod2 = "package/internal1/module";
    let mod3 = "package/module";
    let mod4 = "package/module/inner";

    assert_eq!(config.is_internal_module(mod1), false);
    assert_eq!(config.is_internal_module(mod2), true);
    assert_eq!(config.is_internal_module(mod3), true);
    assert_eq!(config.is_internal_module(mod4), false);
}

#[test]
fn hidden_a_file_in_all_directories_from_docs() {
    let mut config = PackageConfig::default();
    config.internal_modules = Some(vec![Glob::new("package/*/module1").expect("")]);

    let mod1 = "package/internal1/module1";
    let mod2 = "package/internal2/module1";
    let mod3 = "package/internal2/module2";
    let mod4 = "package/module";

    assert_eq!(config.is_internal_module(mod1), true);
    assert_eq!(config.is_internal_module(mod2), true);
    assert_eq!(config.is_internal_module(mod3), false);
    assert_eq!(config.is_internal_module(mod4), false);
}

#[cfg(test)]
fn manifest_package(
    name: &'static str,
    version: &'static str,
    requirements: &'static [&'static str],
) -> ManifestPackage {
    use crate::manifest::Base16Checksum;

    ManifestPackage {
        name: name.into(),
        version: Version::parse(version).unwrap(),
        build_tools: vec![],
        otp_app: None,
        requirements: requirements
            .iter()
            .map(|requirement| (*requirement).into())
            .collect(),
        source: crate::manifest::ManifestPackageSource::Hex {
            outer_checksum: Base16Checksum(vec![]),
        },
    }
}

#[cfg(test)]
fn locked_version(name: &'static str, version: &'static str) -> (EcoString, Version) {
    (name.into(), Version::parse(version).unwrap())
}

impl Default for PackageConfig {
    fn default() -> Self {
        Self {
            name: Default::default(),
            version: default_version(),
            gleam_version: Default::default(),
            description: Default::default(),
            documentation: Default::default(),
            dependencies: Default::default(),
            erlang: Default::default(),
            javascript: Default::default(),
            repository: Default::default(),
            dev_dependencies: Default::default(),
            licences: Default::default(),
            links: Default::default(),
            internal_modules: Default::default(),
            target: Target::Erlang,
        }
    }
}

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Default, Clone)]
pub struct ErlangConfig {
    /// An module that can be set in the `.app` file as the entrypoint for a stateful application
    /// that defines a singleton supervision tree.
    /// Erlang syntax.
    #[serde(default)]
    pub application_start_module: Option<EcoString>,
    /// The argument for the start module start function. If not set then `[]` is used as the
    /// default argument.
    /// Erlang syntax.
    #[serde(default)]
    pub application_start_argument: Option<EcoString>,
    #[serde(default)]
    pub extra_applications: Vec<EcoString>,
}

#[derive(Deserialize, Serialize, Debug, PartialEq, Default, Clone)]
pub struct JavaScriptConfig {
    #[serde(default)]
    pub typescript_declarations: bool,
    #[serde(default = "default_javascript_runtime")]
    pub runtime: Runtime,
    #[serde(default, rename = "deno")]
    pub deno: DenoConfig,
}

#[derive(Deserialize, Debug, PartialEq, Eq, Clone)]
pub enum DenoFlag {
    AllowAll,
    Allow(Vec<String>),
}

impl Default for DenoFlag {
    fn default() -> Self {
        Self::Allow(Vec::new())
    }
}

impl Serialize for DenoFlag {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            DenoFlag::AllowAll => serializer.serialize_bool(true),
            DenoFlag::Allow(items) => {
                let mut seq = serializer.serialize_seq(Some(items.len()))?;
                for e in items {
                    seq.serialize_element(e)?;
                }
                seq.end()
            }
        }
    }
}

fn bool_or_seq_string_to_deno_flag<'de, D>(deserializer: D) -> Result<DenoFlag, D::Error>
where
    D: serde::Deserializer<'de>,
{
    struct StringOrVec(PhantomData<Vec<String>>);

    impl<'de> serde::de::Visitor<'de> for StringOrVec {
        type Value = DenoFlag;

        fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            formatter.write_str("bool or list of strings")
        }

        fn visit_bool<E>(self, value: bool) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            if value {
                Ok(DenoFlag::AllowAll)
            } else {
                Ok(DenoFlag::default())
            }
        }

        fn visit_seq<S>(self, visitor: S) -> Result<Self::Value, S::Error>
        where
            S: serde::de::SeqAccess<'de>,
        {
            let allow: Vec<String> =
                Deserialize::deserialize(serde::de::value::SeqAccessDeserializer::new(visitor))
                    .unwrap_or_default();

            Ok(DenoFlag::Allow(allow))
        }
    }

    deserializer.deserialize_any(StringOrVec(PhantomData))
}

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Default, Clone)]
pub struct DenoConfig {
    #[serde(default, deserialize_with = "bool_or_seq_string_to_deno_flag")]
    pub allow_env: DenoFlag,
    #[serde(default)]
    pub allow_sys: bool,
    #[serde(default)]
    pub allow_hrtime: bool,
    #[serde(default, deserialize_with = "bool_or_seq_string_to_deno_flag")]
    pub allow_net: DenoFlag,
    #[serde(default)]
    pub allow_ffi: bool,
    #[serde(default, deserialize_with = "bool_or_seq_string_to_deno_flag")]
    pub allow_read: DenoFlag,
    #[serde(default, deserialize_with = "bool_or_seq_string_to_deno_flag")]
    pub allow_run: DenoFlag,
    #[serde(default, deserialize_with = "bool_or_seq_string_to_deno_flag")]
    pub allow_write: DenoFlag,
    #[serde(default)]
    pub allow_all: bool,
    #[serde(default)]
    pub unstable: bool,
    #[serde(
        default,
        serialize_with = "uri_serde::serialize_option",
        deserialize_with = "uri_serde::deserialize_option"
    )]
    pub location: Option<Uri>,
}

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Clone)]
#[serde(tag = "type")]
pub enum Repository {
    #[serde(rename = "github")]
    GitHub {
        user: String,
        repo: String,
        path: Option<String>,
        #[serde(alias = "tag-prefix")]
        tag_prefix: Option<String>,
    },
    #[serde(rename = "gitlab")]
    GitLab {
        user: String,
        repo: String,
        path: Option<String>,
        #[serde(alias = "tag-prefix")]
        tag_prefix: Option<String>,
    },
    #[serde(rename = "bitbucket")]
    BitBucket {
        user: String,
        repo: String,
        path: Option<String>,
        #[serde(alias = "tag-prefix")]
        tag_prefix: Option<String>,
    },
    #[serde(rename = "codeberg")]
    Codeberg {
        user: String,
        repo: String,
        path: Option<String>,
        #[serde(alias = "tag-prefix")]
        tag_prefix: Option<String>,
    },
    #[serde(rename = "gitea")]
    Gitea {
        user: String,
        repo: String,
        path: Option<String>,
        #[serde(alias = "tag-prefix")]
        tag_prefix: Option<String>,
        #[serde(
            serialize_with = "uri_serde::serialize",
            deserialize_with = "uri_serde_default_https::deserialize"
        )]
        host: Uri,
    },
    #[serde(rename = "forgejo")]
    Forgejo {
        user: String,
        repo: String,
        path: Option<String>,
        #[serde(alias = "tag-prefix")]
        tag_prefix: Option<String>,
        #[serde(
            serialize_with = "uri_serde::serialize",
            deserialize_with = "uri_serde_default_https::deserialize"
        )]
        host: Uri,
    },
    #[serde(rename = "sourcehut")]
    SourceHut {
        user: String,
        repo: String,
        path: Option<String>,
        #[serde(alias = "tag-prefix")]
        tag_prefix: Option<String>,
    },
    #[serde(rename = "tangled")]
    Tangled {
        user: String,
        repo: String,
        path: Option<String>,
        #[serde(alias = "tag-prefix")]
        tag_prefix: Option<String>,
    },
    #[serde(rename = "custom")]
    Custom {
        url: String,
        #[serde(alias = "tag-prefix")]
        tag_prefix: Option<String>,
    },
}

impl Repository {
    pub fn url(&self) -> String {
        match self {
            Repository::GitHub { repo, user, .. } => {
                format!("https://github.com/{user}/{repo}")
            }
            Repository::GitLab { repo, user, .. } => {
                format!("https://gitlab.com/{user}/{repo}")
            }
            Repository::BitBucket { repo, user, .. } => {
                format!("https://bitbucket.com/{user}/{repo}")
            }
            Repository::Codeberg { repo, user, .. } => {
                format!("https://codeberg.org/{user}/{repo}")
            }
            Repository::SourceHut { repo, user, .. } => {
                format!("https://git.sr.ht/~{user}/{repo}")
            }
            Repository::Tangled { repo, user, .. } => {
                format!("https://tangled.sh/{user}/{repo}")
            }
            Repository::Gitea {
                repo, user, host, ..
            }
            | Repository::Forgejo {
                repo, user, host, ..
            } => {
                let string_host = host.to_string();
                let cleaned_host = string_host.trim_end_matches('/');
                format!("{cleaned_host}/{user}/{repo}")
            }
            Repository::Custom { url, .. } => url.clone(),
        }
    }

    pub fn path(&self) -> Option<&String> {
        match self {
            Repository::GitHub { path, .. }
            | Repository::GitLab { path, .. }
            | Repository::BitBucket { path, .. }
            | Repository::Codeberg { path, .. }
            | Repository::SourceHut { path, .. }
            | Repository::Tangled { path, .. }
            | Repository::Gitea { path, .. }
            | Repository::Forgejo { path, .. } => path.as_ref(),

            Repository::Custom { .. } => None,
        }
    }
}

#[derive(Deserialize, Serialize, Default, Debug, PartialEq, Eq, Clone)]
pub struct Docs {
    #[serde(default)]
    pub pages: Vec<DocsPage>,
}

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Clone)]
pub struct DocsPage {
    pub title: String,
    pub path: String,
    pub source: Utf8PathBuf,
}

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Clone)]
pub struct Link {
    pub title: String,
    #[serde(with = "uri_serde")]
    pub href: Uri,
}

// Note we don't use http-serde since we also want to validate the scheme and host is set.
mod uri_serde {
    use http::uri::InvalidUri;
    use serde::{Deserialize, Deserializer, de::Error as _};

    pub fn deserialize<'de, D>(deserializer: D) -> Result<http::Uri, D::Error>
    where
        D: Deserializer<'de>,
    {
        let string = String::deserialize(deserializer)?;
        let uri: http::Uri = string
            .parse()
            .map_err(|err: InvalidUri| D::Error::custom(err.to_string()))?;
        if uri.scheme().is_none() || uri.host().is_none() {
            return Err(D::Error::custom("uri without scheme"));
        }
        Ok(uri)
    }

    pub fn deserialize_option<'de, D>(deserializer: D) -> Result<Option<http::Uri>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let string: Option<String> = Option::deserialize(deserializer)?;
        match string {
            Some(s) => {
                let deserializer = serde::de::value::StringDeserializer::new(s);
                deserialize(deserializer).map(Some)
            }
            None => Ok(None),
        }
    }

    pub fn serialize_option<S>(uri: &Option<http::Uri>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match uri {
            Some(uri) => serialize(uri, serializer),
            None => serializer.serialize_unit(),
        }
    }

    pub fn serialize<S>(uri: &http::Uri, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&uri.to_string())
    }
}

// This prefixes https as a default in the event no scheme was provided
mod uri_serde_default_https {
    use http::uri::InvalidUri;
    use serde::{Deserialize, Deserializer, de::Error as _};

    pub fn deserialize<'de, D>(deserializer: D) -> Result<http::Uri, D::Error>
    where
        D: Deserializer<'de>,
    {
        let string = String::deserialize(deserializer)?;
        let uri: http::Uri = string
            .parse()
            .map_err(|err: InvalidUri| D::Error::custom(err.to_string()))?;
        if uri.host().is_none() {
            return Err(D::Error::custom("uri without host"));
        }
        match uri.scheme().is_none() {
            true => format!("https://{string}")
                .parse()
                .map_err(|err: InvalidUri| D::Error::custom(err.to_string())),
            false => Ok(uri),
        }
    }
}

mod package_name {
    use ecow::EcoString;
    use regex::Regex;
    use serde::Deserializer;
    use std::{fmt, sync::OnceLock};

    static PACKAGE_NAME_PATTERN: OnceLock<Regex> = OnceLock::new();

    pub fn deserialize<'de, D>(deserializer: D) -> Result<EcoString, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(NameVisitor)
    }

    struct NameVisitor;

    impl<'de> serde::de::Visitor<'de> for NameVisitor {
        type Value = EcoString;

        fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            formatter.write_str("a package name")
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            if PACKAGE_NAME_PATTERN
                .get_or_init(|| Regex::new("^[a-z][a-z0-9_]*$").expect("Package name regex"))
                .is_match(value)
            {
                Ok(value.into())
            } else {
                let error =
                    "Package names may only contain lowercase letters, numbers, and underscores";
                Err(serde::de::Error::custom(error))
            }
        }
    }
}

#[test]
fn name_with_dash() {
    let input = r#"
name = "one-two"
"#;

    insta::assert_snapshot!(
        insta::internals::AutoName,
        toml::from_str::<PackageConfig>(input)
            .unwrap_err()
            .to_string()
    );
}

#[test]
fn name_with_number_start() {
    let input = r#"
name = "1"
"#;
    insta::assert_snapshot!(
        insta::internals::AutoName,
        toml::from_str::<PackageConfig>(input)
            .unwrap_err()
            .to_string(),
    )
}

#[test]
fn package_config_to_json() {
    let input = r#"
name = "my_project"
version = "1.0.0"
licences = ["Apache-2.0", "MIT"]
description = "Pretty complex config"
target = "erlang"
repository = { type = "github", user = "example", repo = "my_dep" }
links = [{ title = "Home page", href = "https://example.com" }]
internal_modules = ["my_app/internal"]
gleam = ">= 0.30.0"

[dependencies]
gleam_stdlib = ">= 0.18.0 and < 2.0.0"
my_other_project = { path = "../my_other_project" }

[dev_dependencies]
gleeunit = ">= 1.0.0 and < 2.0.0"

[documentation]
pages = [{ title = "My Page", path = "my-page.html", source = "./path/to/my-page.md" }]

[erlang]
application_start_module = "my_app/application"
extra_applications = ["inets", "ssl"]

[javascript]
typescript_declarations = true
runtime = "node"

[javascript.deno]
allow_all = false
allow_ffi = true
allow_env = ["DATABASE_URL"]
allow_net = ["example.com:443"]
allow_read = ["./database.sqlite"]
"#;

    let config = toml::from_str::<PackageConfig>(input).unwrap();
    let json = serde_json::to_string_pretty(&config).unwrap();
    let output = format!("--- GLEAM.TOML\n{input}\n\n--- EXPORTED JSON\n\n{json}");
    insta::assert_snapshot!(output);

    let roundtrip = serde_json::from_str::<PackageConfig>(&json).unwrap();
    assert_eq!(config, roundtrip);
}

#[test]
fn barebones_package_config_to_json() {
    let input = r#"
name = "my_project"
version = "1.0.0"
"#;

    let config = toml::from_str::<PackageConfig>(input).unwrap();
    let json = serde_json::to_string_pretty(&config).unwrap();
    let output = format!("--- GLEAM.TOML\n{input}\n\n--- EXPORTED JSON\n\n{json}");
    insta::assert_snapshot!(output);
}

#[test]
fn dev_deps_field_name() {
    let toml = r#"
name = "wibble"
version = "1.0.0"

[dev_dependencies]
wibble = ">= 1.0.0 and < 2.0.0"
"#;
    let hyphen_alternative = deserialise_config("gleam.toml", toml.into()).expect("valid config");
    let toml = r#"
name = "wibble"
version = "1.0.0"

[dev_dependencies]
wibble = ">= 1.0.0 and < 2.0.0"
"#;
    let canonical = deserialise_config("gleam.toml", toml.into()).expect("valid config");
    assert_eq!(canonical, hyphen_alternative)
}
