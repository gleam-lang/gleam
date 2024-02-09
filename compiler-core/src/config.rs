use crate::error::{FileIoAction, FileKind};
use crate::io::FileSystemReader;
use crate::manifest::Manifest;
use crate::requirement::Requirement;
use crate::version::COMPILER_VERSION;
use crate::{Error, Result};
use camino::{Utf8Path, Utf8PathBuf};
use ecow::EcoString;
use globset::{Glob, GlobSetBuilder};
use hexpm::version::Version;
use http::Uri;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fmt;
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
        let s: &str = serde::de::Deserialize::deserialize(deserializer)?;
        match spdx::license_id(s) {
            None => Err(serde::de::Error::custom(format!(
                "{s} is not a valid SPDX License ID"
            ))),
            Some(_) => Ok(SpdxLicense {
                licence: String::from(s),
            }),
        }
    }
}

impl AsRef<str> for SpdxLicense {
    fn as_ref(&self) -> &str {
        self.licence.as_str()
    }
}

#[derive(Deserialize, Debug, PartialEq, Clone)]
pub struct PackageConfig {
    #[serde(with = "package_name")]
    pub name: EcoString,
    #[serde(default = "default_version")]
    pub version: Version,
    #[serde(default, rename = "gleam")]
    pub gleam_version: Option<EcoString>,
    #[serde(default, alias = "licenses")]
    pub licences: Vec<SpdxLicense>,
    #[serde(default)]
    pub description: EcoString,
    #[serde(default, alias = "docs")]
    pub documentation: Docs,
    #[serde(default)]
    pub dependencies: Dependencies,
    #[serde(default, rename = "dev-dependencies")]
    pub dev_dependencies: Dependencies,
    #[serde(default)]
    pub repository: Repository,
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

impl PackageConfig {
    pub fn dependencies_for(&self, mode: Mode) -> Result<Dependencies> {
        match mode {
            Mode::Dev | Mode::Lsp => self.all_dependencies(),
            Mode::Prod => Ok(self.dependencies.clone()),
        }
    }

    pub fn all_dependencies(&self) -> Result<Dependencies> {
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
        let config: PackageConfig = toml::from_str(&toml).map_err(|e| Error::FileIo {
            action: FileIoAction::Parse,
            kind: FileKind::File,
            path: path.as_ref().to_path_buf(),
            err: Some(e.to_string()),
        })?;
        Ok(config)
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
        Ok(match manifest {
            None => HashMap::new(),
            Some(manifest) => {
                StalePackageRemover::fresh_and_locked(&self.all_dependencies()?, manifest)
            }
        })
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
        if let Some(required_version) = &self.gleam_version {
            let compiler_version = hexpm::version::Version::parse(COMPILER_VERSION)
                .expect("Parse compiler semantic version");
            let range = hexpm::version::Range::new(required_version.to_string())
                .to_pubgrub()
                .map_err(|error| Error::InvalidVersionFormat {
                    input: required_version.to_string(),
                    error: error.to_string(),
                })?;

            // We ignore the pre-release and build metadata when checking compatibility
            let mut version_without_pre = compiler_version.clone();
            version_without_pre.pre = vec![];
            version_without_pre.build = None;
            if !range.contains(&version_without_pre) {
                return Err(Error::IncompatibleCompilerVersion {
                    package: self.name.to_string(),
                    required_version: required_version.to_string(),
                    gleam_version: COMPILER_VERSION.to_string(),
                });
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
struct StalePackageRemover<'a> {
    // These are the packages for which the requirement or their parents
    // requirement has not changed.
    fresh: HashSet<&'a str>,
    locked: HashMap<EcoString, &'a Vec<EcoString>>,
}

impl<'a> StalePackageRemover<'a> {
    pub fn fresh_and_locked(
        requirements: &'a HashMap<EcoString, Requirement>,
        manifest: &'a Manifest,
    ) -> HashMap<EcoString, Version> {
        let locked = manifest
            .packages
            .iter()
            .map(|p| (p.name.clone(), &p.requirements))
            .collect();
        Self {
            fresh: HashSet::new(),
            locked,
        }
        .run(requirements, manifest)
    }

    fn run(
        &mut self,
        requirements: &'a HashMap<EcoString, Requirement>,
        manifest: &'a Manifest,
    ) -> HashMap<EcoString, Version> {
        // Record all the requirements that have not changed
        for (name, requirement) in requirements {
            if manifest.requirements.get(name) != Some(requirement) {
                continue; // This package has changed, don't record it
            }

            // Recursively record the package and its deps as being fresh
            self.record_tree_fresh(name);
        }

        // Return all the previously resolved packages that have not been
        // recorded as fresh
        manifest
            .packages
            .iter()
            .filter(|package| {
                let new = requirements.contains_key(package.name.as_str())
                    && !manifest.requirements.contains_key(package.name.as_str());
                let fresh = self.fresh.contains(package.name.as_str());
                let locked = !new && fresh;
                if !locked {
                    tracing::info!(name = package.name.as_str(), "unlocking_stale_package");
                }
                locked
            })
            .map(|package| (package.name.clone(), package.version.clone()))
            .collect()
    }

    fn record_tree_fresh(&mut self, name: &'a str) {
        // Record the top level package
        let _ = self.fresh.insert(name);

        let deps = self
            .locked
            .get(name)
            .expect("Package fresh but not in manifest");
        // Record each of its deps recursively
        for package in *deps {
            self.record_tree_fresh(package);
        }
    }
}

#[test]
fn locked_no_manifest() {
    let mut config = PackageConfig::default();
    config.dependencies = [
        ("prod1".into(), Requirement::hex("~> 1.0")),
        ("prod2".into(), Requirement::hex("~> 2.0")),
    ]
    .into();
    config.dev_dependencies = [
        ("dev1".into(), Requirement::hex("~> 1.0")),
        ("dev2".into(), Requirement::hex("~> 2.0")),
    ]
    .into();
    assert_eq!(config.locked(None).unwrap(), [].into());
}

#[test]
fn locked_no_changes() {
    let mut config = PackageConfig::default();
    config.dependencies = [
        ("prod1".into(), Requirement::hex("~> 1.0")),
        ("prod2".into(), Requirement::hex("~> 2.0")),
    ]
    .into();
    config.dev_dependencies = [
        ("dev1".into(), Requirement::hex("~> 1.0")),
        ("dev2".into(), Requirement::hex("~> 2.0")),
    ]
    .into();
    let manifest = Manifest {
        requirements: config.all_dependencies().unwrap(),
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
    config.dependencies = [("prod1".into(), Requirement::hex("~> 1.0"))].into();
    config.dev_dependencies = [("dev2".into(), Requirement::hex("~> 2.0"))].into();
    let manifest = Manifest {
        requirements: config.all_dependencies().unwrap(),
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
        ("prod1".into(), Requirement::hex("~> 3.0")), // Does not match manifest
        ("prod2".into(), Requirement::hex("~> 2.0")),
    ]
    .into();
    config.dev_dependencies = [
        ("dev1".into(), Requirement::hex("~> 3.0")), // Does not match manifest
        ("dev2".into(), Requirement::hex("~> 2.0")),
    ]
    .into();
    let manifest = Manifest {
        requirements: [
            ("prod1".into(), Requirement::hex("~> 1.0")),
            ("prod2".into(), Requirement::hex("~> 2.0")),
            ("dev1".into(), Requirement::hex("~> 1.0")),
            ("dev2".into(), Requirement::hex("~> 2.0")),
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
        ("1".into(), Requirement::hex("~> 2.0")), // Does not match manifest
        ("2".into(), Requirement::hex("~> 1.0")),
    ]
    .into();
    config.dev_dependencies = [].into();
    let manifest = Manifest {
        requirements: [
            ("1".into(), Requirement::hex("~> 1.0")),
            ("2".into(), Requirement::hex("~> 1.0")),
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
        ("1".into(), Requirement::hex("~> 1.0")),
        ("2".into(), Requirement::hex("~> 1.0")),
        ("3".into(), Requirement::hex("~> 3.0")), // Does not match manifest
    ]
    .into();
    config.dev_dependencies = [].into();
    let manifest = Manifest {
        requirements: [
            ("1".into(), Requirement::hex("~> 1.0")),
            ("2".into(), Requirement::hex("~> 1.0")),
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
        requirements: requirements.iter().map(|e| (*e).into()).collect(),
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

#[derive(Deserialize, Debug, PartialEq, Eq, Default, Clone)]
pub struct ErlangConfig {
    #[serde(default)]
    pub application_start_module: Option<EcoString>,
    #[serde(default)]
    pub extra_applications: Vec<EcoString>,
}

#[derive(Deserialize, Debug, PartialEq, Default, Clone)]
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

#[derive(Deserialize, Debug, PartialEq, Eq, Default, Clone)]
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
}

#[derive(Deserialize, Debug, PartialEq, Eq, Clone)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Repository {
    GitHub {
        user: String,
        repo: String,
    },
    GitLab {
        user: String,
        repo: String,
    },
    BitBucket {
        user: String,
        repo: String,
    },
    CodeBerg {
        user: String,
        repo: String,
    },
    #[serde(alias = "forgejo")]
    Gitea {
        user: String,
        repo: String,
        #[serde(with = "uri_serde_default_https")]
        host: Uri,
    },
    SourceHut {
        user: String,
        repo: String,
    },
    Custom {
        url: String,
    },
    None,
}

impl Repository {
    pub fn url(&self) -> Option<String> {
        match self {
            Repository::GitHub { repo, user } => Some(format!("https://github.com/{user}/{repo}")),
            Repository::GitLab { repo, user } => Some(format!("https://gitlab.com/{user}/{repo}")),
            Repository::BitBucket { repo, user } => {
                Some(format!("https://bitbucket.com/{user}/{repo}"))
            }
            Repository::CodeBerg { repo, user } => {
                Some(format!("https://codeberg.org/{user}/{repo}"))
            }
            Repository::SourceHut { repo, user } => {
                Some(format!("https://git.sr.ht/~{user}/{repo}"))
            }
            Repository::Gitea { repo, user, host } => Some(format!("{host}/{user}/{repo}")),
            Repository::Custom { url } => Some(url.clone()),
            Repository::None => None,
        }
    }
}

impl Default for Repository {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Deserialize, Default, Debug, PartialEq, Eq, Clone)]
pub struct Docs {
    #[serde(default)]
    pub pages: Vec<DocsPage>,
}

#[derive(Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct DocsPage {
    pub title: String,
    pub path: String,
    pub source: Utf8PathBuf,
}

#[derive(Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Link {
    pub title: String,
    #[serde(with = "uri_serde")]
    pub href: Uri,
}

// Note we don't use http-serde since we also want to validate the scheme and host is set.
mod uri_serde {
    use http::uri::InvalidUri;
    use serde::{de::Error as _, Deserialize, Deserializer};

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
}

// This prefixes https as a default in the event no scheme was provided
mod uri_serde_default_https {
    use http::uri::InvalidUri;
    use serde::{de::Error as _, Deserialize, Deserializer};

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
            true => format!("https://{}", string)
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
    use std::sync::OnceLock;

    static PACKAGE_NAME_PATTERN: OnceLock<Regex> = OnceLock::new();

    pub fn deserialize<'de, D>(deserializer: D) -> Result<EcoString, D::Error>
    where
        D: Deserializer<'de>,
    {
        let name: &str = serde::de::Deserialize::deserialize(deserializer)?;
        if PACKAGE_NAME_PATTERN
            .get_or_init(|| Regex::new("^[a-z][a-z0-9_]*$").expect("Package name regex"))
            .is_match(name)
        {
            Ok(name.into())
        } else {
            let error =
                "Package names may only container lowercase letters, numbers, and underscores";
            Err(serde::de::Error::custom(error))
        }
    }
}

#[test]
fn name_with_dash() {
    let input = r#"
name = "one-two"
"#;
    assert_eq!(
        toml::from_str::<PackageConfig>(input)
            .unwrap_err()
            .to_string(),
        "Package names may only container lowercase letters, numbers, and underscores for key `name` at line 1 column 1"
    )
}

#[test]
fn name_with_number_start() {
    let input = r#"
name = "1"
"#;
    assert_eq!(
        toml::from_str::<PackageConfig>(input)
            .unwrap_err()
            .to_string(),
        "Package names may only container lowercase letters, numbers, and underscores for key `name` at line 1 column 1"
    )
}
