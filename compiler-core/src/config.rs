use crate::error::{FileIoAction, FileKind};
use crate::io::FileSystemReader;
use crate::project::Manifest;
use crate::{Error, Result};
use hexpm::version::{Range, Version};
use http::Uri;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

#[cfg(test)]
use crate::project::ManifestPackage;

use crate::build::{Mode, Target};

fn default_version() -> Version {
    Version::parse("0.1.0").expect("default version")
}

fn erlang_target() -> Target {
    Target::Erlang
}

pub type Dependencies = HashMap<String, Range>;

#[derive(Clone, Debug, PartialEq)]
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
                "{} is not a valid SPDX License ID",
                s
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
    pub name: String,
    #[serde(default = "default_version")]
    pub version: Version,
    #[serde(default, alias = "licenses")]
    pub licences: Vec<SpdxLicense>,
    #[serde(default)]
    pub description: String,
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
    #[serde(default = "erlang_target")]
    pub target: Target,
}

impl PackageConfig {
    pub fn dependencies_for(&self, mode: Mode) -> Result<Dependencies> {
        match mode {
            Mode::Dev => self.all_dependencies(),
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

    pub fn read<FS: FileSystemReader, P: AsRef<Path>>(
        path: P,
        fs: &FS,
    ) -> Result<PackageConfig, Error> {
        let toml = fs.read(path.as_ref())?;
        toml::from_str(&toml).map_err(|e| Error::FileIo {
            action: FileIoAction::Parse,
            kind: FileKind::File,
            path: path.as_ref().to_path_buf(),
            err: Some(e.to_string()),
        })
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
    pub fn locked(&self, manifest: Option<&Manifest>) -> Result<HashMap<String, Version>> {
        Ok(match manifest {
            None => HashMap::new(),
            Some(manifest) => {
                StalePackageRemover::fresh_and_locked(&self.all_dependencies()?, manifest)
            }
        })
    }
}

#[derive(Debug)]
struct StalePackageRemover<'a> {
    // These are the packages for which the requirement or their parents
    // requirement has not changed.
    fresh: HashSet<&'a str>,
    locked: HashMap<&'a str, &'a Vec<String>>,
}

impl<'a> StalePackageRemover<'a> {
    pub fn fresh_and_locked(
        requirements: &'a HashMap<String, Range>,
        manifest: &'a Manifest,
    ) -> HashMap<String, Version> {
        let locked = manifest
            .packages
            .iter()
            .map(|p| (p.name.as_str(), &p.requirements))
            .collect();
        Self {
            fresh: HashSet::new(),
            locked,
        }
        .run(requirements, manifest)
    }

    fn run(
        &mut self,
        requirements: &'a HashMap<String, Range>,
        manifest: &'a Manifest,
    ) -> HashMap<String, Version> {
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
                let fresh = self.fresh.contains(package.name.as_str());
                if !fresh {
                    tracing::info!(name = package.name.as_str(), "unlocking_stale_package");
                }
                fresh
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
        ("prod1".into(), Range::new("~> 1.0".into())),
        ("prod2".into(), Range::new("~> 2.0".into())),
    ]
    .into();
    config.dev_dependencies = [
        ("dev1".into(), Range::new("~> 1.0".into())),
        ("dev2".into(), Range::new("~> 2.0".into())),
    ]
    .into();
    assert_eq!(config.locked(None).unwrap(), [].into());
}

#[test]
fn locked_no_changes() {
    let mut config = PackageConfig::default();
    config.dependencies = [
        ("prod1".into(), Range::new("~> 1.0".into())),
        ("prod2".into(), Range::new("~> 2.0".into())),
    ]
    .into();
    config.dev_dependencies = [
        ("dev1".into(), Range::new("~> 1.0".into())),
        ("dev2".into(), Range::new("~> 2.0".into())),
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
    config.dependencies = [("prod1".into(), Range::new("~> 1.0".into()))].into();
    config.dev_dependencies = [("dev2".into(), Range::new("~> 2.0".into()))].into();
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
        ("prod1".into(), Range::new("~> 3.0".into())), // Does not match manifest
        ("prod2".into(), Range::new("~> 2.0".into())),
    ]
    .into();
    config.dev_dependencies = [
        ("dev1".into(), Range::new("~> 3.0".into())), // Does not match manifest
        ("dev2".into(), Range::new("~> 2.0".into())),
    ]
    .into();
    let manifest = Manifest {
        requirements: [
            ("prod1".into(), Range::new("~> 1.0".into())),
            ("prod2".into(), Range::new("~> 2.0".into())),
            ("dev1".into(), Range::new("~> 1.0".into())),
            ("dev2".into(), Range::new("~> 2.0".into())),
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
        ("1".into(), Range::new("~> 2.0".into())), // Does not match manifest
        ("2".into(), Range::new("~> 1.0".into())),
    ]
    .into();
    config.dev_dependencies = [].into();
    let manifest = Manifest {
        requirements: [
            ("1".into(), Range::new("~> 1.0".into())),
            ("2".into(), Range::new("~> 1.0".into())),
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

#[cfg(test)]
fn manifest_package(
    name: &'static str,
    version: &'static str,
    requirements: &'static [&'static str],
) -> ManifestPackage {
    use crate::project::Base16Checksum;

    ManifestPackage {
        name: name.into(),
        version: Version::parse(version).unwrap(),
        build_tools: vec![],
        otp_app: None,
        requirements: requirements.iter().map(|e| (*e).to_string()).collect(),
        source: crate::project::ManifestPackageSource::Hex {
            outer_checksum: Base16Checksum(vec![]),
        },
    }
}

#[cfg(test)]
fn locked_version(name: &'static str, version: &'static str) -> (String, Version) {
    (name.into(), Version::parse(version).unwrap())
}

impl Default for PackageConfig {
    fn default() -> Self {
        Self {
            name: Default::default(),
            version: default_version(),
            description: Default::default(),
            documentation: Default::default(),
            dependencies: Default::default(),
            erlang: Default::default(),
            repository: Default::default(),
            dev_dependencies: Default::default(),
            licences: Default::default(),
            links: Default::default(),
            target: Target::Erlang,
        }
    }
}

#[derive(Deserialize, Debug, PartialEq, Default, Clone)]
pub struct ErlangConfig {
    #[serde(default)]
    pub application_start_module: Option<String>,
    #[serde(default)]
    pub extra_applications: Vec<String>,
}

#[derive(Deserialize, Debug, PartialEq, Clone)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Repository {
    GitHub { user: String, repo: String },
    GitLab { user: String, repo: String },
    BitBucket { user: String, repo: String },
    Custom { url: String },
    None,
}

impl Repository {
    pub fn url(&self) -> Option<String> {
        match self {
            Repository::GitHub { repo, user } => {
                Some(format!("https://github.com/{}/{}", user, repo))
            }
            Repository::GitLab { repo, user } => {
                Some(format!("https://gitlab.com/{}/{}", user, repo))
            }
            Repository::BitBucket { repo, user } => {
                Some(format!("https://bitbucket.com/{}/{}", user, repo))
            }
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

#[derive(Deserialize, Default, Debug, PartialEq, Clone)]
pub struct Docs {
    #[serde(default)]
    pub pages: Vec<DocsPage>,
}

#[derive(Deserialize, Debug, PartialEq, Clone)]
pub struct DocsPage {
    pub title: String,
    pub path: String,
    pub source: PathBuf,
}

#[derive(Deserialize, Debug, PartialEq, Clone)]
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

mod package_name {
    use lazy_static::lazy_static;
    use regex::Regex;
    use serde::Deserializer;

    lazy_static! {
        static ref PACKAGE_NAME_PATTERN: Regex =
            Regex::new("^[a-z][a-z0-9_]*$").expect("Package name regex");
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<String, D::Error>
    where
        D: Deserializer<'de>,
    {
        let name: &str = serde::de::Deserialize::deserialize(deserializer)?;
        if PACKAGE_NAME_PATTERN.is_match(name) {
            Ok(name.to_string())
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
