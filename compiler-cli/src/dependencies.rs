use std::{
    collections::{HashMap, HashSet},
    time::Instant,
};

use camino::{Utf8Path, Utf8PathBuf};
use flate2::read::GzDecoder;
use futures::future;
use gleam_core::{
    build::{Mode, Target, Telemetry},
    config::PackageConfig,
    dependency,
    error::{FileIoAction, FileKind, StandardIoAction},
    hex::{self, HEXPM_PUBLIC_KEY},
    io::{HttpClient as _, TarUnpacker, WrappedReader},
    manifest::{Base16Checksum, Manifest, ManifestPackage, ManifestPackageSource},
    paths::ProjectPaths,
    requirement::Requirement,
    Error, Result,
};
use hexpm::version::Version;
use itertools::Itertools;
use same_file::is_same_file;
use smol_str::SmolStr;
use strum::IntoEnumIterator;

use crate::{
    build_lock::BuildLock,
    cli,
    fs::{self, get_current_directory, ProjectIO},
    http::HttpClient,
};

pub fn list() -> Result<()> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");

    let paths = ProjectPaths::at_filesystem_root();
    let config = crate::config::root_config()?;
    let (_, manifest) = get_manifest(
        &paths,
        runtime.handle().clone(),
        Mode::Dev,
        &config,
        &cli::Reporter::new(),
        UseManifest::Yes,
    )?;
    list_manifest_packages(std::io::stdout(), manifest)
}

fn list_manifest_packages<W: std::io::Write>(mut buffer: W, manifest: Manifest) -> Result<()> {
    manifest
        .packages
        .into_iter()
        .try_for_each(|package| writeln!(buffer, "{} {}", package.name, package.version))
        .map_err(|e| Error::StandardIo {
            action: StandardIoAction::Write,
            err: Some(e.kind()),
        })
}

#[test]
fn list_manifest_format() {
    let mut buffer = vec![];
    let manifest = Manifest {
        requirements: HashMap::new(),
        packages: vec![
            ManifestPackage {
                name: "root".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4]),
                },
            },
            ManifestPackage {
                name: "aaa".into(),
                version: Version::new(0, 4, 2),
                build_tools: ["rebar3".into(), "make".into()].into(),
                otp_app: Some("aaa_app".into()),
                requirements: vec!["zzz".into(), "gleam_stdlib".into()],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![3, 22]),
                },
            },
            ManifestPackage {
                name: "zzz".into(),
                version: Version::new(0, 4, 0),
                build_tools: ["mix".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![3, 22]),
                },
            },
        ],
    };
    list_manifest_packages(&mut buffer, manifest).unwrap();
    assert_eq!(
        std::str::from_utf8(&buffer).unwrap(),
        r#"root 1.0.0
aaa 0.4.2
zzz 0.4.0
"#
    )
}

#[derive(Debug, Clone, Copy)]
pub enum UseManifest {
    Yes,
    No,
}

pub fn update() -> Result<()> {
    let paths = crate::project_paths_at_current_directory();
    _ = download(&paths, cli::Reporter::new(), None, UseManifest::No)?;
    Ok(())
}

pub fn download<Telem: Telemetry>(
    paths: &ProjectPaths,
    telemetry: Telem,
    new_package: Option<(Vec<String>, bool)>,
    // If true we read the manifest from disc. If not set then we ignore any
    // manifest which will result in the latest versions of the dependency
    // packages being resolved (not the locked ones).
    use_manifest: UseManifest,
) -> Result<Manifest> {
    let span = tracing::info_span!("download_deps");
    let _enter = span.enter();

    let mode = Mode::Dev;

    // We do this before acquiring the build lock so that we don't create the
    // build directory if there is no gleam.toml
    crate::config::ensure_config_exists(paths)?;

    let lock = BuildLock::new_packages(paths)?;
    let _guard = lock.lock(&telemetry);

    let fs = ProjectIO::boxed();

    // Read the project config
    let mut config = crate::config::read(paths.root_config())?;
    let project_name = config.name.clone();

    // Insert the new packages to add, if it exists
    if let Some((packages, dev)) = new_package {
        for package in packages {
            let version = Requirement::hex(">= 0.0.0");
            let _ = if dev {
                config.dev_dependencies.insert(package.into(), version)
            } else {
                config.dependencies.insert(package.into(), version)
            };
        }
    }

    // Start event loop so we can run async functions to call the Hex API
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");

    // Determine what versions we need
    let (manifest_updated, manifest) = get_manifest(
        paths,
        runtime.handle().clone(),
        mode,
        &config,
        &telemetry,
        use_manifest,
    )?;
    let local = LocalPackages::read_from_disc(paths)?;

    // Remove any packages that are no longer required due to gleam.toml changes
    remove_extra_packages(paths, &local, &manifest, &telemetry)?;

    // Download them from Hex to the local cache
    runtime.block_on(add_missing_packages(
        paths,
        fs,
        &manifest,
        &local,
        project_name,
        &telemetry,
    ))?;

    if manifest_updated {
        // Record new state of the packages directory
        // TODO: test
        tracing::debug!("writing_manifest_toml");
        write_manifest_to_disc(paths, &manifest)?;
    }
    LocalPackages::from_manifest(&manifest).write_to_disc(paths)?;

    Ok(manifest)
}

async fn add_missing_packages<Telem: Telemetry>(
    paths: &ProjectPaths,
    fs: Box<ProjectIO>,
    manifest: &Manifest,
    local: &LocalPackages,
    project_name: SmolStr,
    telemetry: &Telem,
) -> Result<(), Error> {
    let missing_packages = local.missing_local_packages(manifest, &project_name);

    let mut num_to_download = 0;
    let mut missing_hex_packages = missing_packages
        .into_iter()
        .filter(|package| package.is_hex())
        .map(|package| {
            num_to_download += 1;
            package
        })
        .peekable();

    // If we need to download at-least one package
    if missing_hex_packages.peek().is_some() {
        let http = HttpClient::boxed();
        let downloader = hex::Downloader::new(fs.clone(), fs, http, Untar::boxed(), paths.clone());
        let start = Instant::now();
        telemetry.downloading_package("packages");
        downloader
            .download_hex_packages(missing_hex_packages, &project_name)
            .await?;
        telemetry.packages_downloaded(start, num_to_download);
    }

    Ok(())
}

fn remove_extra_packages<Telem: Telemetry>(
    paths: &ProjectPaths,
    local: &LocalPackages,
    manifest: &Manifest,
    telemetry: &Telem,
) -> Result<()> {
    let _guard = BuildLock::lock_all_build(paths, telemetry)?;

    for (package, version) in local.extra_local_packages(manifest) {
        // TODO: test
        // Delete the package source
        let path = paths.build_packages_package(&package);
        if path.exists() {
            tracing::debug!(package=%package, version=%version, "removing_unneeded_package");
            fs::delete_dir(&path)?;
        }

        // TODO: test
        // Delete any build artefacts for the package
        for mode in Mode::iter() {
            for target in Target::iter() {
                let path = paths.build_directory_for_package(mode, target, &package);
                if path.exists() {
                    tracing::debug!(package=%package, version=%version, "deleting_build_cache");
                    fs::delete_dir(&path)?;
                }
            }
        }
    }
    Ok(())
}

fn read_manifest_from_disc(paths: &ProjectPaths) -> Result<Manifest> {
    tracing::debug!("reading_manifest_toml");
    let manifest_path = paths.manifest();
    let toml = crate::fs::read(&manifest_path)?;
    let manifest = toml::from_str(&toml).map_err(|e| Error::FileIo {
        action: FileIoAction::Parse,
        kind: FileKind::File,
        path: manifest_path.clone(),
        err: Some(e.to_string()),
    })?;
    Ok(manifest)
}

fn write_manifest_to_disc(paths: &ProjectPaths, manifest: &Manifest) -> Result<()> {
    let path = paths.manifest();
    fs::write(
        &path,
        &manifest.to_toml(get_current_directory().expect("Could not get the current directory")),
    )
}

// This is the container for locally pinned packages, representing the current contents of
// the `project/build/packages` directory.
// For descriptions of packages provided by paths and git deps, see the ProvidedPackage struct.
// The same package may appear in both at different times.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct LocalPackages {
    packages: HashMap<String, Version>,
}

impl LocalPackages {
    pub fn extra_local_packages(&self, manifest: &Manifest) -> Vec<(String, Version)> {
        let manifest_packages: HashSet<_> = manifest
            .packages
            .iter()
            .map(|p| (&p.name, &p.version))
            .collect();
        self.packages
            .iter()
            .filter(|(n, v)| !manifest_packages.contains(&(&SmolStr::from(n), v)))
            .map(|(n, v)| (n.clone(), v.clone()))
            .collect()
    }

    pub fn missing_local_packages<'a>(
        &self,
        manifest: &'a Manifest,
        root: &str,
    ) -> Vec<&'a ManifestPackage> {
        manifest
            .packages
            .iter()
            // We don't need to download the root package
            .filter(|p| p.name != root)
            // We don't need to download local packages because we use the linked source directly
            .filter(|p| !p.is_local())
            // We don't need to download packages which we have the correct version of
            .filter(|p| self.packages.get(p.name.as_str()) != Some(&p.version))
            .collect()
    }

    pub fn read_from_disc(paths: &ProjectPaths) -> Result<Self> {
        let path = paths.build_packages_toml();
        if !path.exists() {
            return Ok(Self {
                packages: HashMap::new(),
            });
        }
        let toml = crate::fs::read(&path)?;
        toml::from_str(&toml).map_err(|e| Error::FileIo {
            action: FileIoAction::Parse,
            kind: FileKind::File,
            path: path.clone(),
            err: Some(e.to_string()),
        })
    }

    pub fn write_to_disc(&self, paths: &ProjectPaths) -> Result<()> {
        let path = paths.build_packages_toml();
        let toml = toml::to_string(&self).expect("packages.toml serialization");
        fs::write(&path, &toml)
    }

    pub fn from_manifest(manifest: &Manifest) -> Self {
        Self {
            packages: manifest
                .packages
                .iter()
                .map(|p| (p.name.to_string(), p.version.clone()))
                .collect(),
        }
    }
}

#[test]
fn missing_local_packages() {
    let manifest = Manifest {
        requirements: HashMap::new(),
        packages: vec![
            ManifestPackage {
                name: "root".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4]),
                },
            },
            ManifestPackage {
                name: "local1".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4, 5]),
                },
            },
            ManifestPackage {
                name: "local2".into(),
                version: Version::parse("3.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4, 5]),
                },
            },
        ],
    };
    let mut extra = LocalPackages {
        packages: [
            ("local2".into(), Version::parse("2.0.0").unwrap()),
            ("local3".into(), Version::parse("3.0.0").unwrap()),
        ]
        .into(),
    }
    .missing_local_packages(&manifest, "root");
    extra.sort();
    assert_eq!(
        extra,
        [
            &ManifestPackage {
                name: "local1".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4, 5]),
                },
            },
            &ManifestPackage {
                name: "local2".into(),
                version: Version::parse("3.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4, 5]),
                },
            },
        ]
    )
}

#[test]
fn extra_local_packages() {
    let mut extra = LocalPackages {
        packages: [
            ("local1".into(), Version::parse("1.0.0").unwrap()),
            ("local2".into(), Version::parse("2.0.0").unwrap()),
            ("local3".into(), Version::parse("3.0.0").unwrap()),
        ]
        .into(),
    }
    .extra_local_packages(&Manifest {
        requirements: HashMap::new(),
        packages: vec![
            ManifestPackage {
                name: "local1".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4, 5]),
                },
            },
            ManifestPackage {
                name: "local2".into(),
                version: Version::parse("3.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![4, 5]),
                },
            },
        ],
    });
    extra.sort();
    assert_eq!(
        extra,
        [
            ("local2".into(), Version::new(2, 0, 0)),
            ("local3".into(), Version::new(3, 0, 0)),
        ]
    )
}

fn get_manifest<Telem: Telemetry>(
    paths: &ProjectPaths,
    runtime: tokio::runtime::Handle,
    mode: Mode,
    config: &PackageConfig,
    telemetry: &Telem,
    use_manifest: UseManifest,
) -> Result<(bool, Manifest)> {
    // If there's no manifest (or we have been asked not to use it) then resolve
    // the versions anew
    let should_resolve = match use_manifest {
        _ if !paths.manifest().exists() => {
            tracing::debug!("manifest_not_present");
            true
        }
        UseManifest::No => {
            tracing::debug!("ignoring_manifest");
            true
        }
        UseManifest::Yes => false,
    };

    if should_resolve {
        let manifest = resolve_versions(runtime, mode, paths, config, None, telemetry)?;
        return Ok((true, manifest));
    }

    let manifest = read_manifest_from_disc(paths)?;

    // If the config has unchanged since the manifest was written then it is up
    // to date so we can return it unmodified.
    if manifest.requirements == config.all_dependencies()? {
        tracing::debug!("manifest_up_to_date");
        Ok((false, manifest))
    } else {
        tracing::debug!("manifest_outdated");
        let manifest = resolve_versions(runtime, mode, paths, config, Some(&manifest), telemetry)?;
        Ok((true, manifest))
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct ProvidedPackage {
    version: Version,
    source: ProvidedPackageSource,
    requirements: HashMap<SmolStr, hexpm::version::Range>,
}

#[derive(Clone, Eq, Debug)]
enum ProvidedPackageSource {
    Git { repo: SmolStr, commit: SmolStr },
    Local { path: Utf8PathBuf },
}

impl ProvidedPackage {
    fn to_hex_package(&self, name: &SmolStr) -> hexpm::Package {
        let requirements = self
            .requirements
            .iter()
            .map(|(name, version)| {
                (
                    name.as_str().into(),
                    hexpm::Dependency {
                        requirement: version.clone(),
                        optional: false,
                        app: None,
                        repository: None,
                    },
                )
            })
            .collect();
        let release = hexpm::Release {
            version: self.version.clone(),
            requirements,
            retirement_status: None,
            outer_checksum: vec![],
            meta: (),
        };
        hexpm::Package {
            name: name.as_str().into(),
            repository: "local".into(),
            releases: vec![release],
        }
    }

    fn to_manifest_package(&self, name: &str) -> ManifestPackage {
        let mut package = ManifestPackage {
            name: name.into(),
            version: self.version.clone(),
            otp_app: None, // Note, this will probably need to be set to something eventually
            build_tools: vec!["gleam".into()],
            requirements: self.requirements.keys().map(|e| e.to_string()).collect(),
            source: self.source.to_manifest_package_source(),
        };
        package.requirements.sort();
        package
    }
}

impl ProvidedPackageSource {
    fn to_manifest_package_source(&self) -> ManifestPackageSource {
        match self {
            Self::Git { repo, commit } => ManifestPackageSource::Git {
                repo: repo.clone(),
                commit: commit.clone(),
            },
            Self::Local { path } => ManifestPackageSource::Local { path: path.clone() },
        }
    }

    fn to_toml(&self) -> String {
        match self {
            Self::Git { repo, commit } => {
                format!(r#"{{ repo: "{}", commit: "{}" }}"#, repo, commit)
            }
            Self::Local { path } => {
                format!(r#"{{ path: "{}" }}"#, path)
            }
        }
    }
}

impl PartialEq for ProvidedPackageSource {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Local { path: own_path }, Self::Local { path: other_path }) => {
                is_same_file(own_path, other_path).unwrap_or(false)
            }

            (
                Self::Git {
                    repo: own_repo,
                    commit: own_commit,
                },
                Self::Git {
                    repo: other_repo,
                    commit: other_commit,
                },
            ) => own_repo == other_repo && own_commit == other_commit,

            (Self::Git { .. }, Self::Local { .. }) | (Self::Local { .. }, Self::Git { .. }) => {
                false
            }
        }
    }
}

fn resolve_versions<Telem: Telemetry>(
    runtime: tokio::runtime::Handle,
    mode: Mode,
    project_paths: &ProjectPaths,
    config: &PackageConfig,
    manifest: Option<&Manifest>,
    telemetry: &Telem,
) -> Result<Manifest, Error> {
    telemetry.resolving_package_versions();
    let dependencies = config.dependencies_for(mode)?;
    let locked = config.locked(manifest)?;

    // Packages which are provided directly instead of downloaded from hex
    let mut provided_packages = HashMap::new();
    // The version requires of the current project
    let mut root_requirements = HashMap::new();

    // Populate the provided_packages and root_requrements maps
    for (name, requirement) in dependencies.into_iter() {
        let version = match requirement {
            Requirement::Hex { version } => version,
            Requirement::Path { path } => provide_local_package(
                name.clone(),
                &path,
                project_paths.root(),
                project_paths,
                &mut provided_packages,
                &mut vec![],
            )?,
            Requirement::Git { git } => {
                provide_git_package(name.clone(), &git, project_paths, &mut provided_packages)?
            }
        };
        let _ = root_requirements.insert(name, version);
    }

    // Convert provided packages into hex packages for pub-grub resolve
    let provided_hex_packages = provided_packages
        .iter()
        .map(|(name, package)| (name.clone(), package.to_hex_package(name)))
        .collect();

    let resolved = dependency::resolve_versions(
        PackageFetcher::boxed(runtime.clone()),
        provided_hex_packages,
        config.name.clone(),
        root_requirements.into_iter(),
        &locked,
    )?;

    // Convert the hex packages and local packages into manfiest packages
    let manifest_packages = runtime.block_on(future::try_join_all(
        resolved
            .into_iter()
            .map(|(name, version)| lookup_package(name, version, &provided_packages)),
    ))?;

    let manifest = Manifest {
        packages: manifest_packages,
        requirements: config.all_dependencies()?,
    };

    Ok(manifest)
}

/// Provide a package from a local project
fn provide_local_package(
    package_name: SmolStr,
    package_path: &Utf8Path,
    parent_path: &Utf8Path,
    project_paths: &ProjectPaths,
    provided: &mut HashMap<SmolStr, ProvidedPackage>,
    parents: &mut Vec<SmolStr>,
) -> Result<hexpm::version::Range> {
    let package_path = if package_path.is_absolute() {
        package_path.to_path_buf()
    } else {
        fs::canonicalise(&parent_path.join(package_path))?
    };
    let package_source = ProvidedPackageSource::Local {
        path: package_path.clone(),
    };
    provide_package(
        package_name,
        package_path,
        package_source,
        project_paths,
        provided,
        parents,
    )
}

/// Provide a package from a git repository
fn provide_git_package(
    _package_name: SmolStr,
    _repo: &str,
    _project_paths: &ProjectPaths,
    _provided: &mut HashMap<SmolStr, ProvidedPackage>,
) -> Result<hexpm::version::Range> {
    let _git = ProvidedPackageSource::Git {
        repo: SmolStr::new_inline("repo"),
        commit: SmolStr::new_inline("commit"),
    };
    Err(Error::GitDependencyUnsuported)
}

/// Adds a gleam project located at a specific path to the list of "provided packages"
fn provide_package(
    package_name: SmolStr,
    package_path: Utf8PathBuf,
    package_source: ProvidedPackageSource,
    project_paths: &ProjectPaths,
    provided: &mut HashMap<SmolStr, ProvidedPackage>,
    parents: &mut Vec<SmolStr>,
) -> Result<hexpm::version::Range> {
    // Return early if a package cyle is detected
    if parents.contains(&package_name) {
        let mut last_cycle = parents
            .split(|p| p == &package_name)
            .last()
            .unwrap_or_default()
            .to_vec();
        last_cycle.push(package_name);
        return Err(Error::PackageCycle {
            packages: last_cycle,
        });
    }
    // Check that we do not have a cached version of this package already
    match provided.get(&package_name) {
        Some(package) if package.source == package_source => {
            // This package has already been provided from this source, return the version
            let version = hexpm::version::Range::new(format!("== {}", &package.version));
            return Ok(version);
        }
        Some(package) => {
            // This package has already been provided from a different source which conflicts
            return Err(Error::ProvidedDependencyConflict {
                package: package_name.into(),
                source_1: package_source.to_toml(),
                source_2: package.source.to_toml(),
            });
        }
        None => (),
    }
    // Load the package
    let config = crate::config::read(package_path.join("gleam.toml"))?;
    // Check that we are loading the correct project
    if config.name != package_name {
        return Err(Error::WrongDependencyProvided {
            expected: package_name.into(),
            path: package_path.to_path_buf(),
            found: config.name.into(),
        });
    };
    // Walk the requirements of the package
    let mut requirements = HashMap::new();
    parents.push(package_name);
    for (name, requirement) in config.dependencies.into_iter() {
        let name = name;
        let version = match requirement {
            Requirement::Hex { version } => version,
            Requirement::Path { path } => {
                // Recursively walk local packages
                provide_local_package(
                    name.clone(),
                    &path,
                    &package_path,
                    project_paths,
                    provided,
                    parents,
                )?
            }
            Requirement::Git { git } => {
                provide_git_package(name.clone(), &git, project_paths, provided)?
            }
        };
        let _ = requirements.insert(name, version);
    }
    let _ = parents.pop();
    // Add the package to the provided packages dictionary
    let version = hexpm::version::Range::new(format!("== {}", &config.version));
    let _ = provided.insert(
        config.name,
        ProvidedPackage {
            version: config.version,
            source: package_source,
            requirements,
        },
    );
    // Return the version
    Ok(version)
}

#[test]
fn provide_wrong_package() {
    let mut provided = HashMap::new();
    let project_paths = crate::project_paths_at_current_directory();
    let result = provide_local_package(
        "wrong_name".into(),
        Utf8Path::new("./test/hello_world"),
        Utf8Path::new("./"),
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "subpackage".into()],
    );
    if let Err(Error::WrongDependencyProvided {
        expected, found, ..
    }) = result
    {
        assert_eq!(expected, "wrong_name");
        assert_eq!(found, "hello_world");
    } else {
        panic!("Expected WrongDependencyProvided error")
    }
}

#[test]
fn provide_existing_package() {
    let mut provided = HashMap::new();
    let project_paths = crate::project_paths_at_current_directory();

    let result = provide_local_package(
        "hello_world".into(),
        Utf8Path::new("./test/hello_world"),
        Utf8Path::new("./"),
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "subpackage".into()],
    );
    assert_eq!(result, Ok(hexpm::version::Range::new("== 0.1.0".into())));

    let result = provide_local_package(
        "hello_world".into(),
        Utf8Path::new("./test/hello_world"),
        Utf8Path::new("./"),
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "subpackage".into()],
    );
    assert_eq!(result, Ok(hexpm::version::Range::new("== 0.1.0".into())));
}

#[test]
fn provide_conflicting_package() {
    let mut provided = HashMap::new();
    let project_paths = crate::project_paths_at_current_directory();
    let result = provide_local_package(
        "hello_world".into(),
        Utf8Path::new("./test/hello_world"),
        Utf8Path::new("./"),
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "subpackage".into()],
    );
    assert_eq!(result, Ok(hexpm::version::Range::new("== 0.1.0".into())));

    let result = provide_package(
        "hello_world".into(),
        Utf8PathBuf::from("./test/other"),
        ProvidedPackageSource::Local {
            path: Utf8Path::new("./test/other").to_path_buf(),
        },
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "subpackage".into()],
    );
    if let Err(Error::ProvidedDependencyConflict { package, .. }) = result {
        assert_eq!(package, "hello_world");
    } else {
        panic!("Expected ProvidedDependencyConflict error")
    }
}

#[test]
fn provided_is_absolute() {
    let mut provided = HashMap::new();
    let project_paths = crate::project_paths_at_current_directory();
    let result = provide_local_package(
        "hello_world".into(),
        Utf8Path::new("./test/hello_world"),
        Utf8Path::new("./"),
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "subpackage".into()],
    );
    assert_eq!(result, Ok(hexpm::version::Range::new("== 0.1.0".into())));
    let package = provided.get("hello_world").unwrap().clone();
    if let ProvidedPackageSource::Local { path } = package.source {
        assert!(path.is_absolute())
    } else {
        panic!("Provide_local_package provided a package that is not local!")
    }
}

#[test]
fn provided_recursive() {
    let mut provided = HashMap::new();
    let project_paths = crate::project_paths_at_current_directory();
    let result = provide_local_package(
        "hello_world".into(),
        Utf8Path::new("./test/hello_world"),
        Utf8Path::new("./"),
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "hello_world".into(), "subpackage".into()],
    );
    assert_eq!(
        result,
        Err(Error::PackageCycle {
            packages: vec!["subpackage".into(), "hello_world".into()],
        })
    )
}

/// Determine the information to add to the manifest for a specific package
async fn lookup_package(
    name: String,
    version: Version,
    provided: &HashMap<SmolStr, ProvidedPackage>,
) -> Result<ManifestPackage> {
    match provided.get(name.as_str()) {
        Some(provided_package) => Ok(provided_package.to_manifest_package(name.as_str())),
        None => {
            let config = hexpm::Config::new();
            let release =
                hex::get_package_release(&name, &version, &config, &HttpClient::new()).await?;
            Ok(ManifestPackage {
                name: name.into(),
                version,
                otp_app: Some(release.meta.app),
                build_tools: release.meta.build_tools,
                requirements: release.requirements.keys().cloned().collect_vec(),
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(release.outer_checksum),
                },
            })
        }
    }
}

struct PackageFetcher {
    runtime: tokio::runtime::Handle,
    http: HttpClient,
}

impl PackageFetcher {
    pub fn boxed(runtime: tokio::runtime::Handle) -> Box<Self> {
        Box::new(Self {
            runtime,
            http: HttpClient::new(),
        })
    }
}

#[derive(Debug)]
pub struct Untar;

impl Untar {
    pub fn boxed() -> Box<Self> {
        Box::new(Self)
    }
}

impl TarUnpacker for Untar {
    fn io_result_entries<'a>(
        &self,
        archive: &'a mut tar::Archive<WrappedReader>,
    ) -> std::io::Result<tar::Entries<'a, WrappedReader>> {
        archive.entries()
    }

    fn io_result_unpack(
        &self,
        path: &Utf8Path,
        mut archive: tar::Archive<GzDecoder<tar::Entry<'_, WrappedReader>>>,
    ) -> std::io::Result<()> {
        archive.unpack(path)
    }
}

impl dependency::PackageFetcher for PackageFetcher {
    fn get_dependencies(
        &self,
        package: &str,
    ) -> Result<hexpm::Package, Box<dyn std::error::Error>> {
        tracing::debug!(package = package, "looking_up_hex_package");
        let config = hexpm::Config::new();
        let request = hexpm::get_package_request(package, None, &config);
        let response = self
            .runtime
            .block_on(self.http.send(request))
            .map_err(Box::new)?;
        hexpm::get_package_response(response, HEXPM_PUBLIC_KEY).map_err(|e| e.into())
    }
}

#[test]
fn provided_local_to_hex() {
    let provided_package = ProvidedPackage {
        version: hexpm::version::Version::new(1, 0, 0),
        source: ProvidedPackageSource::Local {
            path: "canonical/path/to/package".into(),
        },
        requirements: [
            (
                "req_1".into(),
                hexpm::version::Range::new("~> 1.0.0".into()),
            ),
            (
                "req_2".into(),
                hexpm::version::Range::new("== 1.0.0".into()),
            ),
        ]
        .into(),
    };

    let hex_package = hexpm::Package {
        name: "package".into(),
        repository: "local".into(),
        releases: vec![hexpm::Release {
            version: hexpm::version::Version::new(1, 0, 0),
            retirement_status: None,
            outer_checksum: vec![],
            meta: (),
            requirements: [
                (
                    "req_1".into(),
                    hexpm::Dependency {
                        requirement: hexpm::version::Range::new("~> 1.0.0".into()),
                        optional: false,
                        app: None,
                        repository: None,
                    },
                ),
                (
                    "req_2".into(),
                    hexpm::Dependency {
                        requirement: hexpm::version::Range::new("== 1.0.0".into()),
                        optional: false,
                        app: None,
                        repository: None,
                    },
                ),
            ]
            .into(),
        }],
    };

    assert_eq!(
        provided_package.to_hex_package(&SmolStr::new_inline("package")),
        hex_package
    );
}

#[test]
fn provided_git_to_hex() {
    let provided_package = ProvidedPackage {
        version: hexpm::version::Version::new(1, 0, 0),
        source: ProvidedPackageSource::Git {
            repo: "https://github.com/gleam-lang/gleam.git".into(),
            commit: "bd9fe02f72250e6a136967917bcb1bdccaffa3c8".into(),
        },
        requirements: [
            (
                "req_1".into(),
                hexpm::version::Range::new("~> 1.0.0".into()),
            ),
            (
                "req_2".into(),
                hexpm::version::Range::new("== 1.0.0".into()),
            ),
        ]
        .into(),
    };

    let hex_package = hexpm::Package {
        name: "package".into(),
        repository: "local".into(),
        releases: vec![hexpm::Release {
            version: hexpm::version::Version::new(1, 0, 0),
            retirement_status: None,
            outer_checksum: vec![],
            meta: (),
            requirements: [
                (
                    "req_1".into(),
                    hexpm::Dependency {
                        requirement: hexpm::version::Range::new("~> 1.0.0".into()),
                        optional: false,
                        app: None,
                        repository: None,
                    },
                ),
                (
                    "req_2".into(),
                    hexpm::Dependency {
                        requirement: hexpm::version::Range::new("== 1.0.0".into()),
                        optional: false,
                        app: None,
                        repository: None,
                    },
                ),
            ]
            .into(),
        }],
    };

    assert_eq!(
        provided_package.to_hex_package(&SmolStr::new_inline("package")),
        hex_package
    );
}

#[test]
fn provided_local_to_manifest() {
    let provided_package = ProvidedPackage {
        version: hexpm::version::Version::new(1, 0, 0),
        source: ProvidedPackageSource::Local {
            path: "canonical/path/to/package".into(),
        },
        requirements: [
            (
                "req_1".into(),
                hexpm::version::Range::new("~> 1.0.0".into()),
            ),
            (
                "req_2".into(),
                hexpm::version::Range::new("== 1.0.0".into()),
            ),
        ]
        .into(),
    };

    let manifest_package = ManifestPackage {
        name: "package".into(),
        version: hexpm::version::Version::new(1, 0, 0),
        otp_app: None,
        build_tools: vec!["gleam".into()],
        requirements: vec!["req_1".into(), "req_2".into()],
        source: ManifestPackageSource::Local {
            path: "canonical/path/to/package".into(),
        },
    };

    assert_eq!(
        provided_package.to_manifest_package(&SmolStr::new_inline("package")),
        manifest_package
    );
}

#[test]
fn provided_git_to_manifest() {
    let provided_package = ProvidedPackage {
        version: hexpm::version::Version::new(1, 0, 0),
        source: ProvidedPackageSource::Git {
            repo: "https://github.com/gleam-lang/gleam.git".into(),
            commit: "bd9fe02f72250e6a136967917bcb1bdccaffa3c8".into(),
        },
        requirements: [
            (
                "req_1".into(),
                hexpm::version::Range::new("~> 1.0.0".into()),
            ),
            (
                "req_2".into(),
                hexpm::version::Range::new("== 1.0.0".into()),
            ),
        ]
        .into(),
    };

    let manifest_package = ManifestPackage {
        name: "package".into(),
        version: hexpm::version::Version::new(1, 0, 0),
        otp_app: None,
        build_tools: vec!["gleam".into()],
        requirements: vec!["req_1".into(), "req_2".into()],
        source: ManifestPackageSource::Git {
            repo: "https://github.com/gleam-lang/gleam.git".into(),
            commit: "bd9fe02f72250e6a136967917bcb1bdccaffa3c8".into(),
        },
    };

    assert_eq!(
        provided_package.to_manifest_package(&SmolStr::new_inline("package")),
        manifest_package
    );
}
