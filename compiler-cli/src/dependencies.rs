mod dependency_manager;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    io::ErrorKind,
    process::Command,
    rc::Rc,
    time::Instant,
};

use camino::{Utf8Path, Utf8PathBuf};
use ecow::{EcoString, eco_format};
use flate2::read::GzDecoder;
use gleam_core::{
    Error, Result,
    build::{Mode, Target, Telemetry},
    config::PackageConfig,
    dependency::{self, PackageFetchError},
    error::{FileIoAction, FileKind, ShellCommandFailureReason, StandardIoAction},
    hex::{self, HEXPM_PUBLIC_KEY},
    io::{HttpClient as _, TarUnpacker, WrappedReader},
    manifest::{Base16Checksum, Manifest, ManifestPackage, ManifestPackageSource, PackageChanges},
    paths::ProjectPaths,
    requirement::Requirement,
};
use hexpm::version::Version;
use itertools::Itertools;
use same_file::is_same_file;
use strum::IntoEnumIterator;

pub use dependency_manager::DependencyManagerConfig;

#[cfg(test)]
mod tests;

use crate::{
    TreeOptions,
    build_lock::{BuildLock, Guard},
    cli,
    fs::{self, ProjectIO},
    http::HttpClient,
    text_layout::space_table,
};

struct Symbols {
    down: &'static str,
    tee: &'static str,
    ell: &'static str,
    right: &'static str,
}

static UTF8_SYMBOLS: Symbols = Symbols {
    down: "│",
    tee: "├",
    ell: "└",
    right: "─",
};

/// When set to `Yes`, the cli will check for major version updates of direct dependencies and
/// print them to the console if the major versions are not upgradeable due to constraints.
#[derive(Debug, Clone, Copy)]
pub enum CheckMajorVersions {
    Yes,
    No,
}

pub fn list(paths: &ProjectPaths) -> Result<()> {
    let (_, manifest) = get_manifest_details(paths)?;
    list_manifest_packages(std::io::stdout(), manifest)
}

pub fn tree(paths: &ProjectPaths, options: TreeOptions) -> Result<()> {
    let (config, manifest) = get_manifest_details(paths)?;

    // Initialize the root package since it is not part of the manifest
    let root_package = ManifestPackage {
        build_tools: vec![],
        name: config.name.clone(),
        requirements: config.all_direct_dependencies()?.keys().cloned().collect(),
        version: config.version.clone(),
        source: ManifestPackageSource::Local {
            path: paths.root().to_path_buf(),
        },
        otp_app: None,
    };

    // Get the manifest packages and add the root package to the vec
    let mut packages = manifest.packages.iter().cloned().collect_vec();
    packages.append(&mut vec![root_package.clone()]);

    list_package_and_dependencies_tree(std::io::stdout(), options, packages.clone(), config.name)
}

fn get_manifest_details(paths: &ProjectPaths) -> Result<(PackageConfig, Manifest)> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let config = crate::config::root_config(paths)?;
    let package_fetcher = PackageFetcher::new(runtime.handle().clone());
    let dependency_manager = DependencyManagerConfig {
        use_manifest: UseManifest::Yes,
        check_major_versions: CheckMajorVersions::No,
    }
    .into_dependency_manager(
        runtime.handle().clone(),
        package_fetcher,
        cli::Reporter::new(),
        Mode::Dev,
    );
    let manifest = dependency_manager
        .resolve_versions(paths, &config, Vec::new())?
        .manifest;
    Ok((config, manifest))
}

fn list_manifest_packages<W: std::io::Write>(mut buffer: W, manifest: Manifest) -> Result<()> {
    let packages = manifest
        .packages
        .into_iter()
        .map(|package| vec![package.name.to_string(), package.version.to_string()])
        .collect_vec();
    let out = space_table(&["Package", "Version"], packages);

    write!(buffer, "{out}").map_err(|e| Error::StandardIo {
        action: StandardIoAction::Write,
        err: Some(e.kind()),
    })
}

fn list_package_and_dependencies_tree<W: std::io::Write>(
    mut buffer: W,
    options: TreeOptions,
    packages: Vec<ManifestPackage>,
    root_package_name: EcoString,
) -> Result<()> {
    let mut invert = false;

    let package: Option<&ManifestPackage> = if let Some(input_package_name) = options.package {
        packages.iter().find(|p| p.name == input_package_name)
    } else if let Some(input_package_name) = options.invert {
        invert = true;
        packages.iter().find(|p| p.name == input_package_name)
    } else {
        packages.iter().find(|p| p.name == root_package_name)
    };

    if let Some(package) = package {
        let tree = Vec::from([eco_format!("{0} v{1}", package.name, package.version)]);
        let tree = list_dependencies_tree(
            tree.clone(),
            package.clone(),
            packages,
            EcoString::new(),
            invert,
        );

        tree.iter()
            .try_for_each(|line| writeln!(buffer, "{line}"))
            .map_err(|e| Error::StandardIo {
                action: StandardIoAction::Write,
                err: Some(e.kind()),
            })
    } else {
        writeln!(buffer, "Package not found. Please check the package name.").map_err(|e| {
            Error::StandardIo {
                action: StandardIoAction::Write,
                err: Some(e.kind()),
            }
        })
    }
}

fn list_dependencies_tree(
    mut tree: Vec<EcoString>,
    package: ManifestPackage,
    packages: Vec<ManifestPackage>,
    accum: EcoString,
    invert: bool,
) -> Vec<EcoString> {
    let dependencies = packages
        .iter()
        .filter(|p| {
            (invert && p.requirements.contains(&package.name))
                || (!invert && package.requirements.contains(&p.name))
        })
        .cloned()
        .collect_vec();

    let dependencies = dependencies.iter().sorted().enumerate();

    let deps_length = dependencies.len();
    for (index, dependency) in dependencies {
        let is_last = index == deps_length - 1;
        let prefix = if is_last {
            UTF8_SYMBOLS.ell
        } else {
            UTF8_SYMBOLS.tee
        };

        tree.push(eco_format!(
            "{0}{1}{2}{2} {3} v{4}",
            accum.clone(),
            prefix,
            UTF8_SYMBOLS.right,
            dependency.name,
            dependency.version
        ));

        let accum = accum.clone() + (if !is_last { UTF8_SYMBOLS.down } else { " " }) + "   ";

        tree = list_dependencies_tree(
            tree.clone(),
            dependency.clone(),
            packages.clone(),
            accum.clone(),
            invert,
        );
    }

    tree
}

pub fn outdated(paths: &ProjectPaths) -> Result<()> {
    let (_, manifest) = get_manifest_details(paths)?;

    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let package_fetcher = PackageFetcher::new(runtime.handle().clone());

    let version_updates = dependency::check_for_version_updates(&manifest, &package_fetcher);

    if !version_updates.is_empty() {
        print!("{}", pretty_print_version_updates(version_updates));
    }

    Ok(())
}

#[derive(Debug, Clone, Copy)]
pub enum UseManifest {
    Yes,
    No,
}

pub fn update(paths: &ProjectPaths, packages: Vec<String>) -> Result<()> {
    let use_manifest = if packages.is_empty() {
        UseManifest::No
    } else {
        UseManifest::Yes
    };

    // Update specific packages
    _ = resolve_and_download(
        paths,
        cli::Reporter::new(),
        None,
        packages.into_iter().map(EcoString::from).collect(),
        DependencyManagerConfig {
            use_manifest,
            check_major_versions: CheckMajorVersions::Yes,
        },
    )?;

    Ok(())
}

/// Edit the manifest.toml file in this proejct, removing all extra requirements and packages
/// that are no longer present in the gleam.toml config.
pub fn cleanup<Telem: Telemetry>(paths: &ProjectPaths, telemetry: Telem) -> Result<Manifest> {
    let span = tracing::info_span!("remove_deps");
    let _enter = span.enter();

    // We do this before acquiring the build lock so that we don't create the
    // build directory if there is no gleam.toml
    crate::config::ensure_config_exists(paths)?;

    let lock = BuildLock::new_packages(paths)?;
    let _guard: Guard = lock.lock(&telemetry)?;

    // Read the project config
    let config = crate::root_config(paths)?;
    let old_manifest = read_manifest_from_disc(paths)?;
    let mut manifest = old_manifest.clone();

    remove_extra_requirements(&config, &mut manifest)?;

    // Remove any packages that are no longer required due to manifest changes
    let local = LocalPackages::read_from_disc(paths)?;
    remove_extra_packages(paths, &local, &manifest, &telemetry)?;

    // Record new state of the packages directory
    tracing::debug!("writing_manifest_toml");
    write_manifest_to_disc(paths, &manifest)?;
    LocalPackages::from_manifest(&manifest).write_to_disc(paths)?;

    let changes = PackageChanges::between_manifests(&old_manifest, &manifest);
    telemetry.resolved_package_versions(&changes);

    Ok(manifest)
}

/// Remove requirements and unneeded packages from manifest that are no longer present in config.
fn remove_extra_requirements(config: &PackageConfig, manifest: &mut Manifest) -> Result<()> {
    // "extra requirements" are all packages that are requirements in the manifest, but no longer
    // part of the gleam.toml config.
    let is_extra_requirement = |name: &EcoString| {
        !config.dev_dependencies.contains_key(name) && !config.dependencies.contains_key(name)
    };

    // If a requirement is also used as a dependency, we do not want to force-unlock it.
    // If the dependents get deleted as well, this transitive dependency will be dropped.
    let is_unlockable_requirement = |name: &EcoString| {
        manifest
            .packages
            .iter()
            .all(|p| !p.requirements.contains(name))
    };

    let extra_requirements = manifest
        .requirements
        .keys()
        .filter(|&name| is_extra_requirement(name) && is_unlockable_requirement(name))
        .cloned()
        .collect::<Vec<_>>();

    manifest
        .requirements
        .retain(|name, _| !is_extra_requirement(name));

    // Unlock all packages that we we want to remove - this removes them and all unneeded
    // dependencies from `locked`.
    let mut locked = config.locked(Some(manifest))?;
    unlock_packages(&mut locked, extra_requirements.as_slice(), Some(manifest))?;
    // Remove all unlocked packages from the manifest - these are truly no longer needed.
    manifest
        .packages
        .retain(|package| locked.contains_key(&package.name));

    Ok(())
}

pub fn parse_gleam_add_specifier(package: &str) -> Result<(EcoString, Requirement)> {
    let Some((package, version)) = package.split_once('@') else {
        // Default to the latest version available.
        return Ok((
            package.into(),
            Requirement::hex(">= 0.0.0").expect("'>= 0.0.0' should be a valid pubgrub range"),
        ));
    };

    // Parse the major and minor from the provided semantic version.
    let parts = version.split('.').collect::<Vec<_>>();
    let major = match parts.first() {
        Some(major) => Ok(major),
        None => Err(Error::InvalidVersionFormat {
            input: package.to_string(),
            error: "Failed to parse semantic major version".to_string(),
        }),
    }?;
    let minor = match parts.get(1) {
        Some(minor) => minor,
        None => "0",
    };

    // Using the major version specifier, calculate the maximum
    // allowable version (i.e., the next major version).
    let Ok(num) = major.parse::<usize>() else {
        return Err(Error::InvalidVersionFormat {
            input: version.to_string(),
            error: "Failed to parse semantic major version as integer".to_string(),
        });
    };

    let max_ver = [&(num + 1).to_string(), "0", "0"].join(".");

    // Pad the provided version specifier with zeros map to a Hex version.
    let requirement = match parts.len() {
        1 | 2 => {
            let min_ver = [major, minor, "0"].join(".");
            Requirement::hex(&[">=", &min_ver, "and", "<", &max_ver].join(" "))
        }
        3 => Requirement::hex(version),
        n => {
            return Err(Error::InvalidVersionFormat {
                input: version.to_string(),
                error: format!(
                    "Expected up to 3 numbers in version specifier (MAJOR.MINOR.PATCH), found {n}"
                ),
            });
        }
    }?;

    Ok((package.into(), requirement))
}

pub fn resolve_and_download<Telem: Telemetry>(
    paths: &ProjectPaths,
    telemetry: Telem,
    new_package: Option<(Vec<(EcoString, Requirement)>, bool)>,
    packages_to_update: Vec<EcoString>,
    config: DependencyManagerConfig,
) -> Result<Manifest> {
    // Start event loop so we can run async functions to call the Hex API
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let package_fetcher = PackageFetcher::new(runtime.handle().clone());

    let dependency_manager = config.into_dependency_manager(
        runtime.handle().clone(),
        package_fetcher,
        telemetry,
        Mode::Dev,
    );

    dependency_manager.resolve_and_download_versions(paths, new_package, packages_to_update)
}

fn format_versions_and_extract_longest_parts(
    versions: dependency::PackageVersionDiffs,
) -> Vec<Vec<String>> {
    versions
        .iter()
        .map(|(name, (v1, v2))| vec![name.to_string(), v1.to_string(), v2.to_string()])
        .sorted()
        .collect_vec()
}

fn pretty_print_major_versions_available(versions: dependency::PackageVersionDiffs) -> String {
    let versions = format_versions_and_extract_longest_parts(versions);

    format!(
        "\nThe following dependencies have new major versions available:\n\n{}",
        space_table(&["Package", "Current", "Latest"], &versions)
    )
}

fn pretty_print_version_updates(versions: dependency::PackageVersionDiffs) -> EcoString {
    let versions = format_versions_and_extract_longest_parts(versions);
    space_table(&["Package", "Current", "Latest"], &versions)
}

async fn add_missing_packages<Telem: Telemetry>(
    paths: &ProjectPaths,
    fs: Box<ProjectIO>,
    manifest: &Manifest,
    local: &LocalPackages,
    project_name: EcoString,
    telemetry: &Telem,
) -> Result<(), Error> {
    let missing_packages = local.missing_local_packages(manifest, &project_name);

    let mut num_to_download = 0;

    let missing_git_packages = missing_packages
        .iter()
        .copied()
        .filter(|package| package.is_git())
        .inspect(|_| {
            num_to_download += 1;
        })
        .collect_vec();

    let mut missing_hex_packages = missing_packages
        .iter()
        .copied()
        .filter(|package| package.is_hex())
        .inspect(|_| {
            num_to_download += 1;
        })
        .peekable();

    // If we need to download at-least one package
    if missing_hex_packages.peek().is_some() || !missing_git_packages.is_empty() {
        let http = HttpClient::boxed();
        let downloader = hex::Downloader::new(fs.clone(), fs, http, Untar::boxed(), paths.clone());
        let start = Instant::now();
        telemetry.downloading_package("packages");
        downloader
            .download_hex_packages(missing_hex_packages, &project_name)
            .await?;
        for package in missing_git_packages {
            let ManifestPackageSource::Git { repo, commit } = &package.source else {
                continue;
            };
            let _ = download_git_package(&package.name, repo, commit, paths)?;
        }
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

    for (package_name, version) in local.extra_local_packages(manifest) {
        // TODO: test
        // Delete the package source
        let path = paths.build_packages_package(&package_name);
        if path.exists() {
            tracing::debug!(package=%package_name, version=%version, "removing_unneeded_package");
            fs::delete_directory(&path)?;
        }

        // TODO: test
        // Delete any build artefacts for the package
        for mode in Mode::iter() {
            for target in Target::iter() {
                let name = manifest
                    .packages
                    .iter()
                    .find(|p| p.name == package_name)
                    .map(|p| p.application_name().as_str())
                    .unwrap_or(package_name.as_str());
                let path = paths.build_directory_for_package(mode, target, name);
                if path.exists() {
                    tracing::debug!(package=%package_name, version=%version, "deleting_build_cache");
                    fs::delete_directory(&path)?;
                }
            }
        }
    }
    Ok(())
}

fn read_manifest_from_disc(paths: &ProjectPaths) -> Result<Manifest> {
    tracing::debug!("reading_manifest_toml");
    let manifest_path = paths.manifest();
    let toml = fs::read(&manifest_path)?;
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
    fs::write(&path, &manifest.to_toml(paths.root()))
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
            .filter(|(n, v)| !manifest_packages.contains(&(&EcoString::from(*n), v)))
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
        let toml = fs::read(&path)?;
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

fn is_same_requirements(
    requirements1: &HashMap<EcoString, Requirement>,
    requirements2: &HashMap<EcoString, Requirement>,
    root_path: &Utf8Path,
) -> Result<bool> {
    if requirements1.len() != requirements2.len() {
        return Ok(false);
    }

    for (key, requirement1) in requirements1 {
        if !same_requirements(requirement1, requirements2.get(key), root_path)? {
            return Ok(false);
        }
    }

    // For path dependencies, we need to check if their manifest.toml files have changed,
    // which indicates that their dependencies have changed
    for (key, requirement2) in requirements2 {
        if let Requirement::Path { path } = requirement2 {
            let dep_manifest_path = root_path.join(path).join("manifest.toml");

            // Check if we have a cached hash for this path dependency's manifest.toml
            // Use the dependency name instead of the path for the manifest hash file
            let cached_hash_path = root_path
                .join("build")
                .join("packages")
                .join(format!("{}.manifest_hash", key));

            if !dep_manifest_path.exists() {
                tracing::debug!("path_dependency_manifest_not_found_forcing_rebuild");
                return Ok(false);
            }

            let manifest_content = match fs::read(&dep_manifest_path) {
                Ok(content) => content,
                Err(_) => {
                    tracing::debug!("cannot_read_path_dependency_manifest_forcing_rebuild");
                    return Ok(false);
                }
            };

            let mut hasher = std::hash::DefaultHasher::new();
            std::hash::Hash::hash(&manifest_content, &mut hasher);
            let current_hash = std::hash::Hasher::finish(&hasher).to_string();

            // If cached hash file doesn't exist, this is the first time we're checking this dependency
            if !cached_hash_path.exists() {
                // Save the current hash for future comparisons
                if let Err(e) = fs::write(&cached_hash_path, &current_hash) {
                    tracing::debug!("failed_to_write_dependency_manifest_hash: {}", e);
                }
                tracing::debug!("no_cached_manifest_hash_for_path_dependency_forcing_rebuild");
                return Ok(false);
            }

            let cached_hash = match std::fs::read_to_string(&cached_hash_path) {
                Ok(content) => content,
                Err(_) => {
                    tracing::debug!("cannot_read_cached_manifest_hash_forcing_rebuild");
                    return Ok(false);
                }
            };

            if cached_hash != current_hash {
                tracing::debug!("path_dependency_manifest_changed_forcing_rebuild");
                if let Err(e) = fs::write(&cached_hash_path, &current_hash) {
                    tracing::debug!("failed_to_update_dependency_manifest_hash: {}", e);
                }
                return Ok(false);
            }

            tracing::debug!("path_dependency_manifest_unchanged_no_rebuild_needed");
        }
    }

    Ok(true)
}

fn same_requirements(
    requirement1: &Requirement,
    requirement2: Option<&Requirement>,
    root_path: &Utf8Path,
) -> Result<bool> {
    let (left, right) = match (requirement1, requirement2) {
        (Requirement::Path { path: path1 }, Some(Requirement::Path { path: path2 })) => {
            let left = fs::canonicalise(&root_path.join(path1))?;
            let right = fs::canonicalise(&root_path.join(path2))?;
            (left, right)
        }
        (_, Some(requirement2)) => return Ok(requirement1 == requirement2),
        (_, None) => return Ok(false),
    };

    Ok(left == right)
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct ProvidedPackage {
    version: Version,
    source: ProvidedPackageSource,
    requirements: HashMap<EcoString, hexpm::version::Range>,
}

#[derive(Clone, Eq, Debug)]
enum ProvidedPackageSource {
    Git { repo: EcoString, commit: EcoString },
    Local { path: Utf8PathBuf },
}

impl ProvidedPackage {
    fn to_hex_package(&self, name: &EcoString) -> hexpm::Package {
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
            requirements: self.requirements.keys().cloned().collect(),
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
                format!(r#"{{ repo: "{repo}", commit: "{commit}" }}"#)
            }
            Self::Local { path } => {
                format!(r#"{{ path: "{path}" }}"#)
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

/// Provide a package from a local project
fn provide_local_package(
    package_name: EcoString,
    package_path: &Utf8Path,
    parent_path: &Utf8Path,
    project_paths: &ProjectPaths,
    provided: &mut HashMap<EcoString, ProvidedPackage>,
    parents: &mut Vec<EcoString>,
) -> Result<hexpm::version::Range> {
    let package_path = if package_path.is_absolute() {
        package_path.to_path_buf()
    } else {
        fs::canonicalise(&parent_path.join(package_path))?
    };

    // We used to always force clean artifacts for path dependencies,
    // but now we use the gleam.toml hash tracking in is_same_requirements
    // to determine when path dependency dependencies have changed

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

fn execute_command(command: &mut Command) -> Result<std::process::Output> {
    let result = command.output();
    match result {
        Ok(output) if output.status.success() => Ok(output),
        Ok(output) => {
            let reason = match String::from_utf8(output.stderr) {
                Ok(stderr) => ShellCommandFailureReason::ShellCommandError(stderr),
                Err(_) => ShellCommandFailureReason::Unknown,
            };
            Err(Error::ShellCommand {
                program: "git".into(),
                reason,
            })
        }
        Err(error) => Err(match error.kind() {
            ErrorKind::NotFound => Error::ShellProgramNotFound {
                program: "git".into(),
                os: fs::get_os(),
            },

            other => Error::ShellCommand {
                program: "git".into(),
                reason: ShellCommandFailureReason::IoError(other),
            },
        }),
    }
}

/// Downloads a git package from a remote repository. The commands that are run
/// looks like this:
///
/// ```sh
/// git init
/// git remote remove origin
/// git remote add origin <repo>
/// git fetch origin
/// git checkout <ref>
/// git rev-parse HEAD
/// ```
///
/// This is somewhat inefficient as we have to fetch the entire git history before
/// switching to the exact commit we want. There a few alternatives to this:
///
/// - `git clone --depth 1 --branch="<ref>"` This works, but only allows us to use
///   branch names as refs, however we want to allow commit hashes as well.
/// - `git fetch --depth 1 origin <ref>` Similarly, this imposes an unwanted
///   restriction. `git fetch` only allows branch names or full commit hashes,
///   but we want to allow partial hashes as well.
///
/// Since Git dependencies will be used quite rarely, this option was settled upon
/// because it allows branch names, full and partial commit hashes as refs.
///
/// In the future we can optimise this more, for example first checking if we
/// are already checked out to the commit stored in the manifest, or by only
/// fetching the history without the objects to resolve partial commit hashes.
/// For now though this is good enough until it become an actual performance
/// problem.
///
fn download_git_package(
    package_name: &str,
    repo: &str,
    ref_: &str,
    project_paths: &ProjectPaths,
) -> Result<EcoString> {
    let package_path = project_paths.build_packages_package(package_name);

    // If the package path exists but is not inside a git work tree, we need to
    // remove the directory because running `git init` in a non-empty directory
    // followed by `git checkout ...` is an error. See
    // https://github.com/gleam-lang/gleam/issues/4488 for details.
    if !fs::is_git_work_tree_root(&package_path) {
        fs::delete_directory(&package_path)?;
    }

    fs::mkdir(&package_path)?;

    let _ = execute_command(Command::new("git").arg("init").current_dir(&package_path))?;

    // If this directory already exists, but the remote URL has been edited in
    // `gleam.toml` without a `gleam clean`, `git remote add` will fail, causing
    // the remote to be stuck as the original value. Here we remove the remote
    // first, which ensures that `git remote add` properly add the remote each
    // time. If this fails, that means we haven't set the remote in the first
    // place, so we can safely ignore the error.
    let _ = Command::new("git")
        .arg("remote")
        .arg("remove")
        .arg("origin")
        .current_dir(&package_path)
        .output();

    let _ = execute_command(
        Command::new("git")
            .arg("remote")
            .arg("add")
            .arg("origin")
            .arg(repo)
            .current_dir(&package_path),
    )?;

    let _ = execute_command(
        Command::new("git")
            .arg("fetch")
            .arg("origin")
            .current_dir(&package_path),
    )?;

    let _ = execute_command(
        Command::new("git")
            .arg("checkout")
            .arg(ref_)
            .current_dir(&package_path),
    )?;

    let output = execute_command(
        Command::new("git")
            .arg("rev-parse")
            .arg("HEAD")
            .current_dir(&package_path),
    )?;

    let commit = String::from_utf8(output.stdout)
        .expect("Output should be UTF-8")
        .trim()
        .into();

    Ok(commit)
}

/// Provide a package from a git repository
fn provide_git_package(
    package_name: EcoString,
    repo: &str,
    // A git ref, such as a branch name, commit hash or tag name
    ref_: &str,
    project_paths: &ProjectPaths,
    provided: &mut HashMap<EcoString, ProvidedPackage>,
    parents: &mut Vec<EcoString>,
) -> Result<hexpm::version::Range> {
    let commit = download_git_package(&package_name, repo, ref_, project_paths)?;

    let package_source = ProvidedPackageSource::Git {
        repo: repo.into(),
        commit,
    };

    let package_path = fs::canonicalise(&project_paths.build_packages_package(&package_name))?;

    provide_package(
        package_name,
        package_path,
        package_source,
        project_paths,
        provided,
        parents,
    )
}

/// Adds a gleam project located at a specific path to the list of "provided packages"
fn provide_package(
    package_name: EcoString,
    package_path: Utf8PathBuf,
    package_source: ProvidedPackageSource,
    project_paths: &ProjectPaths,
    provided: &mut HashMap<EcoString, ProvidedPackage>,
    parents: &mut Vec<EcoString>,
) -> Result<hexpm::version::Range> {
    // Return early if a package cycle is detected
    if parents.contains(&package_name) {
        let mut last_cycle = parents
            .split(|p| p == &package_name)
            .next_back()
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
            let version = hexpm::version::Range::new(format!("== {}", &package.version))
                .expect("== {version} should be a valid range");
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
            Requirement::Git { git, ref_ } => {
                provide_git_package(name.clone(), &git, &ref_, project_paths, provided, parents)?
            }
        };
        let _ = requirements.insert(name, version);
    }
    let _ = parents.pop();
    // Add the package to the provided packages dictionary
    let version = hexpm::version::Range::new(format!("== {}", &config.version))
        .expect("== {version} should be a valid range");
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

/// Unlocks specified packages and their unique dependencies.
///
/// If a manifest is provided, it also unlocks indirect dependencies that are
/// not required by any other package or the root project.
pub fn unlock_packages(
    locked: &mut HashMap<EcoString, Version>,
    packages_to_unlock: &[EcoString],
    manifest: Option<&Manifest>,
) -> Result<()> {
    if let Some(manifest) = manifest {
        let mut packages_to_unlock: Vec<EcoString> = packages_to_unlock.to_vec();

        while let Some(package_name) = packages_to_unlock.pop() {
            if locked.remove(&package_name).is_some()
                && let Some(package) = manifest.packages.iter().find(|p| p.name == package_name)
            {
                let deps_to_unlock = find_deps_to_unlock(package, locked, manifest);
                packages_to_unlock.extend(deps_to_unlock);
            }
        }
    } else {
        for package_name in packages_to_unlock {
            let _ = locked.remove(package_name);
        }
    }

    Ok(())
}

/// Identifies which dependencies of a package should be unlocked.
///
/// A dependency is eligible for unlocking if it is currently locked,
/// is not a root dependency, and is not required by any locked package.
fn find_deps_to_unlock(
    package: &ManifestPackage,
    locked: &HashMap<EcoString, Version>,
    manifest: &Manifest,
) -> Vec<EcoString> {
    package
        .requirements
        .iter()
        .filter(|&dep| {
            locked.contains_key(dep)
                && !manifest.requirements.contains_key(dep)
                && manifest
                    .packages
                    .iter()
                    .all(|p| !locked.contains_key(&p.name) || !p.requirements.contains(dep))
        })
        .cloned()
        .collect()
}

/// Determine the information to add to the manifest for a specific package
async fn lookup_package(
    name: String,
    version: Version,
    provided: &HashMap<EcoString, ProvidedPackage>,
) -> Result<ManifestPackage> {
    match provided.get(name.as_str()) {
        Some(provided_package) => Ok(provided_package.to_manifest_package(name.as_str())),
        None => {
            let config = hexpm::Config::new();
            let release =
                hex::get_package_release(&name, &version, &config, &HttpClient::new()).await?;
            let build_tools = release
                .meta
                .build_tools
                .iter()
                .map(|s| EcoString::from(s.as_str()))
                .collect_vec();
            let requirements = release
                .requirements
                .keys()
                .map(|s| EcoString::from(s.as_str()))
                .collect_vec();
            Ok(ManifestPackage {
                name: name.into(),
                version,
                otp_app: Some(release.meta.app.into()),
                build_tools,
                requirements,
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(release.outer_checksum),
                },
            })
        }
    }
}

struct PackageFetcher {
    runtime_cache: RefCell<HashMap<String, Rc<hexpm::Package>>>,
    runtime: tokio::runtime::Handle,
    http: HttpClient,
}

impl PackageFetcher {
    pub fn new(runtime: tokio::runtime::Handle) -> Self {
        Self {
            runtime_cache: RefCell::new(HashMap::new()),
            runtime,
            http: HttpClient::new(),
        }
    }

    /// Caches the result of `get_dependencies` so that we don't need to make a network request.
    /// Currently dependencies are fetched during initial version resolution, and then during check
    /// for major version availability.
    fn cache_package(&self, package: &str, result: Rc<hexpm::Package>) {
        let mut runtime_cache = self.runtime_cache.borrow_mut();
        let _ = runtime_cache.insert(package.to_string(), result);
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
    fn get_dependencies(&self, package: &str) -> Result<Rc<hexpm::Package>, PackageFetchError> {
        {
            let runtime_cache = self.runtime_cache.borrow();
            let result = runtime_cache.get(package);

            if let Some(result) = result {
                return Ok(result.clone());
            }
        }

        tracing::debug!(package = package, "looking_up_hex_package");
        let config = hexpm::Config::new();
        let request = hexpm::repository_v2_get_package_request(package, None, &config);
        let response = self
            .runtime
            .block_on(self.http.send(request))
            .map_err(PackageFetchError::fetch_error)?;

        let pkg = hexpm::repository_v2_get_package_response(response, HEXPM_PUBLIC_KEY)
            .map_err(|e| PackageFetchError::from_api_error(e, package))?;
        let pkg = Rc::new(pkg);
        let pkg_ref = Rc::clone(&pkg);
        self.cache_package(package, pkg);
        Ok(pkg_ref)
    }
}
