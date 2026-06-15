// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

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
    build::{Mode, SourceFingerprint, Target, Telemetry},
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

    let total_packages = manifest
        .packages
        .iter()
        .filter(|package| matches!(package.source, ManifestPackageSource::Hex { .. }))
        .count();

    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let package_fetcher = PackageFetcher::new(runtime.handle().clone());

    let version_updates = dependency::check_for_version_updates(&manifest, &package_fetcher);

    print!(
        "{}",
        pretty_print_outdated_versions(total_packages, version_updates)
    );

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

fn pretty_print_outdated_versions(
    total_packages: usize,
    versions: dependency::PackageVersionDiffs,
) -> EcoString {
    let summary = eco_format!(
        "{} of {} packages have newer versions available.",
        versions.len(),
        total_packages
    );

    if versions.is_empty() {
        eco_format!("{}\n", summary)
    } else {
        eco_format!("{}\n\n{}", summary, pretty_print_version_updates(versions))
    }
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
            let ManifestPackageSource::Git { repo, commit, path } = &package.source else {
                continue;
            };

            let checkout =
                download_git_package(&package.name, repo, commit, path.as_deref(), paths)?;
            checkout.cleanup()?;
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

    remove_unused_git_clones(paths, manifest)?;
    Ok(())
}

fn remove_unused_git_clones(paths: &ProjectPaths, manifest: &Manifest) -> Result<()> {
    let git_directory = paths.build_git_directory();
    if !git_directory.is_dir() {
        return Ok(());
    }

    let expected: HashSet<String> = manifest
        .packages
        .iter()
        .filter_map(|package| match &package.source {
            ManifestPackageSource::Git {
                repo,
                path: Some(_),
                ..
            } => Some(git_repo_dir_name(repo)),
            _ => None,
        })
        .collect();

    for entry in fs::read_dir(&git_directory)?.filter_map(Result::ok) {
        if !expected.contains(entry.file_name()) {
            tracing::debug!(path=%entry.path(), "removing_unused_git_clone");
            fs::delete_directory(entry.path())?;
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
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
struct LocalPackages {
    #[serde(deserialize_with = "gleam_core::config::map_with_package_name_keys::deserialize")]
    packages: HashMap<EcoString, Version>,
    // Git packages can resolve to a new commit (or a new sub-path) without
    // their version changing, so their on-disc freshness is keyed by commit
    // and path rather than version alone. Absent for hex and local packages.
    #[serde(default)]
    git: HashMap<EcoString, GitState>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
struct GitState {
    commit: EcoString,
    #[serde(default)]
    path: Option<Utf8PathBuf>,
}

impl LocalPackages {
    pub fn extra_local_packages(&self, manifest: &Manifest) -> Vec<(EcoString, Version)> {
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
            // We don't need to download packages which we already have on disc. For
            // git packages this means the same commit and path.
            .filter(|p| !self.has_fresh(p))
            .collect()
    }

    fn has_fresh(&self, package: &ManifestPackage) -> bool {
        match &package.source {
            ManifestPackageSource::Git { commit, path, .. } => match self.git.get(&package.name) {
                Some(state) => &state.commit == commit && &state.path == path,
                None => false,
            },
            ManifestPackageSource::Hex { .. } | ManifestPackageSource::Local { .. } => {
                self.packages.get(package.name.as_str()) == Some(&package.version)
            }
        }
    }

    pub fn read_from_disc(paths: &ProjectPaths) -> Result<Self> {
        let path = paths.build_packages_toml();
        if !path.exists() {
            return Ok(Self {
                packages: HashMap::new(),
                git: HashMap::new(),
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
        let packages = manifest
            .packages
            .iter()
            .map(|p| (p.name.clone(), p.version.clone()))
            .collect();
        let git = manifest
            .packages
            .iter()
            .filter_map(|p| match &p.source {
                ManifestPackageSource::Git { commit, path, .. } => Some((
                    p.name.clone(),
                    GitState {
                        commit: commit.clone(),
                        path: path.clone(),
                    },
                )),
                ManifestPackageSource::Hex { .. } | ManifestPackageSource::Local { .. } => None,
            })
            .collect();

        Self { packages, git }
    }
}

#[test]
fn local_packages_deserialise_ok() {
    let toml = r#"
[packages]
gleam_stdlib = "1.0.0"
gleam_otp = "1.1.0"
"#;
    let packages: LocalPackages = toml::from_str(toml).unwrap();
    assert_eq!(
        packages,
        LocalPackages {
            packages: HashMap::from_iter([
                ("gleam_stdlib".into(), Version::new(1, 0, 0)),
                ("gleam_otp".into(), Version::new(1, 1, 0)),
            ]),
            git: HashMap::new(),
        }
    )
}

#[test]
fn local_packages_deserialise_invalid_name() {
    let toml = r#"
[packages]
gleam_stdlib = "1.0.0"
"../../stuff" = "1.1.0"
"#;
    let error = toml::from_str::<LocalPackages>(toml)
        .expect_err("should fail to deserialise because of invalid name");
    insta::assert_snapshot!(insta::internals::AutoName, error.to_string());
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

    Ok(true)
}

/// Returns true if all path dependency configs are unchanged since last build.
///
/// If any of the path dependency configs have changed that means that we need to
/// re-perform dependency resolution, as their dependencies could have changed
/// themselves.
///
/// We use gleam.toml rather than manifest.toml as:
///
/// 1. The dependency requirements could have changed but resolution not have been
///    run in that package yet, so the manifest would be the same, resulting in us
///    failing to detect that resolution is required.
///
/// 2. Dependency manifests are not used in any way, so a change in the manifest
///    may not have any impact on this package.
///
/// This does mean that changes unrelated to the path dependency's dependencies
/// will trigger resolution, but gleam.toml is edited rarely, and no-change
/// resolution is fast enough, so that's OK.
///
/// Note: This does not check path dependencies of path dependencies! Changes to
/// their configs will fail to be picked up. To resolve this we would need to keep
/// a list of all the path dependencies in the project, instead of only the direct
/// path dependencies.
///
fn path_dependency_configs_unchanged(
    requirements: &HashMap<EcoString, Requirement>,
    paths: &ProjectPaths,
) -> Result<bool> {
    for (name, requirement) in requirements {
        let Requirement::Path { path } = requirement else {
            continue;
        };

        let config_path = paths.path_dependency_gleam_toml_path(path);
        let fingerprint_path = paths.dependency_gleam_toml_fingerprint_path(name.as_str());

        // Check mtimes before hashing, to avoid extra work
        if fingerprint_path.exists() {
            let config_time = fs::modification_time(&config_path)?;
            let fingerprint_time = fs::modification_time(&fingerprint_path)?;
            if config_time <= fingerprint_time {
                continue;
            }
        };

        let config_text = fs::read(&config_path)?;
        let current_fingerprint = SourceFingerprint::new(&config_text).to_numerical_string();

        // If cached hash file doesn't exist, this is the first time we're checking this dependency
        if !fingerprint_path.exists() {
            // Save the current hash for future comparisons
            fs::write(&fingerprint_path, &current_fingerprint)?;
            return Ok(false);
        }

        let previous_fingerprint = fs::read(&fingerprint_path)?;

        if previous_fingerprint != current_fingerprint {
            tracing::debug!("path_dependency_config_changed_forcing_rebuild");
            fs::write(&fingerprint_path, &current_fingerprint)?;
            return Ok(false);
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
    Git {
        repo: EcoString,
        commit: EcoString,
        path: Option<Utf8PathBuf>,
    },
    Local {
        path: Utf8PathBuf,
    },
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
            Self::Git { repo, commit, path } => ManifestPackageSource::Git {
                repo: repo.clone(),
                commit: commit.clone(),
                path: path.clone(),
            },
            Self::Local { path } => ManifestPackageSource::Local { path: path.clone() },
        }
    }

    fn to_toml(&self) -> String {
        match self {
            Self::Git { repo, commit, path } => match path {
                Some(path) => {
                    format!(r#"{{ repo: "{repo}", commit: "{commit}", path: "{path}" }}"#)
                }
                None => format!(r#"{{ repo: "{repo}", commit: "{commit}" }}"#),
            },
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
                    path: own_path,
                },
                Self::Git {
                    repo: other_repo,
                    commit: other_commit,
                    path: other_path,
                },
            ) => own_repo == other_repo && own_commit == other_commit && own_path == other_path,

            (Self::Git { .. }, Self::Local { .. }) | (Self::Local { .. }, Self::Git { .. }) => {
                false
            }
        }
    }
}

/// Where a provided package came from. Git sources carry the on-disc repository
/// root so path dependencies can be resolved relative to it.
enum SourceContext<'a> {
    Local {
        path: Utf8PathBuf,
    },
    Git {
        repo: EcoString,
        commit: EcoString,
        path: Option<Utf8PathBuf>,
        repo_root: &'a Utf8Path,
    },
}

impl SourceContext<'_> {
    fn to_provided_source(&self) -> ProvidedPackageSource {
        match self {
            Self::Local { path } => ProvidedPackageSource::Local { path: path.clone() },
            Self::Git {
                repo, commit, path, ..
            } => ProvidedPackageSource::Git {
                repo: repo.clone(),
                commit: commit.clone(),
                path: path.clone(),
            },
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

    provide_package(
        package_name,
        package_path.clone(),
        SourceContext::Local { path: package_path },
        project_paths,
        provided,
        parents,
    )
}

/// Resolve a path dependency of a git package to its canonical filesystem
/// location and its repository-relative path.
fn resolve_git_path_package(
    package_name: &EcoString,
    path: &Utf8Path,
    repo: &EcoString,
    parent_path: &Utf8Path,
    repo_root: &Utf8Path,
) -> Result<(Utf8PathBuf, Utf8PathBuf)> {
    let location = parent_path.join(path);
    if !location.is_dir() {
        return Err(Error::GitDependencyPathNotFound {
            package: package_name.to_string(),
            path: path.to_string(),
            repo: repo.to_string(),
        });
    }

    // The path may name a symlink that points outside the repository
    // checkout, so resolve it and take the repository-relative path from the
    // canonical location.
    let package_path = fs::canonicalise(&location)?;
    let repo_path = package_path
        .strip_prefix(repo_root)
        .map_err(|_| Error::GitDependencyPathNotFound {
            package: package_name.to_string(),
            path: path.to_string(),
            repo: repo.to_string(),
        })?
        .to_path_buf();

    Ok((package_path, repo_path))
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
        Err(error) => Err(git_io_error(error)),
    }
}

/// A `git` command with its working directory set to `dir`.
fn git_command(dir: &Utf8Path) -> Command {
    let mut command = Command::new("git");
    let _ = command.current_dir(dir);
    command
}

/// Map an IO error from spawning `git` onto the appropriate `Error`.
fn git_io_error(error: std::io::Error) -> Error {
    match error.kind() {
        ErrorKind::NotFound => Error::ShellProgramNotFound {
            program: "git".into(),
            os: fs::get_os(),
        },
        other => Error::ShellCommand {
            program: "git".into(),
            reason: ShellCommandFailureReason::IoError(other),
        },
    }
}

/// Parse the trimmed UTF-8 stdout of a `git` command.
fn git_stdout(output: std::process::Output) -> EcoString {
    String::from_utf8(output.stdout)
        .expect("Output should be UTF-8")
        .trim()
        .into()
}

fn git_repo_dir_name(repo: &str) -> String {
    let name = repo
        .trim_end_matches('/')
        .trim_end_matches(".git")
        .rsplit(['/', ':', '\\'])
        .next()
        .filter(|name| !name.is_empty())
        .unwrap_or("repo");
    let hash = xxhash_rust::xxh3::xxh3_64(repo.as_bytes());
    format!("{name}-{hash:016x}")
}

fn git_staging_path(project_paths: &ProjectPaths, repo: &str, package_name: &str) -> Utf8PathBuf {
    project_paths.build_git_repo(&format!(
        "{}-{package_name}-staging",
        git_repo_dir_name(repo)
    ))
}

enum GitCheckout {
    InPlace {
        commit: EcoString,
    },
    Staged {
        commit: EcoString,
        staging_path: Utf8PathBuf,
    },
}

impl GitCheckout {
    fn cleanup(self) -> Result<()> {
        // The clones dangling worktree registration is left for the next
        // download to prune before it re-adds the staging worktree.
        if let Self::Staged { staging_path, .. } = self {
            fs::delete_directory(&staging_path)?;
        }
        Ok(())
    }
}

/// Downloads a git package from a remote repository.
///
/// For a package at the root of its repository the commands that are run look
/// like this, cloning directly into `build/packages/<name>`:
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
/// For a package in a subdirectory of its repository the repository is
/// instead cloned into `build/git/<repo-name>-<hash>` as a bare repository
/// with no work tree, and the package is checked out into a transient staging
/// worktree which the subdirectory is hard-linked out of:
///
/// ```sh
/// git init --bare
/// git remote remove origin
/// git remote add origin <repo>
/// git fetch origin
/// git rev-parse --verify --quiet <ref>^{commit}
/// git worktree prune
/// git worktree add --force --detach <clone>-<pkg>-staging <commit>
/// ```
///
/// The staging worktree exists only for the duration of one package download.
/// It is deleted (and the clone's worktree registration pruned) by
/// `GitCheckout::cleanup` once the package has been hard-linked and read.
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
    path: Option<&Utf8Path>,
    project_paths: &ProjectPaths,
) -> Result<GitCheckout> {
    match path {
        None => download_git_package_in_place(package_name, repo, ref_, project_paths),
        Some(subdir) => {
            download_git_package_to_staged_path(package_name, repo, ref_, project_paths, subdir)
        }
    }
}

fn download_git_package_in_place(
    package_name: &str,
    repo: &str,
    ref_: &str,
    project_paths: &ProjectPaths,
) -> Result<GitCheckout, Error> {
    let clone_path = project_paths.build_packages_package(package_name);
    prepare_git_clone(&clone_path, repo, false)?;
    let _ = execute_command(git_command(&clone_path).arg("checkout").arg(ref_))?;
    let output = execute_command(git_command(&clone_path).arg("rev-parse").arg("HEAD"))?;
    let commit = git_stdout(output);

    Ok(GitCheckout::InPlace { commit })
}

fn download_git_package_to_staged_path(
    package_name: &str,
    repo: &str,
    ref_: &str,
    project_paths: &ProjectPaths,
    subdir: &Utf8Path,
) -> Result<GitCheckout, Error> {
    let clone_path = project_paths.build_git_repo(&git_repo_dir_name(repo));
    prepare_git_clone(&clone_path, repo, true)?;

    let commit = resolve_git_ref(&clone_path, repo, ref_)?;

    // Delete any staging worktree left behind by a previous crash, and prune
    // its registration from the clone so `git worktree add` can reuse the
    // path.
    let staging_path = git_staging_path(project_paths, repo, package_name);
    fs::delete_directory(&staging_path)?;
    let _ = git_command(&clone_path)
        .arg("worktree")
        .arg("prune")
        .output();

    let _ = execute_command(
        git_command(&clone_path)
            .arg("worktree")
            .arg("add")
            .arg("--force")
            .arg("--detach")
            .arg(&staging_path)
            .arg(commit.as_str()),
    )?;

    let Some(subdir_source) = resolve_git_subdir(&staging_path, subdir) else {
        return Err(Error::GitDependencyPathNotFound {
            package: package_name.into(),
            path: subdir.to_string(),
            repo: repo.into(),
        });
    };

    let package_path = project_paths.build_packages_package(package_name);
    fs::delete_directory(&package_path)?;
    fs::mkdir(&package_path)?;
    fs::hardlink_dir(&subdir_source, &package_path)?;

    Ok(GitCheckout::Staged {
        commit,
        staging_path,
    })
}

/// Initialise (or reuse) a git clone at the given path and fetch from the
/// remote repository. When `bare` is set the clone is a bare repository with
/// no work tree.
fn prepare_git_clone(clone_path: &Utf8Path, repo: &str, bare: bool) -> Result<()> {
    // If the clone path exists but is not the kind of git repo we expect, we
    // need to remove the directory.
    let reusable = if bare {
        fs::is_bare_git_repo_root(clone_path)
    } else {
        fs::is_git_work_tree_root(clone_path)
    };

    if !reusable {
        fs::delete_directory(clone_path)?;
    }

    fs::mkdir(clone_path)?;

    let mut init = git_command(clone_path);
    let _ = init.arg("init");
    if bare {
        let _ = init.arg("--bare");
    }
    let _ = execute_command(&mut init)?;

    // If this directory already exists, but the remote URL has been edited in
    // `gleam.toml` without a `gleam clean`, `git remote add` will fail, causing
    // the remote to be stuck as the original value. Here we remove the remote
    // first, which ensures that `git remote add` properly add the remote each
    // time. If this fails, that means we haven't set the remote in the first
    // place, so we can safely ignore the error.
    let _ = git_command(clone_path)
        .arg("remote")
        .arg("remove")
        .arg("origin")
        .output();

    let _ = execute_command(
        git_command(clone_path)
            .arg("remote")
            .arg("add")
            .arg("origin")
            .arg(repo),
    )?;

    let _ = execute_command(git_command(clone_path).arg("fetch").arg("origin"))?;

    Ok(())
}

/// Resolve a ref (a branch name, tag, or full or partial commit hash) to a
/// full commit hash using the objects fetched into the clone, without
/// checking anything out. Branch names only exist as remote-tracking refs in
/// the never-checked-out clone, so when the ref does not resolve directly we
/// fall back to `origin/<ref>`.
fn resolve_git_ref(clone_path: &Utf8Path, repo: &str, ref_: &str) -> Result<EcoString> {
    let revisions = [
        format!("{ref_}^{{commit}}"),
        format!("origin/{ref_}^{{commit}}"),
    ];

    for revision in revisions {
        let result = git_command(clone_path)
            .arg("rev-parse")
            .arg("--verify")
            .arg("--quiet")
            .arg(&revision)
            .output();

        match result {
            // A failed rev-parse is expected when the ref form doesn't match
            // this pattern, so try the next one.
            Ok(output) if !output.status.success() => (),
            Ok(output) => return Ok(git_stdout(output)),
            Err(error) => return Err(git_io_error(error)),
        }
    }

    Err(Error::ShellCommand {
        program: "git".into(),
        reason: ShellCommandFailureReason::ShellCommandError(format!(
            "Unable to resolve git ref `{ref_}` for repository `{repo}`\n"
        )),
    })
}

/// Resolves a subdirectory within a cloned git repository, ensuring that the
/// resolved directory (after following any symlinks) is still inside the
/// repository checkout. Returns `None` if the directory does not exist or
/// escapes the checkout.
fn resolve_git_subdir(clone_path: &Utf8Path, subdir: &Utf8Path) -> Option<Utf8PathBuf> {
    let subdir_source = clone_path.join(subdir);
    if !subdir_source.is_dir() {
        return None;
    }

    let canonical_clone = fs::canonicalise(clone_path).ok()?;
    let canonical_subdir = fs::canonicalise(&subdir_source).ok()?;
    if !canonical_subdir.starts_with(&canonical_clone) {
        return None;
    }

    Some(canonical_subdir)
}

/// Provide a package from a git repository
fn provide_git_package(
    package_name: EcoString,
    repo: &str,
    // A git ref, such as a branch name, commit hash or tag name
    ref_: &str,
    path: Option<Utf8PathBuf>,
    project_paths: &ProjectPaths,
    provided: &mut HashMap<EcoString, ProvidedPackage>,
    parents: &mut Vec<EcoString>,
) -> Result<hexpm::version::Range> {
    let checkout = download_git_package(&package_name, repo, ref_, path.as_deref(), project_paths)?;
    let (commit, staging_path) = match &checkout {
        GitCheckout::InPlace { commit } => (commit, None),
        GitCheckout::Staged {
            commit,
            staging_path,
        } => (commit, Some(staging_path)),
    };

    // Use the package's location in the staging worktree, where its gleam.toml
    // lives, not build/packages/. A `../sibling` path dep is resolved next to
    // that gleam.toml, and the sibling is only present in the worktree.
    let (package_path, repo_root) = match (&path, staging_path) {
        (Some(subdir), Some(staging_path)) => {
            let repo_root = fs::canonicalise(staging_path)?;
            (fs::canonicalise(&staging_path.join(subdir))?, repo_root)
        }
        _ => {
            let package_path =
                fs::canonicalise(&project_paths.build_packages_package(&package_name))?;
            (package_path.clone(), package_path)
        }
    };

    let version = provide_package(
        package_name,
        package_path,
        SourceContext::Git {
            repo: repo.into(),
            commit: commit.clone(),
            path: path.clone(),
            repo_root: &repo_root,
        },
        project_paths,
        provided,
        parents,
    )?;

    checkout.cleanup()?;
    Ok(version)
}

/// Adds a gleam project located at a specific path to the list of "provided packages"
fn provide_package(
    package_name: EcoString,
    package_path: Utf8PathBuf,
    source: SourceContext<'_>,
    project_paths: &ProjectPaths,
    provided: &mut HashMap<EcoString, ProvidedPackage>,
    parents: &mut Vec<EcoString>,
) -> Result<hexpm::version::Range> {
    let package_source = source.to_provided_source();

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
            Requirement::Path { path } => match &source {
                // A path dependency of a git package points to another
                // package within the same repository, so lock it as a git
                // source to keep the manifest portable.
                SourceContext::Git {
                    repo,
                    commit,
                    repo_root,
                    ..
                } => {
                    let (child_path, child_repo_path) =
                        resolve_git_path_package(&name, &path, repo, &package_path, repo_root)?;
                    provide_package(
                        name.clone(),
                        child_path,
                        SourceContext::Git {
                            repo: repo.clone(),
                            commit: commit.clone(),
                            path: Some(child_repo_path),
                            repo_root,
                        },
                        project_paths,
                        provided,
                        parents,
                    )?
                }
                SourceContext::Local { .. } => {
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
            },
            Requirement::Git { git, ref_, path } => provide_git_package(
                name.clone(),
                &git,
                &ref_,
                path,
                project_paths,
                provided,
                parents,
            )?,
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
