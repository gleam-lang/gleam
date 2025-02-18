use std::{
    collections::{HashMap, HashSet},
    time::Instant,
};

use camino::{Utf8Path, Utf8PathBuf};
use ecow::{eco_format, EcoString};
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
use strum::IntoEnumIterator;

#[cfg(test)]
mod tests;

use crate::{
    build_lock::BuildLock,
    cli,
    fs::{self, ProjectIO},
    http::HttpClient,
    TreeOptions,
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

pub fn list() -> Result<()> {
    let (_, _, manifest) = get_manifest_details()?;
    list_manifest_packages(std::io::stdout(), manifest)
}

pub fn tree(options: TreeOptions) -> Result<()> {
    let (project, config, manifest) = get_manifest_details()?;

    // Initialize the root package since it is not part of the manifest
    let root_package = ManifestPackage {
        build_tools: vec![],
        name: config.name.clone(),
        requirements: config.all_direct_dependencies()?.keys().cloned().collect(),
        version: config.version.clone(),
        source: ManifestPackageSource::Local {
            path: project.clone(),
        },
        otp_app: None,
    };

    // Get the manifest packages and add the root package to the vec
    let mut packages = manifest.packages.iter().cloned().collect_vec();
    packages.append(&mut vec![root_package.clone()]);

    list_package_and_dependencies_tree(std::io::stdout(), options, packages.clone(), config.name)
}

fn get_manifest_details() -> Result<(Utf8PathBuf, PackageConfig, Manifest)> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let project = fs::get_project_root(fs::get_current_directory()?)?;
    let paths = ProjectPaths::new(project.clone());
    let config = crate::config::root_config(&paths)?;
    let (_, manifest) = get_manifest(
        &paths,
        runtime.handle().clone(),
        Mode::Dev,
        &config,
        &cli::Reporter::new(),
        UseManifest::Yes,
        Vec::new(),
    )?;
    Ok((project, config, manifest))
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
            .try_for_each(|line| writeln!(buffer, "{}", line))
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

#[derive(Debug, Clone, Copy)]
pub enum UseManifest {
    Yes,
    No,
}

pub fn update(packages: Vec<String>) -> Result<()> {
    let paths = crate::find_project_paths()?;
    let use_manifest = if packages.is_empty() {
        UseManifest::No
    } else {
        UseManifest::Yes
    };

    // Update specific packages
    _ = download(
        &paths,
        cli::Reporter::new(),
        None,
        packages.into_iter().map(EcoString::from).collect(),
        use_manifest,
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
    let _guard = lock.lock(&telemetry);

    // Read the project config
    let config = crate::config::read(paths.root_config())?;
    let mut manifest = read_manifest_from_disc(paths)?;

    remove_extra_requirements(&config, &mut manifest)?;

    // Remove any packages that are no longer required due to manifest changes
    let local = LocalPackages::read_from_disc(paths)?;
    remove_extra_packages(paths, &local, &manifest, &telemetry)?;

    // Record new state of the packages directory
    tracing::debug!("writing_manifest_toml");
    write_manifest_to_disc(paths, &manifest)?;
    LocalPackages::from_manifest(&manifest).write_to_disc(paths)?;

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
        return Ok((package.into(), Requirement::hex(">= 0.0.0")));
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
            })
        }
    };

    Ok((package.into(), requirement))
}

pub fn download<Telem: Telemetry>(
    paths: &ProjectPaths,
    telemetry: Telem,
    new_package: Option<(Vec<(EcoString, Requirement)>, bool)>,
    packages_to_update: Vec<EcoString>,
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
        for (package, requirement) in packages {
            if dev {
                _ = config.dev_dependencies.insert(package, requirement);
            } else {
                _ = config.dependencies.insert(package, requirement);
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
        packages_to_update,
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
    project_name: EcoString,
    telemetry: &Telem,
) -> Result<(), Error> {
    let missing_packages = local.missing_local_packages(manifest, &project_name);

    let mut num_to_download = 0;
    let mut missing_hex_packages = missing_packages
        .into_iter()
        .filter(|package| package.is_hex())
        .inspect(|_| {
            num_to_download += 1;
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

fn get_manifest<Telem: Telemetry>(
    paths: &ProjectPaths,
    runtime: tokio::runtime::Handle,
    mode: Mode,
    config: &PackageConfig,
    telemetry: &Telem,
    use_manifest: UseManifest,
    packages_to_update: Vec<EcoString>,
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
        let manifest = resolve_versions(runtime, mode, paths, config, None, telemetry, Vec::new())?;
        return Ok((true, manifest));
    }

    let manifest = read_manifest_from_disc(paths)?;

    // If there are no requested updates, and the config is unchanged
    // since the manifest was written then it is up to date so we can return it unmodified.
    if packages_to_update.is_empty()
        && is_same_requirements(
            &manifest.requirements,
            &config.all_direct_dependencies()?,
            paths.root(),
        )?
    {
        tracing::debug!("manifest_up_to_date");
        Ok((false, manifest))
    } else {
        tracing::debug!("manifest_outdated");
        let manifest = resolve_versions(
            runtime,
            mode,
            paths,
            config,
            Some(&manifest),
            telemetry,
            packages_to_update,
        )?;
        Ok((true, manifest))
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

fn resolve_versions<Telem: Telemetry>(
    runtime: tokio::runtime::Handle,
    mode: Mode,
    project_paths: &ProjectPaths,
    config: &PackageConfig,
    manifest: Option<&Manifest>,
    telemetry: &Telem,
    packages_to_update: Vec<EcoString>,
) -> Result<Manifest, Error> {
    telemetry.resolving_package_versions();
    let dependencies = config.dependencies_for(mode)?;
    let mut locked = config.locked(manifest)?;

    if !packages_to_update.is_empty() {
        unlock_packages(&mut locked, &packages_to_update, manifest)?;
    }

    // Packages which are provided directly instead of downloaded from hex
    let mut provided_packages = HashMap::new();
    // The version requires of the current project
    let mut root_requirements = HashMap::new();

    // Populate the provided_packages and root_requirements maps
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

    // Convert the hex packages and local packages into manifest packages
    let manifest_packages = runtime.block_on(future::try_join_all(
        resolved
            .into_iter()
            .map(|(name, version)| lookup_package(name, version, &provided_packages)),
    ))?;

    let manifest = Manifest {
        packages: manifest_packages,
        requirements: config.all_direct_dependencies()?,
    };

    Ok(manifest)
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
    _package_name: EcoString,
    _repo: &str,
    _project_paths: &ProjectPaths,
    _provided: &mut HashMap<EcoString, ProvidedPackage>,
) -> Result<hexpm::version::Range> {
    let _git = ProvidedPackageSource::Git {
        repo: "repo".into(),
        commit: "commit".into(),
    };
    Err(Error::GitDependencyUnsupported)
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
            if locked.remove(&package_name).is_some() {
                if let Some(package) = manifest.packages.iter().find(|p| p.name == package_name) {
                    let deps_to_unlock = find_deps_to_unlock(package, locked, manifest);
                    packages_to_unlock.extend(deps_to_unlock);
                }
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

        match hexpm::get_package_response(response, HEXPM_PUBLIC_KEY) {
            Ok(a) => Ok(a),
            Err(e) => match e {
                hexpm::ApiError::NotFound => {
                    Err(format!("I couldn't find a package called `{}`", package).into())
                }
                _ => Err(e.into()),
            },
        }
    }
}
