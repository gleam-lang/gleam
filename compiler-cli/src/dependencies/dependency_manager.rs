use ecow::EcoString;
use futures::future;
use gleam_core::{
    Error, Result,
    build::{Mode, Telemetry},
    config::PackageConfig,
    dependency,
    manifest::{Manifest, ManifestPackageSource, PackageChanges, Resolved},
    paths::ProjectPaths,
    requirement::Requirement,
};
use std::collections::HashMap;

use crate::{
    build_lock::BuildLock,
    dependencies::{pretty_print_major_versions_available, write_manifest_to_disc},
    fs::ProjectIO,
};

use super::{
    CheckMajorVersions, LocalPackages, UseManifest, add_missing_packages, is_same_requirements,
    lookup_package, path_dependency_configs_unchanged, provide_git_package, provide_local_package,
    read_manifest_from_disc, remove_extra_packages, unlock_packages,
};

/// Verifies that all specified packages exist in the manifest.
pub fn ensure_packages_exist_locally(manifest: &Manifest, packages: &[EcoString]) -> Result<()> {
    let missing_packages: Vec<EcoString> = packages
        .iter()
        .filter(|package_name| !manifest.packages.iter().any(|p| &p.name == *package_name))
        .cloned()
        .collect();

    if !missing_packages.is_empty() {
        return Err(Error::PackagesToUpdateNotExist {
            packages: missing_packages,
        });
    }
    Ok(())
}

pub struct DependencyManagerConfig {
    // If `Yes` we read the manifest from disc. If not set then we ignore any
    // manifest which will result in the latest versions of the dependency
    // packages being resolved (not the locked ones).
    pub use_manifest: UseManifest,
    /// When set to `Yes`, the cli will check for major version updates of direct dependencies and
    /// print them to the console if the major versions are not upgradeable due to constraints.
    pub check_major_versions: CheckMajorVersions,
}

impl DependencyManagerConfig {
    pub fn into_dependency_manager<Telem: Telemetry, P: dependency::PackageFetcher>(
        self,
        runtime: tokio::runtime::Handle,
        package_fetcher: P,
        telemetry: Telem,
        mode: Mode,
    ) -> DependencyManager<Telem, P> {
        DependencyManager {
            runtime,
            package_fetcher,
            telemetry,

            mode,
            use_manifest: self.use_manifest,
            check_major_versions: self.check_major_versions,
        }
    }
}

pub struct DependencyManager<Telem, P> {
    runtime: tokio::runtime::Handle,
    package_fetcher: P,
    mode: Mode,
    use_manifest: UseManifest,
    telemetry: Telem,
    check_major_versions: CheckMajorVersions,
}

impl<Telem, P> DependencyManager<Telem, P>
where
    P: dependency::PackageFetcher,
    Telem: Telemetry,
{
    /// Resolve the dependency versions used by a package.
    ///
    /// If the `use_manifest` configuration was set to `false` then it'll always resolve all the
    /// versions, even if there are already versions locked in the manifest.
    pub fn resolve_versions(
        &self,
        paths: &ProjectPaths,
        config: &PackageConfig,
        packages_to_update: Vec<EcoString>,
    ) -> Result<Resolved> {
        // If there's no manifest then the only thing we can do is attempt to update the versions.
        if !paths.manifest().exists() {
            tracing::debug!("manifest_not_present");
            let manifest = self.perform_version_resolution(paths, config, None, Vec::new())?;
            ensure_packages_exist_locally(&manifest, &packages_to_update)?;
            return Ok(Resolved::all_added(manifest));
        }

        let existing_manifest = read_manifest_from_disc(paths)?;
        ensure_packages_exist_locally(&existing_manifest, &packages_to_update)?;

        // If we have been asked not to use the manifest then
        let (requirements_changed, manifest_for_resolver) = match self.use_manifest {
            UseManifest::No => (true, None),
            UseManifest::Yes => {
                let config_dependencies = config.all_direct_dependencies()?;
                let same_requirements = is_same_requirements(
                    &existing_manifest.requirements,
                    &config_dependencies,
                    paths.root(),
                )?;

                // If the manifest is to be used and the requirements have not changed then there's
                // no point in performing resolution, it'll always result in the same versions
                // already specified in the manifest.
                if packages_to_update.is_empty()
                    && same_requirements
                    && path_dependency_configs_unchanged(&config_dependencies, paths)?
                {
                    return Ok(Resolved::no_change(existing_manifest));
                }

                // Otherwise, use the manifest to inform resolution.
                (!same_requirements, Some(&existing_manifest))
            }
        };

        tracing::debug!("manifest_outdated");
        let new_manifest = self.perform_version_resolution(
            paths,
            config,
            manifest_for_resolver,
            packages_to_update,
        )?;
        let resolved = Resolved {
            package_changes: PackageChanges::between_manifests(&existing_manifest, &new_manifest),
            manifest: new_manifest,
            requirements_changed,
        };
        Ok(resolved)
    }

    pub fn resolve_and_download_versions(
        &self,
        paths: &ProjectPaths,
        new_package: Option<(Vec<(EcoString, Requirement)>, bool)>,
        packages_to_update: Vec<EcoString>,
    ) -> Result<Manifest> {
        let span = tracing::info_span!("download_deps");
        let _enter = span.enter();

        // We do this before acquiring the build lock so that we don't create the
        // build directory if there is no gleam.toml
        crate::config::ensure_config_exists(paths)?;

        let lock = BuildLock::new_packages(paths)?;
        let _guard = lock.lock(&self.telemetry);

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

        // Determine what versions we need
        let resolved = self.resolve_versions(paths, &config, packages_to_update)?;
        let local = LocalPackages::read_from_disc(paths)?;

        // Remove any packages that are no longer required due to gleam.toml changes
        remove_extra_packages(paths, &local, &resolved.manifest, &self.telemetry)?;

        // Download them from Hex to the local cache
        self.runtime.block_on(add_missing_packages(
            paths,
            fs,
            &resolved.manifest,
            &local,
            project_name,
            &self.telemetry,
        ))?;

        if resolved.any_changes() {
            // Record new state of the packages directory
            // TODO: test
            tracing::debug!("writing_manifest_toml");
            write_manifest_to_disc(paths, &resolved.manifest)?;
        }
        LocalPackages::from_manifest(&resolved.manifest).write_to_disc(paths)?;

        // Display the changes in versions to the user.
        self.telemetry
            .resolved_package_versions(&resolved.package_changes);

        // If requested to do so, check if there are major upgrades that could be performed with
        // more relaxed version requirements, and inform the user if so.
        if let CheckMajorVersions::Yes = self.check_major_versions {
            let major_versions_available = dependency::check_for_major_version_updates(
                &resolved.manifest,
                &self.package_fetcher,
            );
            if !major_versions_available.is_empty() {
                eprintln!(
                    "{}",
                    pretty_print_major_versions_available(major_versions_available)
                );
            }
        }
        Ok(resolved.manifest)
    }

    fn perform_version_resolution(
        &self,
        project_paths: &ProjectPaths,
        config: &PackageConfig,
        manifest: Option<&Manifest>,
        packages_to_update: Vec<EcoString>,
    ) -> Result<Manifest, Error> {
        self.telemetry.resolving_package_versions();
        let dependencies = config.dependencies_for(self.mode)?;
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
                Requirement::Git { git, ref_ } => {
                    // If this package is locked and we already resolved a commit
                    // hash for it, we want to use that hash rather than pulling
                    // the latest commit.
                    let ref_to_use = if locked.contains_key(&name)
                        && let Some(manifest) = manifest
                        && let Some(package) = manifest
                            .packages
                            .iter()
                            .find(|package| package.name == name)
                        && let ManifestPackageSource::Git { commit, .. } = &package.source
                    {
                        commit
                    } else {
                        // If the package is unlocked or we haven't resolved a version yet, we use
                        // the ref specified in `gleam.toml`.
                        &ref_
                    };

                    provide_git_package(
                        name.clone(),
                        &git,
                        ref_to_use,
                        project_paths,
                        &mut provided_packages,
                        &mut Vec::new(),
                    )?
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
            &self.package_fetcher,
            provided_hex_packages,
            config.name.clone(),
            root_requirements.into_iter(),
            &locked,
        )?;

        // Convert the hex packages and local packages into manifest packages
        let manifest_packages = self.runtime.block_on(future::try_join_all(
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
}
