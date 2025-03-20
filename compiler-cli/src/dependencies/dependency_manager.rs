use std::collections::HashMap;

use ecow::EcoString;
use futures::future;
use gleam_core::{
    Error, Result,
    build::{Mode, Telemetry},
    config::PackageConfig,
    dependency,
    manifest::Manifest,
    paths::ProjectPaths,
    requirement::Requirement,
};

use crate::{
    build_lock::BuildLock,
    dependencies::{pretty_print_major_versions_available, write_manifest_to_disc},
    fs::ProjectIO,
};

use super::{
    CheckMajorVersions, LocalPackages, UseManifest, add_missing_packages, is_same_requirements,
    lookup_package, provide_git_package, provide_local_package, read_manifest_from_disc,
    remove_extra_packages, unlock_packages,
};

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
    pub fn get_manifest(
        &self,
        paths: &ProjectPaths,
        config: &PackageConfig,
        packages_to_update: Vec<EcoString>,
    ) -> Result<(bool, Manifest)> {
        // If there's no manifest (or we have been asked not to use it) then resolve
        // the versions anew
        let should_resolve = match self.use_manifest {
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
            let manifest = self.resolve_versions(paths, config, None, Vec::new())?;
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
            let manifest =
                self.resolve_versions(paths, config, Some(&manifest), packages_to_update)?;
            Ok((true, manifest))
        }
    }

    pub fn download(
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
        let (manifest_updated, manifest) = self.get_manifest(paths, &config, packages_to_update)?;
        let local = LocalPackages::read_from_disc(paths)?;

        // Remove any packages that are no longer required due to gleam.toml changes
        remove_extra_packages(paths, &local, &manifest, &self.telemetry)?;

        // Download them from Hex to the local cache
        self.runtime.block_on(add_missing_packages(
            paths,
            fs,
            &manifest,
            &local,
            project_name,
            &self.telemetry,
        ))?;

        if manifest_updated {
            // Record new state of the packages directory
            // TODO: test
            tracing::debug!("writing_manifest_toml");
            write_manifest_to_disc(paths, &manifest)?;
        }
        LocalPackages::from_manifest(&manifest).write_to_disc(paths)?;

        if let CheckMajorVersions::Yes = self.check_major_versions {
            let major_versions_available =
                dependency::check_for_major_version_updates(&manifest, &self.package_fetcher);
            if !major_versions_available.is_empty() {
                eprintln!(
                    "{}",
                    pretty_print_major_versions_available(major_versions_available)
                );
            }
        }

        Ok(manifest)
    }

    fn resolve_versions(
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
                Requirement::Git { git, ref_ } => provide_git_package(
                    name.clone(),
                    &git,
                    &ref_,
                    project_paths,
                    &mut provided_packages,
                    &mut Vec::new(),
                )?,
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
