use crate::{build::Mode, config::PackageConfig, manifest::Manifest, Error, Result};
use hexpm::version::PackageVersions;

pub fn resolve_versions(
    package_fetcher: Box<dyn hexpm::version::PackageFetcher>,
    mode: Mode,
    config: &PackageConfig,
    manifest: Option<&Manifest>,
) -> Result<PackageVersions> {
    let specified_dependencies = config.dependency_versions_for(mode)?.into_iter();
    let locked = config.locked(manifest)?;
    tracing::info!("resolving_versions");
    hexpm::version::resolve_versions(
        package_fetcher,
        config.name.to_string(),
        specified_dependencies,
        &locked,
    )
    .map_err(Error::dependency_resolution_failed)
}
