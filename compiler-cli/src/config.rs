use camino::Utf8PathBuf;

use gleam_core::{
    config::PackageConfig,
    error::{Error, FileIoAction, FileKind},
    manifest::{Manifest, ManifestPackage, ManifestPackageSource},
    paths::ProjectPaths,
};

use crate::fs::{get_current_directory, get_project_root};

pub fn root_config() -> Result<PackageConfig, Error> {
    let dir = get_project_root(get_current_directory()?)?;
    let paths = ProjectPaths::new(dir);
    read(paths.root_config())
}

#[derive(Debug, Clone, Copy)]
pub enum PackageKind {
    Dependency,
    Root,
}

/// Get the config for a dependency module. Return the config for the current
/// project if a dependency doesn't have a config file.
pub fn find_package_config_for_module(
    mod_path: &str,
    manifest: &Manifest,
    project_paths: &ProjectPaths,
) -> Result<(PackageConfig, PackageKind), Error> {
    for package in &manifest.packages {
        // Not a Gleam package
        if !package.build_tools.contains(&"gleam".into()) {
            continue;
        }

        let root = package_root(package, project_paths);
        let mut module_path = root.join("src").join(mod_path);
        _ = module_path.set_extension("gleam");

        // This package doesn't have the module we're looking for
        if !module_path.is_file() {
            continue;
        }

        let configuration = read(root.join("gleam.toml"))?;
        return Ok((configuration, PackageKind::Dependency));
    }

    Ok((root_config()?, PackageKind::Root))
}

fn package_root(package: &ManifestPackage, project_paths: &ProjectPaths) -> Utf8PathBuf {
    match &package.source {
        ManifestPackageSource::Local { path } => project_paths.root().join(path),

        ManifestPackageSource::Hex { .. } | ManifestPackageSource::Git { .. } => {
            project_paths.build_packages_package(&package.name)
        }
    }
}

pub fn read(config_path: Utf8PathBuf) -> Result<PackageConfig, Error> {
    let toml = crate::fs::read(&config_path)?;
    let config: PackageConfig = toml::from_str(&toml).map_err(|e| Error::FileIo {
        action: FileIoAction::Parse,
        kind: FileKind::File,
        path: config_path,
        err: Some(e.to_string()),
    })?;
    config.check_gleam_compatibility()?;
    Ok(config)
}

pub fn ensure_config_exists(paths: &ProjectPaths) -> Result<(), Error> {
    let path = paths.root_config();
    if !path.is_file() {
        return Err(Error::FileIo {
            action: FileIoAction::Read,
            kind: FileKind::File,
            path,
            err: Some("File not found".into()),
        });
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use gleam_core::manifest::Base16Checksum;

    #[test]
    fn package_root_hex() {
        let paths = ProjectPaths::new(Utf8PathBuf::from("/app"));
        let package = ManifestPackage {
            name: "the_package".into(),
            version: hexpm::version::Version::new(1, 0, 0),
            build_tools: vec!["gleam".into()],
            otp_app: None,
            requirements: vec![],
            source: ManifestPackageSource::Hex {
                outer_checksum: Base16Checksum(vec![]),
            },
        };
        assert_eq!(
            package_root(&package, &paths),
            Utf8PathBuf::from("/app/build/packages/the_package")
        );
    }

    #[test]
    fn package_root_git() {
        let paths = ProjectPaths::new(Utf8PathBuf::from("/app"));
        let package = ManifestPackage {
            name: "the_package".into(),
            version: hexpm::version::Version::new(1, 0, 0),
            build_tools: vec!["gleam".into()],
            otp_app: None,
            requirements: vec![],
            source: ManifestPackageSource::Git {
                repo: "repo".into(),
                commit: "commit".into(),
            },
        };
        assert_eq!(
            package_root(&package, &paths),
            Utf8PathBuf::from("/app/build/packages/the_package")
        );
    }

    #[test]
    fn package_root_local() {
        let paths = ProjectPaths::new(Utf8PathBuf::from("/app"));
        let package = ManifestPackage {
            name: "the_package".into(),
            version: hexpm::version::Version::new(1, 0, 0),
            build_tools: vec!["gleam".into()],
            otp_app: None,
            requirements: vec![],
            source: ManifestPackageSource::Local {
                path: Utf8PathBuf::from("../wibble"),
            },
        };
        assert_eq!(
            package_root(&package, &paths),
            Utf8PathBuf::from("/app/../wibble")
        );
    }
}
