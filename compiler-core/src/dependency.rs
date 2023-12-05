use std::{borrow::Borrow, cell::RefCell, collections::HashMap, error::Error as StdError};

use crate::{Error, Result};

use ecow::EcoString;
use hexpm::{
    version::{Range, Version},
    Dependency, Release,
};
use pubgrub::{
    error::PubGrubError,
    solver::{choose_package_with_fewest_versions, Dependencies},
    type_aliases::Map,
};

pub type PackageVersions = HashMap<String, Version>;

pub type ResolutionError = PubGrubError<String, Version>;

type PubgrubRange = pubgrub::range::Range<Version>;

pub fn resolve_versions<Requirements>(
    package_fetcher: Box<dyn PackageFetcher>,
    provided_packages: HashMap<EcoString, hexpm::Package>,
    root_name: EcoString,
    dependencies: Requirements,
    locked: &HashMap<EcoString, Version>,
) -> Result<PackageVersions>
where
    Requirements: Iterator<Item = (EcoString, Range)>,
{
    tracing::info!("resolving_versions");
    let root_version = Version::new(0, 0, 0);
    let root = hexpm::Package {
        name: root_name.as_str().into(),
        repository: "local".into(),
        releases: vec![Release {
            version: root_version.clone(),
            outer_checksum: vec![],
            retirement_status: None,
            requirements: root_dependencies(dependencies, locked)
                .map_err(Error::dependency_resolution_failed)?,
            meta: (),
        }],
    };

    let packages = pubgrub::solver::resolve(
        &DependencyProvider::new(package_fetcher, provided_packages, root, locked),
        root_name.as_str().into(),
        root_version,
    )
    .map_err(Error::dependency_resolution_failed)?
    .into_iter()
    .filter(|(name, _)| name.as_str() != root_name.as_str())
    .collect();

    Ok(packages)
}

fn root_dependencies<Requirements>(
    base_requirements: Requirements,
    locked: &HashMap<EcoString, Version>,
) -> Result<HashMap<String, Dependency>, ResolutionError>
where
    Requirements: Iterator<Item = (EcoString, Range)>,
{
    // Record all of the already locked versions as hard requirements
    let mut requirements: HashMap<_, _> = locked
        .iter()
        .map(|(name, version)| {
            (
                name.to_string(),
                Dependency {
                    app: None,
                    optional: false,
                    repository: None,
                    requirement: Range::new(version.to_string()),
                },
            )
        })
        .collect();

    for (name, range) in base_requirements {
        match locked.get(&name) {
            // If the package was not already locked then we can use the
            // specified version requirement without modification.
            None => {
                let _ = requirements.insert(
                    name.into(),
                    Dependency {
                        app: None,
                        optional: false,
                        repository: None,
                        requirement: range,
                    },
                );
            }

            // If the version was locked we verify that the requirement is
            // compatible with the locked version.
            Some(locked_version) => {
                let compatible = range
                    .to_pubgrub()
                    .map_err(|e| ResolutionError::Failure(format!("Failed to parse range {}", e)))?
                    .contains(locked_version);
                if !compatible {
                    return Err(ResolutionError::Failure(format!(
                        "{package} is specified with the requirement `{requirement}`, \
but it is locked to {version}, which is incompatible.",
                        package = name,
                        requirement = range,
                        version = locked_version,
                    )));
                }
            }
        };
    }

    Ok(requirements)
}

pub trait PackageFetcher {
    fn get_dependencies(&self, package: &str) -> Result<hexpm::Package, Box<dyn StdError>>;
}

struct DependencyProvider<'a> {
    packages: RefCell<HashMap<EcoString, hexpm::Package>>,
    remote: Box<dyn PackageFetcher>,
    locked: &'a HashMap<EcoString, Version>,
}

impl<'a> DependencyProvider<'a> {
    fn new(
        remote: Box<dyn PackageFetcher>,
        mut packages: HashMap<EcoString, hexpm::Package>,
        root: hexpm::Package,
        locked: &'a HashMap<EcoString, Version>,
    ) -> Self {
        let _ = packages.insert(root.name.as_str().into(), root);
        Self {
            packages: RefCell::new(packages),
            locked,
            remote,
        }
    }

    /// Download information about the package from the registry into the local
    /// store. Does nothing if the packages are already known.
    ///
    /// Package versions are sorted from newest to oldest, with all pre-releases
    /// at the end to ensure that a non-prerelease version will be picked first
    /// if there is one.
    //
    fn ensure_package_fetched(
        // We would like to use `&mut self` but the pubgrub library enforces
        // `&self` with interop mutability.
        &self,
        name: &str,
    ) -> Result<(), Box<dyn StdError>> {
        let mut packages = self.packages.borrow_mut();
        if packages.get(name).is_none() {
            let mut package = self.remote.get_dependencies(name)?;
            // Sort the packages from newest to oldest, pres after all others
            package.releases.sort_by(|a, b| a.version.cmp(&b.version));
            package.releases.reverse();
            let (pre, mut norm): (_, Vec<_>) = package
                .releases
                .into_iter()
                .partition(|r| r.version.is_pre());
            norm.extend(pre);
            package.releases = norm;
            let _ = packages.insert(name.into(), package);
        }
        Ok(())
    }
}

type PackageName = String;

impl<'a> pubgrub::solver::DependencyProvider<PackageName, Version> for DependencyProvider<'a> {
    fn choose_package_version<Name: Borrow<PackageName>, Ver: Borrow<PubgrubRange>>(
        &self,
        potential_packages: impl Iterator<Item = (Name, Ver)>,
    ) -> Result<(Name, Option<Version>), Box<dyn StdError>> {
        let potential_packages: Vec<_> = potential_packages
            .map::<Result<_, Box<dyn StdError>>, _>(|pair| {
                self.ensure_package_fetched(pair.0.borrow())?;
                Ok(pair)
            })
            .collect::<Result<_, _>>()?;
        let list_available_versions = |name: &String| {
            self.packages
                .borrow()
                .get(name.as_str())
                .cloned()
                .into_iter()
                .flat_map(|p| p.releases.into_iter())
                .map(|p| p.version)
        };
        Ok(choose_package_with_fewest_versions(
            list_available_versions,
            potential_packages.into_iter(),
        ))
    }

    fn get_dependencies(
        &self,
        name: &PackageName,
        version: &Version,
    ) -> Result<Dependencies<PackageName, Version>, Box<dyn StdError>> {
        self.ensure_package_fetched(name)?;
        let packages = self.packages.borrow();
        let release = match packages
            .get(name.as_str())
            .into_iter()
            .flat_map(|p| p.releases.iter())
            .find(|r| &r.version == version)
        {
            Some(release) => release,
            None => return Ok(Dependencies::Unknown),
        };

        // Only use retired versions if they have been locked
        if release.is_retired() && self.locked.get(name.as_str()) != Some(version) {
            return Ok(Dependencies::Unknown);
        }

        let mut deps: Map<String, PubgrubRange> = Default::default();
        for (name, d) in &release.requirements {
            let range = d.requirement.to_pubgrub()?;
            let _ = deps.insert(name.clone(), range);
        }
        Ok(Dependencies::Known(deps))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Remote {
        deps: HashMap<String, hexpm::Package>,
    }

    impl PackageFetcher for Remote {
        fn get_dependencies(&self, package: &str) -> Result<hexpm::Package, Box<dyn StdError>> {
            self.deps
                .get(package)
                .cloned()
                .ok_or(Box::new(hexpm::ApiError::NotFound))
        }
    }

    fn make_remote() -> Box<Remote> {
        let mut deps = HashMap::new();
        let _ = deps.insert(
            "gleam_stdlib".into(),
            hexpm::Package {
                name: "gleam_stdlib".into(),
                repository: "hexpm".into(),
                releases: vec![
                    Release {
                        version: Version::try_from("0.1.0").unwrap(),
                        requirements: [].into(),
                        retirement_status: None,
                        outer_checksum: vec![1, 2, 3],
                        meta: (),
                    },
                    Release {
                        version: Version::try_from("0.2.0").unwrap(),
                        requirements: [].into(),
                        retirement_status: None,
                        outer_checksum: vec![1, 2, 3],
                        meta: (),
                    },
                    Release {
                        version: Version::try_from("0.2.2").unwrap(),
                        requirements: [].into(),
                        retirement_status: None,
                        outer_checksum: vec![1, 2, 3],
                        meta: (),
                    },
                    Release {
                        version: Version::try_from("0.3.0").unwrap(),
                        requirements: [].into(),
                        retirement_status: None,
                        outer_checksum: vec![1, 2, 3],
                        meta: (),
                    },
                ],
            },
        );
        let _ = deps.insert(
            "gleam_otp".into(),
            hexpm::Package {
                name: "gleam_otp".into(),
                repository: "hexpm".into(),
                releases: vec![
                    Release {
                        version: Version::try_from("0.1.0").unwrap(),
                        requirements: [(
                            "gleam_stdlib".into(),
                            Dependency {
                                app: None,
                                optional: false,
                                repository: None,
                                requirement: Range::new(">= 0.1.0".into()),
                            },
                        )]
                        .into(),
                        retirement_status: None,
                        outer_checksum: vec![1, 2, 3],
                        meta: (),
                    },
                    Release {
                        version: Version::try_from("0.2.0").unwrap(),
                        requirements: [(
                            "gleam_stdlib".into(),
                            Dependency {
                                app: None,
                                optional: false,
                                repository: None,
                                requirement: Range::new(">= 0.1.0".into()),
                            },
                        )]
                        .into(),
                        retirement_status: None,
                        outer_checksum: vec![1, 2, 3],
                        meta: (),
                    },
                    Release {
                        version: Version::try_from("0.3.0-rc1").unwrap(),
                        requirements: [(
                            "gleam_stdlib".into(),
                            Dependency {
                                app: None,
                                optional: false,
                                repository: None,
                                requirement: Range::new(">= 0.1.0".into()),
                            },
                        )]
                        .into(),
                        retirement_status: None,
                        outer_checksum: vec![1, 2, 3],
                        meta: (),
                    },
                ],
            },
        );
        let _ = deps.insert(
            "package_with_retired".into(),
            hexpm::Package {
                name: "package_with_retired".into(),
                repository: "hexpm".into(),
                releases: vec![
                    Release {
                        version: Version::try_from("0.1.0").unwrap(),
                        requirements: [].into(),
                        retirement_status: None,
                        outer_checksum: vec![1, 2, 3],
                        meta: (),
                    },
                    Release {
                        version: Version::try_from("0.2.0").unwrap(),
                        requirements: [].into(),
                        retirement_status: Some(hexpm::RetirementStatus {
                            reason: hexpm::RetirementReason::Security,
                            message: "It's bad".into(),
                        }),
                        outer_checksum: vec![1, 2, 3],
                        meta: (),
                    },
                ],
            },
        );
        Box::new(Remote { deps })
    }

    #[test]
    fn resolution_with_locked() {
        let locked_stdlib = ("gleam_stdlib".into(), Version::parse("0.1.0").unwrap());
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_stdlib".into(), Range::new("~> 0.1".into()))].into_iter(),
            &vec![locked_stdlib].into_iter().collect(),
        )
        .unwrap();
        assert_eq!(
            result,
            vec![("gleam_stdlib".into(), Version::parse("0.1.0").unwrap())]
                .into_iter()
                .collect()
        );
    }

    #[test]
    fn resolution_without_deps() {
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![].into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap();
        assert_eq!(result, vec![].into_iter().collect())
    }

    #[test]
    fn resolution_1_dep() {
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_stdlib".into(), Range::new("~> 0.1".into()))].into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap();
        assert_eq!(
            result,
            vec![("gleam_stdlib".into(), Version::try_from("0.3.0").unwrap())]
                .into_iter()
                .collect()
        );
    }

    #[test]
    fn resolution_with_nested_deps() {
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_otp".into(), Range::new("~> 0.1".into()))].into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap();
        assert_eq!(
            result,
            vec![
                ("gleam_otp".into(), Version::try_from("0.2.0").unwrap()),
                ("gleam_stdlib".into(), Version::try_from("0.3.0").unwrap())
            ]
            .into_iter()
            .collect()
        );
    }

    #[test]
    fn resolution_locked_to_older_version() {
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_otp".into(), Range::new("~> 0.1.0".into()))].into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap();
        assert_eq!(
            result,
            vec![
                ("gleam_otp".into(), Version::try_from("0.1.0").unwrap()),
                ("gleam_stdlib".into(), Version::try_from("0.3.0").unwrap())
            ]
            .into_iter()
            .collect()
        );
    }

    #[test]
    fn resolution_retired_versions_not_used_by_default() {
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("package_with_retired".into(), Range::new("> 0.0.0".into()))].into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap();
        assert_eq!(
            result,
            vec![(
                "package_with_retired".into(),
                // Uses the older version that hasn't been retired
                Version::try_from("0.1.0").unwrap()
            ),]
            .into_iter()
            .collect()
        );
    }

    #[test]
    fn resolution_retired_versions_can_be_used_if_locked() {
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("package_with_retired".into(), Range::new("> 0.0.0".into()))].into_iter(),
            &vec![("package_with_retired".into(), Version::new(0, 2, 0))]
                .into_iter()
                .collect(),
        )
        .unwrap();
        assert_eq!(
            result,
            vec![(
                "package_with_retired".into(),
                // Uses the locked version even though it's retired
                Version::new(0, 2, 0)
            ),]
            .into_iter()
            .collect()
        );
    }

    #[test]
    fn resolution_prerelease_can_be_selected() {
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_otp".into(), Range::new("~> 0.3.0-rc1".into()))].into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap();
        assert_eq!(
            result,
            vec![
                ("gleam_otp".into(), Version::try_from("0.3.0-rc1").unwrap()),
                ("gleam_stdlib".into(), Version::try_from("0.3.0").unwrap()),
            ]
            .into_iter()
            .collect(),
        );
    }

    #[test]
    fn resolution_not_found_dep() {
        let _ = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("unknown".into(), Range::new("~> 0.1".into()))].into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap_err();
    }

    #[test]
    fn resolution_no_matching_version() {
        let _ = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_stdlib".into(), Range::new("~> 99.0".into()))].into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap_err();
    }

    #[test]
    fn resolution_locked_version_doesnt_satisfy_requirements() {
        let err = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_stdlib".into(), Range::new("~> 0.1.0".into()))].into_iter(),
            &vec![("gleam_stdlib".into(), Version::new(0, 2, 0))]
                .into_iter()
                .collect(),
        )
        .unwrap_err();

        match err {
        Error::DependencyResolutionFailed(msg) => assert_eq!(
            msg,
            "An unrecoverable error happened while solving dependencies: gleam_stdlib is specified with the requirement `~> 0.1.0`, but it is locked to 0.2.0, which is incompatible."
        ),
        _ => panic!("wrong error: {}", err),
        }
    }
}
