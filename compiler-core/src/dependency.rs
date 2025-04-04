use std::{borrow::Borrow, cell::RefCell, collections::HashMap, error::Error as StdError};

use crate::{Error, Result};

use ecow::EcoString;
use hexpm::{
    version::{Range, ResolutionError, Version},
    Dependency, Release,
};
use pubgrub::{
    solver::{choose_package_with_fewest_versions, Dependencies},
    type_aliases::Map,
};

pub type PackageVersions = HashMap<String, Version>;

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

    let requirements = root_dependencies(dependencies, locked)
        .map_err(|err| Error::dependency_resolution_failed(err, locked))?;

    // Creating a map of all the required packages that have exact versions specified
    let exact_deps = &requirements
        .iter()
        .filter_map(|(name, dep)| parse_exact_version(dep.requirement.as_str()).map(|v| (name, v)))
        .map(|(name, version)| (name.clone(), version))
        .collect();

    let root = hexpm::Package {
        name: root_name.as_str().into(),
        repository: "local".into(),
        releases: vec![Release {
            version: root_version.clone(),
            outer_checksum: vec![],
            retirement_status: None,
            requirements,
            meta: (),
        }],
    };

    let packages = pubgrub::solver::resolve(
        &DependencyProvider::new(package_fetcher, provided_packages, root, locked, exact_deps),
        root_name.as_str().into(),
        root_version,
    )
    .map_err(|err| Error::dependency_resolution_failed(err, locked))?
    .into_iter()
    .filter(|(name, _)| name.as_str() != root_name.as_str())
    .collect();

    Ok(packages)
}

// If the string would parse to an exact version then return the version
fn parse_exact_version(ver: &str) -> Option<Version> {
    let version = ver.trim();
    let first_byte = version.as_bytes().first();

    // Version is exact if it starts with an explicit == or a number
    if version.starts_with("==") || first_byte.map_or(false, |v| v.is_ascii_digit()) {
        let version = version.replace("==", "");
        let version = version.as_str().trim();
        if let Ok(v) = Version::parse(version) {
            Some(v)
        } else {
            None
        }
    } else {
        None
    }
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
                    .map_err(|e| ResolutionError::Failure(format!("Failed to parse range {e}")))?
                    .contains(locked_version);
                if !compatible {
                    return Err(ResolutionError::Failure(format!(
                        "{name} is specified with the requirement `{range}`, \
but it is locked to {locked_version}, which is incompatible.",
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
    // Map of packages where an exact version was requested
    // We need this because by default pubgrub checks exact version by checking if a version is between the exact
    // and the version 1 bump ahead. That default breaks on prerelease builds since a bump includes the whole patch
    exact_only: &'a HashMap<String, Version>,
    optional_dependencies: RefCell<HashMap<EcoString, pubgrub::range::Range<Version>>>,
}

impl<'a> DependencyProvider<'a> {
    fn new(
        remote: Box<dyn PackageFetcher>,
        mut packages: HashMap<EcoString, hexpm::Package>,
        root: hexpm::Package,
        locked: &'a HashMap<EcoString, Version>,
        exact_only: &'a HashMap<String, Version>,
    ) -> Self {
        let _ = packages.insert(root.name.as_str().into(), root);
        Self {
            packages: RefCell::new(packages),
            locked,
            remote,
            exact_only,
            optional_dependencies: RefCell::new(Default::default()),
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

impl pubgrub::solver::DependencyProvider<PackageName, Version> for DependencyProvider<'_> {
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
        let list_available_versions = |name: &PackageName| {
            let name = name.as_str();
            let exact_package = self.exact_only.get(name);
            self.packages
                .borrow()
                .get(name)
                .cloned()
                .into_iter()
                .flat_map(move |p| {
                    p.releases
                        .into_iter()
                        // if an exact version of a package is specified then we only want to allow that version as available
                        .filter(move |release| match exact_package {
                            Some(ver) => ver == &release.version,
                            _ => true,
                        })
                })
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

        let mut deps: Map<PackageName, PubgrubRange> = Default::default();
        for (name, d) in &release.requirements {
            let mut range = d.requirement.to_pubgrub()?;
            let mut opt_deps = self.optional_dependencies.borrow_mut();
            // if it's optional and it was not provided yet, store and skip
            if d.optional && !packages.contains_key(name.as_str()) {
                let _ = opt_deps
                    .entry(name.into())
                    .and_modify(|stored_range| {
                        *stored_range = range.intersection(stored_range);
                    })
                    .or_insert(range);
                continue;
            }

            // if a now required dep was optional before, add back the constraints
            if let Some(other_range) = opt_deps.remove(name.as_str()) {
                range = range.intersection(&other_range);
            }

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
                    Release {
                        version: Version::try_from("0.3.0-rc2").unwrap(),
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

        let _ = deps.insert(
            "package_with_optional".into(),
            hexpm::Package {
                name: "package_with_optional".into(),
                repository: "hexpm".into(),
                releases: vec![Release {
                    version: Version::try_from("0.1.0").unwrap(),
                    requirements: [(
                        "gleam_stdlib".into(),
                        Dependency {
                            app: None,
                            optional: true,
                            repository: None,
                            requirement: Range::new(">= 0.1.0 and < 0.3.0".into()),
                        },
                    )]
                    .into(),
                    retirement_status: None,
                    outer_checksum: vec![1, 2, 3],
                    meta: (),
                }],
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
    fn resolution_with_optional_deps() {
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("package_with_optional".into(), Range::new("~> 0.1".into()))].into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap();
        assert_eq!(
            result,
            vec![(
                "package_with_optional".into(),
                Version::try_from("0.1.0").unwrap()
            )]
            .into_iter()
            .collect()
        );
    }

    #[test]
    fn resolution_with_optional_deps_explicitly_provided() {
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![
                ("package_with_optional".into(), Range::new("~> 0.1".into())),
                ("gleam_stdlib".into(), Range::new("~> 0.1".into())),
            ]
            .into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap();
        assert_eq!(
            result,
            vec![
                ("gleam_stdlib".into(), Version::try_from("0.2.2").unwrap()),
                (
                    "package_with_optional".into(),
                    Version::try_from("0.1.0").unwrap()
                ),
            ]
            .into_iter()
            .collect()
        );
    }

    #[test]
    fn resolution_with_optional_deps_incompatible() {
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![
                ("package_with_optional".into(), Range::new("~> 0.1".into())),
                ("gleam_stdlib".into(), Range::new("~> 0.3".into())),
            ]
            .into_iter(),
            &vec![].into_iter().collect(),
        );
        assert!(result.is_err());
    }

    #[test]
    fn resolution_with_optional_deps_required_by_nested_deps() {
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![
                ("package_with_optional".into(), Range::new("~> 0.1".into())),
                ("gleam_otp".into(), Range::new("~> 0.1".into())),
            ]
            .into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap();
        assert_eq!(
            result,
            vec![
                ("gleam_stdlib".into(), Version::try_from("0.2.2").unwrap()),
                ("gleam_otp".into(), Version::try_from("0.2.0").unwrap()),
                (
                    "package_with_optional".into(),
                    Version::try_from("0.1.0").unwrap()
                ),
            ]
            .into_iter()
            .collect()
        );
    }

    #[test]
    fn resolution_with_optional_deps_keep_constraints() {}

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
                ("gleam_stdlib".into(), Version::try_from("0.3.0").unwrap()),
                ("gleam_otp".into(), Version::try_from("0.3.0-rc2").unwrap()),
            ]
            .into_iter()
            .collect(),
        );
    }

    #[test]
    fn resolution_exact_prerelease_can_be_selected() {
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_otp".into(), Range::new("0.3.0-rc1".into()))].into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap();
        assert_eq!(
            result,
            vec![
                ("gleam_stdlib".into(), Version::try_from("0.3.0").unwrap()),
                ("gleam_otp".into(), Version::try_from("0.3.0-rc1").unwrap()),
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
            Error::DependencyResolutionFailed {
                error,
                locked_conflicts,
            } => {
                assert_eq!(
                    error,
                    "An unrecoverable error happened while solving dependencies: gleam_stdlib is specified with the requirement `~> 0.1.0`, but it is locked to 0.2.0, which is incompatible."
                );
                assert_eq!(locked_conflicts, Vec::<EcoString>::new());
            }
            _ => panic!("wrong error: {err}"),
        }
    }

    #[test]
    fn resolution_locked_version_doesnt_satisfy_requirements_locked() {
        // We're creating a dependency logging v1.4.0 that requires gleam_stdlib v0.40.0
        let mut requirements: HashMap<String, Dependency> = HashMap::new();
        let _ = requirements.insert(
            "gleam_stdlib".to_string(),
            Dependency {
                requirement: Range::new("~> 0.40.0".to_string()),
                optional: false,
                app: None,
                repository: None,
            },
        );
        let mut provided_packages: HashMap<EcoString, hexpm::Package> = HashMap::new();
        let _ = provided_packages.insert(
            "logging".into(),
            hexpm::Package {
                name: "logging".to_string(),
                repository: "repository".to_string(),
                releases: vec![Release {
                    version: Version::new(1, 4, 0),
                    requirements: requirements,
                    retirement_status: None,
                    outer_checksum: vec![0],
                    meta: (),
                }],
            },
        );

        // Now try and resolve versions with gleam_stdlib v0.20.0 in lock.
        let err = resolve_versions(
            make_remote(),
            provided_packages,
            "root_name".into(),
            vec![("logging".into(), Range::new(">= 1.3.0 and < 2.0.0".into()))].into_iter(),
            &vec![("gleam_stdlib".into(), Version::new(0, 20, 0))]
                .into_iter()
                .collect(),
        )
        .unwrap_err();

        match err {
            Error::DependencyResolutionFailed {
                error,
                locked_conflicts,
            } => {
                assert!(error.contains("Unable to find compatible versions for the version constraints in your gleam.toml."));
                assert!(error.contains("The conflicting packages are:"));
                assert!(error.contains("- root_name"));
                assert!(error.contains("- gleam_stdlib"));
                assert_eq!(locked_conflicts, vec!["gleam_stdlib"])
            }
            _ => panic!("wrong error: {err}"),
        }
    }

    #[test]
    fn resolution_with_exact_dep() {
        let result = resolve_versions(
            make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_stdlib".into(), Range::new("0.1.0".into()))].into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap();
        assert_eq!(
            result,
            vec![("gleam_stdlib".into(), Version::try_from("0.1.0").unwrap())]
                .into_iter()
                .collect()
        );
    }

    #[test]
    fn parse_exact_version_test() {
        assert_eq!(
            parse_exact_version("1.0.0"),
            Some(Version::parse("1.0.0").unwrap())
        );
        assert_eq!(
            parse_exact_version("==1.0.0"),
            Some(Version::parse("1.0.0").unwrap())
        );
        assert_eq!(
            parse_exact_version("== 1.0.0"),
            Some(Version::parse("1.0.0").unwrap())
        );
        assert_eq!(parse_exact_version("~> 1.0.0"), None);
        assert_eq!(parse_exact_version(">= 1.0.0"), None);
    }
}
