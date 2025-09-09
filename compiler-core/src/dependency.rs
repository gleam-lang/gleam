use std::{cell::RefCell, cmp::Reverse, collections::HashMap, rc::Rc};

use crate::{Error, Result, manifest};

use ecow::EcoString;
use hexpm::{
    Dependency, Release,
    version::{Range, Version},
};
use pubgrub::{Dependencies, Map};
use thiserror::Error;

pub type PackageVersions = HashMap<String, Version>;

type PubgrubRange = pubgrub::Range<Version>;

pub fn resolve_versions<Requirements>(
    package_fetcher: &impl PackageFetcher,
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
    let requirements = root_dependencies(dependencies, locked)?;

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

    let packages = pubgrub::resolve(
        &DependencyProvider::new(package_fetcher, provided_packages, root, locked, exact_deps),
        root_name.as_str().into(),
        root_version,
    )
    .map_err(|error| Error::dependency_resolution_failed(error, root_name.clone()))?
    .into_iter()
    .filter(|(name, _)| name.as_str() != root_name.as_str())
    .collect();

    Ok(packages)
}

/**
* Used to compare 2 versions of a package.
*/
pub type PackageVersionDiffs = HashMap<String, (Version, Version)>;

fn resolve_major_versions(
    package_fetcher: &impl PackageFetcher,
    versions: PackageVersions,
) -> PackageVersionDiffs {
    versions
        .iter()
        .filter_map(|(package, version)| {
            let Ok(hexpackage) = package_fetcher.get_dependencies(package) else {
                return None;
            };

            let latest = hexpackage
                .releases
                .iter()
                .map(|release| &release.version)
                .filter(|version| !version.is_pre())
                .max()?;

            if latest.major <= version.major {
                return None;
            }

            Some((package.to_string(), (version.clone(), latest.clone())))
        })
        .collect()
}

/// Check for major version updates for direct dependencies that are being blocked by some version
/// constraints.
pub fn check_for_major_version_updates(
    manifest: &manifest::Manifest,
    package_fetcher: &impl PackageFetcher,
) -> PackageVersionDiffs {
    // get the resolved versions of the direct dependencies to check for major
    // version updates.
    let versions = manifest
        .packages
        .iter()
        .filter(|manifest_package| {
            manifest
                .requirements
                .iter()
                .any(|(required_pkg, _)| manifest_package.name == *required_pkg)
        })
        .map(|manifest_pkg| (manifest_pkg.name.to_string(), manifest_pkg.version.clone()))
        .collect();

    resolve_major_versions(package_fetcher, versions)
}

// If the string would parse to an exact version then return the version
fn parse_exact_version(ver: &str) -> Option<Version> {
    let version = ver.trim();
    let first_byte = version.as_bytes().first();

    // Version is exact if it starts with an explicit == or a number
    if version.starts_with("==") || first_byte.is_some_and(|v| v.is_ascii_digit()) {
        let version = version.replace("==", "");
        let version = version.as_str().trim();
        Version::parse(version).ok()
    } else {
        None
    }
}

fn root_dependencies<Requirements>(
    base_requirements: Requirements,
    locked: &HashMap<EcoString, Version>,
) -> Result<HashMap<String, Dependency>, Error>
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
                    requirement: version.clone().into(),
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
                let compatible = range.to_pubgrub().contains(locked_version);
                if !compatible {
                    return Err(Error::IncompatibleLockedVersion {
                        error: format!(
                            "{name} is specified with the requirement `{range}`, \
but it is locked to {locked_version}, which is incompatible.",
                        ),
                    });
                }
            }
        };
    }

    Ok(requirements)
}

pub trait PackageFetcher {
    fn get_dependencies(&self, package: &str) -> Result<Rc<hexpm::Package>, PackageFetchError>;
}

#[derive(Debug, Error)]
pub enum PackageFetchError {
    #[error("{0}")]
    ApiError(hexpm::ApiError),
    #[error("{0}")]
    FetchError(String),
}
impl From<hexpm::ApiError> for PackageFetchError {
    fn from(api_error: hexpm::ApiError) -> Self {
        Self::ApiError(api_error)
    }
}
impl PackageFetchError {
    pub fn fetch_error<T: std::error::Error>(err: T) -> Self {
        Self::FetchError(err.to_string())
    }
}

#[derive(Debug)]
pub struct DependencyProvider<'a, T: PackageFetcher> {
    packages: RefCell<HashMap<EcoString, hexpm::Package>>,
    remote: &'a T,
    locked: &'a HashMap<EcoString, Version>,
    // Map of packages where an exact version was requested
    // We need this because by default pubgrub checks exact version by checking if a version is between the exact
    // and the version 1 bump ahead. That default breaks on prerelease builds since a bump includes the whole patch
    exact_only: &'a HashMap<String, Version>,
    optional_dependencies: RefCell<HashMap<EcoString, pubgrub::Range<Version>>>,
}

impl<'a, T> DependencyProvider<'a, T>
where
    T: PackageFetcher,
{
    fn new(
        remote: &'a T,
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
    ) -> Result<(), PackageFetchError> {
        let mut packages = self.packages.borrow_mut();
        if packages.get(name).is_none() {
            let package = self.remote.get_dependencies(name)?;
            // mut (therefore clone) is required here in order to sort the releases
            let mut package = (*package).clone();
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
pub type ResolutionError<'a, T> = pubgrub::PubGrubError<DependencyProvider<'a, T>>;

impl<T> pubgrub::DependencyProvider for DependencyProvider<'_, T>
where
    T: PackageFetcher,
{
    fn get_dependencies(
        &self,
        package: &Self::P,
        version: &Self::V,
    ) -> Result<Dependencies<Self::P, Self::VS, Self::M>, Self::Err> {
        self.ensure_package_fetched(package)?;
        let packages = self.packages.borrow();
        let release = match packages
            .get(package.as_str())
            .into_iter()
            .flat_map(|p| p.releases.iter())
            .find(|r| &r.version == version)
        {
            Some(release) => release,
            None => {
                return Ok(Dependencies::Unavailable(format!(
                    "{package}@{version} is not available"
                )));
            }
        };

        // Only use retired versions if they have been locked
        if release.is_retired() && self.locked.get(package.as_str()) != Some(version) {
            return Ok(Dependencies::Unavailable(format!(
                "{package}@{version} is retired"
            )));
        }

        let mut deps: Map<PackageName, PubgrubRange> = Default::default();
        for (name, d) in &release.requirements {
            let mut range = d.requirement.to_pubgrub().clone();
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
        Ok(Dependencies::Available(deps))
    }

    fn prioritize(
        &self,
        package: &Self::P,
        range: &Self::VS,
        _package_conflicts_counts: &pubgrub::PackageResolutionStatistics,
    ) -> Self::Priority {
        Reverse(
            self.packages
                .borrow()
                .get(package.as_str())
                .cloned()
                .into_iter()
                .flat_map(|p| {
                    p.releases
                        .into_iter()
                        .filter(|r| range.contains(&r.version))
                })
                .count(),
        )
    }

    fn choose_version(
        &self,
        package: &Self::P,
        range: &Self::VS,
    ) -> std::result::Result<Option<Self::V>, Self::Err> {
        self.ensure_package_fetched(package)?;

        let exact_package = self.exact_only.get(package);
        let potential_versions = self
            .packages
            .borrow()
            .get(package.as_str())
            .cloned()
            .into_iter()
            .flat_map(move |p| {
                p.releases
                    .into_iter()
                    // if an exact version of a package is specified then we only want to allow that version as available
                    .filter_map(move |release| match exact_package {
                        Some(ver) => (ver == &release.version).then_some(release.version),
                        _ => Some(release.version),
                    })
            })
            .filter(|v| range.contains(v));
        match potential_versions.clone().filter(|v| !v.is_pre()).max() {
            // Don't resolve to a pre-releaase package unless we *have* to
            Some(v) => Ok(Some(v)),
            None => Ok(potential_versions.max()),
        }
    }

    type P = PackageName;
    type V = Version;
    type VS = PubgrubRange;
    type Priority = Reverse<usize>;
    type M = String;
    type Err = PackageFetchError;
}

#[cfg(test)]
mod tests {
    use hexpm::RetirementStatus;

    use crate::{
        derivation_tree::DerivationTreePrinter,
        manifest::{Base16Checksum, ManifestPackage, ManifestPackageSource},
        requirement,
    };

    use super::*;

    struct Remote {
        deps: HashMap<String, Rc<hexpm::Package>>,
    }

    impl PackageFetcher for Remote {
        fn get_dependencies(&self, package: &str) -> Result<Rc<hexpm::Package>, PackageFetchError> {
            self.deps
                .get(package)
                .map(Rc::clone)
                .ok_or(hexpm::ApiError::NotFound.into())
        }
    }

    fn make_remote() -> Remote {
        remote(vec![
            (
                "gleam_stdlib",
                vec![
                    release("0.1.0", vec![]),
                    release("0.2.0", vec![]),
                    release("0.2.2", vec![]),
                    release("0.3.0", vec![]),
                ],
            ),
            (
                "gleam_otp",
                vec![
                    release("0.1.0", vec![("gleam_stdlib", ">= 0.1.0")]),
                    release("0.2.0", vec![("gleam_stdlib", ">= 0.1.0")]),
                    release("0.3.0-rc1", vec![("gleam_stdlib", ">= 0.1.0")]),
                    release("0.3.0-rc2", vec![("gleam_stdlib", ">= 0.1.0")]),
                ],
            ),
            (
                "package_with_retired",
                vec![
                    release("0.1.0", vec![]),
                    retired_release(
                        "0.2.0",
                        vec![],
                        hexpm::RetirementReason::Security,
                        "it's bad",
                    ),
                ],
            ),
            (
                "package_with_optional",
                vec![release_with_optional(
                    "0.1.0",
                    vec![],
                    vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")],
                )],
            ),
            (
                "direct_pkg_with_major_version",
                vec![
                    release("0.1.0", vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")]),
                    release("1.0.0", vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")]),
                    release("1.1.0", vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")]),
                ],
            ),
            (
                "depends_on_old_version_of_direct_pkg",
                vec![release(
                    "0.1.0",
                    vec![("direct_pkg_with_major_version", ">= 0.1.0 and < 0.3.0")],
                )],
            ),
            (
                "this_pkg_depends_on_indirect_pkg",
                vec![release(
                    "0.1.0",
                    vec![("indirect_pkg_with_major_version", ">= 0.1.0 and < 1.0.0")],
                )],
            ),
            (
                "indirect_pkg_with_major_version",
                vec![
                    release("0.1.0", vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")]),
                    release("1.0.0", vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")]),
                    release("1.1.0", vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")]),
                ],
            ),
        ])
    }

    #[test]
    fn resolution_with_locked() {
        let locked_stdlib = ("gleam_stdlib".into(), Version::parse("0.1.0").unwrap());
        let result = resolve_versions(
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_stdlib".into(), Range::new("~> 0.1".into()).unwrap())].into_iter(),
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
            &make_remote(),
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
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_stdlib".into(), Range::new("~> 0.1".into()).unwrap())].into_iter(),
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
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_otp".into(), Range::new("~> 0.1".into()).unwrap())].into_iter(),
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
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![(
                "package_with_optional".into(),
                Range::new("~> 0.1".into()).unwrap(),
            )]
            .into_iter(),
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
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![
                (
                    "package_with_optional".into(),
                    Range::new("~> 0.1".into()).unwrap(),
                ),
                ("gleam_stdlib".into(), Range::new("~> 0.1".into()).unwrap()),
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
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![
                (
                    "package_with_optional".into(),
                    Range::new("~> 0.1".into()).unwrap(),
                ),
                ("gleam_stdlib".into(), Range::new("~> 0.3".into()).unwrap()),
            ]
            .into_iter(),
            &vec![].into_iter().collect(),
        );
        assert!(result.is_err());
    }

    #[test]
    fn resolution_with_optional_deps_required_by_nested_deps() {
        let result = resolve_versions(
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![
                (
                    "package_with_optional".into(),
                    Range::new("~> 0.1".into()).unwrap(),
                ),
                ("gleam_otp".into(), Range::new("~> 0.1".into()).unwrap()),
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
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_otp".into(), Range::new("~> 0.1.0".into()).unwrap())].into_iter(),
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
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![(
                "package_with_retired".into(),
                Range::new("> 0.0.0".into()).unwrap(),
            )]
            .into_iter(),
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
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![(
                "package_with_retired".into(),
                Range::new("> 0.0.0".into()).unwrap(),
            )]
            .into_iter(),
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
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![(
                "gleam_otp".into(),
                Range::new("~> 0.3.0-rc1".into()).unwrap(),
            )]
            .into_iter(),
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
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_otp".into(), Range::new("0.3.0-rc1".into()).unwrap())].into_iter(),
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
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("unknown".into(), Range::new("~> 0.1".into()).unwrap())].into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap_err();
    }

    #[test]
    fn resolution_no_matching_version() {
        let _ = resolve_versions(
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_stdlib".into(), Range::new("~> 99.0".into()).unwrap())].into_iter(),
            &vec![].into_iter().collect(),
        )
        .unwrap_err();
    }

    #[test]
    fn resolution_locked_version_doesnt_satisfy_requirements() {
        let err = resolve_versions(
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![(
                "gleam_stdlib".into(),
                Range::new("~> 0.1.0".into()).unwrap(),
            )]
            .into_iter(),
            &vec![("gleam_stdlib".into(), Version::new(0, 2, 0))]
                .into_iter()
                .collect(),
        )
        .unwrap_err();

        match err {
            Error::IncompatibleLockedVersion { error } => assert_eq!(
                error,
                "gleam_stdlib is specified with the requirement `~> 0.1.0`, but it is locked to 0.2.0, which is incompatible."
            ),
            _ => panic!("wrong error: {err}"),
        }
    }

    #[test]
    fn resolution_with_exact_dep() {
        let result = resolve_versions(
            &make_remote(),
            HashMap::new(),
            "app".into(),
            vec![("gleam_stdlib".into(), Range::new("0.1.0".into()).unwrap())].into_iter(),
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

    #[test]
    fn resolve_major_version_upgrades() {
        let manifest = manifest::Manifest {
            requirements: vec![
                (
                    EcoString::from("package_depends_on_indirect_pkg"),
                    requirement::Requirement::Hex {
                        version: Range::new("> 0.1.0 and <= 1.0.0".into()).unwrap(),
                    },
                ),
                (
                    EcoString::from("direct_pkg_with_major_version"),
                    requirement::Requirement::Hex {
                        version: Range::new("> 0.1.0 and <= 2.0.0".into()).unwrap(),
                    },
                ),
                (
                    EcoString::from("depends_on_old_version_of_direct_pkg"),
                    requirement::Requirement::Hex {
                        version: Range::new("> 0.1.0 and <= 1.0.0".into()).unwrap(),
                    },
                ),
            ]
            .into_iter()
            .collect(),
            packages: vec![
                ManifestPackage {
                    name: "direct_pkg_with_major_version".into(),
                    version: Version::parse("0.1.0").unwrap(),
                    build_tools: ["gleam".into()].into(),
                    otp_app: None,
                    requirements: vec![],
                    source: ManifestPackageSource::Hex {
                        outer_checksum: Base16Checksum(vec![1, 2, 3]),
                    },
                },
                ManifestPackage {
                    name: "depends_on_old_version_of_direct_pkg".into(),
                    version: Version::parse("0.1.0").unwrap(),
                    build_tools: ["gleam".into()].into(),
                    otp_app: None,
                    requirements: vec!["direct_pkg_with_major_version".into()],
                    source: ManifestPackageSource::Hex {
                        outer_checksum: Base16Checksum(vec![1, 2, 3]),
                    },
                },
                ManifestPackage {
                    name: "pkg_depends_on_indirect_pkg".into(),
                    version: Version::parse("0.1.0").unwrap(),
                    build_tools: ["gleam".into()].into(),
                    otp_app: None,
                    requirements: vec!["indirect_pkg_with_major_version".into()],
                    source: ManifestPackageSource::Hex {
                        outer_checksum: Base16Checksum(vec![1, 2, 3]),
                    },
                },
                ManifestPackage {
                    name: "indirect_pkg_with_major_version".into(),
                    version: Version::parse("0.1.0").unwrap(),
                    build_tools: ["gleam".into()].into(),
                    otp_app: None,
                    requirements: vec![],
                    source: ManifestPackageSource::Hex {
                        outer_checksum: Base16Checksum(vec![1, 2, 3]),
                    },
                },
            ],
        };
        let result = check_for_major_version_updates(&manifest, &make_remote());

        // indirect package with major version will not be in the result even though a major
        // version of it is available
        assert_eq!(
            result,
            vec![(
                "direct_pkg_with_major_version".into(),
                (
                    Version::try_from("0.1.0").unwrap(),
                    Version::try_from("1.1.0").unwrap()
                )
            ),]
            .into_iter()
            .collect()
        );
    }

    fn retired_release(
        version: &str,
        requirements: Vec<(&str, &str)>,
        reason: hexpm::RetirementReason,
        message: &str,
    ) -> Release<()> {
        Release {
            retirement_status: Some(RetirementStatus {
                reason,
                message: message.into(),
            }),
            ..release(version, requirements)
        }
    }
    fn release(version: &str, requirements: Vec<(&str, &str)>) -> Release<()> {
        release_with_optional(version, requirements, vec![])
    }

    fn release_with_optional(
        version: &str,
        requirements: Vec<(&str, &str)>,
        optional_requirements: Vec<(&str, &str)>,
    ) -> Release<()> {
        let mut all_requirements = HashMap::new();

        for (name, range) in requirements {
            let requirement = Range::new(range.to_string()).unwrap();
            let dependency = Dependency {
                requirement,
                optional: false,
                app: None,
                repository: None,
            };
            let _ = all_requirements.insert(name.to_string(), dependency);
        }

        for (name, range) in optional_requirements {
            let requirement = Range::new(range.to_string()).unwrap();
            let dependency = Dependency {
                requirement,
                optional: true,
                app: None,
                repository: None,
            };
            let _ = all_requirements.insert(name.to_string(), dependency);
        }

        Release {
            version: Version::try_from(version).unwrap(),
            requirements: all_requirements,
            retirement_status: None,
            outer_checksum: vec![1, 2, 3],
            meta: (),
        }
    }

    fn remote(dependencies: Vec<(&str, Vec<Release<()>>)>) -> Remote {
        let mut deps = HashMap::new();
        for (package, releases) in dependencies {
            let _ = deps.insert(
                package.into(),
                Rc::new(hexpm::Package {
                    name: package.into(),
                    repository: "hexpm".into(),
                    releases,
                }),
            );
        }
        Remote { deps }
    }

    #[test]
    fn resolution_error_message() {
        let remote = remote(vec![
            (
                "wibble",
                vec![
                    release("1.2.0", vec![("wobble", ">= 1.0.0 and < 2.0.0")]),
                    release("1.3.0", vec![("wobble", ">= 2.0.0 and < 3.0.0")]),
                ],
            ),
            (
                "wobble",
                vec![
                    release("1.1.0", vec![("woo", ">= 1.0.0 and < 2.0.0")]),
                    release("2.0.0", vec![("waa", ">= 1.0.0 and < 2.0.0")]),
                ],
            ),
            (
                "woo",
                vec![release("1.0.0", vec![]), release("2.0.0", vec![])],
            ),
            (
                "waa",
                vec![release("1.0.0", vec![]), release("2.0.0", vec![])],
            ),
        ]);

        let result = resolve_versions(
            &remote,
            HashMap::new(),
            "app".into(),
            vec![
                (
                    "wibble".into(),
                    Range::new(">= 1.0.0 and < 2.0.0".into()).unwrap(),
                ),
                (
                    "woo".into(),
                    Range::new(">= 2.0.0 and < 3.0.0".into()).unwrap(),
                ),
                (
                    "waa".into(),
                    Range::new(">= 2.0.0 and < 3.0.0".into()).unwrap(),
                ),
            ]
            .into_iter(),
            &vec![].into_iter().collect(),
        );

        if let Err(Error::DependencyResolutionNoSolution {
            root_package_name,
            derivation_tree,
        }) = result
        {
            let message = crate::error::wrap(
                &DerivationTreePrinter::new(root_package_name, derivation_tree.0).print(),
            );
            insta::assert_snapshot!(message)
        } else {
            panic!("expected a resolution error message")
        }
    }
}
