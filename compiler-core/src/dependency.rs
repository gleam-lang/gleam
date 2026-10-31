// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2023 The Gleam contributors

#[cfg(test)]
mod tests;

use crate::{Error, Result, manifest};
use clap::ValueEnum;
use ecow::EcoString;
use hexpm::{
    Dependency, Release,
    version::{Range, Version},
};
use pubgrub::{Dependencies, DerivationTree, Map};
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    cmp::{Ordering, Reverse},
    collections::HashMap,
    fmt,
    ops::Bound,
    rc::Rc,
};
use strum::{EnumString, VariantNames};
use thiserror::Error;

pub type PackageVersions = HashMap<String, Version>;

type PubgrubRange = pubgrub::Range<Presence>;

/// A resolved package version, or `Absent` when the package is not part of the
/// dependency tree at all.
///
/// Hex's optional requirements do not pull a package in, they only constrain
/// one that something else has already pulled in. pubgrub has no way to say
/// "this package, if it is here at all", so absence is modelled as a version
/// that only optional requirements admit. A package resolves to `Absent`
/// exactly when every requirement on it was optional.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Presence {
    Absent,
    Present(Version),
}

/// `Absent` sorts below every version.
///
/// That ordering is what lets a requirement rule absence in or out.
/// `to_presence_range` gives a mandatory requirement the lower bound
/// `Bound::Excluded(Absent)`, which admits every version and nothing else.
impl Ord for Presence {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Presence::Absent, Presence::Absent) => Ordering::Equal,
            (Presence::Absent, Presence::Present(_)) => Ordering::Less,
            (Presence::Present(_), Presence::Absent) => Ordering::Greater,
            (Presence::Present(left), Presence::Present(right)) => left.cmp(right),
        }
    }
}

impl PartialOrd for Presence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for Presence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Presence::Absent => write!(f, "absent"),
            Presence::Present(version) => version.fmt(f),
        }
    }
}

/// Lift a requirement over Hex versions into one over `Presence`.
///
/// An unbounded lower end excludes `Absent`, so a mandatory requirement does
/// not admit absence.
fn to_presence_range(range: &pubgrub::Range<Version>) -> PubgrubRange {
    range
        .iter()
        .map(|(lower, upper)| {
            let lower = to_presence_bound(lower, Bound::Excluded(Presence::Absent));
            let upper = to_presence_bound(upper, Bound::Unbounded);
            PubgrubRange::from_range_bounds((lower, upper))
        })
        .fold(PubgrubRange::empty(), |range, segment| {
            range.union(&segment)
        })
}

fn to_presence_bound(bound: &Bound<Version>, unbounded: Bound<Presence>) -> Bound<Presence> {
    match bound {
        Bound::Unbounded => unbounded,
        Bound::Included(version) => Bound::Included(Presence::Present(version.clone())),
        Bound::Excluded(version) => Bound::Excluded(Presence::Present(version.clone())),
    }
}

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

    // Creating a map of all the required packages that have exact versions
    // specified
    let exact_deps = &requirements
        .iter()
        .filter_map(|(name, dependency)| {
            parse_exact_version(dependency.requirement.as_str()).map(|version| (name, version))
        })
        .map(|(name, version)| (name.clone(), version))
        .collect();

    let root = hexpm::Package {
        name: root_name.as_str().into(),
        repository: "local".into(),
        advisories: vec![],
        releases: vec![Release {
            version: root_version.clone(),
            outer_checksum: vec![],
            retirement_status: None,
            requirements,
            security_advisories: vec![],
            meta: (),
        }],
    };

    let packages = pubgrub::resolve(
        &DependencyProvider::new(package_fetcher, provided_packages, root, locked, exact_deps),
        root_name.as_str().into(),
        Presence::Present(root_version),
    )
    .map_err(|error| Error::dependency_resolution_failed(error, root_name.clone()))?
    .into_iter()
    .filter_map(|(name, version)| match version {
        // Packages that only ever had optional requirements on them are not
        // part of the dependency tree.
        Presence::Absent => None,
        Presence::Present(_) if name.as_str() == root_name.as_str() => None,
        Presence::Present(version) => Some((name, version)),
    })
    .collect();

    Ok(packages)
}

/**
* Used to compare 2 versions of a package.
*/
pub type PackageVersionDiffs = HashMap<String, (Version, Version)>;

fn resolve_versions_diffs(
    package_fetcher: &impl PackageFetcher,
    versions: PackageVersions,
    check_major_versions: bool,
) -> PackageVersionDiffs {
    versions
        .iter()
        .filter_map(|(package, version)| {
            let Ok(hex_package) = package_fetcher.get_package(package) else {
                return None;
            };

            let latest = hex_package
                .releases
                .iter()
                .map(|release| &release.version)
                .filter(|version| !version.is_pre())
                .max()?;

            // If we're checking for major version updates, only include the
            // package if a new major version is available. Otherwise, include
            // the package if there is any new version available.
            match check_major_versions {
                true => {
                    if latest.major <= version.major {
                        return None;
                    }
                }
                false => {
                    if latest <= version {
                        return None;
                    }
                }
            }

            Some((package.to_string(), (version.clone(), latest.clone())))
        })
        .collect()
}

/// Check for major version updates for direct dependencies that are being
/// blocked by some version constraints.
pub fn check_for_major_version_updates(
    manifest: &manifest::Manifest,
    package_fetcher: &impl PackageFetcher,
) -> PackageVersionDiffs {
    let versions: PackageVersions = manifest
        .packages
        .iter()
        .filter(|manifest_package| {
            // It makes sense to check for major version upgrades only for
            // packages that are actually published.
            // git and path dependencies are skipped.
            manifest_package.is_hex()
                && manifest
                    .requirements
                    .iter()
                    .any(|(required_package, _)| manifest_package.name == *required_package)
        })
        .map(|manifest_package| {
            (
                manifest_package.name.to_string(),
                manifest_package.version.clone(),
            )
        })
        .collect();

    resolve_versions_diffs(package_fetcher, versions, true)
}

/// Check for version updates for direct and transitive dependencies that are
/// being blocked by some version constraints.
pub fn get_available_updates(
    manifest: &manifest::Manifest,
    package_fetcher: &impl PackageFetcher,
) -> PackageVersionDiffs {
    let versions = manifest
        .packages
        .iter()
        .filter(|manifest_package| manifest_package.is_hex())
        .map(|manifest_package| {
            (
                manifest_package.name.to_string(),
                manifest_package.version.clone(),
            )
        })
        .collect();

    resolve_versions_diffs(package_fetcher, versions, false)
}

pub fn get_audit(
    manifest: &manifest::Manifest,
    package_fetcher: &impl PackageFetcher,
) -> Result<Audit, PackageFetchError> {
    let mut advisories = Audit::default();

    for package in &manifest.packages {
        let name = &package.name;
        let version = &package.version;

        // We can only look-up advisories information for dependencies from Hex.
        if !package.is_hex() {
            continue;
        }

        let hex_package = package_fetcher.get_package(&package.name)?;

        let Some(release) = hex_package
            .releases
            .iter()
            .find(|release| release.version == package.version)
        else {
            let package = format!("{name}@{version}");
            return Err(PackageFetchError::NotFoundError(package));
        };

        advisories.record_problems(name, version, release);
    }

    Ok(advisories)
}

#[derive(Debug, EnumString, VariantNames, ValueEnum, Clone, Copy, PartialEq, Eq)]
pub enum AuditOutputFormat {
    Text,
    Json,
}

#[derive(Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Audit {
    pub packages: Vec<AuditPackage>,
}

impl Audit {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.packages.is_empty()
    }

    pub fn as_output(&self, format: AuditOutputFormat) -> String {
        match format {
            AuditOutputFormat::Text => self.as_json(),
            AuditOutputFormat::Json => self.as_text(),
        }
    }

    fn as_json(&self) -> String {
        serde_json::to_string_pretty(self).expect("JSON audit serialisation")
    }

    fn as_text(&self) -> String {
        let mut out = String::new();

        for package in &self.packages {
            out.push_str(&package.name);
            out.push(' ');
            out.push_str(&package.version.to_string().as_str());

            if let Some(retirement) = package.retirement.as_ref() {
                out.push_str("\n  Retired (");
                out.push_str(retirement.reason.to_str());
                out.push_str(")\n  ");
                out.push_str(retirement.message.as_str());
                out.push('\n');
            }

            for advisory in &package.advisories {
                out.push_str("\n  ");
                out.push_str(&advisory.id);
                if let Some(rating) = advisory.cvss_rating.as_ref() {
                    out.push_str(" (");
                    out.push_str(rating.to_str());
                    out.push(')');
                }
                if !advisory.aliases.is_empty() {
                    let mut first = true;
                    for alias in &advisory.aliases {
                        if first {
                            first = false;
                            out.push_str("\n  aliases: ");
                        } else {
                            out.push_str(", ");
                        }
                        out.push_str(alias);
                    }
                }
                out.push_str("\n  ");
                out.push_str(&advisory.summary);
                out.push_str("\n  ");
                out.push_str(&advisory.html_url);
                out.push('\n');
            }

            out.push('\n');
        }

        out
    }

    pub fn record_problems(
        &mut self,
        package: &EcoString,
        version: &Version,
        release: &Release<()>,
    ) {
        if !release.retirement_status.is_some() && release.security_advisories.is_empty() {
            return;
        }

        let advisories = release
            .security_advisories
            .iter()
            .map(|advisory| SecurityAdvisory {
                id: advisory.id.clone(),
                summary: advisory.summary.clone(),
                html_url: advisory.html_url.clone(),
                cvss_rating: advisory.cvss_rating.clone(),
                aliases: advisory.aliases.clone(),
            })
            .collect();

        self.packages.push(AuditPackage {
            name: package.clone(),
            version: version.clone(),
            retirement: release.retirement_status.clone(),
            advisories,
        });
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct AuditPackage {
    pub name: EcoString,
    pub version: Version,
    pub retirement: Option<hexpm::RetirementStatus>,
    pub advisories: Vec<SecurityAdvisory>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct SecurityAdvisory {
    pub id: String,
    pub summary: String,
    pub html_url: String,
    pub cvss_rating: Option<hexpm::AdvisorySeverity>,
    pub aliases: Vec<String>,
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
        }
    }

    Ok(requirements)
}

pub trait PackageFetcher {
    fn get_package(&self, package: &str) -> Result<Rc<hexpm::Package>, PackageFetchError>;
}

#[derive(Debug, Error)]
pub enum PackageFetchError {
    #[error("the package {0} was not found in the package repository")]
    NotFoundError(String),
    #[error("{0}")]
    ApiError(hexpm::ApiError),
    #[error("{0}")]
    FetchError(String),
}

impl PackageFetchError {
    pub fn fetch_error<T: std::error::Error>(err: T) -> Self {
        Self::FetchError(err.to_string())
    }

    pub fn from_api_error(api_error: hexpm::ApiError, package: &str) -> Self {
        match &api_error {
            hexpm::ApiError::NotFound => Self::NotFoundError(package.to_string()),

            hexpm::ApiError::Json(_)
            | hexpm::ApiError::IncorrectOneTimePassword
            | hexpm::ApiError::OAuthRefreshTokenRejected
            | hexpm::ApiError::OAuthAccessDenied
            | hexpm::ApiError::OAuthTimeout
            | hexpm::ApiError::ExpiredToken
            | hexpm::ApiError::Io(_)
            | hexpm::ApiError::InvalidProtobuf(_)
            | hexpm::ApiError::UnexpectedResponse(_, _)
            | hexpm::ApiError::RateLimited
            | hexpm::ApiError::InvalidCredentials
            | hexpm::ApiError::InvalidPackageNameFormat(_)
            | hexpm::ApiError::IncorrectPayloadSignature
            | hexpm::ApiError::InvalidVersionFormat(_)
            | hexpm::ApiError::InvalidVersionRequirementFormat(_)
            | hexpm::ApiError::IncorrectChecksum
            | hexpm::ApiError::Forbidden
            | hexpm::ApiError::NotReplacing
            | hexpm::ApiError::LateDeletion
            | hexpm::ApiError::LateModification => Self::ApiError(api_error),
        }
    }
}

#[derive(Debug)]
pub struct DependencyProvider<'a, T: PackageFetcher> {
    packages: RefCell<HashMap<EcoString, hexpm::Package>>,
    remote: &'a T,
    locked: &'a HashMap<EcoString, Version>,
    // Map of packages where an exact version was requested
    // We need this because by default pubgrub checks exact version by checking
    // if a version is between the exact and the version 1 bump ahead.
    // That default breaks on prerelease builds since a bump includes the whole
    // patch.
    exact_only: &'a HashMap<String, Version>,
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
            let package = self.remote.get_package(name)?;
            // mut (therefore clone) is required here in order to sort the releases
            let mut package = (*package).clone();
            // Sort the packages from newest to oldest, pres after all others
            package.releases.sort_by(|a, b| a.version.cmp(&b.version));
            package.releases.reverse();
            let (pre, mut norm): (_, Vec<_>) = package
                .releases
                .into_iter()
                .partition(|release| release.version.is_pre());
            norm.extend(pre);
            package.releases = norm;
            let _ = packages.insert(name.into(), package);
        }
        Ok(())
    }
}

type PackageName = String;
pub type ResolutionError<'a, T> = pubgrub::PubGrubError<DependencyProvider<'a, T>>;

/// Why resolution could not find a set of versions that satisfies everything.
///
#[derive(Debug, Clone)]
pub struct ResolutionFailure(pub(crate) DerivationTree<PackageName, PubgrubRange, String>);

impl<T> pubgrub::DependencyProvider for DependencyProvider<'_, T>
where
    T: PackageFetcher,
{
    fn get_dependencies(
        &self,
        package: &Self::P,
        version: &Self::V,
    ) -> Result<Dependencies<Self::P, Self::VS, Self::M>, Self::Err> {
        let Presence::Present(version) = version else {
            return Ok(Dependencies::Available(Map::default()));
        };

        self.ensure_package_fetched(package)?;
        let packages = self.packages.borrow();
        let release = match packages
            .get(package.as_str())
            .into_iter()
            .flat_map(|package| package.releases.iter())
            .find(|release| &release.version == version)
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

        let absent = PubgrubRange::singleton(Presence::Absent);
        let mut deps: Map<PackageName, PubgrubRange> = Map::default();
        for (name, dependency) in &release.requirements {
            let requirement = to_presence_range(dependency.requirement.to_pubgrub());
            // Admitting absence is the whole of what makes a requirement
            // optional. pubgrub then intersects it with every other requirement
            // on the package in the same pass, so the outcome does not depend
            // on the order packages happen to be visited in.
            let range = if dependency.optional {
                requirement.union(&absent)
            } else {
                requirement
            };

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
                .flat_map(|package| {
                    package.releases.into_iter().filter(|release| {
                        range.contains(&Presence::Present(release.version.clone()))
                    })
                })
                .count(),
        )
    }

    fn choose_version(
        &self,
        package: &Self::P,
        range: &Self::VS,
    ) -> std::result::Result<Option<Self::V>, Self::Err> {
        // Nothing mandatory requires this package, so it stays out of the
        // dependency tree and never needs fetching.
        if range.contains(&Presence::Absent) {
            return Ok(Some(Presence::Absent));
        }

        self.ensure_package_fetched(package)?;

        let exact_package = self.exact_only.get(package);
        let potential_versions = self
            .packages
            .borrow()
            .get(package.as_str())
            .cloned()
            .into_iter()
            .flat_map(move |package| {
                package
                    .releases
                    .into_iter()
                    // if an exact version of a package is specified then we
                    // only want to allow that version as available.
                    .filter_map(move |release| match exact_package {
                        Some(version) => (version == &release.version).then_some(release.version),
                        _ => Some(release.version),
                    })
            })
            .filter(|version| range.contains(&Presence::Present(version.clone())));
        match potential_versions
            .clone()
            .filter(|version| !version.is_pre())
            .max()
        {
            // Don't resolve to a pre-releaase package unless we *have* to
            Some(version) => Ok(Some(Presence::Present(version))),
            None => Ok(potential_versions.max().map(Presence::Present)),
        }
    }

    type P = PackageName;
    type V = Presence;
    type VS = PubgrubRange;
    type Priority = Reverse<usize>;
    type M = String;
    type Err = PackageFetchError;
}
