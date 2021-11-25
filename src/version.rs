//! Functions for parsing and matching versions against requirements, based off
//! and compatible with the Elixir Version module, which is used by Hex
//! internally as well as be the Elixir build tool Hex client.

use std::{
    borrow::Borrow, cell::RefCell, cmp::Ordering, collections::HashMap, convert::TryFrom,
    error::Error as StdError, fmt,
};

use crate::{Dependency, Package, Release};

use self::parser::Parser;
use pubgrub::{
    error::PubGrubError,
    solver::{choose_package_with_fewest_versions, Dependencies},
    type_aliases::Map,
};
use serde::{
    de::{self, Deserializer},
    Deserialize, Serialize,
};

pub use pubgrub::report as pubgrub_report;

mod lexer;
mod parser;
#[cfg(test)]
mod tests;

type PubgrubRange = pubgrub::range::Range<Version>;

/// In a nutshell, a version is represented by three numbers:
///
/// MAJOR.MINOR.PATCH
///
/// Pre-releases are supported by optionally appending a hyphen and a series of
/// period-separated identifiers immediately following the patch version.
/// Identifiers consist of only ASCII alphanumeric characters and hyphens (`[0-9A-Za-z-]`):
///
/// "1.0.0-alpha.3"
///
/// Build information can be added by appending a plus sign and a series of
/// dot-separated identifiers immediately following the patch or pre-release version.
/// Identifiers consist of only ASCII alphanumeric characters and hyphens (`[0-9A-Za-z-]`):
///
/// "1.0.0-alpha.3+20130417140000.amd64"
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub pre: Vec<Identifier>,
    pub build: Option<String>,
}

impl Version {
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        Self {
            major,
            minor,
            patch,
            pre: vec![],
            build: None,
        }
    }

    fn bump_major(&self) -> Self {
        Self {
            major: self.major + 1,
            minor: 0,
            patch: 0,
            pre: vec![],
            build: None,
        }
    }

    fn bump_minor(&self) -> Self {
        Self {
            major: self.major,
            minor: self.minor + 1,
            patch: 0,
            pre: vec![],
            build: None,
        }
    }

    fn bump_patch(&self) -> Self {
        Self {
            major: self.major,
            minor: self.minor,
            patch: self.patch + 1,
            pre: vec![],
            build: None,
        }
    }

    /// Parse a version.
    pub fn parse(input: &str) -> Result<Self, parser::Error> {
        let mut parser = Parser::new(input)?;
        let version = parser.version()?;
        if !parser.is_eof() {
            return Err(parser::Error::MoreInput(
                parser
                    .tail()?
                    .into_iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(""),
            ));
        }
        Ok(version)
    }

    /// Parse a Hex compatible version range. i.e. `> 1 and < 2 or == 4.5.2`.
    fn parse_range(input: &str) -> Result<pubgrub::range::Range<Version>, parser::Error> {
        let mut parser = Parser::new(input)?;
        let version = parser.range()?;
        if !parser.is_eof() {
            return Err(parser::Error::MoreInput(
                parser
                    .tail()?
                    .into_iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(""),
            ));
        }
        Ok(version)
    }

    fn tuple(&self) -> (u32, u32, u32, PreOrder<'_>) {
        (
            self.major,
            self.minor,
            self.patch,
            PreOrder(self.pre.as_slice()),
        )
    }

    pub fn is_pre(&self) -> bool {
        !self.pre.is_empty()
    }
}

impl<'de> Deserialize<'de> for Version {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: &str = Deserialize::deserialize(deserializer)?;
        Version::try_from(s).map_err(de::Error::custom)
    }
}

impl Serialize for Version {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl std::cmp::PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl std::cmp::Ord for Version {
    fn cmp(&self, other: &Self) -> Ordering {
        self.tuple().cmp(&other.tuple())
    }
}

impl pubgrub::version::Version for Version {
    fn lowest() -> Self {
        Self::new(0, 0, 0)
    }

    fn bump(&self) -> Self {
        self.bump_patch()
    }
}

impl<'a> TryFrom<&'a str> for Version {
    type Error = parser::Error;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        Self::parse(value)
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)?;
        if !self.pre.is_empty() {
            write!(f, "-")?;
            for (i, identifier) in self.pre.iter().enumerate() {
                if i != 0 {
                    write!(f, ".")?;
                }
                identifier.fmt(f)?;
            }
        }
        if let Some(build) = self.build.as_ref() {
            write!(f, "+{}", build)?;
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Identifier {
    Numeric(u32),
    AlphaNumeric(String),
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Identifier::Numeric(ref id) => id.fmt(f),
            Identifier::AlphaNumeric(ref id) => id.fmt(f),
        }
    }
}

impl Identifier {
    pub fn concat(self, add_str: &str) -> Identifier {
        match self {
            Identifier::Numeric(n) => Identifier::AlphaNumeric(format!("{}{}", n, add_str)),
            Identifier::AlphaNumeric(mut s) => {
                s.push_str(add_str);
                Identifier::AlphaNumeric(s)
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Range(String);

impl Range {
    pub fn new(spec: String) -> Self {
        Self(spec)
    }
}

impl Range {
    pub fn to_pubgrub(&self) -> Result<pubgrub::range::Range<Version>, parser::Error> {
        Version::parse_range(&self.0)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Debug for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Range").field(&self.0.to_string()).finish()
    }
}

impl<'de> Deserialize<'de> for Range {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: &str = Deserialize::deserialize(deserializer)?;
        Ok(Range::new(s.to_string()))
    }
}

impl Serialize for Range {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)?;
        Ok(())
    }
}

// A wrapper around Vec where an empty vector is greater than a non-empty one.
// This is desires as if there is a pre-segment in a version (1.0.0-rc1) it is
// lower than the same version with no pre-segments (1.0.0).
#[derive(PartialEq, Eq)]
pub struct PreOrder<'a>(&'a [Identifier]);

impl PreOrder<'_> {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl std::cmp::PartialOrd for PreOrder<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl std::cmp::Ord for PreOrder<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.is_empty() && other.is_empty() {
            Ordering::Equal
        } else if self.is_empty() {
            Ordering::Greater
        } else if other.is_empty() {
            Ordering::Less
        } else {
            self.0.cmp(other.0)
        }
    }
}

pub type PackageVersions = HashMap<String, Version>;

pub type ResolutionError = PubGrubError<String, Version>;

pub fn resolve_versions<Requirements>(
    remote: Box<dyn PackageFetcher>,
    root_name: PackageName,
    dependencies: Requirements,
    locked: &HashMap<String, Version>,
) -> Result<PackageVersions, ResolutionError>
where
    Requirements: Iterator<Item = (String, Range)>,
{
    let root_version = Version::new(0, 0, 0);
    let root = Package {
        name: root_name.clone(),
        repository: "local".to_string(),
        releases: vec![Release {
            version: root_version.clone(),
            outer_checksum: vec![],
            retirement_status: None,
            requirements: root_dependencies(dependencies, &locked),
            meta: (),
        }],
    };
    let packages = pubgrub::solver::resolve(
        &DependencyProvider::new(remote, root, locked),
        root_name.clone(),
        root_version,
    )?
    .into_iter()
    .filter(|(name, _)| name.as_str() != root_name.as_str())
    .collect();

    Ok(packages)
}

fn root_dependencies<Requirements>(
    dependencies: Requirements,
    locked: &HashMap<String, Version>,
) -> HashMap<String, Dependency>
where
    Requirements: Iterator<Item = (String, Range)>,
{
    let locked = locked
        .iter()
        .map(|(name, version)| (name.to_string(), Range::new(version.to_string())));
    // Add the locked versions as new requirements that override any existing
    // entry in the dependencies list. Collection into a HashMap is used for
    // de-duplication.
    let deps: HashMap<_, _> = dependencies.chain(locked).collect();
    deps.into_iter()
        .map(|(package, requirement)| {
            (
                package,
                Dependency {
                    app: None,
                    optional: false,
                    repository: None,
                    requirement,
                },
            )
        })
        .collect()
}

pub trait PackageFetcher {
    fn get_dependencies(&self, package: &str) -> Result<Package, Box<dyn StdError>>;
}

struct DependencyProvider<'a> {
    packages: RefCell<HashMap<String, Package>>,
    remote: Box<dyn PackageFetcher>,
    locked: &'a HashMap<String, Version>,
}

impl<'a> DependencyProvider<'a> {
    fn new(
        remote: Box<dyn PackageFetcher>,
        root: Package,
        locked: &'a HashMap<String, Version>,
    ) -> Self {
        let mut packages = HashMap::new();
        let _ = packages.insert(root.name.clone(), root);
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
            let (pre, mut norm): (_, Vec<_>) =
                package.releases.into_iter().partition(Release::is_pre);
            norm.extend(pre);
            package.releases = norm;
            packages.insert(name.to_string(), package);
        }
        Ok(())
    }
}

type PackageName = String;

impl<'a> pubgrub::solver::DependencyProvider<PackageName, Version> for DependencyProvider<'a> {
    fn choose_package_version<
        Name: Borrow<PackageName>,
        Ver: Borrow<pubgrub::range::Range<Version>>,
    >(
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
                .get(name)
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
    ) -> Result<pubgrub::solver::Dependencies<PackageName, Version>, Box<dyn StdError>> {
        self.ensure_package_fetched(name)?;
        let packages = self.packages.borrow();
        let release = match packages
            .get(name)
            .into_iter()
            .flat_map(|p| p.releases.iter())
            .find(|r| &r.version == version)
        {
            Some(release) => release,
            None => return Ok(Dependencies::Unknown),
        };

        // Only use retired versions if they have been locked
        if release.is_retired() && self.locked.get(name) != Some(version) {
            return Ok(Dependencies::Unknown);
        }

        let mut deps: Map<String, PubgrubRange> = Default::default();
        for (name, d) in &release.requirements {
            let range = d.requirement.to_pubgrub()?;
            deps.insert(name.clone(), range);
        }
        Ok(Dependencies::Known(deps))
    }
}
