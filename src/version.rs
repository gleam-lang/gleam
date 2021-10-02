//! Functions for parsing and matching versions against requirements, based off
//! and compatible with the Elixir Version module, which is used by Hex
//! internally as well as be the Elixir build tool Hex client.

// TODO: FIXME: Make it so prereleases are excluded by default.
// e.g. The range `> 1` doesn't contain `2.0.0-rc1`.

use std::{borrow::Borrow, cell::RefCell, cmp::Ordering, convert::TryFrom, error::Error, fmt};

use crate::{ApiError, Package};

use self::parser::Parser;
use pubgrub::{
    error::PubGrubError,
    solver::{Dependencies, OfflineDependencyProvider},
};

mod lexer;
mod parser;
#[cfg(test)]
mod tests;

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
    major: u32,
    minor: u32,
    patch: u32,
    pre: Vec<Identifier>,
    build: Option<String>,
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
            return Err(parser::Error::MoreInput(parser.tail()?));
        }
        Ok(version)
    }

    /// Parse a Hex compatible version range. i.e. `> 1 and < 2 or == 4.5.2`.
    pub fn parse_range(input: &str) -> Result<Range, parser::Error> {
        let mut parser = Parser::new(input)?;
        let version = parser.range()?;
        if !parser.is_eof() {
            return Err(parser::Error::MoreInput(parser.tail()?));
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
    type Error = parser::Error<'a>;

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Range(pubgrub::range::Range<Version>);

impl Range {
    pub fn strictly_lower_than(version: Version) -> Self {
        Self(pubgrub::range::Range::strictly_lower_than(version))
    }

    pub fn higher_than(version: Version) -> Self {
        Self(pubgrub::range::Range::higher_than(version))
    }

    pub fn exact(version: Version) -> Self {
        Self(pubgrub::range::Range::exact(version))
    }

    pub fn intersection(&self, other: &Self) -> Self {
        Self(self.0.intersection(&other.0))
    }

    pub fn union(&self, other: &Self) -> Self {
        Self(self.0.union(&other.0))
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

// TODO: serde
#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub struct Manifest {
    packages: Vec<ManifestPackage>,
}

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ManifestPackage {
    Hex { name: String, version: Version },
}

pub fn resolve_versions<Requirements>(
    remote: Box<dyn PackageFetcher>,
    root_package: String,
    root_version: Version,
    requirements: Requirements,
) -> Result<Manifest, PubGrubError<String, Version>>
where
    Requirements: Iterator<Item = (String, Range)>,
{
    let mut dependency_provider = DependencyProvider::new(remote);
    dependency_provider.add_dependencies(root_package.clone(), root_version.clone(), requirements);

    let resolved = pubgrub::solver::resolve(
        &dependency_provider,
        root_package.clone(),
        root_version.clone(),
    )?;

    let mut packages: Vec<_> = resolved
        .into_iter()
        .map(|(name, version)| ManifestPackage::Hex { name, version })
        .collect();
    packages.sort();

    Ok(Manifest { packages })
}

pub trait PackageFetcher {
    fn get_dependencies(&self, package: &str) -> Result<Package, ApiError>;
}

struct DependencyProvider {
    cache: RefCell<OfflineDependencyProvider<PackageName, Version>>,
    remote: Box<dyn PackageFetcher>,
}

impl DependencyProvider {
    fn new(remote: Box<dyn PackageFetcher>) -> Self {
        Self {
            cache: RefCell::new(OfflineDependencyProvider::new()),
            remote,
        }
    }

    fn add_dependencies<Dependencies>(
        &mut self,
        package: PackageName,
        version: Version,
        dependencies: Dependencies,
    ) where
        Dependencies: IntoIterator<Item = (PackageName, Range)>,
    {
        self.cache.borrow_mut().add_dependencies(
            package,
            version,
            dependencies.into_iter().map(|(p, r)| (p, r.0)),
        )
    }
}

type PackageName = String;

impl pubgrub::solver::DependencyProvider<PackageName, Version> for DependencyProvider {
    fn choose_package_version<
        Package: Borrow<PackageName>,
        Ver: Borrow<pubgrub::range::Range<Version>>,
    >(
        &self,
        potential_packages: impl Iterator<Item = (Package, Ver)>,
    ) -> Result<(Package, Option<Version>), Box<dyn Error>> {
        self.cache
            .borrow()
            .choose_package_version(potential_packages)
    }

    fn get_dependencies(
        &self,
        name: &PackageName,
        version: &Version,
    ) -> Result<pubgrub::solver::Dependencies<PackageName, Version>, Box<dyn Error>> {
        let mut cache = self.cache.borrow_mut();
        match cache.get_dependencies(name, version) {
            // If we don't have the dep in the cache already we look it up from
            // the remote and then insert that into the cache so it'll be fast
            // next time.
            Ok(Dependencies::Unknown) => match self.remote.get_dependencies(name) {
                Ok(package) => {
                    for release in package.releases.into_iter() {
                        let deps = release
                            .dependencies
                            .into_iter()
                            .map(|dep| (dep.package, dep.requirement.0));
                        cache.add_dependencies(name.clone(), release.version, deps);
                    }
                    cache.get_dependencies(name, version)
                }
                Err(ApiError::NotFound) => Ok(Dependencies::Unknown),
                Err(error) => Err(Box::new(error)),
            },
            dependencies @ Ok(_) => dependencies,
            error @ Err(_) => error,
        }
    }
}
