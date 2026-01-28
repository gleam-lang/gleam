//! Functions for parsing and matching versions against requirements, based off
//! and compatible with the Elixir Version module, which is used by Hex
//! internally as well as be the Elixir build tool Hex client.

use std::{cmp::Ordering, convert::TryFrom, fmt};

use self::parser::Parser;
use serde::{
    Deserialize, Serialize,
    de::{self, Deserializer, Visitor},
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
    fn parse_range(input: &str) -> Result<pubgrub::Range<Version>, parser::Error> {
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

    pub fn lowest() -> Self {
        Self::new(0, 0, 0)
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

pub trait LowestVersion {
    fn lowest_version(&self) -> Option<Version>;
}
impl LowestVersion for pubgrub::Range<Version> {
    fn lowest_version(&self) -> Option<Version> {
        self.iter()
            .flat_map(|(lower, _higher)| match lower {
                std::ops::Bound::Included(v) => Some(v.clone()),
                std::ops::Bound::Excluded(_) => None,
                std::ops::Bound::Unbounded => Some(Version::lowest()),
            })
            .min()
    }
}

impl<'de> Deserialize<'de> for Version {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(VersionVisitor)
    }
}

struct VersionVisitor;

impl<'de> Visitor<'de> for VersionVisitor {
    type Value = Version;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a Hex version string")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Version::try_from(value).map_err(de::Error::custom)
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
            write!(f, "+{build}")?;
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
            Identifier::Numeric(n) => Identifier::AlphaNumeric(format!("{n}{add_str}")),
            Identifier::AlphaNumeric(mut s) => {
                s.push_str(add_str);
                Identifier::AlphaNumeric(s)
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Range {
    spec: String,
    range: pubgrub::Range<Version>,
}

impl Range {
    pub fn new(spec: String) -> Result<Self, parser::Error> {
        let range = Version::parse_range(&spec)?;
        Ok(Self { spec, range })
    }
}

impl Range {
    pub fn to_pubgrub(&self) -> &pubgrub::Range<Version> {
        &self.range
    }

    pub fn as_str(&self) -> &str {
        &self.spec
    }
}

impl From<pubgrub::Range<Version>> for Range {
    fn from(range: pubgrub::Range<Version>) -> Self {
        let spec = range.to_string();
        Self { spec, range }
    }
}

impl From<Version> for Range {
    fn from(version: Version) -> Self {
        pubgrub::Range::singleton(version).into()
    }
}

impl From<Range> for pubgrub::Range<Version> {
    fn from(range: Range) -> Self {
        range.range
    }
}

impl fmt::Debug for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Range").field(&self.spec).finish()
    }
}

impl<'de> Deserialize<'de> for Range {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: &str = Deserialize::deserialize(deserializer)?;
        Range::new(s.to_string()).map_err(serde::de::Error::custom)
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
        f.write_str(&self.spec)
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
