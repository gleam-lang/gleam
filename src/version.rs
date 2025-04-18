//! Functions for parsing and matching versions against requirements, based off
//! and compatible with the Elixir Version module, which is used by Hex
//! internally as well as be the Elixir build tool Hex client.

use std::{cmp::Ordering, convert::TryFrom, fmt};

use self::parser::Parser;
use serde::{
    Deserialize, Serialize,
    de::{self, Deserializer},
};

pub use pubgrub::report as pubgrub_report;

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
        if self.is_pre() {
            let mut pre = self.pre.clone();
            let last_component = pre
                .last_mut()
                // This `.expect` is safe, as we know there to be at least
                // one pre-release component.
                .expect("no pre-release components");

            match last_component {
                Identifier::Numeric(pre) => *pre += 1,
                Identifier::AlphaNumeric(pre) => {
                    let mut segments = split_alphanumeric(pre);
                    let last_segment = segments.last_mut().unwrap();

                    match last_segment {
                        AlphaOrNumeric::Numeric(n) => *n += 1,
                        AlphaOrNumeric::Alpha(alpha) => {
                            // We should potentially be smarter about this (for instance, pick the next letter in the
                            // alphabetic sequence), however, this seems like it could be quite a bit more complex.
                            alpha.push('1')
                        }
                    }

                    *pre = segments
                        .into_iter()
                        .map(|segment| match segment {
                            AlphaOrNumeric::Alpha(segment) => segment,
                            AlphaOrNumeric::Numeric(segment) => segment.to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join("");
                }
            }

            Self {
                major: self.major,
                minor: self.minor,
                patch: self.patch,
                pre,
                build: None,
            }
        } else {
            self.bump_patch()
        }
    }
}

enum AlphaOrNumeric {
    Alpha(String),
    Numeric(u32),
}

/// Splits the given string into alphabetic and numeric segments.
fn split_alphanumeric(str: &str) -> Vec<AlphaOrNumeric> {
    let mut segments = Vec::new();
    let mut current_segment = String::new();
    let mut previous_char_was_numeric = None;

    for char in str.chars() {
        let is_numeric = char.is_ascii_digit();
        match previous_char_was_numeric {
            Some(previous_char_was_numeric) if previous_char_was_numeric == is_numeric => {
                current_segment.push(char)
            }
            _ => {
                if !current_segment.is_empty() {
                    if current_segment.chars().any(|char| char.is_ascii_digit()) {
                        segments.push(AlphaOrNumeric::Numeric(current_segment.parse().unwrap()));
                    } else {
                        segments.push(AlphaOrNumeric::Alpha(current_segment));
                    }

                    current_segment = String::new();
                }

                current_segment.push(char);
                previous_char_was_numeric = Some(is_numeric);
            }
        }
    }

    if !current_segment.is_empty() {
        if current_segment.chars().any(|char| char.is_ascii_digit()) {
            segments.push(AlphaOrNumeric::Numeric(current_segment.parse().unwrap()));
        } else {
            segments.push(AlphaOrNumeric::Alpha(current_segment));
        }
    }

    segments
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
pub struct Range {
    spec: String,
    range: pubgrub::range::Range<Version>,
}

impl Range {
    pub fn new(spec: String) -> Result<Self, parser::Error> {
        let range = Version::parse_range(spec.as_str())?;
        Ok(Self { spec, range })
    }
}

impl Range {
    pub fn to_pubgrub(&self) -> &pubgrub::range::Range<Version> {
        &self.range
    }

    pub fn as_str(&self) -> &str {
        &self.spec
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
