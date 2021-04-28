//! Functions for parsing and matching versions against requirements, based off
//! and compatible with the Elixir Version module, which is used by Hex
//! internally as well as be the Elixir build tool Hex client.

use std::{convert::TryFrom, fmt};

use self::parser::Parser;

mod lexer;
mod parser;
mod requirement;
#[cfg(test)]
mod tests;

pub use requirement::*;

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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Version {
    major: u64,
    minor: u64,
    patch: u64,
    pre: Vec<Identifier>,
    build: Option<String>,
}

impl Version {
    fn bump_minor(&self) -> Self {
        Self {
            major: self.major,
            minor: self.minor + 1,
            patch: 0,
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

    pub fn parse(input: &str) -> Result<Self, parser::Error> {
        let mut parser = Parser::new(input)?;
        let version = parser.version()?;
        if !parser.is_eof() {
            return Err(parser::Error::MoreInput(parser.tail()?));
        }
        Ok(version)
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
        let format_pre = |pre: &[Identifier]| {
            pre.iter()
                .map(Identifier::to_string)
                .collect::<Vec<_>>()
                .join(".")
        };
        match (self.pre.as_slice(), self.build.as_ref()) {
            (&[], None) => write!(f, "{}.{}.{}", self.major, self.minor, self.patch),

            (&[], Some(build)) => {
                write!(f, "{}.{}.{}+{}", self.major, self.minor, self.patch, build)
            }
            (pre, None) => {
                let pre = format_pre(pre);
                write!(f, "{}.{}.{}-{}", self.major, self.minor, self.patch, pre,)
            }
            (pre, Some(build)) => {
                let pre = format_pre(pre);
                write!(
                    f,
                    "{}.{}.{}-{}+{}",
                    self.major, self.minor, self.patch, pre, build
                )
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Identifier {
    Numeric(u64),
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
