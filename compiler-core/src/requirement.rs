use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;

use crate::config::PackageConfig;
use crate::error::{Error, FileIoAction, FileKind, Result};
use hexpm::version::Range;
use serde::de::{self, Deserializer, MapAccess, Visitor};
use serde::ser::{Serialize, SerializeMap, Serializer};
use serde::Deserialize;
use smol_str::SmolStr;

#[derive(Deserialize, Debug, PartialEq, Eq, Clone)]
#[serde(untagged, remote = "Self")]
pub enum Requirement {
    Hex { version: Range },
    Path { path: PathBuf },
    Git { git: SmolStr },
}

impl Requirement {
    pub fn version_range(&self) -> Result<Range, Error> {
        match self {
            Requirement::Hex { version: range } => Ok(range.clone()),
            Requirement::Path { path } => {
                let config_path = path.join("gleam.toml");
                let toml =
                    std::fs::read_to_string(&config_path).map_err(|error| Error::FileIo {
                        kind: FileKind::File,
                        action: FileIoAction::Read,
                        path: config_path,
                        err: Some(error.to_string()),
                    })?;
                let config: PackageConfig = toml::from_str(&toml).map_err(|_| {
                    Error::DependencyResolutionFailed(
                        "Local dependency config could not be parsed".into(),
                    )
                })?;
                Ok(Range::new(format!("== {}", config.version)))
            }
            Requirement::Git { .. } => Err(Error::GitDependencyUnsuported),
        }
    }

    pub fn hex(range: &str) -> Requirement {
        Requirement::Hex {
            version: Range::new(range.to_string()),
        }
    }

    pub fn path(path: &str) -> Requirement {
        Requirement::Path { path: path.into() }
    }

    pub fn git(url: &str) -> Requirement {
        Requirement::Git { git: url.into() }
    }
}

// Serialization

impl ToString for Requirement {
    fn to_string(&self) -> String {
        match self {
            Requirement::Hex { version: range } => {
                format!(r#"{{ version = "{}" }}"#, range)
            }
            Requirement::Path { path } => format!(r#"{{ path = "{}" }}"#, path.display()),
            Requirement::Git { git: url } => format!(r#"{{ git = "{}" }}"#, url),
        }
    }
}

impl Serialize for Requirement {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        match self {
            Requirement::Hex { version: range } => map.serialize_entry("version", range)?,
            Requirement::Path { path } => map.serialize_entry("path", path)?,
            Requirement::Git { git: url } => map.serialize_entry("git", url)?,
        }
        map.end()
    }
}

// Deserialization

#[derive(Debug, Copy, Clone)]
pub struct Void;

impl FromStr for Requirement {
    type Err = Void;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Requirement::hex(s))
    }
}

struct RequirementVisitor;

impl<'de> Visitor<'de> for RequirementVisitor {
    type Value = Requirement;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("string or map")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(FromStr::from_str(value).expect("expected string"))
    }

    fn visit_map<M>(self, visitor: M) -> Result<Self::Value, M::Error>
    where
        M: MapAccess<'de>,
    {
        Requirement::deserialize(de::value::MapAccessDeserializer::new(visitor))
    }
}

impl<'de> Deserialize<'de> for Requirement {
    fn deserialize<D>(deserializer: D) -> Result<Requirement, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(RequirementVisitor)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::collections::HashMap;

    #[test]
    fn read_recipe() {
        let toml = r#"
            short = "~> 0.5"
            hex = { version = "~> 1.0.0" }
            local = { path = "/path/to/package" }
            github = { git = "https://github.com/gleam-lang/otp.git" }
        "#;
        let deps: HashMap<String, Requirement> = toml::from_str(toml).unwrap();
        assert_eq!(deps["short"], Requirement::hex("~> 0.5"));
        assert_eq!(deps["hex"], Requirement::hex("~> 1.0.0"));
        assert_eq!(deps["local"], Requirement::path("/path/to/package"));
        assert_eq!(
            deps["github"],
            Requirement::git("https://github.com/gleam-lang/otp.git")
        );
    }
}
