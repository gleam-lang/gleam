use std::fmt;
use std::str::FromStr;

use crate::error::Result;
use crate::io::make_relative;
use camino::{Utf8Path, Utf8PathBuf};
use ecow::EcoString;
use hexpm::version::Range;
use serde::de::{self, Deserializer, MapAccess, Visitor};
use serde::ser::{Serialize, SerializeMap, Serializer};
use serde::Deserialize;

#[derive(Deserialize, Debug, PartialEq, Eq, Clone)]
#[serde(untagged, remote = "Self")]
pub enum Requirement {
    Hex { version: Range },
    Path { path: Utf8PathBuf },
    Git { git: EcoString },
}

impl Requirement {
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

    pub fn to_toml(&self, root_path: &Utf8Path) -> String {
        match self {
            Requirement::Hex { version: range } => {
                format!(r#"{{ version = "{range}" }}"#)
            }
            Requirement::Path { path } => {
                format!(
                    r#"{{ path = "{}" }}"#,
                    make_relative(root_path, path).as_str().replace('\\', "/")
                )
            }
            Requirement::Git { git: url } => format!(r#"{{ git = "{url}" }}"#),
        }
    }
}

// Serialization

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
    fn read_requirement() {
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
