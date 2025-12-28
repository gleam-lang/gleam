use std::fmt;
use std::str::FromStr;

use crate::Error;
use crate::error::Result;
use crate::io::make_relative;
use camino::{Utf8Path, Utf8PathBuf};
use ecow::EcoString;
use hexpm::version::Range;
use serde::Deserialize;
use serde::de::{self, Deserializer, MapAccess, Visitor};
use serde::ser::{Serialize, SerializeMap, Serializer};

#[derive(Deserialize, Debug, PartialEq, Eq, Clone)]
#[serde(untagged, remote = "Self", deny_unknown_fields)]
pub enum Requirement {
    Hex {
        #[serde(deserialize_with = "deserialise_range")]
        version: Range,
    },

    Path {
        path: Utf8PathBuf,
    },

    Git {
        git: EcoString,
        #[serde(rename = "ref")]
        ref_: EcoString,
    },
}

impl Requirement {
    pub fn hex(range: &str) -> Result<Requirement> {
        Ok(Requirement::Hex {
            version: Range::new(range.to_string()).map_err(|e| Error::InvalidVersionFormat {
                input: range.to_string(),
                error: e.to_string(),
            })?,
        })
    }

    pub fn path(path: &str) -> Requirement {
        Requirement::Path { path: path.into() }
    }

    pub fn git(url: &str, ref_: &str) -> Requirement {
        Requirement::Git {
            git: url.into(),
            ref_: ref_.into(),
        }
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
            Requirement::Git { git: url, ref_ } => {
                format!(r#"{{ git = "{url}", ref = "{ref_}" }}"#)
            }
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
            Requirement::Git { git: url, ref_ } => {
                map.serialize_entry("git", url)?;
                map.serialize_entry("ref", ref_)?;
            }
        }
        map.end()
    }
}

// Deserialization

fn deserialise_range<'de, D>(deserializer: D) -> Result<Range, D::Error>
where
    D: Deserializer<'de>,
{
    let version = String::deserialize(deserializer)?;
    Range::new(version).map_err(de::Error::custom)
}

#[derive(Debug, Copy, Clone)]
pub struct Void;

impl FromStr for Requirement {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Requirement::hex(s)
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
        match value.parse::<Requirement>() {
            Ok(value) => Ok(value),
            Err(error) => Err(de::Error::custom(error)),
        }
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
            github = { git = "https://github.com/gleam-lang/otp.git", ref = "4d34935" }
        "#;
        let deps: HashMap<String, Requirement> = toml::from_str(toml).unwrap();
        assert_eq!(deps["short"], Requirement::hex("~> 0.5").unwrap());
        assert_eq!(deps["hex"], Requirement::hex("~> 1.0.0").unwrap());
        assert_eq!(deps["local"], Requirement::path("/path/to/package"));
        assert_eq!(
            deps["github"],
            Requirement::git("https://github.com/gleam-lang/otp.git", "4d34935")
        );
    }

    #[test]
    fn read_wrong_version() {
        let toml = r#"
            short = ">= 2.0 and < 3.0.0"
        "#;

        let error =
            toml::from_str::<HashMap<String, Requirement>>(toml).expect_err("invalid version");
        insta::assert_snapshot!(error.to_string());
    }
}
