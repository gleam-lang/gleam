use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;

use crate::config::PackageConfig;
use crate::{Error, Result};
use hexpm::version::Range;
use serde::de::{self, Deserializer, MapAccess, Visitor};
use serde::ser::{Serialize, SerializeMap, Serializer};
use serde::Deserialize;

#[derive(Deserialize, Debug, PartialEq, Eq, Clone)]
#[serde(untagged, remote = "Self")]
pub enum Recipe {
    Hex { version: Range },
    Path { path: PathBuf },
    Git { git: String },
}

impl Recipe {
    pub fn version_range(&self) -> Result<Range, Error> {
        match self {
            Recipe::Hex { version: range } => Ok(range.clone()),
            Recipe::Path { path } => {
                let config_path = path.join("gleam.toml");
                let toml = std::fs::read_to_string(&config_path).map_err(|_| {
                    Error::DependencyResolutionFailed("Local dependency could not be found".into())
                })?;
                let config: PackageConfig = toml::from_str(&toml).map_err(|_| {
                    Error::DependencyResolutionFailed(
                        "Local dependency config could not be parsed".into(),
                    )
                })?;
                Ok(Range::new(format!("== {}", config.version.to_string())))
            }
            Recipe::Git { .. } => Err(Error::DependencyResolutionFailed(
                "Git dependencies are currently unsuported".to_string(),
            )),
        }
    }

    pub fn hex(range: &str) -> Recipe {
        Recipe::Hex {
            version: Range::new(range.to_string()),
        }
    }

    pub fn path(path: &str) -> Recipe {
        Recipe::Path { path: path.into() }
    }

    pub fn git(url: &str) -> Recipe {
        Recipe::Git {
            git: url.to_string(),
        }
    }
}

// Serialization

impl ToString for Recipe {
    fn to_string(&self) -> String {
        match self {
            Recipe::Hex { version: range } => {
                format!(r#"{{ version = "{}" }}"#, range.to_string())
            }
            Recipe::Path { path } => format!(r#"{{ path = "{}" }}"#, path.display()),
            Recipe::Git { git: url } => format!(r#"{{ git = "{}" }}"#, url),
        }
    }
}

impl Serialize for Recipe {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        match self {
            Recipe::Hex { version: range } => map.serialize_entry("version", range)?,
            Recipe::Path { path } => map.serialize_entry("path", path)?,
            Recipe::Git { git: url } => map.serialize_entry("git", url)?,
        }
        map.end()
    }
}

// Deserialization

#[derive(Debug, Copy, Clone)]
pub struct Void;

impl FromStr for Recipe {
    type Err = Void;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Recipe::hex(s))
    }
}

struct RecipeVisitor;

impl<'de> Visitor<'de> for RecipeVisitor {
    type Value = Recipe;

    fn expecting<'a>(&self, formatter: &mut fmt::Formatter<'a>) -> fmt::Result {
        formatter.write_str("string or map")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(FromStr::from_str(value).unwrap())
    }

    fn visit_map<M>(self, visitor: M) -> Result<Self::Value, M::Error>
    where
        M: MapAccess<'de>,
    {
        Recipe::deserialize(de::value::MapAccessDeserializer::new(visitor))
    }
}

impl<'de> Deserialize<'de> for Recipe {
    fn deserialize<D>(deserializer: D) -> Result<Recipe, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(RecipeVisitor)
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
        let deps: HashMap<String, Recipe> = toml::from_str(toml).unwrap();
        assert_eq!(deps["short"], Recipe::hex("~> 0.5"));
        assert_eq!(deps["hex"], Recipe::hex("~> 1.0.0"));
        assert_eq!(deps["local"], Recipe::path("/path/to/package"));
        assert_eq!(
            deps["github"],
            Recipe::git("https://github.com/gleam-lang/otp.git")
        );
    }
}
