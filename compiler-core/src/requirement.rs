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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Requirement {
    Hex { version: Range },
    Path { path: Utf8PathBuf },
    Git { git: EcoString, ref_: EcoString },
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

    fn visit_str<E: de::Error>(self, value: &str) -> Result<Self::Value, E> {
        match value.parse::<Requirement>() {
            Ok(value) => Ok(value),
            Err(error) => Err(de::Error::custom(error)),
        }
    }

    fn visit_map<M: MapAccess<'de>>(self, mut visitor: M) -> Result<Self::Value, M::Error> {
        let mut git: Option<EcoString> = None;
        let mut ref_: Option<EcoString> = None;
        let mut version: Option<EcoString> = None;
        let mut path: Option<Utf8PathBuf> = None;

        while let Some(field) = visitor.next_key::<DependencyField>()? {
            match field {
                DependencyField::Git => replace_field("git", &mut git, &mut visitor)?,
                DependencyField::Ref => replace_field("ref", &mut ref_, &mut visitor)?,
                DependencyField::Path => replace_field("path", &mut path, &mut visitor)?,
                DependencyField::Version => replace_field("version", &mut version, &mut visitor)?,
            }
        }

        match (git, ref_, version, path) {
            (Some(git), Some(ref_), None, None) => Ok(Requirement::Git { git, ref_ }),
            (None, Some(_ref), None, None) => Err(de::Error::missing_field("git")),
            (Some(_git), None, None, None) => Err(de::Error::missing_field("ref")),

            (None, None, Some(version), None) => {
                Requirement::hex(&version).map_err(de::Error::custom)
            }

            (None, None, None, Some(path)) => Ok(Requirement::Path { path }),

            _ => Err(de::Error::custom(
                "expecting exactly one of `version`, `path`, or both `git` and `ref`",
            )),
        }
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

fn replace_field<'de, M: MapAccess<'de>, A: Deserialize<'de>>(
    name: &'static str,
    existing: &mut Option<A>,
    visitor: &mut M,
) -> Result<(), M::Error> {
    if existing.replace(visitor.next_value()?).is_some() {
        Err(de::Error::duplicate_field(name))
    } else {
        Ok(())
    }
}

/// The possible fields that can appear in a dependency map: `git`, `ref` (for
/// git dependencies), `path` (for path dependencies) and `version` (for Hex
/// dependencies).
///
#[derive(Deserialize)]
#[serde(field_identifier, rename_all = "lowercase")]
enum DependencyField {
    Version,
    Path,
    Git,
    Ref,
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
        let toml = r#"short = ">= 2.0 and < 3.0.0""#;

        let error =
            toml::from_str::<HashMap<String, Requirement>>(toml).expect_err("invalid version");
        insta::assert_snapshot!(error.to_string());
    }

    #[test]
    fn read_wrong_dependency() {
        let toml = r#"github = { a = "123", wibble = "123" }"#;

        let error = toml::from_str::<HashMap<String, Requirement>>(toml).unwrap_err();
        insta::assert_snapshot!(error.to_string());
    }

    #[test]
    fn read_empty_dependency() {
        let toml = "github = {}";
        let error = toml::from_str::<HashMap<String, Requirement>>(toml).unwrap_err();
        insta::assert_snapshot!(error.to_string());
    }

    #[test]
    fn read_dependency_with_duplicate_field() {
        let toml = r#"github = { git = "a", git = "b" }"#;
        let error = toml::from_str::<HashMap<String, Requirement>>(toml).unwrap_err();
        insta::assert_snapshot!(error.to_string());
    }

    #[test]
    fn read_git_dependency_with_unknown_field() {
        let toml = r#"github = { git = "a", ref = "b", mmm = 1 }"#;
        let error = toml::from_str::<HashMap<String, Requirement>>(toml).unwrap_err();
        insta::assert_snapshot!(error.to_string());
    }

    #[test]
    fn read_dependency_with_good_first_field_and_wrong_second_field() {
        let toml = r#"github = { path = "a", stray = "b" }"#;
        let error = toml::from_str::<HashMap<String, Requirement>>(toml).unwrap_err();
        insta::assert_snapshot!(error.to_string());
    }

    #[test]
    fn read_dependency_with_ref_and_invalid_field() {
        let toml = r#"github = { ref = "a", sgit = "b" }"#;
        let error = toml::from_str::<HashMap<String, Requirement>>(toml).unwrap_err();
        insta::assert_snapshot!(error.to_string());
    }

    #[test]
    fn read_dependency_with_git_and_invalid_field() {
        let toml = r#"github = { git = "a", refs = "b" }"#;
        let error = toml::from_str::<HashMap<String, Requirement>>(toml).unwrap_err();
        insta::assert_snapshot!(error.to_string());
    }

    #[test]
    fn read_dependency_with_git_and_no_ref_field() {
        let toml = r#"github = { git = "a" }"#;
        let error = toml::from_str::<HashMap<String, Requirement>>(toml).unwrap_err();
        insta::assert_snapshot!(error.to_string());
    }

    #[test]
    fn read_dependency_with_ref_and_no_git_field() {
        let toml = r#"github = { ref = "a" }"#;
        let error = toml::from_str::<HashMap<String, Requirement>>(toml).unwrap_err();
        insta::assert_snapshot!(error.to_string());
    }
}
