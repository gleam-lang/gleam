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

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match value.parse::<Requirement>() {
            Ok(value) => Ok(value),
            Err(error) => Err(de::Error::custom(error)),
        }
    }

    fn visit_map<M>(self, mut visitor: M) -> Result<Self::Value, M::Error>
    where
        M: MapAccess<'de>,
    {
        let Some((key, value)) = visitor.next_entry()? else {
            return Err(de::Error::custom(
                "no field found, expecting one of `version`, `path`, `git`, `ref` ",
            ));
        };

        // You might wonder why are we deconding the string key into a custom
        // type instead of matching on a string directly? Because of serde's
        // and toml's magic if we were to do that the error would end up
        // highlighting the entire toml dictionary instead of just the field.
        // We'd get this:
        //
        // ```toml
        // { a = "" }
        // ^^^^^^^^^^ unexpected field "a", expected "git", ...
        // ```
        //
        // While we want the more pleasant:
        //
        // ```toml
        // { a = "" }
        //   ^ unexpected field "a", expected "git", ...
        // ```
        //
        match key {
            // If we've found the `git` field, then we're just missing the `ref`
            // one; similarly, if we find the `ref` field, we know we're missing
            // the `git` one.
            DependencyField::Git => {
                let Some((DependencyField::Ref, ref_)) = visitor.next_entry()? else {
                    return Err(de::Error::missing_field("ref"));
                };
                let _ = visitor.next_entry::<DenyFurtherFields, ()>()?;
                Ok(Requirement::Git {
                    git: String::into(value),
                    ref_: String::into(ref_),
                })
            }

            DependencyField::Ref => {
                let Some((DependencyField::Git, git)) = visitor.next_entry()? else {
                    return Err(de::Error::missing_field("git"));
                };
                let _ = visitor.next_entry::<DenyFurtherFields, ()>()?;
                Ok(Requirement::Git {
                    git: String::into(git),
                    ref_: String::into(value),
                })
            }

            // The version and path fields are alone so there's nothing else we
            // need to do here except make sure there's no other fields.
            DependencyField::Version => {
                let _ = visitor.next_entry::<DenyFurtherFields, ()>()?;
                Requirement::hex(&value).map_err(de::Error::custom)
            }

            DependencyField::Path => {
                let _ = visitor.next_entry::<DenyFurtherFields, ()>()?;
                Ok(Requirement::path(&value))
            }
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

/// The possible fields that can appear in a dependency map: `git`, `ref` (for
/// git dependencies), `path` (for path dependencies) and `version` (for Hex
/// dependencies).
///
enum DependencyField {
    Git,
    Ref,
    Path,
    Version,
}

struct DependencyFieldVisitor;

impl<'de> Visitor<'de> for DependencyFieldVisitor {
    type Value = DependencyField;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("str")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match value {
            "git" => Ok(DependencyField::Git),
            "path" => Ok(DependencyField::Path),
            "ref" => Ok(DependencyField::Ref),
            "version" => Ok(DependencyField::Version),
            _ => Err(de::Error::unknown_field(
                value,
                &["version", "path", "git", "ref"],
            )),
        }
    }
}

impl<'de> Deserialize<'de> for DependencyField {
    fn deserialize<D>(deserializer: D) -> Result<DependencyField, D::Error>
    where
        D: Deserializer<'de>,
    {
        // You might wonder why we're going through the hassle of defining a
        // visitor to deserialise a `str` instead of just doing something like
        //
        // ```rs
        // let value: &str = Deserialize::deserialize(deserializer)?;
        // match value { ... }
        // ```
        //
        // The toml library returns an error in that case! Sadly that forces us
        // to define a `Visitor` and use that...
        deserializer.deserialize_str(DependencyFieldVisitor)
    }
}

/// A special dummy field that's used to reject all leftover fields with a
/// custom error message.
///
struct DenyFurtherFields;

struct DenyFurtherFieldsVisitor;

impl<'de> Deserialize<'de> for DenyFurtherFields {
    fn deserialize<D>(deserializer: D) -> Result<DenyFurtherFields, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(DenyFurtherFieldsVisitor)
    }
}

impl<'de> Visitor<'de> for DenyFurtherFieldsVisitor {
    type Value = DenyFurtherFields;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("nothing")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Err(de::Error::custom(format!("unknown field `{value}`")))
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
