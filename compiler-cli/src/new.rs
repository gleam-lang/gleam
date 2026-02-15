use camino::{Utf8Path, Utf8PathBuf};
use clap::ValueEnum;
use gleam_core::{
    Result, erlang, error,
    error::{Error, FileIoAction, FileKind, InvalidProjectNameReason},
    parse,
};
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::{env, io::Write};
use strum::{Display, EnumIter, EnumString, IntoEnumIterator, VariantNames};

#[cfg(test)]
mod tests;

use crate::{NewOptions, fs::get_current_directory};

const GLEAM_STDLIB_REQUIREMENT: &str = ">= 0.44.0 and < 2.0.0";
const GLEEUNIT_REQUIREMENT: &str = ">= 1.0.0 and < 2.0.0";
const ERLANG_OTP_VERSION: &str = "28";
const REBAR3_VERSION: &str = "3";
const ELIXIR_VERSION: &str = "1";

#[derive(
    Debug, Serialize, Deserialize, Display, EnumString, VariantNames, ValueEnum, Clone, Copy,
)]
#[strum(serialize_all = "lowercase")]
#[clap(rename_all = "lower")]
pub enum Template {
    #[clap(skip)]
    Lib,
    Erlang,
    JavaScript,
}

#[derive(Debug)]
pub struct Creator {
    root: Utf8PathBuf,
    src: Utf8PathBuf,
    test: Utf8PathBuf,
    github: Utf8PathBuf,
    workflows: Utf8PathBuf,
    gleam_version: &'static str,
    options: NewOptions,
    project_name: String,
}

#[derive(EnumIter, PartialEq, Eq, Debug, Hash)]
enum FileToCreate {
    Readme,
    Gitignore,
    SrcModule,
    TestModule,
    GleamToml,
    GithubCi,
}

impl FileToCreate {
    pub fn location(&self, creator: &Creator) -> Utf8PathBuf {
        let project_name = &creator.project_name;

        match self {
            Self::Readme => creator.root.join(Utf8PathBuf::from("README.md")),
            Self::Gitignore => creator.root.join(Utf8PathBuf::from(".gitignore")),
            Self::SrcModule => creator
                .src
                .join(Utf8PathBuf::from(format!("{project_name}.gleam"))),
            Self::TestModule => creator
                .test
                .join(Utf8PathBuf::from(format!("{project_name}_test.gleam"))),
            Self::GleamToml => creator.root.join(Utf8PathBuf::from("gleam.toml")),
            Self::GithubCi => creator.workflows.join(Utf8PathBuf::from("test.yml")),
        }
    }

    pub fn contents(&self, creator: &Creator) -> Option<String> {
        let project_name = &creator.project_name;
        let skip_git = creator.options.skip_git;
        let skip_github = creator.options.skip_github;
        let gleam_version = creator.gleam_version;
        let target = match creator.options.template {
            Template::JavaScript => "target = \"javascript\"\n",
            Template::Lib | Template::Erlang => "",
        };

        match self {
            Self::Readme => Some(default_readme(project_name)),
            Self::Gitignore if !skip_git => Some(
                "*.beam
*.ez
/build
erl_crash.dump
"
                .into(),
            ),

            Self::SrcModule => Some(format!(
                r#"import gleam/io

pub fn main() -> Nil {{
  io.println("Hello from {project_name}!")
}}
"#,
            )),

            Self::TestModule => Some(
                r#"import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  let name = "Joe"
  let greeting = "Hello, " <> name <> "!"

  assert greeting == "Hello, Joe!"
}
"#
                .into(),
            ),

            Self::GleamToml => Some(format!(
                r#"name = "{project_name}"
version = "1.0.0"
{target}
# Fill out these fields if you intend to generate HTML documentation or publish
# your project to the Hex package manager.
#
# description = ""
# licences = ["Apache-2.0"]
# repository = {{ type = "github", user = "", repo = "" }}
# links = [{{ title = "Website", href = "" }}]
#
# For a full reference of all the available options, you can have a look at
# https://gleam.run/writing-gleam/gleam-toml/.

[dependencies]
gleam_stdlib = "{GLEAM_STDLIB_REQUIREMENT}"

[dev_dependencies]
gleeunit = "{GLEEUNIT_REQUIREMENT}"
"#,
            )),

            Self::GithubCi if !skip_git && !skip_github => Some(format!(
                r#"name: test

on:
  push:
    branches:
      - master
      - main
  pull_request:

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: "{ERLANG_OTP_VERSION}"
          gleam-version: "{gleam_version}"
          rebar3-version: "{REBAR3_VERSION}"
          # elixir-version: "{ELIXIR_VERSION}"
      - run: gleam deps download
      - run: gleam test
      - run: gleam format --check src test
"#,
            )),
            Self::GithubCi | Self::Gitignore => None,
        }
    }
}

pub fn default_readme(project_name: &str) -> String {
    format!(
        r#"# {project_name}

[![Package Version](https://img.shields.io/hexpm/v/{project_name})](https://hex.pm/packages/{project_name})
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/{project_name}/)

```sh
gleam add {project_name}@1
```
```gleam
import {project_name}

pub fn main() -> Nil {{
  // TODO: An example of the project in use
}}
```

Further documentation can be found at <https://hexdocs.pm/{project_name}>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
"#,
    )
}

impl Creator {
    fn new(options: NewOptions, gleam_version: &'static str) -> Result<Self, Error> {
        Self::new_with_confirmation(options, gleam_version, crate::cli::confirm)
    }

    fn new_with_confirmation(
        mut options: NewOptions,
        gleam_version: &'static str,
        confirm: impl Fn(&str) -> Result<bool, Error>,
    ) -> Result<Self, Error> {
        let name =
            get_valid_project_name(options.name.as_deref(), &options.project_root, &confirm)?;
        options.project_root = name.project_root(&options.project_root);

        let root = get_current_directory()?.join(&options.project_root);
        let src = root.join("src");
        let test = root.join("test");
        let github = root.join(".github");
        let workflows = github.join("workflows");
        let me = Self {
            root: root.clone(),
            src,
            test,
            github,
            workflows,
            gleam_version,
            options,
            project_name: name.decided().to_string(),
        };

        validate_root_folder(&me)?;

        Ok(me)
    }

    fn run(&self) -> Result<()> {
        crate::fs::mkdir(&self.root)?;
        crate::fs::mkdir(&self.src)?;
        crate::fs::mkdir(&self.test)?;

        if !self.options.skip_git && !self.options.skip_github {
            crate::fs::mkdir(&self.github)?;
            crate::fs::mkdir(&self.workflows)?;
        }

        if !self.options.skip_git {
            crate::fs::git_init(&self.root)?;
        }

        match self.options.template {
            Template::Lib | Template::Erlang | Template::JavaScript => {
                for file in FileToCreate::iter() {
                    let path = file.location(self);
                    if let Some(contents) = file.contents(self) {
                        write(path, &contents)?;
                    }
                }
            }
        }

        Ok(())
    }
}

pub fn create(options: NewOptions, version: &'static str) -> Result<()> {
    let creator = Creator::new(options.clone(), version)?;

    creator.run()?;

    let cd_folder = if options.project_root == "." {
        "".into()
    } else {
        format!("\tcd {}\n", creator.options.project_root)
    };

    println!(
        "Your Gleam project {} has been successfully created.
The project can be compiled and tested by running these commands:

{}\tgleam test
",
        creator.project_name, cd_folder,
    );
    Ok(())
}

fn write(path: Utf8PathBuf, contents: &str) -> Result<()> {
    let mut f = File::create(&path).map_err(|err| Error::FileIo {
        kind: FileKind::File,
        path: path.clone(),
        action: FileIoAction::Create,
        err: Some(err.to_string()),
    })?;

    f.write_all(contents.as_bytes())
        .map_err(|err| Error::FileIo {
            kind: FileKind::File,
            path,
            action: FileIoAction::WriteTo,
            err: Some(err.to_string()),
        })?;
    Ok(())
}

fn validate_root_folder(creator: &Creator) -> Result<(), Error> {
    let mut duplicate_files: Vec<Utf8PathBuf> = Vec::new();

    for t in FileToCreate::iter() {
        let full_path = t.location(creator);
        let content = t.contents(creator);
        if full_path.exists() && content.is_some() {
            duplicate_files.push(full_path);
        }
    }

    if !duplicate_files.is_empty() {
        return Err(Error::OutputFilesAlreadyExist {
            file_names: duplicate_files,
        });
    }

    Ok(())
}

fn validate_name(name: &str) -> Result<(), Error> {
    if name.starts_with("gleam_") {
        Err(Error::InvalidProjectName {
            name: name.to_string(),
            reason: InvalidProjectNameReason::GleamPrefix,
        })
    } else if erlang::is_erlang_reserved_word(name) {
        Err(Error::InvalidProjectName {
            name: name.to_string(),
            reason: InvalidProjectNameReason::ErlangReservedWord,
        })
    } else if erlang::is_erlang_standard_library_module(name) {
        Err(Error::InvalidProjectName {
            name: name.to_string(),
            reason: InvalidProjectNameReason::ErlangStandardLibraryModule,
        })
    } else if parse::lexer::str_to_keyword(name).is_some() {
        Err(Error::InvalidProjectName {
            name: name.to_string(),
            reason: InvalidProjectNameReason::GleamReservedWord,
        })
    } else if name == "gleam" {
        Err(Error::InvalidProjectName {
            name: name.to_string(),
            reason: InvalidProjectNameReason::GleamReservedModule,
        })
    } else if regex::Regex::new("^[a-z][a-z0-9_]*$")
        .expect("failed regex to match valid name format")
        .is_match(name)
    {
        Ok(())
    } else if regex::Regex::new("^[a-zA-Z][a-zA-Z0-9_]*$")
        .expect("failed regex to match valid but non-lowercase name format")
        .is_match(name)
    {
        Err(Error::InvalidProjectName {
            name: name.to_string(),
            reason: InvalidProjectNameReason::FormatNotLowercase,
        })
    } else {
        Err(Error::InvalidProjectName {
            name: name.to_string(),
            reason: InvalidProjectNameReason::Format,
        })
    }
}

fn suggest_valid_name(invalid_name: &str, reason: &InvalidProjectNameReason) -> Option<String> {
    match reason {
        InvalidProjectNameReason::GleamPrefix => match invalid_name.strip_prefix("gleam_") {
            Some(stripped) if invalid_name != "gleam_" => {
                let suggestion = stripped.to_string();
                match validate_name(&suggestion) {
                    Ok(_) => Some(suggestion),
                    Err(_) => None,
                }
            }
            _ => None,
        },
        InvalidProjectNameReason::ErlangReservedWord => Some(format!("{invalid_name}_app")),
        InvalidProjectNameReason::ErlangStandardLibraryModule => {
            Some(format!("{invalid_name}_app"))
        }
        InvalidProjectNameReason::GleamReservedWord => Some(format!("{invalid_name}_app")),
        InvalidProjectNameReason::GleamReservedModule => {
            if invalid_name == "gleam" {
                Some("app_gleam".into())
            } else {
                Some(format!("{invalid_name}_app"))
            }
        }
        InvalidProjectNameReason::FormatNotLowercase => Some(invalid_name.to_lowercase()),
        InvalidProjectNameReason::Format => {
            let suggestion = regex::Regex::new(r"[^a-z0-9]")
                .expect("failed regex to match any non-lowercase and non-alphanumeric characters")
                .replace_all(&invalid_name.to_lowercase(), "_")
                .to_string();

            let suggestion = regex::Regex::new(r"_+")
                .expect("failed regex to match consecutive underscores")
                .replace_all(&suggestion, "_")
                .to_string();

            match validate_name(&suggestion) {
                Ok(_) => Some(suggestion),
                Err(_) => None,
            }
        }
    }
}

fn get_valid_project_name(
    provided_name: Option<&str>,
    project_root: &str,
    confirm: impl Fn(&str) -> Result<bool, Error>,
) -> Result<ProjectName, Error> {
    let initial_name = match provided_name {
        Some(name) => name.trim().to_string(),
        None => get_foldername(project_root)?.trim().to_string(),
    };

    let invalid_reason = match validate_name(&initial_name) {
        Ok(_) => {
            return Ok(match provided_name {
                Some(_) => ProjectName::Provided {
                    decided: initial_name,
                },
                None => ProjectName::Derived {
                    folder: initial_name.clone(),
                    decided: initial_name,
                },
            });
        }
        Err(Error::InvalidProjectName { reason, .. }) => reason,
        Err(error) => return Err(error),
    };

    let suggested_name = match suggest_valid_name(&initial_name, &invalid_reason) {
        Some(suggested_name) => suggested_name,
        None => {
            return Err(Error::InvalidProjectName {
                name: initial_name,
                reason: invalid_reason,
            });
        }
    };

    let prompt_for_suggested_name = error::format_invalid_project_name_error(
        &initial_name,
        &invalid_reason,
        &Some(suggested_name.clone()),
    );

    if confirm(&prompt_for_suggested_name)? {
        return Ok(match provided_name {
            Some(_) => ProjectName::Provided {
                decided: suggested_name,
            },
            None => ProjectName::Derived {
                folder: initial_name,
                decided: suggested_name,
            },
        });
    }

    Err(Error::InvalidProjectName {
        name: initial_name,
        reason: invalid_reason,
    })
}

fn get_foldername(path: &str) -> Result<String, Error> {
    match path {
        "." => env::current_dir()
            .expect("invalid folder")
            .file_name()
            .and_then(|x| x.to_str())
            .map(ToString::to_string)
            .ok_or(Error::UnableToFindProjectRoot {
                path: path.to_string(),
            }),
        _ => Utf8Path::new(path)
            .file_name()
            .map(ToString::to_string)
            .ok_or(Error::UnableToFindProjectRoot {
                path: path.to_string(),
            }),
    }
}

#[derive(Debug, Clone)]
enum ProjectName {
    Provided { decided: String },
    Derived { folder: String, decided: String },
}

impl ProjectName {
    fn decided(&self) -> &str {
        match self {
            Self::Provided { decided } | Self::Derived { decided, .. } => decided,
        }
    }

    fn project_root(&self, current_root: &str) -> String {
        match self {
            Self::Provided { .. } => current_root.to_string(),
            Self::Derived { folder, decided } => {
                if current_root == "." || folder == decided {
                    return current_root.to_string();
                }

                // If the name was invalid and generated suggestion was accepted,
                // align the directory path with the new name.
                let original_root = Utf8Path::new(current_root);
                let new_root = match original_root.parent() {
                    Some(parent) if !parent.as_str().is_empty() => parent.join(decided),
                    Some(_) | None => Utf8PathBuf::from(decided),
                };
                new_root.to_string()
            }
        }
    }
}
