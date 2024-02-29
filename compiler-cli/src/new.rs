use camino::{Utf8Path, Utf8PathBuf};
use gleam_core::{
    erlang,
    error::{Error, FileIoAction, FileKind, InvalidProjectNameReason},
    parse, Result,
};
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::{env, io::Write};
use strum::{Display, EnumIter, EnumString, EnumVariantNames, IntoEnumIterator};

#[cfg(test)]
mod tests;

use crate::NewOptions;

const GLEAM_STDLIB_REQUIREMENT: &str = "~> 0.34 or ~> 1.0";
const GLEEUNIT_VERSION: &str = "1.0";
const ERLANG_OTP_VERSION: &str = "26.0.2";
const REBAR3_VERSION: &str = "3";
const ELIXIR_VERSION: &str = "1.15.4";

#[derive(Debug, Serialize, Deserialize, Display, EnumString, EnumVariantNames, Clone, Copy)]
#[strum(serialize_all = "kebab_case")]
pub enum Template {
    Lib,
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

        match self {
            Self::Readme => Some(format!(
                r#"# {project_name}

[![Package Version](https://img.shields.io/hexpm/v/{project_name})](https://hex.pm/packages/{project_name})
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/{project_name}/)

```sh
gleam add {project_name}
```
```gleam
import {project_name}

pub fn main() {{
  // TODO: An example of the project in use
}}
```

Further documentation can be found at <https://hexdocs.pm/{project_name}>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```
"#,
            )),

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

pub fn main() {{
  io.println("Hello from {project_name}!")
}}
"#,
            )),

            Self::TestModule => Some(
                r#"import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}
"#
                .into(),
            ),

            Self::GleamToml => Some(format!(
                r#"name = "{project_name}"
version = "1.0.0"

# Fill out these fields if you intend to generate HTML documentation or publish
# your project to the Hex package manager.
#
# description = ""
# licences = ["Apache-2.0"]
# repository = {{ type = "github", user = "username", repo = "project" }}
# links = [{{ title = "Website", href = "https://gleam.run" }}]
#
# For a full reference of all the available options, you can have a look at
# https://gleam.run/writing-gleam/gleam-toml/. 

[dependencies]
gleam_stdlib = "{GLEAM_STDLIB_REQUIREMENT}"

[dev-dependencies]
gleeunit = "~> {GLEEUNIT_VERSION}"
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

impl Creator {
    fn new(options: NewOptions, gleam_version: &'static str) -> Result<Self, Error> {
        let project_name = if let Some(name) = options.name.clone() {
            name
        } else {
            get_foldername(&options.project_root)?
        }
        .trim()
        .to_string();

        validate_name(&project_name)?;

        let root = Utf8PathBuf::from(&options.project_root);
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
            project_name,
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
            Template::Lib => {
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
        if full_path.exists() {
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
    } else if !regex::Regex::new("^[a-z][a-z0-9_]*$")
        .expect("new name regex could not be compiled")
        .is_match(name)
    {
        Err(Error::InvalidProjectName {
            name: name.to_string(),
            reason: InvalidProjectNameReason::Format,
        })
    } else {
        Ok(())
    }
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
