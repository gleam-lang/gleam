use camino::{Utf8Path, Utf8PathBuf};
use gleam_core::{
    erlang,
    error::{Error, FileIoAction, FileKind, InvalidProjectNameReason},
    parse, Result,
};
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::{env, io::Write};
use strum::{Display, EnumString, EnumVariantNames};

#[cfg(test)]
mod tests;

use crate::NewOptions;

const GLEAM_STDLIB_VERSION: &str = "0.30";
const GLEEUNIT_VERSION: &str = "0.10";
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
        validate_root_folder(&options.project_root)?;

        let root = Utf8PathBuf::from(&options.project_root);
        let src = root.join("src");
        let test = root.join("test");
        let github = root.join(".github");
        let workflows = github.join("workflows");
        Ok(Self {
            root,
            src,
            test,
            github,
            workflows,
            gleam_version,
            options,
            project_name,
        })
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
                if !self.options.skip_git {
                    self.gitignore()?;
                }

                if !self.options.skip_git && !self.options.skip_github {
                    self.github_ci()?;
                }
                self.readme()?;
                self.gleam_toml()?;
                self.src_module()?;
                self.test_module()?;
            }
        }

        Ok(())
    }

    fn src_module(&self) -> Result<()> {
        write(
            self.src.join(format!("{}.gleam", self.project_name)),
            &format!(
                r#"import gleam/io

pub fn main() {{
  io.println("Hello from {}!")
}}
"#,
                self.project_name
            ),
        )
    }

    fn gitignore(&self) -> Result<()> {
        write(
            self.root.join(".gitignore"),
            "*.beam
*.ez
build
erl_crash.dump
",
        )
    }

    fn readme(&self) -> Result<()> {
        write(
            self.root.join("README.md"),
            &format!(
                r#"# {name}

[![Package Version](https://img.shields.io/hexpm/v/{name})](https://hex.pm/packages/{name})
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/{name}/)

{description}

## Quick start

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```

## Installation

If available on Hex this package can be added to your Gleam project:

```sh
gleam add {name}
```

and its documentation can be found at <https://hexdocs.pm/{name}>.
"#,
                name = self.project_name,
                description = self.options.description,
            ),
        )
    }

    fn github_ci(&self) -> Result<()> {
        write(
            self.workflows.join("test.yml"),
            &format!(
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
      - uses: actions/checkout@v3.5.3
      - uses: erlef/setup-beam@v1.16.0
        with:
          otp-version: "{}"
          gleam-version: "{}"
          rebar3-version: "{}"
          # elixir-version: "{}"
      - run: gleam format --check src test
      - run: gleam deps download
      - run: gleam test
"#,
                ERLANG_OTP_VERSION, self.gleam_version, REBAR3_VERSION, ELIXIR_VERSION,
            ),
        )
    }

    fn gleam_toml(&self) -> Result<()> {
        write(
            self.root.join("gleam.toml"),
            &format!(
                r#"name = "{name}"
version = "0.1.0"
description = "{description}"

# Fill out these fields if you intend to generate HTML documentation or publish
# your project to the Hex package manager.
#
# licences = ["Apache-2.0"]
# repository = {{ type = "github", user = "username", repo = "project" }}
# links = [{{ title = "Website", href = "https://gleam.run" }}]

[dependencies]
gleam_stdlib = "~> {gleam_stdlib}"

[dev-dependencies]
gleeunit = "~> {gleeunit}"
"#,
                name = self.project_name,
                description = self.options.description,
                gleam_stdlib = GLEAM_STDLIB_VERSION,
                gleeunit = GLEEUNIT_VERSION,
            ),
        )
    }

    fn test_module(&self) -> Result<()> {
        write(
            self.test.join(format!("{}_test.gleam", self.project_name)),
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
"#,
        )
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

fn validate_root_folder(name: &str) -> Result<(), Error> {
    if Utf8Path::new(name).exists() {
        Err(Error::ProjectRootAlreadyExist {
            path: name.to_string(),
        })
    } else {
        Ok(())
    }
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
