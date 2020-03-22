mod ast;
mod erl;
mod error;
mod format;
mod new;
mod parser;
mod pretty;
mod project;
mod typ;
lalrpop_mod!(
    #[allow(deprecated)]
    #[allow(clippy::all)]
    #[allow(dead_code)]
    grammar
);

#[macro_use]
extern crate im;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate lalrpop_util;

#[macro_use]
extern crate lazy_static;

use crate::error::Error;
use crate::project::ModuleOrigin;
use serde::Deserialize;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use structopt::clap::AppSettings;
use structopt::StructOpt;
use strum::VariantNames;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(StructOpt, Debug)]
#[structopt(global_settings = &[AppSettings::ColoredHelp, AppSettings::VersionlessSubcommands])]
enum Command {
    #[structopt(name = "build", about = "Compile a Gleam project")]
    Build {
        #[structopt(help = "location of the project root", default_value = ".")]
        path: String,
    },

    #[structopt(name = "new", about = "Create a new Gleam project")]
    New {
        #[structopt(help = "name of the project")]
        name: String,

        #[structopt(long = "description", help = "description of the project")]
        description: Option<String>,

        #[structopt(help = "location of the project root")]
        path: Option<String>,

        #[structopt(
            long = "template",
            possible_values = &new::Template::VARIANTS,
            case_insensitive = true,
            default_value = "lib"
        )]
        template: new::Template,
    },
}

#[derive(Deserialize)]
struct ProjectConfig {
    name: String,
}

fn main() {
    let result = match Command::from_args() {
        Command::Build { path } => command_build(path),

        Command::New {
            name,
            description,
            path,
            template,
        } => crate::new::create(template, name, description, path, VERSION),
    };

    if let Err(e) = result {
        e.pretty_print();
        std::process::exit(1);
    }
}

fn command_build(root: String) -> Result<(), Error> {
    let mut srcs = vec![];

    // Read gleam.toml
    let project_config = read_project_config(&root)?;

    let root_path = PathBuf::from(&root);
    let lib_dir = root_path.join("_build").join("default").join("lib");
    let checkouts_dir = root_path.join("_checkouts");

    for project_dir in [lib_dir, checkouts_dir]
        .iter()
        .filter_map(|d| std::fs::read_dir(d).ok())
        .flat_map(|d| d.filter_map(Result::ok))
        .map(|d| d.path())
        .filter(|p| {
            p.file_name().and_then(|os_string| os_string.to_str()) != Some(&project_config.name)
        })
    {
        crate::project::collect_source(
            project_dir.join("src"),
            ModuleOrigin::Dependency,
            &mut srcs,
        )?;
    }

    // Collect source code from top level project
    crate::project::collect_source(root_path.join("src"), ModuleOrigin::Src, &mut srcs)?;
    crate::project::collect_source(root_path.join("test"), ModuleOrigin::Test, &mut srcs)?;

    let compiled = crate::project::compile(srcs)?;

    // Delete the gen directory before generating the newly compiled files
    let gen_dir = root_path
        .parent()
        .ok_or_else(|| Error::FileIO {
            action: error::FileIOAction::FindParent,
            kind: error::FileKind::Directory,
            path: root_path.clone(),
            err: None,
        })?
        .join("gen");

    if gen_dir.exists() {
        std::fs::remove_dir_all(&gen_dir).map_err(|e| Error::FileIO {
            action: error::FileIOAction::Delete,
            kind: error::FileKind::Directory,
            path: gen_dir,
            err: Some(e.to_string()),
        })?;
    }

    for crate::project::Compiled { files, .. } in compiled {
        for crate::project::OutputFile { text, path } in files {
            let dir_path = path.parent().ok_or_else(|| Error::FileIO {
                action: error::FileIOAction::FindParent,
                kind: error::FileKind::Directory,
                path: path.clone(),
                err: None,
            })?;

            std::fs::create_dir_all(dir_path).map_err(|e| Error::FileIO {
                action: error::FileIOAction::Create,
                kind: error::FileKind::Directory,
                path: dir_path.to_path_buf(),
                err: Some(e.to_string()),
            })?;

            let mut f = File::create(&path).map_err(|e| Error::FileIO {
                action: error::FileIOAction::Create,
                kind: error::FileKind::File,
                path: path.clone(),
                err: Some(e.to_string()),
            })?;

            f.write_all(text.as_bytes()).map_err(|e| Error::FileIO {
                action: error::FileIOAction::WriteTo,
                kind: error::FileKind::File,
                path: path.clone(),
                err: Some(e.to_string()),
            })?;
        }
    }

    println!("Done!");
    Ok(())
}

fn read_project_config(root: &str) -> Result<ProjectConfig, Error> {
    use std::io::Read;
    let config_path = PathBuf::from(root).join("gleam.toml");

    let mut file = File::open(&config_path).map_err(|e| Error::FileIO {
        action: error::FileIOAction::Open,
        kind: error::FileKind::File,
        path: config_path.clone(),
        err: Some(e.to_string()),
    })?;

    let mut toml = String::new();
    file.read_to_string(&mut toml).map_err(|e| Error::FileIO {
        action: error::FileIOAction::Read,
        kind: error::FileKind::File,
        path: config_path.clone(),
        err: Some(e.to_string()),
    })?;

    let project_config = toml::from_str(&toml).map_err(|e| Error::FileIO {
        action: error::FileIOAction::Parse,
        kind: error::FileKind::File,
        path: config_path.clone(),
        err: Some(e.to_string()),
    })?;

    Ok(project_config)
}
