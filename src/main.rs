#![deny(warnings)]

mod ast;
mod diagnostic;
mod docs;
mod erl;
mod error;
mod format;
mod new;
mod parser;
mod pretty;
mod project;
mod typ;
mod warning;

lalrpop_mod!(
    #[allow(deprecated)]
    #[allow(clippy::all)]
    #[allow(dead_code)]
    #[allow(unused_parens)]
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

use crate::{
    error::Error,
    project::{ModuleOrigin, OutputFile},
};
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
    #[structopt(name = "build", about = "Compile a project")]
    Build {
        #[structopt(help = "location of the project root", default_value = ".")]
        path: String,
    },

    #[structopt(name = "docs", about = "Render HTML documentation for a project")]
    Docs(Docs),

    #[structopt(name = "new", about = "Create a new project")]
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

    #[structopt(name = "format", about = "Format source code")]
    Format {
        #[structopt(help = "files to format", conflicts_with = "stdin")]
        files: Vec<String>,

        #[structopt(
            help = "read source from standard in",
            long = "stdin",
            conflicts_with = "files"
        )]
        stdin: bool,

        #[structopt(
            help = "check if inputs are formatted without changing them",
            long = "check"
        )]
        check: bool,
    },
}

#[derive(StructOpt, Debug)]
enum Docs {
    #[structopt(name = "build", about = "Render HTML docs locally")]
    Build,

    #[structopt(name = "format", about = "Publish HTML docs to HexDocs")]
    Publish,

    #[structopt(name = "revoke", about = "Remove HTML docs from HexDocs")]
    Revoke {
        #[structopt(help = "the name of the package", long = "package")]
        package: String,

        #[structopt(help = "the version to revoke the docs of", long = "version")]
        version: String,
    },
}

fn main() {
    let result = match Command::from_args() {
        Command::Build { path } => command_build(path, false),

        Command::Docs(Docs::Build) => command_build(".".to_string(), true),

        Command::Docs(Docs::Publish) => command_build(".".to_string(), true).and_then(|_| todo!()),

        Command::Docs(Docs::Revoke { package, version }) => {
            crate::docs::command::revoke(package, version)
        }

        Command::Format {
            stdin,
            files,
            check,
        } => crate::format::command::run(stdin, check, files),

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

fn command_build(root: String, write_docs: bool) -> Result<(), Error> {
    let mut srcs = vec![];

    // Read gleam.toml
    let project_config = project::read_project_config(&root)?;

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

    let analysed = crate::project::analysed(srcs)?;

    // Generate outputs (Erlang code, html documentation, etc)
    let mut output_files = vec![];
    if write_docs {
        let dir = root_path.join("docs");
        crate::docs::generate_html(
            &project_config,
            analysed.as_slice(),
            &mut output_files,
            &dir,
        );
        delete_dir(&dir)?;
    } else {
        let dir = root_path.join("gen");
        crate::project::generate_erlang(analysed.as_slice(), &mut output_files);
        delete_dir(&dir)?;
    }

    // Delete the gen directory before generating the newly compiled files
    for file in output_files {
        write_file(file)?;
    }

    // Print warnings
    for a in analysed.iter() {
        for w in a.warnings.iter() {
            w.pretty_print()
        }
    }

    println!("Done!");

    Ok(())
}

fn delete_dir(dir: &PathBuf) -> Result<(), Error> {
    if dir.exists() {
        std::fs::remove_dir_all(&dir).map_err(|e| Error::FileIO {
            action: error::FileIOAction::Delete,
            kind: error::FileKind::Directory,
            path: dir.clone(),
            err: Some(e.to_string()),
        })?;
    }
    Ok(())
}

pub fn write_file(file: OutputFile) -> Result<(), Error> {
    let OutputFile { path, text } = file;

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
    Ok(())
}
