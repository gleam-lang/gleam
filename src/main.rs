mod ast;
mod erl;
mod error;
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
    match Command::from_args() {
        Command::Build { path } => {
            if let Err(e) = command_build(path) {
                e.pretty_print();
                std::process::exit(1);
            }
        }

        Command::New {
            name,
            path,
            template,
        } => crate::new::create(template, name, path, VERSION),
    }
}

fn command_build(root: String) -> Result<(), Error> {
    let mut srcs = vec![];

    // Read gleam.toml
    let project_config = read_project_config(&root).expect("Could not read gleam.toml");

    let root_path = PathBuf::from(&root);
    let lib_dir = root_path.join("_build").join("default").join("lib");
    let checkouts_dir = root_path.join("_checkouts");

    [lib_dir, checkouts_dir]
        .iter()
        .filter_map(|d| std::fs::read_dir(d).ok())
        .flat_map(|d| d.filter_map(Result::ok))
        .map(|d| d.path())
        .filter(|p| {
            p.file_name().and_then(|os_string| os_string.to_str()) != Some(&project_config.name)
        })
        .for_each(|p| {
            crate::project::collect_source(p.join("src"), ModuleOrigin::Dependency, &mut srcs)
        });

    // Collect source code from top level project
    crate::project::collect_source(root_path.join("src"), ModuleOrigin::Src, &mut srcs);
    crate::project::collect_source(root_path.join("test"), ModuleOrigin::Test, &mut srcs);

    let compiled = crate::project::compile(srcs)?;

    for crate::project::Compiled { files, .. } in compiled {
        for crate::project::OutputFile { text, path } in files {
            let dir_path = path
                .parent()
                .unwrap_or_else(|| panic!("getting output file directory {:?}", path));
            std::fs::create_dir_all(dir_path).unwrap_or_else(|e| {
                panic!(
                    "creating output file directory {:?}: {:?}",
                    dir_path,
                    e.to_string()
                )
            });

            let mut f = File::create(&path)
                .unwrap_or_else(|e| panic!("creating output file {:?}: {:?}", path, e.to_string()));
            f.write_all(text.as_bytes()).unwrap_or_else(|e| {
                panic!("writing to output file {:?}: {:?}", path, e.to_string())
            });
        }
    }

    println!("Done!");
    Ok(())
}

fn read_project_config(root: &str) -> Result<ProjectConfig, ()> {
    fn die(message: String) -> ! {
        use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
        let mut stderr = StandardStream::stderr(ColorChoice::Always);

        // Write "error: " in red
        stderr
            .set_color(
                ColorSpec::new()
                    .set_fg(Some(Color::Red))
                    .set_intense(true)
                    .set_bold(true),
            )
            .unwrap();
        write!(&mut stderr, "error: ").unwrap();

        // Write error message in white
        stderr.reset().unwrap();
        writeln!(&mut stderr, "{}", message).unwrap();

        std::process::exit(1);
    }

    use std::io::Read;
    let config_path = PathBuf::from(root).join("gleam.toml");
    let mut file = File::open(config_path)
        .unwrap_or_else(|e| die(format!("could not open gleam.toml: {}", e.to_string())));

    let mut toml = String::new();
    file.read_to_string(&mut toml)
        .unwrap_or_else(|e| die(format!("could not read gleam.toml: {}", e.to_string())));

    let project_config = toml::from_str(&toml)
        .unwrap_or_else(|e| die(format!("could not parse gleam.toml: {}", e.to_string())));

    Ok(project_config)
}
