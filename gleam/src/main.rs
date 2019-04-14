// https://crates.io/crates/lalrpop
// - https://github.com/lalrpop/lalrpop/issues/323#issuecomment-366681594
// - https://github.com/dagit/rust-prolog/blob/master/src/lexer.rs
// - https://github.com/dagit/rust-prolog/blob/master/src/parser.lalrpop
//
// Error displaying
// - https://github.com/brendanzab/codespan

mod ast;
mod erl;
mod parser;
mod pretty;
mod project;
mod typ;
lalrpop_mod!(
    #[allow(deprecated)]
    #[allow(clippy::all)]
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

#[macro_use]
extern crate serde_derive;

use crate::project::ModuleOrigin;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use structopt::clap::AppSettings;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(raw(
    global_settings = "&[AppSettings::ColoredHelp, AppSettings::VersionlessSubcommands]"
))]
enum Command {
    #[structopt(name = "build", about = "Compile Gleam modules in a given project")]
    Build {
        #[structopt(help = "location of the project root", default_value = ".")]
        path: String,
    },
}

#[derive(Deserialize)]
struct ProjectConfig {
    name: String,
}

fn main() {
    match Command::from_args() {
        Command::Build { path } => command_build(path),
    }
}

fn command_build(root: String) {
    let mut srcs = vec![];

    // Read gleam.toml
    let project_config = read_project_config(&root).expect("Could not read gleam.toml");

    let root_path = PathBuf::from(&root);
    let lib_dir = root_path.join("_build").join("default").join("lib");

    if let Ok(dir) = std::fs::read_dir(lib_dir) {
        dir.filter_map(Result::ok)
            .map(|d| d.path())
            .filter(|p| {
                p.file_name().and_then(|os_string| os_string.to_str()) != Some(&project_config.name)
            })
            .for_each(|p| collect_source(p.join("src"), ModuleOrigin::Src, &mut srcs));
    }

    collect_source(root_path.join("src"), ModuleOrigin::Src, &mut srcs);
    collect_source(root_path.join("test"), ModuleOrigin::Test, &mut srcs);

    let compiled = match crate::project::compile(srcs) {
        Ok(c) => c,
        Err(e) => {
            e.pretty_print();
            std::process::exit(1);
        }
    };

    for crate::project::Compiled { code, path, .. } in compiled {
        let gen_path = path.parent().unwrap();
        std::fs::create_dir_all(gen_path).expect("creating gen dir");

        let mut f = File::create(path).expect("Unable to create file");
        f.write_all(code.as_bytes())
            .expect("Unable to write Erlang code to file");
    }

    println!("Done!");
}

fn read_project_config(root: &str) -> Result<ProjectConfig, ()> {
    use std::io::Read;
    let mut toml = String::new();
    let config_path = PathBuf::from(root).join("gleam.toml");
    let mut file = File::open(config_path).expect("Unable to open gleam.toml");
    file.read_to_string(&mut toml)
        .expect("Unable to read gleam.toml");
    Ok(toml::from_str(&toml).expect("Unable to parse gleam.toml"))
}

fn collect_source(src_dir: PathBuf, origin: ModuleOrigin, srcs: &mut Vec<crate::project::Input>) {
    let entries = match std::fs::read_dir(src_dir.clone()) {
        Ok(e) => e,
        Err(_) => return,
    };

    entries
        .filter_map(Result::ok)
        .filter(|d| {
            if let Some(e) = d.path().extension() {
                e == "gleam"
            } else {
                false
            }
        })
        .for_each(|dir_entry| {
            let src = std::fs::read_to_string(dir_entry.path())
                .unwrap_or_else(|_| panic!("Unable to read {:?}", dir_entry.path()));

            srcs.push(project::Input {
                path: dir_entry.path().canonicalize().unwrap(),
                origin: origin.clone(),
                src,
            })
        });
}
