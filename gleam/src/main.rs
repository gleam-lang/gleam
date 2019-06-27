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
        let gen_path = path.parent().expect("gen_path");
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
    let src_dir = match src_dir.canonicalize() {
        Ok(d) => d,
        Err(_) => return,
    };
    let is_gleam_path = |e: &walkdir::DirEntry| {
        use regex::Regex;
        lazy_static! {
            static ref RE: Regex =
                Regex::new("^([a-z_]+/)*[a-z_]+\\.gleam$").expect("collect_source RE regex");
        }

        RE.is_match(
            e.path()
                .strip_prefix(&*src_dir)
                .expect("collect_source strip_prefix")
                .to_str()
                .unwrap_or(""),
        )
    };

    walkdir::WalkDir::new(src_dir.clone())
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .filter(is_gleam_path)
        .for_each(|dir_entry| {
            let src = std::fs::read_to_string(dir_entry.path())
                .unwrap_or_else(|_| panic!("Unable to read {:?}", dir_entry.path()));

            srcs.push(project::Input {
                path: dir_entry
                    .path()
                    .canonicalize()
                    .expect("collect_source path canonicalize"),
                base_path: src_dir.clone(),
                origin: origin.clone(),
                src,
            })
        });
}
