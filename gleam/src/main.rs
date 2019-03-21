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

fn main() {
    match Command::from_args() {
        Command::Build { path } => command_build(path),
    }
}

fn command_build(root: String) {
    let mut srcs = vec![];
    let root_path = PathBuf::from(&root);
    let lib_dir = root_path.join("_build/default/lib");

    std::fs::read_dir(lib_dir)
        .expect("Could not locate _build/default/lib")
        .filter_map(Result::ok)
        .map(|d| d.path())
        .filter(|p| p.file_name() != root_path.file_name())
        .for_each(|p| collect_app(p, &mut srcs));

    collect_app(root_path, &mut srcs);

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

fn collect_app(app_path: PathBuf, srcs: &mut Vec<crate::project::Input>) {
    let src_dir = app_path.join("src");

    std::fs::read_dir(src_dir.clone())
        .expect("Could not locate src/")
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
                src,
            })
        });
}
