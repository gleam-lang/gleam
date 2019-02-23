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
    grammar
);

#[macro_use]
extern crate im;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate lalrpop_util;

use std::fs::File;
use std::io::Write;
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
    use std::{fs, path::Path};

    let root_dir = Path::new(&root);
    let src_dir = root_dir.join("src");
    let srcs = fs::read_dir(src_dir.clone())
        .expect("Could not locate src/")
        .filter_map(Result::ok)
        .filter(|d| {
            if let Some(e) = d.path().extension() {
                e == "gleam"
            } else {
                false
            }
        })
        .map(|dir_entry| {
            let src = fs::read_to_string(dir_entry.path())
                .expect(&format!("Unable to read {:?}", dir_entry.path()));

            let name = dir_entry
                .path()
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string();
            (name, src)
        })
        .collect::<Vec<_>>();

    let compiled = match crate::project::compile(srcs) {
        Ok(c) => c,
        Err(e) => {
            e.pretty_print();
            std::process::exit(1);
        }
    };

    for crate::project::Compiled { name, out, .. } in compiled {
        let erl_name = format!("gleam_{}.erl", name);
        let mut f = File::create(src_dir.join(erl_name)).expect("Unable to create file");
        f.write_all(out.as_bytes())
            .expect("Unable to write Erlang code to file");
    }

    println!("Done!");
}
