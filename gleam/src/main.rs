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

use clap::{crate_version, App, AppSettings, Arg, SubCommand};

fn main() {
    let matches = App::new("gleam")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .setting(AppSettings::ColoredHelp)
        .setting(AppSettings::VersionlessSubcommands)
        .version(crate_version!())
        .subcommand(
            SubCommand::with_name("build")
                .about("Compile Gleam modules in a given project")
                .setting(AppSettings::ColoredHelp)
                .arg(
                    Arg::with_name("path")
                        .help("location of the project root")
                        .default_value("./")
                        .index(1),
                ),
        )
        .get_matches();

    match matches.subcommand_name() {
        Some("build") => command_build(matches.subcommand_matches("build").unwrap()),
        _ => unreachable!(),
    }
}

fn command_build(matches: &clap::ArgMatches) {
    use std::{fs, path::Path};

    let root_dir = Path::new(matches.value_of("path").unwrap_or("."));
    let src_dir = root_dir.join("src");
    let srcs = fs::read_dir(src_dir)
        .expect("Could not locate src/")
        .filter_map(Result::ok)
        .filter(|d| {
            if let Some(e) = d.path().extension() {
                e == "gleam"
            } else {
                false
            }
        })
        .collect::<Vec<_>>();

    for dir_entry in srcs {
        let src = fs::read_to_string(dir_entry.path())
            .expect(&format!("Unable to read {:?}", dir_entry.path()));

        let name = dir_entry
            .path()
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();

        let mut module = crate::grammar::ModuleParser::new()
            .parse(&src)
            .expect(&format!("Unable to parse {:?}", dir_entry.path()));

        module.name = name;

        // TODO: infer types.

        let module = {
            use crate::{ast::*, typ::*};
            Module {
                name: module.name,
                statements: vec![Statement::Fun {
                    meta: Meta::default(),
                    public: true,
                    name: "hello".to_string(),
                    args: vec![],
                    body: Expr::String {
                        meta: Meta::default(),
                        typ: string(),
                        value: "Hello, world!".to_string(),
                    },
                }],
            }
        };

        let erl = crate::erl::module(module);

        println!("{}", erl);
    }
}
