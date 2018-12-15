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
lalrpop_mod!(pub grammar);

#[macro_use]
extern crate im;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate lalrpop_util;

use clap::{crate_version, App, AppSettings, SubCommand};

fn main() {
    match App::new("gleam")
        .author("Louis Pilfold <louis@lpil.uk>")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .setting(AppSettings::ColoredHelp)
        .version(crate_version!())
        .subcommand(SubCommand::with_name("hello").about("Say hello!"))
        .get_matches()
        .subcommand_name()
    {
        Some("hello") => println!("Hello, sailor!"),
        _ => unreachable!(),
    }
}
