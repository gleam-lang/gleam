// https://crates.io/crates/nom
//
// https://crates.io/crates/pest
// - https://docs.rs/pest/2.0.2/pest/struct.Position.html#method.line_col
//
// https://crates.io/crates/lalrpop
// - https://github.com/lalrpop/lalrpop/issues/323#issuecomment-366681594
//
// https://github.com/Marwes/combine

mod ast;
mod erl;
mod parser;
mod pattern;
mod pretty;
mod typ;

#[macro_use]
extern crate im;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

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
