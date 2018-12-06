// https://crates.io/crates/nom
//
// https://crates.io/crates/pest
// - https://docs.rs/pest/2.0.2/pest/struct.Position.html#method.line_col
//
// https://crates.io/crates/lalrpop
// - https://github.com/lalrpop/lalrpop/issues/323#issuecomment-366681594

mod ast;
mod pattern;
mod typ;

#[macro_use]
extern crate clap;
use clap::{App, AppSettings, SubCommand};

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
