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

use petgraph::Graph;
use std::collections::HashMap;
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
        .collect::<Vec<_>>();

    let mut deps_graph = Graph::new();
    let mut deps_vec = Vec::with_capacity(srcs.len());
    let mut indexes = HashMap::new();
    let mut modules = HashMap::new();

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
            .parse(&crate::parser::strip_extra(&src))
            .expect(&format!("Unable to parse {:?}", name));

        module.name = name.clone();

        let index = deps_graph.add_node(name.clone());
        deps_vec.push((name.clone(), module.dependancies()));
        indexes.insert(name.clone(), index.clone());
        modules.insert(index, module);
    }

    // Register each module's deps so that we can determine a correct order to compile the modules.
    for (module, deps) in deps_vec {
        let module_index = indexes.get(&module).expect("Unable to find module index");
        for dep in deps {
            let dep_index = indexes.get(&dep).expect("Unable to find module index");
            deps_graph.add_edge(module_index.clone(), dep_index.clone(), ());
        }
    }

    let modules = petgraph::algo::toposort(&deps_graph, None)
        .expect("Could not determine module compile order")
        .into_iter()
        .map(|i| modules.remove(&i).expect("Unknown graph index"))
        .for_each(|module| {
            let name = module.name.clone();
            let module = crate::typ::infer_module(module)
                .expect(&format!("Unable to infer types of {:?}", name));

            let erl = crate::erl::module(module);

            println!("{}", erl);

            let erl_name = format!("gleam_{}.erl", name);
            let mut f = File::create(src_dir.join(erl_name)).expect("Unable to create file");
            f.write_all(erl.as_bytes())
                .expect("Unable to write Erlang code to file");
        });
}
