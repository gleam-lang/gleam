use crate::{
    build::{project_root::ProjectRoot, Package},
    erl,
    file::{self, OutputFile},
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct ErlangCodeGenerator<'a> {
    root: &'a ProjectRoot,
    packages: &'a HashMap<String, Package<'a>>,
}

// TODO: test: A couple of packages with a couple of modules and headers.
// Make sure all file names are right etc.
impl<'a> ErlangCodeGenerator<'a> {
    pub fn new(root: &'a ProjectRoot, packages: &'a HashMap<String, Package<'a>>) -> Self {
        Self { root, packages }
    }

    pub fn render(&self) -> Vec<OutputFile> {
        let num_modules = self.packages.values().fold(0, |a, p| p.modules.len() + a);
        let mut outputs = Vec::with_capacity(num_modules);

        for (_name, package) in self.packages {
            self.render_package(package, &mut outputs);
        }

        outputs
    }

    pub fn render_package(&self, package: &Package, outputs: &mut Vec<OutputFile>) {
        let src_dir = self.root.default_build_lib_package_src_path(package.name);
        for module in &package.modules {
            let erl_name = module.name.replace("/", "@");

            // Render record header files
            for (name, text) in erl::records(&module.ast).into_iter() {
                outputs.push(OutputFile {
                    path: src_dir.join(format!("{}_{}.hrl", erl_name, name)),
                    text,
                });
            }

            // Render Erlang module file
            let text = erl::module(&module.ast);
            let path = src_dir.join(format!("{}.erl", erl_name));
            outputs.push(OutputFile { path, text });
        }
    }
}
