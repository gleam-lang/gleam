mod source_tree;
#[cfg(test)]
mod tests;

use crate::{
    ast::TypedModule,
    error::{Error, FileIOAction, FileKind, GleamExpect},
    typ,
};
use serde::Deserialize;
use source_tree::SourceTree;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Deserialize)]
pub struct ProjectConfig {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct Input {
    pub source_base_path: PathBuf,
    pub path: PathBuf,
    pub src: String,
    pub origin: ModuleOrigin,
}

#[derive(Debug, PartialEq)]
pub struct Analysed {
    pub ast: TypedModule,
    pub name: Vec<String>,
    pub origin: ModuleOrigin,
    pub type_info: typ::Module,
    pub source_base_path: PathBuf,
}

#[derive(Debug, PartialEq)]
pub struct OutputFile {
    pub text: String,
    pub path: PathBuf,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ModuleOrigin {
    Src,
    Test,
    Dependency,
}

impl ModuleOrigin {
    pub fn dir_name(&self) -> &'static str {
        match self {
            ModuleOrigin::Src | ModuleOrigin::Dependency => "src",
            ModuleOrigin::Test => "test",
        }
    }
}

#[derive(Debug)]
pub struct Module {
    src: String,
    path: PathBuf,
    source_base_path: PathBuf,
    origin: ModuleOrigin,
    module: crate::ast::UntypedModule,
}

pub fn analysed(inputs: Vec<Input>) -> Result<Vec<Analysed>, Error> {
    let module_count = inputs.len();
    let mut source_tree = SourceTree::new(inputs)?;
    let mut modules_type_infos = HashMap::new();
    let mut compiled_modules = Vec::with_capacity(module_count);

    struct Out {
        source_base_path: PathBuf,
        name_string: String,
        name: Vec<String>,
        origin: ModuleOrigin,
        ast: TypedModule,
    }

    for Module {
        src,
        path,
        module,
        origin,
        source_base_path,
    } in source_tree.consume()?
    {
        let name = module.name.clone();
        let name_string = module.name_string();

        println!("Compiling {}", name_string);

        let ast = crate::typ::infer_module(module, &modules_type_infos)
            .map_err(|error| Error::Type { path, src, error })?;

        modules_type_infos.insert(name_string.clone(), ast.type_info.clone());

        compiled_modules.push(Out {
            name,
            name_string,
            source_base_path,
            origin,
            ast,
        });
    }

    Ok(compiled_modules
        .into_iter()
        .map(|out| {
            let Out {
                name,
                source_base_path,
                name_string,
                origin,
                ast,
            } = out;
            Analysed {
                ast,
                name,
                source_base_path,
                origin,
                type_info: modules_type_infos
                    .remove(&name_string)
                    .gleam_expect("project::compile(): Merging module type info"),
            }
        })
        .collect())
}

pub fn generate_erlang(analysed: &[Analysed], files: &mut Vec<OutputFile>) {
    for Analysed {
        name,
        origin,
        source_base_path,
        ast,
        ..
    } in analysed
    {
        let gen_dir = source_base_path
            .parent()
            .unwrap()
            .join("gen")
            .join(origin.dir_name());
        let erl_module_name = name.join("@");

        for (name, text) in crate::erl::records(&ast).into_iter() {
            files.push(OutputFile {
                path: gen_dir.join(format!("{}_{}.hrl", erl_module_name, name)),
                text,
            })
        }

        files.push(OutputFile {
            path: gen_dir.join(format!("{}.erl", erl_module_name)),
            text: crate::erl::module(&ast),
        });
    }
}

fn is_gleam_path(path: &PathBuf, dir: &PathBuf) -> bool {
    use regex::Regex;
    lazy_static! {
        static ref RE: Regex = Regex::new("^([a-z_]+(/|\\\\))*[a-z_]+\\.gleam$")
            .gleam_expect("project::collect_source() RE regex");
    }

    RE.is_match(
        path.strip_prefix(dir)
            .gleam_expect("project::collect_source(): strip_prefix")
            .to_str()
            .unwrap_or(""),
    )
}

pub fn gleam_files(dir: &PathBuf) -> impl Iterator<Item = PathBuf> + '_ {
    walkdir::WalkDir::new(dir.clone())
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .map(|d| d.path().to_path_buf())
        .filter(move |d| is_gleam_path(d, &dir))
}

pub fn collect_source(
    src_dir: PathBuf,
    origin: ModuleOrigin,
    srcs: &mut Vec<Input>,
) -> Result<(), Error> {
    let src_dir = match src_dir.canonicalize() {
        Ok(d) => d,
        Err(_) => return Ok(()),
    };

    for path in gleam_files(&src_dir) {
        let src = std::fs::read_to_string(&path).map_err(|err| Error::FileIO {
            action: FileIOAction::Read,
            kind: FileKind::File,
            err: Some(err.to_string()),
            path: path.clone(),
        })?;

        srcs.push(Input {
            path: path
                .canonicalize()
                .gleam_expect("project::collect_source(): path canonicalize"),
            source_base_path: src_dir.clone(),
            origin: origin.clone(),
            src,
        })
    }
    Ok(())
}
