mod source_tree;
#[cfg(test)]
mod tests;

use super::doc::doc::EEP48DocChunk;
use crate::error::{Error, FileIOAction, FileKind, GleamExpect};
use crate::typ;
use source_tree::SourceTree;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, PartialEq)]
pub struct Input {
    pub source_base_path: PathBuf,
    pub path: PathBuf,
    pub src: String,
    pub origin: ModuleOrigin,
}

#[derive(Debug, PartialEq)]
pub struct Compiled {
    pub name: Vec<String>,
    pub origin: ModuleOrigin,
    pub files: Vec<OutputFile>,
    pub type_info: typ::Module,
    pub doc: EEP48DocChunk,
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

pub fn compile(inputs: Vec<Input>) -> Result<Vec<Compiled>, Error> {
    let module_count = inputs.len();
    let mut source_tree = SourceTree::new(inputs)?;
    let mut modules_type_infos = HashMap::new();
    let mut compiled_modules = Vec::with_capacity(module_count);

    struct Out {
        name_string: String,
        name: Vec<String>,
        origin: ModuleOrigin,
        files: Vec<OutputFile>,
        doc: EEP48DocChunk,
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

        let module = crate::typ::infer_module(module, &modules_type_infos)
            .map_err(|error| Error::Type { path, src, error })?;

        modules_type_infos.insert(name_string.clone(), module.type_info.clone());

        let gen_dir = source_base_path
            .parent()
            .unwrap()
            .join("gen")
            .join(origin.dir_name());
        let erl_module_name = module.name.join("@");

        let mut files: Vec<_> = crate::erl::records(&module)
            .into_iter()
            .map(|(name, text)| OutputFile {
                path: gen_dir.join(format!("{}_{}.hrl", erl_module_name, name)),
                text,
            })
            .collect();

        let doc_chunk = crate::doc::block_manager::gen_doc_chunk(&module);

        files.push(OutputFile {
            path: gen_dir.join(format!("{}.erl", erl_module_name)),
            text: crate::erl::module(module),
        });

        compiled_modules.push(Out {
            name,
            name_string,
            origin,
            files,
            doc: doc_chunk,
        });
    }

    Ok(compiled_modules
        .into_iter()
        .map(
            |Out {
                 name,
                 name_string,
                 origin,
                 files,
                 doc,
             }| Compiled {
                name,
                files,
                origin,
                type_info: modules_type_infos
                    .remove(&name_string)
                    .gleam_expect("project::compile(): Merging module type info"),
                doc,
            },
        )
        .collect())
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
    let is_gleam_path = |e: &walkdir::DirEntry| {
        use regex::Regex;
        lazy_static! {
            static ref RE: Regex = Regex::new("^([a-z_]+(/|\\\\))*[a-z_]+\\.gleam$")
                .gleam_expect("project::collect_source() RE regex");
        }

        RE.is_match(
            e.path()
                .strip_prefix(&*src_dir)
                .gleam_expect("project::collect_source(): strip_prefix")
                .to_str()
                .unwrap_or(""),
        )
    };

    for dir_entry in walkdir::WalkDir::new(src_dir.clone())
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .filter(is_gleam_path)
    {
        let src = std::fs::read_to_string(dir_entry.path()).map_err(|err| Error::FileIO {
            action: FileIOAction::Read,
            kind: FileKind::File,
            path: dir_entry.path().to_path_buf(),
            err: Some(err.to_string()),
        })?;

        srcs.push(Input {
            path: dir_entry
                .path()
                .canonicalize()
                .gleam_expect("project::collect_source(): path canonicalize"),
            source_base_path: src_dir.clone(),
            origin: origin.clone(),
            src,
        })
    }
    Ok(())
}
