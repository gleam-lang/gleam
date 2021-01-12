mod source_tree;
#[cfg(test)]
mod tests;

use crate::{
    ast::{SrcSpan, TypedModule},
    build::Origin,
    config::{self, PackageConfig},
    error::{Error, FileIOAction, FileKind, GleamExpect},
    parse::extra::Comment,
    typ,
    warning::Warning,
};
use source_tree::SourceTree;
use std::collections::HashMap;
use std::iter::Peekable;
use std::path::{Path, PathBuf};

pub const OUTPUT_DIR_NAME: &str = "gen";

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
    pub src: String,
    pub name: Vec<String>,
    pub path: PathBuf,
    pub origin: ModuleOrigin,
    pub type_info: typ::Module,
    pub source_base_path: PathBuf,
    pub warnings: Vec<Warning>,
    pub module_extra: crate::parse::extra::ModuleExtra,
}
impl Analysed {
    pub fn attach_doc_and_module_comments(&mut self) {
        // Module Comments
        self.ast.documentation = self
            .module_extra
            .module_comments
            .iter()
            .map(|span| Comment::from((span, self.src.as_str())).content.to_string())
            .collect();

        // Doc Comments
        let mut doc_comments = self.module_extra.doc_comments.iter().peekable();
        for statement in &mut self.ast.statements {
            let docs: Vec<&str> = comments_before(
                &mut doc_comments,
                statement.location().start,
                self.src.as_str(),
            );
            if !docs.is_empty() {
                let doc = docs.join("\n");
                statement.put_doc(doc);
            }

            if let crate::ast::Statement::CustomType { constructors, .. } = statement {
                for constructor in constructors {
                    let docs: Vec<&str> = comments_before(
                        &mut doc_comments,
                        constructor.location.start,
                        self.src.as_str(),
                    );
                    if !docs.is_empty() {
                        let doc = docs.join("\n");
                        constructor.put_doc(doc);
                    }
                }
            }
        }
    }
}

fn comments_before<'a>(
    comment_spans: &mut Peekable<impl Iterator<Item = &'a SrcSpan>>,
    byte: usize,
    src: &'a str,
) -> Vec<&'a str> {
    let mut comments = vec![];
    while let Some(SrcSpan { start, .. }) = comment_spans.peek() {
        if start <= &byte {
            let comment = comment_spans.next().unwrap();
            comments.push(Comment::from((comment, src)).content)
        } else {
            break;
        }
    }
    comments
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
            Self::Src | Self::Dependency => "src",
            Self::Test => "test",
        }
    }

    pub fn to_origin(&self) -> Origin {
        match self {
            Self::Test => Origin::Test,
            Self::Src | Self::Dependency => Origin::Src,
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
    module_extra: crate::parse::extra::ModuleExtra,
}

pub fn read_and_analyse(root: impl AsRef<Path>) -> Result<(PackageConfig, Vec<Analysed>), Error> {
    let project_config = config::read_project_config(&root)?;
    let mut srcs = vec![];
    let root = root.as_ref();
    let lib_dir = root.join("_build").join("default").join("lib");
    let checkouts_dir = root.join("_checkouts");
    let mix_lib_dir = root.join("deps");

    for project_dir in [lib_dir, checkouts_dir, mix_lib_dir]
        .iter()
        .filter_map(|d| std::fs::read_dir(d).ok())
        .flat_map(|d| d.filter_map(Result::ok))
        .map(|d| d.path())
        .filter(|p| {
            p.file_name().and_then(|os_string| os_string.to_str()) != Some(&project_config.name)
        })
    {
        collect_source(project_dir.join("src"), ModuleOrigin::Dependency, &mut srcs)?;
    }

    // Collect source code from top level project
    collect_source(root.join("src"), ModuleOrigin::Src, &mut srcs)?;
    collect_source(root.join("test"), ModuleOrigin::Test, &mut srcs)?;

    // Analyse source
    let analysed = analysed(srcs)?;

    Ok((project_config, analysed))
}

pub fn analysed(inputs: Vec<Input>) -> Result<Vec<Analysed>, Error> {
    let module_count = inputs.len();
    let mut source_tree = SourceTree::new(inputs)?;
    let mut modules_type_infos = HashMap::new();
    let mut compiled_modules = Vec::with_capacity(module_count);
    let mut uid = 0;

    struct Out {
        source_base_path: PathBuf,
        name_string: String,
        name: Vec<String>,
        path: PathBuf,
        origin: ModuleOrigin,
        ast: TypedModule,
        src: String,
        warnings: Vec<Warning>,
        module_extra: crate::parse::extra::ModuleExtra,
    }

    for Module {
        src,
        path,
        module,
        origin,
        source_base_path,
        module_extra,
    } in source_tree.consume()?
    {
        let name = module.name.clone();
        let name_string = module.name_string();

        println!("Compiling {}", name_string.as_str());

        let mut warnings = vec![];
        let result = crate::typ::infer_module(&mut uid, module, &modules_type_infos, &mut warnings);
        let warnings = warnings
            .into_iter()
            .map(|warning| Warning::Type {
                path: path.clone(),
                src: src.clone(),
                warning,
            })
            .collect();

        let ast = result.map_err(|error| Error::Type {
            path: path.clone(),
            src: src.clone(),
            error,
        })?;

        modules_type_infos.insert(
            name_string.clone(),
            (origin.to_origin(), ast.type_info.clone()),
        );

        compiled_modules.push(Out {
            name,
            name_string,
            path,
            source_base_path,
            origin,
            ast,
            src,
            warnings,
            module_extra,
        });
    }

    Ok(compiled_modules
        .into_iter()
        .map(|out| {
            let Out {
                name,
                source_base_path,
                path,
                name_string,
                origin,
                ast,
                src,
                warnings,
                module_extra,
            } = out;
            Analysed {
                ast,
                src,
                name,
                path,
                source_base_path,
                origin,
                type_info: modules_type_infos
                    .remove(&name_string)
                    .gleam_expect("project::compile(): Merging module type info")
                    .1,
                warnings,
                module_extra,
            }
        })
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

    for path in crate::fs::gleam_files(&src_dir) {
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
