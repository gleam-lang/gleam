//! TODO: Remove this rebar3 support

mod manifest;
mod source_tree;
#[cfg(test)]
mod tests;

use crate::{
    ast::{self, SrcSpan, Statement, TypedModule},
    build::{Origin, Target},
    parse::extra::{Comment, ModuleExtra},
    type_,
    uid::UniqueIdGenerator,
    Error, Result, Warning,
};
use source_tree::SourceTree;
use std::{iter::Peekable, path::PathBuf};

pub use manifest::{Base16Checksum, Manifest, ManifestPackage, ManifestPackageSource};

pub const OUTPUT_DIR_NAME: &str = "gen";

#[derive(Debug, PartialEq)]
pub struct Analysed {
    pub ast: TypedModule,
    pub src: String,
    pub name: Vec<String>,
    pub path: PathBuf,
    pub origin: ModuleOrigin,
    pub type_info: type_::Module,
    pub source_base_path: PathBuf,
    pub warnings: Vec<Warning>,
    pub module_extra: ModuleExtra,
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
            let docs: Vec<&str> =
                comments_before(&mut doc_comments, statement.location().start, &self.src);
            if !docs.is_empty() {
                let doc = docs.join("\n");
                statement.put_doc(doc);
            }

            if let Statement::CustomType { constructors, .. } = statement {
                for constructor in constructors {
                    let docs: Vec<&str> =
                        comments_before(&mut doc_comments, constructor.location.start, &self.src);
                    if !docs.is_empty() {
                        let doc = docs.join("\n");
                        constructor.put_doc(doc);
                    }

                    for argument in constructor.arguments.iter_mut() {
                        let docs: Vec<&str> =
                            comments_before(&mut doc_comments, argument.location.start, &self.src);
                        if !docs.is_empty() {
                            let doc = docs.join("\n");
                            argument.put_doc(doc);
                        }
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
            let comment = comment_spans
                .next()
                .expect("Comment before accessing next span");
            comments.push(Comment::from((comment, src)).content)
        } else {
            break;
        }
    }
    comments
}

#[derive(Debug, PartialEq, Clone, Copy)]
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

    pub fn to_origin(self) -> Origin {
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
    module: ast::UntypedModule,
    module_extra: ModuleExtra,
}

#[derive(Debug, PartialEq)]
pub struct Input {
    pub source_base_path: PathBuf,
    pub path: PathBuf,
    pub src: String,
    pub origin: ModuleOrigin,
}

pub fn analysed(inputs: Vec<Input>) -> Result<Vec<Analysed>> {
    let module_count = inputs.len();
    let mut source_tree = SourceTree::new(inputs)?;
    let mut modules_type_infos = im::HashMap::new();
    let mut compiled_modules = Vec::with_capacity(module_count);
    let ids = UniqueIdGenerator::new();

    // Insert the prelude
    // DUPE: preludeinsertion
    // TODO: Currently we do this here and also in the tests. It would be better
    // to have one place where we create all this required state for use in each
    // place.
    let _ = modules_type_infos.insert("gleam".to_string(), type_::build_prelude(&ids));

    struct Out {
        source_base_path: PathBuf,
        name_string: String,
        name: Vec<String>,
        path: PathBuf,
        origin: ModuleOrigin,
        ast: TypedModule,
        src: String,
        warnings: Vec<Warning>,
        module_extra: ModuleExtra,
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

        println!("Compiling {}", name_string);

        let mut warnings = vec![];
        let result = type_::infer_module(
            Target::Erlang,
            &ids,
            module,
            origin.to_origin(),
            "old-build-system-doesnt-support-packages",
            &modules_type_infos,
            &mut warnings,
        );
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

        let _ = modules_type_infos.insert(name_string.clone(), ast.type_info.clone());

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
                    .expect("project::compile(): Merging module type info"),
                warnings,
                module_extra,
            }
        })
        .collect())
}
