#![allow(warnings)]

mod build_manifest;
mod dep_tree;
pub mod package_compiler;
mod project_compiler;
mod telemetry;

#[cfg(test)]
mod package_compilation_tests;

pub use self::build_manifest::BuildManifest;
pub use self::package_compiler::PackageCompiler;
pub use self::package_compiler::Source;
pub use self::project_compiler::{Options, ProjectCompiler};
pub use self::telemetry::Telemetry;

use crate::ast::{DefinitionLocation, TypedExpr, TypedStatement};
use crate::{
    ast::{SrcSpan, Statement, TypedModule},
    config::{self, PackageConfig},
    erlang,
    error::{Error, FileIoAction, FileKind},
    io::OutputFile,
    parse::extra::{Comment, ModuleExtra},
    type_,
};
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap, ffi::OsString, fs::DirEntry, iter::Peekable, path::PathBuf, process,
};
use strum::{Display, EnumString, EnumVariantNames, VariantNames};

#[derive(
    Debug, Serialize, Deserialize, Display, EnumString, EnumVariantNames, Clone, Copy, PartialEq,
)]
#[strum(serialize_all = "lowercase")]
pub enum Target {
    #[serde(rename = "erlang")]
    Erlang,
    #[serde(rename = "javascript")]
    JavaScript,
}

impl Target {
    pub fn variant_strings() -> Vec<String> {
        Self::VARIANTS.iter().map(|s| s.to_string()).collect()
    }
}

#[derive(Debug)]
pub enum TargetCodegenConfiguration {
    JavaScript,
    Erlang {
        app_file: Option<ErlangAppCodegenConfiguration>,
    },
}

impl TargetCodegenConfiguration {
    pub fn target(&self) -> Target {
        match self {
            Self::JavaScript => Target::JavaScript,
            Self::Erlang { .. } => Target::Erlang,
        }
    }
}

#[derive(Debug)]
pub struct ErlangAppCodegenConfiguration {
    include_dev_deps: bool,
}

#[derive(
    Debug, Serialize, Deserialize, Display, EnumString, EnumVariantNames, Clone, Copy, PartialEq,
)]
#[strum(serialize_all = "lowercase")]
pub enum Mode {
    Dev,
    Prod,
}

impl Mode {
    /// Returns `true` if the mode is [`Dev`].
    ///
    /// [`Dev`]: Mode::Dev
    pub fn is_dev(&self) -> bool {
        matches!(self, Self::Dev)
    }
}

#[derive(Debug)]
pub struct Package {
    pub config: PackageConfig,
    pub modules: Vec<Module>,
}

impl Package {
    pub fn attach_doc_and_module_comments(&mut self) {
        for mut module in &mut self.modules {
            module.attach_doc_and_module_comments();
        }
    }

    pub fn into_modules_hashmap(self) -> HashMap<String, Module> {
        self.modules
            .into_iter()
            .map(|m| (m.name.to_string(), m))
            .collect()
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub code: String,
    pub input_path: PathBuf,
    pub origin: Origin,
    pub ast: TypedModule,
    pub extra: ModuleExtra,
    pub source_hash: String,
    pub deps: Vec<String>,
}

impl Module {
    pub fn compiled_erlang_path(&self) -> PathBuf {
        let mut path = self.name.replace("/", "@");
        path.push_str(".erl");
        PathBuf::from(path)
    }

    pub fn is_test(&self) -> bool {
        self.origin == Origin::Test
    }

    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.ast.find_node(byte_index)
    }

    pub fn attach_doc_and_module_comments(&mut self) {
        // Module Comments
        self.ast.documentation = self
            .extra
            .module_comments
            .iter()
            .map(|span| {
                Comment::from((span, self.code.as_str()))
                    .content
                    .to_string()
            })
            .collect();

        // Order statements to avoid dissociating doc comments from them
        let mut statements: Vec<_> = self.ast.statements.iter_mut().collect();
        statements.sort_by(|a, b| a.location().start.cmp(&b.location().start));

        // Doc Comments
        let mut doc_comments = self.extra.doc_comments.iter().peekable();
        for statement in &mut statements {
            let docs: Vec<&str> =
                comments_before(&mut doc_comments, statement.location().start, &self.code);
            if !docs.is_empty() {
                let doc = docs.join("\n");
                statement.put_doc(doc);
            }

            if let Statement::CustomType { constructors, .. } = statement {
                for constructor in constructors {
                    let docs: Vec<&str> =
                        comments_before(&mut doc_comments, constructor.location.start, &self.code);
                    if !docs.is_empty() {
                        let doc = docs.join("\n");
                        constructor.put_doc(doc);
                    }

                    for argument in constructor.arguments.iter_mut() {
                        let docs: Vec<&str> =
                            comments_before(&mut doc_comments, argument.location.start, &self.code);
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

#[derive(Debug, Clone, PartialEq)]
pub enum Located<'a> {
    Expression(&'a TypedExpr),
    Statement(&'a TypedStatement),
}

impl<'a> Located<'a> {
    pub fn definition_location(&self) -> Option<DefinitionLocation<'_>> {
        match self {
            Self::Expression(expression) => expression.definition_location(),
            Self::Statement(statement) => Some(DefinitionLocation {
                module: None,
                span: statement.location(),
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Origin {
    Src,
    Test,
}

impl Origin {
    /// Returns `true` if the origin is [`Src`].
    ///
    /// [`Src`]: Origin::Src
    #[must_use]
    pub fn is_src(&self) -> bool {
        matches!(self, Self::Src)
    }
}

fn comments_before<'a>(
    comment_spans: &mut Peekable<impl Iterator<Item = &'a SrcSpan>>,
    byte: u32,
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

pub fn get_source_hash(source_code: &str) -> String {
    //   let mut h0 = SeaHasher::default();
    //   h0.write(&xs);
    //   let h0 = h0.finish();
    blake3::hash(source_code.as_bytes()).to_string()
}
