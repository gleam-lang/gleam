#![allow(warnings)]

mod elixir_libraries;
mod module_loader;
mod native_file_copier;
pub mod package_compiler;
mod package_loader;
mod project_compiler;
mod telemetry;

#[cfg(test)]
mod tests;

pub use self::package_compiler::PackageCompiler;
pub use self::package_loader::StaleTracker;
pub use self::project_compiler::{Built, Options, ProjectCompiler};
pub use self::telemetry::{NullTelemetry, Telemetry};

use crate::ast::{
    CustomType, DefinitionLocation, TypeAst, TypedArg, TypedDefinition, TypedExpr, TypedFunction,
    TypedPattern, TypedStatement,
};
use crate::{
    ast::{Definition, SrcSpan, TypedModule},
    config::{self, PackageConfig},
    erlang,
    error::{Error, FileIoAction, FileKind},
    io::OutputFile,
    parse::extra::{Comment, ModuleExtra},
    type_,
};
use camino::Utf8PathBuf;
use ecow::EcoString;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::time::SystemTime;
use std::{collections::HashMap, ffi::OsString, fs::DirEntry, iter::Peekable, process};
use strum::{Display, EnumIter, EnumString, EnumVariantNames, VariantNames};

#[derive(
    Debug,
    Serialize,
    Deserialize,
    Display,
    EnumString,
    EnumVariantNames,
    EnumIter,
    Clone,
    Copy,
    PartialEq,
    Eq,
)]
#[strum(serialize_all = "lowercase")]
pub enum Target {
    #[strum(serialize = "erlang", serialize = "erl")]
    #[serde(rename = "erlang", alias = "erl")]
    Erlang,
    #[strum(serialize = "javascript", serialize = "js")]
    #[serde(rename = "javascript", alias = "js")]
    JavaScript,
}

impl Target {
    pub fn variant_strings() -> Vec<EcoString> {
        Self::VARIANTS.iter().map(|s| (*s).into()).collect()
    }

    /// Returns `true` if the target is [`JavaScript`].
    ///
    /// [`JavaScript`]: Target::JavaScript
    #[must_use]
    pub fn is_javascript(&self) -> bool {
        matches!(self, Self::JavaScript)
    }

    /// Returns `true` if the target is [`Erlang`].
    ///
    /// [`Erlang`]: Target::Erlang
    #[must_use]
    pub fn is_erlang(&self) -> bool {
        matches!(self, Self::Erlang)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Codegen {
    All,
    DepsOnly,
    None,
}

impl Codegen {
    fn should_codegen(&self, is_root_package: bool) -> bool {
        match self {
            Codegen::All => true,
            Codegen::DepsOnly => !is_root_package,
            Codegen::None => false,
        }
    }
}

#[derive(
    Debug, Serialize, Deserialize, Display, EnumString, EnumVariantNames, Clone, Copy, PartialEq, Eq,
)]
pub enum Runtime {
    #[strum(serialize = "nodejs", serialize = "node")]
    #[serde(rename = "nodejs", alias = "node")]
    NodeJs,
    #[strum(serialize = "deno")]
    #[serde(rename = "deno")]
    Deno,
    #[strum(serialize = "bun")]
    #[serde(rename = "bun")]
    Bun,
}

impl Default for Runtime {
    fn default() -> Self {
        Self::NodeJs
    }
}

#[derive(Debug)]
pub enum TargetCodegenConfiguration {
    JavaScript {
        emit_typescript_definitions: bool,
        prelude_location: Utf8PathBuf,
    },
    Erlang {
        app_file: Option<ErlangAppCodegenConfiguration>,
    },
}

impl TargetCodegenConfiguration {
    pub fn target(&self) -> Target {
        match self {
            Self::JavaScript { .. } => Target::JavaScript,
            Self::Erlang { .. } => Target::Erlang,
        }
    }
}

#[derive(Debug)]
pub struct ErlangAppCodegenConfiguration {
    pub include_dev_deps: bool,
    /// Some packages have a different OTP application name than their package
    /// name, as rebar3 (and Mix?) support this. The .app file must use the OTP
    /// name, not the package name.
    pub package_name_overrides: HashMap<EcoString, EcoString>,
}

#[derive(
    Debug,
    Serialize,
    Deserialize,
    Display,
    EnumString,
    EnumVariantNames,
    EnumIter,
    Clone,
    Copy,
    PartialEq,
)]
#[strum(serialize_all = "lowercase")]
pub enum Mode {
    Dev,
    Prod,
    Lsp,
}

impl Mode {
    /// Returns `true` if the mode includes test code.
    ///
    pub fn includes_tests(&self) -> bool {
        match self {
            Self::Dev | Self::Lsp => true,
            Self::Prod => false,
        }
    }
}

#[test]
fn mode_includes_tests() {
    assert!(Mode::Dev.includes_tests());
    assert!(Mode::Lsp.includes_tests());
    assert!(!Mode::Prod.includes_tests());
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
    pub name: EcoString,
    pub code: EcoString,
    pub mtime: SystemTime,
    pub input_path: Utf8PathBuf,
    pub origin: Origin,
    pub ast: TypedModule,
    pub extra: ModuleExtra,
    pub dependencies: Vec<(EcoString, SrcSpan)>,
}

impl Module {
    pub fn compiled_erlang_path(&self) -> Utf8PathBuf {
        let mut path = self.name.replace("/", "@");
        path.push_str(".erl");
        Utf8PathBuf::from(path.as_ref())
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
            .map(|span| Comment::from((span, self.code.as_str())).content.into())
            .collect();

        // Order statements to avoid missociating doc comments after the order
        // has changed during compilation.
        let mut statements: Vec<_> = self.ast.definitions.iter_mut().collect();
        statements.sort_by(|a, b| a.location().start.cmp(&b.location().start));

        // Doc Comments
        let mut doc_comments = self.extra.doc_comments.iter().peekable();
        for statement in &mut statements {
            let docs: Vec<&str> =
                comments_before(&mut doc_comments, statement.location().start, &self.code);
            if !docs.is_empty() {
                let doc = docs.join("\n").into();
                statement.put_doc(doc);
            }

            if let Definition::CustomType(CustomType { constructors, .. }) = statement {
                for constructor in constructors {
                    let docs: Vec<&str> =
                        comments_before(&mut doc_comments, constructor.location.start, &self.code);
                    if !docs.is_empty() {
                        let doc = docs.join("\n").into();
                        constructor.put_doc(doc);
                    }

                    for argument in constructor.arguments.iter_mut() {
                        let docs: Vec<&str> =
                            comments_before(&mut doc_comments, argument.location.start, &self.code);
                        if !docs.is_empty() {
                            let doc = docs.join("\n").into();
                            argument.put_doc(doc);
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn dependencies_list(&self) -> Vec<EcoString> {
        self.dependencies
            .iter()
            .map(|(name, _)| name.clone())
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnqualifiedImport<'a> {
    pub name: &'a EcoString,
    pub module: &'a EcoString,
    pub is_type: bool,
    pub location: &'a SrcSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Located<'a> {
    Pattern(&'a TypedPattern),
    Statement(&'a TypedStatement),
    Expression(&'a TypedExpr),
    ModuleStatement(&'a TypedDefinition),
    FunctionBody(&'a TypedFunction),
    Arg(&'a TypedArg),
    Annotation(SrcSpan, std::sync::Arc<type_::Type>),
    UnqualifiedImport(UnqualifiedImport<'a>),
}

impl<'a> Located<'a> {
    // Looks up the type constructor for the given type and then create the location.
    fn type_location(
        &self,
        importable_modules: &'a im::HashMap<EcoString, type_::ModuleInterface>,
        type_: std::sync::Arc<type_::Type>,
    ) -> Option<DefinitionLocation<'_>> {
        type_constructor_from_modules(importable_modules, type_).map(|t| DefinitionLocation {
            module: Some(&t.module),
            span: t.origin,
        })
    }

    pub fn definition_location(
        &self,
        importable_modules: &'a im::HashMap<EcoString, type_::ModuleInterface>,
    ) -> Option<DefinitionLocation<'_>> {
        match self {
            Self::Pattern(pattern) => pattern.definition_location(),
            Self::Statement(statement) => statement.definition_location(),
            Self::FunctionBody(statement) => None,
            Self::Expression(expression) => expression.definition_location(),
            Self::ModuleStatement(Definition::Import(import)) => Some(DefinitionLocation {
                module: Some(import.module.as_str()),
                span: SrcSpan { start: 0, end: 0 },
            }),
            Self::ModuleStatement(statement) => Some(DefinitionLocation {
                module: None,
                span: statement.location(),
            }),
            Self::UnqualifiedImport(UnqualifiedImport {
                module,
                name,
                is_type,
                ..
            }) => importable_modules.get(*module).and_then(|m| {
                if *is_type {
                    m.types.get(*name).map(|t| DefinitionLocation {
                        module: Some(&module),
                        span: t.origin,
                    })
                } else {
                    m.values.get(*name).map(|v| DefinitionLocation {
                        module: Some(&module),
                        span: v.definition_location().span,
                    })
                }
            }),
            Self::Arg(_) => None,
            Self::Annotation(_, type_) => self.type_location(importable_modules, type_.clone()),
        }
    }
}

// Looks up the type constructor for the given type
pub fn type_constructor_from_modules(
    importable_modules: &im::HashMap<EcoString, type_::ModuleInterface>,
    type_: std::sync::Arc<type_::Type>,
) -> Option<&type_::TypeConstructor> {
    let type_ = type_::collapse_links(type_);
    match type_.as_ref() {
        type_::Type::Named { name, module, .. } => importable_modules
            .get(module)
            .and_then(|i| i.types.get(name)),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub(crate) struct SourceFingerprint(u64);

impl SourceFingerprint {
    pub(crate) fn new(source: &str) -> Self {
        SourceFingerprint(xxhash_rust::xxh3::xxh3_64(source.as_bytes()))
    }
}
