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
    self, CallArg, CustomType, DefinitionLocation, TypeAst, TypedArg, TypedClauseGuard,
    TypedConstant, TypedCustomType, TypedDefinitions, TypedExpr, TypedFunction, TypedImport,
    TypedModuleConstant, TypedPattern, TypedRecordConstructor, TypedStatement, TypedTypeAlias,
};
use crate::type_::{Type, TypedCallArg};
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
use std::fmt::{Debug, Display};
use std::sync::Arc;
use std::time::SystemTime;
use std::{collections::HashMap, ffi::OsString, fs::DirEntry, iter::Peekable, process};
use strum::{Display, EnumIter, EnumString, EnumVariantNames, VariantNames};
use vec1::Vec1;

#[derive(
    Debug,
    Serialize,
    Deserialize,
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
    pub fn as_presentable_str(&self) -> &str {
        match self {
            Target::Erlang => "Erlang",
            Target::JavaScript => "JavaScript",
        }
    }

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
/// This is used to skip compiling the root package when running the main
/// function coming from a dependency. This way a dependency can be run even
/// there's compilation errors in the root package.
///
pub enum Compile {
    /// The default compiler behaviour, compile all packages.
    ///
    All,
    /// Only compile the dependency packages, skipping the root package.
    ///
    DepsOnly,
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
    Debug, Serialize, Deserialize, EnumString, EnumVariantNames, Clone, Copy, PartialEq, Eq,
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

impl Runtime {
    pub fn as_presentable_str(&self) -> &str {
        match self {
            Runtime::NodeJs => "NodeJS",
            Runtime::Deno => "Deno",
            Runtime::Bun => "Bun",
        }
    }
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
    /// Returns `true` if the mode includes development code.
    ///
    pub fn includes_dev_code(&self) -> bool {
        match self {
            Self::Dev | Self::Lsp => true,
            Self::Prod => false,
        }
    }

    pub fn includes_dev_dependencies(&self) -> bool {
        match self {
            Mode::Dev | Mode::Lsp => true,
            Mode::Prod => false,
        }
    }
}

#[test]
fn mode_includes_dev_code() {
    assert!(Mode::Dev.includes_dev_code());
    assert!(Mode::Lsp.includes_dev_code());
    assert!(!Mode::Prod.includes_dev_code());
}

#[derive(Debug)]
pub struct Package {
    pub config: PackageConfig,
    pub modules: Vec<Module>,
    pub cached_module_names: Vec<EcoString>,
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

#[derive(Debug)]
/// A data structure used to store all definitions in a single homogeneous
/// vector and sort them by their position in order to attach to each the right
/// documentation.
///
enum DocumentableDefinition<'a> {
    Constant(&'a mut TypedModuleConstant),
    TypeAlias(&'a mut TypedTypeAlias),
    CustomType(&'a mut TypedCustomType),
    Function(&'a mut TypedFunction),
    Import(&'a mut TypedImport),
}

impl<'a> DocumentableDefinition<'a> {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Constant(module_constant) => module_constant.location,
            Self::TypeAlias(type_alias) => type_alias.location,
            Self::CustomType(custom_type) => custom_type.location,
            Self::Function(function) => function.location,
            Self::Import(import) => import.location,
        }
    }

    pub fn put_doc(&mut self, new_documentation: (u32, EcoString)) {
        match self {
            Self::Import(_import) => (),
            Self::Function(function) => {
                let _ = function.documentation.replace(new_documentation);
            }
            Self::TypeAlias(type_alias) => {
                let _ = type_alias.documentation.replace(new_documentation);
            }
            Self::CustomType(custom_type) => {
                let _ = custom_type.documentation.replace(new_documentation);
            }
            Self::Constant(constant) => {
                let _ = constant.documentation.replace(new_documentation);
            }
        }
    }
}

impl Module {
    pub fn erlang_name(&self) -> EcoString {
        module_erlang_name(&self.name)
    }

    pub fn compiled_erlang_path(&self) -> Utf8PathBuf {
        let mut path = Utf8PathBuf::from(&module_erlang_name(&self.name));
        assert!(path.set_extension("erl"), "Couldn't set file extension");
        path
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

        self.ast.type_info.documentation = self.ast.documentation.clone();

        // Order definitions to avoid misassociating doc comments after the
        // order has changed during compilation.
        let TypedDefinitions {
            imports,
            constants,
            custom_types,
            type_aliases,
            functions,
        } = &mut self.ast.definitions;

        let mut definitions = ((imports.iter_mut()).map(DocumentableDefinition::Import))
            .chain((constants.iter_mut()).map(DocumentableDefinition::Constant))
            .chain((custom_types.iter_mut()).map(DocumentableDefinition::CustomType))
            .chain((type_aliases.iter_mut()).map(DocumentableDefinition::TypeAlias))
            .chain((functions.iter_mut()).map(DocumentableDefinition::Function))
            .sorted_by_key(|definition| definition.location())
            .collect_vec();

        // Doc Comments
        let mut doc_comments = self.extra.doc_comments.iter().peekable();
        for definition in &mut definitions {
            let (docs_start, docs): (u32, Vec<&str>) = doc_comments_before(
                &mut doc_comments,
                &self.extra,
                definition.location().start,
                &self.code,
            );
            if !docs.is_empty() {
                let doc = docs.join("\n").into();
                definition.put_doc((docs_start, doc));
            }

            if let DocumentableDefinition::CustomType(CustomType { constructors, .. }) = definition
            {
                for constructor in constructors {
                    let (docs_start, docs): (u32, Vec<&str>) = doc_comments_before(
                        &mut doc_comments,
                        &self.extra,
                        constructor.location.start,
                        &self.code,
                    );
                    if !docs.is_empty() {
                        let doc = docs.join("\n").into();
                        constructor.put_doc((docs_start, doc));
                    }

                    for argument in constructor.arguments.iter_mut() {
                        let (docs_start, docs): (u32, Vec<&str>) = doc_comments_before(
                            &mut doc_comments,
                            &self.extra,
                            argument.location.start,
                            &self.code,
                        );
                        if !docs.is_empty() {
                            let doc = docs.join("\n").into();
                            argument.put_doc((docs_start, doc));
                        }
                    }
                }
            }
        }
    }
}

pub fn module_erlang_name(gleam_name: &EcoString) -> EcoString {
    gleam_name.replace("/", "@")
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnqualifiedImport<'a> {
    pub name: &'a EcoString,
    pub module: &'a EcoString,
    pub is_type: bool,
    pub location: &'a SrcSpan,
}

/// The position of a located expression. Used to determine extra context,
/// such as whether to provide label completions if the expression is in
/// argument position.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExpressionPosition<'a> {
    Expression,
    ArgumentOrLabel {
        called_function: &'a TypedExpr,
        function_arguments: &'a [TypedCallArg],
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Located<'a> {
    Pattern(&'a TypedPattern),
    PatternSpread {
        spread_location: SrcSpan,
        pattern: &'a TypedPattern,
    },
    // A prefix alias or a suffix variable defined in a string prefix pattern:
    // "prefix" as alias <> suffix
    StringPrefixPatternVariable {
        location: SrcSpan,
        name: &'a EcoString,
    },
    Statement(&'a TypedStatement),
    Expression {
        expression: &'a TypedExpr,
        position: ExpressionPosition<'a>,
    },
    VariantConstructorDefinition(&'a TypedRecordConstructor),
    FunctionBody(&'a TypedFunction),
    Arg(&'a TypedArg),
    Annotation {
        ast: &'a TypeAst,
        type_: std::sync::Arc<Type>,
    },
    UnqualifiedImport(UnqualifiedImport<'a>),
    Label(SrcSpan, std::sync::Arc<Type>),
    ModuleName {
        location: SrcSpan,
        module_name: EcoString,
        module_alias: EcoString,
        layer: ast::Layer,
    },
    Constant(&'a TypedConstant),

    // A module's top level definitions
    ModuleFunction(&'a TypedFunction),
    ModuleConstant(&'a TypedModuleConstant),
    ModuleImport(&'a TypedImport),
    ModuleCustomType(&'a TypedCustomType),
    ModuleTypeAlias(&'a TypedTypeAlias),
}

impl<'a> Located<'a> {
    // Looks up the type constructor for the given type and then create the location.
    fn type_location(
        &self,
        importable_modules: &'a im::HashMap<EcoString, type_::ModuleInterface>,
        type_: std::sync::Arc<Type>,
    ) -> Option<DefinitionLocation> {
        type_constructor_from_modules(importable_modules, type_).map(|t| DefinitionLocation {
            module: Some(t.module.clone()),
            span: t.origin,
        })
    }

    pub fn definition_location(
        &self,
        importable_modules: &'a im::HashMap<EcoString, type_::ModuleInterface>,
    ) -> Option<DefinitionLocation> {
        match self {
            Self::PatternSpread { .. } => None,
            Self::Pattern(pattern) => pattern.definition_location(),
            Self::StringPrefixPatternVariable { location, .. } => Some(DefinitionLocation {
                module: None,
                span: *location,
            }),
            Self::Statement(statement) => statement.definition_location(),
            Self::FunctionBody(statement) => None,
            Self::Expression { expression, .. } => expression.definition_location(),

            Self::ModuleImport(import) => Some(DefinitionLocation {
                module: Some(import.module.clone()),
                span: SrcSpan { start: 0, end: 0 },
            }),
            Self::ModuleConstant(constant) => Some(DefinitionLocation {
                module: None,
                span: constant.location,
            }),
            Self::ModuleCustomType(custom_type) => Some(DefinitionLocation {
                module: None,
                span: custom_type.location,
            }),
            Self::ModuleFunction(function) => Some(DefinitionLocation {
                module: None,
                span: function.location,
            }),
            Self::ModuleTypeAlias(type_alias) => Some(DefinitionLocation {
                module: None,
                span: type_alias.location,
            }),

            Self::VariantConstructorDefinition(record) => Some(DefinitionLocation {
                module: None,
                span: record.location,
            }),
            Self::UnqualifiedImport(UnqualifiedImport {
                module,
                name,
                is_type,
                ..
            }) => importable_modules.get(*module).and_then(|m| {
                if *is_type {
                    m.types.get(*name).map(|t| DefinitionLocation {
                        module: Some((*module).clone()),
                        span: t.origin,
                    })
                } else {
                    m.values.get(*name).map(|v| DefinitionLocation {
                        module: Some((*module).clone()),
                        span: v.definition_location().span,
                    })
                }
            }),
            Self::Arg(_) => None,
            Self::Annotation { type_, .. } => self.type_location(importable_modules, type_.clone()),
            Self::Label(_, _) => None,
            Self::ModuleName { module_name, .. } => Some(DefinitionLocation {
                module: Some(module_name.clone()),
                span: SrcSpan::new(0, 0),
            }),
            Self::Constant(constant) => constant.definition_location(),
        }
    }

    pub(crate) fn type_(&self) -> Option<Arc<Type>> {
        match self {
            Located::Pattern(pattern) => Some(pattern.type_()),
            Located::StringPrefixPatternVariable { .. } => Some(type_::string()),
            Located::Statement(statement) => Some(statement.type_()),
            Located::Expression { expression, .. } => Some(expression.type_()),
            Located::Arg(arg) => Some(arg.type_.clone()),
            Located::Label(_, type_) | Located::Annotation { type_, .. } => Some(type_.clone()),
            Located::Constant(constant) => Some(constant.type_()),

            Located::PatternSpread { .. }
            | Located::ModuleConstant(_)
            | Located::ModuleCustomType(_)
            | Located::ModuleFunction(_)
            | Located::ModuleImport(_)
            | Located::ModuleTypeAlias(_)
            | Located::VariantConstructorDefinition(_)
            | Located::FunctionBody(_)
            | Located::UnqualifiedImport(_)
            | Located::ModuleName { .. } => None,
        }
    }

    pub fn type_definition_locations(
        &self,
        importable_modules: &im::HashMap<EcoString, type_::ModuleInterface>,
    ) -> Option<Vec<DefinitionLocation>> {
        let type_ = self.type_()?;
        Some(type_to_definition_locations(type_, importable_modules))
    }
}

/// Returns the locations of all the types that one could reach starting from
/// the given type (included). This includes all types that are part of a
/// tuple/function type or that are used as args in a named type.
///
/// For example, given this type `Dict(Int, #(Wibble, Wobble))` all the
/// "reachable" include: `Dict`, `Int`, `Wibble` and `Wobble`.
///
/// This is what powers the "go to type definition" capability of the language
/// server.
///
fn type_to_definition_locations<'a>(
    type_: Arc<Type>,
    importable_modules: &'a im::HashMap<EcoString, type_::ModuleInterface>,
) -> Vec<DefinitionLocation> {
    match type_.as_ref() {
        // For named types we start with the location of the named type itself
        // followed by the locations of all types they reference in their args.
        //
        // For example with a `Dict(Wibble, Wobble)` we'd start with the
        // definition of `Dict`, followed by the definition of `Wibble` and
        // `Wobble`.
        //
        Type::Named {
            module,
            name,
            arguments,
            ..
        } => {
            let Some(module) = importable_modules.get(module) else {
                return vec![];
            };

            let Some(type_) = module.get_public_type(&name) else {
                return vec![];
            };

            let mut locations = vec![DefinitionLocation {
                module: Some(module.name.clone()),
                span: type_.origin,
            }];
            for argument in arguments {
                locations.extend(type_to_definition_locations(
                    argument.clone(),
                    importable_modules,
                ));
            }
            locations
        }

        // For fn types we just get the locations of their arguments and return
        // type.
        //
        Type::Fn { arguments, return_ } => arguments
            .iter()
            .flat_map(|argument| type_to_definition_locations(argument.clone(), importable_modules))
            .chain(type_to_definition_locations(
                return_.clone(),
                importable_modules,
            ))
            .collect_vec(),

        // In case of a var we just follow it and get the locations of the type
        // it points to.
        //
        Type::Var { type_ } => match type_.borrow().clone() {
            type_::TypeVar::Unbound { .. } | type_::TypeVar::Generic { .. } => vec![],
            type_::TypeVar::Link { type_ } => {
                type_to_definition_locations(type_, importable_modules)
            }
        },

        // In case of tuples we get the locations of the wrapped types.
        //
        Type::Tuple { elements } => elements
            .iter()
            .flat_map(|element| type_to_definition_locations(element.clone(), importable_modules))
            .collect_vec(),
    }
}

// Looks up the type constructor for the given type
pub fn type_constructor_from_modules(
    importable_modules: &im::HashMap<EcoString, type_::ModuleInterface>,
    type_: std::sync::Arc<Type>,
) -> Option<&type_::TypeConstructor> {
    let type_ = type_::collapse_links(type_);
    match type_.as_ref() {
        Type::Named { name, module, .. } => importable_modules
            .get(module)
            .and_then(|i| i.types.get(name)),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Origin {
    Src,
    Test,
    Dev,
}

impl Origin {
    /// Returns `true` if the origin is [`Src`].
    ///
    /// [`Src`]: Origin::Src
    #[must_use]
    pub fn is_src(&self) -> bool {
        matches!(self, Self::Src)
    }

    /// Returns `true` if the origin is [`Test`].
    ///
    /// [`Test`]: Origin::Test
    #[must_use]
    pub fn is_test(&self) -> bool {
        matches!(self, Self::Test)
    }

    /// Returns `true` if the origin is [`Dev`].
    ///
    /// [`Dev`]: Origin::Dev
    #[must_use]
    pub fn is_dev(&self) -> bool {
        matches!(self, Self::Dev)
    }

    /// Name of the folder containing the origin.
    #[must_use]
    pub fn folder_name(&self) -> &str {
        match self {
            Origin::Src => "src",
            Origin::Test => "test",
            Origin::Dev => "dev",
        }
    }
}

fn doc_comments_before<'a>(
    doc_comments_spans: &mut Peekable<impl Iterator<Item = &'a SrcSpan>>,
    extra: &ModuleExtra,
    byte: u32,
    src: &'a str,
) -> (u32, Vec<&'a str>) {
    let mut comments = vec![];
    let mut comment_start = u32::MAX;
    while let Some(SrcSpan { start, end }) = doc_comments_spans.peek() {
        if start > &byte {
            break;
        }
        if extra.has_comment_between(*end, byte) {
            // We ignore doc comments that come before a regular comment.
            _ = doc_comments_spans.next();
            continue;
        }
        let comment = doc_comments_spans
            .next()
            .expect("Comment before accessing next span");

        if comment.start < comment_start {
            comment_start = comment.start;
        }

        comments.push(Comment::from((comment, src)).content)
    }
    (comment_start, comments)
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct SourceFingerprint(u64);

impl SourceFingerprint {
    pub fn new(source: &str) -> Self {
        SourceFingerprint(xxhash_rust::xxh3::xxh3_64(source.as_bytes()))
    }

    pub fn to_numerical_string(&self) -> String {
        self.0.to_string()
    }
}

/// Like a `Result`, but the operation can partially succeed or fail.
///
#[derive(Debug)]
pub enum Outcome<T, E> {
    /// The operation was totally succesful.
    Ok(T),

    /// The operation was partially successful but there were problems.
    PartialFailure(T, E),

    /// The operation was entirely unsuccessful.
    TotalFailure(E),
}

impl<T, E> Outcome<T, E>
where
    E: Debug,
{
    #[cfg(test)]
    /// Panic if there's any errors
    pub fn unwrap(self) -> T {
        match self {
            Outcome::Ok(t) => t,
            Outcome::PartialFailure(_, errors) => panic!("Error: {:?}", errors),
            Outcome::TotalFailure(error) => panic!("Error: {:?}", error),
        }
    }

    /// Panic if there's any errors
    pub fn expect(self, e: &'static str) -> T {
        match self {
            Outcome::Ok(t) => t,
            Outcome::PartialFailure(_, errors) => panic!("{e}: {:?}", errors),
            Outcome::TotalFailure(error) => panic!("{e}: {:?}", error),
        }
    }

    pub fn into_result(self) -> Result<T, E> {
        match self {
            Outcome::Ok(t) => Ok(t),
            Outcome::PartialFailure(_, e) | Outcome::TotalFailure(e) => Err(e),
        }
    }

    pub fn map<T2>(self, f: impl FnOnce(T) -> T2) -> Outcome<T2, E> {
        match self {
            Outcome::Ok(t) => Outcome::Ok(f(t)),
            Outcome::PartialFailure(t, e) => Outcome::PartialFailure(f(t), e),
            Outcome::TotalFailure(e) => Outcome::TotalFailure(e),
        }
    }
}
