mod constant;
mod typed;
mod untyped;

#[cfg(test)]
mod tests;
pub mod visit;

pub use self::typed::{InvalidExpression, TypedExpr};
pub use self::untyped::{FunctionLiteralKind, UntypedExpr};

pub use self::constant::{Constant, TypedConstant, UntypedConstant};

use crate::analyse::Inferred;
use crate::ast::typed::pairwise_all;
use crate::bit_array;
use crate::build::{ExpressionPosition, Located, Target, module_erlang_name};
use crate::exhaustiveness::CompiledCase;
use crate::parse::{LiteralFloatValue, SpannedString};
use crate::type_::error::VariableOrigin;
use crate::type_::expression::{Implementations, Purity};
use crate::type_::printer::Names;
use crate::type_::{
    self, Deprecation, HasType, ModuleValueConstructor, PatternConstructor, Type, TypedCallArg,
    ValueConstructor, ValueConstructorVariant, nil,
};
use itertools::Itertools;
use num_traits::Zero;
use std::collections::HashSet;
use std::sync::Arc;

use ecow::EcoString;
use num_bigint::{BigInt, Sign};
use num_traits::{One, ToPrimitive};
#[cfg(test)]
use pretty_assertions::assert_eq;
use vec1::Vec1;

pub const PIPE_VARIABLE: &str = "_pipe";
pub const USE_ASSIGNMENT_VARIABLE: &str = "_use";
pub const RECORD_UPDATE_VARIABLE: &str = "_record";
pub const ASSERT_FAIL_VARIABLE: &str = "_assert_fail";
pub const ASSERT_SUBJECT_VARIABLE: &str = "_assert_subject";
pub const CAPTURE_VARIABLE: &str = "_capture";
pub const BLOCK_VARIABLE: &str = "_block";

pub trait HasLocation {
    fn location(&self) -> SrcSpan;
}

pub type UntypedModule = Module<(), Vec<TargetedDefinition>>;
pub type TypedModule = Module<type_::ModuleInterface, TypedDefinitions>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module<Info, Definitions> {
    pub name: EcoString,
    pub documentation: Vec<EcoString>,
    pub type_info: Info,
    pub definitions: Definitions,
    pub names: Names,
    /// The source byte locations of definition that are unused.
    /// This is used in code generation to know when definitions can be safely omitted.
    pub unused_definition_positions: HashSet<u32>,
}

impl<Info, Definitions> Module<Info, Definitions> {
    pub fn erlang_name(&self) -> EcoString {
        module_erlang_name(&self.name)
    }
}

impl TypedModule {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        let TypedDefinitions {
            imports,
            constants,
            custom_types,
            type_aliases,
            functions,
        } = &self.definitions;

        imports
            .iter()
            .find_map(|import| import.find_node(byte_index))
            .or_else(|| (constants.iter()).find_map(|constant| constant.find_node(byte_index)))
            .or_else(|| (custom_types.iter()).find_map(|type_| type_.find_node(byte_index)))
            .or_else(|| (type_aliases.iter()).find_map(|alias| alias.find_node(byte_index)))
            .or_else(|| (functions.iter()).find_map(|function| function.find_node(byte_index)))
    }

    pub fn find_statement(&self, byte_index: u32) -> Option<&TypedStatement> {
        // Statements can only be found inside a module function, there's no
        // need to go over all the other module definitions.
        self.definitions
            .functions
            .iter()
            .find_map(|function| function.find_statement(byte_index))
    }

    pub fn definitions_len(&self) -> usize {
        let TypedDefinitions {
            imports,
            constants,
            custom_types,
            type_aliases,
            functions,
        } = &self.definitions;

        imports.len() + constants.len() + custom_types.len() + type_aliases.len() + functions.len()
    }
}

#[derive(Debug)]
pub struct TypedDefinitions {
    pub imports: Vec<TypedImport>,
    pub constants: Vec<TypedModuleConstant>,
    pub custom_types: Vec<TypedCustomType>,
    pub type_aliases: Vec<TypedTypeAlias>,
    pub functions: Vec<TypedFunction>,
}

/// The `@target(erlang)` and `@target(javascript)` attributes can be used to
/// mark a definition as only being for a specific target.
///
/// ```gleam
/// const x: Int = 1
///
/// @target(erlang)
/// pub fn main(a) { ...}
/// ```
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetedDefinition {
    pub definition: UntypedDefinition,
    pub target: Option<Target>,
}

impl TargetedDefinition {
    pub fn is_for(&self, target: Target) -> bool {
        self.target.map(|t| t == target).unwrap_or(true)
    }
}

impl UntypedModule {
    pub fn dependencies(&self, target: Target) -> Vec<(EcoString, SrcSpan)> {
        self.iter_definitions(target)
            .flat_map(|definition| match definition {
                Definition::Import(Import {
                    module, location, ..
                }) => Some((module.clone(), *location)),
                _ => None,
            })
            .collect()
    }

    pub fn iter_definitions(&self, target: Target) -> impl Iterator<Item = &UntypedDefinition> {
        self.definitions
            .iter()
            .filter(move |definition| definition.is_for(target))
            .map(|definition| &definition.definition)
    }

    pub fn into_iter_definitions(self, target: Target) -> impl Iterator<Item = UntypedDefinition> {
        self.definitions
            .into_iter()
            .filter(move |definition| definition.is_for(target))
            .map(|definition| definition.definition)
    }
}

#[test]
fn module_dependencies_test() {
    let parsed = crate::parse::parse_module(
        camino::Utf8PathBuf::from("test/path"),
        "import one
         @target(erlang)
         import two

         @target(javascript)
         import three

         import four",
        &crate::warning::WarningEmitter::null(),
    )
    .expect("syntax error");
    let module = parsed.module;

    assert_eq!(
        vec![
            ("one".into(), SrcSpan::new(0, 10)),
            ("two".into(), SrcSpan::new(45, 55)),
            ("four".into(), SrcSpan::new(118, 129)),
        ],
        module.dependencies(Target::Erlang)
    );
}

pub type TypedArg = Arg<Arc<Type>>;
pub type UntypedArg = Arg<()>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Arg<T> {
    pub names: ArgNames,
    pub location: SrcSpan,
    pub annotation: Option<TypeAst>,
    pub type_: T,
}

impl<A> Arg<A> {
    pub fn set_type<B>(self, t: B) -> Arg<B> {
        Arg {
            type_: t,
            names: self.names,
            location: self.location,
            annotation: self.annotation,
        }
    }

    pub fn get_variable_name(&self) -> Option<&EcoString> {
        self.names.get_variable_name()
    }

    pub fn is_capture_hole(&self) -> bool {
        match &self.names {
            ArgNames::Named { name, .. } if name == CAPTURE_VARIABLE => true,
            _ => false,
        }
    }
}

impl TypedArg {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        if self.location.contains(byte_index) {
            if let Some(annotation) = &self.annotation {
                return annotation
                    .find_node(byte_index, self.type_.clone())
                    .or(Some(Located::Arg(self)));
            }
            Some(Located::Arg(self))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgNames {
    Discard {
        name: EcoString,
        location: SrcSpan,
    },
    LabelledDiscard {
        label: EcoString,
        label_location: SrcSpan,
        name: EcoString,
        name_location: SrcSpan,
    },
    Named {
        name: EcoString,
        location: SrcSpan,
    },
    NamedLabelled {
        label: EcoString,
        label_location: SrcSpan,
        name: EcoString,
        name_location: SrcSpan,
    },
}

impl ArgNames {
    pub fn get_label(&self) -> Option<&EcoString> {
        match self {
            ArgNames::Discard { .. } | ArgNames::Named { .. } => None,
            ArgNames::LabelledDiscard { label, .. } | ArgNames::NamedLabelled { label, .. } => {
                Some(label)
            }
        }
    }
    pub fn get_variable_name(&self) -> Option<&EcoString> {
        match self {
            ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => None,
            ArgNames::NamedLabelled { name, .. } | ArgNames::Named { name, .. } => Some(name),
        }
    }
}

pub type TypedRecordConstructor = RecordConstructor<Arc<Type>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordConstructor<T> {
    pub location: SrcSpan,
    pub name_location: SrcSpan,
    pub name: EcoString,
    pub arguments: Vec<RecordConstructorArg<T>>,
    pub documentation: Option<(u32, EcoString)>,
    pub deprecation: Deprecation,
}

impl<A> RecordConstructor<A> {
    pub fn put_doc(&mut self, new_doc: (u32, EcoString)) {
        self.documentation = Some(new_doc);
    }
}

pub type TypedRecordConstructorArg = RecordConstructorArg<Arc<Type>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordConstructorArg<T> {
    pub label: Option<SpannedString>,
    pub ast: TypeAst,
    pub location: SrcSpan,
    pub type_: T,
    pub doc: Option<(u32, EcoString)>,
}

impl<T: PartialEq> RecordConstructorArg<T> {
    pub fn put_doc(&mut self, new_doc: (u32, EcoString)) {
        self.doc = Some(new_doc);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAstConstructor {
    pub location: SrcSpan,
    pub name_location: SrcSpan,
    pub module: Option<(EcoString, SrcSpan)>,
    pub name: EcoString,
    pub arguments: Vec<TypeAst>,
    pub start_parentheses: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAstFn {
    pub location: SrcSpan,
    pub arguments: Vec<TypeAst>,
    pub return_: Box<TypeAst>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAstVar {
    pub location: SrcSpan,
    pub name: EcoString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAstTuple {
    pub location: SrcSpan,
    pub elements: Vec<TypeAst>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAstHole {
    pub location: SrcSpan,
    pub name: EcoString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAst {
    Constructor(TypeAstConstructor),
    Fn(TypeAstFn),
    Var(TypeAstVar),
    Tuple(TypeAstTuple),
    Hole(TypeAstHole),
}

impl TypeAst {
    pub fn location(&self) -> SrcSpan {
        match self {
            TypeAst::Fn(TypeAstFn { location, .. })
            | TypeAst::Var(TypeAstVar { location, .. })
            | TypeAst::Hole(TypeAstHole { location, .. })
            | TypeAst::Tuple(TypeAstTuple { location, .. })
            | TypeAst::Constructor(TypeAstConstructor { location, .. }) => *location,
        }
    }

    pub fn is_logically_equal(&self, other: &TypeAst) -> bool {
        match self {
            TypeAst::Constructor(TypeAstConstructor {
                module,
                name,
                arguments,
                location: _,
                name_location: _,
                start_parentheses: _,
            }) => match other {
                TypeAst::Constructor(TypeAstConstructor {
                    module: o_module,
                    name: o_name,
                    arguments: o_arguments,
                    location: _,
                    name_location: _,
                    start_parentheses: _,
                }) => {
                    let module_name =
                        |m: &Option<(EcoString, _)>| m.as_ref().map(|(m, _)| m.clone());
                    module_name(module) == module_name(o_module)
                        && name == o_name
                        && arguments.len() == o_arguments.len()
                        && arguments
                            .iter()
                            .zip(o_arguments)
                            .all(|a| a.0.is_logically_equal(a.1))
                }
                _ => false,
            },
            TypeAst::Fn(TypeAstFn {
                arguments,
                return_,
                location: _,
            }) => match other {
                TypeAst::Fn(TypeAstFn {
                    arguments: o_arguments,
                    return_: o_return_,
                    location: _,
                }) => {
                    arguments.len() == o_arguments.len()
                        && arguments
                            .iter()
                            .zip(o_arguments)
                            .all(|a| a.0.is_logically_equal(a.1))
                        && return_.is_logically_equal(o_return_)
                }
                _ => false,
            },
            TypeAst::Var(TypeAstVar { name, location: _ }) => match other {
                TypeAst::Var(TypeAstVar {
                    name: o_name,
                    location: _,
                }) => name == o_name,
                _ => false,
            },
            TypeAst::Tuple(TypeAstTuple {
                elements,
                location: _,
            }) => match other {
                TypeAst::Tuple(TypeAstTuple {
                    elements: other_elements,
                    location: _,
                }) => {
                    elements.len() == other_elements.len()
                        && elements
                            .iter()
                            .zip(other_elements)
                            .all(|a| a.0.is_logically_equal(a.1))
                }
                _ => false,
            },
            TypeAst::Hole(TypeAstHole { name, location: _ }) => match other {
                TypeAst::Hole(TypeAstHole {
                    name: o_name,
                    location: _,
                }) => name == o_name,
                _ => false,
            },
        }
    }

    pub fn find_node(&self, byte_index: u32, type_: Arc<Type>) -> Option<Located<'_>> {
        if !self.location().contains(byte_index) {
            return None;
        }

        match self {
            TypeAst::Fn(TypeAstFn {
                arguments, return_, ..
            }) => type_
                .fn_types()
                .and_then(|(arg_types, ret_type)| {
                    if let Some(arg) = arguments
                        .iter()
                        .zip(arg_types)
                        .find_map(|(arg, arg_type)| arg.find_node(byte_index, arg_type.clone()))
                    {
                        return Some(arg);
                    }
                    if let Some(ret) = return_.find_node(byte_index, ret_type) {
                        return Some(ret);
                    }

                    None
                })
                .or(Some(Located::Annotation { ast: self, type_ })),
            TypeAst::Constructor(TypeAstConstructor {
                arguments, module, ..
            }) => type_
                .constructor_types()
                .and_then(|arg_types| {
                    if let Some(arg) = arguments
                        .iter()
                        .zip(arg_types)
                        .find_map(|(arg, arg_type)| arg.find_node(byte_index, arg_type.clone()))
                    {
                        return Some(arg);
                    }

                    None
                })
                .or(module.as_ref().and_then(|(name, location)| {
                    if location.contains(byte_index) {
                        Some(Located::ModuleName {
                            location: *location,
                            name,
                            layer: Layer::Type,
                        })
                    } else {
                        None
                    }
                }))
                .or(Some(Located::Annotation { ast: self, type_ })),
            TypeAst::Tuple(TypeAstTuple { elements, .. }) => type_
                .tuple_types()
                .and_then(|elem_types| {
                    if let Some(e) = elements
                        .iter()
                        .zip(elem_types)
                        .find_map(|(e, e_type)| e.find_node(byte_index, e_type.clone()))
                    {
                        return Some(e);
                    }

                    None
                })
                .or(Some(Located::Annotation { ast: self, type_ })),
            TypeAst::Var(_) | TypeAst::Hole(_) => Some(Located::Annotation { ast: self, type_ }),
        }
    }

    /// Generates an annotation corresponding to the type.
    pub fn print(&self, buffer: &mut EcoString) {
        match &self {
            TypeAst::Var(var) => buffer.push_str(&var.name),
            TypeAst::Hole(hole) => buffer.push_str(&hole.name),
            TypeAst::Tuple(tuple) => {
                buffer.push_str("#(");
                for (i, element) in tuple.elements.iter().enumerate() {
                    element.print(buffer);
                    if i < tuple.elements.len() - 1 {
                        buffer.push_str(", ");
                    }
                }
                buffer.push(')')
            }
            TypeAst::Fn(func) => {
                buffer.push_str("fn(");
                for (i, argument) in func.arguments.iter().enumerate() {
                    argument.print(buffer);
                    if i < func.arguments.len() - 1 {
                        buffer.push_str(", ");
                    }
                }
                buffer.push(')');
                buffer.push_str(" -> ");
                func.return_.print(buffer);
            }
            TypeAst::Constructor(constructor) => {
                if let Some((module, _)) = &constructor.module {
                    buffer.push_str(module);
                    buffer.push('.');
                }
                buffer.push_str(&constructor.name);
                if !constructor.arguments.is_empty() {
                    buffer.push('(');
                    for (i, argument) in constructor.arguments.iter().enumerate() {
                        argument.print(buffer);
                        if i < constructor.arguments.len() - 1 {
                            buffer.push_str(", ");
                        }
                    }
                    buffer.push(')');
                }
            }
        }
    }
}

#[test]
fn type_ast_print_fn() {
    let mut buffer = EcoString::new();
    let ast = TypeAst::Fn(TypeAstFn {
        location: SrcSpan { start: 1, end: 1 },
        arguments: vec![
            TypeAst::Var(TypeAstVar {
                location: SrcSpan { start: 1, end: 1 },
                name: "String".into(),
            }),
            TypeAst::Var(TypeAstVar {
                location: SrcSpan { start: 1, end: 1 },
                name: "Bool".into(),
            }),
        ],
        return_: Box::new(TypeAst::Var(TypeAstVar {
            location: SrcSpan { start: 1, end: 1 },
            name: "Int".into(),
        })),
    });
    ast.print(&mut buffer);
    assert_eq!(&buffer, "fn(String, Bool) -> Int")
}

#[test]
fn type_ast_print_constructor() {
    let mut buffer = EcoString::new();
    let ast = TypeAst::Constructor(TypeAstConstructor {
        name: "SomeType".into(),
        module: Some(("some_module".into(), SrcSpan { start: 1, end: 1 })),
        location: SrcSpan { start: 1, end: 1 },
        name_location: SrcSpan { start: 1, end: 1 },
        arguments: vec![
            TypeAst::Var(TypeAstVar {
                location: SrcSpan { start: 1, end: 1 },
                name: "String".into(),
            }),
            TypeAst::Var(TypeAstVar {
                location: SrcSpan { start: 1, end: 1 },
                name: "Bool".into(),
            }),
        ],
        start_parentheses: Some(1),
    });
    ast.print(&mut buffer);
    assert_eq!(&buffer, "some_module.SomeType(String, Bool)")
}

#[test]
fn type_ast_print_tuple() {
    let mut buffer = EcoString::new();
    let ast = TypeAst::Tuple(TypeAstTuple {
        location: SrcSpan { start: 1, end: 1 },
        elements: vec![
            TypeAst::Constructor(TypeAstConstructor {
                name: "SomeType".into(),
                module: Some(("some_module".into(), SrcSpan { start: 1, end: 1 })),
                location: SrcSpan { start: 1, end: 1 },
                name_location: SrcSpan { start: 1, end: 1 },
                arguments: vec![
                    TypeAst::Var(TypeAstVar {
                        location: SrcSpan { start: 1, end: 1 },
                        name: "String".into(),
                    }),
                    TypeAst::Var(TypeAstVar {
                        location: SrcSpan { start: 1, end: 1 },
                        name: "Bool".into(),
                    }),
                ],
                start_parentheses: Some(1),
            }),
            TypeAst::Fn(TypeAstFn {
                location: SrcSpan { start: 1, end: 1 },
                arguments: vec![
                    TypeAst::Var(TypeAstVar {
                        location: SrcSpan { start: 1, end: 1 },
                        name: "String".into(),
                    }),
                    TypeAst::Var(TypeAstVar {
                        location: SrcSpan { start: 1, end: 1 },
                        name: "Bool".into(),
                    }),
                ],
                return_: Box::new(TypeAst::Var(TypeAstVar {
                    location: SrcSpan { start: 1, end: 1 },
                    name: "Int".into(),
                })),
            }),
        ],
    });
    ast.print(&mut buffer);
    assert_eq!(
        &buffer,
        "#(some_module.SomeType(String, Bool), fn(String, Bool) -> Int)"
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Publicity {
    Public,
    Private,
    Internal { attribute_location: Option<SrcSpan> },
}

impl Publicity {
    pub fn is_private(&self) -> bool {
        match self {
            Self::Private => true,
            Self::Public | Self::Internal { .. } => false,
        }
    }

    pub fn is_internal(&self) -> bool {
        match self {
            Self::Internal { .. } => true,
            Self::Public | Self::Private => false,
        }
    }

    pub fn is_public(&self) -> bool {
        match self {
            Self::Public => true,
            Self::Internal { .. } | Self::Private => false,
        }
    }

    pub fn is_importable(&self) -> bool {
        match self {
            Self::Internal { .. } | Self::Public => true,
            Self::Private => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A function definition
///
/// Note that an anonymous function will have `None` as the name field, while a
/// named function will have `Some`.
///
/// # Example(s)
///
/// ```gleam
/// // Public function
/// pub fn wobble() -> String { ... }
/// // Private function
/// fn wibble(x: Int) -> Int { ... }
/// // Anonymous function
/// fn(x: Int) { ... }
/// ```
pub struct Function<T, Expr> {
    pub location: SrcSpan,
    pub body_start: Option<u32>,
    pub end_position: u32,
    pub name: Option<SpannedString>,
    pub arguments: Vec<Arg<T>>,
    pub body: Vec<Statement<T, Expr>>,
    pub publicity: Publicity,
    pub deprecation: Deprecation,
    pub return_annotation: Option<TypeAst>,
    pub return_type: T,
    pub documentation: Option<(u32, EcoString)>,
    pub external_erlang: Option<(EcoString, EcoString, SrcSpan)>,
    pub external_javascript: Option<(EcoString, EcoString, SrcSpan)>,
    pub implementations: Implementations,
    pub purity: Purity,
}

pub type TypedFunction = Function<Arc<Type>, TypedExpr>;
pub type UntypedFunction = Function<(), UntypedExpr>;

impl<T, E> Function<T, E> {
    pub fn full_location(&self) -> SrcSpan {
        SrcSpan::new(self.location.start, self.end_position)
    }
}

impl TypedFunction {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        // Search for the corresponding node inside the function
        // only if the index falls within the function's full location.
        if !self.full_location().contains(byte_index) {
            return None;
        }

        if let Some(found) = self
            .body
            .iter()
            .find_map(|statement| statement.find_node(byte_index))
        {
            return Some(found);
        }

        if let Some(found_arg) = self
            .arguments
            .iter()
            .find_map(|arg| arg.find_node(byte_index))
        {
            return Some(found_arg);
        };

        if let Some(found_statement) = self
            .body
            .iter()
            .find(|statement| statement.location().contains(byte_index))
        {
            return Some(Located::Statement(found_statement));
        };

        // Check if location is within the return annotation.
        if let Some(located) = self
            .return_annotation
            .iter()
            .find_map(|annotation| annotation.find_node(byte_index, self.return_type.clone()))
        {
            return Some(located);
        };

        // Note that the fn `.location` covers the function head, not
        // the entire statement.
        if self.location.contains(byte_index) {
            Some(Located::ModuleFunction(self))
        } else if self.full_location().contains(byte_index) {
            Some(Located::FunctionBody(self))
        } else {
            None
        }
    }

    pub fn find_statement(&self, byte_index: u32) -> Option<&TypedStatement> {
        if !self.full_location().contains(byte_index) {
            return None;
        }

        self.body
            .iter()
            .find_map(|statement| statement.find_statement(byte_index))
    }

    pub fn main_function(&self) -> Option<&TypedFunction> {
        if let Some((_, name)) = &self.name
            && name == "main"
        {
            Some(self)
        } else {
            None
        }
    }
}

pub type UntypedImport = Import<()>;
pub type TypedImport = Import<EcoString>;

#[derive(Debug, Clone, PartialEq, Eq)]
/// Import another Gleam module so the current module can use the types and
/// values it defines.
///
/// # Example(s)
///
/// ```gleam
/// import unix/cat
/// // Import with alias
/// import animal/cat as kitty
/// ```
pub struct Import<PackageName> {
    pub documentation: Option<EcoString>,
    pub location: SrcSpan,
    pub module: EcoString,
    pub as_name: Option<(AssignName, SrcSpan)>,
    pub unqualified_values: Vec<UnqualifiedImport>,
    pub unqualified_types: Vec<UnqualifiedImport>,
    pub package: PackageName,
}

impl<T> Import<T> {
    pub(crate) fn used_name(&self) -> Option<EcoString> {
        match self.as_name.as_ref() {
            Some((AssignName::Variable(name), _)) => Some(name.clone()),
            Some((AssignName::Discard(_), _)) => None,
            None => self.module.split('/').next_back().map(EcoString::from),
        }
    }

    pub(crate) fn alias_location(&self) -> Option<SrcSpan> {
        self.as_name.as_ref().map(|(_, location)| *location)
    }
}

impl TypedImport {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        if !self.location.contains(byte_index) {
            return None;
        }

        if let Some(unqualified) = self
            .unqualified_values
            .iter()
            .find(|unqualified_value| unqualified_value.location.contains(byte_index))
        {
            return Some(Located::UnqualifiedImport(
                crate::build::UnqualifiedImport {
                    name: &unqualified.name,
                    module: &self.module,
                    is_type: false,
                    location: &unqualified.location,
                },
            ));
        }

        if let Some(unqualified) = self
            .unqualified_types
            .iter()
            .find(|unqualified_value| unqualified_value.location.contains(byte_index))
        {
            return Some(Located::UnqualifiedImport(
                crate::build::UnqualifiedImport {
                    name: &unqualified.name,
                    module: &self.module,
                    is_type: true,
                    location: &unqualified.location,
                },
            ));
        }

        Some(Located::ModuleImport(self))
    }
}

pub type UntypedModuleConstant = ModuleConstant<(), ()>;
pub type TypedModuleConstant = ModuleConstant<Arc<Type>, EcoString>;

#[derive(Debug, Clone, PartialEq, Eq)]
/// A certain fixed value that can be used in multiple places
///
/// # Example(s)
///
/// ```gleam
/// pub const start_year = 2101
/// pub const end_year = 2111
/// ```
pub struct ModuleConstant<T, ConstantRecordTag> {
    pub documentation: Option<(u32, EcoString)>,
    /// The location of the constant, starting at the "(pub) const" keywords and
    /// ending after the ": Type" annotation, or (without an annotation) after its name.
    pub location: SrcSpan,
    pub publicity: Publicity,
    pub name: EcoString,
    pub name_location: SrcSpan,
    pub annotation: Option<TypeAst>,
    pub value: Box<Constant<T, ConstantRecordTag>>,
    pub type_: T,
    pub deprecation: Deprecation,
    pub implementations: Implementations,
}

impl TypedModuleConstant {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        // Check if location is within the annotation.
        if let Some(annotation) = &self.annotation
            && let Some(located) = annotation.find_node(byte_index, self.type_.clone())
        {
            return Some(located);
        }

        if let Some(located) = self.value.find_node(byte_index) {
            return Some(located);
        }

        if self.location.contains(byte_index) {
            Some(Located::ModuleConstant(self))
        } else {
            None
        }
    }
}

pub type UntypedCustomType = CustomType<()>;
pub type TypedCustomType = CustomType<Arc<Type>>;

#[derive(Debug, Clone, PartialEq, Eq)]
/// A newly defined type with one or more constructors.
/// Each variant of the custom type can contain different types, so the type is
/// the product of the types contained by each variant.
///
/// This might be called an algebraic data type (ADT) or tagged union in other
/// languages and type systems.
///
///
/// # Example(s)
///
/// ```gleam
/// pub type Cat {
///   Cat(name: String, cuteness: Int)
/// }
/// ```
pub struct CustomType<T> {
    pub location: SrcSpan,
    pub end_position: u32,
    pub name: EcoString,
    pub name_location: SrcSpan,
    pub publicity: Publicity,
    pub constructors: Vec<RecordConstructor<T>>,
    pub documentation: Option<(u32, EcoString)>,
    pub deprecation: Deprecation,
    pub opaque: bool,
    /// The names of the type parameters.
    pub parameters: Vec<SpannedString>,
    /// Once type checked this field will contain the type information for the
    /// type parameters.
    pub typed_parameters: Vec<T>,
    pub external_erlang: Option<(EcoString, EcoString, SrcSpan)>,
    pub external_javascript: Option<(EcoString, EcoString, SrcSpan)>,
}

impl<T> CustomType<T> {
    /// The `location` field of a `CustomType` is only the location of `pub type
    /// TheName`. This method returns a `SrcSpan` that includes the entire type
    /// definition.
    pub fn full_location(&self) -> SrcSpan {
        SrcSpan::new(self.location.start, self.end_position)
    }
}

impl TypedCustomType {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        // Check if location is within the type of one of the arguments of a constructor.
        if let Some(constructor) = self
            .constructors
            .iter()
            .find(|constructor| constructor.location.contains(byte_index))
        {
            if let Some(annotation) = constructor
                .arguments
                .iter()
                .find(|arg| arg.location.contains(byte_index))
                .and_then(|arg| arg.ast.find_node(byte_index, arg.type_.clone()))
            {
                return Some(annotation);
            }

            return Some(Located::VariantConstructorDefinition(constructor));
        }

        // Note that the custom type `.location` covers the function
        // head, not the entire statement.
        if self.full_location().contains(byte_index) {
            Some(Located::ModuleCustomType(self))
        } else {
            None
        }
    }
}

pub type UntypedTypeAlias = TypeAlias<()>;
pub type TypedTypeAlias = TypeAlias<Arc<Type>>;

#[derive(Debug, Clone, PartialEq, Eq)]
/// A new name for an existing type
///
/// # Example(s)
///
/// ```gleam
/// pub type Headers =
///   List(#(String, String))
/// ```
pub struct TypeAlias<T> {
    pub location: SrcSpan,
    pub alias: EcoString,
    pub name_location: SrcSpan,
    pub parameters: Vec<SpannedString>,
    pub type_ast: TypeAst,
    pub type_: T,
    pub publicity: Publicity,
    pub documentation: Option<(u32, EcoString)>,
    pub deprecation: Deprecation,
}

impl TypedTypeAlias {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        // Check if location is within the type being aliased.
        if let Some(located) = self.type_ast.find_node(byte_index, self.type_.clone()) {
            return Some(located);
        }

        if self.location.contains(byte_index) {
            Some(Located::ModuleTypeAlias(self))
        } else {
            None
        }
    }
}

pub type UntypedDefinition = Definition<(), UntypedExpr, (), ()>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Definition<T, Expr, ConstantRecordTag, PackageName> {
    Function(Function<T, Expr>),
    TypeAlias(TypeAlias<T>),
    CustomType(CustomType<T>),
    Import(Import<PackageName>),
    ModuleConstant(ModuleConstant<T, ConstantRecordTag>),
}

impl<A, B, C, E> Definition<A, B, C, E> {
    pub fn location(&self) -> SrcSpan {
        match self {
            Definition::Function(Function { location, .. })
            | Definition::Import(Import { location, .. })
            | Definition::TypeAlias(TypeAlias { location, .. })
            | Definition::CustomType(CustomType { location, .. })
            | Definition::ModuleConstant(ModuleConstant { location, .. }) => *location,
        }
    }

    /// Returns `true` if the definition is [`Import`].
    ///
    /// [`Import`]: Definition::Import
    #[must_use]
    pub fn is_import(&self) -> bool {
        matches!(self, Self::Import(..))
    }

    /// Returns `true` if the module statement is [`Function`].
    ///
    /// [`Function`]: ModuleStatement::Function
    #[must_use]
    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function(..))
    }

    /// Returns `true` if the module statement is [`CustomType`].
    ///
    /// [`CustomType`]: ModuleStatement::CustomType
    #[must_use]
    pub fn is_custom_type(&self) -> bool {
        matches!(self, Self::CustomType(..))
    }

    pub fn get_doc(&self) -> Option<EcoString> {
        match self {
            Definition::Import(Import { .. }) => None,

            Definition::Function(Function {
                documentation: doc, ..
            })
            | Definition::TypeAlias(TypeAlias {
                documentation: doc, ..
            })
            | Definition::CustomType(CustomType {
                documentation: doc, ..
            })
            | Definition::ModuleConstant(ModuleConstant {
                documentation: doc, ..
            }) => doc.as_ref().map(|(_, doc)| doc.clone()),
        }
    }

    pub fn is_internal(&self) -> bool {
        match self {
            Definition::Function(Function { publicity, .. })
            | Definition::CustomType(CustomType { publicity, .. })
            | Definition::ModuleConstant(ModuleConstant { publicity, .. })
            | Definition::TypeAlias(TypeAlias { publicity, .. }) => publicity.is_internal(),

            Definition::Import(_) => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnqualifiedImport {
    pub location: SrcSpan,
    /// The location excluding the potential `as ...` clause, or the `type` keyword
    pub imported_name_location: SrcSpan,
    pub name: EcoString,
    pub as_name: Option<EcoString>,
}

impl UnqualifiedImport {
    pub fn used_name(&self) -> &EcoString {
        self.as_name.as_ref().unwrap_or(&self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Default, serde::Serialize, serde::Deserialize)]
pub enum Layer {
    #[default]
    Value,
    Type,
}

impl Layer {
    /// Returns `true` if the layer is [`Value`].
    pub fn is_value(&self) -> bool {
        matches!(self, Self::Value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Boolean logic
    And,
    Or,

    // Equality
    Eq,
    NotEq,

    // Order comparison
    LtInt,
    LtEqInt,
    LtFloat,
    LtEqFloat,
    GtEqInt,
    GtInt,
    GtEqFloat,
    GtFloat,

    // Maths
    AddInt,
    AddFloat,
    SubInt,
    SubFloat,
    MultInt,
    MultFloat,
    DivInt,
    DivFloat,
    RemainderInt,

    // Strings
    Concatenate,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OperatorKind {
    BooleanLogic,
    Equality,
    IntComparison,
    FLoatComparison,
    IntMath,
    FloatMath,
    StringConcatenation,
}

pub const PIPE_PRECEDENCE: u8 = 6;

impl BinOp {
    pub fn precedence(&self) -> u8 {
        // Ensure that this matches the other precedence function for guards
        match self {
            Self::Or => 1,

            Self::And => 2,

            Self::Eq | Self::NotEq => 3,

            Self::LtInt
            | Self::LtEqInt
            | Self::LtFloat
            | Self::LtEqFloat
            | Self::GtEqInt
            | Self::GtInt
            | Self::GtEqFloat
            | Self::GtFloat => 4,

            Self::Concatenate => 5,

            // Pipe is 6
            Self::AddInt | Self::AddFloat | Self::SubInt | Self::SubFloat => 7,

            Self::MultInt
            | Self::MultFloat
            | Self::DivInt
            | Self::DivFloat
            | Self::RemainderInt => 8,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Self::And => "&&",
            Self::Or => "||",
            Self::LtInt => "<",
            Self::LtEqInt => "<=",
            Self::LtFloat => "<.",
            Self::LtEqFloat => "<=.",
            Self::Eq => "==",
            Self::NotEq => "!=",
            Self::GtEqInt => ">=",
            Self::GtInt => ">",
            Self::GtEqFloat => ">=.",
            Self::GtFloat => ">.",
            Self::AddInt => "+",
            Self::AddFloat => "+.",
            Self::SubInt => "-",
            Self::SubFloat => "-.",
            Self::MultInt => "*",
            Self::MultFloat => "*.",
            Self::DivInt => "/",
            Self::DivFloat => "/.",
            Self::RemainderInt => "%",
            Self::Concatenate => "<>",
        }
    }

    pub fn operator_kind(&self) -> OperatorKind {
        match self {
            Self::Concatenate => OperatorKind::StringConcatenation,
            Self::Eq | Self::NotEq => OperatorKind::Equality,
            Self::And | Self::Or => OperatorKind::BooleanLogic,
            Self::LtInt | Self::LtEqInt | Self::GtEqInt | Self::GtInt => {
                OperatorKind::IntComparison
            }
            Self::LtFloat | Self::LtEqFloat | Self::GtEqFloat | Self::GtFloat => {
                OperatorKind::FLoatComparison
            }
            Self::AddInt | Self::SubInt | Self::MultInt | Self::RemainderInt | Self::DivInt => {
                OperatorKind::IntMath
            }
            Self::AddFloat | Self::SubFloat | Self::MultFloat | Self::DivFloat => {
                OperatorKind::FloatMath
            }
        }
    }

    pub fn can_be_grouped_with(&self, other: &BinOp) -> bool {
        self.operator_kind() == other.operator_kind()
    }

    pub(crate) fn is_float_operator(&self) -> bool {
        match self {
            BinOp::LtFloat
            | BinOp::LtEqFloat
            | BinOp::GtEqFloat
            | BinOp::GtFloat
            | BinOp::AddFloat
            | BinOp::SubFloat
            | BinOp::MultFloat
            | BinOp::DivFloat => true,

            BinOp::And
            | BinOp::Or
            | BinOp::Eq
            | BinOp::NotEq
            | BinOp::LtInt
            | BinOp::LtEqInt
            | BinOp::GtEqInt
            | BinOp::GtInt
            | BinOp::AddInt
            | BinOp::SubInt
            | BinOp::MultInt
            | BinOp::DivInt
            | BinOp::RemainderInt
            | BinOp::Concatenate => false,
        }
    }

    fn is_bool_operator(&self) -> bool {
        match self {
            BinOp::And | BinOp::Or => true,
            _ => false,
        }
    }

    pub(crate) fn is_int_operator(&self) -> bool {
        match self {
            BinOp::LtInt
            | BinOp::LtEqInt
            | BinOp::GtEqInt
            | BinOp::GtInt
            | BinOp::AddInt
            | BinOp::SubInt
            | BinOp::MultInt
            | BinOp::DivInt
            | BinOp::RemainderInt => true,

            BinOp::And
            | BinOp::Or
            | BinOp::Eq
            | BinOp::NotEq
            | BinOp::LtFloat
            | BinOp::LtEqFloat
            | BinOp::GtEqFloat
            | BinOp::GtFloat
            | BinOp::AddFloat
            | BinOp::SubFloat
            | BinOp::MultFloat
            | BinOp::DivFloat
            | BinOp::Concatenate => false,
        }
    }

    pub fn float_equivalent(&self) -> Option<BinOp> {
        match self {
            BinOp::LtInt => Some(BinOp::LtFloat),
            BinOp::LtEqInt => Some(BinOp::LtEqFloat),
            BinOp::GtEqInt => Some(BinOp::GtEqFloat),
            BinOp::GtInt => Some(BinOp::GtFloat),
            BinOp::AddInt => Some(BinOp::AddFloat),
            BinOp::SubInt => Some(BinOp::SubFloat),
            BinOp::MultInt => Some(BinOp::MultFloat),
            BinOp::DivInt => Some(BinOp::DivFloat),
            _ => None,
        }
    }

    pub fn int_equivalent(&self) -> Option<BinOp> {
        match self {
            BinOp::LtFloat => Some(BinOp::LtInt),
            BinOp::LtEqFloat => Some(BinOp::LtEqInt),
            BinOp::GtEqFloat => Some(BinOp::GtEqInt),
            BinOp::GtFloat => Some(BinOp::GtInt),
            BinOp::AddFloat => Some(BinOp::AddInt),
            BinOp::SubFloat => Some(BinOp::SubInt),
            BinOp::MultFloat => Some(BinOp::MultInt),
            BinOp::DivFloat => Some(BinOp::DivInt),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CallArg<A> {
    pub label: Option<EcoString>,
    pub location: SrcSpan,
    pub value: A,
    pub implicit: Option<ImplicitCallArgOrigin>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ImplicitCallArgOrigin {
    /// The implicit callback argument passed as the last argument to the
    /// function on the right hand side of `use`.
    ///
    Use,
    /// An argument added by the compiler when rewriting a pipe `left |> right`.
    ///
    Pipe,
    /// An argument added by the compiler to fill in all the missing fields of a
    /// record that are being ignored with the `..` syntax.
    ///
    PatternFieldSpread,
    /// An argument used to fill in the missing args when a function on the
    /// right hand side of `use` is being called with the wrong arity.
    ///
    IncorrectArityUse,
    /// An argument added by the compiler to fill in the missing args when using
    /// the record update synax.
    ///
    RecordUpdate,
}

impl<A> CallArg<A> {
    #[must_use]
    pub fn is_implicit(&self) -> bool {
        self.implicit.is_some()
    }

    #[must_use]
    pub fn is_use_implicit_callback(&self) -> bool {
        match self.implicit {
            Some(ImplicitCallArgOrigin::Use | ImplicitCallArgOrigin::IncorrectArityUse) => true,
            Some(_) | None => false,
        }
    }
}

impl CallArg<TypedExpr> {
    pub fn find_node<'a>(
        &'a self,
        byte_index: u32,
        called_function: &'a TypedExpr,
        function_arguments: &'a [TypedCallArg],
    ) -> Option<Located<'a>> {
        match (self.implicit, &self.value) {
            // If a call argument is the implicit use callback then we don't
            // want to look at its arguments and body but we don't want to
            // return the whole anonymous function if anything else doesn't
            // match.
            //
            // In addition, if the callback is invalid because it couldn't be
            // typed, we don't want to return it as it would make it hard for
            // the LSP to give any suggestions on the use function being typed.
            //
            (Some(ImplicitCallArgOrigin::Use), TypedExpr::Invalid { .. }) => None,
            // So the code below is exactly the same as
            // `TypedExpr::Fn{}.find_node()` except we do not return self as a
            // fallback.
            //
            (
                Some(ImplicitCallArgOrigin::Use),
                TypedExpr::Fn {
                    arguments, body, ..
                },
            ) => arguments
                .iter()
                .find_map(|argument| argument.find_node(byte_index))
                .or_else(|| body.iter().find_map(|s| s.find_node(byte_index))),
            // In all other cases we're happy with the default behaviour.
            //
            _ => match self.value.find_node(byte_index) {
                Some(Located::Expression { expression, .. })
                // This is only possibly a label if we are at the end of the expression
                // (so not in the middle like `[abc|]`) and if this argument doesn't
                // already have a label.
                    if byte_index == self.value.location().end && self.label.is_none() =>
                {
                    Some(Located::Expression {
                        expression,
                        position: ExpressionPosition::ArgumentOrLabel {
                            called_function,
                            function_arguments,
                        },
                    })
                }
                Some(located) => Some(located),
                None => {
                    if self.location.contains(byte_index) && self.label.is_some() {
                        Some(Located::Label(self.location, self.value.type_()))
                    } else {
                        None
                    }
                }
            },
        }
    }

    pub fn find_statement(&self, byte_index: u32) -> Option<&TypedStatement> {
        match (self.implicit, &self.value) {
            (Some(ImplicitCallArgOrigin::Use), TypedExpr::Invalid { .. }) => None,
            (Some(ImplicitCallArgOrigin::Use), TypedExpr::Fn { body, .. }) => {
                body.iter().find_map(|s| s.find_statement(byte_index))
            }

            _ => self.value.find_statement(byte_index),
        }
    }

    pub fn is_capture_hole(&self) -> bool {
        match &self.value {
            TypedExpr::Var { name, .. } => name == CAPTURE_VARIABLE,
            _ => false,
        }
    }
}

impl CallArg<TypedPattern> {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        match self.value.find_node(byte_index) {
            Some(located) => Some(located),
            _ => {
                if self.location.contains(byte_index) && self.label.is_some() {
                    Some(Located::Label(self.location, self.value.type_()))
                } else {
                    None
                }
            }
        }
    }
}

impl CallArg<TypedConstant> {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        match self.value.find_node(byte_index) {
            Some(located) => Some(located),
            _ => {
                if self.location.contains(byte_index) && self.label.is_some() {
                    Some(Located::Label(self.location, self.value.type_()))
                } else {
                    None
                }
            }
        }
    }
}

impl CallArg<UntypedExpr> {
    pub fn is_capture_hole(&self) -> bool {
        match &self.value {
            UntypedExpr::Var { name, .. } => name == CAPTURE_VARIABLE,
            _ => false,
        }
    }
}

impl<T> CallArg<T>
where
    T: HasLocation,
{
    #[must_use]
    pub fn uses_label_shorthand(&self) -> bool {
        self.label_shorthand_name().is_some()
    }

    /// If the call arg is defined using a label shorthand, this will return the
    /// label name.
    ///
    pub fn label_shorthand_name(&self) -> Option<&EcoString> {
        if !self.is_implicit() && self.location == self.value.location() {
            self.label.as_ref()
        } else {
            None
        }
    }
}

impl<T> HasLocation for CallArg<T> {
    fn location(&self) -> SrcSpan {
        self.location
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordBeingUpdated {
    pub base: Box<UntypedExpr>,
    pub location: SrcSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UntypedRecordUpdateArg {
    pub label: EcoString,
    pub location: SrcSpan,
    pub value: UntypedExpr,
}

impl UntypedRecordUpdateArg {
    #[must_use]
    pub fn uses_label_shorthand(&self) -> bool {
        self.value.location() == self.location
    }
}

impl HasLocation for UntypedRecordUpdateArg {
    fn location(&self) -> SrcSpan {
        self.location
    }
}

pub type MultiPattern<Type> = Vec<Pattern<Type>>;

pub type UntypedMultiPattern = MultiPattern<()>;
pub type TypedMultiPattern = MultiPattern<Arc<Type>>;

pub type TypedClause = Clause<TypedExpr, Arc<Type>, EcoString>;

pub type UntypedClause = Clause<UntypedExpr, (), ()>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Clause<Expr, Type, RecordTag> {
    pub location: SrcSpan,
    pub pattern: MultiPattern<Type>,
    pub alternative_patterns: Vec<MultiPattern<Type>>,
    pub guard: Option<ClauseGuard<Type, RecordTag>>,
    pub then: Expr,
}

impl<A, B, C> Clause<A, B, C> {
    pub fn pattern_count(&self) -> usize {
        1 + self.alternative_patterns.len()
    }
}

impl TypedClause {
    pub fn location(&self) -> SrcSpan {
        SrcSpan {
            start: self
                .pattern
                .first()
                .map(|p| p.location().start)
                .unwrap_or_default(),
            end: self.then.location().end,
        }
    }

    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.pattern
            .iter()
            .find_map(|p| p.find_node(byte_index))
            .or_else(|| {
                self.alternative_patterns
                    .iter()
                    .flat_map(|p| p.iter())
                    .find_map(|p| p.find_node(byte_index))
            })
            .or_else(|| self.then.find_node(byte_index))
    }

    pub fn pattern_location(&self) -> SrcSpan {
        let start = self.pattern.first().map(|pattern| pattern.location().start);

        let end = if let Some(last_pattern) = self
            .alternative_patterns
            .last()
            .and_then(|patterns| patterns.last())
        {
            Some(last_pattern.location().end)
        } else {
            self.pattern.last().map(|pattern| pattern.location().end)
        };

        SrcSpan::new(start.unwrap_or_default(), end.unwrap_or_default())
    }

    /// If the branch is rebuilding exactly one of the matched subjects and
    /// returning it, this will return the index of that subject.
    ///
    /// For example:
    /// - `n -> n`, `1 -> 1`, `Ok(1) -> Ok(1)` all return `Some(0)`
    /// - `"a", n -> n`, `n, m if n == m -> a` all return `Some(1)`
    /// - `_ -> 1`, `Ok(1), _ -> Ok(2)` all return `None`
    /// ```
    ///
    pub fn returned_subject(&self) -> Option<usize> {
        // The pattern must not have any alternative patterns.
        if !self.alternative_patterns.is_empty() {
            return None;
        }

        self.pattern
            .iter()
            .find_position(|pattern| pattern_and_expression_are_the_same(pattern, &self.then))
            .map(|(position, _)| position)
    }

    /// This returns the names of all the variables bound in this case clause.
    /// For example if we had `#(a, b) | c` this will return "a", "b", and "c".
    pub fn bound_variables(&self) -> impl Iterator<Item = BoundVariable> {
        std::iter::once(&self.pattern)
            .chain(&self.alternative_patterns)
            .flatten()
            .flat_map(|pattern| pattern.bound_variables())
    }

    fn syntactically_eq(&self, other: &Self) -> bool {
        let patterns_are_equal = pairwise_all(&self.pattern, &other.pattern, |(one, other)| {
            one.syntactically_eq(other)
        });

        let alternatives_are_equal = pairwise_all(
            &self.alternative_patterns,
            &other.alternative_patterns,
            |(patterns_one, patterns_other)| {
                pairwise_all(patterns_one, patterns_other, |(one, other)| -> bool {
                    one.syntactically_eq(other)
                })
            },
        );

        let guards_are_equal = match (&self.guard, &other.guard) {
            (None, None) => true,
            (None, Some(_)) | (Some(_), None) => false,
            (Some(one), Some(other)) => one.syntactically_eq(other),
        };

        patterns_are_equal
            && alternatives_are_equal
            && guards_are_equal
            && self.then.syntactically_eq(&other.then)
    }
}

/// Returns true if a pattern and an expression are the same: that is the expression
/// would be building the exact matched value back.
/// For example, if I had a branch like this:
///
/// ```gleam
/// [a, b, c] -> [a, b, c]
/// ```
///
/// The pattern and the expression would indeed be the same. However, if I had
/// something like this:
///
/// ```gleam
/// [first, ..rest] -> [first]
/// ```
///
/// They wouldn't be the same! I'm not building back exactly the value the
/// pattern can match on.
///
fn pattern_and_expression_are_the_same(pattern: &TypedPattern, expression: &TypedExpr) -> bool {
    match (pattern, expression) {
        // A pattern could be the same as a block if the block is wrapping just
        // a single expression that is the same as the pattern itself!
        (pattern, TypedExpr::Block { statements, .. }) if statements.len() == 1 => {
            match statements.first() {
                Statement::Assignment(_) | Statement::Use(_) | Statement::Assert(_) => false,
                Statement::Expression(expression) => {
                    pattern_and_expression_are_the_same(pattern, expression)
                }
            }
        }
        // If the block has many statements then it can never be the same as a
        // pattern.
        (_, TypedExpr::Block { .. }) => false,

        // A pattern and an expression are the same if they're a simple variable
        // with exactly the same name: `x -> x`, `a -> a`
        (
            TypedPattern::Variable {
                name: pattern_var, ..
            },
            TypedExpr::Var { name: body_var, .. },
        ) => pattern_var == body_var,
        (TypedPattern::Variable { .. }, _) => false,

        // Floats, Ints, and Strings are the same if they are exactly the same
        // literal.
        // `1 -> 1`
        // `1.1 -> 1.1`
        // `"wibble" -> "wibble"`
        (
            TypedPattern::Float {
                float_value: pattern_value,
                ..
            },
            TypedExpr::Float { float_value, .. },
        ) => pattern_value == float_value,
        (TypedPattern::Float { .. }, _) => false,

        (
            TypedPattern::Int {
                int_value: pattern_value,
                ..
            },
            TypedExpr::Int { int_value, .. },
        ) => pattern_value == int_value,
        (TypedPattern::Int { .. }, _) => false,

        (
            TypedPattern::String {
                value: pattern_value,
                ..
            },
            TypedExpr::String { value, .. },
        ) => pattern_value == value,
        (TypedPattern::String { .. }, _) => false,

        // A string prefix is equivalent to building the string back:
        // `"wibble" <> wobble -> "wibble" <> wobble`
        // `"wibble" as a <> wobble -> a <> wobble`
        (
            TypedPattern::StringPrefix {
                left_side_assignment,
                left_side_string,
                right_side_assignment,
                ..
            },
            TypedExpr::BinOp {
                name: BinOp::Concatenate,
                left,
                right,
                ..
            },
        ) => {
            let left_side_matches = match (left_side_assignment, left_side_string, left.as_ref()) {
                (_, left_side_string, TypedExpr::String { value, .. }) => value == left_side_string,
                (Some((left_side_name, _)), _, TypedExpr::Var { name, .. }) => {
                    left_side_name == name
                }
                (_, _, _) => false,
            };
            let right_side_matches = match (right_side_assignment, right.as_ref()) {
                (AssignName::Variable(right_side_name), TypedExpr::Var { name, .. }) => {
                    name == right_side_name
                }
                (AssignName::Variable(_) | AssignName::Discard(_), _) => false,
            };
            left_side_matches && right_side_matches
        }
        (TypedPattern::StringPrefix { .. }, _) => false,

        // Two tuples where each element is equivalent to the other:
        // `#(a, 1, "wibble") -> #(a, 1, "wibble")`
        // `#(a, b) -> #(a, b)`
        (
            TypedPattern::Tuple {
                elements: pattern_elements,
                ..
            },
            TypedExpr::Tuple { elements, .. },
        ) => {
            pattern_elements.len() == elements.len()
                && pattern_elements
                    .iter()
                    .zip(elements)
                    .all(|(pattern, expression)| {
                        pattern_and_expression_are_the_same(pattern, expression)
                    })
        }
        (TypedPattern::Tuple { .. }, _) => false,

        // Two lists are the same if each element is equivalent to the other:
        // `[] -> []`
        // `[a, b] -> [a, b]`
        // `[1, ..rest] -> [1, ..rest]`
        (
            TypedPattern::List {
                elements: pattern_elements,
                tail: pattern_tail,
                ..
            },
            TypedExpr::List { elements, tail, .. },
        ) => {
            let tails_are_the_same = match (pattern_tail, tail) {
                (None, None) => true,
                (None, Some(_)) | (Some(_), None) => false,
                (Some(tail_pattern), Some(tail_expression)) => {
                    pattern_and_expression_are_the_same(&tail_pattern.pattern, tail_expression)
                }
            };

            tails_are_the_same
                && pattern_elements.len() == elements.len()
                && pattern_elements
                    .iter()
                    .zip(elements)
                    .all(|(pattern, expression)| {
                        pattern_and_expression_are_the_same(pattern, expression)
                    })
        }
        (TypedPattern::List { .. }, _) => false,

        // Two constructors are the same if the expression is building exactly
        // the same value being matched on (regardless of qualification).
        // `Ok(a) -> Ok(a)`
        // `Ok(1) -> Ok(1)`
        // `Wibble(a, b, c) -> Wibble(a, b, c)`
        // `Ok(a) -> gleam.Ok(a)`
        // `gleam.Ok(1) -> Ok(1)`
        (
            TypedPattern::Constructor {
                constructor:
                    Inferred::Known(PatternConstructor {
                        module: pattern_module,
                        name: pattern_name,
                        ..
                    }),
                arguments: pattern_arguments,
                spread: None,
                ..
            },
            TypedExpr::Call { fun, arguments, .. },
        ) => match fun.as_ref() {
            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant: ValueConstructorVariant::Record { name, module, .. },
                        ..
                    },
                ..
            }
            | TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { name, .. },
                module_name: module,
                ..
            } => {
                pattern_module == module
                    && pattern_name == name
                    && pattern_arguments.len() == arguments.len()
                    && pattern_arguments
                        .iter()
                        .zip(arguments)
                        .all(|(pattern, expression)| {
                            pattern_and_expression_are_the_same(&pattern.value, &expression.value)
                        })
            }

            _ => false,
        },

        // A pattern for a constructor with no arguments:
        // `Nil -> Nil`
        // `gleam.Nil -> Nil`
        // `Nil -> gleam.Nil`
        // `Wibble -> Wibble`
        (
            TypedPattern::Constructor {
                constructor:
                    Inferred::Known(PatternConstructor {
                        module: pattern_module,
                        name: pattern_name,
                        ..
                    }),
                arguments: pattern_arguments,
                spread: None,
                ..
            },
            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant: ValueConstructorVariant::Record { name, module, .. },
                        ..
                    },
                ..
            }
            | TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { name, .. },
                module_name: module,
                ..
            },
        ) => pattern_module == module && pattern_name == name && pattern_arguments.is_empty(),
        (TypedPattern::Constructor { .. }, _) => false,

        // An assignment is the same if the corresponding expression is a
        // variable with the same name, or if the inner pattern is the same:
        // `Ok(1) as a -> a`
        // `Ok(1) as a -> Ok(1)`
        (
            TypedPattern::Assign {
                name: pattern_name, ..
            },
            TypedExpr::Var { name, .. },
        ) => pattern_name == name,
        (TypedPattern::Assign { pattern, .. }, expression) => {
            pattern_and_expression_are_the_same(pattern, expression)
        }

        // Bit arrays are trickier as they can use existing variables in their
        // pattern and shadow existing variables so for now we just ignore
        // those.
        (TypedPattern::BitArray { .. } | TypedPattern::BitArraySize { .. }, _) => false,

        // A discard is never the same as an expression, same goes for an
        // invalid pattern: there's no way to check if it matches an expression!
        (TypedPattern::Discard { .. } | TypedPattern::Invalid { .. }, _) => false,
    }
}

pub type UntypedClauseGuard = ClauseGuard<(), ()>;
pub type TypedClauseGuard = ClauseGuard<Arc<Type>, EcoString>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClauseGuard<Type, RecordTag> {
    Block {
        location: SrcSpan,
        value: Box<ClauseGuard<Type, RecordTag>>,
    },

    Equals {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    NotEquals {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    GtInt {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    GtEqInt {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    LtInt {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    LtEqInt {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    GtFloat {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    GtEqFloat {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    LtFloat {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    LtEqFloat {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    AddInt {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    AddFloat {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    SubInt {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    SubFloat {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    MultInt {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    MultFloat {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    DivInt {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    DivFloat {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    RemainderInt {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    Or {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    And {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    Not {
        location: SrcSpan,
        expression: Box<Self>,
    },

    Var {
        location: SrcSpan,
        type_: Type,
        name: EcoString,
        definition_location: SrcSpan,
    },

    TupleIndex {
        location: SrcSpan,
        index: u64,
        type_: Type,
        tuple: Box<Self>,
    },

    FieldAccess {
        label_location: SrcSpan,
        index: Option<u64>,
        label: EcoString,
        type_: Type,
        container: Box<Self>,
    },

    ModuleSelect {
        location: SrcSpan,
        type_: Type,
        label: EcoString,
        module_name: EcoString,
        module_alias: EcoString,
        literal: Constant<Type, RecordTag>,
    },

    Constant(Constant<Type, RecordTag>),
}

impl<A, B> ClauseGuard<A, B> {
    pub fn location(&self) -> SrcSpan {
        match self {
            ClauseGuard::Constant(constant) => constant.location(),
            ClauseGuard::Or { location, .. }
            | ClauseGuard::And { location, .. }
            | ClauseGuard::Not { location, .. }
            | ClauseGuard::Var { location, .. }
            | ClauseGuard::TupleIndex { location, .. }
            | ClauseGuard::Equals { location, .. }
            | ClauseGuard::NotEquals { location, .. }
            | ClauseGuard::GtInt { location, .. }
            | ClauseGuard::GtEqInt { location, .. }
            | ClauseGuard::LtInt { location, .. }
            | ClauseGuard::LtEqInt { location, .. }
            | ClauseGuard::GtFloat { location, .. }
            | ClauseGuard::GtEqFloat { location, .. }
            | ClauseGuard::LtFloat { location, .. }
            | ClauseGuard::AddInt { location, .. }
            | ClauseGuard::AddFloat { location, .. }
            | ClauseGuard::SubInt { location, .. }
            | ClauseGuard::SubFloat { location, .. }
            | ClauseGuard::MultInt { location, .. }
            | ClauseGuard::MultFloat { location, .. }
            | ClauseGuard::DivInt { location, .. }
            | ClauseGuard::DivFloat { location, .. }
            | ClauseGuard::RemainderInt { location, .. }
            | ClauseGuard::LtEqFloat { location, .. }
            | ClauseGuard::ModuleSelect { location, .. }
            | ClauseGuard::Block { location, .. } => *location,
            ClauseGuard::FieldAccess {
                label_location,
                container,
                ..
            } => container.location().merge(label_location),
        }
    }

    pub fn precedence(&self) -> u8 {
        // Ensure that this matches the other precedence function for guards
        match self.bin_op_name() {
            Some(name) => name.precedence(),
            None => u8::MAX,
        }
    }

    pub fn bin_op_name(&self) -> Option<BinOp> {
        match self {
            ClauseGuard::Or { .. } => Some(BinOp::Or),
            ClauseGuard::And { .. } => Some(BinOp::And),
            ClauseGuard::Equals { .. } => Some(BinOp::Eq),
            ClauseGuard::NotEquals { .. } => Some(BinOp::NotEq),
            ClauseGuard::GtInt { .. } => Some(BinOp::GtInt),
            ClauseGuard::GtEqInt { .. } => Some(BinOp::GtEqInt),
            ClauseGuard::LtInt { .. } => Some(BinOp::LtInt),
            ClauseGuard::LtEqInt { .. } => Some(BinOp::LtEqInt),
            ClauseGuard::GtFloat { .. } => Some(BinOp::GtFloat),
            ClauseGuard::GtEqFloat { .. } => Some(BinOp::GtEqFloat),
            ClauseGuard::LtFloat { .. } => Some(BinOp::LtFloat),
            ClauseGuard::LtEqFloat { .. } => Some(BinOp::LtEqFloat),
            ClauseGuard::AddInt { .. } => Some(BinOp::AddInt),
            ClauseGuard::AddFloat { .. } => Some(BinOp::AddFloat),
            ClauseGuard::SubInt { .. } => Some(BinOp::SubInt),
            ClauseGuard::SubFloat { .. } => Some(BinOp::SubFloat),
            ClauseGuard::MultInt { .. } => Some(BinOp::MultInt),
            ClauseGuard::MultFloat { .. } => Some(BinOp::MultFloat),
            ClauseGuard::DivInt { .. } => Some(BinOp::DivInt),
            ClauseGuard::DivFloat { .. } => Some(BinOp::DivFloat),
            ClauseGuard::RemainderInt { .. } => Some(BinOp::RemainderInt),

            ClauseGuard::Constant(_)
            | ClauseGuard::Var { .. }
            | ClauseGuard::Not { .. }
            | ClauseGuard::TupleIndex { .. }
            | ClauseGuard::FieldAccess { .. }
            | ClauseGuard::ModuleSelect { .. }
            | ClauseGuard::Block { .. } => None,
        }
    }
}

impl TypedClauseGuard {
    pub fn type_(&self) -> Arc<Type> {
        match self {
            ClauseGuard::Var { type_, .. } => type_.clone(),
            ClauseGuard::TupleIndex { type_, .. } => type_.clone(),
            ClauseGuard::FieldAccess { type_, .. } => type_.clone(),
            ClauseGuard::ModuleSelect { type_, .. } => type_.clone(),
            ClauseGuard::Constant(constant) => constant.type_(),
            ClauseGuard::Block { value, .. } => value.type_(),

            ClauseGuard::AddInt { .. }
            | ClauseGuard::SubInt { .. }
            | ClauseGuard::MultInt { .. }
            | ClauseGuard::DivInt { .. }
            | ClauseGuard::RemainderInt { .. } => type_::int(),

            ClauseGuard::AddFloat { .. }
            | ClauseGuard::SubFloat { .. }
            | ClauseGuard::MultFloat { .. }
            | ClauseGuard::DivFloat { .. } => type_::float(),

            ClauseGuard::Or { .. }
            | ClauseGuard::Not { .. }
            | ClauseGuard::And { .. }
            | ClauseGuard::Equals { .. }
            | ClauseGuard::NotEquals { .. }
            | ClauseGuard::GtInt { .. }
            | ClauseGuard::GtEqInt { .. }
            | ClauseGuard::LtInt { .. }
            | ClauseGuard::LtEqInt { .. }
            | ClauseGuard::GtFloat { .. }
            | ClauseGuard::GtEqFloat { .. }
            | ClauseGuard::LtFloat { .. }
            | ClauseGuard::LtEqFloat { .. } => type_::bool(),
        }
    }

    pub(crate) fn referenced_variables(&self) -> im::HashSet<&EcoString> {
        match self {
            ClauseGuard::Var { name, .. } => im::hashset![name],

            ClauseGuard::Block { value, .. } => value.referenced_variables(),
            ClauseGuard::Not { expression, .. } => expression.referenced_variables(),
            ClauseGuard::TupleIndex { tuple, .. } => tuple.referenced_variables(),
            ClauseGuard::FieldAccess { container, .. } => container.referenced_variables(),
            ClauseGuard::Constant(constant) => constant.referenced_variables(),
            ClauseGuard::ModuleSelect { .. } => im::HashSet::new(),

            ClauseGuard::Equals { left, right, .. }
            | ClauseGuard::NotEquals { left, right, .. }
            | ClauseGuard::GtInt { left, right, .. }
            | ClauseGuard::GtEqInt { left, right, .. }
            | ClauseGuard::LtInt { left, right, .. }
            | ClauseGuard::LtEqInt { left, right, .. }
            | ClauseGuard::GtFloat { left, right, .. }
            | ClauseGuard::GtEqFloat { left, right, .. }
            | ClauseGuard::LtFloat { left, right, .. }
            | ClauseGuard::LtEqFloat { left, right, .. }
            | ClauseGuard::AddInt { left, right, .. }
            | ClauseGuard::AddFloat { left, right, .. }
            | ClauseGuard::SubInt { left, right, .. }
            | ClauseGuard::SubFloat { left, right, .. }
            | ClauseGuard::MultInt { left, right, .. }
            | ClauseGuard::MultFloat { left, right, .. }
            | ClauseGuard::DivInt { left, right, .. }
            | ClauseGuard::DivFloat { left, right, .. }
            | ClauseGuard::RemainderInt { left, right, .. }
            | ClauseGuard::And { left, right, .. }
            | ClauseGuard::Or { left, right, .. } => left
                .referenced_variables()
                .union(right.referenced_variables()),
        }
    }

    fn syntactically_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                ClauseGuard::Block { value, .. },
                ClauseGuard::Block {
                    value: other_value, ..
                },
            ) => value.syntactically_eq(other_value),
            (ClauseGuard::Block { .. }, _) => false,

            (
                ClauseGuard::Equals { left, right, .. },
                ClauseGuard::Equals {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::Equals { .. }, _) => false,

            (
                ClauseGuard::NotEquals { left, right, .. },
                ClauseGuard::NotEquals {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::NotEquals { .. }, _) => false,

            (
                ClauseGuard::GtInt { left, right, .. },
                ClauseGuard::GtInt {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::GtInt { .. }, _) => false,

            (
                ClauseGuard::GtEqInt { left, right, .. },
                ClauseGuard::GtEqInt {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::GtEqInt { .. }, _) => false,

            (
                ClauseGuard::LtInt { left, right, .. },
                ClauseGuard::LtInt {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::LtInt { .. }, _) => false,

            (
                ClauseGuard::LtEqInt { left, right, .. },
                ClauseGuard::LtEqInt {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::LtEqInt { .. }, _) => false,

            (
                ClauseGuard::GtFloat { left, right, .. },
                ClauseGuard::GtFloat {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::GtFloat { .. }, _) => false,

            (
                ClauseGuard::GtEqFloat { left, right, .. },
                ClauseGuard::GtEqFloat {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::GtEqFloat { .. }, _) => false,

            (
                ClauseGuard::LtFloat { left, right, .. },
                ClauseGuard::LtFloat {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::LtFloat { .. }, _) => false,

            (
                ClauseGuard::LtEqFloat { left, right, .. },
                ClauseGuard::LtEqFloat {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::LtEqFloat { .. }, _) => false,

            (
                ClauseGuard::AddInt { left, right, .. },
                ClauseGuard::AddInt {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::AddInt { .. }, _) => false,

            (
                ClauseGuard::AddFloat { left, right, .. },
                ClauseGuard::AddFloat {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::AddFloat { .. }, _) => false,

            (
                ClauseGuard::SubInt { left, right, .. },
                ClauseGuard::SubInt {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::SubInt { .. }, _) => false,

            (
                ClauseGuard::SubFloat { left, right, .. },
                ClauseGuard::SubFloat {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::SubFloat { .. }, _) => false,

            (
                ClauseGuard::MultInt { left, right, .. },
                ClauseGuard::MultInt {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::MultInt { .. }, _) => false,

            (
                ClauseGuard::MultFloat { left, right, .. },
                ClauseGuard::MultFloat {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::MultFloat { .. }, _) => false,

            (
                ClauseGuard::DivInt { left, right, .. },
                ClauseGuard::DivInt {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::DivInt { .. }, _) => false,

            (
                ClauseGuard::DivFloat { left, right, .. },
                ClauseGuard::DivFloat {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::DivFloat { .. }, _) => false,

            (
                ClauseGuard::RemainderInt { left, right, .. },
                ClauseGuard::RemainderInt {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::RemainderInt { .. }, _) => false,

            (
                ClauseGuard::Or { left, right, .. },
                ClauseGuard::Or {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::Or { .. }, _) => false,

            (
                ClauseGuard::And { left, right, .. },
                ClauseGuard::And {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (ClauseGuard::And { .. }, _) => false,

            (
                ClauseGuard::Not { expression, .. },
                ClauseGuard::Not {
                    expression: other_expression,
                    ..
                },
            ) => expression.syntactically_eq(other_expression),
            (ClauseGuard::Not { .. }, _) => false,

            (
                ClauseGuard::Var { name, .. },
                ClauseGuard::Var {
                    name: other_name, ..
                },
            ) => name == other_name,
            (ClauseGuard::Var { .. }, _) => false,

            (
                ClauseGuard::TupleIndex { index, tuple, .. },
                ClauseGuard::TupleIndex {
                    index: other_index,
                    tuple: other_tuple,
                    ..
                },
            ) => index == other_index && tuple.syntactically_eq(other_tuple),
            (ClauseGuard::TupleIndex { .. }, _) => false,

            (
                ClauseGuard::FieldAccess {
                    label, container, ..
                },
                ClauseGuard::FieldAccess {
                    label: other_label,
                    container: other_container,
                    ..
                },
            ) => label == other_label && container.syntactically_eq(other_container),
            (ClauseGuard::FieldAccess { .. }, _) => false,

            (
                ClauseGuard::ModuleSelect {
                    label,
                    module_alias,
                    ..
                },
                ClauseGuard::ModuleSelect {
                    label: other_label,
                    module_alias: other_module_alias,
                    ..
                },
            ) => label == other_label && module_alias == other_module_alias,
            (ClauseGuard::ModuleSelect { .. }, _) => false,

            (ClauseGuard::Constant(one), ClauseGuard::Constant(other)) => {
                one.syntactically_eq(other)
            }
            (ClauseGuard::Constant(_), _) => false,
        }
    }
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Default,
    Clone,
    Copy,
    serde::Serialize,
    serde::Deserialize,
    Hash,
)]
pub struct SrcSpan {
    pub start: u32,
    pub end: u32,
}

impl SrcSpan {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub fn contains(&self, byte_index: u32) -> bool {
        byte_index >= self.start && byte_index <= self.end
    }

    pub fn contains_span(&self, span: SrcSpan) -> bool {
        self.contains(span.start) && self.contains(span.end)
    }

    /// Merges two spans into a new one that starts at the start of the smaller
    /// one and ends at the end of the bigger one. For example:
    ///
    /// ```txt
    /// wibble    wobble
    /// ─┬────    ─┬────
    ///  │         ╰─ one span
    ///  ╰─ the other span
    /// ─┬──────────────
    ///  ╰─ the span you get by merging the two
    /// ```
    pub fn merge(&self, with: &SrcSpan) -> SrcSpan {
        Self {
            start: self.start.min(with.start),
            end: self.end.max(with.end),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DefinitionLocation {
    pub module: Option<EcoString>,
    pub span: SrcSpan,
}

pub type UntypedPattern = Pattern<()>;
pub type TypedPattern = Pattern<Arc<Type>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern<Type> {
    Int {
        location: SrcSpan,
        value: EcoString,
        int_value: BigInt,
    },

    Float {
        location: SrcSpan,
        value: EcoString,
        float_value: LiteralFloatValue,
    },

    String {
        location: SrcSpan,
        value: EcoString,
    },

    /// The creation of a variable.
    /// e.g. `assert [this_is_a_var, .._] = x`
    Variable {
        location: SrcSpan,
        name: EcoString,
        type_: Type,
        origin: VariableOrigin,
    },

    /// The specified size of a bit array. This can either be a literal integer,
    /// a reference to a variable, or a maths expression.
    /// e.g. `let assert <<y:size(somevar)>> = x`
    BitArraySize(BitArraySize<Type>),

    /// A name given to a sub-pattern using the `as` keyword.
    /// e.g. `assert #(1, [_, _] as the_list) = x`
    Assign {
        name: EcoString,
        location: SrcSpan,
        pattern: Box<Self>,
    },

    /// A pattern that binds to any value but does not assign a variable.
    /// Always starts with an underscore.
    Discard {
        name: EcoString,
        location: SrcSpan,
        type_: Type,
    },

    List {
        location: SrcSpan,
        elements: Vec<Self>,
        tail: Option<Box<TailPattern<Type>>>,
        /// The type of the list, so this is going to be `List(something)`.
        ///
        type_: Type,
    },

    /// The constructor for a custom type. Starts with an uppercase letter.
    Constructor {
        location: SrcSpan,
        name_location: SrcSpan,
        name: EcoString,
        arguments: Vec<CallArg<Self>>,
        module: Option<(EcoString, SrcSpan)>,
        constructor: Inferred<PatternConstructor>,
        spread: Option<SrcSpan>,
        type_: Type,
    },

    Tuple {
        location: SrcSpan,
        elements: Vec<Self>,
    },

    BitArray {
        location: SrcSpan,
        segments: Vec<BitArraySegment<Self, Type>>,
    },

    // "prefix" <> variable
    StringPrefix {
        location: SrcSpan,
        left_location: SrcSpan,
        left_side_assignment: Option<(EcoString, SrcSpan)>,
        right_location: SrcSpan,
        left_side_string: EcoString,
        /// The variable on the right hand side of the `<>`.
        right_side_assignment: AssignName,
    },

    /// A placeholder pattern used to allow module analysis to continue
    /// even when there are type errors. Should never end up in generated code.
    Invalid {
        location: SrcSpan,
        type_: Type,
    },
}

pub type TypedBitArraySize = BitArraySize<Arc<Type>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BitArraySize<Type> {
    Int {
        location: SrcSpan,
        value: EcoString,
        int_value: BigInt,
    },

    Variable {
        location: SrcSpan,
        name: EcoString,
        constructor: Option<Box<ValueConstructor>>,
        type_: Type,
    },

    BinaryOperator {
        location: SrcSpan,
        operator: IntOperator,
        left: Box<Self>,
        right: Box<Self>,
    },

    Block {
        location: SrcSpan,
        inner: Box<Self>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum IntOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}

impl IntOperator {
    pub fn precedence(&self) -> u8 {
        match self {
            Self::Add | Self::Subtract => 7,

            Self::Multiply | Self::Divide | Self::Remainder => 8,
        }
    }

    pub fn to_bin_op(&self) -> BinOp {
        match self {
            IntOperator::Add => BinOp::AddInt,
            IntOperator::Subtract => BinOp::SubInt,
            IntOperator::Multiply => BinOp::MultInt,
            IntOperator::Divide => BinOp::DivInt,
            IntOperator::Remainder => BinOp::RemainderInt,
        }
    }
}

impl<T> BitArraySize<T> {
    pub fn location(&self) -> SrcSpan {
        match self {
            BitArraySize::Int { location, .. }
            | BitArraySize::Variable { location, .. }
            | BitArraySize::BinaryOperator { location, .. }
            | BitArraySize::Block { location, .. } => *location,
        }
    }

    pub fn non_zero_compile_time_number(&self) -> bool {
        match self {
            BitArraySize::Int { int_value, .. } => !int_value.is_zero(),
            BitArraySize::Block { inner, .. } => inner.non_zero_compile_time_number(),
            BitArraySize::Variable { .. } | BitArraySize::BinaryOperator { .. } => false,
        }
    }

    fn syntactically_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (BitArraySize::Int { int_value: n, .. }, BitArraySize::Int { int_value: m, .. }) => {
                n == m
            }
            (BitArraySize::Int { .. }, _) => false,

            (
                BitArraySize::Variable { name, .. },
                BitArraySize::Variable {
                    name: other_name, ..
                },
            ) => name == other_name,
            (BitArraySize::Variable { .. }, _) => false,

            (
                BitArraySize::BinaryOperator {
                    operator,
                    left,
                    right,
                    ..
                },
                BitArraySize::BinaryOperator {
                    operator: other_operator,
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => {
                operator == other_operator
                    && left.syntactically_eq(other_left)
                    && right.syntactically_eq(other_right)
            }
            (BitArraySize::BinaryOperator { .. }, _) => false,

            (
                BitArraySize::Block { inner, .. },
                BitArraySize::Block {
                    inner: other_inner, ..
                },
            ) => inner.syntactically_eq(other_inner),
            (BitArraySize::Block { .. }, _) => false,
        }
    }
}

pub type TypedTailPattern = TailPattern<Arc<Type>>;

pub type UntypedTailPattern = TailPattern<()>;

/// The pattern one can use to match on the rest of a list:
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TailPattern<Type> {
    /// The entire location of the pattern, covering the `..` as well.
    ///
    pub location: SrcSpan,

    /// The name assigned to the rest of the list being matched:
    ///
    /// ```gleam
    /// [wibble, ..]
    /// //       ^^ no name
    ///
    /// [wibble, ..rest]
    /// //       ^^^^^^ a variable name
    ///
    /// [wibble, .._rest]
    /// //       ^^^^^^^ a discarded name
    /// ```
    ///
    pub pattern: Pattern<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssignName {
    Variable(EcoString),
    Discard(EcoString),
}

impl AssignName {
    pub fn name(&self) -> &EcoString {
        match self {
            AssignName::Variable(name) | AssignName::Discard(name) => name,
        }
    }

    pub fn to_arg_names(self, location: SrcSpan) -> ArgNames {
        match self {
            AssignName::Variable(name) => ArgNames::Named { name, location },
            AssignName::Discard(name) => ArgNames::Discard { name, location },
        }
    }

    pub fn assigned_name(&self) -> Option<&str> {
        match self {
            AssignName::Variable(name) => Some(name),
            AssignName::Discard(_) => None,
        }
    }
}

impl<A> Pattern<A> {
    pub fn location(&self) -> SrcSpan {
        match self {
            Pattern::Assign {
                pattern, location, ..
            } => SrcSpan::new(pattern.location().start, location.end),
            Pattern::Int { location, .. }
            | Pattern::Variable { location, .. }
            | Pattern::List { location, .. }
            | Pattern::Float { location, .. }
            | Pattern::Discard { location, .. }
            | Pattern::String { location, .. }
            | Pattern::Tuple { location, .. }
            | Pattern::Constructor { location, .. }
            | Pattern::StringPrefix { location, .. }
            | Pattern::BitArray { location, .. }
            | Pattern::Invalid { location, .. } => *location,
            Pattern::BitArraySize(size) => size.location(),
        }
    }

    /// Returns `true` if the pattern is [`Discard`].
    ///
    /// [`Discard`]: Pattern::Discard
    #[must_use]
    pub fn is_discard(&self) -> bool {
        matches!(self, Self::Discard { .. })
    }

    #[must_use]
    pub fn is_variable(&self) -> bool {
        match self {
            Pattern::Variable { .. } => true,
            _ => false,
        }
    }

    #[must_use]
    pub fn is_string(&self) -> bool {
        matches!(self, Self::String { .. })
    }
}

impl TypedPattern {
    fn syntactically_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Pattern::Int { int_value: n, .. }, Pattern::Int { int_value: m, .. }) => n == m,
            (Pattern::Int { .. }, _) => false,

            (Pattern::Float { float_value: n, .. }, Pattern::Float { float_value: m, .. }) => {
                n == m
            }
            (Pattern::Float { .. }, _) => false,

            (
                Pattern::String { value, .. },
                Pattern::String {
                    value: other_value, ..
                },
            ) => value == other_value,
            (Pattern::String { .. }, _) => false,

            (
                Pattern::Variable { name, .. },
                Pattern::Variable {
                    name: other_name, ..
                },
            ) => name == other_name,
            (Pattern::Variable { .. }, _) => false,

            (Pattern::BitArraySize(one), Pattern::BitArraySize(other)) => {
                one.syntactically_eq(other)
            }
            (Pattern::BitArraySize(..), _) => false,

            (
                Pattern::Assign { name, pattern, .. },
                Pattern::Assign {
                    name: other_name,
                    pattern: other_pattern,
                    ..
                },
            ) => name == other_name && pattern.syntactically_eq(other_pattern),
            (Pattern::Assign { .. }, _) => false,

            (
                Pattern::Discard { name, .. },
                Pattern::Discard {
                    name: other_name, ..
                },
            ) => name == other_name,
            (Pattern::Discard { .. }, _) => false,

            (
                Pattern::List { elements, tail, .. },
                Pattern::List {
                    elements: other_elements,
                    tail: other_tail,
                    ..
                },
            ) => {
                let tails_are_equal = match (tail, other_tail) {
                    (None, None) => true,
                    (None, Some(_)) | (Some(_), None) => false,
                    (Some(one), Some(other)) => one.pattern.syntactically_eq(&other.pattern),
                };
                tails_are_equal
                    && pairwise_all(elements, other_elements, |(one, other)| {
                        one.syntactically_eq(other)
                    })
            }
            (Pattern::List { .. }, _) => false,

            (
                Pattern::Constructor {
                    name,
                    arguments,
                    module,
                    ..
                },
                Pattern::Constructor {
                    name: other_name,
                    arguments: other_arguments,
                    module: other_module,
                    ..
                },
            ) => {
                let modules_are_equal = match (module, other_module) {
                    (None, None) => true,
                    (None, Some(_)) | (Some(_), None) => false,
                    (Some((one, _)), Some((other, _))) => one == other,
                };
                modules_are_equal
                    && name == other_name
                    && pairwise_all(arguments, other_arguments, |(one, other)| {
                        one.label == other.label && one.value.syntactically_eq(&other.value)
                    })
            }
            (Pattern::Constructor { .. }, _) => false,

            (
                Pattern::Tuple { elements, .. },
                Pattern::Tuple {
                    elements: other_elements,
                    ..
                },
            ) => pairwise_all(elements, other_elements, |(one, other)| {
                one.syntactically_eq(other)
            }),
            (Pattern::Tuple { .. }, _) => false,

            (
                Pattern::BitArray { segments, .. },
                Pattern::BitArray {
                    segments: other_segments,
                    ..
                },
            ) => pairwise_all(segments, other_segments, |(one, other)| {
                one.syntactically_eq(other)
            }),
            (Pattern::BitArray { .. }, _) => false,

            (
                Pattern::StringPrefix {
                    left_side_assignment,
                    left_side_string,
                    right_side_assignment,
                    ..
                },
                Pattern::StringPrefix {
                    left_side_assignment: other_left_side_assignment,
                    left_side_string: other_left_side_string,
                    right_side_assignment: other_right_side_assignment,
                    ..
                },
            ) => {
                let left_side_assignments_are_equal =
                    match (left_side_assignment, other_left_side_assignment) {
                        (None, None) => true,
                        (None, Some(_)) | (Some(_), None) => false,
                        (Some((one, _)), Some((other, _))) => one == other,
                    };
                let right_side_assignments_are_equal =
                    match (right_side_assignment, other_right_side_assignment) {
                        (AssignName::Variable(one), AssignName::Variable(other)) => one == other,
                        (AssignName::Variable(_), AssignName::Discard(_)) => false,
                        (AssignName::Discard(one), AssignName::Discard(other)) => one == other,
                        (AssignName::Discard(_), AssignName::Variable(_)) => false,
                    };
                left_side_string == other_left_side_string
                    && left_side_assignments_are_equal
                    && right_side_assignments_are_equal
            }
            (Pattern::StringPrefix { .. }, _) => false,

            (Pattern::Invalid { .. }, _) => false,
        }
    }
}

/// A variable bound inside a pattern.
#[derive(Debug, Clone)]
pub struct BoundVariable {
    pub name: BoundVariableName,
    pub location: SrcSpan,
    pub type_: Arc<Type>,
}

#[derive(Debug, Clone)]
pub enum BoundVariableName {
    /// A record's labelled field introduced with the shorthand syntax.
    ShorthandLabel { name: EcoString },
    ListTail {
        name: EcoString,
        /// The location of the whole tail, from the `..` prefix until the end of the variable.
        tail_prefix_location: SrcSpan,
    },
    /// Any other variable name.
    Regular { name: EcoString },
}

impl BoundVariable {
    pub fn name(&self) -> EcoString {
        match &self.name {
            BoundVariableName::ShorthandLabel { name }
            | BoundVariableName::ListTail { name, .. }
            | BoundVariableName::Regular { name } => name.clone(),
        }
    }
}

impl TypedPattern {
    pub fn definition_location(&self) -> Option<DefinitionLocation> {
        match self {
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::Variable { .. }
            | Pattern::BitArraySize { .. }
            | Pattern::Assign { .. }
            | Pattern::Discard { .. }
            | Pattern::List { .. }
            | Pattern::Tuple { .. }
            | Pattern::BitArray { .. }
            | Pattern::StringPrefix { .. }
            | Pattern::Invalid { .. } => None,

            Pattern::Constructor { constructor, .. } => constructor.definition_location(),
        }
    }

    pub fn get_documentation(&self) -> Option<&str> {
        match self {
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::Variable { .. }
            | Pattern::BitArraySize { .. }
            | Pattern::Assign { .. }
            | Pattern::Discard { .. }
            | Pattern::List { .. }
            | Pattern::Tuple { .. }
            | Pattern::BitArray { .. }
            | Pattern::StringPrefix { .. }
            | Pattern::Invalid { .. } => None,

            Pattern::Constructor { constructor, .. } => constructor.get_documentation(),
        }
    }

    pub fn type_(&self) -> Arc<Type> {
        match self {
            Pattern::Int { .. } => type_::int(),
            Pattern::Float { .. } => type_::float(),
            Pattern::String { .. } => type_::string(),
            Pattern::BitArray { .. } => type_::bit_array(),
            Pattern::StringPrefix { .. } => type_::string(),

            Pattern::Variable { type_, .. }
            | Pattern::List { type_, .. }
            | Pattern::Constructor { type_, .. }
            | Pattern::Invalid { type_, .. } => type_.clone(),

            Pattern::Assign { pattern, .. } => pattern.type_(),

            // Bit array sizes should always be integers
            Pattern::BitArraySize(_) => type_::int(),

            Pattern::Discard { type_, .. } => type_.clone(),

            Pattern::Tuple { elements, .. } => {
                type_::tuple(elements.iter().map(|p| p.type_()).collect())
            }
        }
    }

    fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        if !self.location().contains(byte_index) {
            return None;
        }

        if let Pattern::Variable { name, .. } = self {
            // For pipes the pattern can't be pointed to
            if name.as_str().eq(PIPE_VARIABLE) {
                return None;
            }
        }

        match self {
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::Variable { .. }
            | Pattern::BitArraySize { .. }
            | Pattern::Assign { .. }
            | Pattern::Discard { .. }
            | Pattern::StringPrefix { .. }
            | Pattern::Invalid { .. } => Some(Located::Pattern(self)),

            Pattern::Constructor {
                arguments, spread, ..
            } => match spread {
                Some(spread_location) if spread_location.contains(byte_index) => {
                    Some(Located::PatternSpread {
                        spread_location: *spread_location,
                        pattern: self,
                    })
                }

                Some(_) | None => arguments
                    .iter()
                    .find_map(|argument| argument.find_node(byte_index)),
            },
            Pattern::List { elements, tail, .. } => elements
                .iter()
                .find_map(|element| element.find_node(byte_index))
                .or_else(|| {
                    tail.as_ref()
                        .and_then(|tail| tail.pattern.find_node(byte_index))
                }),

            Pattern::Tuple { elements, .. } => elements
                .iter()
                .find_map(|element| element.find_node(byte_index)),

            Pattern::BitArray { segments, .. } => segments
                .iter()
                .find_map(|segment| segment.find_node(byte_index))
                .or(Some(Located::Pattern(self))),
        }
        .or(Some(Located::Pattern(self)))
    }

    /// If the pattern is a `Constructor` with a spread, it returns a tuple with
    /// all the ignored fields. Split in unlabelled and labelled ones.
    ///
    pub(crate) fn unused_arguments(&self) -> Option<PatternUnusedArguments> {
        let TypedPattern::Constructor {
            arguments,
            spread: Some(_),
            ..
        } = self
        else {
            return None;
        };

        let mut positional = vec![];
        let mut labelled = vec![];
        for argument in arguments {
            // We only want to display the arguments that were ignored using `..`.
            // Any argument ignored that way is marked as implicit, so if it is
            // not implicit we just ignore it.
            if !argument.is_implicit() {
                continue;
            }
            let type_ = argument.value.type_();
            match &argument.label {
                Some(label) => labelled.push((label.clone(), type_)),
                None => positional.push(type_),
            }
        }

        Some(PatternUnusedArguments {
            positional,
            labelled,
        })
    }

    /// Whether the pattern always matches. For example, a tuple or simple
    /// variable assignment always match and can never fail.
    #[must_use]
    pub fn always_matches(&self) -> bool {
        match self {
            Pattern::Variable { .. } | Pattern::Discard { .. } => true,
            Pattern::Assign { pattern, .. } => pattern.always_matches(),
            Pattern::Tuple { elements, .. } => {
                elements.iter().all(|element| element.always_matches())
            }
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::BitArraySize { .. }
            | Pattern::List { .. }
            | Pattern::Constructor { .. }
            | Pattern::BitArray { .. }
            | Pattern::StringPrefix { .. }
            | Pattern::Invalid { .. } => false,
        }
    }

    pub fn bound_variables(&self) -> Vec<BoundVariable> {
        let mut variables = Vec::new();
        self.collect_bound_variables(&mut variables);
        variables
    }

    fn collect_bound_variables(&self, variables: &mut Vec<BoundVariable>) {
        match self {
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::Discard { .. }
            | Pattern::Invalid { .. } => {}

            Pattern::Variable {
                name,
                location,
                type_,
                ..
            } => variables.push(BoundVariable {
                name: BoundVariableName::Regular { name: name.clone() },
                location: *location,
                type_: type_.clone(),
            }),
            Pattern::BitArraySize { .. } => {}
            Pattern::Assign {
                name,
                pattern,
                location,
            } => {
                variables.push(BoundVariable {
                    name: BoundVariableName::Regular { name: name.clone() },
                    location: *location,
                    type_: pattern.type_(),
                });
                pattern.collect_bound_variables(variables);
            }
            Pattern::List {
                elements,
                tail,
                type_,
                ..
            } => {
                for element in elements {
                    element.collect_bound_variables(variables);
                }
                if let Some(tail) = tail
                    && let Pattern::Variable { name, location, .. } = tail.pattern.to_owned()
                {
                    variables.push(BoundVariable {
                        name: BoundVariableName::ListTail {
                            name,
                            tail_prefix_location: tail.location,
                        },
                        location,
                        type_: type_.clone(),
                    })
                };
            }
            Pattern::Constructor { arguments, .. } => {
                for argument in arguments {
                    if let Some(name) = argument.label_shorthand_name() {
                        variables.push(BoundVariable {
                            name: BoundVariableName::ShorthandLabel { name: name.clone() },
                            location: argument.location,
                            type_: argument.value.type_(),
                        })
                    } else {
                        argument.value.collect_bound_variables(variables);
                    }
                }
            }
            Pattern::Tuple { elements, .. } => {
                for element in elements {
                    element.collect_bound_variables(variables);
                }
            }
            Pattern::BitArray { segments, .. } => {
                for segment in segments {
                    segment.value.collect_bound_variables(variables);
                }
            }
            Pattern::StringPrefix {
                left_side_assignment,
                right_side_assignment,
                right_location,
                ..
            } => {
                if let Some((name, location)) = left_side_assignment {
                    variables.push(BoundVariable {
                        name: BoundVariableName::Regular { name: name.clone() },
                        location: *location,
                        type_: type_::string(),
                    });
                }
                match right_side_assignment {
                    AssignName::Variable(name) => variables.push(BoundVariable {
                        name: BoundVariableName::Regular { name: name.clone() },
                        location: *right_location,
                        type_: type_::string(),
                    }),
                    AssignName::Discard(_) => {}
                }
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct PatternUnusedArguments {
    pub positional: Vec<Arc<Type>>,
    pub labelled: Vec<(EcoString, Arc<Type>)>,
}

impl<A> HasLocation for Pattern<A> {
    fn location(&self) -> SrcSpan {
        self.location()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignmentKind<Expression> {
    /// let x = ...
    Let,
    /// This is a let assignment generated by the compiler for intermediate variables
    /// needed by record updates and `use`.
    /// Like a regular `Let` assignment this can never fail.
    ///
    Generated,
    /// let assert x = ...
    Assert {
        /// The src byte span of the `let assert`
        ///
        /// ```gleam
        /// let assert Wibble = todo
        /// ^^^^^^^^^^
        /// ```
        location: SrcSpan,

        /// The byte index of the start of `assert`
        ///
        /// ```gleam
        /// let assert Wibble = todo
        ///     ^
        /// ```
        assert_keyword_start: u32,

        /// The message given to the assertion:
        ///
        /// ```gleam
        /// let asset Ok(a) = something() as "This will never fail"
        ///                                  ^^^^^^^^^^^^^^^^^^^^^^
        /// ```
        message: Option<Expression>,
    },
}

impl<Expression> AssignmentKind<Expression> {
    /// Returns `true` if the assignment kind is [`Assert`].
    ///
    /// [`Assert`]: AssignmentKind::Assert
    #[must_use]
    pub fn is_assert(&self) -> bool {
        match self {
            Self::Assert { .. } => true,
            Self::Let | Self::Generated => false,
        }
    }
}

// BitArrays

pub type UntypedExprBitArraySegment = BitArraySegment<UntypedExpr, ()>;
pub type TypedExprBitArraySegment = BitArraySegment<TypedExpr, Arc<Type>>;

pub type UntypedConstantBitArraySegment = BitArraySegment<UntypedConstant, ()>;
pub type TypedConstantBitArraySegment = BitArraySegment<TypedConstant, Arc<Type>>;

pub type UntypedPatternBitArraySegment = BitArraySegment<UntypedPattern, ()>;
pub type TypedPatternBitArraySegment = BitArraySegment<TypedPattern, Arc<Type>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BitArraySegment<Value, Type> {
    pub location: SrcSpan,
    pub value: Box<Value>,
    pub options: Vec<BitArrayOption<Value>>,
    pub type_: Type,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub enum Endianness {
    Big,
    Little,
}

impl Endianness {
    pub fn is_big(&self) -> bool {
        *self == Endianness::Big
    }
}

impl<Value, Type> HasLocation for BitArraySegment<Value, Type> {
    fn location(&self) -> SrcSpan {
        self.location
    }
}

impl<Type> BitArraySegment<Pattern<Type>, Type> {
    /// Returns the value of the pattern unwrapping any assign pattern.
    ///
    pub fn value_unwrapping_assign(&self) -> &Pattern<Type> {
        match self.value.as_ref() {
            Pattern::Assign { pattern, .. } => pattern,
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::Variable { .. }
            | Pattern::BitArraySize { .. }
            | Pattern::Discard { .. }
            | Pattern::List { .. }
            | Pattern::Constructor { .. }
            | Pattern::Tuple { .. }
            | Pattern::BitArray { .. }
            | Pattern::StringPrefix { .. }
            | Pattern::Invalid { .. } => self.value.as_ref(),
        }
    }
}

impl<Value, Type> BitArraySegment<Value, Type> {
    #[must_use]
    pub fn has_native_option(&self) -> bool {
        self.options
            .iter()
            .any(|x| matches!(x, BitArrayOption::Native { .. }))
    }

    #[must_use]
    pub fn has_utf16_codepoint_option(&self) -> bool {
        self.options
            .iter()
            .any(|x| matches!(x, BitArrayOption::Utf16Codepoint { .. }))
    }

    #[must_use]
    pub fn has_utf32_codepoint_option(&self) -> bool {
        self.options
            .iter()
            .any(|x| matches!(x, BitArrayOption::Utf32Codepoint { .. }))
    }

    #[must_use]
    pub fn has_utf16_option(&self) -> bool {
        self.options
            .iter()
            .any(|x| matches!(x, BitArrayOption::Utf16 { .. }))
    }

    #[must_use]
    pub fn has_utf32_option(&self) -> bool {
        self.options
            .iter()
            .any(|x| matches!(x, BitArrayOption::Utf32 { .. }))
    }

    pub fn endianness(&self) -> Endianness {
        if self
            .options
            .iter()
            .any(|x| matches!(x, BitArrayOption::Little { .. }))
        {
            Endianness::Little
        } else {
            Endianness::Big
        }
    }

    pub(crate) fn signed(&self) -> bool {
        self.options
            .iter()
            .any(|x| matches!(x, BitArrayOption::Signed { .. }))
    }

    pub fn size(&self) -> Option<&Value> {
        self.options.iter().find_map(|x| match x {
            BitArrayOption::Size { value, .. } => Some(value.as_ref()),
            _ => None,
        })
    }

    pub fn unit(&self) -> u8 {
        self.options
            .iter()
            .find_map(|option| match option {
                BitArrayOption::Unit { value, .. } => Some(*value),
                BitArrayOption::Bytes { .. } => Some(8),
                _ => None,
            })
            .unwrap_or(1)
    }

    pub(crate) fn has_bits_option(&self) -> bool {
        self.options.iter().any(|option| match option {
            BitArrayOption::Bits { .. } => true,
            _ => false,
        })
    }

    pub(crate) fn has_bytes_option(&self) -> bool {
        self.options.iter().any(|option| match option {
            BitArrayOption::Bytes { .. } => true,
            _ => false,
        })
    }
}

impl<Value, Type> BitArraySegment<Value, Type> {
    #[must_use]
    pub(crate) fn has_type_option(&self) -> bool {
        self.options.iter().any(|option| option.is_type_option())
    }
}

impl TypedExprBitArraySegment {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.value.find_node(byte_index)
    }

    fn syntactically_eq(&self, other: &Self) -> bool {
        self.value.syntactically_eq(&other.value)
            && pairwise_all(&self.options, &other.options, |(option, other_option)| {
                option.syntactically_eq(other_option, |size, other_size| {
                    size.syntactically_eq(other_size)
                })
            })
    }
}

impl<TypedValue> BitArraySegment<TypedValue, Arc<Type>>
where
    TypedValue: HasType + HasLocation + Clone + bit_array::GetLiteralValue,
{
    pub(crate) fn check_for_truncated_value(&self) -> Option<BitArraySegmentTruncation> {
        // Both the size and the value must be two compile-time known constants.
        let segment_bits = self.bits_size()?.to_i64()?;
        let literal_value = self.value.as_int_literal()?;
        if segment_bits <= 0 {
            return None;
        }

        let safe_range = match literal_value.sign() {
            Sign::NoSign => return None,
            Sign::Minus => {
                (-(BigInt::one() << (segment_bits - 1)))
                    ..((BigInt::one() << (segment_bits - 1)) - 1)
            }
            Sign::Plus => BigInt::ZERO..(BigInt::one() << segment_bits),
        };

        if !safe_range.contains(&literal_value) {
            Some(BitArraySegmentTruncation {
                truncated_value: literal_value.clone(),
                truncated_into: truncate(&literal_value, segment_bits),
                value_location: self.value.location(),
                segment_bits,
            })
        } else {
            None
        }
    }

    /// If the segment size is a compile-time known constant this returns the
    /// segment size in bits, taking the segment's unit into consideration!
    ///
    fn bits_size(&self) -> Option<BigInt> {
        let size = match self.size() {
            None if self.type_.is_int() => 8.into(),
            None => 64.into(),
            Some(value) => value.as_int_literal()?,
        };

        let unit = self.unit();
        Some(size * unit)
    }
}

/// As Björn said, when a value is smaller than the segment's size it will be
/// truncated, only taking the first `n` bits:
///
/// > It will be silently truncated. In general, when storing value an integer
/// > `I` into a segment of size `N`, the actual value stored will be
/// > `I band ((1 bsl N) - 1)`.
///
/// <https://erlangforums.com/t/what-happens-when-a-bit-array-segment-size-is-smaller-than-its-value/4650/2?u=giacomocavalieri>
///
/// Thank you Björn!
///
fn truncate(literal_value: &BigInt, segment_bits: i64) -> BigInt {
    literal_value & ((BigInt::one() << segment_bits) - BigInt::one())
}

#[derive(serde::Deserialize, serde::Serialize, Eq, PartialEq, Clone, Debug)]
pub struct BitArraySegmentTruncation {
    /// The value that would end up being truncated.
    pub truncated_value: BigInt,
    /// What the value would be truncated into.
    pub truncated_into: BigInt,
    /// The span of the segment's value being truncated.
    pub value_location: SrcSpan,
    /// The size of the segment.
    pub segment_bits: i64,
}

impl TypedPatternBitArraySegment {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.value.find_node(byte_index).or_else(|| {
            self.options
                .iter()
                .find_map(|option| option.find_node(byte_index))
        })
    }

    fn syntactically_eq(&self, other: &Self) -> bool {
        self.value.syntactically_eq(&other.value)
            && pairwise_all(&self.options, &other.options, |(option, other_option)| {
                option.syntactically_eq(other_option, |size, other_size| {
                    size.syntactically_eq(other_size)
                })
            })
    }
}

impl TypedConstantBitArraySegment {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.value.find_node(byte_index).or_else(|| {
            self.options
                .iter()
                .find_map(|option| option.find_node(byte_index))
        })
    }

    fn syntactically_eq(&self, other: &Self) -> bool {
        self.value.syntactically_eq(&other.value)
            && pairwise_all(&self.options, &other.options, |(option, other_option)| {
                option.syntactically_eq(other_option, |size, other_size| {
                    size.syntactically_eq(other_size)
                })
            })
    }
}

pub type TypedConstantBitArraySegmentOption = BitArrayOption<TypedConstant>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BitArrayOption<Value> {
    Bytes {
        location: SrcSpan,
    },

    Int {
        location: SrcSpan,
    },

    Float {
        location: SrcSpan,
    },

    Bits {
        location: SrcSpan,
    },

    Utf8 {
        location: SrcSpan,
    },

    Utf16 {
        location: SrcSpan,
    },

    Utf32 {
        location: SrcSpan,
    },

    Utf8Codepoint {
        location: SrcSpan,
    },

    Utf16Codepoint {
        location: SrcSpan,
    },

    Utf32Codepoint {
        location: SrcSpan,
    },

    Signed {
        location: SrcSpan,
    },

    Unsigned {
        location: SrcSpan,
    },

    Big {
        location: SrcSpan,
    },

    Little {
        location: SrcSpan,
    },

    Native {
        location: SrcSpan,
    },

    Size {
        location: SrcSpan,
        value: Box<Value>,
        short_form: bool,
    },

    Unit {
        location: SrcSpan,
        value: u8,
    },
}

impl<A> BitArrayOption<A> {
    pub fn value(&self) -> Option<&A> {
        match self {
            BitArrayOption::Size { value, .. } => Some(value),
            _ => None,
        }
    }

    pub fn location(&self) -> SrcSpan {
        match self {
            BitArrayOption::Bytes { location }
            | BitArrayOption::Int { location }
            | BitArrayOption::Float { location }
            | BitArrayOption::Bits { location }
            | BitArrayOption::Utf8 { location }
            | BitArrayOption::Utf16 { location }
            | BitArrayOption::Utf32 { location }
            | BitArrayOption::Utf8Codepoint { location }
            | BitArrayOption::Utf16Codepoint { location }
            | BitArrayOption::Utf32Codepoint { location }
            | BitArrayOption::Signed { location }
            | BitArrayOption::Unsigned { location }
            | BitArrayOption::Big { location }
            | BitArrayOption::Little { location }
            | BitArrayOption::Native { location }
            | BitArrayOption::Size { location, .. }
            | BitArrayOption::Unit { location, .. } => *location,
        }
    }

    pub fn label(&self) -> EcoString {
        match self {
            BitArrayOption::Bytes { .. } => "bytes".into(),
            BitArrayOption::Int { .. } => "int".into(),
            BitArrayOption::Float { .. } => "float".into(),
            BitArrayOption::Bits { .. } => "bits".into(),
            BitArrayOption::Utf8 { .. } => "utf8".into(),
            BitArrayOption::Utf16 { .. } => "utf16".into(),
            BitArrayOption::Utf32 { .. } => "utf32".into(),
            BitArrayOption::Utf8Codepoint { .. } => "utf8_codepoint".into(),
            BitArrayOption::Utf16Codepoint { .. } => "utf16_codepoint".into(),
            BitArrayOption::Utf32Codepoint { .. } => "utf32_codepoint".into(),
            BitArrayOption::Signed { .. } => "signed".into(),
            BitArrayOption::Unsigned { .. } => "unsigned".into(),
            BitArrayOption::Big { .. } => "big".into(),
            BitArrayOption::Little { .. } => "little".into(),
            BitArrayOption::Native { .. } => "native".into(),
            BitArrayOption::Size { .. } => "size".into(),
            BitArrayOption::Unit { .. } => "unit".into(),
        }
    }

    fn is_type_option(&self) -> bool {
        match self {
            BitArrayOption::Bytes { .. }
            | BitArrayOption::Int { .. }
            | BitArrayOption::Float { .. }
            | BitArrayOption::Bits { .. }
            | BitArrayOption::Utf8 { .. }
            | BitArrayOption::Utf16 { .. }
            | BitArrayOption::Utf32 { .. }
            | BitArrayOption::Utf8Codepoint { .. }
            | BitArrayOption::Utf16Codepoint { .. }
            | BitArrayOption::Utf32Codepoint { .. } => true,

            BitArrayOption::Signed { .. }
            | BitArrayOption::Unsigned { .. }
            | BitArrayOption::Big { .. }
            | BitArrayOption::Little { .. }
            | BitArrayOption::Native { .. }
            | BitArrayOption::Size { .. }
            | BitArrayOption::Unit { .. } => false,
        }
    }

    fn syntactically_eq(&self, other: &Self, compare_sizes: impl Fn(&A, &A) -> bool) -> bool {
        match (self, other) {
            (BitArrayOption::Bytes { .. }, BitArrayOption::Bytes { .. }) => true,
            (BitArrayOption::Bytes { .. }, _) => false,

            (BitArrayOption::Int { .. }, BitArrayOption::Int { .. }) => true,
            (BitArrayOption::Int { .. }, _) => false,

            (BitArrayOption::Float { .. }, BitArrayOption::Float { .. }) => true,
            (BitArrayOption::Float { .. }, _) => false,

            (BitArrayOption::Bits { .. }, BitArrayOption::Bits { .. }) => true,
            (BitArrayOption::Bits { .. }, _) => false,

            (BitArrayOption::Utf8 { .. }, BitArrayOption::Utf8 { .. }) => true,
            (BitArrayOption::Utf8 { .. }, _) => false,

            (BitArrayOption::Utf16 { .. }, BitArrayOption::Utf16 { .. }) => true,
            (BitArrayOption::Utf16 { .. }, _) => false,

            (BitArrayOption::Utf32 { .. }, BitArrayOption::Utf32 { .. }) => true,
            (BitArrayOption::Utf32 { .. }, _) => false,

            (BitArrayOption::Utf8Codepoint { .. }, BitArrayOption::Utf8Codepoint { .. }) => true,
            (BitArrayOption::Utf8Codepoint { .. }, _) => false,

            (BitArrayOption::Utf16Codepoint { .. }, BitArrayOption::Utf16Codepoint { .. }) => true,
            (BitArrayOption::Utf16Codepoint { .. }, _) => false,

            (BitArrayOption::Utf32Codepoint { .. }, BitArrayOption::Utf32Codepoint { .. }) => true,
            (BitArrayOption::Utf32Codepoint { .. }, _) => false,

            (BitArrayOption::Signed { .. }, BitArrayOption::Signed { .. }) => true,
            (BitArrayOption::Signed { .. }, _) => false,

            (BitArrayOption::Unsigned { .. }, BitArrayOption::Unsigned { .. }) => true,
            (BitArrayOption::Unsigned { .. }, _) => false,

            (BitArrayOption::Big { .. }, BitArrayOption::Big { .. }) => true,
            (BitArrayOption::Big { .. }, _) => false,

            (BitArrayOption::Little { .. }, BitArrayOption::Little { .. }) => true,
            (BitArrayOption::Little { .. }, _) => false,

            (BitArrayOption::Native { .. }, BitArrayOption::Native { .. }) => true,
            (BitArrayOption::Native { .. }, _) => false,

            (
                BitArrayOption::Unit { value, .. },
                BitArrayOption::Unit {
                    value: other_value, ..
                },
            ) => value == other_value,
            (BitArrayOption::Unit { .. }, _) => false,

            (
                BitArrayOption::Size {
                    value, short_form, ..
                },
                BitArrayOption::Size {
                    value: other_value,
                    short_form: other_short_form,
                    ..
                },
            ) => short_form == other_short_form && compare_sizes(value, other_value),
            (BitArrayOption::Size { .. }, _) => false,
        }
    }
}

impl BitArrayOption<TypedConstant> {
    fn referenced_variables(&self) -> im::HashSet<&EcoString> {
        match self {
            BitArrayOption::Bytes { .. }
            | BitArrayOption::Int { .. }
            | BitArrayOption::Float { .. }
            | BitArrayOption::Bits { .. }
            | BitArrayOption::Utf8 { .. }
            | BitArrayOption::Utf16 { .. }
            | BitArrayOption::Utf32 { .. }
            | BitArrayOption::Utf8Codepoint { .. }
            | BitArrayOption::Utf16Codepoint { .. }
            | BitArrayOption::Utf32Codepoint { .. }
            | BitArrayOption::Signed { .. }
            | BitArrayOption::Unsigned { .. }
            | BitArrayOption::Big { .. }
            | BitArrayOption::Little { .. }
            | BitArrayOption::Unit { .. }
            | BitArrayOption::Native { .. } => im::hashset![],

            BitArrayOption::Size { value, .. } => value.referenced_variables(),
        }
    }
}

impl BitArrayOption<TypedPattern> {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        match self {
            BitArrayOption::Bytes { .. }
            | BitArrayOption::Int { .. }
            | BitArrayOption::Float { .. }
            | BitArrayOption::Bits { .. }
            | BitArrayOption::Utf8 { .. }
            | BitArrayOption::Utf16 { .. }
            | BitArrayOption::Utf32 { .. }
            | BitArrayOption::Utf8Codepoint { .. }
            | BitArrayOption::Utf16Codepoint { .. }
            | BitArrayOption::Utf32Codepoint { .. }
            | BitArrayOption::Signed { .. }
            | BitArrayOption::Unsigned { .. }
            | BitArrayOption::Big { .. }
            | BitArrayOption::Little { .. }
            | BitArrayOption::Native { .. }
            | BitArrayOption::Unit { .. } => None,
            BitArrayOption::Size { value, .. } => value.find_node(byte_index),
        }
    }
}

impl BitArrayOption<TypedConstant> {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        match self {
            BitArrayOption::Bytes { .. }
            | BitArrayOption::Int { .. }
            | BitArrayOption::Float { .. }
            | BitArrayOption::Bits { .. }
            | BitArrayOption::Utf8 { .. }
            | BitArrayOption::Utf16 { .. }
            | BitArrayOption::Utf32 { .. }
            | BitArrayOption::Utf8Codepoint { .. }
            | BitArrayOption::Utf16Codepoint { .. }
            | BitArrayOption::Utf32Codepoint { .. }
            | BitArrayOption::Signed { .. }
            | BitArrayOption::Unsigned { .. }
            | BitArrayOption::Big { .. }
            | BitArrayOption::Little { .. }
            | BitArrayOption::Native { .. }
            | BitArrayOption::Unit { .. } => None,
            BitArrayOption::Size { value, .. } => value.find_node(byte_index),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum TodoKind {
    Keyword,
    EmptyFunction { function_location: SrcSpan },
    IncompleteUse,
    EmptyBlock,
}

#[derive(Debug, Default)]
pub struct GroupedDefinitions {
    pub functions: Vec<UntypedFunction>,
    pub constants: Vec<UntypedModuleConstant>,
    pub custom_types: Vec<UntypedCustomType>,
    pub imports: Vec<UntypedImport>,
    pub type_aliases: Vec<UntypedTypeAlias>,
}

impl GroupedDefinitions {
    pub fn new(definitions: impl IntoIterator<Item = UntypedDefinition>) -> Self {
        let mut this = Self::default();

        for definition in definitions {
            this.add(definition)
        }

        this
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        let Self {
            custom_types,
            functions,
            constants,
            imports,
            type_aliases,
        } = self;
        functions.len() + constants.len() + imports.len() + custom_types.len() + type_aliases.len()
    }

    fn add(&mut self, statement: UntypedDefinition) {
        match statement {
            Definition::Import(import) => self.imports.push(import),
            Definition::Function(function) => self.functions.push(function),
            Definition::TypeAlias(type_alias) => self.type_aliases.push(type_alias),
            Definition::CustomType(custom_type) => self.custom_types.push(custom_type),
            Definition::ModuleConstant(constant) => self.constants.push(constant),
        }
    }
}

/// A statement with in a function body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement<TypeT, ExpressionT> {
    /// A bare expression that is not assigned to any variable.
    Expression(ExpressionT),
    /// Assigning an expression to variables using a pattern.
    Assignment(Box<Assignment<TypeT, ExpressionT>>),
    /// A `use` expression.
    Use(Use<TypeT, ExpressionT>),
    /// A bool assertion.
    Assert(Assert<ExpressionT>),
}

pub type UntypedUse = Use<(), UntypedExpr>;
pub type TypedUse = Use<Arc<Type>, TypedExpr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Use<TypeT, ExpressionT> {
    /// In an untyped use this is the expression with the untyped code of the
    /// callback function.
    ///
    /// In a typed use this is the typed function call the use expression
    /// desugars to.
    ///
    pub call: Box<ExpressionT>,

    /// This is the location of the whole use line, starting from the `use`
    /// keyword and ending with the function call on the right hand side of
    /// `<-`.
    ///
    /// ```gleam
    /// use a <- result.try(result)
    /// ^^^^^^^^^^^^^^^^^^^^^^^^^^^
    /// ```
    ///
    pub location: SrcSpan,

    /// This is the location of the expression on the right hand side of the use
    /// arrow.
    ///
    /// ```gleam
    /// use a <- result.try(result)
    ///          ^^^^^^^^^^^^^^^^^^
    /// ```
    ///
    pub right_hand_side_location: SrcSpan,

    /// This is the SrcSpan of the patterns you find on the left hand side of
    /// `<-` in a use expression.
    ///
    /// ```gleam
    /// use pattern1, pattern2 <- todo
    ///     ^^^^^^^^^^^^^^^^^^
    /// ```
    ///
    /// In case there's no patterns it will be corresponding to the SrcSpan of
    /// the `use` keyword itself.
    ///
    pub assignments_location: SrcSpan,

    /// The patterns on the left hand side of `<-` in a use expression.
    ///
    pub assignments: Vec<UseAssignment<TypeT>>,
}

pub type UntypedUseAssignment = UseAssignment<()>;
pub type TypedUseAssignment = UseAssignment<Arc<Type>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseAssignment<TypeT> {
    pub location: SrcSpan,
    pub pattern: Pattern<TypeT>,
    pub annotation: Option<TypeAst>,
}

impl TypedUse {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        for assignment in self.assignments.iter() {
            if let Some(found) = assignment.pattern.find_node(byte_index) {
                return Some(found);
            }
            if let Some(found) = assignment
                .annotation
                .as_ref()
                .and_then(|annotation| annotation.find_node(byte_index, assignment.pattern.type_()))
            {
                return Some(found);
            }
        }
        self.call.find_node(byte_index)
    }

    pub fn callback_arguments(&self) -> Option<&Vec<TypedArg>> {
        let TypedExpr::Call { arguments, .. } = self.call.as_ref() else {
            return None;
        };
        let callback = arguments.iter().last()?;
        let TypedExpr::Fn { arguments, .. } = &callback.value else {
            // The expression might be invalid so we have to return a None here
            return None;
        };
        Some(arguments)
    }
}

pub type TypedStatement = Statement<Arc<Type>, TypedExpr>;
pub type UntypedStatement = Statement<(), UntypedExpr>;

impl<T, E> Statement<T, E> {
    /// Returns `true` if the statement is [`Expression`].
    ///
    /// [`Expression`]: Statement::Expression
    #[must_use]
    pub fn is_expression(&self) -> bool {
        matches!(self, Self::Expression(..))
    }

    #[must_use]
    pub(crate) fn is_use(&self) -> bool {
        match self {
            Self::Use(_) => true,
            _ => false,
        }
    }
}

impl UntypedStatement {
    pub fn location(&self) -> SrcSpan {
        match self {
            Statement::Expression(expression) => expression.location(),
            Statement::Assignment(assignment) => assignment.location,
            Statement::Use(use_) => use_.location.merge(&use_.call.location()),
            Statement::Assert(assert) => assert.location,
        }
    }

    pub fn start_byte_index(&self) -> u32 {
        match self {
            Statement::Expression(expression) => expression.start_byte_index(),
            Statement::Assignment(assignment) => assignment.location.start,
            Statement::Use(use_) => use_.location.start,
            Statement::Assert(assert) => assert.location.start,
        }
    }
}

impl TypedStatement {
    pub fn is_println(&self) -> bool {
        match self {
            Statement::Expression(e) => e.is_println(),
            Statement::Assignment(_) => false,
            Statement::Use(_) => false,
            Statement::Assert(_) => false,
        }
    }

    pub fn location(&self) -> SrcSpan {
        match self {
            Statement::Expression(expression) => expression.location(),
            Statement::Assignment(assignment) => assignment.location,
            // A use statement covers the entire block: `use_.location` covers
            // just the use's first line and not what comes after it.
            Statement::Use(use_) => use_.location.merge(&use_.call.location()),
            Statement::Assert(assert) => assert.location,
        }
    }

    /// Returns the location of the last element of a statement. This means that
    /// if the statement is a use you'll get the location of the last item at
    /// the end of its block.
    pub fn last_location(&self) -> SrcSpan {
        match self {
            Statement::Expression(expression) => expression.last_location(),
            Statement::Assignment(assignment) => assignment.value.last_location(),
            Statement::Use(use_) => use_.call.last_location(),
            Statement::Assert(assert) => assert.value.last_location(),
        }
    }

    pub fn type_(&self) -> Arc<Type> {
        match self {
            Statement::Expression(expression) => expression.type_(),
            Statement::Assignment(assignment) => assignment.type_(),
            Statement::Use(use_) => use_.call.type_(),
            Statement::Assert(_) => nil(),
        }
    }

    pub fn definition_location(&self) -> Option<DefinitionLocation> {
        match self {
            Statement::Expression(expression) => expression.definition_location(),
            Statement::Assignment(_) => None,
            Statement::Use(use_) => use_.call.definition_location(),
            Statement::Assert(_) => None,
        }
    }

    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        match self {
            Statement::Use(use_) => use_.find_node(byte_index),
            Statement::Expression(expression) => expression.find_node(byte_index),
            Statement::Assignment(assignment) => assignment.find_node(byte_index).or_else(|| {
                if assignment.location.contains(byte_index) {
                    Some(Located::Statement(self))
                } else {
                    None
                }
            }),
            Statement::Assert(assert) => assert.find_node(byte_index).or_else(|| {
                if assert.location.contains(byte_index) {
                    Some(Located::Statement(self))
                } else {
                    None
                }
            }),
        }
    }

    pub fn find_statement(&self, byte_index: u32) -> Option<&TypedStatement> {
        match self {
            Statement::Use(use_) => use_.call.find_statement(byte_index),
            Statement::Expression(expression) => expression.find_statement(byte_index),
            Statement::Assignment(assignment) => {
                assignment.value.find_statement(byte_index).or_else(|| {
                    if assignment.location.contains(byte_index) {
                        Some(self)
                    } else {
                        None
                    }
                })
            }
            Statement::Assert(assert) => assert.value.find_statement(byte_index).or_else(|| {
                if assert.location.contains(byte_index) {
                    Some(self)
                } else {
                    None
                }
            }),
        }
    }

    pub fn type_defining_location(&self) -> SrcSpan {
        match self {
            Statement::Expression(expression) => expression.type_defining_location(),
            Statement::Assignment(assignment) => assignment.location,
            Statement::Use(use_) => use_.location,
            Statement::Assert(assert) => assert.location,
        }
    }

    fn is_pure_value_constructor(&self) -> bool {
        match self {
            Statement::Expression(expression) => expression.is_pure_value_constructor(),
            Statement::Assignment(assignment) => {
                // A let assert is not considered a pure value constructor
                // as it could crash the program!
                !assignment.kind.is_assert() && assignment.value.is_pure_value_constructor()
            }
            Statement::Use(Use { call, .. }) => call.is_pure_value_constructor(),
            // Assert statements by definition are not pure
            Statement::Assert(_) => false,
        }
    }

    fn syntactically_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Statement::Expression(one), Statement::Expression(other)) => {
                one.syntactically_eq(other)
            }
            (Statement::Expression(_), _) => false,

            (Statement::Assignment(one), Statement::Assignment(other)) => {
                one.pattern.syntactically_eq(&other.pattern)
                    && one.value.syntactically_eq(&other.value)
            }
            (Statement::Assignment(_), _) => false,

            (Statement::Use(one), Statement::Use(other)) => one.call.syntactically_eq(&other.call),
            (Statement::Use(_), _) => false,

            (Statement::Assert(one), Statement::Assert(other)) => {
                let messages_are_equal = match (&one.message, &other.message) {
                    (None, None) => true,
                    (None, Some(_)) | (Some(_), None) => false,
                    (Some(one), Some(other)) => one.syntactically_eq(other),
                };
                messages_are_equal && one.value.syntactically_eq(&other.value)
            }
            (Statement::Assert(_), _) => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment<TypeT, ExpressionT> {
    pub location: SrcSpan,
    pub value: ExpressionT,
    pub pattern: Pattern<TypeT>,
    pub kind: AssignmentKind<ExpressionT>,
    pub compiled_case: CompiledCase,
    /// This will be true for assignments that are automatically generated by
    /// the compiler.
    pub annotation: Option<TypeAst>,
}

pub type TypedAssignment = Assignment<Arc<Type>, TypedExpr>;
pub type UntypedAssignment = Assignment<(), UntypedExpr>;

impl TypedAssignment {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        if let Some(annotation) = &self.annotation
            && let Some(l) = annotation.find_node(byte_index, self.pattern.type_())
        {
            return Some(l);
        }
        self.pattern
            .find_node(byte_index)
            .or_else(|| self.value.find_node(byte_index))
    }

    pub fn type_(&self) -> Arc<Type> {
        self.value.type_()
    }
}

pub type TypedAssert = Assert<TypedExpr>;
pub type UntypedAssert = Assert<UntypedExpr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assert<Expression> {
    pub location: SrcSpan,
    pub value: Expression,
    pub message: Option<Expression>,
}

impl TypedAssert {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        if let Some(found) = self.value.find_node(byte_index) {
            return Some(found);
        }
        if let Some(message) = &self.message
            && let Some(found) = message.find_node(byte_index)
        {
            return Some(found);
        }
        None
    }
}

/// A pipeline is desugared to a series of assignments:
///
/// ```gleam
/// wibble |> wobble |> woo
/// ```
///
/// Becomes:
///
/// ```erl
/// Pipe1 = wibble
/// Pipe2 = wobble(Pipe1)
/// woo(Pipe2)
/// ```
///
/// This represents one of such assignments once the pipeline has been desugared
/// and each step has been typed.
///
/// > We're not using a more general `TypedAssignment` node since that has much
/// > more informations to carry around. This one is limited since we know it
/// > will always be in the form `VarName = <Expr>`, with no patterns on the
/// > left hand side of the assignment.
/// > Being more constrained simplifies code generation for pipelines!
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedPipelineAssignment {
    pub location: SrcSpan,
    pub name: EcoString,
    pub value: Box<TypedExpr>,
}

impl TypedPipelineAssignment {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.value.find_node(byte_index)
    }

    pub fn find_statement(&self, byte_index: u32) -> Option<&TypedStatement> {
        self.value.find_statement(byte_index)
    }

    pub fn type_(&self) -> Arc<Type> {
        self.value.type_()
    }
}

/// The kind of desugaring that might take place when rewriting a pipeline to
/// regular assignments.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PipelineAssignmentKind {
    /// In case `a |> b(c)` is desugared to `b(a, c)`.
    FirstArgument {
        /// The location of the second argument of the call, in case there's any:
        /// - `a |> b(c, d)`: here it's `Some` wrapping the location of `c`.
        /// - `a |> b()`: here it's `None`.
        second_argument: Option<SrcSpan>,
    },

    /// In case there's an explicit hole and `a |> b(_, c)` is desugared to
    /// `b(a, c)`.
    Hole { hole: SrcSpan },

    /// In case `a |> b(c)` is desugared to `b(c)(a)`
    FunctionCall,

    /// In case there's an echo in the middle of a pipeline `a |> echo`
    Echo,
}
