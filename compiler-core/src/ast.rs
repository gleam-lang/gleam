mod constant;
mod typed;
mod untyped;

#[cfg(test)]
mod tests;
pub mod visit;

pub use self::typed::TypedExpr;
pub use self::untyped::{FunctionLiteralKind, UntypedExpr};

pub use self::constant::{Constant, TypedConstant, UntypedConstant};

use crate::analyse::Inferred;
use crate::build::{Located, Target, module_erlang_name};
use crate::parse::SpannedString;
use crate::type_::error::VariableOrigin;
use crate::type_::expression::Implementations;
use crate::type_::printer::Names;
use crate::type_::{
    self, Deprecation, ModuleValueConstructor, PatternConstructor, Type, ValueConstructor,
};
use std::sync::Arc;

use ecow::EcoString;
use num_bigint::BigInt;
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

pub type TypedModule = Module<type_::ModuleInterface, TypedDefinition>;

pub type UntypedModule = Module<(), TargetedDefinition>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module<Info, Statements> {
    pub name: EcoString,
    pub documentation: Vec<EcoString>,
    pub type_info: Info,
    pub definitions: Vec<Statements>,
    pub names: Names,
}

impl<Info, Statements> Module<Info, Statements> {
    pub fn erlang_name(&self) -> EcoString {
        module_erlang_name(&self.name)
    }
}

impl TypedModule {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.definitions
            .iter()
            .find_map(|statement| statement.find_node(byte_index))
    }

    pub fn find_statement(&self, byte_index: u32) -> Option<&TypedStatement> {
        self.definitions
            .iter()
            .find_map(|definition| definition.find_statement(byte_index))
    }
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
        self.iter_statements(target)
            .flat_map(|s| match s {
                Definition::Import(Import {
                    module, location, ..
                }) => Some((module.clone(), *location)),
                _ => None,
            })
            .collect()
    }

    pub fn iter_statements(&self, target: Target) -> impl Iterator<Item = &UntypedDefinition> {
        self.definitions
            .iter()
            .filter(move |def| def.is_for(target))
            .map(|def| &def.definition)
    }

    pub fn into_iter_statements(self, target: Target) -> impl Iterator<Item = UntypedDefinition> {
        self.definitions
            .into_iter()
            .filter(move |def| def.is_for(target))
            .map(|def| def.definition)
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
            }) => match other {
                TypeAst::Constructor(TypeAstConstructor {
                    module: o_module,
                    name: o_name,
                    arguments: o_arguments,
                    location: _,
                    name_location: _,
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
    pub end_position: u32,
    pub name: Option<SpannedString>,
    pub arguments: Vec<Arg<T>>,
    pub body: Vec1<Statement<T, Expr>>,
    pub publicity: Publicity,
    pub deprecation: Deprecation,
    pub return_annotation: Option<TypeAst>,
    pub return_type: T,
    pub documentation: Option<(u32, EcoString)>,
    pub external_erlang: Option<(EcoString, EcoString, SrcSpan)>,
    pub external_javascript: Option<(EcoString, EcoString, SrcSpan)>,
    pub implementations: Implementations,
}

pub type TypedFunction = Function<Arc<Type>, TypedExpr>;
pub type UntypedFunction = Function<(), UntypedExpr>;

impl<T, E> Function<T, E> {
    pub fn full_location(&self) -> SrcSpan {
        SrcSpan::new(self.location.start, self.end_position)
    }
}

pub type UntypedImport = Import<()>;

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
}

impl<T> CustomType<T> {
    /// The `location` field of a `CustomType` is only the location of `pub type
    /// TheName`. This method returns a `SrcSpan` that includes the entire type
    /// definition.
    pub fn full_location(&self) -> SrcSpan {
        SrcSpan::new(self.location.start, self.end_position)
    }
}

pub type UntypedTypeAlias = TypeAlias<()>;

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

pub type TypedDefinition = Definition<Arc<Type>, TypedExpr, EcoString, EcoString>;
pub type UntypedDefinition = Definition<(), UntypedExpr, (), ()>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Definition<T, Expr, ConstantRecordTag, PackageName> {
    Function(Function<T, Expr>),

    TypeAlias(TypeAlias<T>),

    CustomType(CustomType<T>),

    Import(Import<PackageName>),

    ModuleConstant(ModuleConstant<T, ConstantRecordTag>),
}

impl TypedDefinition {
    pub fn main_function(&self) -> Option<&TypedFunction> {
        match self {
            Definition::Function(f) if f.name.as_ref().is_some_and(|(_, name)| name == "main") => {
                Some(f)
            }
            _ => None,
        }
    }

    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        match self {
            Definition::Function(function) => {
                // Search for the corresponding node inside the function
                // only if the index falls within the function's full location.
                if !function.full_location().contains(byte_index) {
                    return None;
                }

                if let Some(found) = function.body.iter().find_map(|s| s.find_node(byte_index)) {
                    return Some(found);
                }

                if let Some(found_arg) = function
                    .arguments
                    .iter()
                    .find_map(|arg| arg.find_node(byte_index))
                {
                    return Some(found_arg);
                };

                if let Some(found_statement) = function
                    .body
                    .iter()
                    .find(|statement| statement.location().contains(byte_index))
                {
                    return Some(Located::Statement(found_statement));
                };

                // Check if location is within the return annotation.
                if let Some(l) = function
                    .return_annotation
                    .iter()
                    .find_map(|a| a.find_node(byte_index, function.return_type.clone()))
                {
                    return Some(l);
                };

                // Note that the fn `.location` covers the function head, not
                // the entire statement.
                if function.location.contains(byte_index) {
                    Some(Located::ModuleStatement(self))
                } else if function.full_location().contains(byte_index) {
                    Some(Located::FunctionBody(function))
                } else {
                    None
                }
            }

            Definition::CustomType(custom) => {
                // Check if location is within the type of one of the arguments of a constructor.
                if let Some(constructor) = custom
                    .constructors
                    .iter()
                    .find(|constructor| constructor.location.contains(byte_index))
                {
                    return match constructor
                        .arguments
                        .iter()
                        .find(|arg| arg.location.contains(byte_index))
                    {
                        Some(arg) => match arg.ast.find_node(byte_index, arg.type_.clone()) {
                            Some(annotation) => Some(annotation),
                            None => Some(Located::Label(arg.location, arg.type_.clone())),
                        },
                        None => Some(Located::VariantConstructorDefinition(constructor)),
                    };
                }

                // Note that the custom type `.location` covers the function
                // head, not the entire statement.
                if custom.full_location().contains(byte_index) {
                    Some(Located::ModuleStatement(self))
                } else {
                    None
                }
            }

            Definition::TypeAlias(alias) => {
                // Check if location is within the type being aliased.
                if let Some(l) = alias.type_ast.find_node(byte_index, alias.type_.clone()) {
                    return Some(l);
                }

                if alias.location.contains(byte_index) {
                    Some(Located::ModuleStatement(self))
                } else {
                    None
                }
            }

            Definition::ModuleConstant(constant) => {
                // Check if location is within the annotation.
                if let Some(annotation) = &constant.annotation {
                    if let Some(l) = annotation.find_node(byte_index, constant.type_.clone()) {
                        return Some(l);
                    }
                }

                if constant.location.contains(byte_index) {
                    Some(Located::ModuleStatement(self))
                } else {
                    None
                }
            }

            Definition::Import(import) => {
                if self.location().contains(byte_index) {
                    if let Some(unqualified) = import
                        .unqualified_values
                        .iter()
                        .find(|i| i.location.contains(byte_index))
                    {
                        return Some(Located::UnqualifiedImport(
                            crate::build::UnqualifiedImport {
                                name: &unqualified.name,
                                module: &import.module,
                                is_type: false,
                                location: &unqualified.location,
                            },
                        ));
                    }

                    if let Some(unqualified) = import
                        .unqualified_types
                        .iter()
                        .find(|i| i.location.contains(byte_index))
                    {
                        return Some(Located::UnqualifiedImport(
                            crate::build::UnqualifiedImport {
                                name: &unqualified.name,
                                module: &import.module,
                                is_type: true,
                                location: &unqualified.location,
                            },
                        ));
                    }

                    Some(Located::ModuleStatement(self))
                } else {
                    None
                }
            }
        }
    }

    pub fn find_statement(&self, byte_index: u32) -> Option<&TypedStatement> {
        match self {
            Definition::Function(function) => {
                if !function.full_location().contains(byte_index) {
                    return None;
                }

                function
                    .body
                    .iter()
                    .find_map(|statement| statement.find_statement(byte_index))
            }

            _ => None,
        }
    }
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

    pub fn put_doc(&mut self, new_doc: (u32, EcoString)) {
        match self {
            Definition::Import(Import { .. }) => (),

            Definition::Function(Function { documentation, .. })
            | Definition::TypeAlias(TypeAlias { documentation, .. })
            | Definition::CustomType(CustomType { documentation, .. })
            | Definition::ModuleConstant(ModuleConstant { documentation, .. }) => {
                let _ = std::mem::replace(documentation, Some(new_doc));
            }
        }
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
    /// An argument adde by the compiler to fill in the missing args when using
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
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
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
            (Some(ImplicitCallArgOrigin::Use), TypedExpr::Fn { args, body, .. }) => args
                .iter()
                .find_map(|arg| arg.find_node(byte_index))
                .or_else(|| body.iter().find_map(|s| s.find_node(byte_index))),
            // In all other cases we're happy with the default behaviour.
            //
            _ => match self.value.find_node(byte_index) {
                Some(located) => Some(located),
                _ => {
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
        self.label.is_some() && self.location == self.value.location()
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
            .or_else(|| self.then.find_node(byte_index))
    }
}

pub type UntypedClauseGuard = ClauseGuard<(), ()>;
pub type TypedClauseGuard = ClauseGuard<Arc<Type>, EcoString>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClauseGuard<Type, RecordTag> {
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
        location: SrcSpan,
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
            | ClauseGuard::FieldAccess { location, .. }
            | ClauseGuard::LtEqFloat { location, .. }
            | ClauseGuard::ModuleSelect { location, .. } => *location,
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
            | ClauseGuard::ModuleSelect { .. } => None,
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
}

#[derive(Debug, PartialEq, Eq, Default, Clone, Copy, serde::Serialize, serde::Deserialize)]
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

    /// A reference to a variable in a bit array. This is always a variable
    /// being used rather than a new variable being assigned.
    /// e.g. `assert <<y:size(somevar)>> = x`
    VarUsage {
        location: SrcSpan,
        name: EcoString,
        constructor: Option<ValueConstructor>,
        type_: Type,
    },

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
        tail: Option<Box<Self>>,
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

impl Default for Inferred<()> {
    fn default() -> Self {
        Self::Unknown
    }
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
            | Pattern::VarUsage { location, .. }
            | Pattern::List { location, .. }
            | Pattern::Float { location, .. }
            | Pattern::Discard { location, .. }
            | Pattern::String { location, .. }
            | Pattern::Tuple { location, .. }
            | Pattern::Constructor { location, .. }
            | Pattern::StringPrefix { location, .. }
            | Pattern::BitArray { location, .. }
            | Pattern::Invalid { location, .. } => *location,
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
}

impl TypedPattern {
    pub fn definition_location(&self) -> Option<DefinitionLocation> {
        match self {
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::Variable { .. }
            | Pattern::VarUsage { .. }
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
            | Pattern::VarUsage { .. }
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
            Pattern::BitArray { .. } => type_::bits(),
            Pattern::StringPrefix { .. } => type_::string(),

            Pattern::Variable { type_, .. }
            | Pattern::List { type_, .. }
            | Pattern::VarUsage { type_, .. }
            | Pattern::Constructor { type_, .. }
            | Pattern::Invalid { type_, .. } => type_.clone(),

            Pattern::Assign { pattern, .. } => pattern.type_(),

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
            | Pattern::VarUsage { .. }
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

                Some(_) | None => arguments.iter().find_map(|arg| arg.find_node(byte_index)),
            },
            Pattern::List { elements, tail, .. } => elements
                .iter()
                .find_map(|p| p.find_node(byte_index))
                .or_else(|| tail.as_ref().and_then(|p| p.find_node(byte_index))),

            Pattern::Tuple { elements, .. } => {
                elements.iter().find_map(|p| p.find_node(byte_index))
            }

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
    // let x = ...
    Let,
    // let assert x = ...
    Assert {
        location: SrcSpan,
        /// The message given to the assertion:
        /// ```gleam
        /// let asset Ok(a) = something() as "This will never fail"
        /// //                                ^ Message
        /// ```
        message: Option<Box<Expression>>,
    },
    // For assignments generated by the compiler
    Generated,
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
    /// Returns `true` if the assignment kind is [`Generated`].
    ///
    /// [`Generated`]: AssignmentKind::Generated
    #[must_use]
    pub fn is_generated(&self) -> bool {
        match self {
            Self::Generated => true,
            Self::Let | Self::Assert { .. } => false,
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

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Endianness {
    Big,
    Little,
}

impl Endianness {
    pub fn is_big(&self) -> bool {
        *self == Endianness::Big
    }
}

impl<Value> BitArraySegment<Value, Arc<Type>> {
    #[must_use]
    pub fn has_native_option(&self) -> bool {
        self.options
            .iter()
            .any(|x| matches!(x, BitArrayOption::Native { .. }))
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
                _ => None,
            })
            .unwrap_or(1)
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
}

impl TypedPatternBitArraySegment {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.value.find_node(byte_index).or_else(|| {
            self.options
                .iter()
                .find_map(|option| option.find_node(byte_index))
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum TodoKind {
    Keyword,
    EmptyFunction { function_location: SrcSpan },
    IncompleteUse,
    EmptyBlock,
}

#[derive(Debug, Default)]
pub struct GroupedStatements {
    pub functions: Vec<UntypedFunction>,
    pub constants: Vec<UntypedModuleConstant>,
    pub custom_types: Vec<UntypedCustomType>,
    pub imports: Vec<UntypedImport>,
    pub type_aliases: Vec<UntypedTypeAlias>,
}

impl GroupedStatements {
    pub fn new(statements: impl IntoIterator<Item = UntypedDefinition>) -> Self {
        let mut this = Self::default();

        for statement in statements {
            this.add(statement)
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
            Definition::Import(i) => self.imports.push(i),
            Definition::Function(f) => self.functions.push(f),
            Definition::TypeAlias(t) => self.type_aliases.push(t),
            Definition::CustomType(c) => self.custom_types.push(c),
            Definition::ModuleConstant(c) => self.constants.push(c),
        }
    }
}

/// A statement with in a function body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement<TypeT, ExpressionT> {
    /// A bare expression that is not assigned to any variable.
    Expression(ExpressionT),
    /// Assigning an expression to variables using a pattern.
    Assignment(Assignment<TypeT, ExpressionT>),
    /// A `use` expression.
    Use(Use<TypeT, ExpressionT>),
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
            Statement::Use(use_) => use_.location,
        }
    }

    pub fn start_byte_index(&self) -> u32 {
        match self {
            Statement::Expression(expression) => expression.start_byte_index(),
            Statement::Assignment(assignment) => assignment.location.start,
            Statement::Use(use_) => use_.location.start,
        }
    }

    pub fn is_placeholder(&self) -> bool {
        match self {
            Statement::Expression(expression) => expression.is_placeholder(),
            Statement::Assignment(_) | Statement::Use(_) => false,
        }
    }
}

impl TypedStatement {
    pub fn is_println(&self) -> bool {
        match self {
            Statement::Expression(e) => e.is_println(),
            Statement::Assignment(_) => false,
            Statement::Use(_) => false,
        }
    }

    pub fn location(&self) -> SrcSpan {
        match self {
            Statement::Expression(expression) => expression.location(),
            Statement::Assignment(assignment) => assignment.location,
            Statement::Use(use_) => use_.location,
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
        }
    }

    pub fn type_(&self) -> Arc<Type> {
        match self {
            Statement::Expression(expression) => expression.type_(),
            Statement::Assignment(assignment) => assignment.type_(),
            Statement::Use(_use) => _use.call.type_(),
        }
    }

    pub fn definition_location(&self) -> Option<DefinitionLocation> {
        match self {
            Statement::Expression(expression) => expression.definition_location(),
            Statement::Assignment(_) => None,
            Statement::Use(use_) => use_.call.definition_location(),
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
        }
    }

    pub fn type_defining_location(&self) -> SrcSpan {
        match self {
            Statement::Expression(expression) => expression.type_defining_location(),
            Statement::Assignment(assignment) => assignment.location,
            Statement::Use(use_) => use_.location,
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
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment<TypeT, ExpressionT> {
    pub location: SrcSpan,
    pub value: Box<ExpressionT>,
    pub pattern: Pattern<TypeT>,
    pub kind: AssignmentKind<ExpressionT>,
    pub annotation: Option<TypeAst>,
}

pub type TypedAssignment = Assignment<Arc<Type>, TypedExpr>;
pub type UntypedAssignment = Assignment<(), UntypedExpr>;

impl TypedAssignment {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        if let Some(annotation) = &self.annotation {
            if let Some(l) = annotation.find_node(byte_index, self.pattern.type_()) {
                return Some(l);
            }
        }
        self.pattern
            .find_node(byte_index)
            .or_else(|| self.value.find_node(byte_index))
    }

    pub fn type_(&self) -> Arc<Type> {
        self.value.type_()
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
