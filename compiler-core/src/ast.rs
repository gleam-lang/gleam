mod constant;
mod typed;
mod untyped;

#[cfg(test)]
mod tests;

pub use self::typed::TypedExpr;
pub use self::untyped::{UntypedExpr, Use};

pub use self::constant::{Constant, TypedConstant, UntypedConstant};

use crate::analyse::Inferred;
use crate::build::{Located, Target};
use crate::type_::expression::Implementations;
use crate::type_::{
    self, Deprecation, ModuleValueConstructor, PatternConstructor, Type, ValueConstructor,
};
use std::sync::Arc;

use ecow::EcoString;
#[cfg(test)]
use pretty_assertions::assert_eq;
use vec1::Vec1;

pub const TRY_VARIABLE: &str = "_try";
pub const PIPE_VARIABLE: &str = "_pipe";
pub const USE_ASSIGNMENT_VARIABLE: &str = "_use";
pub const ASSERT_FAIL_VARIABLE: &str = "_assert_fail";
pub const ASSERT_SUBJECT_VARIABLE: &str = "_assert_subject";
pub const CAPTURE_VARIABLE: &str = "_capture";

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
}

impl TypedModule {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.definitions
            .iter()
            .find_map(|statement| statement.find_node(byte_index))
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
        "import one
         @target(erlang)
         import two

         @target(javascript)
         import three

         import four",
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
}

impl TypedArg {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        if self.location.contains(byte_index) {
            Some(Located::Arg(self))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgNames {
    Discard { name: EcoString },
    LabelledDiscard { label: EcoString, name: EcoString },
    Named { name: EcoString },
    NamedLabelled { name: EcoString, label: EcoString },
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
            ArgNames::NamedLabelled { name, .. } | ArgNames::Named { name } => Some(name),
        }
    }
}

pub type TypedRecordConstructor = RecordConstructor<Arc<Type>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordConstructor<T> {
    pub location: SrcSpan,
    pub name: EcoString,
    pub arguments: Vec<RecordConstructorArg<T>>,
    pub documentation: Option<EcoString>,
}

impl<A> RecordConstructor<A> {
    pub fn put_doc(&mut self, new_doc: EcoString) {
        self.documentation = Some(new_doc);
    }
}

pub type TypedRecordConstructorArg = RecordConstructorArg<Arc<Type>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordConstructorArg<T> {
    pub label: Option<EcoString>,
    pub ast: TypeAst,
    pub location: SrcSpan,
    pub type_: T,
    pub doc: Option<EcoString>,
}

impl<T: PartialEq> RecordConstructorArg<T> {
    pub fn put_doc(&mut self, new_doc: EcoString) {
        self.doc = Some(new_doc);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAstConstructor {
    pub location: SrcSpan,
    pub module: Option<EcoString>,
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
    pub elems: Vec<TypeAst>,
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
            }) => match other {
                TypeAst::Constructor(TypeAstConstructor {
                    module: o_module,
                    name: o_name,
                    arguments: o_arguments,
                    location: _,
                }) => {
                    module == o_module
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
            TypeAst::Tuple(TypeAstTuple { elems, location: _ }) => match other {
                TypeAst::Tuple(TypeAstTuple {
                    elems: o_elems,
                    location: _,
                }) => {
                    elems.len() == o_elems.len()
                        && elems
                            .iter()
                            .zip(o_elems)
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A function definition
///
/// # Example(s)
///
/// ```gleam
/// // Public function
/// pub fn bar() -> String { ... }
/// // Private function
/// fn foo(x: Int) -> Int { ... }
/// ```
pub struct Function<T, Expr> {
    pub location: SrcSpan,
    pub end_position: u32,
    pub name: EcoString,
    pub arguments: Vec<Arg<T>>,
    pub body: Vec1<Statement<T, Expr>>,
    pub public: bool,
    pub deprecation: Deprecation,
    pub return_annotation: Option<TypeAst>,
    pub return_type: T,
    pub documentation: Option<EcoString>,
    pub external_erlang: Option<(EcoString, EcoString)>,
    pub external_javascript: Option<(EcoString, EcoString)>,
    pub implementations: Implementations,
}

pub type TypedFunction = Function<Arc<Type>, TypedExpr>;
pub type UntypedFunction = Function<(), UntypedExpr>;

impl<T, E> Function<T, E> {
    fn full_location(&self) -> SrcSpan {
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
            None => self.module.split('/').last().map(EcoString::from),
        }
    }

    pub(crate) fn alias_location(&self) -> Option<SrcSpan> {
        self.as_name.as_ref().map(|(_, location)| *location)
    }
}

pub type UntypedModuleConstant = ModuleConstant<(), ()>;

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
    pub documentation: Option<EcoString>,
    pub location: SrcSpan,
    pub public: bool,
    pub name: EcoString,
    pub annotation: Option<TypeAst>,
    pub value: Box<Constant<T, ConstantRecordTag>>,
    pub type_: T,
    pub deprecation: Deprecation,
    pub implementations: Implementations,
}

pub type UntypedCustomType = CustomType<()>;

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
    pub public: bool,
    pub constructors: Vec<RecordConstructor<T>>,
    pub documentation: Option<EcoString>,
    pub deprecation: Deprecation,
    pub opaque: bool,
    /// The names of the type parameters.
    pub parameters: Vec<EcoString>,
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
    pub parameters: Vec<EcoString>,
    pub type_ast: TypeAst,
    pub type_: T,
    pub public: bool,
    pub documentation: Option<EcoString>,
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
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        match self {
            Definition::Function(function) => {
                if let Some(found) = function.body.iter().find_map(|s| s.find_node(byte_index)) {
                    return Some(found);
                };

                if let Some(found_arg) = function
                    .arguments
                    .iter()
                    .find(|arg| arg.location.contains(byte_index))
                {
                    return Some(Located::Arg(found_arg));
                };

                if let Some(found_statement) = function
                    .body
                    .iter()
                    .find(|statement| statement.location().contains(byte_index))
                {
                    return Some(Located::Statement(found_statement));
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
                // Note that the custom type `.location` covers the function
                // head, not the entire statement.
                if custom.full_location().contains(byte_index) {
                    Some(Located::ModuleStatement(self))
                } else {
                    None
                }
            }

            Definition::TypeAlias(_) | Definition::Import(_) | Definition::ModuleConstant(_) => {
                if self.location().contains(byte_index) {
                    Some(Located::ModuleStatement(self))
                } else {
                    None
                }
            }
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

    pub fn put_doc(&mut self, new_doc: EcoString) {
        match self {
            Definition::Import(Import { .. }) => (),

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
            }) => {
                let _ = std::mem::replace(doc, Some(new_doc));
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnqualifiedImport {
    pub location: SrcSpan,
    pub name: EcoString,
    pub as_name: Option<EcoString>,
}

impl UnqualifiedImport {
    pub fn used_name(&self) -> &EcoString {
        self.as_name.as_ref().unwrap_or(&self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Default)]
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
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CallArg<A> {
    pub label: Option<EcoString>,
    pub location: SrcSpan,
    pub value: A,
    // This is true if this argument is given as the callback in a `use`
    // expression. In future it may also be true for pipes too. It is used to
    // determine if we should error if an argument without a label is given or
    // not, which is not permitted if the argument is given explicitly by the
    // programmer rather than implicitly by Gleam's syntactic sugar.
    pub implicit: bool,
}

impl CallArg<TypedExpr> {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.value.find_node(byte_index)
    }
}

impl CallArg<TypedPattern> {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.value.find_node(byte_index)
    }
}

impl CallArg<UntypedExpr> {
    pub fn is_capture_hole(&self) -> bool {
        match &self.value {
            UntypedExpr::Var { ref name, .. } => name == CAPTURE_VARIABLE,
            _ => false,
        }
    }
}

impl<T> HasLocation for CallArg<T> {
    fn location(&self) -> SrcSpan {
        self.location
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordUpdateSpread {
    pub base: Box<UntypedExpr>,
    pub location: SrcSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UntypedRecordUpdateArg {
    pub label: EcoString,
    pub location: SrcSpan,
    pub value: UntypedExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedRecordUpdateArg {
    pub label: EcoString,
    pub location: SrcSpan,
    pub value: TypedExpr,
    pub index: u32,
}

impl TypedRecordUpdateArg {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.value.find_node(byte_index)
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

#[derive(Debug, PartialEq, Eq, Default, Clone, Copy)]
pub struct SrcSpan {
    pub start: u32,
    pub end: u32,
}

impl SrcSpan {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub fn contains(&self, byte_index: u32) -> bool {
        byte_index >= self.start && byte_index < self.end
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DefinitionLocation<'module> {
    pub module: Option<&'module str>,
    pub span: SrcSpan,
}

pub type UntypedPattern = Pattern<()>;
pub type TypedPattern = Pattern<Arc<Type>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern<Type> {
    Int {
        location: SrcSpan,
        value: EcoString,
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
        name: EcoString,
        arguments: Vec<CallArg<Self>>,
        module: Option<EcoString>,
        constructor: Inferred<PatternConstructor>,
        with_spread: bool,
        type_: Type,
    },

    Tuple {
        location: SrcSpan,
        elems: Vec<Self>,
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
}

impl Default for Inferred<()> {
    fn default() -> Self {
        Self::Unknown
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignName {
    Variable(EcoString),
    Discard(EcoString),
}

impl AssignName {
    pub fn name(&self) -> &str {
        match self {
            AssignName::Variable(name) | AssignName::Discard(name) => name,
        }
    }

    pub fn to_arg_names(self) -> ArgNames {
        match self {
            AssignName::Variable(name) => ArgNames::Named { name },
            AssignName::Discard(name) => ArgNames::Discard { name },
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
            Pattern::Assign { pattern, .. } => pattern.location(),
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
            | Pattern::BitArray { location, .. } => *location,
        }
    }

    /// Returns `true` if the pattern is [`Discard`].
    ///
    /// [`Discard`]: Pattern::Discard
    pub fn is_discard(&self) -> bool {
        matches!(self, Self::Discard { .. })
    }
}

impl TypedPattern {
    pub fn definition_location(&self) -> Option<DefinitionLocation<'_>> {
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
            | Pattern::StringPrefix { .. } => None,

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
            | Pattern::StringPrefix { .. } => None,

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
            | Pattern::Constructor { type_, .. } => type_.clone(),

            Pattern::Assign { pattern, .. } => pattern.type_(),

            Pattern::Discard { type_, .. } => type_.clone(),

            Pattern::Tuple { elems, .. } => type_::tuple(elems.iter().map(|p| p.type_()).collect()),
        }
    }

    fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        if !self.location().contains(byte_index) {
            return None;
        }

        match self {
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::Variable { .. }
            | Pattern::VarUsage { .. }
            | Pattern::Assign { .. }
            | Pattern::Discard { .. }
            | Pattern::BitArray { .. }
            | Pattern::StringPrefix { .. } => Some(Located::Pattern(self)),

            Pattern::Constructor { arguments, .. } => {
                arguments.iter().find_map(|arg| arg.find_node(byte_index))
            }
            Pattern::List { elements, tail, .. } => elements
                .iter()
                .find_map(|p| p.find_node(byte_index))
                .or_else(|| tail.as_ref().and_then(|p| p.find_node(byte_index))),

            Pattern::Tuple { elems, .. } => elems.iter().find_map(|p| p.find_node(byte_index)),
        }
        .or(Some(Located::Pattern(self)))
    }
}
impl<A> HasLocation for Pattern<A> {
    fn location(&self) -> SrcSpan {
        self.location()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentKind {
    // let x = ...
    Let,
    // let assert x = ...
    Assert,
}

impl AssignmentKind {
    pub(crate) fn performs_exhaustiveness_check(&self) -> bool {
        match self {
            AssignmentKind::Let => true,
            AssignmentKind::Assert => false,
        }
    }

    /// Returns `true` if the assignment kind is [`Assert`].
    ///
    /// [`Assert`]: AssignmentKind::Assert
    #[must_use]
    pub fn is_assert(&self) -> bool {
        matches!(self, Self::Assert)
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

impl TypedExprBitArraySegment {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.value.find_node(byte_index)
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TodoKind {
    Keyword,
    EmptyFunction,
    IncompleteUse,
}

#[derive(Debug, Default)]
pub struct GroupedStatements {
    pub functions: Vec<Function<(), UntypedExpr>>,
    pub constants: Vec<UntypedModuleConstant>,
    pub custom_types: Vec<CustomType<()>>,
    pub imports: Vec<Import<()>>,
    pub type_aliases: Vec<TypeAlias<()>>,
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
    Use(Use),
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
    pub fn is_non_pipe_expression(&self) -> bool {
        match self {
            Statement::Expression(expression) => !expression.is_pipeline(),
            _ => false,
        }
    }

    pub fn location(&self) -> SrcSpan {
        match self {
            Statement::Expression(expression) => expression.location(),
            Statement::Assignment(assignment) => assignment.location,
            Statement::Use(use_) => use_.location,
        }
    }

    pub fn type_(&self) -> Arc<Type> {
        match self {
            Statement::Expression(expression) => expression.type_(),
            Statement::Assignment(assignment) => assignment.type_(),
            Statement::Use(_use) => unreachable!("Use must not exist for typed code"),
        }
    }

    pub fn definition_location(&self) -> Option<DefinitionLocation<'_>> {
        match self {
            Statement::Expression(expression) => expression.definition_location(),
            Statement::Assignment(_) => None,
            Statement::Use(_) => None,
        }
    }

    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        match self {
            Statement::Use(_) => None,
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

    pub fn type_defining_location(&self) -> SrcSpan {
        match self {
            Statement::Expression(expression) => expression.type_defining_location(),
            Statement::Assignment(assignment) => assignment.location,
            Statement::Use(use_) => use_.location,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment<TypeT, ExpressionT> {
    pub location: SrcSpan,
    pub value: Box<ExpressionT>,
    pub pattern: Pattern<TypeT>,
    pub kind: AssignmentKind,
    pub annotation: Option<TypeAst>,
}

pub type TypedAssignment = Assignment<Arc<Type>, TypedExpr>;
pub type UntypedAssignment = Assignment<(), UntypedExpr>;

impl TypedAssignment {
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        self.pattern
            .find_node(byte_index)
            .or_else(|| self.value.find_node(byte_index))
    }

    pub fn type_(&self) -> Arc<Type> {
        self.value.type_()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseAssignment {
    pub location: SrcSpan,
    pub pattern: UntypedPattern,
    pub annotation: Option<TypeAst>,
}
