mod constant;
mod typed;
mod untyped;

#[cfg(test)]
mod tests;

pub use self::typed::TypedExpr;
pub use self::untyped::UntypedExpr;

pub use self::constant::{Constant, TypedConstant, UntypedConstant};

use crate::build::Target;
use crate::type_::{self, ModuleValueConstructor, PatternConstructor, Type, ValueConstructor};
use std::sync::Arc;

#[cfg(test)]
use pretty_assertions::assert_eq;

pub const TRY_VARIABLE: &str = "_try";
pub const PIPE_VARIABLE: &str = "_pipe";
pub const ASSERT_VARIABLE: &str = "_try";
pub const CAPTURE_VARIABLE: &str = "_capture";

pub trait HasLocation {
    fn location(&self) -> SrcSpan;
}

pub type TypedModule = Module<type_::Module, TypedStatement>;

pub type UntypedModule = Module<(), TargetGroup>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module<Info, Statements> {
    pub name: Vec<String>,
    pub documentation: Vec<String>,
    pub type_info: Info,
    pub statements: Vec<Statements>,
}

impl TypedModule {
    pub fn find_node(&self, byte_index: usize) -> Option<&TypedExpr> {
        self.statements.iter().find_map(|s| s.find_node(byte_index))
    }
}

/// An if is a grouping of statements that will only be compiled if
/// compiling to the specified target. e.g.
///
/// ```gleam
/// const x: Int = 1
///
/// if erlang {
///   pub external fn display(a) -> Bool = "erlang" "display"
/// }
/// ```
/// Outside an if block is `Any`, inside is an `Only`.
///
#[derive(Debug, Clone, PartialEq)]
pub enum TargetGroup {
    Any(Vec<UntypedStatement>),
    Only(Target, Vec<UntypedStatement>),
}

impl TargetGroup {
    pub fn is_for(&self, target: Target) -> bool {
        match self {
            Self::Any(_) => true,
            Self::Only(t, _) => *t == target,
        }
    }

    pub fn statements(self) -> Vec<UntypedStatement> {
        match self {
            Self::Any(s) => s,
            Self::Only(_, s) => s,
        }
    }

    pub fn statements_ref(&self) -> &[UntypedStatement] {
        match self {
            Self::Any(s) => s,
            Self::Only(_, s) => s,
        }
    }

    pub fn len(&self) -> usize {
        self.statements_ref().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<A, B> Module<A, B> {
    pub fn name_string(&self) -> String {
        self.name.join("/")
    }
}

impl UntypedModule {
    pub fn dependencies(&self, target: Target) -> Vec<(String, SrcSpan)> {
        self.iter_statements(target)
            .flat_map(|s| match s {
                Statement::Import {
                    module, location, ..
                } => Some((module.join("/"), *location)),
                _ => None,
            })
            .collect()
    }

    pub fn iter_statements(&self, target: Target) -> impl Iterator<Item = &UntypedStatement> {
        self.statements
            .iter()
            .filter(move |group| group.is_for(target))
            .flat_map(|group| group.statements_ref())
    }

    pub fn into_iter_statements(self, target: Target) -> impl Iterator<Item = UntypedStatement> {
        self.statements
            .into_iter()
            .filter(move |group| group.is_for(target))
            .flat_map(|group| group.statements())
    }
}

#[test]
fn module_dependencies_test() {
    let (module, _) = crate::parse::parse_module(
        "import one 
         if erlang { import two } 
         if javascript { import three } 
         import four",
    )
    .expect("syntax error");

    assert_eq!(
        vec![
            ("one".to_string(), SrcSpan { start: 7, end: 10 }),
            ("two".to_string(), SrcSpan { start: 40, end: 43 }),
            (
                "four".to_string(),
                SrcSpan {
                    start: 104,
                    end: 108
                }
            ),
        ],
        module.dependencies(Target::Erlang)
    );
}

pub type TypedArg = Arg<Arc<Type>>;
pub type UntypedArg = Arg<()>;
pub type TypedExternalFnArg = ExternalFnArg<Arc<Type>>;
pub type UntypedExternalFnArg = ExternalFnArg<()>;

#[derive(Debug, Clone, PartialEq)]
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

    pub fn get_variable_name(&self) -> Option<&str> {
        self.names.get_variable_name()
    }
}

impl<A> ExternalFnArg<A> {
    pub fn set_type<B>(self, t: B) -> ExternalFnArg<B> {
        ExternalFnArg {
            location: self.location,
            label: self.label,
            annotation: self.annotation,
            type_: t,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArgNames {
    Discard { name: String },
    LabelledDiscard { label: String, name: String },
    Named { name: String },
    NamedLabelled { name: String, label: String },
}

impl ArgNames {
    pub fn get_variable_name(&self) -> Option<&str> {
        match self {
            ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => None,
            ArgNames::NamedLabelled { name, .. } | ArgNames::Named { name } => Some(name),
        }
    }
}

pub type TypedRecordConstructor = RecordConstructor<Arc<Type>>;

#[derive(Debug, Clone, PartialEq)]
pub struct RecordConstructor<T> {
    pub location: SrcSpan,
    pub name: String,
    pub arguments: Vec<RecordConstructorArg<T>>,
    pub documentation: Option<String>,
}

impl<A> RecordConstructor<A> {
    pub fn put_doc(&mut self, new_doc: String) {
        self.documentation = Some(new_doc);
    }
}

pub type TypedRecordConstructorArg = RecordConstructorArg<Arc<Type>>;

#[derive(Debug, Clone, PartialEq)]
pub struct RecordConstructorArg<T> {
    pub label: Option<String>,
    pub ast: TypeAst,
    pub location: SrcSpan,
    pub type_: T,
    pub doc: Option<String>,
}

impl<T: PartialEq> RecordConstructorArg<T> {
    pub fn put_doc(&mut self, new_doc: String) {
        self.doc = Some(new_doc);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAst {
    Constructor {
        location: SrcSpan,
        module: Option<String>,
        name: String,
        arguments: Vec<Self>,
    },

    Fn {
        location: SrcSpan,
        arguments: Vec<Self>,
        return_: Box<Self>,
    },

    Var {
        location: SrcSpan,
        name: String,
    },

    Tuple {
        location: SrcSpan,
        elems: Vec<Self>,
    },

    Hole {
        location: SrcSpan,
        name: String,
    },
}

impl TypeAst {
    pub fn location(&self) -> SrcSpan {
        match self {
            TypeAst::Fn { location, .. }
            | TypeAst::Var { location, .. }
            | TypeAst::Hole { location, .. }
            | TypeAst::Tuple { location, .. }
            | TypeAst::Constructor { location, .. } => *location,
        }
    }

    pub fn is_logically_equal(&self, other: &TypeAst) -> bool {
        match self {
            TypeAst::Constructor {
                module,
                name,
                arguments,
                location: _,
            } => match other {
                TypeAst::Constructor {
                    module: o_module,
                    name: o_name,
                    arguments: o_arguments,
                    location: _,
                } => {
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
            TypeAst::Fn {
                arguments,
                return_,
                location: _,
            } => match other {
                TypeAst::Fn {
                    arguments: o_arguments,
                    return_: o_return_,
                    location: _,
                } => {
                    arguments.len() == o_arguments.len()
                        && arguments
                            .iter()
                            .zip(o_arguments)
                            .all(|a| a.0.is_logically_equal(a.1))
                        && return_.is_logically_equal(o_return_)
                }
                _ => false,
            },
            TypeAst::Var { name, location: _ } => match other {
                TypeAst::Var {
                    name: o_name,
                    location: _,
                } => name == o_name,
                _ => false,
            },
            TypeAst::Tuple { elems, location: _ } => match other {
                TypeAst::Tuple {
                    elems: o_elems,
                    location: _,
                } => {
                    elems.len() == o_elems.len()
                        && elems
                            .iter()
                            .zip(o_elems)
                            .all(|a| a.0.is_logically_equal(a.1))
                }
                _ => false,
            },
            TypeAst::Hole { name, location: _ } => match other {
                TypeAst::Hole {
                    name: o_name,
                    location: _,
                } => name == o_name,
                _ => false,
            },
        }
    }
}

pub type TypedStatement = Statement<Arc<Type>, TypedExpr, String, String>;
pub type UntypedStatement = Statement<(), UntypedExpr, (), ()>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<T, Expr, ConstantRecordTag, PackageName> {
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
    Fn {
        location: SrcSpan,
        end_position: usize,
        name: String,
        arguments: Vec<Arg<T>>,
        body: Expr,
        public: bool,
        return_annotation: Option<TypeAst>,
        return_type: T,
        doc: Option<String>,
    },

    /// A new name for an existing type
    ///
    /// # Example(s)
    ///
    /// ```gleam
    /// pub type Headers =
    ///   List(#(String, String))
    /// ```
    TypeAlias {
        location: SrcSpan,
        alias: String,
        parameters: Vec<String>,
        type_ast: TypeAst,
        type_: T,
        public: bool,
        doc: Option<String>,
    },

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
    CustomType {
        location: SrcSpan,
        name: String,
        parameters: Vec<String>,
        public: bool,
        constructors: Vec<RecordConstructor<T>>,
        doc: Option<String>,
        opaque: bool,
        typed_parameters: Vec<T>,
    },

    /// Import a function defined outside of Gleam code.
    /// When compiling to Erlang the function could be implemented in Erlang
    /// or Elixir, when compiling to JavaScript it might be implemented in
    /// JavaScript or TypeScript.
    ///
    /// # Example(s)
    ///
    /// ```gleam
    /// pub external fn random_float() -> Float = "rand" "uniform"
    /// ```
    ExternalFn {
        location: SrcSpan,
        public: bool,
        arguments: Vec<ExternalFnArg<T>>,
        name: String,
        return_: TypeAst,
        return_type: T,
        module: String,
        fun: String,
        doc: Option<String>,
    },

    /// Import a type defined in another language.
    /// Nothing is known about the runtime characteristics of the type, we only
    /// know that it exists and that we have given it this name.
    ///
    /// # Example(s)
    ///
    /// ```gleam
    /// pub external type Queue(a)
    /// ```
    ExternalType {
        location: SrcSpan,
        public: bool,
        name: String,
        arguments: Vec<String>,
        doc: Option<String>,
    },

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
    Import {
        location: SrcSpan,
        module: Vec<String>,
        as_name: Option<String>,
        unqualified: Vec<UnqualifiedImport>,
        package: PackageName,
    },

    /// A certain fixed value that can be used in multiple places
    ///
    /// # Example(s)
    ///
    /// ```gleam
    /// pub const start_year = 2101
    /// pub const end_year = 2111
    /// ```
    ModuleConstant {
        doc: Option<String>,
        location: SrcSpan,
        public: bool,
        name: String,
        annotation: Option<TypeAst>,
        value: Box<Constant<T, ConstantRecordTag>>,
        type_: T,
    },
}

impl TypedStatement {
    pub fn find_node(&self, byte_index: usize) -> Option<&TypedExpr> {
        match self {
            Statement::Fn { body, .. } => body.find_node(byte_index),

            Statement::TypeAlias { .. }
            | Statement::CustomType { .. }
            | Statement::ExternalFn { .. }
            | Statement::ExternalType { .. }
            | Statement::Import { .. }
            | Statement::ModuleConstant { .. } => None,
        }
    }
}

impl<A, B, C, E> Statement<A, B, C, E> {
    pub fn location(&self) -> &SrcSpan {
        match self {
            Statement::Fn { location, .. }
            | Statement::Import { location, .. }
            | Statement::TypeAlias { location, .. }
            | Statement::CustomType { location, .. }
            | Statement::ExternalFn { location, .. }
            | Statement::ExternalType { location, .. }
            | Statement::ModuleConstant { location, .. } => location,
        }
    }

    pub fn put_doc(&mut self, new_doc: String) {
        match self {
            Statement::Import { .. } => (),
            Statement::Fn { doc, .. }
            | Statement::TypeAlias { doc, .. }
            | Statement::CustomType { doc, .. }
            | Statement::ExternalFn { doc, .. }
            | Statement::ExternalType { doc, .. }
            | Statement::ModuleConstant { doc, .. } => {
                let _ = std::mem::replace(doc, Some(new_doc));
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnqualifiedImport {
    pub location: SrcSpan,
    pub name: String,
    pub as_name: Option<String>,
    pub layer: Layer,
}

impl UnqualifiedImport {
    pub fn variable_name(&self) -> &str {
        self.as_name.as_deref().unwrap_or(&self.name)
    }

    pub fn is_value(&self) -> bool {
        self.layer.is_value()
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Layer {
    Value,
    Type,
}

impl Default for Layer {
    fn default() -> Self {
        Layer::Value
    }
}

impl Layer {
    /// Returns `true` if the layer is [`Value`].
    pub fn is_value(&self) -> bool {
        matches!(self, Self::Value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternalFnArg<T> {
    pub location: SrcSpan,
    pub label: Option<String>,
    pub annotation: TypeAst,
    pub type_: T,
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
    ModuloInt,
}

impl BinOp {
    pub fn precedence(&self) -> u8 {
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

            // Pipe is 5
            Self::AddInt | Self::AddFloat | Self::SubInt | Self::SubFloat => 6,

            Self::MultInt | Self::MultFloat | Self::DivInt | Self::DivFloat | Self::ModuloInt => 7,
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
            Self::ModuloInt => "%",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallArg<A> {
    pub label: Option<String>,
    pub location: SrcSpan,
    pub value: A,
}

impl CallArg<TypedExpr> {
    pub fn find_node(&self, byte_index: usize) -> Option<&TypedExpr> {
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

#[derive(Debug, PartialEq, Clone)]
pub struct RecordUpdateSpread {
    pub base: Box<UntypedExpr>,
    pub location: SrcSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UntypedRecordUpdateArg {
    pub label: String,
    pub location: SrcSpan,
    pub value: UntypedExpr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedRecordUpdateArg {
    pub label: String,
    pub location: SrcSpan,
    pub value: TypedExpr,
    pub index: usize,
}

impl TypedRecordUpdateArg {
    pub fn find_node(&self, byte_index: usize) -> Option<&TypedExpr> {
        self.value.find_node(byte_index)
    }
}

pub type MultiPattern<PatternConstructor, Type> = Vec<Pattern<PatternConstructor, Type>>;

pub type UntypedMultiPattern = MultiPattern<(), ()>;
pub type TypedMultiPattern = MultiPattern<PatternConstructor, Arc<Type>>;

pub type TypedClause = Clause<TypedExpr, PatternConstructor, Arc<Type>, String>;

pub type UntypedClause = Clause<UntypedExpr, (), (), ()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Clause<Expr, PatternConstructor, Type, RecordTag> {
    pub location: SrcSpan,
    pub pattern: MultiPattern<PatternConstructor, Type>,
    pub alternative_patterns: Vec<MultiPattern<PatternConstructor, Type>>,
    pub guard: Option<ClauseGuard<Type, RecordTag>>,
    pub then: Expr,
}

impl TypedClause {
    pub fn location(&self) -> SrcSpan {
        SrcSpan {
            start: self
                .pattern
                .get(0)
                .map(|p| p.location().start)
                .unwrap_or_default(),
            end: self.then.location().end,
        }
    }

    pub fn find_node(&self, byte_index: usize) -> Option<&TypedExpr> {
        self.then.find_node(byte_index)
    }
}

pub type UntypedClauseGuard = ClauseGuard<(), ()>;
pub type TypedClauseGuard = ClauseGuard<Arc<Type>, String>;

#[derive(Debug, PartialEq, Clone)]
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

    Var {
        location: SrcSpan,
        type_: Type,
        name: String,
    },

    TupleIndex {
        location: SrcSpan,
        index: u64,
        type_: Type,
        tuple: Box<Self>,
    },

    Constant(Constant<Type, RecordTag>),
}

impl<A, B> ClauseGuard<A, B> {
    pub fn location(&self) -> SrcSpan {
        match self {
            ClauseGuard::Constant(constant) => constant.location(),
            ClauseGuard::Or { location, .. }
            | ClauseGuard::And { location, .. }
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
            | ClauseGuard::LtEqFloat { location, .. } => *location,
        }
    }
}

impl TypedClauseGuard {
    pub fn type_(&self) -> Arc<Type> {
        match self {
            ClauseGuard::Var { type_, .. } => type_.clone(),
            ClauseGuard::TupleIndex { type_, .. } => type_.clone(),
            ClauseGuard::Constant(constant) => constant.type_(),

            ClauseGuard::Or { .. }
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

#[derive(Debug, PartialEq, Default, Clone, Copy)]
pub struct SrcSpan {
    pub start: usize,
    pub end: usize,
}

impl SrcSpan {
    pub fn contains(&self, byte_index: usize) -> bool {
        byte_index >= self.start && byte_index < self.end
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DefinitionLocation<'module> {
    pub module: Option<&'module str>,
    pub span: SrcSpan,
}

pub type UntypedPattern = Pattern<(), ()>;
pub type TypedPattern = Pattern<PatternConstructor, Arc<Type>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern<Constructor, Type> {
    Int {
        location: SrcSpan,
        value: String,
    },

    Float {
        location: SrcSpan,
        value: String,
    },

    String {
        location: SrcSpan,
        value: String,
    },

    /// The creation of a variable.
    /// e.g. `assert [this_is_a_var, .._] = x`
    Var {
        location: SrcSpan,
        name: String,
    },

    /// A reference to a variable in a bit string. This is always a variable
    /// being used rather than a new variable being assigned.
    VarUsage {
        location: SrcSpan,
        name: String,
        type_: Type,
    },

    /// A name given to a sub-pattern using the `as` keyword.
    /// e.g. `assert #(1, [_, _] as the_list) = x`
    Assign {
        name: String,
        location: SrcSpan,
        pattern: Box<Self>,
    },

    /// A pattern that binds to any value but does not assign a variable.
    /// Always starts with an underscore.
    Discard {
        name: String,
        location: SrcSpan,
    },

    List {
        location: SrcSpan,
        elements: Vec<Self>,
        tail: Option<Box<Self>>,
    },

    /// The constructor for a custom type. Starts with an uppercase letter.
    Constructor {
        location: SrcSpan,
        name: String,
        arguments: Vec<CallArg<Self>>,
        module: Option<String>,
        constructor: Constructor,
        with_spread: bool,
        type_: Type,
    },

    Tuple {
        location: SrcSpan,
        elems: Vec<Self>,
    },

    BitString {
        location: SrcSpan,
        segments: Vec<BitStringSegment<Self, Type>>,
    },
}

impl<A, B> Pattern<A, B> {
    pub fn location(&self) -> SrcSpan {
        match self {
            Pattern::Assign { pattern, .. } => pattern.location(),
            Pattern::Int { location, .. }
            | Pattern::Var { location, .. }
            | Pattern::VarUsage { location, .. }
            | Pattern::List { location, .. }
            | Pattern::Float { location, .. }
            | Pattern::Discard { location, .. }
            | Pattern::String { location, .. }
            | Pattern::Tuple { location, .. }
            | Pattern::Constructor { location, .. }
            | Pattern::BitString { location, .. } => *location,
        }
    }

    /// Returns `true` if the pattern is [`Discard`].
    ///
    /// [`Discard`]: Pattern::Discard
    pub fn is_discard(&self) -> bool {
        matches!(self, Self::Discard { .. })
    }
}
impl<A, B> HasLocation for Pattern<A, B> {
    fn location(&self) -> SrcSpan {
        self.location()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AssignmentKind {
    Let,
    Assert,
}

// BitStrings

pub type UntypedExprBitStringSegment = BitStringSegment<UntypedExpr, ()>;
pub type TypedExprBitStringSegment = BitStringSegment<TypedExpr, Arc<Type>>;

pub type UntypedConstantBitStringSegment = BitStringSegment<UntypedConstant, ()>;
pub type TypedConstantBitStringSegment = BitStringSegment<TypedConstant, Arc<Type>>;

pub type UntypedPatternBitStringSegment = BitStringSegment<UntypedPattern, ()>;
pub type TypedPatternBitStringSegment = BitStringSegment<TypedPattern, Arc<Type>>;

#[derive(Debug, Clone, PartialEq)]
pub struct BitStringSegment<Value, Type> {
    pub location: SrcSpan,
    pub value: Box<Value>,
    pub options: Vec<BitStringSegmentOption<Value>>,
    pub type_: Type,
}

impl TypedExprBitStringSegment {
    pub fn find_node(&self, byte_index: usize) -> Option<&TypedExpr> {
        self.value.find_node(byte_index)
    }
}

pub type TypedConstantBitStringSegmentOption = BitStringSegmentOption<TypedConstant>;

#[derive(Debug, PartialEq, Clone)]
pub enum BitStringSegmentOption<Value> {
    Binary {
        location: SrcSpan,
    },

    Int {
        location: SrcSpan,
    },

    Float {
        location: SrcSpan,
    },

    BitString {
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

impl<A> BitStringSegmentOption<A> {
    pub fn location(&self) -> SrcSpan {
        match self {
            BitStringSegmentOption::Binary { location }
            | BitStringSegmentOption::Int { location }
            | BitStringSegmentOption::Float { location }
            | BitStringSegmentOption::BitString { location }
            | BitStringSegmentOption::Utf8 { location }
            | BitStringSegmentOption::Utf16 { location }
            | BitStringSegmentOption::Utf32 { location }
            | BitStringSegmentOption::Utf8Codepoint { location }
            | BitStringSegmentOption::Utf16Codepoint { location }
            | BitStringSegmentOption::Utf32Codepoint { location }
            | BitStringSegmentOption::Signed { location }
            | BitStringSegmentOption::Unsigned { location }
            | BitStringSegmentOption::Big { location }
            | BitStringSegmentOption::Little { location }
            | BitStringSegmentOption::Native { location }
            | BitStringSegmentOption::Size { location, .. }
            | BitStringSegmentOption::Unit { location, .. } => *location,
        }
    }

    pub fn label(&self) -> String {
        match self {
            BitStringSegmentOption::Binary { .. } => "binary".to_string(),
            BitStringSegmentOption::Int { .. } => "int".to_string(),
            BitStringSegmentOption::Float { .. } => "float".to_string(),
            BitStringSegmentOption::BitString { .. } => "bit_string".to_string(),
            BitStringSegmentOption::Utf8 { .. } => "utf8".to_string(),
            BitStringSegmentOption::Utf16 { .. } => "utf16".to_string(),
            BitStringSegmentOption::Utf32 { .. } => "utf32".to_string(),
            BitStringSegmentOption::Utf8Codepoint { .. } => "utf8_codepoint".to_string(),
            BitStringSegmentOption::Utf16Codepoint { .. } => "utf16_codepoint".to_string(),
            BitStringSegmentOption::Utf32Codepoint { .. } => "utf32_codepoint".to_string(),
            BitStringSegmentOption::Signed { .. } => "signed".to_string(),
            BitStringSegmentOption::Unsigned { .. } => "unsigned".to_string(),
            BitStringSegmentOption::Big { .. } => "big".to_string(),
            BitStringSegmentOption::Little { .. } => "little".to_string(),
            BitStringSegmentOption::Native { .. } => "native".to_string(),
            BitStringSegmentOption::Size { .. } => "size".to_string(),
            BitStringSegmentOption::Unit { .. } => "unit".to_string(),
        }
    }
}
