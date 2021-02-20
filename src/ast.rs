mod constant;
mod typed;
mod untyped;

pub use self::typed::TypedExpr;
pub use self::untyped::UntypedExpr;

pub use self::constant::{Constant, TypedConstant, UntypedConstant};

use crate::typ::{self, ModuleValueConstructor, PatternConstructor, Type, ValueConstructor};
use std::sync::Arc;

pub const CAPTURE_VARIABLE: &str = "gleam@capture_variable";

pub trait HasLocation {
    fn location(&self) -> SrcSpan;
}

pub type TypedModule = Module<Arc<Type>, TypedExpr, typ::Module, String>;

pub type UntypedModule = Module<(), UntypedExpr, (), ()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module<T, Expr, Info, ConstantRecordTag> {
    pub name: Vec<String>,
    pub documentation: Vec<String>,
    pub type_info: Info,
    pub statements: Vec<Statement<T, Expr, ConstantRecordTag>>,
}

impl<A, B, C, D> Module<A, B, C, D> {
    pub fn name_string(&self) -> String {
        self.name.join("/")
    }

    // TODO: return &str not String once module is a String not a Vector
    pub fn dependencies(&self) -> Vec<(String, SrcSpan)> {
        self.statements
            .iter()
            .flat_map(|s| match s {
                Statement::Import {
                    module, location, ..
                } => Some((module.join("/"), *location)),
                _ => None,
            })
            .collect()
    }
}

#[test]
fn module_dependencies_test() {
    let (module, _) =
        crate::parse::parse_module("import foo import bar import foo_bar").expect("syntax error");

    assert_eq!(
        vec![
            ("foo".to_string(), SrcSpan { start: 7, end: 10 }),
            ("bar".to_string(), SrcSpan { start: 18, end: 21 }),
            ("foo_bar".to_string(), SrcSpan { start: 29, end: 36 }),
        ],
        module.dependencies()
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
    pub typ: T,
}

impl<A> Arg<A> {
    pub fn set_type<B>(self, t: B) -> Arg<B> {
        Arg {
            typ: t,
            names: self.names,
            location: self.location,
            annotation: self.annotation,
        }
    }
}

impl<A> ExternalFnArg<A> {
    pub fn set_type<B>(self, t: B) -> ExternalFnArg<B> {
        ExternalFnArg {
            location: self.location,
            label: self.label,
            annotation: self.annotation,
            typ: t,
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

#[derive(Debug, Clone, PartialEq)]
pub struct RecordConstructor<T> {
    pub location: SrcSpan,
    pub name: String,
    pub args: Vec<RecordConstructorArg<T>>,
    pub documentation: Option<String>,
}

impl<A> RecordConstructor<A> {
    pub fn put_doc(&mut self, new_doc: String) {
        self.documentation = Some(new_doc);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordConstructorArg<T> {
    pub label: Option<String>,
    pub ast: TypeAst,
    pub location: SrcSpan,
    pub typ: T,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAst {
    Constructor {
        location: SrcSpan,
        module: Option<String>,
        name: String,
        args: Vec<Self>,
    },

    Fn {
        location: SrcSpan,
        args: Vec<Self>,
        retrn: Box<Self>,
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
}

pub type TypedStatement = Statement<Arc<Type>, TypedExpr, String>;
pub type UntypedStatement = Statement<(), UntypedExpr, ()>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<T, Expr, ConstantRecordTag> {
    Fn {
        end_location: usize,
        location: SrcSpan,
        name: String,
        args: Vec<Arg<T>>,
        body: Expr,
        public: bool,
        return_annotation: Option<TypeAst>,
        return_type: T,
        doc: Option<String>,
    },

    TypeAlias {
        location: SrcSpan,
        alias: String,
        args: Vec<String>,
        resolved_type: TypeAst,
        typ: T,
        public: bool,
        doc: Option<String>,
    },

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

    ExternalFn {
        location: SrcSpan,
        public: bool,
        args: Vec<ExternalFnArg<T>>,
        name: String,
        retrn: TypeAst,
        return_type: T,
        module: String,
        fun: String,
        doc: Option<String>,
    },

    ExternalType {
        location: SrcSpan,
        public: bool,
        name: String,
        args: Vec<String>,
        doc: Option<String>,
    },

    Import {
        location: SrcSpan,
        module: Vec<String>,
        as_name: Option<String>,
        unqualified: Vec<UnqualifiedImport>,
    },

    ModuleConstant {
        doc: Option<String>,
        location: SrcSpan,
        public: bool,
        name: String,
        annotation: Option<TypeAst>,
        value: Box<Constant<T, ConstantRecordTag>>,
        typ: T,
    },
}

impl<A, B, C> Statement<A, B, C> {
    pub fn location(&self) -> &SrcSpan {
        match self {
            Statement::Import { location, .. }
            | Statement::Fn { location, .. }
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternalFnArg<T> {
    pub location: SrcSpan,
    pub label: Option<String>,
    pub annotation: TypeAst,
    pub typ: T,
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallArg<A> {
    pub label: Option<String>,
    pub location: SrcSpan,
    pub value: A,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RecordUpdateSpread {
    pub name: String,
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
        typ: Type,
        name: String,
    },

    TupleIndex {
        location: SrcSpan,
        index: u64,
        typ: Type,
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
    pub fn typ(&self) -> Arc<Type> {
        match self {
            ClauseGuard::Var { typ, .. } => typ.clone(),
            ClauseGuard::TupleIndex { typ, .. } => typ.clone(),
            ClauseGuard::Constant(constant) => constant.typ(),

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
            | ClauseGuard::LtEqFloat { .. } => typ::bool(),
        }
    }
}

#[derive(Debug, PartialEq, Default, Clone, Copy)]
pub struct SrcSpan {
    pub start: usize,
    pub end: usize,
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

    Var {
        location: SrcSpan,
        name: String,
    },

    VarUsage {
        location: SrcSpan,
        name: String,
        typ: Type,
    },

    Let {
        name: String,
        location: SrcSpan,
        pattern: Box<Self>,
    },

    Discard {
        name: String,
        location: SrcSpan,
    },

    Nil {
        location: SrcSpan,
    },

    Cons {
        location: SrcSpan,
        head: Box<Self>,
        tail: Box<Self>,
    },

    Constructor {
        location: SrcSpan,
        name: String,
        args: Vec<CallArg<Self>>,
        module: Option<String>,
        constructor: Constructor,
        with_spread: bool,
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
            Pattern::Let { pattern, .. } => pattern.location(),
            Pattern::Int { location, .. }
            | Pattern::Var { location, .. }
            | Pattern::VarUsage { location, .. }
            | Pattern::Nil { location, .. }
            | Pattern::Cons { location, .. }
            | Pattern::Float { location, .. }
            | Pattern::Discard { location, .. }
            | Pattern::String { location, .. }
            | Pattern::Tuple { location, .. }
            | Pattern::Constructor { location, .. }
            | Pattern::BitString { location, .. } => *location,
        }
    }

    pub fn put_list_cons_location_start(self, start: usize) -> Self {
        match self {
            Pattern::Cons {
                location: SrcSpan { end, .. },
                head,
                tail,
            } => Pattern::Cons {
                location: SrcSpan { start, end },
                head,
                tail,
            },

            _ => self,
        }
    }
}
impl<A, B> HasLocation for Pattern<A, B> {
    fn location(&self) -> SrcSpan {
        self.location()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BindingKind {
    Let,
    Assert,
    Try,
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
    pub typ: Type,
}

pub type TypedConstantBitStringSegmentOption = BitStringSegmentOption<TypedConstant>;

#[derive(Debug, PartialEq, Clone)]
pub enum BitStringSegmentOption<Value> {
    Binary {
        location: SrcSpan,
    },

    Integer {
        location: SrcSpan,
    },

    Float {
        location: SrcSpan,
    },

    BitString {
        location: SrcSpan,
    },

    UTF8 {
        location: SrcSpan,
    },

    UTF16 {
        location: SrcSpan,
    },

    UTF32 {
        location: SrcSpan,
    },

    UTF8Codepoint {
        location: SrcSpan,
    },

    UTF16Codepoint {
        location: SrcSpan,
    },

    UTF32Codepoint {
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
        value: Box<usize>,
    },
}

impl<A> BitStringSegmentOption<A> {
    pub fn location(&self) -> SrcSpan {
        match self {
            BitStringSegmentOption::Binary { location }
            | BitStringSegmentOption::Integer { location }
            | BitStringSegmentOption::Float { location }
            | BitStringSegmentOption::BitString { location }
            | BitStringSegmentOption::UTF8 { location }
            | BitStringSegmentOption::UTF16 { location }
            | BitStringSegmentOption::UTF32 { location }
            | BitStringSegmentOption::UTF8Codepoint { location }
            | BitStringSegmentOption::UTF16Codepoint { location }
            | BitStringSegmentOption::UTF32Codepoint { location }
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
            BitStringSegmentOption::Integer { .. } => "int".to_string(),
            BitStringSegmentOption::Float { .. } => "float".to_string(),
            BitStringSegmentOption::BitString { .. } => "bit_string".to_string(),
            BitStringSegmentOption::UTF8 { .. } => "utf8".to_string(),
            BitStringSegmentOption::UTF16 { .. } => "utf16".to_string(),
            BitStringSegmentOption::UTF32 { .. } => "utf32".to_string(),
            BitStringSegmentOption::UTF8Codepoint { .. } => "utf8_codepoint".to_string(),
            BitStringSegmentOption::UTF16Codepoint { .. } => "utf16_codepoint".to_string(),
            BitStringSegmentOption::UTF32Codepoint { .. } => "utf32_codepoint".to_string(),
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
