mod constant;
mod typed;
mod untyped;

pub use self::typed::TypedExpr;
pub use self::untyped::UntypedExpr;

pub use self::constant::{Constant, TypedConstant, UntypedConstant};

use crate::typ::{self, ModuleValueConstructor, PatternConstructor, Type, ValueConstructor};
use itertools::Itertools;
use std::sync::Arc;

pub const CAPTURE_VARIABLE: &str = "gleam@capture_variable";

pub type TypedModule = Module<Arc<Type>, TypedExpr, typ::Module>;

pub type UntypedModule = Module<(), UntypedExpr, ()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module<T, Expr, Info> {
    pub name: Vec<String>,
    pub documentation: Vec<String>,
    pub type_info: Info,
    pub statements: Vec<Statement<T, Expr>>,
}

impl<A, B, C> Module<A, B, C> {
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
                } => Some((module.join("/"), location.clone())),
                _ => None,
            })
            .collect()
    }
}

#[test]
fn module_dependencies_test() {
    assert_eq!(
        vec![
            ("foo".to_string(), SrcSpan { start: 7, end: 10 }),
            ("bar".to_string(), SrcSpan { start: 18, end: 21 }),
            ("foo_bar".to_string(), SrcSpan { start: 29, end: 36 }),
        ],
        crate::grammar::ModuleParser::new()
            .parse("import foo import bar import foo_bar")
            .expect("syntax error")
            .dependencies()
    );
}

pub type TypedArg = Arg<Arc<Type>>;
pub type UntypedArg = Arg<()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Arg<T> {
    pub names: ArgNames,
    pub location: SrcSpan,
    pub annotation: Option<TypeAst>,
    pub typ: T,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArgNames {
    Discard { name: String },
    LabelledDiscard { label: String, name: String },
    Named { name: String },
    NamedLabelled { name: String, label: String },
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordConstructor {
    pub location: SrcSpan,
    pub name: String,
    pub args: Vec<(Option<String>, TypeAst, SrcSpan)>,
    pub documentation: Option<String>,
}

impl RecordConstructor {
    pub fn put_doc<'a>(&mut self, new_doc: impl Iterator<Item = &'a str>) {
        let mut new_doc = new_doc.peekable();
        if new_doc.peek().is_none() {
            return;
        }

        self.documentation = Some(new_doc.join("\n"));
    }
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
}

impl TypeAst {
    pub fn location(&self) -> &SrcSpan {
        match self {
            TypeAst::Fn { location, .. } => location,
            TypeAst::Var { location, .. } => location,
            TypeAst::Tuple { location, .. } => location,
            TypeAst::Constructor { location, .. } => location,
        }
    }
}

pub type TypedStatement = Statement<Arc<Type>, TypedExpr>;
pub type UntypedStatement = Statement<(), UntypedExpr>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<T, Expr> {
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
        args: Vec<String>,
        public: bool,
        constructors: Vec<RecordConstructor>,
        doc: Option<String>,
        opaque: bool,
    },

    ExternalFn {
        location: SrcSpan,
        public: bool,
        args: Vec<ExternalFnArg>,
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
        value: Box<constant::Constant<T>>,
        typ: T,
    },
}

impl<A, B> Statement<A, B> {
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

    pub fn put_doc<'a>(&mut self, new_doc: impl Iterator<Item = &'a str>) {
        let mut new_doc = new_doc.peekable();
        if new_doc.peek().is_none() {
            return;
        }

        let new_doc = Some(new_doc.join("\n"));

        match self {
            Statement::Import { .. } => (),

            Statement::Fn { doc, .. }
            | Statement::TypeAlias { doc, .. }
            | Statement::CustomType { doc, .. }
            | Statement::ExternalFn { doc, .. }
            | Statement::ExternalType { doc, .. }
            | Statement::ModuleConstant { doc, .. } => {
                std::mem::replace(doc, new_doc);
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
pub struct ExternalFnArg {
    pub location: SrcSpan,
    pub label: Option<String>,
    pub typ: TypeAst,
}

#[derive(Debug, Clone, PartialEq)]
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

pub type MultiPattern<PatternConstructor, Type> = Vec<Pattern<PatternConstructor, Type>>;
pub type UntypedMultiPattern = MultiPattern<(), ()>;
pub type TypedMultiPattern = MultiPattern<PatternConstructor, Arc<typ::Type>>;

pub type TypedClause = Clause<TypedExpr, PatternConstructor, Arc<typ::Type>>;

pub type UntypedClause = Clause<UntypedExpr, (), ()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Clause<Expr, PatternConstructor, Type> {
    pub location: SrcSpan,
    pub pattern: MultiPattern<PatternConstructor, Type>,
    pub alternative_patterns: Vec<MultiPattern<PatternConstructor, Type>>,
    pub guard: Option<ClauseGuard<Type>>,
    pub then: Expr,
}

pub type UntypedClauseGuard = ClauseGuard<()>;
pub type TypedClauseGuard = ClauseGuard<Arc<typ::Type>>;

#[derive(Debug, PartialEq, Clone)]
pub enum ClauseGuard<Type> {
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

    Tuple {
        location: SrcSpan,
        elems: Vec<Self>,
        typ: Type,
    },

    List {
        location: SrcSpan,
        elems: Vec<Self>,
        typ: Type,
    },

    Constructor {
        location: SrcSpan,
        module: Option<String>,
        name: String,
        args: Vec<CallArg<Self>>,
        typ: Type,
    },
}

impl<A> ClauseGuard<A> {
    pub fn location(&self) -> &SrcSpan {
        match self {
            ClauseGuard::Or { location, .. } => location,
            ClauseGuard::And { location, .. } => location,
            ClauseGuard::Var { location, .. } => location,
            ClauseGuard::Equals { location, .. } => location,
            ClauseGuard::NotEquals { location, .. } => location,
            ClauseGuard::GtInt { location, .. } => location,
            ClauseGuard::GtEqInt { location, .. } => location,
            ClauseGuard::LtInt { location, .. } => location,
            ClauseGuard::LtEqInt { location, .. } => location,
            ClauseGuard::GtFloat { location, .. } => location,
            ClauseGuard::GtEqFloat { location, .. } => location,
            ClauseGuard::LtFloat { location, .. } => location,
            ClauseGuard::LtEqFloat { location, .. } => location,
            ClauseGuard::Int { location, .. } => location,
            ClauseGuard::Float { location, .. } => location,
            ClauseGuard::String { location, .. } => location,
            ClauseGuard::Tuple { location, .. } => location,
            ClauseGuard::List { location, .. } => location,
            ClauseGuard::Constructor { location, .. } => location,
        }
    }
}

impl TypedClauseGuard {
    pub fn typ(&self) -> Arc<typ::Type> {
        match self {
            ClauseGuard::Var { typ, .. } => typ.clone(),
            ClauseGuard::Int { .. } => typ::int(),
            ClauseGuard::Float { .. } => typ::float(),
            ClauseGuard::String { .. } => typ::string(),
            ClauseGuard::Tuple { typ, .. } => typ.clone(),
            ClauseGuard::Constructor { typ, .. } => typ.clone(),
            _ => typ::bool(),
        }
    }
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct SrcSpan {
    pub start: usize,
    pub end: usize,
}

pub type UntypedPattern = Pattern<(), ()>;
pub type TypedPattern = Pattern<PatternConstructor, Arc<typ::Type>>;

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

    VarCall {
        location: SrcSpan,
        name: String,
        typ: Type,
    },

    Let {
        name: String,
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
        elems: Vec<BinSegment<Self, Type>>,
    },
}

impl<A, B> Pattern<A, B> {
    pub fn location(&self) -> &SrcSpan {
        match self {
            Pattern::Let { pattern, .. } => pattern.location(),
            Pattern::Int { location, .. } => location,
            Pattern::Var { location, .. } => location,
            Pattern::VarCall { location, .. } => location,
            Pattern::Nil { location, .. } => location,
            Pattern::Cons { location, .. } => location,
            Pattern::Float { location, .. } => location,
            Pattern::Discard { location, .. } => location,
            Pattern::String { location, .. } => location,
            Pattern::Tuple { location, .. } => location,
            Pattern::Constructor { location, .. } => location,
            Pattern::BitString { location, .. } => location,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BindingKind {
    Let,
    Assert,
    Try,
}

// BitStrings

pub type UntypedExprBinSegment = BinSegment<UntypedExpr, ()>;
pub type TypedExprBinSegment = BinSegment<TypedExpr, Arc<typ::Type>>;

pub type UntypedPatternBinSegment = BinSegment<UntypedPattern, ()>;
pub type TypedPatternBinSegment = BinSegment<TypedPattern, Arc<typ::Type>>;

#[derive(Debug, Clone, PartialEq)]
pub struct BinSegment<Value, Type> {
    pub location: SrcSpan,
    pub value: Box<Value>,
    pub options: Vec<BinSegmentOption<Value>>,
    pub typ: Type,
}

pub type UntypedExprBinSegmentOption = BinSegmentOption<UntypedExpr>;
pub type TypedExprBinSegmentOption = BinSegmentOption<TypedExpr>;

pub type UntypedPatternBinSegmentOption = BinSegmentOption<UntypedPattern>;
pub type TypedPatternBinSegmentOption = BinSegmentOption<TypedPattern>;

#[derive(Debug, PartialEq, Clone)]
pub enum BinSegmentOption<Value> {
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
        value: Box<Value>,
        short_form: bool,
    },

    Invalid {
        location: SrcSpan,
        label: String,
    },
}
#[derive(Debug, PartialEq, Clone)]
pub enum SegmentOptionCategory {
    Type,
    Endianness,
    Signedness,
    Size,
    Unit,
    Error,
}

impl<A> BinSegmentOption<A> {
    pub fn location(&self) -> &SrcSpan {
        match self {
            BinSegmentOption::Binary { location } => location,
            BinSegmentOption::Integer { location } => location,
            BinSegmentOption::Float { location } => location,
            BinSegmentOption::BitString { location } => location,
            BinSegmentOption::UTF8 { location } => location,
            BinSegmentOption::UTF16 { location } => location,
            BinSegmentOption::UTF32 { location } => location,
            BinSegmentOption::UTF8Codepoint { location } => location,
            BinSegmentOption::UTF16Codepoint { location } => location,
            BinSegmentOption::UTF32Codepoint { location } => location,
            BinSegmentOption::Signed { location } => location,
            BinSegmentOption::Unsigned { location } => location,
            BinSegmentOption::Big { location } => location,
            BinSegmentOption::Little { location } => location,
            BinSegmentOption::Native { location } => location,
            BinSegmentOption::Size { location, .. } => location,
            BinSegmentOption::Unit { location, .. } => location,
            BinSegmentOption::Invalid { location, .. } => location,
        }
    }

    pub fn category(&self) -> SegmentOptionCategory {
        match self {
            BinSegmentOption::Binary { .. } => SegmentOptionCategory::Type,
            BinSegmentOption::Integer { .. } => SegmentOptionCategory::Type,
            BinSegmentOption::Float { .. } => SegmentOptionCategory::Type,
            BinSegmentOption::BitString { .. } => SegmentOptionCategory::Type,
            BinSegmentOption::UTF8 { .. } => SegmentOptionCategory::Type,
            BinSegmentOption::UTF16 { .. } => SegmentOptionCategory::Type,
            BinSegmentOption::UTF32 { .. } => SegmentOptionCategory::Type,
            BinSegmentOption::UTF8Codepoint { .. } => SegmentOptionCategory::Type,
            BinSegmentOption::UTF16Codepoint { .. } => SegmentOptionCategory::Type,
            BinSegmentOption::UTF32Codepoint { .. } => SegmentOptionCategory::Type,
            BinSegmentOption::Signed { .. } => SegmentOptionCategory::Signedness,
            BinSegmentOption::Unsigned { .. } => SegmentOptionCategory::Signedness,
            BinSegmentOption::Big { .. } => SegmentOptionCategory::Endianness,
            BinSegmentOption::Little { .. } => SegmentOptionCategory::Endianness,
            BinSegmentOption::Native { .. } => SegmentOptionCategory::Endianness,
            BinSegmentOption::Size { .. } => SegmentOptionCategory::Size,
            BinSegmentOption::Unit { .. } => SegmentOptionCategory::Unit,
            BinSegmentOption::Invalid { .. } => SegmentOptionCategory::Error,
        }
    }

    pub fn label(&self) -> String {
        match self {
            BinSegmentOption::Binary { .. } => "binary".to_string(),
            BinSegmentOption::Integer { .. } => "int".to_string(),
            BinSegmentOption::Float { .. } => "float".to_string(),
            BinSegmentOption::BitString { .. } => "bit_string".to_string(),
            BinSegmentOption::UTF8 { .. } => "utf8".to_string(),
            BinSegmentOption::UTF16 { .. } => "utf16".to_string(),
            BinSegmentOption::UTF32 { .. } => "utf32".to_string(),
            BinSegmentOption::UTF8Codepoint { .. } => "utf8_codepoint".to_string(),
            BinSegmentOption::UTF16Codepoint { .. } => "utf16_codepoint".to_string(),
            BinSegmentOption::UTF32Codepoint { .. } => "utf32_codepoint".to_string(),
            BinSegmentOption::Signed { .. } => "signed".to_string(),
            BinSegmentOption::Unsigned { .. } => "unsigned".to_string(),
            BinSegmentOption::Big { .. } => "big".to_string(),
            BinSegmentOption::Little { .. } => "little".to_string(),
            BinSegmentOption::Native { .. } => "native".to_string(),
            BinSegmentOption::Size { .. } => "size".to_string(),
            BinSegmentOption::Unit { .. } => "unit".to_string(),
            BinSegmentOption::Invalid { label, .. } => label.clone(),
        }
    }
}
