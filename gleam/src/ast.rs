#![allow(dead_code)]

use super::pattern::Pattern;
use super::typ;

#[derive(Debug)]
pub struct Module<Type> {
    name: String,
    statements: Vec<Statement<Type>>,
}

#[derive(Debug)]
pub struct Arg {
    name: String,
}

#[derive(Debug)]
pub enum Statement<Type> {
    Fun {
        meta: Meta,
        name: String,
        args: Vec<Arg>,
        body: Expr<Type>,
    },
}

#[derive(Debug)]
pub enum BinOp {
    Pipe,
    Lt,
    LtEq,
    Eq,
    GtEq,
    Gt,
    AddInt,
    AddFloat,
    SubInt,
    SubFloat,
    MultInt,
    MultFloat,
    DivInt,
    DivFloat,
}

#[derive(Debug)]
pub enum Scope<Type> {
    Local,
    Module,
    Constant { value: Box<Expr<Type>> },
}

#[derive(Debug)]
pub enum Expr<Type> {
    Int {
        meta: Meta,
        value: i64,
    },

    Float {
        meta: Meta,
        value: f64,
    },

    Atom {
        meta: Meta,
        value: String,
    },

    String {
        meta: Meta,
        value: String,
    },

    Tuple {
        meta: Meta,
        typ: Type,
        elems: Vec<Expr<Type>>,
    },

    Seq {
        meta: Meta,
        first: Box<Expr<Type>>,
        then: Box<Expr<Type>>,
    },

    Var {
        meta: Meta,
        typ: Type,
        scope: Scope<Type>,
        name: String,
    },

    Fun {
        meta: Meta,
        typ: Type,
        args: Vec<Arg>,
        body: Box<Expr<Type>>,
    },

    Nil {
        meta: Meta,
        typ: Type,
    },

    Cons {
        meta: Meta,
        typ: Type,
        head: Box<Expr<Type>>,
        tail: Box<Expr<Type>>,
    },

    Call {
        meta: Meta,
        typ: Type,
        fun: Box<Expr<Type>>,
        args: Vec<Expr<Type>>,
    },

    BinOp {
        meta: Meta,
        typ: Type,
        name: BinOp,
        left: Box<Expr<Type>>,
        right: Box<Expr<Type>>,
    },

    Let {
        meta: Meta,
        typ: Type,
        pattern: Pattern,
        left: Box<Expr<Type>>,
        right: Box<Expr<Type>>,
    },
}

// -record(ast_enum,
//         {meta = #meta{} :: meta(),
//          type = type_not_annotated :: type_annotation(),
//          name :: string(),
//          elems = [] :: [ast_expression()]}). % TODO: Rename to args

// -record(ast_clause,
//         {meta = #meta{} :: meta(),
//          type = type_not_annotated :: type_annotation(),
//          pattern :: ast_pattern(),
//          value :: ast_expression()}).

// -record(ast_case,
//         {meta = #meta{} :: meta(),
//          type = type_not_annotated :: type_annotation(),
//          subject :: ast_expression(),
//          clauses = [#ast_clause{}]}).

// -record(ast_record_empty,
//         {meta = #meta{} :: meta()}).

// -record(ast_record_extend,
//         {meta = #meta{} :: meta(),
//          type = type_not_annotated :: type_annotation(),
//          parent :: ast_expression(),
//          label :: string(),
//          value :: ast_expression()}).

// -record(ast_record_select,
//         {meta = #meta{} :: meta(),
//          type = type_not_annotated :: type_annotation(),
//          record :: ast_expression(),
//          label :: string()}).

// -record(ast_module,
//         {type = type_not_annotated :: type_annotation(),
//          statements = [] :: [mod_statement()]}).

// -record(ast_module_select,
//         {meta = #meta{} :: meta(),
//          type = type_not_annotated :: type_annotation(),
//          module :: ast_expression(),
//          label :: string()}).

impl<Type> Expr<Type> {
    pub fn meta(&self) -> &Meta {
        match self {
            Expr::Int { meta, .. } => meta,
            Expr::Float { meta, .. } => meta,
            Expr::Atom { meta, .. } => meta,
            Expr::String { meta, .. } => meta,
            Expr::Tuple { meta, .. } => meta,
            Expr::Seq { meta, .. } => meta,
            Expr::Var { meta, .. } => meta,
            Expr::Fun { meta, .. } => meta,
            Expr::Nil { meta, .. } => meta,
            Expr::Cons { meta, .. } => meta,
            Expr::Call { meta, .. } => meta,
            Expr::BinOp { meta, .. } => meta,
            Expr::Let { meta, .. } => meta,
        }
    }
}

impl Expr<typ::Type> {
    pub fn typ(&self) -> typ::Type {
        match self {
            Expr::Int { .. } => typ::int(),
            Expr::Float { .. } => typ::float(),
            Expr::Atom { .. } => typ::atom(),
            Expr::String { .. } => typ::string(),
            Expr::Seq { .. } => unimplemented!(),
            Expr::Tuple { typ, .. } => (*typ).clone(),
            Expr::Var { typ, .. } => (*typ).clone(),
            Expr::Fun { typ, .. } => (*typ).clone(),
            Expr::Nil { typ, .. } => (*typ).clone(),
            Expr::Cons { typ, .. } => (*typ).clone(),
            Expr::Call { typ, .. } => (*typ).clone(),
            Expr::BinOp { typ, .. } => (*typ).clone(),
            Expr::Let { typ, .. } => (*typ).clone(),
        }
    }
}

#[derive(Debug)]
pub struct Meta {}
