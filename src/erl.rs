#[cfg(test)]
mod tests;

use crate::ast::*;
use crate::pretty::*;
use crate::typ::{
    ModuleValueConstructor, PatternConstructor, ValueConstructor, ValueConstructorVariant,
};
use heck::{CamelCase, SnakeCase};
use itertools::Itertools;
use std::char;
use std::default::Default;
use std::sync::Arc;

const INDENT: isize = 4;

#[derive(Debug, Clone)]
struct Env<'a> {
    module: &'a Vec<String>,
    current_scope_vars: im::HashMap<String, usize>,
    erl_function_scope_vars: im::HashMap<String, usize>,
}

impl<'a> Env<'a> {
    pub fn new(module: &'a Vec<String>) -> Self {
        Self {
            current_scope_vars: Default::default(),
            erl_function_scope_vars: Default::default(),
            module,
        }
    }

    pub fn local_var_name(&mut self, name: String) -> Document {
        match self.current_scope_vars.get(&name) {
            None => {
                self.current_scope_vars.insert(name.clone(), 0);
                self.erl_function_scope_vars.insert(name.clone(), 0);
                name.to_camel_case().to_doc()
            }
            Some(0) => name.to_camel_case().to_doc(),
            Some(n) => name.to_camel_case().to_doc().append(*n),
        }
    }

    pub fn next_local_var_name(&mut self, name: String) -> Document {
        let next = self.erl_function_scope_vars.get(&name).map_or(0, |i| i + 1);
        self.erl_function_scope_vars.insert(name.clone(), next);
        self.current_scope_vars.insert(name.clone(), next);
        self.local_var_name(name)
    }
}

pub fn records(module: &TypedModule) -> Vec<(&str, String)> {
    module
        .statements
        .iter()
        .flat_map(|s| match s {
            Statement::CustomType {
                public: true,
                constructors,
                ..
            } => &constructors[..],
            _ => &[],
        })
        .filter(|constructor| !constructor.args.is_empty())
        .flat_map(|constructor| {
            let mut fields = Vec::with_capacity(constructor.args.len());
            for (label, _) in constructor.args.iter() {
                match label {
                    Some(s) => fields.push(&**s),
                    None => return None,
                }
            }
            Some((&constructor.name, fields))
        })
        .map(|(name, fields)| (name.as_ref(), record_definition(&name, &fields[..])))
        .collect()
}

pub fn record_definition(name: &str, fields: &[&str]) -> String {
    use std::fmt::Write;
    let mut buffer = format!("-record({}, {{", name.to_snake_case());
    for field in fields.iter().intersperse(&", ") {
        write!(buffer, "{}", field).unwrap();
    }
    writeln!(buffer, "}}).").unwrap();
    buffer
}

pub fn module(module: TypedModule) -> String {
    let module_name = module.name;
    let exports = concat(
        module
            .statements
            .iter()
            .flat_map(|s| match s {
                Statement::Fn {
                    public: true,
                    name,
                    args,
                    ..
                } => Some((name.clone(), args.len())),

                Statement::ExternalFn {
                    public: true,
                    name,
                    args,
                    ..
                } => Some((name.clone(), args.len())),

                _ => None,
            })
            .map(|(n, a)| atom(n).append("/").append(a))
            .intersperse(", ".to_doc()),
    );

    let statements = concat(
        module
            .statements
            .into_iter()
            .flat_map(|s| statement(s, &module_name))
            .intersperse(lines(2)),
    );

    format!("-module({}).", module_name.join("@"))
        .to_doc()
        .append(line())
        .append("-compile(no_auto_import).")
        .append(lines(2))
        .append(if exports == nil() {
            nil()
        } else {
            "-export(["
                .to_doc()
                .append(exports)
                .append("]).")
                .append(lines(2))
        })
        .append(statements)
        .append(line())
        .format(80)
}

fn statement(statement: TypedStatement, module: &Vec<String>) -> Option<Document> {
    match statement {
        Statement::TypeAlias { .. } => None,
        Statement::CustomType { .. } => None,
        Statement::Import { .. } => None,
        Statement::ExternalType { .. } => None,
        Statement::Fn {
            args, name, body, ..
        } => Some(mod_fun(name, args, body, module)),
        Statement::ExternalFn {
            fun,
            module,
            args,
            name,
            ..
        } => Some(external_fun(name, module, fun, args.len())),
    }
}

fn mod_fun(name: String, args: Vec<Arg>, body: TypedExpr, module: &Vec<String>) -> Document {
    let mut env = Env::new(module);

    atom(name)
        .append(fun_args(args, &mut env))
        .append(" ->")
        .append(line().append(expr(body, &mut env)).nest(INDENT).group())
        .append(".")
}

fn fun_args(args: Vec<Arg>, env: &mut Env) -> Document {
    wrap_args(args.into_iter().map(|a| match a.names {
        ArgNames::Discard | ArgNames::LabelledDiscard { .. } => "_".to_doc(),
        ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
            env.next_local_var_name(name)
        }
    }))
}

fn call_args(args: Vec<CallArg<TypedExpr>>, env: &mut Env) -> Document {
    wrap_args(args.into_iter().map(|arg| wrap_expr(arg.value, env)))
}

fn wrap_args<I>(args: I) -> Document
where
    I: Iterator<Item = Document>,
{
    break_("", "")
        .append(concat(args.intersperse(delim(","))))
        .nest(INDENT)
        .append(break_("", ""))
        .surround("(", ")")
        .group()
}

fn atom(value: String) -> Document {
    use regex::Regex;
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^[a-z][a-z0-9_@]*$").unwrap();
    }

    match &*value {
        // Escape because of keyword collision
        "!" | "receive" | "bnot" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr"
        | "not" | "and" | "or" | "xor" | "orelse" | "andalso" | "when" | "end" | "fun" | "try"
        | "catch" | "after" => format!("'{}'", value).to_doc(),

        // No need to escape
        _ if RE.is_match(&value) => value.to_doc(),

        // Escape because of characters contained
        _ => value.to_doc().surround("'", "'"),
    }
}

fn string(value: String) -> Document {
    value.to_doc().surround("<<\"", "\">>")
}

fn tuple(elems: impl Iterator<Item = Document>) -> Document {
    concat(elems.intersperse(delim(",")))
        .nest_current()
        .surround("{", "}")
        .group()
}

fn seq(first: TypedExpr, then: TypedExpr, env: &mut Env) -> Document {
    force_break()
        .append(expr(first, env))
        .append(",")
        .append(line())
        .append(expr(then, env))
}

// TODO: Surround left or right in parens if required
fn bin_op(name: BinOp, left: TypedExpr, right: TypedExpr, env: &mut Env) -> Document {
    let op = match name {
        BinOp::Pipe => return pipe(left, right, env),
        BinOp::And => "andalso",
        BinOp::Or => "orelse",
        BinOp::LtInt | BinOp::LtFloat => "<",
        BinOp::LtEqInt | BinOp::LtEqFloat => "=<",
        BinOp::Eq => "=:=",
        BinOp::NotEq => "/=",
        BinOp::GtInt | BinOp::GtFloat => ">",
        BinOp::GtEqInt | BinOp::GtEqFloat => ">=",
        BinOp::AddInt => "+",
        BinOp::AddFloat => "+",
        BinOp::SubInt => "-",
        BinOp::SubFloat => "-",
        BinOp::MultInt => "*",
        BinOp::MultFloat => "*",
        BinOp::DivInt => "div",
        BinOp::DivFloat => "/",
        BinOp::ModuloInt => "rem",
    };

    expr(left, env)
        .append(break_("", " "))
        .append(op)
        .append(" ")
        .append(expr(right, env))
}

fn pipe(value: TypedExpr, fun: TypedExpr, env: &mut Env) -> Document {
    call(
        fun,
        vec![CallArg {
            label: None,
            meta: Default::default(),
            value,
        }],
        env,
    )
}

fn let_(value: TypedExpr, pat: TypedPattern, then: TypedExpr, env: &mut Env) -> Document {
    let body = expr(value, env);
    pattern(pat, env)
        .append(" = ")
        .append(body)
        .append(",")
        .append(line())
        .append(expr(then, env))
}

fn pattern(p: TypedPattern, env: &mut Env) -> Document {
    match p {
        Pattern::Nil { .. } => "[]".to_doc(),

        Pattern::Let { name, pattern: p } => pattern(*p, env)
            .append(" = ")
            .append(env.next_local_var_name(name)),

        Pattern::Cons { head, tail, .. } => pattern_list_cons(*head, *tail, env),

        Pattern::Discard { .. } => "_".to_doc(),

        Pattern::Var { name, .. } => env.next_local_var_name(name),

        Pattern::Int { value, .. } => value.to_doc(),

        Pattern::Float { value, .. } => value.to_doc(),

        Pattern::String { value, .. } => string(value),

        Pattern::Constructor {
            args,
            constructor: PatternConstructor::Record { name },
            ..
        } => tag_tuple_pattern(name, args, env),

        Pattern::Tuple { elems, .. } => tuple(elems.into_iter().map(|p| pattern(p, env))),
    }
}

fn pattern_list_cons(head: TypedPattern, tail: TypedPattern, env: &mut Env) -> Document {
    list_cons(head, tail, env, pattern, |expr| match expr {
        Pattern::Nil { .. } => ListType::Nil,

        Pattern::Cons { head, tail, .. } => ListType::Cons {
            head: *head,
            tail: *tail,
        },

        other => ListType::NotList(other),
    })
}

fn expr_list_cons(head: TypedExpr, tail: TypedExpr, env: &mut Env) -> Document {
    list_cons(head, tail, env, wrap_expr, |expr| match expr {
        Expr::Nil { .. } => ListType::Nil,

        Expr::Cons { head, tail, .. } => ListType::Cons {
            head: *head,
            tail: *tail,
        },

        other => ListType::NotList(other),
    })
}

fn list_cons<ToDoc, Categorise, Elem>(
    head: Elem,
    tail: Elem,
    env: &mut Env,
    to_doc: ToDoc,
    categorise_element: Categorise,
) -> Document
where
    ToDoc: Fn(Elem, &mut Env) -> Document,
    Categorise: Fn(Elem) -> ListType<Elem, Elem>,
{
    let mut elems = vec![head];
    let final_tail = collect_cons(tail, &mut elems, categorise_element);

    let elems = concat(
        elems
            .into_iter()
            .map(|e| to_doc(e, env))
            .intersperse(delim(",")),
    );

    let elems = if let Some(final_tail) = final_tail {
        elems.append(delim(" |")).append(to_doc(final_tail, env))
    } else {
        elems
    };

    elems.to_doc().nest_current().surround("[", "]").group()
}

fn collect_cons<F, E, T>(e: T, elems: &mut Vec<E>, f: F) -> Option<T>
where
    F: Fn(T) -> ListType<E, T>,
{
    match f(e) {
        ListType::Nil => None,

        ListType::Cons { head, tail } => {
            elems.push(head);
            collect_cons(tail, elems, f)
        }

        ListType::NotList(other) => Some(other),
    }
}

enum ListType<E, T> {
    Nil,
    Cons { head: E, tail: T },
    NotList(T),
}

fn var(name: String, constructor: ValueConstructor, env: &mut Env) -> Document {
    match constructor.variant {
        ValueConstructorVariant::Record { name, arity: 0, .. } => atom(name.to_snake_case()),

        ValueConstructorVariant::Record { arity, .. } => {
            let chars = incrementing_args_list(arity);
            "fun("
                .to_doc()
                .append(chars.clone())
                .append(") -> {")
                .append(name.to_snake_case())
                .append(", ")
                .append(chars)
                .append("} end")
        }

        ValueConstructorVariant::LocalVariable => env.local_var_name(name),

        ValueConstructorVariant::ModuleFn {
            arity, ref module, ..
        } if module == env.module => "fun ".to_doc().append(atom(name)).append("/").append(arity),

        ValueConstructorVariant::ModuleFn { arity, module, .. } => "fun "
            .to_doc()
            .append(module.join("@"))
            .append(":")
            .append(atom(name))
            .append("/")
            .append(arity),
    }
}

fn tag_tuple_pattern(name: String, args: Vec<CallArg<TypedPattern>>, env: &mut Env) -> Document {
    if args.is_empty() {
        atom(name.to_snake_case())
    } else {
        tuple(
            std::iter::once(atom(name.to_snake_case()))
                .chain(args.into_iter().map(|p| pattern(p.value, env))),
        )
    }
}

fn clause(clause: TypedClause, env: &mut Env) -> Document {
    let Clause {
        guard,
        pattern: pat,
        alternative_patterns,
        then,
        ..
    } = clause;

    let docs = std::iter::once(pat)
        .chain(alternative_patterns.into_iter())
        .map(|mut patterns| {
            let patterns_doc = if patterns.len() == 1 {
                pattern(patterns.remove(0), env)
            } else {
                tuple(patterns.into_iter().map(|p| pattern(p, env)))
            };
            let then = then.clone(); // TODO: remove this clone by having expr take a reference
            patterns_doc
                .append(optional_clause_guard(guard.as_ref(), env))
                .append(" ->")
                .append(line().append(expr(then, env)).nest(INDENT).group())
        })
        .intersperse(";".to_doc().append(lines(2)));
    concat(docs)
}

fn optional_clause_guard(guard: Option<&TypedClauseGuard>, env: &mut Env) -> Document {
    match guard {
        Some(guard) => " when ".to_doc().append(bare_clause_guard(guard, env)),
        None => nil(),
    }
}

fn bare_clause_guard(guard: &TypedClauseGuard, env: &mut Env) -> Document {
    match guard {
        ClauseGuard::Or { left, right, .. } => clause_guard(left.as_ref(), env)
            .append(" orelse ")
            .append(clause_guard(right.as_ref(), env)),

        ClauseGuard::And { left, right, .. } => clause_guard(left.as_ref(), env)
            .append(" andalso ")
            .append(clause_guard(right.as_ref(), env)),

        ClauseGuard::Equals { left, right, .. } => clause_guard(left.as_ref(), env)
            .append(" =:= ")
            .append(clause_guard(right.as_ref(), env)),

        ClauseGuard::NotEquals { left, right, .. } => clause_guard(left.as_ref(), env)
            .append(" =/= ")
            .append(clause_guard(right.as_ref(), env)),

        // Only local variables are supported and the typer ensures that all
        // ClauseGuard::Vars are local variables
        ClauseGuard::Var { name, .. } => env.local_var_name(name.to_string()),
    }
}

fn clause_guard(guard: &TypedClauseGuard, env: &mut Env) -> Document {
    match *guard {
        // Binary ops are wrapped in parens
        ClauseGuard::Or { .. }
        | ClauseGuard::And { .. }
        | ClauseGuard::Equals { .. }
        | ClauseGuard::NotEquals { .. } => "("
            .to_doc()
            .append(bare_clause_guard(guard, env))
            .append(")"),

        // Values are not wrapped
        ClauseGuard::Var { .. } => bare_clause_guard(guard, env),
    }
}

fn clauses(cs: Vec<TypedClause>, env: &mut Env) -> Document {
    concat(
        cs.into_iter()
            .map(|c| {
                let vars = env.current_scope_vars.clone();
                let erl = clause(c, env);
                env.current_scope_vars = vars; // Reset the known variables now the clauses' scope has ended
                erl
            })
            .intersperse(";".to_doc().append(lines(2))),
    )
}

fn case(mut subjects: Vec<TypedExpr>, cs: Vec<TypedClause>, env: &mut Env) -> Document {
    let subjects_doc = if subjects.len() == 1 {
        wrap_expr(subjects.remove(0), env).group()
    } else {
        tuple(subjects.into_iter().map(|e| wrap_expr(e, env)))
    };
    "case "
        .to_doc()
        .append(subjects_doc)
        .append(" of")
        .append(line().append(clauses(cs, env)).nest(INDENT))
        .append(line())
        .append("end")
        .group()
}

fn call(fun: TypedExpr, args: Vec<CallArg<TypedExpr>>, env: &mut Env) -> Document {
    match fun {
        Expr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { name },
            ..
        }
        | Expr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::Record { name, .. },
                    ..
                },
            ..
        } => tuple(
            std::iter::once(atom(name.to_snake_case()))
                .chain(args.into_iter().map(|arg| expr(arg.value, env))),
        ),

        Expr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::ModuleFn { module, name, .. },
                    ..
                },
            ..
        } => {
            if &module == env.module {
                atom(name).append(call_args(args, env))
            } else {
                module
                    .join("@")
                    .to_doc()
                    .append(":")
                    .append(atom(name))
                    .append(call_args(args, env))
            }
        }

        Expr::ModuleSelect {
            module_name,
            label,
            constructor: ModuleValueConstructor::Fn,
            ..
        } => module_name
            .join("@")
            .to_doc()
            .append(":")
            .append(atom(label))
            .append(call_args(args, env)),

        call @ Expr::Call { .. } => expr(call, env)
            .surround("(", ")")
            .append(call_args(args, env)),

        Expr::Fn {
            is_capture: true,
            body,
            ..
        } => {
            if let Expr::Call {
                fun,
                args: inner_args,
                ..
            } = *body
            {
                let mut args = args;
                let merged_args = inner_args
                    .into_iter()
                    .map(|a| match &a.value {
                        Expr::Var { name, .. } if name == "capture@1" => args.pop().unwrap(),
                        _ => a,
                    })
                    .collect();
                call(*fun, merged_args, env)
            } else {
                unreachable!()
            }
        }

        fun @ Expr::Fn { .. } => expr(fun, env)
            .surround("(", ")")
            .append(call_args(args, env)),

        other => expr(other, env).append(call_args(args, env)),
    }
}

/// Wrap a document in begin end
///
fn begin_end(document: Document) -> Document {
    force_break()
        .append("begin")
        .append(line().append(document).nest(INDENT))
        .append(line())
        .append("end")
}

/// Same as expr, expect it wraps seq, let, etc in begin end
///
fn wrap_expr(expression: TypedExpr, env: &mut Env) -> Document {
    match &expression {
        Expr::Seq { .. } => begin_end(expr(expression, env)),
        Expr::Let { .. } => begin_end(expr(expression, env)),
        _ => expr(expression, env),
    }
}

fn expr(expression: TypedExpr, env: &mut Env) -> Document {
    match expression {
        Expr::Nil { .. } => "[]".to_doc(),
        Expr::Todo { .. } => "erlang:error({gleam_error, todo})".to_doc(),
        Expr::Int { value, .. } => value.to_doc(),
        Expr::Float { value, .. } => value.to_doc(),
        Expr::String { value, .. } => string(value),
        Expr::Seq { first, then, .. } => seq(*first, *then, env),

        Expr::TupleIndex { tuple, index, .. } => tuple_index(*tuple, index, env),

        Expr::Var {
            name, constructor, ..
        } => var(name, constructor, env),

        Expr::Fn { args, body, .. } => fun(args, *body, env),

        Expr::Cons { head, tail, .. } => expr_list_cons(*head, *tail, env),

        Expr::Call { fun, args, .. } => call(*fun, args, env),

        Expr::FieldSelect {
            label, container, ..
        } => map_select(*container, label, env),

        Expr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { name },
            ..
        } => atom(name.to_snake_case()),

        Expr::ModuleSelect {
            typ,
            label,
            module_name,
            constructor: ModuleValueConstructor::Fn,
            ..
        } => module_select_fn(typ, module_name, label),

        Expr::Let {
            value,
            pattern,
            then,
            ..
        } => let_(*value, pattern, *then, env),

        Expr::Case {
            subjects, clauses, ..
        } => case(subjects, clauses, env),

        Expr::BinOp {
            name, left, right, ..
        } => bin_op(name, *left, *right, env),

        Expr::Tuple { elems, .. } => tuple(elems.into_iter().map(|e| wrap_expr(e, env))),
    }
}

fn tuple_index(tuple: TypedExpr, index: u64, env: &mut Env) -> Document {
    use std::iter::once;
    let index_doc = format!("{}", (index + 1)).to_doc();
    let tuple_doc = expr(tuple, env);
    let iter = once(index_doc).chain(once(tuple_doc));
    "erlang:element".to_doc().append(wrap_args(iter))
}

fn module_select_fn(
    typ: Arc<crate::typ::Type>,
    module_name: Vec<String>,
    label: String,
) -> Document {
    match (*typ).clone().collapse_links() {
        crate::typ::Type::Fn { args, .. } => "fun "
            .to_doc()
            .append(module_name.join("@"))
            .append(":")
            .append(atom(label))
            .append("/")
            .append(args.len()),

        _ => module_name
            .join("@")
            .to_doc()
            .append(":")
            .append(label)
            .append("()"),
    }
}

fn map_select(map: TypedExpr, label: String, env: &mut Env) -> Document {
    "maps:get("
        .to_doc()
        .append(atom(label))
        .append(", ")
        .append(wrap_expr(map, env))
        .append(")")
}

fn fun(args: Vec<Arg>, body: TypedExpr, env: &mut Env) -> Document {
    "fun"
        .to_doc()
        .append(fun_args(args, env).append(" ->"))
        .append(break_("", " ").append(expr(body, env)).nest(INDENT))
        .append(break_("", " "))
        .append("end")
        .group()
}

fn incrementing_args_list(arity: usize) -> String {
    (65..(65 + arity))
        .map(|x| x as u8 as char)
        .map(|c| c.to_string())
        .intersperse(", ".to_string())
        .collect()
}

fn external_fun(name: String, module: String, fun: String, arity: usize) -> Document {
    let chars: String = incrementing_args_list(arity);

    atom(name)
        .append(format!("({}) ->", chars))
        .append(line())
        .append(atom(module))
        .append(":")
        .append(atom(fun))
        .append(format!("({}).", chars))
        .nest(INDENT)
}
