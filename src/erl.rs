#[cfg(test)]
mod tests;

use crate::{
    ast::*,
    error::GleamExpect,
    pretty::*,
    typ::{ModuleValueConstructor, PatternConstructor, ValueConstructor, ValueConstructorVariant},
};
use heck::{CamelCase, SnakeCase};
use itertools::Itertools;
use std::char;
use std::default::Default;
use std::sync::Arc;

const INDENT: isize = 4;

#[derive(Debug, Clone)]
struct Env<'a> {
    module: &'a [String],
    current_scope_vars: im::HashMap<String, usize>,
    erl_function_scope_vars: im::HashMap<String, usize>,
}

impl<'a> Env<'a> {
    pub fn new(module: &'a [String]) -> Self {
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

pub fn module(module: &TypedModule) -> String {
    let module_name = module.name.as_slice();
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
            .iter()
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

fn statement(statement: &TypedStatement, module: &[String]) -> Option<Document> {
    match statement {
        Statement::TypeAlias { .. } => None,
        Statement::CustomType { .. } => None,
        Statement::Import { .. } => None,
        Statement::ExternalType { .. } => None,

        Statement::Fn {
            args, name, body, ..
        } => Some(mod_fun(name.as_ref(), args.as_slice(), body, module)),

        Statement::ExternalFn {
            fun,
            module,
            args,
            name,
            ..
        } => Some(external_fun(
            name.as_ref(),
            module.as_ref(),
            fun.as_ref(),
            args.len(),
        )),
    }
}

fn mod_fun(name: &str, args: &[TypedArg], body: &TypedExpr, module: &[String]) -> Document {
    let mut env = Env::new(module);

    atom(name.to_string())
        .append(fun_args(args, &mut env))
        .append(" ->")
        .append(line().append(expr(body, &mut env)).nest(INDENT).group())
        .append(".")
}

fn fun_args(args: &[TypedArg], env: &mut Env) -> Document {
    wrap_args(args.into_iter().map(|a| match &a.names {
        ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => "_".to_doc(),
        ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
            env.next_local_var_name(name.to_string())
        }
    }))
}

fn call_args(args: &[CallArg<TypedExpr>], env: &mut Env) -> Document {
    wrap_args(args.into_iter().map(|arg| wrap_expr(&arg.value, env)))
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

fn string(value: &str) -> Document {
    value.to_doc().surround("<<\"", "\"/utf8>>")
}

fn tuple(elems: impl Iterator<Item = Document>) -> Document {
    concat(elems.intersperse(delim(",")))
        .nest_current()
        .surround("{", "}")
        .group()
}

fn seq(first: &TypedExpr, then: &TypedExpr, env: &mut Env) -> Document {
    force_break()
        .append(expr(first, env))
        .append(",")
        .append(line())
        .append(expr(then, env))
}

// TODO: Surround left or right in parens if required
fn bin_op(name: &BinOp, left: &TypedExpr, right: &TypedExpr, env: &mut Env) -> Document {
    let op = match name {
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

fn pipe(value: &TypedExpr, fun: &TypedExpr, env: &mut Env) -> Document {
    let arg = CallArg {
        label: None,
        location: Default::default(),
        // TODO: remove this clone
        value: value.clone(),
    };
    call(fun, &[arg], env)
}

fn let_(value: &TypedExpr, pat: &TypedPattern, then: &TypedExpr, env: &mut Env) -> Document {
    let body = expr(value, env);
    pattern(pat, env)
        .append(" = ")
        .append(body)
        .append(",")
        .append(line())
        .append(expr(then, env))
}

fn pattern(p: &TypedPattern, env: &mut Env) -> Document {
    match p {
        Pattern::Nil { .. } => "[]".to_doc(),

        Pattern::Let { name, pattern: p } => pattern(p, env)
            .append(" = ")
            .append(env.next_local_var_name(name.to_string())),

        Pattern::Cons { head, tail, .. } => pattern_list_cons(head, tail, env),

        Pattern::Discard { .. } => "_".to_doc(),

        Pattern::Var { name, .. } => env.next_local_var_name(name.to_string()),

        Pattern::Int { value, .. } => value.as_str().to_doc(),

        Pattern::Float { value, .. } => float(value.as_ref()),

        Pattern::String { value, .. } => string(value),

        Pattern::Constructor {
            args,
            constructor: PatternConstructor::Record { name },
            ..
        } => tag_tuple_pattern(name, args, env),

        Pattern::Tuple { elems, .. } => tuple(elems.into_iter().map(|p| pattern(p, env))),
    }
}

fn float(value: &str) -> Document {
    if value.ends_with(".") {
        format!("{}0", value).to_doc()
    } else {
        value.to_string().to_doc()
    }
}

fn pattern_list_cons(head: &TypedPattern, tail: &TypedPattern, env: &mut Env) -> Document {
    list_cons(head, tail, env, pattern, |expr| match expr {
        Pattern::Nil { .. } => ListType::Nil,

        Pattern::Cons { head, tail, .. } => ListType::Cons { head, tail },

        other => ListType::NotList(other),
    })
}

fn expr_list_cons(head: &TypedExpr, tail: &TypedExpr, env: &mut Env) -> Document {
    list_cons(head, tail, env, wrap_expr, |expr| match expr {
        TypedExpr::ListNil { .. } => ListType::Nil,

        TypedExpr::ListCons { head, tail, .. } => ListType::Cons { head, tail },

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

fn var(name: &str, constructor: &ValueConstructor, env: &mut Env) -> Document {
    match &constructor.variant {
        ValueConstructorVariant::Record { name, arity: 0, .. } => atom(name.to_snake_case()),

        ValueConstructorVariant::Record { arity, .. } => {
            let chars = incrementing_args_list(*arity);
            "fun("
                .to_doc()
                .append(chars.clone())
                .append(") -> {")
                .append(name.to_snake_case())
                .append(", ")
                .append(chars)
                .append("} end")
        }

        ValueConstructorVariant::LocalVariable => env.local_var_name(name.to_string()),

        ValueConstructorVariant::ModuleFn {
            arity, ref module, ..
        } if module.as_slice() == env.module => "fun "
            .to_doc()
            .append(atom(name.to_string()))
            .append("/")
            .append(*arity),

        ValueConstructorVariant::ModuleFn { arity, module, .. } => "fun "
            .to_doc()
            .append(module.join("@"))
            .append(":")
            .append(atom(name.to_string()))
            .append("/")
            .append(*arity),
    }
}

fn tag_tuple_pattern(name: &str, args: &[CallArg<TypedPattern>], env: &mut Env) -> Document {
    if args.is_empty() {
        atom(name.to_snake_case())
    } else {
        tuple(
            std::iter::once(atom(name.to_snake_case()))
                .chain(args.into_iter().map(|p| pattern(&p.value, env))),
        )
    }
}

fn clause(clause: &TypedClause, env: &mut Env) -> Document {
    let Clause {
        guard,
        pattern: pat,
        alternative_patterns,
        then,
        ..
    } = clause;

    let docs = std::iter::once(pat)
        .chain(alternative_patterns.into_iter())
        .map(|patterns| {
            let patterns_doc = if patterns.len() == 1 {
                let p = patterns
                    .get(0)
                    .gleam_expect("Single pattern clause printing");
                pattern(p, env)
            } else {
                tuple(patterns.iter().map(|p| pattern(p, env)))
            };
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

        ClauseGuard::GtInt { left, right, .. } => clause_guard(left.as_ref(), env)
            .append(" > ")
            .append(clause_guard(right.as_ref(), env)),

        ClauseGuard::GtEqInt { left, right, .. } => clause_guard(left.as_ref(), env)
            .append(" >= ")
            .append(clause_guard(right.as_ref(), env)),

        ClauseGuard::LtInt { left, right, .. } => clause_guard(left.as_ref(), env)
            .append(" < ")
            .append(clause_guard(right.as_ref(), env)),

        ClauseGuard::LtEqInt { left, right, .. } => clause_guard(left.as_ref(), env)
            .append(" =< ")
            .append(clause_guard(right.as_ref(), env)),

        ClauseGuard::GtFloat { left, right, .. } => clause_guard(left.as_ref(), env)
            .append(" > ")
            .append(clause_guard(right.as_ref(), env)),

        ClauseGuard::GtEqFloat { left, right, .. } => clause_guard(left.as_ref(), env)
            .append(" >= ")
            .append(clause_guard(right.as_ref(), env)),

        ClauseGuard::LtFloat { left, right, .. } => clause_guard(left.as_ref(), env)
            .append(" < ")
            .append(clause_guard(right.as_ref(), env)),

        ClauseGuard::LtEqFloat { left, right, .. } => clause_guard(left.as_ref(), env)
            .append(" =< ")
            .append(clause_guard(right.as_ref(), env)),

        // Only local variables are supported and the typer ensures that all
        // ClauseGuard::Vars are local variables
        ClauseGuard::Var { name, .. } => env.local_var_name(name.to_string()),
    }
}

fn clause_guard(guard: &TypedClauseGuard, env: &mut Env) -> Document {
    match guard {
        // Binary ops are wrapped in parens
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
        | ClauseGuard::LtEqFloat { .. } => "("
            .to_doc()
            .append(bare_clause_guard(guard, env))
            .append(")"),

        // Values are not wrapped
        ClauseGuard::Var { .. } => bare_clause_guard(guard, env),
    }
}

fn clauses(cs: &[TypedClause], env: &mut Env) -> Document {
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

fn case(subjects: &[TypedExpr], cs: &[TypedClause], env: &mut Env) -> Document {
    let subjects_doc = if subjects.len() == 1 {
        let subject = subjects
            .get(0)
            .gleam_expect("erl case printing of single subject");
        wrap_expr(subject, env).group()
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

fn call(fun: &TypedExpr, args: &[CallArg<TypedExpr>], env: &mut Env) -> Document {
    match fun {
        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { name },
            ..
        }
        | TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::Record { name, .. },
                    ..
                },
            ..
        } => tuple(
            std::iter::once(atom(name.to_snake_case()))
                .chain(args.into_iter().map(|arg| expr(&arg.value, env))),
        ),

        TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::ModuleFn { module, name, .. },
                    ..
                },
            ..
        } => {
            if module.as_slice() == env.module {
                atom(name.to_string()).append(call_args(args, env))
            } else {
                module
                    .join("@")
                    .to_doc()
                    .append(":")
                    .append(atom(name.to_string()))
                    .append(call_args(args, env))
            }
        }

        TypedExpr::ModuleSelect {
            module_name,
            label,
            constructor: ModuleValueConstructor::Fn,
            ..
        } => module_name
            .join("@")
            .to_doc()
            .append(":")
            .append(atom(label.to_string()))
            .append(call_args(args, env)),

        call @ TypedExpr::Call { .. } => expr(call, env)
            .surround("(", ")")
            .append(call_args(args, env)),

        TypedExpr::Fn {
            is_capture: true,
            body,
            ..
        } => {
            if let TypedExpr::Call {
                fun,
                args: inner_args,
                ..
            } = body.as_ref()
            {
                // TODO: this is a mess of cloning.
                let merged_args = inner_args
                    .iter()
                    .map(|a| match &a.value {
                        TypedExpr::Var { name, .. } if name == "capture@1" => args
                            .get(0)
                            .gleam_expect("Erl printing: capture call replacing arg")
                            .clone(),
                        _ => a.clone(),
                    })
                    .collect::<Vec<_>>();
                call(fun, merged_args.as_slice(), env)
            } else {
                unreachable!()
            }
        }

        fun @ TypedExpr::Fn { .. } => expr(fun, env)
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
fn wrap_expr(expression: &TypedExpr, env: &mut Env) -> Document {
    match &expression {
        TypedExpr::Seq { .. } => begin_end(expr(expression, env)),
        TypedExpr::Let { .. } => begin_end(expr(expression, env)),
        _ => expr(expression, env),
    }
}

fn expr(expression: &TypedExpr, env: &mut Env) -> Document {
    match expression {
        TypedExpr::ListNil { .. } => "[]".to_doc(),
        TypedExpr::Todo { .. } => "erlang:error({gleam_error, todo})".to_doc(),
        TypedExpr::Int { value, .. } => value.as_str().to_doc(),
        TypedExpr::Float { value, .. } => float(value.as_ref()),
        TypedExpr::String { value, .. } => string(value),
        TypedExpr::Seq { first, then, .. } => seq(first, then, env),
        TypedExpr::Pipe { left, right, .. } => pipe(left, right, env),

        TypedExpr::TupleIndex { tuple, index, .. } => tuple_index(tuple, *index, env),

        TypedExpr::Var {
            name, constructor, ..
        } => var(name, constructor, env),

        TypedExpr::Fn { args, body, .. } => fun(args, body, env),

        TypedExpr::ListCons { head, tail, .. } => expr_list_cons(head, tail, env),

        TypedExpr::Call { fun, args, .. } => call(fun, args, env),

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { name },
            ..
        } => atom(name.to_snake_case()),

        TypedExpr::ModuleSelect {
            typ,
            label,
            module_name,
            constructor: ModuleValueConstructor::Fn,
            ..
        } => module_select_fn(typ.clone(), module_name, label),

        TypedExpr::RecordAccess { record, index, .. } => tuple_index(record, index + 1, env),

        TypedExpr::Let {
            value,
            pattern,
            then,
            ..
        } => let_(value, pattern, then, env),

        TypedExpr::Case {
            subjects, clauses, ..
        } => case(subjects, clauses.as_slice(), env),

        TypedExpr::BinOp {
            name, left, right, ..
        } => bin_op(&name, left, right, env),

        TypedExpr::Tuple { elems, .. } => tuple(elems.into_iter().map(|e| wrap_expr(e, env))),
    }
}

fn tuple_index(tuple: &TypedExpr, index: u64, env: &mut Env) -> Document {
    use std::iter::once;
    let index_doc = format!("{}", (index + 1)).to_doc();
    let tuple_doc = expr(tuple, env);
    let iter = once(index_doc).chain(once(tuple_doc));
    "erlang:element".to_doc().append(wrap_args(iter))
}

fn module_select_fn(typ: Arc<crate::typ::Type>, module_name: &[String], label: &str) -> Document {
    match crate::typ::collapse_links(typ).as_ref() {
        crate::typ::Type::Fn { args, .. } => "fun "
            .to_doc()
            .append(module_name.join("@"))
            .append(":")
            .append(atom(label.to_string()))
            .append("/")
            .append(args.len()),

        _ => module_name
            .join("@")
            .to_doc()
            .append(":")
            .append(label.to_string())
            .append("()"),
    }
}

fn fun(args: &[TypedArg], body: &TypedExpr, env: &mut Env) -> Document {
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

fn external_fun(name: &str, module: &str, fun: &str, arity: usize) -> Document {
    let chars: String = incrementing_args_list(arity);

    atom(name.to_string())
        .append(format!("({}) ->", chars))
        .append(line())
        .append(atom(module.to_string()))
        .append(":")
        .append(atom(fun.to_string()))
        .append(format!("({}).", chars))
        .nest(INDENT)
}
