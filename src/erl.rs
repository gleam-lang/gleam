#[cfg(test)]
mod tests;

use crate::{
    ast::*,
    error::GleamExpect,
    pretty::*,
    project::{self, Analysed, OutputFile},
    typ::{
        ModuleValueConstructor, PatternConstructor, Type, ValueConstructor, ValueConstructorVariant,
    },
};
use heck::{CamelCase, SnakeCase};
use itertools::Itertools;
use std::borrow::Cow;
use std::char;
use std::default::Default;
use std::sync::Arc;

const INDENT: isize = 4;

pub fn generate_erlang(analysed: &[Analysed]) -> Vec<OutputFile> {
    let mut files = Vec::with_capacity(analysed.len() * 2);

    for Analysed {
        name,
        origin,
        source_base_path,
        ast,
        ..
    } in analysed
    {
        let gen_dir = source_base_path
            .parent()
            .unwrap()
            .join(project::OUTPUT_DIR_NAME)
            .join(origin.dir_name());
        let erl_module_name = name.join("@");

        for (name, text) in crate::erl::records(&ast).into_iter() {
            files.push(OutputFile {
                path: gen_dir.join(format!("{}_{}.hrl", erl_module_name, name)),
                text,
            })
        }

        files.push(OutputFile {
            path: gen_dir.join(format!("{}.erl", erl_module_name)),
            text: crate::erl::module(&ast),
        });
    }

    files
}

#[derive(Debug, Clone)]
struct Env<'a, 'b> {
    module: &'a [String],
    current_scope_vars: im::HashMap<Cow<'b, str>, usize>,
    erl_function_scope_vars: im::HashMap<Cow<'b, str>, usize>,
}

impl<'a, 'b> Env<'a, 'b> {
    pub fn new(module: &'a [String]) -> Self {
        Self {
            current_scope_vars: Default::default(),
            erl_function_scope_vars: Default::default(),
            module,
        }
    }

    pub fn local_var_name(&mut self, name: Cow<'b, str>) -> Document<'b> {
        match self.current_scope_vars.get(&name) {
            None => {
                self.current_scope_vars.insert(name.clone(), 0);
                let doc = name.to_camel_case().to_doc();
                self.erl_function_scope_vars.insert(name, 0);
                doc
            }
            Some(0) => name.to_camel_case().to_doc(),
            Some(n) => name.to_camel_case().to_doc().append(*n),
        }
    }

    pub fn next_local_var_name(&mut self, name: Cow<'b, str>) -> Document<'b> {
        // TODO: investigate if these cloning are all required.
        // If they really are, then explain why.
        let name = name.to_owned();
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
            for (label, ..) in constructor.args.iter() {
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
    let name = &name.to_snake_case();
    let escaped_name = if is_reserved_word(name) {
        format!("'{}'", name)
    } else {
        format!("{}", name)
    };
    use std::fmt::Write;
    let mut buffer = format!("-record({}, {{", escaped_name);
    for field in fields.iter().intersperse(&", ") {
        let escaped_field = if is_reserved_word(field) {
            format!("'{}'", field)
        } else {
            format!("{}", field)
        };
        write!(buffer, "{}", escaped_field).unwrap();
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
            .map(|(n, a)| atom(n.into()).append("/").append(a))
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

fn statement<'a>(statement: &'a TypedStatement, module: &[String]) -> Option<Document<'a>> {
    match statement {
        Statement::TypeAlias { .. } => None,
        Statement::CustomType { .. } => None,
        Statement::Import { .. } => None,
        Statement::ExternalType { .. } => None,

        Statement::Fn {
            args, name, body, ..
        } => Some(mod_fun(name.as_ref(), args.as_slice(), body, module)),

        Statement::ExternalFn { public: false, .. } => None,
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

fn mod_fun<'a>(
    name: &'a str,
    args: &'a [TypedArg],
    body: &'a TypedExpr,
    module: &[String],
) -> Document<'a> {
    let mut env = Env::new(module);

    atom(name.into())
        .append(fun_args(Cow::Borrowed(args), &mut env))
        .append(" ->")
        .append(
            line()
                .append(expr(Cow::Borrowed(body), &mut env))
                .nest(INDENT)
                .group(),
        )
        .append(".")
}

fn fun_args<'a>(args: Cow<'a, [TypedArg]>, env: &mut Env<'_, 'a>) -> Document<'a> {
    match args {
        Cow::Borrowed(args) => wrap_args(args.iter().map(|a| match &a.names {
            ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => "_".to_doc(),
            ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
                env.next_local_var_name(Cow::Borrowed(name))
            }
        })),
        Cow::Owned(args) => wrap_args(args.into_iter().map(|a| match a.names {
            ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => "_".to_doc(),
            ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
                env.next_local_var_name(Cow::Owned(name))
            }
        })),
    }
}

fn call_args<'a, A: Into<Cow<'a, [CallArg<TypedExpr>]>>>(
    args: A,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    let args = args.into();

    match args {
        Cow::Borrowed(args_slice) => wrap_args(
            args_slice
                .iter()
                .map(|arg| &arg.value)
                .map(Cow::Borrowed)
                .map(|arg| wrap_expr(arg, env)),
        ),
        Cow::Owned(args_vector) => wrap_args(
            args_vector
                .into_iter()
                .map(|arg| arg.value)
                .map(Cow::Owned)
                .map(|arg| wrap_expr(arg, env)),
        ),
    }
}

fn wrap_args<'a, I>(args: I) -> Document<'a>
where
    I: Iterator<Item = Document<'a>>,
{
    break_("", "")
        .append(concat(args.intersperse(delim(","))))
        .nest(INDENT)
        .append(break_("", ""))
        .surround("(", ")")
        .group()
}

fn atom<'a>(value: Cow<'a, str>) -> Document<'a> {
    use regex::Regex;
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^[a-z][a-z0-9_@]*$").unwrap();
    }

    match value {
        // Escape because of keyword collision
        value if is_reserved_word(&value) => format!("'{}'", value).to_doc(),

        // No need to escape
        v if RE.is_match(&v) => v.to_doc(),

        // Escape because of characters contained
        v => v.to_doc().surround("'", "'"),
    }
}

fn string<'a>(value: Cow<'a, str>) -> Document<'a> {
    value.to_doc().surround("<<\"", "\"/utf8>>")
}

fn tuple<'a>(elems: impl Iterator<Item = Document<'a>>) -> Document<'a> {
    concat(elems.intersperse(delim(",")))
        .nest_current()
        .surround("{", "}")
        .group()
}

fn bitstring<'a>(elems: impl Iterator<Item = Document<'a>>) -> Document<'a> {
    concat(elems.intersperse(delim(",")))
        .nest_current()
        .surround("<<", ">>")
        .group()
}

fn segment<'a>(
    value: Cow<'a, TypedExpr>,
    options: Vec<TypedExprBinSegmentOption>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    use Cow::*;
    use TypedExpr::*;

    let mut document = match value {
        // Skip the normal <<value/utf8>> surrounds
        Borrowed(String { value, .. }) => value.as_str().to_doc().surround("\"", "\""),
        Owned(String { value, .. }) => value.to_doc().surround("\"", "\""),

        // As normal
        Borrowed(TypedExpr::Int { .. })
        | Borrowed(Float { .. })
        | Borrowed(Var { .. })
        | Borrowed(Bitstring { .. })
        | Owned(TypedExpr::Int { .. })
        | Owned(Float { .. })
        | Owned(Var { .. })
        | Owned(Bitstring { .. }) => expr(value, env),

        // Wrap anything else in parentheses
        value => expr(value, env).surround("(", ")"),
    };

    let mut size: Option<Document> = None;
    let mut unit: Option<Document> = None;
    let mut others = Vec::new();

    options.into_iter().for_each(|option| match option {
        BinSegmentOption::Invalid { .. } => (),

        BinSegmentOption::Integer { .. } => others.push("integer"),
        BinSegmentOption::Float { .. } => others.push("float"),
        BinSegmentOption::Binary { .. } => others.push("binary"),
        BinSegmentOption::Bitstring { .. } => others.push("bitstring"),
        BinSegmentOption::UTF8 { .. } => others.push("utf8"),
        BinSegmentOption::UTF16 { .. } => others.push("utf16"),
        BinSegmentOption::UTF32 { .. } => others.push("utf32"),
        BinSegmentOption::Signed { .. } => others.push("signed"),
        BinSegmentOption::Unsigned { .. } => others.push("unsigned"),
        BinSegmentOption::Big { .. } => others.push("big"),
        BinSegmentOption::Little { .. } => others.push("little"),
        BinSegmentOption::Native { .. } => others.push("native"),
        BinSegmentOption::Size { value, .. } => {
            size = Some(":".to_doc().append(expr(Cow::Owned(*value), env)))
        }
        BinSegmentOption::Unit { value, .. } => {
            unit = Some(":".to_doc().append(expr(Cow::Owned(*value), env)))
        }
    });

    document = document.append(size);

    if !others.is_empty() || unit.is_some() {
        document = document.append("/").append(others.join("-"));
    }

    document.append(unit)
}

// TODO: Merge segment() and pattern_segment() somehow
fn pattern_segment<'a>(
    value: Cow<'a, TypedPattern>,
    options: Vec<TypedPatternBinSegmentOption>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    use Cow::*;
    use Pattern::*;

    let mut document = match value {
        // Skip the normal <<value/utf8>> surrounds
        Borrowed(String { value, .. }) => value.as_str().to_doc().surround("\"", "\""),
        Owned(String { value, .. }) => value.to_doc().surround("\"", "\""),

        // As normal
        Borrowed(Var { .. })
        | Borrowed(Int { .. })
        | Borrowed(Float { .. })
        | Owned(Var { .. })
        | Owned(Int { .. })
        | Owned(Float { .. }) => pattern(value, env),

        // No other pattern variants are allowed in pattern bit string segments
        _ => Document::Nil,
    };

    let mut size: Option<Document> = None;
    let mut unit: Option<Document> = None;
    let mut others = Vec::new();

    options.into_iter().for_each(|option| match option {
        BinSegmentOption::Invalid { .. } => (),

        BinSegmentOption::Integer { .. } => others.push("integer"),
        BinSegmentOption::Float { .. } => others.push("float"),
        BinSegmentOption::Binary { .. } => others.push("binary"),
        BinSegmentOption::Bitstring { .. } => others.push("bitstring"),
        BinSegmentOption::UTF8 { .. } => others.push("utf8"),
        BinSegmentOption::UTF16 { .. } => others.push("utf16"),
        BinSegmentOption::UTF32 { .. } => others.push("utf32"),
        BinSegmentOption::Signed { .. } => others.push("signed"),
        BinSegmentOption::Unsigned { .. } => others.push("unsigned"),
        BinSegmentOption::Big { .. } => others.push("big"),
        BinSegmentOption::Little { .. } => others.push("little"),
        BinSegmentOption::Native { .. } => others.push("native"),
        BinSegmentOption::Size { value, .. } => {
            size = Some(":".to_doc().append(pattern(Cow::Owned(*value), env)))
        }
        BinSegmentOption::Unit { value, .. } => {
            unit = Some(":".to_doc().append(pattern(Cow::Owned(*value), env)))
        }
    });

    document = document.append(size);

    if !others.is_empty() || unit.is_some() {
        document = document.append("/").append(others.join("-"));
    }

    document.append(unit)
}

fn seq<'a>(
    first: Cow<'a, TypedExpr>,
    then: Cow<'a, TypedExpr>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    force_break()
        .append(expr(first, env))
        .append(",")
        .append(line())
        .append(expr(then, env))
}

fn bin_op<'a, T: Into<Cow<'a, TypedExpr>>>(
    name: &BinOp,
    left: T,
    right: T,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    let left = left.into();
    let right = right.into();

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

    let left_expr = match left.as_ref() {
        TypedExpr::BinOp { .. } => expr(left, env).surround("(", ")"),
        _ => expr(left, env),
    };

    let right_expr = match right.as_ref() {
        TypedExpr::BinOp { .. } => expr(right, env).surround("(", ")"),
        _ => expr(right, env),
    };

    left_expr
        .append(break_("", " "))
        .append(op)
        .append(" ")
        .append(right_expr)
}

fn pipe<'a>(
    value: Cow<'a, TypedExpr>,
    fun: Cow<'a, TypedExpr>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    let arg = CallArg {
        label: None,
        location: Default::default(),
        // TODO: remove this into_owned
        value: value.into_owned(),
    };
    call(fun, Cow::Owned(vec![arg]), env)
}

fn try_<'a>(
    value: Cow<'a, TypedExpr>,
    pat: Cow<'a, TypedPattern>,
    then: Cow<'a, TypedExpr>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    let try_error_name = "gleam@try_error";

    "case "
        .to_doc()
        .append(expr(value, env))
        .append(" of")
        .append(
            line()
                .append("{error, ")
                .append(env.next_local_var_name(Cow::Borrowed(try_error_name)))
                .append("} -> {error, ")
                .append(env.local_var_name(Cow::Borrowed(try_error_name)))
                .append("};")
                .nest(INDENT),
        )
        .append(
            line()
                .append("{ok, ")
                .append(pattern(pat, env))
                .append("} ->")
                .append(line().append(expr(then, env)).nest(INDENT))
                .nest(INDENT),
        )
        .append(line())
        .append("end")
        .group()
}

fn let_<'a>(
    value: Cow<'a, TypedExpr>,
    pat: Cow<'a, TypedPattern>,
    then: Cow<'a, TypedExpr>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    let body = expr(value, env);
    pattern(pat, env)
        .append(" = ")
        .append(body)
        .append(",")
        .append(line())
        .append(expr(then, env))
}

fn pattern<'a>(p: Cow<'a, TypedPattern>, env: &mut Env<'_, 'a>) -> Document<'a> {
    use Cow::*;
    use Pattern::*;

    match p {
        Borrowed(Nil { .. }) | Owned(Nil { .. }) => "[]".to_doc(),

        Borrowed(Let { name, pattern: p }) => pattern(Borrowed(p), env)
            .append(" = ")
            .append(env.next_local_var_name(Borrowed(name))),
        Owned(Let { name, pattern: p }) => pattern(Owned(*p), env)
            .append(" = ")
            .append(env.next_local_var_name(Owned(name))),

        Borrowed(Cons { head, tail, .. }) => pattern_list_cons(Borrowed(head), Borrowed(tail), env),
        Owned(Cons { head, tail, .. }) => pattern_list_cons(Owned(*head), Owned(*tail), env),

        Borrowed(Discard { .. }) | Owned(Discard { .. }) => "_".to_doc(),

        Borrowed(Var { name, .. }) => env.next_local_var_name(Borrowed(name)),
        Owned(Var { name, .. }) => env.next_local_var_name(Owned(name)),

        Borrowed(Int { value, .. }) => value.as_str().to_doc(),
        Owned(Int { value, .. }) => value.to_doc(),

        Borrowed(Float { value, .. }) => float(Borrowed(value)),
        Owned(Float { value, .. }) => float(Owned(value)),

        Borrowed(VarCall { name, .. }) => env.local_var_name(Borrowed(name)),
        Owned(VarCall { name, .. }) => env.local_var_name(Owned(name)),

        Borrowed(String { value, .. }) => string(Borrowed(value)),
        Owned(String { value, .. }) => string(Owned(value)),

        Borrowed(Constructor {
            args,
            constructor: PatternConstructor::Record { name },
            ..
        }) => tag_tuple_pattern(name, Borrowed(args), env),

        Owned(Constructor {
            args,
            constructor: PatternConstructor::Record { name },
            ..
        }) => tag_tuple_pattern(name.as_str(), Owned(args), env),

        Borrowed(Tuple { elems, .. }) => {
            tuple(elems.into_iter().map(|p| pattern(Borrowed(p), env)))
        }
        Owned(Tuple { elems, .. }) => tuple(elems.into_iter().map(|p| pattern(Owned(p), env))),

        // TODO: investigate if the clone here is necessary (replacing it with a
        // Cow.
        Borrowed(Bitstring { elems, .. }) => bitstring(
            elems
                .into_iter()
                .map(|s| pattern_segment(Borrowed(&s.value), s.options.clone(), env)),
        ),
        Owned(Bitstring { elems, .. }) => bitstring(
            elems
                .into_iter()
                .map(|s| pattern_segment(Owned(*s.value), s.options.clone(), env)),
        ),
    }
}

fn float<'a>(value: Cow<'a, str>) -> Document<'a> {
    if value.as_ref().ends_with(".") {
        format!("{}0", value).to_doc()
    } else {
        value.to_doc()
    }
}

fn pattern_list_cons<'a>(
    head: Cow<'a, TypedPattern>,
    tail: Cow<'a, TypedPattern>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    use Cow::*;
    use Pattern::*;

    list_cons(head, tail, env, pattern, |expr| match expr {
        Borrowed(Nil { .. }) | Owned(Nil { .. }) => ListType::Nil,

        Borrowed(Cons { head, tail, .. }) => {
            let head = Borrowed(head.as_ref());
            let tail = Borrowed(tail.as_ref());
            ListType::Cons { head, tail }
        }

        Owned(Cons { head, tail, .. }) => {
            let head = Owned(*head);
            let tail = Owned(*tail);
            ListType::Cons { head, tail }
        }

        other => ListType::NotList(other),
    })
}

fn expr_list_cons<'a>(
    head: Cow<'a, TypedExpr>,
    tail: Cow<'a, TypedExpr>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    use Cow::*;
    use TypedExpr::*;

    list_cons(head, tail, env, wrap_expr, |expr| match expr {
        Borrowed(ListNil { .. }) | Owned(ListNil { .. }) => ListType::Nil,

        Borrowed(ListCons { head, tail, .. }) => {
            let head = Cow::Borrowed(head.as_ref());
            let tail = Cow::Borrowed(tail.as_ref());
            ListType::Cons { head, tail }
        }
        Owned(ListCons { head, tail, .. }) => {
            let head = Cow::Owned(*head);
            let tail = Cow::Owned(*tail);
            ListType::Cons { head, tail }
        }

        other => ListType::NotList(other),
    })
}

fn list_cons<'a, ToDoc, Categorise, Elem>(
    head: Elem,
    tail: Elem,
    env: &mut Env<'_, 'a>,
    to_doc: ToDoc,
    categorise_element: Categorise,
) -> Document<'a>
where
    ToDoc: Fn(Elem, &mut Env<'_, 'a>) -> Document<'a>,
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

fn var<'a>(
    name: Cow<'a, str>,
    constructor: Cow<'a, ValueConstructor>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    use Cow::*;
    use ValueConstructorVariant::*;

    let (variant, typ) = match constructor {
        Borrowed(constructor) => (Borrowed(&constructor.variant), Borrowed(&constructor.typ)),
        Owned(constructor) => (Owned(constructor.variant), Owned(constructor.typ)),
    };

    match variant {
        Borrowed(Record {
            name: record_name, ..
        }) => match typ.as_ref().as_ref() {
            Type::Fn { args, .. } => {
                let chars = incrementing_args_list(args.len());
                "fun("
                    .to_doc()
                    .append(chars.clone())
                    .append(") -> {")
                    .append(record_name.to_snake_case())
                    .append(", ")
                    .append(chars)
                    .append("} end")
            }
            _ => atom(record_name.to_snake_case().into()),
        },
        Owned(Record {
            name: record_name, ..
        }) => match typ.as_ref().as_ref() {
            Type::Fn { args, .. } => {
                let chars = incrementing_args_list(args.len());
                "fun("
                    .to_doc()
                    .append(chars.clone())
                    .append(") -> {")
                    .append(record_name.to_snake_case())
                    .append(", ")
                    .append(chars)
                    .append("} end")
            }
            _ => atom(record_name.to_snake_case().into()),
        },

        Borrowed(LocalVariable) | Owned(LocalVariable) => env.local_var_name(name),

        Borrowed(ModuleFn {
            arity, ref module, ..
        }) if module.as_slice() == env.module => "fun "
            .to_doc()
            .append(atom(name.into()))
            .append("/")
            .append(*arity),

        Owned(ModuleFn {
            arity, ref module, ..
        }) if module.as_slice() == env.module => "fun "
            .to_doc()
            .append(atom(name.into()))
            .append("/")
            .append(arity),

        Borrowed(ModuleFn {
            arity,
            module,
            name,
            ..
        }) => "fun "
            .to_doc()
            .append(module.join("@"))
            .append(":")
            .append(atom(name.into()))
            .append("/")
            .append(*arity),
        Owned(ModuleFn {
            arity,
            module,
            name,
            ..
        }) => "fun "
            .to_doc()
            .append(module.join("@"))
            .append(":")
            .append(atom(name.into()))
            .append("/")
            .append(arity),
    }
}

fn tag_tuple_pattern<'a>(
    name: &str,
    args: Cow<'a, [CallArg<TypedPattern>]>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    if args.is_empty() {
        atom(name.to_snake_case().into())
    } else {
        match args {
            Cow::Borrowed(args) => tuple(
                std::iter::once(atom(name.to_snake_case().into()))
                    .chain(args.iter().map(|p| pattern(Cow::Borrowed(&p.value), env))),
            ),
            Cow::Owned(args) => tuple(
                std::iter::once(atom(name.to_snake_case().into()))
                    .chain(args.into_iter().map(|p| pattern(Cow::Owned(p.value), env))),
            ),
        }
    }
}

fn clause<'a>(clause: Cow<'a, TypedClause>, env: &mut Env<'_, 'a>) -> Document<'a> {
    match clause {
        Cow::Borrowed(Clause {
            guard,
            pattern: pat,
            alternative_patterns,
            then,
            ..
        }) => {
            let docs = std::iter::once(pat)
                .chain(alternative_patterns.into_iter())
                .map(|patterns| {
                    let patterns_doc = if patterns.len() == 1 {
                        let p = patterns
                            .get(0)
                            .gleam_expect("Single pattern clause printing");
                        pattern(Cow::Borrowed(p), env)
                    } else {
                        tuple(patterns.iter().map(|p| pattern(Cow::Borrowed(p), env)))
                    };
                    patterns_doc
                        .append(optional_clause_guard(Cow::Borrowed(guard), env))
                        .append(" ->")
                        .append(
                            line()
                                .append(expr(Cow::Borrowed(then), env))
                                .nest(INDENT)
                                .group(),
                        )
                })
                .intersperse(";".to_doc().append(lines(2)));
            concat(docs)
        }

        Cow::Owned(Clause {
            guard,
            pattern: pat,
            alternative_patterns,
            then,
            ..
        }) => {
            let docs = std::iter::once(pat)
                .chain(alternative_patterns.into_iter())
                .map(|patterns| {
                    let patterns_doc = if patterns.len() == 1 {
                        let p = patterns
                            .get(0)
                            .gleam_expect("Single pattern clause printing")
                            .clone();
                        pattern(Cow::Owned(p), env)
                    } else {
                        tuple(patterns.into_iter().map(|p| pattern(Cow::Owned(p), env)))
                    };
                    patterns_doc
                        .append(optional_clause_guard(Cow::Owned(guard.clone()), env))
                        .append(" ->")
                        .append(
                            line()
                                .append(expr(Cow::Owned(then.clone()), env))
                                .nest(INDENT)
                                .group(),
                        )
                })
                .intersperse(";".to_doc().append(lines(2)));
            concat(docs)
        }
    }
}

fn optional_clause_guard<'a>(
    guard: Cow<'a, Option<TypedClauseGuard>>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    match guard {
        Cow::Owned(Some(guard)) => " when "
            .to_doc()
            .append(bare_clause_guard(Cow::Owned(guard), env)),
        Cow::Borrowed(Some(guard)) => " when "
            .to_doc()
            .append(bare_clause_guard(Cow::Borrowed(guard), env)),
        Cow::Owned(None) | Cow::Borrowed(None) => nil(),
    }
}

fn bare_clause_guard<'a>(guard: Cow<'a, TypedClauseGuard>, env: &mut Env<'_, 'a>) -> Document<'a> {
    use ClauseGuard::*;
    use Cow::*;

    match guard {
        Borrowed(Or { left, right, .. }) => clause_guard(Borrowed(left), env)
            .append(" orelse ")
            .append(clause_guard(Borrowed(right), env)),
        Owned(Or { left, right, .. }) => clause_guard(Owned(*left), env)
            .append(" orelse ")
            .append(clause_guard(Owned(*right), env)),

        Borrowed(And { left, right, .. }) => clause_guard(Borrowed(left), env)
            .append(" andalso ")
            .append(clause_guard(Borrowed(right), env)),
        Owned(And { left, right, .. }) => clause_guard(Owned(*left), env)
            .append(" andalso ")
            .append(clause_guard(Owned(*right), env)),

        Borrowed(Equals { left, right, .. }) => clause_guard(Borrowed(left), env)
            .append(" =:= ")
            .append(clause_guard(Borrowed(right), env)),
        Owned(Equals { left, right, .. }) => clause_guard(Owned(*left), env)
            .append(" =:= ")
            .append(clause_guard(Owned(*right), env)),

        Borrowed(NotEquals { left, right, .. }) => clause_guard(Borrowed(left), env)
            .append(" =/= ")
            .append(clause_guard(Borrowed(right), env)),
        Owned(NotEquals { left, right, .. }) => clause_guard(Owned(*left), env)
            .append(" =/= ")
            .append(clause_guard(Owned(*right), env)),

        Borrowed(GtInt { left, right, .. }) => clause_guard(Borrowed(left), env)
            .append(" > ")
            .append(clause_guard(Borrowed(right), env)),
        Owned(GtInt { left, right, .. }) => clause_guard(Owned(*left), env)
            .append(" > ")
            .append(clause_guard(Owned(*right), env)),

        Borrowed(GtEqInt { left, right, .. }) => clause_guard(Borrowed(left), env)
            .append(" >= ")
            .append(clause_guard(Borrowed(right), env)),
        Owned(GtEqInt { left, right, .. }) => clause_guard(Owned(*left), env)
            .append(" >= ")
            .append(clause_guard(Owned(*right), env)),

        Borrowed(LtInt { left, right, .. }) => clause_guard(Borrowed(left), env)
            .append(" < ")
            .append(clause_guard(Borrowed(right), env)),
        Owned(LtInt { left, right, .. }) => clause_guard(Owned(*left), env)
            .append(" < ")
            .append(clause_guard(Owned(*right), env)),

        Borrowed(LtEqInt { left, right, .. }) => clause_guard(Borrowed(left), env)
            .append(" =< ")
            .append(clause_guard(Borrowed(right), env)),
        Owned(LtEqInt { left, right, .. }) => clause_guard(Owned(*left), env)
            .append(" =< ")
            .append(clause_guard(Owned(*right), env)),

        Borrowed(GtFloat { left, right, .. }) => clause_guard(Borrowed(left), env)
            .append(" > ")
            .append(clause_guard(Borrowed(right), env)),
        Owned(GtFloat { left, right, .. }) => clause_guard(Owned(*left), env)
            .append(" > ")
            .append(clause_guard(Owned(*right), env)),

        Borrowed(GtEqFloat { left, right, .. }) => clause_guard(Borrowed(left), env)
            .append(" >= ")
            .append(clause_guard(Borrowed(right), env)),
        Owned(GtEqFloat { left, right, .. }) => clause_guard(Owned(*left), env)
            .append(" >= ")
            .append(clause_guard(Owned(*right), env)),

        Borrowed(LtFloat { left, right, .. }) => clause_guard(Borrowed(left), env)
            .append(" < ")
            .append(clause_guard(Borrowed(right), env)),
        Owned(LtFloat { left, right, .. }) => clause_guard(Owned(*left), env)
            .append(" < ")
            .append(clause_guard(Owned(*right), env)),

        Borrowed(LtEqFloat { left, right, .. }) => clause_guard(Borrowed(left), env)
            .append(" =< ")
            .append(clause_guard(Borrowed(right), env)),
        Owned(LtEqFloat { left, right, .. }) => clause_guard(Owned(*left), env)
            .append(" =< ")
            .append(clause_guard(Owned(*right), env)),

        Borrowed(Int { value, .. }) => value.as_str().to_doc(),
        Owned(Int { value, .. }) => value.to_doc(),

        Borrowed(Float { value, .. }) => value.as_str().to_doc(),
        Owned(Float { value, .. }) => value.to_doc(),

        Borrowed(String { value, .. }) => string(Borrowed(value)),
        Owned(String { value, .. }) => string(Owned(value)),

        Borrowed(Tuple { elems, .. }) => tuple(
            elems
                .into_iter()
                .map(|e| bare_clause_guard(Borrowed(e), env)),
        ),
        Owned(Tuple { elems, .. }) => {
            tuple(elems.into_iter().map(|e| bare_clause_guard(Owned(e), env)))
        }

        Borrowed(Constructor { name, args, .. }) => {
            if args.is_empty() {
                atom(name.to_snake_case().into())
            } else {
                tuple(
                    std::iter::once(atom(name.to_snake_case().into())).chain(
                        args.into_iter()
                            .map(|a| bare_clause_guard(Borrowed(&a.value), env)),
                    ),
                )
            }
        }
        Owned(Constructor { name, args, .. }) => {
            if args.is_empty() {
                atom(name.to_snake_case().into())
            } else {
                tuple(
                    std::iter::once(atom(name.to_snake_case().into())).chain(
                        args.into_iter()
                            .map(|a| bare_clause_guard(Owned(a.value), env)),
                    ),
                )
            }
        }

        // Only local variables are supported and the typer ensures that all
        // ClauseGuard::Vars are local variables
        Borrowed(Var { name, .. }) => env.local_var_name(Borrowed(name)),
        Owned(Var { name, .. }) => env.local_var_name(Owned(name)),
    }
}

fn clause_guard<'a>(guard: Cow<'a, TypedClauseGuard>, env: &mut Env<'_, 'a>) -> Document<'a> {
    match guard.as_ref() {
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
        ClauseGuard::Var { .. }
        | ClauseGuard::Int { .. }
        | ClauseGuard::Float { .. }
        | ClauseGuard::String { .. }
        | ClauseGuard::Tuple { .. }
        | ClauseGuard::Constructor { .. } => bare_clause_guard(guard, env),
    }
}

fn clauses<'a>(cs: Cow<'a, [TypedClause]>, env: &mut Env<'_, 'a>) -> Document<'a> {
    match cs {
        Cow::Borrowed(cs) => concat(
            cs.into_iter()
                .map(|c| {
                    let vars = env.current_scope_vars.clone();
                    let erl = clause(Cow::Borrowed(c), env);
                    env.current_scope_vars = vars; // Reset the known variables now the clauses' scope has ended
                    erl
                })
                .intersperse(";".to_doc().append(lines(2))),
        ),
        Cow::Owned(cs) => concat(
            cs.into_iter()
                .map(|c| {
                    let vars = env.current_scope_vars.clone();
                    let erl = clause(Cow::Owned(c), env);
                    env.current_scope_vars = vars; // Reset the known variables now the clauses' scope has ended
                    erl
                })
                .intersperse(";".to_doc().append(lines(2))),
        ),
    }
}

fn case<'a>(
    subjects: Cow<'a, [TypedExpr]>,
    cs: Cow<'a, [TypedClause]>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    let subjects_doc = if subjects.len() == 1 {
        let subject = subjects
            .as_ref()
            .get(0)
            .cloned()
            .map(Cow::Owned)
            .gleam_expect("erl case printing of single subject");

        wrap_expr(subject, env).group()
    } else {
        match subjects {
            Cow::Borrowed(subjects) => {
                tuple(subjects.iter().map(|e| wrap_expr(Cow::Borrowed(e), env)))
            }
            Cow::Owned(subjects) => {
                tuple(subjects.into_iter().map(|e| wrap_expr(Cow::Owned(e), env)))
            }
        }
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

fn call<'a>(
    fun: Cow<'a, TypedExpr>,
    args: Cow<'a, [CallArg<TypedExpr>]>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    let args = args.into();

    // Some imports here - they will make next match less hard to read.
    use Cow::*;
    use TypedExpr::*;

    // TODO: find a way to refator this thing.
    // This *may* be possible using macros.
    match fun {
        // TODO: This match arm and the next one *can* be merged, because they
        // just require to have a reference to name.
        Borrowed(ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, .. },
            ..
        })
        | Borrowed(Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::Record { name, .. },
                    ..
                },
            ..
        }) => match args {
            Borrowed(v) => tuple(
                std::iter::once(atom(name.to_snake_case().into()))
                    .chain(v.iter().map(|arg| expr(Borrowed(&arg.value), env))),
            ),
            Owned(v) => tuple(
                std::iter::once(atom(name.to_snake_case().into()))
                    .chain(v.into_iter().map(|arg| expr(Owned(arg.value), env))),
            ),
        },

        Owned(ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, .. },
            ..
        })
        | Owned(TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::Record { name, .. },
                    ..
                },
            ..
        }) => match args {
            Borrowed(v) => tuple(
                std::iter::once(atom(name.to_snake_case().into()))
                    .chain(v.iter().map(|arg| expr(Borrowed(&arg.value), env))),
            ),
            Owned(v) => tuple(
                std::iter::once(atom(name.to_snake_case().into()))
                    .chain(v.into_iter().map(|arg| expr(Owned(arg.value), env))),
            ),
        },

        Borrowed(Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::ModuleFn { module, name, .. },
                    ..
                },
            ..
        }) => {
            if module.as_slice() == env.module {
                atom(name.into()).append(call_args(args, env))
            } else {
                atom(module.join("@").into())
                    .append(":")
                    .append(atom(name.into()))
                    .append(call_args(args, env))
            }
        }

        Owned(Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::ModuleFn { module, name, .. },
                    ..
                },
            ..
        }) => {
            if module.as_slice() == env.module {
                atom(name.into()).append(call_args(args, env))
            } else {
                atom(module.join("@").into())
                    .append(":")
                    .append(atom(name.into()))
                    .append(call_args(args, env))
            }
        }

        Borrowed(ModuleSelect {
            module_name,
            label,
            constructor: ModuleValueConstructor::Fn,
            ..
        }) => atom(module_name.join("@").into())
            .append(":")
            .append(atom(label.into()))
            .append(call_args(args, env)),

        Owned(ModuleSelect {
            module_name,
            label,
            constructor: ModuleValueConstructor::Fn,
            ..
        }) => atom(module_name.join("@").into())
            .append(":")
            .append(atom(label.into()))
            .append(call_args(args, env)),

        Borrowed(call @ Call { .. }) => expr(Borrowed(call), env)
            .surround("(", ")")
            .append(call_args(args, env)),

        Owned(call @ Call { .. }) => expr(Owned(call), env)
            .surround("(", ")")
            .append(call_args(args, env)),

        Borrowed(Fn {
            is_capture: true,
            body,
            ..
        }) => {
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
                        TypedExpr::Var { name, .. } if name == CAPTURE_VARIABLE => args
                            .get(0)
                            .gleam_expect("Erl printing: capture call replacing arg")
                            .clone(),
                        _ => a.clone(),
                    })
                    .collect::<Vec<_>>();
                call(Borrowed(fun), Cow::Owned(merged_args), env)
            } else {
                unreachable!()
            }
        }

        Owned(Fn {
            is_capture: true,
            body,
            ..
        }) => {
            if let TypedExpr::Call {
                fun,
                args: inner_args,
                ..
            } = *body
            {
                let fun = Cow::Owned(*fun);
                // TODO: this is a mess of cloning.
                let merged_args = inner_args
                    .iter()
                    .map(|a| match &a.value {
                        TypedExpr::Var { name, .. } if name == CAPTURE_VARIABLE => args
                            .get(0)
                            .gleam_expect("Erl printing: capture call replacing arg")
                            .clone(),
                        _ => a.clone(),
                    })
                    .collect::<Vec<_>>();
                call(fun, Cow::Owned(merged_args), env)
            } else {
                unreachable!()
            }
        }

        Borrowed(fun @ Fn { .. }) => expr(Cow::Borrowed(fun), env)
            .surround("(", ")")
            .append(call_args(args, env)),

        Owned(fun @ Fn { .. }) => expr(Cow::Owned(fun), env)
            .surround("(", ")")
            .append(call_args(args, env)),

        Borrowed(RecordAccess { .. }) | Owned(RecordAccess { .. }) => expr(fun, env)
            .surround("(", ")")
            .append(call_args(args, env)),

        Borrowed(TupleIndex { .. }) | Owned(TupleIndex { .. }) => expr(fun, env)
            .surround("(", ")")
            .append(call_args(args, env)),

        other => expr(other, env).append(call_args(args, env)),
    }
}

/// Wrap a document in begin end
///
fn begin_end<'a>(document: Document<'a>) -> Document<'a> {
    force_break()
        .append("begin")
        .append(line().append(document).nest(INDENT))
        .append(line())
        .append("end")
}

/// Same as expr, expect it wraps seq, let, etc in begin end
///
fn wrap_expr<'a, E: Into<Cow<'a, TypedExpr>>>(
    expression: E,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
    let expression = expression.into();
    match expression.as_ref() {
        TypedExpr::Seq { .. } => begin_end(expr(expression, env)),
        TypedExpr::Let { .. } => begin_end(expr(expression, env)),
        _ => expr(expression, env),
    }
}

fn expr<'a>(expression: Cow<'a, TypedExpr>, env: &mut Env<'_, 'a>) -> Document<'a> {
    let expression = expression.into();

    // Some imports here - they will make next match less hard to read.
    use Cow::*;
    use TypedExpr::*;

    // TODO: find a way to refator this thing.
    // This *may* be possible using macros.
    match expression {
        Borrowed(ListNil { .. }) | Owned(ListNil { .. }) => "[]".to_doc(),

        Borrowed(Todo { .. }) | Owned(Todo { .. }) => "erlang:error({gleam_error, todo})".to_doc(),

        Borrowed(Int { value, .. }) => value.as_str().to_doc(),
        Owned(Int { value, .. }) => value.to_doc(),

        Borrowed(Float { value, .. }) => float(Borrowed(value)),
        Owned(Float { value, .. }) => float(Owned(value)),

        Borrowed(String { value, .. }) => string(Borrowed(value)),
        Owned(String { value, .. }) => string(Owned(value)),

        Borrowed(Seq { first, then, .. }) => seq(Borrowed(first), Borrowed(then), env),
        Owned(Seq { first, then, .. }) => seq(Owned(*first), Owned(*then), env),

        Borrowed(Pipe { left, right, .. }) => pipe(Borrowed(left), Borrowed(right), env),
        Owned(Pipe { left, right, .. }) => pipe(Owned(*left), Owned(*right), env),

        Borrowed(TupleIndex { tuple, index, .. }) => tuple_index(Borrowed(tuple), *index, env),
        Owned(TupleIndex { tuple, index, .. }) => tuple_index(Owned(*tuple), index, env),

        Borrowed(Var {
            name, constructor, ..
        }) => var(Borrowed(name), Borrowed(constructor), env),
        Owned(Var {
            name, constructor, ..
        }) => var(Owned(name), Owned(constructor), env),

        Borrowed(Fn { args, body, .. }) => fun(Borrowed(args), Borrowed(body), env),
        Owned(Fn { args, body, .. }) => fun(Owned(args), Owned(*body), env),

        Borrowed(ListCons { head, tail, .. }) => {
            expr_list_cons(Borrowed(head), Borrowed(tail), env)
        }
        Owned(ListCons { head, tail, .. }) => expr_list_cons(Owned(*head), Owned(*tail), env),

        Borrowed(Call { fun, args, .. }) => call(Borrowed(fun), Borrowed(args), env),
        Owned(Call { fun, args, .. }) => call(Owned(*fun), Owned(args), env),

        Borrowed(ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, arity: 0 },
            ..
        }) => atom(name.to_snake_case().into()),
        Owned(ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, arity: 0 },
            ..
        }) => atom(name.to_snake_case().into()),

        Borrowed(ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, arity },
            ..
        }) => {
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
        Owned(ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, arity },
            ..
        }) => {
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

        Borrowed(ModuleSelect {
            typ,
            label,
            module_name,
            constructor: ModuleValueConstructor::Fn,
            ..
        }) => module_select_fn(typ.clone(), module_name, Borrowed(label)),
        Owned(ModuleSelect {
            typ,
            label,
            module_name,
            constructor: ModuleValueConstructor::Fn,
            ..
        }) => module_select_fn(typ.clone(), module_name.as_slice(), Owned(label)),

        Borrowed(RecordAccess { record, index, .. }) => {
            tuple_index(Borrowed(record), index + 1, env)
        }
        Owned(RecordAccess { record, index, .. }) => tuple_index(Owned(*record), index + 1, env),

        Borrowed(Let {
            value,
            pattern,
            then,
            kind: BindingKind::Try,
            ..
        }) => try_(Borrowed(value), Borrowed(pattern), Borrowed(then), env),
        Owned(Let {
            value,
            pattern,
            then,
            kind: BindingKind::Try,
            ..
        }) => try_(Owned(*value), Owned(pattern), Owned(*then), env),

        Borrowed(Let {
            value,
            pattern,
            then,
            ..
        }) => let_(Borrowed(value), Borrowed(pattern), Borrowed(then), env),
        Owned(Let {
            value,
            pattern,
            then,
            ..
        }) => let_(Owned(*value), Owned(pattern), Owned(*then), env),

        Borrowed(Case {
            subjects, clauses, ..
        }) => case(Borrowed(subjects), Borrowed(clauses), env),
        Owned(Case {
            subjects, clauses, ..
        }) => case(Owned(subjects), Owned(clauses), env),

        Borrowed(BinOp {
            name, left, right, ..
        }) => bin_op(
            &name,
            Cow::Borrowed(left.as_ref()),
            Cow::Borrowed(right.as_ref()),
            env,
        ),
        Owned(BinOp {
            name, left, right, ..
        }) => bin_op(&name, Cow::Owned(*left), Cow::Owned(*right), env),

        Borrowed(Tuple { elems, .. }) => {
            tuple(elems.into_iter().map(|e| wrap_expr(Cow::Borrowed(e), env)))
        }
        Owned(Tuple { elems, .. }) => {
            tuple(elems.into_iter().map(|e| wrap_expr(Cow::Owned(e), env)))
        }

        Borrowed(Bitstring { elems, .. }) => bitstring(
            elems
                .into_iter()
                .map(|s| segment(Borrowed(&s.value), s.options.clone(), env)),
        ),
        Owned(Bitstring { elems, .. }) => bitstring(
            elems
                .into_iter()
                .map(|s| segment(Owned(*s.value), s.options.clone(), env)),
        ),
    }
}

fn tuple_index<'a>(tuple: Cow<'a, TypedExpr>, index: u64, env: &mut Env<'_, 'a>) -> Document<'a> {
    use std::iter::once;
    let index_doc = format!("{}", (index + 1)).to_doc();
    let tuple_doc = expr(tuple, env);
    let iter = once(index_doc).chain(once(tuple_doc));
    "erlang:element".to_doc().append(wrap_args(iter))
}

fn module_select_fn<'a>(
    typ: Arc<crate::typ::Type>,
    module_name: &[String],
    label: Cow<'a, str>,
) -> Document<'a> {
    match crate::typ::collapse_links(typ).as_ref() {
        crate::typ::Type::Fn { args, .. } => "fun "
            .to_doc()
            .append(module_name.join("@"))
            .append(":")
            .append(atom(label.into()))
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

fn fun<'a>(
    args: Cow<'a, [TypedArg]>,
    body: Cow<'a, TypedExpr>,
    env: &mut Env<'_, 'a>,
) -> Document<'a> {
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

fn external_fun<'a>(name: &'a str, module: &'a str, fun: &'a str, arity: usize) -> Document<'a> {
    let chars: String = incrementing_args_list(arity);

    atom(name.into())
        .append(format!("({}) ->", chars))
        .append(line())
        .append(atom(module.into()))
        .append(":")
        .append(atom(fun.into()))
        .append(format!("({}).", chars))
        .nest(INDENT)
}

fn is_reserved_word(name: &str) -> bool {
    return match name {
        "!" | "receive" | "bnot" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr"
        | "not" | "and" | "or" | "xor" | "orelse" | "andalso" | "when" | "end" | "fun" | "try"
        | "catch" | "after" => true,
        _ => false,
    };
}
