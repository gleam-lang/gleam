#[cfg(test)]
mod tests;

use crate::{
    ast::*,
    error::GleamExpect,
    file::OutputFile,
    pretty::*,
    project::{self, Analysed},
    typ::{
        ButcheredModuleValueConstructor, ButcheredPatternConstructor, ButcheredValueConstructor,
        ButcheredValueConstructorVariant, PatternConstructor, Type, ValueConstructor,
    },
};
use heck::{CamelCase, SnakeCase};
use itertools::Itertools;
use std::borrow::Cow;
use std::char;
use std::default::Default;
use std::sync::Arc;

use butcher::iterator::IntoCowIterator;
use butcher::Butcher;

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

    pub fn local_var_name<'b>(&mut self, name: String) -> Document<'b> {
        match self.current_scope_vars.get(&name) {
            None => {
                self.current_scope_vars.insert(name.clone(), 0);
                let tmp = name.to_camel_case().to_doc();
                self.erl_function_scope_vars.insert(name, 0);
                tmp
            }
            Some(0) => name.to_camel_case().to_doc(),
            Some(n) => name.to_camel_case().to_doc().append(*n),
        }
    }

    pub fn next_local_var_name<'b>(&mut self, name: String) -> Document<'b> {
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
            .map(|(n, a)| atom(Cow::Owned(n)).append("/").append(a))
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
        Statement::ModuleConstant { .. } => None,

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
    args: &[TypedArg],
    body: &'a TypedExpr,
    module: &[String],
) -> Document<'a> {
    let mut env = Env::new(module);

    atom(Cow::Borrowed(name))
        .append(fun_args(args, &mut env))
        .append(" ->")
        .append(
            line()
                .append(expr(Cow::Borrowed(body), &mut env))
                .nest(INDENT)
                .group(),
        )
        .append(".")
}

fn fun_args<'a>(args: &[TypedArg], env: &mut Env) -> Document<'a> {
    wrap_args(args.into_iter().map(|a| match &a.names {
        ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => "_".to_doc(),
        ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
            env.next_local_var_name(name.to_string())
        }
    }))
}

fn call_args<'a>(args: Cow<'a, [CallArg<TypedExpr>]>, env: &mut Env) -> Document<'a> {
    wrap_args(
        args.into_cow_iter()
            .map(|arg| wrap_expr(CallArg::butcher(arg).value, env)),
    )
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
        value if is_reserved_word(value.as_ref()) => format!("'{}'", value).to_doc(),

        // No need to escape
        _ if RE.is_match(&value) => value.to_doc(),

        // Escape because of characters contained
        _ => value.to_doc().surround("'", "'"),
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

fn bit_string<'a>(elems: impl Iterator<Item = Document<'a>>) -> Document<'a> {
    concat(elems.intersperse(delim(",")))
        .nest_current()
        .surround("<<", ">>")
        .group()
}

fn const_segment<'a>(
    value: Cow<'a, TypedConstant>,
    options: Cow<'a, [BitStringSegmentOption<TypedConstant>]>,
    env: &mut Env,
) -> Document<'a> {
    let document = match value.as_ref() {
        // Skip the normal <<value/utf8>> surrounds
        Constant::String { value, .. } => value.clone().to_doc().surround("\"", "\""),

        // As normal
        Constant::Int { .. } | Constant::Float { .. } | Constant::BitString { .. } => {
            const_inline(value, env)
        }

        // Wrap anything else in parentheses
        _ => const_inline(value, env).surround("(", ")"),
    };

    let size = |value: Cow<'a, TypedConstant>, env: &mut Env| match value.as_ref() {
        Constant::Int { .. } => Some(":".to_doc().append(const_inline(value, env))),
        _ => Some(
            ":".to_doc()
                .append(const_inline(value, env).surround("(", ")")),
        ),
    };

    let unit = |value: Cow<'a, TypedConstant>, env: &mut Env| match value.as_ref() {
        Constant::Int { .. } => Some("unit:".to_doc().append(const_inline(value, env))),
        _ => None,
    };

    bit_string_segment(document, options, size, unit, env)
}

fn expr_segment<'a>(
    value: Cow<'a, TypedExpr>,
    options: Cow<'a, [BitStringSegmentOption<TypedExpr>]>,
    env: &mut Env,
) -> Document<'a> {
    let document = match TypedExpr::butcher(value) {
        // Skip the normal <<value/utf8>> surrounds
        ButcheredTypedExpr::String { value, .. } => value.to_doc().surround("\"", "\""),

        // As normal
        value @ ButcheredTypedExpr::Int { .. }
        | value @ ButcheredTypedExpr::Float { .. }
        | value @ ButcheredTypedExpr::Var { .. }
        | value @ ButcheredTypedExpr::BitString { .. } => {
            let value = TypedExpr::unbutcher(value);
            expr(Cow::Owned(value), env)
        }

        // Wrap anything else in parentheses
        value => {
            let value = TypedExpr::unbutcher(value);
            expr(Cow::Owned(value), env).surround("(", ")")
        }
    };

    let size = |value: Cow<'a, TypedExpr>, env: &mut Env| match value.as_ref() {
        TypedExpr::Int { .. } | TypedExpr::Var { .. } => {
            Some(":".to_doc().append(expr(value, env)))
        }
        _ => Some(":".to_doc().append(expr(value, env).surround("(", ")"))),
    };

    let unit = |value: Cow<'a, TypedExpr>, env: &mut Env| match value.as_ref() {
        TypedExpr::Int { .. } => Some("unit:".to_doc().append(expr(value, env))),
        _ => None,
    };

    bit_string_segment(document, options, size, unit, env)
}

fn pattern_segment<'a>(
    value: Cow<'a, TypedPattern>,
    options: Cow<'a, [BitStringSegmentOption<TypedPattern>]>,
    env: &mut Env,
) -> Document<'a> {
    let document = match Pattern::butcher(value) {
        // Skip the normal <<value/utf8>> surrounds
        ButcheredPattern::String { value, .. } => value.to_doc().surround("\"", "\""),

        // As normal
        value @ ButcheredPattern::Discard { .. }
        | value @ ButcheredPattern::Var { .. }
        | value @ ButcheredPattern::Int { .. }
        | value @ ButcheredPattern::Float { .. } => {
            let value = Pattern::unbutcher(value);
            pattern(Cow::Owned(value), env)
        }

        // No other pattern variants are allowed in pattern bit string segments
        _ => crate::error::fatal_compiler_bug("Pattern segment match not recognised"),
    };

    let size = |value: Cow<'a, TypedPattern>, env: &mut Env| {
        Some(":".to_doc().append(pattern(value, env)))
    };

    let unit = |value: Cow<'a, TypedPattern>, env: &mut Env| match value.as_ref() {
        Pattern::Int { .. } => Some("unit:".to_doc().append(pattern(value, env))),
        _ => None,
    };

    bit_string_segment(document, options, size, unit, env)
}

fn bit_string_segment<'a, Value, SizeToDoc, UnitToDoc>(
    mut document: Document<'a>,
    options: Cow<'a, [BitStringSegmentOption<Value>]>,
    mut size_to_doc: SizeToDoc,
    mut unit_to_doc: UnitToDoc,
    env: &mut Env,
) -> Document<'a>
where
    Value: Clone + 'a,
    SizeToDoc: FnMut(Cow<'a, Value>, &mut Env) -> Option<Document<'a>>,
    UnitToDoc: FnMut(Cow<'a, Value>, &mut Env) -> Option<Document<'a>>,
{
    let mut size: Option<Document> = None;
    let mut unit: Option<Document> = None;
    let mut others = Vec::new();

    options
        .into_cow_iter()
        .for_each(|option| match BitStringSegmentOption::butcher(option) {
            ButcheredBitStringSegmentOption::Invalid { .. } => (),
            ButcheredBitStringSegmentOption::Integer { .. } => others.push("integer"),
            ButcheredBitStringSegmentOption::Float { .. } => others.push("float"),
            ButcheredBitStringSegmentOption::Binary { .. } => others.push("binary"),
            ButcheredBitStringSegmentOption::BitString { .. } => others.push("bitstring"),
            ButcheredBitStringSegmentOption::UTF8 { .. } => others.push("utf8"),
            ButcheredBitStringSegmentOption::UTF16 { .. } => others.push("utf16"),
            ButcheredBitStringSegmentOption::UTF32 { .. } => others.push("utf32"),
            ButcheredBitStringSegmentOption::UTF8Codepoint { .. } => others.push("utf8"),
            ButcheredBitStringSegmentOption::UTF16Codepoint { .. } => others.push("utf16"),
            ButcheredBitStringSegmentOption::UTF32Codepoint { .. } => others.push("utf32"),
            ButcheredBitStringSegmentOption::Signed { .. } => others.push("signed"),
            ButcheredBitStringSegmentOption::Unsigned { .. } => others.push("unsigned"),
            ButcheredBitStringSegmentOption::Big { .. } => others.push("big"),
            ButcheredBitStringSegmentOption::Little { .. } => others.push("little"),
            ButcheredBitStringSegmentOption::Native { .. } => others.push("native"),
            ButcheredBitStringSegmentOption::Size { value, .. } => size = size_to_doc(value, env),
            ButcheredBitStringSegmentOption::Unit { value, .. } => unit = unit_to_doc(value, env),
        });

    document = document.append(size);

    if !others.is_empty() {
        document = document.append("/").append(others.join("-"));
    }

    if unit.is_some() {
        if !others.is_empty() {
            document = document.append("-").append(unit)
        } else {
            document = document.append("/").append(unit)
        }
    }

    document
}

fn seq<'a>(first: Cow<'a, TypedExpr>, then: Cow<'a, TypedExpr>, env: &mut Env) -> Document<'a> {
    force_break()
        .append(expr(first, env))
        .append(",")
        .append(line())
        .append(expr(then, env))
}

fn bin_op<'a>(
    name: &BinOp,
    left: Cow<'a, TypedExpr>,
    right: Cow<'a, TypedExpr>,
    env: &mut Env,
) -> Document<'a> {
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

fn pipe<'a>(value: &TypedExpr, fun: Cow<'a, TypedExpr>, env: &mut Env) -> Document<'a> {
    let arg = CallArg {
        label: None,
        location: Default::default(),
        // TODO: remove this clone
        value: value.clone(),
    };
    let args = vec![arg];
    call(fun, Cow::Owned(args), env)
}

fn try_<'a>(
    value: Cow<'a, TypedExpr>,
    pat: Cow<'a, TypedPattern>,
    then: Cow<'a, TypedExpr>,
    env: &mut Env,
) -> Document<'a> {
    let try_error_name = "gleam@try_error";

    "case "
        .to_doc()
        .append(expr(value, env))
        .append(" of")
        .append(
            line()
                .append("{error, ")
                .append(env.next_local_var_name(try_error_name.to_string()))
                .append("} -> {error, ")
                .append(env.local_var_name(try_error_name.to_string()))
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
    env: &mut Env,
) -> Document<'a> {
    let body = expr(value, env);
    pattern(pat, env)
        .append(" = ")
        .append(body)
        .append(",")
        .append(line())
        .append(expr(then, env))
}

fn pattern<'a>(p: Cow<'a, TypedPattern>, env: &mut Env) -> Document<'a> {
    match Pattern::butcher(p) {
        ButcheredPattern::Nil { .. } => "[]".to_doc(),

        ButcheredPattern::Let { name, pattern: p } => pattern(p, env)
            .append(" = ")
            .append(env.next_local_var_name(name.to_string())),

        ButcheredPattern::Cons { head, tail, .. } => pattern_list_cons(head, tail, env),

        ButcheredPattern::Discard { name, .. } => name.to_doc(),

        ButcheredPattern::Var { name, .. } => env.next_local_var_name(name.to_string()),

        ButcheredPattern::VarCall { name, .. } => env.local_var_name(name.to_string()),

        ButcheredPattern::Int { value, .. } => value.to_doc(),

        ButcheredPattern::Float { value, .. } => float(value.as_ref()),

        ButcheredPattern::String { value, .. } => string(value),

        ButcheredPattern::Constructor {
            args, constructor, ..
        } => {
            let constructor = PatternConstructor::butcher(constructor);
            let ButcheredPatternConstructor::Record { name } = constructor;
            tag_tuple_pattern(name.as_ref(), args, env)
        }

        ButcheredPattern::Tuple { elems, .. } => {
            tuple(elems.into_cow_iter().map(|p| pattern(p, env)))
        }

        ButcheredPattern::BitString { segments, .. } => bit_string(
            segments
                .into_cow_iter()
                .map(|s| {
                    let ButcheredBitStringSegment { value, options, .. } =
                        BitStringSegment::butcher(s);
                    (value, options)
                })
                .map(|(value, options)| pattern_segment(value, options, env)),
        ),
    }
}

fn float<'a>(value: &str) -> Document<'a> {
    if value.ends_with(".") {
        format!("{}0", value).to_doc()
    } else {
        // TODO: remove this to_string call
        value.to_string().to_doc()
    }
}

fn pattern_list_cons<'a>(
    head: Cow<'a, TypedPattern>,
    tail: Cow<'a, TypedPattern>,
    env: &mut Env,
) -> Document<'a> {
    list_cons(head, tail, env, pattern, |expr| {
        match Pattern::butcher(expr) {
            ButcheredPattern::Nil { .. } => ListType::Nil,

            ButcheredPattern::Cons { head, tail, .. } => ListType::Cons { head, tail },

            expr => {
                let expr = Pattern::unbutcher(expr);
                ListType::NotList(Cow::Owned(expr))
            }
        }
    })
}

fn expr_list_cons<'a>(
    head: Cow<'a, TypedExpr>,
    tail: Cow<'a, TypedExpr>,
    env: &mut Env,
) -> Document<'a> {
    list_cons(
        head,
        tail,
        env,
        wrap_expr,
        |expr| match TypedExpr::butcher(expr) {
            ButcheredTypedExpr::ListNil { .. } => ListType::Nil,

            ButcheredTypedExpr::ListCons { head, tail, .. } => ListType::Cons { head, tail },

            expr => {
                let expr = TypedExpr::unbutcher(expr);
                ListType::NotList(Cow::Owned(expr))
            }
        },
    )
}

fn list_cons<'a, ToDoc, Categorise, Elem>(
    head: Cow<'a, Elem>,
    tail: Cow<'a, Elem>,
    env: &mut Env,
    to_doc: ToDoc,
    categorise_element: Categorise,
) -> Document<'a>
where
    Elem: Clone,
    ToDoc: Fn(Cow<'a, Elem>, &mut Env) -> Document<'a>,
    Categorise: Fn(Cow<Elem>) -> ListType<Cow<Elem>, Cow<Elem>>,
{
    let mut elems = vec![head];
    let final_tail = collect_cons::<_, Elem, Elem>(tail, &mut elems, categorise_element);

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

fn collect_cons<'a, F, E, T>(e: Cow<'a, T>, elems: &mut Vec<Cow<'a, E>>, f: F) -> Option<Cow<'a, T>>
where
    T: Clone,
    E: Clone,
    F: Fn(Cow<'a, T>) -> ListType<Cow<'a, E>, Cow<'a, T>>,
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
    env: &mut Env,
) -> Document<'a> {
    let ButcheredValueConstructor { variant, typ, .. } = ValueConstructor::butcher(constructor);

    match variant {
        ButcheredValueConstructorVariant::Record {
            name: record_name, ..
        } => match typ.as_ref() {
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
            _ => atom(Cow::Owned(record_name.to_snake_case())),
        },

        ButcheredValueConstructorVariant::LocalVariable => env.local_var_name(name.to_string()),

        ButcheredValueConstructorVariant::ModuleConstant { literal } => const_inline(literal, env),

        ButcheredValueConstructorVariant::ModuleFn { arity, module, .. }
            if module.as_ref() == env.module =>
        {
            "fun ".to_doc().append(atom(name)).append("/").append(arity)
        }

        ButcheredValueConstructorVariant::ModuleFn {
            arity,
            module,
            name,
            ..
        } => "fun "
            .to_doc()
            .append(module.join("@"))
            .append(":")
            .append(atom(name))
            .append("/")
            .append(arity),
    }
}

fn const_inline<'a>(literal: Cow<'a, TypedConstant>, env: &mut Env) -> Document<'a> {
    match Constant::butcher(literal) {
        ButcheredConstant::Int { value, .. } => value.to_doc(),
        ButcheredConstant::Float { value, .. } => value.to_doc(),
        ButcheredConstant::String { value, .. } => string(value),
        ButcheredConstant::Tuple { elements, .. } => {
            tuple(elements.into_cow_iter().map(|e| const_inline(e, env)))
        }

        ButcheredConstant::List { elements, .. } => {
            let elements = elements
                .into_cow_iter()
                .map(|e| const_inline(e, env))
                .intersperse(delim(","));
            concat(elements).nest_current().surround("[", "]").group()
        }

        ButcheredConstant::BitString { segments, .. } => bit_string(
            segments
                .into_cow_iter()
                .map(|s| {
                    let ButcheredBitStringSegment { value, options, .. } =
                        BitStringSegment::butcher(s);
                    (value, options)
                })
                .map(|(value, options)| const_segment(value, options, env)),
        ),

        ButcheredConstant::Record { tag, args, .. } => {
            if args.is_empty() {
                atom(Cow::Owned(tag.to_snake_case()))
            } else {
                let args = args
                    .into_cow_iter()
                    .map(|a| CallArg::butcher(a).value)
                    .map(|value| const_inline(value, env));
                let tag = atom(Cow::Owned(tag.to_snake_case()));
                tuple(std::iter::once(tag).chain(args))
            }
        }
    }
}

fn tag_tuple_pattern<'a>(
    name: &str,
    args: Cow<'a, [CallArg<TypedPattern>]>,
    env: &mut Env,
) -> Document<'a> {
    if args.is_empty() {
        atom(Cow::Owned(name.to_snake_case()))
    } else {
        tuple(
            std::iter::once(atom(Cow::Owned(name.to_snake_case()))).chain(
                args.into_cow_iter()
                    .map(|p| CallArg::butcher(p).value)
                    .map(|value| pattern(value, env)),
            ),
        )
    }
}

fn clause<'a>(clause: Cow<'a, TypedClause>, env: &mut Env) -> Document<'a> {
    let ButcheredClause {
        guard,
        pattern: pat,
        alternative_patterns,
        then,
        ..
    } = TypedClause::butcher(clause);

    // These are required to get the alternative patterns working properly.
    // Simply rendering the duplicate erlang clauses breaks the variable rewriting
    let mut then_doc = Document::Nil;
    let erlang_vars = env.erl_function_scope_vars.clone();

    let docs = std::iter::once(pat)
        .chain(alternative_patterns.into_cow_iter())
        .map(move |patterns| {
            env.erl_function_scope_vars = erlang_vars.clone();

            let patterns_doc = if patterns.len() == 1 {
                let p = patterns
                    .get(0)
                    .gleam_expect("Single pattern clause printing")
                    // TODO: investigate this clone
                    .clone();
                pattern(Cow::Owned(p), env)
            } else {
                tuple(patterns.into_cow_iter().map(|p| pattern(p, env)))
            };

            if then_doc == Document::Nil {
                // TODO: explain why this clone is necessary (1)
                let then = Cow::Owned(then.as_ref().to_owned());
                then_doc = expr(then, env);
            }

            // A clone is done for the same reason as the previous one (see (1))
            let guard = Cow::Owned(guard.as_ref().to_owned());
            let guard = to_option_cow(guard);

            patterns_doc.append(
                optional_clause_guard(guard, env)
                    .append(" ->")
                    .append(line().append(then_doc.clone()).nest(INDENT).group()),
            )
        })
        .intersperse(";".to_doc().append(lines(2)));

    concat(docs)
}

fn to_option_cow<'a, T: Clone>(data: Cow<'a, Option<T>>) -> Option<Cow<'a, T>> {
    match data {
        Cow::Owned(None) | Cow::Borrowed(None) => None,
        Cow::Owned(Some(data)) => Some(Cow::Owned(data)),
        Cow::Borrowed(Some(data)) => Some(Cow::Borrowed(data)),
    }
}

fn optional_clause_guard<'a>(
    guard: Option<Cow<'a, TypedClauseGuard>>,
    env: &mut Env,
) -> Document<'a> {
    match guard {
        Some(guard) => " when ".to_doc().append(bare_clause_guard(guard, env)),
        None => nil(),
    }
}

fn bare_clause_guard<'a>(guard: Cow<'a, TypedClauseGuard>, env: &mut Env) -> Document<'a> {
    match TypedClauseGuard::butcher(guard) {
        ButcheredClauseGuard::Or { left, right, .. } => clause_guard(left, env)
            .append(" orelse ")
            .append(clause_guard(right, env)),

        ButcheredClauseGuard::And { left, right, .. } => clause_guard(left, env)
            .append(" andalso ")
            .append(clause_guard(right, env)),

        ButcheredClauseGuard::Equals { left, right, .. } => clause_guard(left, env)
            .append(" =:= ")
            .append(clause_guard(right, env)),

        ButcheredClauseGuard::NotEquals { left, right, .. } => clause_guard(left, env)
            .append(" =/= ")
            .append(clause_guard(right, env)),

        ButcheredClauseGuard::GtInt { left, right, .. } => clause_guard(left, env)
            .append(" > ")
            .append(clause_guard(right, env)),

        ButcheredClauseGuard::GtEqInt { left, right, .. } => clause_guard(left, env)
            .append(" >= ")
            .append(clause_guard(right, env)),

        ButcheredClauseGuard::LtInt { left, right, .. } => clause_guard(left, env)
            .append(" < ")
            .append(clause_guard(right, env)),

        ButcheredClauseGuard::LtEqInt { left, right, .. } => clause_guard(left, env)
            .append(" =< ")
            .append(clause_guard(right, env)),

        ButcheredClauseGuard::GtFloat { left, right, .. } => clause_guard(left, env)
            .append(" > ")
            .append(clause_guard(right, env)),

        ButcheredClauseGuard::GtEqFloat { left, right, .. } => clause_guard(left, env)
            .append(" >= ")
            .append(clause_guard(right, env)),

        ButcheredClauseGuard::LtFloat { left, right, .. } => clause_guard(left, env)
            .append(" < ")
            .append(clause_guard(right, env)),

        ButcheredClauseGuard::LtEqFloat { left, right, .. } => clause_guard(left, env)
            .append(" =< ")
            .append(clause_guard(right, env)),

        // Only local variables are supported and the typer ensures that all
        // ClauseGuard::Vars are local variables
        ButcheredClauseGuard::Var { name, .. } => env.local_var_name(name.to_string()),

        ButcheredClauseGuard::Constant(constant) => const_inline(constant, env),
    }
}

fn clause_guard<'a>(guard: Cow<'a, TypedClauseGuard>, env: &mut Env) -> Document<'a> {
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
        ClauseGuard::Constant(_) | ClauseGuard::Var { .. } => bare_clause_guard(guard, env),
    }
}

fn clauses<'a>(cs: Cow<'a, [TypedClause]>, env: &mut Env) -> Document<'a> {
    concat(
        cs.into_cow_iter()
            .map(|c| {
                let vars = env.current_scope_vars.clone();
                let erl = clause(c, env);
                env.current_scope_vars = vars; // Reset the known variables now the clauses' scope has ended
                erl
            })
            .intersperse(";".to_doc().append(lines(2))),
    )
}

fn case<'a>(
    subjects: Cow<'a, [TypedExpr]>,
    cs: Cow<'a, [TypedClause]>,
    env: &mut Env,
) -> Document<'a> {
    let subjects_doc = if subjects.len() == 1 {
        let subject = subjects
            .get(0)
            .gleam_expect("erl case printing of single subject")
            .clone(); // TODO: remove this clone, match on subjects instead
        wrap_expr(Cow::Owned(subject), env).group()
    } else {
        tuple(subjects.into_cow_iter().map(|e| wrap_expr(e, env)))
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
    env: &mut Env,
) -> Document<'a> {
    match TypedExpr::butcher(fun) {
        ButcheredTypedExpr::ModuleSelect {
            constructor: ButcheredModuleValueConstructor::Record { name, .. },
            ..
        }
        | ButcheredTypedExpr::Var {
            constructor:
                ButcheredValueConstructor {
                    variant: ButcheredValueConstructorVariant::Record { name, .. },
                    ..
                },
            ..
        } => tuple(
            std::iter::once(atom(Cow::Owned(name.to_snake_case()))).chain(
                args.into_cow_iter()
                    .map(|arg| expr(CallArg::butcher(arg).value, env)),
            ),
        ),

        ButcheredTypedExpr::Var {
            constructor:
                ButcheredValueConstructor {
                    variant: ButcheredValueConstructorVariant::ModuleFn { module, name, .. },
                    ..
                },
            ..
        } => {
            if module.as_ref() == env.module {
                atom(name).append(call_args(args, env))
            } else {
                atom(Cow::Owned(module.join("@")))
                    .append(":")
                    .append(atom(name))
                    .append(call_args(args, env))
            }
        }

        ButcheredTypedExpr::ModuleSelect {
            module_name,
            label,
            constructor: ButcheredModuleValueConstructor::Fn,
            ..
        } => atom(Cow::Owned(module_name.join("@")))
            .append(":")
            .append(atom(label))
            .append(call_args(args, env)),

        call @ ButcheredTypedExpr::Call { .. } => {
            let call = TypedExpr::unbutcher(call);

            expr(Cow::Owned(call), env)
                .surround("(", ")")
                .append(call_args(args, env))
        }

        ButcheredTypedExpr::Fn {
            is_capture: true,
            body,
            ..
        } => {
            if let ButcheredTypedExpr::Call {
                fun,
                args: inner_args,
                ..
            } = TypedExpr::butcher(body)
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
                call(fun, Cow::Owned(merged_args), env)
            } else {
                unreachable!()
            }
        }

        fun @ ButcheredTypedExpr::Fn { .. } => {
            let fun = TypedExpr::unbutcher(fun);

            expr(Cow::Owned(fun), env)
                .surround("(", ")")
                .append(call_args(args, env))
        }

        fun @ ButcheredTypedExpr::RecordAccess { .. } => {
            let fun = TypedExpr::unbutcher(fun);

            expr(Cow::Owned(fun), env)
                .surround("(", ")")
                .append(call_args(args, env))
        }

        fun @ ButcheredTypedExpr::TupleIndex { .. } => {
            let fun = TypedExpr::unbutcher(fun);

            expr(Cow::Owned(fun), env)
                .surround("(", ")")
                .append(call_args(args, env))
        }

        other => {
            let other = TypedExpr::unbutcher(other);

            expr(Cow::Owned(other), env).append(call_args(args, env))
        }
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
fn wrap_expr<'a>(expression: Cow<'a, TypedExpr>, env: &mut Env) -> Document<'a> {
    match expression.as_ref() {
        TypedExpr::Seq { .. } => begin_end(expr(expression, env)),
        TypedExpr::Let { .. } => begin_end(expr(expression, env)),
        _ => expr(expression, env),
    }
}

fn expr<'a>(expression: Cow<'a, TypedExpr>, env: &mut Env) -> Document<'a> {
    match TypedExpr::butcher(expression) {
        ButcheredTypedExpr::ListNil { .. } => "[]".to_doc(),
        ButcheredTypedExpr::Todo { label, .. } => match label {
            Cow::Owned(None) | Cow::Borrowed(None) => "erlang:error({gleam_error, todo})".to_doc(),
            Cow::Owned(Some(l)) => l
                .to_doc()
                .surround("erlang:error({gleam_error, todo, \"", "\"})"),
            Cow::Borrowed(Some(l)) => l
                .as_str()
                .to_doc()
                .surround("erlang:error({gleam_error, todo, \"", "\"})"),
        },
        ButcheredTypedExpr::Int { value, .. } => value.to_doc(),
        ButcheredTypedExpr::Float { value, .. } => float(value.as_ref()),
        ButcheredTypedExpr::String { value, .. } => string(value),
        ButcheredTypedExpr::Seq { first, then, .. } => seq(first, then, env),
        ButcheredTypedExpr::Pipe { left, right, .. } => pipe(&left, right, env),

        ButcheredTypedExpr::TupleIndex { tuple, index, .. } => tuple_index(tuple, *index, env),

        ButcheredTypedExpr::Var {
            name, constructor, ..
        } => {
            let constructor = ValueConstructor::unbutcher(constructor);
            var(name, Cow::Owned(constructor), env)
        }

        ButcheredTypedExpr::Fn { args, body, .. } => fun(args, body, env),

        ButcheredTypedExpr::ListCons { head, tail, .. } => expr_list_cons(head, tail, env),

        ButcheredTypedExpr::Call { fun, args, .. } => call(fun, args, env),

        ButcheredTypedExpr::ModuleSelect {
            typ,
            label,
            module_name,
            constructor,
            ..
        } => {
            match constructor {
                ButcheredModuleValueConstructor::Record { name, arity: 0 } => {
                    atom(Cow::Owned(name.to_snake_case()))
                }

                ButcheredModuleValueConstructor::Constant { literal } => const_inline(literal, env),

                ButcheredModuleValueConstructor::Record { name, arity } => {
                    let chars = incrementing_args_list(arity);
                    "fun("
                        .to_doc()
                        // TODO: explain why this clone is mandatory
                        .append(chars.clone())
                        .append(") -> {")
                        .append(name.to_snake_case())
                        .append(", ")
                        .append(chars)
                        .append("} end")
                }

                ButcheredModuleValueConstructor::Fn => module_select_fn(typ, module_name, label),
            }
        }

        ButcheredTypedExpr::RecordAccess { record, index, .. } => {
            tuple_index(record, index.as_ref() + 1, env)
        }

        ButcheredTypedExpr::Let {
            value,
            pattern,
            then,
            kind: BindingKind::Try,
            ..
        } => try_(value, pattern, then, env),

        ButcheredTypedExpr::Let {
            value,
            pattern,
            then,
            ..
        } => let_(value, pattern, then, env),

        ButcheredTypedExpr::Case {
            subjects, clauses, ..
        } => case(subjects, clauses, env),

        ButcheredTypedExpr::BinOp {
            name, left, right, ..
        } => bin_op(&name, left, right, env),

        ButcheredTypedExpr::Tuple { elems, .. } => {
            tuple(elems.into_cow_iter().map(|e| wrap_expr(e, env)))
        }

        ButcheredTypedExpr::BitString { segments, .. } => bit_string(
            segments
                .into_cow_iter()
                .map(|s| {
                    let ButcheredBitStringSegment { value, options, .. } =
                        BitStringSegment::butcher(s);
                    (value, options)
                })
                .map(|(value, options)| expr_segment(value, options, env)),
        ),
    }
}

fn tuple_index<'a>(tuple: Cow<'a, TypedExpr>, index: u64, env: &mut Env) -> Document<'a> {
    use std::iter::once;
    let index_doc = format!("{}", (index + 1)).to_doc();
    let tuple_doc = expr(tuple, env);
    let iter = once(index_doc).chain(once(tuple_doc));
    "erlang:element".to_doc().append(wrap_args(iter))
}

fn module_select_fn<'a>(
    typ: Arc<crate::typ::Type>,
    module_name: Cow<'a, [String]>,
    label: Cow<'a, str>,
) -> Document<'a> {
    match crate::typ::collapse_links(typ).as_ref() {
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

fn fun<'a>(args: Cow<'a, [TypedArg]>, body: Cow<'a, TypedExpr>, env: &mut Env) -> Document<'a> {
    "fun"
        .to_doc()
        .append(fun_args(&args, env).append(" ->"))
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

    atom(Cow::Borrowed(name))
        .append(format!("({}) ->", chars))
        .append(line())
        .append(atom(Cow::Borrowed(module)))
        .append(":")
        .append(atom(Cow::Borrowed(fun)))
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
