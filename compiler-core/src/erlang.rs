// TODO: Refactor this module to be methods on structs rather than free
// functions with a load of arguments. See the JavaScript code generator and the
// formatter for examples.

mod pattern;
#[cfg(test)]
mod tests;

use crate::{
    ast::*,
    docvec,
    io::{OutputFile, Utf8Writer},
    line_numbers::LineNumbers,
    pretty::*,
    project::Analysed,
    type_::{
        ModuleValueConstructor, PatternConstructor, Type, TypeVar, ValueConstructor,
        ValueConstructorVariant,
    },
    Result,
};
use heck::ToSnakeCase;
use itertools::Itertools;
use lazy_static::lazy_static;
use pattern::pattern;
use std::{char, collections::HashMap, ops::Deref, str::FromStr, sync::Arc};

const INDENT: isize = 4;
const MAX_COLUMNS: isize = 80;

pub fn generate_erlang(analysed: &[Analysed]) -> Vec<OutputFile> {
    let mut files = Vec::with_capacity(analysed.len() * 2);

    for Analysed {
        name,
        origin,
        source_base_path,
        ast,
        src,
        ..
    } in analysed
    {
        let gen_dir = source_base_path
            .parent()
            .expect("generate_erlang parent")
            .join(crate::project::OUTPUT_DIR_NAME)
            .join(origin.dir_name());
        let erl_module_name = name.join("@");

        for (name, text) in records(ast) {
            files.push(OutputFile {
                path: gen_dir.join(format!("{}_{}.hrl", erl_module_name, name)),
                text,
            })
        }

        let mut text = String::new();
        let line_numbers = LineNumbers::new(src);
        module(ast, &line_numbers, &mut text).expect("Buffer writing failed");
        files.push(OutputFile {
            path: gen_dir.join(format!("{}.erl", erl_module_name)),
            text,
        });
    }

    files
}

fn module_name_to_erlang(module: &str) -> Document<'_> {
    Document::String(module.replace('/', "@"))
}

fn module_name_join(module: &[String]) -> Document<'_> {
    let mut name = String::new();
    for (i, segment) in module.iter().enumerate() {
        if i != 0 {
            name.push('@')
        }
        name.push_str(segment)
    }
    atom(name)
}

#[derive(Debug, Clone)]
struct Env<'a> {
    module: &'a [String],
    function: &'a str,
    line_numbers: &'a LineNumbers,
    current_scope_vars: im::HashMap<String, usize>,
    erl_function_scope_vars: im::HashMap<String, usize>,
}

impl<'env> Env<'env> {
    pub fn new(
        module: &'env [String],
        function: &'env str,
        line_numbers: &'env LineNumbers,
    ) -> Self {
        let vars: im::HashMap<_, _> = std::iter::once(("_".to_string(), 0)).collect();
        Self {
            current_scope_vars: vars.clone(),
            erl_function_scope_vars: vars,
            line_numbers,
            function,
            module,
        }
    }

    pub fn local_var_name<'a>(&mut self, name: &str) -> Document<'a> {
        match self.current_scope_vars.get(name) {
            None => {
                let _ = self.current_scope_vars.insert(name.to_string(), 0);
                let _ = self.erl_function_scope_vars.insert(name.to_string(), 0);
                Document::String(variable_name(name))
            }
            Some(0) => Document::String(variable_name(name)),
            Some(n) => {
                use std::fmt::Write;
                let mut name = variable_name(name);
                write!(name, "@{}", n).expect("pushing number suffix to name");
                Document::String(name)
            }
        }
    }

    pub fn next_local_var_name<'a>(&mut self, name: &str) -> Document<'a> {
        let next = self.erl_function_scope_vars.get(name).map_or(0, |i| i + 1);
        let _ = self.erl_function_scope_vars.insert(name.to_string(), next);
        let _ = self.current_scope_vars.insert(name.to_string(), next);
        self.local_var_name(name)
    }
}

pub fn records(module: &TypedModule) -> Vec<(&str, String)> {
    module
        .statements
        .iter()
        .filter_map(|s| match s {
            Statement::CustomType {
                public: true,
                constructors,
                ..
            } => Some(constructors),
            _ => None,
        })
        .flatten()
        .filter(|constructor| !constructor.arguments.is_empty())
        .filter_map(|constructor| {
            constructor
                .arguments
                .iter()
                .map(
                    |RecordConstructorArg {
                         label,
                         ast: _,
                         location: _,
                         type_,
                         ..
                     }| {
                        label.as_deref().map(|label| (label, type_.clone()))
                    },
                )
                .collect::<Option<Vec<_>>>()
                .map(|fields| (constructor.name.as_str(), fields))
        })
        .map(|(name, fields)| (name, record_definition(name, &fields)))
        .collect()
}

pub fn record_definition(name: &str, fields: &[(&str, Arc<Type>)]) -> String {
    let name = &name.to_snake_case();
    let type_printer = TypePrinter::new(&[]).var_as_any();
    let fields = fields.iter().map(move |(name, type_)| {
        let type_ = type_printer.print(type_);
        docvec!(atom((*name).to_string()), " :: ", type_.group())
    });
    let fields = break_("", "")
        .append(concat(Itertools::intersperse(fields, break_(",", ", "))))
        .nest(INDENT)
        .append(break_("", ""))
        .group();
    docvec!(
        "-record(",
        atom(name.to_string()),
        ", {",
        fields,
        "}).",
        line()
    )
    .to_pretty_string(MAX_COLUMNS)
}

pub fn module<'a>(
    module: &'a TypedModule,
    line_numbers: &'a LineNumbers,
    writer: &mut impl Utf8Writer,
) -> Result<()> {
    module_document(module, line_numbers)?.pretty_print(MAX_COLUMNS, writer)
}

fn module_document<'a>(
    module: &'a TypedModule,
    line_numbers: &'a LineNumbers,
) -> Result<Document<'a>> {
    let mut exports = vec![];
    let mut type_defs = vec![];
    let mut type_exports = vec![];

    let header = "-module("
        .to_doc()
        .append(module_name_join(&module.name))
        .append(").")
        .append(line());

    for s in &module.statements {
        register_imports(
            s,
            &mut exports,
            &mut type_exports,
            &mut type_defs,
            &module.name,
        );
    }

    let exports = match (!exports.is_empty(), !type_exports.is_empty()) {
        (false, false) => return Ok(header),
        (true, false) => "-export(["
            .to_doc()
            .append(concat(Itertools::intersperse(
                exports.into_iter(),
                ", ".to_doc(),
            )))
            .append("]).")
            .append(lines(2)),

        (true, true) => "-export(["
            .to_doc()
            .append(concat(Itertools::intersperse(
                exports.into_iter(),
                ", ".to_doc(),
            )))
            .append("]).")
            .append(line())
            .append("-export_type([")
            .to_doc()
            .append(concat(Itertools::intersperse(
                type_exports.into_iter(),
                ", ".to_doc(),
            )))
            .append("]).")
            .append(lines(2)),

        (false, true) => "-export_type(["
            .to_doc()
            .append(concat(Itertools::intersperse(
                type_exports.into_iter(),
                ", ".to_doc(),
            )))
            .append("]).")
            .append(lines(2)),
    };

    let type_defs = if type_defs.is_empty() {
        nil()
    } else {
        concat(Itertools::intersperse(type_defs.into_iter(), lines(2))).append(lines(2))
    };

    let statements = concat(Itertools::intersperse(
        module
            .statements
            .iter()
            .flat_map(|s| statement(&module.name, s, &module.name, line_numbers)),
        lines(2),
    ));

    Ok(header
        .append("-compile(no_auto_import).")
        .append(lines(2))
        .append(exports)
        .append(type_defs)
        .append(statements)
        .append(line()))
}

fn register_imports(
    s: &TypedStatement,
    exports: &mut Vec<Document<'_>>,
    type_exports: &mut Vec<Document<'_>>,
    type_defs: &mut Vec<Document<'_>>,
    module_name: &[String],
) {
    match s {
        Statement::Fn {
            public: true,
            name,
            arguments: args,
            ..
        } => exports.push(atom(name.to_string()).append("/").append(args.len())),

        Statement::ExternalFn {
            public: true,
            name,
            arguments: args,
            ..
        } => exports.push(atom(name.to_string()).append("/").append(args.len())),

        Statement::ExternalType {
            name,
            arguments: args,
            ..
        } => {
            // Type Exports
            type_exports.push(
                Document::String(erl_safe_type_name(name.to_snake_case()))
                    .append("/")
                    .append(args.len()),
            );
            // phantom variant
            let phantom = if args.is_empty() {
                nil()
            } else {
                " | ".to_doc().append(tuple(
                    std::iter::once("gleam_phantom".to_doc())
                        .chain(args.iter().map(|a| Document::String(variable_name(a)))),
                ))
            };

            // Type definition
            let args = concat(Itertools::intersperse(
                args.iter().map(|p| Document::String(variable_name(p))),
                ", ".to_doc(),
            ));

            let doc = "-type "
                .to_doc()
                .append(Document::String(erl_safe_type_name(name.to_snake_case())))
                .append("(")
                .append(args)
                .append(") :: any()")
                .append(phantom)
                .append(".");
            type_defs.push(doc);
        }

        Statement::CustomType {
            name,
            constructors,
            typed_parameters,
            opaque,
            ..
        } => {
            // Erlang doesn't allow phantom type variables in type definitions but gleam does
            // so we check the type declaratinon against its constroctors and generate a phantom
            // value that uses the unused type variables.
            let type_var_usages = collect_type_var_usages(HashMap::new(), typed_parameters);
            let mut constructor_var_usages = HashMap::new();
            for c in constructors {
                constructor_var_usages = collect_type_var_usages(
                    constructor_var_usages,
                    c.arguments.iter().map(|a| &a.type_),
                );
            }
            let phantom_vars: Vec<_> = type_var_usages
                .keys()
                .filter(|&id| !constructor_var_usages.contains_key(id))
                .map(|&id| Type::Var {
                    type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id })),
                })
                .collect();
            let phantom_vars_constructor = if !phantom_vars.is_empty() {
                let type_printer = TypePrinter::new(module_name);
                Some(tuple(
                    std::iter::once("gleam_phantom".to_doc())
                        .chain(phantom_vars.iter().map(|pv| type_printer.print(pv))),
                ))
            } else {
                None
            };
            // Type Exports
            type_exports.push(
                Document::String(erl_safe_type_name(name.to_snake_case()))
                    .append("/")
                    .append(typed_parameters.len()),
            );
            // Type definitions
            let constructors = concat(Itertools::intersperse(
                constructors
                    .iter()
                    .map(|c| {
                        let name = atom(c.name.to_snake_case());
                        if c.arguments.is_empty() {
                            name
                        } else {
                            let type_printer = TypePrinter::new(module_name);
                            let args = c.arguments.iter().map(|a| type_printer.print(&a.type_));
                            tuple(std::iter::once(name).chain(args))
                        }
                    })
                    .chain(phantom_vars_constructor),
                break_(" |", " | "),
            ))
            .nest(INDENT)
            .group()
            .append(".");
            let type_printer = TypePrinter::new(module_name);
            let params = concat(Itertools::intersperse(
                typed_parameters.iter().map(|a| type_printer.print(a)),
                ", ".to_doc(),
            ));
            let doc = if *opaque { "-opaque " } else { "-type " }
                .to_doc()
                .append(Document::String(erl_safe_type_name(name.to_snake_case())))
                .append("(")
                .append(params)
                .append(") :: ")
                .append(constructors);
            type_defs.push(doc);
        }

        Statement::Fn { .. }
        | Statement::Import { .. }
        | Statement::TypeAlias { .. }
        | Statement::ExternalFn { .. }
        | Statement::ModuleConstant { .. } => (),
    }
}

fn statement<'a>(
    current_module: &'a [String],
    statement: &'a TypedStatement,
    module: &'a [String],
    line_numbers: &'a LineNumbers,
) -> Vec<Document<'a>> {
    match statement {
        Statement::TypeAlias { .. }
        | Statement::CustomType { .. }
        | Statement::Import { .. }
        | Statement::ExternalType { .. }
        | Statement::ModuleConstant { .. }
        | Statement::ExternalFn { public: false, .. } => vec![],

        Statement::Fn {
            arguments: args,
            name,
            body,
            return_type,
            ..
        } => vec![mod_fun(name, args, body, module, return_type, line_numbers)],

        Statement::ExternalFn {
            fun,
            module,
            arguments: args,
            name,
            return_type,
            ..
        } => vec![external_fun(
            current_module,
            name,
            module,
            fun,
            args,
            return_type,
        )],
    }
}

fn mod_fun<'a>(
    name: &'a str,
    args: &'a [TypedArg],
    body: &'a TypedExpr,
    module: &'a [String],
    return_type: &'a Arc<Type>,
    line_numbers: &'a LineNumbers,
) -> Document<'a> {
    let mut env = Env::new(module, name, line_numbers);
    let var_usages = collect_type_var_usages(
        HashMap::new(),
        std::iter::once(return_type).chain(args.iter().map(|a| &a.type_)),
    );
    let type_printer = TypePrinter::new(module).with_var_usages(&var_usages);
    let args_spec = args.iter().map(|a| type_printer.print(&a.type_));
    let return_spec = type_printer.print(return_type);
    let spec = fun_spec(name, args_spec, return_spec);

    spec.append(atom(name.to_string()))
        .append(fun_args(args, &mut env))
        .append(" ->")
        .append(line().append(expr(body, &mut env)).nest(INDENT).group())
        .append(".")
}

fn fun_args<'a>(args: &'a [TypedArg], env: &mut Env<'a>) -> Document<'a> {
    wrap_args(args.iter().map(|a| match &a.names {
        ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => "_".to_doc(),
        ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
            env.next_local_var_name(name)
        }
    }))
}

fn wrap_args<'a, I>(args: I) -> Document<'a>
where
    I: IntoIterator<Item = Document<'a>>,
{
    break_("", "")
        .append(concat(Itertools::intersperse(
            args.into_iter(),
            break_(",", ", "),
        )))
        .nest(INDENT)
        .append(break_("", ""))
        .surround("(", ")")
        .group()
}

fn fun_spec<'a>(
    name: &str,
    args: impl IntoIterator<Item = Document<'a>>,
    retrn: Document<'a>,
) -> Document<'a> {
    "-spec "
        .to_doc()
        .append(atom(name.to_string()))
        .append(wrap_args(args))
        .append(" -> ")
        .append(retrn)
        .append(".")
        .append(line())
        .group()
}

fn atom<'a>(value: String) -> Document<'a> {
    Document::String(escape_atom(value))
}

fn escape_atom(value: String) -> String {
    use regex::Regex;
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^[a-z][a-z0-9_@]*$").expect("atom RE regex");
    }

    if is_erlang_reserved_word(&value) {
        // Escape because of keyword collision
        format!("'{}'", value)
    } else if RE.is_match(&value) {
        // No need to escape
        value
    } else {
        // Escape because of characters contained
        format!("'{}'", value)
    }
}

fn string(value: &str) -> Document<'_> {
    value.to_doc().surround("<<\"", "\"/utf8>>")
}

fn tuple<'a>(elems: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    concat(Itertools::intersperse(elems.into_iter(), break_(",", ", ")))
        .nest_current()
        .surround("{", "}")
        .group()
}

fn bit_string<'a>(elems: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    concat(Itertools::intersperse(elems.into_iter(), break_(",", ", ")))
        .nest_current()
        .surround("<<", ">>")
        .group()
}

fn const_segment<'a>(
    value: &'a TypedConstant,
    options: &'a [BitStringSegmentOption<TypedConstant>],
    env: &mut Env<'a>,
) -> Document<'a> {
    let document = match value {
        // Skip the normal <<value/utf8>> surrounds
        Constant::String { value, .. } => value.to_doc().surround("\"", "\""),

        // As normal
        Constant::Int { .. } | Constant::Float { .. } | Constant::BitString { .. } => {
            const_inline(value, env)
        }

        // Wrap anything else in parentheses
        value => const_inline(value, env).surround("(", ")"),
    };

    let size = |value: &'a TypedConstant, env: &mut Env<'a>| match value {
        Constant::Int { .. } => Some(":".to_doc().append(const_inline(value, env))),
        _ => Some(
            ":".to_doc()
                .append(const_inline(value, env).surround("(", ")")),
        ),
    };

    let unit = |value: &'a u8| Some(Document::String(format!("unit:{}", value)));

    bit_string_segment(document, options, size, unit, true, env)
}

fn expr_segment<'a>(
    value: &'a TypedExpr,
    options: &'a [BitStringSegmentOption<TypedExpr>],
    env: &mut Env<'a>,
) -> Document<'a> {
    let mut value_is_a_string_literal = false;

    let document = match value {
        // Skip the normal <<value/utf8>> surrounds and set the string literal flag
        TypedExpr::String { value, .. } => {
            value_is_a_string_literal = true;
            value.to_doc().surround("\"", "\"")
        }

        // As normal
        TypedExpr::Int { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::Var { .. }
        | TypedExpr::BitString { .. } => expr(value, env),

        // Wrap anything else in parentheses
        value => expr(value, env).surround("(", ")"),
    };

    let size = |expression: &'a TypedExpr, env: &mut Env<'a>| match expression {
        TypedExpr::Int { value, .. } => {
            let v = value.replace('_', "");
            let v = u64::from_str(&v).unwrap_or(0);
            Some(Document::String(format!(":{}", v)))
        }

        _ => {
            let inner_expr = expr(expression, env).surround("(", ")");
            // The value of size must be a non-negative integer, we use lists:max here to ensure
            // it is at least 0;
            let value_guard = ":(lists:max(["
                .to_doc()
                .append(inner_expr)
                .append(", 0]))")
                .group();
            Some(value_guard)
        }
    };

    let unit = |value: &'a u8| Some(Document::String(format!("unit:{}", value)));

    bit_string_segment(
        document,
        options,
        size,
        unit,
        value_is_a_string_literal,
        env,
    )
}

fn bit_string_segment<'a, Value: 'a, SizeToDoc, UnitToDoc>(
    mut document: Document<'a>,
    options: &'a [BitStringSegmentOption<Value>],
    mut size_to_doc: SizeToDoc,
    mut unit_to_doc: UnitToDoc,
    value_is_a_string_literal: bool,
    env: &mut Env<'a>,
) -> Document<'a>
where
    SizeToDoc: FnMut(&'a Value, &mut Env<'a>) -> Option<Document<'a>>,
    UnitToDoc: FnMut(&'a u8) -> Option<Document<'a>>,
{
    let mut size: Option<Document<'a>> = None;
    let mut unit: Option<Document<'a>> = None;
    let mut others = Vec::new();

    // Erlang only allows valid codepoint integers to be used as values for utf segments
    // We want to support <<string_var:utf8>> for all string variables, but <<StringVar/utf8>> is invalid
    // To work around this we use the binary type specifier for these segments instead
    let override_type = if !value_is_a_string_literal {
        Some("binary")
    } else {
        None
    };

    for option in options {
        use BitStringSegmentOption as Opt;
        if !others.is_empty() && !matches!(option, Opt::Size { .. } | Opt::Unit { .. }) {
            others.push("-".to_doc());
        }
        match option {
            Opt::Utf8 { .. } => others.push(override_type.unwrap_or("utf8").to_doc()),
            Opt::Utf16 { .. } => others.push(override_type.unwrap_or("utf16").to_doc()),
            Opt::Utf32 { .. } => others.push(override_type.unwrap_or("utf32").to_doc()),
            Opt::Int { .. } => others.push("integer".to_doc()),
            Opt::Float { .. } => others.push("float".to_doc()),
            Opt::Binary { .. } => others.push("binary".to_doc()),
            Opt::BitString { .. } => others.push("bitstring".to_doc()),
            Opt::Utf8Codepoint { .. } => others.push("utf8".to_doc()),
            Opt::Utf16Codepoint { .. } => others.push("utf16".to_doc()),
            Opt::Utf32Codepoint { .. } => others.push("utf32".to_doc()),
            Opt::Signed { .. } => others.push("signed".to_doc()),
            Opt::Unsigned { .. } => others.push("unsigned".to_doc()),
            Opt::Big { .. } => others.push("big".to_doc()),
            Opt::Little { .. } => others.push("little".to_doc()),
            Opt::Native { .. } => others.push("native".to_doc()),
            Opt::Size { value, .. } => size = size_to_doc(value, env),
            Opt::Unit { value, .. } => unit = unit_to_doc(value),
        }
    }

    document = document.append(size);
    let others_is_empty = others.is_empty();

    if !others_is_empty {
        document = document.append("/").append(others);
    }

    if unit.is_some() {
        if !others_is_empty {
            document = document.append("-").append(unit)
        } else {
            document = document.append("/").append(unit)
        }
    }

    document
}

fn seq<'a>(expressions: &'a [TypedExpr], env: &mut Env<'a>) -> Document<'a> {
    let count = expressions.len();
    let mut documents = Vec::with_capacity(count * 3);
    documents.push(force_break());
    for (i, expression) in expressions.iter().enumerate() {
        documents.push(expr(expression, env));
        if i + 1 < count {
            // This isn't the final expression so add the delimeters
            documents.push(",".to_doc());
            documents.push(line());
        }
    }
    documents.to_doc()
}

fn bin_op<'a>(
    name: &'a BinOp,
    left: &'a TypedExpr,
    right: &'a TypedExpr,
    env: &mut Env<'a>,
) -> Document<'a> {
    let div_zero = match name {
        BinOp::DivInt | BinOp::ModuloInt => Some("0"),
        BinOp::DivFloat => Some("0.0"),
        _ => None,
    };
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

    let left_expr = match left {
        TypedExpr::BinOp { .. } => expr(left, env).surround("(", ")"),
        _ => maybe_block_expr(left, env),
    };

    let right_expr = match right {
        TypedExpr::BinOp { .. } => expr(right, env).surround("(", ")"),
        _ => maybe_block_expr(right, env),
    };

    let div = |left: Document<'a>, right: Document<'a>| {
        left.append(break_("", " "))
            .append(op)
            .append(" ")
            .append(right)
    };

    match div_zero {
        Some(_) if right.non_zero_compile_time_number() => div(left_expr, right_expr),
        None => div(left_expr, right_expr),

        Some(zero) => {
            let denominator = "gleam@denominator";
            "case "
                .to_doc()
                .append(right_expr)
                .append(" of")
                .append(
                    line()
                        .append(zero)
                        .append(" -> ")
                        .append(zero)
                        .append(";")
                        .append(line())
                        .append(env.next_local_var_name(denominator))
                        .append(" -> ")
                        .append(div(left_expr, env.local_var_name(denominator)))
                        .nest(INDENT),
                )
                .append(line())
                .append("end")
        }
    }
}

fn try_<'a>(
    value: &'a TypedExpr,
    pat: &'a TypedPattern,
    then: &'a TypedExpr,
    env: &mut Env<'a>,
) -> Document<'a> {
    "case "
        .to_doc()
        .append(maybe_block_expr(value, env))
        .append(" of")
        .append(
            line()
                .append("{error, ")
                .append(env.next_local_var_name(TRY_VARIABLE))
                .append("} -> {error, ")
                .append(env.local_var_name(TRY_VARIABLE))
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

fn assert<'a>(value: &'a TypedExpr, pat: &'a TypedPattern, env: &mut Env<'a>) -> Document<'a> {
    let mut vars: Vec<&str> = vec![];
    let body = maybe_block_expr(value, env);
    let pattern1 = pattern::to_doc(pat, &mut vars, env);
    let pattern2 = pattern::to_doc(pat, &mut vars, env);
    let clauses = docvec![
        pattern1.clone(),
        " -> ",
        pattern1.clone(),
        ";",
        line(),
        env.next_local_var_name(ASSERT_VARIABLE),
        " ->",
        docvec![
            line(),
            erlang_error(
                "assert",
                "Assertion pattern match failed",
                pat.location(),
                vec![("value", env.local_var_name(ASSERT_VARIABLE))],
                env,
            )
            .nest(INDENT)
        ]
        .nest(INDENT)
    ];
    docvec![
        pattern2,
        " = case ",
        body,
        " of",
        docvec![line(), clauses].nest(INDENT),
        line(),
        "end",
    ]
}

fn let_<'a>(value: &'a TypedExpr, pat: &'a TypedPattern, env: &mut Env<'a>) -> Document<'a> {
    let body = maybe_block_expr(value, env);
    pattern(pat, env).append(" = ").append(body)
}

fn float<'a>(value: &str) -> Document<'a> {
    let mut value = value.replace('_', "");
    if value.ends_with('.') {
        value.push('0')
    }
    Document::String(value)
}

fn expr_list<'a>(
    elements: &'a [TypedExpr],
    tail: &'a Option<Box<TypedExpr>>,
    env: &mut Env<'a>,
) -> Document<'a> {
    let elements = concat(Itertools::intersperse(
        elements.iter().map(|e| maybe_block_expr(e, env)),
        break_(",", ", "),
    ));
    list(elements, tail.as_ref().map(|e| expr(e, env)))
}

fn list<'a>(elems: Document<'a>, tail: Option<Document<'a>>) -> Document<'a> {
    let elems = if let Some(final_tail) = tail {
        elems.append(break_(" |", " | ")).append(final_tail)
    } else {
        elems
    };

    elems.to_doc().nest_current().surround("[", "]").group()
}

fn var<'a>(name: &'a str, constructor: &'a ValueConstructor, env: &mut Env<'a>) -> Document<'a> {
    match &constructor.variant {
        ValueConstructorVariant::Record {
            name: record_name, ..
        } => match constructor.type_.deref() {
            Type::Fn { args, .. } => {
                let chars = incrementing_args_list(args.len());
                "fun("
                    .to_doc()
                    .append(Document::String(chars.clone()))
                    .append(") -> {")
                    .append(atom(record_name.to_snake_case()))
                    .append(", ")
                    .append(Document::String(chars))
                    .append("} end")
            }
            _ => atom(record_name.to_snake_case()),
        },

        ValueConstructorVariant::LocalVariable { .. } => env.local_var_name(name),

        ValueConstructorVariant::ModuleConstant { literal, .. } => const_inline(literal, env),

        ValueConstructorVariant::ModuleFn {
            arity, ref module, ..
        } if module == env.module => "fun "
            .to_doc()
            .append(atom(name.to_string()))
            .append("/")
            .append(*arity),

        ValueConstructorVariant::ModuleFn {
            arity,
            module,
            name,
            ..
        } => "fun "
            .to_doc()
            .append(module_name_join(module))
            .append(":")
            .append(atom(name.to_string()))
            .append("/")
            .append(*arity),
    }
}

fn int<'a>(value: &str) -> Document<'a> {
    Document::String(
        value
            .replace('_', "")
            .replace("0x", "16#")
            .replace("0o", "8#")
            .replace("0b", "2#"),
    )
}

fn const_inline<'a>(literal: &'a TypedConstant, env: &mut Env<'a>) -> Document<'a> {
    match literal {
        Constant::Int { value, .. } => int(value),
        Constant::Float { value, .. } => float(value),
        Constant::String { value, .. } => string(value),
        Constant::Tuple { elements, .. } => tuple(elements.iter().map(|e| const_inline(e, env))),

        Constant::List { elements, .. } => {
            let elements = Itertools::intersperse(
                elements.iter().map(|e| const_inline(e, env)),
                break_(",", ", "),
            );
            concat(elements).nest_current().surround("[", "]").group()
        }

        Constant::BitString { segments, .. } => bit_string(
            segments
                .iter()
                .map(|s| const_segment(&s.value, &s.options, env)),
        ),

        Constant::Record { tag, args, .. } => {
            if args.is_empty() {
                atom(tag.to_snake_case())
            } else {
                let args = args.iter().map(|a| const_inline(&a.value, env));
                let tag = atom(tag.to_snake_case());
                tuple(std::iter::once(tag).chain(args))
            }
        }
    }
}

fn clause<'a>(clause: &'a TypedClause, env: &mut Env<'a>) -> Document<'a> {
    let Clause {
        guard,
        pattern: pat,
        alternative_patterns,
        then,
        ..
    } = clause;

    // These are required to get the alternative patterns working properly.
    // Simply rendering the duplicate erlang clauses breaks the variable rewriting
    let mut then_doc = None;
    let erlang_vars = env.erl_function_scope_vars.clone();

    let docs = Itertools::intersperse(
        std::iter::once(pat)
            .chain(alternative_patterns)
            .map(|patterns| {
                env.erl_function_scope_vars = erlang_vars.clone();

                let patterns_doc = if patterns.len() == 1 {
                    let p = patterns.get(0).expect("Single pattern clause printing");
                    pattern(p, env)
                } else {
                    tuple(patterns.iter().map(|p| pattern(p, env)))
                };

                if then_doc == None {
                    then_doc = Some(expr(then, env));
                }

                patterns_doc.append(
                    optional_clause_guard(guard.as_ref(), env)
                        .append(" ->")
                        .append(line().append(then_doc.clone()).nest(INDENT).group()),
                )
            }),
        ";".to_doc().append(lines(2)),
    );

    concat(docs)
}

fn optional_clause_guard<'a>(
    guard: Option<&'a TypedClauseGuard>,
    env: &mut Env<'a>,
) -> Document<'a> {
    guard
        .map(|guard| " when ".to_doc().append(bare_clause_guard(guard, env)))
        .unwrap_or_else(nil)
}

fn bare_clause_guard<'a>(guard: &'a TypedClauseGuard, env: &mut Env<'a>) -> Document<'a> {
    match guard {
        ClauseGuard::Or { left, right, .. } => clause_guard(left, env)
            .append(" orelse ")
            .append(clause_guard(right, env)),

        ClauseGuard::And { left, right, .. } => clause_guard(left, env)
            .append(" andalso ")
            .append(clause_guard(right, env)),

        ClauseGuard::Equals { left, right, .. } => clause_guard(left, env)
            .append(" =:= ")
            .append(clause_guard(right, env)),

        ClauseGuard::NotEquals { left, right, .. } => clause_guard(left, env)
            .append(" =/= ")
            .append(clause_guard(right, env)),

        ClauseGuard::GtInt { left, right, .. } => clause_guard(left, env)
            .append(" > ")
            .append(clause_guard(right, env)),

        ClauseGuard::GtEqInt { left, right, .. } => clause_guard(left, env)
            .append(" >= ")
            .append(clause_guard(right, env)),

        ClauseGuard::LtInt { left, right, .. } => clause_guard(left, env)
            .append(" < ")
            .append(clause_guard(right, env)),

        ClauseGuard::LtEqInt { left, right, .. } => clause_guard(left, env)
            .append(" =< ")
            .append(clause_guard(right, env)),

        ClauseGuard::GtFloat { left, right, .. } => clause_guard(left, env)
            .append(" > ")
            .append(clause_guard(right, env)),

        ClauseGuard::GtEqFloat { left, right, .. } => clause_guard(left, env)
            .append(" >= ")
            .append(clause_guard(right, env)),

        ClauseGuard::LtFloat { left, right, .. } => clause_guard(left, env)
            .append(" < ")
            .append(clause_guard(right, env)),

        ClauseGuard::LtEqFloat { left, right, .. } => clause_guard(left, env)
            .append(" =< ")
            .append(clause_guard(right, env)),

        // Only local variables are supported and the typer ensures that all
        // ClauseGuard::Vars are local variables
        ClauseGuard::Var { name, .. } => env.local_var_name(name),

        ClauseGuard::TupleIndex { tuple, index, .. } => tuple_index_inline(tuple, *index, env),

        ClauseGuard::Constant(constant) => const_inline(constant, env),
    }
}

fn tuple_index_inline<'a>(
    tuple: &'a TypedClauseGuard,
    index: u64,
    env: &mut Env<'a>,
) -> Document<'a> {
    let index_doc = Document::String(format!("{}", (index + 1)));
    let tuple_doc = bare_clause_guard(tuple, env);
    "erlang:element"
        .to_doc()
        .append(wrap_args([index_doc, tuple_doc]))
}

fn clause_guard<'a>(guard: &'a TypedClauseGuard, env: &mut Env<'a>) -> Document<'a> {
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
        ClauseGuard::Constant(_) | ClauseGuard::Var { .. } | ClauseGuard::TupleIndex { .. } => {
            bare_clause_guard(guard, env)
        }
    }
}

fn clauses<'a>(cs: &'a [TypedClause], env: &mut Env<'a>) -> Document<'a> {
    concat(Itertools::intersperse(
        cs.iter().map(|c| {
            let vars = env.current_scope_vars.clone();
            let erl = clause(c, env);
            env.current_scope_vars = vars; // Reset the known variables now the clauses' scope has ended
            erl
        }),
        ";".to_doc().append(lines(2)),
    ))
}

fn case<'a>(subjects: &'a [TypedExpr], cs: &'a [TypedClause], env: &mut Env<'a>) -> Document<'a> {
    let subjects_doc = if subjects.len() == 1 {
        let subject = subjects
            .get(0)
            .expect("erl case printing of single subject");
        maybe_block_expr(subject, env).group()
    } else {
        tuple(subjects.iter().map(|e| maybe_block_expr(e, env)))
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

fn call<'a>(fun: &'a TypedExpr, args: &'a [CallArg<TypedExpr>], env: &mut Env<'a>) -> Document<'a> {
    docs_args_call(
        fun,
        args.iter()
            .map(|arg| maybe_block_expr(&arg.value, env))
            .collect(),
        env,
    )
}

fn docs_args_call<'a>(
    fun: &'a TypedExpr,
    mut args: Vec<Document<'a>>,
    env: &mut Env<'a>,
) -> Document<'a> {
    match fun {
        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, .. },
            ..
        }
        | TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::Record { name, .. },
                    ..
                },
            ..
        } => tuple(std::iter::once(atom(name.to_snake_case())).chain(args)),

        TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::ModuleFn { module, name, .. },
                    ..
                },
            ..
        } => {
            let args = wrap_args(args);
            if module == env.module {
                atom(name.to_string()).append(args)
            } else {
                atom(module.join("@"))
                    .append(":")
                    .append(atom(name.to_string()))
                    .append(args)
            }
        }

        TypedExpr::ModuleSelect {
            module_name,
            label,
            constructor: ModuleValueConstructor::Fn { .. },
            ..
        } => {
            let args = wrap_args(args);
            atom(module_name.replace('/', "@"))
                .append(":")
                .append(atom(label.to_string()))
                .append(args)
        }

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
                let mut merged_args = Vec::with_capacity(inner_args.len());
                for arg in inner_args {
                    match &arg.value {
                        TypedExpr::Var { name, .. } if name == CAPTURE_VARIABLE => {
                            merged_args.push(args.swap_remove(0))
                        }
                        e => merged_args.push(expr(e, env)),
                    }
                }
                docs_args_call(fun, merged_args, env)
            } else {
                panic!("Erl printing: Capture was not a call")
            }
        }

        TypedExpr::Call { .. }
        | TypedExpr::Fn { .. }
        | TypedExpr::RecordAccess { .. }
        | TypedExpr::TupleIndex { .. } => {
            let args = wrap_args(args);
            expr(fun, env).surround("(", ")").append(args)
        }

        other => {
            let args = wrap_args(args);
            expr(other, env).append(args)
        }
    }
}

fn record_update<'a>(
    spread: &'a TypedExpr,
    args: &'a [TypedRecordUpdateArg],
    env: &mut Env<'a>,
) -> Document<'a> {
    args.iter().fold(expr(spread, env), |tuple_doc, arg| {
        // Increment the index by 2, because the first element
        // is the name of the record, so our fields are 2-indexed
        let index_doc = (arg.index + 2).to_doc();
        let value_doc = maybe_block_expr(&arg.value, env);

        "erlang:setelement"
            .to_doc()
            .append(wrap_args([index_doc, tuple_doc, value_doc]))
    })
}

/// Wrap a document in begin end
///
fn begin_end(document: Document<'_>) -> Document<'_> {
    force_break()
        .append("begin")
        .append(line().append(document).nest(INDENT))
        .append(line())
        .append("end")
}

/// Same as expr, expect it wraps seq, let, etc in begin end
///
fn maybe_block_expr<'a>(expression: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    match &expression {
        TypedExpr::Pipeline { .. } | TypedExpr::Sequence { .. } | TypedExpr::Assignment { .. } => {
            begin_end(expr(expression, env))
        }
        _ => expr(expression, env),
    }
}

fn todo<'a>(message: &'a Option<String>, location: SrcSpan, env: &mut Env<'a>) -> Document<'a> {
    let message = message
        .as_deref()
        .unwrap_or("This has not yet been implemented");
    erlang_error("todo", message, location, vec![], env)
}

fn erlang_error<'a>(
    name: &'a str,
    message: &'a str,
    location: SrcSpan,
    fields: Vec<(&'a str, Document<'a>)>,
    env: &Env<'a>,
) -> Document<'a> {
    let mut fields_doc = docvec![
        "gleam_error => ",
        name,
        ",",
        line(),
        "message => ",
        string(message)
    ];
    for (key, value) in fields {
        fields_doc = fields_doc
            .append(",")
            .append(line())
            .append(key)
            .append(" => ")
            .append(value);
    }
    let fields_doc = fields_doc
        .append(",")
        .append(line())
        .append("module => ")
        .append(Document::String(env.module.join("/")).surround("<<\"", "\"/utf8>>"))
        .append(",")
        .append(line())
        .append("function => ")
        .append(string(env.function))
        .append(",")
        .append(line())
        .append("line => ")
        .append(env.line_numbers.line_number(location.start));
    let error = "#{"
        .to_doc()
        .append(fields_doc.group().nest_current())
        .append("}");
    docvec!["erlang:error", wrap_args([error.group()])]
}

fn expr<'a>(expression: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    match expression {
        TypedExpr::Todo {
            label, location, ..
        } => todo(label, *location, env),

        TypedExpr::Int { value, .. } => int(value),
        TypedExpr::Float { value, .. } => float(value),
        TypedExpr::String { value, .. } => string(value),
        TypedExpr::Pipeline { expressions, .. } | TypedExpr::Sequence { expressions, .. } => {
            seq(expressions, env)
        }

        TypedExpr::TupleIndex { tuple, index, .. } => tuple_index(tuple, *index, env),

        TypedExpr::Var {
            name, constructor, ..
        } => var(name, constructor, env),

        TypedExpr::Fn { args, body, .. } => fun(args, body, env),

        TypedExpr::Negate { value, .. } => negate(value, env),

        TypedExpr::List { elements, tail, .. } => expr_list(elements, tail, env),

        TypedExpr::Call { fun, args, .. } => call(fun, args, env),

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, arity: 0, .. },
            ..
        } => atom(name.to_snake_case()),

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Constant { literal, .. },
            ..
        } => const_inline(literal, env),

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, arity, .. },
            ..
        } => {
            let chars = incrementing_args_list(*arity);
            "fun("
                .to_doc()
                .append(Document::String(chars.clone()))
                .append(") -> {")
                .append(Document::String(name.to_snake_case()))
                .append(", ")
                .append(Document::String(chars))
                .append("} end")
        }

        TypedExpr::ModuleSelect {
            typ,
            label,
            module_name,
            constructor: ModuleValueConstructor::Fn { .. },
            ..
        } => module_select_fn(typ.clone(), module_name, label),

        TypedExpr::RecordAccess { record, index, .. } => tuple_index(record, index + 1, env),

        TypedExpr::RecordUpdate { spread, args, .. } => record_update(spread, args, env),

        TypedExpr::Try {
            value,
            pattern,
            then,
            ..
        } => try_(value, pattern, then, env),

        TypedExpr::Assignment {
            value,
            pattern,
            kind: AssignmentKind::Assert,
            ..
        } => assert(value, pattern, env),

        TypedExpr::Assignment {
            value,
            pattern,
            kind: AssignmentKind::Let,
            ..
        } => let_(value, pattern, env),

        TypedExpr::Case {
            subjects, clauses, ..
        } => case(subjects, clauses, env),

        TypedExpr::BinOp {
            name, left, right, ..
        } => bin_op(name, left, right, env),

        TypedExpr::Tuple { elems, .. } => tuple(elems.iter().map(|e| maybe_block_expr(e, env))),

        TypedExpr::BitString { segments, .. } => bit_string(
            segments
                .iter()
                .map(|s| expr_segment(&s.value, &s.options, env)),
        ),
    }
}

fn negate<'a>(value: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    docvec!["not ", maybe_block_expr(value, env)]
}

fn tuple_index<'a>(tuple: &'a TypedExpr, index: u64, env: &mut Env<'a>) -> Document<'a> {
    let index_doc = Document::String(format!("{}", (index + 1)));
    let tuple_doc = expr(tuple, env);
    "erlang:element"
        .to_doc()
        .append(wrap_args([index_doc, tuple_doc]))
}

fn module_select_fn<'a>(typ: Arc<Type>, module_name: &'a str, label: &'a str) -> Document<'a> {
    match crate::type_::collapse_links(typ).as_ref() {
        crate::type_::Type::Fn { args, .. } => "fun "
            .to_doc()
            .append(module_name_to_erlang(module_name))
            .append(":")
            .append(atom(label.to_string()))
            .append("/")
            .append(args.len()),

        _ => module_name_to_erlang(module_name)
            .append(":")
            .append(atom(label.to_string()))
            .append("()"),
    }
}

fn fun<'a>(args: &'a [TypedArg], body: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    let current_scope_vars = env.current_scope_vars.clone();
    let doc = "fun"
        .to_doc()
        .append(fun_args(args, env).append(" ->"))
        .append(break_("", " ").append(expr(body, env)).nest(INDENT))
        .append(break_("", " "))
        .append("end")
        .group();
    env.current_scope_vars = current_scope_vars;
    doc
}

fn incrementing_args_list(arity: usize) -> String {
    Itertools::intersperse(
        (65..(65 + arity))
            .map(|x| x as u8 as char)
            .map(|c| c.to_string()),
        ", ".to_string(),
    )
    .collect()
}

fn external_fun<'a>(
    current_module: &'a [String],
    name: &'a str,
    module: &'a str,
    fun: &'a str,
    args: &'a [TypedExternalFnArg],
    return_type: &'a Arc<Type>,
) -> Document<'a> {
    let chars: String = incrementing_args_list(args.len());
    let var_usages = collect_type_var_usages(
        HashMap::new(),
        [return_type]
            .into_iter()
            .chain(args.iter().map(|a| &a.type_)),
    );
    let type_printer = TypePrinter::new(current_module).with_var_usages(&var_usages);
    let args_spec = args.iter().map(|a| type_printer.print(&a.type_));
    let return_spec = type_printer.print(return_type);
    let spec = fun_spec(name, args_spec, return_spec);

    spec.append(atom(name.to_string())).append(
        Document::String(format!("({}) ->", chars))
            .append(line())
            .append(atom(module.to_string()))
            .append(":")
            .append(atom(fun.to_string()))
            .append(Document::String(format!("({}).", chars)))
            .nest(INDENT)
            .group(),
    )
}

fn variable_name(name: &str) -> String {
    let mut chars = name.chars();
    let first_char = chars.next();
    let first_uppercased = first_char.into_iter().flat_map(char::to_uppercase);

    first_uppercased.chain(chars).collect()
}

// When rendering a type variable to an erlang type spec we need all type variables with the
// same id to end up with the same name in the generated erlang.
// This function converts a usize into base 26 A-Z for this purpose.
fn id_to_type_var(id: u64) -> Document<'static> {
    if id < 26 {
        let mut name = "".to_string();
        name.push(std::char::from_u32((id % 26 + 65) as u32).expect("id_to_type_var 0"));
        return Document::String(name);
    }
    let mut name = vec![];
    let mut last_char = id;
    while last_char >= 26 {
        name.push(std::char::from_u32((last_char % 26 + 65) as u32).expect("id_to_type_var 1"));
        last_char /= 26;
    }
    name.push(std::char::from_u32((last_char % 26 + 64) as u32).expect("id_to_type_var 2"));
    name.reverse();
    Document::String(name.into_iter().collect())
}

pub fn is_erlang_reserved_word(name: &str) -> bool {
    matches!(
        name,
        "!" | "receive"
            | "bnot"
            | "div"
            | "rem"
            | "band"
            | "bor"
            | "bxor"
            | "bsl"
            | "bsr"
            | "not"
            | "and"
            | "or"
            | "xor"
            | "orelse"
            | "andalso"
            | "when"
            | "end"
            | "fun"
            | "try"
            | "catch"
            | "after"
            | "begin"
            | "let"
            | "query"
            | "cond"
            | "if"
            | "of"
            | "case"
    )
}

// Includes shell_default & user_default which are looked for by the erlang shell
pub fn is_erlang_standard_library_module(name: &str) -> bool {
    matches!(
        name,
        "array"
            | "base64"
            | "beam_lib"
            | "binary"
            | "c"
            | "calendar"
            | "dets"
            | "dict"
            | "digraph"
            | "digraph_utils"
            | "epp"
            | "erl_anno"
            | "erl_eval"
            | "erl_expand_records"
            | "erl_id_trans"
            | "erl_internal"
            | "erl_lint"
            | "erl_parse"
            | "erl_pp"
            | "erl_scan"
            | "erl_tar"
            | "ets"
            | "file_sorter"
            | "filelib"
            | "filename"
            | "gb_sets"
            | "gb_trees"
            | "gen_event"
            | "gen_fsm"
            | "gen_server"
            | "gen_statem"
            | "io"
            | "io_lib"
            | "lists"
            | "log_mf_h"
            | "maps"
            | "math"
            | "ms_transform"
            | "orddict"
            | "ordsets"
            | "pool"
            | "proc_lib"
            | "proplists"
            | "qlc"
            | "queue"
            | "rand"
            | "random"
            | "re"
            | "sets"
            | "shell"
            | "shell_default"
            | "shell_docs"
            | "slave"
            | "sofs"
            | "string"
            | "supervisor"
            | "supervisor_bridge"
            | "sys"
            | "timer"
            | "unicode"
            | "uri_string"
            | "user_default"
            | "win32reg"
            | "zip"
    )
}

// A TypeVar can either be rendered as an actual type variable such as `A` or `B`,
// or it can be rendered as `any()` depending on how many usages it has. If it
// has only 1 usage it is an `any()` type. If it has more than 1 usage it is a
// type variable. This function gathers usages for this determination.
//
//   Examples:
//     fn(a) -> String       // `a` is `any()`
//     fn() -> Result(a, b)  // `a` and `b` are `any()`
//     fn(a) -> a            // `a` is a type var
fn collect_type_var_usages<'a>(
    mut ids: HashMap<u64, u64>,
    types: impl IntoIterator<Item = &'a Arc<Type>>,
) -> HashMap<u64, u64> {
    for typ in types {
        type_var_ids(typ, &mut ids);
    }
    ids
}

fn type_var_ids(type_: &Type, ids: &mut HashMap<u64, u64>) {
    match type_ {
        Type::Var { type_: typ } => match typ.borrow().deref() {
            TypeVar::Generic { id, .. } | TypeVar::Unbound { id, .. } => {
                let count = ids.entry(*id).or_insert(0);
                *count += 1;
            }
            TypeVar::Link { type_: typ } => type_var_ids(typ, ids),
        },
        Type::App { args, .. } => {
            for arg in args {
                type_var_ids(arg, ids)
            }
        }
        Type::Fn { args, retrn } => {
            for arg in args {
                type_var_ids(arg, ids)
            }
            type_var_ids(retrn, ids);
        }
        Type::Tuple { elems } => {
            for elem in elems {
                type_var_ids(elem, ids)
            }
        }
    }
}

fn erl_safe_type_name(mut name: String) -> String {
    if matches!(
        name.as_str(),
        "any"
            | "atom"
            | "none"
            | "pid"
            | "port"
            | "reference"
            | "float"
            | "integer"
            | "list"
            | "nonempty_improper_list"
            | "tuple"
            | "term"
            | "binary"
            | "bitstring"
            | "boolean"
            | "byte"
            | "char"
            | "nil"
            | "number"
            | "maybe_improper_list"
            | "nonempty_list"
            | "string"
            | "nonempty_string"
            | "iodata"
            | "iolist"
            | "map"
            | "function"
            | "module"
            | "mfa"
            | "arity"
            | "identifier"
            | "node"
            | "timeout"
            | "no_return"
            | "non_neg_integer"
            | "pos_integer"
            | "neg_integer"
    ) {
        name.push('_');
        name
    } else {
        escape_atom(name)
    }
}

#[derive(Debug)]
struct TypePrinter<'a> {
    var_as_any: bool,
    current_module: &'a [String],
    var_usages: Option<&'a HashMap<u64, u64>>,
}

impl<'a> TypePrinter<'a> {
    fn new(current_module: &'a [String]) -> Self {
        Self {
            current_module,
            var_usages: None,
            var_as_any: false,
        }
    }

    pub fn with_var_usages(mut self, var_usages: &'a HashMap<u64, u64>) -> Self {
        self.var_usages = Some(var_usages);
        self
    }

    pub fn print(&self, type_: &Type) -> Document<'static> {
        match type_ {
            Type::Var { type_: typ } => self.print_var(&typ.borrow()),

            Type::App {
                name, module, args, ..
            } if module.is_empty() => self.print_prelude_type(name, args),

            Type::App {
                name, module, args, ..
            } => self.print_type_app(module, name, args),

            Type::Fn { args, retrn } => self.print_fn(args, retrn),

            Type::Tuple { elems } => tuple(elems.iter().map(|e| self.print(e))),
        }
    }

    fn print_var(&self, type_: &TypeVar) -> Document<'static> {
        match type_ {
            TypeVar::Generic { .. } | TypeVar::Unbound { .. } if self.var_as_any => {
                "any()".to_doc()
            }
            TypeVar::Generic { id, .. } | TypeVar::Unbound { id, .. } => match &self.var_usages {
                Some(usages) => match usages.get(id) {
                    Some(&0) => nil(),
                    Some(&1) => "any()".to_doc(),
                    _ => id_to_type_var(*id),
                },
                None => id_to_type_var(*id),
            },
            TypeVar::Link { type_: typ } => self.print(typ),
        }
    }

    fn print_prelude_type(&self, name: &str, args: &[Arc<Type>]) -> Document<'static> {
        match name {
            "Nil" => "nil".to_doc(),
            "Int" | "UtfCodepoint" => "integer()".to_doc(),
            "String" => "binary()".to_doc(),
            "Bool" => "boolean()".to_doc(),
            "Float" => "float()".to_doc(),
            "BitString" => "bitstring()".to_doc(),
            "List" => {
                let arg0 = self.print(args.get(0).expect("print_prelude_type list"));
                "list(".to_doc().append(arg0).append(")")
            }
            "Result" => {
                let arg_ok = self.print(args.get(0).expect("print_prelude_type result ok"));
                let arg_err = self.print(args.get(1).expect("print_prelude_type result err"));
                let ok = tuple(["ok".to_doc(), arg_ok]);
                let error = tuple(["error".to_doc(), arg_err]);
                docvec![ok, break_(" |", " | "), error].nest(INDENT).group()
            }
            // Getting here sholud mean we either forgot a built-in type or there is a
            // compiler error
            name => panic!("{} is not a built-in type.", name),
        }
    }

    fn print_type_app(
        &self,
        module: &[String],
        name: &str,
        args: &[Arc<Type>],
    ) -> Document<'static> {
        let args = concat(Itertools::intersperse(
            args.iter().map(|a| self.print(a)),
            ", ".to_doc(),
        ));
        let name = Document::String(erl_safe_type_name(name.to_snake_case()));
        if self.current_module == module {
            docvec![name, "(", args, ")"]
        } else {
            docvec![self.print_module_name(module), ":", name, "(", args, ")"]
        }
    }

    fn print_module_name(&self, module: &[String]) -> Document<'static> {
        concat(Itertools::intersperse(
            module.iter().map(|m| Document::String(m.to_snake_case())),
            "@".to_doc(),
        ))
    }

    fn print_fn(&self, args: &[Arc<Type>], retrn: &Type) -> Document<'static> {
        let args = concat(Itertools::intersperse(
            args.iter().map(|a| self.print(a)),
            ", ".to_doc(),
        ));
        let retrn = self.print(retrn);
        "fun(("
            .to_doc()
            .append(args)
            .append(") -> ")
            .append(retrn)
            .append(")")
    }

    /// Print type vars as `any()`.
    fn var_as_any(mut self) -> Self {
        self.var_as_any = true;
        self
    }
}
