// TODO: Refactor this module to be methods on structs rather than free
// functions with a load of arguments. See the JavaScript code generator and the
// formatter for examples.

mod pattern;
#[cfg(test)]
mod tests;

use crate::build::Target;
use crate::strings::convert_string_escape_chars;
use crate::type_::is_prelude_module;
use crate::{
    ast::{CustomType, Function, Import, ModuleConstant, TypeAlias, *},
    docvec,
    line_numbers::LineNumbers,
    pretty::*,
    type_::{
        ModuleValueConstructor, PatternConstructor, Type, TypeVar, ValueConstructor,
        ValueConstructorVariant,
    },
    Result,
};
use ecow::EcoString;
use heck::ToSnakeCase;
use im::HashSet;
use itertools::Itertools;
use pattern::pattern;
use regex::{Captures, Regex};
use std::sync::OnceLock;
use std::{collections::HashMap, ops::Deref, str::FromStr, sync::Arc};
use vec1::Vec1;

const INDENT: isize = 4;
const MAX_COLUMNS: isize = 80;

fn module_name_to_erlang(module: &str) -> Document<'_> {
    Document::String(module.replace('/', "@"))
}

fn module_name_atom(module: &str) -> Document<'static> {
    atom_string(module.replace('/', "@"))
}

#[derive(Debug, Clone)]
struct Env<'a> {
    module: &'a str,
    function: &'a str,
    line_numbers: &'a LineNumbers,
    current_scope_vars: im::HashMap<String, usize>,
    erl_function_scope_vars: im::HashMap<String, usize>,
}

impl<'env> Env<'env> {
    pub fn new(module: &'env str, function: &'env str, line_numbers: &'env LineNumbers) -> Self {
        let vars: im::HashMap<_, _> = std::iter::once(("_".into(), 0)).collect();
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
                write!(name, "@{n}").expect("pushing number suffix to name");
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
        .definitions
        .iter()
        .filter_map(|s| match s {
            Definition::CustomType(CustomType {
                publicity: Publicity::Public,
                constructors,
                ..
            }) => Some(constructors),
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
    let type_printer = TypePrinter::new("").var_as_any();
    let fields = fields.iter().map(move |(name, type_)| {
        let type_ = type_printer.print(type_);
        docvec!(atom_string((*name).to_string()), " :: ", type_.group())
    });
    let fields = break_("", "")
        .append(join(fields, break_(",", ", ")))
        .nest(INDENT)
        .append(break_("", ""))
        .group();
    docvec!(
        "-record(",
        atom_string(name.to_string()),
        ", {",
        fields,
        "}).",
        line()
    )
    .to_pretty_string(MAX_COLUMNS)
}

pub fn module<'a>(module: &'a TypedModule, line_numbers: &'a LineNumbers) -> Result<String> {
    Ok(module_document(module, line_numbers)?.to_pretty_string(MAX_COLUMNS))
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
        .append(Document::String(module.name.replace("/", "@").to_string()))
        .append(").")
        .append(line());

    // We need to know which private functions are referenced in importable
    // constants so that we can export them anyway in the generated Erlang.
    // This is because otherwise when the constant is used in another module it
    // would result in an error as it tries to reference this private function.
    let overridden_publicity = find_private_functions_referenced_in_importable_constants(module);

    for s in &module.definitions {
        register_imports(
            s,
            &mut exports,
            &mut type_exports,
            &mut type_defs,
            &module.name,
            &overridden_publicity,
        );
    }

    let exports = match (!exports.is_empty(), !type_exports.is_empty()) {
        (false, false) => return Ok(header),
        (true, false) => "-export(["
            .to_doc()
            .append(join(exports, ", ".to_doc()))
            .append("]).")
            .append(lines(2)),

        (true, true) => "-export(["
            .to_doc()
            .append(join(exports, ", ".to_doc()))
            .append("]).")
            .append(line())
            .append("-export_type([")
            .to_doc()
            .append(join(type_exports, ", ".to_doc()))
            .append("]).")
            .append(lines(2)),

        (false, true) => "-export_type(["
            .to_doc()
            .append(join(type_exports, ", ".to_doc()))
            .append("]).")
            .append(lines(2)),
    };

    let type_defs = if type_defs.is_empty() {
        nil()
    } else {
        join(type_defs, lines(2)).append(lines(2))
    };

    let statements = join(
        module
            .definitions
            .iter()
            .flat_map(|s| module_statement(s, &module.name, line_numbers)),
        lines(2),
    );

    Ok(header
        .append("-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).")
        .append(lines(2))
        .append(exports)
        .append(type_defs)
        .append(statements)
        .append(line()))
}

fn register_imports(
    s: &TypedDefinition,
    exports: &mut Vec<Document<'_>>,
    type_exports: &mut Vec<Document<'_>>,
    type_defs: &mut Vec<Document<'_>>,
    module_name: &str,
    overridden_publicity: &HashSet<EcoString>,
) {
    match s {
        Definition::Function(Function {
            publicity,
            name,
            arguments: args,
            implementations,
            ..
        }) if publicity.is_importable() || overridden_publicity.contains(name) => {
            // If the function isn't for this target then don't attempt to export it
            if implementations.supports(Target::Erlang) {
                exports.push(atom_string(name.to_string()).append("/").append(args.len()))
            }
        }

        Definition::CustomType(CustomType {
            name,
            constructors,
            typed_parameters,
            opaque,
            ..
        }) => {
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
                .sorted()
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
            let definition = if constructors.is_empty() {
                let constructors =
                    std::iter::once("any()".to_doc()).chain(phantom_vars_constructor);
                join(constructors, break_(" |", " | "))
            } else {
                let constructors = constructors
                    .iter()
                    .map(|c| {
                        let name = atom_string(c.name.to_snake_case());
                        if c.arguments.is_empty() {
                            name
                        } else {
                            let type_printer = TypePrinter::new(module_name);
                            let args = c.arguments.iter().map(|a| type_printer.print(&a.type_));
                            tuple(std::iter::once(name).chain(args))
                        }
                    })
                    .chain(phantom_vars_constructor);
                join(constructors, break_(" |", " | "))
            }
            .nest(INDENT);
            let type_printer = TypePrinter::new(module_name);
            let params = join(
                typed_parameters.iter().map(|a| type_printer.print(a)),
                ", ".to_doc(),
            );
            let doc = if *opaque { "-opaque " } else { "-type " }
                .to_doc()
                .append(Document::String(erl_safe_type_name(name.to_snake_case())))
                .append("(")
                .append(params)
                .append(") :: ")
                .append(definition)
                .group()
                .append(".");
            type_defs.push(doc);
        }

        Definition::Function(Function { .. })
        | Definition::Import(Import { .. })
        | Definition::TypeAlias(TypeAlias { .. })
        | Definition::ModuleConstant(ModuleConstant { .. }) => (),
    }
}

fn module_statement<'a>(
    statement: &'a TypedDefinition,
    module: &'a str,
    line_numbers: &'a LineNumbers,
) -> Option<Document<'a>> {
    match statement {
        Definition::TypeAlias(TypeAlias { .. })
        | Definition::CustomType(CustomType { .. })
        | Definition::Import(Import { .. })
        | Definition::ModuleConstant(ModuleConstant { .. }) => None,

        Definition::Function(function) => module_function(function, module, line_numbers),
    }
}

fn module_function<'a>(
    function: &'a TypedFunction,
    module: &'a str,
    line_numbers: &'a LineNumbers,
) -> Option<Document<'a>> {
    // Private external functions don't need to render anything, the underlying
    // Erlang implementation is used directly at the call site.
    if function.external_erlang.is_some() && function.publicity.is_private() {
        return None;
    }

    // If the function has no suitable Erlang implementation then there is nothing
    // to generate for it.
    if !function.implementations.supports(Target::Erlang) {
        return None;
    }

    let mut env = Env::new(module, &function.name, line_numbers);
    let var_usages = collect_type_var_usages(
        HashMap::new(),
        std::iter::once(&function.return_type).chain(function.arguments.iter().map(|a| &a.type_)),
    );
    let type_printer = TypePrinter::new(module).with_var_usages(&var_usages);
    let args_spec = function
        .arguments
        .iter()
        .map(|a| type_printer.print(&a.type_));
    let return_spec = type_printer.print(&function.return_type);
    let spec = fun_spec(&function.name, args_spec, return_spec);
    let arguments = fun_args(&function.arguments, &mut env);

    let body = function
        .external_erlang
        .as_ref()
        .map(|(module, function)| docvec![atom(module), ":", atom(function), arguments.clone()])
        .unwrap_or_else(|| statement_sequence(&function.body, &mut env));

    let doc = spec
        .append(atom_string(function.name.to_string()))
        .append(arguments)
        .append(" ->")
        .append(line().append(body).nest(INDENT).group())
        .append(".");
    Some(doc)
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
        .append(join(args, break_(",", ", ")))
        .nest(INDENT)
        .append(break_("", ""))
        .surround("(", ")")
        .group()
}

fn fun_spec<'a>(
    name: &'a str,
    args: impl IntoIterator<Item = Document<'a>>,
    retrn: Document<'a>,
) -> Document<'a> {
    "-spec "
        .to_doc()
        .append(atom(name))
        .append(wrap_args(args))
        .append(" -> ")
        .append(retrn)
        .append(".")
        .append(line())
        .group()
}

fn atom_string(value: String) -> Document<'static> {
    Document::String(escape_atom_string(value))
}

fn atom_pattern() -> &'static Regex {
    static ATOM_PATTERN: OnceLock<Regex> = OnceLock::new();
    ATOM_PATTERN.get_or_init(|| Regex::new(r"^[a-z][a-z0-9_@]*$").expect("atom RE regex"))
}

fn atom(value: &str) -> Document<'_> {
    if is_erlang_reserved_word(value) {
        // Escape because of keyword collision
        Document::String(format!("'{value}'"))
    } else if atom_pattern().is_match(value) {
        // No need to escape
        Document::Str(value)
    } else {
        // Escape because of characters contained
        Document::String(format!("'{value}'"))
    }
}

fn escape_atom_string(value: String) -> String {
    if is_erlang_reserved_word(&value) {
        // Escape because of keyword collision
        format!("'{value}'")
    } else if atom_pattern().is_match(&value) {
        // No need to escape
        value
    } else {
        // Escape because of characters contained
        format!("'{value}'")
    }
}

fn unicode_escape_sequence_pattern() -> &'static Regex {
    static PATTERN: OnceLock<Regex> = OnceLock::new();
    PATTERN.get_or_init(|| {
        Regex::new(r#"(\\+)(u)"#).expect("Unicode escape sequence regex cannot be constructed")
    })
}

fn string_inner(value: &str) -> Document<'_> {
    let content = unicode_escape_sequence_pattern()
        // `\\u`-s should not be affected, so that "\\u..." is not converted to
        // "\\x...". That's why capturing groups is used to exclude cases that
        // shouldn't be replaced.
        .replace_all(value, |caps: &Captures<'_>| {
            let slashes = caps.get(1).map_or("", |m| m.as_str());

            if slashes.len() % 2 == 0 {
                format!("{slashes}u")
            } else {
                format!("{slashes}x")
            }
        })
        .to_string();
    Document::String(content)
}

fn string(value: &str) -> Document<'_> {
    string_inner(value).surround("<<\"", "\"/utf8>>")
}

fn string_length_utf8_bytes(str: &EcoString) -> usize {
    convert_string_escape_chars(str).len()
}

fn tuple<'a>(elems: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    join(elems, break_(",", ", "))
        .nest(INDENT)
        .surround("{", "}")
        .group()
}

fn string_concatenate<'a>(
    left: &'a TypedExpr,
    right: &'a TypedExpr,
    env: &mut Env<'a>,
) -> Document<'a> {
    let left = string_concatenate_argument(left, env);
    let right = string_concatenate_argument(right, env);
    bit_array([left, right])
}

fn string_concatenate_argument<'a>(value: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    match value {
        TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant:
                        ValueConstructorVariant::ModuleConstant {
                            literal: Constant::String { value, .. },
                            ..
                        },
                    ..
                },
            ..
        }
        | TypedExpr::String { value, .. } => docvec!['"', string_inner(value), "\"/utf8"],

        TypedExpr::Var {
            name,
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::LocalVariable { .. },
                    ..
                },
            ..
        } => docvec![env.local_var_name(name), "/binary"],

        TypedExpr::BinOp {
            name: BinOp::Concatenate,
            ..
        } => docvec![expr(value, env), "/binary"],

        _ => docvec!["(", maybe_block_expr(value, env), ")/binary"],
    }
}

fn bit_array<'a>(elems: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    join(elems, break_(",", ", "))
        .nest(INDENT)
        .surround("<<", ">>")
        .group()
}

fn const_segment<'a>(
    value: &'a TypedConstant,
    options: &'a [BitArrayOption<TypedConstant>],
    env: &mut Env<'a>,
) -> Document<'a> {
    let mut print_value = |value: &'a TypedConstant| const_inline(value, env);

    let doc_value = |print_value: &mut dyn FnMut(&'a TypedConstant) -> Document<'a>| {
        match value {
            // Skip the normal <<value/utf8>> surrounds
            Constant::String { value, .. } => value.to_doc().surround("\"", "\""),

            // As normal
            Constant::Int { .. } | Constant::Float { .. } | Constant::BitArray { .. } => {
                print_value(value)
            }

            // Wrap anything else in parentheses
            value => print_value(value).surround("(", ")"),
        }
    };

    let size = |value: &'a TypedConstant,
                print_value: &mut dyn FnMut(&'a TypedConstant) -> Document<'a>| {
        match value {
            Constant::Int { .. } => Some(":".to_doc().append(print_value(value))),
            _ => Some(":".to_doc().append(print_value(value).surround("(", ")"))),
        }
    };

    let unit = |value: &'a u8| Some(Document::String(format!("unit:{value}")));

    bit_array_segment(doc_value, options, size, unit, true, &mut print_value)
}

fn statement<'a>(statement: &'a TypedStatement, env: &mut Env<'a>) -> Document<'a> {
    match statement {
        Statement::Expression(e) => expr(e, env),
        Statement::Assignment(a) => assignment(a, env),
        Statement::Use(_) => {
            unreachable!("Use statements must not be present for Erlang generation")
        }
    }
}

fn expr_segment<'a>(
    value: &'a TypedExpr,
    options: &'a [BitArrayOption<TypedExpr>],
    env: &mut Env<'a>,
) -> Document<'a> {
    let value_is_a_string_literal = matches!(value, TypedExpr::String { .. });
    let mut print_expr = |value: &'a TypedExpr| expr(value, env);

    let doc_value = |print_expr: &mut dyn FnMut(&'a TypedExpr) -> Document<'a>| {
        match value {
            // Skip the normal <<value/utf8>> surrounds and set the string literal flag
            TypedExpr::String { value, .. } => string_inner(value).surround("\"", "\""),

            // As normal
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::BitArray { .. } => print_expr(value),

            // Wrap anything else in parentheses
            value => print_expr(value).surround("(", ")"),
        }
    };

    let size = |expression: &'a TypedExpr,
                print_expr: &mut dyn FnMut(&'a TypedExpr) -> Document<'a>| {
        match expression {
            TypedExpr::Int { value, .. } => {
                let v = value.replace("_", "");
                let v = u64::from_str(&v).unwrap_or(0);
                Some(Document::String(format!(":{v}")))
            }

            _ => {
                let inner_expr = print_expr(expression).surround("(", ")");
                // The value of size must be a non-negative integer, we use lists:max here to ensure
                // it is at least 0;
                let value_guard = ":(lists:max(["
                    .to_doc()
                    .append(inner_expr)
                    .append(", 0]))")
                    .group();
                Some(value_guard)
            }
        }
    };

    let unit = |value: &'a u8| Some(Document::String(format!("unit:{value}")));

    bit_array_segment(
        doc_value,
        options,
        size,
        unit,
        value_is_a_string_literal,
        &mut print_expr,
    )
}

fn bit_array_segment<'a, Value: 'a, ValueType, ValueToDoc, SizeToDoc, UnitToDoc>(
    mut value_to_doc: ValueToDoc,
    options: &'a [BitArrayOption<Value>],
    mut size_to_doc: SizeToDoc,
    mut unit_to_doc: UnitToDoc,
    value_is_a_string_literal: bool,
    print_value: &mut dyn FnMut(&'a ValueType) -> Document<'a>,
) -> Document<'a>
where
    ValueToDoc: FnMut(&mut dyn FnMut(&'a ValueType) -> Document<'a>) -> Document<'a>,
    SizeToDoc:
        FnMut(&'a Value, &mut dyn FnMut(&'a ValueType) -> Document<'a>) -> Option<Document<'a>>,
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
        use BitArrayOption as Opt;
        if !others.is_empty() && !matches!(option, Opt::Size { .. } | Opt::Unit { .. }) {
            others.push("-".to_doc());
        }
        match option {
            Opt::Utf8 { .. } => others.push(override_type.unwrap_or("utf8").to_doc()),
            Opt::Utf16 { .. } => others.push(override_type.unwrap_or("utf16").to_doc()),
            Opt::Utf32 { .. } => others.push(override_type.unwrap_or("utf32").to_doc()),
            Opt::Int { .. } => others.push("integer".to_doc()),
            Opt::Float { .. } => others.push("float".to_doc()),
            Opt::Bytes { .. } => others.push("binary".to_doc()),
            Opt::Bits { .. } => others.push("bitstring".to_doc()),
            Opt::Utf8Codepoint { .. } => others.push("utf8".to_doc()),
            Opt::Utf16Codepoint { .. } => others.push("utf16".to_doc()),
            Opt::Utf32Codepoint { .. } => others.push("utf32".to_doc()),
            Opt::Signed { .. } => others.push("signed".to_doc()),
            Opt::Unsigned { .. } => others.push("unsigned".to_doc()),
            Opt::Big { .. } => others.push("big".to_doc()),
            Opt::Little { .. } => others.push("little".to_doc()),
            Opt::Native { .. } => others.push("native".to_doc()),
            Opt::Size { value, .. } => size = size_to_doc(value, print_value),
            Opt::Unit { value, .. } => unit = unit_to_doc(value),
        }
    }

    let mut document = value_to_doc(print_value);
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

fn block<'a>(statements: &'a Vec1<TypedStatement>, env: &mut Env<'a>) -> Document<'a> {
    if statements.len() == 1 && statements.first().is_non_pipe_expression() {
        return docvec!['(', statement(statements.first(), env), ')'];
    }

    let vars = env.current_scope_vars.clone();
    let document = statement_sequence(statements, env);
    env.current_scope_vars = vars;

    begin_end(document)
}

fn statement_sequence<'a>(statements: &'a [TypedStatement], env: &mut Env<'a>) -> Document<'a> {
    let count = statements.len();
    let mut documents = Vec::with_capacity(count * 3);
    for (i, expression) in statements.iter().enumerate() {
        documents.push(statement(expression, env).group());

        if i + 1 < count {
            // This isn't the final expression so add the delimeters
            documents.push(",".to_doc());
            documents.push(line());
        }
    }
    if count == 1 {
        documents.to_doc()
    } else {
        documents.to_doc().force_break()
    }
}

fn float_div<'a>(left: &'a TypedExpr, right: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    if right.non_zero_compile_time_number() {
        return binop_exprs(left, "/", right, env);
    }

    let left = expr(left, env);
    let right = expr(right, env);
    let denominator = env.next_local_var_name("gleam@denominator");
    let clauses = docvec![
        line(),
        "+0.0 -> +0.0;",
        line(),
        "-0.0 -> -0.0;",
        line(),
        denominator.clone(),
        " -> ",
        binop_documents(left, "/", denominator)
    ];
    docvec!["case ", right, " of", clauses.nest(INDENT), line(), "end"]
}

fn int_div<'a>(
    left: &'a TypedExpr,
    right: &'a TypedExpr,
    op: &'static str,
    env: &mut Env<'a>,
) -> Document<'a> {
    if right.non_zero_compile_time_number() {
        return binop_exprs(left, op, right, env);
    }

    let left = expr(left, env);
    let right = expr(right, env);
    let denominator = env.next_local_var_name("gleam@denominator");
    let clauses = docvec![
        line(),
        "0 -> 0;",
        line(),
        denominator.clone(),
        " -> ",
        binop_documents(left, op, denominator)
    ];
    docvec!["case ", right, " of", clauses.nest(INDENT), line(), "end"]
}

fn bin_op<'a>(
    name: &'a BinOp,
    left: &'a TypedExpr,
    right: &'a TypedExpr,
    env: &mut Env<'a>,
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
        BinOp::DivFloat => return float_div(left, right, env),
        BinOp::DivInt => return int_div(left, right, "div", env),
        BinOp::RemainderInt => return int_div(left, right, "rem", env),
        BinOp::Concatenate => return string_concatenate(left, right, env),
    };

    binop_exprs(left, op, right, env)
}

fn binop_exprs<'a>(
    left: &'a TypedExpr,
    op: &'static str,
    right: &'a TypedExpr,
    env: &mut Env<'a>,
) -> Document<'a> {
    let left = match left {
        TypedExpr::BinOp { .. } => expr(left, env).surround("(", ")"),
        _ => maybe_block_expr(left, env),
    };
    let right = match right {
        TypedExpr::BinOp { .. } => expr(right, env).surround("(", ")"),
        _ => maybe_block_expr(right, env),
    };
    binop_documents(left, op, right)
}

fn binop_documents<'a>(left: Document<'a>, op: &'static str, right: Document<'a>) -> Document<'a> {
    left.append(break_("", " "))
        .append(op)
        .group()
        .append(" ")
        .append(right)
}

fn let_assert<'a>(value: &'a TypedExpr, pat: &'a TypedPattern, env: &mut Env<'a>) -> Document<'a> {
    let mut vars: Vec<&str> = vec![];
    let body = maybe_block_expr(value, env);
    let (subject_var, subject_definition) = if value.is_var() {
        (body, docvec![])
    } else {
        let var = env.next_local_var_name(ASSERT_SUBJECT_VARIABLE);
        let definition = docvec![var.clone(), " = ", body, ",", line()];
        (var, definition)
    };

    let mut guards = vec![];
    let check_pattern = pattern::to_doc_discarding_all(pat, &mut vars, env, &mut guards);
    let clause_guard = optional_clause_guard(None, guards, env);

    // We don't take the guards from the assign pattern or we would end up with
    // all the same guards repeated twice!
    let assign_pattern = pattern::to_doc(pat, &mut vars, env, &mut vec![]);
    let clauses = docvec![
        check_pattern.clone(),
        clause_guard,
        " -> ",
        subject_var.clone(),
        ";",
        line(),
        env.next_local_var_name(ASSERT_FAIL_VARIABLE),
        " ->",
        docvec![
            line(),
            erlang_error(
                "let_assert",
                &string("Assertion pattern match failed"),
                pat.location(),
                vec![("value", env.local_var_name(ASSERT_FAIL_VARIABLE))],
                env,
            )
            .nest(INDENT)
        ]
        .nest(INDENT)
    ];
    docvec![
        subject_definition,
        assign_pattern,
        " = case ",
        subject_var,
        " of",
        docvec![line(), clauses].nest(INDENT),
        line(),
        "end",
    ]
}

fn let_<'a>(value: &'a TypedExpr, pat: &'a TypedPattern, env: &mut Env<'a>) -> Document<'a> {
    let body = maybe_block_expr(value, env).group();
    let mut guards = vec![];
    pattern(pat, env, &mut guards).append(" = ").append(body)
}

fn float<'a>(value: &str) -> Document<'a> {
    let mut value = value.replace('_', "");
    if value.ends_with('.') {
        value.push('0')
    }

    match value.split('.').collect_vec().as_slice() {
        ["0", "0"] => "+0.0".to_doc(),
        [before_dot, after_dot] if after_dot.starts_with('e') => {
            Document::String(format!("{before_dot}.0{after_dot}"))
        }
        _ => Document::String(value),
    }
}

fn expr_list<'a>(
    elements: &'a [TypedExpr],
    tail: &'a Option<Box<TypedExpr>>,
    env: &mut Env<'a>,
) -> Document<'a> {
    let elements = join(
        elements.iter().map(|e| maybe_block_expr(e, env)),
        break_(",", ", "),
    );
    list(elements, tail.as_ref().map(|e| maybe_block_expr(e, env)))
}

fn list<'a>(elems: Document<'a>, tail: Option<Document<'a>>) -> Document<'a> {
    let elems = match tail {
        Some(tail) if elems.is_empty() => return tail.to_doc(),

        Some(tail) => elems.append(break_(" |", " | ")).append(tail),

        None => elems,
    };

    elems.to_doc().nest(INDENT).surround("[", "]").group()
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
                    .append(atom_string(record_name.to_snake_case()))
                    .append(", ")
                    .append(Document::String(chars))
                    .append("} end")
            }
            _ => atom_string(record_name.to_snake_case()),
        },

        ValueConstructorVariant::LocalVariable { .. } => env.local_var_name(name),

        ValueConstructorVariant::ModuleConstant { literal, .. }
        | ValueConstructorVariant::LocalConstant { literal } => const_inline(literal, env),

        ValueConstructorVariant::ModuleFn {
            arity, ref module, ..
        } if module == env.module => "fun "
            .to_doc()
            .append(atom(name))
            .append("/")
            .append(*arity),

        ValueConstructorVariant::ModuleFn {
            arity,
            module,
            name,
            ..
        } => "fun "
            .to_doc()
            .append(module_name_atom(module))
            .append(":")
            .append(atom(name))
            .append("/")
            .append(*arity),
    }
}

fn int<'a>(value: &str) -> Document<'a> {
    let mut value = value.replace('_', "");
    if value.starts_with("0x") {
        value.replace_range(..2, "16#");
    } else if value.starts_with("0o") {
        value.replace_range(..2, "8#");
    } else if value.starts_with("0b") {
        value.replace_range(..2, "2#");
    }

    Document::String(value)
}

fn const_inline<'a>(literal: &'a TypedConstant, env: &mut Env<'a>) -> Document<'a> {
    match literal {
        Constant::Int { value, .. } => int(value),
        Constant::Float { value, .. } => float(value),
        Constant::String { value, .. } => string(value),
        Constant::Tuple { elements, .. } => tuple(elements.iter().map(|e| const_inline(e, env))),

        Constant::List { elements, .. } => join(
            elements.iter().map(|e| const_inline(e, env)),
            break_(",", ", "),
        )
        .nest(INDENT)
        .surround("[", "]")
        .group(),

        Constant::BitArray { segments, .. } => bit_array(
            segments
                .iter()
                .map(|s| const_segment(&s.value, &s.options, env)),
        ),

        Constant::Record { tag, typ, args, .. } if args.is_empty() => match typ.deref() {
            Type::Fn { args, .. } => record_constructor_function(tag, args.len()),
            _ => atom_string(tag.to_snake_case()),
        },

        Constant::Record { tag, args, .. } => {
            let args = args.iter().map(|a| const_inline(&a.value, env));
            let tag = atom_string(tag.to_snake_case());
            tuple(std::iter::once(tag).chain(args))
        }

        Constant::Var {
            name, constructor, ..
        } => var(
            name,
            constructor
                .as_ref()
                .expect("This is guaranteed to hold a value."),
            env,
        ),

        Constant::Invalid { .. } => panic!("invalid constants should not reach code generation"),
    }
}

fn record_constructor_function(tag: &EcoString, arity: usize) -> Document<'_> {
    let chars = incrementing_args_list(arity);
    "fun("
        .to_doc()
        .append(Document::String(chars.clone()))
        .append(") -> {")
        .append(atom_string(tag.to_snake_case()))
        .append(", ")
        .append(Document::String(chars))
        .append("} end")
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
    // Simply rendering the duplicate erlang clauses breaks the variable
    // rewriting because each pattern would define different (rewritten)
    // variables names.
    let mut then_doc = None;
    let initial_erlang_vars = env.erl_function_scope_vars.clone();
    let mut end_erlang_vars = im::HashMap::new();

    let doc = join(
        std::iter::once(pat)
            .chain(alternative_patterns)
            .map(|patterns| {
                let mut additional_guards = vec![];
                env.erl_function_scope_vars = initial_erlang_vars.clone();

                let patterns_doc = if patterns.len() == 1 {
                    let p = patterns.first().expect("Single pattern clause printing");
                    pattern(p, env, &mut additional_guards)
                } else {
                    tuple(
                        patterns
                            .iter()
                            .map(|p| pattern(p, env, &mut additional_guards)),
                    )
                };

                let guard = optional_clause_guard(guard.as_ref(), additional_guards, env);
                if then_doc.is_none() {
                    then_doc = Some(clause_consequence(then, env));
                    end_erlang_vars = env.erl_function_scope_vars.clone();
                }

                patterns_doc.append(
                    guard
                        .append(" ->")
                        .append(line().append(then_doc.clone()).nest(INDENT).group()),
                )
            }),
        ";".to_doc().append(lines(2)),
    );

    env.erl_function_scope_vars = end_erlang_vars;
    doc
}

fn clause_consequence<'a>(consequence: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    match consequence {
        TypedExpr::Block { statements, .. } => statement_sequence(statements, env),
        _ => expr(consequence, env),
    }
}

fn optional_clause_guard<'a>(
    guard: Option<&'a TypedClauseGuard>,
    additional_guards: Vec<Document<'a>>,
    env: &mut Env<'a>,
) -> Document<'a> {
    let guard_doc = guard.map(|guard| bare_clause_guard(guard, env));

    let guards_count = guard_doc.iter().len() + additional_guards.len();
    let guards_docs = additional_guards.into_iter().chain(guard_doc).map(|guard| {
        if guards_count > 1 {
            guard.surround("(", ")")
        } else {
            guard
        }
    });
    let doc = join(guards_docs, " andalso ".to_doc());
    if doc.is_empty() {
        doc
    } else {
        " when ".to_doc().append(doc)
    }
}

fn bare_clause_guard<'a>(guard: &'a TypedClauseGuard, env: &mut Env<'a>) -> Document<'a> {
    match guard {
        ClauseGuard::Not { expression, .. } => docvec!["not ", bare_clause_guard(expression, env)],

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

        ClauseGuard::AddInt { left, right, .. } => clause_guard(left, env)
            .append(" + ")
            .append(clause_guard(right, env)),

        ClauseGuard::AddFloat { left, right, .. } => clause_guard(left, env)
            .append(" + ")
            .append(clause_guard(right, env)),

        ClauseGuard::SubInt { left, right, .. } => clause_guard(left, env)
            .append(" - ")
            .append(clause_guard(right, env)),

        ClauseGuard::SubFloat { left, right, .. } => clause_guard(left, env)
            .append(" - ")
            .append(clause_guard(right, env)),

        ClauseGuard::MultInt { left, right, .. } => clause_guard(left, env)
            .append(" * ")
            .append(clause_guard(right, env)),

        ClauseGuard::MultFloat { left, right, .. } => clause_guard(left, env)
            .append(" * ")
            .append(clause_guard(right, env)),

        ClauseGuard::DivInt { left, right, .. } => clause_guard(left, env)
            .append(" div ")
            .append(clause_guard(right, env)),

        ClauseGuard::DivFloat { left, right, .. } => clause_guard(left, env)
            .append(" / ")
            .append(clause_guard(right, env)),

        ClauseGuard::RemainderInt { left, right, .. } => clause_guard(left, env)
            .append(" rem ")
            .append(clause_guard(right, env)),

        // Only local variables are supported and the typer ensures that all
        // ClauseGuard::Vars are local variables
        ClauseGuard::Var { name, .. } => env.local_var_name(name),

        ClauseGuard::TupleIndex { tuple, index, .. } => tuple_index_inline(tuple, *index, env),

        ClauseGuard::FieldAccess {
            container, index, ..
        } => tuple_index_inline(container, index.expect("Unable to find index") + 1, env),

        ClauseGuard::ModuleSelect { literal, .. } => const_inline(literal, env),

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
        // Binary operators are wrapped in parens
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
        | ClauseGuard::LtEqFloat { .. }
        | ClauseGuard::AddInt { .. }
        | ClauseGuard::AddFloat { .. }
        | ClauseGuard::SubInt { .. }
        | ClauseGuard::SubFloat { .. }
        | ClauseGuard::MultInt { .. }
        | ClauseGuard::MultFloat { .. }
        | ClauseGuard::DivInt { .. }
        | ClauseGuard::DivFloat { .. }
        | ClauseGuard::RemainderInt { .. } => "("
            .to_doc()
            .append(bare_clause_guard(guard, env))
            .append(")"),

        // Other expressions are not
        ClauseGuard::Constant(_)
        | ClauseGuard::Not { .. }
        | ClauseGuard::Var { .. }
        | ClauseGuard::TupleIndex { .. }
        | ClauseGuard::FieldAccess { .. }
        | ClauseGuard::ModuleSelect { .. } => bare_clause_guard(guard, env),
    }
}

fn clauses<'a>(cs: &'a [TypedClause], env: &mut Env<'a>) -> Document<'a> {
    join(
        cs.iter().map(|c| {
            let vars = env.current_scope_vars.clone();
            let erl = clause(c, env);
            env.current_scope_vars = vars; // Reset the known variables now the clauses' scope has ended
            erl
        }),
        ";".to_doc().append(lines(2)),
    )
}

fn case<'a>(subjects: &'a [TypedExpr], cs: &'a [TypedClause], env: &mut Env<'a>) -> Document<'a> {
    let subjects_doc = if subjects.len() == 1 {
        let subject = subjects
            .first()
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

fn module_fn_with_args<'a>(
    module: &'a str,
    name: &'a str,
    args: Vec<Document<'a>>,
    env: &Env<'a>,
) -> Document<'a> {
    let args = wrap_args(args);
    if module == env.module {
        atom(name).append(args)
    } else {
        atom_string(module.replace('/', "@"))
            .append(":")
            .append(atom(name))
            .append(args)
    }
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
        } => tuple(std::iter::once(atom_string(name.to_snake_case())).chain(args)),

        TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::ModuleFn { module, name, .. },
                    ..
                },
            ..
        } => module_fn_with_args(module, name, args, env),

        // Match against a Constant::Var that contains a function.
        // We want this to be emitted like a normal function call, not a function variable
        // substitution.
        TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant:
                        ValueConstructorVariant::ModuleConstant {
                            literal:
                                Constant::Var {
                                    constructor: Some(ref constructor),
                                    ..
                                },
                            ..
                        },
                    ..
                },
            ..
        } if constructor.variant.is_module_fn() => {
            if let ValueConstructorVariant::ModuleFn { module, name, .. } = &constructor.variant {
                module_fn_with_args(module, name, args, env)
            } else {
                unreachable!("The above clause guard ensures that this is a module fn")
            }
        }

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Fn { module, name, .. },
            ..
        } => {
            let args = wrap_args(args);
            // We use the constructor Fn variant's `module` and function `name`.
            // It would also be valid to use the module and label as in the
            // Gleam code, but using the variant can result in an optimisation
            // in which the target function is used for `external fn`s, removing
            // one layer of wrapping.
            // This also enables an optimisation in the Erlang compiler in which
            // some Erlang BIFs can be replaced with literals if their arguments
            // are literals, such as `binary_to_atom`.
            atom_string(module.replace("/", "@").to_string())
                .append(":")
                .append(atom_string(name.to_string()))
                .append(args)
        }

        TypedExpr::Fn {
            is_capture: true,
            body,
            ..
        } => {
            if let Statement::Expression(TypedExpr::Call {
                fun,
                args: inner_args,
                ..
            }) = body.first()
            {
                let mut merged_args = Vec::with_capacity(inner_args.len());
                for arg in inner_args {
                    match &arg.value {
                        TypedExpr::Var { name, .. } if name == CAPTURE_VARIABLE => {
                            merged_args.push(args.swap_remove(0))
                        }
                        e => merged_args.push(maybe_block_expr(e, env)),
                    }
                }
                docs_args_call(fun, merged_args, env)
            } else {
                panic!("Erl printing: Capture was not a call")
            }
        }

        TypedExpr::Fn { .. }
        | TypedExpr::Call { .. }
        | TypedExpr::Todo { .. }
        | TypedExpr::Panic { .. }
        | TypedExpr::RecordAccess { .. }
        | TypedExpr::TupleIndex { .. } => {
            let args = wrap_args(args);
            expr(fun, env).surround("(", ")").append(args)
        }

        other => {
            let args = wrap_args(args);
            maybe_block_expr(other, env).append(args)
        }
    }
}

fn record_update<'a>(
    spread: &'a TypedExpr,
    args: &'a [TypedRecordUpdateArg],
    env: &mut Env<'a>,
) -> Document<'a> {
    let expr_doc = maybe_block_expr(spread, env);

    args.iter().fold(expr_doc, |tuple_doc, arg| {
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
    docvec!["begin", line().append(document).nest(INDENT), line(), "end"].force_break()
}

fn maybe_block_expr<'a>(expression: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    if needs_begin_end_wrapping(expression) {
        begin_end(expr(expression, env))
    } else {
        expr(expression, env)
    }
}

fn needs_begin_end_wrapping(expression: &TypedExpr) -> bool {
    match expression {
        TypedExpr::Pipeline { .. } => true,

        TypedExpr::Int { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::String { .. }
        | TypedExpr::Var { .. }
        | TypedExpr::Fn { .. }
        | TypedExpr::List { .. }
        | TypedExpr::Call { .. }
        | TypedExpr::BinOp { .. }
        | TypedExpr::Case { .. }
        | TypedExpr::RecordAccess { .. }
        | TypedExpr::Block { .. }
        | TypedExpr::ModuleSelect { .. }
        | TypedExpr::Tuple { .. }
        | TypedExpr::TupleIndex { .. }
        | TypedExpr::Todo { .. }
        | TypedExpr::Panic { .. }
        | TypedExpr::BitArray { .. }
        | TypedExpr::RecordUpdate { .. }
        | TypedExpr::NegateBool { .. }
        | TypedExpr::NegateInt { .. }
        | TypedExpr::Invalid { .. } => false,
    }
}

fn todo<'a>(message: Option<&'a TypedExpr>, location: SrcSpan, env: &mut Env<'a>) -> Document<'a> {
    let message = match message {
        Some(m) => expr(m, env),
        None => string("This has not yet been implemented"),
    };
    erlang_error("todo", &message, location, vec![], env)
}

fn panic<'a>(location: SrcSpan, message: Option<&'a TypedExpr>, env: &mut Env<'a>) -> Document<'a> {
    let message = match message {
        Some(m) => expr(m, env),
        None => string("panic expression evaluated"),
    };
    erlang_error("panic", &message, location, vec![], env)
}

fn erlang_error<'a>(
    name: &'a str,
    message: &Document<'a>,
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
        message.clone()
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
        .append(env.module.to_doc().surround("<<\"", "\"/utf8>>"))
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
        .append(fields_doc.group().nest(INDENT))
        .append("}");
    docvec!["erlang:error", wrap_args([error.group()])]
}

fn expr<'a>(expression: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    match expression {
        TypedExpr::Todo {
            message: label,
            location,
            ..
        } => todo(label.as_deref(), *location, env),

        TypedExpr::Panic {
            location, message, ..
        } => panic(*location, message.as_deref(), env),

        TypedExpr::Int { value, .. } => int(value),
        TypedExpr::Float { value, .. } => float(value),
        TypedExpr::String { value, .. } => string(value),

        TypedExpr::Pipeline {
            assignments,
            finally,
            ..
        } => pipeline(assignments, finally, env),

        TypedExpr::Block { statements, .. } => block(statements, env),

        TypedExpr::TupleIndex { tuple, index, .. } => tuple_index(tuple, *index, env),

        TypedExpr::Var {
            name, constructor, ..
        } => var(name, constructor, env),

        TypedExpr::Fn { args, body, .. } => fun(args, body, env),

        TypedExpr::NegateBool { value, .. } => negate_with("not ", value, env),

        TypedExpr::NegateInt { value, .. } => negate_with("- ", value, env),

        TypedExpr::List { elements, tail, .. } => expr_list(elements, tail, env),

        TypedExpr::Call { fun, args, .. } => call(fun, args, env),

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, arity: 0, .. },
            ..
        } => atom_string(name.to_snake_case()),

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Constant { literal, .. },
            ..
        } => const_inline(literal, env),

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, arity, .. },
            ..
        } => record_constructor_function(name, *arity as usize),

        TypedExpr::ModuleSelect {
            typ,
            constructor: ModuleValueConstructor::Fn { module, name, .. },
            ..
        } => module_select_fn(typ.clone(), module, name),

        TypedExpr::RecordAccess { record, index, .. } => tuple_index(record, index + 1, env),

        TypedExpr::RecordUpdate { spread, args, .. } => record_update(spread, args, env),

        TypedExpr::Case {
            subjects, clauses, ..
        } => case(subjects, clauses, env),

        TypedExpr::BinOp {
            name, left, right, ..
        } => bin_op(name, left, right, env),

        TypedExpr::Tuple { elems, .. } => tuple(elems.iter().map(|e| maybe_block_expr(e, env))),

        TypedExpr::BitArray { segments, .. } => bit_array(
            segments
                .iter()
                .map(|s| expr_segment(&s.value, &s.options, env)),
        ),

        TypedExpr::Invalid { .. } => panic!("invalid expressions should not reach code generation"),
    }
}

fn pipeline<'a>(
    assignments: &'a [Assignment<Arc<Type>, TypedExpr>],
    finally: &'a TypedExpr,
    env: &mut Env<'a>,
) -> Document<'a> {
    let mut documents = Vec::with_capacity((assignments.len() + 1) * 3);

    for a in assignments {
        documents.push(assignment(a, env));
        documents.push(','.to_doc());
        documents.push(line());
    }

    documents.push(expr(finally, env));
    documents.to_doc()
}

fn assignment<'a>(assignment: &'a TypedAssignment, env: &mut Env<'a>) -> Document<'a> {
    match assignment.kind {
        AssignmentKind::Let => let_(&assignment.value, &assignment.pattern, env),
        AssignmentKind::Assert { .. } => let_assert(&assignment.value, &assignment.pattern, env),
    }
}

fn negate_with<'a>(op: &'static str, value: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    docvec![op, maybe_block_expr(value, env)]
}

fn tuple_index<'a>(tuple: &'a TypedExpr, index: u64, env: &mut Env<'a>) -> Document<'a> {
    let index_doc = Document::String(format!("{}", (index + 1)));
    let tuple_doc = maybe_block_expr(tuple, env);
    "erlang:element"
        .to_doc()
        .append(wrap_args([index_doc, tuple_doc]))
}

fn module_select_fn<'a>(typ: Arc<Type>, module_name: &'a str, label: &'a str) -> Document<'a> {
    match crate::type_::collapse_links(typ).as_ref() {
        Type::Fn { args, .. } => "fun "
            .to_doc()
            .append(module_name_to_erlang(module_name))
            .append(":")
            .append(atom(label))
            .append("/")
            .append(args.len()),

        _ => module_name_to_erlang(module_name)
            .append(":")
            .append(atom(label))
            .append("()"),
    }
}

fn fun<'a>(args: &'a [TypedArg], body: &'a [TypedStatement], env: &mut Env<'a>) -> Document<'a> {
    let current_scope_vars = env.current_scope_vars.clone();
    let doc = "fun"
        .to_doc()
        .append(fun_args(args, env).append(" ->"))
        .append(
            break_("", " ")
                .append(statement_sequence(body, env))
                .nest(INDENT),
        )
        .append(break_("", " "))
        .append("end")
        .group();
    env.current_scope_vars = current_scope_vars;
    doc
}

fn incrementing_args_list(arity: usize) -> String {
    let arguments = (0..arity).map(|c| format!("Field@{c}"));
    Itertools::intersperse(arguments, ", ".into()).collect()
}

fn variable_name(name: &str) -> String {
    let mut chars = name.chars();
    let first_char = chars.next();
    let first_uppercased = first_char.into_iter().flat_map(char::to_uppercase);

    first_uppercased.chain(chars).collect()
}

/// When rendering a type variable to an erlang type spec we need all type variables with the
/// same id to end up with the same name in the generated erlang.
/// This function converts a usize into base 26 A-Z for this purpose.
fn id_to_type_var(id: u64) -> Document<'static> {
    if id < 26 {
        let mut name = "".to_string();
        name.push(char::from_u32((id % 26 + 65) as u32).expect("id_to_type_var 0"));
        return Document::String(name);
    }
    let mut name = vec![];
    let mut last_char = id;
    while last_char >= 26 {
        name.push(char::from_u32((last_char % 26 + 65) as u32).expect("id_to_type_var 1"));
        last_char /= 26;
    }
    name.push(char::from_u32((last_char % 26 + 64) as u32).expect("id_to_type_var 2"));
    name.reverse();
    name.into_iter().collect::<EcoString>().to_doc()
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
            | "maybe"
            | "else"
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
        Type::Named { args, .. } => {
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
            | "arity"
            | "atom"
            | "binary"
            | "bitstring"
            | "boolean"
            | "byte"
            | "char"
            | "dynamic"
            | "float"
            | "function"
            | "identifier"
            | "integer"
            | "iodata"
            | "iolist"
            | "list"
            | "map"
            | "maybe_improper_list"
            | "mfa"
            | "module"
            | "neg_integer"
            | "nil"
            | "no_return"
            | "node"
            | "non_neg_integer"
            | "none"
            | "nonempty_improper_list"
            | "nonempty_list"
            | "nonempty_string"
            | "number"
            | "pid"
            | "port"
            | "pos_integer"
            | "reference"
            | "string"
            | "term"
            | "timeout"
            | "tuple"
    ) {
        name.push('_');
        name
    } else {
        escape_atom_string(name)
    }
}

#[derive(Debug)]
struct TypePrinter<'a> {
    var_as_any: bool,
    current_module: &'a str,
    var_usages: Option<&'a HashMap<u64, u64>>,
}

impl<'a> TypePrinter<'a> {
    fn new(current_module: &'a str) -> Self {
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

            Type::Named {
                name, module, args, ..
            } if is_prelude_module(module) => self.print_prelude_type(name, args),

            Type::Named {
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
            "BitArray" => "bitstring()".to_doc(),
            "List" => {
                let arg0 = self.print(args.first().expect("print_prelude_type list"));
                "list(".to_doc().append(arg0).append(")")
            }
            "Result" => {
                let arg_ok = self.print(args.first().expect("print_prelude_type result ok"));
                let arg_err = self.print(args.get(1).expect("print_prelude_type result err"));
                let ok = tuple(["ok".to_doc(), arg_ok]);
                let error = tuple(["error".to_doc(), arg_err]);
                docvec![ok, break_(" |", " | "), error].nest(INDENT).group()
            }
            // Getting here should mean we either forgot a built-in type or there is a
            // compiler error
            name => panic!("{name} is not a built-in type."),
        }
    }

    fn print_type_app(&self, module: &str, name: &str, args: &[Arc<Type>]) -> Document<'static> {
        let args = join(args.iter().map(|a| self.print(a)), ", ".to_doc());
        let name = Document::String(erl_safe_type_name(name.to_snake_case()));
        if self.current_module == module {
            docvec![name, "(", args, ")"]
        } else {
            docvec![module_name_atom(module), ":", name, "(", args, ")"]
        }
    }

    fn print_fn(&self, args: &[Arc<Type>], retrn: &Type) -> Document<'static> {
        let args = join(args.iter().map(|a| self.print(a)), ", ".to_doc());
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

fn find_private_functions_referenced_in_importable_constants(
    module: &TypedModule,
) -> HashSet<EcoString> {
    let mut overridden_publicity = HashSet::new();

    for def in module.definitions.iter() {
        if let Definition::ModuleConstant(c) = def {
            if c.publicity.is_importable() {
                find_referenced_private_functions(&c.value, &mut overridden_publicity)
            }
        }
    }
    overridden_publicity
}

fn find_referenced_private_functions(
    constant: &TypedConstant,
    already_found: &mut HashSet<EcoString>,
) {
    match constant {
        Constant::Invalid { .. } => panic!("invalid constants should not reach code generation"),

        Constant::Int { .. }
        | Constant::Float { .. }
        | Constant::String { .. }
        | Constant::BitArray { .. } => (),

        TypedConstant::Var {
            name, constructor, ..
        } => {
            if let Some(ValueConstructor { type_, .. }) = constructor.as_deref() {
                if let Type::Fn { .. } = **type_ {
                    let _ = already_found.insert(name.clone());
                }
            }
        }

        TypedConstant::Record { args, .. } => args
            .iter()
            .for_each(|arg| find_referenced_private_functions(&arg.value, already_found)),

        Constant::Tuple { elements, .. } | Constant::List { elements, .. } => elements
            .iter()
            .for_each(|element| find_referenced_private_functions(element, already_found)),
    }
}
