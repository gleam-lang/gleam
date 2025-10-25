// TODO: Refactor this module to be methods on structs rather than free
// functions with a load of arguments. See the JavaScript code generator and the
// formatter for examples.

mod pattern;
#[cfg(test)]
mod tests;

use crate::build::{Target, module_erlang_name};
use crate::erlang::pattern::PatternPrinter;
use crate::strings::{convert_string_escape_chars, to_snake_case};
use crate::type_::is_prelude_module;
use crate::{
    Result,
    ast::{CustomType, Function, Import, ModuleConstant, TypeAlias, *},
    docvec,
    line_numbers::LineNumbers,
    pretty::*,
    type_::{
        ModuleValueConstructor, PatternConstructor, Type, TypeVar, TypedCallArg, ValueConstructor,
        ValueConstructorVariant,
    },
};
use camino::Utf8Path;
use ecow::{EcoString, eco_format};
use itertools::Itertools;
use regex::{Captures, Regex};
use std::collections::HashSet;
use std::sync::OnceLock;
use std::{collections::HashMap, ops::Deref, str::FromStr, sync::Arc};
use vec1::Vec1;

const INDENT: isize = 4;
const MAX_COLUMNS: isize = 80;

fn module_name_atom(module: &str) -> Document<'static> {
    atom_string(module.replace('/', "@").into())
}

#[derive(Debug, Clone)]
struct Env<'a> {
    module: &'a str,
    function: &'a str,
    line_numbers: &'a LineNumbers,
    needs_function_docs: bool,
    echo_used: bool,
    current_scope_vars: im::HashMap<String, usize>,
    erl_function_scope_vars: im::HashMap<String, usize>,
}

impl<'env> Env<'env> {
    pub fn new(module: &'env str, function: &'env str, line_numbers: &'env LineNumbers) -> Self {
        let vars: im::HashMap<_, _> = std::iter::once(("_".into(), 0)).collect();
        Self {
            current_scope_vars: vars.clone(),
            erl_function_scope_vars: vars,
            needs_function_docs: false,
            echo_used: false,
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
                variable_name(name).to_doc()
            }
            Some(0) => variable_name(name).to_doc(),
            Some(n) => {
                use std::fmt::Write;
                let mut name = variable_name(name);
                write!(name, "@{n}").expect("pushing number suffix to name");
                name.to_doc()
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
        .filter_map(|definition| match definition {
            Definition::CustomType(CustomType {
                publicity: Publicity::Public,
                constructors,
                location,
                ..
            }) if !module.unused_definition_positions.contains(&location.start) => {
                Some(constructors)
            }
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
                        label
                            .as_ref()
                            .map(|(_, label)| (label.as_str(), type_.clone()))
                    },
                )
                .collect::<Option<Vec<_>>>()
                .map(|fields| (constructor.name.as_str(), fields))
        })
        .map(|(name, fields)| (name, record_definition(name, &fields)))
        .collect()
}

pub fn record_definition(name: &str, fields: &[(&str, Arc<Type>)]) -> String {
    let name = to_snake_case(name);
    let type_printer = TypePrinter::new("").var_as_any();
    let fields = fields.iter().map(move |(name, type_)| {
        let type_ = type_printer.print(type_);
        docvec![atom_string((*name).into()), " :: ", type_.group()]
    });
    let fields = break_("", "")
        .append(join(fields, break_(",", ", ")))
        .nest(INDENT)
        .append(break_("", ""))
        .group();
    docvec!["-record(", atom_string(name), ", {", fields, "}).", line()]
        .to_pretty_string(MAX_COLUMNS)
}

pub fn module<'a>(
    module: &'a TypedModule,
    line_numbers: &'a LineNumbers,
    root: &'a Utf8Path,
) -> Result<String> {
    Ok(module_document(module, line_numbers, root)?.to_pretty_string(MAX_COLUMNS))
}

fn module_document<'a>(
    module: &'a TypedModule,
    line_numbers: &'a LineNumbers,
    root: &'a Utf8Path,
) -> Result<Document<'a>> {
    let mut exports = vec![];
    let mut type_defs = vec![];
    let mut type_exports = vec![];

    let header = "-module("
        .to_doc()
        .append(module.erlang_name())
        .append(").")
        .append(line());

    // We need to know which private functions are referenced in importable
    // constants so that we can export them anyway in the generated Erlang.
    // This is because otherwise when the constant is used in another module it
    // would result in an error as it tries to reference this private function.
    let overridden_publicity = find_private_functions_referenced_in_importable_constants(module);

    for definition in &module.definitions {
        register_imports_and_exports(
            definition,
            &mut exports,
            &mut type_exports,
            &mut type_defs,
            &module.name,
            &overridden_publicity,
            &module.unused_definition_positions,
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

    let src_path_full = &module.type_info.src_path;
    let src_path_relative = EcoString::from(
        src_path_full
            .strip_prefix(root)
            .unwrap_or(src_path_full)
            .as_str(),
    )
    .replace("\\", "\\\\");

    let mut needs_function_docs = false;
    let mut echo_used = false;
    let mut statements = Vec::with_capacity(module.definitions.len());
    for definition in module.definitions.iter() {
        if let Some((statement_document, env)) = module_statement(
            definition,
            &module.name,
            module.type_info.is_internal,
            line_numbers,
            src_path_relative.clone(),
            &module.unused_definition_positions,
        ) {
            needs_function_docs = needs_function_docs || env.needs_function_docs;
            echo_used = echo_used || env.echo_used;
            statements.push(statement_document);
        }
    }

    let module_doc = if module.type_info.is_internal {
        Some(hidden_module_doc().append(lines(2)))
    } else if module.documentation.is_empty() {
        None
    } else {
        Some(module_doc(&module.documentation).append(lines(2)))
    };

    // We're going to need the documentation directives if any of the module's
    // functions need it, or if the module has a module comment that we want to
    // include in the generated Erlang source, or if the module is internal.
    let needs_doc_directive = needs_function_docs || module_doc.is_some();
    let documentation_directive = if needs_doc_directive {
        "-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif."
            .to_doc()
            .append(lines(2))
    } else {
        nil()
    };

    let module = docvec![
        header,
        "-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).",
        line(),
        "-define(FILEPATH, \"",
        src_path_relative,
        "\").",
        line(),
        exports,
        documentation_directive,
        module_doc,
        type_defs,
        join(statements, lines(2)),
    ];

    let module = if echo_used {
        module
            .append(lines(2))
            .append(std::include_str!("../templates/echo.erl").to_doc())
    } else {
        module
    };

    Ok(module.append(line()))
}

fn register_imports_and_exports(
    definition: &TypedDefinition,
    exports: &mut Vec<Document<'_>>,
    type_exports: &mut Vec<Document<'_>>,
    type_defs: &mut Vec<Document<'_>>,
    module_name: &str,
    overridden_publicity: &im::HashSet<EcoString>,
    unused_definition_positions: &HashSet<u32>,
) {
    // Do not generate any code for unused items
    if unused_definition_positions.contains(&definition.location().start) {
        return;
    }

    match definition {
        Definition::Function(Function {
            publicity,
            name: Some((_, name)),
            arguments,
            implementations,
            ..
        }) if publicity.is_importable() || overridden_publicity.contains(name) => {
            // If the function isn't for this target then don't attempt to export it
            if implementations.supports(Target::Erlang) {
                let function_name = escape_erlang_existing_name(name);
                exports.push(
                    atom_string(function_name.into())
                        .append("/")
                        .append(arguments.len()),
                )
            }
        }

        Definition::CustomType(CustomType {
            name,
            constructors,
            typed_parameters,
            opaque,
            external_erlang,
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
                erl_safe_type_name(to_snake_case(name))
                    .to_doc()
                    .append("/")
                    .append(typed_parameters.len()),
            );
            // Type definitions
            let definition = if constructors.is_empty() {
                if let Some((module, external_type, _location)) = external_erlang {
                    let printer = TypePrinter::new(module_name);
                    docvec![
                        module,
                        ":",
                        external_type,
                        "(",
                        join(
                            typed_parameters
                                .iter()
                                .map(|parameter| printer.print(parameter)),
                            ", ".to_doc()
                        ),
                        ")"
                    ]
                } else {
                    let constructors =
                        std::iter::once("any()".to_doc()).chain(phantom_vars_constructor);
                    join(constructors, break_(" |", " | "))
                }
            } else {
                let constructors = constructors
                    .iter()
                    .map(|constructor| {
                        let name = atom_string(to_snake_case(&constructor.name));
                        if constructor.arguments.is_empty() {
                            name
                        } else {
                            let type_printer = TypePrinter::new(module_name);
                            let arguments = constructor
                                .arguments
                                .iter()
                                .map(|argument| type_printer.print(&argument.type_));
                            tuple(std::iter::once(name).chain(arguments))
                        }
                    })
                    .chain(phantom_vars_constructor);
                join(constructors, break_(" |", " | "))
            }
            .nest(INDENT);
            let type_printer = TypePrinter::new(module_name);
            let params = join(
                typed_parameters
                    .iter()
                    .map(|type_| type_printer.print(type_)),
                ", ".to_doc(),
            );
            let doc = if *opaque { "-opaque " } else { "-type " }
                .to_doc()
                .append(erl_safe_type_name(to_snake_case(name)))
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
    is_internal_module: bool,
    line_numbers: &'a LineNumbers,
    src_path: EcoString,
    unused_definition_positions: &HashSet<u32>,
) -> Option<(Document<'a>, Env<'a>)> {
    match statement {
        // Do not generate any code for unused items
        Definition::Function(function)
            if unused_definition_positions.contains(&function.location.start) =>
        {
            None
        }

        Definition::Function(function) => {
            module_function(function, module, is_internal_module, line_numbers, src_path)
        }

        Definition::TypeAlias(TypeAlias { .. })
        | Definition::CustomType(CustomType { .. })
        | Definition::Import(Import { .. })
        | Definition::ModuleConstant(ModuleConstant { .. }) => None,
    }
}

fn module_function<'a>(
    function: &'a TypedFunction,
    module: &'a str,
    is_internal_module: bool,
    line_numbers: &'a LineNumbers,
    src_path: EcoString,
) -> Option<(Document<'a>, Env<'a>)> {
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

    let (_, function_name) = function
        .name
        .as_ref()
        .expect("A module's function must be named");
    let function_name = escape_erlang_existing_name(function_name);
    let file_attribute = file_attribute(src_path, function, line_numbers);

    let mut env = Env::new(module, function_name, line_numbers);
    let var_usages = collect_type_var_usages(
        HashMap::new(),
        std::iter::once(&function.return_type).chain(function.arguments.iter().map(|a| &a.type_)),
    );
    let type_printer = TypePrinter::new(module).with_var_usages(&var_usages);
    let arguments_spec = function
        .arguments
        .iter()
        .map(|a| type_printer.print(&a.type_));
    let return_spec = type_printer.print(&function.return_type);

    let spec = fun_spec(function_name, arguments_spec, return_spec);
    let arguments = if function.external_erlang.is_some() {
        external_fun_arguments(&function.arguments, &mut env)
    } else {
        fun_arguments(&function.arguments, &mut env)
    };

    let body = function
        .external_erlang
        .as_ref()
        .map(|(module, function, _location)| {
            docvec![
                atom(module),
                ":",
                atom(escape_erlang_existing_name(function)),
                arguments.clone()
            ]
        })
        .unwrap_or_else(|| statement_sequence(&function.body, &mut env));

    let attributes = file_attribute;
    let attributes = if is_internal_module || function.publicity.is_internal() {
        // If a function is marked as internal or comes from an internal module
        // we want to hide its documentation in the Erlang shell!
        // So the doc directive will look like this: `-doc(false).`
        env.needs_function_docs = true;
        docvec![attributes, line(), hidden_function_doc()]
    } else {
        match &function.documentation {
            Some((_, documentation)) => {
                env.needs_function_docs = true;
                let doc_lines = documentation
                    .trim_end()
                    .split('\n')
                    .map(EcoString::from)
                    .collect_vec();
                docvec![attributes, line(), function_doc(&doc_lines)]
            }
            _ => attributes,
        }
    };

    Some((
        docvec![
            attributes,
            line(),
            spec,
            atom_string(escape_erlang_existing_name(function_name).into()),
            arguments,
            " ->",
            line().append(body).nest(INDENT).group(),
            ".",
        ],
        env,
    ))
}

fn file_attribute<'a>(
    path: EcoString,
    function: &'a TypedFunction,
    line_numbers: &'a LineNumbers,
) -> Document<'a> {
    let line = line_numbers.line_number(function.location.start);
    docvec!["-file(\"", path, "\", ", line, ")."]
}

enum DocCommentKind {
    Module,
    Function,
}

enum DocCommentContent<'a> {
    String(&'a Vec<EcoString>),
    False,
}

fn hidden_module_doc<'a>() -> Document<'a> {
    doc_attribute(DocCommentKind::Module, DocCommentContent::False)
}

fn module_doc<'a>(content: &Vec<EcoString>) -> Document<'a> {
    doc_attribute(DocCommentKind::Module, DocCommentContent::String(content))
}

fn hidden_function_doc<'a>() -> Document<'a> {
    doc_attribute(DocCommentKind::Function, DocCommentContent::False)
}

fn function_doc<'a>(content: &Vec<EcoString>) -> Document<'a> {
    doc_attribute(DocCommentKind::Function, DocCommentContent::String(content))
}

fn doc_attribute<'a>(kind: DocCommentKind, content: DocCommentContent<'_>) -> Document<'a> {
    let prefix = match kind {
        DocCommentKind::Module => "?MODULEDOC",
        DocCommentKind::Function => "?DOC",
    };

    match content {
        DocCommentContent::False => prefix.to_doc().append("(false)."),
        DocCommentContent::String(doc_lines) => {
            let is_multiline_doc_comment = doc_lines.len() > 1;
            let doc_lines = join(
                doc_lines.iter().map(|line| {
                    let line = line.replace("\\", "\\\\").replace("\"", "\\\"");
                    docvec!["\"", line, "\\n\""]
                }),
                line(),
            );
            if is_multiline_doc_comment {
                let nested_documentation = docvec![line(), doc_lines].nest(INDENT);
                docvec![prefix, "(", nested_documentation, line(), ")."]
            } else {
                docvec![prefix, "(", doc_lines, ")."]
            }
        }
    }
}

fn external_fun_arguments<'a>(arguments: &'a [TypedArg], env: &mut Env<'a>) -> Document<'a> {
    wrap_arguments(arguments.iter().map(|argument| {
        let name = match &argument.names {
            ArgNames::Discard { name, .. }
            | ArgNames::LabelledDiscard { name, .. }
            | ArgNames::Named { name, .. }
            | ArgNames::NamedLabelled { name, .. } => name,
        };
        if name.chars().all(|c| c == '_') {
            env.next_local_var_name("argument")
        } else {
            env.next_local_var_name(name)
        }
    }))
}

fn fun_arguments<'a>(arguments: &'a [TypedArg], env: &mut Env<'a>) -> Document<'a> {
    wrap_arguments(arguments.iter().map(|argument| match &argument.names {
        ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => "_".to_doc(),
        ArgNames::Named { name, .. } | ArgNames::NamedLabelled { name, .. } => {
            env.next_local_var_name(name)
        }
    }))
}

fn wrap_arguments<'a, I>(arguments: I) -> Document<'a>
where
    I: IntoIterator<Item = Document<'a>>,
{
    break_("", "")
        .append(join(arguments, break_(",", ", ")))
        .nest(INDENT)
        .append(break_("", ""))
        .surround("(", ")")
        .group()
}

fn fun_spec<'a>(
    name: &'a str,
    arguments: impl IntoIterator<Item = Document<'a>>,
    return_: Document<'a>,
) -> Document<'a> {
    "-spec "
        .to_doc()
        .append(atom(name))
        .append(wrap_arguments(arguments))
        .append(" -> ")
        .append(return_)
        .append(".")
        .append(line())
        .group()
}

fn atom_string(value: EcoString) -> Document<'static> {
    escape_atom_string(value).to_doc()
}

fn atom_pattern() -> &'static Regex {
    static ATOM_PATTERN: OnceLock<Regex> = OnceLock::new();
    ATOM_PATTERN.get_or_init(|| Regex::new(r"^[a-z][a-z0-9_@]*$").expect("atom RE regex"))
}

fn atom(value: &str) -> Document<'_> {
    if is_erlang_reserved_word(value) {
        // Escape because of keyword collision
        eco_format!("'{value}'").to_doc()
    } else if atom_pattern().is_match(value) {
        // No need to escape
        EcoString::from(value).to_doc()
    } else {
        // Escape because of characters contained
        eco_format!("'{value}'").to_doc()
    }
}

pub fn escape_atom_string(value: EcoString) -> EcoString {
    if is_erlang_reserved_word(&value) {
        // Escape because of keyword collision
        eco_format!("'{value}'")
    } else if atom_pattern().is_match(&value) {
        value
    } else {
        // Escape because of characters contained
        eco_format!("'{value}'")
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

            if slashes.len().is_multiple_of(2) {
                format!("{slashes}u")
            } else {
                format!("{slashes}x")
            }
        });
    EcoString::from(content).to_doc()
}

fn string(value: &str) -> Document<'_> {
    string_inner(value).surround("<<\"", "\"/utf8>>")
}

fn string_length_utf8_bytes(str: &EcoString) -> usize {
    convert_string_escape_chars(str).len()
}

fn tuple<'a>(elements: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    join(elements, break_(",", ", "))
        .nest(INDENT)
        .surround("{", "}")
        .group()
}

fn const_string_concatenate_bit_array<'a>(
    elements: impl IntoIterator<Item = Document<'a>>,
) -> Document<'a> {
    join(elements, break_(",", ", "))
        .nest(INDENT)
        .surround("<<", ">>")
        .group()
}

fn const_string_concatenate<'a>(
    left: &'a TypedConstant,
    right: &'a TypedConstant,
    env: &mut Env<'a>,
) -> Document<'a> {
    let left = const_string_concatenate_argument(left, env);
    let right = const_string_concatenate_argument(right, env);
    const_string_concatenate_bit_array([left, right])
}

fn const_string_concatenate_inner<'a>(
    left: &'a TypedConstant,
    right: &'a TypedConstant,
    env: &mut Env<'a>,
) -> Document<'a> {
    let left = const_string_concatenate_argument(left, env);
    let right = const_string_concatenate_argument(right, env);
    join([left, right], break_(",", ", "))
}

fn const_string_concatenate_argument<'a>(
    value: &'a TypedConstant,
    env: &mut Env<'a>,
) -> Document<'a> {
    match value {
        Constant::String { value, .. } => docvec!['"', string_inner(value), "\"/utf8"],

        Constant::Var {
            constructor: Some(constructor),
            ..
        } => match &constructor.variant {
            ValueConstructorVariant::ModuleConstant {
                literal: Constant::String { value, .. },
                ..
            } => docvec!['"', string_inner(value), "\"/utf8"],
            ValueConstructorVariant::ModuleConstant {
                literal: Constant::StringConcatenation { left, right, .. },
                ..
            } => const_string_concatenate_inner(left, right, env),
            _ => const_inline(value, env),
        },

        Constant::StringConcatenation { left, right, .. } => {
            const_string_concatenate_inner(left, right, env)
        }

        _ => const_inline(value, env),
    }
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

fn bit_array<'a>(elements: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    join(elements, break_(",", ", "))
        .nest(INDENT)
        .surround("<<", ">>")
        .group()
}

fn const_segment<'a>(
    value: &'a TypedConstant,
    options: &'a [TypedConstantBitArraySegmentOption],
    env: &mut Env<'a>,
) -> Document<'a> {
    let value_is_a_string_literal = matches!(value, Constant::String { .. });

    let create_document = |env: &mut Env<'a>| {
        match value {
            // Skip the normal <<value/utf8>> surrounds
            Constant::String { value, .. } => value.to_doc().surround("\"", "\""),

            // As normal
            Constant::Int { .. } | Constant::Float { .. } | Constant::BitArray { .. } => {
                const_inline(value, env)
            }

            // Wrap anything else in parentheses
            value => const_inline(value, env).surround("(", ")"),
        }
    };

    let size = |value: &'a TypedConstant, env: &mut Env<'a>| match value {
        Constant::Int { .. } => Some(":".to_doc().append(const_inline(value, env))),
        _ => Some(
            ":".to_doc()
                .append(const_inline(value, env).surround("(", ")")),
        ),
    };

    let unit = |value: &'a u8| Some(eco_format!("unit:{value}").to_doc());

    bit_array_segment(
        create_document,
        options,
        size,
        unit,
        value_is_a_string_literal,
        false,
        env,
    )
}

enum Position {
    Tail,
    NotTail,
}

fn statement<'a>(
    statement: &'a TypedStatement,
    env: &mut Env<'a>,
    position: Position,
) -> Document<'a> {
    match statement {
        Statement::Expression(e) => expr(e, env),
        Statement::Assignment(a) => assignment(a, env, position),
        Statement::Use(use_) => expr(&use_.call, env),
        Statement::Assert(a) => assert(a, env),
    }
}

fn expr_segment<'a>(
    value: &'a TypedExpr,
    options: &'a [BitArrayOption<TypedExpr>],
    env: &mut Env<'a>,
) -> Document<'a> {
    let value_is_a_string_literal = matches!(value, TypedExpr::String { .. });

    let create_document = |env: &mut Env<'a>| {
        match value {
            // Skip the normal <<value/utf8>> surrounds and set the string literal flag
            TypedExpr::String { value, .. } => string_inner(value).surround("\"", "\""),

            // As normal
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::BitArray { .. } => expr(value, env),

            // Wrap anything else in parentheses
            value => expr(value, env).surround("(", ")"),
        }
    };

    let size = |expression: &'a TypedExpr, env: &mut Env<'a>| match expression {
        TypedExpr::Int { value, .. } => {
            let v = value.replace("_", "");
            let v = u64::from_str(&v).unwrap_or(0);
            Some(eco_format!(":{v}").to_doc())
        }

        _ => {
            let inner_expr = maybe_block_expr(expression, env).surround("(", ")");
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

    let unit = |value: &'a u8| Some(eco_format!("unit:{value}").to_doc());

    bit_array_segment(
        create_document,
        options,
        size,
        unit,
        value_is_a_string_literal,
        false,
        env,
    )
}

fn bit_array_segment<'a, Value: 'a, CreateDoc, SizeToDoc, UnitToDoc, State>(
    mut create_document: CreateDoc,
    options: &'a [BitArrayOption<Value>],
    mut size_to_doc: SizeToDoc,
    mut unit_to_doc: UnitToDoc,
    value_is_a_string_literal: bool,
    value_is_a_discard: bool,
    state: &mut State,
) -> Document<'a>
where
    CreateDoc: FnMut(&mut State) -> Document<'a>,
    SizeToDoc: FnMut(&'a Value, &mut State) -> Option<Document<'a>>,
    UnitToDoc: FnMut(&'a u8) -> Option<Document<'a>>,
{
    let mut size: Option<Document<'a>> = None;
    let mut unit: Option<Document<'a>> = None;
    let mut others = Vec::new();

    // Erlang only allows valid codepoint integers to be used as values for utf segments
    // We want to support <<string_var:utf8>> for all string variables, but <<StringVar/utf8>> is invalid
    // To work around this we use the binary type specifier for these segments instead
    let override_type = if !value_is_a_string_literal && !value_is_a_discard {
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
            Opt::Size { value, .. } => size = size_to_doc(value, state),
            Opt::Unit { value, .. } => unit = unit_to_doc(value),
        }
    }

    let mut document = create_document(state);

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
    if statements.len() == 1
        && let Statement::Expression(expression) = statements.first()
        && !needs_begin_end_wrapping(expression)
    {
        return docvec!['(', expr(expression, env), ')'];
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
        let position = if i + 1 == count {
            Position::Tail
        } else {
            Position::NotTail
        };
        documents.push(statement(expression, env, position).group());

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

    // If we have a constant value divided by zero then it's safe to replace it
    // directly with 0.
    if left.is_literal() && right.zero_compile_time_number() {
        return "0".to_doc();
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

fn let_assert<'a>(
    value: &'a TypedExpr,
    pattern: &'a TypedPattern,
    environment: &mut Env<'a>,
    message: Option<&'a TypedExpr>,
    position: Position,
    location: SrcSpan,
) -> Document<'a> {
    // If the pattern will never fail, like a tuple or a simple variable, we
    // simply treat it as if it were a `let` assignment.
    if pattern.always_matches() {
        return let_(value, pattern, environment);
    }

    let message = match message {
        Some(message) => expr(message, environment),
        None => string("Pattern match failed, no pattern matched the value."),
    };

    let subject = maybe_block_expr(value, environment);

    // The code we generated for a `let assert` assignment looks something like
    // this. For this Gleam code:
    //
    // ```gleam
    // let assert [a, b, c] = [1, 2, 3]
    // ```
    //
    // We generate (roughly) the following Erlang:
    //
    // ```erlang
    // {A, B, C} = case [1, 2, 3] of
    //   [A, B, C] -> {A, B, C};
    //   _ -> erlang:error(...)
    // end.
    // ```
    // This is the most efficient way to properly extract all the required
    // variables from the pattern. However, if the `let assert` assignment is
    // the last in a block, like this:
    //
    // ```gleam
    // let x = {
    //   let assert [a, b, c] = [1, 2, 3]
    // }
    // ```
    //
    // The generated Erlang code will end up assigning the value `#(1, 2, 3)`
    // to the variable `x`, instead of `[1, 2, 3]`. In this case, we must
    // generate slightly different code. Since we know we won't be using the
    // bound variables anywhere (there is nothing else in this scope to
    // reference them), we can safely remove the assignment from the generated
    // code, and generate the following:
    //
    // ```erlang
    // X = begin
    //   _assert_subject = [1, 2, 3]
    //   case _assert_subject of
    //     [A, B, C] -> _assert_subject;
    //     _ -> erlang:error(...)
    //   end
    // end.
    // ```
    //
    // That correctly assigns `[1, 2, 3]` to the `x` variable.
    //
    let is_tail = match position {
        Position::Tail => true,
        Position::NotTail => false,
    };

    let (subject_assignment, subject) = if is_tail && !value.is_var() {
        let variable = environment.next_local_var_name(ASSERT_SUBJECT_VARIABLE);
        let assignment = docvec![variable.clone(), " = ", subject, ",", line()];
        (assignment, variable)
    } else {
        (nil(), subject)
    };

    let mut pattern_printer = PatternPrinter::new(environment);
    let pattern_document = pattern_printer.print(pattern);
    let PatternPrinter {
        environment,
        variables,
        guards,
        assignments,
    } = pattern_printer;

    let clause_guard = optional_clause_guard(None, guards, environment);

    let value_document = match variables.as_slice() {
        _ if is_tail => subject.clone(),
        [] => "nil".to_doc(),
        [variable] => environment.local_var_name(variable),
        variables => {
            let variables = variables
                .iter()
                .map(|variable| environment.local_var_name(variable));
            docvec![
                break_("{", "{"),
                join(variables, break_(",", ", ")).nest(INDENT),
                "}"
            ]
            .group()
        }
    };

    let assignment = match variables.as_slice() {
        _ if is_tail => nil(),
        [] => nil(),
        [variable] => environment.next_local_var_name(variable).append(" = "),
        variables => {
            let variables = variables
                .iter()
                .map(|variable| environment.next_local_var_name(variable));
            docvec![
                break_("{", "{"),
                join(variables, break_(",", ", ")).nest(INDENT),
                "} = "
            ]
            .group()
        }
    };

    let clauses = docvec![
        pattern_document,
        clause_guard,
        " -> ",
        value_document,
        ";",
        line(),
        environment.next_local_var_name(ASSERT_FAIL_VARIABLE),
        " ->",
        docvec![
            line(),
            erlang_error(
                "let_assert",
                &message,
                location,
                vec![
                    ("value", environment.local_var_name(ASSERT_FAIL_VARIABLE)),
                    ("start", location.start.to_doc()),
                    ("'end'", value.location().end.to_doc()),
                    ("pattern_start", pattern.location().start.to_doc()),
                    ("pattern_end", pattern.location().end.to_doc()),
                ],
                environment,
            )
            .nest(INDENT)
        ]
        .nest(INDENT)
    ];

    let assignments = if assignments.is_empty() {
        nil()
    } else {
        docvec![",", line(), join(assignments, ",".to_doc().append(line()))]
    };

    docvec![
        subject_assignment,
        assignment,
        "case ",
        subject,
        " of",
        docvec![line(), clauses].nest(INDENT),
        line(),
        "end",
        assignments,
    ]
}

fn let_<'a>(
    value: &'a TypedExpr,
    pattern: &'a TypedPattern,
    environment: &mut Env<'a>,
) -> Document<'a> {
    let body = maybe_block_expr(value, environment).group();
    PatternPrinter::new(environment)
        .print(pattern)
        .append(" = ")
        .append(body)
}

fn float<'a>(value: &str) -> Document<'a> {
    let mut value = value.replace('_', "");
    if value.ends_with('.') {
        value.push('0')
    }

    match value.split('.').collect_vec().as_slice() {
        ["0", "0"] => "+0.0".to_doc(),
        [before_dot, after_dot] if after_dot.starts_with('e') => {
            eco_format!("{before_dot}.0{after_dot}").to_doc()
        }
        _ => EcoString::from(value).to_doc(),
    }
}

fn expr_list<'a>(
    elements: &'a [TypedExpr],
    tail: &'a Option<Box<TypedExpr>>,
    env: &mut Env<'a>,
) -> Document<'a> {
    let elements = join(
        elements
            .iter()
            .map(|element| maybe_block_expr(element, env)),
        break_(",", ", "),
    );
    list(
        elements,
        tail.as_ref().map(|element| maybe_block_expr(element, env)),
    )
}

fn list<'a>(elements: Document<'a>, tail: Option<Document<'a>>) -> Document<'a> {
    let elements = match tail {
        Some(tail) if elements.is_empty() => return tail.to_doc(),

        Some(tail) => elements.append(break_(" |", " | ")).append(tail),

        None => elements,
    };

    elements.to_doc().nest(INDENT).surround("[", "]").group()
}

fn var<'a>(name: &'a str, constructor: &'a ValueConstructor, env: &mut Env<'a>) -> Document<'a> {
    match &constructor.variant {
        ValueConstructorVariant::Record {
            name: record_name, ..
        } => match constructor.type_.deref() {
            Type::Fn { arguments, .. } => {
                let chars = incrementing_arguments_list(arguments.len());
                "fun("
                    .to_doc()
                    .append(chars.clone())
                    .append(") -> {")
                    .append(atom_string(to_snake_case(record_name)))
                    .append(", ")
                    .append(chars)
                    .append("} end")
            }
            _ => atom_string(to_snake_case(record_name)),
        },

        ValueConstructorVariant::LocalVariable { .. } => env.local_var_name(name),

        ValueConstructorVariant::ModuleConstant { literal, .. }
        | ValueConstructorVariant::LocalConstant { literal } => const_inline(literal, env),

        ValueConstructorVariant::ModuleFn {
            arity,
            external_erlang: Some((module, name)),
            ..
        } if module == env.module => function_reference(None, name, *arity),

        ValueConstructorVariant::ModuleFn {
            arity,
            external_erlang: Some((module, name)),
            ..
        } => function_reference(Some(module), name, *arity),

        ValueConstructorVariant::ModuleFn { arity, module, .. } if module == env.module => {
            function_reference(None, name, *arity)
        }

        ValueConstructorVariant::ModuleFn {
            arity,
            module,
            name,
            ..
        } => function_reference(Some(module), name, *arity),
    }
}

fn function_reference<'a>(module: Option<&'a str>, name: &'a str, arity: usize) -> Document<'a> {
    match module {
        None => "fun ".to_doc(),
        Some(module) => "fun ".to_doc().append(module_name_atom(module)).append(":"),
    }
    .append(atom(escape_erlang_existing_name(name)))
    .append("/")
    .append(arity)
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

    EcoString::from(value).to_doc()
}

fn const_inline<'a>(literal: &'a TypedConstant, env: &mut Env<'a>) -> Document<'a> {
    match literal {
        Constant::Int { value, .. } => int(value),
        Constant::Float { value, .. } => float(value),
        Constant::String { value, .. } => string(value),
        Constant::Tuple { elements, .. } => {
            tuple(elements.iter().map(|element| const_inline(element, env)))
        }

        Constant::List { elements, .. } => join(
            elements.iter().map(|element| const_inline(element, env)),
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

        Constant::Record {
            tag,
            type_,
            arguments,
            ..
        } if arguments.is_empty() => match type_.deref() {
            Type::Fn { arguments, .. } => record_constructor_function(tag, arguments.len()),
            _ => atom_string(to_snake_case(tag)),
        },

        Constant::Record { tag, arguments, .. } => {
            let arguments = arguments
                .iter()
                .map(|argument| const_inline(&argument.value, env));
            let tag = atom_string(to_snake_case(tag));
            tuple(std::iter::once(tag).chain(arguments))
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

        Constant::StringConcatenation { left, right, .. } => {
            const_string_concatenate(left, right, env)
        }

        Constant::Invalid { .. } => panic!("invalid constants should not reach code generation"),
    }
}

fn record_constructor_function(tag: &EcoString, arity: usize) -> Document<'_> {
    let chars = incrementing_arguments_list(arity);
    "fun("
        .to_doc()
        .append(chars.clone())
        .append(") -> {")
        .append(atom_string(to_snake_case(tag)))
        .append(", ")
        .append(chars)
        .append("} end")
}

fn clause<'a>(clause: &'a TypedClause, environment: &mut Env<'a>) -> Document<'a> {
    let Clause {
        guard,
        pattern,
        alternative_patterns,
        then,
        ..
    } = clause;

    // These are required to get the alternative patterns working properly.
    // Simply rendering the duplicate erlang clauses breaks the variable
    // rewriting because each pattern would define different (rewritten)
    // variables names.
    let initial_erlang_vars = environment.erl_function_scope_vars.clone();
    let initial_scope_vars = environment.current_scope_vars.clone();

    let mut branches_docs = Vec::with_capacity(alternative_patterns.len() + 1);
    for patterns in std::iter::once(pattern).chain(alternative_patterns) {
        // Erlang doesn't support alternative patterns, so we turn each
        // alternative into a branch of its own.
        // For each alternative, before generating the body, we need to reset
        // the variables in scope to what they are before the case expression,
        // so that a branch will not interfere with the other ones!
        environment.erl_function_scope_vars = initial_erlang_vars.clone();
        environment.current_scope_vars = initial_scope_vars.clone();
        let mut pattern_printer = PatternPrinter::new(environment);

        let pattern = match patterns.as_slice() {
            [pattern] => pattern_printer.print(pattern),
            _ => tuple(patterns.iter().map(|pattern| {
                pattern_printer.reset_variables();
                pattern_printer.print(pattern)
            })),
        };

        let PatternPrinter {
            environment,
            guards,
            variables: _,
            assignments,
        } = pattern_printer;

        let guard = optional_clause_guard(guard.as_ref(), guards, environment);
        let then = clause_consequence(then, assignments, environment).group();
        branches_docs.push(docvec![
            pattern,
            guard,
            " ->",
            docvec![line(), then].nest(INDENT),
        ]);
    }

    join(branches_docs, ";".to_doc().append(lines(2)))
}

fn clause_consequence<'a>(
    consequence: &'a TypedExpr,
    // Further assignments that the pattern might need to introduce at the start
    // of the new block.
    assignments: Vec<Document<'a>>,
    env: &mut Env<'a>,
) -> Document<'a> {
    let assignment_doc = if assignments.is_empty() {
        nil()
    } else {
        let separator = ",".to_doc().append(line());
        join(assignments, separator.clone()).append(separator)
    };

    let consequence = match consequence {
        TypedExpr::Block { statements, .. } => statement_sequence(statements, env),
        _ => expr(consequence, env),
    };
    assignment_doc.append(consequence)
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
        ClauseGuard::Block { value, .. } => bare_clause_guard(value, env).surround("(", ")"),

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
    let index_doc = eco_format!("{}", (index + 1)).to_doc();
    let tuple_doc = bare_clause_guard(tuple, env);
    "erlang:element"
        .to_doc()
        .append(wrap_arguments([index_doc, tuple_doc]))
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
        | ClauseGuard::ModuleSelect { .. }
        | ClauseGuard::Block { .. } => bare_clause_guard(guard, env),
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
        tuple(
            subjects
                .iter()
                .map(|element| maybe_block_expr(element, env)),
        )
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

fn call<'a>(fun: &'a TypedExpr, arguments: &'a [TypedCallArg], env: &mut Env<'a>) -> Document<'a> {
    docs_arguments_call(
        fun,
        arguments
            .iter()
            .map(|argument| maybe_block_expr(&argument.value, env))
            .collect(),
        env,
    )
}

fn module_fn_with_arguments<'a>(
    module: &'a str,
    name: &'a str,
    arguments: Vec<Document<'a>>,
    env: &Env<'a>,
) -> Document<'a> {
    let name = escape_erlang_existing_name(name);
    let arguments = wrap_arguments(arguments);
    if module == env.module {
        atom(name).append(arguments)
    } else {
        atom_string(module.replace('/', "@").into())
            .append(":")
            .append(atom(name))
            .append(arguments)
    }
}

fn docs_arguments_call<'a>(
    fun: &'a TypedExpr,
    mut arguments: Vec<Document<'a>>,
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
        } => tuple(std::iter::once(atom_string(to_snake_case(name))).chain(arguments)),

        TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant:
                        ValueConstructorVariant::ModuleFn {
                            external_erlang: Some((module, name)),
                            ..
                        }
                        | ValueConstructorVariant::ModuleFn { module, name, .. },
                    ..
                },
            ..
        } => module_fn_with_arguments(module, name, arguments, env),

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
                                    constructor: Some(constructor),
                                    ..
                                },
                            ..
                        },
                    ..
                },
            ..
        } if constructor.variant.is_module_fn() => match &constructor.variant {
            ValueConstructorVariant::ModuleFn {
                external_erlang: Some((module, name)),
                ..
            }
            | ValueConstructorVariant::ModuleFn { module, name, .. } => {
                module_fn_with_arguments(module, name, arguments, env)
            }
            _ => {
                unreachable!("The above clause guard ensures that this is a module fn")
            }
        },

        TypedExpr::ModuleSelect {
            constructor:
                ModuleValueConstructor::Fn {
                    external_erlang: Some((module, name)),
                    ..
                }
                | ModuleValueConstructor::Fn { module, name, .. },
            ..
        } => {
            let arguments = wrap_arguments(arguments);
            let name = escape_erlang_existing_name(name);
            // We use the constructor Fn variant's `module` and function `name`.
            // It would also be valid to use the module and label as in the
            // Gleam code, but using the variant can result in an optimisation
            // in which the target function is used for `external fn`s, removing
            // one layer of wrapping.
            // This also enables an optimisation in the Erlang compiler in which
            // some Erlang BIFs can be replaced with literals if their arguments
            // are literals, such as `binary_to_atom`.
            atom_string(module_erlang_name(module))
                .append(":")
                .append(atom_string(name.into()))
                .append(arguments)
        }

        TypedExpr::Fn { kind, body, .. } if kind.is_capture() => {
            if let Statement::Expression(TypedExpr::Call {
                fun,
                arguments: inner_arguments,
                ..
            }) = body.first()
            {
                let mut merged_arguments = Vec::with_capacity(inner_arguments.len());
                for arg in inner_arguments {
                    match &arg.value {
                        TypedExpr::Var { name, .. } if name == CAPTURE_VARIABLE => {
                            merged_arguments.push(arguments.swap_remove(0))
                        }
                        e => merged_arguments.push(maybe_block_expr(e, env)),
                    }
                }
                docs_arguments_call(fun, merged_arguments, env)
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
            let arguments = wrap_arguments(arguments);
            expr(fun, env).surround("(", ")").append(arguments)
        }

        other => {
            let arguments = wrap_arguments(arguments);
            maybe_block_expr(other, env).append(arguments)
        }
    }
}

fn record_update<'a>(
    record: &'a Option<Box<TypedAssignment>>,
    constructor: &'a TypedExpr,
    arguments: &'a [TypedCallArg],
    env: &mut Env<'a>,
) -> Document<'a> {
    let vars = env.current_scope_vars.clone();

    let document = match record.as_ref() {
        Some(record) => docvec![
            assignment(record, env, Position::NotTail),
            ",",
            line(),
            call(constructor, arguments, env)
        ],
        None => call(constructor, arguments, env),
    };

    env.current_scope_vars = vars;

    document
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
        // Record updates are 1 expression if there's no assignment, multiple otherwise.
        TypedExpr::RecordUpdate {
            record_assignment, ..
        } => record_assignment.is_some(),

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
        | TypedExpr::Echo { .. }
        | TypedExpr::Panic { .. }
        | TypedExpr::BitArray { .. }
        | TypedExpr::NegateBool { .. }
        | TypedExpr::NegateInt { .. }
        | TypedExpr::Invalid { .. } => false,
    }
}

fn todo<'a>(message: Option<&'a TypedExpr>, location: SrcSpan, env: &mut Env<'a>) -> Document<'a> {
    let message = match message {
        Some(m) => expr(m, env),
        None => string("`todo` expression evaluated. This code has not yet been implemented."),
    };
    erlang_error("todo", &message, location, vec![], env)
}

fn panic<'a>(location: SrcSpan, message: Option<&'a TypedExpr>, env: &mut Env<'a>) -> Document<'a> {
    let message = match message {
        Some(m) => expr(m, env),
        None => string("`panic` expression evaluated."),
    };
    erlang_error("panic", &message, location, vec![], env)
}

fn echo<'a>(
    body: Document<'a>,
    message: Option<&'a TypedExpr>,
    location: &SrcSpan,
    env: &mut Env<'a>,
) -> Document<'a> {
    env.echo_used = true;

    let message = message
        .as_ref()
        .map(|message| maybe_block_expr(message, env))
        .unwrap_or("nil".to_doc());

    "echo".to_doc().append(wrap_arguments(vec![
        body,
        message,
        env.line_numbers.line_number(location.start).to_doc(),
    ]))
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
        message.clone(),
        ",",
        line(),
        "file => <<?FILEPATH/utf8>>,",
        line(),
        "module => ",
        env.module.to_doc().surround("<<\"", "\"/utf8>>"),
        ",",
        line(),
        "function => ",
        string(env.function),
        ",",
        line(),
        "line => ",
        env.line_numbers.line_number(location.start),
    ];
    for (key, value) in fields {
        fields_doc = fields_doc
            .append(",")
            .append(line())
            .append(key)
            .append(" => ")
            .append(value);
    }
    let error = docvec!["#{", fields_doc.group().nest(INDENT), "}"];
    docvec!["erlang:error", wrap_arguments([error.group()])]
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

        TypedExpr::Echo {
            expression,
            location,
            message,
            ..
        } => {
            let expression = expression
                .as_ref()
                .expect("echo with no expression outside of pipe");
            let expression = maybe_block_expr(expression, env);
            echo(expression, message.as_deref(), location, env)
        }

        TypedExpr::Int { value, .. } => int(value),
        TypedExpr::Float { value, .. } => float(value),
        TypedExpr::String { value, .. } => string(value),

        TypedExpr::Pipeline {
            first_value,
            assignments,
            finally,
            ..
        } => pipeline(first_value, assignments, finally, env),

        TypedExpr::Block { statements, .. } => block(statements, env),

        TypedExpr::TupleIndex { tuple, index, .. } => tuple_index(tuple, *index, env),

        TypedExpr::Var {
            name, constructor, ..
        } => var(name, constructor, env),

        TypedExpr::Fn {
            arguments, body, ..
        } => fun(arguments, body, env),

        TypedExpr::NegateBool { value, .. } => negate_with("not ", value, env),

        TypedExpr::NegateInt { value, .. } => negate_with("- ", value, env),

        TypedExpr::List { elements, tail, .. } => expr_list(elements, tail, env),

        TypedExpr::Call { fun, arguments, .. } => call(fun, arguments, env),

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, arity: 0, .. },
            ..
        } => atom_string(to_snake_case(name)),

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Constant { literal, .. },
            ..
        } => const_inline(literal, env),

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, arity, .. },
            ..
        } => record_constructor_function(name, *arity as usize),

        TypedExpr::ModuleSelect {
            type_,
            constructor:
                ModuleValueConstructor::Fn {
                    external_erlang: Some((module, name)),
                    ..
                }
                | ModuleValueConstructor::Fn { module, name, .. },
            ..
        } => module_select_fn(type_.clone(), module, name),

        TypedExpr::RecordAccess { record, index, .. } => tuple_index(record, index + 1, env),

        TypedExpr::RecordUpdate {
            record_assignment,
            constructor,
            arguments,
            ..
        } => record_update(record_assignment, constructor, arguments, env),

        TypedExpr::Case {
            subjects, clauses, ..
        } => case(subjects, clauses, env),

        TypedExpr::BinOp {
            name, left, right, ..
        } => bin_op(name, left, right, env),

        TypedExpr::Tuple { elements, .. } => tuple(
            elements
                .iter()
                .map(|element| maybe_block_expr(element, env)),
        ),

        TypedExpr::BitArray { segments, .. } => bit_array(
            segments
                .iter()
                .map(|s| expr_segment(&s.value, &s.options, env)),
        ),

        TypedExpr::Invalid { .. } => panic!("invalid expressions should not reach code generation"),
    }
}

fn pipeline<'a>(
    first_value: &'a TypedPipelineAssignment,
    assignments: &'a [(TypedPipelineAssignment, PipelineAssignmentKind)],
    finally: &'a TypedExpr,
    env: &mut Env<'a>,
) -> Document<'a> {
    let mut documents = Vec::with_capacity((assignments.len() + 1) * 3);

    let all_assignments = std::iter::once(first_value)
        .chain(assignments.iter().map(|(assignment, _kind)| assignment));

    let echo_doc = |var_name: &Option<Document<'a>>,
                    message: Option<&'a TypedExpr>,
                    location: &SrcSpan,
                    env: &mut Env<'a>| {
        let name = var_name
            .to_owned()
            .expect("echo with no previous step in a pipe");
        echo(name, message, location, env)
    };

    let vars = env.current_scope_vars.clone();

    let mut prev_local_var_name = None;
    for a in all_assignments {
        match a.value.as_ref() {
            // An echo in a pipeline won't result in an assignment, instead it
            // just prints the previous variable assigned in the pipeline.
            TypedExpr::Echo {
                expression: None,
                message,
                location,
                ..
            } => documents.push(echo_doc(
                &prev_local_var_name,
                message.as_deref(),
                location,
                env,
            )),

            // Otherwise we assign the intermediate pipe value to a variable.
            _ => {
                let body = maybe_block_expr(&a.value, env).group();
                let name = env.next_local_var_name(&a.name);
                prev_local_var_name = Some(name.clone());
                documents.push(docvec![name, " = ", body]);
            }
        };
        documents.push(",".to_doc());
        documents.push(line());
    }

    match finally {
        TypedExpr::Echo {
            expression: None,
            message,
            location,
            ..
        } => documents.push(echo_doc(
            &prev_local_var_name,
            message.as_deref(),
            location,
            env,
        )),
        _ => documents.push(expr(finally, env)),
    }

    env.current_scope_vars = vars;

    documents.to_doc()
}

fn assignment<'a>(
    assignment: &'a TypedAssignment,
    env: &mut Env<'a>,
    position: Position,
) -> Document<'a> {
    match &assignment.kind {
        AssignmentKind::Let | AssignmentKind::Generated => {
            let_(&assignment.value, &assignment.pattern, env)
        }
        AssignmentKind::Assert {
            message, location, ..
        } => let_assert(
            &assignment.value,
            &assignment.pattern,
            env,
            message.as_ref(),
            position,
            *location,
        ),
    }
}

fn assert<'a>(assert: &'a TypedAssert, env: &mut Env<'a>) -> Document<'a> {
    let Assert {
        value,
        location,
        message,
    } = assert;

    let message = match message {
        Some(message) => expr(message, env),
        None => string("Assertion failed."),
    };

    let mut assignments = Vec::new();

    let (subject, mut fields) = match value {
        TypedExpr::Call { fun, arguments, .. } => {
            assert_call(fun, arguments, &mut assignments, env)
        }
        TypedExpr::BinOp {
            name, left, right, ..
        } => {
            let operator = match name {
                BinOp::And => {
                    return assert_and(left, right, message, *location, env);
                }
                BinOp::Or => {
                    return assert_or(left, right, message, *location, env);
                }
                BinOp::Eq => "=:=",
                BinOp::NotEq => "/=",
                BinOp::LtInt | BinOp::LtFloat => "<",
                BinOp::LtEqInt | BinOp::LtEqFloat => "=<",
                BinOp::GtInt | BinOp::GtFloat => ">",
                BinOp::GtEqInt | BinOp::GtEqFloat => ">=",
                BinOp::AddInt
                | BinOp::AddFloat
                | BinOp::SubInt
                | BinOp::SubFloat
                | BinOp::MultInt
                | BinOp::MultFloat
                | BinOp::DivInt
                | BinOp::DivFloat
                | BinOp::RemainderInt
                | BinOp::Concatenate => {
                    panic!("Non-boolean operators cannot appear here in well-typed code")
                }
            };

            let left_document = assign_to_variable(left, &mut assignments, env);
            let right_document = assign_to_variable(right, &mut assignments, env);
            (
                binop_documents(left_document.clone(), operator, right_document.clone()),
                vec![
                    ("kind", atom("binary_operator")),
                    ("operator", atom(name.name())),
                    (
                        "left",
                        asserted_expression(
                            AssertExpression::from_expression(left),
                            Some(left_document),
                            left.location(),
                        ),
                    ),
                    (
                        "right",
                        asserted_expression(
                            AssertExpression::from_expression(right),
                            Some(right_document),
                            right.location(),
                        ),
                    ),
                ],
            )
        }

        _ => (
            maybe_block_expr(value, env),
            vec![
                ("kind", atom("expression")),
                (
                    "expression",
                    asserted_expression(
                        AssertExpression::from_expression(value),
                        Some("false".to_doc()),
                        value.location(),
                    ),
                ),
            ],
        ),
    };

    fields.push(("start", location.start.to_doc()));
    fields.push(("'end'", value.location().end.to_doc()));
    fields.push(("expression_start", value.location().start.to_doc()));

    let clauses = docvec![
        line(),
        "true -> nil;",
        line(),
        "false -> ",
        erlang_error("assert", &message, *location, fields, env),
    ];

    docvec![
        assignments,
        "case ",
        subject,
        " of",
        clauses.nest(INDENT),
        line(),
        "end"
    ]
}

fn assert_call<'a>(
    function: &'a TypedExpr,
    arguments: &'a Vec<CallArg<TypedExpr>>,
    assignments: &mut Vec<Document<'a>>,
    env: &mut Env<'a>,
) -> (Document<'a>, Vec<(&'static str, Document<'a>)>) {
    let argument_variables = arguments
        .iter()
        .map(|argument| assign_to_variable(&argument.value, assignments, env))
        .collect_vec();

    let arguments = join(
        argument_variables
            .iter()
            .zip(arguments)
            .map(|(variable, argument)| {
                asserted_expression(
                    AssertExpression::from_expression(&argument.value),
                    Some(variable.clone()),
                    argument.location(),
                )
            }),
        break_(",", ", "),
    )
    .nest(INDENT)
    .surround("[", "]");

    (
        docs_arguments_call(function, argument_variables, env),
        vec![("kind", atom("function_call")), ("arguments", arguments)],
    )
}

/// In Gleam, the `&&` operator is short-circuiting, meaning that we can't
/// pre-evaluate both sides of it, and use them in the exception that is
/// thrown.
/// Instead, we need to implement this short-circuiting logic ourself.
///
/// If we short-circuit, we must leave the second expression unevaluated,
/// and signal that using the `unevaluated` variant, as detailed in the
/// exception format. For the first expression, we know it must be `false`,
/// otherwise we would have continued by evaluating the second expression.
///
/// Similarly, if we do evaluate the second expression and fail, we know
/// that the first expression must have evaluated to `true`, and the second
/// to `false`. This way, we avoid needing to evaluate either expression
/// twice.
///
/// The generated code then looks something like this:
/// ```erlang
/// case expr1 of
///   true -> case expr2 of
///     true -> true;
///     false -> <throw exception>
///   end;
///   false -> <throw exception>
/// end
/// ```
///
fn assert_and<'a>(
    left: &'a TypedExpr,
    right: &'a TypedExpr,
    message: Document<'a>,
    location: SrcSpan,
    env: &mut Env<'a>,
) -> Document<'a> {
    let left_kind = AssertExpression::from_expression(left);
    let right_kind = AssertExpression::from_expression(right);

    let fields_if_short_circuiting = vec![
        ("kind", atom("binary_operator")),
        ("operator", atom("&&")),
        (
            "left",
            asserted_expression(left_kind, Some("false".to_doc()), left.location()),
        ),
        (
            "right",
            asserted_expression(AssertExpression::Unevaluated, None, right.location()),
        ),
        ("start", location.start.to_doc()),
        ("'end'", right.location().end.to_doc()),
        ("expression_start", left.location().start.to_doc()),
    ];

    let fields = vec![
        ("kind", atom("binary_operator")),
        ("operator", atom("&&")),
        (
            "left",
            asserted_expression(left_kind, Some("true".to_doc()), left.location()),
        ),
        (
            "right",
            asserted_expression(right_kind, Some("false".to_doc()), right.location()),
        ),
        ("start", location.start.to_doc()),
        ("'end'", right.location().end.to_doc()),
        ("expression_start", left.location().start.to_doc()),
    ];

    let right_clauses = docvec![
        line(),
        "true -> nil;",
        line(),
        "false -> ",
        erlang_error("assert", &message, location, fields, env),
    ];

    let left_clauses = docvec![
        line(),
        "true -> ",
        docvec![
            "case ",
            maybe_block_expr(right, env),
            " of",
            right_clauses.nest(INDENT),
            line(),
            "end"
        ]
        .nest(INDENT),
        ";",
        line(),
        "false -> ",
        erlang_error(
            "assert",
            &message,
            location,
            fields_if_short_circuiting,
            env
        ),
    ];

    docvec![
        "case ",
        maybe_block_expr(left, env),
        " of",
        left_clauses.nest(INDENT),
        line(),
        "end"
    ]
}

/// Similar to `&&`, `||` is also short-circuiting in Gleam. However, if `||`
/// short-circuits, that's because the first expression evaluated to `true`,
/// meaning the whole assertion succeeds. This allows us to directly use Erlang's
/// `orelse` operator as the subject of the `case` expression.
///
/// The only difference is that due to the nature of `||`, if the assertion fails,
/// we know that both sides must have evaluated to `false`, so we don't
/// need to store the values of them in variables beforehand.
fn assert_or<'a>(
    left: &'a TypedExpr,
    right: &'a TypedExpr,
    message: Document<'a>,
    location: SrcSpan,
    env: &mut Env<'a>,
) -> Document<'a> {
    let fields = vec![
        ("kind", atom("binary_operator")),
        ("operator", atom("||")),
        (
            "left",
            asserted_expression(
                AssertExpression::from_expression(left),
                Some("false".to_doc()),
                left.location(),
            ),
        ),
        (
            "right",
            asserted_expression(
                AssertExpression::from_expression(right),
                Some("false".to_doc()),
                right.location(),
            ),
        ),
        ("start", location.start.to_doc()),
        ("'end'", right.location().end.to_doc()),
        ("expression_start", left.location().start.to_doc()),
    ];

    let clauses = docvec![
        line(),
        "true -> nil;",
        line(),
        "false -> ",
        erlang_error("assert", &message, location, fields, env),
    ];

    docvec![
        "case ",
        docvec![
            maybe_block_expr(left, env),
            " orelse ",
            maybe_block_expr(right, env)
        ]
        .nest(INDENT),
        " of",
        clauses.nest(INDENT),
        line(),
        "end"
    ]
}

fn assign_to_variable<'a>(
    value: &'a TypedExpr,
    assignments: &mut Vec<Document<'a>>,
    env: &mut Env<'a>,
) -> Document<'a> {
    if value.is_var() {
        expr(value, env)
    } else {
        let value = maybe_block_expr(value, env);
        let variable = env.next_local_var_name(ASSERT_SUBJECT_VARIABLE);
        let definition = docvec![variable.clone(), " = ", value, ",", line()];
        assignments.push(definition);
        variable
    }
}

#[derive(Debug, Clone, Copy)]
enum AssertExpression {
    Literal,
    Expression,
    Unevaluated,
}

impl AssertExpression {
    fn from_expression(expression: &TypedExpr) -> Self {
        if expression.is_literal() {
            Self::Literal
        } else {
            Self::Expression
        }
    }
}

fn asserted_expression(
    kind: AssertExpression,
    value: Option<Document<'_>>,
    location: SrcSpan,
) -> Document<'_> {
    let kind = match kind {
        AssertExpression::Literal => atom("literal"),
        AssertExpression::Expression => atom("expression"),
        AssertExpression::Unevaluated => atom("unevaluated"),
    };

    let start = location.start.to_doc();
    let end = location.end.to_doc();

    let value_field = if let Some(value) = value {
        docvec!["value => ", value, ",", line()]
    } else {
        nil()
    };

    let fields_doc = docvec![
        "kind => ",
        kind,
        ",",
        line(),
        value_field,
        "start => ",
        start,
        ",",
        line(),
        // `end` is a keyword in Erlang, so we have to quote it
        "'end' => ",
        end,
        line(),
    ];

    "#{".to_doc()
        .append(fields_doc.group().nest(INDENT))
        .append("}")
}

fn negate_with<'a>(op: &'static str, value: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    docvec![op, maybe_block_expr(value, env)]
}

fn tuple_index<'a>(tuple: &'a TypedExpr, index: u64, env: &mut Env<'a>) -> Document<'a> {
    let index_doc = eco_format!("{}", (index + 1)).to_doc();
    let tuple_doc = maybe_block_expr(tuple, env);
    "erlang:element"
        .to_doc()
        .append(wrap_arguments([index_doc, tuple_doc]))
}

fn module_select_fn<'a>(type_: Arc<Type>, module_name: &'a str, label: &'a str) -> Document<'a> {
    match crate::type_::collapse_links(type_).as_ref() {
        Type::Fn { arguments, .. } => function_reference(Some(module_name), label, arguments.len()),

        _ => module_name_atom(module_name)
            .append(":")
            .append(atom(label))
            .append("()"),
    }
}

fn fun<'a>(
    arguments: &'a [TypedArg],
    body: &'a [TypedStatement],
    env: &mut Env<'a>,
) -> Document<'a> {
    let current_scope_vars = env.current_scope_vars.clone();
    let doc = "fun"
        .to_doc()
        .append(fun_arguments(arguments, env).append(" ->"))
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

fn incrementing_arguments_list(arity: usize) -> EcoString {
    let arguments = (0..arity).map(|c| format!("Field@{c}"));
    Itertools::intersperse(arguments, ", ".into())
        .collect::<String>()
        .into()
}

fn variable_name(name: &str) -> EcoString {
    let mut chars = name.chars();
    let first_char = chars.next();
    let first_uppercased = first_char.into_iter().flat_map(char::to_uppercase);

    first_uppercased.chain(chars).collect::<EcoString>()
}

/// When rendering a type variable to an erlang type spec we need all type variables with the
/// same id to end up with the same name in the generated erlang.
/// This function converts a usize into base 26 A-Z for this purpose.
fn id_to_type_var(id: u64) -> Document<'static> {
    if id < 26 {
        let mut name = EcoString::from("");
        name.push(char::from_u32((id % 26 + 65) as u32).expect("id_to_type_var 0"));
        return name.to_doc();
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

// Includes the functions that are autogenerated by erlang itself
pub fn escape_erlang_existing_name(name: &str) -> &str {
    match name {
        "module_info" => "moduleInfo",
        _ => name,
    }
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
    for type_ in types {
        type_var_ids(type_, &mut ids);
    }
    ids
}

fn result_type_var_ids(ids: &mut HashMap<u64, u64>, arg_ok: &Type, arg_err: &Type) {
    let mut ok_ids = HashMap::new();
    type_var_ids(arg_ok, &mut ok_ids);

    let mut err_ids = HashMap::new();
    type_var_ids(arg_err, &mut err_ids);

    let mut result_counts = ok_ids;
    for (id, count) in err_ids {
        let _ = result_counts
            .entry(id)
            .and_modify(|current_count| {
                if *current_count < count {
                    *current_count = count;
                }
            })
            .or_insert(count);
    }
    for (id, count) in result_counts {
        let _ = ids
            .entry(id)
            .and_modify(|current_count| {
                *current_count += count;
            })
            .or_insert(count);
    }
}

fn type_var_ids(type_: &Type, ids: &mut HashMap<u64, u64>) {
    match type_ {
        Type::Var { type_ } => match type_.borrow().deref() {
            TypeVar::Generic { id, .. } | TypeVar::Unbound { id, .. } => {
                let count = ids.entry(*id).or_insert(0);
                *count += 1;
            }
            TypeVar::Link { type_ } => type_var_ids(type_, ids),
        },
        Type::Named {
            arguments,
            module,
            name,
            ..
        } => match arguments[..] {
            [ref arg_ok, ref arg_err] if is_prelude_module(module) && name == "Result" => {
                result_type_var_ids(ids, arg_ok, arg_err)
            }
            _ => {
                for argument in arguments {
                    type_var_ids(argument, ids)
                }
            }
        },
        Type::Fn { arguments, return_ } => {
            for argument in arguments {
                type_var_ids(argument, ids)
            }
            type_var_ids(return_, ids);
        }
        Type::Tuple { elements } => {
            for element in elements {
                type_var_ids(element, ids)
            }
        }
    }
}

fn erl_safe_type_name(mut name: EcoString) -> EcoString {
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
            Type::Var { type_ } => self.print_var(&type_.borrow()),

            Type::Named {
                name,
                module,
                arguments,
                ..
            } if is_prelude_module(module) => self.print_prelude_type(name, arguments),

            Type::Named {
                name,
                module,
                arguments,
                ..
            } => self.print_type_app(module, name, arguments),

            Type::Fn { arguments, return_ } => self.print_fn(arguments, return_),

            Type::Tuple { elements } => tuple(elements.iter().map(|element| self.print(element))),
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
            TypeVar::Link { type_ } => self.print(type_),
        }
    }

    fn print_prelude_type(&self, name: &str, arguments: &[Arc<Type>]) -> Document<'static> {
        match name {
            "Nil" => "nil".to_doc(),
            "Int" | "UtfCodepoint" => "integer()".to_doc(),
            "String" => "binary()".to_doc(),
            "Bool" => "boolean()".to_doc(),
            "Float" => "float()".to_doc(),
            "BitArray" => "bitstring()".to_doc(),
            "List" => {
                let arg0 = self.print(arguments.first().expect("print_prelude_type list"));
                "list(".to_doc().append(arg0).append(")")
            }
            "Result" => match arguments {
                [arg_ok, arg_err] => {
                    let ok = tuple(["ok".to_doc(), self.print(arg_ok)]);
                    let error = tuple(["error".to_doc(), self.print(arg_err)]);
                    docvec![ok, break_(" |", " | "), error].nest(INDENT).group()
                }
                _ => panic!("print_prelude_type result expects ok and err"),
            },
            // Getting here should mean we either forgot a built-in type or there is a
            // compiler error
            name => panic!("{name} is not a built-in type."),
        }
    }

    fn print_type_app(
        &self,
        module: &str,
        name: &str,
        arguments: &[Arc<Type>],
    ) -> Document<'static> {
        let arguments = join(
            arguments.iter().map(|argument| self.print(argument)),
            ", ".to_doc(),
        );
        let name = erl_safe_type_name(to_snake_case(name)).to_doc();
        if self.current_module == module {
            docvec![name, "(", arguments, ")"]
        } else {
            docvec![module_name_atom(module), ":", name, "(", arguments, ")"]
        }
    }

    fn print_fn(&self, arguments: &[Arc<Type>], return_: &Type) -> Document<'static> {
        let arguments = join(
            arguments.iter().map(|argument| self.print(argument)),
            ", ".to_doc(),
        );
        let return_ = self.print(return_);
        "fun(("
            .to_doc()
            .append(arguments)
            .append(") -> ")
            .append(return_)
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
) -> im::HashSet<EcoString> {
    let mut overridden_publicity = im::HashSet::new();

    for definition in module.definitions.iter() {
        if let Definition::ModuleConstant(constant) = definition
            && constant.publicity.is_importable()
        {
            find_referenced_private_functions(&constant.value, &mut overridden_publicity)
        }
    }
    overridden_publicity
}

fn find_referenced_private_functions(
    constant: &TypedConstant,
    already_found: &mut im::HashSet<EcoString>,
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
            if let Some(ValueConstructor { type_, .. }) = constructor.as_deref()
                && let Type::Fn { .. } = **type_
            {
                let _ = already_found.insert(name.clone());
            }
        }

        TypedConstant::Record { arguments, .. } => arguments
            .iter()
            .for_each(|argument| find_referenced_private_functions(&argument.value, already_found)),

        TypedConstant::StringConcatenation { left, right, .. } => {
            find_referenced_private_functions(left, already_found);
            find_referenced_private_functions(right, already_found);
        }

        Constant::Tuple { elements, .. } | Constant::List { elements, .. } => elements
            .iter()
            .for_each(|element| find_referenced_private_functions(element, already_found)),
    }
}
