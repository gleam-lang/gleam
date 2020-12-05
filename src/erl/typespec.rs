use crate::{
    ast::*,
    erl::{atom, is_erlang_reserved_word, wrap_args},
    pretty::*,
    typ::Type,
};

use heck::{CamelCase, SnakeCase};
use itertools::Itertools;

// Creates the contents of the `export_type([..])` line
pub fn type_exports(module: &TypedModule) -> Document {
    concat(
        module
            .statements
            .iter()
            .flat_map(|s| match s {
                Statement::CustomType {
                    public: true,
                    name,
                    constructors,
                    ..
                } => {
                    // We only count the params that we can find in the constructors as we can only
                    // put those ones into the type definition as otherwise we'll have unused type
                    // param warnings
                    let constructor_params = extract_params_from_constructors(&constructors);
                    Some((name.to_snake_case(), constructor_params.len()))
                }

                _ => None,
            })
            .map(|(n, a)| atom(n).append("/").append(a))
            .intersperse(", ".to_doc()),
    )
}

pub fn type_name_to_atom(name: &str) -> String {
    let name = &name.to_snake_case();
    if is_erlang_reserved_word(name) {
        format!("'{}'", name)
    } else {
        name.to_string()
    }
}

/* Generate an erlang '-type' line for this custom type. There are three options for entries:

   1. Basic constructors with no arguments that we convert to a single atom

        -type option(A) :: ... | none

   2. Constructors where all the arguments are labelled, which we treat as records, and output a
      type which uses the record definition that has already been output and included in this file:

        -type queue(Element) :: #queue{in :: Element, out :: Element} | ..

      We declare and pass through any type parameters so that the type has the right number of
      arguments and the type parameters are used.

   3. Constructors some or all of the arguments are not labelled, which are output as tag tuples:

        -type option(A) :: {some, A} | ..
*/
pub fn type_(name: &str, _parameters: &[String], constructors: &[RecordConstructor]) -> Document {
    let found_parameters = extract_params_from_constructors(constructors);

    "-type "
        .to_doc()
        .append(name.to_snake_case())
        .append(wrap_args(
            found_parameters
                .iter()
                .map(|name| name.to_camel_case().to_doc()),
        ))
        .append(" :: ")
        .append(concat(
            constructors
                .iter()
                .map(|constructor| {
                    if constructor.args.is_empty() {
                        // Basic single-atom constructor
                        type_name_to_atom(&constructor.name).to_doc()
                    } else if is_record(&constructor) {
                        record_type(&constructor)
                    } else {
                        non_record_type(&constructor)
                    }
                })
                .intersperse(delim(" |")),
        ))
        .append(".")
}

fn record_type(constructor: &RecordConstructor) -> Document {
    let name = constructor.name.to_snake_case();
    let contents = type_record_definition(&constructor.args);

    "#".to_doc().append(name).append(contents)
}

fn non_record_type(constructor: &RecordConstructor) -> Document {
    constructor
        .name
        .to_snake_case()
        .to_doc()
        .append(delim(","))
        .append(concat(
            constructor
                .args
                .iter()
                .map(|(_, ast, ..)| type_ast_name(ast, None))
                .intersperse(delim(",")),
        ))
        .surround("{", "}")
}

fn is_record(constructor: &RecordConstructor) -> bool {
    for (label, ..) in constructor.args.iter() {
        match label {
            Some(_) => return true,
            None => return false,
        }
    }

    false
}

fn type_record_definition(args: &[(Option<String>, TypeAst, SrcSpan)]) -> Document {
    concat(
        args.iter()
            .map(|(label, ast, ..)| {
                label
                    .clone()
                    .unwrap_or("".to_string())
                    .to_doc()
                    .append(" :: ")
                    .append(type_ast_name(&ast, None))
            })
            .intersperse(", ".to_doc()),
    )
    .surround("{", "}")
}

// Generates the spec for a regular Gleam function
pub fn fun_spec(name: &str, args: &[TypedArg], return_type: &Type) -> Document {
    "-spec "
        .to_doc()
        .append(atom(name.to_string()))
        .append(spec_args(args))
        .append(" -> ")
        .append(typ_to_erl(return_type))
        .append(".")
}

fn spec_args(args: &[TypedArg]) -> Document {
    wrap_args(args.iter().map(|a| typ_to_erl(&*a.typ)))
}

fn typ_to_erl(typ: &Type) -> Document {
    match typ {
        Type::App { name, .. } => name.to_snake_case().to_doc(),
        _ => "term".to_doc(),
    }
}

// Generates the spec for an external Gleam function that wraps an erlang one
pub fn external_fun_spec(name: &str, args: &[ExternalFnArg], return_type: &Type) -> Document {
    // Find all parameters used in the return type - these are the only ones we can show in the
    // args otherwise they are flagged as unused
    let return_type_params = extract_params_from_type(return_type);
    "-spec "
        .to_doc()
        .append(atom(name.to_string()))
        .append(external_spec_args(args, &return_type_params))
        .append(" -> ")
        .append(typ_to_erl(return_type))
        .append(".")
}

fn external_spec_args(args: &[ExternalFnArg], allowed_params: &[String]) -> Document {
    wrap_args(
        args.iter()
            .map(|arg| type_ast_name(&arg.typ, Some(allowed_params))),
    )
}

// Extracts the type names from a TypeAst - We optionally pass in a list of type parameters that we
// are aware of.
fn type_ast_name(ast: &TypeAst, allowed_params: Option<&[String]>) -> Document {
    match ast {
        TypeAst::Var { name, .. } =>
        // If we provide a list of allowed type parameter names then we're in a context with
        // limited possible type parameter names and if we come across one that isn't 'allowed'
        // then we're better just outputting 'any' instead. This prevents unknown or unused
        // type parameter warnings from erlang
        {
            allowed_params
                .map(|slice| {
                    if slice.contains(name) {
                        name.to_camel_case()
                    } else {
                        "any".to_string()
                    }
                })
                .unwrap_or(name.to_camel_case())
                .to_doc()
        }
        TypeAst::Constructor { name, args, .. } => type_to_erl(name, &args, allowed_params),
        _ => "term".to_doc(),
    }
}

fn type_to_erl(name: &str, args: &[TypeAst], allowed_params: Option<&[String]>) -> Document {
    match name {
        "String" => "binary()".to_doc(),
        "Int" => "integer()".to_doc(),
        "Bool" => "boolean()".to_doc(),
        "List" => "list".to_doc().append(wrap_args(
            args.iter().map(|arg| type_ast_name(arg, allowed_params)),
        )),
        "Map" => concat(
            args.iter()
                .map(|arg| type_ast_name(arg, allowed_params))
                .intersperse(" => ".to_doc()),
        )
        .surround("#{", "}"),
        _ => name.to_snake_case().to_doc(),
    }
}

/* Extraction functions - There are various places where we want to make sure that we're only
   outputting type parameters if we know they are going to be used in order to avoid unused type
   parameter warnings. Consequently we have these functions to help us walk the type trees and find
   what we need.
*/
fn extract_params_from_constructors(constructors: &[RecordConstructor]) -> Vec<String> {
    let mut set = std::collections::HashSet::new();
    constructors
        .iter()
        .flat_map(|constructor| {
            constructor
                .args
                .iter()
                .flat_map(|(_, ast, ..)| extract_params_from_ast(&ast))
                .collect::<Vec<String>>()
        })
        .filter(|name| set.insert(name.clone()))
        .collect::<Vec<String>>()
}

fn extract_params_from_ast(ast: &TypeAst) -> Vec<String> {
    match ast {
        TypeAst::Var { name, .. } => vec![name.to_camel_case()],
        TypeAst::Constructor { args, .. } => args
            .iter()
            .flat_map(|ast| extract_params_from_ast(&ast))
            .collect::<Vec<String>>(),
        _ => Vec::new(),
    }
}

fn extract_params_from_type(typ: &Type) -> Vec<String> {
    match typ {
        Type::App { name, .. } => vec![name.to_camel_case()],
        _ => Vec::new(),
    }
}
