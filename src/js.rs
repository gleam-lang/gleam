#[cfg(test)]
mod tests;

use crate::{
    ast::*,
    error::GleamExpect,
    fs::{OutputFile, Utf8Writer},
    line_numbers::LineNumbers,
    pretty::*,
    project::{self, Analysed},
    type_::{
        ModuleValueConstructor, PatternConstructor, Type, TypeVar, ValueConstructor,
        ValueConstructorVariant,
    },
    Result,
};
use itertools::Itertools;


pub fn module(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    writer: &mut impl Utf8Writer,
) -> Result<()> {
    let statements = concat(Itertools::intersperse(
        module
            .statements
            .iter()
            .flat_map(|s| statement(&module.name, s, &module.name, line_numbers)),
        lines(2),
    ));

    statements.pretty_print(80, writer)
}

fn statement<'a>(
    current_module: &'a [String],
    statement: &'a TypedStatement,
    module: &'a [String],
    line_numbers: &'a LineNumbers,
) -> Option<Document<'a>> {
    match statement {
        Statement::TypeAlias { .. } => None,
        Statement::CustomType { .. } => None,
        Statement::Import { .. } => None,
        Statement::ExternalType { .. } => None,
        Statement::ModuleConstant { .. } => None,

        _ => unimplemented!()
        // Statement::Fn {
        //     arguments: args,
        //     name,
        //     body,
        //     return_type,
        //     ..
        // } => Some(mod_fun(name, args, body, module, return_type, line_numbers)),

        // Statement::ExternalFn { public: false, .. } => None,
        // Statement::ExternalFn {
        //     fun,
        //     module,
        //     arguments: args,
        //     name,
        //     return_type,
        //     ..
        // } => Some(external_fun(
        //     current_module,
        //     name,
        //     module,
        //     fun,
        //     args,
        //     return_type,
        // )),
    }
}