use std::sync::Arc;

use ecow::EcoString;
use im::HashMap;
use itertools::Itertools;


use crate::ast::TypeAst;

use super::{pretty::Printer, Type, TypeVar};

#[derive(Debug)]
pub struct LspPrinter<'a, > {
    pretty_printer: Printer,
    type_qualifiers: &'a HashMap<EcoString, EcoString>,
    module_qualifiers: &'a HashMap<EcoString, EcoString>,
    type_parameters: &'a HashMap<u64, TypeAst>,
}

impl<'a> LspPrinter<'a> {
    pub fn new(
        type_qualifiers: &'a HashMap<EcoString, EcoString>,
        module_qualifiers: &'a HashMap<EcoString, EcoString>,
        type_parameters: &'a HashMap<u64, TypeAst>,
    ) -> Self {
        LspPrinter {
            pretty_printer: Printer::default(),
            type_qualifiers,
            module_qualifiers,
            type_parameters
        }
    }

    /// Render a Type as a well formatted string.
    pub fn pretty_print(&mut self, typ: &Type) -> String {
        self.print(typ)
    }

    pub fn print(&mut self, typ: &Type) -> String {
        match typ {
            Type::Named {
                name, args, module, ..
            } => {
                let key = module.clone() + "." + name.clone();
                let res = self.type_qualifiers.get(&key).unwrap_or(&key).to_string();
                if args.is_empty() {
                    if module == "gleam" {
                        // Ignoring module names for in-built gleam types like Int
                        name.to_string()
                    } else if self.type_qualifiers.contains_key(&key) {
                        // Imported type has been qualified. eg: import mod1.{type Cat as c}
                        res.to_string()
                    } else {
                        // Need to check if imported module has been qualified eg: import mod1 as m
                        (self.module_qualifiers.get(module).unwrap_or(module).clone()
                            + "."
                            + name.clone())
                        .to_string()
                    }
                } else {
                    res + "(" + &self.args_to_string(args) + ")"
                }
            }
            Type::Fn { args, retrn } => {
                String::from("fn(") + &self.args_to_string(args) + ") ->" + &(self.print(retrn))
            }

            Type::Var { type_: typ, .. } => match *typ.borrow() {
                TypeVar::Link { type_: ref typ, .. } => self.print(typ),
                TypeVar::Unbound { id, .. } | TypeVar::Generic { id, .. } => {
                    if let Some(crate::ast::TypeAst::Var(type_var)) = self.type_parameters.get(&id) {
                        return type_var.name.to_string();
                    }
                    self
                    .pretty_printer
                    .generic_type_var(id)
                    .to_pretty_string(80)},
            },

            Type::Tuple { elems, .. } => "#(".to_string() + &self.args_to_string(elems) + ")",
        }
    }

    fn args_to_string(&mut self, args: &[Arc<Type>]) -> String {
        if args.is_empty() {
            return String::new();
        }

        let args =
            Itertools::intersperse(args.iter().map(|t| self.print(t)), ", ".to_string()).collect();
        args
    }
}
