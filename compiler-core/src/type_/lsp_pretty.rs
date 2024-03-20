use std::sync::Arc;

use ecow::EcoString;
use im::HashMap;
use itertools::Itertools;

use crate::{
    //ast::TypeAst,
    pretty::{break_, concat, nil, Document, Documentable},
};

use super::{pretty::Printer, Type, TypeVar};

const INDENT: isize = 2;

#[derive(Debug)]
pub struct LspPrinter<'a> {
    pretty_printer: Printer,
    type_qualifiers: &'a HashMap<EcoString, EcoString>,
    module_qualifiers: &'a HashMap<EcoString, EcoString>,
}

impl<'a> LspPrinter<'a> {
    pub fn new(
        type_qualifiers: &'a HashMap<EcoString, EcoString>,
        module_qualifiers: &'a HashMap<EcoString, EcoString>,
    ) -> Self {
        LspPrinter {
            pretty_printer: Printer::default(),
            type_qualifiers,
            module_qualifiers,
        }
    }

    /// Render a Type as a well formatted string.
    ///
    pub fn pretty_print(&mut self, typ: &Type, initial_indent: usize) -> String {
        let mut buffer = String::with_capacity(initial_indent);
        for _ in 0..initial_indent {
            buffer.push(' ');
        }
        buffer
            .to_doc()
            .append(self.print(typ))
            .nest(initial_indent as isize)
            .to_pretty_string(80)
    }

    pub fn print(&mut self, typ: &Type) -> Document<'a> {
        match typ {
            Type::Named {
                name, args, module, ..
            } => {
                let key = module.clone() + "." + name.clone();
                let doc = self.type_qualifiers.get(&key).unwrap_or(&key).to_doc();
                if args.is_empty() {
                    if module == "gleam" {
                        // Ignoring module names for in-built gleam types like Int
                        name.to_doc()
                    } else if self.type_qualifiers.contains_key(&key) {
                        // Imported type has been qualified. eg: import mod1.{type Cat as c}
                        doc
                    } else {
                        // Need to check if imported module has been qualified eg: import mod1 as m
                        (self.module_qualifiers.get(module).unwrap_or(module).clone()
                            + "."
                            + name.clone())
                        .to_doc()
                    }
                } else {
                    doc.append("(")
                        .append(self.args_to_gleam_doc(args))
                        .append(")")
                }
            }
            Type::Fn { args, retrn } => "fn("
                .to_doc()
                .append(self.args_to_gleam_doc(args))
                .append(") ->")
                .append(
                    break_("", " ")
                        .append(self.print(retrn))
                        .nest(INDENT)
                        .group(),
                ),

            Type::Var { type_: typ, .. } => match *typ.borrow() {
                TypeVar::Link { type_: ref typ, .. } => self.print(typ),
                TypeVar::Unbound { id, .. } | TypeVar::Generic { id, .. } => {
                    self.pretty_printer.generic_type_var(id)
                }
            },

            Type::Tuple { elems, .. } => self.args_to_gleam_doc(elems).surround("#(", ")"),
        }
    }

    fn args_to_gleam_doc(&mut self, args: &[Arc<Type>]) -> Document<'a> {
        if args.is_empty() {
            return nil();
        }

        let args = concat(Itertools::intersperse(
            args.iter().map(|t| self.print(t).group()),
            break_(",", ", "),
        ));
        break_("", "")
            .append(args)
            .nest(INDENT)
            .append(break_(",", ""))
            .group()
    }
}
