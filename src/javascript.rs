mod expression;
mod pattern;
#[cfg(test)]
mod tests;

use crate::{ast::*, error::GleamExpect, fs::Utf8Writer, line_numbers::LineNumbers, pretty::*};
use itertools::Itertools;

const INDENT: isize = 2;

const DEEP_EQUAL: &str = "

function $equal(x, y) {
  let toCheck = [x, y];
  while (toCheck) {
    let a = toCheck.pop();
    let b = toCheck.pop();
    if (a === b) return true;
    if (!$is_object(a) || !$is_object(b)) return false;
    for (let k of Object.keys(a)) {
      toCheck.push(a[k], b[k]);
    }
  }
  return true;
}

function $is_object(object) {
  return object !== null && typeof object === 'object';
}";

const FUNCTION_DIVIDE: &str = "

function $divide(a, b) {
  if (b === 0) return 0;
  return a / b;
}";

pub type Output<'a> = Result<Document<'a>, Error>;

#[derive(Debug)]
pub struct Generator<'a> {
    line_numbers: &'a LineNumbers,
    module: &'a TypedModule,
    float_division_used: bool,
    object_equality_used: bool,
    initial_scope_vars: im::HashMap<String, usize>,
}

impl<'a> Generator<'a> {
    pub fn new(line_numbers: &'a LineNumbers, module: &'a TypedModule) -> Self {
        Self {
            line_numbers,
            module,
            float_division_used: false,
            object_equality_used: false,
            initial_scope_vars: Default::default(),
        }
    }

    pub fn compile(&mut self) -> Output<'a> {
        let statements = std::iter::once(Ok(r#""use strict";"#.to_doc())).chain(
            self.module
                .statements
                .iter()
                .flat_map(|s| self.statement(s)),
        );

        // Two lines between each statement
        let statements = Itertools::intersperse(statements, Ok(lines(2)));
        let mut statements = statements.collect::<Result<Vec<_>, _>>()?;

        // If float division has been used render an appropriate function
        if self.float_division_used {
            statements.push(FUNCTION_DIVIDE.to_doc());
        };

        if self.object_equality_used {
            statements.push(DEEP_EQUAL.to_doc());
        };

        statements.push(line());
        Ok(statements.to_doc())
    }

    pub fn statement(&mut self, statement: &'a TypedStatement) -> Option<Output<'a>> {
        match statement {
            Statement::TypeAlias { .. } => None,
            Statement::CustomType { .. } => None,
            Statement::Import {
                module,
                as_name,
                unqualified,
                ..
            } => Some(Ok(self.import(module, as_name, unqualified))),
            Statement::ExternalType { .. } => None,
            Statement::ModuleConstant {
                public,
                name,
                value,
                ..
            } => Some(self.module_constant(*public, name, value)),
            Statement::Fn {
                arguments,
                name,
                body,
                public,
                ..
            } => Some(self.module_function(*public, name, arguments, body)),
            Statement::ExternalFn {
                public,
                name,
                arguments,
                module,
                fun,
                ..
            } => Some(Ok(
                self.external_function(*public, name, arguments, module, fun)
            )),
        }
    }

    fn import(
        &mut self,
        module: &'a [String],
        as_name: &'a Option<String>,
        unqualified: &'a [UnqualifiedImport],
    ) -> Document<'a> {
        let module_name = as_name
            .as_ref()
            .map(|n| n.as_str().to_doc())
            .unwrap_or_else(|| {
                module
                    .last()
                    .gleam_expect("JavaScript code generator could not identify module name.")
                    .to_doc()
            });
        let path: Document<'a> = docvec![
            "\"",
            match self.module.name.len() {
                1 => "./".to_doc(),
                _ => Document::String("../".repeat(module.len() - 1)),
            },
            Document::String(module.join("/")),
            ".js\""
        ];

        let import_line = docvec!["import * as ", module_name.clone(), " from ", path, ";"];
        let mut any_unqualified_values = false;
        let matches = unqualified
            .iter()
            .filter(|i| {
                // We do not create a JS import for uppercase names are they are
                // type or record constructors, both of which are not used at runtime
                i.name
                    .chars()
                    .next()
                    .map(char::is_lowercase)
                    .unwrap_or(false)
            })
            .map(|i| {
                any_unqualified_values = true;
                (i.name.to_doc(), i.as_name.as_ref().map(|n| n.to_doc()))
            });

        let matches = wrap_object(matches);
        if any_unqualified_values {
            docvec![
                import_line,
                line(),
                "const ",
                matches,
                " = ",
                module_name,
                ";"
            ]
        } else {
            import_line
        }
    }

    fn module_constant(
        &mut self,
        public: bool,
        name: &'a str,
        value: &'a TypedConstant,
    ) -> Output<'a> {
        let head = if public { "export const " } else { "const " };
        let _ = self.initial_scope_vars.insert(name.to_string(), 0);
        Ok(docvec![
            head,
            name,
            " = ",
            expression::constant_expression(value)?,
            ";",
        ])
    }

    fn module_function(
        &mut self,
        public: bool,
        name: &'a str,
        args: &'a [TypedArg],
        body: &'a TypedExpr,
    ) -> Output<'a> {
        let argument_names = args
            .iter()
            .map(|arg| arg.names.get_variable_name())
            .collect();
        let mut generator = expression::Generator::new(
            &self.module.name,
            self.line_numbers,
            name,
            argument_names,
            &mut self.float_division_used,
            &mut self.object_equality_used,
            self.initial_scope_vars.clone(),
        );
        let head = if public {
            "export function "
        } else {
            "function "
        };
        Ok(docvec![
            head,
            name,
            fun_args(args),
            " {",
            docvec![line(), generator.function_body(body)?]
                .nest(INDENT)
                .group(),
            line(),
            "}",
        ])
    }

    fn external_function<T>(
        &mut self,
        public: bool,
        name: &'a str,
        arguments: &'a [ExternalFnArg<T>],
        module: &'a str,
        fun: &'a str,
    ) -> Document<'a> {
        if module == "" {
            self.global_external_function(public, name, arguments, fun)
        } else {
            self.imported_external_function(public, name, module, fun)
        }
    }

    fn imported_external_function(
        &mut self,
        public: bool,
        name: &'a str,
        module: &'a str,
        fun: &'a str,
    ) -> Document<'a> {
        let import = if name == fun {
            docvec!["import { ", name, r#" } from ""#, module, r#"";"#]
        } else {
            docvec![
                "import { ",
                fun,
                " as ",
                name,
                r#" } from ""#,
                module,
                r#"";"#
            ]
        };
        if public {
            import
                .append(line())
                .append("export { ")
                .append(name)
                .append(" };")
        } else {
            import
        }
    }

    fn global_external_function<T>(
        &mut self,
        public: bool,
        name: &'a str,
        arguments: &'a [ExternalFnArg<T>],
        fun: &'a str,
    ) -> Document<'a> {
        let head = if public {
            "export function "
        } else {
            "function "
        };
        let arguments = external_fn_args(arguments);
        let body = docvec!["return ", fun, arguments.clone()];
        docvec![
            head,
            name,
            arguments,
            " {",
            docvec![line(), body].nest(INDENT).group(),
            line(),
            "}",
        ]
    }
}

fn external_fn_args<'a, T>(arguments: &'a [ExternalFnArg<T>]) -> Document<'a> {
    wrap_args(arguments.iter().enumerate().map(|a| {
        match a {
            (index, ExternalFnArg { label, .. }) => label
                .as_ref()
                .map(|l| l.as_str().to_doc())
                .unwrap_or_else(|| Document::String(format!("arg{}", index))),
        }
    }))
}

pub fn module(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    writer: &mut impl Utf8Writer,
) -> Result<(), crate::Error> {
    Generator::new(line_numbers, module)
        .compile()
        .map_err(crate::Error::JavaScript)?
        .pretty_print(80, writer)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Unsupported { feature: String },
}

fn unsupported<M: ToString, T>(label: M) -> Result<T, Error> {
    Err(Error::Unsupported {
        feature: label.to_string(),
    })
}

fn fun_args(args: &'_ [TypedArg]) -> Document<'_> {
    wrap_args(args.iter().map(|a| match &a.names {
        ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => "_".to_doc(),
        ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => name.to_doc(),
    }))
}

fn wrap_args<'a, I>(args: I) -> Document<'a>
where
    I: Iterator<Item = Document<'a>>,
{
    break_("", "")
        .append(concat(Itertools::intersperse(args, break_(",", ", "))))
        .nest(INDENT)
        .append(break_("", ""))
        .surround("(", ")")
        .group()
}

fn wrap_object<'a>(
    items: impl Iterator<Item = (Document<'a>, Option<Document<'a>>)>,
) -> Document<'a> {
    let fields = items.map(|(key, value)| match value {
        Some(value) => docvec![key, ": ", value,],
        None => key.to_doc(),
    });

    docvec![
        docvec![
            "{",
            break_("", " "),
            concat(Itertools::intersperse(fields, break_(",", ", ")))
        ]
        .nest(INDENT)
        .append(break_("", " "))
        .group(),
        "}"
    ]
}
