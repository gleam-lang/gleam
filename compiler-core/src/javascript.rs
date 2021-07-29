mod expression;
mod pattern;
#[cfg(test)]
mod tests;

use std::path::Path;

use crate::{ast::*, docvec, io::Utf8Writer, line_numbers::LineNumbers, pretty::*};
use heck::CamelCase;
use itertools::Itertools;

const INDENT: isize = 2;

const DEEP_EQUAL: &str = "

function $equal(x, y) {
  let toCheck = [x, y];
  while (toCheck) {
    let a = toCheck.pop();
    let b = toCheck.pop();
    if (a === b) return true;
    if (a instanceof ArrayBuffer && b instanceof ArrayBuffer) {
      return $bits_equal(a, b)
    }
    if (!$is_object(a) || !$is_object(b)) return false;
    if (a.length !== b.length) return false;
    for (let k of Object.keys(a)) {
      toCheck.push(a[k], b[k]);
    }
  }
  return true;
}

function $bits_equal(x, y) {
  let a = new DataView(x);
  let b = new DataView(y);
  if (a.byteLength !== b.byteLength) return false;
  for (let i=0; i < a.byteLength; i++) {
    if (a.getUint8(i) !== b.getUint8(i)) return false;
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

const FUNCTION_BIT_STRING: &str = "

function $bit_string(segments) {
  let size = segment => segment instanceof Uint8Array ? segment.byteLength : 1;
  let bytes = segments.reduce((acc, segment) => acc + size(segment), 0);
  let bits = new DataView(new ArrayBuffer(bytes));
  let cursor = 0;
  for (let segment of segments) {
    if (segment instanceof Uint8Array) {
      new Uint8Array(bits.buffer).set(segment, cursor);
      cursor += segment.byteLength;
    } else {
      bits.setInt8(cursor, segment);
      cursor++;
    }
  }
  return bits.buffer;
}";

pub type Output<'a> = Result<Document<'a>, Error>;

#[derive(Debug)]
pub struct Generator<'a> {
    line_numbers: &'a LineNumbers,
    module: &'a TypedModule,
    float_division_used: bool,
    object_equality_used: bool,
    bit_string_literal_used: bool,
    module_scope: im::HashMap<String, usize>,
}

impl<'a> Generator<'a> {
    pub fn new(line_numbers: &'a LineNumbers, module: &'a TypedModule) -> Self {
        Self {
            line_numbers,
            module,
            float_division_used: false,
            object_equality_used: false,
            bit_string_literal_used: false,
            module_scope: Default::default(),
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
        let mut statements: Vec<_> =
            Itertools::intersperse(statements, Ok(lines(2))).try_collect()?;

        // If float division has been used render an appropriate function
        if self.float_division_used {
            statements.push(FUNCTION_DIVIDE.to_doc());
        };

        // If structural equality is used render an appropriate function
        if self.object_equality_used {
            statements.push(DEEP_EQUAL.to_doc());
        };

        // If bit string literals have been used render an appropriate function
        if self.bit_string_literal_used {
            statements.push(FUNCTION_BIT_STRING.to_doc());
        };

        statements.push(line());
        Ok(statements.to_doc())
    }

    pub fn statement(&mut self, statement: &'a TypedStatement) -> Vec<Output<'a>> {
        match statement {
            Statement::TypeAlias { .. }
            | Statement::CustomType { .. }
            | Statement::ExternalType { .. } => vec![],

            Statement::Import { module, .. } if module == &["gleam"] => vec![],

            Statement::Import {
                module,
                as_name,
                unqualified,
                package,
                ..
            } => vec![Ok(self.import(package, module, as_name, unqualified))],

            Statement::ModuleConstant {
                public,
                name,
                value,
                ..
            } => vec![self.module_constant(*public, name, value)],

            Statement::Fn {
                arguments,
                name,
                body,
                public,
                ..
            } => vec![self.module_function(*public, name, arguments, body)],

            Statement::ExternalFn {
                public,
                name,
                arguments,
                module,
                fun,
                ..
            } => vec![Ok(
                self.external_function(*public, name, arguments, module, fun)
            )],
        }
    }

    fn import_path(&mut self, package: &'a str, module: &'a [String]) -> Document<'a> {
        let path = Document::String(module.join("/"));

        if package == self.module.type_info.package {
            // Same package uses relative paths
            // TODO: strip shared prefixed between current module and imported
            // module to avoid decending and climbing back out again
            let prefix = match self.module.name.len() {
                1 => "./".to_doc(),
                _ => Document::String("../".repeat(self.module.name.len() - 1)),
            };
            docvec!["\"", prefix, path, ".js\""]
        } else {
            // Different packages uses absolute imports
            docvec!["\"", package, "/", path, ".js\""]
        }
    }

    fn import(
        &mut self,
        package: &'a str,
        module: &'a [String],
        as_name: &'a Option<String>,
        unqualified: &'a [UnqualifiedImport],
    ) -> Document<'a> {
        let module_name = as_name
            .as_ref()
            .unwrap_or_else(|| {
                module
                    .last()
                    .expect("JavaScript generator could not identify imported module name.")
            })
            .to_camel_case();
        self.register_in_scope(&module_name);
        let path: Document<'a> = self.import_path(package, module);
        let module_name = Document::String(module_name);
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

                let alias = i.as_name.as_ref().map(|n| {
                    self.register_in_scope(n);
                    maybe_escape_identifier_doc(n)
                });
                (maybe_escape_identifier_doc(&i.name), alias)
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
        self.register_in_scope(name);
        Ok(docvec![
            head,
            maybe_escape_identifier_doc(name),
            " = ",
            expression::constant_expression(value)?,
            ";",
        ])
    }

    fn register_in_scope(&mut self, name: &str) {
        let _ = self.module_scope.insert(name.to_string(), 0);
    }

    fn module_function(
        &mut self,
        public: bool,
        name: &'a str,
        args: &'a [TypedArg],
        body: &'a TypedExpr,
    ) -> Output<'a> {
        self.register_in_scope(name);
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
            &mut self.bit_string_literal_used,
            self.module_scope.clone(),
        );
        let head = if public {
            "export function "
        } else {
            "function "
        };
        Ok(docvec![
            head,
            maybe_escape_identifier_doc(name),
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
        if module.is_empty() {
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
        let args = external_fn_args(arguments);
        let fun = if name == fun {
            docvec!["globalThis.", fun]
        } else {
            fun.to_doc()
        };
        let body = docvec!["return ", fun, args.clone()];
        let body = docvec![line(), body].nest(INDENT).group();
        docvec![head, name, args, " {", body, line(), "}"]
    }
}

fn external_fn_args<T>(arguments: &[ExternalFnArg<T>]) -> Document<'_> {
    wrap_args(
        arguments
            .iter()
            .enumerate()
            .map(|(index, ExternalFnArg { label, .. })| {
                label
                    .as_ref()
                    .map(|l| l.to_doc())
                    .unwrap_or_else(|| Document::String(format!("arg{}", index)))
            }),
    )
}

pub fn module(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    path: &Path,
    src: &str,
    writer: &mut impl Utf8Writer,
) -> Result<(), crate::Error> {
    Generator::new(line_numbers, module)
        .compile()
        .map_err(|error| crate::Error::JavaScript {
            path: path.to_path_buf(),
            src: src.to_string(),
            error,
        })?
        .pretty_print(80, writer)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Unsupported { feature: String, location: SrcSpan },
}

fn fun_args(args: &'_ [TypedArg]) -> Document<'_> {
    let mut discards = 0;
    wrap_args(args.iter().map(|a| match &a.names {
        ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => {
            let doc = if discards == 0 {
                "_".to_doc()
            } else {
                Document::String(format!("_{}", discards))
            };
            discards += 1;
            doc
        }
        ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
            maybe_escape_identifier_doc(name)
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

fn wrap_object<'a>(
    items: impl IntoIterator<Item = (Document<'a>, Option<Document<'a>>)>,
) -> Document<'a> {
    let fields = items.into_iter().map(|(key, value)| match value {
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

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar
// And we add `undefined` to avoid any unintentional overriding which could
// cause bugs.
fn is_valid_js_identifier(word: &str) -> bool {
    !matches!(
        word,
        "await"
            | "arguments"
            | "break"
            | "case"
            | "catch"
            | "class"
            | "const"
            | "continue"
            | "debugger"
            | "default"
            | "delete"
            | "do"
            | "else"
            | "enum"
            | "export"
            | "extends"
            | "eval"
            | "false"
            | "finally"
            | "for"
            | "function"
            | "if"
            | "implements"
            | "import"
            | "in"
            | "instanceof"
            | "interface"
            | "let"
            | "new"
            | "null"
            | "package"
            | "private"
            | "protected"
            | "public"
            | "return"
            | "static"
            | "super"
            | "switch"
            | "this"
            | "throw"
            | "true"
            | "try"
            | "typeof"
            | "undefined"
            | "var"
            | "void"
            | "while"
            | "with"
            | "yield"
    )
}

fn maybe_escape_identifier_string(word: &str) -> String {
    if is_valid_js_identifier(word) {
        word.to_string()
    } else {
        escape_identifier(word)
    }
}

fn escape_identifier(word: &str) -> String {
    format!("{}$", word)
}

fn maybe_escape_identifier_doc(word: &str) -> Document<'_> {
    if is_valid_js_identifier(word) {
        word.to_doc()
    } else {
        Document::String(escape_identifier(word))
    }
}
