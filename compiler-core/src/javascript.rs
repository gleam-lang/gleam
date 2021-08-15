mod expression;
mod import;
mod pattern;
#[cfg(test)]
mod tests;

use std::{iter, path::Path};

use crate::{ast::*, docvec, io::Utf8Writer, line_numbers::LineNumbers, pretty::*};
use heck::CamelCase;
use itertools::Itertools;

use self::import::{Imports, Member};

const INDENT: isize = 2;

pub const PRELUDE: &str = include_str!("../templates/prelude.js");

const FUNCTION_BIT_STRING: &str = "

function $bit_string(segments) {
  let size = segment => segment instanceof Uint8Array ? segment.byteLength : 1;
  let bytes = segments.reduce((acc, segment) => acc + size(segment), 0);
  let view = new DataView(new ArrayBuffer(bytes));
  let cursor = 0;
  for (let segment of segments) {
    if (segment instanceof Uint8Array) {
      new Uint8Array(view.buffer).set(segment, cursor);
      cursor += segment.byteLength;
    } else {
      view.setInt8(cursor, segment);
      cursor++;
    }
  }
  return new Uint8Array(view.buffer);
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
        let mut imports = self.collect_imports();
        let statements = self
            .module
            .statements
            .iter()
            .flat_map(|s| self.statement(s));

        // Two lines between each statement
        let mut statements: Vec<_> =
            Itertools::intersperse(statements, Ok(lines(2))).try_collect()?;

        // If float division has been used render an appropriate function
        if self.float_division_used {
            self.register_prelude_usage(&mut imports, "divideFloat");
        };

        // If structural equality is used render an appropriate function
        if self.object_equality_used {
            self.register_prelude_usage(&mut imports, "isEqual");
        };

        // If bit string literals have been used render an appropriate function
        if self.bit_string_literal_used {
            statements.push(FUNCTION_BIT_STRING.to_doc());
        };

        if imports.is_empty() && statements.is_empty() {
            Ok(docvec!("export {};", line()))
        } else if imports.is_empty() {
            statements.push(line());
            Ok(statements.to_doc())
        } else if statements.is_empty() {
            Ok(imports.into_doc())
        } else {
            Ok(docvec![imports.into_doc(), line(), statements, line()])
        }
    }

    fn register_prelude_usage(&self, imports: &mut Imports<'a>, name: &'static str) {
        let path = self.import_path(&self.module.type_info.package, &["gleam".to_string()]);
        let member = Member {
            name: name.to_doc(),
            alias: None,
        };
        imports.register_module(path, iter::empty(), iter::once(member));
    }

    pub fn statement(&mut self, statement: &'a TypedStatement) -> Vec<Output<'a>> {
        match statement {
            Statement::TypeAlias { .. }
            | Statement::CustomType { .. }
            | Statement::ExternalType { .. } => vec![],

            Statement::Import { .. } => vec![],

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
            } if module.is_empty() => vec![Ok(
                self.global_external_function(*public, name, arguments, fun)
            )],

            Statement::ExternalFn { .. } => vec![],
        }
    }

    fn collect_imports(&mut self) -> Imports<'a> {
        let mut imports = Imports::new();

        for statement in &self.module.statements {
            match statement {
                Statement::Fn { .. }
                | Statement::TypeAlias { .. }
                | Statement::CustomType { .. }
                | Statement::ExternalType { .. }
                | Statement::ModuleConstant { .. } => (),
                Statement::ExternalFn { module, .. } if module.is_empty() => (),
                Statement::Import { module, .. } if module == &["gleam"] => (),

                Statement::ExternalFn {
                    public,
                    name,
                    module,
                    fun,
                    ..
                } => self.register_external_function(&mut imports, *public, name, module, fun),

                Statement::Import {
                    module,
                    as_name,
                    unqualified,
                    package,
                    ..
                } => {
                    self.register_import(&mut imports, package, module, as_name, unqualified);
                }
            }
        }

        imports
    }

    fn import_path(&self, package: &'a str, module: &'a [String]) -> String {
        let path = module.join("/");

        if package == self.module.type_info.package || package.is_empty() {
            // Same package uses relative paths
            // TODO: strip shared prefixed between current module and imported
            // module to avoid decending and climbing back out again
            match self.module.name.len() {
                1 => format!("./{}.js", path),
                _ => {
                    let prefix = "../".repeat(self.module.name.len() - 1);
                    format!("{}{}.js", prefix, path)
                }
            }
        } else {
            // Different packages uses absolute imports
            format!("{}/{}.js", package, path)
        }
    }

    fn register_import(
        &mut self,
        imports: &mut Imports<'a>,
        package: &'a str,
        module: &'a [String],
        as_name: &'a Option<String>,
        unqualified: &'a [UnqualifiedImport],
    ) {
        let module_name = as_name
            .as_ref()
            .unwrap_or_else(|| {
                module
                    .last()
                    .expect("JavaScript generator could not identify imported module name.")
            })
            .to_camel_case();
        self.register_in_scope(&module_name);
        let path = self.import_path(package, module);
        let unqualified_imports = unqualified
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
                let alias = i.as_name.as_ref().map(|n| {
                    self.register_in_scope(n);
                    maybe_escape_identifier_doc(n)
                });
                let name = maybe_escape_identifier_doc(&i.name);
                Member { name, alias }
            });
        imports.register_module(path, iter::once(module_name), unqualified_imports);
    }

    fn register_external_function(
        &mut self,
        imports: &mut Imports<'a>,
        public: bool,
        name: &'a str,
        module: &'a str,
        fun: &'a str,
    ) {
        let member = Member {
            name: fun.to_doc(),
            alias: if name == fun {
                None
            } else {
                Some(maybe_escape_identifier_doc(name))
            },
        };
        if public {
            imports.register_export(maybe_escape_identifier_string(name))
        }
        imports.register_module(module.to_string(), iter::empty(), iter::once(member));
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
