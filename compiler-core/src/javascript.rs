mod expression;
mod import;
mod pattern;
#[cfg(test)]
mod tests;

use std::path::Path;

use crate::{ast::*, docvec, io::Utf8Writer, line_numbers::LineNumbers, pretty::*};
use itertools::Itertools;

use self::import::{Imports, Member};

const INDENT: isize = 2;

pub const PRELUDE: &str = include_str!("../templates/prelude.js");

pub type Output<'a> = Result<Document<'a>, Error>;

#[derive(Debug)]
pub struct Generator<'a> {
    line_numbers: &'a LineNumbers,
    module: &'a TypedModule,
    tracker: UsageTracker,
    module_scope: im::HashMap<String, usize>,
}

impl<'a> Generator<'a> {
    pub fn new(line_numbers: &'a LineNumbers, module: &'a TypedModule) -> Self {
        Self {
            line_numbers,
            module,
            tracker: UsageTracker::default(),
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

        // Import any prelude functions that have been used

        if self.tracker.ok_used {
            self.register_prelude_usage(&mut imports, "Ok");
        };

        if self.tracker.error_used {
            self.register_prelude_usage(&mut imports, "Error");
        };

        if self.tracker.list_used {
            self.register_prelude_usage(&mut imports, "toList");
        };

        if self.tracker.custom_type_used {
            self.register_prelude_usage(&mut imports, "CustomType");
        };

        if self.tracker.throw_error_used {
            self.register_prelude_usage(&mut imports, "throwError");
        };

        if self.tracker.float_division_used {
            self.register_prelude_usage(&mut imports, "divideFloat");
        };

        if self.tracker.int_division_used {
            self.register_prelude_usage(&mut imports, "divideInt");
        };

        if self.tracker.object_equality_used {
            self.register_prelude_usage(&mut imports, "isEqual");
        };

        if self.tracker.bit_string_literal_used {
            self.register_prelude_usage(&mut imports, "toBitString");
        };

        if self.tracker.sized_integer_segment_used {
            self.register_prelude_usage(&mut imports, "sizedInteger");
        };

        if self.tracker.string_bit_string_segment_used {
            self.register_prelude_usage(&mut imports, "stringBits");
        };

        if self.tracker.codepoint_bit_string_segment_used {
            self.register_prelude_usage(&mut imports, "codepointBits");
        };

        if self.tracker.float_bit_string_segment_used {
            self.register_prelude_usage(&mut imports, "float64Bits");
        };

        // Put it all together

        if imports.is_empty() && statements.is_empty() {
            Ok(docvec!("export {}", line()))
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
        imports.register_module(path, [], [member]);
    }

    pub fn statement(&mut self, statement: &'a TypedStatement) -> Vec<Output<'a>> {
        match statement {
            Statement::TypeAlias { .. } | Statement::ExternalType { .. } => vec![],

            Statement::Import { .. } => vec![],

            Statement::CustomType {
                public,
                constructors,
                opaque,
                ..
            } => self.custom_type_definition(constructors, *public, *opaque),

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

    fn custom_type_definition(
        &mut self,
        constructors: &'a [TypedRecordConstructor],
        public: bool,
        opaque: bool,
    ) -> Vec<Output<'a>> {
        self.tracker.custom_type_used = true;
        constructors
            .iter()
            .map(|constructor| Ok(self.record_definition(constructor, public, opaque)))
            .collect()
    }

    fn record_definition(
        &self,
        constructor: &'a TypedRecordConstructor,
        public: bool,
        opaque: bool,
    ) -> Document<'a> {
        fn parameter((i, arg): (usize, &TypedRecordConstructorArg)) -> Document<'_> {
            arg.label
                .as_ref()
                .map(|s| maybe_escape_identifier_doc(s))
                .unwrap_or_else(|| Document::String(format!("x{}", i)))
        }

        let head = if public && !opaque {
            "export class "
        } else {
            "class "
        };
        let head = docvec![head, &constructor.name, " extends CustomType {"];

        if constructor.arguments.is_empty() {
            return head.append("}");
        };

        let parameters = concat(Itertools::intersperse(
            constructor.arguments.iter().enumerate().map(parameter),
            break_(",", ", "),
        ));

        let constructor_body = concat(Itertools::intersperse(
            constructor.arguments.iter().enumerate().map(|(i, arg)| {
                let var = parameter((i, arg));
                match &arg.label {
                    None => docvec!["this[", i, "] = ", var, ";"],
                    Some(name) => docvec!["this.", name, " = ", var, ";"],
                }
            }),
            line(),
        ));

        let class_body = docvec![
            line(),
            "constructor(",
            parameters,
            ") {",
            docvec![line(), "super();", line(), constructor_body].nest(INDENT),
            line(),
            "}",
        ]
        .nest(INDENT);

        docvec![head, class_body, line(), "}"]
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

        // TODO: strip shared prefixed between current module and imported
        // module to avoid decending and climbing back out again
        if package == self.module.type_info.package || package.is_empty() {
            // Same package
            match self.module.name.len() {
                1 => format!("./{}.mjs", path),
                _ => {
                    let prefix = "../".repeat(self.module.name.len() - 1);
                    format!("{}{}.mjs", prefix, path)
                }
            }
        } else {
            // Different package
            let prefix = "../".repeat(self.module.name.len() + 1);
            format!("{}{}/dist/{}.mjs", prefix, package, path)
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
        let module_name = as_name.as_ref().unwrap_or_else(|| {
            module
                .last()
                .expect("JavaScript generator could not identify imported module name.")
        });
        let module_name = format!("${}", module_name);
        let path = self.import_path(package, module);
        let unqualified_imports = unqualified
            .iter()
            // We do not create a JS import for types as they are not used at runtime
            .filter(|import| import.is_value())
            .map(|i| {
                let alias = i.as_name.as_ref().map(|n| {
                    self.register_in_scope(n);
                    maybe_escape_identifier_doc(n)
                });
                let name = maybe_escape_identifier_doc(&i.name);
                Member { name, alias }
            });
        imports.register_module(path, [module_name], unqualified_imports);
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
        imports.register_module(module.to_string(), [], [member]);
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
            expression::constant_expression(&mut self.tracker, value)?,
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
            &mut self.tracker,
            self.module_scope.clone(),
        );
        let head = if public {
            "export function "
        } else {
            "function "
        };
        let body = generator.function_body(body, args)?;
        Ok(docvec![
            head,
            maybe_escape_identifier_doc(name),
            fun_args(args, generator.tail_recursion_used),
            " {",
            docvec![line(), body].nest(INDENT).group(),
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

fn fun_args(args: &'_ [TypedArg], tail_recursion_used: bool) -> Document<'_> {
    let mut discards = 0;
    wrap_args(args.iter().map(|a| match a.get_variable_name() {
        None => {
            let doc = if discards == 0 {
                "_".to_doc()
            } else {
                Document::String(format!("_{}", discards))
            };
            discards += 1;
            doc
        }
        Some(name) if tail_recursion_used => Document::String(format!("loop${}", name)),
        Some(name) => maybe_escape_identifier_doc(name),
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
    let mut empty = true;
    let fields = items.into_iter().map(|(key, value)| {
        empty = false;
        match value {
            Some(value) => docvec![key, ": ", value],
            None => key.to_doc(),
        }
    });
    let fields = concat(Itertools::intersperse(fields, break_(",", ", ")));

    if empty {
        "{}".to_doc()
    } else {
        docvec![
            docvec!["{", break_("", " "), fields]
                .nest(INDENT)
                .append(break_("", " "))
                .group(),
            "}"
        ]
    }
}

fn try_wrap_object<'a>(items: impl IntoIterator<Item = (Document<'a>, Output<'a>)>) -> Output<'a> {
    let fields = items
        .into_iter()
        .map(|(key, value)| Ok(docvec![key, ": ", value?]));
    let fields: Vec<_> = Itertools::intersperse(fields, Ok(break_(",", ", "))).try_collect()?;

    Ok(docvec![
        docvec!["{", break_("", " "), fields]
            .nest(INDENT)
            .append(break_("", " "))
            .group(),
        "}"
    ])
}

fn is_usable_js_identifier(word: &str) -> bool {
    !matches!(
        word,
        // Keywords and reserved works
        // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar
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
            | "var"
            | "void"
            | "while"
            | "with"
            | "yield"
            // `undefined` to avoid any unintentional overriding.
            | "undefined"
            // `then` to avoid a module that defines a `then` function being
            // used as a `thenable` in JavaScript when the module is imported
            // dynamically, which results in unexpected behaviour.
            // It is rather unfortunate that we have to do this.
            | "then"
    )
}

fn maybe_escape_identifier_string(word: &str) -> String {
    if is_usable_js_identifier(word) {
        word.to_string()
    } else {
        escape_identifier(word)
    }
}

fn escape_identifier(word: &str) -> String {
    format!("{}$", word)
}

fn maybe_escape_identifier_doc(word: &str) -> Document<'_> {
    if is_usable_js_identifier(word) {
        word.to_doc()
    } else {
        Document::String(escape_identifier(word))
    }
}

#[derive(Debug, Default)]
pub(crate) struct UsageTracker {
    pub ok_used: bool,
    pub list_used: bool,
    pub error_used: bool,
    pub throw_error_used: bool,
    pub custom_type_used: bool,
    pub int_division_used: bool,
    pub float_division_used: bool,
    pub object_equality_used: bool,
    pub bit_string_literal_used: bool,
    pub sized_integer_segment_used: bool,
    pub string_bit_string_segment_used: bool,
    pub codepoint_bit_string_segment_used: bool,
    pub float_bit_string_segment_used: bool,
}
