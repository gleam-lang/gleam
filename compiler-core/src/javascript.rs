mod expression;
mod import;
mod pattern;
#[cfg(test)]
mod tests;
mod typescript;

use camino::Utf8Path;

use crate::type_::PRELUDE_MODULE_NAME;
use crate::{
    ast::{CustomType, Function, Import, ModuleConstant, TypeAlias, *},
    docvec,
    line_numbers::LineNumbers,
    pretty::*,
};
use itertools::Itertools;
use smol_str::SmolStr;

use self::import::{Imports, Member};

const INDENT: isize = 2;

pub const PRELUDE: &str = include_str!("../templates/prelude.js");
pub const PRELUDE_TS_DEF: &str = include_str!("../templates/prelude.d.ts");

pub type Output<'a> = Result<Document<'a>, Error>;

#[derive(Debug)]
pub struct Generator<'a> {
    line_numbers: &'a LineNumbers,
    module: &'a TypedModule,
    tracker: UsageTracker,
    module_scope: im::HashMap<SmolStr, usize>,
    current_module_name_segments_count: usize,
}

impl<'a> Generator<'a> {
    pub fn new(line_numbers: &'a LineNumbers, module: &'a TypedModule) -> Self {
        let current_module_name_segments_count = module.name.split('/').count();
        Self {
            current_module_name_segments_count,
            line_numbers,
            module,
            tracker: UsageTracker::default(),
            module_scope: Default::default(),
        }
    }

    pub fn compile(&mut self) -> Output<'a> {
        // Determine what JavaScript imports we need to generate
        let mut imports = self.collect_imports();

        // Determine what names are defined in the module scope so we know to
        // rename any variables that are defined within functions using the same
        // names.
        self.register_module_definitions_in_scope();

        // Generate JavaScript code for each statement
        let statements = self.collect_definitions().into_iter().chain(
            self.module
                .definitions
                .iter()
                .flat_map(|s| self.statement(s)),
        );

        // Two lines between each statement
        let mut statements: Vec<_> =
            Itertools::intersperse(statements, Ok(lines(2))).try_collect()?;

        // Import any prelude functions that have been used

        if self.tracker.ok_used {
            self.register_prelude_usage(&mut imports, "Ok", None);
        };

        if self.tracker.error_used {
            self.register_prelude_usage(&mut imports, "Error", None);
        };

        if self.tracker.list_used {
            self.register_prelude_usage(&mut imports, "toList", None);
        };

        if self.tracker.custom_type_used {
            self.register_prelude_usage(&mut imports, "CustomType", Some("$CustomType"));
        };

        if self.tracker.make_error_used {
            self.register_prelude_usage(&mut imports, "makeError", None);
        };

        if self.tracker.int_remainder_used {
            self.register_prelude_usage(&mut imports, "remainderInt", None);
        };

        if self.tracker.float_division_used {
            self.register_prelude_usage(&mut imports, "divideFloat", None);
        };

        if self.tracker.int_division_used {
            self.register_prelude_usage(&mut imports, "divideInt", None);
        };

        if self.tracker.object_equality_used {
            self.register_prelude_usage(&mut imports, "isEqual", None);
        };

        if self.tracker.bit_string_literal_used {
            self.register_prelude_usage(&mut imports, "toBitString", None);
        };

        if self.tracker.sized_integer_segment_used {
            self.register_prelude_usage(&mut imports, "sizedInt", None);
        };

        if self.tracker.string_bit_string_segment_used {
            self.register_prelude_usage(&mut imports, "stringBits", None);
        };

        if self.tracker.codepoint_bit_string_segment_used {
            self.register_prelude_usage(&mut imports, "codepointBits", None);
        };

        if self.tracker.float_bit_string_segment_used {
            self.register_prelude_usage(&mut imports, "float64Bits", None);
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

    fn register_prelude_usage(
        &self,
        imports: &mut Imports<'a>,
        name: &'static str,
        alias: Option<&'static str>,
    ) {
        let path = self.import_path(&self.module.type_info.package, PRELUDE_MODULE_NAME);
        let member = Member {
            name: name.to_doc(),
            alias: alias.map(|a| a.to_doc()),
        };
        imports.register_module(path, [], [member]);
    }

    pub fn statement(&mut self, statement: &'a TypedDefinition) -> Vec<Output<'a>> {
        match statement {
            Definition::TypeAlias(TypeAlias { .. }) => vec![],

            // Handled in collect_imports
            Definition::Import(Import { .. }) => vec![],

            // Handled in collect_definitions
            Definition::CustomType(CustomType { .. }) => vec![],

            Definition::ModuleConstant(ModuleConstant {
                public,
                name,
                value,
                ..
            }) => vec![self.module_constant(*public, name, value)],

            Definition::Function(Function {
                arguments,
                name,
                body,
                public,
                external_javascript: None,
                ..
            }) => vec![self.module_function(*public, name, arguments, body)],

            Definition::Function(_) => vec![],
        }
    }

    fn custom_type_definition(
        &mut self,
        constructors: &'a [TypedRecordConstructor],
        public: bool,
        opaque: bool,
    ) -> Vec<Output<'a>> {
        // If there's no constructors then there's nothing to do here.
        if constructors.is_empty() {
            return vec![];
        }

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
                .unwrap_or_else(|| Document::String(format!("x{i}")))
        }

        let head = if public && !opaque {
            "export class "
        } else {
            "class "
        };
        let head = docvec![head, &constructor.name, " extends $CustomType {"];

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

    fn collect_definitions(&mut self) -> Vec<Output<'a>> {
        self.module
            .definitions
            .iter()
            .flat_map(|statement| match statement {
                Definition::CustomType(CustomType {
                    public,
                    constructors,
                    opaque,
                    ..
                }) => self.custom_type_definition(constructors, *public, *opaque),

                Definition::Function(Function { .. })
                | Definition::TypeAlias(TypeAlias { .. })
                | Definition::Import(Import { .. })
                | Definition::ModuleConstant(ModuleConstant { .. }) => vec![],
            })
            .collect()
    }

    fn collect_imports(&mut self) -> Imports<'a> {
        let mut imports = Imports::new();

        for statement in &self.module.definitions {
            match statement {
                Definition::Import(Import {
                    module,
                    as_name,
                    unqualified,
                    package,
                    ..
                }) => {
                    self.register_import(&mut imports, package, module, as_name, unqualified);
                }

                Definition::Function(Function {
                    name,
                    public,
                    external_javascript: Some((module, function)),
                    ..
                }) => {
                    self.register_external_function(&mut imports, *public, name, module, function);
                }

                Definition::Function(Function { .. })
                | Definition::TypeAlias(TypeAlias { .. })
                | Definition::CustomType(CustomType { .. })
                | Definition::ModuleConstant(ModuleConstant { .. }) => (),
            }
        }

        imports
    }

    fn import_path(&self, package: &'a str, module: &'a str) -> String {
        // TODO: strip shared prefixed between current module and imported
        // module to avoid decending and climbing back out again
        if package == self.module.type_info.package || package.is_empty() {
            // Same package
            match self.current_module_name_segments_count {
                1 => format!("./{module}.mjs"),
                _ => {
                    let prefix = "../".repeat(self.current_module_name_segments_count - 1);
                    format!("{prefix}{module}.mjs")
                }
            }
        } else {
            // Different package
            let prefix = "../".repeat(self.current_module_name_segments_count);
            format!("{prefix}{package}/{module}.mjs")
        }
    }

    fn register_import(
        &mut self,
        imports: &mut Imports<'a>,
        package: &'a str,
        module: &'a str,
        as_name: &'a Option<SmolStr>,
        unqualified: &'a [UnqualifiedImport],
    ) {
        let module_name = as_name.as_ref().map(SmolStr::as_str).unwrap_or_else(|| {
            module
                .split('/')
                .last()
                .expect("JavaScript generator could not identify imported module name.")
        });
        let module_name = format!("${module_name}");
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
        let needs_escaping = !is_usable_js_identifier(name);
        let member = Member {
            name: fun.to_doc(),
            alias: if name == fun && !needs_escaping {
                None
            } else if needs_escaping {
                Some(Document::String(escape_identifier(name)))
            } else {
                Some(name.to_doc())
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
        Ok(docvec![
            head,
            maybe_escape_identifier_doc(name),
            " = ",
            expression::constant_expression(&mut self.tracker, value)?,
            ";",
        ])
    }

    fn register_in_scope(&mut self, name: &str) {
        let _ = self.module_scope.insert(name.into(), 0);
    }

    fn module_function(
        &mut self,
        public: bool,
        name: &'a SmolStr,
        args: &'a [TypedArg],
        body: &'a [TypedStatement],
    ) -> Output<'a> {
        let argument_names = args
            .iter()
            .map(|arg| arg.names.get_variable_name())
            .collect();
        let mut generator = expression::Generator::new(
            self.module.name.clone(),
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

    fn register_module_definitions_in_scope(&mut self) {
        for statement in self.module.definitions.iter() {
            match statement {
                Definition::ModuleConstant(ModuleConstant { name, .. })
                | Definition::Function(Function { name, .. }) => self.register_in_scope(name),

                Definition::Import(Import { unqualified, .. }) => unqualified
                    .iter()
                    .for_each(|unq_import| self.register_in_scope(unq_import.variable_name())),

                Definition::TypeAlias(TypeAlias { .. })
                | Definition::CustomType(CustomType { .. }) => (),
            }
        }
    }
}

pub fn module(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    path: &Utf8Path,
    src: &SmolStr,
) -> Result<String, crate::Error> {
    let document = Generator::new(line_numbers, module)
        .compile()
        .map_err(|error| crate::Error::JavaScript {
            path: path.to_path_buf(),
            src: src.clone(),
            error,
        })?;
    Ok(document.to_pretty_string(80))
}

pub fn ts_declaration(
    module: &TypedModule,
    path: &Utf8Path,
    src: &SmolStr,
) -> Result<String, crate::Error> {
    let document = typescript::TypeScriptGenerator::new(module)
        .compile()
        .map_err(|error| crate::Error::JavaScript {
            path: path.to_path_buf(),
            src: src.clone(),
            error,
        })?;
    Ok(document.to_pretty_string(80))
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
                Document::String(format!("_{discards}"))
            };
            discards += 1;
            doc
        }
        Some(name) if tail_recursion_used => Document::String(format!("loop${name}")),
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
            | "deoh dear"
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
    format!("{word}$")
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
    pub int_remainder_used: bool,
    pub make_error_used: bool,
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
