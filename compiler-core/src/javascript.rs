mod endianness;
mod expression;
mod import;
mod pattern;
#[cfg(test)]
mod tests;
mod typescript;

use crate::analyse::TargetSupport;
use crate::build::Target;
use crate::codegen::TypeScriptDeclarations;
use crate::type_::PRELUDE_MODULE_NAME;
use crate::{
    ast::{CustomType, Function, Import, ModuleConstant, TypeAlias, *},
    docvec,
    line_numbers::LineNumbers,
    pretty::*,
};
use camino::Utf8Path;
use ecow::{eco_format, EcoString};
use expression::Context;
use itertools::Itertools;

use self::import::{Imports, Member};

const INDENT: isize = 2;

pub const PRELUDE: &str = include_str!("../templates/prelude.mjs");
pub const PRELUDE_TS_DEF: &str = include_str!("../templates/prelude.d.mts");

pub type Output<'a> = Result<Document<'a>, Error>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JavaScriptCodegenTarget {
    JavaScript,
    TypeScriptDeclarations,
}

#[derive(Debug)]
pub struct Generator<'a> {
    line_numbers: &'a LineNumbers,
    module: &'a TypedModule,
    tracker: UsageTracker,
    module_scope: im::HashMap<EcoString, usize>,
    current_module_name_segments_count: usize,
    target_support: TargetSupport,
    typescript: TypeScriptDeclarations,
}

impl<'a> Generator<'a> {
    pub fn new(
        line_numbers: &'a LineNumbers,
        module: &'a TypedModule,
        target_support: TargetSupport,
        typescript: TypeScriptDeclarations,
    ) -> Self {
        let current_module_name_segments_count = module.name.split('/').count();

        Self {
            current_module_name_segments_count,
            line_numbers,
            module,
            tracker: UsageTracker::default(),
            module_scope: Default::default(),
            target_support,
            typescript,
        }
    }

    fn type_reference(&self) -> Document<'a> {
        if self.typescript == TypeScriptDeclarations::None {
            return "".to_doc();
        }

        // Get the name of the module relative the directory (similar to basename)
        let module = self
            .module
            .name
            .as_str()
            .split('/')
            .last()
            .expect("JavaScript generator could not identify imported module name.");

        docvec!["/// <reference types=\"./", module, ".d.mts\" />", line()]
    }

    pub fn compile(&mut self) -> Output<'a> {
        let type_reference = self.type_reference();

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

        if self.tracker.prepend_used {
            self.register_prelude_usage(&mut imports, "prepend", Some("listPrepend"));
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

        if self.tracker.bit_array_literal_used {
            self.register_prelude_usage(&mut imports, "toBitArray", None);
        };

        if self.tracker.sized_integer_segment_used {
            self.register_prelude_usage(&mut imports, "sizedInt", None);
        };

        if self.tracker.string_bit_array_segment_used {
            self.register_prelude_usage(&mut imports, "stringBits", None);
        };

        if self.tracker.codepoint_bit_array_segment_used {
            self.register_prelude_usage(&mut imports, "codepointBits", None);
        };

        if self.tracker.float_bit_array_segment_used {
            self.register_prelude_usage(&mut imports, "sizedFloat", None);
        };

        // Put it all together

        if imports.is_empty() && statements.is_empty() {
            Ok(docvec![type_reference, "export {}", line()])
        } else if imports.is_empty() {
            statements.push(line());
            Ok(docvec![type_reference, statements])
        } else if statements.is_empty() {
            Ok(docvec![
                type_reference,
                imports.into_doc(JavaScriptCodegenTarget::JavaScript)
            ])
        } else {
            Ok(docvec![
                type_reference,
                imports.into_doc(JavaScriptCodegenTarget::JavaScript),
                line(),
                statements,
                line()
            ])
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

    pub fn statement(&mut self, statement: &'a TypedDefinition) -> Option<Output<'a>> {
        match statement {
            Definition::TypeAlias(TypeAlias { .. }) => None,

            // Handled in collect_imports
            Definition::Import(Import { .. }) => None,

            // Handled in collect_definitions
            Definition::CustomType(CustomType { .. }) => None,

            Definition::ModuleConstant(ModuleConstant {
                publicity,
                name,
                value,
                ..
            }) => Some(self.module_constant(*publicity, name, value)),

            Definition::Function(function) => {
                // If there's an external JavaScript implementation then it will be imported,
                // so we don't need to generate a function definition.
                if function.external_javascript.is_some() {
                    return None;
                }

                // If the function does not support JavaScript then we don't need to generate
                // a function definition.
                if !function.implementations.supports(Target::JavaScript) {
                    return None;
                }

                self.module_function(function)
            }
        }
    }

    fn custom_type_definition(
        &mut self,
        constructors: &'a [TypedRecordConstructor],
        publicity: Publicity,
        opaque: bool,
    ) -> Vec<Output<'a>> {
        // If there's no constructors then there's nothing to do here.
        if constructors.is_empty() {
            return vec![];
        }

        self.tracker.custom_type_used = true;
        constructors
            .iter()
            .map(|constructor| Ok(self.record_definition(constructor, publicity, opaque)))
            .collect()
    }

    fn record_definition(
        &self,
        constructor: &'a TypedRecordConstructor,
        publicity: Publicity,
        opaque: bool,
    ) -> Document<'a> {
        fn parameter((i, arg): (usize, &TypedRecordConstructorArg)) -> Document<'_> {
            arg.label
                .as_ref()
                .map(|(_, s)| maybe_escape_identifier_doc(s))
                .unwrap_or_else(|| eco_format!("x{i}").to_doc())
        }

        let head = if publicity.is_private() || opaque {
            "class "
        } else {
            "export class "
        };
        let head = docvec![head, &constructor.name, " extends $CustomType {"];

        if constructor.arguments.is_empty() {
            return head.append("}");
        };

        let parameters = join(
            constructor.arguments.iter().enumerate().map(parameter),
            break_(",", ", "),
        );

        let constructor_body = join(
            constructor.arguments.iter().enumerate().map(|(i, arg)| {
                let var = parameter((i, arg));
                match &arg.label {
                    None => docvec!["this[", i, "] = ", var, ";"],
                    Some((_, name)) => {
                        docvec!["this.", maybe_escape_property_doc(name), " = ", var, ";"]
                    }
                }
            }),
            line(),
        );

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
                    publicity,
                    constructors,
                    opaque,
                    ..
                }) => self.custom_type_definition(constructors, *publicity, *opaque),

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
                    unqualified_values: unqualified,
                    package,
                    ..
                }) => {
                    self.register_import(&mut imports, package, module, as_name, unqualified);
                }

                Definition::Function(Function {
                    name: Some((_, name)),
                    publicity,
                    external_javascript: Some((module, function, _location)),
                    ..
                }) => {
                    self.register_external_function(
                        &mut imports,
                        *publicity,
                        name,
                        module,
                        function,
                    );
                }

                Definition::Function(Function { .. })
                | Definition::TypeAlias(TypeAlias { .. })
                | Definition::CustomType(CustomType { .. })
                | Definition::ModuleConstant(ModuleConstant { .. }) => (),
            }
        }

        imports
    }

    fn import_path(&self, package: &'a str, module: &'a str) -> EcoString {
        // TODO: strip shared prefixed between current module and imported
        // module to avoid descending and climbing back out again
        if package == self.module.type_info.package || package.is_empty() {
            // Same package
            match self.current_module_name_segments_count {
                1 => eco_format!("./{module}.mjs"),
                _ => {
                    let prefix = "../".repeat(self.current_module_name_segments_count - 1);
                    eco_format!("{prefix}{module}.mjs")
                }
            }
        } else {
            // Different package
            let prefix = "../".repeat(self.current_module_name_segments_count);
            eco_format!("{prefix}{package}/{module}.mjs")
        }
    }

    fn register_import(
        &mut self,
        imports: &mut Imports<'a>,
        package: &'a str,
        module: &'a str,
        as_name: &'a Option<(AssignName, SrcSpan)>,
        unqualified: &'a [UnqualifiedImport],
    ) {
        let get_name = |module: &'a str| {
            module
                .split('/')
                .last()
                .expect("JavaScript generator could not identify imported module name.")
        };

        let (discarded, module_name) = match as_name {
            None => (false, get_name(module)),
            Some((AssignName::Discard(_), _)) => (true, get_name(module)),
            Some((AssignName::Variable(name), _)) => (false, name.as_str()),
        };

        let module_name = eco_format!("${module_name}");
        let path = self.import_path(package, module);
        let unqualified_imports = unqualified.iter().map(|i| {
            let alias = i.as_name.as_ref().map(|n| {
                self.register_in_scope(n);
                maybe_escape_identifier_doc(n)
            });
            let name = maybe_escape_identifier_doc(&i.name);
            Member { name, alias }
        });

        let aliases = if discarded { vec![] } else { vec![module_name] };
        imports.register_module(path, aliases, unqualified_imports);
    }

    fn register_external_function(
        &mut self,
        imports: &mut Imports<'a>,
        publicity: Publicity,
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
                Some(escape_identifier(name).to_doc())
            } else {
                Some(name.to_doc())
            },
        };
        if publicity.is_importable() {
            imports.register_export(maybe_escape_identifier_string(name))
        }
        imports.register_module(EcoString::from(module), [], [member]);
    }

    fn module_constant(
        &mut self,
        publicity: Publicity,
        name: &'a str,
        value: &'a TypedConstant,
    ) -> Output<'a> {
        let head = if publicity.is_private() {
            "const "
        } else {
            "export const "
        };

        let document =
            expression::constant_expression(Context::Constant, &mut self.tracker, value)?;

        Ok(docvec![
            head,
            maybe_escape_identifier_doc(name),
            " = ",
            document,
            ";",
        ])
    }

    fn register_in_scope(&mut self, name: &str) {
        let _ = self.module_scope.insert(name.into(), 0);
    }

    fn module_function(&mut self, function: &'a TypedFunction) -> Option<Output<'a>> {
        let (_, name) = function
            .name
            .as_ref()
            .expect("A module's function must be named");
        let argument_names = function
            .arguments
            .iter()
            .map(|arg| arg.names.get_variable_name())
            .collect();
        let mut generator = expression::Generator::new(
            self.module.name.clone(),
            self.line_numbers,
            name.clone(),
            argument_names,
            &mut self.tracker,
            self.module_scope.clone(),
        );
        let head = if function.publicity.is_private() {
            "function "
        } else {
            "export function "
        };

        let body = match generator.function_body(&function.body, function.arguments.as_slice()) {
            // No error, let's continue!
            Ok(body) => body,

            // There is an error coming from some expression that is not supported on JavaScript
            // and the target support is not enforced. In this case we do not error, instead
            // returning nothing which will cause no function to be generated.
            Err(error) if error.is_unsupported() && !self.target_support.is_enforced() => {
                return None
            }

            // Some other error case which will be returned to the user.
            Err(error) => return Some(Err(error)),
        };

        let document = docvec![
            head,
            maybe_escape_identifier_doc(name.as_str()),
            fun_args(function.arguments.as_slice(), generator.tail_recursion_used),
            " {",
            docvec![line(), body].nest(INDENT).group(),
            line(),
            "}",
        ];
        Some(Ok(document))
    }

    fn register_module_definitions_in_scope(&mut self) {
        for statement in self.module.definitions.iter() {
            match statement {
                Definition::ModuleConstant(ModuleConstant { name, .. }) => {
                    self.register_in_scope(name)
                }

                Definition::Function(Function { name, .. }) => self.register_in_scope(
                    name.as_ref()
                        .map(|(_, name)| name)
                        .expect("Function in a definition must be named"),
                ),

                Definition::Import(Import {
                    unqualified_values: unqualified,
                    ..
                }) => unqualified
                    .iter()
                    .for_each(|unq_import| self.register_in_scope(unq_import.used_name())),

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
    src: &EcoString,
    target_support: TargetSupport,
    typescript: TypeScriptDeclarations,
) -> Result<String, crate::Error> {
    let document = Generator::new(line_numbers, module, target_support, typescript)
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
    src: &EcoString,
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

impl Error {
    /// Returns `true` if the error is [`Unsupported`].
    ///
    /// [`Unsupported`]: Error::Unsupported
    #[must_use]
    pub fn is_unsupported(&self) -> bool {
        matches!(self, Self::Unsupported { .. })
    }
}

fn fun_args(args: &'_ [TypedArg], tail_recursion_used: bool) -> Document<'_> {
    let mut discards = 0;
    wrap_args(args.iter().map(|a| match a.get_variable_name() {
        None => {
            let doc = if discards == 0 {
                "_".to_doc()
            } else {
                eco_format!("_{discards}").to_doc()
            };
            discards += 1;
            doc
        }
        Some(name) if tail_recursion_used => eco_format!("loop${name}").to_doc(),
        Some(name) => maybe_escape_identifier_doc(name),
    }))
}

fn wrap_args<'a, I>(args: I) -> Document<'a>
where
    I: IntoIterator<Item = Document<'a>>,
{
    break_("", "")
        .append(join(args, break_(",", ", ")))
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
    let fields = join(fields, break_(",", ", "));

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
        // Keywords and reserved words
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

fn is_usable_js_property(label: &str) -> bool {
    !matches!(
        label,
        // `then` to avoid a custom type that defines a `then` function being used as a `thenable`
        // in Javascript.
        "then"
            // `constructor` to avoid unintentional overriding of the constructor of records,
            // leading to potential runtime crashes while using `withFields`.
            | "constructor"
            // `prototype` and `__proto__` to avoid unintentionally overriding the prototype chain
            | "prototype"
            | "__proto__"
    )
}

fn maybe_escape_identifier_string(word: &str) -> EcoString {
    if is_usable_js_identifier(word) {
        EcoString::from(word)
    } else {
        escape_identifier(word)
    }
}

fn escape_identifier(word: &str) -> EcoString {
    eco_format!("{word}$")
}

fn maybe_escape_identifier_doc(word: &str) -> Document<'_> {
    if is_usable_js_identifier(word) {
        word.to_doc()
    } else {
        escape_identifier(word).to_doc()
    }
}

fn maybe_escape_property_doc(label: &str) -> Document<'_> {
    if is_usable_js_property(label) {
        label.to_doc()
    } else {
        escape_identifier(label).to_doc()
    }
}

#[derive(Debug, Default)]
pub(crate) struct UsageTracker {
    pub ok_used: bool,
    pub list_used: bool,
    pub prepend_used: bool,
    pub error_used: bool,
    pub int_remainder_used: bool,
    pub make_error_used: bool,
    pub custom_type_used: bool,
    pub int_division_used: bool,
    pub float_division_used: bool,
    pub object_equality_used: bool,
    pub bit_array_literal_used: bool,
    pub sized_integer_segment_used: bool,
    pub string_bit_array_segment_used: bool,
    pub codepoint_bit_array_segment_used: bool,
    pub float_bit_array_segment_used: bool,
}

fn bool(bool: bool) -> Document<'static> {
    match bool {
        true => "true".to_doc(),
        false => "false".to_doc(),
    }
}
