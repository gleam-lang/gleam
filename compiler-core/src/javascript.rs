mod decision;
mod expression;
mod import;
#[cfg(test)]
mod tests;
mod typescript;

use std::collections::HashMap;

use num_bigint::BigInt;
use num_traits::ToPrimitive;

use crate::build::Target;
use crate::build::package_compiler::StdlibPackage;
use crate::codegen::TypeScriptDeclarations;
use crate::type_::{PRELUDE_MODULE_NAME, RecordAccessor};
use crate::{
    ast::{CustomType, Function, Import, ModuleConstant, TypeAlias, *},
    docvec,
    line_numbers::LineNumbers,
    pretty::*,
};
use camino::Utf8Path;
use ecow::{EcoString, eco_format};
use expression::Context;
use itertools::Itertools;

use self::import::{Imports, Member};

const INDENT: isize = 2;

pub const PRELUDE: &str = include_str!("../templates/prelude.mjs");
pub const PRELUDE_TS_DEF: &str = include_str!("../templates/prelude.d.mts");

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
    typescript: TypeScriptDeclarations,
    stdlib_package: StdlibPackage,
    /// Relative path to the module, surrounded in `"`s to make it a string, and with `\`s escaped
    /// to `\\`.
    src_path: EcoString,
}

impl<'a> Generator<'a> {
    pub fn new(config: ModuleConfig<'a>) -> Self {
        let ModuleConfig {
            typescript,
            stdlib_package,
            module,
            line_numbers,
            src: _,
            path: _,
            project_root,
        } = config;
        let current_module_name_segments_count = module.name.split('/').count();

        let src_path = &module.type_info.src_path;
        let src_path = src_path
            .strip_prefix(project_root)
            .unwrap_or(src_path)
            .as_str();
        let src_path = eco_format!("\"{src_path}\"").replace("\\", "\\\\");

        Self {
            current_module_name_segments_count,
            line_numbers,
            module,
            src_path,
            tracker: UsageTracker::default(),
            module_scope: Default::default(),
            typescript,
            stdlib_package,
        }
    }

    fn type_reference(&self) -> Document<'a> {
        if self.typescript == TypeScriptDeclarations::None {
            return nil();
        }

        // Get the name of the module relative the directory (similar to basename)
        let module = self
            .module
            .name
            .as_str()
            .split('/')
            .next_back()
            .expect("JavaScript generator could not identify imported module name.");

        docvec!["/// <reference types=\"./", module, ".d.mts\" />", line()]
    }

    pub fn compile(&mut self) -> Document<'a> {
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
                .flat_map(|definition| self.definition(definition)),
        );

        // Two lines between each statement
        let mut statements = Itertools::intersperse(statements, lines(2)).collect_vec();

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

        if self.tracker.list_empty_class_used || self.tracker.echo_used {
            self.register_prelude_usage(&mut imports, "Empty", Some("$Empty"));
        };

        if self.tracker.list_non_empty_class_used || self.tracker.echo_used {
            self.register_prelude_usage(&mut imports, "NonEmpty", Some("$NonEmpty"));
        };

        if self.tracker.prepend_used {
            self.register_prelude_usage(&mut imports, "prepend", Some("listPrepend"));
        };

        if self.tracker.custom_type_used || self.tracker.echo_used {
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
        }

        if self.tracker.bit_array_slice_used || self.tracker.echo_used {
            self.register_prelude_usage(&mut imports, "bitArraySlice", None);
        }

        if self.tracker.bit_array_slice_to_float_used {
            self.register_prelude_usage(&mut imports, "bitArraySliceToFloat", None);
        }

        if self.tracker.bit_array_slice_to_int_used || self.tracker.echo_used {
            self.register_prelude_usage(&mut imports, "bitArraySliceToInt", None);
        }

        if self.tracker.sized_integer_segment_used {
            self.register_prelude_usage(&mut imports, "sizedInt", None);
        }

        if self.tracker.string_bit_array_segment_used {
            self.register_prelude_usage(&mut imports, "stringBits", None);
        }

        if self.tracker.string_utf16_bit_array_segment_used {
            self.register_prelude_usage(&mut imports, "stringToUtf16", None);
        }

        if self.tracker.string_utf32_bit_array_segment_used {
            self.register_prelude_usage(&mut imports, "stringToUtf32", None);
        }

        if self.tracker.codepoint_bit_array_segment_used {
            self.register_prelude_usage(&mut imports, "codepointBits", None);
        }

        if self.tracker.codepoint_utf16_bit_array_segment_used {
            self.register_prelude_usage(&mut imports, "codepointToUtf16", None);
        }

        if self.tracker.codepoint_utf32_bit_array_segment_used {
            self.register_prelude_usage(&mut imports, "codepointToUtf32", None);
        }

        if self.tracker.float_bit_array_segment_used {
            self.register_prelude_usage(&mut imports, "sizedFloat", None);
        }

        let echo_definition = self.echo_definition(&mut imports);
        let type_reference = self.type_reference();
        let filepath_definition = self.filepath_definition();

        // Put it all together

        if imports.is_empty() && statements.is_empty() {
            docvec![
                type_reference,
                filepath_definition,
                "export {}",
                line(),
                echo_definition
            ]
        } else if imports.is_empty() {
            statements.push(line());
            docvec![
                type_reference,
                filepath_definition,
                statements,
                echo_definition
            ]
        } else if statements.is_empty() {
            docvec![
                type_reference,
                imports.into_doc(JavaScriptCodegenTarget::JavaScript),
                filepath_definition,
                echo_definition,
            ]
        } else {
            docvec![
                type_reference,
                imports.into_doc(JavaScriptCodegenTarget::JavaScript),
                line(),
                filepath_definition,
                statements,
                line(),
                echo_definition
            ]
        }
    }

    fn echo_definition(&mut self, imports: &mut Imports<'a>) -> Document<'a> {
        if !self.tracker.echo_used {
            return nil();
        }

        if StdlibPackage::Present == self.stdlib_package {
            let value = Some((
                AssignName::Variable("stdlib$dict".into()),
                SrcSpan::default(),
            ));
            self.register_import(imports, "gleam_stdlib", "dict", &value, &[]);
        }
        self.register_prelude_usage(imports, "BitArray", Some("$BitArray"));
        self.register_prelude_usage(imports, "List", Some("$List"));
        self.register_prelude_usage(imports, "UtfCodepoint", Some("$UtfCodepoint"));
        docvec![line(), std::include_str!("../templates/echo.mjs"), line()]
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

    pub fn definition(&mut self, definition: &'a TypedDefinition) -> Option<Document<'a>> {
        match definition {
            Definition::TypeAlias(TypeAlias { .. }) => None,

            // Handled in collect_imports
            Definition::Import(Import { .. }) => None,

            // Handled in collect_definitions
            Definition::CustomType(CustomType { .. }) => None,

            // If a definition is unused then we don't need to generate code for it
            Definition::ModuleConstant(ModuleConstant { location, .. })
            | Definition::Function(Function { location, .. })
                if self
                    .module
                    .unused_definition_positions
                    .contains(&location.start) =>
            {
                None
            }

            Definition::ModuleConstant(ModuleConstant {
                publicity,
                name,
                value,
                documentation,
                ..
            }) => Some(self.module_constant(*publicity, name, value, documentation)),

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

                Some(self.module_function(function))
            }
        }
    }

    fn custom_type_definition(
        &mut self,
        name: &'a str,
        constructors: &'a [TypedRecordConstructor],
        publicity: Publicity,
        opaque: bool,
    ) -> Vec<Document<'a>> {
        // If there's no constructors then there's nothing to do here.
        if constructors.is_empty() {
            return vec![];
        }

        self.tracker.custom_type_used = true;

        let constructor_publicity = if opaque || publicity.is_private() {
            Publicity::Private
        } else {
            Publicity::Public
        };

        let mut definitions = constructors
            .iter()
            .map(|constructor| self.variant_definition(constructor, name, constructor_publicity))
            .collect_vec();

        // Generate getters for fields shared between variants
        if let Some(accessors_map) = self.module.type_info.accessors.get(name)
            && !accessors_map.shared_accessors.is_empty()
            // Don't bother generating shared getters when there's only one variant,
            // since the specific accessors can always be uses instead.
            && constructors.len() != 1
            // Only generate accessors for the API if the constructors are public
            && constructor_publicity.is_public()
        {
            definitions.push(self.shared_custom_type_fields(name, &accessors_map.shared_accessors));
        }

        definitions
    }

    fn variant_definition(
        &self,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
        publicity: Publicity,
    ) -> Document<'a> {
        let class_definition = self.variant_class_definition(constructor, publicity);

        // If the custom type is private or opaque, we don't need to generate API
        // functions for it.
        if publicity.is_private() {
            return class_definition;
        }

        let constructor_definition = self.variant_constructor_definition(constructor, type_name);
        let variant_check_definition = self.variant_check_definition(constructor, type_name);
        let fields_definition = self.variant_fields_definition(constructor, type_name);

        docvec![
            class_definition,
            line(),
            constructor_definition,
            line(),
            variant_check_definition,
            fields_definition,
        ]
    }

    fn variant_constructor_definition(
        &self,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
    ) -> Document<'a> {
        let mut arguments = Vec::new();

        for (index, parameter) in constructor.arguments.iter().enumerate() {
            if let Some((_, label)) = &parameter.label {
                arguments.push(maybe_escape_identifier(label).to_doc());
            } else {
                arguments.push(eco_format!("${index}").to_doc());
            }
        }

        let construction = docvec![
            break_("", " "),
            "new ",
            constructor.name.as_str(),
            "(",
            join(arguments.clone(), break_(",", ", ")).group(),
            ");"
        ]
        .group();

        docvec![
            "export const ",
            type_name,
            "$",
            constructor.name.as_str(),
            " = (",
            join(arguments, break_(",", ", ")),
            ") =>",
            construction.nest(INDENT),
        ]
    }

    fn variant_check_definition(
        &self,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
    ) -> Document<'a> {
        let construction = docvec![
            break_("", " "),
            "value instanceof ",
            constructor.name.as_str(),
            ";"
        ]
        .group();

        docvec![
            "export const ",
            type_name,
            "$is",
            constructor.name.as_str(),
            " = (value) =>",
            construction.nest(INDENT),
        ]
    }

    fn variant_fields_definition(
        &self,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
    ) -> Document<'a> {
        let mut functions = Vec::new();

        for (index, argument) in constructor.arguments.iter().enumerate() {
            // Always generate the accessor for the value at this index. Although
            // this is not necessary when a label is present, we want to make sure
            // that adding a label to a record isn't a breaking change. For this
            // reason, we need to generate an index getter even when a label is
            // present to ensure consistent behaviour between labelled and unlabelled
            // field access.
            let function_name = eco_format!(
                "{type_name}${record_name}${index}",
                record_name = constructor.name,
            );

            let contents;

            // If the argument is labelled, also generate a getter for the labelled
            // argument.
            if let Some((_, label)) = &argument.label {
                let function_name = eco_format!(
                    "{type_name}${record_name}${label}",
                    record_name = constructor.name,
                );

                contents =
                    docvec![break_("", " "), "value.", maybe_escape_property(label), ";"].group();

                functions.push(docvec![
                    line(),
                    "export const ",
                    function_name,
                    " = (value) =>",
                    contents.clone().nest(INDENT),
                ]);
            } else {
                contents = docvec![break_("", " "), "value[", index, "];"].group()
            }

            functions.push(docvec![
                line(),
                "export const ",
                function_name,
                " = (value) =>",
                contents.nest(INDENT),
            ]);
        }

        concat(functions)
    }

    fn shared_custom_type_fields(
        &self,
        type_name: &'a str,
        shared_accessors: &HashMap<EcoString, RecordAccessor>,
    ) -> Document<'a> {
        let accessors = shared_accessors.keys().sorted().map(|field| {
            let function_name = eco_format!("{type_name}${field}");

            let contents =
                docvec![break_("", " "), "value.", maybe_escape_property(field), ";"].group();

            docvec![
                "export const ",
                function_name,
                " = (value) =>",
                contents.nest(INDENT),
            ]
        });
        concat(Itertools::intersperse(accessors, line()))
    }

    fn variant_class_definition(
        &self,
        constructor: &'a TypedRecordConstructor,
        publicity: Publicity,
    ) -> Document<'a> {
        fn parameter((i, arg): (usize, &TypedRecordConstructorArg)) -> Document<'_> {
            arg.label
                .as_ref()
                .map(|(_, s)| maybe_escape_identifier(s))
                .unwrap_or_else(|| eco_format!("${i}"))
                .to_doc()
        }

        let doc = if let Some((_, documentation)) = &constructor.documentation {
            jsdoc_comment(documentation, publicity).append(line())
        } else {
            nil()
        };

        let head = if publicity.is_public() {
            "export class "
        } else {
            "class "
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
                        docvec!["this.", maybe_escape_property(name), " = ", var, ";"]
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

        docvec![doc, head, class_body, line(), "}"]
    }

    fn collect_definitions(&mut self) -> Vec<Document<'a>> {
        self.module
            .definitions
            .iter()
            .flat_map(|definition| match definition {
                // If a custom type is unused then we don't need to generate code for it
                Definition::CustomType(CustomType { location, .. })
                    if self
                        .module
                        .unused_definition_positions
                        .contains(&location.start) =>
                {
                    vec![]
                }

                Definition::CustomType(CustomType {
                    publicity,
                    constructors,
                    opaque,
                    name,
                    ..
                }) => self.custom_type_definition(name, constructors, *publicity, *opaque),

                Definition::Function(Function { .. })
                | Definition::TypeAlias(TypeAlias { .. })
                | Definition::Import(Import { .. })
                | Definition::ModuleConstant(ModuleConstant { .. }) => vec![],
            })
            .collect()
    }

    fn collect_imports(&mut self) -> Imports<'a> {
        let mut imports = Imports::new();

        for definition in &self.module.definitions {
            match definition {
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
        as_name: &Option<(AssignName, SrcSpan)>,
        unqualified: &[UnqualifiedImport],
    ) {
        let get_name = |module: &'a str| {
            module
                .split('/')
                .next_back()
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
                maybe_escape_identifier(n).to_doc()
            });
            let name = maybe_escape_identifier(&i.name).to_doc();
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
        name: &'a EcoString,
        value: &'a TypedConstant,
        documentation: &'a Option<(u32, EcoString)>,
    ) -> Document<'a> {
        let head = if publicity.is_private() {
            "const "
        } else {
            "export const "
        };

        let mut generator = expression::Generator::new(
            self.module.name.clone(),
            self.src_path.clone(),
            self.line_numbers,
            "".into(),
            vec![],
            &mut self.tracker,
            self.module_scope.clone(),
        );

        let document = generator.constant_expression(Context::Constant, value);

        let jsdoc = if let Some((_, documentation)) = documentation {
            jsdoc_comment(documentation, publicity).append(line())
        } else {
            nil()
        };

        docvec![
            jsdoc,
            head,
            maybe_escape_identifier(name),
            " = ",
            document,
            ";",
        ]
    }

    fn register_in_scope(&mut self, name: &str) {
        let _ = self.module_scope.insert(name.into(), 0);
    }

    fn module_function(&mut self, function: &'a TypedFunction) -> Document<'a> {
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
            self.src_path.clone(),
            self.line_numbers,
            name.clone(),
            argument_names,
            &mut self.tracker,
            self.module_scope.clone(),
        );

        let function_doc = match &function.documentation {
            None => nil(),
            Some((_, documentation)) => {
                jsdoc_comment(documentation, function.publicity).append(line())
            }
        };

        let head = if function.publicity.is_private() {
            "function "
        } else {
            "export function "
        };

        let body = generator.function_body(function.body.as_slice(), function.arguments.as_slice());

        docvec![
            function_doc,
            head,
            maybe_escape_identifier(name.as_str()),
            fun_arguments(function.arguments.as_slice(), generator.tail_recursion_used),
            " {",
            docvec![line(), body].nest(INDENT).group(),
            line(),
            "}",
        ]
    }

    fn register_module_definitions_in_scope(&mut self) {
        for definition in self.module.definitions.iter() {
            match definition {
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

    fn filepath_definition(&self) -> Document<'a> {
        if !self.tracker.make_error_used {
            return nil();
        }

        docvec!["const FILEPATH = ", self.src_path.clone(), ';', lines(2)]
    }
}

fn jsdoc_comment(documentation: &EcoString, publicity: Publicity) -> Document<'_> {
    let doc_lines = documentation
        .trim_end()
        .split('\n')
        .map(|line| eco_format!(" *{line}", line = line.replace("*/", "*\\/")).to_doc())
        .collect_vec();

    // We start with the documentation of the function
    let doc_body = join(doc_lines, line());
    let mut doc = docvec!["/**", line(), doc_body, line()];
    if !publicity.is_public() {
        // If the function is not public we hide the documentation using
        // the `@ignore` tag: https://jsdoc.app/tags-ignore
        doc = docvec![doc, " * ", line(), " * @ignore", line()];
    }
    // And finally we close the doc comment
    docvec![doc, " */"]
}

#[derive(Debug)]
pub struct ModuleConfig<'a> {
    pub module: &'a TypedModule,
    pub line_numbers: &'a LineNumbers,
    pub src: &'a EcoString,
    pub typescript: TypeScriptDeclarations,
    pub stdlib_package: StdlibPackage,
    pub path: &'a Utf8Path,
    pub project_root: &'a Utf8Path,
}

pub fn module(config: ModuleConfig<'_>) -> String {
    let document = Generator::new(config).compile();
    document.to_pretty_string(80)
}

pub fn ts_declaration(module: &TypedModule) -> String {
    let document = typescript::TypeScriptGenerator::new(module).compile();
    document.to_pretty_string(80)
}

fn fun_arguments(arguments: &'_ [TypedArg], tail_recursion_used: bool) -> Document<'_> {
    let mut discards = 0;
    wrap_arguments(
        arguments
            .iter()
            .map(|argument| match argument.get_variable_name() {
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
                Some(name) => maybe_escape_identifier(name).to_doc(),
            }),
    )
}

fn wrap_arguments<'a, I>(arguments: I) -> Document<'a>
where
    I: IntoIterator<Item = Document<'a>>,
{
    break_("", "")
        .append(join(arguments, break_(",", ", ")))
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
    match label {
        // `then` to avoid a custom type that defines a `then` function being
        // used as a `thenable` in Javascript.
        "then"
        // `constructor` to avoid unintentional overriding of the constructor of
        // records, leading to potential runtime crashes while using `withFields`.
        | "constructor"
        // `prototype` and `__proto__` to avoid unintentionally overriding the
        // prototype chain.
        | "prototype" | "__proto__" => false,
        _ => true
    }
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

fn maybe_escape_identifier(word: &str) -> EcoString {
    if is_usable_js_identifier(word) {
        EcoString::from(word)
    } else {
        escape_identifier(word)
    }
}

fn maybe_escape_property(label: &str) -> EcoString {
    if is_usable_js_property(label) {
        EcoString::from(label)
    } else {
        escape_identifier(label)
    }
}

#[derive(Debug, Default)]
pub(crate) struct UsageTracker {
    pub ok_used: bool,
    pub list_used: bool,
    pub list_empty_class_used: bool,
    pub list_non_empty_class_used: bool,
    pub prepend_used: bool,
    pub error_used: bool,
    pub int_remainder_used: bool,
    pub make_error_used: bool,
    pub custom_type_used: bool,
    pub int_division_used: bool,
    pub float_division_used: bool,
    pub object_equality_used: bool,
    pub bit_array_literal_used: bool,
    pub bit_array_slice_used: bool,
    pub bit_array_slice_to_float_used: bool,
    pub bit_array_slice_to_int_used: bool,
    pub sized_integer_segment_used: bool,
    pub string_bit_array_segment_used: bool,
    pub string_utf16_bit_array_segment_used: bool,
    pub string_utf32_bit_array_segment_used: bool,
    pub codepoint_bit_array_segment_used: bool,
    pub codepoint_utf16_bit_array_segment_used: bool,
    pub codepoint_utf32_bit_array_segment_used: bool,
    pub float_bit_array_segment_used: bool,
    pub echo_used: bool,
}

fn bool(bool: bool) -> Document<'static> {
    match bool {
        true => "true".to_doc(),
        false => "false".to_doc(),
    }
}

/// Int segments <= 48 bits wide in bit arrays are within JavaScript's safe range and are evaluated
/// at compile time when all inputs are known. This is done for both bit array expressions and
/// pattern matching.
///
/// Int segments of any size could be evaluated at compile time, but currently aren't due to the
/// potential for causing large generated JS for inputs such as `<<0:8192>>`.
///
pub(crate) const SAFE_INT_SEGMENT_MAX_SIZE: usize = 48;

/// Evaluates the value of an Int segment in a bit array into its corresponding bytes. This avoids
/// needing to do the evaluation at runtime when all inputs are known at compile-time.
///
pub(crate) fn bit_array_segment_int_value_to_bytes(
    mut value: BigInt,
    size: BigInt,
    endianness: Endianness,
) -> Vec<u8> {
    // Clamp negative sizes to zero
    let size = size.max(BigInt::ZERO);

    // Convert size to u32. This is safe because this function isn't called with a size greater
    // than `SAFE_INT_SEGMENT_MAX_SIZE`.
    let size = size
        .to_u32()
        .expect("bit array segment size to be a valid u32");

    // Convert negative number to two's complement representation
    if value < BigInt::ZERO {
        let value_modulus = BigInt::from(2).pow(size);
        value = &value_modulus + (value % &value_modulus);
    }

    // Convert value to the desired number of bytes
    let mut bytes = vec![0u8; size as usize / 8];
    for byte in bytes.iter_mut() {
        *byte = (&value % BigInt::from(256))
            .to_u8()
            .expect("modulo result to be a valid u32");
        value /= BigInt::from(256);
    }

    if endianness.is_big() {
        bytes.reverse();
    }

    bytes
}
