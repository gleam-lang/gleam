//! This module is responsible for generating TypeScript type declaration files.
//! This code is run during the code generation phase along side the normal
//! Javascript code emission. Here we walk through the typed AST and translate
//! the Gleam statements into their TypeScript equivalent. Unlike the Javascript
//! code generation, the TypeScript generation only needs to look at the module
//! statements and not the expressions that may be _inside_ those statements.
//! This is due to the TypeScript declarations only caring about inputs and outputs
//! rather than _how_ those outputs are generated.
//!
//! ## Links
//! <https://www.typescriptlang.org/>
//! <https://www.typescriptlang.org/docs/handbook/declaration-files/introduction.html>

use crate::ast::{
    AssignName, Publicity, TypedCustomType, TypedFunction, TypedModuleConstant, TypedTypeAlias,
};
use crate::javascript::import::Member;
use crate::type_::{PRELUDE_MODULE_NAME, RecordAccessor, is_prelude_module};
use crate::{
    ast::{TypedModule, TypedRecordConstructor},
    docvec,
    javascript::JavaScriptCodegenTarget,
    pretty::{Document, Documentable, break_},
    type_::{Type, TypeVar},
};
use ecow::{EcoString, eco_format};
use itertools::Itertools;
use std::{collections::HashMap, ops::Deref, sync::Arc};

use super::{INDENT, concat, import::Imports, join, line, lines, wrap_arguments};

/// When rendering a type variable to an TypeScript type spec we need all type
/// variables with the same id to end up with the same name in the generated
/// TypeScript. This function converts a usize into base 26 A-Z for this purpose.
fn id_to_type_var(id: u64) -> Document<'static> {
    if id < 26 {
        return std::iter::once(
            std::char::from_u32((id % 26 + 65) as u32).expect("id_to_type_var 0"),
        )
        .collect::<EcoString>()
        .to_doc();
    }
    let mut name = vec![];
    let mut last_char = id;
    while last_char >= 26 {
        name.push(std::char::from_u32((last_char % 26 + 65) as u32).expect("id_to_type_var 1"));
        last_char /= 26;
    }
    name.push(std::char::from_u32((last_char % 26 + 64) as u32).expect("id_to_type_var 2"));
    name.reverse();
    name.into_iter().collect::<EcoString>().to_doc()
}

fn name_with_generics<'a>(
    name: Document<'a>,
    types: impl IntoIterator<Item = &'a Arc<Type>>,
) -> Document<'a> {
    let generic_usages = collect_generic_usages(HashMap::new(), types);
    let generic_names: Vec<Document<'_>> = generic_usages
        .keys()
        .sorted()
        .map(|id| id_to_type_var(*id))
        .collect();

    docvec![
        name,
        if generic_names.is_empty() {
            super::nil()
        } else {
            wrap_generic_arguments(generic_names)
        },
    ]
}

/// A generic can either be rendered as an actual type variable such as `A` or `B`,
/// or it can be rendered as `any` depending on how many usages it has. If it
/// has only 1 usage it is an `any` type. If it has more than 1 usage it is a
/// TS generic. This function gathers usages for this determination.
///
///   Examples:
///     fn(a) -> String       // `a` is `any`
///     `fn()` -> Result(a, b)  // `a` and `b` are `any`
///     fn(a) -> a            // `a` is a generic
fn collect_generic_usages<'a>(
    mut ids: HashMap<u64, u64>,
    types: impl IntoIterator<Item = &'a Arc<Type>>,
) -> HashMap<u64, u64> {
    for type_ in types {
        generic_ids(type_, &mut ids);
    }
    ids
}

fn generic_ids(type_: &Type, ids: &mut HashMap<u64, u64>) {
    match type_ {
        Type::Var { type_ } => match type_.borrow().deref() {
            TypeVar::Unbound { id, .. } | TypeVar::Generic { id, .. } => {
                let count = ids.entry(*id).or_insert(0);
                *count += 1;
            }
            TypeVar::Link { type_ } => generic_ids(type_, ids),
        },
        Type::Named { arguments, .. } => {
            for argument in arguments {
                generic_ids(argument, ids)
            }
        }
        Type::Fn { arguments, return_ } => {
            for argument in arguments {
                generic_ids(argument, ids)
            }
            generic_ids(return_, ids);
        }
        Type::Tuple { elements } => {
            for element in elements {
                generic_ids(element, ids)
            }
        }
    }
}

/// Prints a Gleam tuple in the TypeScript equivalent syntax
///
fn tuple<'a>(elements: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    break_("", "")
        .append(join(elements, break_(",", ", ")))
        .nest(INDENT)
        .append(break_("", ""))
        .surround("[", "]")
        .group()
}

fn wrap_generic_arguments<'a, I>(arguments: I) -> Document<'a>
where
    I: IntoIterator<Item = Document<'a>>,
{
    break_("", "")
        .append(join(arguments, break_(",", ", ")))
        .nest(INDENT)
        .append(break_("", ""))
        .surround("<", ">")
        .group()
}

/// Returns a name that can be used as a TypeScript type name. If there is a
/// naming clash a '_' will be appended.
///
fn ts_safe_type_name(mut name: String) -> EcoString {
    if matches!(
        name.as_str(),
        "any"
            | "boolean"
            | "constructor"
            | "declare"
            | "get"
            | "module"
            | "require"
            | "number"
            | "set"
            | "string"
            | "symbol"
            | "type"
            | "from"
            | "of"
    ) {
        name.push('_');
        EcoString::from(name)
    } else {
        super::maybe_escape_identifier_string(&name)
    }
}

/// The `TypeScriptGenerator` contains the logic of how to convert Gleam's typed
/// AST into the equivalent TypeScript type declaration file.
///
#[derive(Debug)]
pub struct TypeScriptGenerator<'a> {
    module: &'a TypedModule,
    aliased_module_names: HashMap<&'a str, &'a str>,
    tracker: UsageTracker,
    current_module_name_segments_count: usize,
}

impl<'a> TypeScriptGenerator<'a> {
    pub fn new(module: &'a TypedModule) -> Self {
        let current_module_name_segments_count = module.name.split('/').count();
        Self {
            module,
            aliased_module_names: HashMap::new(),
            tracker: UsageTracker::default(),
            current_module_name_segments_count,
        }
    }

    pub fn compile(&mut self) -> Document<'a> {
        let mut imports = self.collect_imports();
        let statements = self.definitions(&mut imports);

        // Two lines between each statement
        let mut statements = Itertools::intersperse(statements.into_iter(), lines(2)).collect_vec();

        // Put it all together

        if self.prelude_used() {
            let path = self.import_path(&self.module.type_info.package, PRELUDE_MODULE_NAME);
            imports.register_module(path, ["_".into()], []);
        }

        if imports.is_empty() && statements.is_empty() {
            docvec!["export {}", line()]
        } else if imports.is_empty() {
            statements.push(line());
            statements.to_doc()
        } else if statements.is_empty() {
            imports.into_doc(JavaScriptCodegenTarget::TypeScriptDeclarations)
        } else {
            docvec![
                imports.into_doc(JavaScriptCodegenTarget::TypeScriptDeclarations),
                line(),
                statements,
                line()
            ]
        }
    }

    fn collect_imports(&mut self) -> Imports<'a> {
        let mut imports = Imports::new();

        for function in &self.module.definitions.functions {
            for argument in &function.arguments {
                self.collect_imports_for_type(&argument.type_, &mut imports);
            }
            self.collect_imports_for_type(&function.return_type, &mut imports);
        }

        for type_alias in &self.module.definitions.type_aliases {
            self.collect_imports_for_type(&type_alias.type_, &mut imports);
        }

        for custom_type in &self.module.definitions.custom_types {
            for type_ in &custom_type.typed_parameters {
                self.collect_imports_for_type(type_, &mut imports);
            }

            for constructor in &custom_type.constructors {
                for argument in &constructor.arguments {
                    self.collect_imports_for_type(&argument.type_, &mut imports);
                }
            }
        }

        for constant in &self.module.definitions.constants {
            self.collect_imports_for_type(&constant.type_, &mut imports);
        }

        for import in &self.module.definitions.imports {
            match &import.as_name {
                Some((AssignName::Variable(name), _)) => {
                    let _ = self.aliased_module_names.insert(&import.module, name);
                }
                Some((AssignName::Discard(_), _)) | None => (),
            }
        }

        imports
    }

    /// Recurses through a type and any types it references, registering all of their imports.
    ///
    fn collect_imports_for_type<'b>(&mut self, type_: &'b Type, imports: &mut Imports<'a>) {
        match &type_ {
            Type::Named {
                package,
                module,
                arguments,
                ..
            } => {
                let is_prelude = module == "gleam" && package.is_empty();
                let is_current_module = *module == self.module.name;

                if !is_prelude && !is_current_module {
                    self.register_import(imports, package, module);
                }

                for argument in arguments {
                    self.collect_imports_for_type(argument, imports);
                }
            }
            Type::Fn { arguments, return_ } => {
                for argument in arguments {
                    self.collect_imports_for_type(argument, imports);
                }
                self.collect_imports_for_type(return_, imports);
            }
            Type::Tuple { elements } => {
                for element in elements {
                    self.collect_imports_for_type(element, imports);
                }
            }
            Type::Var { type_ } => {
                if let TypeVar::Link { type_ } = type_
                    .as_ref()
                    .try_borrow()
                    .expect("borrow type after inference")
                    .deref()
                {
                    self.collect_imports_for_type(type_, imports);
                }
            }
        }
    }

    /// Registers an import of an external module so that it can be added to
    /// the top of the generated Document. The module names are prefixed with a
    /// "$" symbol to prevent any clashes with other Gleam names that may be
    /// used in this module.
    ///
    fn register_import<'b>(
        &mut self,
        imports: &mut Imports<'a>,
        package: &'b str,
        module: &'b str,
    ) {
        let path = self.import_path(package, module);
        imports.register_module(path, [self.module_name(module)], []);
    }

    /// Calculates the path of where to import an external module from
    ///
    fn import_path<'b>(&self, package: &'b str, module: &'b str) -> EcoString {
        // DUPE: current_module_name_segments_count
        // TODO: strip shared prefixed between current module and imported
        // module to avoid descending and climbing back out again
        if package == self.module.type_info.package || package.is_empty() {
            // Same package
            match self.current_module_name_segments_count {
                1 => eco_format!("./{module}.d.mts"),
                _ => {
                    let prefix = "../".repeat(self.current_module_name_segments_count - 1);
                    eco_format!("{prefix}{module}.d.mts")
                }
            }
        } else {
            // Different package
            let prefix = "../".repeat(self.current_module_name_segments_count);
            eco_format!("{prefix}{package}/{module}.d.mts")
        }
    }

    fn definitions(&mut self, imports: &mut Imports<'_>) -> Vec<Document<'a>> {
        let mut documents = vec![];

        for custom_type in &self.module.definitions.custom_types {
            if let Some(mut new_documents) = self.custom_type_definition(custom_type, imports) {
                documents.append(&mut new_documents);
            }
        }

        for type_alias in &self.module.definitions.type_aliases {
            if let Some(document) = self.type_alias(type_alias) {
                documents.push(document);
            }
        }

        for constant in &self.module.definitions.constants {
            if let Some(document) = self.module_constant(constant) {
                documents.push(document);
            }
        }

        for function in &self.module.definitions.functions {
            if let Some(document) = self.module_function(function) {
                documents.push(document);
            }
        }

        documents
    }

    fn type_alias(&mut self, type_alias: &TypedTypeAlias) -> Option<Document<'a>> {
        if !type_alias.publicity.is_importable() {
            return None;
        }

        Some(docvec![
            "export type ",
            ts_safe_type_name(type_alias.alias.to_string()),
            " = ",
            self.print_type(&type_alias.type_),
            ";"
        ])
    }

    /// Converts a Gleam custom type definition into the TypeScript equivalent.
    /// In Gleam, all custom types have one to many concrete constructors. This
    /// function first converts the constructors into TypeScript then finally
    /// emits a union type to represent the TypeScript type itself. Because in
    /// Gleam constructors can have the same name as the custom type, here we
    /// append a "$" symbol to the emitted TypeScript type to prevent those
    /// naming classes.
    ///
    fn custom_type_definition(
        &mut self,
        custom_type: &'a TypedCustomType,
        imports: &mut Imports<'_>,
    ) -> Option<Vec<Document<'a>>> {
        let TypedCustomType {
            name,
            publicity,
            constructors,
            opaque,
            typed_parameters,
            external_javascript,
            ..
        } = custom_type;

        // Constructors for opaque and private types are not exported
        let constructor_publicity = if publicity.is_private() || *opaque {
            Publicity::Private
        } else {
            Publicity::Public
        };

        let type_name = name_with_generics(eco_format!("{name}$").to_doc(), typed_parameters);

        let mut definitions = constructors
            .iter()
            .map(|constructor| {
                self.variant_definition(
                    constructor,
                    constructor_publicity,
                    name,
                    &type_name,
                    typed_parameters,
                )
            })
            .collect_vec();

        let definition = if constructors.is_empty() {
            if let Some((module, external_name, _location)) = external_javascript {
                let member = Member {
                    name: external_name.to_doc(),
                    alias: Some(eco_format!("{name}$").to_doc()),
                };
                imports.register_export(eco_format!("{name}$"));

                imports.register_module(module.clone(), [], [member]);
                return Some(Vec::new());
            } else {
                "any".to_doc()
            }
        } else {
            let constructors = constructors.iter().map(|x| {
                name_with_generics(
                    super::maybe_escape_identifier(&x.name).to_doc(),
                    x.arguments.iter().map(|a| &a.type_),
                )
            });
            join(constructors, break_("| ", " | "))
        };

        let head = if publicity.is_private() {
            "type "
        } else {
            "export type "
        };

        definitions.push(docvec![head, type_name.clone(), " = ", definition, ";",]);

        // Generate getters for fields shared between variants
        if let Some(accessors_map) = self.module.type_info.accessors.get(name)
            && !accessors_map.shared_accessors.is_empty()
            // Don't bother generating shared getters when there's only one variant,
            // since the specific accessors can always be uses instead.
            && constructors.len() != 1
            // Only generate accessors for the API if the constructors are public
            && constructor_publicity.is_public()
        {
            definitions.push(self.shared_custom_type_fields(
                name,
                &type_name,
                typed_parameters,
                &accessors_map.shared_accessors,
            ));
        }

        Some(definitions)
    }

    fn variant_definition(
        &mut self,
        constructor: &'a TypedRecordConstructor,
        publicity: Publicity,
        type_name: &'a str,
        type_name_with_generics: &Document<'a>,
        type_parameters: &'a [Arc<Type>],
    ) -> Document<'a> {
        self.set_prelude_used();
        let class_definition = self.variant_class_definition(constructor, publicity);

        // If the custom type is private or opaque, we don't need to generate API
        // functions for it.
        if publicity.is_private() {
            return class_definition;
        }

        let constructor_definition = self.variant_constructor_definition(
            constructor,
            type_name,
            type_name_with_generics,
            type_parameters,
        );
        let variant_check_definition = self.variant_check_definition(
            constructor,
            type_name,
            type_name_with_generics,
            type_parameters,
        );
        let fields_definition = self.variant_fields_definition(
            constructor,
            type_name,
            type_name_with_generics,
            type_parameters,
        );

        docvec![
            class_definition,
            line(),
            constructor_definition,
            line(),
            variant_check_definition,
            fields_definition,
        ]
    }

    fn variant_class_definition(
        &mut self,
        constructor: &'a TypedRecordConstructor,
        publicity: Publicity,
    ) -> Document<'a> {
        let head = docvec![
            if publicity.is_public() {
                "export ".to_doc()
            } else {
                "declare ".to_doc()
            },
            "class ",
            name_with_generics(
                super::maybe_escape_identifier(&constructor.name).to_doc(),
                constructor.arguments.iter().map(|a| &a.type_)
            ),
            " extends _.CustomType {"
        ];

        if constructor.arguments.is_empty() {
            return head.append("}");
        };

        let class_body = docvec![
            line(),
            "/** @deprecated */",
            line(),
            // First add the constructor
            "constructor",
            wrap_arguments(
                constructor
                    .arguments
                    .iter()
                    .enumerate()
                    .map(|(i, argument)| {
                        let name = argument
                            .label
                            .as_ref()
                            .map(|(_, s)| super::maybe_escape_identifier(s))
                            .unwrap_or_else(|| eco_format!("argument${i}"))
                            .to_doc();
                        docvec![
                            name,
                            ": ",
                            self.do_print_force_generic_param(&argument.type_)
                        ]
                    })
            ),
            ";",
            line(),
            // Then add each field to the class
            join(
                constructor.arguments.iter().enumerate().map(|(i, arg)| {
                    let name = arg
                        .label
                        .as_ref()
                        .map(|(_, s)| super::maybe_escape_identifier(s))
                        .unwrap_or_else(|| eco_format!("{i}"))
                        .to_doc();
                    docvec![
                        "/** @deprecated */",
                        line(),
                        name,
                        ": ",
                        self.do_print_force_generic_param(&arg.type_),
                        ";"
                    ]
                }),
                line(),
            ),
        ]
        .nest(INDENT);

        docvec![head, class_body, line(), "}"]
    }

    fn variant_constructor_definition(
        &mut self,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
        type_name_with_generics: &Document<'a>,
        type_parameters: &'a [Arc<Type>],
    ) -> Document<'a> {
        let mut arguments = Vec::new();

        for (index, parameter) in constructor.arguments.iter().enumerate() {
            let name = if let Some((_, label)) = &parameter.label {
                super::maybe_escape_identifier(label)
            } else {
                eco_format!("${index}")
            };

            arguments.push(docvec![
                name,
                ": ",
                self.do_print_force_generic_param(&parameter.type_)
            ])
        }

        let function_name = eco_format!(
            "{type_name}${variant_name}",
            variant_name = constructor.name
        )
        .to_doc();

        let has_arguments = !arguments.is_empty();

        docvec![
            "export function ",
            name_with_generics(function_name, type_parameters),
            "(",
            docvec![break_("", ""), join(arguments, break_(",", ", ")),].nest(INDENT),
            break_(if has_arguments { "," } else { "" }, ""),
            "): ",
            type_name_with_generics.clone(),
            ";"
        ]
        .group()
    }

    fn variant_check_definition(
        &self,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
        type_name_with_generics: &Document<'a>,
        type_parameters: &'a [Arc<Type>],
    ) -> Document<'a> {
        let function_name = eco_format!(
            "{type_name}$is{variant_name}",
            variant_name = constructor.name
        )
        .to_doc();

        docvec![
            "export function ",
            name_with_generics(function_name, type_parameters),
            "(",
            docvec![break_("", "",), "value: ", type_name_with_generics.clone(),].nest(INDENT),
            break_(",", ""),
            "): boolean;",
        ]
        .group()
    }

    fn variant_fields_definition(
        &mut self,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
        type_name_with_generics: &Document<'a>,
        type_parameters: &'a [Arc<Type>],
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
                "{type_name}${variant_name}${index}",
                variant_name = constructor.name
            )
            .to_doc();

            functions.push(
                docvec![
                    line(),
                    "export function ",
                    name_with_generics(function_name, type_parameters),
                    "(",
                    docvec![break_("", "",), "value: ", type_name_with_generics.clone(),]
                        .nest(INDENT),
                    break_(",", ""),
                    "): ",
                    self.do_print_force_generic_param(&argument.type_),
                    ";",
                ]
                .group(),
            );

            // If the argument is labelled, also generate a getter for the labelled
            // argument.
            if let Some((_, label)) = &argument.label {
                let function_name = eco_format!(
                    "{type_name}${variant_name}${label}",
                    variant_name = constructor.name
                )
                .to_doc();

                functions.push(
                    docvec![
                        line(),
                        "export function ",
                        name_with_generics(function_name, type_parameters),
                        "(",
                        docvec![break_("", "",), "value: ", type_name_with_generics.clone(),]
                            .nest(INDENT),
                        break_(",", ""),
                        "): ",
                        self.do_print_force_generic_param(&argument.type_),
                        ";",
                    ]
                    .group(),
                );
            }
        }

        concat(functions)
    }

    fn shared_custom_type_fields(
        &mut self,
        type_name: &'a str,
        type_name_with_generics: &Document<'a>,
        type_parameters: &'a [Arc<Type>],
        shared_accessors: &HashMap<EcoString, RecordAccessor>,
    ) -> Document<'a> {
        let accessors = shared_accessors
            .iter()
            .sorted_by_key(|(name, _)| *name)
            .map(|(field, accessor)| {
                let function_name = eco_format!("{type_name}${field}").to_doc();

                docvec![
                    "export function ",
                    name_with_generics(function_name, type_parameters),
                    "(",
                    docvec![break_("", "",), "value: ", type_name_with_generics.clone(),]
                        .nest(INDENT),
                    break_(",", ""),
                    "): ",
                    self.do_print_force_generic_param(&accessor.type_),
                    ";"
                ]
                .group()
            });
        join(accessors, line())
    }

    fn module_constant(&mut self, constant: &TypedModuleConstant) -> Option<Document<'a>> {
        if !constant.publicity.is_importable() {
            return None;
        }

        Some(docvec![
            "export const ",
            super::maybe_escape_identifier(&constant.name),
            ": ",
            self.print_type(&constant.value.type_()),
            ";",
        ])
    }

    fn module_function(&mut self, function: &'a TypedFunction) -> Option<Document<'a>> {
        let TypedFunction {
            name: Some((_, name)),
            arguments,
            publicity,
            return_type,
            ..
        } = function
        else {
            return None;
        };

        if !publicity.is_importable() {
            return None;
        }

        let generic_usages = collect_generic_usages(
            HashMap::new(),
            std::iter::once(return_type).chain(arguments.iter().map(|a| &a.type_)),
        );
        let generic_names: Vec<Document<'_>> = generic_usages
            .iter()
            .filter(|(_id, use_count)| **use_count > 1)
            .sorted_by_key(|x| x.0)
            .map(|(id, _use_count)| id_to_type_var(*id))
            .collect();

        Some(docvec![
            "export function ",
            super::maybe_escape_identifier(name),
            if generic_names.is_empty() {
                super::nil()
            } else {
                wrap_generic_arguments(generic_names)
            },
            wrap_arguments(arguments.iter().enumerate().map(|(i, argument)| {
                match argument.get_variable_name() {
                    None => {
                        docvec![
                            "x",
                            i,
                            ": ",
                            self.print_type_with_generic_usages(&argument.type_, &generic_usages)
                        ]
                    }
                    Some(name) => docvec![
                        super::maybe_escape_identifier(name),
                        ": ",
                        self.print_type_with_generic_usages(&argument.type_, &generic_usages)
                    ],
                }
            }),),
            ": ",
            self.print_type_with_generic_usages(return_type, &generic_usages),
            ";",
        ])
    }

    /// Converts a Gleam type into a TypeScript type string
    ///
    pub fn print_type(&mut self, type_: &Type) -> Document<'static> {
        self.do_print(type_, GenericPrinting::AsAny)
    }

    /// Helper function for generating a TypeScript type string after collecting
    /// all of the generics used in a statement
    ///
    pub fn print_type_with_generic_usages(
        &mut self,
        type_: &Type,
        generic_usages: &HashMap<u64, u64>,
    ) -> Document<'static> {
        self.do_print(type_, GenericPrinting::FromUsage(generic_usages))
    }

    /// Get the locally used name for a module. Either the last segment, or the
    /// alias if one was given when imported.
    ///
    fn module_name(&self, name: &str) -> EcoString {
        // The prelude is always `_`
        if name.is_empty() {
            return "_".into();
        }

        let name = match self.aliased_module_names.get(name) {
            Some(name) => name,
            None => name.split('/').next_back().expect("Non empty module path"),
        };

        eco_format!("${name}")
    }

    fn do_print(
        &mut self,
        type_: &Type,
        generic_printing: GenericPrinting<'_>,
    ) -> Document<'static> {
        match type_ {
            Type::Var { type_ } => self.print_var(&type_.borrow(), generic_printing),

            Type::Named {
                name,
                module,
                arguments,
                ..
            } if is_prelude_module(module) => {
                self.print_prelude_type(name, arguments, generic_printing)
            }

            Type::Named {
                name,
                arguments,
                module,
                ..
            } => self.print_type_app(name, arguments, module, generic_printing),

            Type::Fn { arguments, return_ } => self.print_fn(arguments, return_, generic_printing),

            Type::Tuple { elements } => tuple(
                elements
                    .iter()
                    .map(|element| self.do_print(element, generic_printing)),
            ),
        }
    }

    fn do_print_force_generic_param(&mut self, type_: &Type) -> Document<'static> {
        match type_ {
            Type::Var { type_ } => self.print_var(&type_.borrow(), GenericPrinting::AlwaysGeneric),

            Type::Named {
                name,
                module,
                arguments,
                ..
            } if is_prelude_module(module) => {
                self.print_prelude_type(name, arguments, GenericPrinting::AlwaysGeneric)
            }

            Type::Named {
                name,
                arguments,
                module,
                ..
            } => self.print_type_app(name, arguments, module, GenericPrinting::AlwaysGeneric),

            Type::Fn { arguments, return_ } => {
                self.print_fn(arguments, return_, GenericPrinting::AlwaysGeneric)
            }

            Type::Tuple { elements } => tuple(
                elements
                    .iter()
                    .map(|element| self.do_print(element, GenericPrinting::AlwaysGeneric)),
            ),
        }
    }

    fn print_var(
        &mut self,
        type_: &TypeVar,
        generic_printing: GenericPrinting<'_>,
    ) -> Document<'static> {
        match type_ {
            TypeVar::Unbound { id } | TypeVar::Generic { id } => match generic_printing {
                GenericPrinting::FromUsage(usages) => match usages.get(id) {
                    Some(&0) => super::nil(),
                    Some(&1) => "any".to_doc(),
                    _ => id_to_type_var(*id),
                },
                GenericPrinting::AlwaysGeneric => id_to_type_var(*id),
                GenericPrinting::AsAny => "any".to_doc(),
            },
            TypeVar::Link { type_ } => self.do_print(type_, generic_printing),
        }
    }

    /// Prints a type coming from the Gleam prelude module. These are often the
    /// low level types the rest of the type system are built up from. If there
    /// is no built-in TypeScript equivalent, the type is prefixed with "_."
    /// and the Gleam prelude namespace will be imported during the code emission.
    ///
    fn print_prelude_type(
        &mut self,
        name: &str,
        arguments: &[Arc<Type>],
        generic_printing: GenericPrinting<'_>,
    ) -> Document<'static> {
        match name {
            "Nil" => "undefined".to_doc(),
            "Int" | "Float" => "number".to_doc(),
            "UtfCodepoint" => {
                self.tracker.prelude_used = true;
                "_.UtfCodepoint".to_doc()
            }
            "String" => "string".to_doc(),
            "Bool" => "boolean".to_doc(),
            "BitArray" => {
                self.tracker.prelude_used = true;
                "_.BitArray".to_doc()
            }
            "List" => {
                self.tracker.prelude_used = true;
                docvec![
                    "_.List",
                    wrap_generic_arguments(
                        arguments
                            .iter()
                            .map(|argument| self.do_print(argument, generic_printing))
                    )
                ]
            }
            "Result" => {
                self.tracker.prelude_used = true;
                docvec![
                    "_.Result",
                    wrap_generic_arguments(
                        arguments.iter().map(|x| self.do_print(x, generic_printing))
                    )
                ]
            }
            // Getting here should mean we either forgot a built-in type or there is a
            // compiler error
            name => panic!("{name} is not a built-in type."),
        }
    }

    /// Prints a "named" programmer-defined Gleam type into the TypeScript
    /// equivalent.
    ///
    fn print_type_app(
        &mut self,
        name: &str,
        arguments: &[Arc<Type>],
        module: &str,
        generic_printing: GenericPrinting<'_>,
    ) -> Document<'static> {
        let name = eco_format!("{}$", ts_safe_type_name(name.to_string()));
        let name = match module == self.module.name {
            true => name.to_doc(),
            false => {
                // If type comes from a separate module, use that module's name
                // as a TypeScript namespace prefix
                docvec![self.module_name(module), ".", name]
            }
        };
        if arguments.is_empty() {
            return name;
        }

        // If the App type takes arguments, pass them in as TypeScript generics
        docvec![
            name,
            wrap_generic_arguments(
                arguments
                    .iter()
                    .map(|argument| self.do_print(argument, generic_printing))
            )
        ]
    }

    /// Prints the TypeScript type for an anonymous function (aka lambda)
    ///
    fn print_fn(
        &mut self,
        arguments: &[Arc<Type>],
        return_: &Type,
        generic_printing: GenericPrinting<'_>,
    ) -> Document<'static> {
        docvec![
            wrap_arguments(arguments.iter().enumerate().map(|(idx, argument)| docvec![
                "x",
                idx,
                ": ",
                self.do_print(argument, generic_printing)
            ])),
            " => ",
            self.do_print(return_, generic_printing)
        ]
    }

    /// Allows an outside module to mark the Gleam prelude as "used"
    ///
    pub fn set_prelude_used(&mut self) {
        self.tracker.prelude_used = true
    }

    /// Returns if the Gleam prelude has been used at all during the process
    /// of printing the TypeScript types
    ///
    pub fn prelude_used(&self) -> bool {
        self.tracker.prelude_used
    }
}

#[derive(Debug, Default)]
pub(crate) struct UsageTracker {
    pub prelude_used: bool,
}

/// How to print generic type parameters when generating declarations.
#[derive(Debug, Clone, Copy)]
enum GenericPrinting<'a> {
    /// Print the generic parameters based on how many times they are used:
    /// - If a parameter isn't used, it is not printed.
    /// - If a parameter is used exactly once, it is printed as `any`.
    /// - If a parameter is used more than once, it is printed as a generic.
    ///
    /// For example, the following function:
    ///
    /// ```gleam
    /// pub fn wibble(thing: one, other_thing: other) -> one { ... }
    /// ```
    ///
    /// Would result in the following declaration:
    ///
    /// ```typescript
    /// export function wibble<A>(thing: A, other_thing: any): A;
    /// ```
    ///
    FromUsage(&'a HashMap<u64, u64>),
    /// Print every generic parameter as a generic in TypeScript. This is used in
    /// custom types where every generic needs to be emitted, even if it is only
    /// referenced once, or not at all.
    ///
    /// For example:
    ///
    /// ```gleam
    /// pub type Wibble(multiple, single, phantom) {
    ///   Wibble(a: single, b: multiple, b: multiple)
    /// }
    /// ```
    ///
    /// Generates more or less the following TypeScript:
    ///
    /// ```typescript
    /// export class Wibble<A, B, C> extends CustomType {
    ///   a: A;
    ///   b: B;
    ///   c: B;
    /// }
    /// ```
    ///
    AlwaysGeneric,
    /// Print generic parameters as TypeScript `any`. This is used in constants,
    /// where generics are not allowed.
    AsAny,
}
