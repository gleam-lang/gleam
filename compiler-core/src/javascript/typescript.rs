// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2022 The Gleam contributors

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
    javascript::JavaScriptCodegenTarget,
    type_::{Type, TypeVar},
};
use ecow::{EcoString, eco_format};
use itertools::Itertools;
use pretty_arena::*;
use std::{collections::HashMap, ops::Deref, sync::Arc};

use super::{INDENT, import::Imports, wrap_arguments};

/// When rendering a type variable to an TypeScript type spec we need all type
/// variables with the same id to end up with the same name in the generated
/// TypeScript. This function converts a usize into base 26 A-Z for this purpose.
fn id_to_type_var<'a, 'doc>(arena: &'doc DocumentArena<'a, 'doc>, id: u64) -> Document<'a, 'doc> {
    if id < 26 {
        return std::iter::once(
            std::char::from_u32((id % 26 + 65) as u32).expect("id_to_type_var 0"),
        )
        .collect::<EcoString>()
        .to_doc(arena);
    }
    let mut name = vec![];
    let mut last_char = id;
    while last_char >= 26 {
        name.push(std::char::from_u32((last_char % 26 + 65) as u32).expect("id_to_type_var 1"));
        last_char /= 26;
    }
    name.push(std::char::from_u32((last_char % 26 + 64) as u32).expect("id_to_type_var 2"));
    name.reverse();
    name.into_iter().collect::<EcoString>().to_doc(arena)
}

fn name_with_generics<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    name: Document<'a, 'doc>,
    types: impl IntoIterator<Item = &'a Arc<Type>>,
) -> Document<'a, 'doc> {
    let generic_usages = collect_generic_usages(HashMap::new(), types);
    let generic_names: Vec<Document<'_, '_>> = generic_usages
        .keys()
        .sorted()
        .map(|id| id_to_type_var(arena, *id))
        .collect();

    docvec![
        arena,
        name,
        if generic_names.is_empty() {
            EMPTY_DOCUMENT
        } else {
            wrap_generic_arguments(arena, generic_names)
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
fn tuple<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    elements: impl IntoIterator<Item = Document<'a, 'doc>>,
) -> Document<'a, 'doc> {
    EMPTY_BREAK_DOCUMENT
        .append(arena, arena.join(elements, COMMA_BREAK_DOCUMENT))
        .nest(arena, INDENT)
        .append(arena, EMPTY_BREAK_DOCUMENT)
        .surround(arena, OPEN_SQUARE_DOCUMENT, CLOSE_SQUARE_DOCUMENT)
        .group(arena)
}

fn wrap_generic_arguments<'a, 'doc, I>(
    arena: &'doc DocumentArena<'a, 'doc>,
    arguments: I,
) -> Document<'a, 'doc>
where
    I: IntoIterator<Item = Document<'a, 'doc>>,
{
    EMPTY_BREAK_DOCUMENT
        .append(arena, arena.join(arguments, COMMA_BREAK_DOCUMENT))
        .nest(arena, INDENT)
        .append(arena, EMPTY_BREAK_DOCUMENT)
        .surround(arena, LT_INT_DOCUMENT, GT_INT_DOCUMENT)
        .group(arena)
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

impl<'a, 'doc> TypeScriptGenerator<'a> {
    pub fn new(module: &'a TypedModule) -> Self {
        let current_module_name_segments_count = module.name.split('/').count();
        Self {
            module,
            aliased_module_names: HashMap::new(),
            tracker: UsageTracker::default(),
            current_module_name_segments_count,
        }
    }

    pub fn compile(&mut self, arena: &'doc DocumentArena<'a, 'doc>) -> Document<'a, 'doc> {
        let mut imports = self.collect_imports();

        let statements = self.definitions(arena, &mut imports);
        let no_statements = statements.is_empty();

        // Two lines between each statement
        let statements = arena.join(statements, TWO_LINES_DOCUMENT);

        // Put it all together

        if self.prelude_used() {
            let path = self.import_path(&self.module.type_info.package, PRELUDE_MODULE_NAME);
            imports.register_module(path, ["_".into()], []);
        }

        if imports.is_empty() && no_statements {
            docvec![arena, EXPORT_SPACE_OPEN_CLOSE_CURLY_DOCUMENT, LINE_DOCUMENT]
        } else if imports.is_empty() {
            statements.append(arena, LINE_DOCUMENT)
        } else if no_statements {
            imports.into_doc(arena, JavaScriptCodegenTarget::TypeScriptDeclarations)
        } else {
            docvec![
                arena,
                imports.into_doc(arena, JavaScriptCodegenTarget::TypeScriptDeclarations),
                LINE_DOCUMENT,
                statements,
                LINE_DOCUMENT
            ]
        }
    }

    fn collect_imports(&mut self) -> Imports<'a, 'doc> {
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
    fn collect_imports_for_type<'b>(&mut self, type_: &'b Type, imports: &mut Imports<'a, 'doc>) {
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
        imports: &mut Imports<'a, 'doc>,
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

    fn definitions(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        imports: &mut Imports<'a, 'doc>,
    ) -> Vec<Document<'a, 'doc>> {
        let mut documents = vec![];

        for custom_type in &self.module.definitions.custom_types {
            if let Some(mut new_documents) =
                self.custom_type_definition(arena, custom_type, imports)
            {
                documents.append(&mut new_documents);
            }
        }

        for type_alias in &self.module.definitions.type_aliases {
            if let Some(document) = self.type_alias(arena, type_alias) {
                documents.push(document);
            }
        }

        for constant in &self.module.definitions.constants {
            if let Some(document) = self.module_constant(arena, constant) {
                documents.push(document);
            }
        }

        for function in &self.module.definitions.functions {
            if let Some(document) = self.module_function(arena, function) {
                documents.push(document);
            }
        }

        documents
    }

    fn type_alias(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        type_alias: &TypedTypeAlias,
    ) -> Option<Document<'a, 'doc>> {
        if !type_alias.publicity.is_importable() {
            return None;
        }

        Some(docvec![
            arena,
            EXPORT_TYPE_SPACE_DOCUMENT,
            ts_safe_type_name(type_alias.alias.to_string()),
            SPACE_EQUAL_SPACE_DOCUMENT,
            self.print_type(arena, &type_alias.type_),
            SEMICOLON_DOCUMENT
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
        arena: &'doc DocumentArena<'a, 'doc>,
        custom_type: &'a TypedCustomType,
        imports: &mut Imports<'a, 'doc>,
    ) -> Option<Vec<Document<'a, 'doc>>> {
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

        let type_name = name_with_generics(
            arena,
            eco_format!("{name}$").to_doc(arena),
            typed_parameters,
        );

        let mut definitions = constructors
            .iter()
            .map(|constructor| {
                self.variant_definition(
                    arena,
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
                    name: external_name.clone(),
                    alias: Some(eco_format!("{name}$").to_doc(arena)),
                };
                imports.register_export(eco_format!("{name}$"));

                imports.register_module(module.clone(), [], [member]);
                return Some(Vec::new());
            } else {
                ANY_DOCUMENT
            }
        } else {
            let constructors = constructors.iter().map(|x| {
                name_with_generics(
                    arena,
                    super::maybe_escape_identifier(&x.name).to_doc(arena),
                    x.arguments.iter().map(|a| &a.type_),
                )
            });
            arena.join(constructors, arena.break_("| ", " | "))
        };

        let head = if publicity.is_private() {
            TYPE_SPACE_DOCUMENT
        } else {
            EXPORT_TYPE_SPACE_DOCUMENT
        };

        definitions.push(docvec![
            arena,
            head,
            type_name,
            SPACE_EQUAL_SPACE_DOCUMENT,
            definition,
            SEMICOLON_DOCUMENT,
        ]);

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
                arena,
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
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a TypedRecordConstructor,
        publicity: Publicity,
        type_name: &'a str,
        type_name_with_generics: &Document<'a, 'doc>,
        type_parameters: &'a [Arc<Type>],
    ) -> Document<'a, 'doc> {
        self.set_prelude_used();
        let class_definition = self.variant_class_definition(arena, constructor, publicity);

        // If the custom type is private or opaque, we don't need to generate API
        // functions for it.
        if publicity.is_private() {
            return class_definition;
        }

        let constructor_definition = self.variant_constructor_definition(
            arena,
            constructor,
            type_name,
            type_name_with_generics,
            type_parameters,
        );
        let variant_check_definition =
            self.variant_check_definition(arena, constructor, type_name, type_parameters);
        let fields_definition = self.variant_fields_definition(
            arena,
            constructor,
            type_name,
            type_name_with_generics,
            type_parameters,
        );

        docvec![
            arena,
            class_definition,
            LINE_DOCUMENT,
            constructor_definition,
            LINE_DOCUMENT,
            variant_check_definition,
            fields_definition,
        ]
    }

    fn variant_class_definition(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a TypedRecordConstructor,
        publicity: Publicity,
    ) -> Document<'a, 'doc> {
        let head = docvec![
            arena,
            if publicity.is_public() {
                EXPORT_SPACE_DOCUMENT
            } else {
                DECLARE_SPACE_DOCUMENT
            },
            CLASS_SPACE_DOCUMENT,
            name_with_generics(
                arena,
                super::maybe_escape_identifier(&constructor.name).to_doc(arena),
                constructor.arguments.iter().map(|a| &a.type_)
            ),
            SPACE_EXTENDS_UNDERSCORE_CUSTOM_TYPE_DOCUMENT
        ];

        if constructor.arguments.is_empty() {
            return head.append(arena, CLOSE_CURLY_DOCUMENT);
        };

        let class_body = docvec![
            arena,
            LINE_DOCUMENT,
            DEPRECATED_MULTILINE_COMMENT_DOCUMENT,
            LINE_DOCUMENT,
            // First add the constructor
            CONSTRUCTOR_DOCUMENT,
            wrap_arguments(
                arena,
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
                            .to_doc(arena);
                        docvec![
                            arena,
                            name,
                            COLON_SPACE_DOCUMENT,
                            self.do_print_force_generic_param(arena, &argument.type_)
                        ]
                    })
            ),
            SEMICOLON_DOCUMENT,
            LINE_DOCUMENT,
            // Then add each field to the class
            arena.join(
                constructor.arguments.iter().enumerate().map(|(i, arg)| {
                    let name = arg
                        .label
                        .as_ref()
                        .map(|(_, s)| super::maybe_escape_property(s))
                        .unwrap_or_else(|| eco_format!("{i}"))
                        .to_doc(arena);
                    docvec![
                        arena,
                        DEPRECATED_MULTILINE_COMMENT_DOCUMENT,
                        LINE_DOCUMENT,
                        name,
                        COLON_SPACE_DOCUMENT,
                        self.do_print_force_generic_param(arena, &arg.type_),
                        SEMICOLON_DOCUMENT
                    ]
                }),
                LINE_DOCUMENT,
            ),
        ]
        .nest(arena, INDENT);

        docvec![arena, head, class_body, LINE_DOCUMENT, "}"]
    }

    fn variant_constructor_definition(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
        type_name_with_generics: &Document<'a, 'doc>,
        type_parameters: &'a [Arc<Type>],
    ) -> Document<'a, 'doc> {
        let mut arguments = Vec::new();

        for (index, parameter) in constructor.arguments.iter().enumerate() {
            let name = if let Some((_, label)) = &parameter.label {
                super::maybe_escape_identifier(label)
            } else {
                eco_format!("${index}")
            };

            arguments.push(docvec![
                arena,
                name,
                COLON_SPACE_DOCUMENT,
                self.do_print_force_generic_param(arena, &parameter.type_)
            ])
        }

        let function_name = eco_format!(
            "{type_name}${variant_name}",
            variant_name = constructor.name
        )
        .to_doc(arena);

        let has_arguments = !arguments.is_empty();

        docvec![
            arena,
            EXPORT_FUNCTION_SPACE_DOCUMENT,
            name_with_generics(arena, function_name, type_parameters),
            OPEN_PAREN_DOCUMENT,
            docvec![
                arena,
                EMPTY_BREAK_DOCUMENT,
                arena.join(arguments, COMMA_BREAK_DOCUMENT),
            ]
            .nest(arena, INDENT),
            if has_arguments {
                TRAILING_COMMA_BREAK_DOCUMENT
            } else {
                EMPTY_BREAK_DOCUMENT
            },
            CLOSE_PAREN_COLON_SPACE_DOCUMENT,
            *type_name_with_generics,
            SEMICOLON_DOCUMENT
        ]
        .group(arena)
    }

    fn variant_check_definition(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
        type_parameters: &'a [Arc<Type>],
    ) -> Document<'a, 'doc> {
        let function_name = eco_format!(
            "{type_name}$is{variant_name}",
            variant_name = constructor.name
        )
        .to_doc(arena);
        let mut document = docvec![
            arena,
            EXPORT_FUNCTION_SPACE_DOCUMENT,
            name_with_generics(arena, function_name, type_parameters),
            OPEN_PAREN_DOCUMENT,
            docvec![arena, EMPTY_BREAK_DOCUMENT, VALUE_COLON_SPACE_ANY_DOCUMENT]
                .nest(arena, INDENT),
            TRAILING_COMMA_BREAK_DOCUMENT,
            CLOSE_PAREN_COLON_SPACE_VALUE_IS_SPACE_DOCUMENT,
            type_name,
            DOLLAR_DOCUMENT,
        ];
        if !type_parameters.is_empty() {
            for i in 0..type_parameters.len() {
                if i == 0 {
                    document = document.append(arena, LT_INT_UNKNOWN_DOCUMENT);
                } else {
                    document = document.append(arena, COMMA_SPACE_UNKNOWN_DOCUMENT);
                }
            }
            document = document.append(arena, GT_INT_DOCUMENT);
        };
        document = document.append(arena, SEMICOLON_DOCUMENT);
        document.group(arena)
    }

    fn variant_fields_definition(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
        type_name_with_generics: &Document<'a, 'doc>,
        type_parameters: &'a [Arc<Type>],
    ) -> Document<'a, 'doc> {
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
            .to_doc(arena);

            functions.push(
                docvec![
                    arena,
                    LINE_DOCUMENT,
                    EXPORT_FUNCTION_SPACE_DOCUMENT,
                    name_with_generics(arena, function_name, type_parameters),
                    OPEN_PAREN_DOCUMENT,
                    docvec![
                        arena,
                        EMPTY_BREAK_DOCUMENT,
                        VALUE_COLON_SPACE_DOCUMENT,
                        *type_name_with_generics,
                    ]
                    .nest(arena, INDENT),
                    TRAILING_COMMA_BREAK_DOCUMENT,
                    CLOSE_PAREN_COLON_SPACE_DOCUMENT,
                    self.do_print_force_generic_param(arena, &argument.type_),
                    SEMICOLON_DOCUMENT,
                ]
                .group(arena),
            );

            // If the argument is labelled, also generate a getter for the labelled
            // argument.
            if let Some((_, label)) = &argument.label {
                let function_name = eco_format!(
                    "{type_name}${variant_name}${label}",
                    variant_name = constructor.name
                )
                .to_doc(arena);

                functions.push(
                    docvec![
                        arena,
                        LINE_DOCUMENT,
                        EXPORT_FUNCTION_SPACE_DOCUMENT,
                        name_with_generics(arena, function_name, type_parameters),
                        OPEN_PAREN_DOCUMENT,
                        docvec![
                            arena,
                            EMPTY_BREAK_DOCUMENT,
                            VALUE_COLON_SPACE_DOCUMENT,
                            *type_name_with_generics,
                        ]
                        .nest(arena, INDENT),
                        TRAILING_COMMA_BREAK_DOCUMENT,
                        CLOSE_PAREN_COLON_SPACE_DOCUMENT,
                        self.do_print_force_generic_param(arena, &argument.type_),
                        SEMICOLON_DOCUMENT,
                    ]
                    .group(arena),
                );
            }
        }

        arena.concat(functions)
    }

    fn shared_custom_type_fields(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        type_name: &'a str,
        type_name_with_generics: &Document<'a, 'doc>,
        type_parameters: &'a [Arc<Type>],
        shared_accessors: &HashMap<EcoString, RecordAccessor>,
    ) -> Document<'a, 'doc> {
        let accessors = shared_accessors
            .iter()
            .sorted_by_key(|(name, _)| *name)
            .map(|(field, accessor)| {
                let function_name = eco_format!("{type_name}${field}").to_doc(arena);

                docvec![
                    arena,
                    EXPORT_FUNCTION_SPACE_DOCUMENT,
                    name_with_generics(arena, function_name, type_parameters),
                    OPEN_PAREN_DOCUMENT,
                    docvec![
                        arena,
                        EMPTY_BREAK_DOCUMENT,
                        VALUE_COLON_SPACE_DOCUMENT,
                        *type_name_with_generics,
                    ]
                    .nest(arena, INDENT),
                    TRAILING_COMMA_BREAK_DOCUMENT,
                    CLOSE_PAREN_COLON_SPACE_DOCUMENT,
                    self.do_print_force_generic_param(arena, &accessor.type_),
                    SEMICOLON_DOCUMENT
                ]
                .group(arena)
            });
        arena.join(accessors, LINE_DOCUMENT)
    }

    fn module_constant(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constant: &TypedModuleConstant,
    ) -> Option<Document<'a, 'doc>> {
        if !constant.publicity.is_importable() {
            return None;
        }

        Some(docvec![
            arena,
            EXPORT_CONST_SPACE_DOCUMENT,
            super::maybe_escape_identifier(&constant.name),
            COLON_SPACE_DOCUMENT,
            self.print_type(arena, &constant.value.type_()),
            SEMICOLON_DOCUMENT,
        ])
    }

    fn module_function(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        function: &'a TypedFunction,
    ) -> Option<Document<'a, 'doc>> {
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
        let generic_names: Vec<Document<'_, '_>> = generic_usages
            .iter()
            .filter(|(_id, use_count)| **use_count > 1)
            .sorted_by_key(|x| x.0)
            .map(|(id, _use_count)| id_to_type_var(arena, *id))
            .collect();

        Some(docvec![
            arena,
            EXPORT_FUNCTION_SPACE_DOCUMENT,
            super::maybe_escape_identifier(name),
            if generic_names.is_empty() {
                EMPTY_DOCUMENT
            } else {
                wrap_generic_arguments(arena, generic_names)
            },
            wrap_arguments(
                arena,
                arguments.iter().enumerate().map(|(i, argument)| {
                    match argument.get_variable_name() {
                        None => {
                            docvec![
                                arena,
                                X_DOCUMENT,
                                i,
                                COLON_SPACE_DOCUMENT,
                                self.print_type_with_generic_usages(
                                    arena,
                                    &argument.type_,
                                    &generic_usages
                                )
                            ]
                        }
                        Some(name) => docvec![
                            arena,
                            super::maybe_escape_identifier(name),
                            COLON_SPACE_DOCUMENT,
                            self.print_type_with_generic_usages(
                                arena,
                                &argument.type_,
                                &generic_usages
                            )
                        ],
                    }
                }),
            ),
            COLON_SPACE_DOCUMENT,
            self.print_type_with_generic_usages(arena, return_type, &generic_usages),
            SEMICOLON_DOCUMENT,
        ])
    }

    /// Converts a Gleam type into a TypeScript type string
    ///
    pub fn print_type(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        type_: &Type,
    ) -> Document<'a, 'doc> {
        self.do_print(arena, type_, GenericPrinting::AsAny)
    }

    /// Helper function for generating a TypeScript type string after collecting
    /// all of the generics used in a statement
    ///
    pub fn print_type_with_generic_usages(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        type_: &Type,
        generic_usages: &HashMap<u64, u64>,
    ) -> Document<'a, 'doc> {
        self.do_print(arena, type_, GenericPrinting::FromUsage(generic_usages))
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
        arena: &'doc DocumentArena<'a, 'doc>,
        type_: &Type,
        generic_printing: GenericPrinting<'_>,
    ) -> Document<'a, 'doc> {
        match type_ {
            Type::Var { type_ } => self.print_var(arena, &type_.borrow(), generic_printing),

            Type::Named {
                name,
                module,
                arguments,
                ..
            } if is_prelude_module(module) => {
                self.print_prelude_type(arena, name, arguments, generic_printing)
            }

            Type::Named {
                name,
                arguments,
                module,
                ..
            } => self.print_type_app(arena, name, arguments, module, generic_printing),

            Type::Fn { arguments, return_ } => {
                self.print_fn(arena, arguments, return_, generic_printing)
            }

            Type::Tuple { elements } => tuple(
                arena,
                elements
                    .iter()
                    .map(|element| self.do_print(arena, element, generic_printing)),
            ),
        }
    }

    fn do_print_force_generic_param(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        type_: &Type,
    ) -> Document<'a, 'doc> {
        match type_ {
            Type::Var { type_ } => {
                self.print_var(arena, &type_.borrow(), GenericPrinting::AlwaysGeneric)
            }

            Type::Named {
                name,
                module,
                arguments,
                ..
            } if is_prelude_module(module) => {
                self.print_prelude_type(arena, name, arguments, GenericPrinting::AlwaysGeneric)
            }

            Type::Named {
                name,
                arguments,
                module,
                ..
            } => self.print_type_app(
                arena,
                name,
                arguments,
                module,
                GenericPrinting::AlwaysGeneric,
            ),

            Type::Fn { arguments, return_ } => {
                self.print_fn(arena, arguments, return_, GenericPrinting::AlwaysGeneric)
            }

            Type::Tuple { elements } => tuple(
                arena,
                elements
                    .iter()
                    .map(|element| self.do_print(arena, element, GenericPrinting::AlwaysGeneric)),
            ),
        }
    }

    fn print_var(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        type_: &TypeVar,
        generic_printing: GenericPrinting<'_>,
    ) -> Document<'a, 'doc> {
        match type_ {
            TypeVar::Unbound { id } | TypeVar::Generic { id } => match generic_printing {
                GenericPrinting::FromUsage(usages) => match usages.get(id) {
                    Some(&0) => EMPTY_DOCUMENT,
                    Some(&1) => ANY_DOCUMENT,
                    _ => id_to_type_var(arena, *id),
                },
                GenericPrinting::AlwaysGeneric => id_to_type_var(arena, *id),
                GenericPrinting::AsAny => ANY_DOCUMENT,
            },
            TypeVar::Link { type_ } => self.do_print(arena, type_, generic_printing),
        }
    }

    /// Prints a type coming from the Gleam prelude module. These are often the
    /// low level types the rest of the type system are built up from. If there
    /// is no built-in TypeScript equivalent, the type is prefixed with "_."
    /// and the Gleam prelude namespace will be imported during the code emission.
    ///
    fn print_prelude_type(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &str,
        arguments: &[Arc<Type>],
        generic_printing: GenericPrinting<'_>,
    ) -> Document<'a, 'doc> {
        match name {
            "Nil" => UNDEFINED_DOCUMENT,
            "Int" | "Float" => NUMBER_DOCUMENT,
            "UtfCodepoint" => {
                self.tracker.prelude_used = true;
                UNDERSCORE_DOT_UTF_CODEPOINT_DOCUMENT
            }
            "String" => STRING_DOCUMENT,
            "Bool" => BOOLEAN_DOCUMENT,
            "BitArray" => {
                self.tracker.prelude_used = true;
                UNDERSCORE_DOT_BIT_ARRAY_DOCUMENT
            }
            "List" => {
                self.tracker.prelude_used = true;
                docvec![
                    arena,
                    UNDERSCORE_DOT_LIST_DOCUMENT,
                    wrap_generic_arguments(
                        arena,
                        arguments.iter().map(|argument| self.do_print(
                            arena,
                            argument,
                            generic_printing
                        ))
                    )
                ]
            }
            "Result" => {
                self.tracker.prelude_used = true;
                docvec![
                    arena,
                    UNDERSCORE_DOT_RESULT_DOCUMENT,
                    wrap_generic_arguments(
                        arena,
                        arguments
                            .iter()
                            .map(|x| self.do_print(arena, x, generic_printing))
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
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &str,
        arguments: &[Arc<Type>],
        module: &str,
        generic_printing: GenericPrinting<'_>,
    ) -> Document<'a, 'doc> {
        let name = eco_format!("{}$", ts_safe_type_name(name.to_string()));
        let name = match module == self.module.name {
            true => name.to_doc(arena),
            false => {
                // If type comes from a separate module, use that module's name
                // as a TypeScript namespace prefix
                docvec![arena, self.module_name(module), DOT_DOCUMENT, name]
            }
        };
        if arguments.is_empty() {
            return name;
        }

        // If the App type takes arguments, pass them in as TypeScript generics
        docvec![
            arena,
            name,
            wrap_generic_arguments(
                arena,
                arguments
                    .iter()
                    .map(|argument| self.do_print(arena, argument, generic_printing))
            )
        ]
    }

    /// Prints the TypeScript type for an anonymous function (aka lambda)
    ///
    fn print_fn(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        arguments: &[Arc<Type>],
        return_: &Type,
        generic_printing: GenericPrinting<'_>,
    ) -> Document<'a, 'doc> {
        docvec![
            arena,
            wrap_arguments(
                arena,
                arguments.iter().enumerate().map(|(idx, argument)| docvec![
                    arena,
                    X_DOCUMENT,
                    idx,
                    COLON_SPACE_DOCUMENT,
                    self.do_print(arena, argument, generic_printing)
                ])
            ),
            SPACE_EQUAL_RIGHT_ARROW_SPACE_DOCUMENT,
            self.do_print(arena, return_, generic_printing)
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
