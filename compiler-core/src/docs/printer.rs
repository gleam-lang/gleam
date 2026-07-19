// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use ecow::{EcoString, eco_format};

use crate::{
    ast::{
        ArgNames, CustomType, Function, Publicity, RecordConstructorArg, TypeAlias, TypedArg,
        TypedDefinitions, TypedModuleConstant, TypedRecordConstructor,
    },
    type_::{
        Deprecation, PRELUDE_MODULE_NAME, PRELUDE_PACKAGE_NAME, Type, TypeVar,
        printer::{Names, PrintMode},
    },
};
use pretty_arena::*;
use src_span::SrcSpan;

use super::{
    Dependency, DependencyKind, DocsValues, TypeConstructor, TypeConstructorArg, TypeDefinition,
    markdown_documentation, source_links::SourceLinker, text_documentation,
};

#[derive(Clone, Copy)]
pub struct PrintOptions {
    pub print_highlighting: bool,
    pub print_html: bool,
}

impl PrintOptions {
    pub fn all() -> Self {
        Self {
            print_highlighting: true,
            print_html: true,
        }
    }
}

pub struct Printer<'a> {
    options: PrintOptions,
    names: &'a Names,

    package: EcoString,
    module: EcoString,

    /// Type variables which don't have annotated names and we have generated
    /// names for.
    printed_type_variables: HashMap<u64, EcoString>,
    /// Names of type variables that we have generated or printed in a given
    /// definition. This ensures that we have no duplicate generated names.
    printed_type_variable_names: HashSet<EcoString>,
    /// An incrementing number used to generate the next type variable name.
    /// `0` becomes `a`, `1` becomes `b`, etc.
    next_type_variable_id: u64,

    dependencies: &'a HashMap<EcoString, Dependency>,
}

impl<'a, 'doc> Printer<'a> {
    pub fn new(
        package: EcoString,
        module: EcoString,
        names: &'a Names,
        dependencies: &'a HashMap<EcoString, Dependency>,
    ) -> Printer<'a> {
        Printer {
            options: PrintOptions::all(),
            names,
            package,
            module,
            printed_type_variables: HashMap::new(),
            printed_type_variable_names: HashSet::new(),
            next_type_variable_id: 0,
            dependencies,
        }
    }

    // This is currently only used in the tests, though it might be useful in
    // application code in future. If it is needed, simply remove this attribute.
    #[cfg(test)]
    pub fn set_options(&mut self, options: PrintOptions) {
        self.options = options;
    }

    pub fn type_definitions(
        &mut self,
        source_links: &SourceLinker,
        definitions: &'a TypedDefinitions,
    ) -> Vec<TypeDefinition<'a>> {
        let mut type_definitions = vec![];
        let arena = DocumentArena::new();

        for CustomType {
            location,
            name,
            publicity,
            constructors,
            documentation,
            deprecation,
            opaque,
            parameters,
            ..
        } in &definitions.custom_types
        {
            if !publicity.is_public() {
                continue;
            }

            type_definitions.push(TypeDefinition {
                name,
                definition: print(self.custom_type(
                    &arena,
                    name,
                    parameters,
                    constructors,
                    *opaque,
                )),
                raw_definition: self
                    .raw(|this| this.custom_type(&arena, name, parameters, constructors, *opaque)),
                documentation: markdown_documentation(documentation),
                text_documentation: text_documentation(documentation),
                deprecation_message: match deprecation {
                    Deprecation::NotDeprecated => "".to_string(),
                    Deprecation::Deprecated { message } => message.to_string(),
                },
                constructors: if *opaque {
                    Vec::new()
                } else {
                    constructors
                        .iter()
                        .map(|constructor| TypeConstructor {
                            definition: print(self.record_constructor(&arena, constructor)),
                            raw_definition: self
                                .raw(|this| this.record_constructor(&arena, constructor)),
                            documentation: markdown_documentation(&constructor.documentation),
                            text_documentation: text_documentation(&constructor.documentation),
                            arguments: constructor
                                .arguments
                                .iter()
                                .filter_map(|arg| arg.label.as_ref().map(|(_, label)| (arg, label)))
                                .map(|(argument, label)| TypeConstructorArg {
                                    name: label.trim_end().to_string(),
                                    doc: markdown_documentation(&argument.doc),
                                    text_documentation: text_documentation(&argument.doc),
                                })
                                .filter(|arg| !arg.doc.is_empty())
                                .collect(),
                        })
                        .collect()
                },
                source_url: source_links.url(*location),
                opaque: *opaque,
            });
        }

        for TypeAlias {
            location,
            alias: name,
            parameters,
            type_,
            publicity,
            documentation,
            deprecation,
            ..
        } in &definitions.type_aliases
        {
            if !publicity.is_public() {
                continue;
            }
            type_definitions.push(TypeDefinition {
                name,
                definition: print(
                    self.type_alias(&arena, name, type_, parameters)
                        .group(&arena),
                ),
                raw_definition: self.raw(|this| {
                    this.type_alias(&arena, name, type_, parameters)
                        .group(&arena)
                }),
                documentation: markdown_documentation(documentation),
                text_documentation: text_documentation(documentation),
                constructors: vec![],
                source_url: source_links.url(*location),
                deprecation_message: match deprecation {
                    Deprecation::NotDeprecated => "".to_string(),
                    Deprecation::Deprecated { message } => message.to_string(),
                },
                opaque: false,
            });
        }

        type_definitions.sort();
        type_definitions
    }

    /// Print a definition without HTML highlighting, such as for search data
    fn raw<'doc1, 'a1: 'doc1>(
        &mut self,
        definition: impl FnOnce(&mut Self) -> Document<'a1, 'doc1>,
    ) -> String {
        let options = self.options;
        // Turn off highlighting for this definition
        self.options = PrintOptions {
            print_highlighting: false,
            print_html: false,
        };
        let result = print(definition(self));
        // Restore previous options
        self.options = options;
        format!("```\n{result}\n```")
    }

    pub fn value_definitions(
        &mut self,
        source_links: &SourceLinker,
        definitions: &'a TypedDefinitions,
    ) -> Vec<DocsValues<'a>> {
        let mut value_definitions = vec![];
        let arena = DocumentArena::new();

        for Function {
            location,
            name,
            arguments,
            publicity,
            deprecation,
            return_type,
            documentation,
            ..
        } in &definitions.functions
        {
            let Some((_, name)) = name else { continue };
            if !publicity.is_public() {
                continue;
            }

            // Ensure that any type variables we printed in previous definitions don't
            // affect our printing of this definition. Two type variables in different
            // definitions can have the same name without clashing.
            self.printed_type_variable_names.clear();
            self.next_type_variable_id = 0;

            value_definitions.push(DocsValues {
                name,
                definition: print(self.function_signature(&arena, name, arguments, return_type)),
                raw_definition: self
                    .raw(|this| this.function_signature(&arena, name, arguments, return_type)),
                documentation: markdown_documentation(documentation),
                text_documentation: text_documentation(documentation),
                source_url: source_links.url(*location),
                deprecation_message: match deprecation {
                    Deprecation::NotDeprecated => "".to_string(),
                    Deprecation::Deprecated { message } => message.to_string(),
                },
            });
        }

        for TypedModuleConstant {
            documentation,
            location,
            publicity,
            name,
            type_,
            deprecation,
            ..
        } in &definitions.constants
        {
            if !publicity.is_public() {
                continue;
            }

            value_definitions.push(DocsValues {
                name,
                definition: print(self.constant(&arena, name, type_)),
                raw_definition: self.raw(|this| this.constant(&arena, name, type_)),
                documentation: markdown_documentation(documentation),
                text_documentation: text_documentation(documentation),
                source_url: source_links.url(*location),
                deprecation_message: match deprecation {
                    Deprecation::NotDeprecated => "".to_string(),
                    Deprecation::Deprecated { message } => message.to_string(),
                },
            });
        }

        value_definitions.sort();
        value_definitions
    }

    fn custom_type(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &'a str,
        parameters: &'a [(SrcSpan, EcoString)],
        constructors: &'a [TypedRecordConstructor],
        opaque: bool,
    ) -> Document<'a, 'doc> {
        let arguments = if parameters.is_empty() {
            EMPTY_DOCUMENT
        } else {
            Self::wrap_arguments(
                arena,
                parameters
                    .iter()
                    .map(|(_, parameter)| self.variable(arena, parameter)),
            )
        };

        let type_head = docvec![
            arena,
            self.keyword(
                arena,
                if opaque {
                    PUB_OPAQUE_TYPE_SPACE_DOCUMENT
                } else {
                    PUB_TYPE_SPACE_DOCUMENT
                }
            ),
            self.title(arena, name),
            arguments
        ];

        if constructors.is_empty() || opaque {
            return type_head;
        }

        let constructors = arena.concat(constructors.iter().map(|constructor| {
            LINE_DOCUMENT
                .append(arena, self.record_constructor(arena, constructor))
                .nest(arena, INDENT)
        }));

        docvec![
            arena,
            type_head,
            SPACE_OPEN_CURLY_DOCUMENT,
            constructors,
            LINE_DOCUMENT,
            CLOSE_CURLY_DOCUMENT
        ]
    }

    pub fn record_constructor(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a TypedRecordConstructor,
    ) -> Document<'a, 'doc> {
        if constructor.arguments.is_empty() {
            return self.title(arena, &constructor.name);
        }

        let arguments = constructor.arguments.iter().map(
            |RecordConstructorArg { label, type_, .. }| match label {
                Some((_, label)) => self
                    .variable(arena, label)
                    .append(arena, COLON_SPACE_DOCUMENT)
                    .append(arena, self.type_(arena, type_, PrintMode::Normal)),
                None => self.type_(arena, type_, PrintMode::Normal),
            },
        );

        let arguments = Self::wrap_arguments(arena, arguments);

        docvec![arena, self.title(arena, &constructor.name), arguments].group(arena)
    }

    fn type_alias(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &'a str,
        type_: &Type,
        parameters: &[(SrcSpan, EcoString)],
    ) -> Document<'a, 'doc> {
        let parameters = if parameters.is_empty() {
            EMPTY_DOCUMENT
        } else {
            let arguments = parameters
                .iter()
                .map(|(_, parameter)| self.variable(arena, parameter));
            Self::wrap_arguments(arena, arguments)
        };

        docvec![
            arena,
            self.keyword(arena, PUB_TYPE_SPACE_DOCUMENT),
            self.title(arena, name),
            parameters,
            SPACE_EQUAL_DOCUMENT,
            LINE_DOCUMENT
                .append(arena, self.type_(arena, type_, PrintMode::ExpandAliases))
                .nest(arena, INDENT)
        ]
    }

    fn constant(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &'a str,
        type_: &Type,
    ) -> Document<'a, 'doc> {
        self.register_local_type_variable_names(type_);

        docvec![
            arena,
            self.keyword(arena, PUB_CONST_SPACE_DOCUMENT),
            self.title(arena, name),
            COLON_SPACE_DOCUMENT,
            self.type_(arena, type_, PrintMode::Normal)
        ]
    }

    fn function_signature(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &'a str,
        arguments: &'a [TypedArg],
        return_type: &Type,
    ) -> Document<'a, 'doc> {
        for argument in arguments {
            self.register_local_type_variable_names(&argument.type_);
        }
        self.register_local_type_variable_names(return_type);

        let arguments = if arguments.is_empty() {
            OPEN_CLOSE_PAREN_DOCUMENT
        } else {
            Self::wrap_arguments(
                arena,
                arguments.iter().map(|argument| {
                    let name = self.variable(arena, self.argument_name(arena, argument));
                    docvec![
                        arena,
                        name,
                        COLON_SPACE_DOCUMENT,
                        self.type_(arena, &argument.type_, PrintMode::Normal)
                    ]
                    .group(arena)
                }),
            )
        };

        docvec![
            arena,
            self.keyword(arena, PUB_FN_SPACE_DOCUMENT),
            self.title(arena, name),
            arguments,
            SPACE_RIGHT_ARROW_SPACE_DOCUMENT,
            self.type_(arena, return_type, PrintMode::Normal)
        ]
        .group(arena)
    }

    fn argument_name(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        arg: &'a TypedArg,
    ) -> Document<'a, 'doc> {
        match &arg.names {
            ArgNames::Named { name, .. } => name.to_doc(arena),
            ArgNames::NamedLabelled { label, name, .. } => {
                docvec![arena, label, SPACE_DOCUMENT, name]
            }
            // We remove the underscore from discarded function arguments since we don't want to
            // expose this kind of detail: https://github.com/gleam-lang/gleam/issues/2561
            ArgNames::Discard { name, .. } => match name.strip_prefix('_').unwrap_or(name) {
                "" => LOWERCASE_ARG_DOCUMENT,
                name => name.to_doc(arena),
            },
            ArgNames::LabelledDiscard { label, name, .. } => {
                docvec![
                    arena,
                    label,
                    SPACE_DOCUMENT,
                    name.strip_prefix('_').unwrap_or(name).to_doc(arena)
                ]
            }
        }
    }

    fn wrap_arguments(
        arena: &'doc DocumentArena<'a, 'doc>,
        arguments: impl IntoIterator<Item = Document<'a, 'doc>>,
    ) -> Document<'a, 'doc> {
        OPEN_PAREN_BREAK_DOCUMENT
            .append(arena, arena.join(arguments, COMMA_BREAK_DOCUMENT))
            .nest_if_broken(arena, INDENT)
            .append(arena, TRAILING_COMMA_BREAK_DOCUMENT)
            .append(arena, CLOSE_PAREN_DOCUMENT)
    }

    fn type_arguments(
        arena: &'doc DocumentArena<'a, 'doc>,
        arguments: impl IntoIterator<Item = Document<'a, 'doc>>,
    ) -> Document<'a, 'doc> {
        EMPTY_BREAK_DOCUMENT
            .append(arena, arena.join(arguments, COMMA_BREAK_DOCUMENT))
            .nest_if_broken(arena, INDENT)
            .append(arena, TRAILING_COMMA_BREAK_DOCUMENT)
            .group(arena)
            .surround(arena, OPEN_PAREN_DOCUMENT, CLOSE_PAREN_DOCUMENT)
    }

    fn type_(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        type_: &Type,
        print_mode: PrintMode,
    ) -> Document<'a, 'doc> {
        match type_ {
            Type::Named {
                package,
                module,
                name,
                arguments,
                publicity,
                ..
            } => {
                let name = match print_mode {
                    // If we are printing a type for a type alias, and the alias
                    // is reexporting an internal type, we want to show that it
                    // is aliasing that internal type, rather than showing it as
                    // aliasing itself.
                    PrintMode::ExpandAliases if *package == self.package => {
                        self.named_type_name(arena, publicity, package, module, name)
                    }
                    // If we are printing a type alias which aliases an internal
                    // type from a different package, we still want to print the
                    // public name for that type. If we are not printing a type
                    // alias at all, we also want to use the public name.
                    PrintMode::ExpandAliases | PrintMode::Normal => {
                        // If we are using a reexported internal type, we want to
                        // print it public name, whether it is from this package
                        // or otherwise.
                        if let Some((module, alias)) =
                            self.names.reexport_alias(module.clone(), name.clone())
                        {
                            self.named_type_name(arena, &Publicity::Public, package, module, alias)
                        } else {
                            self.named_type_name(arena, publicity, package, module, name)
                        }
                    }
                };

                if arguments.is_empty() {
                    name
                } else {
                    name.append(
                        arena,
                        Self::type_arguments(
                            arena,
                            arguments
                                .iter()
                                .map(|argument| self.type_(arena, argument, PrintMode::Normal)),
                        ),
                    )
                }
            }
            Type::Fn { arguments, return_ } => docvec![
                arena,
                self.keyword(arena, FN_DOCUMENT),
                Self::type_arguments(
                    arena,
                    arguments
                        .iter()
                        .map(|argument| self.type_(arena, argument, PrintMode::Normal))
                ),
                SPACE_RIGHT_ARROW_SPACE_DOCUMENT,
                self.type_(arena, return_, PrintMode::Normal)
            ],
            Type::Tuple { elements } => docvec![
                arena,
                HASHTAG_DOCUMENT,
                Self::type_arguments(
                    arena,
                    elements
                        .iter()
                        .map(|element| self.type_(arena, element, PrintMode::Normal))
                ),
            ],
            Type::Var { type_ } => match type_.as_ref().borrow().deref() {
                TypeVar::Link { type_ } => self.type_(arena, type_, PrintMode::Normal),

                TypeVar::Unbound { id } | TypeVar::Generic { id } => {
                    let name = self.type_variable(*id);
                    self.variable(arena, name)
                }
            },
        }
    }

    fn type_variable(&mut self, id: u64) -> EcoString {
        if let Some(name) = self.names.get_type_variable(id) {
            return name.clone();
        }

        if let Some(name) = self.printed_type_variables.get(&id) {
            return name.clone();
        }

        loop {
            let name = self.next_letter();
            if !self.printed_type_variable_names.contains(&name) {
                _ = self.printed_type_variable_names.insert(name.clone());
                _ = self.printed_type_variables.insert(id, name.clone());
                return name;
            }
        }
    }

    // Copied from the `next_letter` method of the `type_::printer`.
    fn next_letter(&mut self) -> EcoString {
        let alphabet_length = 26;
        let char_offset = b'a';
        let mut chars = vec![];
        let mut n;
        let mut rest = self.next_type_variable_id;

        loop {
            n = rest % alphabet_length;
            rest = rest / alphabet_length;
            chars.push((n as u8 + char_offset) as char);

            if rest == 0 {
                break;
            }
            rest -= 1;
        }

        self.next_type_variable_id += 1;
        chars.into_iter().rev().collect()
    }

    fn named_type_name(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        publicity: &Publicity,
        package: &str,
        module: &str,
        name: &EcoString,
    ) -> Document<'a, 'doc> {
        // There's no documentation page for the prelude
        if package == PRELUDE_PACKAGE_NAME && module == PRELUDE_MODULE_NAME {
            return self.title(arena, name);
        }

        // Internal types don't get linked
        if !publicity.is_public() {
            return docvec![
                arena,
                self.comment(arena, INTERNAL_ATTRIBUTE_SPACE_DOCUMENT),
                self.title(arena, name)
            ];
        }

        // Linking to a type within the same page
        if package == self.package && module == self.module {
            return self.link(arena, eco_format!("#{name}"), self.title(arena, name), None);
        }

        // Linking to a module within the package
        if package == self.package {
            // If we are linking to the current package, we might be viewing the
            // documentation locally and so we need to generate a relative link.

            let mut module_path = module.split('/').peekable();
            let mut current_module = self.module.split('/');

            // The documentation page for the final segment of the module is just
            // an html file by itself, so it doesn't form part of the path and doesn't
            // need to be backtracked using `..`.
            let module_name = module_path.next_back().unwrap_or(module);
            _ = current_module.next_back();

            // The two modules might have some sharer part of the path, which we
            // don't need to traverse back through. However, if the two modules are
            // something like `gleam/a/wibble/wobble` and `gleam/b/wibble/wobble`,
            // the `wibble` folders are two different folders despite being at the
            // same position with the same name.
            let mut encountered_different_path = false;
            let mut path = Vec::new();

            // Calculate how far backwards in the directory tree we need to walk
            for segment in current_module {
                // If this is still part of the shared path, we can just skip it:
                // no need to go back and forth through the same directory in the
                // path!
                if !encountered_different_path && module_path.peek() == Some(&segment) {
                    _ = module_path.next();
                } else {
                    encountered_different_path = true;
                    path.push("..");
                }
            }

            // Once we have walked backwards, we walk forwards again to the correct
            // page.
            path.extend(module_path);
            path.push(module_name);

            let qualified_name = docvec![
                arena,
                self.variable(arena, EcoString::from(module_name)),
                DOT_DOCUMENT,
                self.title(arena, name)
            ];

            let title = eco_format!("{module}.{{type {name}}}");

            return self.link(
                arena,
                eco_format!("{path}.html#{name}", path = path.join("/")),
                qualified_name,
                Some(title),
            );
        }

        let module_name = module.split('/').next_back().unwrap_or(module);
        let qualified_name = docvec![
            arena,
            self.variable(arena, EcoString::from(module_name)),
            DOT_DOCUMENT,
            self.title(arena, name)
        ];
        let title = eco_format!("{module}.{{type {name}}}");

        // We can't reliably link to documentation if the type is from a path
        // or git dependency
        match self.dependencies.get(package) {
            Some(Dependency {
                kind: DependencyKind::Hex,
                version,
            }) => self.link(
                arena,
                eco_format!(
                    "https://{package}.hexdocs.pm/{version}/{module}.html#{name}",
                    package = package.replace('_', "-")
                ),
                qualified_name,
                Some(title),
            ),
            Some(_) | None => self.span_with_title(arena, qualified_name, title),
        }
    }

    /// Walk a type and register all the type variable names which occur within
    /// it. This is to ensure that when generating type variable names for
    /// unannotated arguments, we don't print any that clash with existing names.
    ///
    /// We preregister all names before actually printing anything, because we
    /// could run into code like this:
    ///
    /// ```gleam
    /// pub fn wibble(_, _: a) -> b {}
    /// ```
    ///
    /// If we did not preregister the type variables in this case, we would end
    /// up printing `fn wibble(_: a, _: a) -> b` which is not correct.
    ///
    fn register_local_type_variable_names(&mut self, type_: &Type) {
        match type_ {
            Type::Named { arguments, .. } => {
                for argument in arguments {
                    self.register_local_type_variable_names(argument);
                }
            }
            Type::Fn { arguments, return_ } => {
                for argument in arguments {
                    self.register_local_type_variable_names(argument);
                }
                self.register_local_type_variable_names(return_);
            }
            Type::Var { type_ } => match type_.borrow().deref() {
                TypeVar::Link { type_ } => self.register_local_type_variable_names(type_),
                TypeVar::Unbound { id } | TypeVar::Generic { id } => {
                    if let Some(name) = self.names.get_type_variable(*id) {
                        _ = self.printed_type_variable_names.insert(name.clone());
                    }
                }
            },
            Type::Tuple { elements } => {
                for element in elements {
                    self.register_local_type_variable_names(element);
                }
            }
        }
    }

    fn keyword(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        keyword: Document<'static, 'static>,
    ) -> Document<'a, 'doc> {
        self.colour_span(arena, keyword, ColourClass::Keyword)
    }

    fn comment(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: impl Documentable<'a, 'doc>,
    ) -> Document<'a, 'doc> {
        self.colour_span(arena, name, ColourClass::Comment)
    }

    fn title(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: impl Documentable<'a, 'doc>,
    ) -> Document<'a, 'doc> {
        self.colour_span(arena, name, ColourClass::Title)
    }

    fn variable(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: impl Documentable<'a, 'doc>,
    ) -> Document<'a, 'doc> {
        self.colour_span(arena, name, ColourClass::Variable)
    }

    fn colour_span(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: impl Documentable<'a, 'doc>,
        colour_class: ColourClass,
    ) -> Document<'a, 'doc> {
        if !self.options.print_highlighting {
            return name.to_doc(arena);
        }

        let open_tag = match colour_class {
            ColourClass::Variable => ZERO_WIDTH_OPEN_VARIABLE_COLOUR_SPAN,
            ColourClass::Title => ZERO_WIDTH_OPEN_TITLE_COLOUR_SPAN,
            ColourClass::Keyword => ZERO_WIDTH_OPEN_KEYWORD_COLOUR_SPAN,
            ColourClass::Comment => ZERO_WIDTH_OPEN_COMMENT_COLOUR_SPAN,
        };

        name.to_doc(arena)
            .surround(arena, open_tag, ZERO_WIDTH_CLOSED_SPAN_TAG_DOCUMENT)
    }

    fn link(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        href: EcoString,
        name: impl Documentable<'a, 'doc>,
        title: Option<EcoString>,
    ) -> Document<'a, 'doc> {
        if !self.options.print_html {
            return name.to_doc(arena);
        }

        let opening_tag = if let Some(title) = title {
            eco_format!(r#"<a href="{href}" title="{title}">"#)
        } else {
            eco_format!(r#"<a href="{href}">"#)
        };

        name.to_doc(arena).surround(
            arena,
            arena.zero_width_string(opening_tag),
            ZERO_WIDTH_CLOSED_A_TAG_DOCUMENT,
        )
    }

    fn span_with_title(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: impl Documentable<'a, 'doc>,
        title: EcoString,
    ) -> Document<'a, 'doc> {
        if !self.options.print_html {
            return name.to_doc(arena);
        }

        name.to_doc(arena).surround(
            arena,
            arena.zero_width_string(eco_format!(r#"<span title="{title}">"#)),
            ZERO_WIDTH_CLOSED_SPAN_TAG_DOCUMENT,
        )
    }
}

enum ColourClass {
    Variable,
    Title,
    Keyword,
    Comment,
}

const MAX_COLUMNS: isize = 65;
const INDENT: isize = 2;

fn print<'a, 'doc>(doc: Document<'a, 'doc>) -> String {
    doc.to_pretty_string(MAX_COLUMNS)
}
