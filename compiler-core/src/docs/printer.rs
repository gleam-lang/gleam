use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use ecow::{EcoString, eco_format};
use itertools::Itertools;

use crate::{
    ast::{
        ArgNames, CustomType, Definition, Function, ModuleConstant, Publicity,
        RecordConstructorArg, SrcSpan, TypeAlias, TypedArg, TypedDefinition,
        TypedRecordConstructor,
    },
    docvec,
    pretty::{Document, Documentable, break_, join, line, nil, zero_width_string},
    type_::{
        Deprecation, PRELUDE_MODULE_NAME, PRELUDE_PACKAGE_NAME, Type, TypeVar,
        printer::{Names, PrintMode},
    },
};

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

impl Printer<'_> {
    pub fn new<'a>(
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

    pub fn type_definition<'a>(
        &mut self,
        source_links: &SourceLinker,
        statement: &'a TypedDefinition,
    ) -> Option<TypeDefinition<'a>> {
        match statement {
            Definition::CustomType(CustomType {
                publicity: Publicity::Public,
                location,
                name,
                constructors,
                documentation,
                deprecation,
                opaque,
                parameters,
                ..
            }) => Some(TypeDefinition {
                name,
                definition: print(self.custom_type(name, parameters, constructors, *opaque)),
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
                            definition: print(self.record_constructor(constructor)),
                            documentation: markdown_documentation(&constructor.documentation),
                            text_documentation: text_documentation(&constructor.documentation),
                            arguments: constructor
                                .arguments
                                .iter()
                                .filter_map(|arg| arg.label.as_ref().map(|(_, label)| (arg, label)))
                                .map(|(argument, label)| TypeConstructorArg {
                                    name: label.trim_end().to_string(),
                                    doc: markdown_documentation(&argument.doc),
                                })
                                .filter(|arg| !arg.doc.is_empty())
                                .collect(),
                        })
                        .collect()
                },
                source_url: source_links.url(*location),
                opaque: *opaque,
            }),

            Definition::TypeAlias(TypeAlias {
                publicity: Publicity::Public,
                location,
                alias: name,
                parameters,
                type_,
                documentation,
                deprecation,
                ..
            }) => Some(TypeDefinition {
                name,
                definition: print(self.type_alias(name, type_, parameters).group()),
                documentation: markdown_documentation(documentation),
                text_documentation: text_documentation(documentation),
                constructors: vec![],
                source_url: source_links.url(*location),
                deprecation_message: match deprecation {
                    Deprecation::NotDeprecated => "".to_string(),
                    Deprecation::Deprecated { message } => message.to_string(),
                },
                opaque: false,
            }),

            Definition::TypeAlias(_)
            | Definition::CustomType(_)
            | Definition::Function(_)
            | Definition::Import(_)
            | Definition::ModuleConstant(_) => None,
        }
    }

    pub fn value<'a>(
        &mut self,
        source_links: &SourceLinker,
        statement: &'a TypedDefinition,
    ) -> Option<DocsValues<'a>> {
        // Ensure that any type variables we printed in previous definitions don't
        // affect our printing of this definition. Two type variables in different
        // definitions can have the same name without clashing.
        self.printed_type_variable_names.clear();
        self.next_type_variable_id = 0;

        match statement {
            Definition::Function(Function {
                publicity: Publicity::Public,
                name: Some((_, name)),
                documentation: doc,
                location,
                deprecation,
                arguments,
                return_type,
                ..
            }) => Some(DocsValues {
                name,
                definition: print(self.function_signature(name, arguments, return_type)),
                documentation: markdown_documentation(doc),
                text_documentation: text_documentation(doc),
                source_url: source_links.url(*location),
                deprecation_message: match deprecation {
                    Deprecation::NotDeprecated => "".to_string(),
                    Deprecation::Deprecated { message } => message.to_string(),
                },
            }),

            Definition::ModuleConstant(ModuleConstant {
                publicity: Publicity::Public,
                documentation,
                location,
                name,
                type_,
                deprecation,
                ..
            }) => Some(DocsValues {
                name,
                definition: print(self.constant(name, type_)),
                documentation: markdown_documentation(documentation),
                text_documentation: text_documentation(documentation),
                source_url: source_links.url(*location),
                deprecation_message: match deprecation {
                    Deprecation::NotDeprecated => "".to_string(),
                    Deprecation::Deprecated { message } => message.to_string(),
                },
            }),

            Definition::TypeAlias(_)
            | Definition::CustomType(_)
            | Definition::Function(_)
            | Definition::Import(_)
            | Definition::ModuleConstant(_) => None,
        }
    }

    fn custom_type<'a>(
        &mut self,
        name: &'a str,
        parameters: &'a [(SrcSpan, EcoString)],
        constructors: &'a [TypedRecordConstructor],
        opaque: bool,
    ) -> Document<'a> {
        let arguments = if parameters.is_empty() {
            nil()
        } else {
            Self::wrap_arguments(
                parameters
                    .iter()
                    .map(|(_, parameter)| self.variable(parameter)),
            )
        };

        let keywords = if opaque {
            "pub opaque type "
        } else {
            "pub type "
        };

        let type_head = docvec![self.keyword(keywords), self.title(name), arguments];

        if constructors.is_empty() || opaque {
            return type_head;
        }

        let constructors = constructors
            .iter()
            .map(|constructor| {
                line()
                    .append(self.record_constructor(constructor))
                    .nest(INDENT)
            })
            .collect_vec();

        docvec![type_head, " {", constructors, line(), "}"]
    }

    pub fn record_constructor<'a>(
        &mut self,
        constructor: &'a TypedRecordConstructor,
    ) -> Document<'a> {
        if constructor.arguments.is_empty() {
            return self.title(&constructor.name);
        }

        let arguments = constructor.arguments.iter().map(
            |RecordConstructorArg { label, type_, .. }| match label {
                Some((_, label)) => self
                    .variable(label)
                    .append(": ")
                    .append(self.type_(type_, PrintMode::Normal)),
                None => self.type_(type_, PrintMode::Normal),
            },
        );

        let arguments = Self::wrap_arguments(arguments);

        docvec![self.title(&constructor.name), arguments].group()
    }

    fn type_alias<'a>(
        &mut self,
        name: &'a str,
        type_: &Type,
        parameters: &[(SrcSpan, EcoString)],
    ) -> Document<'a> {
        let parameters = if parameters.is_empty() {
            nil()
        } else {
            let arguments = parameters
                .iter()
                .map(|(_, parameter)| self.variable(parameter));
            Self::wrap_arguments(arguments)
        };

        docvec![
            self.keyword("pub type "),
            self.title(name),
            parameters,
            " =",
            line()
                .append(self.type_(type_, PrintMode::ExpandAliases))
                .nest(INDENT)
        ]
    }

    fn constant<'a>(&mut self, name: &'a str, type_: &Type) -> Document<'a> {
        self.register_local_type_variable_names(type_);

        docvec![
            self.keyword("pub const "),
            self.title(name),
            ": ",
            self.type_(type_, PrintMode::Normal)
        ]
    }

    fn function_signature<'a>(
        &mut self,
        name: &'a str,
        arguments: &'a [TypedArg],
        return_type: &Type,
    ) -> Document<'a> {
        for argument in arguments {
            self.register_local_type_variable_names(&argument.type_);
        }
        self.register_local_type_variable_names(return_type);

        let arguments = if arguments.is_empty() {
            "()".to_doc()
        } else {
            Self::wrap_arguments(arguments.iter().map(|argument| {
                let name = self.variable(self.argument_name(argument));
                docvec![name, ": ", self.type_(&argument.type_, PrintMode::Normal)].group()
            }))
        };

        docvec![
            self.keyword("pub fn "),
            self.title(name),
            arguments,
            " -> ",
            self.type_(return_type, PrintMode::Normal)
        ]
        .group()
    }

    fn argument_name<'a>(&self, arg: &'a TypedArg) -> Document<'a> {
        match &arg.names {
            ArgNames::Named { name, .. } => name.to_doc(),
            ArgNames::NamedLabelled { label, name, .. } => docvec![label, " ", name],
            // We remove the underscore from discarded function arguments since we don't want to
            // expose this kind of detail: https://github.com/gleam-lang/gleam/issues/2561
            ArgNames::Discard { name, .. } => match name.strip_prefix('_').unwrap_or(name) {
                "" => "arg".to_doc(),
                name => name.to_doc(),
            },
            ArgNames::LabelledDiscard { label, name, .. } => {
                docvec![label, " ", name.strip_prefix('_').unwrap_or(name).to_doc()]
            }
        }
    }

    fn wrap_arguments<'a>(arguments: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
        break_("(", "(")
            .append(join(arguments, break_(",", ", ")))
            .nest_if_broken(INDENT)
            .append(break_(",", ""))
            .append(")")
    }

    fn type_arguments<'a>(arguments: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
        break_("", "")
            .append(join(arguments, break_(",", ", ")))
            .nest_if_broken(INDENT)
            .append(break_(",", ""))
            .group()
            .surround("(", ")")
    }

    fn type_(&mut self, type_: &Type, print_mode: PrintMode) -> Document<'static> {
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
                        self.named_type_name(publicity, package, module, name)
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
                            self.named_type_name(&Publicity::Public, package, module, alias)
                        } else {
                            self.named_type_name(publicity, package, module, name)
                        }
                    }
                };

                if arguments.is_empty() {
                    name
                } else {
                    name.append(Self::type_arguments(
                        arguments
                            .iter()
                            .map(|argument| self.type_(argument, PrintMode::Normal)),
                    ))
                }
            }
            Type::Fn { arguments, return_ } => docvec![
                self.keyword("fn"),
                Self::type_arguments(
                    arguments
                        .iter()
                        .map(|argument| self.type_(argument, PrintMode::Normal))
                ),
                " -> ",
                self.type_(return_, PrintMode::Normal)
            ],
            Type::Tuple { elements } => docvec![
                "#",
                Self::type_arguments(
                    elements
                        .iter()
                        .map(|element| self.type_(element, PrintMode::Normal))
                ),
            ],
            Type::Var { type_ } => match type_.as_ref().borrow().deref() {
                TypeVar::Link { type_ } => self.type_(type_, PrintMode::Normal),

                TypeVar::Unbound { id } | TypeVar::Generic { id } => {
                    let name = self.type_variable(*id);
                    self.variable(name)
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
            rest -= 1
        }

        self.next_type_variable_id += 1;
        chars.into_iter().rev().collect()
    }

    fn named_type_name(
        &self,
        publicity: &Publicity,
        package: &str,
        module: &str,
        name: &EcoString,
    ) -> Document<'static> {
        // There's no documentation page for the prelude
        if package == PRELUDE_PACKAGE_NAME && module == PRELUDE_MODULE_NAME {
            return self.title(name);
        }

        // Internal types don't get linked
        if !publicity.is_public() {
            return docvec![self.comment("@internal ".to_doc()), self.title(name)];
        }

        // Linking to a type within the same page
        if package == self.package && module == self.module {
            return self.link(eco_format!("#{name}"), self.title(name), None);
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
                self.variable(EcoString::from(module_name)),
                ".",
                self.title(name)
            ];

            let title = eco_format!("{module}.{{type {name}}}");

            return self.link(
                eco_format!("{path}.html#{name}", path = path.join("/")),
                qualified_name,
                Some(title),
            );
        }

        let module_name = module.split('/').next_back().unwrap_or(module);
        let qualified_name = docvec![
            self.variable(EcoString::from(module_name)),
            ".",
            self.title(name)
        ];
        let title = eco_format!("{module}.{{type {name}}}");

        // We can't reliably link to documentation if the type is from a path
        // or git dependency
        match self.dependencies.get(package) {
            Some(Dependency {
                kind: DependencyKind::Hex,
                version,
            }) => self.link(
                eco_format!("https://hexdocs.pm/{package}/{version}/{module}.html#{name}"),
                qualified_name,
                Some(title),
            ),
            Some(_) | None => self.span_with_title(qualified_name, title),
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

    fn keyword<'a>(&self, keyword: impl Documentable<'a>) -> Document<'a> {
        self.colour_span(keyword, "keyword")
    }

    fn comment<'a>(&self, name: impl Documentable<'a>) -> Document<'a> {
        self.colour_span(name, "comment")
    }

    fn title<'a>(&self, name: impl Documentable<'a>) -> Document<'a> {
        self.colour_span(name, "title")
    }

    fn variable<'a>(&self, name: impl Documentable<'a>) -> Document<'a> {
        self.colour_span(name, "variable")
    }

    fn colour_span<'a>(
        &self,
        name: impl Documentable<'a>,
        colour_class: &'static str,
    ) -> Document<'a> {
        if !self.options.print_highlighting {
            return name.to_doc();
        }

        name.to_doc().surround(
            zero_width_string(eco_format!(r#"<span class="hljs-{colour_class}">"#)),
            zero_width_string("</span>".into()),
        )
    }

    fn link<'a>(
        &self,
        href: EcoString,
        name: impl Documentable<'a>,
        title: Option<EcoString>,
    ) -> Document<'a> {
        if !self.options.print_html {
            return name.to_doc();
        }

        let opening_tag = if let Some(title) = title {
            eco_format!(r#"<a href="{href}" title="{title}">"#)
        } else {
            eco_format!(r#"<a href="{href}">"#)
        };

        name.to_doc().surround(
            zero_width_string(opening_tag),
            zero_width_string("</a>".into()),
        )
    }

    fn span_with_title<'a>(&self, name: impl Documentable<'a>, title: EcoString) -> Document<'a> {
        if !self.options.print_html {
            return name.to_doc();
        }

        name.to_doc().surround(
            zero_width_string(eco_format!(r#"<span title="{title}">"#)),
            zero_width_string("</span>".into()),
        )
    }
}

const MAX_COLUMNS: isize = 65;
const INDENT: isize = 2;

fn print(doc: Document<'_>) -> String {
    doc.to_pretty_string(MAX_COLUMNS)
}
