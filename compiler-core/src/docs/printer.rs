use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use ecow::EcoString;
use itertools::Itertools;

use crate::{
    ast::{
        ArgNames, CustomType, Definition, Function, ModuleConstant, Publicity,
        RecordConstructorArg, SrcSpan, TypeAlias, TypedArg, TypedDefinition,
        TypedRecordConstructor,
    },
    docvec,
    pretty::{Document, Documentable, break_, join, line, nil},
    type_::{Deprecation, Type, TypeVar, printer::Names},
};

use super::{
    DocsValues, TypeConstructor, TypeConstructorArg, TypeDefinition, markdown_documentation,
    source_links::SourceLinker, text_documentation,
};

pub struct Printer<'a> {
    names: &'a Names,
    printed_type_variables: HashMap<u64, EcoString>,
    printed_type_variable_names: HashSet<EcoString>,
    uid: u64,
}

impl Printer<'_> {
    pub fn new<'a>(names: &'a Names) -> Printer<'a> {
        Printer {
            names,
            printed_type_variables: HashMap::new(),
            printed_type_variable_names: HashSet::new(),
            uid: 0,
        }
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
                constructors: constructors
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
                    .collect(),
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

            _ => None,
        }
    }

    pub fn value<'a>(
        &mut self,
        source_links: &SourceLinker,
        statement: &'a TypedDefinition,
    ) -> Option<DocsValues<'a>> {
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

            _ => None,
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
            Self::wrap_arguments(parameters.iter().map(|(_, parameter)| parameter.to_doc()))
        };

        let keywords = if opaque { "opaque type " } else { "type " };

        let type_head = docvec!["pub ", keywords, name, arguments];

        if constructors.is_empty() {
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
            return constructor.name.as_str().to_doc();
        }

        let arguments = constructor.arguments.iter().map(
            |RecordConstructorArg { label, type_, .. }| match label {
                Some((_, label)) => label.to_doc().append(": ").append(self.type_(type_)),
                None => self.type_(type_),
            },
        );

        constructor
            .name
            .as_str()
            .to_doc()
            .append(Self::wrap_arguments(arguments))
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
            let arguments = parameters.iter().map(|(_, parameter)| parameter.to_doc());
            Self::wrap_arguments(arguments)
        };

        docvec![
            "pub type ",
            name,
            parameters,
            break_(" =", " = "),
            self.type_(type_).nest(INDENT)
        ]
    }

    fn constant<'a>(&mut self, name: &'a str, type_: &Type) -> Document<'a> {
        docvec!["pub const ", name, ": ", self.type_(type_)]
    }

    fn function_signature<'a>(
        &mut self,
        name: &'a str,
        arguments: &'a [TypedArg],
        return_type: &Type,
    ) -> Document<'a> {
        let arguments = arguments.iter().map(|argument| {
            let name = self.argument_name(argument);
            docvec![name, ": ", self.type_(&argument.type_)].group()
        });

        docvec![
            "pub fn ",
            name,
            Self::wrap_arguments(arguments),
            " -> ",
            self.type_(return_type)
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

    fn type_(&mut self, type_: &Type) -> Document<'static> {
        match type_ {
            Type::Named { name, args, .. } => {
                if args.is_empty() {
                    name.clone().to_doc()
                } else {
                    name.clone().to_doc().append(Self::type_arguments(
                        args.iter().map(|argument| self.type_(argument)),
                    ))
                }
            }
            Type::Fn { args, return_ } => docvec![
                "fn",
                Self::type_arguments(args.iter().map(|argument| self.type_(argument))),
                " -> ",
                self.type_(return_)
            ],
            Type::Tuple { elements } => docvec![
                "#",
                Self::type_arguments(elements.iter().map(|element| self.type_(element))),
            ],
            Type::Var { type_ } => match type_.as_ref().borrow().deref() {
                TypeVar::Link { type_ } => self.type_(type_),

                TypeVar::Unbound { id } | TypeVar::Generic { id } => self.type_variable(*id),
            },
        }
    }

    fn type_variable(&mut self, id: u64) -> Document<'static> {
        if let Some(name) = self.names.get_type_variable(id) {
            return name.clone().to_doc();
        }

        if let Some(name) = self.printed_type_variables.get(&id) {
            return name.clone().to_doc();
        }

        loop {
            let name = self.next_letter();
            if !self.printed_type_variable_names.contains(&name) {
                _ = self.printed_type_variable_names.insert(name.clone());
                _ = self.printed_type_variables.insert(id, name.clone());
                return name.to_doc();
            }
        }
    }

    fn next_letter(&mut self) -> EcoString {
        let alphabet_length = 26;
        let char_offset = 97;
        let mut chars = vec![];
        let mut n;
        let mut rest = self.uid;

        loop {
            n = rest % alphabet_length;
            rest /= alphabet_length;
            chars.push((n as u8 + char_offset) as char);

            if rest == 0 {
                break;
            }
            rest -= 1
        }

        self.uid += 1;
        chars.into_iter().rev().collect()
    }
}

const MAX_COLUMNS: isize = 65;
const INDENT: isize = 2;

fn print(doc: Document<'_>) -> String {
    doc.to_pretty_string(MAX_COLUMNS)
}
