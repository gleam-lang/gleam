use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use ecow::EcoString;
use itertools::Itertools;

use crate::{
    ast::{
        ArgNames, Definition, Function, Publicity, RecordConstructorArg, TypedArg, TypedCustomType,
        TypedDefinition, TypedFunction, TypedModuleConstant, TypedRecordConstructor,
        TypedTypeAlias,
    },
    docvec,
    pretty::{Document, Documentable, break_, join, line},
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
            Definition::CustomType(ct) if ct.publicity.is_public() => Some(TypeDefinition {
                name: &ct.name,
                definition: print(self.custom_type(ct)),
                documentation: markdown_documentation(&ct.documentation),
                text_documentation: text_documentation(&ct.documentation),
                deprecation_message: match &ct.deprecation {
                    Deprecation::NotDeprecated => "".to_string(),
                    Deprecation::Deprecated { message } => message.to_string(),
                },
                constructors: ct
                    .constructors
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
                source_url: source_links.url(ct.location),
                opaque: ct.opaque,
            }),

            Definition::TypeAlias(alias) if alias.publicity.is_public() => Some(TypeDefinition {
                name: alias.alias.as_str(),
                definition: print(self.type_alias(alias).group()),
                documentation: markdown_documentation(&alias.documentation),
                text_documentation: text_documentation(&alias.documentation),
                constructors: vec![],
                source_url: source_links.url(alias.location),
                deprecation_message: match &alias.deprecation {
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
            Definition::Function(
                function @ Function {
                    publicity: Publicity::Public,
                    name,
                    documentation: doc,

                    location,
                    deprecation,
                    ..
                },
            ) => {
                let (_, name) = name
                    .as_ref()
                    .expect("Function in a definition must be named");

                Some(DocsValues {
                    name,
                    definition: print(self.function_signature(function)),
                    documentation: markdown_documentation(doc),
                    text_documentation: text_documentation(doc),
                    source_url: source_links.url(*location),
                    deprecation_message: match deprecation {
                        Deprecation::NotDeprecated => "".to_string(),
                        Deprecation::Deprecated { message } => message.to_string(),
                    },
                })
            }

            Definition::ModuleConstant(constant) if constant.publicity.is_public() => {
                Some(DocsValues {
                    name: constant.name.as_str(),
                    definition: print(self.constant(constant)),
                    documentation: markdown_documentation(&constant.documentation),
                    text_documentation: text_documentation(&constant.documentation),
                    source_url: source_links.url(constant.location),
                    deprecation_message: match &constant.deprecation {
                        Deprecation::NotDeprecated => "".to_string(),
                        Deprecation::Deprecated { message } => message.to_string(),
                    },
                })
            }

            _ => None,
        }
    }

    fn custom_type<'a>(&mut self, custom_type: &'a TypedCustomType) -> Document<'a> {
        let doc = "pub "
            .to_doc()
            .append(if custom_type.opaque {
                "opaque type "
            } else {
                "type "
            })
            .append(if custom_type.parameters.is_empty() {
                custom_type.name.clone().to_doc()
            } else {
                let arguments = custom_type.parameters.iter().map(|(_, e)| e.to_doc());

                custom_type
                    .name
                    .as_str()
                    .to_doc()
                    .append(Self::wrap_arguments(arguments))
                    .group()
            });

        if custom_type.constructors.is_empty() {
            return doc;
        }
        let doc = doc.append(" {");

        let inner = custom_type
            .constructors
            .iter()
            .map(|constructor| {
                line()
                    .append(self.record_constructor(constructor))
                    .nest(INDENT)
            })
            .collect_vec();

        doc.append(inner).append(line()).append("}")
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

    fn type_alias<'a>(&mut self, alias: &'a TypedTypeAlias) -> Document<'a> {
        "pub type "
            .to_doc()
            .append(alias.alias.as_str())
            .append(" = ")
            .append(self.type_(&alias.type_))
    }

    fn constant<'a>(&mut self, constant: &'a TypedModuleConstant) -> Document<'a> {
        "pub const "
            .to_doc()
            .append(constant.name.as_str())
            .append(": ")
            .append(self.type_(&constant.type_))
    }

    fn function_signature<'a>(&mut self, function: &'a TypedFunction) -> Document<'a> {
        let arguments = function.arguments.iter().map(|argument| {
            let name = self.argument_name(argument);
            docvec![name, ": ", self.type_(&argument.type_)].group()
        });

        "pub fn "
            .to_doc()
            .append(
                function
                    .name
                    .as_ref()
                    .expect("Module functions must have names")
                    .1
                    .as_str(),
            )
            .append(Self::wrap_arguments(arguments))
            .append(" -> ")
            .append(self.type_(&function.return_type))
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
