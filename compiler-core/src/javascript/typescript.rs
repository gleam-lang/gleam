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

use crate::ast::AssignName;
use crate::type_::{is_prelude_module, PRELUDE_MODULE_NAME};
use crate::{
    ast::{
        CustomType, Definition, Function, Import, ModuleConstant, TypeAlias, TypedArg,
        TypedConstant, TypedDefinition, TypedModule, TypedRecordConstructor,
    },
    docvec,
    javascript::JavaScriptCodegenTarget,
    pretty::{break_, Document, Documentable},
    type_::{Type, TypeVar},
};
use ecow::EcoString;
use itertools::Itertools;
use std::{collections::HashMap, ops::Deref, sync::Arc};

use super::{concat, import::Imports, line, lines, wrap_args, Output, INDENT};

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
        .map(|id| id_to_type_var(*id))
        .collect();

    docvec![
        name,
        if generic_names.is_empty() {
            super::nil()
        } else {
            wrap_generic_args(generic_names)
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
    for typ in types {
        generic_ids(typ, &mut ids);
    }
    ids
}

fn generic_ids(type_: &Type, ids: &mut HashMap<u64, u64>) {
    match type_ {
        Type::Var { type_: typ } => match typ.borrow().deref() {
            TypeVar::Unbound { id, .. } | TypeVar::Generic { id, .. } => {
                let count = ids.entry(*id).or_insert(0);
                *count += 1;
            }
            TypeVar::Link { type_: typ } => generic_ids(typ, ids),
        },
        Type::Named { args, .. } => {
            for arg in args {
                generic_ids(arg, ids)
            }
        }
        Type::Fn { args, retrn } => {
            for arg in args {
                generic_ids(arg, ids)
            }
            generic_ids(retrn, ids);
        }
        Type::Tuple { elems } => {
            for elem in elems {
                generic_ids(elem, ids)
            }
        }
    }
}

/// Prints a Gleam tuple in the TypeScript equivalent syntax
///
fn tuple<'a>(elems: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    break_("", "")
        .append(concat(Itertools::intersperse(
            elems.into_iter(),
            break_(",", ", "),
        )))
        .nest(INDENT)
        .append(break_("", ""))
        .surround("[", "]")
        .group()
}

fn wrap_generic_args<'a, I>(args: I) -> Document<'a>
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
        .surround("<", ">")
        .group()
}

/// Returns a name that can be used as a TypeScript type name. If there is a
/// naming clash a '_' will be appended.
///
fn ts_safe_type_name(mut name: String) -> String {
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
        name
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

    pub fn compile(&mut self) -> Output<'a> {
        let mut imports = self.collect_imports();
        let statements = self
            .module
            .definitions
            .iter()
            .flat_map(|s| self.statement(s));

        // Two lines between each statement
        let mut statements: Vec<_> =
            Itertools::intersperse(statements, Ok(lines(2))).try_collect()?;

        // Put it all together

        if self.prelude_used() {
            let path = self.import_path(&self.module.type_info.package, PRELUDE_MODULE_NAME);
            imports.register_module(path, ["_".into()], []);
        }

        if imports.is_empty() && statements.is_empty() {
            Ok(docvec!("export {}", line()))
        } else if imports.is_empty() {
            statements.push(line());
            Ok(statements.to_doc())
        } else if statements.is_empty() {
            Ok(imports.into_doc(JavaScriptCodegenTarget::TypeScriptDeclarations))
        } else {
            Ok(docvec![
                imports.into_doc(JavaScriptCodegenTarget::TypeScriptDeclarations),
                line(),
                statements,
                line()
            ])
        }
    }

    fn collect_imports(&mut self) -> Imports<'a> {
        let mut imports = Imports::new();

        for statement in &self.module.definitions {
            match statement {
                Definition::Function(Function { .. })
                | Definition::TypeAlias(TypeAlias { .. })
                | Definition::CustomType(CustomType { .. })
                | Definition::ModuleConstant(ModuleConstant { .. }) => (),

                Definition::Import(Import {
                    module,
                    package,
                    as_name,
                    ..
                }) => {
                    match as_name {
                        Some((AssignName::Variable(name), _)) => {
                            let _ = self.aliased_module_names.insert(module, name);
                        }
                        Some((AssignName::Discard(_), _)) | None => (),
                    }

                    self.register_import(&mut imports, package, module);
                }
            }
        }

        imports
    }

    /// Registers an import of an external module so that it can be added to
    /// the top of the generated Document. The module names are prefixed with a
    /// "$" symbol to prevent any clashes with other Gleam names that may be
    /// used in this module.
    ///
    fn register_import(&mut self, imports: &mut Imports<'a>, package: &'a str, module: &'a str) {
        let path = self.import_path(package, module);
        imports.register_module(path, [self.module_name(module)], []);
    }

    /// Calculates the path of where to import an external module from
    ///
    fn import_path(&self, package: &'a str, module: &'a str) -> String {
        // DUPE: current_module_name_segments_count
        // TODO: strip shared prefixed between current module and imported
        // module to avoid descending and climbing back out again
        if package == self.module.type_info.package || package.is_empty() {
            // Same package
            match self.current_module_name_segments_count {
                1 => format!("./{module}.d.mts"),
                _ => {
                    let prefix = "../".repeat(self.current_module_name_segments_count - 1);
                    format!("{prefix}{module}.d.mts")
                }
            }
        } else {
            // Different package
            let prefix = "../".repeat(self.current_module_name_segments_count);
            format!("{prefix}{package}/{module}.d.mts")
        }
    }

    fn statement(&mut self, statement: &'a TypedDefinition) -> Vec<Output<'a>> {
        match statement {
            Definition::TypeAlias(TypeAlias {
                alias,
                public,
                type_,
                ..
            }) if *public => vec![self.type_alias(alias, type_)],
            Definition::TypeAlias(TypeAlias { .. }) => vec![],

            Definition::Import(Import { .. }) => vec![],

            Definition::CustomType(CustomType {
                public,
                constructors,
                opaque,
                name,
                typed_parameters,
                ..
            }) if *public => {
                self.custom_type_definition(name, typed_parameters, constructors, *opaque)
            }
            Definition::CustomType(CustomType { .. }) => vec![],

            Definition::ModuleConstant(ModuleConstant {
                public,
                name,
                value,
                ..
            }) if *public => vec![self.module_constant(name, value)],
            Definition::ModuleConstant(ModuleConstant { .. }) => vec![],

            Definition::Function(Function {
                arguments,
                name,
                public,
                return_type,
                ..
            }) if *public => vec![self.module_function(name, arguments, return_type)],
            Definition::Function(Function { .. }) => vec![],
        }
    }

    fn type_alias(&mut self, alias: &str, type_: &Type) -> Output<'a> {
        Ok(docvec![
            "export type ",
            Document::String(ts_safe_type_name(alias.to_string())),
            " = ",
            self.print_type(type_),
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
        name: &'a str,
        typed_parameters: &'a [Arc<Type>],
        constructors: &'a [TypedRecordConstructor],
        opaque: bool,
    ) -> Vec<Output<'a>> {
        let mut definitions: Vec<Output<'_>> = constructors
            .iter()
            .map(|constructor| Ok(self.record_definition(constructor, opaque)))
            .collect();

        let definition = if constructors.is_empty() {
            "any".to_doc()
        } else {
            let constructors = constructors.iter().map(|x| {
                name_with_generics(
                    super::maybe_escape_identifier_doc(&x.name),
                    x.arguments.iter().map(|a| &a.type_),
                )
            });
            concat(Itertools::intersperse(constructors, break_("| ", " | ")))
        };

        definitions.push(Ok(docvec![
            "export type ",
            name_with_generics(Document::String(format!("{name}$")), typed_parameters),
            " = ",
            definition,
            ";",
        ]));

        definitions
    }

    fn record_definition(
        &mut self,
        constructor: &'a TypedRecordConstructor,
        opaque: bool,
    ) -> Document<'a> {
        self.set_prelude_used();
        let head = docvec![
            // opaque type constructors are not exposed to JS
            if opaque {
                super::nil()
            } else {
                "export ".to_doc()
            },
            "class ",
            name_with_generics(
                super::maybe_escape_identifier_doc(&constructor.name),
                constructor.arguments.iter().map(|a| &a.type_)
            ),
            " extends _.CustomType {"
        ];

        if constructor.arguments.is_empty() {
            return head.append("}");
        };

        let class_body = docvec![
            line(),
            // First add the constructor
            "constructor",
            wrap_args(constructor.arguments.iter().enumerate().map(|(i, arg)| {
                let name = arg
                    .label
                    .as_ref()
                    .map(|s| super::maybe_escape_identifier_doc(s))
                    .unwrap_or_else(|| Document::String(format!("argument${i}")));
                docvec![name, ": ", self.do_print_force_generic_param(&arg.type_)]
            })),
            ";",
            line(),
            line(),
            // Then add each field to the class
            concat(Itertools::intersperse(
                constructor.arguments.iter().enumerate().map(|(i, arg)| {
                    let name = arg
                        .label
                        .as_ref()
                        .map(|s| super::maybe_escape_identifier_doc(s))
                        .unwrap_or_else(|| Document::String(format!("{i}")));
                    docvec![
                        name,
                        ": ",
                        self.do_print_force_generic_param(&arg.type_),
                        ";"
                    ]
                }),
                line(),
            )),
        ]
        .nest(INDENT);

        docvec![head, class_body, line(), "}"]
    }

    fn module_constant(&mut self, name: &'a str, value: &'a TypedConstant) -> Output<'a> {
        Ok(docvec![
            "export const ",
            super::maybe_escape_identifier_doc(name),
            ": ",
            self.print_type(&value.type_()),
            ";",
        ])
    }

    fn module_function(
        &mut self,
        name: &'a str,
        args: &'a [TypedArg],
        return_type: &'a Arc<Type>,
    ) -> Output<'a> {
        let generic_usages = collect_generic_usages(
            HashMap::new(),
            std::iter::once(return_type).chain(args.iter().map(|a| &a.type_)),
        );
        let generic_names: Vec<Document<'_>> = generic_usages
            .iter()
            .filter(|(_id, use_count)| **use_count > 1)
            .sorted_by_key(|x| x.0)
            .map(|(id, _use_count)| id_to_type_var(*id))
            .collect();

        Ok(docvec![
            "export function ",
            super::maybe_escape_identifier_doc(name),
            if generic_names.is_empty() {
                super::nil()
            } else {
                wrap_generic_args(generic_names)
            },
            wrap_args(
                args.iter()
                    .enumerate()
                    .map(|(i, a)| match a.get_variable_name() {
                        None => {
                            docvec![
                                "x",
                                i,
                                ": ",
                                self.print_type_with_generic_usages(&a.type_, &generic_usages)
                            ]
                        }
                        Some(name) => docvec![
                            super::maybe_escape_identifier_doc(name),
                            ": ",
                            self.print_type_with_generic_usages(&a.type_, &generic_usages)
                        ],
                    }),
            ),
            ": ",
            self.print_type_with_generic_usages(return_type, &generic_usages),
            ";",
        ])
    }

    /// Converts a Gleam type into a TypeScript type string
    ///
    pub fn print_type(&mut self, type_: &Type) -> Document<'static> {
        self.do_print(type_, None)
    }

    /// Helper function for generating a TypeScript type string after collecting
    /// all of the generics used in a statement
    ///
    pub fn print_type_with_generic_usages(
        &mut self,
        type_: &Type,
        generic_usages: &HashMap<u64, u64>,
    ) -> Document<'static> {
        self.do_print(type_, Some(generic_usages))
    }

    /// Get the locally used name for a module. Either the last segment, or the
    /// alias if one was given when imported.
    ///
    fn module_name(&self, name: &str) -> String {
        // The prelude is always `_`
        if name.is_empty() {
            return "_".into();
        }

        let name = match self.aliased_module_names.get(name) {
            Some(name) => (*name).to_string(),
            None => name
                .split('/')
                .last()
                .expect("Non empty module path")
                .to_string(),
        };

        format!("${name}")
    }

    fn do_print(
        &mut self,
        type_: &Type,
        generic_usages: Option<&HashMap<u64, u64>>,
    ) -> Document<'static> {
        match type_ {
            Type::Var { type_: typ } => self.print_var(&typ.borrow(), generic_usages, false),

            Type::Named {
                name, module, args, ..
            } if is_prelude_module(module) => self.print_prelude_type(name, args, generic_usages),

            Type::Named {
                name, args, module, ..
            } => self.print_type_app(name, args, module, generic_usages),

            Type::Fn { args, retrn } => self.print_fn(args, retrn, generic_usages),

            Type::Tuple { elems } => tuple(elems.iter().map(|e| self.do_print(e, generic_usages))),
        }
    }

    fn do_print_force_generic_param(&mut self, type_: &Type) -> Document<'static> {
        match type_ {
            Type::Var { type_: typ } => self.print_var(&typ.borrow(), None, true),

            Type::Named {
                name, module, args, ..
            } if is_prelude_module(module) => self.print_prelude_type(name, args, None),

            Type::Named {
                name, args, module, ..
            } => self.print_type_app(name, args, module, None),

            Type::Fn { args, retrn } => self.print_fn(args, retrn, None),

            Type::Tuple { elems } => tuple(elems.iter().map(|e| self.do_print(e, None))),
        }
    }

    fn print_var(
        &mut self,
        type_: &TypeVar,
        generic_usages: Option<&HashMap<u64, u64>>,
        force_generic_id: bool,
    ) -> Document<'static> {
        match type_ {
            TypeVar::Unbound { id } | TypeVar::Generic { id } => match &generic_usages {
                Some(usages) => match usages.get(id) {
                    Some(&0) => super::nil(),
                    Some(&1) => "any".to_doc(),
                    _ => id_to_type_var(*id),
                },
                None => {
                    if force_generic_id {
                        id_to_type_var(*id)
                    } else {
                        "any".to_doc()
                    }
                }
            },
            TypeVar::Link { type_: typ } => self.do_print(typ, generic_usages),
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
        args: &[Arc<Type>],
        generic_usages: Option<&HashMap<u64, u64>>,
    ) -> Document<'static> {
        match name {
            "Nil" => "null".to_doc(),
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
                    wrap_generic_args(args.iter().map(|x| self.do_print(x, generic_usages)))
                ]
            }
            "Result" => {
                self.tracker.prelude_used = true;
                docvec![
                    "_.Result",
                    wrap_generic_args(args.iter().map(|x| self.do_print(x, generic_usages)))
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
        args: &[Arc<Type>],
        module: &str,
        generic_usages: Option<&HashMap<u64, u64>>,
    ) -> Document<'static> {
        let name = format!("{}$", ts_safe_type_name(name.to_string()));
        let name = match module == self.module.name {
            true => Document::String(name),
            false => {
                // If type comes from a separate module, use that module's name
                // as a TypeScript namespace prefix
                docvec![
                    Document::String(self.module_name(module)),
                    ".",
                    Document::String(name),
                ]
            }
        };
        if args.is_empty() {
            return name;
        }

        // If the App type takes arguments, pass them in as TypeScript generics
        docvec![
            name,
            wrap_generic_args(args.iter().map(|a| self.do_print(a, generic_usages)))
        ]
    }

    /// Prints the TypeScript type for an anonymous function (aka lambda)
    ///
    fn print_fn(
        &mut self,
        args: &[Arc<Type>],
        retrn: &Type,
        generic_usages: Option<&HashMap<u64, u64>>,
    ) -> Document<'static> {
        docvec![
            wrap_args(args.iter().enumerate().map(|(idx, a)| docvec![
                "x",
                idx,
                ": ",
                self.do_print(a, generic_usages)
            ])),
            " => ",
            self.do_print(retrn, generic_usages)
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
