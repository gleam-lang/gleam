use std::collections::HashMap;

use ecow::EcoString;
use lsp_types::Location;

use crate::{
    analyse,
    ast::{
        self, visit::Visit, ArgNames, AssignName, CallArg, Constant, CustomType, Definition, Function, Import, ModuleConstant, Pattern, RecordConstructor, SrcSpan, TypeAst, TypeAstConstructor, TypeAstFn, TypeAstTuple, TypedConstant, TypedExpr, TypedFunction, TypedModule, TypedPattern
    },
    build::Located,
    type_::{
        error::{Named, VariableOrigin}, ModuleInterface, ModuleValueConstructor, PatternConstructor, Type, TypeVar, ValueConstructor, ValueConstructorVariant
    },
};

use super::{
    compiler::ModuleSourceInformation, rename::RenameTarget, src_span_to_lsp_range, url_from_path,
};

pub enum Referenced {
    LocalVariable {
        definition_location: SrcSpan,
        location: SrcSpan,
        origin: Option<VariableOrigin>,
    },
    ModuleName {
        name: EcoString,
        location: SrcSpan,
    },
    ModuleValue {
        module: EcoString,
        name: EcoString,
        location: SrcSpan,
        name_kind: Named,
        target_kind: RenameTarget,
    },
    ModuleType {
        module: EcoString,
        name: EcoString,
        location: SrcSpan,
        target_kind: RenameTarget,
    },
}

pub fn reference_for_ast_node(
    found: Located<'_>,
    current_module: &EcoString,
) -> Option<Referenced> {
    match found {
        Located::Expression {
            expression:
                TypedExpr::Var {
                    constructor:
                        ValueConstructor {
                            variant:
                                ValueConstructorVariant::LocalVariable {
                                    location: definition_location,
                                    origin,
                                },
                            ..
                        },
                    location,
                    ..
                },
            ..
        } => Some(Referenced::LocalVariable {
            definition_location: *definition_location,
            location: *location,
            origin: Some(origin.clone()),
        }),
        Located::Pattern(Pattern::Variable {
            location, origin, ..
        }) => Some(Referenced::LocalVariable {
            definition_location: *location,
            location: *location,
            origin: Some(origin.clone()),
        }),
        Located::Pattern(Pattern::VarUsage {
            constructor,
            location,
            ..
        }) => constructor
            .as_ref()
            .and_then(|constructor| match &constructor.variant {
                ValueConstructorVariant::LocalVariable {
                    location: definition_location,
                    origin,
                } => Some(Referenced::LocalVariable {
                    definition_location: *definition_location,
                    location: *location,
                    origin: Some(origin.clone()),
                }),
                _ => None,
            }),
        Located::Pattern(Pattern::Assign { location, .. }) => Some(Referenced::LocalVariable {
            definition_location: *location,
            location: *location,
            origin: None,
        }),
        Located::Arg(arg) => match &arg.names {
            ArgNames::Named { location, .. }
            | ArgNames::NamedLabelled {
                name_location: location,
                ..
            } => Some(Referenced::LocalVariable {
                definition_location: *location,
                location: *location,
                origin: None,
            }),
            ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => None,
        },
        Located::Expression {
            expression:
                TypedExpr::Var {
                    constructor:
                        ValueConstructor {
                            variant:
                                ValueConstructorVariant::ModuleConstant { module, .. }
                                | ValueConstructorVariant::ModuleFn { module, .. },
                            ..
                        },
                    name,
                    location,
                    ..
                },
            ..
        } => Some(Referenced::ModuleValue {
            module: module.clone(),
            name: name.clone(),
            location: *location,
            name_kind: Named::Function,
            target_kind: RenameTarget::Unqualified,
        }),

        Located::Expression {
            expression:
                TypedExpr::ModuleSelect {
                    module_name,
                    label,
                    constructor:
                        ModuleValueConstructor::Fn { .. } | ModuleValueConstructor::Constant { .. },
                    location,
                    field_start,
                    ..
                },
            ..
        } => Some(Referenced::ModuleValue {
            module: module_name.clone(),
            name: label.clone(),

            location: SrcSpan::new(*field_start, location.end),
            name_kind: Named::Function,
            target_kind: RenameTarget::Qualified,
        }),
        Located::ModuleStatement(
            Definition::Function(Function {
                name: Some((location, name)),
                ..
            })
            | Definition::ModuleConstant(ModuleConstant {
                name,
                name_location: location,
                ..
            }),
        ) => Some(Referenced::ModuleValue {
            module: current_module.clone(),
            name: name.clone(),
            location: *location,
            name_kind: Named::Function,
            target_kind: RenameTarget::Definition,
        }),
        Located::Expression {
            expression:
                TypedExpr::Var {
                    constructor:
                        ValueConstructor {
                            variant: ValueConstructorVariant::Record { module, name, .. },
                            ..
                        },
                    location,
                    ..
                },
            ..
        } => Some(Referenced::ModuleValue {
            module: module.clone(),
            name: name.clone(),
            location: *location,
            name_kind: Named::CustomTypeVariant,
            target_kind: RenameTarget::Unqualified,
        }),
        Located::Expression {
            expression:
                TypedExpr::ModuleSelect {
                    module_name,
                    label,
                    constructor: ModuleValueConstructor::Record { .. },
                    location,
                    field_start,
                    ..
                },
            ..
        } => Some(Referenced::ModuleValue {
            module: module_name.clone(),
            name: label.clone(),
            location: SrcSpan::new(*field_start, location.end),
            name_kind: Named::CustomTypeVariant,
            target_kind: RenameTarget::Qualified,
        }),
        Located::VariantConstructorDefinition(RecordConstructor {
            name,
            name_location,
            ..
        }) => Some(Referenced::ModuleValue {
            module: current_module.clone(),
            name: name.clone(),
            location: *name_location,
            name_kind: Named::CustomTypeVariant,
            target_kind: RenameTarget::Definition,
        }),
        Located::Pattern(Pattern::Constructor {
            constructor: analyse::Inferred::Known(constructor),
            module: module_select,
            name_location: location,
            ..
        }) => Some(Referenced::ModuleValue {
            module: constructor.module.clone(),
            name: constructor.name.clone(),
            location: *location,
            name_kind: Named::CustomTypeVariant,
            target_kind: if module_select.is_some() {
                RenameTarget::Qualified
            } else {
                RenameTarget::Unqualified
            },
        }),
        Located::Annotation { ast, type_ } => match type_.named_type_name() {
            Some((module, name)) => {
                let (target_kind, location) = match ast {
                    TypeAst::Constructor(constructor) => {
                        let kind = if constructor.module.is_some() {
                            RenameTarget::Qualified
                        } else {
                            RenameTarget::Unqualified
                        };
                        (kind, constructor.name_location)
                    }
                    TypeAst::Fn(_) | TypeAst::Var(_) | TypeAst::Tuple(_) | TypeAst::Hole(_) => {
                        (RenameTarget::Unqualified, ast.location())
                    }
                };
                Some(Referenced::ModuleType {
                    module,
                    name,
                    location,
                    target_kind,
                })
            }
            None => None,
        },
        Located::ModuleStatement(Definition::CustomType(CustomType {
            name,
            name_location,
            ..
        })) => Some(Referenced::ModuleType {
            module: current_module.clone(),
            name: name.clone(),
            location: *name_location,
            target_kind: RenameTarget::Definition,
        }),
        Located::ModuleName { location, name, .. } => Some(Referenced::ModuleName {
            name: name.clone(),
            location,
        }),
        Located::ModuleStatement(Definition::Import(import @ Import { .. })) => {
            Some(Referenced::ModuleName {
                name: import.module.0.clone(),
                location: get_module_name_span(import),
            })
        }
        _ => None,
    }
}

pub fn find_module_references(
    module_name: EcoString,
    name: EcoString,
    modules: &im::HashMap<EcoString, ModuleInterface>,
    sources: &HashMap<EcoString, ModuleSourceInformation>,
    layer: ast::Layer,
) -> Vec<Location> {
    let mut reference_locations = Vec::new();

    for module in modules.values() {
        if module.name == module_name || module.references.imported_modules.contains(&module_name) {
            let Some(source_information) = sources.get(&module.name) else {
                continue;
            };

            find_references_in_module(
                &module_name,
                &name,
                module,
                source_information,
                &mut reference_locations,
                layer,
            );
        }
    }

    reference_locations
}

fn find_references_in_module(
    module_name: &EcoString,
    name: &EcoString,
    module: &ModuleInterface,
    source_information: &ModuleSourceInformation,
    reference_locations: &mut Vec<Location>,
    layer: ast::Layer,
) {
    let reference_map = match layer {
        ast::Layer::Value => &module.references.value_references,
        ast::Layer::Type => &module.references.type_references,
    };

    let Some(references) = reference_map.get(&(module_name.clone(), name.clone())) else {
        return;
    };

    let Some(uri) = url_from_path(source_information.path.as_str()) else {
        return;
    };

    for reference in references {
        reference_locations.push(Location {
            uri: uri.clone(),
            range: src_span_to_lsp_range(reference.location, &source_information.line_numbers),
        });
    }
}

#[derive(Debug, Clone, Copy)]
pub struct VariableReference {
    pub location: SrcSpan,
    pub kind: VariableReferenceKind,
}

#[derive(Debug, Clone, Copy)]
pub enum VariableReferenceKind {
    Variable,
    LabelShorthand,
}

pub fn find_variable_references(
    module: &TypedModule,
    definition_location: SrcSpan,
) -> Vec<VariableReference> {
    let mut finder = FindVariableReferences {
        references: Vec::new(),
        definition_location,
    };
    finder.visit_typed_module(module);
    finder.references
}

struct FindVariableReferences {
    references: Vec<VariableReference>,
    definition_location: SrcSpan,
}

impl<'ast> Visit<'ast> for FindVariableReferences {
    fn visit_typed_function(&mut self, fun: &'ast TypedFunction) {
        if fun.full_location().contains(self.definition_location.start) {
            ast::visit::visit_typed_function(self, fun);
        }
    }

    fn visit_typed_expr_var(
        &mut self,
        location: &'ast SrcSpan,
        constructor: &'ast ValueConstructor,
        _name: &'ast EcoString,
    ) {
        match constructor.variant {
            ValueConstructorVariant::LocalVariable {
                location: definition_location,
                ..
            } if definition_location == self.definition_location => {
                self.references.push(VariableReference {
                    location: *location,
                    kind: VariableReferenceKind::Variable,
                })
            }
            _ => {}
        }
    }

    fn visit_typed_clause_guard_var(
        &mut self,
        location: &'ast SrcSpan,
        _name: &'ast EcoString,
        _type_: &'ast std::sync::Arc<Type>,
        definition_location: &'ast SrcSpan,
    ) {
        if *definition_location == self.definition_location {
            self.references.push(VariableReference {
                location: *location,
                kind: VariableReferenceKind::Variable,
            })
        }
    }

    fn visit_typed_pattern_var_usage(
        &mut self,
        location: &'ast SrcSpan,
        _name: &'ast EcoString,
        constructor: &'ast Option<ValueConstructor>,
        _type_: &'ast std::sync::Arc<Type>,
    ) {
        let variant = match constructor {
            Some(constructor) => &constructor.variant,
            None => return,
        };
        match variant {
            ValueConstructorVariant::LocalVariable {
                location: definition_location,
                ..
            } if *definition_location == self.definition_location => {
                self.references.push(VariableReference {
                    location: *location,
                    kind: VariableReferenceKind::Variable,
                })
            }
            _ => {}
        }
    }

    fn visit_typed_call_arg(&mut self, arg: &'ast crate::type_::TypedCallArg) {
        if let TypedExpr::Var {
            location,
            constructor,
            ..
        } = &arg.value
        {
            match &constructor.variant {
                ValueConstructorVariant::LocalVariable {
                    location: definition_location,
                    ..
                } if arg.uses_label_shorthand()
                    && *definition_location == self.definition_location =>
                {
                    self.references.push(VariableReference {
                        location: *location,
                        kind: VariableReferenceKind::LabelShorthand,
                    });
                    return;
                }
                _ => {}
            }
        }

        ast::visit::visit_typed_call_arg(self, arg);
    }
}

fn get_module_name_span<T>(import: &Import<T>) -> SrcSpan {
    match &import.as_name {
        Some((AssignName::Variable(name), location)) => SrcSpan::new(location.end-(name.len() as u32), location.end),

        Some((AssignName::Discard(_), location)) => SrcSpan::new(location.end-1, location.end),
        None => {
            let used_name = import.module.0.split('/').next_back();
            let len = used_name.map(|str| str.len()).unwrap_or(0) as u32;
            SrcSpan::new(import.module.1.end-len, import.module.1.end)
        }
    }
}

pub fn find_module_name_references(
    module: &TypedModule,
    module_name: &EcoString,
) -> Vec<ModuleNameReference> {
    let mut finder = FindModuleNameReferences {
        references: Vec::new(),
        module_name: module_name.clone(),
    };
    finder.visit_typed_module(module);
    finder.references
}

pub struct ModuleNameReference {
    pub location: SrcSpan,
    pub kind: ModuleNameReferenceKind,
}

pub enum ModuleNameReferenceKind {
    // Import of module without an alias
    // location is entire import statement
    //
    // import lustre/attribute.{type ...}
    // __________________________________
    Import,

    // Import of module with alias
    // location is everything starting from " as" until the alias
    //
    // import lustre/attribute   as attr
    //                          ________
    AliasedImport,

    // Use of module
    // location is module name/alias until '.'
    //
    // attribute.action("...")
    // _________
    Name,
}

struct FindModuleNameReferences {
    references: Vec<ModuleNameReference>,
    module_name: EcoString,
}

impl<'ast> Visit<'ast> for FindModuleNameReferences {
    fn visit_typed_import(&mut self, import: &'ast ast::TypedImport) {
        if import.module.0 == self.module_name {
            match &import.as_name {
                Some(alias) => self.references.push(ModuleNameReference {
                    location: SrcSpan::new(alias.1.start - 1, alias.1.end),
                    kind: ModuleNameReferenceKind::AliasedImport,
                }),
                None => self.references.push(ModuleNameReference {
                    location: import.location,
                    kind: ModuleNameReferenceKind::Import,
                }),
            }
        }

        ast::visit::visit_typed_import(self, import);
    }

    fn visit_typed_custom_type(&mut self, custom_type: &'ast ast::TypedCustomType) {
        for constructor in &custom_type.constructors {
            for arg in &constructor.arguments {
                self.visit_type_ast(&arg.ast, &arg.type_);
            }
        }

        ast::visit::visit_typed_custom_type(self, custom_type);
    }

    fn visit_typed_type_alias(&mut self, type_alias: &'ast ast::TypedTypeAlias) {
        self.visit_type_ast(&type_alias.type_ast, &type_alias.type_);

        ast::visit::visit_typed_type_alias(self, type_alias);
    }

    fn visit_typed_function(&mut self, fun: &'ast TypedFunction) {
        for arg in &fun.arguments {
            if let Some(annotation) = &arg.annotation {
                self.visit_type_ast(annotation, &arg.type_);
            }
        }

        ast::visit::visit_typed_function(self, fun);
    }

    fn visit_typed_clause_guard_module_select(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast std::sync::Arc<Type>,
        label: &'ast EcoString,
        module_name: &'ast EcoString,
        module_alias: &'ast EcoString,
        literal: &'ast TypedConstant,
    ) {
        if *module_name == self.module_name {
            self.references.push(ModuleNameReference {
                location: *location,
                kind: ModuleNameReferenceKind::Name,
            });
        }

        ast::visit::visit_typed_clause_guard_module_select(
            self,
            location,
            type_,
            label,
            module_name,
            module_alias,
            literal,
        );
    }

    fn visit_typed_expr_module_select(
        &mut self,
        location: &'ast SrcSpan,
        field_start: &'ast u32,
        type_: &'ast std::sync::Arc<Type>,
        label: &'ast EcoString,
        module_name: &'ast EcoString,
        module_alias: &'ast EcoString,
        constructor: &'ast ModuleValueConstructor,
    ) {
        if *module_name == self.module_name {
            self.references.push(ModuleNameReference {
                location: SrcSpan::new(location.start, field_start - 1),
                kind: ModuleNameReferenceKind::Name,
            });
        }

        ast::visit::visit_typed_expr_module_select(
            self,
            location,
            field_start,
            type_,
            label,
            module_name,
            module_alias,
            constructor,
        );
    }

    fn visit_typed_module_constant(&mut self, constant: &'ast ast::TypedModuleConstant) {
        if let Some(annotation) = &constant.annotation {
            self.visit_type_ast(annotation, &constant.type_);
        }

        ast::visit::visit_typed_module_constant(self, constant);
    }

    fn visit_typed_constant(&mut self, constant: &'ast TypedConstant) {
        match constant {
            Constant::Record {
                module,
                record_constructor: con,
                ..
            }
            | Constant::Var {
                module,
                constructor: con,
                ..
            } => {
                if let Some((_, location)) = module {
                    if let Some(con) = con {
                        match &con.variant {
                            ValueConstructorVariant::ModuleConstant { module, .. }
                            | ValueConstructorVariant::ModuleFn { module, .. }
                            | ValueConstructorVariant::Record { module, .. } => {
                                if *module == self.module_name {
                                    self.references.push(ModuleNameReference {
                                        location: *location,
                                        kind: ModuleNameReferenceKind::Name,
                                    });
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            _ => {}
        }

        ast::visit::visit_typed_constant(self, constant);
    }

    fn visit_typed_pattern_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name_location: &'ast SrcSpan,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<TypedPattern>>,
        module: &'ast Option<(EcoString, SrcSpan)>,
        constructor: &'ast analyse::Inferred<PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast std::sync::Arc<Type>,
    ) {
        if let Some((_, module_location)) = module {
            if let Some((referenced_module, ..)) = type_.named_type_name() {
                if referenced_module == self.module_name {
                    self.references.push(ModuleNameReference {
                        location: *module_location,
                        kind: ModuleNameReferenceKind::Name,
                    });
                }
            }
        }

        ast::visit::visit_typed_pattern_constructor(
            self,
            location,
            name_location,
            name,
            arguments,
            module,
            constructor,
            spread,
            type_,
        );
    }

    fn visit_typed_assignment(&mut self, assignment: &'ast ast::TypedAssignment) {
        if let Some(annotation) = &assignment.annotation {
            self.visit_type_ast(annotation, &assignment.type_());
        }

        ast::visit::visit_typed_assignment(self, assignment);
    }

    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast std::sync::Arc<Type>,
        kind: &'ast ast::FunctionLiteralKind,
        args: &'ast [ast::TypedArg],
        body: &'ast vec1::Vec1<ast::TypedStatement>,
        return_annotation: &'ast Option<TypeAst>,
    ) {
        for arg in args {
            if let Some(annotation) = &arg.annotation {
                self.visit_type_ast(annotation, &arg.type_);
            }
        }
        if let Some(ret) = return_annotation {
            if let Type::Fn { return_, .. } = type_.as_ref() {
                self.visit_type_ast(ret, &return_);
            }
        }

        ast::visit::visit_typed_expr_fn(
            self,
            location,
            type_,
            kind,
            args,
            body,
            return_annotation
        );
    }
}

impl FindModuleNameReferences {
    fn visit_type_ast(&mut self, type_ast: &TypeAst, type_: &Type) {
        match type_ast {
            TypeAst::Constructor(TypeAstConstructor { module, arguments, .. }) => {
                match type_ {
                    Type::Named { module: module_name, args, .. } => {
                        if let Some((_, module_location)) = module {
                            if *module_name == self.module_name {
                                self.references.push(ModuleNameReference {
                                    location: *module_location,
                                    kind: ModuleNameReferenceKind::Name
                                });
                            }
                        }
                        arguments
                            .iter()
                            .zip(args.iter())
                            .for_each(|(arg, type_)| self.visit_type_ast(arg, type_));
                    },
                    // Lists store their arguments' type as a var link
                    Type::Var { type_: var_type } => {
                        if let TypeVar::Link { type_ } = var_type.borrow().clone() {
                            self.visit_type_ast(type_ast, &type_);
                        }
                    },
                    _ => {},
                }
            },
            TypeAst::Tuple(TypeAstTuple { elements, .. }) => {
                if let Type::Tuple { elements: elements_type, } = type_ {
                    elements
                        .iter()
                        .zip(elements_type.iter())
                        .for_each(|(e, type_)| self.visit_type_ast(e, type_));
                }
            }
            TypeAst::Fn(TypeAstFn { arguments, return_, .. }) => {
                if let Type::Fn { args, return_: ret_type, } = type_ {
                    arguments
                        .iter()
                        .zip(args.iter())
                        .for_each(|(arg, type_)| self.visit_type_ast(arg, type_));
                    self.visit_type_ast(return_, ret_type);
                }
            }
            _ => {},
        }
    }
}
