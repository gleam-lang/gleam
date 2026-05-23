// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2019 The Gleam contributors

use std::collections::{HashMap, HashSet};

use ecow::EcoString;
use lsp_types::Location;

use gleam_core::{
    analyse,
    ast::{
        self, ArgNames, AssignName, BitArraySize, ClauseGuard, CustomType, Function,
        ModuleConstant, Pattern, RecordConstructor, SrcSpan, TypedExpr, TypedModule, visit::Visit,
    },
    build::{LabelContainer, Located},
    type_::{
        ModuleInterface, ModuleValueConstructor, Type, ValueConstructor, ValueConstructorVariant,
        error::{Named, VariableOrigin},
    },
};

use super::{
    compiler::ModuleSourceInformation, rename::RenameTarget, src_span_to_lsp_range, url_from_path,
};

#[derive(Debug)]
pub enum Referenced {
    LocalVariable {
        definition_location: SrcSpan,
        location: SrcSpan,
        origin: Option<VariableOrigin>,
        name: EcoString,
    },
    ModuleName {
        module_name: EcoString,
        module_alias: EcoString,
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
    TypeVariable {
        location: SrcSpan,
        name: EcoString,
    },
    Label {
        type_module: EcoString,
        type_name: EcoString,
        label: EcoString,
        location: SrcSpan,
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
                    name,
                },
            ..
        } => Some(Referenced::LocalVariable {
            definition_location: *definition_location,
            location: *location,
            origin: Some(origin.clone()),
            name: name.clone(),
        }),
        Located::Pattern(Pattern::Variable {
            location,
            origin,
            name,
            ..
        }) => Some(Referenced::LocalVariable {
            definition_location: *location,
            location: *location,
            origin: Some(origin.clone()),
            name: name.clone(),
        }),
        Located::Pattern(Pattern::BitArraySize(BitArraySize::Variable {
            constructor,
            location,
            name,
            ..
        })) => constructor
            .as_ref()
            .and_then(|constructor| match &constructor.variant {
                ValueConstructorVariant::LocalVariable {
                    location: definition_location,
                    origin,
                } => Some(Referenced::LocalVariable {
                    definition_location: *definition_location,
                    location: *location,
                    origin: Some(origin.clone()),
                    name: name.clone(),
                }),
                ValueConstructorVariant::ModuleConstant { .. }
                | ValueConstructorVariant::ModuleFn { .. }
                | ValueConstructorVariant::Record { .. } => None,
            }),
        Located::Pattern(Pattern::Assign { location, name, .. }) => {
            Some(Referenced::LocalVariable {
                definition_location: *location,
                location: *location,
                origin: None,
                name: name.clone(),
            })
        }
        Located::Arg(arg) => match &arg.names {
            ArgNames::Named { location, name }
            | ArgNames::NamedLabelled {
                name_location: location,
                name,
                ..
            } => Some(Referenced::LocalVariable {
                definition_location: *location,
                location: *location,
                origin: None,
                name: name.clone(),
            }),
            ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => None,
        },
        Located::Expression {
            expression:
                TypedExpr::Var {
                    constructor:
                        ValueConstructor {
                            variant:
                                ValueConstructorVariant::ModuleConstant { module, name, .. }
                                | ValueConstructorVariant::ModuleFn { module, name, .. },
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

        Located::ModuleFunction(Function {
            name: Some((location, name)),
            ..
        })
        | Located::ModuleConstant(ModuleConstant {
            name,
            name_location: location,
            ..
        }) => Some(Referenced::ModuleValue {
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
        Located::StringPrefixPatternVariable { location, name, .. } => {
            Some(Referenced::LocalVariable {
                definition_location: location,
                location,
                origin: None,
                name: name.clone(),
            })
        }
        Located::Annotation { ast, type_ } => match ast {
            ast::TypeAst::Constructor(constructor)
                if let Some((module, name)) = type_.named_type_name() =>
            {
                let target_kind = if constructor.name.is_qualified() {
                    RenameTarget::Qualified
                } else {
                    RenameTarget::Unqualified
                };
                let location = constructor.name.name_location()?;

                Some(Referenced::ModuleType {
                    module,
                    name,
                    location,
                    target_kind,
                })
            }

            ast::TypeAst::Var(variable) => Some(Referenced::TypeVariable {
                location: variable.location,
                name: variable.name.clone(),
            }),

            ast::TypeAst::Constructor(_)
            | ast::TypeAst::Fn(_)
            | ast::TypeAst::Tuple(_)
            | ast::TypeAst::Hole(_) => None,
        },
        Located::TypeVariable { name, location } => {
            Some(Referenced::TypeVariable { location, name })
        }
        Located::ModuleCustomType(CustomType {
            name,
            name_location,
            ..
        }) => Some(Referenced::ModuleType {
            module: current_module.clone(),
            name: name.clone(),
            location: *name_location,
            target_kind: RenameTarget::Definition,
        }),
        Located::ModuleName {
            location,
            module_name,
            module_alias,
            ..
        } => Some(Referenced::ModuleName {
            module_name,
            module_alias,
            location,
        }),
        Located::ModuleImport(import) => {
            let module_name = match &import.as_name {
                Some((
                    AssignName::Variable(module_alias) | AssignName::Discard(module_alias),
                    alias_location,
                )) => Referenced::ModuleName {
                    module_name: import.module.clone(),
                    module_alias: module_alias.clone(),
                    location: SrcSpan {
                        start: alias_location.end - (module_alias.len() as u32),
                        end: alias_location.end,
                    },
                },
                None => Referenced::ModuleName {
                    module_name: import.module.clone(),
                    module_alias: import
                        .module
                        .split('/')
                        .next_back()
                        .map(EcoString::from)
                        .unwrap_or_else(|| import.module.clone()),
                    location: import.module_location,
                },
            };

            Some(module_name)
        }

        Located::ClauseGuard(ClauseGuard::Var {
            location,
            type_: _,
            name,
            definition_location,
            origin,
        }) => Some(Referenced::LocalVariable {
            definition_location: *definition_location,
            location: *location,
            origin: Some(origin.clone()),
            name: name.clone(),
        }),

        Located::ClauseGuard(ClauseGuard::ModuleSelect {
            location,
            field_start,
            label,
            module_name,
            ..
        }) => Some(Referenced::ModuleValue {
            module: module_name.clone(),
            name: label.clone(),
            location: SrcSpan::new(*field_start, location.end),
            name_kind: Named::Function,
            target_kind: RenameTarget::Qualified,
        }),

        Located::Expression {
            expression:
                TypedExpr::RecordAccess {
                    record,
                    label,
                    location,
                    ..
                },
            ..
        } => record
            .type_()
            .named_type_name()
            .map(|(type_module, type_name)| Referenced::Label {
                type_module,
                type_name,
                label: label.clone(),
                // `field_start` is the start of the whole `record.field`
                // expression, not the field. The field label is the trailing
                // part of the access, so we recover its span from the end.
                location: SrcSpan::new(location.end - label.len() as u32, location.end),
            }),

        Located::Label {
            container: Some(LabelContainer::Usage { type_, .. }),
            label,
            location,
            ..
        } => type_
            .named_type_name()
            .map(|(type_module, type_name)| Referenced::Label {
                type_module,
                type_name,
                label: label.clone(),
                location,
            }),

        // A label at its declaration in a custom type, which lives in the
        // current module.
        Located::Label {
            container: Some(LabelContainer::Definition { type_name, .. }),
            label,
            location,
            ..
        } => Some(Referenced::Label {
            type_module: current_module.clone(),
            type_name,
            label: label.clone(),
            location,
        }),

        Located::Pattern(_)
        | Located::ClauseGuard(_)
        | Located::PatternSpread { .. }
        | Located::Statement(_)
        | Located::Expression { .. }
        | Located::FunctionBody(_)
        | Located::UnqualifiedImport(_)
        | Located::Label { .. }
        | Located::Constant(_)
        | Located::ModuleFunction(_)
        | Located::ModuleTypeAlias(_) => None,
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

pub fn find_module_references_in_module(
    module_name: EcoString,
    name: EcoString,
    module: &ModuleInterface,
    source_information: &ModuleSourceInformation,
    layer: ast::Layer,
) -> Vec<Location> {
    let mut reference_locations = Vec::new();

    find_references_in_module(
        &module_name,
        &name,
        module,
        source_information,
        &mut reference_locations,
        layer,
    );

    reference_locations
}

pub fn find_label_references(
    type_module: EcoString,
    type_name: EcoString,
    label: EcoString,
    modules: &im::HashMap<EcoString, ModuleInterface>,
    sources: &HashMap<EcoString, ModuleSourceInformation>,
) -> Vec<Location> {
    let mut reference_locations = Vec::new();
    let key = (type_module.clone(), type_name, label);

    for module in modules.values() {
        if module.name == type_module || module.references.imported_modules.contains(&type_module) {
            let Some(source_information) = sources.get(&module.name) else {
                continue;
            };
            find_label_references_in_module(
                &key,
                module,
                source_information,
                &mut reference_locations,
            );
        }
    }

    reference_locations
}

pub fn find_label_references_in_current_module(
    type_module: EcoString,
    type_name: EcoString,
    label: EcoString,
    module: &ModuleInterface,
    source_information: &ModuleSourceInformation,
) -> Vec<Location> {
    let mut reference_locations = Vec::new();
    let key = (type_module, type_name, label);
    find_label_references_in_module(&key, module, source_information, &mut reference_locations);
    reference_locations
}

fn find_label_references_in_module(
    key: &(EcoString, EcoString, EcoString),
    module: &ModuleInterface,
    source_information: &ModuleSourceInformation,
    reference_locations: &mut Vec<Location>,
) {
    let Some(uri) = url_from_path(source_information.path.as_str()) else {
        return;
    };

    let Some(references) = module.references.label_references.get(key) else {
        return;
    };

    for reference in references {
        reference_locations.push(Location {
            uri: uri.clone(),
            range: src_span_to_lsp_range(reference.location, &source_information.line_numbers),
        });
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableReference {
    pub location: SrcSpan,
    pub kind: VariableReferenceKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VariableReferenceKind {
    Variable,
    LabelShorthand,
}

/// How to treat variables defined in alternative patterns
enum AlternativeVariable {
    Track,
    Ignore,
}

pub struct FindVariableReferences {
    // Due to the structure of some AST nodes (for example, record updates),
    // when we traverse the AST it is possible to accidentally duplicate references.
    // To avoid this, we use a `HashSet` instead of a `Vec` here.
    // See: https://github.com/gleam-lang/gleam/issues/4859 and the linked PR.
    references: HashSet<VariableReference>,
    definition_location: DefinitionLocation,
    alternative_variable: AlternativeVariable,
    name: EcoString,
}

/// Where the variable we're finding references for is defined.
///
enum DefinitionLocation {
    /// This is the location where the variable is defined, nothing special is
    /// going on here. For example:
    ///
    /// ```gleam
    ///    let wibble = 1
    /// //   ^^^^^^ Definition location for `wibble`
    ///    wibble + 1
    ///    ^^^^^^
    /// // `wibble` used here, defined earlier
    /// ```
    ///
    Regular { location: SrcSpan },

    /// When dealing with alternative patterns and aliases we need special care:
    /// each usage wil always reference the first alternative where a variable
    /// is defined and not the following ones. For example:
    ///
    /// ```gleam
    /// case wibble {
    ///   [] as var | [_] as var -> var
    ///   //    ^^^                 ^^^ If we look where `var` thinks it's defined
    ///   //    It will say it's defined here!
    /// }
    /// ```
    ///
    /// This poses a problem if we start the renaming from the second
    /// alternative pattern:
    ///
    /// ```gleam
    /// case wibble {
    ///   [] as var | [_] as var -> var
    ///   //                 ^^^ Since `var` uses the first alternative as its
    ///   //                     definition location, this would not be considered
    ///   //                     a reference to that same var.
    /// }
    /// ```
    ///
    /// So we keep track of the location of this definition, but we also need
    /// to store the location of the first definition in the alternative case
    /// (that's `first_alternative_location`), so that when we look for
    /// references we can check against this one that is canonically used by
    /// expressions in the AST
    ///
    Alternative {
        location: SrcSpan,
        first_alternative_location: SrcSpan,
    },
}

impl FindVariableReferences {
    pub fn new(variable_definition_location: SrcSpan, variable_name: EcoString) -> Self {
        Self {
            references: HashSet::new(),
            definition_location: DefinitionLocation::Regular {
                location: variable_definition_location,
            },
            alternative_variable: AlternativeVariable::Ignore,
            name: variable_name,
        }
    }

    /// Where the definition for which we're accumulating references is
    /// originally defined. In case of alternative patterns this will point to
    /// the first occurrence of that name! Look at the docs for
    /// `DefinitionLocation` to learn more on why this is needed.
    ///
    fn definition_origin_location(&self) -> SrcSpan {
        match self.definition_location {
            DefinitionLocation::Regular { location }
            | DefinitionLocation::Alternative {
                first_alternative_location: location,
                ..
            } => location,
        }
    }

    /// This is the location of the definition for which we're accumulating
    /// references. In most cases you'll want to use `definition_origin_location`.
    /// The difference between the two is explained in greater detail in the docs
    /// for `DefinitionLocation`.
    ///
    fn definition_location(&self) -> SrcSpan {
        match self.definition_location {
            DefinitionLocation::Regular { location }
            | DefinitionLocation::Alternative { location, .. } => location,
        }
    }

    fn update_alternative_origin(&mut self, alternative_location: SrcSpan) {
        match self.definition_location {
            // We've found the location of the origin of an alternative pattern.
            DefinitionLocation::Regular { location } if alternative_location < location => {
                self.definition_location = DefinitionLocation::Alternative {
                    location,
                    first_alternative_location: alternative_location,
                };
            }

            // Since the new alternative location we've found is smaller, that
            // is the actual first one for the alternative pattern!
            DefinitionLocation::Alternative {
                location,
                first_alternative_location,
            } if alternative_location < first_alternative_location => {
                self.definition_location = DefinitionLocation::Alternative {
                    location,
                    first_alternative_location: alternative_location,
                };
            }

            DefinitionLocation::Regular { .. } | DefinitionLocation::Alternative { .. } => (),
        };
    }

    pub fn find_in_module(mut self, module: &TypedModule) -> HashSet<VariableReference> {
        self.visit_typed_module(module);
        self.references
    }

    pub fn find(mut self, expression: &TypedExpr) -> HashSet<VariableReference> {
        self.visit_typed_expr(expression);
        self.references
    }

    fn register_alternative_definition(&mut self, name: &EcoString, location: &SrcSpan) {
        match self.alternative_variable {
            // If we are inside the same alternative pattern as the target
            // variable and the name is the same, this is an alternative definition
            // of the same variable. We don't register the reference if this is
            // the exact variable though, as that would result in a duplicated
            // reference.
            AlternativeVariable::Track
                if *name == self.name && *location != self.definition_location() =>
            {
                self.update_alternative_origin(*location);

                _ = self.references.insert(VariableReference {
                    location: *location,
                    kind: VariableReferenceKind::Variable,
                });
            }
            AlternativeVariable::Track | AlternativeVariable::Ignore => {}
        }
    }
}

impl<'ast> Visit<'ast> for FindVariableReferences {
    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        if fun
            .full_location()
            .contains(self.definition_origin_location().start)
        {
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
            } if definition_location == self.definition_origin_location() => {
                _ = self.references.insert(VariableReference {
                    location: *location,
                    kind: VariableReferenceKind::Variable,
                });
            }
            ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::ModuleFn { .. }
            | ValueConstructorVariant::Record { .. } => {}
        }
    }

    fn visit_typed_clause_guard_var(
        &mut self,
        location: &'ast SrcSpan,
        _name: &'ast EcoString,
        _type_: &'ast std::sync::Arc<Type>,
        definition_location: &'ast SrcSpan,
        _origin: &'ast VariableOrigin,
    ) {
        if *definition_location == self.definition_origin_location() {
            _ = self.references.insert(VariableReference {
                location: *location,
                kind: VariableReferenceKind::Variable,
            });
        }
    }

    fn visit_typed_clause(&mut self, clause: &'ast ast::TypedClause) {
        // If this alternative pattern contains the variable we are finding
        // references for, we track that so we can find alternative definitions
        // of the target variable.
        if clause
            .pattern_location()
            .contains(self.definition_origin_location().start)
        {
            self.alternative_variable = AlternativeVariable::Track;
        }

        for pattern in clause.pattern.iter() {
            self.visit_typed_pattern(pattern);
        }
        for patterns in clause.alternative_patterns.iter() {
            for pattern in patterns {
                self.visit_typed_pattern(pattern);
            }
        }

        self.alternative_variable = AlternativeVariable::Ignore;

        if let Some(guard) = &clause.guard {
            self.visit_typed_clause_guard(guard);
        }
        self.visit_typed_expr(&clause.then);
    }

    fn visit_typed_pattern_variable(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        _type_: &'ast std::sync::Arc<Type>,
        _origin: &'ast VariableOrigin,
    ) {
        self.register_alternative_definition(name, location);
    }

    fn visit_typed_pattern_assign(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        pattern: &'ast ast::TypedPattern,
    ) {
        self.register_alternative_definition(name, location);

        ast::visit::visit_typed_pattern_assign(self, location, name, pattern);
    }

    fn visit_typed_bit_array_size_variable(
        &mut self,
        location: &'ast SrcSpan,
        _name: &'ast EcoString,
        constructor: &'ast Option<Box<ValueConstructor>>,
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
            } if *definition_location == self.definition_origin_location() => {
                _ = self.references.insert(VariableReference {
                    location: *location,
                    kind: VariableReferenceKind::Variable,
                });
            }
            ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::ModuleFn { .. }
            | ValueConstructorVariant::Record { .. } => {}
        }
    }

    fn visit_typed_call_arg(&mut self, arg: &'ast gleam_core::type_::TypedCallArg) {
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
                    && *definition_location == self.definition_origin_location() =>
                {
                    _ = self.references.insert(VariableReference {
                        location: *location,
                        kind: VariableReferenceKind::LabelShorthand,
                    });
                    return;
                }
                ValueConstructorVariant::LocalVariable { .. }
                | ValueConstructorVariant::ModuleConstant { .. }
                | ValueConstructorVariant::ModuleFn { .. }
                | ValueConstructorVariant::Record { .. } => {}
            }
        }

        ast::visit::visit_typed_call_arg(self, arg);
    }

    fn visit_typed_pattern_string_prefix(
        &mut self,
        location: &'ast SrcSpan,
        left_location: &'ast SrcSpan,
        left_side_assignment: &'ast Option<(EcoString, SrcSpan)>,
        right_location: &'ast SrcSpan,
        left_side_string: &'ast EcoString,
        right_side_assignment: &'ast AssignName,
    ) {
        // Handle the prefix alias in alternative pattern: "prefix" as name | "other_prefix" as name
        if let Some((name, left_side_assignment_location)) = left_side_assignment {
            self.register_alternative_definition(name, left_side_assignment_location);
        }

        // Handle the suffix in alternative pattern: "prefix" <> name | "other_prefix" <> name
        match right_side_assignment {
            AssignName::Variable(name) => {
                self.register_alternative_definition(name, right_location)
            }
            AssignName::Discard(_) => {}
        }

        ast::visit::visit_typed_pattern_string_prefix(
            self,
            location,
            left_location,
            left_side_assignment,
            right_location,
            left_side_string,
            right_side_assignment,
        );
    }
}

pub struct FindTypeVariableReferences<'a> {
    pub references: Vec<SrcSpan>,
    pub name: &'a EcoString,
    pub location: SrcSpan,
}

impl<'a> FindTypeVariableReferences<'a> {
    pub fn find_in_module(
        module: &TypedModule,
        type_variable_location: SrcSpan,
        type_variable_name: &'a EcoString,
    ) -> Vec<SrcSpan> {
        let mut finder = Self {
            references: Vec::new(),
            name: type_variable_name,
            location: type_variable_location,
        };
        finder.visit_typed_module(module);
        finder.references
    }
}

impl<'ast> Visit<'ast> for FindTypeVariableReferences<'_> {
    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        if fun.full_location().contains_span(self.location) {
            ast::visit::visit_typed_function(self, fun);
        }
    }

    fn visit_typed_custom_type(&mut self, custom_type: &'ast ast::TypedCustomType) {
        if custom_type.full_location().contains_span(self.location) {
            for (location, name) in custom_type.parameters.iter() {
                if name == self.name {
                    self.references.push(*location)
                }
            }
            ast::visit::visit_typed_custom_type(self, custom_type);
        }
    }

    fn visit_typed_module_constant(&mut self, constant: &'ast ast::TypedModuleConstant) {
        if constant.location.contains_span(self.location) {
            ast::visit::visit_typed_module_constant(self, constant);
        }
    }

    fn visit_typed_type_alias(&mut self, type_alias: &'ast ast::TypedTypeAlias) {
        if type_alias.location.contains_span(self.location) {
            for (location, name) in type_alias.parameters.iter() {
                if name == self.name {
                    self.references.push(*location)
                }
            }
            ast::visit::visit_typed_type_alias(self, type_alias);
        }
    }

    fn visit_type_ast_var(&mut self, location: &'ast SrcSpan, name: &'ast EcoString) {
        if name == self.name {
            self.references.push(*location);
        }
    }
}
