use ecow::EcoString;

use crate::{
    analyse,
    ast::{
        ArgNames, Definition, Function, ModuleConstant, Pattern, RecordConstructor, SrcSpan,
        TypedExpr,
    },
    build::Located,
    type_::{
        ModuleValueConstructor, ValueConstructor, ValueConstructorVariant,
        error::{Named, VariableOrigin},
    },
};

use super::rename::RenameTarget;

pub enum Referenced {
    LocalVariable {
        definition_location: SrcSpan,
        location: SrcSpan,
        origin: Option<VariableOrigin>,
    },
    ModuleValue {
        module: EcoString,
        name: EcoString,
        location: SrcSpan,
        name_kind: Named,
        target_kind: RenameTarget,
    },
}

pub fn reference_for_ast_node(
    found: Located<'_>,
    current_module: &EcoString,
) -> Option<Referenced> {
    match found {
        Located::Expression(TypedExpr::Var {
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
        }) => Some(Referenced::LocalVariable {
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
        Located::Expression(TypedExpr::Var {
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
        }) => Some(Referenced::ModuleValue {
            module: module.clone(),
            name: name.clone(),
            location: *location,
            name_kind: Named::Function,
            target_kind: RenameTarget::Unqualified,
        }),

        Located::Expression(TypedExpr::ModuleSelect {
            module_name,
            label,
            constructor: ModuleValueConstructor::Fn { .. } | ModuleValueConstructor::Constant { .. },
            location,
            ..
        }) => Some(Referenced::ModuleValue {
            module: module_name.clone(),
            name: label.clone(),
            location: *location,
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
        Located::Expression(TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::Record { module, name, .. },
                    ..
                },
            location,
            ..
        }) => Some(Referenced::ModuleValue {
            module: module.clone(),
            name: name.clone(),
            location: *location,
            name_kind: Named::CustomTypeVariant,
            target_kind: RenameTarget::Unqualified,
        }),
        Located::Expression(TypedExpr::ModuleSelect {
            module_name,
            label,
            constructor: ModuleValueConstructor::Record { .. },
            location,
            ..
        }) => Some(Referenced::ModuleValue {
            module: module_name.clone(),
            name: label.clone(),
            location: *location,
            name_kind: Named::CustomTypeVariant,
            target_kind: RenameTarget::Qualified,
        }),
        Located::VariantConstructorDefinition(RecordConstructor { name, location, .. }) => {
            Some(Referenced::ModuleValue {
                module: current_module.clone(),
                name: name.clone(),
                location: *location,
                name_kind: Named::CustomTypeVariant,
                target_kind: RenameTarget::Definition,
            })
        }
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
        _ => None,
    }
}
