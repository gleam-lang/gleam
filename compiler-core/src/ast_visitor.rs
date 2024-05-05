use std::sync::Arc;

use ecow::EcoString;
use vec1::Vec1;

use crate::{
    ast::{
        Arg, Definition, Import, Module, Pattern, SrcSpan, Statement, TypedDefinition, TypedExpr,
    },
    type_::{ModuleInterface, ModuleValueConstructor, Type},
};

#[derive(Default)]
pub struct Visitor<'a> {
    pub owned_visit_import: Option<&'a mut dyn FnMut(&Import<EcoString>)>,
    pub owned_visit_pattern_of_var: Option<&'a mut dyn FnMut(&SrcSpan, &EcoString, &Type)>,
    pub owned_visit_record_access:
        Option<&'a mut dyn FnMut(&SrcSpan, &Type, &EcoString, u64, &TypedExpr)>,
    pub owned_visit_module_select: Option<
        &'a mut dyn FnMut(
            &SrcSpan,
            &Type,
            &EcoString,
            &EcoString,
            &EcoString,
            &ModuleValueConstructor,
        ),
    >,
    pub owned_visit_tuple_index: Option<&'a mut dyn FnMut(&SrcSpan, &Type, u64, &TypedExpr)>,
}

impl<'a> std::fmt::Debug for Visitor<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Visitor").finish()
    }
}

impl<'a> Visitor<'a> {
    pub fn visit_module(&mut self, module: &'a Module<ModuleInterface, TypedDefinition>) {
        for definition in &module.definitions {
            self.visit_definition(definition)
        }
    }

    fn visit_import(&mut self, import: &Import<EcoString>) {
        if let Some(owned_visit_import) = self.owned_visit_import.as_mut() {
            owned_visit_import(import);
        }
    }

    fn visit_definition(
        &mut self,
        definition: &'a Definition<Arc<Type>, TypedExpr, EcoString, EcoString>,
    ) {
        match definition {
            Definition::Function(fun) => self.visit_function(&fun.arguments, &fun.body),
            Definition::TypeAlias(_) => {}
            Definition::CustomType(_) => {}
            Definition::Import(import) => self.visit_import(&import),
            Definition::ModuleConstant(_) => {}
        }
    }

    fn visit_statements(
        &mut self,
        list_of_statements: impl Iterator<Item = &'a Statement<Arc<Type>, TypedExpr>>,
    ) {
        for statement in list_of_statements {
            self.visit_statement(statement)
        }
    }

    fn visit_statement(&mut self, statement: &'a Statement<Arc<Type>, TypedExpr>) {
        match statement {
            Statement::Expression(expression) => self.visit_expression(&expression),
            Statement::Assignment(asmt) => {
                self.visit_pattern(&asmt.pattern);
                self.visit_expression(&asmt.value)
            }
            Statement::Use(_) => {}
        }
    }

    fn visit_pattern_of_var(&mut self, location: &SrcSpan, name: &EcoString, type_: &Type) {
        if let Some(owned_visit_pattern_of_var) = self.owned_visit_pattern_of_var.as_mut() {
            owned_visit_pattern_of_var(&location, &name, &type_);
        }
    }

    fn visit_pattern(&mut self, pat: &Pattern<Arc<Type>>) {
        match pat {
            Pattern::Int { .. } => {}
            Pattern::Float { .. } => {}
            Pattern::String { .. } => {}
            Pattern::Variable {
                location,
                name,
                type_,
            } => self.visit_pattern_of_var(location, name, type_),
            Pattern::VarUsage { .. } => {}
            Pattern::Assign { .. } => {}
            Pattern::Discard { .. } => {}
            Pattern::List { .. } => {}
            Pattern::Constructor { .. } => {}
            Pattern::Tuple { .. } => {}
            Pattern::BitArray { .. } => {}
            Pattern::StringPrefix { .. } => {}
        }
    }

    fn visit_function(
        &mut self,
        _args: &Vec<Arg<Arc<Type>>>,
        body: &'a Vec1<Statement<Arc<Type>, TypedExpr>>,
    ) {
        self.visit_statements(body.iter())
    }

    fn visit_record_access(
        &mut self,
        location: &SrcSpan,
        typ: &Type,
        label: &EcoString,
        index: u64,
        record: &'a TypedExpr,
    ) {
        if let Some(owned_visit_record_access) = self.owned_visit_record_access.as_mut() {
            owned_visit_record_access(&location, &typ, &label, index, &record);
        }
        self.visit_expression(&record)
    }

    fn visit_module_select(
        &mut self,
        location: &SrcSpan,
        typ: &Type,
        label: &EcoString,
        module_name: &EcoString,
        module_alias: &EcoString,
        constructor: &ModuleValueConstructor,
    ) {
        if let Some(owned_visit_module_select) = self.owned_visit_module_select.as_mut() {
            owned_visit_module_select(location, typ, label, module_name, module_alias, constructor);
        }
    }

    fn visit_tuple_index(
        &mut self,
        location: &SrcSpan,
        typ: &Type,
        index: u64,
        tuple: &'a TypedExpr,
    ) {
        if let Some(owned_visit_tuple_index) = self.owned_visit_tuple_index.as_mut() {
            owned_visit_tuple_index(&location, &typ, index, &tuple);
        }
        self.visit_expression(&tuple);
    }

    fn visit_expression(&mut self, expression: &'a TypedExpr) {
        match expression {
            TypedExpr::Int { .. } => {}
            TypedExpr::Float { .. } => {}
            TypedExpr::String { .. } => {}
            TypedExpr::Block { statements, .. } => self.visit_statements(statements.iter()),
            TypedExpr::Pipeline {
                assignments,
                finally,
                ..
            } => {
                for assignment in assignments {
                    self.visit_pattern(&assignment.pattern);
                    self.visit_expression(&assignment.value);
                }
                self.visit_expression(finally)
            }
            TypedExpr::Var { .. } => {}
            TypedExpr::Fn { args, body, .. } => self.visit_function(args, body),
            TypedExpr::List { elements, tail, .. } => {
                elements
                    .iter()
                    .for_each(|element| self.visit_expression(element));
                tail.iter()
                    .for_each(|element| self.visit_expression(element));
            }
            TypedExpr::Call { fun, args, .. } => {
                self.visit_expression(fun);
                args.iter()
                    .for_each(|arg| self.visit_expression(&arg.value));
            }
            TypedExpr::BinOp { left, right, .. } => {
                self.visit_expression(&left);
                self.visit_expression(&right);
            }
            TypedExpr::Case {
                subjects, clauses, ..
            } => {
                subjects
                    .iter()
                    .for_each(|subject| self.visit_expression(&subject));
                clauses.iter().for_each(|clause| {
                    clause
                        .pattern
                        .iter()
                        .for_each(|pat| self.visit_pattern(pat));

                    clause.alternative_patterns.iter().for_each(|patterns| {
                        patterns.iter().for_each(|pat| self.visit_pattern(pat));
                    });

                    self.visit_expression(&clause.then);
                });
            }
            TypedExpr::RecordAccess {
                location,
                typ,
                label,
                index,
                record,
            } => {
                self.visit_record_access(location, typ, label, *index, record);
            }
            TypedExpr::ModuleSelect {
                location,
                typ,
                label,
                module_name,
                module_alias,
                constructor,
            } => {
                self.visit_module_select(
                    location,
                    typ,
                    label,
                    module_name,
                    module_alias,
                    constructor,
                );
            }
            TypedExpr::Tuple { elems, .. } => {
                elems.iter().for_each(|elem| self.visit_expression(elem))
            }
            TypedExpr::TupleIndex {
                location,
                typ,
                index,
                tuple,
            } => {
                self.visit_tuple_index(location, typ, *index, tuple);
            }
            TypedExpr::Todo { message, .. } => {
                message.iter().for_each(|msg| self.visit_expression(msg))
            }
            TypedExpr::Panic { message, .. } => {
                message.iter().for_each(|msg| self.visit_expression(msg))
            }
            TypedExpr::BitArray { segments, .. } => segments
                .iter()
                .for_each(|segment| self.visit_expression(&segment.value)),
            TypedExpr::RecordUpdate { spread, args, .. } => {
                self.visit_expression(spread);
                args.iter()
                    .for_each(|arg| self.visit_expression(&arg.value));
            }
            TypedExpr::NegateBool { value, .. } => self.visit_expression(value),
            TypedExpr::NegateInt { value, .. } => self.visit_expression(value),
        }
    }
}
