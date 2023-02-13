//! Graphs that represent the relationships between entities in a Gleam module,
//! such as module functions or constants.

#![allow(unused)]

#[cfg(test)]
mod into_dependency_order_tests;

use crate::ast::{
    AssignName, BitStringSegmentOption, ClauseGuard, Constant, ModuleFunction, Pattern, SrcSpan,
    UntypedPattern, Use,
};
use crate::type_::Error;
use crate::{
    ast::{ExternalFunction, Function, UntypedExpr},
    Result,
};
use itertools::Itertools;
use petgraph::stable_graph::NodeIndex;
use petgraph::{stable_graph::StableGraph, Directed};
use smol_str::SmolStr;

#[derive(Debug, Default)]
struct CallGraphBuilder<'a> {
    names: im::HashMap<&'a str, Option<(NodeIndex, SrcSpan)>>,
    graph: StableGraph<(), (), Directed>,
    current_function: NodeIndex,
}

impl<'a> CallGraphBuilder<'a> {
    fn into_graph(self) -> StableGraph<(), (), Directed> {
        self.graph
    }

    /// Add each function to the graph, storing the index of the node under the
    /// name of the function.
    fn register_module_function_existance(
        &mut self,
        function: &'a ModuleFunction,
    ) -> Result<(), Error> {
        let name = function.name();
        let location = function.location();

        let index = self.graph.add_node(());
        let previous = self.names.insert(name, Some((index, location)));

        if let Some(Some((_, previous_location))) = previous {
            return Err(Error::DuplicateName {
                location_a: location,
                location_b: previous_location,
                name: name.clone(),
            });
        }
        Ok(())
    }

    fn register_references(&mut self, function: &'a ModuleFunction) {
        match function {
            ModuleFunction::Internal(f) => {
                let current = self
                    .names
                    .get(f.name.as_str())
                    .expect("Function must already have been registered as existing")
                    .expect("Function must not be shadowed at module level")
                    .0;
                self.expression(current, &f.body)
            }
            ModuleFunction::External(_) => {}
        }
    }

    fn referenced(&mut self, current: NodeIndex, name: &str) {
        // If we don't know what the target is then it's either a programmer
        // error to be detected later, or it's not a module function and as such
        // is not a value we are tracking.
        let Some(target) = self.names.get(name) else { return };

        // If the target is known but registered as None then it's local value
        // that shadows a module function.
        let Some((target, _)) = target else { return };

        _ = self.graph.add_edge(current, *target, ());
    }

    fn scoped_expression(&mut self, current: NodeIndex, expression: &'a UntypedExpr) {
        let names = self.names.clone();
        self.expression(current, expression);
        self.names = names;
    }

    fn expression(&mut self, current: NodeIndex, expression: &'a UntypedExpr) {
        match expression {
            UntypedExpr::Todo { .. }
            | UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. } => (),

            // Aha! A variable is being referenced.
            UntypedExpr::Var { name, .. } => {
                self.referenced(current, name);
            }

            UntypedExpr::Call { fun, arguments, .. } => {
                self.expression(current, fun);
                for argument in arguments {
                    self.scoped_expression(current, &argument.value);
                }
            }

            UntypedExpr::PipeLine { expressions } => {
                for expression in expressions {
                    self.expression(current, expression);
                }
            }

            UntypedExpr::Tuple { elems, .. } => {
                for expression in elems {
                    self.scoped_expression(current, expression);
                }
            }

            UntypedExpr::Sequence { expressions, .. } => {
                let names = self.names.clone();
                for expression in expressions {
                    self.expression(current, expression);
                }
                self.names = names;
            }

            UntypedExpr::BinOp { left, right, .. } => {
                self.scoped_expression(current, left);
                self.scoped_expression(current, right);
            }

            UntypedExpr::List { elements, tail, .. } => {
                for element in elements {
                    self.scoped_expression(current, element);
                }
                if let Some(tail) = tail {
                    self.scoped_expression(current, tail);
                }
            }

            UntypedExpr::Negate {
                value: expression, ..
            }
            | UntypedExpr::TupleIndex {
                tuple: expression, ..
            }
            | UntypedExpr::FieldAccess {
                container: expression,
                ..
            } => {
                self.scoped_expression(current, expression);
            }

            UntypedExpr::BitString { segments, .. } => {
                for segment in segments {
                    self.scoped_expression(current, &segment.value);
                }
            }

            UntypedExpr::RecordUpdate {
                spread, arguments, ..
            } => {
                self.scoped_expression(current, &spread.base);
                for argument in arguments {
                    self.scoped_expression(current, &argument.value);
                }
            }

            UntypedExpr::Use(use_) => {
                for name in use_.assigned_names() {
                    self.define(name);
                }
                self.expression(current, &use_.call);
            }

            UntypedExpr::Fn {
                arguments, body, ..
            } => {
                let names = self.names.clone();
                for argument in arguments {
                    if let Some(name) = argument.names.get_variable_name() {
                        self.define(name)
                    }
                }
                self.scoped_expression(current, body);
                self.names = names;
            }

            UntypedExpr::Assignment { value, pattern, .. } => {
                self.expression(current, value);
                self.pattern(current, pattern);
            }

            UntypedExpr::Try { value, then, .. } => {
                self.expression(current, value);
                self.expression(current, then);
            }

            UntypedExpr::Case {
                subjects, clauses, ..
            } => {
                for subject in subjects {
                    self.expression(current, subject);
                }
                for clause in clauses {
                    let names = self.names.clone();
                    for pattern in &clause.pattern {
                        self.pattern(current, pattern);
                    }
                    if let Some(guard) = &clause.guard {
                        self.guard(current, guard);
                    }
                    self.expression(current, &clause.then);
                    self.names = names;
                }
            }
        }
    }

    fn pattern(&mut self, current: NodeIndex, pattern: &'a UntypedPattern) {
        match pattern {
            Pattern::Discard { .. }
            | Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::Concatenate {
                right_side_assignment: AssignName::Discard(_),
                ..
            } => (),

            Pattern::Concatenate {
                right_side_assignment: AssignName::Variable(name),
                ..
            }
            | Pattern::Var { name, .. } => {
                self.define(name);
            }

            Pattern::Tuple {
                elems: patterns, ..
            } => {
                for pattern in patterns {
                    self.pattern(current, pattern);
                }
            }

            Pattern::List { elements, tail, .. } => {
                for element in elements {
                    self.pattern(current, element);
                }
                if let Some(tail) = tail {
                    self.pattern(current, tail);
                }
            }

            Pattern::VarUsage { name, .. } => {
                self.referenced(current, name);
            }

            Pattern::Assign { name, pattern, .. } => {
                self.define(name);
                self.pattern(current, pattern);
            }

            Pattern::Constructor { arguments, .. } => {
                for argument in arguments {
                    self.pattern(current, &argument.value);
                }
            }

            Pattern::BitString { segments, .. } => {
                for segment in segments {
                    for option in &segment.options {
                        self.bit_string_option(current, option, |s, p, c| s.pattern(p, c));
                    }
                    self.pattern(current, &segment.value);
                }
            }
        }
    }

    fn define(&mut self, name: &'a str) {
        _ = self.names.insert(name, None);
    }

    fn bit_string_option<T>(
        &mut self,
        current: NodeIndex,
        option: &'a BitStringSegmentOption<T>,
        process: impl Fn(&mut Self, NodeIndex, &'a T),
    ) {
        match option {
            BitStringSegmentOption::Big { .. }
            | BitStringSegmentOption::Binary { .. }
            | BitStringSegmentOption::BitString { .. }
            | BitStringSegmentOption::Float { .. }
            | BitStringSegmentOption::Int { .. }
            | BitStringSegmentOption::Little { .. }
            | BitStringSegmentOption::Native { .. }
            | BitStringSegmentOption::Signed { .. }
            | BitStringSegmentOption::Unit { .. }
            | BitStringSegmentOption::Unsigned { .. }
            | BitStringSegmentOption::Utf16 { .. }
            | BitStringSegmentOption::Utf16Codepoint { .. }
            | BitStringSegmentOption::Utf32 { .. }
            | BitStringSegmentOption::Utf32Codepoint { .. }
            | BitStringSegmentOption::Utf8 { .. }
            | BitStringSegmentOption::Utf8Codepoint { .. } => (),

            BitStringSegmentOption::Size { value: pattern, .. } => {
                process(self, current, pattern);
            }
        }
    }

    fn guard(&mut self, current: NodeIndex, guard: &'a ClauseGuard<(), ()>) {
        match guard {
            ClauseGuard::Equals { left, right, .. }
            | ClauseGuard::NotEquals { left, right, .. }
            | ClauseGuard::GtInt { left, right, .. }
            | ClauseGuard::GtEqInt { left, right, .. }
            | ClauseGuard::LtInt { left, right, .. }
            | ClauseGuard::LtEqInt { left, right, .. }
            | ClauseGuard::GtFloat { left, right, .. }
            | ClauseGuard::GtEqFloat { left, right, .. }
            | ClauseGuard::LtFloat { left, right, .. }
            | ClauseGuard::LtEqFloat { left, right, .. }
            | ClauseGuard::Or { left, right, .. }
            | ClauseGuard::And { left, right, .. } => {
                self.guard(current, left);
                self.guard(current, right);
            }

            ClauseGuard::Var { name, .. } => self.referenced(current, name),

            ClauseGuard::TupleIndex { tuple, .. } => self.guard(current, tuple),

            ClauseGuard::Constant(constant) => self.constant(current, constant),
        }
    }

    fn constant(&mut self, current: NodeIndex, constant: &'a Constant<(), ()>) {
        match constant {
            Constant::Int { .. }
            | Constant::Float { .. }
            | Constant::String { .. }
            | Constant::Var {
                module: Some(_), ..
            } => (),

            Constant::List { elements, .. } | Constant::Tuple { elements, .. } => {
                for element in elements {
                    self.constant(current, element);
                }
            }

            Constant::Record { args, .. } => {
                for arg in args {
                    self.constant(current, &arg.value);
                }
            }

            Constant::Var {
                module: None, name, ..
            } => self.referenced(current, name),

            Constant::BitString { segments, .. } => {
                for segment in segments {
                    for option in &segment.options {
                        self.bit_string_option(current, option, |s, p, c| s.constant(p, c));
                    }
                    self.constant(current, &segment.value);
                }
            }
        }
    }
}

/// Determine the order in which functions should be compiled and if any
/// mutually recursive functions need to be compiled together.
///
pub fn into_dependency_order(
    functions: Vec<ModuleFunction>,
) -> Result<Vec<Vec<ModuleFunction>>, Error> {
    let mut grapher = CallGraphBuilder::default();

    for function in &functions {
        grapher.register_module_function_existance(function)?;
    }

    // Build the call graph between the module functions.
    for function in &functions {
        grapher.register_references(function);
    }

    // Consume the grapher to get the graph
    let graph = grapher.into_graph();

    // Determine the order in which the functions should be compiled by looking
    // at which other functions they depend on.
    let indicies = crate::graph::into_dependency_order(graph);

    // We got node indicies back, so we need to map them back to the functions
    // they represent.
    // We wrap them each with `Some` so we can use `.take()`.
    let mut functions = functions.into_iter().map(Some).collect_vec();
    let ordered = indicies
        .into_iter()
        .map(|level| {
            level
                .into_iter()
                .map(|index| {
                    functions
                        .get_mut(index.index())
                        .expect("Index out of bounds")
                        .take()
                        .expect("Function already taken")
                })
                .collect_vec()
        })
        .collect_vec();

    Ok(ordered)
}
