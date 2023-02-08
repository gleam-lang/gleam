//! Graphs that represent the relationships between entities in a Gleam module,
//! such as module functions or constants.

#![allow(unused)]

#[cfg(test)]
mod into_dependency_order_tests;

use crate::ast::{AssignName, BitStringSegmentOption, Pattern, SrcSpan, UntypedPattern, Use};
use crate::{
    ast::{ExternalFunction, Function as ModuleFunction, UntypedExpr},
    Result,
};
use itertools::Itertools;
use petgraph::stable_graph::NodeIndex;
use petgraph::{stable_graph::StableGraph, Directed};

#[derive(Debug)]
pub enum Function {
    Module(ModuleFunction<(), UntypedExpr>),
    External(ExternalFunction<()>),
}

impl Function {
    pub fn name(&self) -> &str {
        match self {
            Function::Module(f) => &f.name,
            Function::External(f) => &f.name,
        }
    }

    pub fn location(&self) -> SrcSpan {
        match self {
            Function::Module(f) => f.location,
            Function::External(f) => f.location,
        }
    }
}

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
    fn register_module_function_existance(&mut self, function: &'a Function) -> Result<()> {
        let name = function.name();
        let location = function.location();

        let index = self.graph.add_node(());
        let previous = self.names.insert(name, Some((index, location)));

        // TODO: return an error if there are duplicate function names
        if let Some(Some((_, previous_location))) = previous {
            panic!(
                "Duplicate function name {name} found at {location:?} and {previous_location:?}"
            );
        }
        Ok(())
    }

    fn register_references(&mut self, function: &'a Function) {
        match function {
            Function::Module(f) => {
                let current = self
                    .names
                    .get(f.name.as_str())
                    .expect("Function must exist")
                    .expect("Function must not be shadowed")
                    .0;
                self.expression(current, &f.body)
            }
            Function::External(_) => {}
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
                    // TODO: do the guard.
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
                        self.bit_string_option_pattern(current, option);
                    }
                    self.pattern(current, &segment.value);
                }
            }
        }
    }

    fn define(&mut self, name: &'a str) {
        _ = self.names.insert(name, None);
    }

    fn bit_string_option_pattern(
        &mut self,
        current: NodeIndex,
        option: &'a BitStringSegmentOption<Pattern<(), ()>>,
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
                self.pattern(current, pattern);
            }
        }
    }
}

/// Determine the order in which functions should be compiled and if any
/// mutually recursive functions need to be compiled together.
///
pub fn into_dependency_order(functions: Vec<Function>) -> Result<Vec<Vec<Function>>> {
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
