//! Graphs that represent the relationships between entities in a Gleam module,
//! such as module functions or constants.

#[cfg(test)]
mod into_dependency_order_tests;

use crate::{
    ast::{
        AssignName, BitStringSegmentOption, ClauseGuard, Constant, Pattern, SrcSpan, Statement,
        UntypedExpr, UntypedFunction, UntypedPattern, UntypedStatement,
    },
    type_::Error,
    Result,
};
use itertools::Itertools;
use petgraph::stable_graph::NodeIndex;
use petgraph::{stable_graph::StableGraph, Directed};

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
        function: &'a UntypedFunction,
    ) -> Result<(), Error> {
        let name = &function.name;
        let location = function.location;

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

    fn register_references(&mut self, function: &'a UntypedFunction) {
        let names = self.names.clone();
        self.current_function = self
            .names
            .get(function.name.as_str())
            .expect("Function must already have been registered as existing")
            .expect("Function must not be shadowed at module level")
            .0;
        for name in function
            .arguments
            .iter()
            .flat_map(|a| a.get_variable_name())
        {
            self.define(name);
        }
        self.statements(&function.body);
        self.names = names;
    }

    fn referenced(&mut self, name: &str) {
        // If we don't know what the target is then it's either a programmer
        // error to be detected later, or it's not a module function and as such
        // is not a value we are tracking.
        let Some(target) = self.names.get(name) else {
            return;
        };

        // If the target is known but registered as None then it's local value
        // that shadows a module function.
        let Some((target, _)) = target else { return };

        _ = self.graph.add_edge(self.current_function, *target, ());
    }

    fn statements(&mut self, statements: &'a [UntypedStatement]) {
        let names = self.names.clone();
        for statement in statements {
            self.statement(statement);
        }
        self.names = names;
    }

    fn statement(&mut self, statement: &'a UntypedStatement) {
        match statement {
            Statement::Expression(expression) => {
                self.expression(expression);
            }
            Statement::Assignment(assignment) => {
                self.expression(&assignment.value);
                self.pattern(&assignment.pattern);
            }
            Statement::Use(use_) => {
                self.expression(&use_.call);
                for assignment in &use_.assignments {
                    self.pattern(&assignment.pattern);
                }
            }
        };
    }

    fn expression(&mut self, expression: &'a UntypedExpr) {
        match expression {
            UntypedExpr::Todo { .. }
            | UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::Panic { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Placeholder { .. } => (),

            // Aha! A variable is being referenced.
            UntypedExpr::Var { name, .. } => {
                self.referenced(name);
            }

            UntypedExpr::Call { fun, arguments, .. } => {
                self.expression(fun);
                for argument in arguments {
                    self.expression(&argument.value);
                }
            }

            UntypedExpr::PipeLine { expressions } => {
                for expression in expressions {
                    self.expression(expression);
                }
            }

            UntypedExpr::Tuple { elems, .. } => {
                for expression in elems {
                    self.expression(expression);
                }
            }

            UntypedExpr::Block { statements, .. } => {
                let names = self.names.clone();
                self.statements(statements);
                self.names = names;
            }

            UntypedExpr::BinOp { left, right, .. } => {
                self.expression(left);
                self.expression(right);
            }

            UntypedExpr::List { elements, tail, .. } => {
                for element in elements {
                    self.expression(element);
                }
                if let Some(tail) = tail {
                    self.expression(tail);
                }
            }

            UntypedExpr::NegateInt {
                value: expression, ..
            }
            | UntypedExpr::NegateBool {
                value: expression, ..
            }
            | UntypedExpr::TupleIndex {
                tuple: expression, ..
            }
            | UntypedExpr::FieldAccess {
                container: expression,
                ..
            } => {
                self.expression(expression);
            }

            UntypedExpr::BitString { segments, .. } => {
                for segment in segments {
                    self.expression(&segment.value);
                }
            }

            UntypedExpr::RecordUpdate {
                spread, arguments, ..
            } => {
                self.expression(&spread.base);
                for argument in arguments {
                    self.expression(&argument.value);
                }
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
                self.statements(body);
                self.names = names;
            }

            UntypedExpr::Case {
                subjects, clauses, ..
            } => {
                for subject in subjects {
                    self.expression(subject);
                }
                for clause in clauses {
                    let names = self.names.clone();
                    for pattern in &clause.pattern {
                        self.pattern(pattern);
                    }
                    if let Some(guard) = &clause.guard {
                        self.guard(guard);
                    }
                    self.expression(&clause.then);
                    self.names = names;
                }
            }
        }
    }

    fn pattern(&mut self, pattern: &'a UntypedPattern) {
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
                    self.pattern(pattern);
                }
            }

            Pattern::List { elements, tail, .. } => {
                for element in elements {
                    self.pattern(element);
                }
                if let Some(tail) = tail {
                    self.pattern(tail);
                }
            }

            Pattern::VarUsage { name, .. } => {
                self.referenced(name);
            }

            Pattern::Assign { name, pattern, .. } => {
                self.define(name);
                self.pattern(pattern);
            }

            Pattern::Constructor { arguments, .. } => {
                for argument in arguments {
                    self.pattern(&argument.value);
                }
            }

            Pattern::BitString { segments, .. } => {
                for segment in segments {
                    for option in &segment.options {
                        self.bit_string_option(option, |s, p| s.pattern(p));
                    }
                    self.pattern(&segment.value);
                }
            }
        }
    }

    fn define(&mut self, name: &'a str) {
        _ = self.names.insert(name, None);
    }

    fn bit_string_option<T>(
        &mut self,
        option: &'a BitStringSegmentOption<T>,
        process: impl Fn(&mut Self, &'a T),
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
                process(self, pattern);
            }
        }
    }

    fn guard(&mut self, guard: &'a ClauseGuard<(), ()>) {
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
                self.guard(left);
                self.guard(right);
            }

            ClauseGuard::Var { name, .. } => self.referenced(name),

            ClauseGuard::TupleIndex { tuple, .. } => self.guard(tuple),

            ClauseGuard::Constant(constant) => self.constant(constant),
        }
    }

    fn constant(&mut self, constant: &'a Constant<(), ()>) {
        match constant {
            Constant::Int { .. }
            | Constant::Float { .. }
            | Constant::String { .. }
            | Constant::Var {
                module: Some(_), ..
            } => (),

            Constant::List { elements, .. } | Constant::Tuple { elements, .. } => {
                for element in elements {
                    self.constant(element);
                }
            }

            Constant::Record { args, .. } => {
                for arg in args {
                    self.constant(&arg.value);
                }
            }

            Constant::Var {
                module: None, name, ..
            } => self.referenced(name),

            Constant::BitString { segments, .. } => {
                for segment in segments {
                    for option in &segment.options {
                        self.bit_string_option(option, |s, c| s.constant(c));
                    }
                    self.constant(&segment.value);
                }
            }
        }
    }
}

/// Determine the order in which functions should be compiled and if any
/// mutually recursive functions need to be compiled together.
///
pub fn into_dependency_order(
    functions: Vec<UntypedFunction>,
) -> Result<Vec<Vec<UntypedFunction>>, Error> {
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
