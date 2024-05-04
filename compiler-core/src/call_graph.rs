//! Graphs that represent the relationships between entities in a Gleam module,
//! such as module functions or constants.

#[cfg(test)]
mod into_dependency_order_tests;

use crate::ast::{ModuleConstant, UntypedModuleConstant};
use crate::{
    ast::{
        AssignName, BitArrayOption, ClauseGuard, Constant, Pattern, SrcSpan, Statement,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CallGraphNode {
    Function(UntypedFunction),

    ModuleConstant(ModuleConstant<(), ()>),
}

impl<'a> CallGraphBuilder<'a> {
    fn into_graph(self) -> StableGraph<(), (), Directed> {
        self.graph
    }

    /// Add each function to the graph, storing the index of the node under the
    /// name of the function.
    fn register_module_function_existence(
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

    /// Add each constant to the graph, storing the index of the node under the
    /// name of the constant.
    fn register_module_const_existence(
        &mut self,
        constant: &'a UntypedModuleConstant,
    ) -> Result<(), Error> {
        let name = &constant.name;
        let location = constant.location;

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

    fn register_references_constant(&mut self, constant: &'a UntypedModuleConstant) {
        self.current_function = self
            .names
            .get(constant.name.as_str())
            .expect("Constant must already have been registered as existing")
            .expect("Constant must not be shadowed at module level")
            .0;
        self.constant(&constant.value);
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
            UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Placeholder { .. } => (),

            UntypedExpr::Todo { message, .. } => {
                if let Some(msg_expr) = message {
                    self.expression(msg_expr)
                }
            }

            UntypedExpr::Panic { message, .. } => {
                if let Some(msg_expr) = message {
                    self.expression(msg_expr)
                }
            }

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

            UntypedExpr::BitArray { segments, .. } => {
                for segment in segments {
                    self.expression(&segment.value);
                    for option in &segment.options {
                        if let BitArrayOption::Size { value, .. } = option {
                            self.expression(value);
                        }
                    }
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
            | Pattern::StringPrefix {
                right_side_assignment: AssignName::Discard(_),
                ..
            } => (),

            Pattern::StringPrefix {
                right_side_assignment: AssignName::Variable(name),
                ..
            }
            | Pattern::Variable { name, .. } => {
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

            Pattern::BitArray { segments, .. } => {
                for segment in segments {
                    for option in &segment.options {
                        self.bit_array_option(option, |s, p| s.pattern(p));
                    }
                    self.pattern(&segment.value);
                }
            }
        }
    }

    fn define(&mut self, name: &'a str) {
        _ = self.names.insert(name, None);
    }

    fn bit_array_option<T>(
        &mut self,
        option: &'a BitArrayOption<T>,
        process: impl Fn(&mut Self, &'a T),
    ) {
        match option {
            BitArrayOption::Big { .. }
            | BitArrayOption::Bytes { .. }
            | BitArrayOption::Bits { .. }
            | BitArrayOption::Float { .. }
            | BitArrayOption::Int { .. }
            | BitArrayOption::Little { .. }
            | BitArrayOption::Native { .. }
            | BitArrayOption::Signed { .. }
            | BitArrayOption::Unit { .. }
            | BitArrayOption::Unsigned { .. }
            | BitArrayOption::Utf16 { .. }
            | BitArrayOption::Utf16Codepoint { .. }
            | BitArrayOption::Utf32 { .. }
            | BitArrayOption::Utf32Codepoint { .. }
            | BitArrayOption::Utf8 { .. }
            | BitArrayOption::Utf8Codepoint { .. } => (),

            BitArrayOption::Size { value: pattern, .. } => {
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

            ClauseGuard::Not { expression, .. } => self.guard(expression),

            ClauseGuard::Var { name, .. } => self.referenced(name),

            ClauseGuard::TupleIndex { tuple, .. } => self.guard(tuple),

            ClauseGuard::FieldAccess { container, .. } => self.guard(container),

            ClauseGuard::ModuleSelect { module_name, .. } => self.referenced(module_name),

            ClauseGuard::Constant(constant) => self.constant(constant),
        }
    }

    fn constant(&mut self, constant: &'a Constant<(), ()>) {
        match constant {
            Constant::Int { .. }
            | Constant::Float { .. }
            | Constant::String { .. }
            | Constant::Invalid { .. }
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

            Constant::BitArray { segments, .. } => {
                for segment in segments {
                    for option in &segment.options {
                        self.bit_array_option(option, |s, c| s.constant(c));
                    }
                    self.constant(&segment.value);
                }
            }
        }
    }
}

/// Determine the order in which functions and constants should be compiled and if any
/// mutually recursive functions need to be compiled together.
///
pub fn into_dependency_order(
    functions: Vec<UntypedFunction>,
    constants: Vec<UntypedModuleConstant>,
) -> Result<Vec<Vec<CallGraphNode>>, Error> {
    let mut grapher = CallGraphBuilder::default();

    for function in &functions {
        grapher.register_module_function_existence(function)?;
    }

    for constant in &constants {
        grapher.register_module_const_existence(constant)?;
    }

    // Build the call graph between the module functions.
    for function in &functions {
        grapher.register_references(function);
    }

    for constant in &constants {
        grapher.register_references_constant(constant);
    }

    // Consume the grapher to get the graph
    let graph = grapher.into_graph();

    // Determine the order in which the functions should be compiled by looking
    // at which other functions they depend on.
    let indices = crate::graph::into_dependency_order(graph);

    // We got node indices back, so we need to map them back to the functions
    // they represent.
    // We wrap them each with `Some` so we can use `.take()`.
    let mut definitions = functions
        .into_iter()
        .map(CallGraphNode::Function)
        .chain(constants.into_iter().map(CallGraphNode::ModuleConstant))
        .map(Some)
        .collect_vec();

    let ordered = indices
        .into_iter()
        .map(|level| {
            level
                .into_iter()
                .map(|index| {
                    definitions
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
