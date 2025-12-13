//! Graphs that represent the relationships between entities in a Gleam module,
//! such as module functions or constants.

#[cfg(test)]
mod into_dependency_order_tests;

#[cfg(test)]
mod into_mutual_recursion_groups_tests;

use crate::{
    Result,
    ast::{
        self, AssignName, AssignmentKind, BitArrayOption, BitArraySize, ClauseGuard, Constant,
        Pattern, SrcSpan, Statement, TypedFunction, UntypedClauseGuard, UntypedExpr,
        UntypedFunction, UntypedModuleConstant, UntypedPattern, UntypedStatement, visit::Visit,
    },
    type_::Error,
};
use ecow::EcoString;
use itertools::Itertools;
use petgraph::stable_graph::NodeIndex;
use petgraph::{Directed, stable_graph::StableGraph};

#[derive(Debug, Default)]
struct CallGraphBuilder<'a> {
    names: im::HashMap<&'a str, Option<(NodeIndex, SrcSpan)>>,
    graph: StableGraph<(), (), Directed>,
    current_function: NodeIndex,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CallGraphNode {
    Function(UntypedFunction),

    ModuleConstant(UntypedModuleConstant),
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
        let (_, name) = function
            .name
            .as_ref()
            .expect("A module's function must be named");
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
            .get(
                function
                    .name
                    .as_ref()
                    .map(|(_, name)| name.as_str())
                    .expect("A module's function must be named"),
            )
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
                match &assignment.kind {
                    AssignmentKind::Assert {
                        message: Some(message),
                        ..
                    } => self.expression(message),
                    AssignmentKind::Let
                    | AssignmentKind::Generated
                    | AssignmentKind::Assert { message: None, .. } => {}
                }
            }
            Statement::Use(use_) => {
                self.expression(&use_.call);
                for assignment in &use_.assignments {
                    self.pattern(&assignment.pattern);
                }
            }
            Statement::Assert(assert) => {
                self.expression(&assert.value);
                if let Some(message) = &assert.message {
                    self.expression(message)
                }
            }
        };
    }

    fn expression(&mut self, expression: &'a UntypedExpr) {
        match expression {
            UntypedExpr::Int { .. } | UntypedExpr::Float { .. } | UntypedExpr::String { .. } => (),

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

            UntypedExpr::Echo {
                expression,
                location: _,
                keyword_end: _,
                message,
            } => {
                if let Some(expression) = expression {
                    self.expression(expression);
                }
                if let Some(message) = message {
                    self.expression(message);
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

            UntypedExpr::Tuple { elements, .. } => {
                for expression in elements {
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
                record, arguments, ..
            } => {
                self.expression(&record.base);
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
                for clause in clauses.as_deref().unwrap_or_default() {
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
            }
            | Pattern::Invalid { .. } => (),

            Pattern::StringPrefix {
                right_side_assignment: AssignName::Variable(name),
                ..
            }
            | Pattern::Variable { name, .. } => {
                self.define(name);
            }

            Pattern::Tuple {
                elements: patterns, ..
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
                    self.pattern(&tail.pattern);
                }
            }

            Pattern::BitArraySize(size) => {
                self.bit_array_size(size);
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

    fn bit_array_size(&mut self, size: &'a BitArraySize<()>) {
        match size {
            BitArraySize::Int { .. } => {}
            BitArraySize::Variable { name, .. } => self.referenced(name),
            BitArraySize::BinaryOperator { left, right, .. } => {
                self.bit_array_size(left);
                self.bit_array_size(right);
            }
            BitArraySize::Block { inner, .. } => self.bit_array_size(inner),
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

    fn guard(&mut self, guard: &'a UntypedClauseGuard) {
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
            | ClauseGuard::AddInt { left, right, .. }
            | ClauseGuard::AddFloat { left, right, .. }
            | ClauseGuard::SubInt { left, right, .. }
            | ClauseGuard::SubFloat { left, right, .. }
            | ClauseGuard::MultInt { left, right, .. }
            | ClauseGuard::MultFloat { left, right, .. }
            | ClauseGuard::DivInt { left, right, .. }
            | ClauseGuard::DivFloat { left, right, .. }
            | ClauseGuard::RemainderInt { left, right, .. }
            | ClauseGuard::Or { left, right, .. }
            | ClauseGuard::And { left, right, .. } => {
                self.guard(left);
                self.guard(right);
            }

            ClauseGuard::Block { value, .. } => self.guard(value),

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

            Constant::Record { arguments, .. } => {
                for argument in arguments {
                    self.constant(&argument.value);
                }
            }

            Constant::RecordUpdate {
                record, arguments, ..
            } => {
                self.constant(&record.base);

                for argument in arguments {
                    self.constant(&argument.value);
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

            Constant::StringConcatenation { left, right, .. } => {
                self.constant(left);
                self.constant(right);
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

pub(crate) fn into_mutually_recursive_groups(
    functions: Vec<TypedFunction>,
) -> Vec<Vec<TypedFunction>> {
    let mut builder = MutualRecursionGraphBuilder::new();
    for function in &functions {
        builder.register_module_function_existence(function);
    }
    for function in &functions {
        builder.visit_typed_function(function);
    }

    // Determine the order in which the functions should be compiled by looking
    // at which other functions they depend on.
    let indices = crate::graph::into_dependency_order(builder.graph);

    // We got node indices back, so we need to map them back to the functions
    // they represent.
    // We wrap them each with `Some` so we can use `.take()`.
    let mut definitions = functions.into_iter().map(Some).collect_vec();

    indices
        .into_iter()
        .map(|indices| {
            indices
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
        .collect_vec()
}

#[derive(Debug, PartialEq, Eq)]
enum Position {
    /// This is the positions of expressions in tail call position.
    ///
    Tail,

    /// The position of everything else.
    ///
    NotTail,
}

struct MutualRecursionGraphBuilder {
    names: im::HashMap<EcoString, Option<NodeIndex>>,
    graph: StableGraph<(), (), Directed>,
    current_function: NodeIndex,
    position: Position,
}

impl MutualRecursionGraphBuilder {
    fn new() -> Self {
        MutualRecursionGraphBuilder {
            names: im::HashMap::new(),
            graph: StableGraph::new(),
            current_function: NodeIndex::default(),
            position: Position::NotTail,
        }
    }

    fn assign(&mut self, name: &EcoString) {
        let _ = self.names.insert(name.clone(), None);
    }

    fn referenced(&mut self, name: &EcoString) {
        // If we don't know what the target is then it's either a programmer
        // error to be detected later, or it's not a module function and as such
        // is not a value we are tracking.
        let Some(target) = self.names.get(name) else {
            return;
        };

        // If the target is known but registered as None then it's local value
        // that shadows a module function.
        let Some(target) = target else { return };

        _ = self.graph.add_edge(self.current_function, *target, ());
    }

    fn register_module_function_existence(&mut self, function: &TypedFunction) {
        let (_, name) = function
            .name
            .as_ref()
            .expect("A module's function must be named");

        let index = self.graph.add_node(());
        let _ = self.names.insert(name.clone(), Some(index));
    }
}

impl<'a> Visit<'a> for MutualRecursionGraphBuilder {
    fn visit_typed_function(&mut self, fun: &'a TypedFunction) {
        self.current_function = self
            .names
            .get(
                fun.name
                    .as_ref()
                    .map(|(_, name)| name.as_str())
                    .expect("A module's function must be named"),
            )
            .expect("Function must already have been registered as existing")
            .expect("Function must not be shadowed at module level");

        let names = self.names.clone();

        for (i, statement) in fun.body.iter().enumerate() {
            if i == fun.body.len() - 1 {
                // If the statement is the last one in a function's body, then
                // we know it's in tail position.
                self.position = Position::Tail;
                self.visit_typed_statement(statement);
            } else {
                self.position = Position::NotTail;
                self.visit_typed_statement(statement);
            }
        }

        self.names = names;
    }

    fn visit_typed_expr(&mut self, expr: &'a ast::TypedExpr) {
        // If an expression is not in tail position we can just ignore it.
        if self.position == Position::NotTail {
            return;
        }

        match expr {
            // Any expression that is not a call or a case in tail position is
            // not relevant to detecting mutually recursive functions, so we can
            // safely ignore them and not visit any further!
            ast::TypedExpr::Int { .. }
            | ast::TypedExpr::Float { .. }
            | ast::TypedExpr::String { .. }
            | ast::TypedExpr::Pipeline { .. }
            | ast::TypedExpr::Var { .. }
            | ast::TypedExpr::Fn { .. }
            | ast::TypedExpr::List { .. }
            | ast::TypedExpr::BinOp { .. }
            | ast::TypedExpr::RecordAccess { .. }
            | ast::TypedExpr::ModuleSelect { .. }
            | ast::TypedExpr::Tuple { .. }
            | ast::TypedExpr::TupleIndex { .. }
            | ast::TypedExpr::Todo { .. }
            | ast::TypedExpr::Panic { .. }
            | ast::TypedExpr::Echo { .. }
            | ast::TypedExpr::BitArray { .. }
            | ast::TypedExpr::RecordUpdate { .. }
            | ast::TypedExpr::NegateBool { .. }
            | ast::TypedExpr::NegateInt { .. }
            | ast::TypedExpr::Invalid { .. } => (),

            // If we're here we've already checked that we are in tail position,
            // so that means that the final statement in the block is going to
            // be in tail position as well!
            ast::TypedExpr::Block { statements, .. } => {
                let names = self.names.clone();
                for (i, statement) in statements.iter().enumerate() {
                    if i == statements.len() - 1 {
                        // If the statement is the last one in the block, then
                        // we know it's in tail position.
                        self.position = Position::Tail;
                        self.visit_typed_statement(statement);
                    } else {
                        // Otherwise all the other statements are not in tail
                        // position.
                        self.position = Position::NotTail;
                        self.visit_typed_statement(statement);
                    }
                }
                self.names = names;
            }

            ast::TypedExpr::Call { fun, .. } => {
                // Here we might be calling a top level function and so have a
                // mutually recursive call!
                if let ast::TypedExpr::Var { name, .. } = fun.as_ref() {
                    self.referenced(name)
                }
            }

            ast::TypedExpr::Case { clauses, .. } => {
                for clause in clauses {
                    // We need to keep separate scopes for each of the branches
                    // so we will clone the names and restore them afterwards.
                    let names = self.names.clone();
                    ast::visit::visit_typed_clause(self, clause);
                    self.names = names;
                }
            }
        }
    }

    // When visiting a variable, assignment, or string prefix pattern; we need
    // to remember the variables they're defining so that we can tell if they
    // are shadowing existing functions defined in the outer scope.

    fn visit_typed_pattern_variable(
        &mut self,
        _location: &'a SrcSpan,
        name: &'a EcoString,
        _type_: &'a std::sync::Arc<crate::type_::Type>,
        _origin: &'a crate::type_::error::VariableOrigin,
    ) {
        self.assign(name);
    }

    fn visit_typed_pattern_assign(
        &mut self,
        _location: &'a SrcSpan,
        name: &'a EcoString,
        pattern: &'a ast::TypedPattern,
    ) {
        self.assign(name);
        ast::visit::visit_typed_pattern(self, pattern);
    }

    fn visit_typed_pattern_string_prefix(
        &mut self,
        _location: &'a SrcSpan,
        _left_location: &'a SrcSpan,
        left_side_assignment: &'a Option<(EcoString, SrcSpan)>,
        _right_location: &'a SrcSpan,
        _left_side_string: &'a EcoString,
        right_side_assignment: &'a AssignName,
    ) {
        if let Some((name, _)) = left_side_assignment {
            self.assign(name);
        }

        match right_side_assignment {
            AssignName::Variable(name) => self.assign(name),
            AssignName::Discard(_) => (),
        }
    }
}
