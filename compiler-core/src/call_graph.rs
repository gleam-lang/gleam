//! Graphs that represent the relationships between entities in a Gleam module,
//! such as module functions or constants.

// TODO: remove
#![allow(unused)]

use crate::ast::SrcSpan;
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
    names: im::HashMap<&'a str, (NodeIndex, SrcSpan)>,
    graph: StableGraph<(), (), Directed>,
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
        let previous = self.names.insert(name, (index, location));

        // TODO: return an error if there are duplicate function names
        if let Some((_, previous_location)) = previous {
            panic!(
                "Duplicate function name {} found at {:?} and {:?}",
                name, location, previous_location
            );
        }
        Ok(())
    }

    fn register_references(&mut self, function: &Function) {
        match function {
            Function::Module(f) => self.register_expression(&f.body),
            Function::External(_) => {}
        }
    }

    fn register_expression(&self, expression: &UntypedExpr) {
        match expression {
            UntypedExpr::Todo { .. }
            | UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. } => (),

            // TODO: test
            UntypedExpr::Sequence { expressions, .. } => todo!(),
            // TODO: test
            UntypedExpr::Var { name, .. } => todo!(),
            // TODO: test
            UntypedExpr::Fn { body, .. } => todo!(),
            // TODO: test
            UntypedExpr::List { elements, tail, .. } => todo!(),
            // TODO: test
            UntypedExpr::Call { fun, arguments, .. } => todo!(),
            // TODO: test
            UntypedExpr::BinOp { left, right, .. } => todo!(),
            // TODO: test
            UntypedExpr::PipeLine { expressions } => todo!(),
            // TODO: test
            UntypedExpr::Assignment { value, .. } => todo!(),
            // TODO: test
            UntypedExpr::Try { value, then, .. } => todo!(),
            // TODO: test
            UntypedExpr::Use(_) => todo!(),
            // TODO: test
            UntypedExpr::Case {
                subjects, clauses, ..
            } => todo!(),
            // TODO: test
            UntypedExpr::FieldAccess { container, .. } => todo!(),
            // TODO: test
            UntypedExpr::Tuple { elems, .. } => todo!(),
            // TODO: test
            UntypedExpr::TupleIndex { tuple, .. } => todo!(),
            // TODO: test
            UntypedExpr::BitString { segments, .. } => todo!(),
            // TODO: test
            UntypedExpr::RecordUpdate {
                constructor,
                spread,
                arguments,
                ..
            } => todo!(),
            // TODO: test
            UntypedExpr::Negate { value, .. } => todo!(),
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
                    functions[index.index()]
                        .take()
                        .expect("Function already taken")
                })
                .collect_vec()
        })
        .collect_vec();

    Ok(ordered)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::TypeAst;
    use smol_str::SmolStr;

    enum Input {
        Module(&'static str, &'static str),
        External(&'static str),
    }

    #[test]
    fn into_dependency_order_empty() {
        let functions = [];
        assert_eq!(
            parse_and_order(&functions).unwrap(),
            Vec::<Vec<SmolStr>>::new()
        );
    }

    #[test]
    fn into_dependency_order_no_deps() {
        let functions = [
            Input::External("a"),
            Input::Module("b", r#""ok""#),
            Input::Module("c", r#"1"#),
            Input::Module("d", r#"1.0"#),
            Input::Module("e", r#"todo"#),
        ];
        assert_eq!(
            parse_and_order(&functions).unwrap(),
            vec![vec!["a"], vec!["b"], vec!["c"], vec!["d"], vec!["e"]]
        );
    }

    fn parse_and_order(functions: &[Input]) -> Result<Vec<Vec<SmolStr>>> {
        let functions = functions
            .iter()
            .map(|input| match input {
                Input::Module(name, src) => Function::Module(ModuleFunction {
                    name: name.into(),
                    arguments: vec![],
                    body: crate::parse::parse_expression_sequence(src).expect("syntax error"),
                    location: Default::default(),
                    return_annotation: None,
                    public: true,
                    end_position: src.len() as u32,
                    return_type: (),
                    doc: None,
                }),
                Input::External(name) => Function::External(ExternalFunction {
                    name: name.into(),
                    arguments: vec![],
                    module: "themodule".into(),
                    fun: name.into(),
                    location: Default::default(),
                    public: true,
                    return_: TypeAst::Hole {
                        location: Default::default(),
                        name: "_".into(),
                    },
                    return_type: (),
                    doc: None,
                }),
            })
            .collect_vec();
        Ok(into_dependency_order(functions)?
            .into_iter()
            .map(|level| {
                level
                    .into_iter()
                    .map(|function| function.name().into())
                    .collect_vec()
            })
            .collect())
    }
}
