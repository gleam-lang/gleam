#[cfg(test)]
mod tests;

use std::path::Path;

use smol_str::SmolStr;
use strum::IntoEnumIterator;

use crate::{
    ast::{
        AssignName, AssignmentKind, CallArg, Import, Pattern, Statement, TargetGroup, UntypedExpr,
        UntypedModule, UntypedPattern, UntypedStatement, Use,
    },
    build::Target,
    format::{Formatter, Intermediate},
    Error, Result,
};

pub fn parse_fix_and_format(src: &SmolStr, path: &Path) -> Result<String> {
    // Parse
    let (mut module, extra) = crate::parse::parse_module(src).map_err(|error| Error::Parse {
        path: path.to_path_buf(),
        src: src.clone(),
        error,
    })?;
    let intermediate = Intermediate::from_extra(&extra, src);

    // Fix
    for target in Target::iter() {
        Fixer::fix(&mut module, target);
    }

    // Format
    let mut buffer = String::new();
    Formatter::with_comments(&intermediate)
        .module(&module)
        .pretty_print(80, &mut buffer)?;

    Ok(buffer)
}

#[derive(Debug)]
enum ResultModule {
    Existing(String),
    Insert(String),
}

impl ResultModule {
    fn name(&self) -> &str {
        match self {
            ResultModule::Existing(name) | ResultModule::Insert(name) => &name,
        }
    }

    fn into_name(self) -> String {
        match self {
            ResultModule::Existing(name) | ResultModule::Insert(name) => name,
        }
    }
}

#[derive(Debug)]
pub struct Fixer {
    target: Target,
    module_used: bool,
    result_module: ResultModule,
}

impl Fixer {
    /// Fix this module to remove use of the `try` keyword.
    /// Mutates the module in place.
    ///
    pub fn fix(module: &mut UntypedModule, target: Target) {
        Self {
            target,
            module_used: false,
            result_module: check_for_result_module_import(target, &module),
        }
        .fix_module(module)
    }

    fn fix_module(&mut self, module: &mut UntypedModule) {
        // Rewrite any `try`s into `use`s.
        for group in module.statements.iter_mut() {
            match group {
                TargetGroup::Only(t, _) if *t != self.target => (),
                TargetGroup::Any(statements) | TargetGroup::Only(_, statements) => {
                    for statement in statements.iter_mut() {
                        self.fix_statement(statement);
                    }
                }
            }
        }

        // Insert the `result` module import if it is used and not already
        // imported.
        if self.module_used {
            match &self.result_module {
                ResultModule::Existing(_) => (),
                ResultModule::Insert(name) => {
                    let import = result_module_import_statement(&name);
                    module.statements.insert(0, TargetGroup::Any(vec![import]));
                }
            }
        }
    }

    fn fix_statement(&mut self, statement: &mut UntypedStatement) {
        match statement {
            Statement::Function(f) => self.fix_expression(&mut f.body),
            Statement::TypeAlias(_)
            | Statement::CustomType(_)
            | Statement::ExternalFunction(_)
            | Statement::ExternalType(_)
            | Statement::Import(_)
            | Statement::ModuleConstant(_) => (),
        }
    }

    fn fix_expression(&mut self, expression: &mut UntypedExpr) {
        match expression {
            UntypedExpr::Try {
                value: try_value,
                then: try_then,
                pattern: try_pattern,
                location,
                ..
            } => {
                // We've seen a `try` expression, so the result module is needed.
                self.module_used = true;

                // Take ownership of the sub-expressions and patterns.
                let mut then = placeholder_expression();
                std::mem::swap(&mut then, &mut *try_then);
                let mut value = placeholder_expression();
                std::mem::swap(&mut value, &mut *try_value);
                let mut pattern = placeholder_pattern();
                std::mem::swap(&mut pattern, &mut *try_pattern);

                // Fix the sub-expressions.
                self.fix_expression(&mut value);
                self.fix_expression(&mut then);

                // Replace the `try` with `use`.
                let result_then = UntypedExpr::FieldAccess {
                    location: value.location(),
                    label: "then".into(),
                    container: Box::new(UntypedExpr::Var {
                        location: *location,
                        name: self.result_module.name().into(),
                    }),
                };
                let call = UntypedExpr::Call {
                    location: *location,
                    fun: Box::new(result_then),
                    arguments: vec![CallArg {
                        label: None,
                        location: value.location(),
                        implicit: false,
                        value,
                    }],
                };
                let pattern_location = pattern.location();
                let (assignment, include_pattern) = match &pattern {
                    Pattern::Var { name, .. } => (AssignName::Variable(name.clone()), false),
                    Pattern::Discard { name, .. } => (AssignName::Discard(name.clone()), false),
                    _ => (AssignName::Variable("x".into()), true),
                };
                let use_ = UntypedExpr::Use(Use {
                    location: *location,
                    call: Box::new(call),
                    assignments: vec![(assignment, pattern_location)],
                });
                let expressions = if include_pattern {
                    let assign = UntypedExpr::Assignment {
                        location: pattern_location,
                        value: Box::new(UntypedExpr::Var {
                            location: pattern_location,
                            name: "x".into(),
                        }),
                        pattern,
                        kind: AssignmentKind::Let,
                        annotation: None,
                    };
                    vec![use_, assign, then]
                } else {
                    vec![use_, then]
                };
                let mut sequence = UntypedExpr::Sequence {
                    location: *location,
                    expressions,
                };
                std::mem::swap(expression, &mut sequence);
            }

            UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Todo { .. } => (),

            UntypedExpr::Negate { value: e, .. }
            | UntypedExpr::FieldAccess { container: e, .. }
            | UntypedExpr::TupleIndex { tuple: e, .. }
            | UntypedExpr::Fn { body: e, .. }
            | UntypedExpr::Assignment { value: e, .. }
            | UntypedExpr::Use(Use { call: e, .. }) => self.fix_expression(e),

            UntypedExpr::Tuple {
                elems: expressions, ..
            }
            | UntypedExpr::Sequence { expressions, .. } => {
                for expression in expressions.iter_mut() {
                    self.fix_expression(expression);
                }
            }

            UntypedExpr::List { elements, tail, .. } => {
                for element in elements.iter_mut() {
                    self.fix_expression(element);
                }
                if let Some(tail) = tail {
                    self.fix_expression(tail);
                }
            }

            UntypedExpr::Call { fun, arguments, .. } => {
                self.fix_expression(fun);
                for argument in arguments.iter_mut() {
                    self.fix_expression(&mut argument.value);
                }
            }

            UntypedExpr::BinOp { left, right, .. } => {
                self.fix_expression(left);
                self.fix_expression(right);
            }

            UntypedExpr::PipeLine { expressions, .. } => {
                for expression in expressions.iter_mut() {
                    self.fix_expression(expression);
                }
            }

            UntypedExpr::Case {
                subjects, clauses, ..
            } => {
                for subject in subjects.iter_mut() {
                    self.fix_expression(subject);
                }
                for clause in clauses.iter_mut() {
                    self.fix_expression(&mut clause.then);
                }
            }

            UntypedExpr::BitString { segments, .. } => todo!(),

            UntypedExpr::RecordUpdate {
                spread, arguments, ..
            } => {
                self.fix_expression(&mut *spread.base);
                for argument in arguments.iter_mut() {
                    self.fix_expression(&mut argument.value);
                }
            }
        }
    }
}

fn placeholder_expression() -> UntypedExpr {
    UntypedExpr::Int {
        location: Default::default(),
        value: "0".into(),
    }
}

fn placeholder_pattern() -> UntypedPattern {
    UntypedPattern::Int {
        location: Default::default(),
        value: "0".into(),
    }
}

fn result_module_import_statement(name: &String) -> UntypedStatement {
    let as_name = match name.as_str() {
        "result" => None,
        _ => Some(name.clone().into()),
    };
    let import = Import {
        location: Default::default(),
        module: "gleam/result".into(),
        unqualified: vec![],
        as_name,
        package: (),
    };
    Statement::Import(import)
}

fn check_for_result_module_import(target: Target, module: &UntypedModule) -> ResultModule {
    let mut action = ResultModule::Insert("result".into());

    for group in &module.statements {
        match group {
            TargetGroup::Only(t, _) if *t != target => (),
            TargetGroup::Any(statements) | TargetGroup::Only(_, statements) => {
                for statement in statements {
                    if let Statement::Import(import) = statement {
                        action = check_import_for_result(import, action);
                    }
                }
            }
        }
    }

    action
}

fn check_import_for_result(import: &Import<()>, action: ResultModule) -> ResultModule {
    let import_name = import.variable_name();

    if import.module == "gleam/result" {
        return ResultModule::Existing(import_name.into());
    }

    if import_name == action.name() {
        return ResultModule::Insert("gleam_result".into());
    }

    action
}
