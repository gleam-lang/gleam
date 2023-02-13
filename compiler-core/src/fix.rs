#[cfg(test)]
mod tests;

use std::path::Path;

use smol_str::SmolStr;
use strum::IntoEnumIterator;

use crate::{
    ast::{Function, Import, Statement, TargetGroup, UntypedExpr, UntypedModule, UntypedStatement},
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

enum ResultModule {
    Existing(String),
    Insert(String),
}

#[derive(Debug, Clone)]
pub struct Fixer {
    target: Target,
    result_module: String,
}

impl Fixer {
    /// Fix this module to remove use of the `try` keyword.
    /// Mutates the module in place.
    ///
    pub fn fix(module: &mut UntypedModule, target: Target) {
        let result_module = match check_for_result_module_import(target, &module) {
            ResultModule::Existing(name) => name,
            ResultModule::Insert(name) => {
                let import = result_module_import_statement(&name);
                module.statements.push(TargetGroup::Any(vec![import]));
                name
            }
        };

        Self {
            target,
            result_module,
        }
        .fix_module(module)
    }

    fn fix_module(&self, module: &mut UntypedModule) {
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
    }

    fn fix_statement(&self, statement: &mut UntypedStatement) {
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

    fn fix_expression(&self, expression: &mut UntypedExpr) {
        match expression {
            UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Todo { .. } => (),

            UntypedExpr::Sequence { expressions, .. } => todo!(),
            UntypedExpr::Fn { body, .. } => todo!(),
            UntypedExpr::List { elements, tail, .. } => todo!(),
            UntypedExpr::Call { fun, arguments, .. } => todo!(),
            UntypedExpr::BinOp { left, right, .. } => todo!(),
            UntypedExpr::PipeLine { expressions, .. } => todo!(),
            UntypedExpr::Assignment { value, .. } => todo!(),
            UntypedExpr::Try { value, then, .. } => unreachable!("try should have been removed"),
            UntypedExpr::Use(_) => todo!(),
            UntypedExpr::Case {
                subjects, clauses, ..
            } => todo!(),
            UntypedExpr::FieldAccess { container, .. } => todo!(),
            UntypedExpr::Tuple { elems, .. } => todo!(),
            UntypedExpr::TupleIndex { tuple, .. } => todo!(),
            UntypedExpr::BitString { segments, .. } => todo!(),
            UntypedExpr::RecordUpdate {
                spread, arguments, ..
            } => todo!(),
            UntypedExpr::Negate { value, .. } => todo!(),
        }
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
                        action = check_import_for_result(import);
                    }
                }
            }
        }
    }

    action
}

fn check_import_for_result(import: &Import<()>) -> ResultModule {
    todo!()
}
