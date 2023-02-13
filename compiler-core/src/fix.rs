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

    fn fix_module(&self, module: &mut UntypedModule) {
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
                    module.statements.push(TargetGroup::Any(vec![import]));
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
        let mut name = action.into_name();
        name.push('_');
        return ResultModule::Insert(name);
    }

    action
}
