use std::sync::Arc;

use crate::{
    ast::{Import, Statement, TargetGroup, UntypedModule, UntypedStatement},
    build::Target,
};

enum ResultModule {
    Existing(String),
    Insert(String),
}

#[derive(Debug, Clone)]
pub struct Fixer {
    result_module: String,
}

impl Fixer {
    pub fn fix(module: &mut UntypedModule, target: Target) -> UntypedModule {
        let result_module = match check_for_result_module_import(target, &module) {
            ResultModule::Existing(name) => name,
            ResultModule::Insert(name) => {
                let import = result_module_import_statement(&name);
                module
                    .statements
                    .push(TargetGroup::Only(target, vec![import]));
                name
            }
        };

        Self { result_module }.fix_module(module)
    }

    fn fix_module(&self, module: &mut crate::ast::Module<(), TargetGroup>) -> UntypedModule {
        todo!()
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
