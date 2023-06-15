#[cfg(test)]
mod tests;

use smol_str::SmolStr;
use std::{
    collections::{HashMap, HashSet},
    path::Path,
};
use vec1::vec1;

use crate::{
    ast::{
        Definition, ExternalFunction, Function, Statement, TargettedDefinition, UntypedExpr,
        UntypedFunction, UntypedModule,
    },
    build::Target,
    format::{Formatter, Intermediate},
    Error, Result,
};

pub fn parse_fix_and_format(src: &SmolStr, path: &Path) -> Result<String> {
    // Parse
    let parsed = crate::parse::parse_module(src).map_err(|error| Error::Parse {
        path: path.to_path_buf(),
        src: src.clone(),
        error,
    })?;
    let intermediate = Intermediate::from_extra(&parsed.extra, src);

    // Fix
    let module = Fixer::fix(parsed.module);

    // Format
    let mut buffer = String::new();
    Formatter::with_comments(&intermediate)
        .module(&module)
        .pretty_print(80, &mut buffer)?;

    Ok(buffer)
}

#[derive(Debug, Default)]
pub struct Fixer {
    replacements: HashMap<SmolStr, UntypedFunction>,
    replaced: HashSet<SmolStr>,
}

impl Fixer {
    pub fn fix(module: UntypedModule) -> UntypedModule {
        Self::default().fix_module(module)
    }

    fn fix_module(&mut self, mut module: UntypedModule) -> UntypedModule {
        for definition in &module.definitions {
            self.register(definition);
        }

        module.definitions = module
            .definitions
            .into_iter()
            .flat_map(|definition| self.replace(definition))
            .collect();

        module
    }

    fn replace(&mut self, definition: TargettedDefinition) -> Option<TargettedDefinition> {
        match definition {
            TargettedDefinition::Only(_, Definition::ExternalFunction(f))
            | TargettedDefinition::Any(Definition::ExternalFunction(f)) => {
                if !self.replaced.insert(f.name.clone()) {
                    return None;
                }
                let definition = self.replacements.remove(&f.name)?;
                Some(TargettedDefinition::Any(Definition::Function(definition)))
            }

            _ => Some(definition),
        }
    }

    fn register(&mut self, definition: &TargettedDefinition) {
        match definition {
            TargettedDefinition::Any(Definition::ExternalFunction(external_function)) => {
                self.convert_function(None, external_function);
            }

            TargettedDefinition::Only(target, Definition::ExternalFunction(external_function)) => {
                self.convert_function(Some(*target), external_function);
            }

            _ => (),
        }
    }

    fn convert_function(
        &mut self,
        target: Option<Target>,
        external_function: &ExternalFunction<()>,
    ) {
        let target = self.external_target(target, external_function);
        let function = self.make_function(external_function);

        let external = Some((
            external_function.module.clone(),
            external_function.fun.clone(),
        ));
        match target {
            Some(Target::Erlang) => function.external_erlang = external,
            Some(Target::JavaScript) => function.external_javascript = external,
            None => todo!("Handle unknown"),
        }
    }

    fn make_function(
        &mut self,
        external_function: &ExternalFunction<()>,
    ) -> &mut Function<(), UntypedExpr> {
        self.replacements
            .entry(external_function.name.clone())
            .or_insert_with(|| Function {
                location: external_function.location,
                end_position: external_function.location.end,
                name: external_function.name.clone(),
                body: vec1![Statement::Expression(UntypedExpr::Placeholder {
                    location: external_function.location,
                })],
                public: external_function.public,
                return_annotation: Some(external_function.return_.clone()),
                return_type: (),
                documentation: None,
                external_erlang: None,
                external_javascript: None,
                // TODO: arguments
                arguments: vec![],
            })
    }

    fn external_target(
        &self,
        target: Option<Target>,
        external_function: &ExternalFunction<()>,
    ) -> Option<Target> {
        if let Some(target) = target {
            Some(target)
        } else if external_function.module.ends_with(".jsx") {
            Some(Target::JavaScript)
        } else if external_function.module.ends_with(".js") {
            Some(Target::JavaScript)
        } else if external_function.module.ends_with(".tsx") {
            Some(Target::JavaScript)
        } else if external_function.module.ends_with(".ts") {
            Some(Target::JavaScript)
        } else if external_function.module.ends_with(".mjs") {
            Some(Target::JavaScript)
        } else if external_function.module.contains("/") {
            Some(Target::JavaScript)
        } else if external_function.module.starts_with("Elixir.") {
            Some(Target::Erlang)
        } else {
            None
        }
    }
}
