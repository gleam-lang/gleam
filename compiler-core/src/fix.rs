#[cfg(test)]
mod tests;

use smol_str::SmolStr;
use std::collections::HashMap;
use vec1::vec1;

use camino::Utf8Path;

use crate::{
    ast::{
        Arg, ArgNames, Definition, ExternalFunction, Function, Statement, TargettedDefinition,
        TypeAst, UntypedExpr, UntypedFunction, UntypedModule,
    },
    build::Target,
    format::{Formatter, Intermediate},
    Error, Result,
};

pub fn parse_fix_and_format(
    assumed_target: Option<Target>,
    src: &SmolStr,
    path: &Utf8Path,
) -> Result<(String, bool)> {
    // Parse
    let parsed = crate::parse::parse_module(src).map_err(|error| Error::Parse {
        path: path.to_path_buf(),
        src: src.clone(),
        error,
    })?;
    let intermediate = Intermediate::from_extra(&parsed.extra, src);

    // Fix
    let (module, complete) = Fixer::fix(assumed_target, parsed.module);

    // Format
    let mut buffer = String::new();
    Formatter::with_comments(&intermediate)
        .module(&module)
        .pretty_print(80, &mut buffer)?;

    Ok((buffer, complete))
}

#[derive(Debug, Default)]
struct Replacement {
    both: Option<UntypedFunction>,
    erlang: Option<UntypedFunction>,
    javascript: Option<UntypedFunction>,
    no_change: bool,
}

impl Replacement {
    pub fn take_for(&mut self, target: Target) -> Option<TargettedDefinition> {
        self.both
            .take()
            .map(Definition::Function)
            .map(TargettedDefinition::Any)
            .or_else(|| {
                let function = match target {
                    Target::Erlang => self.erlang.take(),
                    Target::JavaScript => self.javascript.take(),
                }?;
                Some(TargettedDefinition::Only(
                    target,
                    Definition::Function(function),
                ))
            })
    }

    fn mut_for(&mut self, target: Target) -> &mut Option<UntypedFunction> {
        match target {
            Target::Erlang => &mut self.erlang,
            Target::JavaScript => &mut self.javascript,
        }
    }
}

#[derive(Debug, Default)]
pub struct Fixer {
    assumed_target: Option<Target>,
    replacements: HashMap<SmolStr, Replacement>,
    complete: bool,
}

impl Fixer {
    pub fn fix(assumed_target: Option<Target>, module: UntypedModule) -> (UntypedModule, bool) {
        let mut fixer = Self {
            assumed_target,
            replacements: HashMap::new(),
            complete: true,
        };
        let fixed = fixer.fix_module(module);
        (fixed, fixer.complete)
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
            TargettedDefinition::Only(t, Definition::ExternalFunction(f)) => {
                let replacement = self.replacements.get_mut(&f.name)?;
                if replacement.no_change {
                    let external_function = Definition::ExternalFunction(f);
                    Some(TargettedDefinition::Only(t, external_function))
                } else {
                    replacement.take_for(t)
                }
            }

            TargettedDefinition::Any(Definition::ExternalFunction(f)) => {
                let replacement = self.replacements.get_mut(&f.name)?;
                if replacement.no_change {
                    Some(TargettedDefinition::Any(Definition::ExternalFunction(f)))
                } else {
                    let function = replacement.both.take()?;
                    Some(TargettedDefinition::Any(Definition::Function(function)))
                }
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
        conditional_target: Option<Target>,
        external_function: &ExternalFunction<()>,
    ) {
        let implementation_target =
            match self.external_target(conditional_target, external_function) {
                Some(target) => target,
                // If we can't infer the target then we can't do anything with this
                // function so we mark the module as incomplete and move on.
                None => {
                    self.complete = false;
                    self.replacements
                        .entry(external_function.name.clone())
                        .or_default()
                        .no_change = true;
                    return;
                }
            };

        let function = self.make_function(conditional_target, external_function);

        let external = Some((
            external_function.module.clone(),
            external_function.fun.clone(),
        ));
        match implementation_target {
            Target::Erlang => function.external_erlang = external,
            Target::JavaScript => function.external_javascript = external,
        }
    }

    fn make_function(
        &mut self,
        conditional_target: Option<Target>,
        external_function: &ExternalFunction<()>,
    ) -> &mut Function<(), UntypedExpr> {
        let replacements = self
            .replacements
            .entry(external_function.name.clone())
            .or_default();
        let default = || function_from_external(external_function);

        let target = match conditional_target {
            Some(target) => target,
            None => return replacements.both.get_or_insert_with(default),
        };

        let other = replacements.mut_for(match target {
            Target::Erlang => Target::JavaScript,
            Target::JavaScript => Target::Erlang,
        });

        let existing_replacement = match other.take() {
            Some(f) => f,
            None => return replacements.mut_for(target).get_or_insert_with(default),
        };

        // TODO: ensure they have compatible types
        if has_compatible_types(&existing_replacement, external_function) {
            replacements.both.insert(existing_replacement)
        } else {
            _ = other.insert(existing_replacement);
            replacements.mut_for(target).get_or_insert(default())
        }
    }

    fn external_target(
        &self,
        target: Option<Target>,
        external_function: &ExternalFunction<()>,
    ) -> Option<Target> {
        let module = &external_function.module;
        if let Some(target) = target {
            Some(target)
        } else if module.ends_with(".jsx")
            || module.ends_with(".js")
            || module.ends_with(".tsx")
            || module.ends_with(".ts")
            || module.ends_with(".mjs")
            || module.contains('/')
        {
            Some(Target::JavaScript)
        } else if module.starts_with("Elixir.") {
            Some(Target::Erlang)
        } else {
            self.assumed_target
        }
    }
}

#[derive(Debug, Default)]
struct TypeComparison {
    left_variables: HashMap<SmolStr, usize>,
    right_variables: HashMap<SmolStr, usize>,
}

impl TypeComparison {
    fn reset(&mut self) {
        self.left_variables.clear();
        self.right_variables.clear();
    }

    fn equal(&mut self, left: &TypeAst, right: &TypeAst) -> bool {
        match (left, right) {
            (
                TypeAst::Constructor {
                    module: left_m,
                    name: left_n,
                    arguments: left_a,
                    ..
                },
                TypeAst::Constructor {
                    module: right_m,
                    name: right_n,
                    arguments: right_a,
                    ..
                },
            ) if left_m == right_m && left_n == right_n && left_a.len() == right_a.len() => {
                for (left, right) in left_a.iter().zip(right_a.iter()) {
                    if !self.equal(left, right) {
                        return false;
                    }
                }
                true
            }

            (
                TypeAst::Fn {
                    arguments: left_a,
                    return_: left_r,
                    ..
                },
                TypeAst::Fn {
                    arguments: right_a,
                    return_: right_r,
                    ..
                },
            ) if left_a.len() == right_a.len() && self.equal(left_r, right_r) => {
                for (left, right) in left_a.iter().zip(right_a.iter()) {
                    if !self.equal(left, right) {
                        return false;
                    }
                }
                true
            }

            (TypeAst::Var { name: left_n, .. }, TypeAst::Var { name: right_n, .. }) => {
                self.equal_parameters(left_n, right_n)
            }

            (TypeAst::Tuple { elems: left_e, .. }, TypeAst::Tuple { elems: right_e, .. })
                if left_e.len() == right_e.len() =>
            {
                for (left, right) in left_e.iter().zip(right_e.iter()) {
                    if !self.equal(left, right) {
                        return false;
                    }
                }
                true
            }

            (TypeAst::Hole { .. }, TypeAst::Hole { .. }) => true,

            _ => false,
        }
    }

    fn equal_parameters(&mut self, left: &SmolStr, right: &SmolStr) -> bool {
        let lefts = self.left_variables.len();
        let rights = self.right_variables.len();
        let left = self.left_variables.entry(left.clone()).or_insert(lefts);
        let right = self.right_variables.entry(right.clone()).or_insert(rights);
        left == right
    }

    fn optional_equal(&mut self, left: Option<&TypeAst>, right: &TypeAst) -> bool {
        let left = match left {
            None => return false,
            Some(left) => left,
        };
        self.equal(left, right)
    }
}

fn has_compatible_types(left: &UntypedFunction, right: &ExternalFunction<()>) -> bool {
    let mut compare = TypeComparison::default();
    if !compare.optional_equal(left.return_annotation.as_ref(), &right.return_) {
        return false;
    }

    for (left, right) in left.arguments.iter().zip(right.arguments.iter()) {
        compare.reset();
        if !compare.optional_equal(left.annotation.as_ref(), &right.annotation) {
            return false;
        }
    }

    true
}

pub fn function_from_external(external_function: &ExternalFunction<()>) -> UntypedFunction {
    let mut i: u8 = 96;
    Function {
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
        arguments: external_function
            .arguments
            .iter()
            .map(|arg| Arg {
                names: match arg.label.as_ref() {
                    Some(label) => ArgNames::NamedLabelled {
                        name: label.clone(),
                        label: label.clone(),
                    },
                    None => {
                        i += 1;
                        ArgNames::Named {
                            name: (i as char).to_string().into(),
                        }
                    }
                },
                location: arg.location,
                annotation: Some(arg.annotation.clone()),
                type_: (),
            })
            .collect(),
    }
}
