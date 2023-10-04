use std::collections::HashMap;

use crate::{
    ast::{
        CallArg, Constant, Definition, SrcSpan, TypeAst, TypeAstConstructor, UntypedConstant,
        UntypedDefinition, UntypedExpr, UntypedImport, UntypedModule,
    },
    ast_folder::{TypeAstFolder, UntypedConstantFolder, UntypedExprFolder, UntypedModuleFolder},
};
use im::HashSet;
use smol_str::SmolStr;

#[derive(Debug, Default)]
pub struct Fixer {
    imports: HashMap<SmolStr, Imported>,
    local_types: HashSet<SmolStr>,
    local_values: HashSet<SmolStr>,
}

impl Fixer {
    pub fn fix(module: UntypedModule) -> UntypedModule {
        Self::default().fix_module(module)
    }

    fn fix_module(&mut self, module: UntypedModule) -> UntypedModule {
        for d in module.definitions.iter() {
            self.register_items(&d.definition);
        }

        // Determine which imported constructors are used as types and which are
        // used as values.
        let mut module = self.fold_module(module);

        for d in module.definitions.iter_mut() {
            if let Definition::Import(ref mut i) = d.definition {
                self.fix_import(i);
            }
        }

        module
    }

    fn register_unqualified_imported_constructors(&mut self, i: &UntypedImport) {
        for unqualified in &i.unqualified_values {
            let first_char = unqualified.name.chars().next().unwrap_or('a');
            if first_char.is_uppercase() {
                let data = Imported {
                    module: i.module.clone(),
                    used_as_type: false,
                    used_as_value: false,
                };
                let _ = self
                    .imports
                    .insert(unqualified.variable_name().into(), data);
            }
        }
    }

    fn fix_import(&mut self, import: &mut UntypedImport) {
        let mut types = vec![];
        let mut values = vec![];

        for unqualified in import.unqualified_values.drain(..) {
            match self.imports.get_mut(&unqualified.name) {
                Some(i) if i.module == import.module => {
                    if i.used_as_type {
                        types.push(unqualified.clone());
                    }
                    if i.used_as_value {
                        values.push(unqualified.clone());
                    }
                }
                _ => values.push(unqualified),
            }
        }

        import.unqualified_values = values;
        for t in types {
            import.unqualified_types.push(t);
        }
    }

    fn register_items(&mut self, definition: &UntypedDefinition) {
        match definition {
            Definition::Function(_) | Definition::ModuleConstant(_) => (),

            Definition::TypeAlias(a) => {
                let _ = self.local_types.insert(a.alias.clone());
            }

            Definition::CustomType(c) => {
                let _ = self.local_types.insert(c.name.clone());
                for c in &c.constructors {
                    let _ = self.local_values.insert(c.name.clone());
                }
            }

            Definition::Import(i) => self.register_unqualified_imported_constructors(i),
        }
    }
}

impl UntypedModuleFolder for Fixer {}

impl UntypedConstantFolder for Fixer {
    fn fold_constant_record(
        &mut self,
        location: SrcSpan,
        module: Option<SmolStr>,
        name: SmolStr,
        args: Vec<CallArg<UntypedConstant>>,
    ) -> UntypedConstant {
        if module.is_none() && !self.local_values.contains(&name) {
            if let Some(import) = self.imports.get_mut(&name) {
                import.used_as_value = true;
            }
        }
        Constant::Record {
            location,
            module,
            name,
            args,
            tag: (),
            typ: (),
            field_map: None,
        }
    }

    fn fold_constant_var(
        &mut self,
        location: SrcSpan,
        module: Option<SmolStr>,
        name: SmolStr,
    ) -> UntypedConstant {
        if module.is_none() && !self.local_values.contains(&name) {
            if let Some(import) = self.imports.get_mut(&name) {
                import.used_as_value = true;
            }
        }
        Constant::Var {
            location,
            module,
            name,
            constructor: None,
            typ: (),
        }
    }
}

impl UntypedExprFolder for Fixer {
    fn fold_var(&mut self, location: SrcSpan, name: SmolStr) -> UntypedExpr {
        if !self.local_values.contains(&name) {
            if let Some(import) = self.imports.get_mut(&name) {
                import.used_as_value = true;
            }
        }
        UntypedExpr::Var { location, name }
    }
}

impl TypeAstFolder for Fixer {
    fn fold_type_constructor(&mut self, constructor: TypeAstConstructor) -> TypeAst {
        if constructor.module.is_none() && !self.local_types.contains(&constructor.name) {
            if let Some(import) = self.imports.get_mut(&constructor.name) {
                import.used_as_type = true;
            }
        }

        TypeAst::Constructor(constructor)
    }
}

#[derive(Debug, Default)]
struct Imported {
    module: SmolStr,
    used_as_type: bool,
    used_as_value: bool,
}
