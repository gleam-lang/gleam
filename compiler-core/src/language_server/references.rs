use std::{collections::HashMap, sync::Arc};

use ecow::EcoString;

use crate::{
    ast::{self, visit::Visit as _, SrcSpan},
    build,
    type_::{ModuleValueConstructor, Type, ValueConstructor, ValueConstructorVariant},
};

pub struct ReferenceSearcher<'a> {
    modules: &'a HashMap<EcoString, build::Module>,
    def_module: &'a str,
    def_location: SrcSpan,
    include_declaration: bool,
    current_module: &'a str,
    references: HashMap<&'a str, Vec<SrcSpan>>,
}

impl<'a> ReferenceSearcher<'a> {
    pub fn new(
        modules: &'a HashMap<EcoString, build::Module>,
        def_module: &'a str,
        def_location: SrcSpan,
        include_declaration: bool,
    ) -> ReferenceSearcher<'a> {
        Self {
            modules,
            def_module,
            def_location,
            include_declaration,
            current_module: "",
            references: Default::default(),
        }
    }

    pub fn references(mut self) -> HashMap<&'a str, Vec<SrcSpan>> {
        if self.include_declaration {
            self.current_module = self.def_module;
            self.add_reference(self.def_location);
        }

        for module in self.modules.values() {
            self.current_module = &module.name;
            self.visit_typed_module(&module.ast);
        }

        self.references
    }

    /// Adds location on the current module to references
    fn add_reference(&mut self, location: SrcSpan) {
        self.references
            .entry(self.current_module)
            .or_default()
            .push(location);
    }
}

impl<'ast> ast::visit::Visit<'ast> for ReferenceSearcher<'_> {
    fn visit_typed_expr_var(
        &mut self,
        location: &'ast SrcSpan,
        constructor: &'ast ValueConstructor,
        name: &'ast EcoString,
    ) {
        match &constructor.variant {
            ValueConstructorVariant::ModuleFn {
                module: def_module,
                location: def_location,
                ..
            } if def_module == self.def_module && *def_location == self.def_location => {
                self.add_reference(*location);
            }
            _ => {}
        }

        ast::visit::visit_typed_expr_var(self, location, constructor, name);
    }

    fn visit_typed_expr_module_select(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        label: &'ast EcoString,
        module_name: &'ast EcoString,
        module_alias: &'ast EcoString,
        constructor: &'ast ModuleValueConstructor,
    ) {
        match constructor {
            ModuleValueConstructor::Fn {
                location: def_location,
                module: def_module,
                ..
            } if def_module == self.def_module && *def_location == self.def_location => {
                self.add_reference(*location);
            }
            _ => {}
        }

        ast::visit::visit_typed_expr_module_select(
            self,
            location,
            typ,
            label,
            module_name,
            module_alias,
            constructor,
        )
    }
}
