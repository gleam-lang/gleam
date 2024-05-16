use std::collections::HashMap;

use ecow::EcoString;

use crate::{
    ast::{self, visit::Visit as _, SrcSpan},
    build,
    type_::ValueConstructorVariant,
};

pub struct ReferenceSearcher<'a> {
    modules: &'a HashMap<EcoString, build::Module>,
    def_module: &'a str,
    def_location: SrcSpan,
    current_module: &'a str,
    references: HashMap<&'a str, Vec<SrcSpan>>,
}

impl<'a> ReferenceSearcher<'a> {
    pub fn new(
        modules: &'a HashMap<EcoString, build::Module>,
        def_module: &'a str,
        def_location: SrcSpan,
    ) -> ReferenceSearcher<'a> {
        Self {
            modules,
            def_module,
            def_location,
            current_module: "",
            references: Default::default(),
        }
    }

    pub fn references(mut self) -> HashMap<&'a str, Vec<SrcSpan>> {
        for module in self.modules.values() {
            self.current_module = &module.name;
            self.visit_typed_module(&module.ast);
        }

        self.references
    }
}

impl<'ast> ast::visit::Visit<'ast> for ReferenceSearcher<'_> {
    fn visit_typed_expr_var(
        &mut self,
        location: &'ast SrcSpan,
        constructor: &'ast crate::type_::ValueConstructor,
        name: &'ast EcoString,
    ) {
        match &constructor.variant {
            ValueConstructorVariant::ModuleFn {
                module: def_module,
                location: def_location,
                ..
            } if def_module == self.def_module && *def_location == self.def_location => {
                self.references
                    .entry(self.current_module)
                    .or_default()
                    .push(*location);
            }
            _ => {}
        }

        ast::visit::visit_typed_expr_var(self, location, constructor, name);
    }
}
