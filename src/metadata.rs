//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using the Cap'n Proto schema.

// TODO: remove
#![allow(unused)]

use crate::{
    fs::Writer,
    schema_capnp::{module, property, type_, type_constructor},
    typ::{self, TypeConstructor},
    GleamExpect,
};

pub struct ModuleBuilder<'a> {
    data: &'a typ::Module,
}

impl<'a> ModuleBuilder<'a> {
    pub fn new(data: &'a typ::Module) -> Self {
        Self { data }
    }

    pub fn write(mut self, mut writer: &mut Writer) -> crate::Result<()> {
        let mut message = capnp::message::Builder::new_default();

        let mut module = message.init_root::<module::Builder>();
        self.set_name(&mut module);
        self.set_module_types(&mut module);
        // add_module_values(data,&mut module);
        // add_module_accessors(data,&mut module);

        let result = capnp::serialize_packed::write_message(&mut writer, &message);
        writer.convert_err(result)
    }

    fn set_name(&mut self, module: &mut module::Builder) {
        let mut name = module.reborrow().init_name(self.data.name.len() as u32);
        for (i, s) in self.data.name.iter().enumerate() {
            name.set(i as u32, s);
        }
    }

    fn set_module_types(&mut self, module: &mut module::Builder) {
        let mut types = module.reborrow().init_types(self.data.types.len() as u32);
        for (i, (name, type_)) in self.data.types.iter().enumerate() {
            let mut property = types.reborrow().get(i as u32);
            property.set_key(name);
            self.build_type_constructor(property.init_value(), type_)
        }
    }

    fn build_type_constructor(
        &mut self,
        mut builder: type_constructor::Builder,
        type_: &TypeConstructor,
    ) {
        todo!()
    }
}
