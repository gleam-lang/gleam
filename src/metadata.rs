//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using the Cap'n Proto schema.

// TODO: remove
#![allow(unused)]

use std::{cell::RefCell, collections::HashMap, sync::Arc};

use typ::ValueConstructorVariant;

use crate::{
    fs::Writer,
    schema_capnp::*,
    typ::{self, Type, TypeConstructor, TypeVar, ValueConstructor},
};

pub struct ModuleBuilder<'a> {
    data: &'a typ::Module,
    next_type_var_id: u16,
    type_var_id_map: HashMap<usize, u16>,
}

impl<'a> ModuleBuilder<'a> {
    pub fn new(data: &'a typ::Module) -> Self {
        Self {
            data,
            next_type_var_id: 0,
            type_var_id_map: HashMap::new(),
        }
    }

    pub fn write(mut self, mut writer: &mut Writer) -> crate::Result<()> {
        let mut message = capnp::message::Builder::new_default();

        let mut module = message.init_root::<module::Builder>();
        self.set_name(&mut module);
        self.set_module_types(&mut module);
        self.set_module_values(&mut module);
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

    fn set_module_values(&mut self, module: &mut module::Builder) {
        let mut values = module.reborrow().init_values(self.data.values.len() as u32);
        for (i, (name, value)) in self.data.values.iter().enumerate() {
            let mut property = values.reborrow().get(i as u32);
            property.set_key(name);
            self.build_value_constructor(property.init_value(), value)
        }
    }

    fn build_type_constructor(
        &mut self,
        mut builder: type_constructor::Builder,
        constructor: &TypeConstructor,
    ) {
        let type_builder = builder.reborrow().init_type();
        self.build_type(type_builder, &constructor.typ);
        self.build_types(
            builder.init_parameters(constructor.parameters.len() as u32),
            constructor.parameters.as_slice(),
        );
    }

    fn build_value_constructor(
        &mut self,
        mut builder: value_constructor::Builder,
        constructor: &ValueConstructor,
    ) {
        self.build_type(builder.reborrow().init_type(), &constructor.typ);
        self.build_value_constructor_variant(builder.init_variant(), &constructor.variant);
    }

    fn build_value_constructor_variant(
        &mut self,
        mut builder: value_constructor_variant::Builder,
        constructor: &ValueConstructorVariant,
    ) {
        todo!()
    }

    fn build_type(&mut self, mut builder: type_::Builder, type_: &Type) {
        match type_ {
            Type::Fn { args, retrn } => {
                let mut fun = builder.init_fn();
                self.build_types(
                    fun.reborrow().init_arguments(args.len() as u32),
                    args.as_slice(),
                );
                self.build_type(fun.init_return(), retrn.as_ref())
            }

            Type::App { name, args, .. } => {
                let mut app = builder.init_app();
                app.set_name(name.as_str());
                self.build_types(app.init_parameters(args.len() as u32), args.as_slice());
            }

            Type::Tuple { elems } => self.build_types(
                builder.init_tuple().init_elements(elems.len() as u32),
                elems.as_slice(),
            ),

            Type::Var { typ } => match &*typ.borrow() {
                TypeVar::Link { typ } => self.build_type(builder, &*typ),
                TypeVar::Generic { id } => self.build_type_var(builder.init_var(), *id),
                TypeVar::Unbound { id, .. } => crate::error::fatal_compiler_bug(
                    "Unexpected unbound var when serialising module metadata",
                ),
            },
        }
    }

    fn build_types(
        &mut self,
        mut builder: capnp::struct_list::Builder<type_::Owned>,
        types: &[Arc<Type>],
    ) {
        for (i, type_) in types.iter().enumerate() {
            self.build_type(builder.reborrow().get(i as u32), type_.as_ref());
        }
    }

    fn build_type_var(&mut self, mut builder: type_::var::Builder, id: usize) {
        let serialised_id = match self.type_var_id_map.get(&id) {
            Some(id) => *id,
            None => {
                let new_id = self.next_type_var_id;
                self.next_type_var_id += 1;
                let _ = self.type_var_id_map.insert(id, new_id);
                new_id
            }
        };
        builder.set_id(serialised_id);
    }
}
