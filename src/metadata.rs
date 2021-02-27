//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using the Cap'n Proto schema.

// TODO: remove
#![allow(unused)]

mod module_encoder;

#[cfg(test)]
mod tests;

pub use self::module_encoder::ModuleEncoder;

use crate::{
    schema_capnp as schema,
    typ::{
        self, AccessorsMap, FieldMap, Module, Type, TypeConstructor, ValueConstructor,
        ValueConstructorVariant,
    },
    Result,
};
use std::{collections::HashMap, io::BufRead, sync::Arc};

#[derive(Debug, Default)]
pub struct ModuleDecoder {
    next_type_var_id: usize,
    type_var_id_map: HashMap<usize, usize>,
}

impl ModuleDecoder {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn read(&mut self, reader: impl BufRead) -> Result<Module> {
        let message_reader =
            capnp::serialize_packed::read_message(reader, capnp::message::ReaderOptions::new())?;
        let module = message_reader.get_root::<schema::module::Reader<'_>>()?;

        Ok(Module {
            name: name(&module.get_name()?)?,
            types: self.module_types(&module)?,
            values: self.module_values(&module)?,
            accessors: self.module_accessors(&module)?,
        })
    }

    fn module_types(
        &mut self,
        reader: &schema::module::Reader<'_>,
    ) -> Result<HashMap<String, TypeConstructor>> {
        let types_reader = reader.get_types()?;
        let mut types = HashMap::with_capacity(types_reader.len() as usize);
        for prop in types_reader.into_iter() {
            let name = prop.get_key()?;
            let type_ = self.type_constructor(&prop.get_value()?)?;
            let _ = types.insert(name.to_string(), type_);
        }
        Ok(types)
    }

    fn type_constructor(
        &mut self,
        reader: &schema::type_constructor::Reader<'_>,
    ) -> Result<TypeConstructor> {
        let type_ = self.type_(&reader.get_type()?)?;
        let module = name(&reader.get_module()?)?;
        let reader = reader.get_parameters()?;
        let mut parameters = Vec::with_capacity(reader.len() as usize);
        for reader in reader.into_iter() {
            parameters.push(self.type_(&reader)?);
        }
        Ok(TypeConstructor {
            public: true,
            origin: Default::default(),
            module,
            parameters,
            typ: type_,
        })
    }

    fn types(
        &mut self,
        reader: &capnp::struct_list::Reader<'_, schema::type_::Owned>,
    ) -> Result<Vec<Arc<Type>>> {
        let mut types = Vec::with_capacity(reader.len() as usize);
        for reader in reader.into_iter() {
            types.push(self.type_(&reader)?);
        }
        Ok(types)
    }

    fn type_(&mut self, reader: &schema::type_::Reader<'_>) -> Result<Arc<Type>> {
        use schema::type_::Which;
        match reader.which()? {
            Which::App(reader) => self.type_app(&reader),
            Which::Fn(reader) => self.type_fn(&reader),
            Which::Tuple(reader) => self.type_tuple(&reader),
            Which::Var(reader) => self.type_var(&reader),
        }
    }

    fn type_app(&mut self, reader: &schema::type_::app::Reader<'_>) -> Result<Arc<Type>> {
        let module = name(&reader.get_module()?)?;
        let name = reader.get_name()?.to_string();
        let args = self.types(&reader.get_parameters()?)?;
        Ok(Arc::new(Type::App {
            public: true,
            module,
            name,
            args,
        }))
    }

    fn type_fn(&mut self, reader: &schema::type_::fn_::Reader<'_>) -> Result<Arc<Type>> {
        let retrn = self.type_(&reader.get_return()?)?;
        let args = self.types(&reader.get_arguments()?)?;
        Ok(Arc::new(Type::Fn { args, retrn }))
    }

    fn type_tuple(&mut self, reader: &schema::type_::tuple::Reader<'_>) -> Result<Arc<Type>> {
        let elems = self.types(&reader.get_elements()?)?;
        Ok(Arc::new(Type::Tuple { elems }))
    }

    fn type_var(&mut self, reader: &schema::type_::var::Reader<'_>) -> Result<Arc<Type>> {
        let serialized_id = reader.get_id() as usize;
        let id = match self.type_var_id_map.get(&serialized_id) {
            Some(id) => *id,
            None => {
                let new_id = self.next_type_var_id;
                self.next_type_var_id += 1;
                let _ = self.type_var_id_map.insert(serialized_id, new_id);
                new_id
            }
        };
        Ok(typ::generic_var(id))
    }

    fn module_values(
        &mut self,
        reader: &schema::module::Reader<'_>,
    ) -> Result<HashMap<String, ValueConstructor>> {
        let reader = reader.get_values()?;
        let mut values = HashMap::with_capacity(reader.len() as usize);
        for prop in reader.into_iter() {
            let name = prop.get_key()?;
            let value = self.value_constructor(&prop.get_value()?)?;
            let _ = values.insert(name.to_string(), value);
        }
        Ok(values)
    }

    // TODO: test
    fn value_constructor(
        &mut self,
        reader: &schema::value_constructor::Reader<'_>,
    ) -> Result<ValueConstructor> {
        let type_ = self.type_(&reader.get_type()?)?;
        let variant = self.value_constructor_variant(&reader.get_variant()?)?;
        Ok(ValueConstructor {
            public: true,
            origin: Default::default(),
            typ: type_,
            variant,
        })
    }

    fn value_constructor_variant(
        &self,
        reader: &schema::value_constructor_variant::Reader<'_>,
    ) -> Result<ValueConstructorVariant> {
        use schema::value_constructor_variant::Which;
        match reader.which()? {
            Which::ModuleConstant(reader) => todo!(),
            Which::ModuleFn(reader) => self.module_fn_variant(&reader),
            Which::Record(reader) => todo!(),
        }
    }

    // TODO: test
    fn module_fn_variant(
        &self,
        reader: &schema::value_constructor_variant::module_fn::Reader<'_>,
    ) -> Result<ValueConstructorVariant> {
        Ok(ValueConstructorVariant::ModuleFn {
            name: reader.get_name()?.to_string(),
            module: name(&reader.get_module()?)?,
            arity: reader.get_arity() as usize,
            field_map: self.field_map(&reader.get_field_map()?)?,
        })
    }

    fn field_map(
        &self,
        reader: &schema::option::Reader<'_, schema::field_map::Owned>,
    ) -> Result<Option<FieldMap>> {
        // TODO
        todo!()
    }

    fn module_accessors(
        &self,
        reader: &schema::module::Reader<'_>,
    ) -> Result<HashMap<String, AccessorsMap>> {
        // TODO
        Ok(HashMap::new())
    }
}

fn name(module: &capnp::text_list::Reader<'_>) -> Result<Vec<String>> {
    Ok(module
        .iter()
        .map(|s| s.map(String::from))
        .collect::<Result<_, _>>()?)
}
