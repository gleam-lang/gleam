//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using the Cap'n Proto schema.

// TODO: remove
#![allow(unused)]

mod module_encoder;

#[cfg(test)]
mod tests;

use capnp::struct_list;

pub use self::module_encoder::ModuleEncoder;

use crate::{
    ast::{CallArg, Constant, TypedConstant},
    schema_capnp::*,
    typ::{
        self, AccessorsMap, FieldMap, Module, RecordAccessor, Type, TypeConstructor,
        ValueConstructor, ValueConstructorVariant,
    },
    Result,
};
use std::{collections::HashMap, io::BufRead, sync::Arc};

macro_rules! read_vec {
    ($reader:expr, $self:expr,$method:ident) => {{
        let reader = $reader;
        let mut vec = Vec::with_capacity(reader.len() as usize);
        for reader in reader.into_iter() {
            let value = $self.$method(&reader)?;
            vec.push(value);
        }
        vec
    }};
}

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
        let module = message_reader.get_root::<module::Reader<'_>>()?;

        Ok(Module {
            name: name(&module.get_name()?)?,
            types: self.module_types(&module)?,
            values: self.module_values(&module)?,
            accessors: self.module_accessors(&module)?,
        })
    }

    fn module_types(
        &mut self,
        reader: &module::Reader<'_>,
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
        reader: &type_constructor::Reader<'_>,
    ) -> Result<TypeConstructor> {
        let type_ = self.type_(&reader.get_type()?)?;
        let module = name(&reader.get_module()?)?;
        Ok(TypeConstructor {
            public: true,
            origin: Default::default(),
            module,
            parameters: read_vec!(reader.get_parameters()?, self, type_),
            typ: type_,
        })
    }

    fn type_(&mut self, reader: &type_::Reader<'_>) -> Result<Arc<Type>> {
        use type_::Which;
        match reader.which()? {
            Which::App(reader) => self.type_app(&reader),
            Which::Fn(reader) => self.type_fn(&reader),
            Which::Tuple(reader) => self.type_tuple(&reader),
            Which::Var(reader) => self.type_var(&reader),
        }
    }

    fn type_app(&mut self, reader: &type_::app::Reader<'_>) -> Result<Arc<Type>> {
        let module = name(&reader.get_module()?)?;
        let name = reader.get_name()?.to_string();
        let args = read_vec!(&reader.get_parameters()?, self, type_);
        Ok(Arc::new(Type::App {
            public: true,
            module,
            name,
            args,
        }))
    }

    fn type_fn(&mut self, reader: &type_::fn_::Reader<'_>) -> Result<Arc<Type>> {
        let retrn = self.type_(&reader.get_return()?)?;
        let args = read_vec!(&reader.get_arguments()?, self, type_);
        Ok(Arc::new(Type::Fn { args, retrn }))
    }

    fn type_tuple(&mut self, reader: &type_::tuple::Reader<'_>) -> Result<Arc<Type>> {
        let elems = read_vec!(&reader.get_elements()?, self, type_);
        Ok(Arc::new(Type::Tuple { elems }))
    }

    fn type_var(&mut self, reader: &type_::var::Reader<'_>) -> Result<Arc<Type>> {
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
        reader: &module::Reader<'_>,
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

    fn value_constructor(
        &mut self,
        reader: &value_constructor::Reader<'_>,
    ) -> Result<ValueConstructor> {
        let type_ = self.type_(&reader.get_type()?)?;
        let variant = self.value_constructor_variant(&reader.get_variant()?)?;
        Ok(ValueConstructor {
            public: true,
            origin: Default::default(),
            type_,
            variant,
        })
    }

    fn constant(&mut self, reader: &constant::Reader<'_>) -> Result<TypedConstant> {
        use constant::Which;
        match reader.which()? {
            Which::Int(reader) => self.constant_int(&reader?),
            Which::Float(reader) => self.constant_float(&reader?),
            Which::String(reader) => self.constant_string(&reader?),
            Which::Tuple(reader) => self.constant_tuple(&reader?),
            Which::List(reader) => self.constant_list(&reader),
            Which::Record(reader) => self.constant_record(&reader),
            Which::BitString(reader) => self.constant_bit_string(&reader?),
        }
    }

    // TODO: test
    fn constant_int(&self, value: &str) -> Result<TypedConstant> {
        Ok(Constant::Int {
            location: Default::default(),
            value: value.to_string(),
        })
    }

    // TODO: test
    fn constant_float(&self, value: &str) -> Result<TypedConstant> {
        Ok(Constant::Float {
            location: Default::default(),
            value: value.to_string(),
        })
    }

    // TODO: test
    fn constant_string(&self, value: &str) -> Result<TypedConstant> {
        Ok(Constant::String {
            location: Default::default(),
            value: value.to_string(),
        })
    }

    // TODO: test
    fn constant_tuple(
        &mut self,
        reader: &capnp::struct_list::Reader<'_, constant::Owned>,
    ) -> Result<TypedConstant> {
        Ok(Constant::Tuple {
            location: Default::default(),
            elements: read_vec!(reader, self, constant),
        })
    }

    // TODO: test
    fn constant_list(&mut self, reader: &constant::list::Reader<'_>) -> Result<TypedConstant> {
        let type_ = self.type_(&reader.get_type()?)?;
        Ok(Constant::List {
            location: Default::default(),
            elements: read_vec!(reader.get_elements()?, self, constant),
            typ: type_,
        })
    }

    // TODO: test
    fn constant_record(&mut self, reader: &constant::record::Reader<'_>) -> Result<TypedConstant> {
        let type_ = self.type_(&reader.get_typ()?)?;
        let tag = reader.get_tag()?.to_string();
        let args = read_vec!(reader.get_args()?, self, constant_call_arg);
        Ok(Constant::Record {
            location: Default::default(),
            module: Default::default(),
            name: Default::default(),
            args,
            tag,
            typ: type_,
        })
    }

    fn constant_call_arg(
        &mut self,
        reader: &constant::Reader<'_>,
    ) -> Result<CallArg<TypedConstant>> {
        Ok(CallArg {
            label: Default::default(),
            location: Default::default(),
            value: self.constant(reader)?,
        })
    }

    // TODO: test
    fn constant_bit_string(
        &self,
        reader: &capnp::struct_list::Reader<'_, bit_string_segment::Owned>,
    ) -> Result<TypedConstant> {
        todo!()
    }

    // TODO: test
    fn value_constructor_variant(
        &mut self,
        reader: &value_constructor_variant::Reader<'_>,
    ) -> Result<ValueConstructorVariant> {
        use value_constructor_variant::Which;
        match reader.which()? {
            Which::ModuleConstant(reader) => self.module_constant_variant(&reader?),
            Which::ModuleFn(reader) => self.module_fn_variant(&reader),
            Which::Record(reader) => self.record(&reader),
        }
    }

    fn module_constant_variant(
        &mut self,
        reader: &constant::Reader<'_>,
    ) -> Result<ValueConstructorVariant> {
        Ok(ValueConstructorVariant::ModuleConstant {
            literal: self.constant(reader)?,
        })
    }

    fn module_fn_variant(
        &self,
        reader: &value_constructor_variant::module_fn::Reader<'_>,
    ) -> Result<ValueConstructorVariant> {
        Ok(ValueConstructorVariant::ModuleFn {
            name: reader.get_name()?.to_string(),
            module: name(&reader.get_module()?)?,
            arity: reader.get_arity() as usize,
            field_map: self.field_map(&reader.get_field_map()?)?,
        })
    }

    fn record(
        &self,
        reader: &value_constructor_variant::record::Reader<'_>,
    ) -> Result<ValueConstructorVariant> {
        Ok(ValueConstructorVariant::Record {
            name: reader.get_name()?.to_string(),
            arity: reader.get_arity() as usize,
            field_map: self.field_map(&reader.get_field_map()?)?,
        })
    }

    fn field_map(&self, reader: &option::Reader<'_, field_map::Owned>) -> Result<Option<FieldMap>> {
        use option::Which;
        Ok(match reader.which()? {
            Which::None(_) => None,
            Which::Some(reader) => Some({
                let reader = reader?;
                FieldMap {
                    arity: reader.get_arity() as usize,
                    fields: self.field_map_fields(&reader.get_fields()?)?,
                }
            }),
        })
    }

    fn field_map_fields(
        &self,
        reader: &capnp::struct_list::Reader<'_, property::Owned<boxed_u_int16::Owned>>,
    ) -> Result<HashMap<String, usize>> {
        let mut fields = HashMap::with_capacity(reader.len() as usize);
        for prop in reader.into_iter() {
            let name = prop.get_key()?;
            let index = prop.get_value()?.get_value();
            let _ = fields.insert(name.to_string(), index as usize);
        }
        Ok(fields)
    }

    fn module_accessors(
        &mut self,
        reader: &module::Reader<'_>,
    ) -> Result<HashMap<String, AccessorsMap>> {
        let reader = reader.get_accessors()?;
        let mut accessors = HashMap::with_capacity(reader.len() as usize);
        for prop in reader.into_iter() {
            let name = prop.get_key()?;
            let accessor = self.accessors_map(&prop.get_value()?)?;
            let _ = accessors.insert(name.to_string(), accessor);
        }
        Ok(accessors)
    }

    fn accessors_map(&mut self, reader: &accessors_map::Reader<'_>) -> Result<AccessorsMap> {
        Ok(AccessorsMap {
            public: true,
            type_: self.type_(&reader.get_type()?)?,
            accessors: self.accessors(&reader.get_accessors()?)?,
        })
    }

    fn accessors(
        &mut self,
        reader: &capnp::struct_list::Reader<'_, property::Owned<record_accessor::Owned>>,
    ) -> Result<HashMap<String, RecordAccessor>> {
        let mut accessors = HashMap::with_capacity(reader.len() as usize);
        for prop in reader.into_iter() {
            let name = prop.get_key()?;
            let accessor = self.record_accessor(&prop.get_value()?)?;
            let _ = accessors.insert(name.to_string(), accessor);
        }
        Ok(accessors)
    }

    fn record_accessor(&mut self, reader: &record_accessor::Reader<'_>) -> Result<RecordAccessor> {
        Ok(RecordAccessor {
            index: reader.get_index() as u64,
            label: reader.get_label()?.to_string(),
            type_: self.type_(&reader.get_type()?)?,
        })
    }
}

fn name(module: &capnp::text_list::Reader<'_>) -> Result<Vec<String>> {
    Ok(module
        .iter()
        .map(|s| s.map(String::from))
        .collect::<Result<_, _>>()?)
}
