#![allow(clippy::unnecessary_wraps)] // Needed for macro

use ecow::EcoString;
use itertools::Itertools;

use crate::{
    ast::{
        BitArrayOption, BitArraySegment, CallArg, Constant, SrcSpan, TypedConstant,
        TypedConstantBitArraySegment, TypedConstantBitArraySegmentOption,
    },
    build::Origin,
    schema_capnp::{self as schema, *},
    type_::{
        self, expression::Implementations, AccessorsMap, Deprecation, FieldMap, ModuleInterface,
        RecordAccessor, Type, TypeConstructor, TypeValueConstructor, TypeValueConstructorField,
        TypeVariantConstructors, ValueConstructor, ValueConstructorVariant,
    },
    uid::UniqueIdGenerator,
    Result,
};
use std::{collections::HashMap, io::BufRead, sync::Arc, u64};

macro_rules! read_vec {
    ($reader:expr, $self:expr, $method:ident) => {{
        let reader = $reader;
        let mut vec = Vec::with_capacity(reader.len() as usize);
        for reader in reader.into_iter() {
            let value = $self.$method(&reader)?;
            vec.push(value);
        }
        vec
    }};
}

macro_rules! read_hashmap {
    ($reader:expr, $self:expr, $method:ident) => {{
        let reader = $reader;
        let mut map = HashMap::with_capacity(reader.len() as usize);
        for prop in reader.into_iter() {
            let name = prop.get_key()?.into();
            let values = $self.$method(&prop.get_value()?.into())?;
            let _ = map.insert(name, values);
        }
        map
    }};
}

#[derive(Debug)]
pub struct ModuleDecoder {
    ids: UniqueIdGenerator,
    type_var_id_map: HashMap<u64, u64>,
}

impl ModuleDecoder {
    pub fn new(ids: UniqueIdGenerator) -> Self {
        Self {
            ids,
            type_var_id_map: Default::default(),
        }
    }

    pub fn read(&mut self, reader: impl BufRead) -> Result<ModuleInterface> {
        let message_reader =
            capnp::serialize_packed::read_message(reader, capnp::message::ReaderOptions::new())?;
        let reader = message_reader.get_root::<module::Reader<'_>>()?;

        Ok(ModuleInterface {
            name: reader.get_name()?.into(),
            package: reader.get_package()?.into(),
            contains_todo: reader.get_contains_todo(),
            origin: Origin::Src,
            values: read_hashmap!(reader.get_values()?, self, value_constructor),
            types: read_hashmap!(reader.get_types()?, self, type_constructor),
            types_value_constructors: read_hashmap!(
                reader.get_types_constructors()?,
                self,
                type_variants_constructors
            ),
            accessors: read_hashmap!(reader.get_accessors()?, self, accessors_map),
            unused_imports: read_vec!(reader.get_unused_imports()?, self, src_span),
        })
    }

    fn type_constructor(
        &mut self,
        reader: &type_constructor::Reader<'_>,
    ) -> Result<TypeConstructor> {
        let type_ = self.type_(&reader.get_type()?)?;
        let deprecation = match reader.get_deprecated()? {
            "" => Deprecation::NotDeprecated,
            message => Deprecation::Deprecated {
                message: message.into(),
            },
        };
        Ok(TypeConstructor {
            public: reader.get_public(),
            origin: Default::default(),
            module: reader.get_module()?.into(),
            parameters: read_vec!(reader.get_parameters()?, self, type_),
            typ: type_,
            deprecation,
        })
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
        let package = reader.get_package()?.into();
        let module = reader.get_module()?.into();
        let name = reader.get_name()?.into();
        let args = read_vec!(&reader.get_parameters()?, self, type_);
        Ok(Arc::new(Type::Named {
            public: true,
            package,
            module,
            name,
            args,
        }))
    }

    fn type_fn(&mut self, reader: &schema::type_::fn_::Reader<'_>) -> Result<Arc<Type>> {
        let retrn = self.type_(&reader.get_return()?)?;
        let args = read_vec!(&reader.get_arguments()?, self, type_);
        Ok(Arc::new(Type::Fn { args, retrn }))
    }

    fn type_tuple(&mut self, reader: &schema::type_::tuple::Reader<'_>) -> Result<Arc<Type>> {
        let elems = read_vec!(&reader.get_elements()?, self, type_);
        Ok(Arc::new(Type::Tuple { elems }))
    }

    fn type_var(&mut self, reader: &schema::type_::var::Reader<'_>) -> Result<Arc<Type>> {
        let serialized_id = reader.get_id();
        let id = self.get_or_insert_type_var_id(serialized_id);
        Ok(type_::generic_var(id))
    }

    fn get_or_insert_type_var_id(&mut self, id: u64) -> u64 {
        match self.type_var_id_map.get(&id) {
            Some(&id) => id,
            None => {
                let new_id = self.ids.next();
                let _ = self.type_var_id_map.insert(id, new_id);
                new_id
            }
        }
    }

    fn type_variants_constructors(
        &mut self,
        reader: &types_variant_constructors::Reader<'_>,
    ) -> Result<TypeVariantConstructors> {
        let variants = reader
            .get_variants()?
            .iter()
            .map(|r| self.type_value_constructor(&r))
            .try_collect()?;
        let type_parameters_ids = read_vec!(
            reader.get_type_parameters_ids()?,
            self,
            type_variant_constructor_type_parameter_id
        );
        Ok(TypeVariantConstructors {
            variants,
            type_parameters_ids,
        })
    }

    fn type_variant_constructor_type_parameter_id(&mut self, i: &u16) -> Result<u64> {
        Ok(self.get_or_insert_type_var_id(*i as u64))
    }

    fn type_value_constructor(
        &mut self,
        reader: &type_value_constructor::Reader<'_>,
    ) -> Result<TypeValueConstructor> {
        Ok(TypeValueConstructor {
            name: reader.get_name()?.into(),
            parameters: read_vec!(
                reader.get_parameters()?,
                self,
                type_value_constructor_parameter
            ),
        })
    }

    fn type_value_constructor_parameter(
        &mut self,
        reader: &type_value_constructor_parameter::Reader<'_>,
    ) -> Result<TypeValueConstructorField> {
        Ok(TypeValueConstructorField {
            type_: self.type_(&reader.get_type()?)?,
        })
    }

    fn value_constructor(
        &mut self,
        reader: &value_constructor::Reader<'_>,
    ) -> Result<ValueConstructor> {
        let type_ = self.type_(&reader.get_type()?)?;
        let variant = self.value_constructor_variant(&reader.get_variant()?)?;
        let public = reader.get_public();
        let deprecation = match reader.get_deprecated()? {
            "" => Deprecation::NotDeprecated,
            message => Deprecation::Deprecated {
                message: message.into(),
            },
        };
        Ok(ValueConstructor {
            deprecation,
            public,
            type_,
            variant,
        })
    }

    fn constant(&mut self, reader: &constant::Reader<'_>) -> Result<TypedConstant> {
        use constant::Which;
        match reader.which()? {
            Which::Int(reader) => Ok(self.constant_int(reader?)),
            Which::Float(reader) => Ok(self.constant_float(reader?)),
            Which::String(reader) => Ok(self.constant_string(reader?)),
            Which::Tuple(reader) => self.constant_tuple(&reader?),
            Which::List(reader) => self.constant_list(&reader),
            Which::Record(reader) => self.constant_record(&reader),
            Which::BitArray(reader) => self.constant_bit_array(&reader?),
            Which::Var(reader) => self.constant_var(&reader),
        }
    }

    fn constant_int(&self, value: &str) -> TypedConstant {
        Constant::Int {
            location: Default::default(),
            value: value.into(),
        }
    }

    fn constant_float(&self, value: &str) -> TypedConstant {
        Constant::Float {
            location: Default::default(),
            value: value.into(),
        }
    }

    fn constant_string(&self, value: &str) -> TypedConstant {
        Constant::String {
            location: Default::default(),
            value: value.into(),
        }
    }

    fn constant_tuple(
        &mut self,
        reader: &capnp::struct_list::Reader<'_, constant::Owned>,
    ) -> Result<TypedConstant> {
        Ok(Constant::Tuple {
            location: Default::default(),
            elements: read_vec!(reader, self, constant),
        })
    }

    fn constant_list(&mut self, reader: &constant::list::Reader<'_>) -> Result<TypedConstant> {
        let type_ = self.type_(&reader.get_type()?)?;
        Ok(Constant::List {
            location: Default::default(),
            elements: read_vec!(reader.get_elements()?, self, constant),
            typ: type_,
        })
    }

    fn constant_record(&mut self, reader: &constant::record::Reader<'_>) -> Result<TypedConstant> {
        let type_ = self.type_(&reader.get_typ()?)?;
        let tag = reader.get_tag()?.into();
        let args = read_vec!(reader.get_args()?, self, constant_call_arg);
        Ok(Constant::Record {
            location: Default::default(),
            module: Default::default(),
            name: Default::default(),
            args,
            tag,
            typ: type_,
            field_map: None,
        })
    }

    fn constant_call_arg(
        &mut self,
        reader: &constant::Reader<'_>,
    ) -> Result<CallArg<TypedConstant>> {
        Ok(CallArg {
            implicit: false,
            label: Default::default(),
            location: Default::default(),
            value: self.constant(reader)?,
        })
    }

    fn constant_bit_array(
        &mut self,
        reader: &capnp::struct_list::Reader<'_, bit_array_segment::Owned>,
    ) -> Result<TypedConstant> {
        Ok(Constant::BitArray {
            location: Default::default(),
            segments: read_vec!(reader, self, bit_array_segment),
        })
    }

    fn constant_var(&mut self, reader: &constant::var::Reader<'_>) -> Result<TypedConstant> {
        let type_ = self.type_(&reader.get_typ()?)?;
        let module = match reader.get_module()? {
            "" => None,
            module_str => Some(module_str),
        };
        let name = reader.get_name()?;
        let constructor = self.value_constructor(&reader.get_constructor()?)?;
        Ok(Constant::Var {
            location: Default::default(),
            module: module.map(EcoString::from),
            name: name.into(),
            constructor: Some(Box::from(constructor)),
            typ: type_,
        })
    }

    fn bit_array_segment(
        &mut self,
        reader: &bit_array_segment::Reader<'_>,
    ) -> Result<TypedConstantBitArraySegment> {
        Ok(BitArraySegment {
            location: Default::default(),
            type_: self.type_(&reader.get_type()?)?,
            value: Box::new(self.constant(&reader.get_value()?)?),
            options: read_vec!(reader.get_options()?, self, bit_array_segment_option),
        })
    }

    fn bit_array_segment_option(
        &mut self,
        reader: &bit_array_segment_option::Reader<'_>,
    ) -> Result<TypedConstantBitArraySegmentOption> {
        use bit_array_segment_option::Which;
        Ok(match reader.which()? {
            Which::Bytes(_) => BitArrayOption::Bytes {
                location: Default::default(),
            },
            Which::Integer(_) => BitArrayOption::Int {
                location: Default::default(),
            },
            Which::Float(_) => BitArrayOption::Float {
                location: Default::default(),
            },
            Which::Bits(_) => BitArrayOption::Bits {
                location: Default::default(),
            },
            Which::Utf8(_) => BitArrayOption::Utf8 {
                location: Default::default(),
            },
            Which::Utf16(_) => BitArrayOption::Utf16 {
                location: Default::default(),
            },
            Which::Utf32(_) => BitArrayOption::Utf32 {
                location: Default::default(),
            },
            Which::Utf8Codepoint(_) => BitArrayOption::Utf8Codepoint {
                location: Default::default(),
            },
            Which::Utf16Codepoint(_) => BitArrayOption::Utf16Codepoint {
                location: Default::default(),
            },
            Which::Utf32Codepoint(_) => BitArrayOption::Utf32Codepoint {
                location: Default::default(),
            },
            Which::Signed(_) => BitArrayOption::Signed {
                location: Default::default(),
            },
            Which::Unsigned(_) => BitArrayOption::Unsigned {
                location: Default::default(),
            },
            Which::Big(_) => BitArrayOption::Big {
                location: Default::default(),
            },
            Which::Little(_) => BitArrayOption::Little {
                location: Default::default(),
            },
            Which::Native(_) => BitArrayOption::Native {
                location: Default::default(),
            },
            Which::Size(reader) => BitArrayOption::Size {
                location: Default::default(),
                short_form: reader.get_short_form(),
                value: Box::new(self.constant(&reader.get_value()?)?),
            },
            Which::Unit(reader) => BitArrayOption::Unit {
                location: Default::default(),
                value: reader.get_value(),
            },
        })
    }

    fn value_constructor_variant(
        &mut self,
        reader: &value_constructor_variant::Reader<'_>,
    ) -> Result<ValueConstructorVariant> {
        use value_constructor_variant::Which;
        match reader.which()? {
            Which::ModuleConstant(reader) => self.module_constant_variant(&reader),
            Which::ModuleFn(reader) => self.module_fn_variant(&reader),
            Which::Record(reader) => self.record(&reader),
        }
    }

    fn module_constant_variant(
        &mut self,
        reader: &value_constructor_variant::module_constant::Reader<'_>,
    ) -> Result<ValueConstructorVariant> {
        Ok(ValueConstructorVariant::ModuleConstant {
            documentation: self.optional_string(reader.get_documentation()?),
            location: self.src_span(&reader.get_location()?)?,
            literal: self.constant(&reader.get_literal()?)?,
            module: reader.get_module()?.into(),
            implementations: self.implementations(reader.get_implementations()?),
        })
    }

    fn optional_string(&self, str: &str) -> Option<EcoString> {
        if str.is_empty() {
            None
        } else {
            Some(str.into())
        }
    }

    fn src_span(&self, reader: &src_span::Reader<'_>) -> Result<SrcSpan> {
        Ok(SrcSpan {
            start: reader.get_start(),
            end: reader.get_end(),
        })
    }

    fn module_fn_variant(
        &self,
        reader: &value_constructor_variant::module_fn::Reader<'_>,
    ) -> Result<ValueConstructorVariant> {
        Ok(ValueConstructorVariant::ModuleFn {
            name: reader.get_name()?.into(),
            module: reader.get_module()?.into(),
            arity: reader.get_arity() as usize,
            field_map: self.field_map(&reader.get_field_map()?)?,
            location: self.src_span(&reader.get_location()?)?,
            documentation: self.optional_string(reader.get_documentation()?),
            implementations: self.implementations(reader.get_implementations()?),
        })
    }

    fn implementations(&self, reader: implementations::Reader<'_>) -> Implementations {
        Implementations {
            gleam: reader.get_gleam(),
            uses_erlang_externals: reader.get_erlang(),
            uses_javascript_externals: reader.get_javascript(),
        }
    }

    fn record(
        &self,
        reader: &value_constructor_variant::record::Reader<'_>,
    ) -> Result<ValueConstructorVariant> {
        Ok(ValueConstructorVariant::Record {
            name: reader.get_name()?.into(),
            module: reader.get_module()?.into(),
            arity: reader.get_arity(),
            constructors_count: reader.get_constructors_count(),
            field_map: self.field_map(&reader.get_field_map()?)?,
            location: self.src_span(&reader.get_location()?)?,
            documentation: self.optional_string(reader.get_documentation()?),
            constructor_index: reader.get_constructor_index(),
        })
    }

    fn field_map(&self, reader: &option::Reader<'_, field_map::Owned>) -> Result<Option<FieldMap>> {
        use option::Which;
        Ok(match reader.which()? {
            Which::None(_) => None,
            Which::Some(reader) => Some({
                let reader = reader?;
                FieldMap {
                    arity: reader.get_arity(),
                    fields: read_hashmap!(&reader.get_fields()?, self, u32),
                }
            }),
        })
    }

    fn u32(&self, i: &boxed_u_int32::Reader<'_>) -> Result<u32> {
        Ok(i.get_value())
    }

    fn accessors_map(&mut self, reader: &accessors_map::Reader<'_>) -> Result<AccessorsMap> {
        Ok(AccessorsMap {
            public: true,
            type_: self.type_(&reader.get_type()?)?,
            accessors: read_hashmap!(&reader.get_accessors()?, self, record_accessor),
        })
    }

    fn record_accessor(&mut self, reader: &record_accessor::Reader<'_>) -> Result<RecordAccessor> {
        Ok(RecordAccessor {
            index: reader.get_index() as u64,
            label: reader.get_label()?.into(),
            type_: self.type_(&reader.get_type()?)?,
        })
    }
}
