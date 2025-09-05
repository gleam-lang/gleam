#![allow(clippy::unnecessary_wraps)] // Needed for macro

use capnp::{text, text_list};
use ecow::EcoString;
use itertools::Itertools;

use crate::{
    Result,
    ast::{
        BitArrayOption, BitArraySegment, CallArg, Constant, Publicity, SrcSpan, TypedConstant,
        TypedConstantBitArraySegment, TypedConstantBitArraySegmentOption,
    },
    build::Origin,
    line_numbers::{Character, LineNumbers},
    reference::{Reference, ReferenceKind, ReferenceMap},
    schema_capnp::{self as schema, *},
    type_::{
        self, AccessorsMap, Deprecation, FieldMap, ModuleInterface, Opaque, RecordAccessor,
        References, Type, TypeAliasConstructor, TypeConstructor, TypeValueConstructor,
        TypeValueConstructorField, TypeVariantConstructors, ValueConstructor,
        ValueConstructorVariant,
        expression::{Implementations, Purity},
    },
    uid::UniqueIdGenerator,
};
use std::{collections::HashMap, collections::HashSet, io::BufRead, sync::Arc};

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
            let name = $self.string(prop.get_key()?)?;
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
            name: self.string(reader.get_name()?)?,
            package: self.string(reader.get_package()?)?,
            is_internal: reader.get_is_internal(),
            origin: Origin::Src,
            values: read_hashmap!(reader.get_values()?, self, value_constructor),
            types: read_hashmap!(reader.get_types()?, self, type_constructor),
            types_value_constructors: read_hashmap!(
                reader.get_types_constructors()?,
                self,
                type_variants_constructors
            ),
            accessors: read_hashmap!(reader.get_accessors()?, self, accessors_map),
            line_numbers: self.line_numbers(&reader.get_line_numbers()?)?,
            src_path: self.str(reader.get_src_path()?)?.into(),
            warnings: vec![],
            minimum_required_version: self.version(&reader.get_required_version()?),
            type_aliases: read_hashmap!(reader.get_type_aliases()?, self, type_alias_constructor),
            documentation: self.string_list(reader.get_documentation()?)?,
            contains_echo: reader.get_contains_echo(),
            references: self.references(reader.get_references()?)?,
            inline_functions: HashMap::new(),
        })
    }

    fn string(&self, reader: text::Reader<'_>) -> Result<EcoString> {
        self.str(reader).map(|str| str.into())
    }

    fn string_list(&self, reader: text_list::Reader<'_>) -> Result<Vec<EcoString>> {
        let mut vec = Vec::with_capacity(reader.len() as usize);
        for reader in reader.into_iter() {
            vec.push(self.string(reader?)?);
        }
        Ok(vec)
    }

    fn str<'a>(&self, reader: text::Reader<'a>) -> Result<&'a str> {
        reader
            .to_str()
            .map_err(|_| capnp::Error::failed("String contains non-utf8 characters".into()).into())
    }

    fn references(&self, reader: references::Reader<'_>) -> Result<References> {
        Ok(References {
            imported_modules: self.string_set(reader.get_imported_modules()?)?,
            value_references: self.reference_map(reader.get_value_references()?)?,
            type_references: self.reference_map(reader.get_type_references()?)?,
        })
    }

    fn string_set(&self, reader: text_list::Reader<'_>) -> Result<HashSet<EcoString>> {
        let mut set = HashSet::with_capacity(reader.len() as usize);
        for reader in reader.into_iter() {
            let _ = set.insert(self.string(reader?)?);
        }
        Ok(set)
    }

    fn reference_map(
        &self,
        reader: capnp::struct_list::Reader<'_, reference_map::Owned>,
    ) -> Result<ReferenceMap> {
        let mut map = HashMap::with_capacity(reader.len() as usize);
        for prop in reader.into_iter() {
            let module = self.string(prop.get_module()?)?;
            let name = self.string(prop.get_name()?)?;
            let references = read_vec!(prop.get_references()?, self, reference);
            let _ = map.insert((module, name), references);
        }
        Ok(map)
    }

    fn reference(&self, reader: &reference::Reader<'_>) -> Result<Reference> {
        Ok(Reference {
            location: self.src_span(&reader.get_location()?)?,
            kind: self.reference_kind(&reader.get_kind()?)?,
        })
    }

    fn reference_kind(&self, reader: &reference_kind::Reader<'_>) -> Result<ReferenceKind> {
        use reference_kind::Which;
        Ok(match reader.which()? {
            Which::Qualified(_) => ReferenceKind::Qualified,
            Which::Unqualified(_) => ReferenceKind::Unqualified,
            Which::Import(_) => ReferenceKind::Import,
            Which::Definition(_) => ReferenceKind::Definition,
            Which::Alias(_) => ReferenceKind::Alias,
        })
    }

    fn type_constructor(
        &mut self,
        reader: &type_constructor::Reader<'_>,
    ) -> Result<TypeConstructor> {
        let type_ = self.type_(&reader.get_type()?)?;
        let deprecation = reader.get_deprecated()?;
        let deprecation = if deprecation.is_empty() {
            Deprecation::NotDeprecated
        } else {
            Deprecation::Deprecated {
                message: self.string(deprecation)?,
            }
        };
        Ok(TypeConstructor {
            publicity: self.publicity(reader.get_publicity()?)?,
            origin: self.src_span(&reader.get_origin()?)?,
            module: self.string(reader.get_module()?)?,
            parameters: read_vec!(reader.get_parameters()?, self, type_),
            type_,
            deprecation,
            documentation: self.optional_string(self.str(reader.get_documentation()?)?),
        })
    }

    fn type_alias_constructor(
        &mut self,
        reader: &type_alias_constructor::Reader<'_>,
    ) -> Result<TypeAliasConstructor> {
        let type_ = self.type_(&reader.get_type()?)?;
        let deprecation = reader.get_deprecation()?;
        let deprecation = if deprecation.is_empty() {
            Deprecation::NotDeprecated
        } else {
            Deprecation::Deprecated {
                message: self.string(deprecation)?,
            }
        };
        let parameters = read_vec!(&reader.get_parameters()?, self, type_);
        Ok(TypeAliasConstructor {
            publicity: self.publicity(reader.get_publicity()?)?,
            origin: self.src_span(&reader.get_origin()?)?,
            module: self.string(reader.get_module()?)?,
            type_,
            deprecation,
            documentation: self.optional_string(self.str(reader.get_documentation()?)?),
            arity: reader.get_arity() as usize,
            parameters,
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
        let package = self.string(reader.get_package()?)?;
        let module = self.string(reader.get_module()?)?;
        let name = self.string(reader.get_name()?)?;
        let arguments = read_vec!(&reader.get_parameters()?, self, type_);
        let inferred_variant = self.inferred_variant(&reader.get_inferred_variant()?)?;
        let publicity = self.publicity(reader.get_publicity()?)?;

        Ok(Arc::new(Type::Named {
            publicity,
            package,
            module,
            name,
            arguments,
            inferred_variant,
        }))
    }

    fn type_fn(&mut self, reader: &schema::type_::fn_::Reader<'_>) -> Result<Arc<Type>> {
        let return_ = self.type_(&reader.get_return()?)?;
        let arguments = read_vec!(&reader.get_arguments()?, self, type_);
        Ok(Arc::new(Type::Fn { arguments, return_ }))
    }

    fn type_tuple(&mut self, reader: &schema::type_::tuple::Reader<'_>) -> Result<Arc<Type>> {
        let elements = read_vec!(&reader.get_elements()?, self, type_);
        Ok(Arc::new(Type::Tuple { elements }))
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
        let opaque = if reader.get_opaque() {
            Opaque::Opaque
        } else {
            Opaque::NotOpaque
        };

        Ok(TypeVariantConstructors {
            variants,
            type_parameters_ids,
            opaque,
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
            name: self.string(reader.get_name()?)?,
            parameters: read_vec!(
                reader.get_parameters()?,
                self,
                type_value_constructor_parameter
            ),
            documentation: self.optional_string(self.str(reader.get_documentation()?)?),
        })
    }

    fn type_value_constructor_parameter(
        &mut self,
        reader: &type_value_constructor_parameter::Reader<'_>,
    ) -> Result<TypeValueConstructorField> {
        Ok(TypeValueConstructorField {
            type_: self.type_(&reader.get_type()?)?,
            label: self.optional_string(self.str(reader.get_label()?)?),
            documentation: self.optional_string(self.str(reader.get_documentation()?)?),
        })
    }

    fn inferred_variant(&mut self, reader: &inferred_variant::Reader<'_>) -> Result<Option<u16>> {
        use schema::inferred_variant::Which;
        match reader.which()? {
            Which::Unknown(_) => Ok(None),
            Which::Inferred(variant) => Ok(Some(variant)),
        }
    }

    fn value_constructor(
        &mut self,
        reader: &value_constructor::Reader<'_>,
    ) -> Result<ValueConstructor> {
        let type_ = self.type_(&reader.get_type()?)?;
        let variant = self.value_constructor_variant(&reader.get_variant()?)?;
        let publicity = self.publicity(reader.get_publicity()?)?;
        let deprecation = reader.get_deprecated()?;
        let deprecation = if deprecation.is_empty() {
            Deprecation::NotDeprecated
        } else {
            Deprecation::Deprecated {
                message: self.string(deprecation)?,
            }
        };
        Ok(ValueConstructor {
            deprecation,
            publicity,
            type_,
            variant,
        })
    }

    fn publicity(&self, reader: publicity::Reader<'_>) -> Result<Publicity> {
        match reader.which()? {
            publicity::Which::Public(()) => Ok(Publicity::Public),
            publicity::Which::Private(()) => Ok(Publicity::Private),
            publicity::Which::Internal(reader) => match reader?.which()? {
                option::Which::None(()) => Ok(Publicity::Internal {
                    attribute_location: None,
                }),
                option::Which::Some(reader) => Ok(Publicity::Internal {
                    attribute_location: Some(self.src_span(&reader?)?),
                }),
            },
        }
    }

    fn constant(&mut self, reader: &constant::Reader<'_>) -> Result<TypedConstant> {
        use constant::Which;
        match reader.which()? {
            Which::Int(reader) => Ok(self.constant_int(self.str(reader?)?)),
            Which::Float(reader) => Ok(self.constant_float(self.str(reader?)?)),
            Which::String(reader) => Ok(self.constant_string(self.str(reader?)?)),
            Which::Tuple(reader) => self.constant_tuple(&reader?),
            Which::List(reader) => self.constant_list(&reader),
            Which::Record(reader) => self.constant_record(&reader),
            Which::BitArray(reader) => self.constant_bit_array(&reader?),
            Which::Var(reader) => self.constant_var(&reader),
            Which::StringConcatenation(reader) => self.constant_string_concatenation(&reader),
        }
    }

    fn constant_int(&self, value: &str) -> TypedConstant {
        Constant::Int {
            location: Default::default(),
            value: value.into(),
            int_value: crate::parse::parse_int_value(value).expect("int value to parse as bigint"),
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
            type_,
        })
    }

    fn constant_record(&mut self, reader: &constant::record::Reader<'_>) -> Result<TypedConstant> {
        let type_ = self.type_(&reader.get_type()?)?;
        let tag = self.string(reader.get_tag()?)?;
        let arguments = read_vec!(reader.get_args()?, self, constant_call_arg);
        Ok(Constant::Record {
            location: Default::default(),
            module: Default::default(),
            name: Default::default(),
            arguments,
            tag,
            type_,
            field_map: None,
            record_constructor: None,
        })
    }

    fn constant_call_arg(
        &mut self,
        reader: &constant::Reader<'_>,
    ) -> Result<CallArg<TypedConstant>> {
        Ok(CallArg {
            implicit: None,
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
        let type_ = self.type_(&reader.get_type()?)?;
        let module = self.optional_string(self.str(reader.get_module()?)?);
        let name = reader.get_name()?;
        let constructor = self.value_constructor(&reader.get_constructor()?)?;
        Ok(Constant::Var {
            location: Default::default(),
            module: module.map(|module| (module, Default::default())),
            name: self.string(name)?,
            constructor: Some(Box::from(constructor)),
            type_,
        })
    }

    fn constant_string_concatenation(
        &mut self,
        reader: &constant::string_concatenation::Reader<'_>,
    ) -> Result<TypedConstant> {
        Ok(Constant::StringConcatenation {
            location: Default::default(),
            left: Box::new(self.constant(&reader.get_left()?)?),
            right: Box::new(self.constant(&reader.get_right()?)?),
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
            documentation: self.optional_string(self.str(reader.get_documentation()?)?),
            location: self.src_span(&reader.get_location()?)?,
            literal: self.constant(&reader.get_literal()?)?,
            module: self.string(reader.get_module()?)?,
            name: self.string(reader.get_name()?)?,
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
        let purity = match reader.get_purity()?.which()? {
            purity::Which::Pure(()) => Purity::Pure,
            purity::Which::TrustedPure(()) => Purity::TrustedPure,
            purity::Which::Impure(()) => Purity::Impure,
            purity::Which::Unknown(()) => Purity::Unknown,
        };

        Ok(ValueConstructorVariant::ModuleFn {
            name: self.string(reader.get_name()?)?,
            module: self.string(reader.get_module()?)?,
            arity: reader.get_arity() as usize,
            field_map: self.field_map(&reader.get_field_map()?)?,
            location: self.src_span(&reader.get_location()?)?,
            documentation: self.optional_string(self.str(reader.get_documentation()?)?),
            implementations: self.implementations(reader.get_implementations()?),
            external_erlang: self.optional_external(reader.get_external_erlang()?)?,
            external_javascript: self.optional_external(reader.get_external_javascript()?)?,
            purity,
        })
    }

    fn implementations(&self, reader: implementations::Reader<'_>) -> Implementations {
        Implementations {
            gleam: reader.get_gleam(),
            uses_erlang_externals: reader.get_uses_erlang_externals(),
            uses_javascript_externals: reader.get_uses_javascript_externals(),
            can_run_on_erlang: reader.get_can_run_on_erlang(),
            can_run_on_javascript: reader.get_can_run_on_javascript(),
        }
    }

    fn record(
        &self,
        reader: &value_constructor_variant::record::Reader<'_>,
    ) -> Result<ValueConstructorVariant> {
        Ok(ValueConstructorVariant::Record {
            name: self.string(reader.get_name()?)?,
            module: self.string(reader.get_module()?)?,
            arity: reader.get_arity(),
            variants_count: reader.get_constructors_count(),
            field_map: self.field_map(&reader.get_field_map()?)?,
            location: self.src_span(&reader.get_location()?)?,
            documentation: self.optional_string(self.str(reader.get_documentation()?)?),
            variant_index: reader.get_constructor_index(),
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
            publicity: self.publicity(reader.get_publicity()?)?,
            type_: self.type_(&reader.get_type()?)?,
            shared_accessors: read_hashmap!(&reader.get_shared_accessors()?, self, record_accessor),
            variant_specific_accessors: read_vec!(
                &reader.get_variant_specific_accessors()?,
                self,
                variant_specific_accessors
            ),
        })
    }

    fn variant_specific_accessors(
        &mut self,
        reader: &variant_specific_accessors::Reader<'_>,
    ) -> Result<HashMap<EcoString, RecordAccessor>> {
        Ok(read_hashmap!(
            &reader.get_accessors()?,
            self,
            record_accessor
        ))
    }

    fn record_accessor(&mut self, reader: &record_accessor::Reader<'_>) -> Result<RecordAccessor> {
        Ok(RecordAccessor {
            index: reader.get_index() as u64,
            label: self.string(reader.get_label()?)?,
            type_: self.type_(&reader.get_type()?)?,
            documentation: self.optional_string(self.str(reader.get_documentation()?)?),
        })
    }

    fn line_starts(&mut self, i: &u32) -> Result<u32> {
        Ok(*i)
    }

    fn line_numbers(&mut self, reader: &line_numbers::Reader<'_>) -> Result<LineNumbers> {
        Ok(LineNumbers {
            length: reader.get_length(),
            line_starts: read_vec!(reader.get_line_starts()?, self, line_starts),
            mapping: self.mapping(reader.get_mapping()?),
        })
    }

    fn mapping(
        &self,
        reader: capnp::struct_list::Reader<'_, character::Owned>,
    ) -> HashMap<usize, Character> {
        let mut map = HashMap::with_capacity(reader.len() as usize);
        for character in reader.into_iter() {
            let byte_index = character.get_byte_index() as usize;
            let length_utf8 = character.get_length_utf8();
            let length_utf16 = character.get_length_utf16();
            _ = map.insert(
                byte_index,
                Character {
                    length_utf16,
                    length_utf8,
                },
            )
        }
        map
    }

    fn version(&self, reader: &version::Reader<'_>) -> hexpm::version::Version {
        hexpm::version::Version::new(reader.get_major(), reader.get_minor(), reader.get_patch())
    }

    fn optional_external(
        &self,
        reader: option::Reader<'_, external::Owned>,
    ) -> Result<Option<(EcoString, EcoString)>> {
        match reader.which()? {
            option::Which::None(()) => Ok(None),
            option::Which::Some(reader) => {
                let reader = reader?;
                let module = self.string(reader.get_module()?)?;
                let function = self.string(reader.get_function()?)?;
                Ok(Some((module, function)))
            }
        }
    }
}
