use crate::{
    ast::{
        Constant, SrcSpan, TypedConstant, TypedConstantBitStringSegment,
        TypedConstantBitStringSegmentOption,
    },
    io::Writer,
    schema_capnp::{self as schema, *},
    type_::{
        self, AccessorsMap, FieldMap, RecordAccessor, Type, TypeConstructor, TypeVar,
        ValueConstructor, ValueConstructorVariant,
    },
};
use std::{collections::HashMap, ops::Deref, sync::Arc};

#[derive(Debug)]
pub struct ModuleEncoder<'a> {
    data: &'a type_::Module,
    next_type_var_id: u64,
    type_var_id_map: HashMap<u64, u64>,
}

impl<'a> ModuleEncoder<'a> {
    pub fn new(data: &'a type_::Module) -> Self {
        Self {
            data,
            next_type_var_id: 0,
            type_var_id_map: HashMap::new(),
        }
    }

    pub fn write(mut self, mut writer: impl Writer) -> crate::Result<()> {
        let span = tracing::info_span!("metadata");
        let _enter = span.enter();

        let mut message = capnp::message::Builder::new_default();

        let mut module = message.init_root::<module::Builder<'_>>();
        self.set_name(&mut module);
        self.set_module_types(&mut module);
        self.set_module_values(&mut module);
        self.set_module_accessors(&mut module);
        module.set_package(&self.data.package);
        self.set_module_types_constructors(&mut module);

        let result = capnp::serialize_packed::write_message(&mut writer, &message);
        result.map_err(|e| writer.convert_err(e))
    }

    fn set_module_accessors(&mut self, module: &mut module::Builder<'_>) {
        tracing::trace!("Writing module metadata accessors");
        let mut builder = module
            .reborrow()
            .init_accessors(self.data.accessors.len() as u32);
        for (i, (key, map)) in self.data.accessors.iter().enumerate() {
            let mut property = builder.reborrow().get(i as u32);
            property.set_key(key);
            self.build_accessors_map(property.init_value(), map);
        }
    }

    fn build_accessors_map(
        &mut self,
        mut builder: accessors_map::Builder<'_>,
        accessors: &AccessorsMap,
    ) {
        self.build_type(builder.reborrow().init_type(), &accessors.type_);
        let mut builder = builder.init_accessors(accessors.accessors.len() as u32);
        for (i, (name, accessor)) in accessors.accessors.iter().enumerate() {
            let mut property = builder.reborrow().get(i as u32);
            property.set_key(name);
            self.build_record_accessor(property.init_value(), accessor)
        }
    }

    fn build_record_accessor(
        &mut self,
        mut builder: record_accessor::Builder<'_>,
        accessor: &RecordAccessor,
    ) {
        self.build_type(builder.reborrow().init_type(), &accessor.type_);
        builder.reborrow().set_label(&accessor.label);
        builder.set_index(accessor.index as u16);
    }

    fn set_name(&mut self, module: &mut module::Builder<'_>) {
        let mut name = module.reborrow().init_name(self.data.name.len() as u32);
        for (i, s) in self.data.name.iter().enumerate() {
            name.set(i as u32, s);
        }
    }

    fn build_module_name(&mut self, mut builder: capnp::text_list::Builder<'_>, module: &[String]) {
        for (i, s) in module.iter().enumerate() {
            builder.set(i as u32, s);
        }
    }

    fn set_module_types(&mut self, module: &mut module::Builder<'_>) {
        tracing::trace!("Writing module metadata types");
        let mut types = module.reborrow().init_types(self.data.types.len() as u32);
        for (i, (name, type_)) in self.data.types.iter().enumerate() {
            let mut property = types.reborrow().get(i as u32);
            property.set_key(name);
            self.build_type_constructor(property.init_value(), type_)
        }
    }

    fn set_module_types_constructors(&mut self, module: &mut module::Builder<'_>) {
        tracing::trace!("Writing module metadata types to constructors mapping");
        let mut types_constructors = module
            .reborrow()
            .init_types_constructors(self.data.types_constructors.len() as u32);
        for (i, (name, constructors)) in self.data.types_constructors.iter().enumerate() {
            let mut property = types_constructors.reborrow().get(i as u32);
            property.set_key(name);
            self.build_types_constructors_mapping(
                property.initn_value(constructors.len() as u32),
                constructors,
            )
        }
    }

    fn set_module_values(&mut self, module: &mut module::Builder<'_>) {
        tracing::trace!("Writing module metadata values");
        let mut values = module.reborrow().init_values(self.data.values.len() as u32);
        for (i, (name, value)) in self.data.values.iter().enumerate() {
            let mut property = values.reborrow().get(i as u32);
            property.set_key(name);
            self.build_value_constructor(property.init_value(), value)
        }
    }

    fn build_type_constructor(
        &mut self,
        mut builder: type_constructor::Builder<'_>,
        constructor: &TypeConstructor,
    ) {
        let type_builder = builder.reborrow().init_type();
        self.build_type(type_builder, &constructor.typ);
        self.build_types(
            builder
                .reborrow()
                .init_parameters(constructor.parameters.len() as u32),
            &constructor.parameters,
        );
        self.build_module_name(
            builder.init_module(constructor.module.len() as u32),
            &constructor.module,
        );
    }

    fn build_types_constructors_mapping(
        &mut self,
        mut builder: capnp::text_list::Builder<'_>,
        constructors: &[String],
    ) {
        for (i, s) in constructors.iter().enumerate() {
            builder.set(i as u32, s);
        }
    }

    fn build_value_constructor(
        &mut self,
        mut builder: value_constructor::Builder<'_>,
        constructor: &ValueConstructor,
    ) {
        self.build_type(builder.reborrow().init_type(), &constructor.type_);
        self.build_value_constructor_variant(builder.init_variant(), &constructor.variant);
    }

    fn build_src_span(&mut self, mut builder: src_span::Builder<'_>, span: SrcSpan) {
        builder.set_start(span.start as u16);
        builder.set_end(span.end as u16);
    }

    fn build_value_constructor_variant(
        &mut self,
        builder: value_constructor_variant::Builder<'_>,
        constructor: &ValueConstructorVariant,
    ) {
        match constructor {
            ValueConstructorVariant::LocalVariable { .. } => {
                panic!("Unexpected local variable value constructor in module interface",)
            }

            ValueConstructorVariant::ModuleConstant {
                literal,
                location,
                module,
            } => {
                let mut builder = builder.init_module_constant();
                self.build_src_span(builder.reborrow().init_location(), *location);
                self.build_constant(builder.reborrow().init_literal(), literal);
                builder.reborrow().set_module(module);
            }

            ValueConstructorVariant::Record {
                name,
                field_map,
                arity,
                location,
                module,
            } => {
                let mut builder = builder.init_record();
                builder.set_name(name);
                builder.set_module(module);
                builder.set_arity(*arity as u16);
                self.build_optional_field_map(builder.reborrow().init_field_map(), field_map);
                self.build_src_span(builder.init_location(), *location);
            }

            ValueConstructorVariant::ModuleFn {
                arity,
                field_map,
                module,
                name,
                location,
            } => {
                let mut builder = builder.init_module_fn();
                builder.set_name(name);
                self.build_optional_field_map(builder.reborrow().init_field_map(), field_map);
                {
                    let mut builder = builder.reborrow().init_module(module.len() as u32);
                    for (i, s) in module.iter().enumerate() {
                        builder.set(i as u32, s);
                    }
                }
                builder.set_arity(*arity as u16);
                self.build_src_span(builder.init_location(), *location);
            }
        }
    }

    fn build_optional_field_map(
        &mut self,
        mut builder: option::Builder<'_, field_map::Owned>,
        field_map: &Option<FieldMap>,
    ) {
        match field_map {
            Some(field_map) => self.build_field_map(builder.init_some(), field_map),
            None => builder.set_none(()),
        };
    }

    fn build_field_map(&mut self, mut builder: field_map::Builder<'_>, field_map: &FieldMap) {
        builder.set_arity(field_map.arity as u32);
        let mut builder = builder.init_fields(field_map.fields.len() as u32);
        for (i, (name, &position)) in field_map.fields.iter().enumerate() {
            let mut field = builder.reborrow().get(i as u32);
            field.set_key(name);
            field.init_value().set_value(position as u16);
        }
    }

    fn build_constant(&mut self, mut builder: constant::Builder<'_>, constant: &TypedConstant) {
        match constant {
            Constant::Int { value, .. } => builder.set_int(value),
            Constant::Float { value, .. } => builder.set_float(value),
            Constant::String { value, .. } => builder.set_string(value),

            Constant::Tuple { elements, .. } => {
                self.build_constants(builder.init_tuple(elements.len() as u32), elements)
            }

            Constant::List { elements, typ, .. } => {
                let mut builder = builder.init_list();
                self.build_constants(
                    builder.reborrow().init_elements(elements.len() as u32),
                    elements,
                );
                self.build_type(builder.init_type(), typ);
            }

            Constant::BitString { segments, .. } => {
                let mut builder = builder.init_bit_string(segments.len() as u32);
                for (i, segment) in segments.iter().enumerate() {
                    self.build_bit_string_segment(builder.reborrow().get(i as u32), segment);
                }
            }

            Constant::Record { args, tag, typ, .. } => {
                let mut builder = builder.init_record();
                {
                    let mut builder = builder.reborrow().init_args(args.len() as u32);
                    for (i, arg) in args.iter().enumerate() {
                        self.build_constant(builder.reborrow().get(i as u32), &arg.value);
                    }
                }
                builder.reborrow().set_tag(tag);
                self.build_type(builder.reborrow().init_typ(), typ);
            }
        }
    }

    fn build_constants(
        &mut self,
        mut builder: capnp::struct_list::Builder<'_, constant::Owned>,
        constant: &[TypedConstant],
    ) {
        for (i, constant) in constant.iter().enumerate() {
            self.build_constant(builder.reborrow().get(i as u32), constant);
        }
    }

    fn build_bit_string_segment(
        &mut self,
        mut builder: bit_string_segment::Builder<'_>,
        segment: &TypedConstantBitStringSegment,
    ) {
        self.build_constant(builder.reborrow().init_value(), &segment.value);
        {
            let mut builder = builder
                .reborrow()
                .init_options(segment.options.len() as u32);
            for (i, option) in segment.options.iter().enumerate() {
                self.build_bit_string_segment_option(builder.reborrow().get(i as u32), option);
            }
        }
        self.build_type(builder.init_type(), &segment.type_);
    }

    fn build_bit_string_segment_option(
        &mut self,
        mut builder: bit_string_segment_option::Builder<'_>,
        option: &TypedConstantBitStringSegmentOption,
    ) {
        use crate::ast::TypedConstantBitStringSegmentOption as Opt;
        match option {
            Opt::Binary { .. } => builder.set_binary(()),
            Opt::Int { .. } => builder.set_integer(()),
            Opt::Float { .. } => builder.set_float(()),
            Opt::BitString { .. } => builder.set_bitstring(()),
            Opt::Utf8 { .. } => builder.set_utf8(()),
            Opt::Utf16 { .. } => builder.set_utf16(()),
            Opt::Utf32 { .. } => builder.set_utf32(()),
            Opt::Utf8Codepoint { .. } => builder.set_utf8_codepoint(()),
            Opt::Utf16Codepoint { .. } => builder.set_utf16_codepoint(()),
            Opt::Utf32Codepoint { .. } => builder.set_utf32_codepoint(()),
            Opt::Signed { .. } => builder.set_signed(()),
            Opt::Unsigned { .. } => builder.set_unsigned(()),
            Opt::Big { .. } => builder.set_big(()),
            Opt::Little { .. } => builder.set_little(()),
            Opt::Native { .. } => builder.set_native(()),

            Opt::Size {
                value, short_form, ..
            } => {
                let mut builder = builder.init_size();
                self.build_constant(builder.reborrow().init_value(), value);
                builder.set_short_form(*short_form);
            }

            Opt::Unit { value, .. } => {
                let mut builder = builder.init_unit();
                builder.set_value(*value);
            }
        }
    }

    fn build_type(&mut self, builder: schema::type_::Builder<'_>, type_: &Type) {
        match type_ {
            Type::Fn { args, retrn } => {
                let mut fun = builder.init_fn();
                self.build_types(fun.reborrow().init_arguments(args.len() as u32), args);
                self.build_type(fun.init_return(), retrn)
            }

            Type::App {
                name, args, module, ..
            } => {
                let mut app = builder.init_app();
                app.set_name(name);
                self.build_types(app.reborrow().init_parameters(args.len() as u32), args);
                self.build_module_name(app.init_module(module.len() as u32), module);
            }

            Type::Tuple { elems } => self.build_types(
                builder.init_tuple().init_elements(elems.len() as u32),
                elems,
            ),

            Type::Var { type_: typ } => match typ.borrow().deref() {
                TypeVar::Link { type_: typ } => self.build_type(builder, typ),
                TypeVar::Generic { id } => self.build_type_var(builder.init_var(), *id),
                TypeVar::Unbound { .. } => {
                    panic!("Unexpected unbound var when serialising module metadata",)
                }
            },
        }
    }

    fn build_types(
        &mut self,
        mut builder: capnp::struct_list::Builder<'_, schema::type_::Owned>,
        types: &[Arc<Type>],
    ) {
        for (i, type_) in types.iter().enumerate() {
            self.build_type(builder.reborrow().get(i as u32), type_);
        }
    }

    fn build_type_var(&mut self, mut builder: schema::type_::var::Builder<'_>, id: u64) {
        let serialised_id = match self.type_var_id_map.get(&id) {
            Some(&id) => id,
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
