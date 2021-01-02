use std::{cell::RefCell, collections::HashMap, sync::Arc};

use typ::{AccessorsMap, RecordAccessor};

use crate::{
    ast::{
        BitStringSegmentOption, Constant, TypedConstant, TypedConstantBitStringSegment,
        TypedConstantBitStringSegmentOption, TypedExprBitStringSegment,
    },
    fs::Writer,
    schema_capnp::*,
    typ::{
        self, FieldMap, Type, TypeConstructor, TypeVar, ValueConstructor, ValueConstructorVariant,
    },
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

    pub fn write(mut self, mut writer: impl Writer) -> crate::Result<()> {
        let mut message = capnp::message::Builder::new_default();

        let mut module = message.init_root::<module::Builder>();
        self.set_name(&mut module);
        self.set_module_types(&mut module);
        self.set_module_values(&mut module);
        self.set_module_accessors(&mut module);

        let result = capnp::serialize_packed::write_message(&mut writer, &message);
        writer.convert_err(result)
    }

    fn set_module_accessors(&mut self, module: &mut module::Builder) {
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
        mut builder: accessors_map::Builder,
        accessors: &AccessorsMap,
    ) {
        self.build_type(builder.reborrow().init_type(), accessors.typ.as_ref());
        let mut builder = builder.init_accessors(accessors.accessors.len() as u32);
        for (i, (name, accessor)) in accessors.accessors.iter().enumerate() {
            let mut property = builder.reborrow().get(i as u32);
            property.set_key(name);
            self.build_record_accessor(property.init_value(), accessor)
        }
    }

    fn build_record_accessor(
        &mut self,
        mut builder: record_accessor::Builder,
        accessor: &RecordAccessor,
    ) {
        self.build_type(builder.reborrow().init_type(), accessor.typ.as_ref());
        builder.set_index(accessor.index as u16);
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
        match constructor {
            ValueConstructorVariant::LocalVariable => crate::error::fatal_compiler_bug(
                "Unexpected local variable value constructor in module interface",
            ),

            ValueConstructorVariant::ModuleConstant { literal } => {
                self.build_constant(builder.init_module_constant(), literal)
            }

            ValueConstructorVariant::Record { name, field_map } => {
                let mut builder = builder.init_record();
                builder.set_name(name);
                self.build_optional_field_map(builder.init_field_map(), field_map);
            }

            ValueConstructorVariant::ModuleFn {
                arity,
                field_map,
                module,
                name,
            } => {
                let mut builder = builder.init_module_fn();
                builder.set_name(name);
                self.build_optional_field_map(builder.reborrow().init_field_map(), field_map);
                {
                    let mut builder = builder.reborrow().init_module(self.data.name.len() as u32);
                    for (i, s) in module.iter().enumerate() {
                        builder.set(i as u32, s);
                    }
                }
                builder.set_arity(*arity as u16);
            }
        }
    }

    fn build_optional_field_map(
        &mut self,
        mut builder: option::Builder<field_map::Owned>,
        field_map: &Option<FieldMap>,
    ) {
        match field_map {
            Some(field_map) => self.build_field_map(builder.init_some(), field_map),
            None => builder.set_none(()),
        };
    }

    fn build_field_map(&mut self, mut builder: field_map::Builder, field_map: &FieldMap) {
        builder.set_arity(field_map.arity as u32);
        let mut builder = builder.init_fields(field_map.fields.len() as u32);
        for (i, (name, position)) in field_map.fields.iter().enumerate() {
            let mut field = builder.reborrow().get(i as u32);
            field.set_key(name);
            field.init_value().set_value(*position as u16);
        }
    }

    fn build_constant(&mut self, mut builder: constant::Builder, constant: &TypedConstant) {
        match constant {
            Constant::Int { value, .. } => builder.set_int(value),
            Constant::Float { value, .. } => builder.set_float(value),
            Constant::String { value, .. } => builder.set_string(value),

            Constant::Tuple { elements, .. } => self.build_constants(
                builder.init_tuple(elements.len() as u32),
                elements.as_slice(),
            ),

            Constant::List { elements, typ, .. } => {
                let mut builder = builder.init_list();
                self.build_constants(
                    builder.reborrow().init_elements(elements.len() as u32),
                    elements.as_slice(),
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
        mut builder: capnp::struct_list::Builder<constant::Owned>,
        constant: &[TypedConstant],
    ) {
        for (i, constant) in constant.iter().enumerate() {
            self.build_constant(builder.reborrow().get(i as u32), constant);
        }
    }

    fn build_bit_string_segment(
        &mut self,
        mut builder: bit_string_segment::Builder,
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
        self.build_type(builder.init_type(), &segment.typ);
    }

    fn build_bit_string_segment_option(
        &mut self,
        mut builder: bit_string_segment_option::Builder,
        option: &TypedConstantBitStringSegmentOption,
    ) {
        use crate::ast::TypedConstantBitStringSegmentOption as Opt;
        match option {
            Opt::Binary { .. } => builder.set_binary(()),
            Opt::Integer { .. } => builder.set_integer(()),
            Opt::Float { .. } => builder.set_float(()),
            Opt::BitString { .. } => builder.set_bitstring(()),
            Opt::UTF8 { .. } => builder.set_utf8(()),
            Opt::UTF16 { .. } => builder.set_utf16(()),
            Opt::UTF32 { .. } => builder.set_utf32(()),
            Opt::UTF8Codepoint { .. } => builder.set_utf8_codepoint(()),
            Opt::UTF16Codepoint { .. } => builder.set_utf16_codepoint(()),
            Opt::UTF32Codepoint { .. } => builder.set_utf32_codepoint(()),
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

            Opt::Unit {
                value, short_form, ..
            } => {
                let mut builder = builder.init_unit();
                self.build_constant(builder.reborrow().init_value(), value);
                builder.set_short_form(*short_form);
            }

            Opt::Invalid { label, .. } => builder.init_invalid().set_label(label),
        }
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
