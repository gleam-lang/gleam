use ecow::EcoString;

use crate::{
    ast::{
        Constant, Publicity, SrcSpan, TypedConstant, TypedConstantBitArraySegment,
        TypedConstantBitArraySegmentOption,
    },
    schema_capnp::{self as schema, *},
    type_::{
        self, expression::Implementations, AccessorsMap, Deprecation, FieldMap, RecordAccessor,
        Type, TypeConstructor, TypeValueConstructor, TypeVar, TypeVariantConstructors,
        ValueConstructor, ValueConstructorVariant,
    },
};
use std::{collections::HashMap, ops::Deref, sync::Arc};

#[derive(Debug)]
pub struct ModuleEncoder<'a> {
    data: &'a type_::ModuleInterface,
    next_type_var_id: u64,
    type_var_id_map: HashMap<u64, u64>,
}

impl<'a> ModuleEncoder<'a> {
    pub fn new(data: &'a type_::ModuleInterface) -> Self {
        Self {
            data,
            next_type_var_id: 0,
            type_var_id_map: HashMap::new(),
        }
    }

    pub fn encode(mut self) -> crate::Result<Vec<u8>> {
        let span = tracing::info_span!("metadata");
        let _enter = span.enter();
        let mut buffer = Vec::new();

        let mut message = capnp::message::Builder::new_default();

        let mut module = message.init_root::<module::Builder<'_>>();
        module.set_name(&self.data.name);
        module.set_package(&self.data.package);
        module.set_contains_todo(self.data.contains_todo);
        module.set_src_path(self.data.src_path.as_str());
        module.set_is_internal(self.data.is_internal);
        self.set_module_types(&mut module);
        self.set_module_values(&mut module);
        self.set_module_accessors(&mut module);
        self.set_module_types_constructors(&mut module);
        self.set_unused_imports(&mut module);
        self.set_line_numbers(&mut module);

        capnp::serialize_packed::write_message(&mut buffer, &message).expect("capnp encode");
        Ok(buffer)
    }

    fn set_line_numbers(&mut self, module: &mut module::Builder<'_>) {
        let mut line_numbers = module.reborrow().init_line_numbers();
        line_numbers.set_length(self.data.line_numbers.length);
        let line_starts =
            line_numbers.init_line_starts(self.data.line_numbers.line_starts.len() as u32);
        for (i, l) in self.data.line_numbers.line_starts.iter().enumerate() {
            line_starts.reborrow().set(i as u32, *l);
        }
    }

    fn set_unused_imports(&mut self, module: &mut module::Builder<'_>) {
        let mut unused_imports = module
            .reborrow()
            .init_unused_imports(self.data.unused_imports.len() as u32);
        for (i, span) in self.data.unused_imports.iter().enumerate() {
            let unused_import = unused_imports.reborrow().get(i as u32);
            self.build_src_span(unused_import, *span)
        }
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
            .init_types_constructors(self.data.types_value_constructors.len() as u32);
        for (i, (name, data)) in self.data.types_value_constructors.iter().enumerate() {
            let mut property = types_constructors.reborrow().get(i as u32);
            property.set_key(name);
            self.build_type_variant_constructors(property.init_value(), data)
        }
    }

    fn build_type_variant_constructors(
        &mut self,
        mut builder: types_variant_constructors::Builder<'_>,
        data: &TypeVariantConstructors,
    ) {
        {
            let mut builder = builder
                .reborrow()
                .init_type_parameters_ids(data.type_parameters_ids.len() as u32);
            for (i, id) in data.type_parameters_ids.iter().enumerate() {
                let id = self.get_or_insert_type_var_id(*id);
                builder.set(i as u32, id as u16);
            }
        }
        let mut builder = builder.init_variants(data.variants.len() as u32);
        for (i, constructor) in data.variants.iter().enumerate() {
            self.build_type_value_constructor(builder.reborrow().get(i as u32), constructor);
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
        builder.set_module(&constructor.module);
        builder.set_deprecated(match &constructor.deprecation {
            Deprecation::NotDeprecated => "",
            Deprecation::Deprecated { message } => message,
        });
        builder.set_publicity(self.publicity(constructor.publicity));
        let type_builder = builder.reborrow().init_type();
        self.build_type(type_builder, &constructor.typ);
        self.build_types(
            builder
                .reborrow()
                .init_parameters(constructor.parameters.len() as u32),
            &constructor.parameters,
        );
        self.build_src_span(builder.reborrow().init_origin(), constructor.origin);
        builder.set_documentation(
            constructor
                .documentation
                .as_ref()
                .map(EcoString::as_str)
                .unwrap_or_default(),
        );
    }

    fn build_type_value_constructor(
        &mut self,
        mut builder: type_value_constructor::Builder<'_>,
        constructor: &TypeValueConstructor,
    ) {
        builder.set_name(&constructor.name);
        let mut builder = builder.init_parameters(constructor.parameters.len() as u32);
        for (i, parameter) in constructor.parameters.iter().enumerate() {
            self.build_type_value_constructor_parameter(
                builder.reborrow().get(i as u32),
                parameter,
            );
        }
    }

    fn build_type_value_constructor_parameter(
        &mut self,
        builder: type_value_constructor_parameter::Builder<'_>,
        parameter: &type_::TypeValueConstructorField,
    ) {
        self.build_type(builder.init_type(), parameter.type_.as_ref())
    }

    fn build_value_constructor(
        &mut self,
        mut builder: value_constructor::Builder<'_>,
        constructor: &ValueConstructor,
    ) {
        builder.set_deprecated(match &constructor.deprecation {
            Deprecation::NotDeprecated => "",
            Deprecation::Deprecated { message } => message,
        });
        builder.set_publicity(self.publicity(constructor.publicity));
        self.build_type(builder.reborrow().init_type(), &constructor.type_);
        self.build_value_constructor_variant(builder.init_variant(), &constructor.variant);
    }

    fn publicity(&self, publicity: Publicity) -> crate::schema_capnp::Publicity {
        match publicity {
            Publicity::Public => crate::schema_capnp::Publicity::Public,
            Publicity::Private => crate::schema_capnp::Publicity::Private,
            Publicity::Internal => crate::schema_capnp::Publicity::Internal,
        }
    }

    fn build_src_span(&mut self, mut builder: src_span::Builder<'_>, span: SrcSpan) {
        builder.set_start(span.start);
        builder.set_end(span.end);
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

            ValueConstructorVariant::LocalConstant { .. } => {
                panic!("Unexpected local constant value constructor in module interface",)
            }

            ValueConstructorVariant::ModuleConstant {
                literal,
                location,
                module,
                documentation: doc,
                implementations,
            } => {
                let mut builder = builder.init_module_constant();
                builder.set_documentation(doc.as_ref().map(EcoString::as_str).unwrap_or_default());
                self.build_src_span(builder.reborrow().init_location(), *location);
                self.build_constant(builder.reborrow().init_literal(), literal);
                builder.reborrow().set_module(module);
                self.build_implementations(builder.init_implementations(), *implementations)
            }

            ValueConstructorVariant::Record {
                name,
                field_map,
                arity,
                location,
                module,
                constructors_count,
                constructor_index,
                documentation: doc,
            } => {
                let mut builder = builder.init_record();
                builder.set_name(name);
                builder.set_module(module);
                builder.set_arity(*arity);
                builder.set_documentation(doc.as_ref().map(EcoString::as_str).unwrap_or_default());
                builder.set_constructors_count(*constructors_count);
                builder.set_constructor_index(*constructor_index);
                self.build_optional_field_map(builder.reborrow().init_field_map(), field_map);
                self.build_src_span(builder.init_location(), *location);
            }

            ValueConstructorVariant::ModuleFn {
                arity,
                field_map,
                module,
                name,
                location,
                documentation: doc,
                implementations,
            } => {
                let mut builder = builder.init_module_fn();
                builder.set_name(name);
                builder.set_module(module);
                builder.set_arity(*arity as u16);
                builder.set_documentation(doc.as_ref().map(EcoString::as_str).unwrap_or_default());
                self.build_optional_field_map(builder.reborrow().init_field_map(), field_map);
                self.build_src_span(builder.reborrow().init_location(), *location);
                self.build_implementations(builder.init_implementations(), *implementations);
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
        builder.set_arity(field_map.arity);
        let mut builder = builder.init_fields(field_map.fields.len() as u32);
        for (i, (name, &position)) in field_map.fields.iter().enumerate() {
            let mut field = builder.reborrow().get(i as u32);
            field.set_key(name);
            field.init_value().set_value(position);
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

            Constant::BitArray { segments, .. } => {
                let mut builder = builder.init_bit_array(segments.len() as u32);
                for (i, segment) in segments.iter().enumerate() {
                    self.build_bit_array_segment(builder.reborrow().get(i as u32), segment);
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

            Constant::Var {
                module,
                name,
                typ,
                constructor,
                ..
            } => {
                let mut builder = builder.init_var();
                match module {
                    Some(name) => builder.set_module(name),
                    None => builder.set_module(""),
                };
                builder.set_name(name);
                self.build_type(builder.reborrow().init_typ(), typ);
                self.build_value_constructor(
                    builder.reborrow().init_constructor(),
                    constructor
                        .as_ref()
                        .expect("This is guaranteed to hold a value."),
                );
            }

            Constant::Invalid { .. } => {
                panic!("invalid constants should not reach code generation")
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

    fn build_bit_array_segment(
        &mut self,
        mut builder: bit_array_segment::Builder<'_>,
        segment: &TypedConstantBitArraySegment,
    ) {
        self.build_constant(builder.reborrow().init_value(), &segment.value);
        {
            let mut builder = builder
                .reborrow()
                .init_options(segment.options.len() as u32);
            for (i, option) in segment.options.iter().enumerate() {
                self.build_bit_array_segment_option(builder.reborrow().get(i as u32), option);
            }
        }
        self.build_type(builder.init_type(), &segment.type_);
    }

    fn build_bit_array_segment_option(
        &mut self,
        mut builder: bit_array_segment_option::Builder<'_>,
        option: &TypedConstantBitArraySegmentOption,
    ) {
        use crate::ast::TypedConstantBitArraySegmentOption as Opt;
        match option {
            Opt::Bytes { .. } => builder.set_bytes(()),
            Opt::Int { .. } => builder.set_integer(()),
            Opt::Float { .. } => builder.set_float(()),
            Opt::Bits { .. } => builder.set_bits(()),
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

            Type::Named {
                name,
                args,
                module,
                package,
                ..
            } => {
                let mut app = builder.init_app();
                app.set_name(name);
                app.set_module(module);
                app.set_package(package);
                self.build_types(app.reborrow().init_parameters(args.len() as u32), args);
            }

            Type::Tuple { elems } => self.build_types(
                builder.init_tuple().init_elements(elems.len() as u32),
                elems,
            ),

            Type::Var { type_: typ } => match typ.borrow().deref() {
                TypeVar::Link { type_: typ } => self.build_type(builder, typ),
                TypeVar::Unbound { id, .. } | TypeVar::Generic { id } => {
                    self.build_type_var(builder.init_var(), *id)
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
        let serialised_id = self.get_or_insert_type_var_id(id);
        builder.set_id(serialised_id);
    }

    fn get_or_insert_type_var_id(&mut self, id: u64) -> u64 {
        match self.type_var_id_map.get(&id) {
            Some(&id) => id,
            None => {
                let new_id = self.next_type_var_id;
                self.next_type_var_id += 1;
                let _ = self.type_var_id_map.insert(id, new_id);
                new_id
            }
        }
    }

    fn build_implementations(
        &self,
        mut builder: implementations::Builder<'_>,
        implementations: Implementations,
    ) {
        builder.set_gleam(implementations.gleam);
        builder.set_uses_erlang_externals(implementations.uses_erlang_externals);
        builder.set_uses_javascript_externals(implementations.uses_javascript_externals);
        builder.set_can_run_on_erlang(implementations.can_run_on_erlang);
        builder.set_can_run_on_javascript(implementations.can_run_on_javascript);
    }
}
