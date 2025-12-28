use ecow::EcoString;

use crate::{
    ast::{
        Constant, Publicity, SrcSpan, TypedConstant, TypedConstantBitArraySegment,
        TypedConstantBitArraySegmentOption,
    },
    reference::{Reference, ReferenceKind, ReferenceMap},
    schema_capnp::{self as schema, *},
    type_::{
        self, AccessorsMap, Deprecation, FieldMap, Opaque, RecordAccessor, Type,
        TypeAliasConstructor, TypeConstructor, TypeValueConstructor, TypeVar,
        TypeVariantConstructors, ValueConstructor, ValueConstructorVariant,
        expression::{Implementations, Purity},
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
        module.set_src_path(self.data.src_path.as_str());
        module.set_is_internal(self.data.is_internal);
        module.set_contains_echo(self.data.contains_echo);
        self.set_module_types(&mut module);
        self.set_module_values(&mut module);
        self.set_module_accessors(&mut module);
        self.set_module_types_constructors(&mut module);
        self.set_line_numbers(&mut module);
        self.set_version(&mut module);
        self.set_module_documentation(&mut module);
        self.set_module_type_aliases(&mut module);
        self.set_module_references(&mut module);

        capnp::serialize_packed::write_message(&mut buffer, &message).expect("capnp encode");
        Ok(buffer)
    }

    fn set_line_numbers(&mut self, module: &mut module::Builder<'_>) {
        let mut line_numbers = module.reborrow().init_line_numbers();
        line_numbers.set_length(self.data.line_numbers.length);

        let mut line_starts = line_numbers
            .reborrow()
            .init_line_starts(self.data.line_numbers.line_starts.len() as u32);
        for (i, l) in self.data.line_numbers.line_starts.iter().enumerate() {
            line_starts.reborrow().set(i as u32, *l);
        }

        let mut mapping = line_numbers.init_mapping(self.data.line_numbers.mapping.len() as u32);
        for (i, (byte_index, character)) in self.data.line_numbers.mapping.iter().enumerate() {
            let mut builder = mapping.reborrow().get(i as u32);
            builder.set_byte_index(*byte_index as u64);
            builder.set_length_utf8(character.length_utf8);
            builder.set_length_utf16(character.length_utf16);
        }
    }

    fn set_module_documentation(&mut self, module: &mut module::Builder<'_>) {
        let mut documentation = module
            .reborrow()
            .init_documentation(self.data.documentation.len() as u32);
        for (i, documentation_part) in self.data.documentation.iter().enumerate() {
            documentation.set(i as u32, documentation_part.as_str());
        }
    }

    fn set_module_accessors(&mut self, module: &mut module::Builder<'_>) {
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
        self.build_publicity(builder.reborrow().init_publicity(), accessors.publicity);
        let mut accessors_builder = builder
            .reborrow()
            .init_shared_accessors(accessors.shared_accessors.len() as u32);
        for (i, (name, accessor)) in accessors.shared_accessors.iter().enumerate() {
            let mut property = accessors_builder.reborrow().get(i as u32);
            property.set_key(name);
            self.build_record_accessor(property.init_value(), accessor)
        }

        let mut variant_accessors = builder
            .reborrow()
            .init_variant_specific_accessors(accessors.variant_specific_accessors.len() as u32);
        for (i, map) in accessors.variant_specific_accessors.iter().enumerate() {
            self.build_variant_accessors(variant_accessors.reborrow().get(i as u32), map);
        }

        let mut positional_accessors =
            builder.init_positional_accessors(accessors.variant_positional_accessors.len() as u32);
        for (i, fields) in accessors.variant_positional_accessors.iter().enumerate() {
            self.build_positional_accessors(positional_accessors.reborrow().get(i as u32), fields);
        }
    }

    fn build_variant_accessors(
        &mut self,
        builder: variant_specific_accessors::Builder<'_>,
        accessors: &HashMap<EcoString, RecordAccessor>,
    ) {
        let mut builder = builder.init_accessors(accessors.len() as u32);
        for (i, (name, accessor)) in accessors.iter().enumerate() {
            let mut property = builder.reborrow().get(i as u32);
            property.set_key(name);
            self.build_record_accessor(property.init_value(), accessor)
        }
    }

    fn build_positional_accessors(
        &mut self,
        builder: positional_accessors::Builder<'_>,
        accessors: &[Arc<Type>],
    ) {
        let mut builder = builder.init_accessors(accessors.len() as u32);
        for (i, type_) in accessors.iter().enumerate() {
            self.build_type(builder.reborrow().get(i as u32), type_);
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
        builder.set_documentation(accessor.documentation.as_deref().unwrap_or_default());
    }

    fn set_module_types(&mut self, module: &mut module::Builder<'_>) {
        let mut types = module.reborrow().init_types(self.data.types.len() as u32);
        for (i, (name, type_)) in self.data.types.iter().enumerate() {
            let mut property = types.reborrow().get(i as u32);
            property.set_key(name);
            self.build_type_constructor(property.init_value(), type_)
        }
    }

    fn set_module_type_aliases(&mut self, module: &mut module::Builder<'_>) {
        let mut types = module
            .reborrow()
            .init_type_aliases(self.data.type_aliases.len() as u32);
        for (i, (name, alias)) in self.data.type_aliases.iter().enumerate() {
            let mut property = types.reborrow().get(i as u32);
            property.set_key(name);
            self.build_type_alias_constructor(property.init_value(), alias)
        }
    }

    fn set_module_types_constructors(&mut self, module: &mut module::Builder<'_>) {
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
        match data.opaque {
            Opaque::Opaque => builder.set_opaque(true),
            Opaque::NotOpaque => builder.set_opaque(false),
        }
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
        let mut values = module.reborrow().init_values(self.data.values.len() as u32);
        for (i, (name, value)) in self.data.values.iter().enumerate() {
            let mut property = values.reborrow().get(i as u32);
            property.set_key(name);
            self.build_value_constructor(property.init_value(), value)
        }
    }

    fn set_module_references(&mut self, module: &mut module::Builder<'_>) {
        let references = &self.data.references;
        let mut builder = module.reborrow().init_references();
        let mut imported_modules = builder
            .reborrow()
            .init_imported_modules(references.imported_modules.len() as u32);
        for (i, module) in references.imported_modules.iter().enumerate() {
            imported_modules.set(i as u32, module);
        }

        let value_references = builder
            .reborrow()
            .init_value_references(references.value_references.len() as u32);
        self.build_reference_map(value_references, &references.value_references);
        let type_references = builder
            .reborrow()
            .init_type_references(references.type_references.len() as u32);
        self.build_reference_map(type_references, &references.type_references);
    }

    fn build_reference_map(
        &mut self,
        mut builder: capnp::struct_list::Builder<'_, reference_map::Owned>,
        map: &ReferenceMap,
    ) {
        for (i, ((module, name), references)) in map.iter().enumerate() {
            let mut builder = builder.reborrow().get(i as u32);
            builder.set_module(module);
            builder.set_name(name);
            let mut references_builder =
                builder.reborrow().init_references(references.len() as u32);
            for (i, reference) in references.iter().enumerate() {
                let builder = references_builder.reborrow().get(i as u32);
                self.build_reference(builder, reference);
            }
        }
    }

    fn build_reference(&mut self, mut builder: reference::Builder<'_>, reference: &Reference) {
        let mut kind = builder.reborrow().init_kind();
        match reference.kind {
            ReferenceKind::Qualified => kind.set_qualified(()),
            ReferenceKind::Unqualified => kind.set_unqualified(()),
            ReferenceKind::Import => kind.set_import(()),
            ReferenceKind::Definition => kind.set_definition(()),
            ReferenceKind::Alias => kind.set_alias(()),
        }
        self.build_src_span(builder.init_location(), reference.location);
    }

    fn set_version(&mut self, module: &mut module::Builder<'_>) {
        let mut version = module.reborrow().init_required_version();
        version.set_major(self.data.minimum_required_version.major);
        version.set_minor(self.data.minimum_required_version.minor);
        version.set_patch(self.data.minimum_required_version.patch);
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
        self.build_publicity(builder.reborrow().init_publicity(), constructor.publicity);
        let type_builder = builder.reborrow().init_type();
        self.build_type(type_builder, &constructor.type_);
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

    fn build_type_alias_constructor(
        &mut self,
        mut builder: type_alias_constructor::Builder<'_>,
        constructor: &TypeAliasConstructor,
    ) {
        builder.set_module(&constructor.module);
        builder.set_deprecation(match &constructor.deprecation {
            Deprecation::NotDeprecated => "",
            Deprecation::Deprecated { message } => message,
        });
        self.build_publicity(builder.reborrow().init_publicity(), constructor.publicity);
        let type_builder = builder.reborrow().init_type();
        self.build_type(type_builder, &constructor.type_);
        self.build_src_span(builder.reborrow().init_origin(), constructor.origin);
        builder.set_documentation(constructor.documentation.as_deref().unwrap_or_default());
        builder.set_arity(constructor.arity as u32);

        let mut parameters_builder = builder.init_parameters(constructor.parameters.len() as u32);
        for (index, parameter) in constructor.parameters.iter().enumerate() {
            self.build_type(parameters_builder.reborrow().get(index as u32), parameter);
        }
    }

    fn build_type_value_constructor(
        &mut self,
        mut builder: type_value_constructor::Builder<'_>,
        constructor: &TypeValueConstructor,
    ) {
        builder.set_name(&constructor.name);
        builder.set_documentation(constructor.documentation.as_deref().unwrap_or_default());
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
        mut builder: type_value_constructor_parameter::Builder<'_>,
        parameter: &type_::TypeValueConstructorField,
    ) {
        self.build_type(builder.reborrow().init_type(), parameter.type_.as_ref());
        builder.set_label(parameter.label.as_deref().unwrap_or_default());
        builder.set_documentation(parameter.documentation.as_deref().unwrap_or_default());
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

        self.build_publicity(builder.reborrow().init_publicity(), constructor.publicity);
        self.build_type(builder.reborrow().init_type(), &constructor.type_);
        self.build_value_constructor_variant(builder.init_variant(), &constructor.variant);
    }

    fn build_publicity(&mut self, mut builder: publicity::Builder<'_>, publicity: Publicity) {
        match publicity {
            Publicity::Public => builder.set_public(()),
            Publicity::Private => builder.set_private(()),
            Publicity::Internal {
                attribute_location: None,
            } => {
                let mut builder = builder.init_internal();
                builder.set_none(());
            }
            Publicity::Internal {
                attribute_location: Some(location),
            } => {
                let builder = builder.init_internal();
                let builder = builder.init_some();
                self.build_src_span(builder, location);
            }
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

            ValueConstructorVariant::ModuleConstant {
                literal,
                location,
                module,
                documentation: doc,
                implementations,
                name,
            } => {
                let mut builder = builder.init_module_constant();
                builder.set_documentation(doc.as_ref().map(EcoString::as_str).unwrap_or_default());
                self.build_src_span(builder.reborrow().init_location(), *location);
                self.build_constant(builder.reborrow().init_literal(), literal);
                builder.reborrow().set_module(module);
                builder.reborrow().set_name(name);
                self.build_implementations(builder.init_implementations(), *implementations)
            }

            ValueConstructorVariant::Record {
                name,
                field_map,
                arity,
                location,
                module,
                variants_count: constructors_count,
                variant_index: constructor_index,
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
                external_erlang,
                external_javascript,
                purity,
            } => {
                let mut builder = builder.init_module_fn();
                builder.set_name(name);
                builder.set_module(module);
                builder.set_arity(*arity as u16);
                builder.set_documentation(doc.as_ref().map(EcoString::as_str).unwrap_or_default());

                let mut purity_builder = builder.reborrow().init_purity();
                match purity {
                    Purity::Pure => purity_builder.set_pure(()),
                    Purity::TrustedPure => purity_builder.set_trusted_pure(()),
                    Purity::Impure => purity_builder.set_impure(()),
                    Purity::Unknown => purity_builder.set_unknown(()),
                }

                self.build_external(builder.reborrow().init_external_erlang(), external_erlang);
                self.build_external(
                    builder.reborrow().init_external_javascript(),
                    external_javascript,
                );
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

            Constant::Tuple {
                elements, type_, ..
            } => {
                let mut builder = builder.init_tuple();
                self.build_constants(
                    builder.reborrow().init_elements(elements.len() as u32),
                    elements,
                );
                self.build_type(builder.init_type(), type_);
            }

            Constant::List {
                elements, type_, ..
            } => {
                let mut builder = builder.init_list();
                self.build_constants(
                    builder.reborrow().init_elements(elements.len() as u32),
                    elements,
                );
                self.build_type(builder.init_type(), type_);
            }

            Constant::BitArray { segments, .. } => {
                let mut builder = builder.init_bit_array(segments.len() as u32);
                for (i, segment) in segments.iter().enumerate() {
                    self.build_bit_array_segment(builder.reborrow().get(i as u32), segment);
                }
            }

            Constant::Record {
                arguments,
                tag,
                type_,
                ..
            } => {
                let mut builder = builder.init_record();
                {
                    let mut builder = builder.reborrow().init_args(arguments.len() as u32);
                    for (i, argument) in arguments.iter().enumerate() {
                        self.build_constant(builder.reborrow().get(i as u32), &argument.value);
                    }
                }
                builder.reborrow().set_tag(tag);
                self.build_type(builder.reborrow().init_type(), type_);
            }
            Constant::Var {
                module,
                name,
                type_,
                constructor,
                ..
            } => {
                let mut builder = builder.init_var();
                match module {
                    Some((name, _)) => builder.set_module(name),
                    None => builder.set_module(""),
                };
                builder.set_name(name);
                self.build_type(builder.reborrow().init_type(), type_);
                self.build_value_constructor(
                    builder.reborrow().init_constructor(),
                    constructor
                        .as_ref()
                        .expect("This is guaranteed to hold a value."),
                );
            }

            Constant::StringConcatenation { right, left, .. } => {
                let mut builder = builder.init_string_concatenation();
                self.build_constant(builder.reborrow().init_right(), right);
                self.build_constant(builder.reborrow().init_left(), left);
            }

            Constant::RecordUpdate { .. } => {
                panic!("record updates should not reach code generation")
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
            Type::Fn { arguments, return_ } => {
                let mut fun = builder.init_fn();
                self.build_types(
                    fun.reborrow().init_arguments(arguments.len() as u32),
                    arguments,
                );
                self.build_type(fun.init_return(), return_)
            }

            Type::Named {
                name,
                arguments,
                module,
                package,
                inferred_variant,
                publicity,
            } => {
                let mut app = builder.init_app();
                app.set_name(name);
                app.set_module(module);
                app.set_package(package);
                let mut variant_builder = app.reborrow().init_inferred_variant();
                match inferred_variant {
                    Some(variant) => variant_builder.set_inferred(*variant),
                    None => variant_builder.set_unknown(()),
                }
                self.build_types(
                    app.reborrow().init_parameters(arguments.len() as u32),
                    arguments,
                );
                self.build_publicity(app.init_publicity(), *publicity);
            }

            Type::Tuple { elements } => self.build_types(
                builder.init_tuple().init_elements(elements.len() as u32),
                elements,
            ),

            Type::Var { type_ } => match type_.borrow().deref() {
                TypeVar::Link { type_ } => self.build_type(builder, type_),
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

    fn build_external(
        &self,
        mut builder: option::Builder<'_, external::Owned>,
        external: &Option<(EcoString, EcoString)>,
    ) {
        match external {
            None => builder.set_none(()),
            Some((module, function)) => {
                let mut builder = builder.init_some();
                builder.set_module(module);
                builder.set_function(function);
            }
        }
    }
}
