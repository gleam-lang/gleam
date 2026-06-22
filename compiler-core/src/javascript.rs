// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

mod decision;
mod expression;
mod import;
#[cfg(test)]
mod tests;
mod typescript;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use debug_ignore::DebugIgnore;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use sourcemap::SourceMap;

use crate::build::Target;
use crate::build::package_compiler::StdlibPackage;
use crate::codegen::TypeScriptDeclarations;
use crate::line_numbers::LineColumn;
use crate::type_::{PRELUDE_MODULE_NAME, RecordAccessor};
use crate::{
    ast::{Import, *},
    line_numbers::LineNumbers,
};
use camino::Utf8Path;
use ecow::{EcoString, eco_format};
use expression::Context;
use itertools::Itertools;
use pretty_arena::*;

use self::import::{Imports, Member};

const INDENT: isize = 2;

pub const PRELUDE: &str = include_str!("../templates/prelude.mjs");
pub const PRELUDE_TS_DEF: &str = include_str!("../templates/prelude.d.mts");

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JavaScriptCodegenTarget {
    JavaScript,
    TypeScriptDeclarations,
}

/// A cursor position observer that does nothing.
/// Mainly used to allow us to create a cursor position observer that does
/// nothing without having to check if the source map builder is present.
#[derive(Debug, Clone, Copy)]
pub struct NullCursorPositionObserver;

impl CursorPositionObserver for NullCursorPositionObserver {
    fn observe_cursor_position(&mut self, _line: isize, _width: isize) {
        // Do nothing
    }
}

/// A cursor position observer that observes the cursor position and adds it
/// to the source map builder. When notified it will take the destination
/// location and map it to the initial source location given when
/// creating the observer.
#[derive(Debug)]
pub struct SourceMapCursorPositionObserver {
    source_start_location: LineColumn,
    source_map_builder: Rc<RefCell<DebugIgnore<sourcemap::SourceMapBuilder>>>,
}

impl SourceMapCursorPositionObserver {
    pub fn new(
        source_start_location: LineColumn,
        source_map_builder: Rc<RefCell<DebugIgnore<sourcemap::SourceMapBuilder>>>,
    ) -> Self {
        Self {
            source_start_location,
            source_map_builder,
        }
    }
}

impl CursorPositionObserver for SourceMapCursorPositionObserver {
    fn observe_cursor_position(&mut self, line: isize, column: isize) {
        let _ = self.source_map_builder.borrow_mut().add_raw(
            line as u32,
            column as u32,
            // SourceMapBuilder expects 0-based line and column numbers and we use 1-based ones
            self.source_start_location.line - 1,
            self.source_start_location.column - 1,
            // Since we have a 1-1 mapping between source and destination locations
            // We always use 0 for the source index.
            Some(0),
            None,
            false,
        );
    }
}

#[derive(Debug)]
pub struct Generator<'a> {
    line_numbers: &'a LineNumbers,
    module: &'a TypedModule,
    tracker: UsageTracker,
    module_scope: im::HashMap<EcoString, usize>,
    current_module_name_segments_count: usize,
    typescript: TypeScriptDeclarations,
    // Debug ignored since SourceMapBuilder doesn't implement debug
    source_map_builder: Option<Rc<RefCell<DebugIgnore<sourcemap::SourceMapBuilder>>>>,
    stdlib_package: StdlibPackage,
    /// Relative path to the module, surrounded in `"`s to make it a string, and with `\`s escaped
    /// to `\\`.
    src_path: EcoString,
}

impl<'a, 'doc> Generator<'a> {
    pub fn new(config: ModuleConfig<'a>) -> Self {
        let ModuleConfig {
            typescript,
            source_map,
            stdlib_package,
            module,
            line_numbers,
            src: _,
            path: _,
            project_root,
        } = config;
        let current_module_name_segments_count = module.name.split('/').count();

        let src_path = &module.type_info.src_path;
        let src_path = src_path
            .strip_prefix(project_root)
            .unwrap_or(src_path)
            .as_str();
        let src_path = eco_format!("\"{src_path}\"").replace("\\", "\\\\");
        Self {
            current_module_name_segments_count,
            line_numbers,
            module,
            src_path,
            tracker: UsageTracker::default(),
            module_scope: Default::default(),
            typescript,
            source_map_builder: if source_map {
                let module_name = module.name.clone();
                let output_path = format!("{module_name}.mjs");
                let module_alias = module_name.split('/').next_back().unwrap_or(&module_name);
                let input_path = format!("{module_alias}.gleam",);
                let mut source_map_builder =
                    sourcemap::SourceMapBuilder::new(Some(&output_path.clone()));
                let _ = source_map_builder.add_source(&input_path);
                Some(Rc::new(RefCell::new(DebugIgnore(source_map_builder))))
            } else {
                None
            },
            stdlib_package,
        }
    }

    fn type_reference(&self, arena: &'doc DocumentArena<'a, 'doc>) -> Document<'a, 'doc> {
        if self.typescript == TypeScriptDeclarations::None {
            return EMPTY_DOCUMENT;
        }

        // Get the name of the module relative the directory (similar to basename)
        let module = self
            .module
            .name
            .as_str()
            .split('/')
            .next_back()
            .expect("JavaScript generator could not identify imported module name.");

        docvec![
            arena,
            REFERENCE_TYPES_DOCUMENT,
            module,
            DOT_D_DOT_MTS_CLOSE_QUOTE_CLOSE_TAG_DOCUMENT,
            LINE_DOCUMENT
        ]
    }

    fn sourcemap_reference(&self, arena: &'doc DocumentArena<'a, 'doc>) -> Document<'a, 'doc> {
        match self.source_map_builder {
            None => "".to_doc(arena),
            Some(_) => {
                // Get the name of the module relative the directory (similar to basename)
                let module = self
                    .module
                    .name
                    .as_str()
                    .split('/')
                    .next_back()
                    .expect("JavaScript generator could not identify imported module name.");

                docvec![
                    arena,
                    SOURCE_MAPPING_URL_EQUAL_DOCUMENT,
                    module,
                    DOT_MJS_DOT_MAP_DOCUMENT,
                    LINE_DOCUMENT
                ]
            }
        }
    }

    pub fn compile(&mut self, arena: &'doc DocumentArena<'a, 'doc>) -> Document<'a, 'doc> {
        // Determine what JavaScript imports we need to generate
        let mut imports = self.collect_imports(arena);

        // Determine what names are defined in the module scope so we know to
        // rename any variables that are defined within functions using the same
        // names.
        self.register_module_definitions_in_scope();

        // Generate JavaScript code for each statement.
        let statements = self.definitions(arena);

        // Two lines between each statement
        let no_statements = statements.is_empty();
        let statements = arena.join(statements, TWO_LINES_DOCUMENT);

        // Import any prelude functions that have been used

        if self.tracker.ok_used {
            self.register_prelude_usage(arena, &mut imports, "Ok", None);
        };

        if self.tracker.error_used {
            self.register_prelude_usage(arena, &mut imports, "Error", None);
        };

        if self.tracker.list_used {
            self.register_prelude_usage(arena, &mut imports, "toList", None);
        };

        if self.tracker.list_empty_class_used || self.tracker.echo_used {
            self.register_prelude_usage(arena, &mut imports, "Empty", Some("$Empty"));
        };

        if self.tracker.list_empty_const_used {
            self.register_prelude_usage(
                arena,
                &mut imports,
                "List$Empty$const",
                Some("$List$Empty$const"),
            );
        };

        if self.tracker.list_non_empty_class_used || self.tracker.echo_used {
            self.register_prelude_usage(arena, &mut imports, "NonEmpty", Some("$NonEmpty"));
        };

        if self.tracker.prepend_used {
            self.register_prelude_usage(arena, &mut imports, "prepend", Some("listPrepend"));
        };

        if self.tracker.custom_type_used || self.tracker.echo_used {
            self.register_prelude_usage(arena, &mut imports, "CustomType", Some("$CustomType"));
        };

        if self.tracker.make_error_used {
            self.register_prelude_usage(arena, &mut imports, "makeError", None);
        };

        if self.tracker.int_remainder_used {
            self.register_prelude_usage(arena, &mut imports, "remainderInt", None);
        };

        if self.tracker.float_division_used {
            self.register_prelude_usage(arena, &mut imports, "divideFloat", None);
        };

        if self.tracker.int_division_used {
            self.register_prelude_usage(arena, &mut imports, "divideInt", None);
        };

        if self.tracker.object_equality_used {
            self.register_prelude_usage(arena, &mut imports, "isEqual", None);
        };

        if self.tracker.bit_array_literal_used {
            self.register_prelude_usage(arena, &mut imports, "toBitArray", None);
        }

        if self.tracker.bit_array_slice_used || self.tracker.echo_used {
            self.register_prelude_usage(arena, &mut imports, "bitArraySlice", None);
        }

        if self.tracker.bit_array_slice_to_float_used {
            self.register_prelude_usage(arena, &mut imports, "bitArraySliceToFloat", None);
        }

        if self.tracker.bit_array_slice_to_int_used || self.tracker.echo_used {
            self.register_prelude_usage(arena, &mut imports, "bitArraySliceToInt", None);
        }

        if self.tracker.sized_integer_segment_used {
            self.register_prelude_usage(arena, &mut imports, "sizedInt", None);
        }

        if self.tracker.string_bit_array_segment_used {
            self.register_prelude_usage(arena, &mut imports, "stringBits", None);
        }

        if self.tracker.string_utf16_bit_array_segment_used {
            self.register_prelude_usage(arena, &mut imports, "stringToUtf16", None);
        }

        if self.tracker.string_utf32_bit_array_segment_used {
            self.register_prelude_usage(arena, &mut imports, "stringToUtf32", None);
        }

        if self.tracker.codepoint_bit_array_segment_used {
            self.register_prelude_usage(arena, &mut imports, "codepointBits", None);
        }

        if self.tracker.codepoint_utf16_bit_array_segment_used {
            self.register_prelude_usage(arena, &mut imports, "codepointToUtf16", None);
        }

        if self.tracker.codepoint_utf32_bit_array_segment_used {
            self.register_prelude_usage(arena, &mut imports, "codepointToUtf32", None);
        }

        if self.tracker.float_bit_array_segment_used {
            self.register_prelude_usage(arena, &mut imports, "sizedFloat", None);
        }

        for (variant, alias) in self.tracker.variant_constants_used.iter() {
            let path = self.import_path(&variant.package, &variant.module);
            let member = Member {
                name: eco_format!("{}${}$const", variant.type_name, variant.name),
                alias: alias.as_ref().map(|alias| alias.to_doc(arena)),
            };
            imports.register_module(path, [], [member]);
        }

        // If we have some Gleam code that looks something like this:
        // ```gleam
        // import option.{None}
        //
        // pub fn main() {
        //   None
        // }
        // ```
        //
        // Here, the generated JavaScript code for `main` will look something
        // like this:
        //
        // ```javascript
        // export function main() {
        //   return Option$None$const;
        // }
        // ```
        //
        // The `None` variant is technically being imported, but we don't
        // actually use the generated `None` class in the JavaScript code, so
        // we don't want to import it.
        //
        // However, if we are pattern matching on the `None` variant, there
        // will be some code that looks like `if (value instanceof None)`, in
        // which case we still need to import `None`. Therefore we remove all
        // variants which are used as singleton constants, and that are not used
        // in pattern matching.
        //
        imports.filter_unused_variants(
            self.tracker
                .variant_constants_used
                .keys()
                .filter(|variant| !self.tracker.variants_used_in_instanceof.contains(variant))
                .map(|variant| {
                    let path = self.import_path(&variant.package, &variant.module);
                    (path, variant.name.clone())
                }),
        );

        let echo_definition = self.echo_definition(arena, &mut imports);
        let sourcemap_reference = self.sourcemap_reference(arena);
        let type_reference = self.type_reference(arena);
        let filepath_definition = self.filepath_definition(arena);

        // Put it all together

        if imports.is_empty() && no_statements {
            docvec![
                arena,
                sourcemap_reference,
                type_reference,
                filepath_definition,
                EXPORT_SPACE_OPEN_CLOSE_CURLY_DOCUMENT,
                LINE_DOCUMENT,
                echo_definition
            ]
        } else if imports.is_empty() {
            docvec![
                arena,
                sourcemap_reference,
                type_reference,
                filepath_definition,
                statements.append(arena, LINE_DOCUMENT),
                echo_definition
            ]
        } else if no_statements {
            docvec![
                arena,
                sourcemap_reference,
                type_reference,
                imports.into_doc(arena, JavaScriptCodegenTarget::JavaScript),
                filepath_definition,
                echo_definition,
            ]
        } else {
            docvec![
                arena,
                sourcemap_reference,
                type_reference,
                imports.into_doc(arena, JavaScriptCodegenTarget::JavaScript),
                LINE_DOCUMENT,
                filepath_definition,
                statements,
                LINE_DOCUMENT,
                echo_definition
            ]
        }
    }

    fn echo_definition(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        imports: &mut Imports<'a, 'doc>,
    ) -> Document<'a, 'doc> {
        if !self.tracker.echo_used {
            return EMPTY_DOCUMENT;
        }

        if StdlibPackage::Present == self.stdlib_package {
            let value = Some((
                AssignName::Variable("stdlib$dict".into()),
                SrcSpan::default(),
            ));

            self.register_import(arena, imports, "gleam_stdlib", "gleam/dict", &value, &[]);
        }
        self.register_prelude_usage(arena, imports, "BitArray", Some("$BitArray"));
        self.register_prelude_usage(arena, imports, "List", Some("$List"));
        self.register_prelude_usage(arena, imports, "UtfCodepoint", Some("$UtfCodepoint"));
        docvec![
            arena,
            LINE_DOCUMENT,
            std::include_str!("../templates/echo.mjs"),
            LINE_DOCUMENT
        ]
    }

    fn register_prelude_usage(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        imports: &mut Imports<'a, 'doc>,
        name: &'static str,
        alias: Option<&'static str>,
    ) {
        let path = self.import_path(&self.module.type_info.package, PRELUDE_MODULE_NAME);
        let member = Member {
            name: name.into(),
            alias: alias.map(|a| a.to_doc(arena)),
        };
        imports.register_module(path, [], [member]);
    }

    fn custom_type_definition(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        custom_type: &'a TypedCustomType,
    ) -> Option<Vec<Document<'a, 'doc>>> {
        if self
            .module
            .unused_definition_positions
            .contains(&custom_type.location.start)
        {
            return None;
        }

        let TypedCustomType {
            name,
            publicity,
            constructors,
            opaque,
            ..
        } = custom_type;

        // If there's no constructors then there's nothing to do here.
        if constructors.is_empty() {
            return Some(vec![]);
        }

        self.tracker.custom_type_used = true;

        let constructor_publicity = if *opaque || publicity.is_private() {
            Publicity::Private
        } else {
            Publicity::Public
        };

        let mut definitions = constructors
            .iter()
            .map(|constructor| {
                self.variant_definition(arena, constructor, name, constructor_publicity)
            })
            .collect_vec();

        // Generate getters for fields shared between variants
        if let Some(accessors_map) = self.module.type_info.accessors.get(name)
            && !accessors_map.shared_accessors.is_empty()
            // Don't bother generating shared getters when there's only one variant,
            // since the specific accessors can always be uses instead.
            && constructors.len() != 1
            // Only generate accessors for the API if the constructors are public
            && constructor_publicity.is_public()
        {
            definitions.push(self.shared_custom_type_fields(
                arena,
                name,
                &accessors_map.shared_accessors,
            ));
        }
        // Add start and end position source map trackers to the first and last
        // definition in place. This is to prevent extra new lines from being added
        // to the output Since each definition is a separate statement in the output.
        let start_location = custom_type
            .documentation
            .as_ref()
            // 3 is the length of the "///" documentation marker.
            // Start is the index of the actual content, so we need to subtract
            // the length of the marker.
            .map_or(custom_type.location.start, |(start, _)| *start - 3);
        let first_definition = definitions.remove(0);
        definitions.insert(
            0,
            self.source_map_tracker(arena, start_location)
                .append(arena, first_definition),
        );
        let last_definition = definitions
            .pop()
            .expect("Custom type must have at least one definition here");
        definitions.push(last_definition.append(
            arena,
            self.source_map_tracker(arena, custom_type.end_position),
        ));

        Some(definitions)
    }

    fn variant_definition(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
        publicity: Publicity,
    ) -> Document<'a, 'doc> {
        let class_definition = self.variant_class_definition(arena, constructor, publicity);

        // Singleton constants are used internally by the compiler, so they aren't
        // part of the public JS API and need to be generated even for private
        // types.
        let constructor_singleton = if constructor.arguments.is_empty() {
            docvec![
                arena,
                LINE_DOCUMENT,
                self.variant_constructor_constant(arena, constructor, type_name, publicity)
            ]
        } else {
            EMPTY_DOCUMENT
        };

        // If the custom type is private or opaque, we don't need to generate API
        // functions for it.
        if publicity.is_private() {
            return class_definition.append(arena, constructor_singleton);
        }

        let constructor_definition =
            self.variant_constructor_definition(arena, constructor, type_name);
        let variant_check_definition = self.variant_check_definition(arena, constructor, type_name);
        let fields_definition = self.variant_fields_definition(arena, constructor, type_name);

        docvec![
            arena,
            class_definition,
            constructor_singleton,
            LINE_DOCUMENT,
            constructor_definition,
            LINE_DOCUMENT,
            variant_check_definition,
            fields_definition,
        ]
    }

    /// Generate a singleton constant for a variant with no fields. This means
    /// that all values of a particular variant are the same underlying reference,
    /// allowing them to be compared more efficiently.
    ///
    fn variant_constructor_constant(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
        publicity: Publicity,
    ) -> Document<'a, 'doc> {
        let keyword = if publicity.is_importable() {
            EXPORT_CONST_SPACE_DOCUMENT
        } else {
            CONST_SPACE_DOCUMENT
        };
        docvec![
            arena,
            self.source_map_tracker(arena, constructor.location.start),
            keyword,
            type_name,
            DOLLAR_DOCUMENT,
            constructor.name.as_str(),
            DOLLAR_CONST_DOCUMENT,
            SPACE_EQUAL_DOCUMENT,
            docvec![
                arena,
                BREAKABLE_SPACE_DOCUMENT,
                NEW_SPACE_DOCUMENT,
                constructor.name.as_str(),
                OPEN_CLOSE_PAREN_SEMICOLON_DOCUMENT,
            ]
            .group(arena)
            .nest(arena, INDENT)
        ]
    }

    fn variant_constructor_definition(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
    ) -> Document<'a, 'doc> {
        // If the constructor has no fields, return the singleton constant
        // instead.
        if constructor.arguments.is_empty() {
            return docvec![
                arena,
                EXPORT_CONST_SPACE_DOCUMENT,
                type_name,
                DOLLAR_DOCUMENT,
                constructor.name.as_str(),
                SPACE_EQUAL_OPEN_CLOSE_PAREN_SPACE_ARROW_DOCUMENT,
                docvec![
                    arena,
                    BREAKABLE_SPACE_DOCUMENT,
                    type_name,
                    DOLLAR_DOCUMENT,
                    constructor.name.as_str(),
                    DOLLAR_CONST_SEMICOLON_DOCUMENT
                ]
                .group(arena)
                .nest(arena, INDENT),
            ];
        }

        let mut arguments = Vec::new();

        for (index, parameter) in constructor.arguments.iter().enumerate() {
            if let Some((_, label)) = &parameter.label {
                arguments.push(maybe_escape_identifier(label).to_doc(arena));
            } else {
                arguments.push(eco_format!("${index}").to_doc(arena));
            }
        }

        let construction = docvec![
            arena,
            BREAKABLE_SPACE_DOCUMENT,
            NEW_SPACE_DOCUMENT,
            constructor.name.as_str(),
            OPEN_PAREN_DOCUMENT,
            arena
                .join(arguments.clone(), COMMA_BREAK_DOCUMENT)
                .group(arena),
            CLOSE_PAREN_SEMICOLON_DOCUMENT,
        ]
        .group(arena);

        docvec![
            arena,
            self.source_map_tracker(arena, constructor.location.start),
            EXPORT_CONST_SPACE_DOCUMENT,
            type_name,
            DOLLAR_DOCUMENT,
            constructor.name.as_str(),
            SPACE_EQUAL_SPACE_OPEN_PAREN_DOCUMENT,
            arena.join(arguments, COMMA_BREAK_DOCUMENT),
            CLOSE_PAREN_ARROW_DOCUMENT,
            construction.nest(arena, INDENT),
        ]
    }

    fn variant_check_definition(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
    ) -> Document<'a, 'doc> {
        let construction = docvec![
            arena,
            BREAKABLE_SPACE_DOCUMENT,
            VALUE_INSTANCE_OF_SPACE_DOCUMENT,
            constructor.name.as_str(),
            SEMICOLON_DOCUMENT
        ]
        .group(arena);

        docvec![
            arena,
            self.source_map_tracker(arena, constructor.location.start),
            EXPORT_CONST_SPACE_DOCUMENT,
            type_name,
            DOLLAR_IS_DOCUMENT,
            constructor.name.as_str(),
            SPACE_EQUAL_SPACE_VALUE_SPACE_ARROW,
            construction.nest(arena, INDENT),
        ]
    }

    fn variant_fields_definition(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a TypedRecordConstructor,
        type_name: &'a str,
    ) -> Document<'a, 'doc> {
        let mut functions = Vec::new();

        for (index, argument) in constructor.arguments.iter().enumerate() {
            // Always generate the accessor for the value at this index. Although
            // this is not necessary when a label is present, we want to make sure
            // that adding a label to a record isn't a breaking change. For this
            // reason, we need to generate an index getter even when a label is
            // present to ensure consistent behaviour between labelled and unlabelled
            // field access.
            let function_name = eco_format!(
                "{type_name}${record_name}${index}",
                record_name = constructor.name,
            );

            let contents;

            // If the argument is labelled, also generate a getter for the labelled
            // argument.
            if let Some((_, label)) = &argument.label {
                let function_name = eco_format!(
                    "{type_name}${record_name}${label}",
                    record_name = constructor.name,
                );

                contents = docvec![
                    arena,
                    BREAKABLE_SPACE_DOCUMENT,
                    VALUE_DOT_DOCUMENT,
                    maybe_escape_property(label),
                    SEMICOLON_DOCUMENT
                ]
                .group(arena);

                functions.push(docvec![
                    arena,
                    LINE_DOCUMENT,
                    self.source_map_tracker(arena, constructor.location.start),
                    EXPORT_CONST_SPACE_DOCUMENT,
                    function_name,
                    SPACE_EQUAL_SPACE_VALUE_SPACE_ARROW,
                    contents.nest(arena, INDENT),
                ]);
            } else {
                contents = docvec![
                    arena,
                    BREAKABLE_SPACE_DOCUMENT,
                    VALUE_OPEN_SQUARE_DOCUMENT,
                    index,
                    CLOSE_SQUARE_SEMICOLON_DOCUMENT
                ]
                .group(arena)
            }

            functions.push(docvec![
                arena,
                LINE_DOCUMENT,
                self.source_map_tracker(arena, constructor.location.start),
                EXPORT_CONST_SPACE_DOCUMENT,
                function_name,
                SPACE_EQUAL_SPACE_VALUE_SPACE_ARROW,
                contents.nest(arena, INDENT),
            ]);
        }

        arena.concat(functions)
    }

    fn shared_custom_type_fields(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        type_name: &'a str,
        shared_accessors: &HashMap<EcoString, RecordAccessor>,
    ) -> Document<'a, 'doc> {
        let accessors = shared_accessors.keys().sorted().map(|field| {
            let function_name = eco_format!("{type_name}${field}");

            let contents = docvec![
                arena,
                BREAKABLE_SPACE_DOCUMENT,
                VALUE_DOT_DOCUMENT,
                maybe_escape_property(field),
                SEMICOLON_DOCUMENT
            ]
            .group(arena);

            docvec![
                arena,
                EXPORT_CONST_SPACE_DOCUMENT,
                function_name,
                SPACE_EQUAL_SPACE_VALUE_SPACE_ARROW,
                contents.nest(arena, INDENT),
            ]
        });
        arena.join(accessors, LINE_DOCUMENT)
    }

    fn variant_class_definition(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a TypedRecordConstructor,
        publicity: Publicity,
    ) -> Document<'a, 'doc> {
        fn parameter<'a, 'doc>(
            arena: &'doc DocumentArena<'a, 'doc>,
            (i, arg): (usize, &TypedRecordConstructorArg),
        ) -> Document<'a, 'doc> {
            arg.label
                .as_ref()
                .map(|(_, s)| maybe_escape_identifier(s))
                .unwrap_or_else(|| eco_format!("${i}"))
                .to_doc(arena)
        }

        let doc = if let Some((start, documentation)) = &constructor.documentation {
            docvec![
                arena,
                // 3 is the length of the "///" documentation marker.
                // Start is the index of the actual content, so we need to subtract
                // the length of the marker.
                self.source_map_tracker(arena, *start - 3),
                jsdoc_comment(arena, documentation, publicity),
                LINE_DOCUMENT
            ]
        } else {
            EMPTY_DOCUMENT
        };

        let head = if publicity.is_public() {
            EXPORT_CLASS_SPACE_DOCUMENT
        } else {
            CLASS_SPACE_DOCUMENT
        };

        let head = docvec![
            arena,
            self.source_map_tracker(arena, constructor.location.start),
            head,
            &constructor.name,
            SPACE_EXTENDS_CUSTOM_TYPE_OPEN_CURLY_DOCUMENT
        ];

        if constructor.arguments.is_empty() {
            return docvec![arena, doc, head, CLOSE_CURLY_DOCUMENT];
        };

        let parameters = arena.join(
            constructor
                .arguments
                .iter()
                .enumerate()
                .map(|argument| parameter(arena, argument)),
            COMMA_BREAK_DOCUMENT,
        );

        let constructor_body = arena.join(
            constructor.arguments.iter().enumerate().map(|(i, arg)| {
                let var = parameter(arena, (i, arg));
                match &arg.label {
                    None => docvec![
                        arena,
                        THIS_OPEN_SQUARE_DOCUMENT,
                        i,
                        CLOSE_SQUARE_SPACE_EQUAL_SPACE_DOCUMENT,
                        var,
                        SEMICOLON_DOCUMENT
                    ],
                    Some((_, name)) => {
                        docvec![
                            arena,
                            THIS_DOT_DOCUMENT,
                            maybe_escape_property(name),
                            SPACE_EQUAL_SPACE_DOCUMENT,
                            var,
                            SEMICOLON_DOCUMENT
                        ]
                    }
                }
            }),
            LINE_DOCUMENT,
        );

        let class_body = docvec![
            arena,
            LINE_DOCUMENT,
            CONSTRUCTOR_OPEN_PAREN_DOCUMENT,
            parameters,
            CLOSE_PAREN_SPACE_OPEN_CURLY_DOCUMENT,
            docvec![
                arena,
                LINE_DOCUMENT,
                SUPER_CALL_SEMICOLON_DOCUMENT,
                LINE_DOCUMENT,
                constructor_body
            ]
            .nest(arena, INDENT),
            LINE_DOCUMENT,
            CLOSE_CURLY_DOCUMENT,
        ]
        .nest(arena, INDENT);

        docvec![arena, doc, head, class_body, LINE_DOCUMENT, "}"]
    }

    fn definitions(&mut self, arena: &'doc DocumentArena<'a, 'doc>) -> Vec<Document<'a, 'doc>> {
        let mut definitions = vec![];

        for custom_type in &self.module.definitions.custom_types {
            if let Some(mut new_definitions) = self.custom_type_definition(arena, custom_type) {
                definitions.append(&mut new_definitions)
            }
        }

        for constant in &self.module.definitions.constants {
            if let Some(definition) = self.module_constant(arena, constant) {
                definitions.push(definition)
            }
        }

        for function in &self.module.definitions.functions {
            if let Some(definition) = self.module_function(arena, function) {
                definitions.push(definition)
            }
        }

        definitions
    }

    fn collect_imports(&mut self, arena: &'doc DocumentArena<'a, 'doc>) -> Imports<'a, 'doc> {
        let mut imports = Imports::new();

        for Import {
            module,
            as_name,
            unqualified_values,
            package,
            ..
        } in &self.module.definitions.imports
        {
            self.register_import(
                arena,
                &mut imports,
                package,
                module,
                as_name,
                unqualified_values,
            );
        }

        for function in &self.module.definitions.functions {
            if let Some((_, name)) = &function.name
                && let Some((module, external_function, _)) = &function.external_javascript
            {
                self.register_external_function(
                    arena,
                    &mut imports,
                    function.publicity,
                    name,
                    module,
                    external_function,
                )
            }
        }

        imports
    }

    fn import_path(&self, package: &str, module: &str) -> EcoString {
        // TODO: strip shared prefixed between current module and imported
        // module to avoid descending and climbing back out again
        if package == self.module.type_info.package || package.is_empty() {
            // Same package
            match self.current_module_name_segments_count {
                1 => eco_format!("./{module}.mjs"),
                _ => {
                    let prefix = "../".repeat(self.current_module_name_segments_count - 1);
                    eco_format!("{prefix}{module}.mjs")
                }
            }
        } else {
            // Different package
            let prefix = "../".repeat(self.current_module_name_segments_count);
            eco_format!("{prefix}{package}/{module}.mjs")
        }
    }

    fn register_import(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        imports: &mut Imports<'a, 'doc>,
        package: &'a str,
        module: &'a str,
        as_name: &Option<(AssignName, SrcSpan)>,
        unqualified: &[UnqualifiedImport],
    ) {
        let get_name = |module: &'a str| {
            module
                .split('/')
                .next_back()
                .expect("JavaScript generator could not identify imported module name.")
        };

        let (discarded, module_name) = match as_name {
            None => (false, get_name(module)),
            Some((AssignName::Discard(_), _)) => (true, get_name(module)),
            Some((AssignName::Variable(name), _)) => (false, name.as_str()),
        };

        let module_name = eco_format!("${module_name}");
        let path = self.import_path(package, module);
        let unqualified_imports = unqualified.iter().map(|i| {
            let alias = i.as_name.as_ref().map(|n| {
                self.register_in_scope(n);
                maybe_escape_identifier(n).to_doc(arena)
            });
            let name = maybe_escape_identifier(&i.name);
            Member { name, alias }
        });

        let aliases = if discarded { vec![] } else { vec![module_name] };
        imports.register_module(path, aliases, unqualified_imports);
    }

    fn register_external_function(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        imports: &mut Imports<'a, 'doc>,
        publicity: Publicity,
        name: &'a str,
        module: &'a str,
        fun: &EcoString,
    ) {
        let needs_escaping = !is_usable_js_identifier(name);
        let member = Member {
            name: fun.clone(),
            alias: if name == fun && !needs_escaping {
                None
            } else if needs_escaping {
                Some(escape_identifier(name).to_doc(arena))
            } else {
                Some(name.to_doc(arena))
            },
        };
        if publicity.is_importable() {
            imports.register_export(maybe_escape_identifier_string(name))
        }
        imports.register_module(EcoString::from(module), [], [member]);
    }

    fn module_constant(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constant: &'a TypedModuleConstant,
    ) -> Option<Document<'a, 'doc>> {
        let TypedModuleConstant {
            documentation,
            location,
            publicity,
            name,
            value,
            ..
        } = constant;

        // We don't generate any code for unused constants.
        if self
            .module
            .unused_definition_positions
            .contains(&location.start)
        {
            return None;
        }

        let head = if publicity.is_private() {
            CONST_SPACE_DOCUMENT
        } else {
            EXPORT_CONST_SPACE_DOCUMENT
        };

        let mut generator = expression::Generator::new(
            self.module.name.clone(),
            self.src_path.clone(),
            self.line_numbers,
            "".into(),
            vec![],
            &mut self.tracker,
            self.module_scope.clone(),
            self.source_map_builder.clone(),
        );

        let document = generator.constant_expression(arena, Context::Constant, value);

        let jsdoc = if let Some((start, documentation)) = documentation {
            docvec![
                arena,
                // 3 is the length of the "///" documentation marker.
                // Start is the index of the actual content, so we need to subtract
                // the length of the marker.
                self.source_map_tracker(arena, *start - 3),
                jsdoc_comment(arena, documentation, *publicity),
                LINE_DOCUMENT
            ]
        } else {
            EMPTY_DOCUMENT
        };

        Some(docvec![
            arena,
            jsdoc,
            self.source_map_tracker(arena, location.start),
            head,
            maybe_escape_identifier(name),
            SPACE_EQUAL_SPACE_DOCUMENT,
            document,
            SEMICOLON_DOCUMENT,
            self.source_map_tracker(arena, value.location().end),
        ])
    }

    fn register_in_scope(&mut self, name: &str) {
        let _ = self.module_scope.insert(name.into(), 0);
    }

    fn module_function(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        function: &'a TypedFunction,
    ) -> Option<Document<'a, 'doc>> {
        // We don't generate any code for unused functions.
        if self
            .module
            .unused_definition_positions
            .contains(&function.location.start)
        {
            return None;
        }

        // If there's an external JavaScript implementation then it will be imported,
        // so we don't need to generate a function definition.
        if function.external_javascript.is_some() {
            return None;
        }

        // If the function does not support JavaScript then we don't need to generate
        // a function definition.
        if !function.implementations.supports(Target::JavaScript) {
            return None;
        }
        let function_source_mapping = self.source_map_tracker(arena, function.location.start);

        let (_, name) = function
            .name
            .as_ref()
            .expect("A module's function must be named");
        let argument_names = function
            .arguments
            .iter()
            .map(|arg| arg.names.get_variable_name())
            .collect();

        let function_doc = match &function.documentation {
            None => EMPTY_DOCUMENT,
            Some((start, documentation)) => {
                docvec![
                    arena,
                    // 3 is the length of the "///" documentation marker.
                    // Start is the index of the actual content, so we need to subtract
                    // the length of the marker.
                    self.source_map_tracker(arena, *start - 3),
                    jsdoc_comment(arena, documentation, function.publicity),
                    LINE_DOCUMENT
                ]
            }
        };

        let mut generator = expression::Generator::new(
            self.module.name.clone(),
            self.src_path.clone(),
            self.line_numbers,
            name.clone(),
            argument_names,
            &mut self.tracker,
            self.module_scope.clone(),
            self.source_map_builder.clone(),
        );

        let head = if function.publicity.is_private() {
            FUNCTION_SPACE_DOCUMENT
        } else {
            EXPORT_FUNCTION_SPACE_DOCUMENT
        };

        let body = generator.function_body(
            arena,
            function.body.as_slice(),
            function.arguments.as_slice(),
        );

        Some(docvec![
            arena,
            function_doc,
            function_source_mapping,
            head,
            maybe_escape_identifier(name.as_str()),
            fun_arguments(
                arena,
                function.arguments.as_slice(),
                generator.tail_recursion_used
            ),
            SPACE_OPEN_CURLY_DOCUMENT,
            docvec![arena, LINE_DOCUMENT, body]
                .nest(arena, INDENT)
                .group(arena),
            LINE_DOCUMENT,
            CLOSE_CURLY_DOCUMENT,
            self.source_map_tracker(arena, function.end_position),
        ])
    }

    fn register_module_definitions_in_scope(&mut self) {
        for constant in &self.module.definitions.constants {
            self.register_in_scope(&constant.name)
        }

        for function in &self.module.definitions.functions {
            if let Some((_, name)) = &function.name {
                self.register_in_scope(name);
            }
        }

        for import in &self.module.definitions.imports {
            for unqualified_value in &import.unqualified_values {
                self.register_in_scope(unqualified_value.used_name())
            }
        }
    }

    fn filepath_definition(&self, arena: &'doc DocumentArena<'a, 'doc>) -> Document<'a, 'doc> {
        if !self.tracker.make_error_used {
            return EMPTY_DOCUMENT;
        }

        docvec![
            arena,
            CONST_FILEPATH_EQUALS_DOCUMENT,
            self.src_path.clone(),
            SEMICOLON_DOCUMENT,
            TWO_LINES_DOCUMENT
        ]
    }

    fn source_map_tracker(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        start_index: u32,
    ) -> Document<'a, 'doc> {
        create_cursor_position_observer(
            arena,
            &self.source_map_builder,
            self.line_numbers,
            start_index,
        )
    }
}

fn jsdoc_comment<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    documentation: &EcoString,
    publicity: Publicity,
) -> Document<'a, 'doc> {
    // We start with the documentation of the function
    let doc_body = arena.join(
        documentation
            .trim_end()
            .split('\n')
            .map(|line| eco_format!(" *{}", line.replace("*/", "*\\/")).to_doc(arena)),
        LINE_DOCUMENT,
    );

    let mut doc = docvec![
        arena,
        SLASH_TIMES_TIMES_DOCUMENT,
        LINE_DOCUMENT,
        doc_body,
        LINE_DOCUMENT
    ];
    if !publicity.is_public() {
        // If the function is not public we hide the documentation using
        // the `@ignore` tag: https://jsdoc.app/tags-ignore
        doc = docvec![
            arena,
            doc,
            SPACE_TIMES_SPACE_DOCUMENT,
            LINE_DOCUMENT,
            SPACE_TIMES_AT_IGNORE_DOCUMENT,
            LINE_DOCUMENT
        ];
    }
    // And finally we close the doc comment
    docvec![arena, doc, SPACE_TIMES_SLASH_DOCUMENT]
}

#[derive(Debug)]
pub struct ModuleConfig<'a> {
    pub module: &'a TypedModule,
    pub line_numbers: &'a LineNumbers,
    pub src: &'a EcoString,
    pub typescript: TypeScriptDeclarations,
    pub source_map: bool,
    pub stdlib_package: StdlibPackage,
    pub path: &'a Utf8Path,
    pub project_root: &'a Utf8Path,
}

pub fn module(config: ModuleConfig<'_>) -> (String, Option<SourceMap>) {
    let (output, sourcemap_builder) = {
        let arena = DocumentArena::new();
        let mut generator = Generator::new(config);
        let document = generator.compile(&arena).to_pretty_string(80);
        let builder = generator.source_map_builder;
        (document, builder)
    };

    let source_map = sourcemap_builder.map(|builder| {
        // We have completed the generation of the module, so we can now take ownership
        // of the builder.
        Rc::try_unwrap(builder)
            .expect("Failed to take ownership of sourcemap builder")
            .into_inner()
            .0
            .into_sourcemap()
    });
    (output, source_map)
}

pub fn ts_declaration(module: &TypedModule) -> String {
    let arena = DocumentArena::new();
    let document = typescript::TypeScriptGenerator::new(module).compile(&arena);
    document.to_pretty_string(80)
}

fn create_cursor_position_observer<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    builder: &Option<Rc<RefCell<DebugIgnore<sourcemap::SourceMapBuilder>>>>,
    line_numbers: &LineNumbers,
    start_index: u32,
) -> Document<'a, 'doc> {
    let start_location = line_numbers.line_and_column_number(start_index);
    arena.position_observer(match builder {
        None => Rc::new(RefCell::new(NullCursorPositionObserver)),
        Some(builder) => Rc::new(RefCell::new(SourceMapCursorPositionObserver::new(
            start_location,
            builder.clone(),
        ))),
    })
}

fn fun_arguments<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    arguments: &'a [TypedArg],
    tail_recursion_used: bool,
) -> Document<'a, 'doc> {
    let mut discards = 0;
    wrap_arguments(
        arena,
        arguments
            .iter()
            .map(|argument| match argument.get_variable_name() {
                None => {
                    let doc = if discards == 0 {
                        UNDERSCORE_DOCUMENT
                    } else {
                        eco_format!("_{discards}").to_doc(arena)
                    };
                    discards += 1;
                    doc
                }
                Some(name) if tail_recursion_used => eco_format!("loop${name}").to_doc(arena),
                Some(name) => maybe_escape_identifier(name).to_doc(arena),
            }),
    )
}

fn wrap_arguments<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    arguments: impl IntoIterator<Item = Document<'a, 'doc>>,
) -> Document<'a, 'doc> {
    EMPTY_BREAK_DOCUMENT
        .append(arena, arena.join(arguments, COMMA_BREAK_DOCUMENT))
        .nest(arena, INDENT)
        .append(arena, EMPTY_BREAK_DOCUMENT)
        .surround(arena, OPEN_PAREN_DOCUMENT, CLOSE_PAREN_DOCUMENT)
        .group(arena)
}

fn wrap_object<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    items: impl IntoIterator<Item = (Document<'a, 'doc>, Option<Document<'a, 'doc>>)>,
) -> Document<'a, 'doc> {
    let mut empty = true;
    let fields = items.into_iter().map(|(key, value)| {
        empty = false;
        match value {
            Some(value) => docvec![arena, key, COLON_SPACE_DOCUMENT, value],
            None => key.to_doc(arena),
        }
    });
    let fields = arena.join(fields, COMMA_BREAK_DOCUMENT);

    if empty {
        OPEN_CLOSE_CURLY_DOCUMENT
    } else {
        docvec![
            arena,
            docvec![arena, OPEN_CURLY_DOCUMENT, BREAKABLE_SPACE_DOCUMENT, fields]
                .nest(arena, INDENT)
                .append(arena, BREAKABLE_SPACE_DOCUMENT)
                .group(arena),
            CLOSE_CURLY_DOCUMENT
        ]
    }
}

fn is_usable_js_identifier(word: &str) -> bool {
    !matches!(
        word,
        // Keywords and reserved words
        // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar
        "await"
            | "arguments"
            | "break"
            | "case"
            | "catch"
            | "class"
            | "const"
            | "continue"
            | "debugger"
            | "default"
            | "delete"
            | "do"
            | "else"
            | "enum"
            | "export"
            | "extends"
            | "eval"
            | "false"
            | "finally"
            | "for"
            | "function"
            | "if"
            | "implements"
            | "import"
            | "in"
            | "instanceof"
            | "interface"
            | "let"
            | "new"
            | "null"
            | "package"
            | "private"
            | "protected"
            | "public"
            | "return"
            | "static"
            | "super"
            | "switch"
            | "this"
            | "throw"
            | "true"
            | "try"
            | "typeof"
            | "var"
            | "void"
            | "while"
            | "with"
            | "yield"
            // `undefined` to avoid any unintentional overriding.
            | "undefined"
            // `then` to avoid a module that defines a `then` function being
            // used as a `thenable` in JavaScript when the module is imported
            // dynamically, which results in unexpected behaviour.
            // It is rather unfortunate that we have to do this.
            | "then"
    )
}

fn is_usable_js_property(label: &str) -> bool {
    match label {
        // `then` to avoid a custom type that defines a `then` function being
        // used as a `thenable` in Javascript.
        "then"
        // `constructor` to avoid unintentional overriding of the constructor of
        // records, leading to potential runtime crashes while using `withFields`.
        | "constructor"
        // `prototype` and `__proto__` to avoid unintentionally overriding the
        // prototype chain.
        | "prototype" | "__proto__" => false,
        _ => true
    }
}

fn maybe_escape_identifier_string(word: &str) -> EcoString {
    if is_usable_js_identifier(word) {
        EcoString::from(word)
    } else {
        escape_identifier(word)
    }
}

fn escape_identifier(word: &str) -> EcoString {
    eco_format!("{word}$")
}

fn maybe_escape_identifier(word: &str) -> EcoString {
    if is_usable_js_identifier(word) {
        EcoString::from(word)
    } else {
        escape_identifier(word)
    }
}

fn maybe_escape_property(label: &str) -> EcoString {
    if is_usable_js_property(label) {
        EcoString::from(label)
    } else {
        escape_identifier(label)
    }
}

#[derive(Debug, Default)]
pub(crate) struct UsageTracker {
    pub ok_used: bool,
    pub list_used: bool,
    pub list_empty_class_used: bool,
    pub list_empty_const_used: bool,
    pub list_non_empty_class_used: bool,
    pub prepend_used: bool,
    pub error_used: bool,
    pub int_remainder_used: bool,
    pub make_error_used: bool,
    pub custom_type_used: bool,
    pub int_division_used: bool,
    pub float_division_used: bool,
    pub object_equality_used: bool,
    pub bit_array_literal_used: bool,
    pub bit_array_slice_used: bool,
    pub bit_array_slice_to_float_used: bool,
    pub bit_array_slice_to_int_used: bool,
    pub sized_integer_segment_used: bool,
    pub string_bit_array_segment_used: bool,
    pub string_utf16_bit_array_segment_used: bool,
    pub string_utf32_bit_array_segment_used: bool,
    pub codepoint_bit_array_segment_used: bool,
    pub codepoint_utf16_bit_array_segment_used: bool,
    pub codepoint_utf32_bit_array_segment_used: bool,
    pub float_bit_array_segment_used: bool,
    pub echo_used: bool,
    /// Keeps track of each time a fieldless variant is constructed, so we can
    /// import the singleton constant. Optionally contains an alias, if the variant
    /// was imported unqualified and aliased.
    pub variant_constants_used: HashMap<TypeVariant, Option<EcoString>>,
    /// Fieldless variants that are used in `instanceof` checks during pattern
    /// matching or comparison. If we're only using a fieldless variant for
    /// construction, we don't need to import the variant class itself, just it
    /// singleton constant. However, if we are using `instanceof`, we still need
    /// to import it.
    pub variants_used_in_instanceof: HashSet<TypeVariant>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TypeVariant {
    package: EcoString,
    module: EcoString,
    type_name: EcoString,
    name: EcoString,
}

fn bool(bool: bool) -> Document<'static, 'static> {
    match bool {
        true => TRUE_LOWERCASE_DOCUMENT,
        false => FALSE_LOWERCASE_DOCUMENT,
    }
}

/// Int segments <= 48 bits wide in bit arrays are within JavaScript's safe range and are evaluated
/// at compile time when all inputs are known. This is done for both bit array expressions and
/// pattern matching.
///
/// Int segments of any size could be evaluated at compile time, but currently aren't due to the
/// potential for causing large generated JS for inputs such as `<<0:8192>>`.
///
pub(crate) const SAFE_INT_SEGMENT_MAX_SIZE: usize = 48;

/// Evaluates the value of an Int segment in a bit array into its corresponding bytes. This avoids
/// needing to do the evaluation at runtime when all inputs are known at compile-time.
///
pub(crate) fn bit_array_segment_int_value_to_bytes(
    mut value: BigInt,
    size: BigInt,
    endianness: Endianness,
) -> Vec<u8> {
    // Clamp negative sizes to zero
    let size = size.max(BigInt::ZERO);

    // Convert size to u32. This is safe because this function isn't called with a size greater
    // than `SAFE_INT_SEGMENT_MAX_SIZE`.
    let size = size
        .to_u32()
        .expect("bit array segment size to be a valid u32");

    // Convert negative number to two's complement representation
    if value < BigInt::ZERO {
        let value_modulus = BigInt::from(2).pow(size);
        value = &value_modulus + (value % &value_modulus);
    }

    // Convert value to the desired number of bytes
    let mut bytes = vec![0u8; size as usize / 8];
    for byte in bytes.iter_mut() {
        *byte = (&value % BigInt::from(256))
            .to_u8()
            .expect("modulo result to be a valid u32");
        value /= BigInt::from(256);
    }

    if endianness.is_big() {
        bytes.reverse();
    }

    bytes
}
