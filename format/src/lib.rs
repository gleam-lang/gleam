// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2020 The Gleam contributors

#[cfg(test)]
mod tests;

use camino::Utf8Path;
use ecow::{EcoString, eco_format};
use gleam_core::{
    Error, Result,
    ast::{
        CustomType, Import, ModuleConstant, TypeAlias, TypeAstConstructor, TypeAstFn, TypeAstHole,
        TypeAstTuple, TypeAstVar, *,
    },
    build::Target,
    io::Utf8Writer,
    parse::extra::{Comment, ModuleExtra},
    type_::Deprecation,
    warning::WarningEmitter,
};
use itertools::Itertools;
use pretty_arena::*;
use std::cmp::Ordering;
use vec1::Vec1;

const INDENT: isize = 2;

pub fn pretty(writer: &mut impl Utf8Writer, src: &EcoString, path: &Utf8Path) -> Result<()> {
    let parsed = gleam_core::parse::parse_module(path.to_owned(), src, &WarningEmitter::null())
        .map_err(|error| Error::Parse {
            path: path.to_path_buf(),
            src: src.clone(),
            error: Box::new(error),
        })?;
    let intermediate = Intermediate::from_extra(&parsed.extra, src);
    let arena = DocumentArena::new();

    Formatter::with_comments(&intermediate)
        .module(&arena, &parsed.module)
        .pretty_print(80, writer)
        .map_err(|error| writer.convert_err(error))
}

pub(crate) struct Intermediate<'a> {
    comments: Vec<Comment<'a>>,
    doc_comments: Vec<Comment<'a>>,
    module_comments: Vec<Comment<'a>>,
    empty_lines: &'a [u32],
    new_lines: &'a [u32],
    trailing_commas: &'a [u32],
}

impl<'a> Intermediate<'a> {
    pub fn from_extra(extra: &'a ModuleExtra, src: &'a EcoString) -> Intermediate<'a> {
        Intermediate {
            comments: extra
                .comments
                .iter()
                .map(|span| Comment::from((span, src)))
                .collect(),
            doc_comments: extra
                .doc_comments
                .iter()
                .map(|span| Comment::from((span, src)))
                .collect(),
            empty_lines: &extra.empty_lines,
            module_comments: extra
                .module_comments
                .iter()
                .map(|span| Comment::from((span, src)))
                .collect(),
            new_lines: &extra.new_lines,
            trailing_commas: &extra.trailing_commas,
        }
    }
}

#[derive(Debug)]
enum FnCapturePosition {
    RightHandSideOfPipe,
    EverywhereElse,
}

#[derive(Debug)]
/// One of the pieces making a record update arg list: it could be the starting
/// record being updated, or one of the subsequent arguments.
///
enum RecordUpdatePiece<'a, A> {
    Record(&'a RecordBeingUpdated<A>),
    Argument(&'a RecordUpdateArg<A>),
}

impl<A> HasLocation for RecordUpdatePiece<'_, A> {
    fn location(&self) -> SrcSpan {
        match self {
            RecordUpdatePiece::Record(record) => record.location,
            RecordUpdatePiece::Argument(arg) => arg.location,
        }
    }
}

type UntypedRecordUpdatePiece<'a> = RecordUpdatePiece<'a, UntypedExpr>;

/// Hayleigh's bane
#[derive(Debug)]
pub struct Formatter<'a> {
    comments: &'a [Comment<'a>],
    doc_comments: &'a [Comment<'a>],
    module_comments: &'a [Comment<'a>],
    empty_lines: &'a [u32],
    new_lines: &'a [u32],
    trailing_commas: &'a [u32],
}

impl<'a, 'doc> Formatter<'a> {
    pub(crate) fn with_comments(extra: &'a Intermediate<'a>) -> Self {
        Self {
            comments: &extra.comments,
            doc_comments: &extra.doc_comments,
            module_comments: &extra.module_comments,
            empty_lines: extra.empty_lines,
            new_lines: extra.new_lines,
            trailing_commas: extra.trailing_commas,
        }
    }

    /// Returns true if there's any comment that comes before the given
    /// position.
    ///
    fn any_comments(&self, limit: u32) -> bool {
        self.comments
            .first()
            .is_some_and(|comment| comment.start < limit)
    }

    /// Returns true if there's any comment that appears inside the given span.
    ///
    fn any_comment_between(&self, start: u32, end: u32) -> bool {
        self.comments
            .binary_search_by(|comment| {
                if comment.start < start {
                    Ordering::Less
                } else if comment.start > end {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            })
            .is_ok()
    }

    fn any_empty_lines(&self, limit: u32) -> bool {
        self.empty_lines.first().is_some_and(|line| *line < limit)
    }

    /// Pop comments that occur before a byte-index in the source, consuming
    /// and retaining any empty lines contained within.
    /// Returns an iterator of comments with their start position.
    fn pop_comments_with_position(
        &mut self,
        limit: u32,
    ) -> impl Iterator<Item = (u32, Option<&'a str>)> + use<'a> {
        let (popped, rest, empty_lines) =
            comments_before(self.comments, self.empty_lines, limit, true);
        self.comments = rest;
        self.empty_lines = empty_lines;
        popped
    }

    /// Pop comments that occur before a byte-index in the source, consuming
    /// and retaining any empty lines contained within.
    fn pop_comments(&mut self, limit: u32) -> impl Iterator<Item = Option<&'a str>> + use<'a> {
        self.pop_comments_with_position(limit)
            .map(|(_position, comment)| comment)
    }

    /// Pop doc comments that occur before a byte-index in the source, consuming
    /// and dropping any empty lines contained within.
    fn pop_doc_comments(&mut self, limit: u32) -> impl Iterator<Item = Option<&'a str>> + use<'a> {
        let (popped, rest, empty_lines) =
            comments_before(self.doc_comments, self.empty_lines, limit, false);
        self.doc_comments = rest;
        self.empty_lines = empty_lines;
        popped.map(|(_position, comment)| comment)
    }

    /// Remove between 0 and `limit` empty lines following the current position,
    /// returning true if any empty lines were removed.
    fn pop_empty_lines(&mut self, limit: u32) -> bool {
        let mut end = 0;
        for (i, &position) in self.empty_lines.iter().enumerate() {
            if position > limit {
                break;
            }
            end = i + 1;
        }

        self.empty_lines = self
            .empty_lines
            .get(end..)
            .expect("Pop empty lines slicing");
        end != 0
    }

    fn targeted_definition(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        definition: &'a TargetedDefinition,
    ) -> Document<'a, 'doc> {
        let target = definition.target;
        let definition = &definition.definition;
        let start = definition.location().start;

        let comments = self.pop_comments_with_position(start);
        let comments = self.printed_documented_comments(arena, comments);
        let document = self.documented_definition(arena, definition);
        let document = match target {
            None => document,
            Some(Target::Erlang) => {
                docvec![arena, "@target(erlang)", LINE_DOCUMENT, document]
            }
            Some(Target::JavaScript) => {
                docvec![arena, "@target(javascript)", LINE_DOCUMENT, document]
            }
        };
        let document = document.group(arena);
        match comments {
            Some(comments) => comments.append(arena, document),
            None => document,
        }
    }

    pub(crate) fn module(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        module: &'a UntypedModule,
    ) -> Document<'a, 'doc> {
        let mut documents = vec![];
        let mut previous_was_a_definition = false;

        // Here we take consecutive groups of imports so that they can be sorted
        // alphabetically.
        for (is_import_group, definitions) in &module
            .definitions
            .iter()
            .chunk_by(|definition| definition.definition.is_import())
        {
            if is_import_group {
                if previous_was_a_definition {
                    documents.push(TWO_LINES_DOCUMENT);
                }
                documents.append(&mut self.imports(arena, definitions.collect_vec()));
                previous_was_a_definition = false;
            } else {
                for definition in definitions {
                    if !documents.is_empty() {
                        documents.push(TWO_LINES_DOCUMENT);
                    }
                    documents.push(self.targeted_definition(arena, definition));
                }
                previous_was_a_definition = true;
            }
        }

        let definitions = arena.concat(documents);

        // Now that definitions has been collected, only freestanding comments (//)
        // and doc comments (///) remain. Freestanding comments aren't associated
        // with any statement, and are moved to the bottom of the module.
        let doc_comments = arena.join(
            self.doc_comments.iter().map(|comment| {
                DOC_COMMENT_DOCUMENT
                    .to_doc(arena)
                    .append(arena, arena.zero_width_str(comment.content))
            }),
            LINE_DOCUMENT,
        );

        let comments = self.pop_comments(u32::MAX);
        let comments = match printed_comments(arena, comments, false) {
            Some(comments) => comments,
            None => EMPTY_DOCUMENT,
        };

        let module_comments = if !self.module_comments.is_empty() {
            let comments = self.module_comments.iter().map(|s| {
                MODULE_COMMENT_DOCUMENT
                    .to_doc(arena)
                    .append(arena, arena.zero_width_str(s.content))
            });
            arena
                .join(comments, LINE_DOCUMENT)
                .append(arena, LINE_DOCUMENT)
        } else {
            EMPTY_DOCUMENT
        };

        let non_empty = vec![module_comments, definitions, doc_comments, comments]
            .into_iter()
            .filter(|doc| !doc.is_empty());

        arena
            .join(non_empty, LINE_DOCUMENT)
            .append(arena, LINE_DOCUMENT)
    }

    /// Separates the imports in groups delimited by comments or empty lines and
    /// sorts each group alphabetically.
    ///
    /// The formatter needs to play nicely with import groups defined by the
    /// programmer. If one puts a comment before an import then that's a clue
    /// for the formatter that it has run into a gorup of related imports.
    ///
    /// So we can't just sort `imports` and format each one, we have to be a
    /// bit smarter and see if each import is preceded by a comment.
    /// Once we find a comment we know we're done with the current import
    /// group and a new one has started.
    ///
    /// ```gleam
    /// // This is an import group.
    /// import gleam/int
    /// import gleam/string
    ///
    /// // This marks the beginning of a new import group that can't
    /// // be mushed together with the previous one!
    /// import wibble
    /// import wobble
    /// ```
    fn imports(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        imports: Vec<&'a TargetedDefinition>,
    ) -> Vec<Document<'a, 'doc>> {
        let mut import_groups_docs = vec![];
        let mut current_group = vec![];
        let mut current_group_delimiter = EMPTY_DOCUMENT;

        for import in imports {
            let start = import.definition.location().start;

            // We need to start a new group if the `import` is preceded by one or
            // more empty lines or a `//` comment.
            let start_new_group = self.any_comments(start) || self.any_empty_lines(start);
            if start_new_group {
                // First we print the previous group and clear it out to start a
                // new empty group containing the import we've just ran into.
                if !current_group.is_empty() {
                    import_groups_docs.push(docvec![
                        arena,
                        current_group_delimiter,
                        self.sorted_import_group(arena, &current_group)
                    ]);
                    current_group.clear();
                }

                // Now that we've taken care of the previous group we can start
                // the new one. We know it's preceded either by an empty line or
                // some comments se we have to be a bit more precise and save the
                // actual delimiter that we're going to put at the top of this
                // group.

                let comments = self.pop_comments(start);
                let _ = self.pop_empty_lines(start);
                current_group_delimiter =
                    printed_comments(arena, comments, true).unwrap_or(EMPTY_DOCUMENT);
            }
            // Lastly we add the import to the group.
            current_group.push(import);
        }

        // Let's not forget about the last import group!
        if !current_group.is_empty() {
            import_groups_docs.push(docvec![
                arena,
                current_group_delimiter,
                self.sorted_import_group(arena, &current_group)
            ]);
        }

        // We want all consecutive import groups to be separated by an empty line.
        // This should really be `.intersperse(LINE_DOCUMENT)` but I can't do that
        // because of https://github.com/rust-lang/rust/issues/48919.
        Itertools::intersperse(import_groups_docs.into_iter(), TWO_LINES_DOCUMENT).collect_vec()
    }

    /// Prints the imports as a single sorted group of import statements.
    ///
    fn sorted_import_group(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        imports: &[&'a TargetedDefinition],
    ) -> Document<'a, 'doc> {
        let imports = imports
            .iter()
            .sorted_by(|one, other| match (&one.definition, &other.definition) {
                (Definition::Import(one), Definition::Import(other)) => {
                    one.module.cmp(&other.module)
                }
                // It shouldn't really be possible for a non import to be here so
                // we just return a default value.
                _ => Ordering::Equal,
            })
            .map(|import| self.targeted_definition(arena, import));

        arena.join(imports, LINE_DOCUMENT)
    }

    fn unqualified_import(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        unqualified_import: &'a UnqualifiedImport,
    ) -> Document<'a, 'doc> {
        unqualified_import.name.as_ref().to_doc(arena).append(
            arena,
            match &unqualified_import.as_name {
                None => EMPTY_DOCUMENT,
                Some(s) if s == &unqualified_import.name => EMPTY_DOCUMENT,
                Some(s) => " as ".to_doc(arena).append(arena, s.as_str()),
            },
        )
    }

    fn definition(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        statement: &'a UntypedDefinition,
    ) -> Document<'a, 'doc> {
        match statement {
            Definition::Function(function) => self.statement_fn(arena, function),

            Definition::TypeAlias(alias) => self.type_alias(arena, alias),

            Definition::CustomType(custom_type) => self.custom_type(arena, custom_type),

            Definition::Import(Import {
                module,
                as_name,
                unqualified_values,
                unqualified_types,
                ..
            }) => {
                let second = if unqualified_values.is_empty() && unqualified_types.is_empty() {
                    EMPTY_DOCUMENT
                } else {
                    let unqualified_types = unqualified_types
                        .iter()
                        .sorted_by(|a, b| a.name.cmp(&b.name))
                        .map(|import_| {
                            docvec![
                                arena,
                                TYPE_SPACE_DOCUMENT,
                                self.unqualified_import(arena, import_)
                            ]
                        });
                    let unqualified_values = unqualified_values
                        .iter()
                        .sorted_by(|a, b| a.name.cmp(&b.name))
                        .map(|import_| self.unqualified_import(arena, import_));
                    let unqualified = arena.join(
                        unqualified_types.chain(unqualified_values),
                        FLEX_COMMA_DOCUMENT,
                    );
                    let unqualified = EMPTY_BREAK_DOCUMENT
                        .append(arena, unqualified)
                        .nest(arena, INDENT)
                        .append(arena, TRAILING_COMMA_BREAK_DOCUMENT)
                        .group(arena);
                    ".{".to_doc(arena)
                        .append(arena, unqualified)
                        .append(arena, CLOSE_CURLY_DOCUMENT)
                };

                let doc = docvec![arena, IMPORT_SPACE_DOCUMENT, module.as_str(), second];
                let default_module_access_name = module.split('/').next_back().map(EcoString::from);
                match (default_module_access_name, as_name) {
                    // If the `as name` is the same as the module name that would be
                    // used anyways we won't render it. For example:
                    // ```gleam
                    // import gleam/int as int
                    //                  ^^^^^^ this is redundant and removed
                    // ```
                    (Some(module_name), Some((AssignName::Variable(name), _)))
                        if &module_name == name =>
                    {
                        doc
                    }
                    (_, None) => doc,
                    (_, Some((AssignName::Variable(name) | AssignName::Discard(name), _))) => doc
                        .append(arena, SPACE_AS_SPACE_DOCUMENT)
                        .append(arena, name),
                }
            }

            Definition::ModuleConstant(ModuleConstant {
                publicity,
                name,
                annotation,
                value,
                deprecation,
                documentation: _,
                location: _,
                name_location: _,
                type_: _,
                implementations: _,
            }) => {
                let attributes = AttributesPrinter::new()
                    .set_internal(*publicity)
                    .set_deprecation(deprecation)
                    .to_doc(arena);
                let head = attributes
                    .append(arena, pub_(*publicity))
                    .append(arena, CONST_SPACE_DOCUMENT)
                    .append(arena, name.as_str());
                let head = match annotation {
                    None => head,
                    Some(type_) => head
                        .append(arena, COLON_SPACE_DOCUMENT)
                        .append(arena, self.type_ast(arena, type_)),
                };
                head.append(arena, SPACE_EQUAL_SPACE_DOCUMENT)
                    .append(arena, self.const_expr(arena, value).group(arena))
            }
        }
    }

    fn const_expr<A>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        value: &'a Constant<A>,
    ) -> Document<'a, 'doc> {
        let comments = self.pop_comments(value.location().start);
        let document = match value {
            Constant::Todo { message, .. } => {
                self.append_as_message_constant(arena, TODO_DOCUMENT, message.as_deref())
            }

            Constant::Int { value, .. } => self.int(arena, value),

            Constant::Float { value, .. } => self.float(arena, value),

            Constant::String { value, .. } => self.string(arena, value),

            Constant::List {
                elements,
                location,
                tail,
                ..
            } => self.const_list(arena, elements, location, tail),

            Constant::Tuple {
                elements, location, ..
            } => self.const_tuple(arena, elements, location),

            Constant::BitArray {
                segments, location, ..
            } => {
                let segment_docs = segments
                    .iter()
                    .map(|segment| {
                        bit_array_segment(arena, segment, |expression| {
                            self.const_expr(arena, expression)
                        })
                    })
                    .collect_vec();

                let packing = self.items_sequence_packing(
                    segments,
                    None,
                    |segment| segment.value.can_have_multiple_per_line(),
                    *location,
                );

                self.bit_array(arena, segment_docs, packing, location)
            }

            Constant::Record {
                name,
                arguments: None,
                module: None,
                ..
            } => name.to_doc(arena),

            Constant::Record {
                name,
                arguments: None,
                module: Some((module, _)),
                ..
            } => module
                .to_doc(arena)
                .append(arena, DOT_DOCUMENT)
                .append(arena, name.as_str()),

            Constant::Record {
                name,
                arguments: Some(arguments),
                module: None,
                location,
                ..
            } => {
                let arguments = arguments
                    .iter()
                    .map(|argument| self.constant_call_arg(arena, argument))
                    .collect_vec();
                name.to_doc(arena)
                    .append(arena, self.wrap_arguments(arena, arguments, location.end))
                    .group(arena)
            }

            Constant::Record {
                name,
                arguments: Some(arguments),
                module: Some((module, _)),
                location,
                ..
            } => {
                let arguments = arguments
                    .iter()
                    .map(|argument| self.constant_call_arg(arena, argument))
                    .collect_vec();
                module
                    .to_doc(arena)
                    .append(arena, DOT_DOCUMENT)
                    .append(arena, name.as_str())
                    .append(arena, self.wrap_arguments(arena, arguments, location.end))
                    .group(arena)
            }

            Constant::Var {
                name, module: None, ..
            } => name.to_doc(arena),

            Constant::Var {
                name,
                module: Some((module, _)),
                ..
            } => docvec![arena, module, DOT_DOCUMENT, name],

            Constant::StringConcatenation { left, right, .. } => self
                .const_expr(arena, left)
                .append(
                    arena,
                    BREAKABLE_SPACE_DOCUMENT.append(arena, CONCAT_DOCUMENT),
                )
                .nest(arena, INDENT)
                .append(arena, SPACE_DOCUMENT)
                .append(arena, self.const_expr(arena, right)),

            Constant::RecordUpdate {
                module,
                name,
                record,
                arguments,
                location,
                ..
            } => self.const_record_update(arena, module, name, record, arguments, location),

            Constant::Invalid { .. } => panic!("invalid constants can not be in an untyped ast"),
        };
        commented(arena, document, comments)
    }

    fn const_list<A>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        elements: &'a [Constant<A>],
        location: &SrcSpan,
        tail: &'a Option<Box<Constant<A>>>,
    ) -> Document<'a, 'doc> {
        if elements.is_empty() {
            // We take all comments that come _before_ the end of the list,
            // that is all comments that are inside "[" and "]", if there's
            // any comment we want to put it inside the empty list!
            let comments = self.pop_comments(location.end);
            return match printed_comments(arena, comments, false) {
                None => OPEN_CLOSE_SQUARE_DOCUMENT,
                Some(comments) => OPEN_SQUARE_DOCUMENT
                    .append(arena, EMPTY_BREAK_DOCUMENT.nest(arena, INDENT))
                    .append(arena, comments)
                    .append(arena, EMPTY_BREAK_DOCUMENT)
                    .append(arena, CLOSE_SQUARE_DOCUMENT)
                    // vvv We want to make sure the comments are on a separate
                    //     line from the opening and closing brackets so we
                    //     force the breaks to be split on newlines.
                    .force_break(arena),
            };
        }

        let list_packing = self.items_sequence_packing(
            elements,
            tail.as_deref(),
            |element| element.can_have_multiple_per_line(),
            *location,
        );
        let comma = match list_packing {
            ItemsPacking::FitMultiplePerLine => FLEX_COMMA_DOCUMENT,
            ItemsPacking::FitOnePerLine | ItemsPacking::BreakOnePerLine => COMMA_BREAK_DOCUMENT,
        };

        let mut is_empty = true;
        let mut elements_doc = EMPTY_DOCUMENT;
        for element in elements.iter() {
            let empty_lines = self.pop_empty_lines(element.location().start);
            let element_doc = self.const_expr(arena, element);

            elements_doc = if is_empty {
                is_empty = false;
                element_doc
            } else if empty_lines {
                // If there's empty lines before the list item we want to add an
                // empty line here. Notice how we're making sure no nesting is
                // added after the comma, otherwise we would be adding needless
                // whitespace in the empty line!
                docvec![
                    arena,
                    elements_doc,
                    comma.set_nesting(arena, 0),
                    LINE_DOCUMENT,
                    element_doc
                ]
            } else {
                docvec![arena, elements_doc, comma, element_doc]
            };
        }
        elements_doc = elements_doc.next_break_fits(arena, NextBreakFitsMode::Disabled);

        let doc = OPEN_SQUARE_BREAK_DOCUMENT.append(arena, elements_doc);
        let (doc, final_break) = match tail {
            None => (doc.nest(arena, INDENT), TRAILING_COMMA_BREAK_DOCUMENT),
            Some(tail) => {
                let comments = self.pop_comments(tail.location().start);

                let tail = commented(
                    arena,
                    docvec![arena, DOT_DOT_DOCUMENT, self.const_expr(arena, tail)],
                    comments,
                );
                (
                    doc.append(arena, COMMA_BREAK_DOCUMENT)
                        .append(arena, tail)
                        .nest(arena, INDENT),
                    EMPTY_BREAK_DOCUMENT,
                )
            }
        };

        // We get all remaining comments that come before the list's closing
        // square bracket.
        // If there's any we add those before the closing square bracket instead
        // of moving those out of the list.
        // Otherwise those would be moved out of the list.
        let comments = self.pop_comments(location.end);
        let doc = match printed_comments(arena, comments, false) {
            None => doc
                .append(arena, final_break)
                .append(arena, CLOSE_SQUARE_DOCUMENT),
            Some(comment) => doc
                .append(arena, final_break.nest(arena, INDENT))
                // ^ See how here we're adding the missing indentation to the
                //   final break so that the final comment is as indented as the
                //   list's items.
                .append(arena, comment.nest(arena, INDENT))
                .append(arena, LINE_DOCUMENT)
                .append(arena, CLOSE_SQUARE_DOCUMENT)
                .force_break(arena),
        };

        match list_packing {
            ItemsPacking::FitOnePerLine | ItemsPacking::FitMultiplePerLine => doc.group(arena),
            ItemsPacking::BreakOnePerLine => doc.force_break(arena),
        }
    }

    pub fn const_tuple<A>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        elements: &'a [Constant<A>],
        location: &SrcSpan,
    ) -> Document<'a, 'doc> {
        if elements.is_empty() {
            // We take all comments that come _before_ the end of the tuple,
            // that is all comments that are inside "#(" and ")", if there's
            // any comment we want to put it inside the empty list!
            let comments = self.pop_comments(location.end);
            return match printed_comments(arena, comments, false) {
                None => EMPTY_TUPLE_DOCUMENT,
                Some(comments) => OPEN_TUPLE_DOCUMENT
                    .append(arena, EMPTY_BREAK_DOCUMENT.nest(arena, INDENT))
                    .append(arena, comments)
                    .append(arena, EMPTY_BREAK_DOCUMENT)
                    .append(arena, CLOSE_PAREN_DOCUMENT)
                    // vvv We want to make sure the comments are on a separate
                    //     line from the opening and closing parentheses so we
                    //     force the breaks to be split on newlines.
                    .force_break(arena),
            };
        }

        let arguments_docs = elements
            .iter()
            .map(|element| self.const_expr(arena, element));
        let tuple_doc = OPEN_TUPLE_BREAK_DOCUMENT
            .append(
                arena,
                arena
                    .join(arguments_docs, COMMA_BREAK_DOCUMENT)
                    .next_break_fits(arena, NextBreakFitsMode::Disabled),
            )
            .nest(arena, INDENT);

        let comments = self.pop_comments(location.end);
        match printed_comments(arena, comments, false) {
            None => tuple_doc
                .append(arena, TRAILING_COMMA_BREAK_DOCUMENT)
                .append(arena, CLOSE_PAREN_DOCUMENT)
                .group(arena),
            Some(comments) => tuple_doc
                .append(arena, TRAILING_COMMA_BREAK_DOCUMENT.nest(arena, INDENT))
                .append(arena, comments.nest(arena, INDENT))
                .append(arena, LINE_DOCUMENT)
                .append(arena, CLOSE_PAREN_DOCUMENT)
                .force_break(arena),
        }
    }

    fn documented_definition(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        definition: &'a UntypedDefinition,
    ) -> Document<'a, 'doc> {
        let comments = self.doc_comments(arena, definition.location().start);
        comments
            .append(arena, self.definition(arena, definition).group(arena))
            .group(arena)
    }

    fn doc_comments(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        limit: u32,
    ) -> Document<'a, 'doc> {
        let mut comments = self.pop_doc_comments(limit).peekable();
        match comments.peek() {
            None => EMPTY_DOCUMENT,
            Some(_) => arena
                .join(
                    comments.map(|comment| match comment {
                        Some(comment) => {
                            DOC_COMMENT_DOCUMENT.append(arena, arena.zero_width_str(comment))
                        }
                        None => unreachable!("empty lines dropped by pop_doc_comments"),
                    }),
                    LINE_DOCUMENT,
                )
                .append(arena, LINE_DOCUMENT)
                .force_break(arena),
        }
    }

    fn type_ast_constructor(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &'a TypeAstConstructorName,
        arguments: &'a [TypeAst],
        location: &SrcSpan,
    ) -> Document<'a, 'doc> {
        let head = match name {
            TypeAstConstructorName::Unqualified { name, .. } => name.to_doc(arena),
            TypeAstConstructorName::Qualified { module, name, .. } => {
                module.to_doc(arena).append(arena, DOT_DOCUMENT).append(
                    arena,
                    name.as_ref()
                        .map_or(EMPTY_DOCUMENT, |(name, _name_location)| name.to_doc(arena)),
                )
            }
        };

        if arguments.is_empty() {
            head
        } else {
            head.append(arena, self.type_arguments(arena, arguments, location))
        }
    }

    fn type_ast(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        type_: &'a TypeAst,
    ) -> Document<'a, 'doc> {
        let comments = self.pop_comments(type_.location().start);

        let type_ = match type_ {
            TypeAst::Hole(TypeAstHole { name, .. }) => name.to_doc(arena),

            TypeAst::Constructor(TypeAstConstructor {
                name,
                arguments,
                location,
                start_parentheses: _,
            }) => self.type_ast_constructor(arena, name, arguments, location),

            TypeAst::Fn(TypeAstFn {
                arguments,
                return_,
                location,
            }) => FN_DOCUMENT
                .append(arena, self.type_arguments(arena, arguments, location))
                .group(arena)
                .append(arena, SPACE_RIGHT_ARROW_DOCUMENT)
                .append(
                    arena,
                    BREAKABLE_SPACE_DOCUMENT
                        .append(arena, self.type_ast(arena, return_))
                        .group(arena)
                        .nest(arena, INDENT),
                ),

            TypeAst::Var(TypeAstVar { name, .. }) => name.to_doc(arena),

            TypeAst::Tuple(TypeAstTuple { elements, location }) => {
                HASHTAG_DOCUMENT.append(arena, self.type_arguments(arena, elements, location))
            }
        };

        commented(arena, type_.group(arena), comments)
    }

    fn type_arguments(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        arguments: &'a [TypeAst],
        location: &SrcSpan,
    ) -> Document<'a, 'doc> {
        let arguments = arguments
            .iter()
            .map(|type_| self.type_ast(arena, type_))
            .collect_vec();
        self.wrap_arguments(arena, arguments, location.end)
    }

    pub fn type_alias<A>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        alias: &'a TypeAlias<A>,
    ) -> Document<'a, 'doc> {
        let TypeAlias {
            alias: name,
            parameters: arguments,
            type_ast: type_,
            publicity,
            deprecation,
            location,
            name_location: _,
            type_: _,
            documentation: _,
        } = alias;

        let attributes = AttributesPrinter::new()
            .set_deprecation(deprecation)
            .set_internal(*publicity)
            .to_doc(arena);

        let head = docvec![
            arena,
            attributes,
            pub_(*publicity),
            TYPE_SPACE_DOCUMENT,
            name
        ];
        let head = if arguments.is_empty() {
            head
        } else {
            let arguments = arguments.iter().map(|(_, e)| e.to_doc(arena)).collect_vec();
            head.append(
                arena,
                self.wrap_arguments(arena, arguments, location.end)
                    .group(arena),
            )
        };

        head.append(arena, SPACE_EQUAL_DOCUMENT).append(
            arena,
            LINE_DOCUMENT
                .append(arena, self.type_ast(arena, type_))
                .group(arena)
                .nest(arena, INDENT),
        )
    }

    fn argument_names(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        argument_names: &'a ArgNames,
    ) -> Document<'a, 'doc> {
        match argument_names {
            ArgNames::Named { name, .. } | ArgNames::Discard { name, .. } => name.to_doc(arena),
            ArgNames::LabelledDiscard { label, name, .. }
            | ArgNames::NamedLabelled { label, name, .. } => {
                docvec![arena, label, " ", name]
            }
        }
    }

    fn fn_arg<A>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        argument: &'a Arg<A>,
    ) -> Document<'a, 'doc> {
        let comments = self.pop_comments(argument.location.start);
        let doc = match &argument.annotation {
            None => self.argument_names(arena, &argument.names),
            Some(type_) => self
                .argument_names(arena, &argument.names)
                .append(arena, COLON_SPACE_DOCUMENT)
                .append(arena, self.type_ast(arena, type_)),
        }
        .group(arena);
        commented(arena, doc, comments)
    }

    fn statement_fn(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        function: &'a UntypedFunction,
    ) -> Document<'a, 'doc> {
        let Function {
            location,
            body_start: _,
            end_position,
            name,
            arguments,
            body,
            publicity,
            deprecation,
            return_annotation,
            return_type: _,
            documentation: _,
            external_erlang,
            external_javascript,
            implementations: _,
            purity: _,
        } = function;

        let attributes = AttributesPrinter::new()
            .set_deprecation(deprecation)
            .set_internal(*publicity)
            .set_external_erlang(external_erlang)
            .set_external_javascript(external_javascript)
            .to_doc(arena);

        // Fn name and args
        let arguments = arguments
            .iter()
            .map(|argument| self.fn_arg(arena, argument))
            .collect_vec();
        let signature = pub_(*publicity)
            .append(arena, FN_SPACE_DOCUMENT)
            .append(
                arena,
                &name
                    .as_ref()
                    .expect("Function in a statement must be named")
                    .1,
            )
            .append(
                arena,
                self.wrap_arguments(
                    arena,
                    arguments,
                    // Calculate end location of arguments to not consume comments in
                    // return annotation
                    return_annotation
                        .as_ref()
                        .map_or(location.end, |ann| ann.location().start),
                ),
            );

        // Add return annotation
        let signature = match &return_annotation {
            Some(annotation) => signature
                .append(arena, SPACE_RIGHT_ARROW_SPACE_DOCUMENT)
                .append(arena, self.type_ast(arena, annotation)),
            None => signature,
        };

        if body.is_empty() {
            return docvec![arena, attributes, signature.group(arena)];
        }

        // Format body and add any trailing comments
        let body = self.statements(arena, body);
        let comments = self.pop_comments(*end_position);
        let body = match printed_comments(arena, comments, false) {
            Some(comments) => body.append(arena, LINE_DOCUMENT).append(arena, comments),
            None => body,
        };

        // Stick it all together
        let function = signature
            .append(arena, SPACE_OPEN_CURLY_DOCUMENT)
            .group(arena)
            .append(
                arena,
                LINE_DOCUMENT
                    .append(arena, body)
                    .nest(arena, INDENT)
                    .group(arena),
            )
            .append(arena, LINE_DOCUMENT)
            .append(arena, CLOSE_CURLY_DOCUMENT);

        docvec![arena, attributes, function]
    }

    #[allow(clippy::too_many_arguments)]
    fn expr_fn(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        arguments: &'a [UntypedArg],
        return_annotation: Option<&'a TypeAst>,
        body: &'a Vec1<UntypedStatement>,
        location: &SrcSpan,
        end_of_head_byte_index: &u32,
    ) -> Document<'a, 'doc> {
        let arguments_docs = arguments
            .iter()
            .map(|argument| self.fn_arg(arena, argument))
            .collect_vec();
        let arguments = self
            .wrap_arguments(arena, arguments_docs, *end_of_head_byte_index)
            .group(arena)
            .next_break_fits(arena, NextBreakFitsMode::Disabled);
        //   ^^^ We add this so that when an expression function is passed as
        //       the last argument of a function and it goes over the line
        //       limit with just its arguments we don't get some strange
        //       splitting.
        //       See https://github.com/gleam-lang/gleam/issues/2571
        //
        // There's many ways we could be smarter than this. For example:
        //  - still split the arguments like it did in the example shown in the
        //    issue if the expr_fn has more than one argument
        //  - make sure that an anonymous function whose body is made up of a
        //    single expression doesn't get split (I think that could boil down
        //    to wrapping the body with a `next_break_fits(Disabled)`)
        //
        // These are some of the ways we could tweak the look of expression
        // functions in the future if people are not satisfied with it.

        let header = "fn".to_doc(arena).append(arena, arguments);

        let header = match return_annotation {
            None => header,
            Some(return_annotation) => header
                .append(arena, SPACE_RIGHT_ARROW_SPACE_DOCUMENT)
                .append(arena, self.type_ast(arena, return_annotation)),
        };

        let statements = self.statements(arena, body.as_vec());
        let body = match printed_comments(arena, self.pop_comments(location.end), false) {
            None => statements,
            Some(comments) => statements
                .append(arena, LINE_DOCUMENT)
                .append(arena, comments)
                .force_break(arena),
        };

        header
            .append(arena, SPACE_DOCUMENT)
            .append(arena, wrap_block(arena, body))
            .group(arena)
    }

    fn statements(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        statements: &'a [UntypedStatement],
    ) -> Document<'a, 'doc> {
        let mut previous_position = 0;
        let count = statements.len();

        let mut documents = Vec::with_capacity(count * 2);
        for (i, statement) in statements.iter().enumerate() {
            let preceding_newline = self.pop_empty_lines(previous_position + 1);
            if i != 0 && preceding_newline {
                documents.push(TWO_LINES_DOCUMENT);
            } else if i != 0 {
                documents.push(LINE_DOCUMENT);
            }
            previous_position = statement.location().end;
            documents.push(self.statement(arena, statement).group(arena));

            // If the last statement is a use we make sure it's followed by a
            // todo to make it explicit it has an unimplemented callback.
            if statement.is_use() && i == count - 1 {
                documents.push(LINE_DOCUMENT);
                documents.push(TODO_DOCUMENT);
            }
        }

        if count == 1
            && statements
                .first()
                .is_some_and(|statement| statement.is_expression())
        {
            arena.concat(documents)
        } else {
            arena.concat(documents).force_break(arena)
        }
    }

    fn assignment(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        assignment: &'a UntypedAssignment,
    ) -> Document<'a, 'doc> {
        let comments = self.pop_comments(assignment.location.start);
        let Assignment {
            pattern,
            value,
            kind,
            annotation,
            ..
        } = assignment;

        let _ = self.pop_empty_lines(pattern.location().end);

        let (keyword, message) = match kind {
            AssignmentKind::Let | AssignmentKind::Generated => (LET_SPACE_DOCUMENT, None),
            AssignmentKind::Assert { message, .. } => (LET_ASSERT_SPACE_DOCUMENT, message.as_ref()),
        };

        let pattern = self.pattern(arena, pattern);

        let annotation = annotation
            .as_ref()
            .map(|annotation| COLON_SPACE_DOCUMENT.append(arena, self.type_ast(arena, annotation)))
            .unwrap_or(EMPTY_DOCUMENT);

        let doc = keyword
            .to_doc(arena)
            .append(arena, pattern.append(arena, annotation).group(arena))
            .append(arena, SPACE_EQUAL_DOCUMENT)
            .append(arena, self.assigned_value(arena, value));

        commented(
            arena,
            self.append_as_message_expression(arena, doc, PrecedingAs::Expression, message),
            comments,
        )
    }

    fn expr(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: &'a UntypedExpr,
    ) -> Document<'a, 'doc> {
        let comments = self.pop_comments(expression.start_byte_index());

        let document = match expression {
            UntypedExpr::Panic { message, .. } => self.append_as_message_expression(
                arena,
                PANIC_DOCUMENT,
                PrecedingAs::Keyword,
                message.as_deref(),
            ),

            UntypedExpr::Todo { message, .. } => self.append_as_message_expression(
                arena,
                TODO_DOCUMENT,
                PrecedingAs::Keyword,
                message.as_deref(),
            ),

            UntypedExpr::Echo {
                expression,
                location: _,
                keyword_end: _,
                message,
            } => self.echo(arena, expression, message),

            UntypedExpr::PipeLine { expressions, .. } => self.pipeline(arena, expressions, false),

            UntypedExpr::Int { value, .. } => self.int(arena, value),

            UntypedExpr::Float { value, .. } => self.float(arena, value),

            UntypedExpr::String { value, .. } => self.string(arena, value),

            UntypedExpr::Block {
                statements,
                location,
                ..
            } => self.block(arena, location, statements, false),

            UntypedExpr::Var { name, .. } if name == CAPTURE_VARIABLE => UNDERSCORE_DOCUMENT,

            UntypedExpr::Var { name, .. } => name.to_doc(arena),

            UntypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(arena, tuple, *index),

            UntypedExpr::NegateInt { value, .. } => self.negate_int(arena, value),

            UntypedExpr::NegateBool { value, .. } => self.negate_bool(arena, value),

            UntypedExpr::Fn { kind, body, .. } if kind.is_capture() => {
                self.fn_capture(arena, body, FnCapturePosition::EverywhereElse)
            }

            UntypedExpr::Fn {
                return_annotation,
                arguments,
                body,
                location,
                end_of_head_byte_index,
                ..
            } => self.expr_fn(
                arena,
                arguments,
                return_annotation.as_ref(),
                body,
                location,
                end_of_head_byte_index,
            ),

            UntypedExpr::List {
                elements,
                tail,
                location,
            } => self.list(arena, elements, tail.as_deref(), location),

            UntypedExpr::Call {
                fun,
                arguments,
                location,
                ..
            } => self.call(arena, fun, arguments, location),

            UntypedExpr::BinOp {
                operator,
                left,
                right,
                ..
            } => self.bin_op(arena, operator, left, right, false),

            UntypedExpr::Case {
                subjects,
                clauses,
                location,
            } => self.case(
                arena,
                subjects,
                clauses.as_deref().unwrap_or_default(),
                location,
            ),

            UntypedExpr::FieldAccess {
                label, container, ..
            } => self
                .expr(arena, container)
                .append(arena, DOT_DOCUMENT)
                .append(arena, label.as_str()),

            UntypedExpr::Tuple { elements, location } => self.tuple(arena, elements, location),

            UntypedExpr::BitArray {
                segments, location, ..
            } => {
                let segment_docs = segments
                    .iter()
                    .map(|segment| {
                        bit_array_segment(arena, segment, |e| self.bit_array_segment_expr(arena, e))
                    })
                    .collect_vec();

                let packing = self.items_sequence_packing(
                    segments,
                    None,
                    |segment| segment.value.can_have_multiple_per_line(),
                    *location,
                );

                self.bit_array(arena, segment_docs, packing, location)
            }
            UntypedExpr::RecordUpdate {
                constructor,
                record,
                arguments,
                location,
                ..
            } => self.record_update(arena, constructor, record, arguments, location),
        };
        commented(arena, document, comments)
    }

    fn string(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        string: &'a EcoString,
    ) -> Document<'a, 'doc> {
        let doc =
            string
                .to_doc(arena)
                .surround(arena, DOUBLE_QUOTE_DOCUMENT, DOUBLE_QUOTE_DOCUMENT);
        if string.contains('\n') {
            doc.force_break(arena)
        } else {
            doc
        }
    }

    fn bin_op_string(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        string: &'a EcoString,
    ) -> Document<'a, 'doc> {
        let lines = string.split('\n').collect_vec();
        match lines.as_slice() {
            [] | [_] => {
                string
                    .to_doc(arena)
                    .surround(arena, DOUBLE_QUOTE_DOCUMENT, DOUBLE_QUOTE_DOCUMENT)
            }
            [first_line, lines @ ..] => {
                let mut doc = docvec![arena, DOUBLE_QUOTE_DOCUMENT, *first_line];
                for line in lines {
                    doc = doc
                        .append(arena, LINE_DOCUMENT.set_nesting(arena, 0))
                        .append(arena, line.to_doc(arena))
                }
                doc.append(arena, DOUBLE_QUOTE_DOCUMENT).group(arena)
            }
        }
    }

    fn float(&self, arena: &'doc DocumentArena<'a, 'doc>, value: &'a str) -> Document<'a, 'doc> {
        // Create parts
        let mut parts = value.split('.');
        let integer_part = parts.next().unwrap_or_default();
        let floating_part = parts.next().unwrap_or_default();
        let integer_doc = self.underscore_integer_string(arena, integer_part);
        // Split fp_part into a regular fractional and maybe a scientific part
        let (fractional_part, scientific_part) = floating_part.split_at(
            floating_part
                .chars()
                .position(|character| character == 'e')
                .unwrap_or(floating_part.len()),
        );

        // Trim right any consequtive '0's
        let mut fractional_part = fractional_part.trim_end_matches('0').to_string();
        // If there is no fractional part left, add a '0', thus that 1. becomes 1.0 etc.
        if fractional_part.is_empty() {
            fractional_part.push('0');
        }
        let float_doc = fractional_part.chars().collect::<EcoString>();

        integer_doc
            .append(arena, DOT_DOCUMENT)
            .append(arena, float_doc)
            .append(arena, scientific_part)
    }

    fn int(&self, arena: &'doc DocumentArena<'a, 'doc>, value: &'a str) -> Document<'a, 'doc> {
        if value.starts_with("0x") || value.starts_with("0b") || value.starts_with("0o") {
            return value.to_doc(arena);
        }

        self.underscore_integer_string(arena, value)
    }

    fn underscore_integer_string(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        value: &'a str,
    ) -> Document<'a, 'doc> {
        let underscore = '_';
        let minus = '-';

        let len = value.len();
        let underscore_ch_cnt = value.matches(underscore).count();
        let reformat_watershed = if value.starts_with(minus) { 6 } else { 5 };
        let insert_underscores = (len - underscore_ch_cnt) >= reformat_watershed;

        let mut new_value = String::new();
        let mut j = 0;
        for (i, character) in value.chars().rev().enumerate() {
            if character == underscore {
                continue;
            }

            if insert_underscores && i != 0 && character != minus && i < len && j % 3 == 0 {
                new_value.push(underscore);
            }
            new_value.push(character);

            j += 1;
        }

        new_value.chars().rev().collect::<EcoString>().to_doc(arena)
    }

    #[allow(clippy::too_many_arguments)]
    fn pattern_constructor(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &'a str,
        arguments: &'a [CallArg<UntypedPattern>],
        module: &'a Option<(EcoString, SrcSpan)>,
        spread: Option<SrcSpan>,
        location: &SrcSpan,
    ) -> Document<'a, 'doc> {
        fn is_breakable(expression: &UntypedPattern) -> bool {
            match expression {
                Pattern::Tuple { .. } | Pattern::List { .. } | Pattern::BitArray { .. } => true,
                Pattern::Constructor { arguments, .. } => !arguments.is_empty(),
                Pattern::Int { .. }
                | Pattern::Float { .. }
                | Pattern::String { .. }
                | Pattern::Variable { .. }
                | Pattern::BitArraySize(_)
                | Pattern::Assign { .. }
                | Pattern::Discard { .. }
                | Pattern::StringPrefix { .. }
                | Pattern::Invalid { .. } => false,
            }
        }

        let name = match module {
            Some((module, _)) => module
                .to_doc(arena)
                .append(arena, DOT_DOCUMENT)
                .append(arena, name),
            None => name.to_doc(arena),
        };

        if arguments.is_empty() && spread.is_some() {
            name.append(arena, OPEN_PAREN_DOT_DOT_CLOSE_PAREN_DOCUMENT)
        } else if arguments.is_empty() {
            name
        } else if spread.is_some() {
            let arguments = arguments
                .iter()
                .map(|argument| self.pattern_call_arg(arena, argument))
                .collect_vec();
            name.append(
                arena,
                self.wrap_arguments_with_spread(arena, arguments, location.end),
            )
            .group(arena)
        } else {
            match arguments {
                [argument] if is_breakable(&argument.value) => name
                    .append(arena, OPEN_PAREN_DOCUMENT)
                    .append(arena, self.pattern_call_arg(arena, argument))
                    .append(arena, CLOSE_PAREN_DOCUMENT)
                    .group(arena),

                _ => {
                    let arguments = arguments
                        .iter()
                        .map(|argument| self.pattern_call_arg(arena, argument))
                        .collect_vec();
                    name.append(arena, self.wrap_arguments(arena, arguments, location.end))
                        .group(arena)
                }
            }
        }
    }

    fn call(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        function: &'a UntypedExpr,
        arguments: &'a [CallArg<UntypedExpr>],
        location: &SrcSpan,
    ) -> Document<'a, 'doc> {
        let expression = match function {
            UntypedExpr::PipeLine { .. } => break_block(arena, self.expr(arena, function)),

            UntypedExpr::BinOp { .. }
            | UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Block { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::Case { .. }
            | UntypedExpr::FieldAccess { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::TupleIndex { .. }
            | UntypedExpr::Todo { .. }
            | UntypedExpr::Echo { .. }
            | UntypedExpr::Panic { .. }
            | UntypedExpr::BitArray { .. }
            | UntypedExpr::RecordUpdate { .. }
            | UntypedExpr::NegateBool { .. }
            | UntypedExpr::NegateInt { .. } => self.expr(arena, function),
        };

        let arity = arguments.len();
        self.append_inlinable_wrapped_arguments(
            arena,
            expression,
            arguments,
            location,
            |argument| is_breakable_argument(&argument.value, arguments.len()),
            |self_, argument| self_.call_arg(arena, argument, arity),
        )
    }

    fn tuple(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        elements: &'a [UntypedExpr],
        location: &SrcSpan,
    ) -> Document<'a, 'doc> {
        if elements.is_empty() {
            // We take all comments that come _before_ the end of the tuple,
            // that is all comments that are inside "#(" and ")", if there's
            // any comment we want to put it inside the empty tuple!
            return match printed_comments(arena, self.pop_comments(location.end), false) {
                None => EMPTY_TUPLE_DOCUMENT,
                Some(comments) => OPEN_TUPLE_DOCUMENT
                    .append(arena, EMPTY_BREAK_DOCUMENT.nest(arena, INDENT))
                    .append(arena, comments)
                    .append(arena, EMPTY_BREAK_DOCUMENT)
                    .append(arena, CLOSE_PAREN_DOCUMENT)
                    // vvv We want to make sure the comments are on a separate
                    //     line from the opening and closing parentheses so we
                    //     force the breaks to be split on newlines.
                    .force_break(arena),
            };
        }

        self.append_inlinable_wrapped_arguments(
            arena,
            HASHTAG_DOCUMENT,
            elements,
            location,
            |expression| {
                !expression.is_tuple() && is_breakable_argument(expression, elements.len())
            },
            |self_, expression| self_.comma_separated_item(arena, expression, elements.len()),
        )
    }

    // Appends to the given docs a comma-separated list of documents wrapped by
    // parentheses. If the last item of the argument list is splittable the
    // resulting document will try to first split that before splitting all the
    // other arguments.
    // This is used for function calls and tuples.
    #[allow(clippy::too_many_arguments)]
    fn append_inlinable_wrapped_arguments<'b, T, Predicate, ToDoc>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        doc: Document<'a, 'doc>,
        values: &'b [T],
        location: &SrcSpan,
        is_breakable_argument: Predicate,
        to_doc: ToDoc,
    ) -> Document<'a, 'doc>
    where
        T: HasLocation,
        T: std::fmt::Debug,
        Predicate: Fn(&T) -> bool,
        ToDoc: Fn(&mut Self, &'b T) -> Document<'a, 'doc>,
    {
        match init_and_last(values) {
            Some((initial_values, last_value))
                if is_breakable_argument(last_value)
                    && !self.any_comments(last_value.location().start)
                    && !self.any_comment_between(last_value.location().end, location.end) =>
            {
                let mut docs = initial_values
                    .iter()
                    .map(|value| to_doc(self, value))
                    .collect_vec();

                let last_value_doc = to_doc(self, last_value)
                    .group(arena)
                    .next_break_fits(arena, NextBreakFitsMode::Enabled);

                docs.append(&mut vec![last_value_doc]);

                doc.append(
                    arena,
                    self.wrap_function_call_arguments(arena, docs, location),
                )
                .next_break_fits(arena, NextBreakFitsMode::Disabled)
                .group(arena)
            }

            Some(_) | None => {
                let docs = values.iter().map(|value| to_doc(self, value)).collect_vec();
                doc.append(
                    arena,
                    self.wrap_function_call_arguments(arena, docs, location),
                )
                .group(arena)
            }
        }
    }

    pub fn case(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        subjects: &'a [UntypedExpr],
        clauses: &'a [UntypedClause],
        location: &'a SrcSpan,
    ) -> Document<'a, 'doc> {
        let subjects_doc = CASE_BREAK_DOCUMENT
            .append(
                arena,
                arena.join(
                    subjects
                        .iter()
                        .map(|subject| self.expr(arena, subject).group(arena)),
                    COMMA_BREAK_DOCUMENT,
                ),
            )
            .nest(arena, INDENT)
            .append(arena, BREAKABLE_SPACE_DOCUMENT)
            .append(arena, OPEN_CURLY_DOCUMENT)
            .next_break_fits(arena, NextBreakFitsMode::Disabled)
            .group(arena);

        let clauses_doc = arena.concat(
            clauses
                .iter()
                .enumerate()
                .map(|(i, clause)| self.clause(arena, clause, i as u32).group(arena)),
        );

        // We get all remaining comments that come before the case's closing
        // bracket. If there's any we add those before the closing bracket
        // instead of moving those out of the case expression.
        // Otherwise those would be moved out of the case expression.
        let comments = self.pop_comments(location.end);
        let closing_bracket = match printed_comments(arena, comments, false) {
            None => docvec![arena, LINE_DOCUMENT, CLOSE_CURLY_DOCUMENT],
            Some(comment) => docvec![arena, LINE_DOCUMENT, comment]
                .nest(arena, INDENT)
                .append(arena, LINE_DOCUMENT)
                .append(arena, CLOSE_CURLY_DOCUMENT),
        };

        subjects_doc
            .append(
                arena,
                LINE_DOCUMENT.append(arena, clauses_doc).nest(arena, INDENT),
            )
            .append(arena, closing_bracket)
            .force_break(arena)
    }

    pub fn record_update(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a UntypedExpr,
        record: &'a RecordBeingUpdated<UntypedExpr>,
        arguments: &'a [UntypedRecordUpdateArg],
        location: &SrcSpan,
    ) -> Document<'a, 'doc> {
        let constructor_doc: Document<'a, 'doc> = self.expr(arena, constructor);
        let pieces = std::iter::once(UntypedRecordUpdatePiece::Record(record))
            .chain(arguments.iter().map(UntypedRecordUpdatePiece::Argument))
            .collect_vec();

        self.append_inlinable_wrapped_arguments(
            arena,
            constructor_doc,
            &pieces,
            location,
            |argument| {
                let expression = match argument {
                    UntypedRecordUpdatePiece::Argument(argument) => &argument.value,
                    UntypedRecordUpdatePiece::Record(record) => &record.base,
                };
                is_breakable_argument(expression, pieces.len())
            },
            |this, argument| match argument {
                UntypedRecordUpdatePiece::Argument(arg) => this.record_update_arg(arena, arg),
                UntypedRecordUpdatePiece::Record(record) => {
                    let comments = this.pop_comments(record.location.start);
                    commented(
                        arena,
                        DOT_DOT_DOCUMENT.append(arena, this.expr(arena, &record.base)),
                        comments,
                    )
                }
            },
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub fn const_record_update<A>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        module: &Option<(EcoString, SrcSpan)>,
        name: &'a EcoString,
        record: &'a RecordBeingUpdated<Constant<A>>,
        arguments: &'a [RecordUpdateArg<Constant<A>>],
        location: &SrcSpan,
    ) -> Document<'a, 'doc> {
        let constructor_doc = match module {
            Some((m, _)) => m
                .to_doc(arena)
                .append(arena, DOT_DOCUMENT)
                .append(arena, name.as_str()),
            None => name.to_doc(arena),
        };

        let pieces = std::iter::once(RecordUpdatePiece::Record(record))
            .chain(arguments.iter().map(RecordUpdatePiece::Argument))
            .collect_vec();

        let docs = pieces
            .iter()
            .map(|piece| match piece {
                RecordUpdatePiece::Argument(argument) => {
                    let comments = self.pop_comments(argument.location.start);
                    let doc = match argument {
                        _ if argument.uses_label_shorthand() => argument
                            .label
                            .as_str()
                            .to_doc(arena)
                            .append(arena, COLON_DOCUMENT),
                        _ => argument
                            .label
                            .as_str()
                            .to_doc(arena)
                            .append(arena, COLON_SPACE_DOCUMENT)
                            .append(arena, self.const_expr(arena, &argument.value))
                            .group(arena),
                    };
                    commented(arena, doc, comments)
                }
                RecordUpdatePiece::Record(record) => {
                    let comments = self.pop_comments(record.location.start);
                    commented(
                        arena,
                        DOT_DOT_DOCUMENT.append(arena, self.const_expr(arena, &record.base)),
                        comments,
                    )
                }
            })
            .collect_vec();

        constructor_doc
            .append(arena, self.wrap_arguments(arena, docs, location.end))
            .group(arena)
    }

    pub fn bin_op(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &'a BinOp,
        left: &'a UntypedExpr,
        right: &'a UntypedExpr,
        nest_steps: bool,
    ) -> Document<'a, 'doc> {
        let left_side = self.bin_op_side(arena, name, left, nest_steps);

        let comments = self.pop_comments(right.start_byte_index());
        let name_doc =
            BREAKABLE_SPACE_DOCUMENT.append(arena, commented(arena, binop(*name), comments));

        let right_side = self.bin_op_side(arena, name, right, nest_steps);

        left_side
            .append(
                arena,
                if nest_steps {
                    name_doc.nest(arena, INDENT)
                } else {
                    name_doc
                },
            )
            .append(arena, SPACE_DOCUMENT)
            .append(arena, right_side)
    }

    fn bin_op_side(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        operator: &'a BinOp,
        side: &'a UntypedExpr,
        nest_steps: bool,
    ) -> Document<'a, 'doc> {
        let side_doc = match side {
            UntypedExpr::String { value, .. } => self.bin_op_string(arena, value),
            UntypedExpr::BinOp {
                operator,
                left,
                right,
                ..
            } => self.bin_op(arena, operator, left, right, nest_steps),
            UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::Block { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::PipeLine { .. }
            | UntypedExpr::Case { .. }
            | UntypedExpr::FieldAccess { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::TupleIndex { .. }
            | UntypedExpr::Todo { .. }
            | UntypedExpr::Panic { .. }
            | UntypedExpr::Echo { .. }
            | UntypedExpr::BitArray { .. }
            | UntypedExpr::RecordUpdate { .. }
            | UntypedExpr::NegateBool { .. }
            | UntypedExpr::NegateInt { .. } => self.expr(arena, side),
        };
        match side.bin_op_name() {
            // In case the other side is a binary operation as well and it can
            // be grouped together with the current binary operation, the two
            // docs are simply concatenated, so that they will end up in the
            // same group and the formatter will try to keep those on a single
            // line.
            Some(side_name) if side_name.can_be_grouped_with(operator) => side_doc,
            // In case the binary operations cannot be grouped together the
            // other side is treated as a group on its own so that it can be
            // broken independently of other pieces of the binary operations
            // chain.
            _ => self.operator_side(
                arena,
                side_doc.group(arena),
                operator.precedence(),
                side.bin_op_precedence(),
            ),
        }
    }

    pub fn operator_side(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        doc: Document<'a, 'doc>,
        op: u8,
        side: u8,
    ) -> Document<'a, 'doc> {
        if op > side {
            wrap_block(arena, doc).group(arena)
        } else {
            doc
        }
    }

    fn spans_multiple_lines(&self, start: u32, end: u32) -> bool {
        self.new_lines
            .binary_search_by(|newline| {
                if *newline <= start {
                    Ordering::Less
                } else if *newline >= end {
                    Ordering::Greater
                } else {
                    // If the newline is in between the pipe start and end
                    // then we've found it!
                    Ordering::Equal
                }
            })
            // If we couldn't find any newline between the start and end of
            // the pipeline then we will try and keep it on a single line.
            .is_ok()
    }

    /// Returns true if there's a trailing comma between `start` and `end`.
    ///
    fn has_trailing_comma(&self, start: u32, end: u32) -> bool {
        self.trailing_commas
            .binary_search_by(|comma| {
                if *comma < start {
                    Ordering::Less
                } else if *comma > end {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            })
            .is_ok()
    }

    fn pipeline(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expressions: &'a Vec1<UntypedExpr>,
        nest_pipe: bool,
    ) -> Document<'a, 'doc> {
        // We start by producing the document for the first step of the pipeline.
        let first = expressions.first();
        let first_precedence = first.bin_op_precedence();
        let first = self.expr(arena, first).group(arena);
        let first_step = self.operator_side(arena, first, 5, first_precedence);

        // We then produce a doc for each item in the rest of the pipeline.
        let pipeline_start = expressions.first().location().start;
        let pipeline_end = expressions.last().location().end;
        let try_to_keep_on_one_line = !self.spans_multiple_lines(pipeline_start, pipeline_end);
        let steps_separator = if try_to_keep_on_one_line {
            BREAKABLE_SPACE_DOCUMENT
        } else {
            LINE_DOCUMENT
        };

        let other_steps = expressions.iter().skip(1).map(|expression| {
            // We start by producing the document for the pipe itself `|>`,
            // we also want to put comments before it!
            let comments = self.pop_comments(expression.location().start);
            let commented_pipe = commented(arena, PIPE_SPACE_DOCUMENT, comments);
            let mut pipe = docvec![arena, steps_separator, commented_pipe];
            if nest_pipe {
                pipe = pipe.nest(arena, INDENT);
            };

            // We then produce the document for the expression being piped into.
            let expression_doc =
                self.expression_on_right_hand_side_of_pipe(arena, nest_pipe, expression);
            let expression_doc =
                self.operator_side(arena, expression_doc, 4, expression.bin_op_precedence());

            // And finally piece everything together.
            pipe.append(arena, expression_doc)
        });

        let stops = std::iter::once(first_step).chain(other_steps);
        if try_to_keep_on_one_line {
            arena.concat(stops)
        } else {
            arena.concat(stops).force_break(arena)
        }
    }

    fn expression_on_right_hand_side_of_pipe(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        nest_pipe: bool,
        expression: &'a UntypedExpr,
    ) -> Document<'a, 'doc> {
        let expression = if let UntypedExpr::Fn { kind, body, .. } = expression
            && kind.is_capture()
        {
            self.fn_capture(arena, body, FnCapturePosition::RightHandSideOfPipe)
        } else {
            self.expr(arena, expression)
        };

        if nest_pipe {
            expression.nest(arena, INDENT)
        } else {
            expression
        }
    }

    fn fn_capture(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        call: &'a [UntypedStatement],
        position: FnCapturePosition,
    ) -> Document<'a, 'doc> {
        // The body of a capture being multiple statements shouldn't be possible...
        if call.len() != 1 {
            panic!("Function capture found not to have a single statement call");
        }

        let Some(Statement::Expression(UntypedExpr::Call {
            fun,
            arguments,
            location,
            ..
        })) = call.first()
        else {
            // The body of a capture being not a fn shouldn't be possible...
            panic!("Function capture body found not to be a call in the formatter")
        };

        match (position, arguments.as_slice()) {
            // The capture has a single unlabelled hole:
            //
            //     wibble |> wobble(_)
            //     list.map([], wobble(_))
            //
            // We want these to become:
            //
            //     wibble |> wobble
            //     list.map([], wobble)
            //
            (
                FnCapturePosition::RightHandSideOfPipe | FnCapturePosition::EverywhereElse,
                [argument],
            ) if argument.is_capture_hole() && argument.label.is_none() => self.expr(arena, fun),

            // The capture is on the right hand side of a pipe and its first
            // argument it an unlabelled capture hole:
            //
            //     wibble |> wobble(_, woo)
            //
            // We want it to become:
            //
            //     wibble |> wobble(woo)
            //
            (FnCapturePosition::RightHandSideOfPipe, [argument, rest @ ..])
                if argument.is_capture_hole() && argument.label.is_none() =>
            {
                let expression = self.expr(arena, fun);
                let arity = rest.len();
                self.append_inlinable_wrapped_arguments(
                    arena,
                    expression,
                    rest,
                    location,
                    |argument| is_breakable_argument(&argument.value, rest.len()),
                    |self_, argument| self_.call_arg(arena, argument, arity),
                )
            }

            // In all other cases we print it like a regular function call
            // without changing it.
            //
            (
                FnCapturePosition::RightHandSideOfPipe | FnCapturePosition::EverywhereElse,
                arguments,
            ) => {
                let expression = self.expr(arena, fun);
                let arity = arguments.len();
                self.append_inlinable_wrapped_arguments(
                    arena,
                    expression,
                    arguments,
                    location,
                    |argument| is_breakable_argument(&argument.value, arguments.len()),
                    |self_, argument| self_.call_arg(arena, argument, arity),
                )
            }
        }
    }

    pub fn record_constructor<A>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        constructor: &'a RecordConstructor<A>,
    ) -> Document<'a, 'doc> {
        let comments = self.pop_comments(constructor.location.start);
        let doc_comments = self.doc_comments(arena, constructor.location.start);
        let attributes = AttributesPrinter::new()
            .set_deprecation(&constructor.deprecation)
            .to_doc(arena);

        let doc = if constructor.arguments.is_empty() {
            if self.any_comments(constructor.location.end) {
                attributes
                    .append(arena, constructor.name.as_str().to_doc(arena))
                    .append(
                        arena,
                        self.wrap_arguments(arena, vec![], constructor.location.end),
                    )
                    .group(arena)
            } else {
                attributes.append(arena, constructor.name.as_str().to_doc(arena))
            }
        } else {
            let arguments = constructor
                .arguments
                .iter()
                .map(
                    |RecordConstructorArg {
                         label,
                         ast,
                         location,
                         ..
                     }| {
                        let arg_comments = self.pop_comments(location.start);
                        let argument = match label {
                            Some((_, label)) => label
                                .to_doc(arena)
                                .append(arena, COLON_SPACE_DOCUMENT)
                                .append(arena, self.type_ast(arena, ast)),
                            None => self.type_ast(arena, ast),
                        };

                        commented(
                            arena,
                            self.doc_comments(arena, location.start)
                                .append(arena, argument)
                                .group(arena),
                            arg_comments,
                        )
                    },
                )
                .collect_vec();

            attributes
                .append(arena, constructor.name.as_str().to_doc(arena))
                .append(
                    arena,
                    self.wrap_arguments(arena, arguments, constructor.location.end)
                        .group(arena),
                )
        };

        commented(
            arena,
            doc_comments.append(arena, doc).group(arena),
            comments,
        )
    }

    pub fn custom_type<A>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        type_: &'a CustomType<A>,
    ) -> Document<'a, 'doc> {
        let CustomType {
            location,
            end_position,
            name,
            name_location: _,
            publicity,
            constructors,
            documentation: _,
            deprecation,
            opaque,
            parameters,
            typed_parameters: _,
            external_erlang,
            external_javascript,
        } = type_;

        let _ = self.pop_empty_lines(location.end);

        let attributes = AttributesPrinter::new()
            .set_deprecation(deprecation)
            .set_internal(*publicity)
            .set_external_erlang(external_erlang)
            .set_external_javascript(external_javascript)
            .to_doc(arena);

        let doc = attributes
            .append(arena, pub_(*publicity))
            .append(
                arena,
                if *opaque {
                    OPAQUE_TYPE_SPACE_DOCUMENT
                } else {
                    TYPE_SPACE_DOCUMENT
                },
            )
            .append(
                arena,
                if parameters.is_empty() {
                    name.clone().to_doc(arena)
                } else {
                    let arguments = type_
                        .parameters
                        .iter()
                        .map(|(_, e)| e.to_doc(arena))
                        .collect_vec();
                    type_
                        .name
                        .clone()
                        .to_doc(arena)
                        .append(arena, self.wrap_arguments(arena, arguments, location.end))
                        .group(arena)
                },
            );

        if constructors.is_empty() {
            return doc;
        }
        let doc = doc.append(arena, SPACE_OPEN_CURLY_DOCUMENT);

        let inner = arena.concat(constructors.iter().map(|c| {
            if self.pop_empty_lines(c.location.start) {
                TWO_LINES_DOCUMENT
            } else {
                LINE_DOCUMENT
            }
            .append(arena, self.record_constructor(arena, c))
        }));

        // Add any trailing comments
        let inner = match printed_comments(arena, self.pop_comments(*end_position), false) {
            Some(comments) => inner.append(arena, LINE_DOCUMENT).append(arena, comments),
            None => inner,
        }
        .nest(arena, INDENT)
        .group(arena);

        doc.append(arena, inner)
            .append(arena, LINE_DOCUMENT)
            .append(arena, CLOSE_CURLY_DOCUMENT)
    }

    fn call_arg(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        argument: &'a CallArg<UntypedExpr>,
        arity: usize,
    ) -> Document<'a, 'doc> {
        self.format_call_arg(arena, argument, expr_call_arg_formatting, |this, value| {
            this.comma_separated_item(arena, value, arity)
        })
    }

    fn format_call_arg<A, F, G>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        argument: &'a CallArg<A>,
        figure_formatting: F,
        format_value: G,
    ) -> Document<'a, 'doc>
    where
        F: Fn(&'a CallArg<A>) -> CallArgFormatting<'a, A>,
        G: Fn(&mut Self, &'a A) -> Document<'a, 'doc>,
    {
        match figure_formatting(argument) {
            CallArgFormatting::Unlabelled(value) => format_value(self, value),
            CallArgFormatting::ShorthandLabelled(label) => {
                let comments = self.pop_comments(argument.location.start);
                let label = label.as_ref().to_doc(arena).append(arena, COLON_DOCUMENT);
                commented(arena, label, comments)
            }
            CallArgFormatting::Labelled(label, value) => {
                let comments = self.pop_comments(argument.location.start);
                let label = label
                    .as_ref()
                    .to_doc(arena)
                    .append(arena, COLON_SPACE_DOCUMENT);
                let value = format_value(self, value);
                commented(arena, label, comments).append(arena, value)
            }
        }
    }

    fn record_update_arg(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        argument: &'a UntypedRecordUpdateArg,
    ) -> Document<'a, 'doc> {
        let comments = self.pop_comments(argument.location.start);
        match argument {
            // Argument supplied with a label shorthand.
            _ if argument.uses_label_shorthand() => commented(
                arena,
                argument
                    .label
                    .as_str()
                    .to_doc(arena)
                    .append(arena, COLON_DOCUMENT),
                comments,
            ),
            // Labelled argument.
            _ => {
                let doc = argument
                    .label
                    .as_str()
                    .to_doc(arena)
                    .append(arena, COLON_SPACE_DOCUMENT)
                    .append(arena, self.expr(arena, &argument.value))
                    .group(arena);

                if argument.value.is_binop() || argument.value.is_pipeline() {
                    commented(arena, doc, comments).nest(arena, INDENT)
                } else {
                    commented(arena, doc, comments)
                }
            }
        }
    }

    fn tuple_index(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        tuple: &'a UntypedExpr,
        index: u64,
    ) -> Document<'a, 'doc> {
        // In case we have a block with a single variable tuple access we
        // remove that redundant wrapper:
        //
        //     {tuple.1}.0 becomes
        //     tuple.1.0
        //
        if let UntypedExpr::Block { statements, .. } = tuple {
            match statements.as_slice() {
                [Statement::Expression(tuple @ UntypedExpr::TupleIndex { tuple: inner, .. })]
                    // We can't apply this change if the inner thing is a
                    // literal tuple because the compiler cannot currently parse
                    // it:  `#(1, #(2, 3)).1.0` is a syntax error at the moment.
                    if !inner.is_tuple() =>
                {
                    self.expr(arena,tuple)
                }
                _ => self.expr(arena,tuple),
            }
        } else {
            self.expr(arena, tuple)
        }
        .append(arena, DOT_DOCUMENT)
        .append(arena, index)
    }

    fn case_clause_value(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: &'a UntypedExpr,
    ) -> Document<'a, 'doc> {
        match expression {
            UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::BitArray { .. } => {
                let expression_comments = self.pop_comments(expression.location().start);
                let expression_doc = self.expr(arena, expression);
                match printed_comments(arena, expression_comments, true) {
                    Some(comments) => LINE_DOCUMENT
                        .append(arena, comments)
                        .append(arena, expression_doc)
                        .nest(arena, INDENT),
                    None => SPACE_DOCUMENT.append(arena, expression_doc),
                }
            }

            UntypedExpr::Case { .. } => LINE_DOCUMENT
                .append(arena, self.expr(arena, expression))
                .nest(arena, INDENT),

            UntypedExpr::Block {
                statements,
                location,
                ..
            } => SPACE_DOCUMENT.append(arena, self.block(arena, location, statements, true)),

            UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::BinOp { .. }
            | UntypedExpr::PipeLine { .. }
            | UntypedExpr::FieldAccess { .. }
            | UntypedExpr::TupleIndex { .. }
            | UntypedExpr::Todo { .. }
            | UntypedExpr::Panic { .. }
            | UntypedExpr::Echo { .. }
            | UntypedExpr::RecordUpdate { .. }
            | UntypedExpr::NegateBool { .. }
            | UntypedExpr::NegateInt { .. } => BREAKABLE_SPACE_DOCUMENT
                .append(arena, self.expr(arena, expression).group(arena))
                .nest(arena, INDENT),
        }
        .next_break_fits(arena, NextBreakFitsMode::Disabled)
        .group(arena)
    }

    fn assigned_value(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: &'a UntypedExpr,
    ) -> Document<'a, 'doc> {
        match expression {
            UntypedExpr::Case { .. } => SPACE_DOCUMENT
                .append(arena, self.expr(arena, expression))
                .group(arena),
            UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Block { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::BinOp { .. }
            | UntypedExpr::PipeLine { .. }
            | UntypedExpr::FieldAccess { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::TupleIndex { .. }
            | UntypedExpr::Todo { .. }
            | UntypedExpr::Panic { .. }
            | UntypedExpr::Echo { .. }
            | UntypedExpr::BitArray { .. }
            | UntypedExpr::RecordUpdate { .. }
            | UntypedExpr::NegateBool { .. }
            | UntypedExpr::NegateInt { .. } => self.case_clause_value(arena, expression),
        }
    }

    fn clause(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        clause: &'a UntypedClause,
        index: u32,
    ) -> Document<'a, 'doc> {
        let space_before = self.pop_empty_lines(clause.location.start);
        let comments = self.pop_comments(clause.location.start);

        let clause_doc = match &clause.guard {
            None => self.alternative_patterns(arena, clause),
            Some(guard) => self
                .alternative_patterns(arena, clause)
                .append(arena, BREAKABLE_SPACE_DOCUMENT.nest(arena, INDENT))
                .append(arena, IF_SPACE_DOCUMENT)
                .append(
                    arena,
                    self.clause_guard(arena, guard)
                        .group(arena)
                        .nest(arena, INDENT),
                ),
        };

        // In case there's a guard or multiple subjects, if we decide to break
        // the patterns on multiple lines we also want the arrow to end up on
        // its own line to improve legibility.
        //
        // This looks like this:
        // ```gleam
        // case wibble, wobble {
        //   Wibble(_),  // pretend this goes over the line limit
        //     Wobble(_)
        //   -> todo
        //   // Notice how the arrow is broken on its own line, the same goes
        //   // for patterns with `if` guards.
        // }
        // ```

        let has_guard = clause.guard.is_some();
        let has_multiple_subjects = clause.pattern.len() > 1;
        let arrow_break = if has_guard || has_multiple_subjects {
            BREAKABLE_SPACE_DOCUMENT
        } else {
            SPACE_DOCUMENT
        };

        let clause_doc = docvec![arena, clause_doc, arrow_break, RIGHT_ARROW_DOCUMENT]
            .group(arena)
            .append(arena, self.case_clause_value(arena, &clause.then))
            .group(arena);

        let clause_doc = match printed_comments(arena, comments, false) {
            Some(comments) => comments
                .append(arena, LINE_DOCUMENT)
                .append(arena, clause_doc),
            None => clause_doc,
        };

        if index == 0 {
            clause_doc
        } else if space_before {
            TWO_LINES_DOCUMENT.append(arena, clause_doc)
        } else {
            LINE_DOCUMENT.append(arena, clause_doc)
        }
    }

    fn alternative_patterns(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        clause: &'a UntypedClause,
    ) -> Document<'a, 'doc> {
        let has_guard = clause.guard.is_some();
        let has_multiple_subjects = clause.pattern.len() > 1;

        // In case there's an `if` guard but no multiple subjects we want to add
        // additional indentation before the vartical bar separating alternative
        // patterns `|`.
        // We're not adding the indentation if there's multiple subjects as that
        // would make things harder to read, aligning the vertical bar with the
        // different subjects:
        // ```
        // case wibble, wobble {
        //   Wibble,
        //     Wobble
        //     | Wibble, // <- we don't want this indentation!
        //     Wobble -> todo
        // }
        // ```
        let alternatives_separator = if has_guard && !has_multiple_subjects {
            BREAKABLE_SPACE_DOCUMENT
                .nest(arena, INDENT)
                .append(arena, VERTICAL_BAR_SPACE_DOCUMENT)
        } else {
            BREAKABLE_SPACE_DOCUMENT.append(arena, VERTICAL_BAR_SPACE_DOCUMENT)
        };

        let alternative_patterns = std::iter::once(&clause.pattern)
            .chain(&clause.alternative_patterns)
            .enumerate()
            .map(|(alternative_index, patterns)| {
                // Here `p` is a single pattern that can be comprised of
                // multiple subjects.
                // ```gleam
                // case wibble, wobble {
                //   True, False
                // //^^^^^^^^^^^ This is a single pattern with multiple subjects
                //   | _, _ -> todo
                // }
                // ```
                let is_first_alternative = alternative_index == 0;
                let pattern_docs = patterns.iter().enumerate().map(|(pattern_index, pattern)| {
                    // There's a small catch in turning each subject into a document.
                    // Sadly we can't simply call `self.pattern` on each subject and
                    // then nest each one in case it gets broken.
                    // The first ever pattern that appears in a case clause (that is
                    // the first subject of the first alternative) must not be nested
                    // further; otherwise, when broken, it would have 2 extra spaces
                    // of indentation: https://github.com/gleam-lang/gleam/issues/2940.
                    let is_first_pattern = pattern_index == 0;
                    let is_first_pattern_of_clause = is_first_pattern && is_first_alternative;
                    let pattern_doc = self.pattern(arena, pattern);
                    if is_first_pattern_of_clause {
                        pattern_doc
                    } else {
                        pattern_doc.nest(arena, INDENT)
                    }
                });
                // We join all subjects with a breakable comma (that's also
                // going to be nested) and make the subjects into a group to
                // make sure the formatter tries to keep them on a single line.
                arena
                    .join(pattern_docs, COMMA_BREAK_DOCUMENT.nest(arena, INDENT))
                    .group(arena)
            });
        arena.join(alternative_patterns, alternatives_separator)
    }

    fn list(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        elements: &'a [UntypedExpr],
        tail: Option<&'a UntypedExpr>,
        location: &SrcSpan,
    ) -> Document<'a, 'doc> {
        if elements.is_empty() {
            return match tail {
                Some(tail) => self.expr(arena, tail),
                // We take all comments that come _before_ the end of the list,
                // that is all comments that are inside "[" and "]", if there's
                // any comment we want to put it inside the empty list!
                None => {
                    let comments = self.pop_comments(location.end);
                    match printed_comments(arena, comments, false) {
                        None => OPEN_CLOSE_SQUARE_DOCUMENT,
                        Some(comments) => OPEN_SQUARE_DOCUMENT
                            .append(arena, EMPTY_BREAK_DOCUMENT.nest(arena, INDENT))
                            .append(arena, comments)
                            .append(arena, EMPTY_BREAK_DOCUMENT)
                            .append(arena, CLOSE_SQUARE_DOCUMENT)
                            // vvv We want to make sure the comments are on a separate
                            //     line from the opening and closing brackets so we
                            //     force the breaks to be split on newlines.
                            .force_break(arena),
                    }
                }
            };
        }

        let list_packing = self.items_sequence_packing(
            elements,
            tail,
            UntypedExpr::can_have_multiple_per_line,
            *location,
        );

        let comma = match list_packing {
            ItemsPacking::FitMultiplePerLine => FLEX_COMMA_DOCUMENT,
            ItemsPacking::FitOnePerLine | ItemsPacking::BreakOnePerLine => COMMA_BREAK_DOCUMENT,
        };

        let list_size = elements.len()
            + match tail {
                Some(_) => 1,
                None => 0,
            };

        let mut is_empty = true;
        let mut elements_doc = EMPTY_DOCUMENT;
        for element in elements {
            let empty_lines = self.pop_empty_lines(element.location().start);
            let element_doc = self.comma_separated_item(arena, element, list_size);

            elements_doc = if is_empty {
                is_empty = false;
                element_doc
            } else if empty_lines {
                // If there's empty lines before the list item we want to add an
                // empty line here. Notice how we're making sure no nesting is
                // added after the comma, otherwise we would be adding needless
                // whitespace in the empty line!
                docvec![
                    arena,
                    elements_doc,
                    comma.set_nesting(arena, 0),
                    LINE_DOCUMENT,
                    element_doc
                ]
            } else {
                docvec![arena, elements_doc, comma, element_doc]
            };
        }
        elements_doc = elements_doc.next_break_fits(arena, NextBreakFitsMode::Disabled);

        let doc = OPEN_SQUARE_BREAK_DOCUMENT.append(arena, elements_doc);
        // We need to keep the last break aside and do not add it immediately
        // because in case there's a final comment before the closing square
        // bracket we want to add indentation (to just that break). Otherwise,
        // the final comment would be less indented than list's elements.
        let (doc, last_break) = match tail {
            None => (doc.nest(arena, INDENT), TRAILING_COMMA_BREAK_DOCUMENT),

            Some(tail) => {
                let comments = self.pop_comments(tail.location().start);
                let tail = commented(
                    arena,
                    docvec![arena, DOT_DOT_DOCUMENT, self.expr(arena, tail)],
                    comments,
                );
                (
                    doc.append(arena, COMMA_BREAK_DOCUMENT)
                        .append(arena, tail)
                        .nest(arena, INDENT),
                    EMPTY_BREAK_DOCUMENT,
                )
            }
        };

        // We get all remaining comments that come before the list's closing
        // square bracket.
        // If there's any we add those before the closing square bracket instead
        // of moving those out of the list.
        // Otherwise those would be moved out of the list.
        let comments = self.pop_comments(location.end);
        let doc = match printed_comments(arena, comments, false) {
            None => doc
                .append(arena, last_break)
                .append(arena, CLOSE_SQUARE_DOCUMENT),
            Some(comment) => doc
                .append(arena, last_break.nest(arena, INDENT))
                // ^ See how here we're adding the missing indentation to the
                //   final break so that the final comment is as indented as the
                //   list's items.
                .append(arena, comment)
                .append(arena, LINE_DOCUMENT)
                .append(arena, CLOSE_SQUARE_DOCUMENT)
                .force_break(arena),
        };

        match list_packing {
            ItemsPacking::FitOnePerLine | ItemsPacking::FitMultiplePerLine => doc.group(arena),
            ItemsPacking::BreakOnePerLine => doc.force_break(arena),
        }
    }

    fn items_sequence_packing<T: HasLocation>(
        &self,
        items: &'a [T],
        tail: Option<&'a T>,
        can_have_multiple_per_line: impl Fn(&'a T) -> bool,
        list_location: SrcSpan,
    ) -> ItemsPacking {
        let ends_with_trailing_comma = tail
            .map(|tail| tail.location().end)
            .or_else(|| items.last().map(|last| last.location().end))
            .is_some_and(|last_element_end| {
                self.has_trailing_comma(last_element_end, list_location.end)
            });

        let has_multiple_elements_per_line =
            self.has_items_on_the_same_line(items.iter().chain(tail));

        let has_empty_lines_between_elements = match (items.first(), items.last().or(tail)) {
            (Some(first), Some(last)) => self.empty_lines.first().is_some_and(|empty_line| {
                *empty_line >= first.location().end && *empty_line < last.location().start
            }),
            _ => false,
        };

        if has_empty_lines_between_elements {
            // If there's any empty line between elements we want to force each
            // item onto its own line to preserve the empty lines that were
            // intentionally added.
            ItemsPacking::BreakOnePerLine
        } else if !ends_with_trailing_comma {
            // If the list doesn't end with a trailing comma we try and pack it in
            // a single line; if we can't we'll put one item per line, no matter
            // the content of the list.
            ItemsPacking::FitOnePerLine
        } else if tail.is_none()
            && items.iter().all(can_have_multiple_per_line)
            && has_multiple_elements_per_line
            && self.spans_multiple_lines(list_location.start, list_location.end)
        {
            // If there's a trailing comma, we can have multiple items per line,
            // and there's already multiple items per line, we try and pack as
            // many items as possible on each line.
            //
            // Note how we only ever try and pack lists where all items are
            // unbreakable primitives. To pack a list we need to use put
            // `flex_break`s between each item.
            // If the items themselves had breaks we could end up in a situation
            // where an item gets broken making it span multiple lines and the
            // spaces are not, for example:
            //
            // ```gleam
            // [Constructor("wibble", "lorem ipsum dolor sit amet something something"), Other(1)]
            // ```
            //
            // If we used flex breaks here the list would be formatted as:
            //
            // ```gleam
            // [
            //   Constructor(
            //     "wibble",
            //     "lorem ipsum dolor sit amet something something",
            //   ), Other(1)
            // ]
            // ```
            //
            // The first item is broken, meaning that once we get to the flex
            // space separating it from the following one the formatter is not
            // going to break it since there's enough space in the current line!
            ItemsPacking::FitMultiplePerLine
        } else {
            // If it ends with a trailing comma we will force the list on
            // multiple lines, with one item per line.
            ItemsPacking::BreakOnePerLine
        }
    }

    fn has_items_on_the_same_line<L: HasLocation + 'a, T: Iterator<Item = &'a L>>(
        &self,
        items: T,
    ) -> bool {
        let mut previous: Option<SrcSpan> = None;
        for item in items {
            let item_location = item.location();
            // A list has multiple items on the same line if two consecutive
            // ones do not span multiple lines.
            if let Some(previous) = previous
                && !self.spans_multiple_lines(previous.end, item_location.start)
            {
                return true;
            }
            previous = Some(item_location);
        }
        false
    }

    /// Pretty prints an expression to be used in a comma separated list; for
    /// example as a list item, a tuple item or as an argument of a function call.
    fn comma_separated_item(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: &'a UntypedExpr,
        siblings: usize,
    ) -> Document<'a, 'doc> {
        // If there's more than one item in the comma separated list and there's a
        // pipeline or long binary chain, we want to indent those to make it
        // easier to tell where one item ends and the other starts.
        // Othewise we just print the expression as a normal expr.
        match expression {
            UntypedExpr::BinOp {
                operator,
                left,
                right,
                ..
            } if siblings > 1 => {
                let comments = self.pop_comments(expression.start_byte_index());
                let doc = self.bin_op(arena, operator, left, right, true).group(arena);
                commented(arena, doc, comments)
            }
            UntypedExpr::PipeLine { expressions } if siblings > 1 => {
                let comments = self.pop_comments(expression.start_byte_index());
                let doc = self.pipeline(arena, expressions, true).group(arena);
                commented(arena, doc, comments)
            }
            UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Block { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::BinOp { .. }
            | UntypedExpr::PipeLine { .. }
            | UntypedExpr::Case { .. }
            | UntypedExpr::FieldAccess { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::TupleIndex { .. }
            | UntypedExpr::Todo { .. }
            | UntypedExpr::Panic { .. }
            | UntypedExpr::Echo { .. }
            | UntypedExpr::BitArray { .. }
            | UntypedExpr::RecordUpdate { .. }
            | UntypedExpr::NegateBool { .. }
            | UntypedExpr::NegateInt { .. } => self.expr(arena, expression).group(arena),
        }
    }

    fn pattern(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        pattern: &'a UntypedPattern,
    ) -> Document<'a, 'doc> {
        let comments = self.pop_comments(pattern.location().start);
        let doc = match pattern {
            Pattern::Int { value, .. } => self.int(arena, value),

            Pattern::Float { value, .. } => self.float(arena, value),

            Pattern::String { value, .. } => self.string(arena, value),

            Pattern::Variable { name, .. } => name.to_doc(arena),

            Pattern::BitArraySize(size) => self.bit_array_size(arena, size),

            Pattern::Assign { name, pattern, .. } => {
                if pattern.is_discard() {
                    name.to_doc(arena)
                } else {
                    self.pattern(arena, pattern)
                        .append(arena, SPACE_AS_SPACE_DOCUMENT)
                        .append(arena, name.as_str())
                }
            }

            Pattern::Discard { name, .. } => name.to_doc(arena),

            Pattern::List { elements, tail, .. } => self.list_pattern(arena, elements, tail),

            Pattern::Constructor {
                name,
                arguments,
                module,
                spread,
                location,
                ..
            } => self.pattern_constructor(arena, name, arguments, module, *spread, location),

            Pattern::Tuple {
                elements, location, ..
            } => {
                let arguments = elements
                    .iter()
                    .map(|element| self.pattern(arena, element))
                    .collect_vec();
                "#".to_doc(arena)
                    .append(arena, self.wrap_arguments(arena, arguments, location.end))
                    .group(arena)
            }

            Pattern::BitArray {
                segments, location, ..
            } => {
                let segment_docs = segments
                    .iter()
                    .map(|segment| {
                        bit_array_segment(arena, segment, |pattern| self.pattern(arena, pattern))
                    })
                    .collect_vec();

                self.bit_array(arena, segment_docs, ItemsPacking::FitOnePerLine, location)
            }

            Pattern::StringPrefix {
                left_side_string: left,
                right_side_assignment: right,
                left_side_assignment: left_assign,
                ..
            } => {
                let left = self.string(arena, left);
                let right = match right {
                    AssignName::Variable(name) => name.to_doc(arena),
                    AssignName::Discard(name) => name.to_doc(arena),
                };
                match left_assign {
                    Some((name, _)) => {
                        docvec![
                            arena,
                            left,
                            SPACE_AS_SPACE_DOCUMENT,
                            name,
                            SPACE_CONCAT_SPACE_DOCUMENT,
                            right
                        ]
                    }
                    None => docvec![arena, left, SPACE_CONCAT_SPACE_DOCUMENT, right],
                }
            }

            Pattern::Invalid { .. } => panic!("invalid patterns can not be in an untyped ast"),
        };
        commented(arena, doc, comments)
    }

    fn bit_array_size(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        size: &'a BitArraySize<()>,
    ) -> Document<'a, 'doc> {
        match size {
            BitArraySize::Int { value, .. } => self.int(arena, value),
            BitArraySize::Variable { name, .. } => name.to_doc(arena),
            BitArraySize::BinaryOperator {
                left,
                right,
                operator,
                ..
            } => {
                let operator = match operator {
                    IntOperator::Add => SPACE_PLUS_SPACE_DOCUMENT,
                    IntOperator::Subtract => SPACE_MINUS_SPACE_DOCUMENT,
                    IntOperator::Multiply => SPACE_TIMES_SPACE_DOCUMENT,
                    IntOperator::Divide => SPACE_SLASH_SPACE_DOCUMENT,
                    IntOperator::Remainder => SPACE_MODULE_SPACE_DOCUMENT,
                };

                docvec![
                    arena,
                    self.bit_array_size(arena, left),
                    operator,
                    self.bit_array_size(arena, right)
                ]
            }
            BitArraySize::Block { inner, .. } => self.bit_array_size(arena, inner).surround(
                arena,
                OPEN_CURLY_SPACE_DOCUMENT,
                SPACE_CLOSE_CURLY_DOCUMENT,
            ),
        }
    }

    fn list_pattern(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        elements: &'a [UntypedPattern],
        tail: &'a Option<Box<UntypedTailPattern>>,
    ) -> Document<'a, 'doc> {
        if elements.is_empty() {
            return match tail {
                Some(tail) => self.pattern(arena, &tail.pattern),
                None => OPEN_CLOSE_SQUARE_DOCUMENT,
            };
        }
        let elements = arena.join(
            elements.iter().map(|element| self.pattern(arena, element)),
            COMMA_BREAK_DOCUMENT,
        );
        let doc = OPEN_SQUARE_BREAK_DOCUMENT.append(arena, elements);
        match tail {
            None => doc
                .nest(arena, INDENT)
                .append(arena, TRAILING_COMMA_BREAK_DOCUMENT),

            Some(tail) => {
                let tail = &tail.pattern;
                let comments = self.pop_comments(tail.location().start);
                let tail = if tail.is_discard() {
                    DOT_DOT_DOCUMENT
                } else {
                    docvec![arena, DOT_DOT_DOCUMENT, self.pattern(arena, tail)]
                };
                let tail = commented(arena, tail, comments);
                doc.append(arena, COMMA_BREAK_DOCUMENT)
                    .append(arena, tail)
                    .nest(arena, INDENT)
                    .append(arena, EMPTY_BREAK_DOCUMENT)
            }
        }
        .append(arena, "]")
        .group(arena)
    }

    fn pattern_call_arg(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        argument: &'a CallArg<UntypedPattern>,
    ) -> Document<'a, 'doc> {
        self.format_call_arg(
            arena,
            argument,
            pattern_call_arg_formatting,
            |this, value| this.pattern(arena, value),
        )
    }

    pub fn clause_guard_bin_op(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &'a BinOp,
        left: &'a UntypedClauseGuard,
        right: &'a UntypedClauseGuard,
    ) -> Document<'a, 'doc> {
        self.clause_guard_bin_op_side(arena, name, left, left.precedence())
            .append(arena, BREAKABLE_SPACE_DOCUMENT)
            .append(arena, binop(*name))
            .append(arena, SPACE_DOCUMENT)
            .append(
                arena,
                self.clause_guard_bin_op_side(arena, name, right, right.precedence() - 1),
            )
    }

    fn clause_guard_bin_op_side(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &BinOp,
        side: &'a UntypedClauseGuard,
        // As opposed to `bin_op_side`, here we take the side precedence as an
        // argument instead of computing it ourselves. That's because
        // `clause_guard_bin_op` will reduce the precedence of any right side to
        // make sure the formatter doesn't remove any needed curly bracket.
        side_precedence: u8,
    ) -> Document<'a, 'doc> {
        let side_doc = self.clause_guard(arena, side);
        match side.bin_op_name() {
            // In case the other side is a binary operation as well and it can
            // be grouped together with the current binary operation, the two
            // docs are simply concatenated, so that they will end up in the
            // same group and the formatter will try to keep those on a single
            // line.
            Some(side_name) if side_name.can_be_grouped_with(name) => {
                self.operator_side(arena, side_doc, name.precedence(), side_precedence)
            }
            // In case the binary operations cannot be grouped together the
            // other side is treated as a group on its own so that it can be
            // broken independently of other pieces of the binary operations
            // chain.
            _ => self.operator_side(
                arena,
                side_doc.group(arena),
                name.precedence(),
                side_precedence,
            ),
        }
    }

    fn clause_guard(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        clause_guard: &'a UntypedClauseGuard,
    ) -> Document<'a, 'doc> {
        match clause_guard {
            ClauseGuard::Invalid { .. } => unreachable!("invalid guard made it to formatting"),

            ClauseGuard::BinaryOperator {
                operator,
                left,
                right,
                ..
            } => self.clause_guard_bin_op(arena, operator, left, right),

            ClauseGuard::Var { name, .. } => name.to_doc(arena),

            ClauseGuard::TupleIndex { tuple, index, .. } => self
                .clause_guard(arena, tuple)
                .append(arena, DOT_DOCUMENT)
                .append(arena, *index)
                .to_doc(arena),

            ClauseGuard::FieldAccess {
                container, label, ..
            } => self
                .clause_guard(arena, container)
                .append(arena, DOT_DOCUMENT)
                .append(arena, label)
                .to_doc(arena),

            ClauseGuard::ModuleSelect {
                module_name, label, ..
            } => module_name
                .to_doc(arena)
                .append(arena, DOT_DOCUMENT)
                .append(arena, label)
                .to_doc(arena),

            ClauseGuard::Constant(constant) => self.const_expr(arena, constant),

            ClauseGuard::Not { expression, .. } => {
                docvec![
                    arena,
                    EXCLAMATION_MARK_DOCUMENT,
                    self.clause_guard(arena, expression)
                ]
            }

            ClauseGuard::Block { value, .. } => {
                wrap_block(arena, self.clause_guard(arena, value)).group(arena)
            }
        }
    }

    fn constant_call_arg<A>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        argument: &'a CallArg<Constant<A>>,
    ) -> Document<'a, 'doc> {
        self.format_call_arg(
            arena,
            argument,
            constant_call_arg_formatting,
            |this, value| this.const_expr(arena, value),
        )
    }

    fn negate_bool(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: &'a UntypedExpr,
    ) -> Document<'a, 'doc> {
        match expression {
            UntypedExpr::NegateBool { value, .. } => self.expr(arena, value),
            UntypedExpr::BinOp { .. } => {
                let doc = self.expr(arena, expression);

                EXCLAMATION_MARK_DOCUMENT.append(arena, wrap_block(arena, doc))
            }
            UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Block { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::PipeLine { .. }
            | UntypedExpr::Case { .. }
            | UntypedExpr::FieldAccess { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::TupleIndex { .. }
            | UntypedExpr::Todo { .. }
            | UntypedExpr::Panic { .. }
            | UntypedExpr::Echo { .. }
            | UntypedExpr::BitArray { .. }
            | UntypedExpr::RecordUpdate { .. }
            | UntypedExpr::NegateInt { .. } => {
                docvec![
                    arena,
                    EXCLAMATION_MARK_DOCUMENT,
                    self.expr(arena, expression)
                ]
            }
        }
    }

    fn negate_int(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: &'a UntypedExpr,
    ) -> Document<'a, 'doc> {
        match expression {
            UntypedExpr::NegateInt { value, .. } => self.expr(arena, value),
            UntypedExpr::Int { value, .. } if value.starts_with('-') => {
                self.int(arena, &value[1..])
            }
            UntypedExpr::BinOp { .. } => {
                MINUS_SPACE_DOCUMENT.append(arena, self.expr(arena, expression))
            }

            UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Block { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::PipeLine { .. }
            | UntypedExpr::Case { .. }
            | UntypedExpr::FieldAccess { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::TupleIndex { .. }
            | UntypedExpr::Todo { .. }
            | UntypedExpr::Panic { .. }
            | UntypedExpr::Echo { .. }
            | UntypedExpr::BitArray { .. }
            | UntypedExpr::RecordUpdate { .. }
            | UntypedExpr::NegateBool { .. } => {
                docvec![arena, SUB_INT_DOCUMENT, self.expr(arena, expression)]
            }
        }
    }

    fn use_(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        use_: &'a UntypedUse,
    ) -> Document<'a, 'doc> {
        let comments = self.pop_comments(use_.location.start);

        let call = if use_.call.is_call() {
            docvec![arena, SPACE_DOCUMENT, self.expr(arena, &use_.call)]
        } else {
            docvec![
                arena,
                BREAKABLE_SPACE_DOCUMENT,
                self.expr(arena, &use_.call)
            ]
            .nest(arena, INDENT)
        }
        .group(arena);

        let doc = if use_.assignments.is_empty() {
            docvec![arena, USE_AND_ARROW_DOCUMENT, call]
        } else {
            let assignments = use_.assignments.iter().map(|use_assignment| {
                let pattern = self.pattern(arena, &use_assignment.pattern);
                let annotation = use_assignment
                    .annotation
                    .as_ref()
                    .map(|annotation| {
                        COLON_SPACE_DOCUMENT.append(arena, self.type_ast(arena, annotation))
                    })
                    .unwrap_or(EMPTY_DOCUMENT);

                pattern.append(arena, annotation).group(arena)
            });
            let assignments = Itertools::intersperse(assignments, COMMA_BREAK_DOCUMENT);
            let left = [USE_DOCUMENT, BREAKABLE_SPACE_DOCUMENT]
                .into_iter()
                .chain(assignments);
            let left = arena
                .concat(left)
                .nest(arena, INDENT)
                .append(arena, BREAKABLE_SPACE_DOCUMENT)
                .group(arena);
            docvec![arena, left, LEFT_ARROW_DOCUMENT, call].group(arena)
        };

        commented(arena, doc, comments)
    }

    fn assert(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        assert: &'a UntypedAssert,
    ) -> Document<'a, 'doc> {
        let comments = self.pop_comments(assert.location.start);

        let expression = if assert.value.is_binop() || assert.value.is_pipeline() {
            self.expr(arena, &assert.value).nest(arena, INDENT)
        } else {
            self.expr(arena, &assert.value)
        };

        let doc = self.append_as_message_expression(
            arena,
            expression,
            PrecedingAs::Expression,
            assert.message.as_ref(),
        );
        commented(arena, docvec![arena, ASSERT_SPACE_DOCUMENT, doc], comments)
    }

    fn bit_array(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        segments: Vec<Document<'a, 'doc>>,
        packing: ItemsPacking,
        location: &SrcSpan,
    ) -> Document<'a, 'doc> {
        let comments = self.pop_comments(location.end);
        let comments_doc = printed_comments(arena, comments, false);

        // Avoid adding illegal comma in empty bit array by explicitly handling it
        if segments.is_empty() {
            // We take all comments that come _before_ the end of the bit array,
            // that is all comments that are inside "<<" and ">>", if there's
            // any comment we want to put it inside the empty bit array!
            // Refer to the `list` function for a similar procedure.
            return match comments_doc {
                None => EMPTY_BIT_ARRAY_DOCUMENT,
                Some(comments) => OPEN_BIT_ARRAY_DOCUMENT
                    .append(arena, EMPTY_BREAK_DOCUMENT.nest(arena, INDENT))
                    .append(arena, comments)
                    .append(arena, EMPTY_BREAK_DOCUMENT)
                    .append(arena, CLOSE_BIT_ARRAY_DOCUMENT)
                    // vvv We want to make sure the comments are on a separate
                    //     line from the opening and closing angle brackets so
                    //     we force the breaks to be split on newlines.
                    .force_break(arena),
            };
        }

        let comma = match packing {
            ItemsPacking::FitMultiplePerLine => FLEX_COMMA_DOCUMENT,
            ItemsPacking::FitOnePerLine | ItemsPacking::BreakOnePerLine => COMMA_BREAK_DOCUMENT,
        };

        let last_break = TRAILING_COMMA_BREAK_DOCUMENT;
        let doc = OPEN_BIT_ARRAY_BREAK_DOCUMENT
            .append(arena, arena.join(segments, comma))
            .nest(arena, INDENT);

        let doc = match comments_doc {
            None => doc
                .append(arena, last_break)
                .append(arena, CLOSE_BIT_ARRAY_DOCUMENT),
            Some(comments) => doc
                .append(arena, last_break.nest(arena, INDENT))
                // ^ Notice how in this case we nest the final break before
                //   adding it: this way the comments are going to be as
                //   indented as the bit array items.
                .append(arena, comments.nest(arena, INDENT))
                .append(arena, LINE_DOCUMENT)
                .append(arena, CLOSE_BIT_ARRAY_DOCUMENT)
                .force_break(arena),
        };

        match packing {
            ItemsPacking::FitOnePerLine | ItemsPacking::FitMultiplePerLine => doc.group(arena),
            ItemsPacking::BreakOnePerLine => doc.force_break(arena),
        }
    }

    fn bit_array_segment_expr(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: &'a UntypedExpr,
    ) -> Document<'a, 'doc> {
        match expression {
            UntypedExpr::BinOp { .. } => EMPTY_BREAK_DOCUMENT
                .append(arena, self.expr(arena, expression))
                .nest_if_broken(arena, INDENT)
                .append(arena, EMPTY_BREAK_DOCUMENT),

            UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::PipeLine { .. }
            | UntypedExpr::Case { .. }
            | UntypedExpr::FieldAccess { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::TupleIndex { .. }
            | UntypedExpr::Todo { .. }
            | UntypedExpr::Panic { .. }
            | UntypedExpr::Echo { .. }
            | UntypedExpr::BitArray { .. }
            | UntypedExpr::RecordUpdate { .. }
            | UntypedExpr::NegateBool { .. }
            | UntypedExpr::NegateInt { .. }
            | UntypedExpr::Block { .. } => self.expr(arena, expression),
        }
    }

    fn statement(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        statement: &'a UntypedStatement,
    ) -> Document<'a, 'doc> {
        match statement {
            Statement::Expression(expression) => self.expr(arena, expression),
            Statement::Assignment(assignment) => self.assignment(arena, assignment),
            Statement::Use(use_) => self.use_(arena, use_),
            Statement::Assert(assert) => self.assert(arena, assert),
        }
    }

    fn block(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        location: &SrcSpan,
        statements: &'a Vec1<UntypedStatement>,
        force_breaks: bool,
    ) -> Document<'a, 'doc> {
        let statements_doc = docvec![
            arena,
            BREAKABLE_SPACE_DOCUMENT,
            self.statements(arena, statements.as_vec())
        ]
        .nest(arena, INDENT);
        let trailing_comments = self.pop_comments(location.end);
        let trailing_comments = printed_comments(arena, trailing_comments, false);
        let block_doc = match trailing_comments {
            Some(trailing_comments_doc) => docvec![
                arena,
                OPEN_CURLY_DOCUMENT,
                statements_doc,
                LINE_DOCUMENT.nest(arena, INDENT),
                trailing_comments_doc.nest(arena, INDENT),
                LINE_DOCUMENT,
                CLOSE_CURLY_DOCUMENT
            ]
            .force_break(arena),
            None => docvec![
                arena,
                OPEN_CURLY_DOCUMENT,
                statements_doc,
                BREAKABLE_SPACE_DOCUMENT,
                CLOSE_CURLY_DOCUMENT
            ],
        };

        if force_breaks {
            block_doc.force_break(arena).group(arena)
        } else {
            block_doc.group(arena)
        }
    }

    pub fn wrap_function_call_arguments<I>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        arguments: I,
        location: &SrcSpan,
    ) -> Document<'a, 'doc>
    where
        I: IntoIterator<Item = Document<'a, 'doc>>,
    {
        let mut arguments = arguments.into_iter().peekable();
        if arguments.peek().is_none() {
            return OPEN_CLOSE_PAREN_DOCUMENT;
        }

        let arguments_doc = EMPTY_BREAK_DOCUMENT
            .append(arena, arena.join(arguments, COMMA_BREAK_DOCUMENT))
            .nest_if_broken(arena, INDENT);

        // We get all remaining comments that come before the call's closing
        // parenthesis.
        // If there's any we add those before the closing parenthesis instead
        // of moving those out of the call.
        // Otherwise those would be moved out of the call.
        let comments = self.pop_comments(location.end);
        let closing_parens = match printed_comments(arena, comments, false) {
            None => docvec![arena, TRAILING_COMMA_BREAK_DOCUMENT, CLOSE_PAREN_DOCUMENT],
            Some(comment) => docvec![
                arena,
                TRAILING_COMMA_BREAK_DOCUMENT.nest(arena, INDENT),
                comment,
                LINE_DOCUMENT,
                CLOSE_PAREN_DOCUMENT
            ]
            .force_break(arena),
        };

        OPEN_PAREN_DOCUMENT
            .append(arena, arguments_doc)
            .append(arena, closing_parens)
            .group(arena)
    }

    pub fn wrap_arguments<I>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        arguments: I,
        comments_limit: u32,
    ) -> Document<'a, 'doc>
    where
        I: IntoIterator<Item = Document<'a, 'doc>>,
    {
        let mut arguments = arguments.into_iter().peekable();
        if arguments.peek().is_none() {
            let comments = self.pop_comments(comments_limit);
            return match printed_comments(arena, comments, false) {
                Some(comments) => OPEN_PAREN_DOCUMENT
                    .to_doc(arena)
                    .append(arena, EMPTY_BREAK_DOCUMENT)
                    .append(arena, comments)
                    .nest_if_broken(arena, INDENT)
                    .force_break(arena)
                    .append(arena, EMPTY_BREAK_DOCUMENT)
                    .append(arena, CLOSE_PAREN_DOCUMENT),
                None => OPEN_CLOSE_PAREN_DOCUMENT,
            };
        }
        let doc =
            OPEN_PAREN_BREAK_DOCUMENT.append(arena, arena.join(arguments, COMMA_BREAK_DOCUMENT));

        // Include trailing comments if there are any
        let comments = self.pop_comments(comments_limit);
        match printed_comments(arena, comments, false) {
            Some(comments) => doc
                .append(arena, TRAILING_COMMA_BREAK_DOCUMENT)
                .append(arena, comments)
                .nest_if_broken(arena, INDENT)
                .force_break(arena)
                .append(arena, EMPTY_BREAK_DOCUMENT)
                .append(arena, CLOSE_PAREN_DOCUMENT),
            None => doc
                .nest_if_broken(arena, INDENT)
                .append(arena, TRAILING_COMMA_BREAK_DOCUMENT)
                .append(arena, CLOSE_PAREN_DOCUMENT),
        }
    }

    pub fn wrap_arguments_with_spread<I>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        arguments: I,
        comments_limit: u32,
    ) -> Document<'a, 'doc>
    where
        I: IntoIterator<Item = Document<'a, 'doc>>,
    {
        let mut arguments = arguments.into_iter().peekable();
        if arguments.peek().is_none() {
            return self.wrap_arguments(arena, arguments, comments_limit);
        }
        let doc = OPEN_PAREN_BREAK_DOCUMENT
            .append(arena, arena.join(arguments, COMMA_BREAK_DOCUMENT))
            .append(arena, COMMA_BREAK_DOCUMENT)
            .append(arena, DOT_DOT_DOCUMENT);

        // Include trailing comments if there are any
        let comments = self.pop_comments(comments_limit);
        match printed_comments(arena, comments, false) {
            Some(comments) => doc
                .append(arena, TRAILING_COMMA_BREAK_DOCUMENT)
                .append(arena, comments)
                .nest_if_broken(arena, INDENT)
                .force_break(arena)
                .append(arena, EMPTY_BREAK_DOCUMENT)
                .append(arena, CLOSE_PAREN_DOCUMENT),
            None => doc
                .nest_if_broken(arena, INDENT)
                .append(arena, TRAILING_COMMA_BREAK_DOCUMENT)
                .append(arena, CLOSE_PAREN_DOCUMENT),
        }
    }

    /// Given some regular comments it pretty prints those with any respective
    /// doc comment that might be preceding those.
    /// For example:
    ///
    /// ```gleam
    /// /// Doc
    /// // comment
    ///
    /// /// Doc
    /// pub fn wibble() {}
    /// ```
    ///
    /// We don't want the first doc comment to be merged together with
    /// `wibble`'s doc comment, so when we run into comments like `// comment`
    /// we need to first print all documentation comments that come before it.
    ///
    fn printed_documented_comments<'b>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        comments: impl IntoIterator<Item = (u32, Option<&'b str>)>,
    ) -> Option<Document<'a, 'doc>> {
        let mut comments = comments.into_iter().peekable();
        let _ = comments.peek()?;

        let mut doc = Vec::new();
        while let Some(comment) = comments.next() {
            let (is_doc_commented, comment) = match comment {
                (comment_start, Some(c)) => {
                    let doc_comment = self.doc_comments(arena, comment_start);
                    let is_doc_commented = !doc_comment.is_empty();
                    doc.push(doc_comment);
                    (is_doc_commented, c)
                }
                (_, None) => continue,
            };
            doc.push(
                COMMENT_DOCUMENT.append(arena, arena.zero_width_string(EcoString::from(comment))),
            );
            match comments.peek() {
                // Next line is a comment
                Some((_, Some(_))) => doc.push(LINE_DOCUMENT),
                // Next line is empty
                Some((_, None)) => {
                    let _ = comments.next();
                    doc.push(TWO_LINES_DOCUMENT);
                }
                // We've reached the end, there are no more lines
                None => {
                    if is_doc_commented {
                        doc.push(TWO_LINES_DOCUMENT);
                    } else {
                        doc.push(LINE_DOCUMENT);
                    }
                }
            }
        }
        let doc = arena.concat(doc);
        Some(doc.force_break(arena))
    }

    fn append_as_message_expression(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        doc: Document<'a, 'doc>,
        preceding_as: PrecedingAs,
        message: Option<&'a UntypedExpr>,
    ) -> Document<'a, 'doc> {
        let Some(message) = message else { return doc };

        let comments = self.pop_comments(message.location().start);
        let comments = printed_comments(arena, comments, false);

        let as_ = match preceding_as {
            PrecedingAs::Keyword => SPACE_AS_DOCUMENT,
            PrecedingAs::Expression => {
                docvec![arena, BREAKABLE_SPACE_DOCUMENT, AS_DOCUMENT].nest(arena, INDENT)
            }
        };

        let doc = match comments {
            // If there's comments between the document and the message we want
            // the `as` bit to be on the same line as the original document and
            // go on a new indented line with the message and comments:
            // ```gleam
            // todo as
            //   // comment!
            //   "wibble"
            // ```
            Some(comments) => docvec![
                arena,
                doc.group(arena),
                as_,
                docvec![
                    arena,
                    LINE_DOCUMENT,
                    comments,
                    LINE_DOCUMENT,
                    self.expr(arena, message).group(arena)
                ]
                .nest(arena, INDENT)
            ],

            None => {
                let message = match (preceding_as, message) {
                    // If we have `as` preceded by a keyword (like with `panic` and `todo`)
                    // and the message is a block, we don't want to nest it any further. That is,
                    // we want it to look like this:
                    // ```gleam
                    // panic as {
                    //   wibble wobble
                    // }
                    // ```
                    // instead of this:
                    // ```gleam
                    // panic as {
                    //     wibble wobble
                    //   }
                    // ```
                    (PrecedingAs::Keyword, UntypedExpr::Block { .. }) => {
                        self.expr(arena, message).group(arena)
                    }
                    _ => self.expr(arena, message).group(arena).nest(arena, INDENT),
                };
                docvec![arena, doc.group(arena), as_, SPACE_DOCUMENT, message]
            }
        };

        doc.group(arena)
    }

    fn append_as_message_constant<A>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        doc: Document<'a, 'doc>,
        message: Option<&'a Constant<A>>,
    ) -> Document<'a, 'doc> {
        let Some(message) = message else { return doc };

        let comments = self.pop_comments(message.location().start);
        let comments = printed_comments(arena, comments, false);

        let doc = match comments {
            // If there's comments between the document and the message we want
            // the `as` bit to be on the same line as the original document and
            // go on a new indented line with the message and comments:
            // ```gleam
            // todo as
            //   // comment!
            //   "wibble"
            // ```
            Some(comments) => docvec![
                arena,
                doc.group(arena),
                SPACE_AS_DOCUMENT,
                docvec![
                    arena,
                    LINE_DOCUMENT,
                    comments,
                    LINE_DOCUMENT,
                    self.const_expr(arena, message).group(arena)
                ]
                .nest(arena, INDENT)
            ],

            None => {
                let message = self
                    .const_expr(arena, message)
                    .group(arena)
                    .nest(arena, INDENT);
                docvec![arena, doc.group(arena), SPACE_AS_SPACE_DOCUMENT, message]
            }
        };

        doc.group(arena)
    }

    fn echo(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: &'a Option<Box<UntypedExpr>>,
        message: &'a Option<Box<UntypedExpr>>,
    ) -> Document<'a, 'doc> {
        let Some(expression) = expression else {
            return self.append_as_message_expression(
                arena,
                ECHO_DOCUMENT,
                PrecedingAs::Keyword,
                message.as_deref(),
            );
        };

        // When a binary expression gets broken on multiple lines we don't want
        // it to be on the same line as echo, or it would look confusing;
        // instead it's nested onto a new line:
        //
        // ```gleam
        // echo first
        //   |> wobble
        //   |> wibble
        // ```
        //
        // So it's easier to see echo is printing the whole thing. Otherwise,
        // it would look like echo is printing just the first item:
        //
        // ```gleam
        // echo first
        // |> wobble
        // |> wibble
        // ```
        //
        let doc = self.expr(arena, expression);
        if expression.is_binop() || expression.is_pipeline() {
            let doc = self.append_as_message_expression(
                arena,
                doc.nest(arena, INDENT),
                PrecedingAs::Expression,
                message.as_deref(),
            );
            docvec![arena, ECHO_SPACE_DOCUMENT, doc]
        } else {
            docvec![
                arena,
                ECHO_SPACE_DOCUMENT,
                self.append_as_message_expression(
                    arena,
                    doc,
                    PrecedingAs::Expression,
                    message.as_deref()
                )
            ]
        }
    }
}

/// This is used to describe the kind of things that might preceding an `as`
/// message that can be added to various places: `panic`, `echo`, `let assert`,
/// `assert`, `todo`.
///
/// It might be preceded by a keyword, like with `echo` and `panic`, or by
/// an expression, like in `assert` or `let assert`.
///
enum PrecedingAs {
    /// An expression is preceding the `as` message:
    /// ```gleam
    /// echo 1 as "message"
    /// assert 1 == 2 as "message"
    /// let assert Ok(_) = result as "message"
    /// ```
    ///
    Expression,

    /// A keyword is preceding the `as` message:
    /// ```gleam
    /// 1 |> echo as "message"
    /// panic as "message"
    /// todo as "message"
    /// ```
    ///
    Keyword,
}

fn init_and_last<T>(vec: &[T]) -> Option<(&[T], &T)> {
    match vec {
        [] => None,
        _ => match vec.split_at(vec.len() - 1) {
            (init, [last]) => Some((init, last)),
            _ => panic!("unreachable"),
        },
    }
}

fn binop<'a, 'doc>(binop: BinOp) -> Document<'a, 'doc> {
    match binop {
        BinOp::And => AND_DOCUMENT,
        BinOp::Or => OR_DOCUMENT,
        BinOp::LtInt => LT_INT_DOCUMENT,
        BinOp::LtEqInt => LT_EQ_INT_DOCUMENT,
        BinOp::LtFloat => LT_FLOAT_DOCUMENT,
        BinOp::LtEqFloat => LT_EQ_FLOAT_DOCUMENT,
        BinOp::Eq => EQ_DOCUMENT,
        BinOp::NotEq => NOT_EQ_DOCUMENT,
        BinOp::GtEqInt => GT_EQ_INT_DOCUMENT,
        BinOp::GtInt => GT_INT_DOCUMENT,
        BinOp::GtEqFloat => GT_EQ_FLOAT_DOCUMENT,
        BinOp::GtFloat => GT_FLOAT_DOCUMENT,
        BinOp::AddInt => ADD_INT_DOCUMENT,
        BinOp::AddFloat => ADD_FLOAT_DOCUMENT,
        BinOp::SubInt => SUB_INT_DOCUMENT,
        BinOp::SubFloat => SUB_FLOAT_DOCUMENT,
        BinOp::MultInt => MULT_INT_DOCUMENT,
        BinOp::MultFloat => MULT_FLOAT_DOCUMENT,
        BinOp::DivInt => DIV_INT_DOCUMENT,
        BinOp::DivFloat => DIV_FLOAT_DOCUMENT,
        BinOp::RemainderInt => REMAINDER_INT_DOCUMENT,
        BinOp::Concatenate => CONCAT_DOCUMENT,
    }
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug)]
/// This is used to determine how to fit the items of a list, or the segments of
/// a bit array in a line.
///
enum ItemsPacking {
    /// Try and fit everything on a single line; if the items don't fit, break
    /// the list putting each item into its own line.
    ///
    /// ```gleam
    /// // unbroken
    /// [1, 2, 3]
    ///
    /// // broken
    /// [
    ///   1,
    ///   2,
    ///   3,
    /// ]
    /// ```
    ///
    FitOnePerLine,

    /// Try and fit everything on a single line; if the items don't fit, break
    /// the list putting as many items as possible in a single line.
    ///
    /// ```gleam
    /// // unbroken
    /// [1, 2, 3]
    ///
    /// // broken
    /// [
    ///   1, 2, 3, ...
    ///   4, 100,
    /// ]
    /// ```
    ///
    FitMultiplePerLine,

    /// Always break the list, putting each item into its own line:
    ///
    /// ```gleam
    /// [
    ///   1,
    ///   2,
    ///   3,
    /// ]
    /// ```
    ///
    BreakOnePerLine,
}

pub fn comments_before<'a>(
    comments: &'a [Comment<'a>],
    empty_lines: &'a [u32],
    limit: u32,
    retain_empty_lines: bool,
) -> (
    impl Iterator<Item = (u32, Option<&'a str>)>,
    &'a [Comment<'a>],
    &'a [u32],
) {
    let end_comments = comments
        .iter()
        .position(|comment| comment.start > limit)
        .unwrap_or(comments.len());
    let end_empty_lines = empty_lines
        .iter()
        .position(|empty_line| *empty_line > limit)
        .unwrap_or(empty_lines.len());
    let popped_comments = comments
        .get(0..end_comments)
        .expect("0..end_comments is guaranteed to be in bounds")
        .iter()
        .map(|comment| (comment.start, Some(comment.content)));
    let popped_empty_lines = if retain_empty_lines { empty_lines } else { &[] }
        .get(0..end_empty_lines)
        .unwrap_or(&[])
        .iter()
        .map(|i| (i, i))
        // compact consecutive empty lines into a single line
        .coalesce(|(a_start, a_end), (b_start, b_end)| {
            if *a_end + 1 == *b_start {
                Ok((a_start, b_end))
            } else {
                Err(((a_start, a_end), (b_start, b_end)))
            }
        })
        .map(|l| (*l.0, None));
    let popped = popped_comments
        .merge_by(popped_empty_lines, |(a, _), (b, _)| a < b)
        .skip_while(|(_, comment_or_line)| comment_or_line.is_none());
    (
        popped,
        comments.get(end_comments..).expect("in bounds"),
        empty_lines.get(end_empty_lines..).expect("in bounds"),
    )
}

fn is_breakable_argument(expression: &UntypedExpr, arity: usize) -> bool {
    match expression {
        // A call is only breakable if it is the only argument
        UntypedExpr::Call { .. } => arity == 1,

        UntypedExpr::Fn { .. }
        | UntypedExpr::Block { .. }
        | UntypedExpr::Case { .. }
        | UntypedExpr::List { .. }
        | UntypedExpr::Tuple { .. }
        | UntypedExpr::BitArray { .. } => true,

        UntypedExpr::Int { .. }
        | UntypedExpr::Float { .. }
        | UntypedExpr::String { .. }
        | UntypedExpr::Var { .. }
        | UntypedExpr::BinOp { .. }
        | UntypedExpr::PipeLine { .. }
        | UntypedExpr::FieldAccess { .. }
        | UntypedExpr::TupleIndex { .. }
        | UntypedExpr::Todo { .. }
        | UntypedExpr::Panic { .. }
        | UntypedExpr::Echo { .. }
        | UntypedExpr::RecordUpdate { .. }
        | UntypedExpr::NegateBool { .. }
        | UntypedExpr::NegateInt { .. } => false,
    }
}

enum CallArgFormatting<'a, A> {
    ShorthandLabelled(&'a EcoString),
    Unlabelled(&'a A),
    Labelled(&'a EcoString, &'a A),
}

fn expr_call_arg_formatting(argument: &CallArg<UntypedExpr>) -> CallArgFormatting<'_, UntypedExpr> {
    match argument {
        // An argument supplied using label shorthand syntax.
        _ if argument.uses_label_shorthand() => CallArgFormatting::ShorthandLabelled(
            argument
                .label
                .as_ref()
                .expect("label shorthand with no label"),
        ),
        // A labelled argument.
        CallArg {
            label: Some(label),
            value,
            ..
        } => CallArgFormatting::Labelled(label, value),
        // An unlabelled argument.
        CallArg { value, .. } => CallArgFormatting::Unlabelled(value),
    }
}

fn pattern_call_arg_formatting(
    argument: &CallArg<UntypedPattern>,
) -> CallArgFormatting<'_, UntypedPattern> {
    match argument {
        // An argument supplied using label shorthand syntax.
        _ if argument.uses_label_shorthand() => CallArgFormatting::ShorthandLabelled(
            argument
                .label
                .as_ref()
                .expect("label shorthand with no label"),
        ),
        // A labelled argument.
        CallArg {
            label: Some(label),
            value,
            ..
        } => CallArgFormatting::Labelled(label, value),
        // An unlabelled argument.
        CallArg { value, .. } => CallArgFormatting::Unlabelled(value),
    }
}

fn constant_call_arg_formatting<A>(
    argument: &CallArg<Constant<A>>,
) -> CallArgFormatting<'_, Constant<A>> {
    match argument {
        // An argument supplied using label shorthand syntax.
        _ if argument.uses_label_shorthand() => CallArgFormatting::ShorthandLabelled(
            argument
                .label
                .as_ref()
                .expect("label shorthand with no label"),
        ),
        // A labelled argument.
        CallArg {
            label: Some(label),
            value,
            ..
        } => CallArgFormatting::Labelled(label, value),
        // An unlabelled argument.
        CallArg { value, .. } => CallArgFormatting::Unlabelled(value),
    }
}

struct AttributesPrinter<'a> {
    external_erlang: &'a Option<(EcoString, EcoString, SrcSpan)>,
    external_javascript: &'a Option<(EcoString, EcoString, SrcSpan)>,
    deprecation: &'a Deprecation,
    internal: bool,
}

impl<'a> AttributesPrinter<'a> {
    pub fn new() -> Self {
        Self {
            external_erlang: &None,
            external_javascript: &None,
            deprecation: &Deprecation::NotDeprecated,
            internal: false,
        }
    }

    pub fn set_external_erlang(
        mut self,
        external: &'a Option<(EcoString, EcoString, SrcSpan)>,
    ) -> Self {
        self.external_erlang = external;
        self
    }

    pub fn set_external_javascript(
        mut self,
        external: &'a Option<(EcoString, EcoString, SrcSpan)>,
    ) -> Self {
        self.external_javascript = external;
        self
    }

    pub fn set_internal(mut self, publicity: Publicity) -> Self {
        self.internal = publicity.is_internal();
        self
    }

    pub fn set_deprecation(mut self, deprecation: &'a Deprecation) -> Self {
        self.deprecation = deprecation;
        self
    }
}

impl<'a, 'doc> AttributesPrinter<'a> {
    fn to_doc(&self, arena: &'doc DocumentArena<'a, 'doc>) -> Document<'a, 'doc> {
        let mut attributes = vec![];

        // @deprecated attribute
        if let Deprecation::Deprecated { message } = self.deprecation {
            attributes.push(docvec![
                arena,
                DEPRECATED_ATTRIBUTE_QUOTE_DOCUMENT,
                message,
                QUOTE_CLOSE_PAREN_DOCUMENT
            ])
        };

        // @external attributes
        if let Some((module, function, _)) = self.external_erlang {
            attributes.push(docvec![
                arena,
                EXTERNAL_ERLANG_QUOTE_DOCUMENT,
                module,
                QUOTE_COMMA_SPACE_QUOTE_DOCUMENT,
                function,
                QUOTE_CLOSE_PAREN_DOCUMENT,
            ])
        };

        if let Some((module, function, _)) = self.external_javascript {
            attributes.push(docvec![
                arena,
                EXTERNAL_JAVASCRIPT_QUOTE_DOCUMENT,
                module,
                QUOTE_COMMA_SPACE_QUOTE_DOCUMENT,
                function,
                QUOTE_CLOSE_PAREN_DOCUMENT
            ])
        };

        // @internal attribute
        if self.internal {
            attributes.push(INTERNAL_ATTRIBUTE_DOCUMENT);
        };

        if attributes.is_empty() {
            EMPTY_DOCUMENT
        } else {
            arena
                .join(attributes, arena.line())
                .append(arena, arena.line())
        }
    }
}

pub fn break_block<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    doc: Document<'a, 'doc>,
) -> Document<'a, 'doc> {
    OPEN_CURLY_DOCUMENT
        .append(arena, LINE_DOCUMENT.append(arena, doc).nest(arena, INDENT))
        .append(arena, LINE_DOCUMENT)
        .append(arena, CLOSE_CURLY_DOCUMENT)
        .force_break(arena)
}

pub fn wrap_block<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    doc: Document<'a, 'doc>,
) -> Document<'a, 'doc> {
    OPEN_CURLY_BREAK_DOCUMENT
        .append(arena, doc)
        .nest(arena, INDENT)
        .append(arena, BREAKABLE_SPACE_DOCUMENT)
        .append(arena, CLOSE_CURLY_DOCUMENT)
}

fn printed_comments<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    comments: impl IntoIterator<Item = Option<&'a str>>,
    trailing_newline: bool,
) -> Option<Document<'a, 'doc>> {
    let mut comments = comments.into_iter().peekable();
    let _ = comments.peek()?;

    let mut doc = Vec::new();
    while let Some(comment) = comments.next() {
        let Some(comment) = comment else { continue };

        // The comment is turned into a zero width string rather than a regular
        // string document: comment lines are never touched by the formatter and
        // we don't need to know how long each one is.
        // So we can do this to avoid counting the graphemes of each one, which
        // is a lot of wasted work.
        let comment = arena.zero_width_str(comment);

        doc.push(COMMENT_DOCUMENT.append(arena, comment));
        match comments.peek() {
            // Next line is a comment
            Some(Some(_)) => doc.push(LINE_DOCUMENT),
            // Next line is empty
            Some(None) => {
                let _ = comments.next();
                match comments.peek() {
                    Some(_) => doc.push(TWO_LINES_DOCUMENT),
                    None => {
                        if trailing_newline {
                            doc.push(TWO_LINES_DOCUMENT);
                        }
                    }
                }
            }
            // We've reached the end, there are no more lines
            None => {
                if trailing_newline {
                    doc.push(LINE_DOCUMENT);
                }
            }
        }
    }
    let doc = arena.concat(doc);
    if trailing_newline {
        Some(doc.force_break(arena))
    } else {
        Some(doc)
    }
}

fn commented<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    doc: Document<'a, 'doc>,
    comments: impl IntoIterator<Item = Option<&'a str>>,
) -> Document<'a, 'doc> {
    match printed_comments(arena, comments, true) {
        Some(comments) => comments.append(arena, doc.group(arena)),
        None => doc,
    }
}

fn bit_array_segment<'a, 'doc, Value, Type, ToDoc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    segment: &'a BitArraySegment<Value, Type>,
    mut to_doc: ToDoc,
) -> Document<'a, 'doc>
where
    ToDoc: FnMut(&'a Value) -> Document<'a, 'doc>,
{
    match segment {
        BitArraySegment { value, options, .. } if options.is_empty() => to_doc(value),

        BitArraySegment { value, options, .. } => {
            to_doc(value).append(arena, COLON_DOCUMENT).append(
                arena,
                arena.join(
                    options
                        .iter()
                        .map(|option| segment_option(arena, option, &mut to_doc)),
                    SUB_INT_DOCUMENT,
                ),
            )
        }
    }
}

fn segment_option<'a, 'doc, ToDoc, Value>(
    arena: &'doc DocumentArena<'a, 'doc>,
    option: &'a BitArrayOption<Value>,
    mut to_doc: ToDoc,
) -> Document<'a, 'doc>
where
    ToDoc: FnMut(&'a Value) -> Document<'a, 'doc>,
{
    match option {
        BitArrayOption::Bytes { .. } => BYTES_DOCUMENT,
        BitArrayOption::Bits { .. } => BITS_DOCUMENT,
        BitArrayOption::Int { .. } => INT_DOCUMENT,
        BitArrayOption::Float { .. } => FLOAT_DOCUMENT,
        BitArrayOption::Utf8 { .. } => UTF8_DOCUMENT,
        BitArrayOption::Utf16 { .. } => UTF16_DOCUMENT,
        BitArrayOption::Utf32 { .. } => UTF32_DOCUMENT,
        BitArrayOption::Utf8Codepoint { .. } => UTF8_CODEPOINT_DOCUMENT,
        BitArrayOption::Utf16Codepoint { .. } => UTF16_CODEPOINT_DOCUMENT,
        BitArrayOption::Utf32Codepoint { .. } => UTF32_CODEPOINT_DOCUMENT,
        BitArrayOption::Signed { .. } => SIGNED_DOCUMENT,
        BitArrayOption::Unsigned { .. } => UNSIGNED_DOCUMENT,
        BitArrayOption::Big { .. } => BIG_DOCUMENT,
        BitArrayOption::Little { .. } => LITTLE_DOCUMENT,
        BitArrayOption::Native { .. } => NATIVE_DOCUMENT,

        BitArrayOption::Size {
            value,
            short_form: false,
            ..
        } => SIZE_DOCUMENT
            .append(arena, OPEN_PAREN_DOCUMENT)
            .append(arena, to_doc(value))
            .append(arena, CLOSE_PAREN_DOCUMENT),

        BitArrayOption::Size {
            value,
            short_form: true,
            ..
        } => to_doc(value),

        BitArrayOption::Unit { value, .. } => UNIT_DOCUMENT
            .append(arena, OPEN_PAREN_DOCUMENT)
            .append(arena, eco_format!("{value}"))
            .append(arena, CLOSE_PAREN_DOCUMENT),
    }
}

fn pub_<'a, 'doc>(publicity: Publicity) -> Document<'a, 'doc> {
    match publicity {
        Publicity::Public | Publicity::Internal { .. } => PUB_SPACE_DOCUMENT,
        Publicity::Private => EMPTY_DOCUMENT,
    }
}
