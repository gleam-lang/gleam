#[cfg(test)]
mod tests;

use crate::{
    Error, Result,
    ast::{
        CustomType, Import, ModuleConstant, TypeAlias, TypeAstConstructor, TypeAstFn, TypeAstHole,
        TypeAstTuple, TypeAstVar, *,
    },
    build::Target,
    docvec,
    io::Utf8Writer,
    parse::extra::{Comment, ModuleExtra},
    pretty::{self, *},
    warning::WarningEmitter,
};
use ecow::{EcoString, eco_format};
use itertools::Itertools;
use std::cmp::Ordering;
use vec1::Vec1;

use crate::type_::Deprecation;
use camino::Utf8Path;

const INDENT: isize = 2;

pub fn pretty(writer: &mut impl Utf8Writer, src: &EcoString, path: &Utf8Path) -> Result<()> {
    let parsed = crate::parse::parse_module(path.to_owned(), src, &WarningEmitter::null())
        .map_err(|error| Error::Parse {
            path: path.to_path_buf(),
            src: src.clone(),
            error: Box::new(error),
        })?;
    let intermediate = Intermediate::from_extra(&parsed.extra, src);
    Formatter::with_comments(&intermediate)
        .module(&parsed.module)
        .pretty_print(80, writer)
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
#[derive(Debug, Clone, Default)]
pub struct Formatter<'a> {
    comments: &'a [Comment<'a>],
    doc_comments: &'a [Comment<'a>],
    module_comments: &'a [Comment<'a>],
    empty_lines: &'a [u32],
    new_lines: &'a [u32],
    trailing_commas: &'a [u32],
}

impl<'comments> Formatter<'comments> {
    pub fn new() -> Self {
        Default::default()
    }

    pub(crate) fn with_comments(extra: &'comments Intermediate<'comments>) -> Self {
        Self {
            comments: &extra.comments,
            doc_comments: &extra.doc_comments,
            module_comments: &extra.module_comments,
            empty_lines: extra.empty_lines,
            new_lines: extra.new_lines,
            trailing_commas: extra.trailing_commas,
        }
    }

    fn any_comments(&self, limit: u32) -> bool {
        self.comments
            .first()
            .is_some_and(|comment| comment.start < limit)
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
    ) -> impl Iterator<Item = (u32, Option<&'comments str>)> + use<'comments> {
        let (popped, rest, empty_lines) =
            comments_before(self.comments, self.empty_lines, limit, true);
        self.comments = rest;
        self.empty_lines = empty_lines;
        popped
    }

    /// Pop comments that occur before a byte-index in the source, consuming
    /// and retaining any empty lines contained within.
    fn pop_comments(
        &mut self,
        limit: u32,
    ) -> impl Iterator<Item = Option<&'comments str>> + use<'comments> {
        self.pop_comments_with_position(limit)
            .map(|(_position, comment)| comment)
    }

    /// Pop doc comments that occur before a byte-index in the source, consuming
    /// and dropping any empty lines contained within.
    fn pop_doc_comments(
        &mut self,
        limit: u32,
    ) -> impl Iterator<Item = Option<&'comments str>> + use<'comments> {
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

    fn targeted_definition<'a>(&mut self, definition: &'a TargetedDefinition) -> Document<'a> {
        let target = definition.target;
        let definition = &definition.definition;
        let start = definition.location().start;

        let comments = self.pop_comments_with_position(start);
        let comments = self.printed_documented_comments(comments);
        let document = self.documented_definition(definition);
        let document = match target {
            None => document,
            Some(Target::Erlang) => docvec!["@target(erlang)", line(), document],
            Some(Target::JavaScript) => docvec!["@target(javascript)", line(), document],
        };

        comments.to_doc().append(document.group())
    }

    pub(crate) fn module<'a>(&mut self, module: &'a UntypedModule) -> Document<'a> {
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
                    documents.push(lines(2));
                }
                documents.append(&mut self.imports(definitions.collect_vec()));
                previous_was_a_definition = false;
            } else {
                for definition in definitions {
                    if !documents.is_empty() {
                        documents.push(lines(2));
                    }
                    documents.push(self.targeted_definition(definition));
                }
                previous_was_a_definition = true;
            }
        }

        let definitions = concat(documents);

        // Now that definitions has been collected, only freestanding comments (//)
        // and doc comments (///) remain. Freestanding comments aren't associated
        // with any statement, and are moved to the bottom of the module.
        let doc_comments = join(
            self.doc_comments
                .iter()
                .map(|comment| "///".to_doc().append(EcoString::from(comment.content))),
            line(),
        );

        let comments = match printed_comments(self.pop_comments(u32::MAX), false) {
            Some(comments) => comments,
            None => nil(),
        };

        let module_comments = if !self.module_comments.is_empty() {
            let comments = self
                .module_comments
                .iter()
                .map(|s| "////".to_doc().append(EcoString::from(s.content)));
            join(comments, line()).append(line())
        } else {
            nil()
        };

        let non_empty = vec![module_comments, definitions, doc_comments, comments]
            .into_iter()
            .filter(|doc| !doc.is_empty());

        join(non_empty, line()).append(line())
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
    fn imports<'a>(&mut self, imports: Vec<&'a TargetedDefinition>) -> Vec<Document<'a>> {
        let mut import_groups_docs = vec![];
        let mut current_group = vec![];
        let mut current_group_delimiter = nil();

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
                        current_group_delimiter,
                        self.sorted_import_group(&current_group)
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
                current_group_delimiter = printed_comments(comments, true).unwrap_or(nil());
            }
            // Lastly we add the import to the group.
            current_group.push(import);
        }

        // Let's not forget about the last import group!
        if !current_group.is_empty() {
            import_groups_docs.push(docvec![
                current_group_delimiter,
                self.sorted_import_group(&current_group)
            ]);
        }

        // We want all consecutive import groups to be separated by an empty line.
        // This should really be `.intersperse(line())` but I can't do that
        // because of https://github.com/rust-lang/rust/issues/48919.
        Itertools::intersperse(import_groups_docs.into_iter(), lines(2)).collect_vec()
    }

    /// Prints the imports as a single sorted group of import statements.
    ///
    fn sorted_import_group<'a>(&mut self, imports: &[&'a TargetedDefinition]) -> Document<'a> {
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
            .map(|import| self.targeted_definition(import));

        // This should really be `.intersperse(line())` but I can't do that
        // because of https://github.com/rust-lang/rust/issues/48919.
        Itertools::intersperse(imports, line())
            .collect_vec()
            .to_doc()
    }

    fn definition<'a>(&mut self, statement: &'a UntypedDefinition) -> Document<'a> {
        match statement {
            Definition::Function(function) => self.statement_fn(function),

            Definition::TypeAlias(alias) => self.type_alias(alias),

            Definition::CustomType(ct) => self.custom_type(ct),

            Definition::Import(Import {
                module,
                as_name,
                unqualified_values,
                unqualified_types,
                ..
            }) => {
                let second = if unqualified_values.is_empty() && unqualified_types.is_empty() {
                    nil()
                } else {
                    let unqualified_types = unqualified_types
                        .iter()
                        .sorted_by(|a, b| a.name.cmp(&b.name))
                        .map(|type_| docvec!["type ", type_]);
                    let unqualified_values = unqualified_values
                        .iter()
                        .sorted_by(|a, b| a.name.cmp(&b.name))
                        .map(|value| value.to_doc());
                    let unqualified = join(
                        unqualified_types.chain(unqualified_values),
                        flex_break(",", ", "),
                    );
                    let unqualified = break_("", "")
                        .append(unqualified)
                        .nest(INDENT)
                        .append(break_(",", ""))
                        .group();
                    ".{".to_doc().append(unqualified).append("}")
                };

                let doc = docvec!["import ", module.as_str(), second];
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
                    (_, Some((AssignName::Variable(name) | AssignName::Discard(name), _))) => {
                        doc.append(" as ").append(name)
                    }
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
                    .to_doc();
                let head = attributes
                    .append(pub_(*publicity))
                    .append("const ")
                    .append(name.as_str());
                let head = match annotation {
                    None => head,
                    Some(t) => head.append(": ").append(self.type_ast(t)),
                };
                head.append(" = ").append(self.const_expr(value))
            }
        }
    }

    fn const_expr<'a, A, B>(&mut self, value: &'a Constant<A, B>) -> Document<'a> {
        let comments = self.pop_comments(value.location().start);
        let document = match value {
            Constant::Int { value, .. } => self.int(value),

            Constant::Float { value, .. } => self.float(value),

            Constant::String { value, .. } => self.string(value),

            Constant::List {
                elements, location, ..
            } => self.const_list(elements, location),

            Constant::Tuple {
                elements, location, ..
            } => self.const_tuple(elements, location),

            Constant::BitArray {
                segments, location, ..
            } => {
                let segment_docs = segments
                    .iter()
                    .map(|segment| bit_array_segment(segment, |e| self.const_expr(e)))
                    .collect_vec();

                let packing = self.items_sequence_packing(
                    segments,
                    None,
                    |segment| segment.value.can_have_multiple_per_line(),
                    *location,
                );

                self.bit_array(segment_docs, packing, location)
            }

            Constant::Record {
                name,
                arguments,
                module: None,
                ..
            } if arguments.is_empty() => name.to_doc(),

            Constant::Record {
                name,
                arguments,
                module: Some((m, _)),
                ..
            } if arguments.is_empty() => m.to_doc().append(".").append(name.as_str()),

            Constant::Record {
                name,
                arguments,
                module: None,
                location,
                ..
            } => {
                let arguments = arguments
                    .iter()
                    .map(|argument| self.constant_call_arg(argument))
                    .collect_vec();
                name.to_doc()
                    .append(self.wrap_arguments(arguments, location.end))
                    .group()
            }

            Constant::Record {
                name,
                arguments,
                module: Some((m, _)),
                location,
                ..
            } => {
                let arguments = arguments
                    .iter()
                    .map(|argument| self.constant_call_arg(argument))
                    .collect_vec();
                m.to_doc()
                    .append(".")
                    .append(name.as_str())
                    .append(self.wrap_arguments(arguments, location.end))
                    .group()
            }

            Constant::Var {
                name, module: None, ..
            } => name.to_doc(),

            Constant::Var {
                name,
                module: Some((module, _)),
                ..
            } => docvec![module, ".", name],

            Constant::StringConcatenation { left, right, .. } => self
                .const_expr(left)
                .append(break_("", " ").append("<>".to_doc()))
                .nest(INDENT)
                .append(" ")
                .append(self.const_expr(right)),

            Constant::RecordUpdate {
                module,
                name,
                record,
                arguments,
                location,
                ..
            } => self.const_record_update(module, name, record, arguments, location),

            Constant::Invalid { .. } => panic!("invalid constants can not be in an untyped ast"),
        };
        commented(document, comments)
    }

    fn const_list<'a, A, B>(
        &mut self,
        elements: &'a [Constant<A, B>],
        location: &SrcSpan,
    ) -> Document<'a> {
        if elements.is_empty() {
            // We take all comments that come _before_ the end of the list,
            // that is all comments that are inside "[" and "]", if there's
            // any comment we want to put it inside the empty list!
            return match printed_comments(self.pop_comments(location.end), false) {
                None => "[]".to_doc(),
                Some(comments) => "["
                    .to_doc()
                    .append(break_("", "").nest(INDENT))
                    .append(comments)
                    .append(break_("", ""))
                    .append("]")
                    // vvv We want to make sure the comments are on a separate
                    //     line from the opening and closing brackets so we
                    //     force the breaks to be split on newlines.
                    .force_break(),
            };
        }

        let list_packing = self.items_sequence_packing(
            elements,
            None,
            |element| element.can_have_multiple_per_line(),
            *location,
        );
        let comma = match list_packing {
            ItemsPacking::FitMultiplePerLine => flex_break(",", ", "),
            ItemsPacking::FitOnePerLine | ItemsPacking::BreakOnePerLine => break_(",", ", "),
        };

        let mut elements_doc = nil();
        for element in elements.iter() {
            let empty_lines = self.pop_empty_lines(element.location().start);
            let element_doc = self.const_expr(element);

            elements_doc = if elements_doc.is_empty() {
                element_doc
            } else if empty_lines {
                // If there's empty lines before the list item we want to add an
                // empty line here. Notice how we're making sure no nesting is
                // added after the comma, otherwise we would be adding needless
                // whitespace in the empty line!
                docvec![
                    elements_doc,
                    comma.clone().set_nesting(0),
                    line(),
                    element_doc
                ]
            } else {
                docvec![elements_doc, comma.clone(), element_doc]
            };
        }
        elements_doc = elements_doc.next_break_fits(NextBreakFitsMode::Disabled);

        let doc = break_("[", "[").append(elements_doc).nest(INDENT);

        // We get all remaining comments that come before the list's closing
        // square bracket.
        // If there's any we add those before the closing square bracket instead
        // of moving those out of the list.
        // Otherwise those would be moved out of the list.
        let comments = self.pop_comments(location.end);
        let doc = match printed_comments(comments, false) {
            None => doc.append(break_(",", "")).append("]"),
            Some(comment) => doc
                .append(break_(",", "").nest(INDENT))
                // ^ See how here we're adding the missing indentation to the
                //   final break so that the final comment is as indented as the
                //   list's items.
                .append(comment)
                .append(line())
                .append("]")
                .force_break(),
        };

        match list_packing {
            ItemsPacking::FitOnePerLine | ItemsPacking::FitMultiplePerLine => doc.group(),
            ItemsPacking::BreakOnePerLine => doc.force_break(),
        }
    }

    pub fn const_tuple<'a, A, B>(
        &mut self,
        elements: &'a [Constant<A, B>],
        location: &SrcSpan,
    ) -> Document<'a> {
        if elements.is_empty() {
            // We take all comments that come _before_ the end of the tuple,
            // that is all comments that are inside "#(" and ")", if there's
            // any comment we want to put it inside the empty list!
            return match printed_comments(self.pop_comments(location.end), false) {
                None => "#()".to_doc(),
                Some(comments) => "#("
                    .to_doc()
                    .append(break_("", "").nest(INDENT))
                    .append(comments)
                    .append(break_("", ""))
                    .append(")")
                    // vvv We want to make sure the comments are on a separate
                    //     line from the opening and closing parentheses so we
                    //     force the breaks to be split on newlines.
                    .force_break(),
            };
        }

        let arguments_docs = elements.iter().map(|element| self.const_expr(element));
        let tuple_doc = break_("#(", "#(")
            .append(
                join(arguments_docs, break_(",", ", "))
                    .next_break_fits(NextBreakFitsMode::Disabled),
            )
            .nest(INDENT);

        let comments = self.pop_comments(location.end);
        match printed_comments(comments, false) {
            None => tuple_doc.append(break_(",", "")).append(")").group(),
            Some(comments) => tuple_doc
                .append(break_(",", "").nest(INDENT))
                .append(comments)
                .append(line())
                .append(")")
                .force_break(),
        }
    }

    fn documented_definition<'a>(&mut self, s: &'a UntypedDefinition) -> Document<'a> {
        let comments = self.doc_comments(s.location().start);
        comments.append(self.definition(s).group()).group()
    }

    fn doc_comments<'a>(&mut self, limit: u32) -> Document<'a> {
        let mut comments = self.pop_doc_comments(limit).peekable();
        match comments.peek() {
            None => nil(),
            Some(_) => join(
                comments.map(|c| match c {
                    Some(c) => "///".to_doc().append(EcoString::from(c)),
                    None => unreachable!("empty lines dropped by pop_doc_comments"),
                }),
                line(),
            )
            .append(line())
            .force_break(),
        }
    }

    fn type_ast_constructor<'a>(
        &mut self,
        module: &'a Option<(EcoString, SrcSpan)>,
        name: &'a str,
        arguments: &'a [TypeAst],
        location: &SrcSpan,
        _name_location: &SrcSpan,
    ) -> Document<'a> {
        let head = module
            .as_ref()
            .map(|(qualifier, _)| qualifier.to_doc().append(".").append(name))
            .unwrap_or_else(|| name.to_doc());

        if arguments.is_empty() {
            head
        } else {
            head.append(self.type_arguments(arguments, location))
        }
    }

    fn type_ast<'a>(&mut self, t: &'a TypeAst) -> Document<'a> {
        match t {
            TypeAst::Hole(TypeAstHole { name, .. }) => name.to_doc(),

            TypeAst::Constructor(TypeAstConstructor {
                name,
                arguments,
                module,
                location,
                name_location,
                start_parentheses: _,
            }) => self.type_ast_constructor(module, name, arguments, location, name_location),

            TypeAst::Fn(TypeAstFn {
                arguments,
                return_,
                location,
            }) => "fn"
                .to_doc()
                .append(self.type_arguments(arguments, location))
                .group()
                .append(" ->")
                .append(break_("", " ").append(self.type_ast(return_)).nest(INDENT)),

            TypeAst::Var(TypeAstVar { name, .. }) => name.to_doc(),

            TypeAst::Tuple(TypeAstTuple { elements, location }) => {
                "#".to_doc().append(self.type_arguments(elements, location))
            }
        }
        .group()
    }

    fn type_arguments<'a>(&mut self, arguments: &'a [TypeAst], location: &SrcSpan) -> Document<'a> {
        let arguments = arguments
            .iter()
            .map(|type_| self.type_ast(type_))
            .collect_vec();
        self.wrap_arguments(arguments, location.end)
    }

    pub fn type_alias<'a, A>(&mut self, alias: &'a TypeAlias<A>) -> Document<'a> {
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
            .to_doc();

        let head = docvec![attributes, pub_(*publicity), "type ", name];
        let head = if arguments.is_empty() {
            head
        } else {
            let arguments = arguments.iter().map(|(_, e)| e.to_doc()).collect_vec();
            head.append(self.wrap_arguments(arguments, location.end).group())
        };

        head.append(" =")
            .append(line().append(self.type_ast(type_)).group().nest(INDENT))
    }

    fn fn_arg<'a, A>(&mut self, arg: &'a Arg<A>) -> Document<'a> {
        let comments = self.pop_comments(arg.location.start);
        let doc = match &arg.annotation {
            None => arg.names.to_doc(),
            Some(a) => arg.names.to_doc().append(": ").append(self.type_ast(a)),
        }
        .group();
        commented(doc, comments)
    }

    fn statement_fn<'a>(&mut self, function: &'a UntypedFunction) -> Document<'a> {
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
            .to_doc();

        // Fn name and args
        let arguments = arguments
            .iter()
            .map(|argument| self.fn_arg(argument))
            .collect_vec();
        let signature = pub_(*publicity)
            .append("fn ")
            .append(
                &name
                    .as_ref()
                    .expect("Function in a statement must be named")
                    .1,
            )
            .append(
                self.wrap_arguments(
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
            Some(anno) => signature.append(" -> ").append(self.type_ast(anno)),
            None => signature,
        }
        .group();

        if body.is_empty() {
            return attributes.append(signature);
        }

        let head = attributes.append(signature);

        // Format body

        let body = self.statements(body);

        // Add any trailing comments
        let body = match printed_comments(self.pop_comments(*end_position), false) {
            Some(comments) => body.append(line()).append(comments),
            None => body,
        };

        // Stick it all together
        head.append(" {")
            .append(line().append(body).nest(INDENT).group())
            .append(line())
            .append("}")
    }

    fn expr_fn<'a>(
        &mut self,
        arguments: &'a [UntypedArg],
        return_annotation: Option<&'a TypeAst>,
        body: &'a Vec1<UntypedStatement>,
        location: &SrcSpan,
        end_of_head_byte_index: &u32,
    ) -> Document<'a> {
        let arguments_docs = arguments
            .iter()
            .map(|argument| self.fn_arg(argument))
            .collect_vec();
        let arguments = self
            .wrap_arguments(arguments_docs, *end_of_head_byte_index)
            .group()
            .next_break_fits(NextBreakFitsMode::Disabled);
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

        let header = "fn".to_doc().append(arguments);

        let header = match return_annotation {
            None => header,
            Some(t) => header.append(" -> ").append(self.type_ast(t)),
        };

        let statements = self.statements(body.as_vec());
        let body = match printed_comments(self.pop_comments(location.end), false) {
            None => statements,
            Some(comments) => statements.append(line()).append(comments).force_break(),
        };

        header.append(" ").append(wrap_block(body)).group()
    }

    fn statements<'a>(&mut self, statements: &'a [UntypedStatement]) -> Document<'a> {
        let mut previous_position = 0;
        let count = statements.len();
        let mut documents = Vec::with_capacity(count * 2);
        for (i, statement) in statements.iter().enumerate() {
            let preceding_newline = self.pop_empty_lines(previous_position + 1);
            if i != 0 && preceding_newline {
                documents.push(lines(2));
            } else if i != 0 {
                documents.push(line());
            }
            previous_position = statement.location().end;
            documents.push(self.statement(statement).group());

            // If the last statement is a use we make sure it's followed by a
            // todo to make it explicit it has an unimplemented callback.
            if statement.is_use() && i == count - 1 {
                documents.push(line());
                documents.push("todo".to_doc());
            }
        }

        if count == 1
            && statements
                .first()
                .is_some_and(|statement| statement.is_expression())
        {
            documents.to_doc()
        } else {
            documents.to_doc().force_break()
        }
    }

    fn assignment<'a>(&mut self, assignment: &'a UntypedAssignment) -> Document<'a> {
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
            AssignmentKind::Let | AssignmentKind::Generated => ("let ", None),
            AssignmentKind::Assert { message, .. } => ("let assert ", message.as_ref()),
        };

        let pattern = self.pattern(pattern);

        let annotation = annotation
            .as_ref()
            .map(|a| ": ".to_doc().append(self.type_ast(a)));

        let doc = keyword
            .to_doc()
            .append(pattern.append(annotation).group())
            .append(" =")
            .append(self.assigned_value(value));

        commented(
            self.append_as_message(doc, PrecedingAs::Expression, message),
            comments,
        )
    }

    fn expr<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        let comments = self.pop_comments(expr.start_byte_index());

        let document = match expr {
            UntypedExpr::Panic { message, .. } => {
                self.append_as_message("panic".to_doc(), PrecedingAs::Keyword, message.as_deref())
            }

            UntypedExpr::Todo { message, .. } => {
                self.append_as_message("todo".to_doc(), PrecedingAs::Keyword, message.as_deref())
            }

            UntypedExpr::Echo {
                expression,
                location: _,
                keyword_end: _,
                message,
            } => self.echo(expression, message),

            UntypedExpr::PipeLine { expressions, .. } => self.pipeline(expressions, false),

            UntypedExpr::Int { value, .. } => self.int(value),

            UntypedExpr::Float { value, .. } => self.float(value),

            UntypedExpr::String { value, .. } => self.string(value),

            UntypedExpr::Block {
                statements,
                location,
                ..
            } => self.block(location, statements, false),

            UntypedExpr::Var { name, .. } if name == CAPTURE_VARIABLE => "_".to_doc(),

            UntypedExpr::Var { name, .. } => name.to_doc(),

            UntypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(tuple, *index),

            UntypedExpr::NegateInt { value, .. } => self.negate_int(value),

            UntypedExpr::NegateBool { value, .. } => self.negate_bool(value),

            UntypedExpr::Fn { kind, body, .. } if kind.is_capture() => {
                self.fn_capture(body, FnCapturePosition::EverywhereElse)
            }

            UntypedExpr::Fn {
                return_annotation,
                arguments,
                body,
                location,
                end_of_head_byte_index,
                ..
            } => self.expr_fn(
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
            } => self.list(elements, tail.as_deref(), location),

            UntypedExpr::Call {
                fun,
                arguments,
                location,
                ..
            } => self.call(fun, arguments, location),

            UntypedExpr::BinOp {
                name, left, right, ..
            } => self.bin_op(name, left, right, false),

            UntypedExpr::Case {
                subjects,
                clauses,
                location,
            } => self.case(subjects, clauses.as_deref().unwrap_or_default(), location),

            UntypedExpr::FieldAccess {
                label, container, ..
            } => if let UntypedExpr::TupleIndex { .. } = container.as_ref() {
                self.expr(container).surround("{ ", " }")
            } else {
                self.expr(container)
            }
            .append(".")
            .append(label.as_str()),

            UntypedExpr::Tuple { elements, location } => self.tuple(elements, location),

            UntypedExpr::BitArray {
                segments, location, ..
            } => {
                let segment_docs = segments
                    .iter()
                    .map(|segment| bit_array_segment(segment, |e| self.bit_array_segment_expr(e)))
                    .collect_vec();

                let packing = self.items_sequence_packing(
                    segments,
                    None,
                    |segment| segment.value.can_have_multiple_per_line(),
                    *location,
                );

                self.bit_array(segment_docs, packing, location)
            }
            UntypedExpr::RecordUpdate {
                constructor,
                record,
                arguments,
                location,
                ..
            } => self.record_update(constructor, record, arguments, location),
        };
        commented(document, comments)
    }

    fn string<'a>(&self, string: &'a EcoString) -> Document<'a> {
        let doc = string.to_doc().surround("\"", "\"");
        if string.contains('\n') {
            doc.force_break()
        } else {
            doc
        }
    }

    fn bin_op_string<'a>(&self, string: &'a EcoString) -> Document<'a> {
        let lines = string.split('\n').collect_vec();
        match lines.as_slice() {
            [] | [_] => string.to_doc().surround("\"", "\""),
            [first_line, lines @ ..] => {
                let mut doc = docvec!["\"", first_line];
                for line in lines {
                    doc = doc
                        .append(pretty::line().set_nesting(0))
                        .append(line.to_doc())
                }
                doc.append("\"".to_doc()).group()
            }
        }
    }

    fn float<'a>(&self, value: &'a str) -> Document<'a> {
        // Create parts
        let mut parts = value.split('.');
        let integer_part = parts.next().unwrap_or_default();
        // floating point part
        let fp_part = parts.next().unwrap_or_default();
        let integer_doc = self.underscore_integer_string(integer_part);
        let dot_doc = ".".to_doc();

        // Split fp_part into a regular fractional and maybe a scientific part
        let (fp_part_fractional, fp_part_scientific) = fp_part.split_at(
            fp_part
                .chars()
                .position(|ch| ch == 'e')
                .unwrap_or(fp_part.len()),
        );

        // Trim right any consequtive '0's
        let mut fp_part_fractional = fp_part_fractional.trim_end_matches('0').to_string();
        // If there is no fractional part left, add a '0', thus that 1. becomes 1.0 etc.
        if fp_part_fractional.is_empty() {
            fp_part_fractional.push('0');
        }
        let fp_doc = fp_part_fractional.chars().collect::<EcoString>();

        integer_doc
            .append(dot_doc)
            .append(fp_doc)
            .append(fp_part_scientific)
    }

    fn int<'a>(&self, value: &'a str) -> Document<'a> {
        if value.starts_with("0x") || value.starts_with("0b") || value.starts_with("0o") {
            return value.to_doc();
        }

        self.underscore_integer_string(value)
    }

    fn underscore_integer_string<'a>(&self, value: &'a str) -> Document<'a> {
        let underscore_ch = '_';
        let minus_ch = '-';

        let len = value.len();
        let underscore_ch_cnt = value.matches(underscore_ch).count();
        let reformat_watershed = if value.starts_with(minus_ch) { 6 } else { 5 };
        let insert_underscores = (len - underscore_ch_cnt) >= reformat_watershed;

        let mut new_value = String::new();
        let mut j = 0;
        for (i, ch) in value.chars().rev().enumerate() {
            if ch == '_' {
                continue;
            }

            if insert_underscores && i != 0 && ch != minus_ch && i < len && j % 3 == 0 {
                new_value.push(underscore_ch);
            }
            new_value.push(ch);

            j += 1;
        }

        new_value.chars().rev().collect::<EcoString>().to_doc()
    }

    fn pattern_constructor<'a>(
        &mut self,
        name: &'a str,
        arguments: &'a [CallArg<UntypedPattern>],
        module: &'a Option<(EcoString, SrcSpan)>,
        spread: Option<SrcSpan>,
        location: &SrcSpan,
    ) -> Document<'a> {
        fn is_breakable(expr: &UntypedPattern) -> bool {
            match expr {
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
            Some((m, _)) => m.to_doc().append(".").append(name),
            None => name.to_doc(),
        };

        if arguments.is_empty() && spread.is_some() {
            name.append("(..)")
        } else if arguments.is_empty() {
            name
        } else if spread.is_some() {
            let arguments = arguments
                .iter()
                .map(|argument| self.pattern_call_arg(argument))
                .collect_vec();
            name.append(self.wrap_arguments_with_spread(arguments, location.end))
                .group()
        } else {
            match arguments {
                [argument] if is_breakable(&argument.value) => name
                    .append("(")
                    .append(self.pattern_call_arg(argument))
                    .append(")")
                    .group(),

                _ => {
                    let arguments = arguments
                        .iter()
                        .map(|argument| self.pattern_call_arg(argument))
                        .collect_vec();
                    name.append(self.wrap_arguments(arguments, location.end))
                        .group()
                }
            }
        }
    }

    fn call<'a>(
        &mut self,
        fun: &'a UntypedExpr,
        arguments: &'a [CallArg<UntypedExpr>],
        location: &SrcSpan,
    ) -> Document<'a> {
        let expr = match fun {
            UntypedExpr::PipeLine { .. } => break_block(self.expr(fun)),

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
            | UntypedExpr::NegateInt { .. } => self.expr(fun),
        };

        let arity = arguments.len();
        self.append_inlinable_wrapped_arguments(
            expr,
            arguments,
            location,
            |argument| &argument.value,
            |self_, arg| self_.call_arg(arg, arity),
        )
    }

    fn tuple<'a>(&mut self, elements: &'a [UntypedExpr], location: &SrcSpan) -> Document<'a> {
        if elements.is_empty() {
            // We take all comments that come _before_ the end of the tuple,
            // that is all comments that are inside "#(" and ")", if there's
            // any comment we want to put it inside the empty tuple!
            return match printed_comments(self.pop_comments(location.end), false) {
                None => "#()".to_doc(),
                Some(comments) => "#("
                    .to_doc()
                    .append(break_("", "").nest(INDENT))
                    .append(comments)
                    .append(break_("", ""))
                    .append(")")
                    // vvv We want to make sure the comments are on a separate
                    //     line from the opening and closing parentheses so we
                    //     force the breaks to be split on newlines.
                    .force_break(),
            };
        }

        self.append_inlinable_wrapped_arguments(
            "#".to_doc(),
            elements,
            location,
            |e| e,
            |self_, e| self_.comma_separated_item(e, elements.len()),
        )
    }

    // Appends to the given docs a comma-separated list of documents wrapped by
    // parentheses. If the last item of the argument list is splittable the
    // resulting document will try to first split that before splitting all the
    // other arguments.
    // This is used for function calls and tuples.
    fn append_inlinable_wrapped_arguments<'a, 'b, T, ToExpr, ToDoc>(
        &mut self,
        doc: Document<'a>,
        values: &'b [T],
        location: &SrcSpan,
        to_expr: ToExpr,
        to_doc: ToDoc,
    ) -> Document<'a>
    where
        T: HasLocation,
        ToExpr: Fn(&T) -> &UntypedExpr,
        ToDoc: Fn(&mut Self, &'b T) -> Document<'a>,
    {
        match init_and_last(values) {
            Some((initial_values, last_value))
                if is_breakable_argument(to_expr(last_value), values.len())
                    && !self.any_comments(last_value.location().start) =>
            {
                let mut docs = initial_values
                    .iter()
                    .map(|value| to_doc(self, value))
                    .collect_vec();

                let last_value_doc = to_doc(self, last_value)
                    .group()
                    .next_break_fits(NextBreakFitsMode::Enabled);

                docs.append(&mut vec![last_value_doc]);

                doc.append(self.wrap_function_call_arguments(docs, location))
                    .next_break_fits(NextBreakFitsMode::Disabled)
                    .group()
            }

            Some(_) | None => {
                let docs = values.iter().map(|value| to_doc(self, value)).collect_vec();
                doc.append(self.wrap_function_call_arguments(docs, location))
                    .group()
            }
        }
    }

    pub fn case<'a>(
        &mut self,
        subjects: &'a [UntypedExpr],
        clauses: &'a [UntypedClause],
        location: &'a SrcSpan,
    ) -> Document<'a> {
        let subjects_doc = break_("case", "case ")
            .append(join(
                subjects.iter().map(|s| self.expr(s).group()),
                break_(",", ", "),
            ))
            .nest(INDENT)
            .append(break_("", " "))
            .append("{")
            .next_break_fits(NextBreakFitsMode::Disabled)
            .group();

        let clauses_doc = concat(
            clauses
                .iter()
                .enumerate()
                .map(|(i, c)| self.clause(c, i as u32).group()),
        );

        // We get all remaining comments that come before the case's closing
        // bracket. If there's any we add those before the closing bracket
        // instead of moving those out of the case expression.
        // Otherwise those would be moved out of the case expression.
        let comments = self.pop_comments(location.end);
        let closing_bracket = match printed_comments(comments, false) {
            None => docvec![line(), "}"],
            Some(comment) => docvec![line(), comment]
                .nest(INDENT)
                .append(line())
                .append("}"),
        };

        subjects_doc
            .append(line().append(clauses_doc).nest(INDENT))
            .append(closing_bracket)
            .force_break()
    }

    pub fn record_update<'a>(
        &mut self,
        constructor: &'a UntypedExpr,
        record: &'a RecordBeingUpdated<UntypedExpr>,
        arguments: &'a [UntypedRecordUpdateArg],
        location: &SrcSpan,
    ) -> Document<'a> {
        let constructor_doc: Document<'a> = self.expr(constructor);
        let pieces = std::iter::once(UntypedRecordUpdatePiece::Record(record))
            .chain(arguments.iter().map(UntypedRecordUpdatePiece::Argument))
            .collect_vec();

        self.append_inlinable_wrapped_arguments(
            constructor_doc,
            &pieces,
            location,
            |arg| match arg {
                UntypedRecordUpdatePiece::Argument(arg) => &arg.value,
                UntypedRecordUpdatePiece::Record(record) => record.base.as_ref(),
            },
            |this, arg| match arg {
                UntypedRecordUpdatePiece::Argument(arg) => this.record_update_arg(arg),
                UntypedRecordUpdatePiece::Record(record) => {
                    let comments = this.pop_comments(record.location.start);
                    commented("..".to_doc().append(this.expr(&record.base)), comments)
                }
            },
        )
    }

    pub fn const_record_update<'a, A, B>(
        &mut self,
        module: &Option<(EcoString, SrcSpan)>,
        name: &'a EcoString,
        record: &'a RecordBeingUpdated<Constant<A, B>>,
        arguments: &'a [RecordUpdateArg<Constant<A, B>>],
        location: &SrcSpan,
    ) -> Document<'a> {
        let constructor_doc = match module {
            Some((m, _)) => m.to_doc().append(".").append(name.as_str()),
            None => name.to_doc(),
        };

        let pieces = std::iter::once(RecordUpdatePiece::Record(record))
            .chain(arguments.iter().map(RecordUpdatePiece::Argument))
            .collect_vec();

        let docs = pieces
            .iter()
            .map(|piece| match piece {
                RecordUpdatePiece::Argument(arg) => {
                    let comments = self.pop_comments(arg.location.start);
                    let doc = match arg {
                        _ if arg.uses_label_shorthand() => arg.label.as_str().to_doc().append(":"),
                        _ => arg
                            .label
                            .as_str()
                            .to_doc()
                            .append(": ")
                            .append(self.const_expr(&arg.value))
                            .group(),
                    };
                    commented(doc, comments)
                }
                RecordUpdatePiece::Record(record) => {
                    let comments = self.pop_comments(record.location.start);
                    commented(
                        "..".to_doc().append(self.const_expr(&record.base)),
                        comments,
                    )
                }
            })
            .collect_vec();

        constructor_doc
            .append(self.wrap_arguments(docs, location.end))
            .group()
    }

    pub fn bin_op<'a>(
        &mut self,
        name: &'a BinOp,
        left: &'a UntypedExpr,
        right: &'a UntypedExpr,
        nest_steps: bool,
    ) -> Document<'a> {
        let left_side = self.bin_op_side(name, left, nest_steps);

        let comments = self.pop_comments(right.start_byte_index());
        let name_doc = break_("", " ").append(commented(name.to_doc(), comments));

        let right_side = self.bin_op_side(name, right, nest_steps);

        left_side
            .append(if nest_steps {
                name_doc.nest(INDENT)
            } else {
                name_doc
            })
            .append(" ")
            .append(right_side)
    }

    fn bin_op_side<'a>(
        &mut self,
        operator: &'a BinOp,
        side: &'a UntypedExpr,
        nest_steps: bool,
    ) -> Document<'a> {
        let side_doc = match side {
            UntypedExpr::String { value, .. } => self.bin_op_string(value),
            UntypedExpr::BinOp {
                name, left, right, ..
            } => self.bin_op(name, left, right, nest_steps),
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
            | UntypedExpr::NegateInt { .. } => self.expr(side),
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
                side_doc.group(),
                operator.precedence(),
                side.bin_op_precedence(),
            ),
        }
    }

    pub fn operator_side<'a>(&self, doc: Document<'a>, op: u8, side: u8) -> Document<'a> {
        if op > side {
            wrap_block(doc).group()
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

    fn pipeline<'a>(
        &mut self,
        expressions: &'a Vec1<UntypedExpr>,
        nest_pipe: bool,
    ) -> Document<'a> {
        let mut docs = Vec::with_capacity(expressions.len() * 3);
        let first = expressions.first();
        let first_precedence = first.bin_op_precedence();
        let first = self.expr(first).group();
        docs.push(self.operator_side(first, 5, first_precedence));

        let pipeline_start = expressions.first().location().start;
        let pipeline_end = expressions.last().location().end;
        let try_to_keep_on_one_line = !self.spans_multiple_lines(pipeline_start, pipeline_end);

        for expr in expressions.iter().skip(1) {
            let comments = self.pop_comments(expr.location().start);
            let doc = if let UntypedExpr::Fn { kind, body, .. } = expr
                && kind.is_capture()
            {
                self.fn_capture(body, FnCapturePosition::RightHandSideOfPipe)
            } else {
                self.expr(expr)
            };
            let doc = if nest_pipe { doc.nest(INDENT) } else { doc };
            let space = if try_to_keep_on_one_line {
                break_("", " ")
            } else {
                line()
            };
            let pipe = space.append(commented("|> ".to_doc(), comments));
            let pipe = if nest_pipe { pipe.nest(INDENT) } else { pipe };
            docs.push(pipe);
            docs.push(self.operator_side(doc, 4, expr.bin_op_precedence()));
        }

        if try_to_keep_on_one_line {
            docs.to_doc()
        } else {
            docs.to_doc().force_break()
        }
    }

    fn fn_capture<'a>(
        &mut self,
        call: &'a [UntypedStatement],
        position: FnCapturePosition,
    ) -> Document<'a> {
        // The body of a capture being multiple statements shouldn't be possible...
        if call.len() != 1 {
            panic!("Function capture found not to have a single statement call");
        }

        let Some(Statement::Expression(UntypedExpr::Call {
            fun,
            arguments,
            location,
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
            (FnCapturePosition::RightHandSideOfPipe | FnCapturePosition::EverywhereElse, [arg])
                if arg.is_capture_hole() && arg.label.is_none() =>
            {
                self.expr(fun)
            }

            // The capture is on the right hand side of a pipe and its first
            // argument it an unlabelled capture hole:
            //
            //     wibble |> wobble(_, woo)
            //
            // We want it to become:
            //
            //     wibble |> wobble(woo)
            //
            (FnCapturePosition::RightHandSideOfPipe, [arg, rest @ ..])
                if arg.is_capture_hole() && arg.label.is_none() =>
            {
                let expr = self.expr(fun);
                let arity = rest.len();
                self.append_inlinable_wrapped_arguments(
                    expr,
                    rest,
                    location,
                    |arg| &arg.value,
                    |self_, arg| self_.call_arg(arg, arity),
                )
            }

            // In all other cases we print it like a regular function call
            // without changing it.
            //
            (
                FnCapturePosition::RightHandSideOfPipe | FnCapturePosition::EverywhereElse,
                arguments,
            ) => {
                let expr = self.expr(fun);
                let arity = arguments.len();
                self.append_inlinable_wrapped_arguments(
                    expr,
                    arguments,
                    location,
                    |arg| &arg.value,
                    |self_, arg| self_.call_arg(arg, arity),
                )
            }
        }
    }

    pub fn record_constructor<'a, A>(
        &mut self,
        constructor: &'a RecordConstructor<A>,
    ) -> Document<'a> {
        let comments = self.pop_comments(constructor.location.start);
        let doc_comments = self.doc_comments(constructor.location.start);
        let attributes = AttributesPrinter::new()
            .set_deprecation(&constructor.deprecation)
            .to_doc();

        let doc = if constructor.arguments.is_empty() {
            if self.any_comments(constructor.location.end) {
                attributes
                    .append(constructor.name.as_str().to_doc())
                    .append(self.wrap_arguments(vec![], constructor.location.end))
                    .group()
            } else {
                attributes.append(constructor.name.as_str().to_doc())
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
                        let arg = match label {
                            Some((_, l)) => l.to_doc().append(": ").append(self.type_ast(ast)),
                            None => self.type_ast(ast),
                        };

                        commented(
                            self.doc_comments(location.start).append(arg).group(),
                            arg_comments,
                        )
                    },
                )
                .collect_vec();

            attributes
                .append(constructor.name.as_str().to_doc())
                .append(
                    self.wrap_arguments(arguments, constructor.location.end)
                        .group(),
                )
        };

        commented(doc_comments.append(doc).group(), comments)
    }

    pub fn custom_type<'a, A>(&mut self, type_: &'a CustomType<A>) -> Document<'a> {
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
            .to_doc();

        let doc = attributes
            .append(pub_(*publicity))
            .append(if *opaque { "opaque type " } else { "type " })
            .append(if parameters.is_empty() {
                name.clone().to_doc()
            } else {
                let arguments = type_
                    .parameters
                    .iter()
                    .map(|(_, e)| e.to_doc())
                    .collect_vec();
                type_
                    .name
                    .clone()
                    .to_doc()
                    .append(self.wrap_arguments(arguments, location.end))
                    .group()
            });

        if constructors.is_empty() {
            return doc;
        }
        let doc = doc.append(" {");

        let inner = concat(constructors.iter().map(|c| {
            if self.pop_empty_lines(c.location.start) {
                lines(2)
            } else {
                line()
            }
            .append(self.record_constructor(c))
        }));

        // Add any trailing comments
        let inner = match printed_comments(self.pop_comments(*end_position), false) {
            Some(comments) => inner.append(line()).append(comments),
            None => inner,
        }
        .nest(INDENT)
        .group();

        doc.append(inner).append(line()).append("}")
    }

    fn call_arg<'a>(&mut self, arg: &'a CallArg<UntypedExpr>, arity: usize) -> Document<'a> {
        self.format_call_arg(arg, expr_call_arg_formatting, |this, value| {
            this.comma_separated_item(value, arity)
        })
    }

    fn format_call_arg<'a, A, F, G>(
        &mut self,
        arg: &'a CallArg<A>,
        figure_formatting: F,
        format_value: G,
    ) -> Document<'a>
    where
        F: Fn(&'a CallArg<A>) -> CallArgFormatting<'a, A>,
        G: Fn(&mut Self, &'a A) -> Document<'a>,
    {
        match figure_formatting(arg) {
            CallArgFormatting::Unlabelled(value) => format_value(self, value),
            CallArgFormatting::ShorthandLabelled(label) => {
                let comments = self.pop_comments(arg.location.start);
                let label = label.as_ref().to_doc().append(":");
                commented(label, comments)
            }
            CallArgFormatting::Labelled(label, value) => {
                let comments = self.pop_comments(arg.location.start);
                let label = label.as_ref().to_doc().append(": ");
                let value = format_value(self, value);
                commented(label, comments).append(value)
            }
        }
    }

    fn record_update_arg<'a>(&mut self, arg: &'a UntypedRecordUpdateArg) -> Document<'a> {
        let comments = self.pop_comments(arg.location.start);
        match arg {
            // Argument supplied with a label shorthand.
            _ if arg.uses_label_shorthand() => {
                commented(arg.label.as_str().to_doc().append(":"), comments)
            }
            // Labelled argument.
            _ => {
                let doc = arg
                    .label
                    .as_str()
                    .to_doc()
                    .append(": ")
                    .append(self.expr(&arg.value))
                    .group();

                if arg.value.is_binop() || arg.value.is_pipeline() {
                    commented(doc, comments).nest(INDENT)
                } else {
                    commented(doc, comments)
                }
            }
        }
    }

    fn tuple_index<'a>(&mut self, tuple: &'a UntypedExpr, index: u64) -> Document<'a> {
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
                    self.expr(tuple)
                }
                _ => self.expr(tuple),
            }
        } else {
            self.expr(tuple)
        }
        .append(".")
        .append(index)
    }

    fn case_clause_value<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::BitArray { .. } => {
                let expression_comments = self.pop_comments(expr.location().start);
                let expression_doc = self.expr(expr);
                match printed_comments(expression_comments, true) {
                    Some(comments) => line().append(comments).append(expression_doc).nest(INDENT),
                    None => " ".to_doc().append(expression_doc),
                }
            }

            UntypedExpr::Case { .. } => line().append(self.expr(expr)).nest(INDENT),

            UntypedExpr::Block {
                statements,
                location,
                ..
            } => " ".to_doc().append(self.block(location, statements, true)),

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
            | UntypedExpr::NegateInt { .. } => {
                break_("", " ").append(self.expr(expr).group()).nest(INDENT)
            }
        }
        .next_break_fits(NextBreakFitsMode::Disabled)
        .group()
    }

    fn assigned_value<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::Case { .. } => " ".to_doc().append(self.expr(expr)).group(),
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
            | UntypedExpr::NegateInt { .. } => self.case_clause_value(expr),
        }
    }

    fn clause<'a>(&mut self, clause: &'a UntypedClause, index: u32) -> Document<'a> {
        let space_before = self.pop_empty_lines(clause.location.start);
        let comments = self.pop_comments(clause.location.start);

        let clause_doc = match &clause.guard {
            None => self.alternative_patterns(clause),
            Some(guard) => self
                .alternative_patterns(clause)
                .append(break_("", " ").nest(INDENT))
                .append("if ")
                .append(self.clause_guard(guard).group().nest(INDENT)),
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
            break_("", " ")
        } else {
            " ".to_doc()
        };

        let clause_doc = clause_doc
            .append(arrow_break)
            .group()
            .append("->")
            .append(self.case_clause_value(&clause.then).group())
            .group();

        let clause_doc = match printed_comments(comments, false) {
            Some(comments) => comments.append(line()).append(clause_doc),
            None => clause_doc,
        };

        if index == 0 {
            clause_doc
        } else if space_before {
            lines(2).append(clause_doc)
        } else {
            line().append(clause_doc)
        }
    }

    fn alternative_patterns<'a>(&mut self, clause: &'a UntypedClause) -> Document<'a> {
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
            break_("", " ").nest(INDENT).append("| ")
        } else {
            break_("", " ").append("| ")
        };

        let alternative_patterns = std::iter::once(&clause.pattern)
            .chain(&clause.alternative_patterns)
            .enumerate()
            .map(|(alternative_index, p)| {
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
                let subject_docs = p.iter().enumerate().map(|(subject_index, subject)| {
                    // There's a small catch in turning each subject into a document.
                    // Sadly we can't simply call `self.pattern` on each subject and
                    // then nest each one in case it gets broken.
                    // The first ever pattern that appears in a case clause (that is
                    // the first subject of the first alternative) must not be nested
                    // further; otherwise, when broken, it would have 2 extra spaces
                    // of indentation: https://github.com/gleam-lang/gleam/issues/2940.
                    let is_first_subject = subject_index == 0;
                    let is_first_pattern_of_clause = is_first_subject && is_first_alternative;
                    let subject_doc = self.pattern(subject);
                    if is_first_pattern_of_clause {
                        subject_doc
                    } else {
                        subject_doc.nest(INDENT)
                    }
                });
                // We join all subjects with a breakable comma (that's also
                // going to be nested) and make the subjects into a group to
                // make sure the formatter tries to keep them on a single line.
                join(subject_docs, break_(",", ", ").nest(INDENT)).group()
            });
        // Last, we make sure that the formatter tries to keep each
        // alternative on a single line by making it a group!
        join(alternative_patterns, alternatives_separator).group()
    }

    fn list<'a>(
        &mut self,
        elements: &'a [UntypedExpr],
        tail: Option<&'a UntypedExpr>,
        location: &SrcSpan,
    ) -> Document<'a> {
        if elements.is_empty() {
            return match tail {
                Some(tail) => self.expr(tail),
                // We take all comments that come _before_ the end of the list,
                // that is all comments that are inside "[" and "]", if there's
                // any comment we want to put it inside the empty list!
                None => match printed_comments(self.pop_comments(location.end), false) {
                    None => "[]".to_doc(),
                    Some(comments) => "["
                        .to_doc()
                        .append(break_("", "").nest(INDENT))
                        .append(comments)
                        .append(break_("", ""))
                        .append("]")
                        // vvv We want to make sure the comments are on a separate
                        //     line from the opening and closing brackets so we
                        //     force the breaks to be split on newlines.
                        .force_break(),
                },
            };
        }

        let list_packing = self.items_sequence_packing(
            elements,
            tail,
            UntypedExpr::can_have_multiple_per_line,
            *location,
        );

        let comma = match list_packing {
            ItemsPacking::FitMultiplePerLine => flex_break(",", ", "),
            ItemsPacking::FitOnePerLine | ItemsPacking::BreakOnePerLine => break_(",", ", "),
        };

        let list_size = elements.len()
            + match tail {
                Some(_) => 1,
                None => 0,
            };

        let mut elements_doc = nil();
        for element in elements.iter() {
            let empty_lines = self.pop_empty_lines(element.location().start);
            let element_doc = self.comma_separated_item(element, list_size);

            elements_doc = if elements_doc.is_empty() {
                element_doc
            } else if empty_lines {
                // If there's empty lines before the list item we want to add an
                // empty line here. Notice how we're making sure no nesting is
                // added after the comma, otherwise we would be adding needless
                // whitespace in the empty line!
                docvec![
                    elements_doc,
                    comma.clone().set_nesting(0),
                    line(),
                    element_doc
                ]
            } else {
                docvec![elements_doc, comma.clone(), element_doc]
            };
        }
        elements_doc = elements_doc.next_break_fits(NextBreakFitsMode::Disabled);

        let doc = break_("[", "[").append(elements_doc);
        // We need to keep the last break aside and do not add it immediately
        // because in case there's a final comment before the closing square
        // bracket we want to add indentation (to just that break). Otherwise,
        // the final comment would be less indented than list's elements.
        let (doc, last_break) = match tail {
            None => (doc.nest(INDENT), break_(",", "")),

            Some(tail) => {
                let comments = self.pop_comments(tail.location().start);
                let tail = commented(docvec!["..", self.expr(tail)], comments);
                (
                    doc.append(break_(",", ", ")).append(tail).nest(INDENT),
                    break_("", ""),
                )
            }
        };

        // We get all remaining comments that come before the list's closing
        // square bracket.
        // If there's any we add those before the closing square bracket instead
        // of moving those out of the list.
        // Otherwise those would be moved out of the list.
        let comments = self.pop_comments(location.end);
        let doc = match printed_comments(comments, false) {
            None => doc.append(last_break).append("]"),
            Some(comment) => doc
                .append(last_break.nest(INDENT))
                // ^ See how here we're adding the missing indentation to the
                //   final break so that the final comment is as indented as the
                //   list's items.
                .append(comment)
                .append(line())
                .append("]")
                .force_break(),
        };

        match list_packing {
            ItemsPacking::FitOnePerLine | ItemsPacking::FitMultiplePerLine => doc.group(),
            ItemsPacking::BreakOnePerLine => doc.force_break(),
        }
    }

    fn items_sequence_packing<'a, T: HasLocation>(
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

    fn has_items_on_the_same_line<'a, L: HasLocation + 'a, T: Iterator<Item = &'a L>>(
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
    fn comma_separated_item<'a>(
        &mut self,
        expression: &'a UntypedExpr,
        siblings: usize,
    ) -> Document<'a> {
        // If there's more than one item in the comma separated list and there's a
        // pipeline or long binary chain, we want to indent those to make it
        // easier to tell where one item ends and the other starts.
        // Othewise we just print the expression as a normal expr.
        match expression {
            UntypedExpr::BinOp {
                name, left, right, ..
            } if siblings > 1 => {
                let comments = self.pop_comments(expression.start_byte_index());
                let doc = self.bin_op(name, left, right, true).group();
                commented(doc, comments)
            }
            UntypedExpr::PipeLine { expressions } if siblings > 1 => {
                let comments = self.pop_comments(expression.start_byte_index());
                let doc = self.pipeline(expressions, true).group();
                commented(doc, comments)
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
            | UntypedExpr::NegateInt { .. } => self.expr(expression).group(),
        }
    }

    fn pattern<'a>(&mut self, pattern: &'a UntypedPattern) -> Document<'a> {
        let comments = self.pop_comments(pattern.location().start);
        let doc = match pattern {
            Pattern::Int { value, .. } => self.int(value),

            Pattern::Float { value, .. } => self.float(value),

            Pattern::String { value, .. } => self.string(value),

            Pattern::Variable { name, .. } => name.to_doc(),

            Pattern::BitArraySize(size) => self.bit_array_size(size),

            Pattern::Assign { name, pattern, .. } => {
                if pattern.is_discard() {
                    name.to_doc()
                } else {
                    self.pattern(pattern).append(" as ").append(name.as_str())
                }
            }

            Pattern::Discard { name, .. } => name.to_doc(),

            Pattern::List { elements, tail, .. } => self.list_pattern(elements, tail),

            Pattern::Constructor {
                name,
                arguments,
                module,
                spread,
                location,
                ..
            } => self.pattern_constructor(name, arguments, module, *spread, location),

            Pattern::Tuple {
                elements, location, ..
            } => {
                let arguments = elements
                    .iter()
                    .map(|element| self.pattern(element))
                    .collect_vec();
                "#".to_doc()
                    .append(self.wrap_arguments(arguments, location.end))
                    .group()
            }

            Pattern::BitArray {
                segments, location, ..
            } => {
                let segment_docs = segments
                    .iter()
                    .map(|segment| bit_array_segment(segment, |pattern| self.pattern(pattern)))
                    .collect_vec();

                self.bit_array(segment_docs, ItemsPacking::FitOnePerLine, location)
            }

            Pattern::StringPrefix {
                left_side_string: left,
                right_side_assignment: right,
                left_side_assignment: left_assign,
                ..
            } => {
                let left = self.string(left);
                let right = match right {
                    AssignName::Variable(name) => name.to_doc(),
                    AssignName::Discard(name) => name.to_doc(),
                };
                match left_assign {
                    Some((name, _)) => docvec![left, " as ", name, " <> ", right],
                    None => docvec![left, " <> ", right],
                }
            }

            Pattern::Invalid { .. } => panic!("invalid patterns can not be in an untyped ast"),
        };
        commented(doc, comments)
    }

    fn bit_array_size<'a>(&mut self, size: &'a BitArraySize<()>) -> Document<'a> {
        match size {
            BitArraySize::Int { value, .. } => self.int(value),
            BitArraySize::Variable { name, .. } => name.to_doc(),
            BitArraySize::BinaryOperator {
                left,
                right,
                operator,
                ..
            } => {
                let operator = match operator {
                    IntOperator::Add => " + ",
                    IntOperator::Subtract => " - ",
                    IntOperator::Multiply => " * ",
                    IntOperator::Divide => " / ",
                    IntOperator::Remainder => " % ",
                };

                docvec![
                    self.bit_array_size(left),
                    operator,
                    self.bit_array_size(right)
                ]
            }
            BitArraySize::Block { inner, .. } => self.bit_array_size(inner).surround("{ ", " }"),
        }
    }

    fn list_pattern<'a>(
        &mut self,
        elements: &'a [UntypedPattern],
        tail: &'a Option<Box<UntypedTailPattern>>,
    ) -> Document<'a> {
        if elements.is_empty() {
            return match tail {
                Some(tail) => self.pattern(&tail.pattern),
                None => "[]".to_doc(),
            };
        }
        let elements = join(
            elements.iter().map(|element| self.pattern(element)),
            break_(",", ", "),
        );
        let doc = break_("[", "[").append(elements);
        match tail {
            None => doc.nest(INDENT).append(break_(",", "")),

            Some(tail) => {
                let tail = &tail.pattern;
                let comments = self.pop_comments(tail.location().start);
                let tail = if tail.is_discard() {
                    "..".to_doc()
                } else {
                    docvec!["..", self.pattern(tail)]
                };
                let tail = commented(tail, comments);
                doc.append(break_(",", ", "))
                    .append(tail)
                    .nest(INDENT)
                    .append(break_("", ""))
            }
        }
        .append("]")
        .group()
    }

    fn pattern_call_arg<'a>(&mut self, arg: &'a CallArg<UntypedPattern>) -> Document<'a> {
        self.format_call_arg(arg, pattern_call_arg_formatting, |this, value| {
            this.pattern(value)
        })
    }

    pub fn clause_guard_bin_op<'a>(
        &mut self,
        name: &'a BinOp,
        left: &'a UntypedClauseGuard,
        right: &'a UntypedClauseGuard,
    ) -> Document<'a> {
        self.clause_guard_bin_op_side(name, left, left.precedence())
            .append(break_("", " "))
            .append(name.to_doc())
            .append(" ")
            .append(self.clause_guard_bin_op_side(name, right, right.precedence() - 1))
    }

    fn clause_guard_bin_op_side<'a>(
        &mut self,
        name: &BinOp,
        side: &'a UntypedClauseGuard,
        // As opposed to `bin_op_side`, here we take the side precedence as an
        // argument instead of computing it ourselves. That's because
        // `clause_guard_bin_op` will reduce the precedence of any right side to
        // make sure the formatter doesn't remove any needed curly bracket.
        side_precedence: u8,
    ) -> Document<'a> {
        let side_doc = self.clause_guard(side);
        match side.bin_op_name() {
            // In case the other side is a binary operation as well and it can
            // be grouped together with the current binary operation, the two
            // docs are simply concatenated, so that they will end up in the
            // same group and the formatter will try to keep those on a single
            // line.
            Some(side_name) if side_name.can_be_grouped_with(name) => {
                self.operator_side(side_doc, name.precedence(), side_precedence)
            }
            // In case the binary operations cannot be grouped together the
            // other side is treated as a group on its own so that it can be
            // broken independently of other pieces of the binary operations
            // chain.
            _ => self.operator_side(side_doc.group(), name.precedence(), side_precedence),
        }
    }

    fn clause_guard<'a>(&mut self, clause_guard: &'a UntypedClauseGuard) -> Document<'a> {
        match clause_guard {
            ClauseGuard::And { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::And, left, right)
            }
            ClauseGuard::Or { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::Or, left, right)
            }
            ClauseGuard::Equals { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::Eq, left, right)
            }
            ClauseGuard::NotEquals { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::NotEq, left, right)
            }
            ClauseGuard::GtInt { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::GtInt, left, right)
            }
            ClauseGuard::GtEqInt { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::GtEqInt, left, right)
            }
            ClauseGuard::LtInt { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::LtInt, left, right)
            }
            ClauseGuard::LtEqInt { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::LtEqInt, left, right)
            }
            ClauseGuard::GtFloat { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::GtFloat, left, right)
            }
            ClauseGuard::GtEqFloat { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::GtEqFloat, left, right)
            }
            ClauseGuard::LtFloat { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::LtFloat, left, right)
            }
            ClauseGuard::LtEqFloat { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::LtEqFloat, left, right)
            }
            ClauseGuard::AddInt { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::AddInt, left, right)
            }
            ClauseGuard::AddFloat { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::AddFloat, left, right)
            }
            ClauseGuard::SubInt { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::SubInt, left, right)
            }
            ClauseGuard::SubFloat { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::SubFloat, left, right)
            }
            ClauseGuard::MultInt { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::MultInt, left, right)
            }
            ClauseGuard::MultFloat { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::MultFloat, left, right)
            }
            ClauseGuard::DivInt { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::DivInt, left, right)
            }
            ClauseGuard::DivFloat { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::DivFloat, left, right)
            }
            ClauseGuard::RemainderInt { left, right, .. } => {
                self.clause_guard_bin_op(&BinOp::RemainderInt, left, right)
            }

            ClauseGuard::Var { name, .. } => name.to_doc(),

            ClauseGuard::TupleIndex { tuple, index, .. } => {
                self.clause_guard(tuple).append(".").append(*index).to_doc()
            }

            ClauseGuard::FieldAccess {
                container, label, ..
            } => self
                .clause_guard(container)
                .append(".")
                .append(label)
                .to_doc(),

            ClauseGuard::ModuleSelect {
                module_name, label, ..
            } => module_name.to_doc().append(".").append(label).to_doc(),

            ClauseGuard::Constant(constant) => self.const_expr(constant),

            ClauseGuard::Not { expression, .. } => docvec!["!", self.clause_guard(expression)],

            ClauseGuard::Block { value, .. } => wrap_block(self.clause_guard(value)).group(),
        }
    }

    fn constant_call_arg<'a, A, B>(&mut self, arg: &'a CallArg<Constant<A, B>>) -> Document<'a> {
        self.format_call_arg(arg, constant_call_arg_formatting, |this, value| {
            this.const_expr(value)
        })
    }

    fn negate_bool<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::NegateBool { value, .. } => self.expr(value),
            UntypedExpr::BinOp { .. } => "!".to_doc().append(wrap_block(self.expr(expr))),
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
            | UntypedExpr::NegateInt { .. } => docvec!["!", self.expr(expr)],
        }
    }

    fn negate_int<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::NegateInt { value, .. } => self.expr(value),
            UntypedExpr::Int { value, .. } if value.starts_with('-') => self.int(&value[1..]),
            UntypedExpr::BinOp { .. } => "- ".to_doc().append(self.expr(expr)),

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
            | UntypedExpr::NegateBool { .. } => docvec!["-", self.expr(expr)],
        }
    }

    fn use_<'a>(&mut self, use_: &'a UntypedUse) -> Document<'a> {
        let comments = self.pop_comments(use_.location.start);

        let call = if use_.call.is_call() {
            docvec![" ", self.expr(&use_.call)]
        } else {
            docvec![break_("", " "), self.expr(&use_.call)].nest(INDENT)
        }
        .group();

        let doc = if use_.assignments.is_empty() {
            docvec!["use <-", call]
        } else {
            let assignments = use_.assignments.iter().map(|use_assignment| {
                let pattern = self.pattern(&use_assignment.pattern);
                let annotation = use_assignment
                    .annotation
                    .as_ref()
                    .map(|a| ": ".to_doc().append(self.type_ast(a)));

                pattern.append(annotation).group()
            });
            let assignments = Itertools::intersperse(assignments, break_(",", ", "));
            let left = ["use".to_doc(), break_("", " ")]
                .into_iter()
                .chain(assignments);
            let left = concat(left).nest(INDENT).append(break_("", " ")).group();
            docvec![left, "<-", call].group()
        };

        commented(doc, comments)
    }

    fn assert<'a>(&mut self, assert: &'a UntypedAssert) -> Document<'a> {
        let comments = self.pop_comments(assert.location.start);

        let expression = if assert.value.is_binop() || assert.value.is_pipeline() {
            self.expr(&assert.value).nest(INDENT)
        } else {
            self.expr(&assert.value)
        };

        let doc =
            self.append_as_message(expression, PrecedingAs::Expression, assert.message.as_ref());
        commented(docvec!["assert ", doc], comments)
    }

    fn bit_array<'a>(
        &mut self,
        segments: Vec<Document<'a>>,
        packing: ItemsPacking,
        location: &SrcSpan,
    ) -> Document<'a> {
        let comments = self.pop_comments(location.end);
        let comments_doc = printed_comments(comments, false);

        // Avoid adding illegal comma in empty bit array by explicitly handling it
        if segments.is_empty() {
            // We take all comments that come _before_ the end of the bit array,
            // that is all comments that are inside "<<" and ">>", if there's
            // any comment we want to put it inside the empty bit array!
            // Refer to the `list` function for a similar procedure.
            return match comments_doc {
                None => "<<>>".to_doc(),
                Some(comments) => "<<"
                    .to_doc()
                    .append(break_("", "").nest(INDENT))
                    .append(comments)
                    .append(break_("", ""))
                    .append(">>")
                    // vvv We want to make sure the comments are on a separate
                    //     line from the opening and closing angle brackets so
                    //     we force the breaks to be split on newlines.
                    .force_break(),
            };
        }

        let comma = match packing {
            ItemsPacking::FitMultiplePerLine => flex_break(",", ", "),
            ItemsPacking::FitOnePerLine | ItemsPacking::BreakOnePerLine => break_(",", ", "),
        };

        let last_break = break_(",", "");
        let doc = break_("<<", "<<")
            .append(join(segments, comma))
            .nest(INDENT);

        let doc = match comments_doc {
            None => doc.append(last_break).append(">>"),
            Some(comments) => doc
                .append(last_break.nest(INDENT))
                // ^ Notice how in this case we nest the final break before
                //   adding it: this way the comments are going to be as
                //   indented as the bit array items.
                .append(comments.nest(INDENT))
                .append(line())
                .append(">>")
                .force_break(),
        };

        match packing {
            ItemsPacking::FitOnePerLine | ItemsPacking::FitMultiplePerLine => doc.group(),
            ItemsPacking::BreakOnePerLine => doc.force_break(),
        }
    }

    fn bit_array_segment_expr<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::BinOp { .. } => wrap_block(self.expr(expr)),

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
            | UntypedExpr::Block { .. } => self.expr(expr),
        }
    }

    fn statement<'a>(&mut self, statement: &'a UntypedStatement) -> Document<'a> {
        match statement {
            Statement::Expression(expression) => self.expr(expression),
            Statement::Assignment(assignment) => self.assignment(assignment),
            Statement::Use(use_) => self.use_(use_),
            Statement::Assert(assert) => self.assert(assert),
        }
    }

    fn block<'a>(
        &mut self,
        location: &SrcSpan,
        statements: &'a Vec1<UntypedStatement>,
        force_breaks: bool,
    ) -> Document<'a> {
        let statements_doc =
            docvec![break_("", " "), self.statements(statements.as_vec())].nest(INDENT);
        let trailing_comments = self.pop_comments(location.end);
        let trailing_comments = printed_comments(trailing_comments, false);
        let block_doc = match trailing_comments {
            Some(trailing_comments_doc) => docvec![
                "{",
                statements_doc,
                line().nest(INDENT),
                trailing_comments_doc.nest(INDENT),
                line(),
                "}"
            ]
            .force_break(),
            None => docvec!["{", statements_doc, break_("", " "), "}"],
        };

        if force_breaks {
            block_doc.force_break().group()
        } else {
            block_doc.group()
        }
    }

    pub fn wrap_function_call_arguments<'a, I>(
        &mut self,
        arguments: I,
        location: &SrcSpan,
    ) -> Document<'a>
    where
        I: IntoIterator<Item = Document<'a>>,
    {
        let mut arguments = arguments.into_iter().peekable();
        if arguments.peek().is_none() {
            return "()".to_doc();
        }

        let arguments_doc = break_("", "")
            .append(join(arguments, break_(",", ", ")))
            .nest_if_broken(INDENT);

        // We get all remaining comments that come before the call's closing
        // parenthesis.
        // If there's any we add those before the closing parenthesis instead
        // of moving those out of the call.
        // Otherwise those would be moved out of the call.
        let comments = self.pop_comments(location.end);
        let closing_parens = match printed_comments(comments, false) {
            None => docvec![break_(",", ""), ")"],
            Some(comment) => {
                docvec![break_(",", "").nest(INDENT), comment, line(), ")"].force_break()
            }
        };

        "(".to_doc()
            .append(arguments_doc)
            .append(closing_parens)
            .group()
    }

    pub fn wrap_arguments<'a, I>(&mut self, arguments: I, comments_limit: u32) -> Document<'a>
    where
        I: IntoIterator<Item = Document<'a>>,
    {
        let mut arguments = arguments.into_iter().peekable();
        if arguments.peek().is_none() {
            let comments = self.pop_comments(comments_limit);
            return match printed_comments(comments, false) {
                Some(comments) => "("
                    .to_doc()
                    .append(break_("", ""))
                    .append(comments)
                    .nest_if_broken(INDENT)
                    .force_break()
                    .append(break_("", ""))
                    .append(")"),
                None => "()".to_doc(),
            };
        }
        let doc = break_("(", "(").append(join(arguments, break_(",", ", ")));

        // Include trailing comments if there are any
        let comments = self.pop_comments(comments_limit);
        match printed_comments(comments, false) {
            Some(comments) => doc
                .append(break_(",", ""))
                .append(comments)
                .nest_if_broken(INDENT)
                .force_break()
                .append(break_("", ""))
                .append(")"),
            None => doc
                .nest_if_broken(INDENT)
                .append(break_(",", ""))
                .append(")"),
        }
    }

    pub fn wrap_arguments_with_spread<'a, I>(
        &mut self,
        arguments: I,
        comments_limit: u32,
    ) -> Document<'a>
    where
        I: IntoIterator<Item = Document<'a>>,
    {
        let mut arguments = arguments.into_iter().peekable();
        if arguments.peek().is_none() {
            return self.wrap_arguments(arguments, comments_limit);
        }
        let doc = break_("(", "(")
            .append(join(arguments, break_(",", ", ")))
            .append(break_(",", ", "))
            .append("..");

        // Include trailing comments if there are any
        let comments = self.pop_comments(comments_limit);
        match printed_comments(comments, false) {
            Some(comments) => doc
                .append(break_(",", ""))
                .append(comments)
                .nest_if_broken(INDENT)
                .force_break()
                .append(break_("", ""))
                .append(")"),
            None => doc
                .nest_if_broken(INDENT)
                .append(break_(",", ""))
                .append(")"),
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
    fn printed_documented_comments<'a, 'b>(
        &mut self,
        comments: impl IntoIterator<Item = (u32, Option<&'b str>)>,
    ) -> Option<Document<'a>> {
        let mut comments = comments.into_iter().peekable();
        let _ = comments.peek()?;

        let mut doc = Vec::new();
        while let Some(c) = comments.next() {
            let (is_doc_commented, c) = match c {
                (comment_start, Some(c)) => {
                    let doc_comment = self.doc_comments(comment_start);
                    let is_doc_commented = !doc_comment.is_empty();
                    doc.push(doc_comment);
                    (is_doc_commented, c)
                }
                (_, None) => continue,
            };
            doc.push("//".to_doc().append(EcoString::from(c)));
            match comments.peek() {
                // Next line is a comment
                Some((_, Some(_))) => doc.push(line()),
                // Next line is empty
                Some((_, None)) => {
                    let _ = comments.next();
                    doc.push(lines(2));
                }
                // We've reached the end, there are no more lines
                None => {
                    if is_doc_commented {
                        doc.push(lines(2));
                    } else {
                        doc.push(line());
                    }
                }
            }
        }
        let doc = concat(doc);
        Some(doc.force_break())
    }

    fn append_as_message<'a>(
        &mut self,
        doc: Document<'a>,
        preceding_as: PrecedingAs,
        message: Option<&'a UntypedExpr>,
    ) -> Document<'a> {
        let Some(message) = message else { return doc };

        let comments = self.pop_comments(message.location().start);
        let comments = printed_comments(comments, false);

        let as_ = match preceding_as {
            PrecedingAs::Keyword => " as".to_doc(),
            PrecedingAs::Expression => docvec![break_("", " "), "as"].nest(INDENT),
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
                doc.group(),
                as_,
                docvec![line(), comments, line(), self.expr(message).group()].nest(INDENT)
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
                    (PrecedingAs::Keyword, UntypedExpr::Block { .. }) => self.expr(message).group(),
                    _ => self.expr(message).group().nest(INDENT),
                };
                docvec![doc.group(), as_, " ", message]
            }
        };

        doc.group()
    }

    fn echo<'a>(
        &mut self,
        expression: &'a Option<Box<UntypedExpr>>,
        message: &'a Option<Box<UntypedExpr>>,
    ) -> Document<'a> {
        let Some(expression) = expression else {
            return self.append_as_message(
                "echo".to_doc(),
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
        let doc = self.expr(expression);
        if expression.is_binop() || expression.is_pipeline() {
            let doc = self.append_as_message(
                doc.nest(INDENT),
                PrecedingAs::Expression,
                message.as_deref(),
            );
            docvec!["echo ", doc]
        } else {
            docvec![
                "echo ",
                self.append_as_message(doc, PrecedingAs::Expression, message.as_deref())
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

impl<'a> Documentable<'a> for &'a ArgNames {
    fn to_doc(self) -> Document<'a> {
        match self {
            ArgNames::Named { name, .. } | ArgNames::Discard { name, .. } => name.to_doc(),
            ArgNames::LabelledDiscard { label, name, .. }
            | ArgNames::NamedLabelled { label, name, .. } => {
                docvec![label, " ", name]
            }
        }
    }
}

fn pub_(publicity: Publicity) -> Document<'static> {
    match publicity {
        Publicity::Public | Publicity::Internal { .. } => "pub ".to_doc(),
        Publicity::Private => nil(),
    }
}

impl<'a> Documentable<'a> for &'a UnqualifiedImport {
    fn to_doc(self) -> Document<'a> {
        self.name.as_str().to_doc().append(match &self.as_name {
            None => nil(),
            Some(s) => " as ".to_doc().append(s.as_str()),
        })
    }
}

impl<'a> Documentable<'a> for &'a BinOp {
    fn to_doc(self) -> Document<'a> {
        match self {
            BinOp::And => "&&",
            BinOp::Or => "||",
            BinOp::LtInt => "<",
            BinOp::LtEqInt => "<=",
            BinOp::LtFloat => "<.",
            BinOp::LtEqFloat => "<=.",
            BinOp::Eq => "==",
            BinOp::NotEq => "!=",
            BinOp::GtEqInt => ">=",
            BinOp::GtInt => ">",
            BinOp::GtEqFloat => ">=.",
            BinOp::GtFloat => ">.",
            BinOp::AddInt => "+",
            BinOp::AddFloat => "+.",
            BinOp::SubInt => "-",
            BinOp::SubFloat => "-.",
            BinOp::MultInt => "*",
            BinOp::MultFloat => "*.",
            BinOp::DivInt => "/",
            BinOp::DivFloat => "/.",
            BinOp::RemainderInt => "%",
            BinOp::Concatenate => "<>",
        }
        .to_doc()
    }
}

#[allow(clippy::enum_variant_names)]
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

pub fn break_block(doc: Document<'_>) -> Document<'_> {
    "{".to_doc()
        .append(line().append(doc).nest(INDENT))
        .append(line())
        .append("}")
        .force_break()
}

pub fn wrap_block(doc: Document<'_>) -> Document<'_> {
    break_("{", "{ ")
        .append(doc)
        .nest(INDENT)
        .append(break_("", " "))
        .append("}")
}

fn printed_comments<'a, 'comments>(
    comments: impl IntoIterator<Item = Option<&'comments str>>,
    trailing_newline: bool,
) -> Option<Document<'a>> {
    let mut comments = comments.into_iter().peekable();
    let _ = comments.peek()?;

    let mut doc = Vec::new();
    while let Some(c) = comments.next() {
        let c = match c {
            Some(c) => c,
            None => continue,
        };
        doc.push("//".to_doc().append(EcoString::from(c)));
        match comments.peek() {
            // Next line is a comment
            Some(Some(_)) => doc.push(line()),
            // Next line is empty
            Some(None) => {
                let _ = comments.next();
                match comments.peek() {
                    Some(_) => doc.push(lines(2)),
                    None => {
                        if trailing_newline {
                            doc.push(lines(2));
                        }
                    }
                }
            }
            // We've reached the end, there are no more lines
            None => {
                if trailing_newline {
                    doc.push(line());
                }
            }
        }
    }
    let doc = concat(doc);
    if trailing_newline {
        Some(doc.force_break())
    } else {
        Some(doc)
    }
}

fn commented<'a, 'comments>(
    doc: Document<'a>,
    comments: impl IntoIterator<Item = Option<&'comments str>>,
) -> Document<'a> {
    match printed_comments(comments, true) {
        Some(comments) => comments.append(doc.group()),
        None => doc,
    }
}

fn bit_array_segment<Value, Type, ToDoc>(
    segment: &BitArraySegment<Value, Type>,
    mut to_doc: ToDoc,
) -> Document<'_>
where
    ToDoc: FnMut(&Value) -> Document<'_>,
{
    match segment {
        BitArraySegment { value, options, .. } if options.is_empty() => to_doc(value),

        BitArraySegment { value, options, .. } => to_doc(value).append(":").append(join(
            options
                .iter()
                .map(|option| segment_option(option, |value| to_doc(value))),
            "-".to_doc(),
        )),
    }
}

fn segment_option<ToDoc, Value>(option: &BitArrayOption<Value>, mut to_doc: ToDoc) -> Document<'_>
where
    ToDoc: FnMut(&Value) -> Document<'_>,
{
    match option {
        BitArrayOption::Bytes { .. } => "bytes".to_doc(),
        BitArrayOption::Bits { .. } => "bits".to_doc(),
        BitArrayOption::Int { .. } => "int".to_doc(),
        BitArrayOption::Float { .. } => "float".to_doc(),
        BitArrayOption::Utf8 { .. } => "utf8".to_doc(),
        BitArrayOption::Utf16 { .. } => "utf16".to_doc(),
        BitArrayOption::Utf32 { .. } => "utf32".to_doc(),
        BitArrayOption::Utf8Codepoint { .. } => "utf8_codepoint".to_doc(),
        BitArrayOption::Utf16Codepoint { .. } => "utf16_codepoint".to_doc(),
        BitArrayOption::Utf32Codepoint { .. } => "utf32_codepoint".to_doc(),
        BitArrayOption::Signed { .. } => "signed".to_doc(),
        BitArrayOption::Unsigned { .. } => "unsigned".to_doc(),
        BitArrayOption::Big { .. } => "big".to_doc(),
        BitArrayOption::Little { .. } => "little".to_doc(),
        BitArrayOption::Native { .. } => "native".to_doc(),

        BitArrayOption::Size {
            value,
            short_form: false,
            ..
        } => "size"
            .to_doc()
            .append("(")
            .append(to_doc(value))
            .append(")"),

        BitArrayOption::Size {
            value,
            short_form: true,
            ..
        } => to_doc(value),

        BitArrayOption::Unit { value, .. } => "unit"
            .to_doc()
            .append("(")
            .append(eco_format!("{value}"))
            .append(")"),
    }
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
        .position(|c| c.start > limit)
        .unwrap_or(comments.len());
    let end_empty_lines = empty_lines
        .iter()
        .position(|l| *l > limit)
        .unwrap_or(empty_lines.len());
    let popped_comments = comments
        .get(0..end_comments)
        .expect("0..end_comments is guaranteed to be in bounds")
        .iter()
        .map(|c| (c.start, Some(c.content)));
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

fn is_breakable_argument(expr: &UntypedExpr, arity: usize) -> bool {
    match expr {
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

fn expr_call_arg_formatting(arg: &CallArg<UntypedExpr>) -> CallArgFormatting<'_, UntypedExpr> {
    match arg {
        // An argument supplied using label shorthand syntax.
        _ if arg.uses_label_shorthand() => CallArgFormatting::ShorthandLabelled(
            arg.label.as_ref().expect("label shorthand with no label"),
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
    arg: &CallArg<UntypedPattern>,
) -> CallArgFormatting<'_, UntypedPattern> {
    match arg {
        // An argument supplied using label shorthand syntax.
        _ if arg.uses_label_shorthand() => CallArgFormatting::ShorthandLabelled(
            arg.label.as_ref().expect("label shorthand with no label"),
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

fn constant_call_arg_formatting<A, B>(
    arg: &CallArg<Constant<A, B>>,
) -> CallArgFormatting<'_, Constant<A, B>> {
    match arg {
        // An argument supplied using label shorthand syntax.
        _ if arg.uses_label_shorthand() => CallArgFormatting::ShorthandLabelled(
            arg.label.as_ref().expect("label shorthand with no label"),
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

impl<'a> Documentable<'a> for AttributesPrinter<'a> {
    fn to_doc(self) -> Document<'a> {
        let mut attributes = vec![];

        // @deprecated attribute
        if let Deprecation::Deprecated { message } = self.deprecation {
            attributes.push(docvec!["@deprecated(\"", message, "\")"])
        };

        // @external attributes
        if let Some((m, f, _)) = self.external_erlang {
            attributes.push(docvec!["@external(erlang, \"", m, "\", \"", f, "\")"])
        };

        if let Some((m, f, _)) = self.external_javascript {
            attributes.push(docvec!["@external(javascript, \"", m, "\", \"", f, "\")"])
        };

        // @internal attribute
        if self.internal {
            attributes.push("@internal".to_doc());
        };

        if attributes.is_empty() {
            nil()
        } else {
            join(attributes, line()).append(line())
        }
    }
}
