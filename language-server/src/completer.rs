use std::{collections::HashMap, sync::Arc};

use ecow::{EcoString, eco_format};
use itertools::Itertools;
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionTextEdit,
    Documentation, MarkupContent, MarkupKind, Position, Range, TextDocumentPositionParams,
    TextEdit,
};
use strum::IntoEnumIterator;
use vec1::Vec1;

use gleam_core::{
    Result,
    ast::{
        self, Arg, CallArg, Function, FunctionLiteralKind, Pattern, Publicity, TypedExpr,
        visit::Visit,
    },
    build::{Module, Origin},
    line_numbers::LineNumbers,
    type_::{
        self, FieldMap, ModuleInterface, PRELUDE_MODULE_NAME, PreludeType, RecordAccessor, Type,
        TypeConstructor, ValueConstructorVariant, collapse_links, error::VariableOrigin,
        pretty::Printer,
    },
};

use super::{
    compiler::LspProjectCompiler,
    edits::{
        Newlines, add_newlines_after_import, get_import_edit,
        position_of_first_definition_if_import,
    },
    files::FileSystemProxy,
};

// Represents the kind/specificity of completion that is being requested.
#[derive(Copy, Clone)]
enum CompletionKind {
    // A language keyword that we can offer completions for, like `todo`,
    // `panic`, or `echo`
    Keyword,
    // A label for a function or type definition
    Label,
    // A field of a record
    FieldAccessor,
    // Values or types defined in the current module
    LocallyDefined,
    // Values or types defined in an already imported module
    ImportedModule,
    // Types or values defined in the prelude
    Prelude,
    // Types defined in a module that has not been imported
    ImportableModule,
}

#[derive(Copy, Clone)]
enum TypeMatch {
    Matching,
    Incompatible,
    Unknown,
}

// Gives the sort text for a completion item based on the kind and label.
// This ensures that more specific kinds of completions are placed before
// less specific ones..
fn sort_text(kind: CompletionKind, label: &str, type_match: TypeMatch) -> String {
    let priority: u8 = match kind {
        CompletionKind::Keyword => 0,
        CompletionKind::Label => 1,
        CompletionKind::FieldAccessor => 2,
        CompletionKind::LocallyDefined => 3,
        CompletionKind::ImportedModule => 4,
        CompletionKind::Prelude => 5,
        CompletionKind::ImportableModule => 6,
    };
    match type_match {
        // We want to prioritise type which match what is expected in the completion
        // as those are more likely to be what the user wants.
        TypeMatch::Matching => format!("0{priority}_{label}"),
        TypeMatch::Incompatible | TypeMatch::Unknown => format!("{priority}_{label}"),
    }
}

/// The form in which a type completion is needed in context.
#[derive(Debug)]
enum TypeCompletionContext {
    /// The type completion is for an unqualified import that doesn't have an
    /// import list yet. So adding the type will also require adding braces:
    ///
    /// ```gleam
    /// import wibble.Wib|
    /// //           ^^^^^ We're typing this...
    /// import wibble.{type Wibble}
    /// //           ^^^^^^^^^^^^^^ ...so this will be the completion
    /// ```
    ///
    UnqualifiedImport,

    /// The type completion is for an unqualified import within already existing
    /// braces.
    ///
    /// ```gleam
    /// import wibble.{type AlreadyImported, Wibb|}
    /// //                                   ^^^^^ We're typing this...
    /// import wibble.{type AlreadyImported, type Wibble}
    /// //                                   ^^^^^^^^^^^ ...so this will be the completion
    /// ```
    ///
    UnqualifiedImportWithinBraces,

    /// The type completion is for an unqualified type (for example something
    /// coming from the prelude, or a type that was already imported in a
    /// unqualified way).
    ///
    /// ```gleam
    /// import wobble.{type Wobble}
    ///
    /// pub fn new_wobble() -> Wob|
    /// //                     ^^^^ We're typing this...
    /// pub fn new_wobble() -> Wobble
    /// //                     ^^^^^^ ... so this will be the completion
    /// ```
    ///
    UnqualifiedType,

    /// The type completion is for a qualified type.
    ///
    /// ```gleam
    /// import wobble
    ///
    /// pub fn new_wobble() -> wobble.W|
    /// //                     ^^^^^^^^ We're typing this...
    /// pub fn new_wobble() -> wobble.Wobble
    /// //                     ^^^^^^^^^^^^^ ...so this will be the completion
    /// ```
    ///
    QualifiedType,
}

/// Represents the surroundings of the cursor when trying to figure out a
/// completion.
///
/// ```gleam
/// wibble.w|ob
/// //      ^ cursor here
/// ```
struct CursorSurroundings {
    /// The text surrounding the cursor. For example:
    ///
    /// ```gleam
    /// wibble.w|ob
    /// //      ^ cursor here
    /// // The surrounding text is: "wibble.wob"
    /// ```
    ///
    surrounding_text: EcoString,
    surrounding_text_range: Range,

    /// The text that comes immediately before the cursor.
    /// For example:
    ///
    /// ```gleam
    ///    wibble.w|ob
    /// //         ^ cursor here
    /// // The text before is: "wibble.w"
    /// ```
    ///
    text_before_cursor: EcoString,

    /// The range of the text that comes immediately before the cursor.
    /// For example:
    ///
    /// ```gleam
    ///    wibble.w|ob
    /// //         ^ cursor here
    /// // ^^^^^^^^ This is the range
    /// ```
    ///
    /// This is what is usually replaced with a completion.
    ///
    text_before_cursor_range: Range,

    /// The text that comes immediately after the cursor.
    /// For example:
    ///
    /// ```gleam
    ///    wibble.w|ob
    /// //         ^ cursor here
    /// // The text before is: "ob"
    /// ```
    ///
    text_after_cursor: EcoString,
}

impl CursorSurroundings {
    fn selected_module(&self) -> Option<EcoString> {
        self.surrounding_text
            .split_once('.')
            .map(|(selected_module, _)| EcoString::from(selected_module))
    }

    /// Given a proposed completion, this returns the text edit to obtain that
    /// completion.
    ///
    /// > This could also return `None` as sometimes no further edit is actually
    /// > needed!
    ///
    fn to_text_edit(&self, new_text: String) -> Option<CompletionTextEdit> {
        // We need to check if the new text we're adding could actually be
        // a simple addition in the middle of something that is already being
        // typed.
        // Say we're editing our code to actually make the `Json` type
        // qualified:
        //
        // ```gleam
        // jso|Json
        //    ^ cursor here
        // ```
        //
        // Halfway through writing the module name we might just want to accept
        // the completion for: `json.Json`.
        // What one would normally expect is for the final code to be:
        //
        // ```gleam
        // json.Json|
        // // after accepting completion
        //
        // // and not something like this!
        // // json.JsonJson
        // ```
        //
        // This only makes sense if the completion we're adding has a prefix and
        // suffix in common with the surrounding text:
        let remaining_label = new_text
            .strip_prefix(self.text_before_cursor.as_str())
            .and_then(|rest| rest.strip_suffix(self.text_after_cursor.as_str()));

        match remaining_label {
            // The entire existing text is already the same as the new text we
            // might want to add.
            // In that case there's no need to perform any edit at all!
            Some("") => None,

            // This is one of the cases (like the one in the example above) were
            // the label is a more complete version of the text surrounding the
            // cursor. So we replace the entire range.
            Some(_) => Some(CompletionTextEdit::Edit(TextEdit {
                range: self.surrounding_text_range,
                new_text,
            })),
            // In all other cases the completion is never meant to replace text
            // that comes after the cursor.
            // We only replace what comes before it with the new text.
            None => Some(CompletionTextEdit::Edit(TextEdit {
                range: self.text_before_cursor_range,
                new_text,
            })),
        }
    }
}

pub struct Completer<'a, IO> {
    /// The direct buffer source code
    src: &'a EcoString,
    /// The line number information of the buffer source code
    pub src_line_numbers: LineNumbers,
    /// The current cursor position within the buffer source code
    cursor_position: &'a Position,
    /// A reference to the lsp compiler for getting module information
    compiler: &'a LspProjectCompiler<FileSystemProxy<IO>>,
    /// A reference to the current module the completion is for
    module: &'a Module,
    /// The line number information of the latest compiled module.
    /// This is not necessarily the same as src_line_numbers if the module
    /// is in a non-compiling state
    pub module_line_numbers: LineNumbers,

    /// The expected type of the value we are completing. `None` if we are
    /// completing a type annotation or label, where this information is not
    /// applicable.
    pub expected_type: Option<Arc<Type>>,
}

impl<'a, IO> Completer<'a, IO> {
    pub fn new(
        src: &'a EcoString,
        params: &'a TextDocumentPositionParams,
        compiler: &'a LspProjectCompiler<FileSystemProxy<IO>>,
        module: &'a Module,
    ) -> Self {
        Completer {
            src,
            src_line_numbers: LineNumbers::new(src.as_str()),
            cursor_position: &params.position,
            compiler,
            module,
            module_line_numbers: LineNumbers::new(&module.code),
            expected_type: None,
        }
    }

    // Gets the current range around the cursor to place a completion
    // and the phrase surrounding the cursor to use for completion.
    // This method takes in a helper to determine what qualifies as
    // a phrase depending on context.
    fn get_phrase_surrounding_for_completion(
        &'a self,
        valid_phrase_char: &impl Fn(char) -> bool,
    ) -> CursorSurroundings {
        let cursor = self.src_line_numbers.byte_index(*self.cursor_position);

        // Get part of phrase prior to cursor
        let before = self
            .src
            .get(..cursor as usize)
            .and_then(|line| line.rsplit_once(valid_phrase_char).map(|r| r.1))
            .unwrap_or("");

        // Get part of phrase following cursor
        let after = self
            .src
            .get(cursor as usize..)
            .and_then(|line| line.split_once(valid_phrase_char).map(|r| r.0))
            .unwrap_or("");

        let text_before_cursor_range = Range {
            start: Position {
                line: self.cursor_position.line,
                character: self.cursor_position.character - before.len() as u32,
            },
            end: Position {
                line: self.cursor_position.line,
                character: self.cursor_position.character,
            },
        };

        let surrounding_text_range = Range {
            start: text_before_cursor_range.start,
            end: Position {
                line: self.cursor_position.line,
                character: self.cursor_position.character + after.len() as u32,
            },
        };

        CursorSurroundings {
            surrounding_text: eco_format!("{before}{after}"),
            surrounding_text_range,
            text_before_cursor: EcoString::from(before),
            text_before_cursor_range,
            text_after_cursor: EcoString::from(after),
        }
    }

    // Gets the current range around the cursor to place a completion
    // and any part of the phrase preceeding a dot if a module is being selected from.
    // A continuous phrase in this case is a name or typename that may have a dot in it.
    // This is used to match the exact location to fill in the completion.
    fn get_phrase_surrounding_completion(&'a self) -> CursorSurroundings {
        self.get_phrase_surrounding_for_completion(&|c: char| {
            // Checks if a character is not a valid name/upname character or a dot.
            !c.is_ascii_alphanumeric() && c != '.' && c != '_'
        })
    }

    // Gets the current range around the cursor to place a completion.
    // For unqualified imports we special case the word being completed to allow for whitespace but not dots.
    // This is to allow `type MyType` to be treated as 1 "phrase" for the sake of completion.
    fn get_phrase_surrounding_completion_for_import(&'a self) -> CursorSurroundings {
        self.get_phrase_surrounding_for_completion(&|c: char| {
            // Checks if a character is not a valid name/upname character or whitespace.
            // The newline character is not included as well.
            !c.is_ascii_alphanumeric() && c != '_' && c != ' ' && c != '\t'
        })
    }

    /// Checks if the line being editted is an import line and provides completions if it is.
    /// If the line includes a dot then it provides unqualified import completions.
    /// Otherwise it provides direct module import completions.
    pub fn import_completions(&'a self) -> Option<Result<Option<Vec<CompletionItem>>>> {
        let start_of_line = self.src_line_numbers.byte_index(Position {
            line: self.cursor_position.line,
            character: 0,
        });
        let end_of_line = self.src_line_numbers.byte_index(Position {
            line: self.cursor_position.line + 1,
            character: 0,
        });

        // Drop all lines except the line the cursor is on
        let src = self.src.get(start_of_line as usize..end_of_line as usize)?;

        // If this isn't an import line then we don't offer import completions
        if !src.trim_start().starts_with("import") {
            return None;
        }

        // Check if we are completing an unqualified import
        if let Some(dot_index) = src.find('.') {
            // Find the module that is being imported from
            let importing_module_name = src.get(6..dot_index)?.trim();
            let importing_module: &ModuleInterface =
                self.compiler.get_module_interface(importing_module_name)?;
            let within_braces = match src.get(dot_index + 1..) {
                Some(x) => x.trim_start().starts_with('{'),
                None => false,
            };

            Some(Ok(Some(self.unqualified_completions_from_module(
                importing_module,
                within_braces,
            ))))
        } else {
            // Find where to start and end the import completion
            let start = self.src_line_numbers.line_and_column_number(start_of_line);
            let end = self
                .src_line_numbers
                .line_and_column_number(end_of_line - 1);
            let start = Position::new(start.line - 1, start.column + 6);
            let end = Position::new(end.line - 1, end.column - 1);
            let completions = self.complete_modules_for_import(start, end);

            Some(Ok(Some(completions)))
        }
    }

    /// Gets the completes for unqualified imports from a module.
    pub fn unqualified_completions_from_module(
        &'a self,
        module_being_imported_from: &'a ModuleInterface,
        within_braces: bool,
    ) -> Vec<CompletionItem> {
        let cursor_surroundings = self.get_phrase_surrounding_completion_for_import();
        let mut completions = vec![];

        // Find values and type that have already previously been imported
        let mut already_imported_types = std::collections::HashSet::new();
        let mut already_imported_values = std::collections::HashSet::new();

        // Search the ast for import statements
        for import in &self.module.ast.definitions.imports {
            // Find the import that matches the module being imported from
            if import.module == module_being_imported_from.name {
                // Add the values and types that have already been imported
                for unqualified in &import.unqualified_types {
                    let _ = already_imported_types.insert(&unqualified.name);
                }

                for unqualified in &import.unqualified_values {
                    let _ = already_imported_values.insert(&unqualified.name);
                }
            }
        }

        // Get completable types
        for (name, type_) in &module_being_imported_from.types {
            // Skip types that should not be suggested
            if !self.is_suggestable_import(
                &type_.publicity,
                module_being_imported_from.package.as_str(),
            ) {
                continue;
            }

            // Skip type that are already imported
            if already_imported_types.contains(name) {
                continue;
            }

            completions.push(type_completion(
                None,
                name,
                type_,
                &cursor_surroundings,
                if within_braces {
                    TypeCompletionContext::UnqualifiedImportWithinBraces
                } else {
                    TypeCompletionContext::UnqualifiedImport
                },
                CompletionKind::ImportedModule,
            ));
        }

        // Get completable values
        for (name, value) in &module_being_imported_from.values {
            // Skip values that should not be suggested
            if !self.is_suggestable_import(
                &value.publicity,
                module_being_imported_from.package.as_str(),
            ) {
                continue;
            }

            // Skip values that are already imported
            if already_imported_values.contains(name) {
                continue;
            }
            completions.push(self.value_completion(
                None,
                &module_being_imported_from.name,
                name,
                value,
                &cursor_surroundings,
                CompletionKind::ImportedModule,
            ));
        }

        completions
    }

    // Get all the modules that can be imported that have not already been imported.
    fn completable_modules_for_import(&self) -> Vec<(&EcoString, &ModuleInterface)> {
        let mut direct_dep_packages: std::collections::HashSet<&EcoString> =
            std::collections::HashSet::from_iter(
                self.compiler.project_compiler.config.dependencies.keys(),
            );
        if !self.module.origin.is_src() {
            // In tests we can import direct dev dependencies
            direct_dep_packages.extend(
                self.compiler
                    .project_compiler
                    .config
                    .dev_dependencies
                    .keys(),
            )
        }

        let already_imported: std::collections::HashSet<EcoString> =
            std::collections::HashSet::from_iter(
                self.module.dependencies.iter().map(|d| d.0.clone()),
            );
        self.compiler
            .project_compiler
            .get_importable_modules()
            .iter()
            //
            // You cannot import yourself
            .filter(|(name, _)| *name != &self.module.name)
            //
            // Different origin directories will get different import completions
            .filter(|(_, module)| match self.module.origin {
                // src/ can import from src/
                Origin::Src => module.origin.is_src(),
                // dev/ can import from src/ or dev/
                Origin::Dev => !module.origin.is_test(),
                // Test can import from anywhere
                Origin::Test => true,
            })
            //
            // It is possible to import internal modules from other packages,
            // but it's not recommended so we don't include them in completions
            .filter(|(_, module)| module.package == self.root_package_name() || !module.is_internal)
            //
            // You cannot import a module twice
            .filter(|(name, _)| !already_imported.contains(*name))
            //
            // It is possible to import modules from dependencies of dependencies
            // but it's not recommended so we don't include them in completions
            .filter(|(_, module)| {
                let is_root_or_prelude =
                    module.package == self.root_package_name() || module.package.is_empty();
                is_root_or_prelude || direct_dep_packages.contains(&module.package)
            })
            .collect()
    }

    // Get all the completions for modules that can be imported
    fn complete_modules_for_import(
        &'a self,
        start: Position,
        end: Position,
    ) -> Vec<CompletionItem> {
        self.completable_modules_for_import()
            .iter()
            .map(|(name, _)| CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::MODULE),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range { start, end },
                    new_text: name.to_string(),
                })),
                ..Default::default()
            })
            .collect()
    }

    // NOTE: completion_types and completion_values are really similar
    // but just different enough that an abstraction would
    // be really hard to understand or use a lot of trait magic.
    // For now I've left it as is but might be worth revisiting.

    /// Provides completions for when the context being editted is a type.
    pub fn completion_types(&'a self) -> Vec<CompletionItem> {
        let cursor_surroundings = self.get_phrase_surrounding_completion();
        let selected_module = cursor_surroundings.selected_module();
        let mut completions = vec![];

        // Module and prelude types
        // Do not complete direct module types if the user has already started
        // typing a module select. That is, when the user has already typed
        // `mymodule.|` we know local module types and prelude types are no
        // longer relevant and needed for completions.
        if selected_module.is_none() {
            for (name, type_) in &self.module.ast.type_info.types {
                completions.push(type_completion(
                    None,
                    name,
                    type_,
                    &cursor_surroundings,
                    TypeCompletionContext::UnqualifiedType,
                    CompletionKind::LocallyDefined,
                ));
            }

            for type_ in PreludeType::iter() {
                let label: String = type_.name().into();
                let sort_text = Some(sort_text(
                    CompletionKind::Prelude,
                    &label,
                    TypeMatch::Unknown,
                ));
                completions.push(CompletionItem {
                    label,
                    detail: Some("Type".into()),
                    kind: Some(CompletionItemKind::CLASS),
                    sort_text,
                    ..Default::default()
                });
            }
        }

        // Qualified types
        for import in &self.module.ast.definitions.imports {
            // The module may not be known of yet if it has not previously
            // compiled yet in this editor session.
            let Some(module) = self.compiler.get_module_interface(&import.module) else {
                continue;
            };
            let Some(module_name) = &import.used_name() else {
                continue;
            };

            for (name, type_) in &module.types {
                if !self.is_suggestable_import(&type_.publicity, module.package.as_str()) {
                    continue;
                }

                // If the user has already started typing a module select
                // then don't show irrelevant modules.
                // For example: when the user has typed `mymodule.|` we
                // should only show items from `mymodule`.
                if let Some(typed_module) = &selected_module
                    && module_name != typed_module
                {
                    continue;
                }

                completions.push(type_completion(
                    Some(module_name),
                    name,
                    type_,
                    &cursor_surroundings,
                    TypeCompletionContext::QualifiedType,
                    CompletionKind::ImportedModule,
                ));
            }

            // Unqualified types
            // Do not complete unqualified types if the user has already started
            // typing a module select.
            // For example, when the user has already typed `mymodule.|` we know
            // unqualified module types are no longer relevant.
            if selected_module.is_none() {
                for unqualified in &import.unqualified_types {
                    if let Some(type_) = module.get_public_type(&unqualified.name) {
                        completions.push(type_completion(
                            None,
                            unqualified.used_name(),
                            type_,
                            &cursor_surroundings,
                            TypeCompletionContext::UnqualifiedType,
                            CompletionKind::ImportedModule,
                        ))
                    }
                }
            }
        }

        // Importable modules
        let first_import_pos =
            position_of_first_definition_if_import(self.module, &self.src_line_numbers);
        let first_is_import = first_import_pos.is_some();
        let import_location = first_import_pos.unwrap_or_default();

        let after_import_newlines = add_newlines_after_import(
            import_location,
            first_is_import,
            &self.src_line_numbers,
            self.src,
        );
        for (module_full_name, module) in self.completable_modules_for_import() {
            // Do not try to import the prelude.
            if module_full_name == "gleam" {
                continue;
            }

            let qualifier = module_full_name
                .split('/')
                .next_back()
                .unwrap_or(module_full_name);

            // If the user has already started a module select then don't show irrelevant modules.
            // e.x. when the user has typed mymodule.| we should only show items from mymodule.
            if let Some(selected_module) = &selected_module
                && qualifier != selected_module
            {
                continue;
            }

            // Qualified types
            for (name, type_) in &module.types {
                if !self.is_suggestable_import(&type_.publicity, module.package.as_str()) {
                    continue;
                }

                let mut completion = type_completion(
                    Some(qualifier),
                    name,
                    type_,
                    &cursor_surroundings,
                    TypeCompletionContext::QualifiedType,
                    CompletionKind::ImportableModule,
                );
                add_import_to_completion(
                    &mut completion,
                    import_location,
                    module_full_name,
                    &after_import_newlines,
                );
                completions.push(completion);
            }
        }

        completions
    }

    /// Provides completions for when the context being editted is a value.
    pub fn completion_values(&'a self) -> Vec<CompletionItem> {
        let cursor_surroundings = self.get_phrase_surrounding_completion();
        let selected_module = cursor_surroundings.selected_module();
        let mut completions = vec![];
        let mod_name = self.module.name.as_str();
        let cursor = self.src_line_numbers.byte_index(*self.cursor_position);

        // Keyword completions
        if !cursor_surroundings.surrounding_text.is_empty() {
            for keyword in ["panic", "todo", "echo"] {
                if keyword.starts_with(cursor_surroundings.surrounding_text.as_str()) {
                    completions.push(self.keyword_completion(keyword, &cursor_surroundings))
                }
            }
        }

        // Module and prelude values
        // Do not complete direct module values if the user has already started typing a module select.
        // e.x. when the user has typed mymodule.| we know local module and prelude values are no longer
        // relevant.
        if selected_module.is_none() {
            // Find the function that the cursor is in and push completions for
            // its arguments and local variables.
            if let Some(function) = self
                .module
                .ast
                .definitions
                .functions
                .iter()
                .filter(|function| function.full_location().contains(cursor))
                .peekable()
                .peek()
            {
                completions.extend(
                    LocalCompletion::new(
                        mod_name,
                        cursor_surroundings.surrounding_text_range,
                        cursor,
                        self.expected_type.clone(),
                    )
                    .fn_completions(function),
                );
            }

            for (name, value) in &self.module.ast.type_info.values {
                // Here we do not check for the internal attribute: we always want
                // to show autocompletions for values defined in the same module,
                // even if those are internal.
                completions.push(self.value_completion(
                    None,
                    mod_name,
                    name,
                    value,
                    &cursor_surroundings,
                    CompletionKind::LocallyDefined,
                ));
            }

            let mut push_prelude_completion = |label: &str, kind, type_: Arc<Type>| {
                let label = label.to_string();
                let sort_text = Some(sort_text(
                    CompletionKind::Prelude,
                    &label,
                    match_type(&self.expected_type, &type_),
                ));
                completions.push(CompletionItem {
                    label,
                    detail: Some(PRELUDE_MODULE_NAME.into()),
                    kind: Some(kind),
                    sort_text,
                    ..Default::default()
                });
            };

            for type_ in PreludeType::iter() {
                match type_ {
                    PreludeType::Bool => {
                        push_prelude_completion(
                            "True",
                            CompletionItemKind::ENUM_MEMBER,
                            type_::bool(),
                        );
                        push_prelude_completion(
                            "False",
                            CompletionItemKind::ENUM_MEMBER,
                            type_::bool(),
                        );
                    }
                    PreludeType::Nil => {
                        push_prelude_completion(
                            "Nil",
                            CompletionItemKind::ENUM_MEMBER,
                            type_::nil(),
                        );
                    }
                    PreludeType::Result => {
                        push_prelude_completion(
                            "Ok",
                            CompletionItemKind::CONSTRUCTOR,
                            type_::result(type_::unbound_var(0), type_::unbound_var(0)),
                        );
                        push_prelude_completion(
                            "Error",
                            CompletionItemKind::CONSTRUCTOR,
                            type_::result(type_::unbound_var(0), type_::unbound_var(0)),
                        );
                    }
                    PreludeType::BitArray
                    | PreludeType::Float
                    | PreludeType::Int
                    | PreludeType::List
                    | PreludeType::String
                    | PreludeType::UtfCodepoint => {}
                }
            }
        }

        // Imported modules
        for import in &self.module.ast.definitions.imports {
            // The module may not be known of yet if it has not previously
            // compiled yet in this editor session.
            let Some(module) = self.compiler.get_module_interface(&import.module) else {
                continue;
            };

            // Qualified values
            for (name, value) in &module.values {
                if !self.is_suggestable_import(&value.publicity, module.package.as_str()) {
                    continue;
                }

                if let Some(module) = import.used_name() {
                    // If the user has already started a module select then don't show irrelevant modules.
                    // e.x. when the user has typed mymodule.| we should only show items from mymodule.
                    if let Some(input_mod_name) = &selected_module
                        && &module != input_mod_name
                    {
                        continue;
                    }
                    completions.push(self.value_completion(
                        Some(&module),
                        mod_name,
                        name,
                        value,
                        &cursor_surroundings,
                        CompletionKind::ImportedModule,
                    ));
                }
            }

            // Unqualified values
            // Do not complete unqualified values if the user has already started typing a module select.
            // e.x. when the user has typed mymodule.| we know unqualified module values are no longer relevant.
            if selected_module.is_none() {
                for unqualified in &import.unqualified_values {
                    if let Some(value) = module.get_public_value(&unqualified.name) {
                        let name = unqualified.used_name();
                        completions.push(self.value_completion(
                            None,
                            mod_name,
                            name,
                            value,
                            &cursor_surroundings,
                            CompletionKind::ImportedModule,
                        ))
                    }
                }
            }
        }

        // Importable modules
        let first_import_pos =
            position_of_first_definition_if_import(self.module, &self.src_line_numbers);
        let first_is_import = first_import_pos.is_some();
        let import_location = first_import_pos.unwrap_or_default();
        let after_import_newlines = add_newlines_after_import(
            import_location,
            first_is_import,
            &self.src_line_numbers,
            self.src,
        );
        for (module_full_name, module) in self.completable_modules_for_import() {
            // Do not try to import the prelude.
            if module_full_name == "gleam" {
                continue;
            }
            let qualifier = module_full_name
                .split('/')
                .next_back()
                .unwrap_or(module_full_name);

            // If the user has already started a module select then don't show irrelevant modules.
            // e.x. when the user has typed mymodule.| we should only show items from mymodule.
            if let Some(selected_module) = &selected_module
                && qualifier != selected_module
            {
                continue;
            }

            // Qualified values
            for (name, value) in &module.values {
                if !self.is_suggestable_import(&value.publicity, module.package.as_str()) {
                    continue;
                }

                let mut completion = self.value_completion(
                    Some(qualifier),
                    module_full_name,
                    name,
                    value,
                    &cursor_surroundings,
                    CompletionKind::ImportableModule,
                );

                add_import_to_completion(
                    &mut completion,
                    import_location,
                    module_full_name,
                    &after_import_newlines,
                );
                completions.push(completion);
            }
        }

        completions
    }

    // Looks up the type accessors for the given type
    fn type_accessors_from_modules(
        &'a self,
        importable_modules: &'a im::HashMap<EcoString, ModuleInterface>,
        type_: Arc<Type>,
    ) -> Option<&'a HashMap<EcoString, RecordAccessor>> {
        let type_ = collapse_links(type_);
        match type_.as_ref() {
            Type::Named {
                name,
                module,
                inferred_variant,
                ..
            } => importable_modules
                .get(module)
                .and_then(|i| i.accessors.get(name))
                .filter(|a| a.publicity.is_importable() || module == &self.module.name)
                .map(|a| a.accessors_for_variant(*inferred_variant)),

            Type::Fn { .. } | Type::Var { .. } | Type::Tuple { .. } => None,
        }
    }

    /// Provides completions for field accessors when the context being editted
    /// is a custom type instance
    pub fn completion_field_accessors(&'a self, type_: Arc<Type>) -> Vec<CompletionItem> {
        self.type_accessors_from_modules(
            self.compiler.project_compiler.get_importable_modules(),
            type_,
        )
        .map(|accessors| {
            accessors
                .values()
                .map(|accessor| self.field_completion(&accessor.label, accessor.type_.clone()))
                .collect_vec()
        })
        .unwrap_or_default()
    }

    fn callable_field_map(
        &'a self,
        expr: &'a TypedExpr,
        importable_modules: &'a im::HashMap<EcoString, ModuleInterface>,
    ) -> Option<&'a FieldMap> {
        match expr {
            TypedExpr::Var { constructor, .. } => constructor.field_map(),
            TypedExpr::ModuleSelect {
                module_name, label, ..
            } => importable_modules
                .get(module_name)
                .and_then(|i| i.values.get(label))
                .and_then(|a| a.field_map()),
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => None,
        }
    }

    /// Provides completions for labels when the context being editted is a call
    /// that has labelled arguments that can be passed
    pub fn completion_labels(
        &'a self,
        fun: &TypedExpr,
        existing_arguments: &[CallArg<TypedExpr>],
    ) -> Vec<CompletionItem> {
        let fun_type = fun.type_().fn_types().map(|(arguments, _)| arguments);
        let already_included_labels = existing_arguments
            .iter()
            .filter_map(|a| a.label.clone())
            .collect_vec();
        let Some(field_map) =
            self.callable_field_map(fun, self.compiler.project_compiler.get_importable_modules())
        else {
            return vec![];
        };

        field_map
            .fields
            .iter()
            .filter(|field| !already_included_labels.contains(field.0))
            .map(|(label, arg_index)| {
                let detail = fun_type.as_ref().and_then(|arguments| {
                    arguments
                        .get(*arg_index as usize)
                        .map(|argument| Printer::new().pretty_print(argument, 0))
                });
                let label = format!("{label}:");
                let sort_text = Some(sort_text(CompletionKind::Label, &label, TypeMatch::Unknown));
                CompletionItem {
                    label,
                    detail,
                    kind: Some(CompletionItemKind::FIELD),
                    sort_text,
                    ..Default::default()
                }
            })
            .collect()
    }

    fn root_package_name(&self) -> &str {
        self.compiler.project_compiler.config.name.as_str()
    }

    // checks based on the publicity if something should be suggested for import from root package
    fn is_suggestable_import(&self, publicity: &Publicity, package: &str) -> bool {
        match publicity {
            // We skip private types as we never want those to appear in
            // completions.
            Publicity::Private => false,
            // We only skip internal types if those are not defined in
            // the root package.
            Publicity::Internal { .. } if package != self.root_package_name() => false,
            Publicity::Internal { .. } => true,
            // We never skip public types.
            Publicity::Public => true,
        }
    }

    fn keyword_completion(
        &self,
        keyword: &str,
        cursor_surrounding: &CursorSurroundings,
    ) -> CompletionItem {
        let label = keyword.to_string();

        CompletionItem {
            label: label.clone(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: None,
            label_details: None,
            documentation: None,
            sort_text: Some(sort_text(
                CompletionKind::Keyword,
                &label,
                TypeMatch::Matching,
            )),
            text_edit: cursor_surrounding.to_text_edit(label),
            ..Default::default()
        }
    }

    fn value_completion(
        &self,
        module_qualifier: Option<&str>,
        module_name: &str,
        name: &str,
        value: &type_::ValueConstructor,
        cursor_surrounding: &CursorSurroundings,
        priority: CompletionKind,
    ) -> CompletionItem {
        let type_match = match_type(&self.expected_type, &value.type_);
        let label = match module_qualifier {
            Some(module) => format!("{module}.{name}"),
            None => name.to_string(),
        };

        let type_ = Printer::new().pretty_print(&value.type_, 0);

        let kind = Some(match value.variant {
            ValueConstructorVariant::LocalVariable { .. } => CompletionItemKind::VARIABLE,
            ValueConstructorVariant::ModuleConstant { .. } => CompletionItemKind::CONSTANT,
            ValueConstructorVariant::ModuleFn { .. } => CompletionItemKind::FUNCTION,
            ValueConstructorVariant::Record { arity: 0, .. } => CompletionItemKind::ENUM_MEMBER,
            ValueConstructorVariant::Record { .. } => CompletionItemKind::CONSTRUCTOR,
        });

        let documentation = value.get_documentation().map(|documentation| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: documentation.into(),
            })
        });

        CompletionItem {
            label: label.clone(),
            kind,
            detail: Some(type_),
            label_details: Some(CompletionItemLabelDetails {
                detail: None,
                description: Some(module_name.into()),
            }),
            documentation,
            sort_text: Some(sort_text(priority, &label, type_match)),
            text_edit: cursor_surrounding.to_text_edit(label),
            ..Default::default()
        }
    }

    fn field_completion(&self, label: &str, type_: Arc<Type>) -> CompletionItem {
        let type_match = match_type(&self.expected_type, &type_);
        let type_ = Printer::new().pretty_print(&type_, 0);

        CompletionItem {
            label: label.into(),
            kind: Some(CompletionItemKind::FIELD),
            detail: Some(type_),
            sort_text: Some(sort_text(CompletionKind::FieldAccessor, label, type_match)),
            ..Default::default()
        }
    }
}

fn add_import_to_completion(
    item: &mut CompletionItem,
    import_location: Position,
    module_full_name: &EcoString,
    insert_newlines: &Newlines,
) {
    item.additional_text_edits = Some(vec![get_import_edit(
        import_location,
        module_full_name,
        insert_newlines,
    )]);
}

fn type_completion(
    module: Option<&str>,
    name: &str,
    type_: &TypeConstructor,
    cursor_surrounding: &CursorSurroundings,
    type_completion_context: TypeCompletionContext,
    priority: CompletionKind,
) -> CompletionItem {
    let label = match module {
        Some(module) => format!("{module}.{name}"),
        None => name.to_string(),
    };

    let kind = Some(if type_.type_.is_variable() {
        CompletionItemKind::VARIABLE
    } else {
        CompletionItemKind::CLASS
    });

    let completion_text = match type_completion_context {
        TypeCompletionContext::UnqualifiedImport => format!("{{type {label}}}"),
        TypeCompletionContext::UnqualifiedImportWithinBraces => format!("type {label}"),
        TypeCompletionContext::QualifiedType | TypeCompletionContext::UnqualifiedType => {
            label.clone()
        }
    };

    CompletionItem {
        label: label.clone(),
        kind,
        detail: Some("Type".into()),
        sort_text: Some(sort_text(priority, &label, TypeMatch::Unknown)),
        text_edit: cursor_surrounding.to_text_edit(completion_text),
        ..Default::default()
    }
}

fn match_type(expected_type: &Option<Arc<Type>>, type_: &Type) -> TypeMatch {
    if let Some(expected_type) = expected_type {
        // If the type of the value we are completing is unbound, that
        // technically means that all types match, which doesn't give us
        // any useful information so we treat it as not knowing what the
        // type is, which generally is the case.
        if expected_type.is_unbound() {
            TypeMatch::Unknown
        }
        // We also want to prioritise functions which return the desired type,
        // as often the user's intention will be to write a function call.
        else if let Some((_, return_)) = type_.fn_types()
            && expected_type.same_as(&return_)
        {
            TypeMatch::Matching
        } else if expected_type.same_as(type_) {
            TypeMatch::Matching
        } else {
            TypeMatch::Incompatible
        }
    } else {
        TypeMatch::Unknown
    }
}

pub struct LocalCompletion<'a> {
    mod_name: &'a str,
    insert_range: Range,
    cursor: u32,
    completions: HashMap<EcoString, CompletionItem>,
    expected_type: Option<Arc<Type>>,
}

impl<'a> LocalCompletion<'a> {
    pub fn new(
        mod_name: &'a str,
        insert_range: Range,
        cursor: u32,
        expected_type: Option<Arc<Type>>,
    ) -> Self {
        Self {
            mod_name,
            insert_range,
            cursor,
            completions: HashMap::new(),
            expected_type,
        }
    }

    /// Generates completion items for a given function, including its arguments
    /// and local variables.
    pub fn fn_completions(
        mut self,
        fun: &'a Function<Arc<Type>, TypedExpr>,
    ) -> Vec<CompletionItem> {
        // Add function arguments to completions
        self.visit_fn_arguments(&fun.arguments);

        // Visit the function body statements
        for statement in &fun.body {
            // Visit the statement to find local variables
            self.visit_typed_statement(statement);
        }

        self.completions.into_values().collect_vec()
    }

    fn visit_fn_arguments(&mut self, arguments: &[Arg<Arc<Type>>]) {
        for argument in arguments {
            if let Some(name) = argument.get_variable_name() {
                self.push_completion(name, argument.type_.clone());
            }
        }
    }

    fn push_completion(&mut self, name: &EcoString, type_: Arc<Type>) {
        if name.is_empty() || name.starts_with('_') {
            return;
        }

        _ = self.completions.insert(
            name.clone(),
            self.local_value_completion(self.mod_name, name, type_, self.insert_range),
        );
    }

    fn local_value_completion(
        &self,
        module_name: &str,
        name: &str,
        type_: Arc<Type>,
        insert_range: Range,
    ) -> CompletionItem {
        let type_match = match_type(&self.expected_type, &type_);

        let label = name.to_string();
        let type_ = Printer::new().pretty_print(&type_, 0);

        let documentation = Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: String::from("A locally defined variable."),
        });

        CompletionItem {
            label: label.clone(),
            kind: Some(CompletionItemKind::VARIABLE),
            detail: Some(type_),
            label_details: Some(CompletionItemLabelDetails {
                detail: None,
                description: Some(module_name.into()),
            }),
            documentation: Some(documentation),
            sort_text: Some(sort_text(
                CompletionKind::LocallyDefined,
                &label,
                type_match,
            )),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range: insert_range,
                new_text: label.clone(),
            })),
            ..Default::default()
        }
    }
}

impl<'ast> Visit<'ast> for LocalCompletion<'_> {
    fn visit_typed_statement(&mut self, statement: &'ast ast::TypedStatement) {
        // We only want to suggest local variables that are defined before
        // the cursor
        if statement.location().start >= self.cursor {
            return;
        }
        ast::visit::visit_typed_statement(self, statement);
    }

    /// Visits a typed assignment, selectively processing either the value or the pattern
    /// based on the cursor position.
    /// - If the cursor is within the assignment It visits only the value expression.
    ///   This avoids suggesting variables that are being defined in the assignment itself.
    /// - If the cursor is outside the assignment It visits only the pattern.
    ///   This prevents suggesting variables that might be out of scope.
    fn visit_typed_assignment(&mut self, assignment: &'ast ast::TypedAssignment) {
        if assignment.location.contains(self.cursor) {
            self.visit_typed_expr(&assignment.value);
        } else {
            self.visit_typed_pattern(&assignment.pattern);
        }
    }

    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast ast::SrcSpan,
        _: &'ast Arc<Type>,
        _: &'ast FunctionLiteralKind,
        arguments: &'ast [ast::TypedArg],
        body: &'ast Vec1<ast::TypedStatement>,
        _: &'ast Option<ast::TypeAst>,
    ) {
        // If we are completing after the function body, any locally defined
        // variables are now out of scope so we don't register any.
        if self.cursor >= location.end {
            return;
        }
        self.visit_fn_arguments(arguments);
        for statement in body {
            self.visit_typed_statement(statement);
        }
    }

    fn visit_typed_expr_block(
        &mut self,
        location: &'ast ast::SrcSpan,
        statements: &'ast [ast::TypedStatement],
    ) {
        // If we are completing after the block, any locally defined variables
        // are now out of scope so we don't register any.
        if self.cursor >= location.end {
            return;
        }
        ast::visit::visit_typed_expr_block(self, location, statements);
    }

    fn visit_typed_clause(&mut self, clause: &'ast ast::TypedClause) {
        // Any code which comes before or after a case clause cannot access any
        // of the variables defined within it, so we ignore this clause if so.
        if self.cursor < clause.location.start || self.cursor > clause.location.end {
            return;
        }
        ast::visit::visit_typed_clause(self, clause);
    }

    fn visit_typed_pattern_variable(
        &mut self,
        _: &'ast ast::SrcSpan,
        name: &'ast EcoString,
        type_: &'ast Arc<Type>,
        _origin: &'ast VariableOrigin,
    ) {
        self.push_completion(name, type_.clone());
    }

    fn visit_typed_pattern_discard(
        &mut self,
        _: &'ast ast::SrcSpan,
        name: &'ast EcoString,
        type_: &'ast Arc<Type>,
    ) {
        self.push_completion(name, type_.clone());
    }

    fn visit_typed_pattern_string_prefix(
        &mut self,
        _: &'ast ast::SrcSpan,
        _: &'ast ast::SrcSpan,
        _: &'ast Option<(EcoString, ast::SrcSpan)>,
        _: &'ast ast::SrcSpan,
        _: &'ast EcoString,
        right_side_assignment: &'ast ast::AssignName,
    ) {
        self.push_completion(right_side_assignment.name(), type_::string());
    }

    fn visit_typed_pattern_assign(
        &mut self,
        _: &'ast ast::SrcSpan,
        name: &'ast EcoString,
        pattern: &'ast Pattern<Arc<Type>>,
    ) {
        self.visit_typed_pattern(pattern);
        self.push_completion(name, pattern.type_().clone());
    }
}
