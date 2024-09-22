use std::{collections::HashMap, sync::Arc};

use ecow::EcoString;
use itertools::Itertools;
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionTextEdit,
    Documentation, MarkupContent, MarkupKind, Position, Range, TextDocumentPositionParams,
    TextEdit,
};
use strum::IntoEnumIterator;

use crate::{
    ast::{self, Arg, CallArg, Definition, Function, Pattern, Publicity, TypedExpr},
    build::Module,
    io::{CommandExecutor, FileSystemReader, FileSystemWriter},
    line_numbers::LineNumbers,
    type_::{
        self, collapse_links, pretty::Printer, AccessorsMap, FieldMap, ModuleInterface,
        PreludeType, Type, TypeConstructor, ValueConstructorVariant, PRELUDE_MODULE_NAME,
    },
    Result,
};

use super::{
    compiler::LspProjectCompiler,
    edits::{
        add_newlines_after_import, get_import, get_import_edit,
        position_of_first_definition_if_import, Newlines,
    },
    files::FileSystemProxy,
    DownloadDependencies, MakeLocker,
};

// Represents the kind/specificity of completion that is being requested.
#[derive(Copy, Clone)]
enum CompletionKind {
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

// Gives the sort text for a completion item based on the kind and label.
// This ensures that more specific kinds of completions are placed before
// less specific ones..
fn sort_text(kind: CompletionKind, label: &str) -> String {
    let priority: u8 = match kind {
        CompletionKind::Label => 0,
        CompletionKind::FieldAccessor => 1,
        CompletionKind::LocallyDefined => 2,
        CompletionKind::ImportedModule => 3,
        CompletionKind::Prelude => 4,
        CompletionKind::ImportableModule => 5,
    };
    format!("{priority}_{label}")
}

// The form in which a type completion is needed in context.
// Mainly used to determine if the "type" keyword should be appended to the completion
enum TypeCompletionForm {
    // The type completion is for an unqualified import.
    UnqualifiedImport,
    Default,
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
}

impl<'a, IO> Completer<'a, IO>
where
    // IO to be supplied from outside of gleam-core
    IO: FileSystemReader
        + FileSystemWriter
        + CommandExecutor
        + DownloadDependencies
        + MakeLocker
        + Clone,
{
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
        }
    }

    // Gets the current range around the cursor to place a completion
    // and the phrase surrounding the cursor to use for completion.
    // This method takes in a helper to determine what qualifies as
    // a phrase depending on context.
    fn get_phrase_surrounding_for_completion(
        &'a self,
        valid_phrase_char: &impl Fn(char) -> bool,
    ) -> (Range, String) {
        let cursor = self
            .src_line_numbers
            .byte_index(self.cursor_position.line, self.cursor_position.character);

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

        (
            Range {
                start: Position {
                    line: self.cursor_position.line,
                    character: self.cursor_position.character - before.len() as u32,
                },
                end: Position {
                    line: self.cursor_position.line,
                    character: self.cursor_position.character + after.len() as u32,
                },
            },
            format!("{before}{after}"),
        )
    }

    // Gets the current range around the cursor to place a completion
    // and any part of the phrase preceeding a dot if a module is being selected from.
    // A continuous phrase in this case is a name or typename that may have a dot in it.
    // This is used to match the exact location to fill in the completion.
    fn get_phrase_surrounding_completion(&'a self) -> (Range, Option<String>) {
        let valid_phrase_char = |c: char| {
            // Checks if a character is not a valid name/upname character or a dot.
            !c.is_ascii_alphanumeric() && c != '.' && c != '_'
        };
        let (range, word) = self.get_phrase_surrounding_for_completion(&valid_phrase_char);
        (range, word.split_once('.').map(|c| String::from(c.0)))
    }

    // Gets the current range around the cursor to place a completion.
    // For unqualified imports we special case the word being completed to allow for whitespace but not dots.
    // This is to allow `type MyType` to be treated as 1 "phrase" for the sake of completion.
    fn get_phrase_surrounding_completion_for_import(&'a self) -> Range {
        let valid_phrase_char = |c: char| {
            // Checks if a character is not a valid name/upname character or whitespace.
            // The newline character is not included as well.
            !c.is_ascii_alphanumeric() && c != '_' && c != ' ' && c != '\t'
        };
        let (range, _) = self.get_phrase_surrounding_for_completion(&valid_phrase_char);
        range
    }

    /// Checks if the line being editted is an import line and provides completions if it is.
    /// If the line includes a dot then it provides unqualified import completions.
    /// Otherwise it provides direct module import completions.
    pub fn import_completions(&'a self) -> Option<Result<Option<Vec<CompletionItem>>>> {
        let start_of_line = self
            .src_line_numbers
            .byte_index(self.cursor_position.line, 0);
        let end_of_line = self
            .src_line_numbers
            .byte_index(self.cursor_position.line + 1, 0);

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

            Some(Ok(Some(
                self.unqualified_completions_from_module(importing_module),
            )))
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
    ) -> Vec<CompletionItem> {
        let insert_range = self.get_phrase_surrounding_completion_for_import();
        let mut completions = vec![];

        // Find values and type that have already previously been imported
        let mut already_imported_types = std::collections::HashSet::new();
        let mut already_imported_values = std::collections::HashSet::new();

        // Search the ast for import statements
        for import in self.module.ast.definitions.iter().filter_map(get_import) {
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
                insert_range,
                TypeCompletionForm::UnqualifiedImport,
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
            completions.push(value_completion(
                None,
                &module_being_imported_from.name,
                name,
                value,
                insert_range,
                CompletionKind::ImportedModule,
            ));
        }

        completions
    }

    // Get all the modules that can be imported that have not already been imported.
    fn completable_modules_for_import(&'a self) -> Vec<(&EcoString, &ModuleInterface)> {
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
            // It is possible to import modules from dependencies of dependencies
            // but it's not recommended so we don't include them in completions
            .filter(|(_, module)| {
                let is_root_or_prelude =
                    module.package == self.root_package_name() || module.package.is_empty();
                is_root_or_prelude || direct_dep_packages.contains(&module.package)
            })
            //
            // src/ cannot import test/
            .filter(|(_, module)| module.origin.is_src() || !self.module.origin.is_src())
            //
            // It is possible to import internal modules from other packages,
            // but it's not recommended so we don't include them in completions
            .filter(|(_, module)| module.package == self.root_package_name() || !module.is_internal)
            //
            // You cannot import a module twice
            .filter(|(name, _)| !already_imported.contains(*name))
            //
            // You cannot import yourself
            .filter(|(name, _)| *name != &self.module.name)
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
        let surrounding_completion = self.get_phrase_surrounding_completion();
        let mut completions = vec![];

        let (insert_range, module_select) = surrounding_completion;

        // Prelude types
        for type_ in PreludeType::iter() {
            let label: String = type_.name().into();
            let sort_text = Some(sort_text(CompletionKind::Prelude, &label));
            completions.push(CompletionItem {
                label,
                detail: Some("Type".into()),
                kind: Some(CompletionItemKind::CLASS),
                sort_text,
                ..Default::default()
            });
        }

        // Module types
        // Do not complete direct module types if the user has already started typing a module select.
        // e.x. when the user has typed mymodule.| we know local module types are no longer relevant
        if module_select.is_none() {
            for (name, type_) in &self.module.ast.type_info.types {
                completions.push(type_completion(
                    None,
                    name,
                    type_,
                    insert_range,
                    TypeCompletionForm::Default,
                    CompletionKind::LocallyDefined,
                ));
            }
        }

        // Imported modules
        for import in self.module.ast.definitions.iter().filter_map(get_import) {
            // The module may not be known of yet if it has not previously
            // compiled yet in this editor session.
            let Some(module) = self.compiler.get_module_interface(&import.module) else {
                continue;
            };

            // Qualified types
            for (name, type_) in &module.types {
                if !self.is_suggestable_import(&type_.publicity, module.package.as_str()) {
                    continue;
                }

                if let Some(module) = import.used_name() {
                    // If the user has already started a module select then don't show irrelevant modules.
                    // e.x. when the user has typed mymodule.| we should only show items from mymodule.
                    if let Some(input_mod_name) = &module_select {
                        if &module != input_mod_name {
                            continue;
                        }
                    }
                    completions.push(type_completion(
                        Some(&module),
                        name,
                        type_,
                        insert_range,
                        TypeCompletionForm::Default,
                        CompletionKind::ImportedModule,
                    ));
                }
            }

            // Unqualified types
            // Do not complete unqualified types if the user has already started typing a module select.
            // e.x. when the user has typed mymodule.| we know unqualified module types are no longer relevant.
            if module_select.is_none() {
                for unqualified in &import.unqualified_types {
                    match module.get_public_type(&unqualified.name) {
                        Some(type_) => completions.push(type_completion(
                            None,
                            unqualified.used_name(),
                            type_,
                            insert_range,
                            TypeCompletionForm::Default,
                            CompletionKind::ImportedModule,
                        )),
                        None => continue,
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
                .last()
                .unwrap_or(module_full_name);

            // If the user has already started a module select then don't show irrelevant modules.
            // e.x. when the user has typed mymodule.| we should only show items from mymodule.
            if let Some(input_mod_name) = &module_select {
                if qualifier != input_mod_name {
                    continue;
                }
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
                    insert_range,
                    TypeCompletionForm::Default,
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
        let surrounding_completion = self.get_phrase_surrounding_completion();
        let mut completions = vec![];
        let mod_name = self.module.name.as_str();

        let (insert_range, module_select) = surrounding_completion;

        let mut push_prelude_completion = |label: &str, kind| {
            let label = label.to_string();
            let sort_text = Some(sort_text(CompletionKind::Prelude, &label));
            completions.push(CompletionItem {
                label,
                detail: Some(PRELUDE_MODULE_NAME.into()),
                kind: Some(kind),
                sort_text,
                ..Default::default()
            });
        };

        // Prelude values
        for type_ in PreludeType::iter() {
            match type_ {
                PreludeType::Bool => {
                    push_prelude_completion("True", CompletionItemKind::ENUM_MEMBER);
                    push_prelude_completion("False", CompletionItemKind::ENUM_MEMBER);
                }
                PreludeType::Nil => {
                    push_prelude_completion("Nil", CompletionItemKind::ENUM_MEMBER);
                }
                PreludeType::Result => {
                    push_prelude_completion("Ok", CompletionItemKind::CONSTRUCTOR);
                    push_prelude_completion("Error", CompletionItemKind::CONSTRUCTOR);
                }
                PreludeType::BitArray
                | PreludeType::Float
                | PreludeType::Int
                | PreludeType::List
                | PreludeType::String
                | PreludeType::UtfCodepoint => {}
            }
        }

        // Module values
        // Do not complete direct module values if the user has already started typing a module select.
        // e.x. when the user has typed mymodule.| we know local module values are no longer relevant
        if module_select.is_none() {
            let cursor = self
                .src_line_numbers
                .byte_index(self.cursor_position.line, self.cursor_position.character);

            // Find the function that the cursor is in and push completions for
            // its arguments and local variables.
            if let Some(fun) = self.module.ast.definitions.iter().find_map(|d| match d {
                Definition::Function(f) if f.full_location().contains(cursor) => Some(f),
                _ => None,
            }) {
                completions.extend(
                    LocalCompletion::new(mod_name, insert_range, cursor).fn_completions(fun),
                );
            }

            for (name, value) in &self.module.ast.type_info.values {
                // Here we do not check for the internal attribute: we always want
                // to show autocompletions for values defined in the same module,
                // even if those are internal.
                completions.push(value_completion(
                    None,
                    mod_name,
                    name,
                    value,
                    insert_range,
                    CompletionKind::LocallyDefined,
                ));
            }
        }

        // Imported modules
        for import in self.module.ast.definitions.iter().filter_map(get_import) {
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
                    if let Some(input_mod_name) = &module_select {
                        if &module != input_mod_name {
                            continue;
                        }
                    }
                    completions.push(value_completion(
                        Some(&module),
                        mod_name,
                        name,
                        value,
                        insert_range,
                        CompletionKind::ImportedModule,
                    ));
                }
            }

            // Unqualified values
            // Do not complete unqualified values if the user has already started typing a module select.
            // e.x. when the user has typed mymodule.| we know unqualified module values are no longer relevant.
            if module_select.is_none() {
                for unqualified in &import.unqualified_values {
                    match module.get_public_value(&unqualified.name) {
                        Some(value) => {
                            let name = unqualified.used_name();
                            completions.push(value_completion(
                                None,
                                mod_name,
                                name,
                                value,
                                insert_range,
                                CompletionKind::ImportedModule,
                            ))
                        }
                        None => continue,
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
                .last()
                .unwrap_or(module_full_name);

            // If the user has already started a module select then don't show irrelevant modules.
            // e.x. when the user has typed mymodule.| we should only show items from mymodule.
            if let Some(input_mod_name) = &module_select {
                if qualifier != input_mod_name {
                    continue;
                }
            }

            // Qualified values
            for (name, value) in &module.values {
                if !self.is_suggestable_import(&value.publicity, module.package.as_str()) {
                    continue;
                }

                let mut completion = value_completion(
                    Some(qualifier),
                    module_full_name,
                    name,
                    value,
                    insert_range,
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
    ) -> Option<&AccessorsMap> {
        let type_ = collapse_links(type_);
        match type_.as_ref() {
            Type::Named { name, module, .. } => importable_modules
                .get(module)
                .and_then(|i| i.accessors.get(name))
                .filter(|a| a.publicity.is_importable() || module == &self.module.name),
            _ => None,
        }
    }

    /// Provides completions for field accessors when the context being editted
    /// is a custom type instance
    pub fn completion_field_accessors(&'a self, type_: Arc<Type>) -> Vec<CompletionItem> {
        self.type_accessors_from_modules(
            self.compiler.project_compiler.get_importable_modules(),
            type_,
        )
        .map(|accessors_map| {
            accessors_map
                .accessors
                .values()
                .map(|accessor| field_completion(&accessor.label, accessor.type_.clone()))
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
            _ => None,
        }
    }

    /// Provides completions for labels when the context being editted is a call
    /// that has labelled arguments that can be passed
    pub fn completion_labels(
        &'a self,
        fun: &TypedExpr,
        existing_args: &[CallArg<TypedExpr>],
    ) -> Vec<CompletionItem> {
        let fun_type = fun.type_().fn_types().map(|(args, _)| args);
        let already_included_labels = existing_args
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
                let detail = fun_type.as_ref().and_then(|args| {
                    args.get(*arg_index as usize)
                        .map(|a| Printer::new().pretty_print(a, 0))
                });
                let label = format!("{label}:");
                let sort_text = Some(sort_text(CompletionKind::Label, &label));
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
    insert_range: Range,
    include_type_in_completion: TypeCompletionForm,
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

    CompletionItem {
        label: label.clone(),
        kind,
        detail: Some("Type".into()),
        sort_text: Some(sort_text(priority, &label)),
        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
            range: insert_range,
            new_text: match include_type_in_completion {
                TypeCompletionForm::UnqualifiedImport => format!("type {label}"),
                TypeCompletionForm::Default => label.clone(),
            },
        })),
        ..Default::default()
    }
}

fn value_completion(
    module_qualifier: Option<&str>,
    module_name: &str,
    name: &str,
    value: &type_::ValueConstructor,
    insert_range: Range,
    priority: CompletionKind,
) -> CompletionItem {
    let label = match module_qualifier {
        Some(module) => format!("{module}.{name}"),
        None => name.to_string(),
    };

    let type_ = Printer::new().pretty_print(&value.type_, 0);

    let kind = Some(match value.variant {
        ValueConstructorVariant::LocalVariable { .. } => CompletionItemKind::VARIABLE,
        ValueConstructorVariant::ModuleConstant { .. } => CompletionItemKind::CONSTANT,
        ValueConstructorVariant::LocalConstant { .. } => CompletionItemKind::CONSTANT,
        ValueConstructorVariant::ModuleFn { .. } => CompletionItemKind::FUNCTION,
        ValueConstructorVariant::Record { arity: 0, .. } => CompletionItemKind::ENUM_MEMBER,
        ValueConstructorVariant::Record { .. } => CompletionItemKind::CONSTRUCTOR,
    });

    let documentation = value.get_documentation().map(|d| {
        Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: d.into(),
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
        sort_text: Some(sort_text(priority, &label)),
        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
            range: insert_range,
            new_text: label.clone(),
        })),
        ..Default::default()
    }
}

fn local_value_completion(
    module_name: &str,
    name: &str,
    type_: Arc<Type>,
    insert_range: Range,
) -> CompletionItem {
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
        sort_text: Some(sort_text(CompletionKind::LocallyDefined, &label)),
        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
            range: insert_range,
            new_text: label.clone(),
        })),
        ..Default::default()
    }
}

fn field_completion(label: &str, type_: Arc<Type>) -> CompletionItem {
    let type_ = Printer::new().pretty_print(&type_, 0);

    CompletionItem {
        label: label.into(),
        kind: Some(CompletionItemKind::FIELD),
        detail: Some(type_),
        sort_text: Some(sort_text(CompletionKind::FieldAccessor, label)),
        ..Default::default()
    }
}

pub struct LocalCompletion<'a> {
    mod_name: &'a str,
    insert_range: Range,
    cursor: u32,
    completions: HashMap<EcoString, CompletionItem>,
}

impl<'a> LocalCompletion<'a> {
    pub fn new(mod_name: &'a str, insert_range: Range, cursor: u32) -> Self {
        Self {
            mod_name,
            insert_range,
            cursor,
            completions: HashMap::new(),
        }
    }

    /// Generates completion items for a given function, including its arguments
    /// and local variables.
    pub fn fn_completions(
        mut self,
        fun: &'a Function<Arc<Type>, TypedExpr>,
    ) -> Vec<CompletionItem> {
        // Add function arguments to completions
        self.visit_fn_args(&fun.arguments);

        // Visit the function body statements
        for statement in &fun.body {
            // We only want to suggest local variables that are defined before
            // the cursor
            if statement.location().start >= self.cursor {
                continue;
            }

            // Visit the statement to find local variables
            ast::visit::visit_typed_statement(&mut self, statement);
        }

        self.completions.into_values().collect_vec()
    }

    fn visit_fn_args(&mut self, args: &[Arg<Arc<Type>>]) {
        for arg in args {
            if let Some(name) = arg.get_variable_name() {
                self.push_completion(name, arg.type_.clone());
            }
        }
    }

    fn push_completion(&mut self, name: &EcoString, type_: Arc<Type>) {
        if name.is_empty() || name.starts_with('_') {
            return;
        }

        _ = self.completions.insert(
            name.clone(),
            local_value_completion(self.mod_name, name, type_, self.insert_range),
        );
    }
}

impl<'ast> ast::visit::Visit<'ast> for LocalCompletion<'_> {
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
        _: &'ast ast::SrcSpan,
        _: &'ast Arc<Type>,
        _: &'ast bool,
        args: &'ast [ast::TypedArg],
        body: &'ast [ast::TypedStatement],
        _: &'ast Option<ast::TypeAst>,
    ) {
        self.visit_fn_args(args);
        for statement in body {
            self.visit_typed_statement(statement);
        }
    }

    fn visit_typed_pattern_variable(
        &mut self,
        _: &'ast ast::SrcSpan,
        name: &'ast EcoString,
        type_: &'ast Arc<Type>,
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
