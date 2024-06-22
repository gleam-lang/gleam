use ecow::EcoString;
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionTextEdit,
    Documentation, MarkupContent, MarkupKind, Position, Range, TextDocumentPositionParams,
    TextEdit,
};
use strum::IntoEnumIterator;

use crate::{
    ast::{Definition, Import, Publicity, TypedDefinition},
    build::Module,
    io::{CommandExecutor, FileSystemReader, FileSystemWriter},
    line_numbers::LineNumbers,
    type_::{
        pretty::Printer, ModuleInterface, PreludeType, TypeConstructor, ValueConstructorVariant,
    },
    Result,
};

use super::{
    compiler::LspProjectCompiler, files::FileSystemProxy, DownloadDependencies, MakeLocker,
};

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
    src_line_numbers: LineNumbers,
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
            format!("{}{}", before, after),
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
                self.compiler.get_module_inferface(importing_module_name)?;

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
            std::collections::HashSet::from_iter(self.module.dependencies_list());
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
            completions.push(CompletionItem {
                label: type_.name().into(),
                detail: Some("Type".into()),
                kind: Some(CompletionItemKind::CLASS),
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
                ));
            }
        }

        // Imported modules
        for import in self.module.ast.definitions.iter().filter_map(get_import) {
            // The module may not be known of yet if it has not previously
            // compiled yet in this editor session.
            let Some(module) = self.compiler.get_module_inferface(&import.module) else {
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
                        )),
                        None => continue,
                    }
                }
            }
        }

        // Importable modules
        let import_location = self.first_import_line_in_module();
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
                );
                add_import_to_completion(&mut completion, import_location, module_full_name);
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

        // Module values
        // Do not complete direct module values if the user has already started typing a module select.
        // e.x. when the user has typed mymodule.| we know local module values are no longer relevant
        if module_select.is_none() {
            for (name, value) in &self.module.ast.type_info.values {
                // Here we do not check for the internal attribute: we always want
                // to show autocompletions for values defined in the same module,
                // even if those are internal.
                completions.push(value_completion(None, mod_name, name, value, insert_range));
            }
        }

        // Imported modules
        for import in self.module.ast.definitions.iter().filter_map(get_import) {
            // The module may not be known of yet if it has not previously
            // compiled yet in this editor session.
            let Some(module) = self.compiler.get_module_inferface(&import.module) else {
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
                            ))
                        }
                        None => continue,
                    }
                }
            }
        }

        // Importable modules
        let import_location = self.first_import_line_in_module();
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

                let mut completion =
                    value_completion(Some(qualifier), module_full_name, name, value, insert_range);

                add_import_to_completion(&mut completion, import_location, module_full_name);
                completions.push(completion);
            }
        }

        completions
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
            Publicity::Internal if package != self.root_package_name() => false,
            Publicity::Internal => true,
            // We never skip public types.
            Publicity::Public => true,
        }
    }

    // Gets the position of the line with the first import statement in the file.
    fn first_import_line_in_module(&'a self) -> Position {
        let import_location = self
            .module
            .ast
            .definitions
            .iter()
            .find_map(get_import)
            .map_or(0, |i| i.location.start);
        let import_location = self.module_line_numbers.line_number(import_location);
        Position::new(import_location - 1, 0)
    }
}

fn add_import_to_completion(
    item: &mut CompletionItem,
    import_location: Position,
    module_full_name: &EcoString,
) {
    item.additional_text_edits = Some(vec![TextEdit {
        range: Range {
            start: import_location,
            end: import_location,
        },
        new_text: ["import ", module_full_name, "\n"].concat(),
    }]);
}

fn type_completion(
    module: Option<&str>,
    name: &str,
    type_: &TypeConstructor,
    insert_range: Range,
    include_type_in_completion: TypeCompletionForm,
) -> CompletionItem {
    let label = match module {
        Some(module) => format!("{module}.{name}"),
        None => name.to_string(),
    };

    let kind = Some(if type_.typ.is_variable() {
        CompletionItemKind::VARIABLE
    } else {
        CompletionItemKind::CLASS
    });

    CompletionItem {
        label: label.clone(),
        kind,
        detail: Some("Type".into()),
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
    value: &crate::type_::ValueConstructor,
    insert_range: Range,
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
        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
            range: insert_range,
            new_text: label.clone(),
        })),
        ..Default::default()
    }
}

fn get_import(statement: &TypedDefinition) -> Option<&Import<EcoString>> {
    match statement {
        Definition::Import(import) => Some(import),
        _ => None,
    }
}
