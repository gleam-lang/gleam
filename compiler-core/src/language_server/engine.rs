use crate::{
    analyse::name::correct_name_case,
    ast::{
        CustomType, Definition, ModuleConstant, SrcSpan, TypedArg, TypedExpr, TypedFunction,
        TypedModule, TypedPattern,
    },
    build::{type_constructor_from_modules, Located, Module, UnqualifiedImport},
    config::PackageConfig,
    io::{BeamCompiler, CommandExecutor, FileSystemReader, FileSystemWriter},
    language_server::{
        compiler::LspProjectCompiler, files::FileSystemProxy, progress::ProgressReporter,
    },
    line_numbers::LineNumbers,
    paths::ProjectPaths,
    type_::{
        self, printer::Printer, Deprecation, ModuleInterface, Type, TypeConstructor,
        ValueConstructorVariant,
    },
    Error, Result, Warning,
};
use camino::Utf8PathBuf;
use ecow::EcoString;
use itertools::Itertools;
use lsp::CodeAction;
use lsp_types::{
    self as lsp, DocumentSymbol, Hover, HoverContents, MarkedString, Position, Range,
    SignatureHelp, SymbolKind, SymbolTag, TextEdit, Url,
};
use std::sync::Arc;

use super::{
    code_action::{
        code_action_add_missing_patterns, code_action_convert_qualified_constructor_to_unqualified,
        code_action_convert_unqualified_constructor_to_qualified, code_action_import_module,
        AddAnnotations, CodeActionBuilder, FillInMissingLabelledArgs, LabelShorthandSyntax,
        LetAssertToCase, RedundantTupleInCaseSubject,
    },
    completer::Completer,
    signature_help, src_span_to_lsp_range, DownloadDependencies, MakeLocker,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Response<T> {
    pub result: Result<T, Error>,
    pub warnings: Vec<Warning>,
    pub compilation: Compilation,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Compilation {
    /// Compilation was attempted and succeeded for these modules.
    Yes(Vec<Utf8PathBuf>),
    /// Compilation was not attempted for this operation.
    No,
}

#[derive(Debug)]
pub struct LanguageServerEngine<IO, Reporter> {
    pub(crate) paths: ProjectPaths,

    /// A compiler for the project that supports repeat compilation of the root
    /// package.
    /// In the event the project config changes this will need to be
    /// discarded and reloaded to handle any changes to dependencies.
    pub(crate) compiler: LspProjectCompiler<FileSystemProxy<IO>>,

    modules_compiled_since_last_feedback: Vec<Utf8PathBuf>,
    compiled_since_last_feedback: bool,
    error: Option<Error>,

    // Used to publish progress notifications to the client without waiting for
    // the usual request-response loop.
    progress_reporter: Reporter,

    /// Used to know if to show the "View on HexDocs" link
    /// when hovering on an imported value
    hex_deps: std::collections::HashSet<EcoString>,
}

impl<'a, IO, Reporter> LanguageServerEngine<IO, Reporter>
where
    // IO to be supplied from outside of gleam-core
    IO: FileSystemReader
        + FileSystemWriter
        + BeamCompiler
        + CommandExecutor
        + DownloadDependencies
        + MakeLocker
        + Clone,
    // IO to be supplied from inside of gleam-core
    Reporter: ProgressReporter + Clone + 'a,
{
    pub fn new(
        config: PackageConfig,
        progress_reporter: Reporter,
        io: FileSystemProxy<IO>,
        paths: ProjectPaths,
    ) -> Result<Self> {
        let locker = io.inner().make_locker(&paths, config.target)?;

        // Download dependencies to ensure they are up-to-date for this new
        // configuration and new instance of the compiler
        progress_reporter.dependency_downloading_started();
        let manifest = io.inner().download_dependencies(&paths);
        progress_reporter.dependency_downloading_finished();

        // NOTE: This must come after the progress reporter has finished!
        let manifest = manifest?;

        let compiler: LspProjectCompiler<FileSystemProxy<IO>> =
            LspProjectCompiler::new(manifest, config, paths.clone(), io.clone(), locker)?;

        let hex_deps = compiler
            .project_compiler
            .packages
            .iter()
            .flat_map(|(k, v)| match &v.source {
                crate::manifest::ManifestPackageSource::Hex { .. } => {
                    Some(EcoString::from(k.as_str()))
                }

                _ => None,
            })
            .collect();

        Ok(Self {
            modules_compiled_since_last_feedback: vec![],
            compiled_since_last_feedback: false,
            progress_reporter,
            compiler,
            paths,
            error: None,
            hex_deps,
        })
    }

    pub fn compile_please(&mut self) -> Response<()> {
        self.respond(Self::compile)
    }

    /// Compile the project if we are in one. Otherwise do nothing.
    fn compile(&mut self) -> Result<(), Error> {
        self.compiled_since_last_feedback = true;

        self.progress_reporter.compilation_started();
        let outcome = self.compiler.compile();
        self.progress_reporter.compilation_finished();

        let result = outcome
            // Register which modules have changed
            .map(|modules| self.modules_compiled_since_last_feedback.extend(modules))
            // Return the error, if present
            .into_result();

        self.error = match &result {
            Ok(_) => None,
            Err(error) => Some(error.clone()),
        };

        result
    }

    fn take_warnings(&mut self) -> Vec<Warning> {
        self.compiler.take_warnings()
    }

    // TODO: implement unqualified imported module functions
    //
    pub fn goto_definition(
        &mut self,
        params: lsp::GotoDefinitionParams,
    ) -> Response<Option<lsp::Location>> {
        self.respond(|this| {
            let params = params.text_document_position_params;
            let (line_numbers, node) = match this.node_at_position(&params) {
                Some(location) => location,
                None => return Ok(None),
            };

            let location = match node
                .definition_location(this.compiler.project_compiler.get_importable_modules())
            {
                Some(location) => location,
                None => return Ok(None),
            };

            let (uri, line_numbers) = match location.module {
                None => (params.text_document.uri, &line_numbers),
                Some(name) => {
                    let module = match this.compiler.get_source(name) {
                        Some(module) => module,
                        _ => return Ok(None),
                    };
                    let url = Url::parse(&format!("file:///{}", &module.path))
                        .expect("goto definition URL parse");
                    (url, &module.line_numbers)
                }
            };
            let range = src_span_to_lsp_range(location.span, line_numbers);

            Ok(Some(lsp::Location { uri, range }))
        })
    }

    pub fn completion(
        &mut self,
        params: lsp::TextDocumentPositionParams,
        src: EcoString,
    ) -> Response<Option<Vec<lsp::CompletionItem>>> {
        self.respond(|this| {
            let module = match this.module_for_uri(&params.text_document.uri) {
                Some(m) => m,
                None => return Ok(None),
            };

            let completer = Completer::new(&src, &params, &this.compiler, module);
            let byte_index = completer
                .module_line_numbers
                .byte_index(params.position.line, params.position.character);

            // If in comment context, do not provide completions
            if module.extra.is_within_comment(byte_index) {
                return Ok(None);
            }

            // Check current filercontents if the user is writing an import
            // and handle separately from the rest of the completion flow
            // Check if an import is being written
            if let Some(value) = completer.import_completions() {
                return value;
            }

            let Some(found) = module.find_node(byte_index) else {
                return Ok(None);
            };

            let completions = match found {
                Located::PatternSpread { .. } => None,
                Located::Pattern(_pattern) => None,
                // Do not show completions when typing inside a string.
                Located::Expression(TypedExpr::String { .. }) => None,
                Located::Expression(TypedExpr::Call { fun, args, .. }) => {
                    let mut completions = vec![];
                    completions.append(&mut completer.completion_values());
                    completions.append(&mut completer.completion_labels(fun, args));
                    Some(completions)
                }
                Located::Expression(TypedExpr::RecordAccess { record, .. }) => {
                    let mut completions = vec![];
                    completions.append(&mut completer.completion_values());
                    completions.append(&mut completer.completion_field_accessors(record.type_()));
                    Some(completions)
                }
                Located::Statement(_) | Located::Expression(_) => {
                    Some(completer.completion_values())
                }
                Located::ModuleStatement(Definition::Function(_)) => {
                    Some(completer.completion_types())
                }

                Located::FunctionBody(_) => Some(completer.completion_values()),

                Located::ModuleStatement(Definition::TypeAlias(_) | Definition::CustomType(_)) => {
                    Some(completer.completion_types())
                }

                // If the import completions returned no results and we are in an import then
                // we should try to provide completions for unqualified values
                Located::ModuleStatement(Definition::Import(import)) => this
                    .compiler
                    .get_module_interface(import.module.as_str())
                    .map(|importing_module| {
                        completer.unqualified_completions_from_module(importing_module)
                    }),

                Located::ModuleStatement(Definition::ModuleConstant(_)) => None,

                Located::UnqualifiedImport(_) => None,

                Located::Arg(_) => None,

                Located::Annotation(_, _) => Some(completer.completion_types()),

                Located::Label(_, _) => None,
            };

            Ok(completions)
        })
    }

    pub fn code_actions(
        &mut self,
        params: lsp::CodeActionParams,
    ) -> Response<Option<Vec<CodeAction>>> {
        self.respond(|this| {
            let mut actions = vec![];
            let Some(module) = this.module_for_uri(&params.text_document.uri) else {
                return Ok(None);
            };

            code_action_unused_values(module, &params, &mut actions);
            code_action_unused_imports(module, &params, &mut actions);
            code_action_convert_qualified_constructor_to_unqualified(module, &params, &mut actions);
            code_action_convert_unqualified_constructor_to_qualified(module, &params, &mut actions);
            code_action_fix_names(module, &params, &this.error, &mut actions);
            code_action_import_module(module, &params, &this.error, &mut actions);
            code_action_add_missing_patterns(module, &params, &this.error, &mut actions);
            actions.extend(LetAssertToCase::new(module, &params).code_actions());
            actions.extend(RedundantTupleInCaseSubject::new(module, &params).code_actions());
            actions.extend(LabelShorthandSyntax::new(module, &params).code_actions());
            actions.extend(FillInMissingLabelledArgs::new(module, &params).code_actions());
            AddAnnotations::new(module, &params).code_action(&mut actions);

            Ok(if actions.is_empty() {
                None
            } else {
                Some(actions)
            })
        })
    }

    pub fn document_symbol(
        &mut self,
        params: lsp::DocumentSymbolParams,
    ) -> Response<Vec<DocumentSymbol>> {
        self.respond(|this| {
            let mut symbols = vec![];
            let Some(module) = this.module_for_uri(&params.text_document.uri) else {
                return Ok(symbols);
            };
            let line_numbers = LineNumbers::new(&module.code);

            for definition in &module.ast.definitions {
                match definition {
                    // Typically, imports aren't considered document symbols.
                    Definition::Import(_) => {}

                    Definition::Function(function) => {
                        // By default, the function's location ends right after the return type.
                        // For the full symbol range, have it end at the end of the body.
                        // Also include the documentation, if available.
                        //
                        // By convention, the symbol span starts from the leading slash in the
                        // documentation comment's marker ('///'), not from its content (of which
                        // we have the position), so we must convert the content start position
                        // to the leading slash's position using 'get_doc_marker_pos'.
                        let full_function_span = SrcSpan {
                            start: function
                                .documentation
                                .as_ref()
                                .map(|(doc_start, _)| get_doc_marker_pos(*doc_start))
                                .unwrap_or(function.location.start),

                            end: function.end_position,
                        };

                        let (name_location, name) = function
                            .name
                            .as_ref()
                            .expect("Function in a definition must be named");

                        // The 'deprecated' field is deprecated, but we have to specify it anyway
                        // to be able to construct the 'DocumentSymbol' type, so
                        // we suppress the warning. We specify 'None' as specifying 'Some'
                        // is what is actually deprecated.
                        #[allow(deprecated)]
                        symbols.push(DocumentSymbol {
                            name: name.to_string(),
                            detail: Some(
                                Printer::new(&module.ast.names)
                                    .print_type(&get_function_type(function))
                                    .to_string(),
                            ),
                            kind: SymbolKind::FUNCTION,
                            tags: make_deprecated_symbol_tag(&function.deprecation),
                            deprecated: None,
                            range: src_span_to_lsp_range(full_function_span, &line_numbers),
                            selection_range: src_span_to_lsp_range(*name_location, &line_numbers),
                            children: None,
                        });
                    }

                    Definition::TypeAlias(alias) => {
                        let full_alias_span = match alias.documentation {
                            Some((doc_position, _)) => {
                                SrcSpan::new(get_doc_marker_pos(doc_position), alias.location.end)
                            }
                            None => alias.location,
                        };

                        // The 'deprecated' field is deprecated, but we have to specify it anyway
                        // to be able to construct the 'DocumentSymbol' type, so
                        // we suppress the warning. We specify 'None' as specifying 'Some'
                        // is what is actually deprecated.
                        #[allow(deprecated)]
                        symbols.push(DocumentSymbol {
                            name: alias.alias.to_string(),
                            detail: Some(
                                Printer::new(&module.ast.names)
                                    // If we print with aliases, we end up printing the alias which the user
                                    // is currently hovering, which is not helpful. Instead, we print the
                                    // raw type, so the user can see which type the alias represents
                                    .print_type_without_aliases(&alias.type_)
                                    .to_string(),
                            ),
                            kind: SymbolKind::CLASS,
                            tags: make_deprecated_symbol_tag(&alias.deprecation),
                            deprecated: None,
                            range: src_span_to_lsp_range(full_alias_span, &line_numbers),
                            selection_range: src_span_to_lsp_range(
                                alias.name_location,
                                &line_numbers,
                            ),
                            children: None,
                        });
                    }

                    Definition::CustomType(type_) => {
                        symbols.push(custom_type_symbol(type_, &line_numbers, module));
                    }

                    Definition::ModuleConstant(constant) => {
                        // `ModuleConstant.location` ends at the constant's name or type.
                        // For the full symbol span, necessary for `range`, we need to
                        // include the constant value as well.
                        // Also include the documentation at the start, if available.
                        let full_constant_span = SrcSpan {
                            start: constant
                                .documentation
                                .as_ref()
                                .map(|(doc_start, _)| get_doc_marker_pos(*doc_start))
                                .unwrap_or(constant.location.start),

                            end: constant.value.location().end,
                        };

                        // The 'deprecated' field is deprecated, but we have to specify it anyway
                        // to be able to construct the 'DocumentSymbol' type, so
                        // we suppress the warning. We specify 'None' as specifying 'Some'
                        // is what is actually deprecated.
                        #[allow(deprecated)]
                        symbols.push(DocumentSymbol {
                            name: constant.name.to_string(),
                            detail: Some(
                                Printer::new(&module.ast.names)
                                    .print_type(&constant.type_)
                                    .to_string(),
                            ),
                            kind: SymbolKind::CONSTANT,
                            tags: make_deprecated_symbol_tag(&constant.deprecation),
                            deprecated: None,
                            range: src_span_to_lsp_range(full_constant_span, &line_numbers),
                            selection_range: src_span_to_lsp_range(
                                constant.name_location,
                                &line_numbers,
                            ),
                            children: None,
                        });
                    }
                }
            }

            Ok(symbols)
        })
    }

    fn respond<T>(&mut self, handler: impl FnOnce(&mut Self) -> Result<T>) -> Response<T> {
        let result = handler(self);
        let warnings = self.take_warnings();
        // TODO: test. Ensure hover doesn't report as compiled
        let compilation = if self.compiled_since_last_feedback {
            let modules = std::mem::take(&mut self.modules_compiled_since_last_feedback);
            self.compiled_since_last_feedback = false;
            Compilation::Yes(modules)
        } else {
            Compilation::No
        };
        Response {
            result,
            warnings,
            compilation,
        }
    }

    pub fn hover(&mut self, params: lsp::HoverParams) -> Response<Option<Hover>> {
        self.respond(|this| {
            let params = params.text_document_position_params;

            let (lines, found) = match this.node_at_position(&params) {
                Some(value) => value,
                None => return Ok(None),
            };

            let Some(module) = this.module_for_uri(&params.text_document.uri) else {
                return Ok(None);
            };

            Ok(match found {
                Located::Statement(_) => None, // TODO: hover for statement
                Located::ModuleStatement(Definition::Function(fun)) => {
                    Some(hover_for_function_head(fun, lines, module))
                }
                Located::ModuleStatement(Definition::ModuleConstant(constant)) => {
                    Some(hover_for_module_constant(constant, lines, module))
                }
                Located::ModuleStatement(_) => None,
                Located::UnqualifiedImport(UnqualifiedImport {
                    name,
                    module: module_name,
                    is_type,
                    location,
                }) => this
                    .compiler
                    .get_module_interface(module_name.as_str())
                    .and_then(|module_interface| {
                        if is_type {
                            module_interface.types.get(name).map(|t| {
                                hover_for_annotation(
                                    *location,
                                    t.type_.as_ref(),
                                    Some(t),
                                    lines,
                                    module,
                                )
                            })
                        } else {
                            module_interface.values.get(name).map(|v| {
                                let m = if this.hex_deps.contains(&module_interface.package) {
                                    Some(module_interface)
                                } else {
                                    None
                                };
                                hover_for_imported_value(v, location, lines, m, name, module)
                            })
                        }
                    }),
                Located::Pattern(pattern) => Some(hover_for_pattern(pattern, lines, module)),
                Located::PatternSpread {
                    spread_location,
                    arguments,
                } => {
                    let range = Some(src_span_to_lsp_range(spread_location, &lines));

                    let mut positional = vec![];
                    let mut labelled = vec![];
                    for argument in arguments {
                        // We only want to display the arguments that were ignored using `..`.
                        // Any argument ignored that way is marked as implicit, so if it is
                        // not implicit we just ignore it.
                        if !argument.is_implicit() {
                            continue;
                        }

                        let type_ = Printer::new(&module.ast.names)
                            .print_type(argument.value.type_().as_ref());
                        match &argument.label {
                            Some(label) => labelled.push(format!("- `{label}: {type_}`")),
                            None => positional.push(format!("- `{type_}`")),
                        }
                    }

                    let positional = positional.join("\n");
                    let labelled = labelled.join("\n");
                    let content = match (positional.is_empty(), labelled.is_empty()) {
                        (true, false) => format!("Unused labelled fields:\n{labelled}"),
                        (false, true) => format!("Unused positional fields:\n{positional}"),
                        (_, _) => format!(
                            "Unused positional fields:
{positional}

Unused labelled fields:
{labelled}"
                        ),
                    };

                    Some(Hover {
                        contents: HoverContents::Scalar(MarkedString::from_markdown(content)),
                        range,
                    })
                }
                Located::Expression(expression) => Some(hover_for_expression(
                    expression,
                    lines,
                    module,
                    &this.hex_deps,
                )),
                Located::Arg(arg) => Some(hover_for_function_argument(arg, lines, module)),
                Located::FunctionBody(_) => None,
                Located::Annotation(annotation, type_) => {
                    let type_constructor = type_constructor_from_modules(
                        this.compiler.project_compiler.get_importable_modules(),
                        type_.clone(),
                    );
                    Some(hover_for_annotation(
                        annotation,
                        &type_,
                        type_constructor,
                        lines,
                        module,
                    ))
                }
                Located::Label(location, type_) => {
                    Some(hover_for_label(location, type_, lines, module))
                }
            })
        })
    }

    pub(crate) fn signature_help(
        &mut self,
        params: lsp_types::SignatureHelpParams,
    ) -> Response<Option<SignatureHelp>> {
        self.respond(
            |this| match this.node_at_position(&params.text_document_position_params) {
                Some((_lines, Located::Expression(expr))) => {
                    Ok(signature_help::for_expression(expr))
                }
                Some((_lines, _located)) => Ok(None),
                None => Ok(None),
            },
        )
    }

    fn module_node_at_position(
        &self,
        params: &lsp::TextDocumentPositionParams,
        module: &'a Module,
    ) -> Option<(LineNumbers, Located<'a>)> {
        let line_numbers = LineNumbers::new(&module.code);
        let byte_index = line_numbers.byte_index(params.position.line, params.position.character);
        let node = module.find_node(byte_index);
        let node = node?;
        Some((line_numbers, node))
    }

    fn node_at_position(
        &self,
        params: &lsp::TextDocumentPositionParams,
    ) -> Option<(LineNumbers, Located<'_>)> {
        let module = self.module_for_uri(&params.text_document.uri)?;
        self.module_node_at_position(params, module)
    }

    fn module_for_uri(&self, uri: &Url) -> Option<&Module> {
        // The to_file_path method is available on these platforms
        #[cfg(any(unix, windows, target_os = "redox", target_os = "wasi"))]
        let path = uri.to_file_path().expect("URL file");

        #[cfg(not(any(unix, windows, target_os = "redox", target_os = "wasi")))]
        let path: Utf8PathBuf = uri.path().into();

        let components = path
            .strip_prefix(self.paths.root())
            .ok()?
            .components()
            .skip(1)
            .map(|c| c.as_os_str().to_string_lossy());
        let module_name: EcoString = Itertools::intersperse(components, "/".into())
            .collect::<String>()
            .strip_suffix(".gleam")?
            .into();

        self.compiler.modules.get(&module_name)
    }
}

fn custom_type_symbol(
    type_: &CustomType<Arc<Type>>,
    line_numbers: &LineNumbers,
    module: &Module,
) -> DocumentSymbol {
    let constructors = type_
        .constructors
        .iter()
        .map(|constructor| {
            let mut arguments = vec![];

            // List named arguments as field symbols.
            for argument in &constructor.arguments {
                let Some((label_location, label)) = &argument.label else {
                    continue;
                };

                let full_arg_span = match argument.doc {
                    Some((doc_position, _)) => {
                        SrcSpan::new(get_doc_marker_pos(doc_position), argument.location.end)
                    }
                    None => argument.location,
                };

                // The 'deprecated' field is deprecated, but we have to specify it anyway
                // to be able to construct the 'DocumentSymbol' type, so
                // we suppress the warning. We specify 'None' as specifying 'Some'
                // is what is actually deprecated.
                #[allow(deprecated)]
                arguments.push(DocumentSymbol {
                    name: label.to_string(),
                    detail: Some(
                        Printer::new(&module.ast.names)
                            .print_type(&argument.type_)
                            .to_string(),
                    ),
                    kind: SymbolKind::FIELD,
                    tags: None,
                    deprecated: None,
                    range: src_span_to_lsp_range(full_arg_span, line_numbers),
                    selection_range: src_span_to_lsp_range(*label_location, line_numbers),
                    children: None,
                });
            }

            // Start from the documentation if available, otherwise from the constructor's name,
            // all the way to the end of its arguments.
            let full_constructor_span = SrcSpan {
                start: constructor
                    .documentation
                    .as_ref()
                    .map(|(doc_start, _)| get_doc_marker_pos(*doc_start))
                    .unwrap_or(constructor.location.start),

                end: constructor.location.end,
            };

            // The 'deprecated' field is deprecated, but we have to specify it anyway
            // to be able to construct the 'DocumentSymbol' type, so
            // we suppress the warning. We specify 'None' as specifying 'Some'
            // is what is actually deprecated.
            #[allow(deprecated)]
            DocumentSymbol {
                name: constructor.name.to_string(),
                detail: None,
                kind: if constructor.arguments.is_empty() {
                    SymbolKind::ENUM_MEMBER
                } else {
                    SymbolKind::CONSTRUCTOR
                },
                tags: None,
                deprecated: None,
                range: src_span_to_lsp_range(full_constructor_span, line_numbers),
                selection_range: src_span_to_lsp_range(constructor.name_location, line_numbers),
                children: if arguments.is_empty() {
                    None
                } else {
                    Some(arguments)
                },
            }
        })
        .collect_vec();

    // The type's location, by default, ranges from "(pub) type" to the end of its name.
    // We need it to range to the end of its constructors instead for the full symbol range.
    // We also include documentation, if available, by LSP convention.
    let full_type_span = SrcSpan {
        start: type_
            .documentation
            .as_ref()
            .map(|(doc_start, _)| get_doc_marker_pos(*doc_start))
            .unwrap_or(type_.location.start),

        end: type_.end_position,
    };

    // The 'deprecated' field is deprecated, but we have to specify it anyway
    // to be able to construct the 'DocumentSymbol' type, so
    // we suppress the warning. We specify 'None' as specifying 'Some'
    // is what is actually deprecated.
    #[allow(deprecated)]
    DocumentSymbol {
        name: type_.name.to_string(),
        detail: None,
        kind: SymbolKind::CLASS,
        tags: make_deprecated_symbol_tag(&type_.deprecation),
        deprecated: None,
        range: src_span_to_lsp_range(full_type_span, line_numbers),
        selection_range: src_span_to_lsp_range(type_.name_location, line_numbers),
        children: if constructors.is_empty() {
            None
        } else {
            Some(constructors)
        },
    }
}

fn hover_for_pattern(pattern: &TypedPattern, line_numbers: LineNumbers, module: &Module) -> Hover {
    let documentation = pattern.get_documentation().unwrap_or_default();

    // Show the type of the hovered node to the user
    let type_ = Printer::new(&module.ast.names).print_type(pattern.type_().as_ref());
    let contents = format!(
        "```gleam
{type_}
```
{documentation}"
    );
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(pattern.location(), &line_numbers)),
    }
}

fn get_function_type(fun: &TypedFunction) -> Type {
    Type::Fn {
        args: fun.arguments.iter().map(|arg| arg.type_.clone()).collect(),
        retrn: fun.return_type.clone(),
    }
}

fn hover_for_function_head(
    fun: &TypedFunction,
    line_numbers: LineNumbers,
    module: &Module,
) -> Hover {
    let empty_str = EcoString::from("");
    let documentation = fun
        .documentation
        .as_ref()
        .map(|(_, doc)| doc)
        .unwrap_or(&empty_str);
    let function_type = get_function_type(fun);
    let formatted_type = Printer::new(&module.ast.names).print_type(&function_type);
    let contents = format!(
        "```gleam
{formatted_type}
```
{documentation}"
    );
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(fun.location, &line_numbers)),
    }
}

fn hover_for_function_argument(
    argument: &TypedArg,
    line_numbers: LineNumbers,
    module: &Module,
) -> Hover {
    let type_ = Printer::new(&module.ast.names).print_type(&argument.type_);
    let contents = format!("```gleam\n{type_}\n```");
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(argument.location, &line_numbers)),
    }
}

fn hover_for_annotation(
    location: SrcSpan,
    annotation_type: &Type,
    type_constructor: Option<&TypeConstructor>,
    line_numbers: LineNumbers,
    module: &Module,
) -> Hover {
    let empty_str = EcoString::from("");
    let documentation = type_constructor
        .and_then(|t| t.documentation.as_ref())
        .unwrap_or(&empty_str);
    // If a user is hovering an annotation, it's not very useful to show the
    // local representation of that type, since that's probably what they see
    // in the source code anyway. So here, we print the raw type,
    // which is probably more helpful.
    let type_ = Printer::new(&module.ast.names).print_type_without_aliases(annotation_type);
    let contents = format!(
        "```gleam
{type_}
```
{documentation}"
    );
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(location, &line_numbers)),
    }
}

fn hover_for_label(
    location: SrcSpan,
    type_: Arc<Type>,
    line_numbers: LineNumbers,
    module: &Module,
) -> Hover {
    let type_ = Printer::new(&module.ast.names).print_type(&type_);
    let contents = format!("```gleam\n{type_}\n```");
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(location, &line_numbers)),
    }
}

fn hover_for_module_constant(
    constant: &ModuleConstant<Arc<Type>, EcoString>,
    line_numbers: LineNumbers,
    module: &Module,
) -> Hover {
    let empty_str = EcoString::from("");
    let type_ = Printer::new(&module.ast.names).print_type(&constant.type_);
    let documentation = constant
        .documentation
        .as_ref()
        .map(|(_, doc)| doc)
        .unwrap_or(&empty_str);
    let contents = format!("```gleam\n{type_}\n```\n{documentation}");
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(constant.location, &line_numbers)),
    }
}

fn hover_for_expression(
    expression: &TypedExpr,
    line_numbers: LineNumbers,
    module: &Module,
    hex_deps: &std::collections::HashSet<EcoString>,
) -> Hover {
    let documentation = expression.get_documentation().unwrap_or_default();

    let link_section = get_expr_qualified_name(expression)
        .and_then(|(module_name, name)| {
            get_hexdocs_link_section(module_name, name, &module.ast, hex_deps)
        })
        .unwrap_or("".to_string());

    // Show the type of the hovered node to the user
    let type_ = Printer::new(&module.ast.names).print_type(expression.type_().as_ref());
    let contents = format!(
        "```gleam
{type_}
```
{documentation}{link_section}"
    );
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(expression.location(), &line_numbers)),
    }
}

fn hover_for_imported_value(
    value: &type_::ValueConstructor,
    location: &SrcSpan,
    line_numbers: LineNumbers,
    hex_module_imported_from: Option<&ModuleInterface>,
    name: &EcoString,
    module: &Module,
) -> Hover {
    let documentation = value.get_documentation().unwrap_or_default();

    let link_section = hex_module_imported_from.map_or("".to_string(), |m| {
        format_hexdocs_link_section(m.package.as_str(), m.name.as_str(), name)
    });

    // Show the type of the hovered node to the user
    let type_ = Printer::new(&module.ast.names).print_type(value.type_.as_ref());
    let contents = format!(
        "```gleam
{type_}
```
{documentation}{link_section}"
    );
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(*location, &line_numbers)),
    }
}

// Returns true if any part of either range overlaps with the other.
pub fn overlaps(a: Range, b: Range) -> bool {
    position_within(a.start, b)
        || position_within(a.end, b)
        || position_within(b.start, a)
        || position_within(b.end, a)
}

// Returns true if a range is contained within another.
pub fn within(a: Range, b: Range) -> bool {
    position_within(a.start, b) && position_within(a.end, b)
}

// Returns true if a position is within a range.
fn position_within(position: Position, range: Range) -> bool {
    position >= range.start && position <= range.end
}

fn code_action_unused_values(
    module: &Module,
    params: &lsp::CodeActionParams,
    actions: &mut Vec<CodeAction>,
) {
    let uri = &params.text_document.uri;
    let mut unused_values: Vec<&SrcSpan> = module
        .ast
        .type_info
        .warnings
        .iter()
        .filter_map(|warning| match warning {
            type_::Warning::ImplicitlyDiscardedResult { location } => Some(location),
            _ => None,
        })
        .collect();

    if unused_values.is_empty() {
        return;
    }

    // Convert src spans to lsp range
    let line_numbers = LineNumbers::new(&module.code);

    // Sort spans by start position, with longer spans coming first
    unused_values.sort_by_key(|span| (span.start, -(span.end as i64 - span.start as i64)));

    let mut processed_lsp_range = Vec::new();

    for unused in unused_values {
        let SrcSpan { start, end } = *unused;
        let hover_range = src_span_to_lsp_range(SrcSpan::new(start, end), &line_numbers);

        // Check if this span is contained within any previously processed span
        if processed_lsp_range
            .iter()
            .any(|&prev_lsp_range| within(hover_range, prev_lsp_range))
        {
            continue;
        }

        // Check if the cursor is within this span
        if !within(params.range, hover_range) {
            continue;
        }

        let edit = TextEdit {
            range: src_span_to_lsp_range(SrcSpan::new(start, start), &line_numbers),
            new_text: "let _ = ".into(),
        };

        CodeActionBuilder::new("Assign unused Result value to `_`")
            .kind(lsp_types::CodeActionKind::QUICKFIX)
            .changes(uri.clone(), vec![edit])
            .preferred(true)
            .push_to(actions);

        processed_lsp_range.push(hover_range);
    }
}

fn code_action_unused_imports(
    module: &Module,
    params: &lsp::CodeActionParams,
    actions: &mut Vec<CodeAction>,
) {
    let uri = &params.text_document.uri;
    let unused: Vec<&SrcSpan> = module
        .ast
        .type_info
        .warnings
        .iter()
        .filter_map(|warning| match warning {
            type_::Warning::UnusedImportedModuleAlias { location, .. }
            | type_::Warning::UnusedImportedModule { location, .. } => Some(location),
            _ => None,
        })
        .collect();

    if unused.is_empty() {
        return;
    }

    // Convert src spans to lsp range
    let line_numbers = LineNumbers::new(&module.code);
    let mut hovered = false;
    let mut edits = Vec::with_capacity(unused.len());

    for unused in unused {
        let SrcSpan { start, end } = *unused;

        // If removing an unused alias or at the beginning of the file, don't backspace
        // Otherwise, adjust the end position by 1 to ensure the entire line is deleted with the import.
        let adjusted_end = if delete_line(unused, &line_numbers) {
            end + 1
        } else {
            end
        };

        let range = src_span_to_lsp_range(SrcSpan::new(start, adjusted_end), &line_numbers);
        // Keep track of whether any unused import has is where the cursor is
        hovered = hovered || overlaps(params.range, range);

        edits.push(TextEdit {
            range,
            new_text: "".into(),
        });
    }

    // If none of the imports are where the cursor is we do nothing
    if !hovered {
        return;
    }
    edits.sort_by_key(|edit| edit.range.start);

    CodeActionBuilder::new("Remove unused imports")
        .kind(lsp_types::CodeActionKind::QUICKFIX)
        .changes(uri.clone(), edits)
        .preferred(true)
        .push_to(actions);
}

struct NameCorrection {
    pub location: SrcSpan,
    pub correction: EcoString,
}

fn code_action_fix_names(
    module: &Module,
    params: &lsp::CodeActionParams,
    error: &Option<Error>,
    actions: &mut Vec<CodeAction>,
) {
    let uri = &params.text_document.uri;
    let Some(Error::Type { errors, .. }) = error else {
        return;
    };
    let name_corrections = errors
        .iter()
        .filter_map(|error| match error {
            type_::Error::BadName {
                location,
                name,
                kind,
            } => Some(NameCorrection {
                correction: correct_name_case(name, *kind),
                location: *location,
            }),
            _ => None,
        })
        .collect_vec();

    if name_corrections.is_empty() {
        return;
    }

    // Convert src spans to lsp range
    let line_numbers = LineNumbers::new(&module.code);

    for name_correction in name_corrections {
        let NameCorrection {
            location,
            correction,
        } = name_correction;

        let range = src_span_to_lsp_range(location, &line_numbers);
        // Check if the user's cursor is on the invalid name
        if overlaps(params.range, range) {
            let edit = TextEdit {
                range,
                new_text: correction.to_string(),
            };

            CodeActionBuilder::new(&format!("Rename to {correction}"))
                .kind(lsp_types::CodeActionKind::QUICKFIX)
                .changes(uri.clone(), vec![edit])
                .preferred(true)
                .push_to(actions);
        }
    }
}

// Check if the edit empties a whole line; if so, delete the line.
fn delete_line(span: &SrcSpan, line_numbers: &LineNumbers) -> bool {
    line_numbers.line_starts.iter().any(|&line_start| {
        line_start == span.start && line_numbers.line_starts.contains(&(span.end + 1))
    })
}

fn get_expr_qualified_name(expression: &TypedExpr) -> Option<(&EcoString, &EcoString)> {
    match expression {
        TypedExpr::Var {
            name, constructor, ..
        } if constructor.publicity.is_importable() => match &constructor.variant {
            ValueConstructorVariant::ModuleFn {
                module: module_name,
                ..
            } => Some((module_name, name)),

            ValueConstructorVariant::ModuleConstant {
                module: module_name,
                ..
            } => Some((module_name, name)),

            _ => None,
        },

        TypedExpr::ModuleSelect {
            label, module_name, ..
        } => Some((module_name, label)),

        _ => None,
    }
}

fn format_hexdocs_link_section(package_name: &str, module_name: &str, name: &str) -> String {
    let link = format!("https://hexdocs.pm/{package_name}/{module_name}.html#{name}");
    format!("\nView on [HexDocs]({link})")
}

fn get_hexdocs_link_section(
    module_name: &str,
    name: &str,
    ast: &TypedModule,
    hex_deps: &std::collections::HashSet<EcoString>,
) -> Option<String> {
    let package_name = ast.definitions.iter().find_map(|def| match def {
        Definition::Import(p) if p.module == module_name && hex_deps.contains(&p.package) => {
            Some(&p.package)
        }
        _ => None,
    })?;

    Some(format_hexdocs_link_section(package_name, module_name, name))
}

/// Converts the source start position of a documentation comment's contents into
/// the position of the leading slash in its marker ('///').
fn get_doc_marker_pos(content_pos: u32) -> u32 {
    content_pos.saturating_sub(3)
}

fn make_deprecated_symbol_tag(deprecation: &Deprecation) -> Option<Vec<SymbolTag>> {
    deprecation
        .is_deprecated()
        .then(|| vec![SymbolTag::DEPRECATED])
}
