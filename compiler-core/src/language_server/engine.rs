use crate::{
    ast::{
        Arg, Definition, Function, Import, ModuleConstant, Pattern, Publicity, Statement,
        TypedDefinition, TypedExpr, TypedPattern,
    },
    build::{Located, Module},
    config::PackageConfig,
    io::{CommandExecutor, FileSystemReader, FileSystemWriter},
    language_server::{
        compiler::LspProjectCompiler, files::FileSystemProxy, progress::ProgressReporter,
    },
    line_numbers::LineNumbers,
    paths::ProjectPaths,
    type_::{self, pretty::Printer, PreludeType, Type, ValueConstructorVariant},
    Error, Result, Warning,
};
use camino::Utf8PathBuf;
use ecow::EcoString;
use lsp::CodeAction;
use lsp_types::{self as lsp, Hover, HoverContents, MarkedString, Url};
use std::{collections::BTreeMap, sync::Arc};
use strum::IntoEnumIterator;

use super::{
    code_action::CodeActionBuilder, src_span_to_lsp_range, DownloadDependencies, MakeLocker,
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

        let compiler =
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
        let result = self.compiler.compile();
        self.progress_reporter.compilation_finished();

        let modules = result?;
        self.modules_compiled_since_last_feedback.extend(modules);

        Ok(())
    }

    fn take_warnings(&mut self) -> Vec<Warning> {
        self.compiler.take_warnings()
    }

    // TODO: test different package module function calls
    //
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

            let location = match node.definition_location() {
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

            // Check current file contents if the user is writing an import
            // and handle separately from the rest of the completion flow
            // Check if an import is being written
            {
                let line_num = LineNumbers::new(src.as_str());
                let start_of_line = line_num.byte_index(params.position.line, 0);
                let end_of_line = line_num.byte_index(params.position.line + 1, 0);

                // Check if the line starts with "import"
                let from_ind = &src.get(start_of_line as usize..end_of_line as usize);
                if let Some(from_ind) = from_ind {
                    if from_ind.starts_with("import") {
                        // Find where to start and end the import completion
                        let start = line_num.line_and_column_number(start_of_line);
                        let end = line_num.line_and_column_number(end_of_line - 1);
                        let start = lsp::Position {
                            line: start.line - 1,
                            character: start.column + 6,
                        };
                        let end = lsp::Position {
                            line: end.line - 1,
                            character: end.column,
                        };
                        return Ok(Some(this.completion_imports(module, start, end)));
                    }
                }
            }

            let line_numbers = LineNumbers::new(&module.code);
            let byte_index =
                line_numbers.byte_index(params.position.line, params.position.character);

            let Some(found) = module.find_node(byte_index) else {
                return Ok(None);
            };

            let completions = match found {
                Located::Pattern(_pattern) => None,

                Located::Statement(_) | Located::Expression(_) => {
                    let mut completions = this.completion_values(module);
                    completions.extend(this.completion_variables(module, byte_index));
                    Some(completions)
                }

                Located::ModuleStatement(Definition::Function(_)) => {
                    Some(this.completion_types(module))
                }

                Located::FunctionBody(_) => {
                    let mut completions = this.completion_values(module);
                    completions.extend(this.completion_variables(module, byte_index));
                    Some(completions)
                }

                Located::ModuleStatement(Definition::TypeAlias(_) | Definition::CustomType(_)) => {
                    Some(this.completion_types(module))
                }

                Located::ModuleStatement(Definition::Import(_) | Definition::ModuleConstant(_)) => {
                    None
                }

                Located::Arg(_) => None,
            };

            Ok(completions)
        })
    }

    pub fn action(&mut self, params: lsp::CodeActionParams) -> Response<Option<Vec<CodeAction>>> {
        self.respond(|this| {
            let mut actions = vec![];
            let Some(module) = this.module_for_uri(&params.text_document.uri) else {
                return Ok(None);
            };

            code_action_unused_imports(module, &params, &mut actions);

            Ok(if actions.is_empty() {
                None
            } else {
                Some(actions)
            })
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

            Ok(match found {
                Located::Statement(_) => None, // TODO: hover for statement
                Located::ModuleStatement(Definition::Function(fun)) => {
                    Some(hover_for_function_head(fun, lines))
                }
                Located::ModuleStatement(Definition::ModuleConstant(constant)) => {
                    Some(hover_for_module_constant(constant, lines))
                }
                Located::ModuleStatement(_) => None,
                Located::Pattern(pattern) => Some(hover_for_pattern(pattern, lines)),
                Located::Expression(expression) => {
                    let module = this.module_for_uri(&params.text_document.uri);

                    Some(hover_for_expression(
                        expression,
                        lines,
                        module,
                        &this.hex_deps,
                    ))
                }
                Located::Arg(arg) => Some(hover_for_function_argument(arg, lines)),
                Located::FunctionBody(_) => None,
            })
        })
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
        use itertools::Itertools;

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

    fn completion_types<'b>(&'b self, module: &'b Module) -> Vec<lsp::CompletionItem> {
        let mut completions = vec![];

        // Prelude types
        for type_ in PreludeType::iter() {
            completions.push(lsp::CompletionItem {
                label: type_.name().into(),
                detail: Some("Type".into()),
                kind: Some(lsp::CompletionItemKind::CLASS),
                ..Default::default()
            });
        }

        // Module types
        for (name, type_) in &module.ast.type_info.types {
            completions.push(type_completion(None, name, type_));
        }

        // Imported modules
        for import in module.ast.definitions.iter().filter_map(get_import) {
            // The module may not be known of yet if it has not previously
            // compiled yet in this editor session.
            // TODO: test getting completions from modules defined in other packages
            let Some(module) = self.compiler.get_module_inferface(&import.module) else {
                continue;
            };

            // Qualified types
            for (name, type_) in &module.types {
                match type_.publicity {
                    // We skip private types as we never want those to appear in
                    // completions.
                    Publicity::Private => continue,
                    // We only skip internal types if those are not defined in
                    // the root package.
                    Publicity::Internal if module.package != self.root_package_name() => continue,
                    Publicity::Internal => {}
                    // We never skip public types.
                    Publicity::Public => {}
                }

                let module = import.used_name();
                if module.is_some() {
                    completions.push(type_completion(module.as_ref(), name, type_));
                }
            }

            // Unqualified types
            for unqualified in &import.unqualified_types {
                match module.get_public_type(&unqualified.name) {
                    Some(type_) => {
                        completions.push(type_completion(None, unqualified.used_name(), type_))
                    }
                    None => continue,
                }
            }
        }

        completions
    }

    fn completion_values<'b>(&'b self, module: &'b Module) -> Vec<lsp::CompletionItem> {
        let mut completions = vec![];

        // Module functions
        for (name, value) in &module.ast.type_info.values {
            // Here we do not check for the internal attribute: we always want
            // to show autocompletions for values defined in the same module,
            // even if those are internal.
            completions.push(value_completion(None, name, value));
        }

        // Imported modules
        for import in module.ast.definitions.iter().filter_map(get_import) {
            // The module may not be known of yet if it has not previously
            // compiled yet in this editor session.
            // TODO: test getting completions from modules defined in other packages
            let Some(module) = self.compiler.get_module_inferface(&import.module) else {
                continue;
            };

            // Qualified values
            for (name, value) in &module.values {
                match value.publicity {
                    // We skip private values as we never want those to appear in
                    // completions.
                    Publicity::Private => continue,
                    // We only skip internal values if those are not defined in
                    // the root package.
                    Publicity::Internal if module.package != self.root_package_name() => continue,
                    Publicity::Internal => {}
                    // We never skip public values.
                    Publicity::Public => {}
                }

                let module = import.used_name();
                if module.is_some() {
                    completions.push(value_completion(module.as_deref(), name, value));
                }
            }

            // Unqualified values
            for unqualified in &import.unqualified_values {
                match module.get_public_value(&unqualified.name) {
                    Some(value) => {
                        completions.push(value_completion(None, unqualified.used_name(), value))
                    }
                    None => continue,
                }
            }
        }

        completions
    }

    fn completion_imports<'b>(
        &'b self,
        module: &'b Module,
        start: lsp::Position,
        end: lsp::Position,
    ) -> Vec<lsp::CompletionItem> {
        let already_imported: std::collections::HashSet<EcoString> =
            std::collections::HashSet::from_iter(module.dependencies_list());
        self.compiler
            .project_compiler
            .get_importable_modules()
            .iter()
            .filter(|(name, m)| {
                *name != &module.name
                    && !already_imported.contains(*name)
                    && (m.origin.is_src() || !module.origin.is_src())
            })
            .map(|(name, _)| lsp::CompletionItem {
                label: name.to_string(),
                kind: Some(lsp::CompletionItemKind::MODULE),
                text_edit: {
                    Some(lsp::CompletionTextEdit::Edit(lsp::TextEdit {
                        range: lsp::Range { start, end },
                        new_text: name.to_string(),
                    }))
                },
                ..Default::default()
            })
            .collect()
    }

    fn root_package_name(&self) -> &str {
        self.compiler.project_compiler.config.name.as_str()
    }

    fn completion_variables<'b>(
        &'b self,
        module: &'b Module,
        byte_index: u32,
    ) -> Vec<lsp::CompletionItem> {
        let Some(fun) = module.ast.definitions.iter().find_map(|def| match def {
            Definition::Function(fun) if fun.full_location().contains(byte_index) => Some(fun),
            _ => None,
        }) else {
            return vec![];
        };

        let mut completions = BTreeMap::new();
        let mut variable_completion = |name: &str, ty: Arc<_>| {
            if name.is_empty() || name == "_" {
                return;
            }

            let type_ = Printer::new().pretty_print(&ty, 0);
            let completion_item = lsp::CompletionItem {
                label: name.to_string(),
                kind: Some(lsp::CompletionItemKind::VARIABLE),
                detail: Some(type_),
                documentation: None,
                ..Default::default()
            };

            let _ = completions.insert(EcoString::from(name), completion_item);
        };

        extract_arg_variables(&fun.arguments, &mut variable_completion);
        traverse_local_patterns(&fun.body, byte_index, &mut variable_completion);

        completions.into_values().collect()
    }
}

fn type_completion(
    module: Option<&EcoString>,
    name: &str,
    type_: &crate::type_::TypeConstructor,
) -> lsp::CompletionItem {
    let label = match module {
        Some(module) => format!("{module}.{name}"),
        None => name.to_string(),
    };

    let kind = Some(if type_.typ.is_variable() {
        lsp::CompletionItemKind::VARIABLE
    } else {
        lsp::CompletionItemKind::CLASS
    });

    lsp::CompletionItem {
        label,
        kind,
        detail: Some("Type".into()),
        ..Default::default()
    }
}

fn value_completion(
    module: Option<&str>,
    name: &str,
    value: &crate::type_::ValueConstructor,
) -> lsp::CompletionItem {
    let label = match module {
        Some(module) => format!("{module}.{name}"),
        None => name.to_string(),
    };

    let type_ = Printer::new().pretty_print(&value.type_, 0);

    let kind = Some(match value.variant {
        ValueConstructorVariant::LocalVariable { .. } => lsp::CompletionItemKind::VARIABLE,
        ValueConstructorVariant::ModuleConstant { .. } => lsp::CompletionItemKind::CONSTANT,
        ValueConstructorVariant::LocalConstant { .. } => lsp::CompletionItemKind::CONSTANT,
        ValueConstructorVariant::ModuleFn { .. } => lsp::CompletionItemKind::FUNCTION,
        ValueConstructorVariant::Record { arity: 0, .. } => lsp::CompletionItemKind::ENUM_MEMBER,
        ValueConstructorVariant::Record { .. } => lsp::CompletionItemKind::CONSTRUCTOR,
    });

    let documentation = value.get_documentation().map(|d| {
        lsp::Documentation::MarkupContent(lsp::MarkupContent {
            kind: lsp::MarkupKind::Markdown,
            value: d.to_string(),
        })
    });

    lsp::CompletionItem {
        label,
        kind,
        detail: Some(type_),
        documentation,
        ..Default::default()
    }
}

fn extract_arg_variables<F: FnMut(&str, Arc<Type>)>(args: &[Arg<Arc<Type>>], f: &mut F) {
    args.iter()
        .filter_map(|arg| Some((arg.names.get_variable_name()?.as_str(), arg.type_.clone())))
        .for_each(|(n, t)| f(n, t))
}

fn resolve_pattern<F: FnMut(&str, Arc<Type>)>(pattern: &Pattern<Arc<Type>>, f: &mut F) {
    match pattern {
        Pattern::Variable { name, type_, .. } | Pattern::Discard { name, type_, .. } => {
            f(name, type_.clone());
        }

        Pattern::Assign { name, pattern, .. } => {
            resolve_pattern(pattern, f);
            f(name, pattern.type_());
        }

        Pattern::List { elements, .. }
        | Pattern::Tuple {
            elems: elements, ..
        } => {
            for ele in elements {
                resolve_pattern(ele, f)
            }
        }

        Pattern::Constructor { arguments, .. } => {
            for arg in arguments {
                resolve_pattern(&arg.value, f)
            }
        }

        Pattern::BitArray { segments, .. } => {
            for seg in segments {
                resolve_pattern(&seg.value, f)
            }
        }
        Pattern::StringPrefix {
            right_side_assignment,
            ..
        } => f(right_side_assignment.name(), type_::string()),

        _ => {}
    }
}

fn traverse_expr_patterns<F: FnMut(&str, Arc<Type>)>(expr: &TypedExpr, byte_index: u32, f: &mut F) {
    if !expr.location().contains(byte_index) {
        return;
    }

    match expr {
        TypedExpr::Call { args, .. } => args
            .iter()
            .for_each(|arg| traverse_expr_patterns(&arg.value, byte_index, f)),
        TypedExpr::Block { statements, .. } => traverse_local_patterns(&statements, byte_index, f),

        TypedExpr::Pipeline { assignments, .. } => {
            for assig in assignments {
                traverse_expr_patterns(&assig.value, byte_index, f)
            }
        }
        TypedExpr::Fn { args, body, .. } => {
            extract_arg_variables(&args, f);
            traverse_local_patterns(&body, byte_index, f);
        }

        TypedExpr::List {
            elements: elems, ..
        }
        | TypedExpr::Tuple { elems, .. } => {
            for ele in elems {
                traverse_expr_patterns(ele, byte_index, f)
            }
        }

        TypedExpr::Case {
            subjects, clauses, ..
        } => {
            for expr in subjects {
                traverse_expr_patterns(expr, byte_index, f)
            }

            clauses
                .iter()
                .filter(|clause| clause.then.location().contains(byte_index))
                .for_each(|clause| {
                    for pat in &clause.pattern {
                        resolve_pattern(pat, f)
                    }

                    traverse_expr_patterns(&clause.then, byte_index, f);
                });
        }
        TypedExpr::RecordAccess { record: expr, .. }
        | TypedExpr::TupleIndex { tuple: expr, .. } => traverse_expr_patterns(expr, byte_index, f),
        TypedExpr::RecordUpdate { spread, args, .. } => {
            traverse_expr_patterns(&spread, byte_index, f);

            for arg in args.iter() {
                traverse_expr_patterns(&arg.value, byte_index, f)
            }
        }
        TypedExpr::NegateInt { value, .. } | TypedExpr::NegateBool { value, .. } => {
            traverse_expr_patterns(value, byte_index, f)
        }
        _ => {}
    }
}

fn traverse_local_patterns<F: FnMut(&str, Arc<Type>)>(
    stmts: &[Statement<Arc<Type>, TypedExpr>],
    byte_index: u32,
    f: &mut F,
) {
    stmts
        .into_iter()
        .filter(move |stmt| stmt.location().start < byte_index)
        .for_each(move |stmt| match stmt {
            Statement::Assignment(assignment) if !stmt.location().contains(byte_index) => {
                resolve_pattern(&assignment.pattern, f)
            }
            Statement::Assignment(assignment) => {
                traverse_expr_patterns(&assignment.value, byte_index, f)
            }

            Statement::Expression(expr) if stmt.location().end > byte_index => {
                traverse_expr_patterns(expr, byte_index, f)
            }
            _ => {}
        });
}

fn get_import(statement: &TypedDefinition) -> Option<&Import<EcoString>> {
    match statement {
        Definition::Import(import) => Some(import),
        _ => None,
    }
}

fn hover_for_pattern(pattern: &TypedPattern, line_numbers: LineNumbers) -> Hover {
    let documentation = pattern.get_documentation().unwrap_or_default();

    // Show the type of the hovered node to the user
    let type_ = Printer::new().pretty_print(pattern.type_().as_ref(), 0);
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

fn hover_for_function_head(
    fun: &Function<Arc<Type>, TypedExpr>,
    line_numbers: LineNumbers,
) -> Hover {
    let empty_str = EcoString::from("");
    let documentation = fun.documentation.as_ref().unwrap_or(&empty_str);
    let function_type = Type::Fn {
        args: fun.arguments.iter().map(|arg| arg.type_.clone()).collect(),
        retrn: fun.return_type.clone(),
    };
    let formatted_type = Printer::new().pretty_print(&function_type, 0);
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

fn hover_for_function_argument(argument: &Arg<Arc<Type>>, line_numbers: LineNumbers) -> Hover {
    let type_ = Printer::new().pretty_print(&argument.type_, 0);
    let contents = format!("```gleam\n{type_}\n```");
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(argument.location, &line_numbers)),
    }
}

fn hover_for_module_constant(
    constant: &ModuleConstant<Arc<Type>, EcoString>,
    line_numbers: LineNumbers,
) -> Hover {
    let empty_str = EcoString::from("");
    let type_ = Printer::new().pretty_print(&constant.type_, 0);
    let documentation = constant.documentation.as_ref().unwrap_or(&empty_str);
    let contents = format!("```gleam\n{type_}\n```\n{documentation}");
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(constant.location, &line_numbers)),
    }
}

fn hover_for_expression(
    expression: &TypedExpr,
    line_numbers: LineNumbers,
    module: Option<&Module>,
    hex_deps: &std::collections::HashSet<EcoString>,
) -> Hover {
    let documentation = expression.get_documentation().unwrap_or_default();

    let link_section = module
        .and_then(|m: &Module| {
            let (module_name, name) = get_expr_qualified_name(expression)?;
            get_hexdocs_link_section(module_name, name, &m.ast, hex_deps)
        })
        .unwrap_or("".to_string());

    // Show the type of the hovered node to the user
    let type_ = Printer::new().pretty_print(expression.type_().as_ref(), 0);
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

// Check if the inner range is included in the outer range.
fn range_includes(outer: &lsp_types::Range, inner: &lsp_types::Range) -> bool {
    (outer.start >= inner.start && outer.start <= inner.end)
        || (outer.end >= inner.start && outer.end <= inner.end)
}

fn code_action_unused_imports(
    module: &Module,
    params: &lsp::CodeActionParams,
    actions: &mut Vec<CodeAction>,
) {
    let uri = &params.text_document.uri;
    let unused = &module.ast.type_info.unused_imports;

    if unused.is_empty() {
        return;
    }

    // Convert src spans to lsp range
    let line_numbers = LineNumbers::new(&module.code);
    let mut hovered = false;
    let mut edits = Vec::with_capacity(unused.len());

    for unused in unused {
        let range = src_span_to_lsp_range(*unused, &line_numbers);
        // Keep track of whether any unused import has is where the cursor is
        hovered = hovered || range_includes(&params.range, &range);

        edits.push(lsp_types::TextEdit {
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

fn get_hexdocs_link_section(
    module_name: &str,
    name: &str,
    ast: &crate::ast::TypedModule,
    hex_deps: &std::collections::HashSet<EcoString>,
) -> Option<String> {
    let package_name = ast.definitions.iter().find_map(|def| match def {
        Definition::Import(p) if p.module == module_name && hex_deps.contains(&p.package) => {
            Some(&p.package)
        }
        _ => None,
    })?;

    let link = format!("https://hexdocs.pm/{package_name}/{module_name}.html#{name}");
    Some(format!("\nView on [HexDocs]({link})"))
}
