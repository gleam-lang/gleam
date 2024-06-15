use crate::{
    ast::{
        Arg, Definition, ModuleConstant, SrcSpan, TypedExpr, TypedFunction, TypedModule,
        TypedPattern,
    },
    build::{type_constructor_from_modules, Located, Module, UnqualifiedImport},
    config::PackageConfig,
    io::{CommandExecutor, FileSystemReader, FileSystemWriter},
    language_server::{
        compiler::LspProjectCompiler, files::FileSystemProxy, progress::ProgressReporter,
    },
    line_numbers::LineNumbers,
    paths::ProjectPaths,
    type_::{pretty::Printer, ModuleInterface, Type, TypeConstructor, ValueConstructorVariant},
    Error, Result, Warning,
};
use camino::Utf8PathBuf;
use ecow::EcoString;
use lsp::CodeAction;
use lsp_types::{self as lsp, Hover, HoverContents, MarkedString, Url};
use std::sync::Arc;

use super::{
    code_action::{CodeActionBuilder, RedundantTupleInCaseSubject},
    completer::Completer,
    src_span_to_lsp_range, DownloadDependencies, MakeLocker,
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
        let outcome = self.compiler.compile();
        self.progress_reporter.compilation_finished();

        outcome
            // Register which modules have changed
            .map(|modules| self.modules_compiled_since_last_feedback.extend(modules))
            // Return the error, if present
            .into_result()
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

            // Check current filercontents if the user is writing an import
            // and handle separately from the rest of the completion flow
            // Check if an import is being written
            if let Some(value) = completer.import_completions() {
                return value;
            }

            let byte_index = completer
                .module_line_numbers
                .byte_index(params.position.line, params.position.character);

            let Some(found) = module.find_node(byte_index) else {
                return Ok(None);
            };

            let completions = match found {
                Located::Pattern(_pattern) => None,

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
                    .get_module_inferface(import.module.as_str())
                    .map(|importing_module| {
                        completer.unqualified_completions_from_module(importing_module)
                    }),

                Located::ModuleStatement(Definition::ModuleConstant(_)) => None,

                Located::UnqualifiedImport(_) => None,

                Located::Arg(_) => None,

                Located::Annotation(_, _) => Some(completer.completion_types()),
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
            actions.extend(RedundantTupleInCaseSubject::new(module, &params).code_actions());

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
                Located::UnqualifiedImport(UnqualifiedImport {
                    name,
                    module,
                    is_type,
                    location,
                }) => this
                    .compiler
                    .get_module_inferface(module.as_str())
                    .and_then(|module| {
                        if is_type {
                            module.types.get(name).map(|t| {
                                hover_for_annotation(*location, t.typ.as_ref(), Some(t), lines)
                            })
                        } else {
                            module.values.get(name).map(|v| {
                                let m = if this.hex_deps.contains(&module.package) {
                                    Some(module)
                                } else {
                                    None
                                };
                                hover_for_imported_value(v, location, lines, m, name)
                            })
                        }
                    }),
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
                    ))
                }
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

fn hover_for_function_head(fun: &TypedFunction, line_numbers: LineNumbers) -> Hover {
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

fn hover_for_annotation(
    location: SrcSpan,
    annotation_type: &Type,
    type_constructor: Option<&TypeConstructor>,
    line_numbers: LineNumbers,
) -> Hover {
    let empty_str = EcoString::from("");
    let documentation = type_constructor
        .and_then(|t| t.documentation.as_ref())
        .unwrap_or(&empty_str);
    let type_ = Printer::new().pretty_print(annotation_type, 0);
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

fn hover_for_imported_value(
    value: &crate::type_::ValueConstructor,
    location: &SrcSpan,
    line_numbers: LineNumbers,
    hex_module_imported_from: Option<&ModuleInterface>,
    name: &EcoString,
) -> Hover {
    let documentation = value.get_documentation().unwrap_or_default();

    let link_section = hex_module_imported_from.map_or("".to_string(), |m| {
        format_hexdocs_link_section(m.package.as_str(), m.name.as_str(), name)
    });

    // Show the type of the hovered node to the user
    let type_ = Printer::new().pretty_print(value.type_.as_ref(), 0);
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
pub fn overlaps(a: lsp_types::Range, b: lsp_types::Range) -> bool {
    within(a.start, b) || within(a.end, b) || within(b.start, a) || within(b.end, a)
}

// Returns true if a position is within a range
fn within(position: lsp_types::Position, range: lsp_types::Range) -> bool {
    position >= range.start && position < range.end
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
