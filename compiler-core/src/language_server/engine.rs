use crate::{
    ast::{Import, Statement},
    build::{Located, Module, Target},
    config::PackageConfig,
    io::{CommandExecutor, FileSystemReader, FileSystemWriter},
    language_server::{
        compiler::LspProjectCompiler,
        feedback::{Feedback, FeedbackBookKeeper},
        files::FileSystemProxy,
        progress::ProgressReporter,
        Locker,
    },
    line_numbers::LineNumbers,
    manifest::Manifest,
    paths::ProjectPaths,
    type_::pretty::Printer,
    Error, Result, Warning,
};
use lsp::DidOpenTextDocumentParams;
use lsp_types::{
    self as lsp, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidSaveTextDocumentParams, Hover, HoverContents, MarkedString, Position, Range, TextEdit, Url,
};
use std::path::{Path, PathBuf};

use super::src_span_to_lsp_range;

#[derive(Debug, PartialEq, Eq)]
pub struct Response<T> {
    pub payload: Option<T>,
    pub feedback: Feedback,
}

#[derive(Debug)]
pub struct LanguageServerEngine<'a, IO, DepsDownloader, LockerMaker> {
    paths: ProjectPaths,

    /// A compiler for the project that supports repeat compilation of the root
    /// package.
    /// In the event the the project config changes this will need to be
    /// discarded and reloaded to handle any changes to dependencies.
    compiler: Option<LspProjectCompiler<FileSystemProxy<IO>>>,

    fs_proxy: FileSystemProxy<IO>,

    config: Option<PackageConfig>,

    feedback: FeedbackBookKeeper,
    modules_compiled_since_last_feedback: Vec<PathBuf>,

    // Used to publish progress notifications to the client without waiting for
    // the usual request-response loop.
    progress_reporter: ProgressReporter<'a>,

    // A function which downloads the dependencies of the project.
    // Used to ensure that the project is up to date when we are asked to create
    // a new compiler.
    dependencies_downloader: DepsDownloader,

    make_locker: LockerMaker,
}

impl<'a, IO, DepsDownloader, LockerMaker> LanguageServerEngine<'a, IO, DepsDownloader, LockerMaker>
where
    IO: FileSystemReader + FileSystemWriter + CommandExecutor + Clone,
    DepsDownloader: Fn(&ProjectPaths) -> Result<Manifest>,
    LockerMaker: Fn(&ProjectPaths, Target) -> Result<Box<dyn Locker>>,
{
    pub fn new(
        config: Option<PackageConfig>,
        progress_reporter: ProgressReporter<'a>,
        dependencies_downloader: DepsDownloader,
        fs_proxy: FileSystemProxy<IO>,
        make_locker: LockerMaker,
        paths: ProjectPaths,
    ) -> Result<Self> {
        let mut language_server = Self {
            modules_compiled_since_last_feedback: vec![],
            dependencies_downloader,
            progress_reporter,
            make_locker,
            fs_proxy,
            feedback: FeedbackBookKeeper::default(),
            compiler: None,
            config,
            paths,
        };
        language_server.create_new_compiler()?;
        Ok(language_server)
    }

    pub fn compile_please(&mut self) -> Feedback {
        self.notified(Self::compile)
    }

    /// Compile the project if we are in one. Otherwise do nothing.
    fn compile(&mut self) -> Result<(), Error> {
        self.progress_reporter.compilation_started();
        let result = match self.compiler.as_mut() {
            Some(compiler) => compiler.compile(),
            None => Ok(vec![]),
        };
        self.progress_reporter.compilation_finished();

        let modules = result?;
        self.modules_compiled_since_last_feedback
            .extend(modules.into_iter());

        Ok(())
    }

    fn take_warnings(&mut self) -> Vec<Warning> {
        if let Some(compiler) = self.compiler.as_mut() {
            compiler.take_warnings()
        } else {
            vec![]
        }
    }

    pub fn create_new_compiler(&mut self) -> Result<(), Error> {
        if let Some(config) = self.config.as_ref() {
            let locker = (self.make_locker)(&self.paths, config.target)?;

            // Download dependencies to ensure they are up-to-date for this new
            // configuration and new instance of the compiler
            self.progress_reporter.dependency_downloading_started();
            let manifest = (self.dependencies_downloader)(&self.paths);
            self.progress_reporter.dependency_downloading_finished();
            let manifest = manifest?;

            let compiler = LspProjectCompiler::new(
                manifest,
                config.clone(),
                self.paths.clone(),
                self.fs_proxy.clone(),
                locker,
            )?;
            self.compiler = Some(compiler);
        }
        Ok(())
    }

    pub fn text_document_did_open(&mut self, params: DidOpenTextDocumentParams) -> Feedback {
        self.notified(|this| {
            // A file opened in the editor which might be unsaved so store a copy of the new content in memory and compile
            let path = params.text_document.uri.path().to_string();
            this.fs_proxy
                .write_mem_cache(Path::new(path.as_str()), &params.text_document.text)?;
            this.compile()?;
            Ok(())
        })
    }

    pub fn text_document_did_save(&mut self, params: DidSaveTextDocumentParams) -> Feedback {
        self.notified(|this| {
            // The file is in sync with the file system, discard our cache of the changes
            this.fs_proxy
                .delete_mem_cache(Path::new(params.text_document.uri.path()))?;
            // The files on disc have changed, so compile the project with the new changes
            this.compile()?;
            Ok(())
        })
    }

    pub fn text_document_did_close(&mut self, params: DidCloseTextDocumentParams) -> Feedback {
        self.notified(|this| {
            // The file is in sync with the file system, discard our cache of the changes
            this.fs_proxy
                .delete_mem_cache(Path::new(params.text_document.uri.path()))?;
            Ok(())
        })
    }

    pub fn text_document_did_change(&mut self, params: DidChangeTextDocumentParams) -> Feedback {
        self.notified(|this| {
            // A file has changed in the editor so store a copy of the new content in memory and compile
            let path = params.text_document.uri.path().to_string();
            if let Some(changes) = params.content_changes.into_iter().next() {
                this.fs_proxy
                    .write_mem_cache(Path::new(path.as_str()), changes.text.as_str())?;
                this.compile()?;
            }
            Ok(())
        })
    }

    // TODO: test local variables
    // TODO: test same module constants
    // TODO: test imported module constants
    // TODO: test unqualified imported module constants
    // TODO: test same module records
    // TODO: test imported module records
    // TODO: test unqualified imported module records
    // TODO: test same module functions
    // TODO: test module function calls
    // TODO: test different package module function calls
    //
    //
    //
    // TODO: implement unqualified imported module functions
    // TODO: implement goto definition of modules that do not belong to the top
    // level package.
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
                    let module = match this
                        .compiler
                        .as_ref()
                        .and_then(|compiler| compiler.get_source(name))
                    {
                        Some(module) => module,
                        // TODO: support goto definition for functions defined in
                        // different packages. Currently it is not possible as the
                        // required LineNumbers and source file path information is
                        // not stored in the module metadata.
                        None => return Ok(None),
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

    // TODO: function & constructor labels
    // TODO: module types (including private)
    // TODO: module values (including private)
    // TODO: locally defined variables
    // TODO: imported module values
    // TODO: imported module types
    // TODO: record accessors
    pub fn completion(
        &mut self,
        params: lsp::CompletionParams,
    ) -> Response<Option<Vec<lsp::CompletionItem>>> {
        self.respond(|this| {
            let found = this
                .node_at_position(&params.text_document_position)
                .map(|(_, found)| found);

            Ok(match found {
                // TODO: test
                None | Some(Located::Statement(Statement::Import(Import { .. }))) => {
                    this.completion_for_import()
                }

                // TODO: autocompletion for other statements
                Some(Located::Statement(_expression)) => None,

                // TODO: autocompletion for expressions
                Some(Located::Expression(_expression)) => None,
            })
        })
    }

    fn respond<T>(&mut self, handler: impl FnOnce(&Self) -> Result<T>) -> Response<T> {
        let result = handler(self);
        let warnings = self.take_warnings();
        let modules = self.modules_compiled_since_last_feedback.drain(..);
        match result {
            Ok(payload) => Response {
                payload: Some(payload),
                feedback: self.feedback.diagnostics(modules, warnings),
            },
            Err(e) => Response {
                payload: None,
                feedback: self.feedback.diagnostics_with_error(e, modules, warnings),
            },
        }
    }

    fn notified(&mut self, handler: impl FnOnce(&mut Self) -> Result<()>) -> Feedback {
        let result = handler(self);
        let warnings = self.take_warnings();
        let modules = self.modules_compiled_since_last_feedback.drain(..);
        match result {
            Ok(()) => self.feedback.diagnostics(modules, warnings),
            Err(e) => self.feedback.diagnostics_with_error(e, modules, warnings),
        }
    }

    pub fn format(&mut self, params: lsp::DocumentFormattingParams) -> Response<Vec<TextEdit>> {
        self.respond(|this| {
            let path = params.text_document.uri.path();
            let mut new_text = String::new();

            let src = this.fs_proxy.read(Path::new(path))?.into();
            crate::format::pretty(&mut new_text, &src, Path::new(path))?;
            let line_count = src.lines().count() as u32;

            let edit = TextEdit {
                range: Range::new(Position::new(0, 0), Position::new(line_count, 0)),
                new_text,
            };
            Ok(vec![edit])
        })
    }

    fn completion_for_import(&self) -> Option<Vec<lsp::CompletionItem>> {
        let compiler = self.compiler.as_ref()?;
        // TODO: Test
        let dependencies_modules = compiler
            .project_compiler
            .get_importable_modules()
            .keys()
            .map(|name| name.to_string());
        // TODO: Test
        let project_modules = compiler
            .modules
            .iter()
            // TODO: We should autocomplete test modules if we are in the test dir
            // TODO: Test
            .filter(|(_name, module)| module.origin.is_src())
            .map(|(name, _module)| name)
            .cloned();
        let modules = dependencies_modules
            .chain(project_modules)
            .map(|label| lsp::CompletionItem {
                label,
                kind: None,
                documentation: None,
                ..Default::default()
            })
            .collect();
        Some(modules)
    }

    pub fn hover(&mut self, params: lsp::HoverParams) -> Response<Option<Hover>> {
        self.respond(|this| {
            let params = params.text_document_position_params;

            let (line_numbers, found) = match this.node_at_position(&params) {
                Some(value) => value,
                None => return Ok(None),
            };

            let expression = match found {
                Located::Expression(expression) => expression,
                Located::Statement(_) => return Ok(None),
            };

            // Show the type of the hovered node to the user
            let type_ = Printer::new().pretty_print(expression.type_().as_ref(), 0);
            let contents = format!(
                "```gleam
{type_}
```"
            );
            Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(contents)),
                range: Some(src_span_to_lsp_range(expression.location(), &line_numbers)),
            }))
        })
    }

    fn node_at_position(
        &self,
        params: &lsp::TextDocumentPositionParams,
    ) -> Option<(LineNumbers, Located<'_>)> {
        let module = self.module_for_uri(&params.text_document.uri);
        let module = module?;
        let line_numbers = LineNumbers::new(&module.code);
        let byte_index = line_numbers.byte_index(params.position.line, params.position.character);
        let node = module.find_node(byte_index);
        let node = node?;
        Some((line_numbers, node))
    }

    fn module_for_uri(&self, uri: &Url) -> Option<&Module> {
        self.compiler.as_ref().and_then(|compiler| {
            let module_name =
                uri_to_module_name(uri, self.paths.root()).expect("uri to module name");
            compiler.modules.get(&module_name)
        })
    }
}

// TODO: Fix this rubbish.
#[cfg(target_os = "windows")]
fn uri_to_module_name(uri: &Url, root: &Path) -> Option<String> {
    use itertools::Itertools;
    use urlencoding::decode;

    let mut uri_path = decode(&*uri.path().replace('/', "\\"))
        .expect("Invalid formatting")
        .to_string();
    if uri_path.starts_with("\\") {
        uri_path = uri_path
            .strip_prefix("\\")
            .expect("Failed to remove \"\\\" prefix")
            .to_string();
    }
    let path = PathBuf::from(uri_path);
    let components = path
        .strip_prefix(&root)
        .ok()?
        .components()
        .skip(1)
        .map(|c| c.as_os_str().to_string_lossy());
    let module_name = Itertools::intersperse(components, "/".into())
        .collect::<String>()
        .strip_suffix(".gleam")?
        .to_string();
    tracing::info!("(uri_to_module_name) module_name: {}", module_name);
    Some(module_name)
}

#[test]
#[cfg(target_os = "windows")]
fn uri_to_module_name_test() {
    let root = PathBuf::from("/projects/app");
    let uri = Url::parse("file:///b%3A/projects/app/src/one/two/three.rs").unwrap();
    assert_eq!(uri_to_module_name(&uri, &root), None);

    let root = PathBuf::from("/projects/app");
    let uri = Url::parse("file:///c%3A/projects/app/src/one/two/three.rs").unwrap();
    assert_eq!(uri_to_module_name(&uri, &root), None);
}

#[cfg(not(target_os = "windows"))]
fn uri_to_module_name(uri: &Url, root: &Path) -> Option<String> {
    use itertools::Itertools;

    let path = PathBuf::from(uri.path());
    let components = path
        .strip_prefix(root)
        .ok()?
        .components()
        .skip(1)
        .map(|c| c.as_os_str().to_string_lossy());
    let module_name = Itertools::intersperse(components, "/".into())
        .collect::<String>()
        .strip_suffix(".gleam")?
        .to_string();
    Some(module_name)
}

#[test]
#[cfg(not(target_os = "windows"))]
fn uri_to_module_name_test() {
    let root = PathBuf::from("/projects/app");
    let uri = Url::parse("file:///projects/app/src/one/two/three.gleam").unwrap();
    assert_eq!(
        uri_to_module_name(&uri, &root),
        Some("one/two/three".into())
    );

    let root = PathBuf::from("/projects/app");
    let uri = Url::parse("file:///projects/app/test/one/two/three.gleam").unwrap();
    assert_eq!(
        uri_to_module_name(&uri, &root),
        Some("one/two/three".into())
    );

    let root = PathBuf::from("/projects/app");
    let uri = Url::parse("file:///somewhere/else/src/one/two/three.gleam").unwrap();
    assert_eq!(uri_to_module_name(&uri, &root), None);

    let root = PathBuf::from("/projects/app");
    let uri = Url::parse("file:///projects/app/src/one/two/three.rs").unwrap();
    assert_eq!(uri_to_module_name(&uri, &root), None);
}
