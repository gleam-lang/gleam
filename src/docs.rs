pub(crate) mod command;
mod source_links;
#[cfg(test)]
mod tests;

use crate::{
    ast::{Statement, TypedStatement},
    config::{DocsPage, PackageConfig},
    docs::source_links::SourceLinker,
    error::{Error, GleamExpect},
    format,
    fs::OutputFile,
    pretty,
    project::{self, Analysed, ModuleOrigin},
};
use askama::Template;
use itertools::Itertools;
use std::path::Path;

const MAX_COLUMNS: isize = 65;
const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn build_project(
    project_root: impl AsRef<Path>,
    version: String,
    output_dir: &Path,
) -> Result<(PackageConfig, Vec<OutputFile>), Error> {
    // Read and type check project
    let (mut config, analysed) = project::read_and_analyse(&project_root)?;
    config.version = version;
    check_app_file_version_matches(&project_root, &config)?;

    // Attach documentation to Src modules
    let analysed: Vec<_> = analysed
        .into_iter()
        .filter(|a| a.origin == ModuleOrigin::Src)
        .map(|mut a| {
            a.attach_doc_and_module_comments();
            a
        })
        .collect();

    // Initialize pages with the README
    let mut pages = vec![DocsPage {
        title: "README".to_string(),
        path: "index.html".to_string(),
        source: project_root.as_ref().join("README.md"),
    }];

    // Add any user-supplied pages
    pages.extend(config.docs.pages.iter().cloned());

    // Generate HTML
    let outputs = generate_html(project_root, &config, &analysed, &pages, output_dir);
    Ok((config, outputs))
}

pub fn generate_html(
    project_root: impl AsRef<Path>,
    project_config: &PackageConfig,
    analysed: &[Analysed],
    docspages: &[DocsPage],
    output_dir: &Path,
) -> Vec<OutputFile> {
    let modules = analysed.iter();

    // Define user-supplied (or README) pages
    let pages: Vec<_> = docspages
        .iter()
        .map(|page| Link {
            name: page.title.to_string(),
            path: page.path.to_string(),
        })
        .collect();

    let doc_links = project_config.docs.links.iter().map(|doc_link| Link {
        name: doc_link.title.to_string(),
        path: doc_link.href.to_string(),
    });

    let repo_link = project_config.repository.url().map(|path| Link {
        name: "Repository".to_string(),
        path,
    });

    let links: Vec<_> = doc_links.chain(repo_link).collect();

    // index.css
    // highlightjs-gleam.js
    let num_asset_files = 2;
    let mut files = Vec::with_capacity(analysed.len() + pages.len() + 1 + num_asset_files);

    let modules_links: Vec<_> = modules
        .clone()
        .map(|m| {
            let name = m.name.join("/");
            let path = [&name, "/"].concat();
            Link { path, name }
        })
        .sorted()
        .collect();

    // Generate user-supplied (or README) pages
    for page in docspages {
        let content = std::fs::read_to_string(&page.source).unwrap_or_default();

        let temp = PageTemplate {
            gleam_version: VERSION,
            unnest: ".".to_string(),
            links: &links,
            pages: &pages,
            modules: &modules_links,
            project_name: &project_config.name,
            page_title: &project_config.name,
            project_version: &project_config.version,
            content: render_markdown(&content),
        };

        files.push(OutputFile {
            path: output_dir.join(&page.path),
            text: temp.render().gleam_expect("Page template rendering"),
        });
    }

    // Generate module documentation pages
    for module in modules {
        let name = module.name.join("/");

        // Read module src & create line number lookup structure
        let source_links = SourceLinker::new(&project_root, project_config, module);

        let template = ModuleTemplate {
            gleam_version: VERSION,
            unnest: Itertools::intersperse(module.name.iter().map(|_| ".."), "/").collect(),
            links: &links,
            pages: &pages,
            documentation: render_markdown(&module.ast.documentation.iter().join("\n")),
            modules: &modules_links,
            project_name: &project_config.name,
            page_title: &format!("{} - {}", name, project_config.name),
            module_name: name,
            project_version: &project_config.version,
            functions: module
                .ast
                .statements
                .iter()
                .flat_map(|statement| function(&source_links, statement))
                .sorted()
                .collect(),
            types: module
                .ast
                .statements
                .iter()
                .flat_map(|statement| type_(&source_links, statement))
                .sorted()
                .collect(),
            constants: module
                .ast
                .statements
                .iter()
                .flat_map(|statement| constant(&source_links, statement))
                .sorted()
                .collect(),
        };

        let mut path = output_dir.to_path_buf();
        path.extend(&module.name);
        path.push("index.html");
        files.push(OutputFile {
            path,
            text: template
                .render()
                .gleam_expect("Module documentation template rendering"),
        });
    }

    // Render static assets
    files.push(OutputFile {
        path: output_dir.join("index.css"),
        text: std::include_str!("../templates/index.css").to_string(),
    });

    files.push(OutputFile {
        path: output_dir.join("gleam.js"),
        text: std::include_str!("../templates/gleam.js").to_string(),
    });

    files.push(OutputFile {
        path: output_dir.join("highlightjs-gleam.js"),
        text: std::include_str!("../templates/highlightjs-gleam.js").to_string(),
    });

    files
}

fn function<'a>(
    source_links: &SourceLinker,
    statement: &'a TypedStatement,
) -> Option<Function<'a>> {
    let mut formatter = format::Formatter::new();

    match statement {
        Statement::ExternalFn {
            public: true,
            name,
            doc,
            return_: retrn,
            arguments: args,
            location,
            ..
        } => Some(Function {
            name,
            documentation: markdown_documentation(doc),
            signature: print(formatter.external_fn_signature(true, name, args, retrn)),
            source_url: source_links.url(location),
        }),

        Statement::Fn {
            public: true,
            name,
            doc,
            arguments: args,
            return_type: ret,
            location,
            ..
        } => Some(Function {
            name,
            documentation: markdown_documentation(doc),
            signature: print(formatter.docs_fn_signature(true, name, args, ret.clone())),
            source_url: source_links.url(location),
        }),

        _ => None,
    }
}

fn markdown_documentation(doc: &Option<String>) -> String {
    doc.as_deref().map(render_markdown).unwrap_or_default()
}

fn render_markdown(text: &str) -> String {
    let mut s = String::with_capacity(text.len() * 3 / 2);
    let p = pulldown_cmark::Parser::new_ext(text, pulldown_cmark::Options::all());
    pulldown_cmark::html::push_html(&mut s, p);
    s
}

fn type_<'a>(source_links: &SourceLinker, statement: &'a TypedStatement) -> Option<Type<'a>> {
    let mut formatter = format::Formatter::new();

    match statement {
        Statement::ExternalType {
            public: true,
            name,
            doc,
            arguments: args,
            location,
            ..
        } => Some(Type {
            name,
            definition: print(formatter.external_type(true, name, args)),
            documentation: markdown_documentation(doc),
            constructors: vec![],
            source_url: source_links.url(location),
        }),

        Statement::CustomType {
            public: true,
            opaque: false,
            name,
            parameters,
            doc,
            constructors: cs,
            location,
            ..
        } => Some(Type {
            name,
            // TODO: Don't use the same printer for docs as for the formatter
            definition: print(formatter.custom_type(true, false, name, parameters, cs, location)),
            documentation: markdown_documentation(doc),
            constructors: cs
                .iter()
                .map(|constructor| TypeConstructor {
                    definition: print(formatter.record_constructor(constructor)),
                    documentation: markdown_documentation(&constructor.documentation),
                    arguments: constructor
                        .arguments
                        .iter()
                        .filter_map(|arg| {
                            if let Some(label) = arg.label.clone() {
                                Some((arg, label))
                            } else {
                                None
                            }
                        })
                        .map(|(argument, label)| TypeConstructorArg {
                            name: label.trim_end().to_string(),
                            doc: markdown_documentation(&argument.doc),
                        })
                        .filter(|arg| !arg.doc.is_empty())
                        .collect(),
                })
                .collect(),
            source_url: source_links.url(location),
        }),

        Statement::CustomType {
            public: true,
            opaque: true,
            name,
            parameters,
            doc,
            location,
            ..
        } => Some(Type {
            name,
            definition: print(formatter.docs_opaque_custom_type(true, name, parameters, location)),
            documentation: markdown_documentation(doc),
            constructors: vec![],
            source_url: source_links.url(location),
        }),

        Statement::TypeAlias {
            public: true,
            alias: name,
            type_ast: typ,
            doc,
            parameters: args,
            location,
            ..
        } => Some(Type {
            name,
            definition: print(formatter.type_alias(true, name, args, typ)),
            documentation: markdown_documentation(doc),
            constructors: vec![],
            source_url: source_links.url(location),
        }),

        _ => None,
    }
}

fn constant<'a>(
    source_links: &SourceLinker,
    statement: &'a TypedStatement,
) -> Option<Constant<'a>> {
    let mut formatter = format::Formatter::new();
    match statement {
        Statement::ModuleConstant {
            public: true,
            doc,
            name,
            value,
            location,
            ..
        } => Some(Constant {
            name,
            definition: print(formatter.docs_const_expr(true, name, value)),
            documentation: markdown_documentation(doc),
            source_url: source_links.url(location),
        }),

        _ => None,
    }
}

fn print(doc: pretty::Document<'_>) -> String {
    doc.to_pretty_string(MAX_COLUMNS)
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Link {
    name: String,
    path: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Function<'a> {
    name: &'a str,
    signature: String,
    documentation: String,
    source_url: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct TypeConstructor {
    definition: String,
    documentation: String,
    arguments: Vec<TypeConstructorArg>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct TypeConstructorArg {
    name: String,
    doc: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct Type<'a> {
    name: &'a str,
    definition: String,
    documentation: String,
    constructors: Vec<TypeConstructor>,
    source_url: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Constant<'a> {
    name: &'a str,
    definition: String,
    documentation: String,
    source_url: String,
}

#[derive(Template)]
#[template(path = "documentation_page.html")]
struct PageTemplate<'a> {
    gleam_version: &'a str,
    unnest: String,
    page_title: &'a str,
    project_name: &'a str,
    project_version: &'a str,
    pages: &'a [Link],
    links: &'a [Link],
    modules: &'a [Link],
    content: String,
}

#[derive(Template)]
#[template(path = "documentation_module.html")]
struct ModuleTemplate<'a> {
    gleam_version: &'a str,
    unnest: String,
    page_title: &'a str,
    module_name: String,
    project_name: &'a str,
    project_version: &'a str,
    pages: &'a [Link],
    links: &'a [Link],
    modules: &'a [Link],
    functions: Vec<Function<'a>>,
    types: Vec<Type<'a>>,
    constants: Vec<Constant<'a>>,
    documentation: String,
}

fn check_app_file_version_matches(
    root: impl AsRef<Path>,
    project_config: &PackageConfig,
) -> Result<(), Error> {
    let mut app_src_path = root.as_ref().to_path_buf();
    app_src_path.push("src");
    app_src_path.push(format!("{}.app.src", &project_config.name));

    let re = regex::Regex::new("\\{ *vsn *, *\"([^\"]*)\" *\\}")
        .gleam_expect("Could not compile vsn regex");

    std::fs::read_to_string(&app_src_path)
        // Remove all new lines so we can regex easily across the content
        .map(|contents| contents.replace("\n", ""))
        .ok()
        .and_then(|contents| {
            // Extract the vsn if we can
            re.captures(&contents)
                .and_then(|captures| captures.get(1))
                .map(|capture| capture.as_str().to_string())
        })
        .map(|version| {
            if version == project_config.version {
                Ok(())
            } else {
                // Error if we've found the version and it doesn't match
                Err(Error::VersionDoesNotMatch {
                    toml_ver: project_config.version.clone(),
                    app_ver: version,
                })
            }
        })
        // Don't mind if we never found the version
        .unwrap_or(Ok(()))
}
