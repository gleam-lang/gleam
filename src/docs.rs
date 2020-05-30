pub(crate) mod command;
#[cfg(test)]
mod tests;

use crate::{
    ast::{Statement, TypedStatement},
    config::{DocsPage, PackageConfig},
    error::{Error, GleamExpect},
    format, pretty,
    project::{self, Analysed, ModuleOrigin, OutputFile},
};
use askama::Template;
use itertools::Itertools;
use std::path::{Path, PathBuf};

const MAX_COLUMNS: isize = 65;

pub fn build_project(
    project_root: impl AsRef<Path>,
    output_dir: &PathBuf,
) -> Result<(PackageConfig, Vec<OutputFile>), Error> {
    // Read and type check project
    let (config, analysed) = project::read_and_analyse(&project_root)?;

    // Initialize pages with the README
    let mut pages = vec![DocsPage {
        title: "README".to_string(),
        path: "index.html".to_string(),
        source: project_root.as_ref().join("README.md"),
    }];

    // Add any user-supplied pages
    pages.extend(config.docs.pages.to_vec());

    // Generate HTML
    let outputs = generate_html(&config, analysed.as_slice(), &pages, &output_dir);
    Ok((config, outputs))
}

pub fn generate_html(
    project_config: &PackageConfig,
    analysed: &[Analysed],
    docspages: &[DocsPage],
    output_dir: &PathBuf,
) -> Vec<OutputFile> {
    let modules = analysed.iter().filter(|m| m.origin == ModuleOrigin::Src);

    // Define user-supplied (or README) pages
    let pages = docspages
        .iter()
        .map(|page| Link {
            name: page.title.to_string(),
            path: page.path.to_string(),
        })
        .collect::<Vec<_>>();

    let links = &[];

    // index.css
    let num_asset_files = 1;
    let mut files = Vec::with_capacity(analysed.len() + pages.len() + 1 + num_asset_files);

    let mut modules_links: Vec<_> = modules
        .clone()
        .map(|m| {
            let name = m.name.join("/");
            let path = [&name, "/"].concat();
            Link { path, name }
        })
        .collect();
    modules_links.sort();

    // Generate user-supplied (or README) pages
    for page in docspages {
        let content = std::fs::read_to_string(&page.source).unwrap_or_default();

        let temp = PageTemplate {
            unnest: ".".to_string(),
            links,
            pages: &pages,
            modules: &modules_links,
            project_name: &project_config.name,
            page_title: &project_config.name,
            project_version: "", // TODO
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
        let template = ModuleTemplate {
            unnest: module.name.iter().map(|_| "..").intersperse("/").collect(),
            links,
            pages: &pages,
            documentation: render_markdown(module.ast.documentation.iter().join("\n").as_str()),
            modules: modules_links.as_slice(),
            project_name: &project_config.name,
            page_title: &format!("{} - {}", name, project_config.name),
            module_name: name,
            project_version: "", // TODO
            functions: {
                let mut f: Vec<_> = module.ast.statements.iter().flat_map(function).collect();
                f.sort();
                f
            },
            types: {
                let mut t: Vec<_> = module.ast.statements.iter().flat_map(type_).collect();
                t.sort();
                t
            },
        };

        let mut path = output_dir.clone();
        for segment in module.name.iter() {
            path.push(segment)
        }
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

    files
}

fn function<'a>(statement: &'a TypedStatement) -> Option<Function<'a>> {
    let mut formatter = format::Formatter::new();
    match statement {
        Statement::ExternalFn {
            public: true,
            name,
            doc,
            retrn,
            args,
            ..
        } => Some(Function {
            name,
            signature: print(formatter.external_fn_signature(true, name, args, retrn)),
            documentation: markdown_documentation(doc),
        }),

        Statement::Fn {
            public: true,
            name,
            doc,
            args,
            return_type: ret,
            ..
        } => Some(Function {
            name,
            documentation: markdown_documentation(doc),
            signature: print(formatter.docs_fn_signature(true, name, args, ret.clone())),
        }),

        _ => None,
    }
}

fn markdown_documentation(doc: &Option<String>) -> String {
    match doc {
        None => "".to_string(),
        Some(d) => render_markdown(d),
    }
}

fn render_markdown(text: &str) -> String {
    let mut s = String::with_capacity(text.len() * 3 / 2);
    let p = pulldown_cmark::Parser::new(&*text);
    pulldown_cmark::html::push_html(&mut s, p);
    s
}

fn type_<'a>(statement: &'a TypedStatement) -> Option<Type<'a>> {
    let mut formatter = format::Formatter::new();
    match statement {
        Statement::ExternalType {
            public: true,
            name,
            doc,
            args,
            ..
        } => Some(Type {
            name,
            definition: print(formatter.external_type(true, name.as_str(), args)),
            documentation: markdown_documentation(doc),
            constructors: vec![],
        }),

        Statement::CustomType {
            public: true,
            opaque: false,
            name,
            args,
            doc,
            constructors: cs,
            location,
            ..
        } => Some(Type {
            name,
            // TODO: Don't use the same printer for docs as for the formatter
            definition: print(formatter.custom_type(
                true,
                false,
                name,
                args,
                cs.as_slice(),
                location,
            )),
            documentation: markdown_documentation(doc),
            constructors: cs
                .into_iter()
                .map(|constructor| TypeConstructor {
                    definition: print(formatter.record_constructor(constructor)),
                    documentation: markdown_documentation(&constructor.documentation),
                })
                .collect(),
        }),

        Statement::CustomType {
            public: true,
            opaque: true,
            name,
            args,
            doc,
            location,
            ..
        } => Some(Type {
            name,
            definition: print(formatter.docs_opaque_custom_type(true, name, args, location)),
            documentation: markdown_documentation(doc),
            constructors: vec![],
        }),

        Statement::TypeAlias {
            public: true,
            alias: name,
            resolved_type: typ,
            doc,
            args,
            ..
        } => Some(Type {
            name,
            definition: print(formatter.type_alias(true, name, args, typ)),
            documentation: markdown_documentation(doc),
            constructors: vec![],
        }),

        _ => None,
    }
}

fn print(doc: pretty::Document) -> String {
    pretty::format(MAX_COLUMNS, doc)
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Link {
    name: String,
    path: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Function<'a> {
    name: &'a str,
    signature: String,
    documentation: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct TypeConstructor {
    definition: String,
    documentation: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct Type<'a> {
    name: &'a str,
    definition: String,
    documentation: String,
    constructors: Vec<TypeConstructor>,
}

#[derive(Template)]
#[template(path = "documentation_page.html")]
struct PageTemplate<'a> {
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
    documentation: String,
}
