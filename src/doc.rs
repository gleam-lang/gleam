use crate::{
    ast::{Statement, TypedStatement},
    error::GleamExpect,
    format, pretty,
    project::{Analysed, ModuleOrigin, OutputFile, ProjectConfig},
};
use askama::Template;
use itertools::Itertools;
use std::path::PathBuf;

const MAX_COLUMNS: isize = 65;

pub fn generate_html(
    project_config: &ProjectConfig,
    analysed: &[Analysed],
    files: &mut Vec<OutputFile>,
    dir: &PathBuf,
) {
    let modules = analysed.iter().filter(|m| m.origin == ModuleOrigin::Src);

    let modules_links: Vec<_> = modules
        .clone()
        .map(|m| {
            let name = m.name.join("/");
            let path = name.clone();
            Link { path, name }
        })
        .collect();
    let pages = &[Link {
        name: "README".to_string(),
        path: "".to_string(),
    }];
    let links = &[];

    // Generate README page
    let readme = PageTemplate {
        unnest: ".".to_string(),
        links,
        pages,
        modules: &modules_links,
        content: "", // TODO
        project_name: &project_config.name,
        page_title: &project_config.name,
        project_version: "", // TODO
    };
    files.push(OutputFile {
        path: dir.join("index.html"),
        text: readme.render().gleam_expect("README template rendering"),
    });

    // Generate module documentation pages
    for module in modules {
        let template = ModuleTemplate {
            unnest: module.name.iter().map(|_| "..").intersperse("/").collect(),
            links,
            pages,
            module_name: module.name.join("/"),
            documentation: "",
            modules: modules_links.as_slice(),
            project_name: &project_config.name,
            page_title: &project_config.name,
            project_version: "", // TODO
            functions: module.ast.statements.iter().flat_map(function).collect(),
            types: module.ast.statements.iter().flat_map(type_).collect(),
        };
        let mut path = dir.clone();
        for segment in module.name.iter() {
            path.push(segment);
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
        path: dir.join("index.css"),
        text: std::include_str!("../templates/index.css").to_string(),
    });
}

fn function<'a>(statement: &'a TypedStatement) -> Option<Function<'a>> {
    match statement {
        Statement::ExternalFn {
            public: true,
            name,
            doc,
            ..
        } => Some(Function {
            name,
            signature: "".to_string(),
            documentation: markdown_documentation(doc),
        }),

        Statement::Fn {
            public: true,
            name,
            doc,
            ..
        } => Some(Function {
            name,
            signature: "".to_string(),
            documentation: markdown_documentation(doc),
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
    match statement {
        Statement::ExternalType {
            public: true,
            name,
            doc,
            args,
            ..
        } => Some(Type {
            name,
            definition: print(format::external_type(true, name.as_str(), args)),
            documentation: markdown_documentation(doc),
        }),

        Statement::CustomType {
            public: true,
            name,
            args,
            doc,
            constructors: cs,
            ..
        } => Some(Type {
            name,
            definition: print(format::custom_type(true, name, args, cs.as_slice())),
            documentation: markdown_documentation(doc),
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
            definition: print(format::type_alias(true, name, args, typ)),
            documentation: markdown_documentation(doc),
        }),

        _ => None,
    }
}

fn print(doc: pretty::Document) -> String {
    pretty::format(MAX_COLUMNS, doc)
}

struct Link {
    name: String,
    path: String,
}

struct Function<'a> {
    name: &'a str,
    signature: String,
    documentation: String,
}

struct Type<'a> {
    name: &'a str,
    definition: String,
    documentation: String,
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
    content: &'a str,
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
    documentation: &'a str,
}
