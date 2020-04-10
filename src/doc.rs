use crate::{
    error::GleamExpect,
    project::{Analysed, ModuleOrigin, OutputFile, ProjectConfig},
};
use askama::Template;
use std::path::PathBuf;

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
            let path = format!("{}/index.html", name);
            Link { path, name }
        })
        .collect();
    let pages = &[Link {
        name: "README".to_string(),
        path: "index.html".to_string(),
    }];
    let links = &[];

    // Generate README page
    let readme = PageTemplate {
        links,
        pages,
        content: "", // TODO
        project_name: &project_config.name,
        page_title: &project_config.name, // TODO
        project_version: "",              // TODO
    };
    files.push(OutputFile {
        path: dir.join("index.html"),
        text: readme.render().gleam_expect("README template rendering"),
    });

    // Generate module documentation pages
    for module in modules {
        let template = ModuleTemplate {
            links,
            pages,
            module_name: module.name.join("/"),
            documentation: "",
            modules: modules_links.as_slice(),
            project_name: &project_config.name,
            page_title: &project_config.name, // TODO
            project_version: "",              // TODO
            functions: &[],
            types: &[],
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
}

struct Link {
    name: String,
    path: String,
}

struct Function<'a> {
    name: &'a str,
    signature: &'a str,
    documentation: &'a str,
}

struct Type<'a> {
    name: &'a str,
    definition: &'a str,
    documentation: &'a str,
}

#[derive(Template)]
#[template(path = "documentation_page.html")]
struct PageTemplate<'a> {
    page_title: &'a str,
    project_name: &'a str,
    project_version: &'a str,
    pages: &'a [Link],
    links: &'a [Link],
    content: &'a str,
}

#[derive(Template)]
#[template(path = "documentation_module.html")]
struct ModuleTemplate<'a> {
    page_title: &'a str,
    module_name: String,
    project_name: &'a str,
    project_version: &'a str,
    pages: &'a [Link],
    links: &'a [Link],
    modules: &'a [Link],
    functions: &'a [Function<'a>],
    types: &'a [Type<'a>],
    documentation: &'a str,
}
