use gleam_core::{
    config::PackageConfig,
    error::{Error, FileIoAction, FileKind},
    project::{Analysed, Input, ModuleOrigin},
};
use std::path::{Path, PathBuf};

use crate::config;

pub const OUTPUT_DIR_NAME: &str = "gen";

pub fn read_and_analyse(root: impl AsRef<Path>) -> Result<(PackageConfig, Vec<Analysed>), Error> {
    let project_config = config::read(root.as_ref().join("gleam.toml"))?;
    let mut srcs = vec![];
    let root = root.as_ref();
    let lib_dir = root.join("_build").join("default").join("lib");
    let checkouts_dir = root.join("_checkouts");
    let mix_lib_dir = root.join("deps");

    for project_dir in [lib_dir, checkouts_dir, mix_lib_dir]
        .iter()
        .filter_map(|d| std::fs::read_dir(d).ok())
        .flat_map(|d| d.filter_map(Result::ok))
        .map(|d| d.path())
        .filter(|p| {
            p.file_name().and_then(|os_string| os_string.to_str()) != Some(&project_config.name)
        })
    {
        collect_source(project_dir.join("src"), ModuleOrigin::Dependency, &mut srcs)?;
    }

    // Collect source code from top level project
    collect_source(root.join("src"), ModuleOrigin::Src, &mut srcs)?;
    collect_source(root.join("test"), ModuleOrigin::Test, &mut srcs)?;

    // Analyse source
    let analysed = gleam_core::project::analysed(srcs)?;

    Ok((project_config, analysed))
}

pub fn collect_source(
    src_dir: PathBuf,
    origin: ModuleOrigin,
    srcs: &mut Vec<Input>,
) -> Result<(), Error> {
    let src_dir = match src_dir.canonicalize() {
        Ok(d) => d,
        Err(_) => return Ok(()),
    };

    for path in crate::fs::gleam_files(&src_dir) {
        let src = std::fs::read_to_string(&path).map_err(|err| Error::FileIo {
            action: FileIoAction::Read,
            kind: FileKind::File,
            err: Some(err.to_string()),
            path: path.clone(),
        })?;

        srcs.push(Input {
            path: path
                .canonicalize()
                .expect("project::collect_source(): path canonicalize"),
            source_base_path: src_dir.clone(),
            origin,
            src,
        })
    }
    Ok(())
}
