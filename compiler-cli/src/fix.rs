use camino::{Utf8Path, Utf8PathBuf};
use gleam_core::{
    error::{FileIoAction, FileKind},
    Error, Result,
};

pub fn run() -> Result<()> {
    for path in crate::fs::gleam_files_excluding_gitignore(Utf8Path::new(".")) {
        fix_file(path)?;
    }

    // Set the version requirement in gleam.toml
    let mut toml = crate::fs::read("gleam.toml")?
        .parse::<toml_edit::Document>()
        .map_err(|e| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Parse,
            path: Utf8PathBuf::from("gleam.toml"),
            err: Some(e.to_string()),
        })?;

    #[allow(clippy::indexing_slicing)]
    {
        toml["gleam"] = toml_edit::value(">= 0.32.0");
    }

    // Write the updated config
    crate::fs::write(Utf8Path::new("gleam.toml"), &toml.to_string())?;

    println!(
        "Your Gleam code has been fixed!

If you have any JavaScript code that used the BitString class
you will need to update it to use the BitArray class instead.
"
    );
    Ok(())
}

fn fix_file(path: Utf8PathBuf) -> Result<()> {
    let src = crate::fs::read(&path)?;
    let out = gleam_core::fix::parse_fix_and_format(&src.into(), &path)?;
    crate::fs::write(&path, &out)?;
    Ok(())
}
