// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2023 The Gleam contributors

use crate::{
    Error, Result,
    format::{Formatter, FormatterCache, Intermediate},
    io::Utf8Writer,
    warning::WarningEmitter,
};
use camino::Utf8Path;
use ecow::EcoString;
use pretty_arena::DocumentArena;

pub fn parse_fix_and_format(src: &EcoString, path: &Utf8Path) -> Result<String> {
    // Parse
    let parsed = crate::parse::parse_module(path.to_owned(), src, &WarningEmitter::null())
        .map_err(|error| Error::Parse {
            path: path.to_path_buf(),
            src: src.clone(),
            error: Box::new(error),
        })?;
    let intermediate = Intermediate::from_extra(&parsed.extra, src);
    let module = parsed.module;

    // Fix
    // let module = some_fixer_module::Fixer::fix(module);

    // Format
    let mut buffer = String::new();
    let arena = DocumentArena::new();
    let cache = FormatterCache::allocate(&arena);
    Formatter::with_comments(&intermediate)
        .module(&arena, &cache, &module)
        .pretty_print(80, &mut buffer)
        .map_err(|error| buffer.convert_err(error))?;

    Ok(buffer)
}
