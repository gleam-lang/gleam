mod statements;
#[cfg(test)]
mod tests;

use crate::{ast::*, fs::Utf8Writer, line_numbers::LineNumbers, pretty::*, Result};
use itertools::Itertools;

const INDENT: isize = 4;

pub fn module(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    writer: &mut impl Utf8Writer,
) -> Result<()> {
    let rendered = module
        .statements
        .iter()
        .flat_map(|s| statements::statement(&module.name, s, &module.name, line_numbers));
    let rendered: Result<Vec<Document<'_>>> = rendered.collect();
    let rendered = rendered?;
    let statements = concat(Itertools::intersperse(rendered.into_iter(), lines(2)));

    statements.pretty_print(80, writer)
}

#[derive(Debug, Clone)]
struct Env<'a> {
    return_last: &'a bool,
    semicolon: &'a bool,
}
