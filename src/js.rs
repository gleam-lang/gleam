mod statements;
#[cfg(test)]
mod tests;

use crate::{ast::*, fs::Utf8Writer, line_numbers::LineNumbers, pretty::*, Result};
use itertools::Itertools;

pub fn module(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    writer: &mut impl Utf8Writer,
) -> Result<()> {
    let statements = concat(Itertools::intersperse(
        module
            .statements
            .iter()
            .flat_map(|s| statements::statement(&module.name, s, &module.name, line_numbers)),
        lines(2),
    ));

    statements.pretty_print(80, writer)
}
