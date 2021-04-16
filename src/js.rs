use crate::{ast::*, fs::Utf8Writer, line_numbers::LineNumbers, Result};

pub fn module(
    _module: &TypedModule,
    _line_numbers: &LineNumbers,
    _writer: &mut impl Utf8Writer,
) -> Result<()> {
    unimplemented!("JS backend in progress");
}
