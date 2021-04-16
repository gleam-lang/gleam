#[cfg(test)]
mod tests;

use crate::{
    ast::*, error::Error::UnsupportedFeature, fs::Utf8Writer, line_numbers::LineNumbers, Result,
};

pub fn module(
    _module: &TypedModule,
    _line_numbers: &LineNumbers,
    _writer: &mut impl Utf8Writer,
) -> Result<()> {
    Err(UnsupportedFeature {
        target: crate::Target::JavaScript,
        feature: "Any feature! ".to_string(),
    })
}
