use crate::{ast::*, line_numbers::LineNumbers, pretty::*};

pub fn statement<'a>(
    _current_module: &'a [String],
    _statement: &'a TypedStatement,
    _module: &'a [String],
    _line_numbers: &'a LineNumbers,
) -> Option<Document<'a>> {
    unimplemented!("statement match")
}
