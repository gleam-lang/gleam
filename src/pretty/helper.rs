use super::*;

use itertools::Itertools;

pub fn wrap_args<I>(indent: isize, args: I) -> Document
where
    I: Iterator<Item = Document>,
{
    break_("", "")
        .append(concat(args.intersperse(delim(","))))
        .nest(indent)
        .append(break_("", ""))
        .surround("(", ")")
        .group()
}
