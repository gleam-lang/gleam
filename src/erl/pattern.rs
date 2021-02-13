use super::*;

pub(super) fn pattern<'a>(p: &'a TypedPattern, env: &mut Env<'a>) -> Document<'a> {
    let mut vars = vec![];
    to_doc(p, &mut vars, env)
}

pub(super) fn to_doc<'a>(
    p: &'a TypedPattern,
    vars: &mut Vec<&'a str>,
    env: &mut Env<'a>,
) -> Document<'a> {
    match p {
        Pattern::Nil { .. } => "[]".to_doc(),

        Pattern::Let {
            name, pattern: p, ..
        } => {
            vars.push(name);
            to_doc(p, vars, env)
                .append(" = ")
                .append(env.next_local_var_name(name))
        }

        Pattern::Cons { head, tail, .. } => pattern_list_cons(head, tail, vars, env),

        Pattern::Discard { .. } => "_".to_doc(),

        Pattern::VarUsage { name, .. } => env.local_var_name(name),

        Pattern::Var { name, .. } => {
            vars.push(name);
            env.next_local_var_name(name)
        }

        Pattern::Int { value, .. } => int(value.as_ref()),

        Pattern::Float { value, .. } => float(value.as_ref()),

        Pattern::String { value, .. } => string(value),

        Pattern::Constructor {
            args,
            constructor: PatternConstructor::Record { name },
            ..
        } => tag_tuple_pattern(name, args, vars, env),

        Pattern::Tuple { elems, .. } => tuple(elems.iter().map(|p| to_doc(p, vars, env))),

        Pattern::BitString { segments, .. } => bit_string(
            segments
                .iter()
                .map(|s| pattern_segment(&s.value, s.options.as_slice(), vars, env)),
        ),
    }
}

fn tag_tuple_pattern<'a>(
    name: &'a str,
    args: &'a [CallArg<TypedPattern>],
    vars: &mut Vec<&'a str>,
    env: &mut Env<'a>,
) -> Document<'a> {
    if args.is_empty() {
        atom(name.to_snake_case())
    } else {
        tuple(
            std::iter::once(atom(name.to_snake_case()))
                .chain(args.iter().map(|p| to_doc(&p.value, vars, env))),
        )
    }
}

fn pattern_segment<'a>(
    value: &'a TypedPattern,
    options: &'a [BitStringSegmentOption<TypedPattern>],
    vars: &mut Vec<&'a str>,
    env: &mut Env<'a>,
) -> Document<'a> {
    let document = match value {
        // Skip the normal <<value/utf8>> surrounds
        Pattern::String { value, .. } => value.to_doc().surround("\"", "\""),

        // As normal
        Pattern::Discard { .. }
        | Pattern::Var { .. }
        | Pattern::Int { .. }
        | Pattern::Float { .. } => to_doc(value, vars, env),

        // No other pattern variants are allowed in pattern bit string segments
        _ => crate::error::fatal_compiler_bug("Pattern segment match not recognised"),
    };

    let size = |value: &'a TypedPattern, env: &mut Env<'a>| {
        Some(":".to_doc().append(to_doc(value, vars, env)))
    };

    let unit = |value: &'a usize| Some(Document::String(format!("unit:{}", value)));

    bit_string_segment(document, options, size, unit, true, env)
}

fn pattern_list_cons<'a>(
    head: &'a TypedPattern,
    tail: &'a TypedPattern,
    vars: &mut Vec<&'a str>,
    env: &mut Env<'a>,
) -> Document<'a> {
    let mut elems = vec![head];
    let final_tail = collect_cons(tail, &mut elems, categorise_element);
    let elems = concat(
        elems
            .into_iter()
            .map(|e| to_doc(e, vars, env))
            .intersperse(break_(",", ", ")),
    );
    list(elems, final_tail.map(|e| to_doc(e, vars, env)))
}

fn categorise_element(expr: &TypedPattern) -> ListType<&TypedPattern, &TypedPattern> {
    match expr {
        Pattern::Cons { head, tail, .. } => ListType::Cons {
            head: head.as_ref(),
            tail: tail.as_ref(),
        },
        Pattern::Nil { .. } => ListType::Nil,
        other => ListType::NotList(other),
    }
}
