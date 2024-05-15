use crate::analyse::Inferred;

use super::*;

pub(super) fn pattern<'a>(p: &'a TypedPattern, env: &mut Env<'a>) -> Document<'a> {
    let mut vars = vec![];
    to_doc(p, &mut vars, env)
}

pub(super) fn requires_guard(p: &TypedPattern) -> bool {
    match p {
        Pattern::StringPrefix {
            left_side_assignment: Some(_),
            ..
        } => true,
        _ => false,
    }
}

fn print<'a>(
    p: &'a TypedPattern,
    vars: &mut Vec<&'a str>,
    define_variables: bool,
    env: &mut Env<'a>,
) -> Document<'a> {
    match p {
        Pattern::Assign {
            name, pattern: p, ..
        } if define_variables => {
            vars.push(name);
            print(p, vars, define_variables, env)
                .append(" = ")
                .append(env.next_local_var_name(name))
        }

        Pattern::Assign { pattern: p, .. } => print(p, vars, define_variables, env),

        Pattern::List { elements, tail, .. } => {
            pattern_list(elements, tail.as_deref(), vars, define_variables, env)
        }

        Pattern::Discard { .. } => "_".to_doc(),

        Pattern::VarUsage {
            name, constructor, ..
        } => {
            let v = &constructor
                .as_ref()
                .expect("Constructor not found for variable usage")
                .variant;
            match v {
                ValueConstructorVariant::ModuleConstant { literal, .. } => {
                    const_inline(literal, env)
                }
                _ => env.local_var_name(name),
            }
        }

        Pattern::Variable { name, .. } if define_variables => {
            vars.push(name);
            env.next_local_var_name(name)
        }

        Pattern::Variable { .. } => "_".to_doc(),

        Pattern::Int { value, .. } => int(value),

        Pattern::Float { value, .. } => float(value),

        Pattern::String { value, .. } => string(value),

        Pattern::Constructor {
            arguments: args,
            constructor: Inferred::Known(PatternConstructor { name, .. }),
            ..
        } => tag_tuple_pattern(name, args, vars, define_variables, env),

        Pattern::Constructor {
            constructor: Inferred::Unknown,
            ..
        } => {
            panic!("Erlang generation performed with uninferred pattern constructor")
        }

        Pattern::Tuple { elems, .. } => {
            tuple(elems.iter().map(|p| print(p, vars, define_variables, env)))
        }

        Pattern::BitArray { segments, .. } => bit_array(
            segments
                .iter()
                .map(|s| pattern_segment(&s.value, &s.options, vars, define_variables, env)),
        ),

        Pattern::StringPrefix {
            left_side_string,
            right_side_assignment,
            left_side_assignment,
            ..
        } => {
            let right = match right_side_assignment {
                AssignName::Variable(right) if define_variables => env.next_local_var_name(right),
                AssignName::Variable(_) | AssignName::Discard(_) => "_".to_doc(),
            };

            match left_side_assignment {
                Some((left_name, _)) => {
                    // "foo" as prefix <> rest
                    //       ^^^^^^^^^ In case the left prefix of the pattern matching is given an alias
                    //                 we bind it to a local variable so that it can be correctly
                    //                 referenced inside the case branch.
                    //
                    // <<Prefix:3/binary, Rest/binary>> when Prefix =:= <<"foo">>
                    //   ^^^^^^^^                       ^^^^^^^^^^^^^^^^^^^^^^^^^
                    //   since erlang's binary pattern matching doesn't allow direct string assignment
                    //   to variables within the pattern, we first match the expected prefix length in
                    //   bytes, then use a guard clause to verify the content.
                    //
                    let name = env.next_local_var_name(left_name);
                    docvec![
                        "<<",
                        name.clone(),
                        ":",
                        string_length_utf8_bytes(left_side_string),
                        "/binary",
                        ", ",
                        right,
                        "/binary>>",
                        " when ",
                        name,
                        " =:= ",
                        string(left_side_string)
                    ]
                }
                None => docvec![
                    "<<\"",
                    string_inner(left_side_string),
                    "\"/utf8",
                    ", ",
                    right,
                    "/binary>>"
                ],
            }
        }
    }
}

pub(super) fn to_doc<'a>(
    p: &'a TypedPattern,
    vars: &mut Vec<&'a str>,
    env: &mut Env<'a>,
) -> Document<'a> {
    print(p, vars, true, env)
}

pub(super) fn to_doc_discarding_all<'a>(
    p: &'a TypedPattern,
    vars: &mut Vec<&'a str>,
    env: &mut Env<'a>,
) -> Document<'a> {
    print(p, vars, false, env)
}

fn tag_tuple_pattern<'a>(
    name: &'a str,
    args: &'a [CallArg<TypedPattern>],
    vars: &mut Vec<&'a str>,
    define_variables: bool,
    env: &mut Env<'a>,
) -> Document<'a> {
    if args.is_empty() {
        atom_string(name.to_snake_case())
    } else {
        tuple(
            [atom_string(name.to_snake_case())].into_iter().chain(
                args.iter()
                    .map(|p| print(&p.value, vars, define_variables, env)),
            ),
        )
    }
}

fn pattern_segment<'a>(
    value: &'a TypedPattern,
    options: &'a [BitArrayOption<TypedPattern>],
    vars: &mut Vec<&'a str>,
    define_variables: bool,
    env: &mut Env<'a>,
) -> Document<'a> {
    let document = match value {
        // Skip the normal <<value/utf8>> surrounds
        Pattern::String { value, .. } => value.to_doc().surround("\"", "\""),

        // As normal
        Pattern::Discard { .. }
        | Pattern::Variable { .. }
        | Pattern::Int { .. }
        | Pattern::Float { .. } => print(value, vars, define_variables, env),

        // No other pattern variants are allowed in pattern bit array segments
        _ => panic!("Pattern segment match not recognised"),
    };

    let size = |value: &'a TypedPattern, env: &mut Env<'a>| {
        Some(
            ":".to_doc()
                .append(print(value, vars, define_variables, env)),
        )
    };

    let unit = |value: &'a u8| Some(Document::String(format!("unit:{value}")));

    bit_array_segment(document, options, size, unit, true, env)
}

fn pattern_list<'a>(
    elements: &'a [TypedPattern],
    tail: Option<&'a TypedPattern>,
    vars: &mut Vec<&'a str>,
    define_variables: bool,
    env: &mut Env<'a>,
) -> Document<'a> {
    let elements = join(
        elements
            .iter()
            .map(|e| print(e, vars, define_variables, env)),
        break_(",", ", "),
    );
    let tail = tail.map(|tail| print(tail, vars, define_variables, env));
    list(elements, tail)
}
