use std::cell::RefCell;

use ecow::eco_format;

use crate::analyse::Inferred;

use super::*;

pub(super) fn pattern<'a>(
    p: &'a TypedPattern,
    env: &mut Env<'a>,
    guards: &mut Vec<Document<'a>>,
) -> Document<'a> {
    let mut vars = vec![];
    to_doc(p, &mut vars, env, guards)
}

fn print<'a>(
    p: &'a TypedPattern,
    vars: &mut Vec<&'a str>,
    env: &mut Env<'a>,
    guards: &mut Vec<Document<'a>>,
) -> Document<'a> {
    match p {
        Pattern::Assign {
            name, pattern: p, ..
        } => {
            vars.push(name);
            print(p, vars, env, guards)
                .append(" = ")
                .append(env.next_local_var_name(name))
        }

        Pattern::List { elements, tail, .. } => {
            pattern_list(elements, tail.as_deref(), vars, env, guards)
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

        Pattern::Variable { name, .. } => {
            vars.push(name);
            env.next_local_var_name(name)
        }

        Pattern::Int { value, .. } => int(value),

        Pattern::Float { value, .. } => float(value),

        Pattern::String { value, .. } => string(value),

        Pattern::Constructor {
            arguments: args,
            constructor: Inferred::Known(PatternConstructor { name, .. }),
            ..
        } => tag_tuple_pattern(name, args, vars, env, guards),

        Pattern::Constructor {
            constructor: Inferred::Unknown,
            ..
        } => {
            panic!("Erlang generation performed with uninferred pattern constructor")
        }

        Pattern::Tuple { elements, .. } => {
            tuple(elements.iter().map(|p| print(p, vars, env, guards)))
        }

        Pattern::BitArray { segments, .. } => bit_array(
            segments
                .iter()
                .map(|s| pattern_segment(&s.value, &s.options, vars, env, guards)),
        ),

        Pattern::StringPrefix {
            left_side_string,
            right_side_assignment,
            left_side_assignment,
            ..
        } => {
            let right = match right_side_assignment {
                AssignName::Variable(right) => {
                    vars.push(right);
                    env.next_local_var_name(right)
                }
                AssignName::Discard(_) => "_".to_doc(),
            };

            match left_side_assignment {
                Some((left_name, _)) => {
                    // "wibble" as prefix <> rest
                    //             ^^^^^^^^^ In case the left prefix of the pattern matching is given an alias
                    //                       we bind it to a local variable so that it can be correctly
                    //                       referenced inside the case branch.
                    //
                    // <<Prefix:3/binary, Rest/binary>> when Prefix =:= <<"wibble">>
                    //   ^^^^^^^^                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    //   since erlang's binary pattern matching doesn't allow direct string assignment
                    //   to variables within the pattern, we first match the expected prefix length in
                    //   bytes, then use a guard clause to verify the content.
                    //
                    vars.push(left_name);
                    let name = env.next_local_var_name(left_name);
                    guards.push(docvec![name.clone(), " =:= ", string(left_side_string)]);
                    docvec![
                        "<<",
                        name.clone(),
                        ":",
                        string_length_utf8_bytes(left_side_string),
                        "/binary",
                        ", ",
                        right,
                        "/binary>>",
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

        Pattern::Invalid { .. } => panic!("invalid patterns should not reach code generation"),
    }
}

pub(super) fn to_doc<'a>(
    p: &'a TypedPattern,
    vars: &mut Vec<&'a str>,
    env: &mut Env<'a>,
    guards: &mut Vec<Document<'a>>,
) -> Document<'a> {
    print(p, vars, env, guards)
}

fn tag_tuple_pattern<'a>(
    name: &'a str,
    args: &'a [CallArg<TypedPattern>],
    vars: &mut Vec<&'a str>,
    env: &mut Env<'a>,
    guards: &mut Vec<Document<'a>>,
) -> Document<'a> {
    if args.is_empty() {
        atom_string(to_snake_case(name))
    } else {
        tuple(
            [atom_string(to_snake_case(name))]
                .into_iter()
                .chain(args.iter().map(|p| print(&p.value, vars, env, guards))),
        )
    }
}

fn pattern_segment<'a>(
    value: &'a TypedPattern,
    options: &'a [BitArrayOption<TypedPattern>],
    vars: &mut Vec<&'a str>,
    env: &mut Env<'a>,
    guards: &mut Vec<Document<'a>>,
) -> Document<'a> {
    let pattern_is_a_string_literal = matches!(value, Pattern::String { .. });
    let pattern_is_a_discard = matches!(value, Pattern::Discard { .. });

    let vars = RefCell::new(vars);
    let guards = RefCell::new(guards);

    let create_document = |env: &mut Env<'a>| match value {
        Pattern::String { value, .. } => value.to_doc().surround("\"", "\""),
        Pattern::Discard { .. }
        | Pattern::Variable { .. }
        | Pattern::Int { .. }
        | Pattern::Float { .. } => {
            print(value, &mut vars.borrow_mut(), env, &mut guards.borrow_mut())
        }

        Pattern::Assign { name, pattern, .. } => {
            vars.borrow_mut().push(name);
            let variable_name = env.next_local_var_name(name);

            match pattern.as_ref() {
                // In Erlang, assignment patterns inside bit arrays are not allowed. So instead of
                // generating `<<1 = A>>`, we  use guards, and generate `<<A>> when A =:= 1`.
                Pattern::Int { value, .. } => {
                    guards
                        .borrow_mut()
                        .push(docvec![variable_name.clone(), " =:= ", int(value)]);
                    variable_name
                }
                Pattern::Float { value, .. } => {
                    guards
                        .borrow_mut()
                        .push(docvec![variable_name.clone(), " =:= ", float(value)]);
                    variable_name
                }

                // Here we do the same as for floats and ints, but we must calculate the size of
                // the string first, so we can correctly match the bit array segment then compare
                // it afterwards.
                Pattern::String { value, .. } => {
                    guards.borrow_mut().push(docvec![
                        variable_name.clone(),
                        " =:= ",
                        string(value)
                    ]);
                    docvec![variable_name, ":", string_length_utf8_bytes(value)]
                }

                // Doing a pattern such as `<<_ as a>>` is the same as just `<<a>>`, so we treat it
                // as such.
                Pattern::Discard { .. } => variable_name,

                // Any other pattern is invalid as a bit array segment. We already handle the case
                // of `<<a as b>>` in the type-checker, and assignment patterns cannot be nested.
                _ => panic!("Pattern segment match not recognised"),
            }
        }

        _ => panic!("Pattern segment match not recognised"),
    };

    let size = |value: &'a TypedPattern, env: &mut Env<'a>| {
        Some(":".to_doc().append(print(
            value,
            &mut vars.borrow_mut(),
            env,
            &mut guards.borrow_mut(),
        )))
    };

    let unit = |value: &'a u8| Some(eco_format!("unit:{value}").to_doc());

    bit_array_segment(
        create_document,
        options,
        size,
        unit,
        pattern_is_a_string_literal,
        pattern_is_a_discard,
        env,
    )
}

fn pattern_list<'a>(
    elements: &'a [TypedPattern],
    tail: Option<&'a TypedPattern>,
    vars: &mut Vec<&'a str>,
    env: &mut Env<'a>,
    guards: &mut Vec<Document<'a>>,
) -> Document<'a> {
    let elements = join(
        elements
            .iter()
            .map(|element| print(element, vars, env, guards)),
        break_(",", ", "),
    );
    let tail = tail.map(|tail| print(tail, vars, env, guards));
    list(elements, tail)
}
