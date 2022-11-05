//just an idea atm, need a lot of work

pub fn (
    module: &PartialParsedModule,
    point: Point,
    environment: LspEnv,
) -> Option<WhatToDisplay> {
    let prev_point = Point {
        column: point.column - 1,
        ..point
    };

    let node = module
        .tree
        .root_node()
        .named_descendant_for_point_range(prev_point, point);

    if node.is_none() {
        let _ = writeln!(LogFile.lock().unwrap(), "cursor was none");
        let _ = LogFile.lock().unwrap().flush();
    }

    let _ = writeln!(LogFile.lock().unwrap(), "cursor was {:?}", node);
    let _ = LogFile.lock().unwrap().flush();

    let res = node.and_then(|node| match node.kind() {
        "identifier" => {
            let text = node
                .utf8_text(module.source_code.as_bytes())
                .expect("should have text");
            let imported = module
                .imports
                .iter()
                .find(|x| x.module_import_name == *text);
            let module_name = match imported {
                Some(alias) => &alias.module,
                _ => text,
            };

            //println!("identifier {text:?} {module_name:?}",);
            if let Some(module) = environment.get_importable_module(module_name) {
                Some(WhatToDisplay::Names(
                    module.values.keys().map(|x| x.to_string()).collect(),
                ))
            } else {
                None
            }
        }
        _ => None,
    });

    //println!("{res:?}");
    res
}

fn parse_function_args(text: &str) -> Vec<Bindings> {
    let lex = gleam_core::parse::lexer::make_tokenizer(text);
    let mut parser = gleam_core::parse::Parser::new(lex);

    let _ = parser.expect_one(&gleam_core::parse::token::Token::LeftParen);
    let expr = parser.parse_function_args(false);
    // let expr = parser.ensure_no_errors_or_remaining_input(expr);

    match expr {
        Ok(args) => args
            .iter()
            .map(|arg| match arg {
                Arg {
                    names: ArgNames::Named { name: n },
                    ..
                } => Some(Bindings {
                    name: n.clone(),
                    type_notation: "".to_string(),
                }),
                _ => None,
            })
            .filter(|x| x.is_some())
            .map(|x| x.expect("is some"))
            .collect(),
        _ => Vec::new(),
    }
}

fn parse_function_body(text: &str) -> Vec<Bindings> {
    let lex = gleam_core::parse::lexer::make_tokenizer(text);
    let mut parser = gleam_core::parse::Parser::new(lex);

    let expr = parser.parse_expression_seq();

    let mut outbindings = Vec::new();

    if let Ok(expr) = expr {
        if let Some(ast) = expr {
            extract_bindings_from_ast(&ast.0, &mut outbindings)
        }
    }

    outbindings
}

fn extract_binding_from_assign(pattern: &gleam_core::ast::Pattern<(), ()>, to: &mut Vec<Bindings>) {
    match pattern {
        gleam_core::ast::Pattern::Var { name: n, .. } => to.push(Bindings {
            name: n.to_string(),
            type_notation: "unknown".to_string(),
        }),
        gleam_core::ast::Pattern::Tuple { elems, .. } => elems
            .iter()
            .for_each(|x| extract_binding_from_assign(x, to)),
        _ => (),
    }
}

fn extract_bindings_from_ast(ast: &UntypedExpr, to: &mut Vec<Bindings>) {
    match ast {
        gleam_core::ast::UntypedExpr::Assignment { pattern: p, .. } => {
            extract_binding_from_assign(p, to)
        }
        gleam_core::ast::UntypedExpr::Sequence {
            expressions: exprs, ..
        } => exprs.iter().for_each(|x| extract_bindings_from_ast(x, to)),
        something => println!("unhandled: {something:?}"),
    }
}

fn binds_in_scope(module: &PartialParsedModule, point: Point) -> Vec<(String, String)> {
    let prev_point = Point {
        column: if point.column > 0 {
            point.column - 1
        } else {
            point.column
        },
        ..point
    };
    let mut node = module
        .tree
        .root_node()
        .named_descendant_for_point_range(prev_point, point);

    let mut file: std::fs::File = OpenOptions::new()
        .write(true)
        .create(true)
        .append(true)
        .open("/tmp/lsp.log")
        .unwrap();

    if node.is_none() {
        writeln!(file, "cursor was none");
        file.flush();

        return Vec::new();
    }

    writeln!(file, "cursor was {:?}", node);
    file.flush();

    //are we in a function ?
    //let tc : TreeCursor = node.expect("has one").walk();
    let mut collected: Vec<tree_sitter::Node<'_>> = Vec::new();
    let mut bindings: Vec<Bindings> = Vec::new();

    // get its parameters
    // get the previous statements, parent by parent
    loop {
        if node == None {
            break;
        }
        let parent = node.expect("already has").parent();
        //reached top level
        if parent.is_none() {
            break;
        }

        let prev_sibling = node.expect("already has").prev_sibling();
        //reached top level
        if prev_sibling.is_none() {
            node = parent;
        } else {
            match node.expect("already has").kind() {
                "function_body" => bindings.append(&mut parse_function_body(
                    node.expect("")
                        .utf8_text(module.source_code.as_bytes())
                        .expect("source should have code"),
                )),
                "function_parameters" => bindings.append(&mut parse_function_args(
                    node.expect("")
                        .utf8_text(module.source_code.as_bytes())
                        .expect("source should have code"),
                )),
                "function" => collected.push(node.unwrap()),
                _ => (),
            }
            node = prev_sibling;
        }
    }
    println!("{collected:?}");
    println!("bindings {bindings:?}");

    Vec::new()
}

