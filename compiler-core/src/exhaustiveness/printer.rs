#![allow(dead_code)]
use std::collections::HashMap;

use ecow::EcoString;

use crate::type_::printer::{NamedTypeNames, TypeNames};

use super::missing_patterns::Term;

#[derive(Debug)]
pub struct Printer<'a> {
    names: &'a mut TypeNames,
}

impl<'a> Printer<'a> {
    pub fn new(names: &'a mut TypeNames) -> Self {
        Printer { names }
    }

    pub fn print_term(
        &mut self,
        term: &Term,
        terms: &[Term],
        mapping: &HashMap<usize, usize>,
    ) -> EcoString {
        let mut buffer = EcoString::new();
        self.print(term, terms, mapping, &mut buffer);
        buffer
    }

    fn print(
        &mut self,
        term: &Term,
        terms: &[Term],
        mapping: &HashMap<usize, usize>,
        buffer: &mut EcoString,
    ) {
        match term {
            Term::Variant {
                name,
                module,
                arguments,
                ..
            } => {
                let (module, name) = match self.names.named_value(module, name) {
                    NamedTypeNames::Qualified(m, n) => (Some(m), n),
                    NamedTypeNames::Unqualified(n) => (None, n),
                    NamedTypeNames::Unimported(n) => {
                        (Some(module.split('/').last().unwrap_or(module)), n)
                    }
                };

                if let Some(module) = module {
                    buffer.push_str(module);
                    buffer.push('.');
                }
                buffer.push_str(name);

                if arguments.is_empty() {
                    return;
                }
                buffer.push('(');
                for (i, variable) in arguments.iter().enumerate() {
                    if i != 0 {
                        buffer.push_str(", ");
                    }

                    if let Some(&idx) = mapping.get(&variable.id) {
                        self.print(
                            terms.get(idx).expect("Term must exist"),
                            terms,
                            mapping,
                            buffer,
                        );
                    } else {
                        buffer.push('_');
                    }
                }
                buffer.push(')');
            }
            Term::Infinite { .. } => buffer.push('_'),
            Term::EmptyList { .. } => buffer.push_str("[]"),
            Term::List { .. } => {
                buffer.push('[');
                self.print_list(term, terms, mapping, buffer);
                buffer.push(']');
            }
        }
    }

    fn print_list(
        &mut self,
        term: &Term,
        terms: &[Term],
        mapping: &HashMap<usize, usize>,
        buffer: &mut EcoString,
    ) {
        match term {
            Term::Infinite { .. } | Term::Variant { .. } => buffer.push('_'),

            Term::EmptyList { .. } => {}

            Term::List { first, rest, .. } => {
                if let Some(&idx) = mapping.get(&first.id) {
                    self.print(
                        terms.get(idx).expect("Term must exist"),
                        terms,
                        mapping,
                        buffer,
                    )
                } else {
                    buffer.push('_');
                }

                if let Some(&idx) = mapping.get(&rest.id) {
                    let term = terms.get(idx).expect("Term must exist");

                    match term {
                        Term::EmptyList { .. } => {}
                        _ => {
                            buffer.push_str(", ");
                            self.print_list(term, terms, mapping, buffer)
                        }
                    }
                } else {
                    buffer.push_str(", ..");
                }
            }
        }
    }
}

mod tests {
    use std::{collections::HashMap, sync::Arc};

    use super::Printer;

    use crate::{
        exhaustiveness::{missing_patterns::Term, Variable},
        type_::{printer::TypeNames, Type},
    };

    /// Create a variable with a dummy type, for ease of writing tests
    fn make_variable(id: usize) -> Variable {
        Variable {
            id,
            type_: Arc::new(Type::Tuple { elems: Vec::new() }),
        }
    }

    fn get_terms_and_mapping(terms: Vec<Term>) -> (Vec<Term>, HashMap<usize, usize>) {
        let mut mapping: HashMap<usize, usize> = HashMap::new();

        for (index, term) in terms.iter().enumerate() {
            _ = mapping.insert(term.variable().id, index);
        }
        (terms, mapping)
    }

    #[test]
    fn test_value_in_current_module() {
        let mut names = TypeNames::new("module".into());

        names.named_value_in_scope("module".into(), "Wibble".into(), "Wibble".into());

        let mut printer = Printer::new(&mut names);

        let term = Term::Variant {
            variable: make_variable(0),
            name: "Wibble".into(),
            module: "module".into(),
            arguments: Vec::new(),
        };

        assert_eq!(
            printer.print_term(&term, &Vec::new(), &HashMap::new()),
            "Wibble"
        );
    }

    #[test]
    fn test_value_in_current_module_with_arguments() {
        let mut names = TypeNames::new("module".into());

        names.named_value_in_scope("module".into(), "Wibble".into(), "Wibble".into());

        let mut printer = Printer::new(&mut names);

        let var1 = make_variable(1);

        let var2 = make_variable(2);

        let term = Term::Variant {
            variable: make_variable(0),
            name: "Wibble".into(),
            module: "module".into(),
            arguments: vec![var1.clone(), var2.clone()],
        };

        let (terms, mapping) = get_terms_and_mapping(vec![
            Term::EmptyList { variable: var1 },
            Term::Infinite { variable: var2 },
        ]);

        assert_eq!(printer.print_term(&term, &terms, &mapping), "Wibble([], _)");
    }

    #[test]
    fn test_module_alias() {
        let mut names = TypeNames::new("module".into());

        names.imported_module("mod".into(), "shapes".into());

        let mut printer = Printer::new(&mut names);

        let term = Term::Variant {
            variable: make_variable(0),
            name: "Rectangle".into(),
            module: "mod".into(),
            arguments: Vec::new(),
        };

        assert_eq!(
            printer.print_term(&term, &Vec::new(), &HashMap::new()),
            "shapes.Rectangle"
        );
    }

    #[test]
    fn test_unqualified_value() {
        let mut names = TypeNames::new("module".into());

        names.named_value_in_scope("regex".into(), "Regex".into(), "Regex".into());

        let mut printer = Printer::new(&mut names);

        let arg = make_variable(1);

        let term = Term::Variant {
            variable: make_variable(0),
            name: "Regex".into(),
            module: "regex".into(),
            arguments: vec![arg.clone()],
        };

        let (terms, mapping) = get_terms_and_mapping(vec![Term::Infinite { variable: arg }]);

        assert_eq!(printer.print_term(&term, &terms, &mapping), "Regex(_)");
    }

    #[test]
    fn test_unqualified_value_with_alias() {
        let mut names = TypeNames::new("module".into());

        names.named_value_in_scope("regex".into(), "Regex".into(), "Reg".into());
        names.named_value_in_scope("gleam".into(), "None".into(), "None".into());

        let mut printer = Printer::new(&mut names);

        let arg = make_variable(1);

        let term = Term::Variant {
            variable: make_variable(0),
            name: "Regex".into(),
            module: "regex".into(),
            arguments: vec![arg.clone()],
        };

        let (terms, mapping) = get_terms_and_mapping(vec![Term::Variant {
            variable: arg,
            name: "None".into(),
            module: "gleam".into(),
            arguments: vec![],
        }]);

        assert_eq!(printer.print_term(&term, &terms, &mapping), "Reg(None)");
    }

    #[test]
    fn test_list_pattern() {
        let mut names = TypeNames::new("module".into());

        names.named_value_in_scope("module".into(), "Type".into(), "Type".into());

        let mut printer = Printer::new(&mut names);

        let var1 = make_variable(1);
        let var2 = make_variable(2);
        let var3 = make_variable(3);

        let term = Term::List {
            variable: make_variable(0),
            first: var1.clone(),
            rest: var2.clone(),
        };

        let (terms, mapping) = get_terms_and_mapping(vec![
            Term::Variant {
                variable: var1,
                name: "Type".into(),
                module: "module".into(),
                arguments: Vec::new(),
            },
            Term::List {
                variable: var2,
                first: var3.clone(),
                rest: make_variable(0),
            },
            Term::Infinite { variable: var3 },
        ]);

        assert_eq!(printer.print_term(&term, &terms, &mapping), "[Type, _, ..]");
    }
}
