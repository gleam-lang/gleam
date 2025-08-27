use std::collections::HashMap;

use ecow::EcoString;

use crate::type_::printer::{NameContextInformation, Names};

use super::{Variable, missing_patterns::Term};

#[derive(Debug)]
pub struct Printer<'a> {
    names: &'a Names,
}

impl<'a> Printer<'a> {
    pub fn new(names: &'a Names) -> Self {
        Printer { names }
    }

    pub fn print_terms(
        &self,
        subjects: &[Variable],
        terms: &[Term],
        mapping: &HashMap<usize, usize>,
    ) -> EcoString {
        let mut buffer = EcoString::new();
        for (i, subject) in subjects.iter().enumerate() {
            if i != 0 {
                buffer.push_str(", ");
            }

            match mapping.get(&subject.id) {
                Some(&index) => {
                    let term = terms.get(index).expect("Term must exist");
                    self.print(term, terms, mapping, &mut buffer);
                }
                None => buffer.push('_'),
            }
        }
        buffer
    }

    fn print(
        &self,
        term: &Term,
        terms: &[Term],
        mapping: &HashMap<usize, usize>,
        buffer: &mut EcoString,
    ) {
        match term {
            Term::Variant {
                name,
                module,
                fields,
                ..
            } => {
                let (module, name) = match self.names.named_constructor(module, name) {
                    NameContextInformation::Qualified(module, name) => (Some(module), name),
                    NameContextInformation::Unqualified(name) => (None, name),
                    NameContextInformation::Unimported(module, name) => {
                        (module.split('/').next_back(), name)
                    }
                };

                if let Some(module) = module {
                    buffer.push_str(module);
                    buffer.push('.');
                }
                buffer.push_str(name);

                if fields.is_empty() {
                    return;
                }
                buffer.push('(');
                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        buffer.push_str(", ");
                    }

                    let mut has_label = false;

                    if let Some(label) = &field.label {
                        buffer.push_str(label);
                        buffer.push(':');
                        has_label = true;
                    }

                    if let Some(&idx) = mapping.get(&field.variable.id) {
                        let term = terms.get(idx).expect("Term must exist");

                        match term {
                            // If it is an infinite term and this field is labelled, it is generally
                            // more useful to print just the label using label shorthand syntax.
                            // For example, printing `Person(name:, age:)` instead of
                            // `Person(name: _, age: _)`.
                            Term::Infinite { .. } if has_label => {}
                            Term::Infinite { .. }
                            | Term::Variant { .. }
                            | Term::Tuple { .. }
                            | Term::EmptyList { .. }
                            | Term::List { .. } => {
                                // If this field has a label, the current buffer looks like `label:`,
                                // so we want to print a space before printing the pattern for it.
                                // If there is no label, we don't need to print the space.
                                if has_label {
                                    buffer.push(' ');
                                }
                                self.print(term, terms, mapping, buffer);
                            }
                        }
                    } else if !has_label {
                        buffer.push('_');
                    }
                }
                buffer.push(')');
            }
            Term::Tuple { elements, .. } => {
                buffer.push_str("#(");
                for (i, variable) in elements.iter().enumerate() {
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
        &self,
        term: &Term,
        terms: &[Term],
        mapping: &HashMap<usize, usize>,
        buffer: &mut EcoString,
    ) {
        match term {
            Term::Infinite { .. } | Term::Variant { .. } | Term::Tuple { .. } => buffer.push('_'),

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

#[cfg(test)]
mod tests {
    use ecow::EcoString;

    use super::Printer;
    use std::{collections::HashMap, sync::Arc};

    use crate::{
        ast::SrcSpan,
        exhaustiveness::{
            Variable,
            missing_patterns::{Term, VariantField},
        },
        type_::{Type, printer::Names},
    };

    /// Create a variable with a dummy type, for ease of writing tests
    fn make_variable(id: usize) -> Variable {
        Variable {
            id,
            type_: Arc::new(Type::Tuple {
                elements: Vec::new(),
            }),
        }
    }

    fn field(variable: Variable, label: Option<&str>) -> VariantField {
        VariantField {
            variable,
            label: label.map(EcoString::from),
        }
    }

    fn get_mapping(terms: &[Term]) -> HashMap<usize, usize> {
        let mut mapping: HashMap<usize, usize> = HashMap::new();

        for (index, term) in terms.iter().enumerate() {
            _ = mapping.insert(term.variable().id, index);
        }
        mapping
    }

    #[test]
    fn test_value_in_current_module() {
        let mut names = Names::new();

        names.named_constructor_in_scope("module".into(), "Wibble".into(), "Wibble".into());

        let printer = Printer::new(&names);

        let subjects = &[make_variable(0)];
        let term = Term::Variant {
            variable: subjects[0].clone(),
            name: "Wibble".into(),
            module: "module".into(),
            fields: Vec::new(),
        };

        let terms = &[term];
        let mapping = get_mapping(terms);

        assert_eq!(printer.print_terms(subjects, terms, &mapping), "Wibble");
    }

    #[test]
    fn test_value_in_current_module_with_arguments() {
        let mut names = Names::new();

        names.named_constructor_in_scope("module".into(), "Wibble".into(), "Wibble".into());

        let printer = Printer::new(&names);

        let var1 = make_variable(1);

        let var2 = make_variable(2);

        let subjects = &[make_variable(0)];
        let term = Term::Variant {
            variable: subjects[0].clone(),
            name: "Wibble".into(),
            module: "module".into(),
            fields: vec![field(var1.clone(), None), field(var2.clone(), None)],
        };

        let terms = &[
            term,
            Term::EmptyList { variable: var1 },
            Term::Infinite { variable: var2 },
        ];
        let mapping = get_mapping(terms);

        assert_eq!(
            printer.print_terms(subjects, terms, &mapping),
            "Wibble([], _)"
        );
    }

    #[test]
    fn test_value_in_current_module_with_labelled_arguments() {
        let mut names = Names::new();

        names.named_constructor_in_scope("module".into(), "Wibble".into(), "Wibble".into());

        let printer = Printer::new(&names);

        let var1 = make_variable(1);

        let var2 = make_variable(2);

        let subjects = &[make_variable(0)];
        let term = Term::Variant {
            variable: subjects[0].clone(),
            name: "Wibble".into(),
            module: "module".into(),
            fields: vec![
                field(var1.clone(), Some("list")),
                field(var2.clone(), Some("other")),
            ],
        };

        let terms = &[
            term,
            Term::EmptyList { variable: var1 },
            Term::Infinite { variable: var2 },
        ];
        let mapping = get_mapping(terms);

        assert_eq!(
            printer.print_terms(subjects, terms, &mapping),
            "Wibble(list: [], other:)"
        );
    }

    #[test]
    fn test_module_alias() {
        let mut names = Names::new();

        assert!(
            names
                .imported_module("mod".into(), "shapes".into(), SrcSpan::new(50, 60))
                .is_none()
        );

        let printer = Printer::new(&names);

        let subjects = &[make_variable(0)];
        let term = Term::Variant {
            variable: subjects[0].clone(),
            name: "Rectangle".into(),
            module: "mod".into(),
            fields: Vec::new(),
        };

        let terms = &[term];
        let mapping = get_mapping(terms);

        assert_eq!(
            printer.print_terms(subjects, terms, &mapping),
            "shapes.Rectangle"
        );
    }

    #[test]
    fn test_unqualified_value() {
        let mut names = Names::new();

        names.named_constructor_in_scope("regex".into(), "Regex".into(), "Regex".into());

        let printer = Printer::new(&names);

        let arg = make_variable(1);

        let subjects = &[make_variable(0)];
        let term = Term::Variant {
            variable: subjects[0].clone(),
            name: "Regex".into(),
            module: "regex".into(),
            fields: vec![field(arg.clone(), None)],
        };

        let terms = &[term, Term::Infinite { variable: arg }];
        let mapping = get_mapping(terms);

        assert_eq!(printer.print_terms(subjects, terms, &mapping), "Regex(_)");
    }

    #[test]
    fn test_unqualified_value_with_alias() {
        let mut names = Names::new();

        names.named_constructor_in_scope("regex".into(), "Regex".into(), "Reg".into());
        names.named_constructor_in_scope("gleam".into(), "None".into(), "None".into());

        let printer = Printer::new(&names);

        let arg = make_variable(1);

        let subjects = &[make_variable(0)];
        let term = Term::Variant {
            variable: subjects[0].clone(),
            name: "Regex".into(),
            module: "regex".into(),
            fields: vec![field(arg.clone(), None)],
        };

        let terms = &[
            term,
            Term::Variant {
                variable: arg,
                name: "None".into(),
                module: "gleam".into(),
                fields: vec![],
            },
        ];
        let mapping = get_mapping(terms);

        assert_eq!(printer.print_terms(subjects, terms, &mapping), "Reg(None)");
    }

    #[test]
    fn test_list_pattern() {
        let mut names = Names::new();

        names.named_constructor_in_scope("module".into(), "Type".into(), "Type".into());

        let printer = Printer::new(&names);

        let var1 = make_variable(1);
        let var2 = make_variable(2);
        let var3 = make_variable(3);

        let subjects = &[make_variable(0)];
        let term = Term::List {
            variable: subjects[0].clone(),
            first: var1.clone(),
            rest: var2.clone(),
        };

        let terms = &[
            term,
            Term::Variant {
                variable: var1,
                name: "Type".into(),
                module: "module".into(),
                fields: Vec::new(),
            },
            Term::List {
                variable: var2,
                first: var3.clone(),
                rest: make_variable(4),
            },
            Term::Infinite { variable: var3 },
        ];
        let mapping = get_mapping(terms);

        assert_eq!(
            printer.print_terms(subjects, terms, &mapping),
            "[Type, _, ..]"
        );
    }

    #[test]
    fn test_multi_pattern() {
        let mut names = Names::new();

        names.named_constructor_in_scope("gleam".into(), "Ok".into(), "Ok".into());
        names.named_constructor_in_scope("gleam".into(), "False".into(), "False".into());

        let printer = Printer::new(&names);

        let subjects = &[make_variable(0), make_variable(1), make_variable(2)];

        let terms = &[
            Term::Variant {
                variable: subjects[0].clone(),
                name: "Ok".into(),
                module: "gleam".into(),
                fields: vec![field(make_variable(3), None)],
            },
            Term::Variant {
                variable: subjects[2].clone(),
                name: "False".into(),
                module: "gleam".into(),
                fields: Vec::new(),
            },
        ];
        let mapping = get_mapping(terms);

        assert_eq!(
            printer.print_terms(subjects, terms, &mapping),
            "Ok(_), _, False"
        );
    }
}
