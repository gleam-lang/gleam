use std::collections::HashMap;

use ecow::EcoString;

use crate::type_::PRELUDE_MODULE_NAME;

use super::missing_patterns::Term;

#[derive(Debug, Default)]
pub struct ValueNames {
    /// Mapping of imported modules to their locally used named
    ///
    /// key:   The name of the module
    /// value: The name the module is aliased to
    ///
    /// # Example 1
    ///
    /// ```gleam
    /// import mod1 as my_mod
    /// ```
    /// would result in:
    /// - key:   "mod1"
    /// - value: "my_mod"
    ///
    /// # Example 2
    ///
    /// ```gleam
    /// import mod1
    /// ```
    /// would result in:
    /// - key:   "mod1"
    /// - value: "mod1"
    ///
    imported_modules: HashMap<EcoString, EcoString>,

    /// Constructors which are imported in the current module in an
    /// unqualified fashion.
    ///
    /// key:   (Defining module name, type name)
    /// value: Alias name
    ///
    /// # Example 1
    ///
    /// ```gleam
    /// import wibble.{Wobble}
    /// ```
    /// would result in
    /// - key:   `("wibble", "Wobble")`
    /// - value: `"Wobble"`
    ///
    /// # Example 2
    ///
    /// ```gleam
    /// import wibble.{Wobble as Woo}
    /// ```
    /// would result in
    /// - key:   `("wibble", "Wobble")`
    /// - value: `"Woo"`
    ///
    local_constructors: HashMap<(EcoString, EcoString), EcoString>,
}

impl ValueNames {
    pub fn new() -> Self {
        Self {
            local_constructors: Default::default(),
            imported_modules: Default::default(),
        }
    }

    /// Record a named value in this module.
    pub fn named_constructor_in_scope(
        &mut self,
        module_name: EcoString,
        value_name: EcoString,
        local_alias: EcoString,
    ) {
        _ = self
            .local_constructors
            .insert((module_name, value_name), local_alias);
    }

    /// Record an imported module in this module.
    pub fn imported_module(&mut self, module_name: EcoString, module_alias: EcoString) {
        _ = self.imported_modules.insert(module_name, module_alias)
    }

    /// Get the name and optional module qualifier for a named constructor.
    pub fn named_constructor<'a>(
        &'a self,
        module: &'a EcoString,
        name: &'a EcoString,
    ) -> NamedValueNames<'a> {
        let key: (EcoString, EcoString) = (module.clone(), name.clone());

        // There is a local name for this type, use that.
        if let Some(name) = self.local_constructors.get(&key) {
            return NamedValueNames::Unqualified(name.as_str());
        }

        // This type is from a module that has been imported
        if let Some(module) = self.imported_modules.get(module) {
            return NamedValueNames::Qualified(module, name.as_str());
        };

        NamedValueNames::Unimported(name.as_str())
    }
}

#[derive(Debug)]
pub enum NamedValueNames<'a> {
    /// This value is from a module that has not been imported in this module.
    Unimported(&'a str),
    /// This value has been imported in an unqualified fashion in this module.
    Unqualified(&'a str),
    /// This value is from a module that has been imported.
    Qualified(&'a str, &'a str),
}

#[derive(Debug)]
pub struct Printer<'a> {
    names: &'a ValueNames,
}

impl<'a> Printer<'a> {
    pub fn new(names: &'a ValueNames) -> Self {
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
                let (module, name) = match self.names.named_constructor(module, name) {
                    NamedValueNames::Qualified(m, n) => (Some(m), n),
                    NamedValueNames::Unqualified(n) => (None, n),
                    NamedValueNames::Unimported(n) => {
                        (Some(module.split('/').last().unwrap_or(module)), n)
                    }
                };

                if let Some(module) = module {
                    // Don't qualify values in the prelude
                    if module != PRELUDE_MODULE_NAME {
                        buffer.push_str(module);
                        buffer.push('.');
                    }
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
    // I don't know why the compiler complains about these unused values, they are all
    // used, but there seems to be a bug where if they're only used in tests,
    // it doesn't count them as being used. If you're reading this in the future,
    // try removing this and see if it doesn't complain anymore. (Rust v1.80.0)
    #![allow(dead_code)]
    #[allow(unused_imports)]
    use super::{Printer, ValueNames};
    use std::{collections::HashMap, sync::Arc};

    use crate::{
        exhaustiveness::{missing_patterns::Term, Variable},
        type_::Type,
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
        let mut names = ValueNames::new();

        names.named_constructor_in_scope("module".into(), "Wibble".into(), "Wibble".into());

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
        let mut names = ValueNames::new();

        names.named_constructor_in_scope("module".into(), "Wibble".into(), "Wibble".into());

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
        let mut names = ValueNames::new();

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
        let mut names = ValueNames::new();

        names.named_constructor_in_scope("regex".into(), "Regex".into(), "Regex".into());

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
        let mut names = ValueNames::new();

        names.named_constructor_in_scope("regex".into(), "Regex".into(), "Reg".into());
        names.named_constructor_in_scope("gleam".into(), "None".into(), "None".into());

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
        let mut names = ValueNames::new();

        names.named_constructor_in_scope("module".into(), "Type".into(), "Type".into());

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
