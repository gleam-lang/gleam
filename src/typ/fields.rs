use super::Error;
use crate::ast::{CallArg, SrcSpan};
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub struct FieldMap {
    pub arity: usize,
    pub fields: HashMap<String, usize>,
}

pub struct DuplicateField;

impl FieldMap {
    pub fn new(arity: usize) -> Self {
        Self {
            arity,
            fields: HashMap::new(),
        }
    }

    pub fn insert(&mut self, label: String, index: usize) -> Result<(), DuplicateField> {
        match self.fields.insert(label, index) {
            Some(_) => Err(DuplicateField),
            None => Ok(()),
        }
    }

    pub fn into_option(self) -> Option<Self> {
        if self.fields.is_empty() {
            None
        } else {
            Some(self)
        }
    }

    /// Reorder an argument list so that labelled fields supplied out-of-order are
    /// in the correct order.
    ///
    pub fn reorder<A>(&self, args: &mut Vec<CallArg<A>>, location: &SrcSpan) -> Result<(), Error> {
        let mut labelled_arguments_given = false;
        let mut seen_labels = std::collections::HashSet::new();
        let mut unknown_labels = Vec::new();

        if self.arity != args.len() {
            return Err(Error::IncorrectArity {
                labels: self.incorrect_arity_labels(args),
                location: location.clone(),
                expected: self.arity,
                given: args.len(),
            });
        }

        for i in 0..args.len() {
            match &args[i].label {
                Some(_) => {
                    labelled_arguments_given = true;
                }

                None => {
                    if labelled_arguments_given {
                        return Err(Error::PositionalArgumentAfterLabelled {
                            location: args[i].location.clone(),
                        });
                    }
                }
            }
        }

        let mut i = 0;
        while i < args.len() {
            let (label, location) = match &args[i].label {
                // A labelled argument, we may need to reposition it in the array vector
                Some(l) => (l, &args[i].location),

                // Not a labelled argument
                None => {
                    i = i + 1;
                    continue;
                }
            };

            let position = match self.fields.get(label) {
                None => {
                    unknown_labels.push((label.clone(), location.clone()));
                    i = i + 1;
                    continue;
                }

                Some(p) => *p,
            };

            // If the argument is already in the right place
            if position == i {
                seen_labels.insert(label.clone());
                i = i + 1;
            } else {
                if seen_labels.contains(label) {
                    return Err(Error::DuplicateArgument {
                        location: location.clone(),
                        label: label.to_string(),
                    });
                }
                seen_labels.insert(label.clone());

                args.swap(position, i);
            }
        }

        if unknown_labels.len() > 0 {
            Err(Error::UnknownLabels {
                valid: self.fields.keys().map(|t| t.to_string()).collect(),
                unknown: unknown_labels,
                supplied: seen_labels.into_iter().collect(),
            })
        } else {
            Ok(())
        }
    }

    pub fn incorrect_arity_labels<A>(&self, args: &Vec<CallArg<A>>) -> Vec<String> {
        let mut given = HashSet::with_capacity(args.len());
        for arg in args {
            if let Some(label) = &arg.label {
                given.insert(label.as_ref());
            }
        }
        self.fields
            .keys()
            .map(|f| f.clone())
            .filter(|f| !given.contains(f.as_str()))
            .sorted()
            .collect()
    }
}
