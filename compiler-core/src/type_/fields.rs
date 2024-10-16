use super::Error;
use crate::ast::{CallArg, SrcSpan};
use ecow::EcoString;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldMap {
    pub arity: u32,
    pub fields: HashMap<EcoString, u32>,
}

#[derive(Debug, Clone, Copy)]
pub struct DuplicateField;

impl FieldMap {
    pub fn new(arity: u32) -> Self {
        Self {
            arity,
            fields: HashMap::new(),
        }
    }

    pub fn insert(&mut self, label: EcoString, index: u32) -> Result<(), DuplicateField> {
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
    pub fn reorder<A>(&self, args: &mut Vec<CallArg<A>>, location: SrcSpan) -> Result<(), Error> {
        let mut labelled_arguments_given = false;
        let mut seen_labels = HashSet::new();
        let mut unknown_labels = Vec::new();
        let number_of_arguments = args.len();

        if self.arity as usize != args.len() {
            return Err(Error::IncorrectArity {
                labels: self.incorrect_arity_labels(args),
                location,
                expected: self.arity as usize,
                given: args.len(),
            });
        }

        for arg in args.iter() {
            match &arg.label {
                Some(_) => {
                    labelled_arguments_given = true;
                }

                None => {
                    if labelled_arguments_given && !arg.is_implicit() {
                        return Err(Error::PositionalArgumentAfterLabelled {
                            location: arg.location,
                        });
                    }
                }
            }
        }

        // Keeps track of which labelled arguments need to be inserted into which indices
        let mut labelled_arguments = HashMap::new();

        // We iterate the argument in reverse order, because we have to remove elements
        // from the `args` list quite a lot, and removing from the end of a list is more
        // efficient than removing from the beginning or the middle.
        let mut i = args.len();
        while i > 0 {
            i -= 1;
            let (label, &location) = match &args.get(i).expect("Field indexing to get label").label
            {
                // A labelled argument, we may need to reposition it
                Some(l) => (
                    l,
                    &args
                        .get(i)
                        .expect("Indexing in labelled field reordering")
                        .location,
                ),

                // Not a labelled argument
                None => {
                    continue;
                }
            };

            let position = match self.fields.get(label) {
                None => {
                    unknown_labels.push((label.clone(), location));
                    continue;
                }

                Some(&p) => p,
            };

            if seen_labels.contains(label) {
                return Err(Error::DuplicateArgument {
                    location,
                    label: label.clone(),
                });
            }
            let _ = seen_labels.insert(label.clone());

            // Add this argument to the `labelled_arguments` map, and remove if from the
            // existing arguments list. It will be reinserted later in the correct index
            let _ = labelled_arguments.insert(position as usize, args.remove(i));
        }

        // The labelled arguments must be reinserted in order
        for i in 0..number_of_arguments {
            if let Some(arg) = labelled_arguments.remove(&i) {
                args.insert(i, arg);
            }
        }

        if unknown_labels.is_empty() {
            Ok(())
        } else {
            Err(Error::UnknownLabels {
                valid: self.fields.keys().cloned().collect(),
                unknown: unknown_labels,
                supplied: seen_labels.into_iter().collect(),
            })
        }
    }

    pub fn incorrect_arity_labels<A>(&self, args: &[CallArg<A>]) -> Vec<EcoString> {
        let given: HashSet<_> = args.iter().filter_map(|arg| arg.label.as_ref()).collect();

        self.fields
            .keys()
            .filter(|f| !given.contains(f))
            .cloned()
            .sorted()
            .collect()
    }
}

#[derive(Debug)]
pub struct FieldMapBuilder {
    index: u32,
    any_labels: bool,
    field_map: FieldMap,
}

impl FieldMapBuilder {
    pub fn new(size: u32) -> Self {
        Self {
            index: 0,
            any_labels: false,
            field_map: FieldMap::new(size),
        }
    }

    pub fn add(&mut self, label: Option<&EcoString>, location: SrcSpan) -> Result<(), Error> {
        match label {
            Some(label) => self.labelled(label, location)?,
            None => self.unlabelled(location)?,
        }
        self.index += 1;
        Ok(())
    }

    fn labelled(&mut self, label: &EcoString, location: SrcSpan) -> Result<(), Error> {
        if self.field_map.insert(label.clone(), self.index).is_err() {
            return Err(Error::DuplicateField {
                label: label.clone(),
                location,
            });
        };
        self.any_labels = true;
        Ok(())
    }

    fn unlabelled(&mut self, location: SrcSpan) -> Result<(), Error> {
        if self.any_labels {
            return Err(Error::UnlabelledAfterlabelled { location });
        }
        Ok(())
    }

    pub fn finish(self) -> Option<FieldMap> {
        self.field_map.into_option()
    }
}
