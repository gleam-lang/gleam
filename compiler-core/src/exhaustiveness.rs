// TODO: remove
#![allow(unused)]

//! An implementation of the algorithm described at
//! https://julesjacobs.com/notes/patternmatching/patternmatching.pdf.
//!
//! Adapted from Yorick Peterse's implementation at
//! https://github.com/yorickpeterse/pattern-matching-in-rust. Thank you Yorick!

#[cfg(test)]
mod tests;

use crate::ast::AssignName;
use id_arena::{Arena, Id};
use itertools::Itertools;
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};

type PatternId = Id<Pattern>;

/// The body of code to evaluate in case of a match.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Body {
    /// Any variables to bind before running the code.
    ///
    /// The tuples are in the form `(name, source)` (i.e `bla = source`).
    bindings: Vec<(SmolStr, Variable)>,

    /// The index of the clause in the case expression that should be run.
    clause_index: u16,
}

/// A type constructor.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constructor {
    Int(SmolStr),
    Float(SmolStr),
    Tuple(Vec<TypeId>),
    Variant(TypeId, usize),
    String(SmolStr),
}

impl Constructor {
    /// Returns the index of this constructor relative to its type.
    fn index(&self) -> usize {
        match self {
            Constructor::Int(_)
            | Constructor::Float(_)
            | Constructor::Tuple(_)
            | Constructor::String(_) => 0,

            Constructor::Variant(_, index) => *index,
        }
    }
}

// VarUsage { name: SmolStr, type_: Type, },
// BitString { segments: Vec<BitStringSegment<Self, Type>>, },
// Concatenate { left_side_string: SmolStr, right_side_assignment: AssignName, },

/// A user defined pattern such as `Some((x, 10))`.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Pattern {
    Discard,
    Or {
        left: PatternId,
        right: PatternId,
    },
    Int {
        value: SmolStr,
    },
    Float {
        value: SmolStr,
    },
    String {
        value: SmolStr,
    },
    Assign {
        name: SmolStr,
        pattern: PatternId,
    },
    Variable {
        value: SmolStr,
    },
    Tuple {
        elements: Vec<PatternId>,
    },
    Constructor {
        constructor: Constructor,
        arguments: Vec<PatternId>,
    },
    List {
        type_: TypeId,
        first: PatternId,
        rest: PatternId,
    },
    EmptyList,
}

/// A representation of a type.
///
/// In a real compiler this would probably be a more complicated structure, but
/// for the sake of simplicity we limit ourselves to a few basic types.
#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Float,
    String,
    List(TypeId),
    Tuple(Vec<TypeId>),
    Enum(Vec<(SmolStr, Vec<TypeId>)>),
}

/// A unique ID to a type.
///
/// In a real compiler this may just be a regular pointer, or an ID value like
/// this.
#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub struct TypeId(usize);

/// A variable used in a match expression.
///
/// In a real compiler these would probably be registers or some other kind of
/// variable/temporary generated by your compiler.
#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub struct Variable {
    id: usize,
    type_id: TypeId,
}

/// A single case (or row) in a match expression/table.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Row {
    columns: Vec<Column>,
    guard: Option<usize>,
    body: Body,
}

impl Row {
    fn new(columns: Vec<Column>, guard: Option<usize>, body: Body) -> Self {
        Self {
            columns,
            guard,
            body,
        }
    }

    fn remove_column(&mut self, variable: &Variable) -> Option<Column> {
        self.columns
            .iter()
            .position(|c| &c.variable == variable)
            .map(|idx| self.columns.remove(idx))
    }
}

/// A column in a pattern matching table.
///
/// A column contains a single variable to test, and a pattern to test against
/// that variable. A row may contain multiple columns, though this wouldn't be
/// exposed to the source language (= it's an implementation detail).
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Column {
    variable: Variable,
    pattern: PatternId,
}

impl Column {
    fn new(variable: Variable, pattern: PatternId) -> Self {
        Self { variable, pattern }
    }
}

/// A case in a decision tree to test against a variable.
#[derive(Eq, PartialEq, Debug)]
pub struct Case {
    /// The constructor to test against an input variable.
    constructor: Constructor,

    /// Variables to introduce to the body of this case.
    ///
    /// At runtime these would be populated with the values a pattern is matched
    /// against. For example, this pattern:
    ///
    /// ```text
    /// case (10, 20, one) -> ...
    /// ```
    ///
    /// Would result in three arguments, assigned the values `10`, `20` and
    /// `one`.
    ///
    /// In a real compiler you'd assign these variables in your IR first, then
    /// generate the code for the sub tree.
    arguments: Vec<Variable>,

    /// The sub tree of this case.
    body: Decision,
}

impl Case {
    fn new(constructor: Constructor, arguments: Vec<Variable>, body: Decision) -> Self {
        Self {
            constructor,
            arguments,
            body,
        }
    }
}

/// A decision tree compiled from a list of match cases.
#[derive(Eq, PartialEq, Debug)]
pub enum Decision {
    /// A pattern is matched and the right-hand value is to be returned.
    Success(Body),

    /// A pattern is missing.
    Failure,

    /// Checks if a guard evaluates to true, running the body if it does.
    ///
    /// The arguments are as follows:
    ///
    /// 1. The "condition" to evaluate. We just use a dummy value, but in a real
    ///    compiler this would likely be an AST node of sorts.
    /// 2. The body to evaluate if the guard matches.
    /// 3. The sub tree to evaluate when the guard fails.
    Guard(usize, Body, Box<Decision>),

    /// Checks if a value is any of the given patterns.
    ///
    /// The values are as follows:
    ///
    /// 1. The variable to test.
    /// 2. The cases to test against this variable.
    /// 3. A fallback decision to take, in case none of the cases matched.
    Switch(Variable, Vec<Case>, Option<Box<Decision>>),

    /// Checks if a list is empty or non-empty.
    List {
        /// The variable to test.
        variable: Variable,
        /// The decision to take if the list is empty.
        empty: Box<Decision>,
        /// The decision to take if the list is non-empty.
        non_empty: Box<NonEmptyListDecision>,
    },
}

#[derive(Eq, PartialEq, Debug)]
pub struct NonEmptyListDecision {
    first: Variable,
    rest: Variable,
    decision: Decision,
}

/// A type for storing diagnostics produced by the decision tree compiler.
#[derive(Debug)]
pub struct Diagnostics {
    /// A flag indicating the match is missing one or more pattern.
    missing: bool,

    /// The right-hand sides that are reachable.
    ///
    /// If a right-hand side isn't in this list it means its pattern is
    /// redundant.
    reachable: Vec<u16>,
}

/// The result of compiling a pattern match expression.
pub struct Match {
    pub types: Vec<Type>,
    pub tree: Decision,
    pub diagnostics: Diagnostics,
}

/// Information about a single constructor/value (aka term) being tested, used
/// to build a list of names of missing patterns.
#[derive(Debug)]
enum Term {
    Variant {
        variable: Variable,
        name: SmolStr,
        arguments: Vec<Variable>,
    },
    Infinite {
        variable: Variable,
    },
    EmptyList {
        variable: Variable,
    },
    List {
        variable: Variable,
        first: Variable,
        rest: Variable,
    },
}

impl Term {
    fn variable(&self) -> Variable {
        match self {
            Term::Variant { variable, .. } => *variable,
            Term::Infinite { variable } => *variable,
            Term::EmptyList { variable } => *variable,
            Term::List { variable, .. } => *variable,
        }
    }

    fn pattern_string(&self, terms: &[Term], mapping: &HashMap<Variable, usize>) -> SmolStr {
        match self {
            Term::Variant {
                variable,
                name,
                arguments,
            } => {
                if arguments.is_empty() {
                    return name.clone();
                }
                let args = arguments
                    .iter()
                    .map(|arg| {
                        mapping
                            .get(arg)
                            .map(|&idx| terms[idx].pattern_string(terms, mapping))
                            .unwrap_or_else(|| "_".into())
                    })
                    .join(", ");
                format!("{}({})", name, args).into()
            }

            Term::Infinite { variable } => "_".into(),

            Term::EmptyList { variable } => "[]".into(),

            Term::List { .. } => format!("[{}]", self.list_pattern_string(terms, mapping)).into(),
        }
    }

    fn list_pattern_string(&self, terms: &[Term], mapping: &HashMap<Variable, usize>) -> SmolStr {
        match self {
            Term::Infinite { .. } | Term::Variant { .. } => "_".into(),

            Term::EmptyList { .. } => "".into(),

            Term::List {
                variable,
                first,
                rest,
            } => {
                let first = mapping
                    .get(first)
                    .map(|&idx| terms[idx].list_pattern_string(terms, mapping))
                    .unwrap_or_else(|| "_".into());
                let rest = mapping
                    .get(rest)
                    .map(|&idx| terms[idx].list_pattern_string(terms, mapping))
                    .unwrap_or_else(|| "_".into());

                match rest.as_str() {
                    "" => first,
                    "_" => format!("{first}, ..").into(),
                    _ => format!("{first}, {rest}").into(),
                }
            }
        }
    }
}

impl Match {
    /// Returns a list of patterns not covered by the match expression.
    pub fn missing_patterns(&self) -> Vec<SmolStr> {
        let mut names = HashSet::new();
        let mut steps = Vec::new();

        self.add_missing_patterns(&self.tree, &mut steps, &mut names);

        let mut missing: Vec<SmolStr> = names.into_iter().collect();

        // Sorting isn't necessary, but it makes it a bit easier to write tests.
        missing.sort();
        missing
    }

    fn add_missing_patterns(
        &self,
        node: &Decision,
        terms: &mut Vec<Term>,
        missing: &mut HashSet<SmolStr>,
    ) {
        match node {
            Decision::Success(_) => {}

            Decision::Failure => {
                let mut mapping = HashMap::new();

                // At this point the terms stack looks something like this:
                // `[term, term + arguments, term, ...]`. To construct a pattern
                // name from this stack, we first map all variables to their
                // term indexes. This is needed because when a term defines
                // arguments, the terms for those arguments don't necessarily
                // appear in order in the term stack.
                //
                // This mapping is then used when (recursively) generating a
                // pattern name.
                //
                // This approach could probably be done more efficiently, so if
                // you're reading this and happen to know of a way, please
                // submit a merge request :)
                for (index, step) in terms.iter().enumerate() {
                    _ = mapping.insert(step.variable(), index);
                }

                let name = terms
                    .first()
                    .map(|term| term.pattern_string(terms, &mapping))
                    .unwrap_or_else(|| "_".into());

                _ = missing.insert(name);
            }

            Decision::Guard(_, _, fallback) => {
                self.add_missing_patterns(fallback, terms, missing);
            }

            Decision::Switch(variable, cases, fallback) => {
                let variable = *variable;
                for case in cases {
                    match &case.constructor {
                        Constructor::Int(_) | Constructor::Float(_) | Constructor::String(_) => {
                            terms.push(Term::Infinite { variable });
                        }

                        Constructor::Tuple(_) => {
                            let arguments = case.arguments.clone();
                            terms.push(Term::Variant {
                                variable,
                                name: "#".into(),
                                arguments,
                            });
                        }

                        Constructor::Variant(type_, idx) => {
                            let name = if let Type::Enum(variants) = &self.types[type_.0] {
                                variants[*idx].0.clone()
                            } else {
                                unreachable!()
                            };
                            terms.push(Term::Variant {
                                variable,
                                name,
                                arguments: case.arguments.clone(),
                            });
                        }
                    }

                    self.add_missing_patterns(&case.body, terms, missing);
                    _ = terms.pop();
                }

                if let Some(node) = fallback {
                    self.add_missing_patterns(node, terms, missing);
                }
            }

            Decision::List {
                variable,
                empty,
                non_empty,
            } => {
                let variable = *variable;

                terms.push(Term::EmptyList { variable });
                self.add_missing_patterns(empty, terms, missing);
                _ = terms.pop();

                terms.push(Term::List {
                    variable,
                    first: non_empty.first,
                    rest: non_empty.rest,
                });
                self.add_missing_patterns(&non_empty.decision, terms, missing);
                _ = terms.pop();
            }
        }
    }
}

/// The `match` compiler itself (shocking, I know).
#[derive(Debug)]
pub struct Compiler {
    variable_id: usize,
    types: Vec<Type>,
    diagnostics: Diagnostics,
    patterns: Arena<Pattern>,
}

impl Compiler {
    pub fn new(patterns: Arena<Pattern>) -> Self {
        Self {
            patterns,
            variable_id: 0,
            types: Vec::new(),
            diagnostics: Diagnostics {
                missing: false,
                reachable: Vec::new(),
            },
        }
    }

    pub fn compile(mut self, rows: Vec<Row>) -> Match {
        Match {
            tree: self.compile_rows(rows),
            diagnostics: self.diagnostics,
            types: self.types,
        }
    }

    fn pattern(&self, id: PatternId) -> &Pattern {
        self.patterns.get(id).expect("Unknown pattern id")
    }

    fn flatten_or(&self, id: PatternId, row: Row) -> Vec<(PatternId, Row)> {
        match self.pattern(id) {
            Pattern::Or { left, right } => vec![(*left, row.clone()), (*right, row.clone())],

            Pattern::Int { .. }
            | Pattern::List { .. }
            | Pattern::Tuple { .. }
            | Pattern::Float { .. }
            | Pattern::Assign { .. }
            | Pattern::String { .. }
            | Pattern::Discard
            | Pattern::Variable { .. }
            | Pattern::EmptyList
            | Pattern::Constructor { .. } => vec![(id, row)],
        }
    }

    fn compile_rows(&mut self, rows: Vec<Row>) -> Decision {
        if rows.is_empty() {
            self.diagnostics.missing = true;
            return Decision::Failure;
        }

        let mut rows = rows
            .into_iter()
            .map(|row| self.move_unconditional_patterns(row))
            .collect::<Vec<_>>();

        // There may be multiple rows, but if the first one has no patterns
        // those extra rows are redundant, as a row without columns/patterns
        // always matches.
        if rows.first().map_or(false, |c| c.columns.is_empty()) {
            let row = rows.remove(0);
            self.diagnostics.reachable.push(row.body.clause_index);

            return if let Some(guard) = row.guard {
                Decision::Guard(guard, row.body, Box::new(self.compile_rows(rows)))
            } else {
                Decision::Success(row.body)
            };
        }

        let branch_var = self.branch_variable(&rows[0], &rows);

        match self.variable_type(branch_var).clone() {
            Type::String | Type::Float | Type::Int => {
                let (cases, fallback) = self.compile_infinite_cases(rows, branch_var);
                Decision::Switch(branch_var, cases, Some(fallback))
            }

            Type::Tuple(types) => {
                let variables = self.new_variables(&types);
                let cases = vec![(Constructor::Tuple(types), variables, Vec::new())];
                let cases = self.compile_constructor_cases(rows, branch_var, cases);
                Decision::Switch(branch_var, cases, None)
            }

            Type::List(elem) => self.compile_list_cases(rows, branch_var, elem),

            Type::Enum(variants) => {
                let cases = variants
                    .iter()
                    .enumerate()
                    .map(|(idx, (_, args))| {
                        let variant = Constructor::Variant(branch_var.type_id, idx);
                        (variant, self.new_variables(args), Vec::new())
                    })
                    .collect();
                let cases = self.compile_constructor_cases(rows, branch_var, cases);
                Decision::Switch(branch_var, cases, None)
            }
        }
    }

    /// String, ints and floats have an infinite number of constructors, so we
    /// specialise the compilation of their patterns with this function.
    fn compile_infinite_cases(
        &mut self,
        rows: Vec<Row>,
        branch_var: Variable,
    ) -> (Vec<Case>, Box<Decision>) {
        let mut raw_cases: Vec<(Constructor, Vec<Variable>, Vec<Row>)> = Vec::new();
        let mut fallback_rows = Vec::new();
        let mut tested: HashMap<(SmolStr, SmolStr), usize> = HashMap::new();

        for mut row in rows {
            if let Some(col) = row.remove_column(&branch_var) {
                for (pat, row) in self.flatten_or(col.pattern, row) {
                    let (key, cons) = match self.pattern(pat) {
                        Pattern::Int { value: val } => {
                            ((val.clone(), val.clone()), Constructor::Int(val.clone()))
                        }
                        Pattern::Float { value: val } => {
                            ((val.clone(), val.clone()), Constructor::Float(val.clone()))
                        }
                        Pattern::String { value: val } => {
                            ((val.clone(), val.clone()), Constructor::String(val.clone()))
                        }

                        pattern @ (Pattern::Constructor { .. }
                        | Pattern::Assign { .. }
                        | Pattern::Tuple { .. }
                        | Pattern::Variable { .. }
                        | Pattern::Discard
                        | Pattern::EmptyList
                        | Pattern::List { .. }
                        | Pattern::Or { .. }) => panic!("Unexpected pattern {:?}", pattern),
                    };

                    if let Some(index) = tested.get(&key) {
                        raw_cases[*index].2.push(row);
                        continue;
                    }

                    _ = tested.insert(key, raw_cases.len());
                    raw_cases.push((cons, Vec::new(), vec![row]));
                }
            } else {
                fallback_rows.push(row);
            }
        }

        for (_, _, rows) in &mut raw_cases {
            rows.append(&mut fallback_rows.clone());
        }

        let cases = raw_cases
            .into_iter()
            .map(|(cons, vars, rows)| Case::new(cons, vars, self.compile_rows(rows)))
            .collect();

        (cases, Box::new(self.compile_rows(fallback_rows)))
    }

    /// Compiles the cases and sub cases for the constructor located at the
    /// column of the branching variable.
    ///
    /// What exactly this method does may be a bit hard to understand from the
    /// code, as there's simply quite a bit going on. Roughly speaking, it does
    /// the following:
    ///
    /// 1. It takes the column we're branching on (based on the branching
    ///    variable) and removes it from every row.
    /// 2. We add additional columns to this row, if the constructor takes any
    ///    arguments (which we'll handle in a nested match).
    /// 3. We turn the resulting list of rows into a list of cases, then compile
    ///    those into decision (sub) trees.
    ///
    /// If a row didn't include the branching variable, we copy that row into
    /// the list of rows for every constructor to test.
    ///
    /// For this to work, the `cases` variable must be prepared such that it has
    /// a triple for every constructor we need to handle. For an ADT with 10
    /// constructors, that means 10 triples. This is needed so this method can
    /// assign the correct sub matches to these constructors.
    ///
    /// Types with infinite constructors (e.g. int and string) are handled
    /// separately; they don't need most of this work anyway.
    fn compile_constructor_cases(
        &mut self,
        rows: Vec<Row>,
        branch_var: Variable,
        mut cases: Vec<(Constructor, Vec<Variable>, Vec<Row>)>,
    ) -> Vec<Case> {
        for mut row in rows {
            let column = match row.remove_column(&branch_var) {
                // This row had the branching variable, so we compile it below.
                Some(column) => column,

                // This row didn't have the branching variable, meaning it does
                // not match on this constructor. In this case we copy the row
                // into each of the other cases.
                None => {
                    for (_, _, other_case_rows) in &mut cases {
                        other_case_rows.push(row.clone());
                    }
                    continue;
                }
            };

            for (pattern, row) in self.flatten_or(column.pattern, row) {
                // We should only be able to reach constructors here for well
                // typed code. Invalid patterns should have been caught by
                // earlier analysis.
                let (cons, args) = match self.pattern(pattern) {
                    Pattern::Constructor {
                        constructor,
                        arguments,
                    } => (constructor.clone(), arguments.clone()),

                    pattern @ (Pattern::Or { .. }
                    | Pattern::Int { .. }
                    | Pattern::List { .. }
                    | Pattern::Float { .. }
                    | Pattern::Tuple { .. }
                    | Pattern::String { .. }
                    | Pattern::Assign { .. }
                    | Pattern::Discard
                    | Pattern::Variable { .. }
                    | Pattern::EmptyList) => panic!("Unexpected pattern {:?}", pattern),
                };

                let index = cons.index();
                let mut columns = row.columns;

                for (var, pattern) in cases[index].1.iter().zip(args.into_iter()) {
                    columns.push(Column::new(*var, pattern));
                }

                cases[index].2.push(Row::new(columns, row.guard, row.body));
            }
        }

        cases
            .into_iter()
            .map(|(cons, vars, rows)| Case::new(cons, vars, self.compile_rows(rows)))
            .collect()
    }

    fn compile_list_cases(
        &mut self,
        rows: Vec<Row>,
        branch_var: Variable,
        element_type: TypeId,
    ) -> Decision {
        let mut empty_rows = vec![];
        let mut non_empty_rows = vec![];
        let first_var = self.new_variable(element_type);
        let rest_var = self.new_variable(branch_var.type_id);

        for mut row in rows {
            let column = match row.remove_column(&branch_var) {
                // This row had the branching variable, so we compile it below.
                Some(column) => column,

                // This row didn't have the branching variable, meaning it does
                // not match on this constructor. In this case we copy the row
                // into each of the other cases.
                None => {
                    empty_rows.push(row.clone());
                    non_empty_rows.push(row.clone());
                    continue;
                }
            };

            for (pattern, row) in self.flatten_or(column.pattern, row) {
                let mut columns = row.columns;

                // We should only be able to reach list patterns here for well
                // typed code. Invalid patterns should have been caught by
                // earlier analysis.
                match self.pattern(pattern) {
                    Pattern::EmptyList => {
                        empty_rows.push(Row::new(columns, row.guard, row.body));
                    }

                    Pattern::List { first, rest, .. } => {
                        columns.push(Column::new(first_var, *first));
                        columns.push(Column::new(rest_var, *rest));
                        non_empty_rows.push(Row::new(columns, row.guard, row.body));
                    }

                    pattern @ (Pattern::Or { .. }
                    | Pattern::Int { .. }
                    | Pattern::Float { .. }
                    | Pattern::Tuple { .. }
                    | Pattern::String { .. }
                    | Pattern::Discard
                    | Pattern::Assign { .. }
                    | Pattern::Variable { .. }
                    | Pattern::Constructor { .. }) => {
                        panic!("Unexpected non-list pattern {:?}", pattern)
                    }
                };
            }
        }

        Decision::List {
            variable: branch_var,
            empty: Box::new(self.compile_rows(empty_rows)),
            non_empty: Box::new(NonEmptyListDecision {
                first: first_var,
                rest: rest_var,
                decision: self.compile_rows(non_empty_rows),
            }),
        }
    }

    /// Moves variable-only patterns/tests into the right-hand side/body of a
    /// case.
    ///
    /// This turns cases like this:
    ///
    /// ```text
    /// case one -> print(one)
    /// case _ -> print("nothing")
    /// ```
    ///
    /// Into this:
    ///
    /// ```text
    /// case -> {
    ///     let one = it
    ///     print(one)
    /// }
    /// case -> {
    ///     print("nothing")
    /// }
    /// ```
    ///
    /// Where `it` is a variable holding the value `case one` is compared
    /// against, and the case/row has no patterns (i.e. always matches).
    fn move_unconditional_patterns(&self, row: Row) -> Row {
        let mut bindings = row.body.bindings;
        let mut columns = Vec::new();
        let mut iterator = row.columns.into_iter();
        let mut next = iterator.next();

        while let Some(column) = next {
            match self.pattern(column.pattern) {
                Pattern::Discard => {
                    next = iterator.next();
                }

                Pattern::Variable { value: bind } => {
                    next = iterator.next();
                    bindings.push((bind.clone(), column.variable));
                }

                Pattern::Assign { name, pattern } => {
                    next = Some(Column::new(column.variable, *pattern));
                    bindings.push((name.clone(), column.variable));
                }

                Pattern::Or { .. }
                | Pattern::Int { .. }
                | Pattern::List { .. }
                | Pattern::Float { .. }
                | Pattern::Tuple { .. }
                | Pattern::String { .. }
                | Pattern::EmptyList
                | Pattern::Constructor { .. } => {
                    next = iterator.next();
                    columns.push(column);
                }
            }
        }

        Row {
            columns,
            guard: row.guard,
            body: Body {
                bindings,
                clause_index: row.body.clause_index,
            },
        }
    }

    /// Given a row, returns the variable in that row that's referred to the
    /// most across all rows.
    fn branch_variable(&self, row: &Row, rows: &[Row]) -> Variable {
        let mut counts = HashMap::new();

        for row in rows {
            for col in &row.columns {
                *counts.entry(&col.variable).or_insert(0_usize) += 1
            }
        }

        row.columns
            .iter()
            .map(|col| col.variable)
            .max_by_key(|var| counts[var])
            .expect("Row must have at least one column")
    }

    /// Returns a new variable to use in the decision tree.
    ///
    /// In a real compiler you'd have to ensure these variables don't conflict
    /// with other variables.
    fn new_variable(&mut self, type_id: TypeId) -> Variable {
        let var = Variable {
            id: self.variable_id,
            type_id,
        };

        self.variable_id += 1;
        var
    }

    fn new_variables(&mut self, type_ids: &[TypeId]) -> Vec<Variable> {
        type_ids.iter().map(|t| self.new_variable(*t)).collect()
    }

    /// Returns the type of a given variable.
    ///
    /// In a real compiler the implementation of this would likely be quite
    /// different, depending on how your type system is implemented.
    ///
    /// For the sake of simplicity, we just store types in a Vec and retrieve
    /// them here according to the variable's type ID.
    fn variable_type(&self, id: Variable) -> &Type {
        &self.types[id.type_id.0]
    }
}
