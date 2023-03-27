// TODO: remove
#![allow(unused)]

//! An implementation of the algorithm described at
//! https://julesjacobs.com/notes/patternmatching/patternmatching.pdf.
//!
//! Adapted from Yorick Peterse's implementation at
//! https://github.com/yorickpeterse/pattern-matching-in-rust. Thank you Yorick!
//!
//! # How to compile pattern matching
//!
//! This directory contains an implementation of the algorithm discussed in the
//! article [How to compile pattern
//! matching](https://julesjacobs.com/notes/patternmatching/patternmatching.pdf) by
//! Jules Jacobs. The algorithm in question took me a while to understand, and I'm
//! grateful for all the help provided by Jules via Email. Thanks!
//!
//! Now on to the algorithm. In hindsight it ended up not being as difficult as I
//! initially thought, rather the way it was explained was a bit hard to understand.
//! The algorithm works as follows:
//!
//! First, we treat a match expression as if it were a table (in the database
//! sense), consisting of rows and columns. The rows are the match cases (sometimes
//! called "match arms"), and the columns the patterns to test. Consider this match
//! expression (I'm using Rust syntax here):
//!
//! ```text
//! match some_number {
//!     10 => one,
//!     20 => bar,
//!     30 => baz
//! }
//! ```
//!
//! Here `10 -> one`, `20 -> bar` and `30 -> baz` are the rows, and `10`, `20` and
//! `30` are the columns for each row. User provided match expressions only support
//! single columns (OR patterns are just turned into separate rows), but internally
//! the compiler supports multiple columns.
//!
//! Internally our match expression is represented not as a list of rows and columns
//! implicitly testing against an outer variable (`some_number` in the above case),
//! instead each column explicitly specifies what it tests against. This means the
//! above match expression is internally represented as follows:
//!
//! ```text
//! match {
//!     some_number is 10 => one,
//!     some_number is 20 => bar,
//!     some_number is 30 => baz
//! }
//! ```
//!
//! Here I used the made-up syntax `x is y` to indicate the column tests against the
//! variable `some_number`, and the pattern tested is e.g. `10`.
//!
//! Next, we need to get rid of variable patterns. This is done by pushing them into
//! the right-hand side (= the code to run upon a match) of each case. This means we
//! transform this expression:
//!
//!
//! ```text
//! match {
//!     some_number is 10 => one,
//!     some_number is num => bar
//! }
//! ```
//!
//! Into this:
//!
//! ```text
//! match {
//!     some_number is 10 => one,
//!     // I'm using "∅" here to signal a row without any columns.
//!     ∅ => {
//!         let num = some_number;
//!         bar
//!     }
//! }
//! ```
//!
//! The article explains this makes things easier, though it doesn't really say
//! clearly why. The reason for this is as follows:
//!
//! 1. It reduces the amount of duplication in the resulting decision tree, as we
//!    don't need to branch for variable and wildcard patterns.
//! 1. It means variable patterns don't influence branching decisions discussed
//!    below.
//! 1. When we branch on columns (again, discussed below), we can just forget about
//!    variable patterns.
//!
//! Essentially it takes the following steps:
//!
//! 1. Each right-hand side can store zero or more variables to define _before_
//!    running the code.
//! 1. Iterate over the columns in a row.
//! 1. If the column is a variable pattern, copy/move the variable into the
//!    right-hand side's variable list.
//! 1. Return a new row that only includes non-variable columns.
//!
//! The implementation handles this in the method `move_variable_patterns`.
//!
//! Now we need to decide what column to branch on. In practise it probably won't
//! matter much which strategy is used, so the algorithm takes a simple approach: it
//! takes the columns of the first row, and for every column counts how many times
//! the variable tested against is tested against across all columns in all rows. It
//! then returns the column of which the variable is tested against the most. The
//! implementation of this is in method `branch_variable`
//!
//! Now that we know what variable/column to branch on, we can generate the
//! necessary branches and sub trees. The article only covers simple constructor
//! patterns, but my implementation also handles integer literals, booleans, and
//! more. The exact approach differs a bit and I recommend studying the Rust code to
//! get a better understanding, but it roughly works as follows:
//!
//! 1. Create an array containing triples in the form
//!   `(constructor, arguments, rows)`. In this triple `constructor` is the
//!    constructor we're testing against, `arguments` is a list of variables exposed
//!    to the sub tree, and `rows` is the list of rows to compile for this test.
//!    The `arguments` array is filled with one variable for every argument.
//! 1. Iterate over all the current rows.
//! 1. Obtain the column index of the branching variable.
//! 1. If we found an index (remember that a now doesn't have to contain any columns
//!    testing the branching variable), use it to remove the column from the row.
//! 1. Determine the index of the constructor in the array created in step 1. For
//!    ADTs you'd use the tag values, for booleans you could use 0 and 1 for false
//!    and true respectively, etc.
//! 1. Zip the pattern arguments (also patterns) with the values in the `arguments`
//!    array from the triple for this constructor, and create a new column for every
//!    resulting pair.
//! 1. Create a new row containing the old columns (minus the one we removed
//!    earlier), the new columns (created in the previous step), and the body of the
//!    row. Push this row into the `rows` array for our constructor.
//! 1. If in step 3 we didn't find an index, copy the row into the `rows` array for
//!    every triple in the array created in step 1.
//! 1. Finally, for every triple created in step 1 (and populated in later steps),
//!    create a Switch node for our decision tree. The constructor and arguments are
//!    stored in this Switch node, and the rows are compiled into a sub tree.
//!
//! This is a lot to take in, so I recommend taking a look at the following methods:
//!
//! - `compile_rows`
//! - `compile_constructor_cases`
//!
//! The output of all this is a decision tree, with three possible nodes: Success,
//! Failure, and Switch (see the `Decision` type). A "Failure" node indicates a
//! pattern that didn't match, and is used to check for exhaustiveness. In my
//! implementation I opted to check for exhaustiveness separately, as this saves us
//! from having to manage some extra data structures until we actually need them.
//! The implementation works as follows:
//!
//! When we produce a "Failure" node, a "missing" flag is set to `true`. After
//! compiling our decision tree, we check this flag. If set to `true`, the method
//! `Match::missing_patterns` is used to produce a list of patterns to add to make
//! the match exhaustive.
//!
//! The implementation of this method is a bit messy in my opinion, but it's the
//! best I could come up with at this time. The implementation essentially maintains
//! a stack of "terms" (I couldn't come up with a better name), each describing a
//! test and its arguments in the tree. These terms also store the variables tested
//! against, which combined with the names is used to (recursively) reconstruct a
//! pattern name.
//!
//! Checking for redundant patterns is easy: when reaching a "Success" node you'd
//! somehow mark the right-hand side as processed. In my case I just store an
//! integer value in an array. At the end you check for any right-hand sides that
//! aren't marked, or in my case you check if any of their values are not in the
//! array.
//!
//! This about sums up how the algorithm works. Don't worry if the above wall of
//! text hurts your head, it took me about two weeks to understand it. My advice is
//! to read the article from Jules, then read this README, then take a look at the
//! code and corresponding tests.
//!
//! ## OR patterns
//!
//! OR patterns are not covered in the article, but supporting them is easy.
//! Supporting these requires an extra `flatten_or` function that takes as input a
//! pattern and a `Row`, returning an array of `(Pattern, Row)` tuples. If the input
//! pattern is an OR pattern, it returns its sub patterns zipped with a copy of the
//! input row. If the input pattern is any other pattern, the function just returns
//! an array of the input pattern and row:
//!
//! ```text
//! fn flatten_or(pattern: Pattern, row: Row) -> Vec<(Pattern, Row)> {
//!     if let Pattern::Or(args) = pattern {
//!         args.into_iter().map(|p| (p, row.clone())).collect()
//!     } else {
//!         vec![(pattern, row)]
//!     }
//! }
//! ```
//!
//! When removing the branch column from a row you then use this function, instead
//! of acting upon a column's pattern directly:
//!
//! ```text
//! if let Some(col) = row.remove_column(&branch_var) {
//!     for (pat, row) in flatten_or(col.pattern, row) {
//!         ...
//!     }
//! } else {
//!     ...
//! }
//! ```
//!
//! ## Guards
//!
//! Guards are supported as follows: each `Row` has a guard field, storing a
//! `Option<usize>`, where the `usize` is just a dummy value for the guard; normally
//! this would be (for example) an AST node to evaluate/lower. When we are about to
//! produce a Success node for a row, we check if it defines a guard. If so, all
//! remaining rows are compiled into the guard's fallback tree.

#[cfg(test)]
mod tests;

use std::collections::{HashMap, HashSet};

use smol_str::SmolStr;

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
    True,
    False,
    Int(SmolStr),
    Float(SmolStr),
    Tuple(Vec<TypeId>),
    Variant(TypeId, usize),
}

impl Constructor {
    /// Returns the index of this constructor relative to its type.
    fn index(&self) -> usize {
        match self {
            Constructor::False
            | Constructor::Float(_)
            | Constructor::Int(_)
            | Constructor::Tuple(_) => 0,
            Constructor::True => 1,
            Constructor::Variant(_, index) => *index,
        }
    }
}

/// A user defined pattern such as `Some((x, 10))`.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Pattern {
    /// A pattern such as `Some(42)`.
    Constructor(Constructor, Vec<Pattern>),
    Float(SmolStr),
    Int(SmolStr),
    Variable(SmolStr),
    Discard,
    Or(Vec<Pattern>),
}

impl Pattern {
    fn flatten_or(self, row: Row) -> Vec<(Pattern, Row)> {
        match self {
            Pattern::Or(args) => args.into_iter().map(|p| (p, row.clone())).collect(),

            Pattern::Constructor(_, _)
            | Pattern::Int(_)
            | Pattern::Float(_)
            | Pattern::Discard
            | Pattern::Variable(_) => vec![(self, row)],
        }
    }

    /// Returns true if this pattern always matches no matter what the value is.
    fn is_unconditional(&self) -> bool {
        match self {
            Pattern::Variable(_) | Pattern::Discard => true,
            Pattern::Constructor(_, _) | Pattern::Float(_) | Pattern::Int(_) | Pattern::Or(_) => {
                false
            }
        }
    }
}

/// A representation of a type.
///
/// In a real compiler this would probably be a more complicated structure, but
/// for the sake of simplicity we limit ourselves to a few basic types.
#[derive(Clone)]
pub enum Type {
    Int,
    Float,
    Boolean,
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
    pattern: Pattern,
}

impl Column {
    fn new(variable: Variable, pattern: Pattern) -> Self {
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
}

/// A type for storing diagnostics produced by the decision tree compiler.
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
struct Term {
    variable: Variable,
    name: SmolStr,
    arguments: Vec<Variable>,
}

impl Term {
    fn new(variable: Variable, name: SmolStr, arguments: Vec<Variable>) -> Self {
        Self {
            variable,
            name,
            arguments,
        }
    }

    fn pattern_name(&self, terms: &[Term], mapping: &HashMap<&Variable, usize>) -> SmolStr {
        if self.arguments.is_empty() {
            self.name.clone()
        } else {
            let args = self
                .arguments
                .iter()
                .map(|arg| {
                    mapping
                        .get(arg)
                        .map(|&idx| terms[idx].pattern_name(terms, mapping))
                        .unwrap_or_else(|| "_".into())
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", self.name, args).into()
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
                    _ = mapping.insert(&step.variable, index);
                }

                let name = terms
                    .first()
                    .map(|term| term.pattern_name(terms, &mapping))
                    .unwrap_or_else(|| "_".into());

                _ = missing.insert(name);
            }
            Decision::Guard(_, _, fallback) => {
                self.add_missing_patterns(fallback, terms, missing);
            }
            Decision::Switch(var, cases, fallback) => {
                for case in cases {
                    match &case.constructor {
                        Constructor::True => {
                            let name = "true".into();
                            terms.push(Term::new(*var, name, Vec::new()));
                        }
                        Constructor::False => {
                            let name = "false".into();
                            terms.push(Term::new(*var, name, Vec::new()));
                        }
                        Constructor::Int(_) => {
                            let name = "_".into();
                            terms.push(Term::new(*var, name, Vec::new()));
                        }
                        Constructor::Float(_) => {
                            let name = "_".into();
                            terms.push(Term::new(*var, name, Vec::new()));
                        }
                        Constructor::Tuple(_) => {
                            let args = case.arguments.clone();
                            terms.push(Term::new(*var, "#".into(), args));
                        }
                        Constructor::Variant(typ, idx) => {
                            let args = case.arguments.clone();
                            let name = if let Type::Enum(variants) = &self.types[typ.0] {
                                variants[*idx].0.clone()
                            } else {
                                unreachable!()
                            };
                            terms.push(Term::new(*var, name, args));
                        }
                    }

                    self.add_missing_patterns(&case.body, terms, missing);
                    _ = terms.pop();
                }

                if let Some(node) = fallback {
                    self.add_missing_patterns(node, terms, missing);
                }
            }
        }
    }
}

/// The `match` compiler itself (shocking, I know).
pub struct Compiler {
    variable_id: usize,
    types: Vec<Type>,
    diagnostics: Diagnostics,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
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
            Type::Float | Type::Int => {
                let (cases, fallback) = self.compile_number_cases(rows, branch_var);
                Decision::Switch(branch_var, cases, Some(fallback))
            }

            Type::Boolean => {
                let cases = vec![
                    (Constructor::False, Vec::new(), Vec::new()),
                    (Constructor::True, Vec::new(), Vec::new()),
                ];
                let cases = self.compile_constructor_cases(rows, branch_var, cases);
                Decision::Switch(branch_var, cases, None)
            }

            Type::Tuple(types) => {
                let variables = self.new_variables(&types);
                let cases = vec![(Constructor::Tuple(types), variables, Vec::new())];
                let cases = self.compile_constructor_cases(rows, branch_var, cases);
                Decision::Switch(branch_var, cases, None)
            }

            Type::Enum(variants) => {
                let cases = variants
                    .iter()
                    .enumerate()
                    .map(|(idx, (_, args))| {
                        let variant = Constructor::Variant(branch_var.type_id, idx);
                        (variant, self.new_variables(args), Vec::new())
                    })
                    .collect();
                Decision::Switch(
                    branch_var,
                    self.compile_constructor_cases(rows, branch_var, cases),
                    None,
                )
            }
        }
    }

    /// Compiles the cases and fallback cases for int and float patterns.
    ///
    /// Ints and floats have an infinite number of constructors, so we
    /// specialise the compilation of their patterns.
    fn compile_number_cases(
        &mut self,
        rows: Vec<Row>,
        branch_var: Variable,
    ) -> (Vec<Case>, Box<Decision>) {
        let mut raw_cases: Vec<(Constructor, Vec<Variable>, Vec<Row>)> = Vec::new();
        let mut fallback_rows = Vec::new();
        let mut tested: HashMap<(SmolStr, SmolStr), usize> = HashMap::new();

        for mut row in rows {
            if let Some(col) = row.remove_column(&branch_var) {
                for (pat, row) in col.pattern.flatten_or(row) {
                    let (key, cons) = match pat {
                        Pattern::Int(val) => ((val.clone(), val.clone()), Constructor::Int(val)),
                        Pattern::Float(val) => {
                            ((val.clone(), val.clone()), Constructor::Float(val))
                        }
                        Pattern::Constructor(_, _)
                        | Pattern::Variable(_)
                        | Pattern::Discard
                        | Pattern::Or(_) => unreachable!(),
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
    /// If a row didn't include the branching variable, we simply copy that row
    /// into the list of rows for every constructor to test.
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
            if let Some(col) = row.remove_column(&branch_var) {
                for (pat, row) in col.pattern.flatten_or(row) {
                    if let Pattern::Constructor(cons, args) = pat {
                        let idx = cons.index();
                        let mut cols = row.columns;

                        for (var, pat) in cases[idx].1.iter().zip(args.into_iter()) {
                            cols.push(Column::new(*var, pat));
                        }

                        cases[idx].2.push(Row::new(cols, row.guard, row.body));
                    }
                }
            } else {
                for (_, _, rows) in &mut cases {
                    rows.push(row.clone());
                }
            }
        }

        cases
            .into_iter()
            .map(|(cons, vars, rows)| Case::new(cons, vars, self.compile_rows(rows)))
            .collect()
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

        for col in &row.columns {
            if let Pattern::Variable(bind) = &col.pattern {
                bindings.push((bind.clone(), col.variable));
            }
        }

        let columns = row
            .columns
            .into_iter()
            .filter(|col| !col.pattern.is_unconditional())
            .collect();

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
