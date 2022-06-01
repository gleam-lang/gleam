# How to compile pattern matching

This directory contains an implementation of the algorithm discussed in the
article [How to compile pattern
matching](https://julesjacobs.com/notes/patternmatching/patternmatching.pdf) by
Jules Jacobs. The algorithm in question took me a while to understand, and I'm
grateful for all the help provided by Jules via Email. Thanks!

Now on to the algorithm. In hindsight it ended up not being as difficult as I
initially thought, rather the way it was explained was a bit hard to understand.
The algorithm works as follows:

First, we treat a match expression as if it were a table (in the database
sense), consisting of rows and columns. The rows are the match cases (sometimes
called "match arms"), and the columns the patterns to test. Consider this match
expression (I'm using Rust syntax here):

```rust
match some_number {
    10 => foo,
    20 => bar,
    30 => baz
}
```

Here `10 -> foo`, `20 -> bar` and `30 -> baz` are the rows, and `10`, `20` and
`30` are the columns for each row. User provided match expressions only support
single columns (OR patterns are just turned into separate rows), but internally
the compiler supports multiple columns.

Internally our match expression is represented not as a list of rows and columns
implicitly testing against an outer variable (`some_number` in the above case),
instead each column explicitly specifies what it tests against. This means the
above match expression is internally represented as follows:

```rust
match {
  some_number is 10 => foo,
  some_number is 20 => bar,
  some_number is 30 => baz
}
```

Here I used the made-up syntax `x is y` to indicate the column tests against the
variable `some_number`, and the pattern tested is e.g. `10`.

Next, we need to get rid of variable patterns. This is done by pushing them into
the right-hand side (= the code to run upon a match) of each case. This means we
transform this expression:


```rust
match {
    some_number is 10 => foo,
    some_number is num => bar
}
```

Into this:

```rust
match {
    some_number is 10 => foo,
    // I'm using "∅" here to signal a row without any columns.
    ∅ => {
        let num = some_number;
        bar
    }
}
```

The article explains this makes things easier, though it doesn't really say
clearly why. The reason for this is as follows:

1. It reduces the amount of duplication in the resulting decision tree, as we
   don't need to branch for variable and wildcard patterns.
1. It means variable patterns don't influence branching decisions discussed
   below.
1. When we branch on columns (again, discussed below), we can just forget about
   variable patterns.

Essentially it takes the following steps:

1. Each right-hand side can store zero or more variables to define _before_
   running the code.
1. Iterate over the columns in a row.
1. If the column is a variable pattern, copy/move the variable into the
   right-hand side's variable list.
1. Return a new row that only includes non-variable columns.

The implementation handles this in the method `move_variable_patterns`.

Now we need to decide what column to branch on. In practise it probably won't
matter much which strategy is used, so the algorithm takes a simple approach: it
takes the columns of the first row, and for every column counts how many times
the variable tested against is tested against across all columns in all rows. It
then returns the column of which the variable is tested against the most. The
implementation of this is in method `branch_variable`

Now that we know what variable/column to branch on, we can generate the
necessary branches and sub trees. The article only covers simple constructor
patterns, but my implementation also handles integer literals, booleans, and
more. The exact approach differs a bit and I recommend studying the Rust code to
get a better understanding, but it roughly works as follows:

1. Create an array containing triples in the form
  `(constructor, arguments, rows)`. In this triple `constructor` is the
   constructor we're testing against, `arguments` is a list of variables exposed
   to the sub tree, and `rows` is the list of rows to compile for this test.
   The `arguments` array is filled with one variable for every argument.
1. Iterate over all the current rows.
1. Obtain the column index of the branching variable.
1. If we found an index (remember that a now doesn't have to contain any columns
   testing the branching variable), use it to remove the column from the row.
1. Determine the index of the constructor in the array created in step 1. For
   ADTs you'd use the tag values, for booleans you could use 0 and 1 for false
   and true respectively, etc.
1. Zip the pattern arguments (also patterns) with the values in the `arguments`
   array from the triple for this constructor, and create a new column for every
   resulting pair.
1. Create a new row containing the old columns (minus the one we removed
   earlier), the new columns (created in the previous step), and the body of the
   row. Push this row into the `rows` array for our constructor.
1. If in step 3 we didn't find an index, copy the row into the `rows` array for
   every triple in the array created in step 1.
1. Finally, for every triple created in step 1 (and populated in later steps),
   create a Switch node for our decision tree. The constructor and arguments are
   stored in this Switch node, and the rows are compiled into a sub tree.

This is a lot to take in, so I recommend taking a look at the following methods:

- `compile_rows`
- `compile_constructor_cases`

The output of all this is a decision tree, with three possible nodes: Success,
Failure, and Switch (see the `Decision` type). A "Failure" node indicates a
pattern that didn't match, and is used to check for exhaustiveness. In my
implementation I opted to check for exhaustiveness separately, as this saves us
from having to manage some extra data structures until we actually need them.
The implementation works as follows:

When we produce a "Failure" node, a "missing" flag is set to `true`. After
compiling our decision tree, we check this flag. If set to `true`, the method
`Match::missing_patterns` is used to produce a list of patterns to add to make
the match exhaustive.

The implementation of this method is a bit messy in my opinion, but it's the
best I could come up with at this time. The implementation essentially maintains
a stack of "terms" (I couldn't come up with a better name), each describing a
test and its arguments in the tree. These terms also store the variables tested
against, which combined with the names is used to (recursively) reconstruct a
pattern name.

Checking for redundant patterns is easy: when reaching a "Success" node you'd
somehow mark the right-hand side as processed. In my case I just store an
integer value in an array. At the end you check for any right-hand sides that
aren't marked, or in my case you check if any of their values are not in the
array.

This about sums up how the algorithm works. Don't worry if the above wall of
text hurts your head, it took me about two weeks to understand it. My advice is
to read the article from Jules, then read this README, then take a look at the
code and corresponding tests.

## OR patterns

OR patterns are not covered in the article, but supporting them is easy.
Supporting these requires an extra `flatten_or` function that takes as input a
pattern and a `Row`, returning an array of `(Pattern, Row)` tuples. If the input
pattern is an OR pattern, it returns its sub patterns zipped with a copy of the
input row. If the input pattern is any other pattern, the function just returns
an array of the input pattern and row:

```rust
fn flatten_or(pattern: Pattern, row: Row) -> Vec<(Pattern, Row)> {
    if let Pattern::Or(args) = pattern {
        args.into_iter().map(|p| (p, row.clone())).collect()
    } else {
        vec![(pattern, row)]
    }
}
```

When removing the branch column from a row you then use this function, instead
of acting upon a column's pattern directly:

```rust
if let Some(col) = row.remove_column(&branch_var) {
    for (pat, row) in flatten_or(col.pattern, row) {
        ...
    }
} else {
    ...
}
```

## Range patterns

Range patterns are handled using a `Range` constructor
(`Constructor::Range(start, stop)`), produced when matching against integer
types only (meaning we only support integer ranges). Just like regular integers
we assume ranges are of infinite length, so a variable pattern is needed to make
the match exhaustive.

## Guards

Guards are supported as follows: each `Row` has a guard field, storing a
`Option<usize>`, where the `usize` is just a dummy value for the guard; normally
this would be (for example) an AST node to evaluate/lower. When we are about to
produce a Success node for a row, we check if it defines a guard. If so, all
remaining rows are compiled into the guard's fallback tree.
