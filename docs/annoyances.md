# Annoyances

This document contains a list of issues and annoyances that we have writing
Gleam code today, so that we can devise solutions to them in future.

There are other annoyances that have known solutions that are yet to be
implemented. These are tracked in the Gleam issue tracker instead.

## Bundling compiled JavaScript is non-obvious

## Cannot shift repeated work to compile or initialise time

For example, regex compilation.

## Runtime debugging is basic

Rust's `dbg!` and Elixir's `dbg` were mentioned as good additions.

We have a reserved `echo` keyword for this.

## JavaScript custom type representation could be faster

There's performance improvements to be had. Would would be optimal?
