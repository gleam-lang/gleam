# Changelog

## Unreleased

- Syntax has been updated for Gleam v0.4.
- The `map_dict` module has been renamed to `map`.
- `list:sort` now requires a compare function as comparison operators
  now only work on Ints.
- `list:sort`'s performance has been slightly optimised.
- The `float` module gains a `compare` function.
- `any:tuple` has been renamed `any:pair`.
- The `tuple` module has been renamed to `pair` and has a `Pair` type.
- `pair.fetch` has been replaced with `list.key_find`.

## v0.3.1 - 2019-08-08

- `result:map_error` has been relaxed to allow mapping to a different error
  type.

## v0.3.0 - 2019-06-25

- The `map_dict` module gains a `fold` function.
- All modules moved under the `std` namespace.
- The `http` module has been split out into the `gleam_http` package.

## v0.2.0 - 2019-05-11

- Library renamed to `gleam_stdlib`.
- The `map_dict` module gains `update`, `merge` and `delete` functions.
- The `bool` module gains a `compare` function.
- The `int` module gains a `compare` function.
- The `list` module gains `range`, `repeat`, `split`, `split_while` and
  `strict_zip` functions.

## v0.1.2 - 2019-04-25

- The `list` module gains `at`, `all`, `any`, `index_map`, `intersperse`,
  `sort`, `unique`, and `zip` functions.
- `map_dict:Map` renamed to `map_dict:MapDict`.
- The `map_dict` module gains `drop`, and `take` functions.
- The `str` module gains `append` function and loses `from_int`, `parse_int`,
  `from_float`, `parse_float`, and `base_from_int`.
- `int` module created with `parse`, `to_string`, and `to_base_string`.
- `float` module created with `ceiling`, `floor`, `round`, `truncate`,
  `parse`, and `to_string`.

## v0.1.1 - 2019-04-17

- Included missing gleam.toml in hex package.

## v0.1.0 - 2019-04-15

- Initial release!
