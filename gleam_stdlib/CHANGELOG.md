# Changelog

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
