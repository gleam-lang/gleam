# Changelog

## v0.18.0 - 2021-11-23
## v0.18.0-rc1 - 2021-11-23

- Converted to use the Gleam build tool, not rebar3.
- The `iterator` module gains the `first` and `at` functions.
- The `list` module renames the `head` and `tail` functions to `first` and `rest`.
- The `list.at` function now behaves uniformly to `iterator.at`.
- `int.to_base_string` now returns a `Result(Int, InvalidBase)`.
- The `int` module gains the `to_base2`, `to_base8`, `to_base16` and `to_base36` functions.

## v0.17.1 - 2021-09-15

- `uri.parse` now returns a result.

## v0.17.0 - 2021-09-11

- All modules have been updated to work on JavaScript as well as Erlang.
- The `bit_string` module gains the `concat` function and has the `part`
  function renamed to `slice`.
- The `os` module has been removed in favour of target specific libraries.
- The `rescue` function has been removed from the `function` library in favour
  of target specific versions in Erlang and JavaScript specific libraries.
- The `map.update` function now uses `Option` rather than `Result`.
- The `iterator` module gains the `fold_until` and `try_fold` functions.
- The `bit_string` module loses the u32 functions in favour of bit string literals.
- The `dynamic` module loses the `atom` function and gains the `classify` function.
- The `dynamic.option` function has been renamed to `optional` and made more
  permissive to other null values.
- The `dynamic.result` function has been made more permissive to other result values.
- The `dynamic.thunk` function has been removed.
- The `dynamic.element` label `postion` was renamed to `get`.
- The `dynamic.element` now accepts negative indexes.
- The `io.get_line` function has been moved to the `gleam_erlang` library.
- The `atom` module has been moved to the `gleam_erlang` library.
- Prelude types like `Result`, `List` etc. are no longer redefined in their
  stdlib modules.
- The `dynamic` module functions now return structured error values instead of a
  string error description.
- The `string` module gains the `to_option` function.
- Fixed a bug where `io.print` could crash when printing special characters.
- The `regex.Match` record no longer has the `byte_index` field any more.
- The `should` module has been moved to the `gleam_should_assertions` package.
- The `uri.percent_encode` function has a slightly different behaviour. For
  example spaces are encoded as `%20`, not as `+`.
- The order of the arguments of the the function accepted by the
  `list.map_fold`, `list.fold`, `list.fold_right`, `list.index_fold`,
  `list.try_fold`, `list.fold_until`, `list.reduce`, `list.scan`, `map.fold`,
  `set.fold`, `iterator.fold`, `iterator.scan`, `iterator.reduce`,
  `iterator.fold_until`, and `iterator.try_fold` have been flipped.

## v0.16.0 - 2021-06-17

- The `list` module gains the `interleave`, `flat_map` and `transpose` functions.
- The `option` module gains the `all` and `values` functions.
- The `os` module now uses unicode to encode/decode environment variables.
  This fixes an issue when non-latin characters are present in environment.
- The `result` module gains the `values` function.
- All modules now use the new `#(a, b, ...)` tuple syntax.

## v0.15.0 - 2021-05-05

- The `list.split_while` function's second argument now has the label
  `satisfying` to match the other `_while` functions in `list` and `iterator`.
- The `dynamic` module gains the `tuple3`, `tuple4`, `tuple5`, `tuple6`
  functions and their typed equivalents `typed_tuple3`, `typed_tuple4`,
  `typed_tuple5`, `typed_tuple6`.
- The `list` module gains the `combinations`, `combination_pairs`, `drop_while`,
  `map_fold`, `take_while`, `reduce`, `chunk`, `sized_chunk`, `last` and `scan`
  functions.
- The `iterator` module gains the `index`, `iterate`, `zip`, `scan`, `last`,
  `take_while`, `drop_while`, `chunk`, `sized_chunk`, `intersperse`, `interleave`, `reduce`,
  `any`, `all`, `empty`, `once` and `single` functions.
- Breaking change in `iterator.take`. Now it returns an iterator instead of a list.
- The `string` module gains the `crop` function.

## v0.14.0 - 2021-02-18

- The `list` modules gains the `fold_until`, `window`, and `window_by_2` functions.
- The `int` module gains the `clamp` function.
- The `float` module gains the `clamp` function.
- The `io` module gains the `get_line` function.

## v0.13.0 - 2021-01-13

- The `int` module gains the `absolute_value`, `sum` and `product` functions.
- The `float` module gains the `sum` and `product` functions.
- The `result` module gains the `lazy_or`, `lazy_unwrap`, and `replace_error` functions.
- The `bool` module gains the `nand`, `nor`, `exclusive_nor`, and `exclusive_or` functions.
- The `bit_builder` module gains the `from_string_builder` function.
- The `list` modules gains the `index_fold`, `permutations`, and `try_fold` functions.
- Breaking change in `queue.from_list`. The head element in the list becomes the
  first element in the queue.
- Fix `queue.pop_back` and `queue.pop_front`

## v0.12.0 - 2020-11-04

- The `function` module gains `curry2` to `curry6`.
- The `list` module gains the `each`, and `partition` functions.
- The `int` and `float` modules gain the `negate` function.
- The `int` module gains the `to_float` function.
- The `result` module gains the `all` function.
- The `dynamic` module gains the `option`, `result` and `typed_result`
  functions.
- The `uri` module gains the `percent_encode` and `percent_decode` functions.
- The `os` module gains the `erlang_timestamp` function.
- The `iterator` module gains the `append`, `flatten`, `flat_map`, `step`,
  and `find` functions.

## v0.11.0 - 2020-08-22

- Fix `uri.parse_query` to handle the case where query parameters are present
  without a value.
- The types for `list.find_map` have been relaxed.
- The `dynamic.typed_list` argument label has changed from `containing` to
  `of`.
- The `dynamic` module gains the `any` function.
- The `bit_builder` module gains the `from_string` function.
- The `list` module gains the `key_set` and `unzip` function.
- The `function` module gains the `rescue` function.
- The `float` module gains the `power`, `square_root`, and `absolute_value`
  functions.

## v0.10.1 - 2020-07-01

- Fix `dynamic.string` to check that binary contains only utf8 characters.

## v0.10.0 - 2020-06-30

- `bit_string` module created with `from_string`, `byte_size`, `append`,
  `part`, `to_string`, `is_utf8`, `int_to_u32` and `int_from_u32` functions.
- The `bit_builder` module has been introduced with `prepend`, `append`,
  `prepend_builder`, `append_builder`, `prepend_string`, `append_string`,
  `concat`, `from_bit_string`, `to_bit_string`, and `byte_size` functions.
- The `iodata` module has been renamed to `string_builder`.
- `os` module created with `get_env`, `insert_env`, `delete_env` and
  `system_time`.
- The `string` module gains the `split_once` and `utf_codepoint` functions.
- The `dynamic` module gains the `bit_string` function.
- The `uri` module gains the `origin` and `merge` function.
- The `io.debug` function returns the printed term.
- The `dynamic.list` function has been renamed to `dynamic.typed_list`.
- The `dynamic.opaque_list` function has been renamed to `dynamic.list`.
- The `dynamic.tuple2_of` function has been renamed to `dynamic.typed_tuple2`.
- The `list.traverse` function has been renamed to `list.try_map`.
- The `list.traverse` first argument gains the label `over`.
- The `option` module gains the the `map`, `flatten`, `then` and `or`
  functions.
- The `result` module gains the the `or` function.
- Created the `regex` module with the `from_string`, `compile`, `check`,
  `split` and `scan` functions.
- The `list` module gains the the `pop`, `pop_map` and `key_pop` functions.
- `base` module created with `encode64`, `decode64`, `url_encode64` and
  `url_decode64`.

## v0.9.0 - 2020-05-26

- Created the `iterator` module with the `unfold`, `repeatedly`, `repeat`,
  `from_list`, `fold`, `run`, `to_list`, `take`, `drop`, `map`, `filter`,
  `cycle`, and `range` functions.
- Created the `set` module with the `new`, `insert`, `delete`, `to_list`,
  `from_list`, `fold`, `take`, `union`, `intersection`, and `contains`
  functions.
- Created the `io` module with the `print`, `println`, and `debug` functions.
- Created the `queue` module with the `new`, `from_list`, `to_list`,
  `is_empty`, `length`, `push_back`, `push_front`, `pop_back`, `pop_front`,
  `reverse`, `is_logically_equal`, and `is_equal` functions.
- Created the `option` module containing the `Option` type and the `is_some`
  and `is_none` functions.
- Created the `option` module containing the `Option` type and the `is_some`,
  `is_none`, `to_result`, `from_result` and `unwrap` functions.
- Removed the `Option` alias and the `none` function from the `result` module.
- The `result` module gains the `nil_error` function.
- The `string` module gains `trim`, `trim_left`, `trim_right`, `starts_with`,
  `ends_with`, `slice`, `pad_left`, `pad_right` `drop_left`, `drop_right`,
  `pop_grapheme` and `to_graphemes` functions.
- `uri` module created with `parse`, `parse_query`, `path_segments`,
  `query_to_string` and `to_string`.
- The `dynamic` module gains the `map`, `opaque_list`, `tuple2`, and
  `tuple2_of` functions.
- The `list` module gains the `filter_map` function.
- The `list.contains` label `has` has been changed to `any`.
- The `list.sort` label `sort_by` has been changed to `by`.
- The `list.fold`'s first argument gained the label `over`.
- The `map.fold`'s first argument gained the label `over`.
- The `map.take`'s `drop` arguement has been changed to `keeping`.

## v0.8.0 - 2020-04-28

- The error type for `atom.from_string` has been renamed to `FromStringError`.
- The `string` module gains `contains` and `repeat` functions.
- The `expect` module has been renamed to `should`. Functions in the module
  starting with `is_` have been changed to `be_`.
- The `string.replace` and `iodata.replace` `all` arguement label has been
  changed to `each`.
- The `string` module gains `is_empty`, `join` and `concat` functions.
- The `int` module gains `is_even` and `is_odd` functions.
- The `list.length` function now accepts a labelled argument.
- The `list.length` function now accepts a labelled argument.
- The the second argument of `bool.compare`, `float.compare`, `int.compare`,
  and `order.compare` now have the label `with`.
- The `dynamic.unsafe_coerce` function now only accepts Dynamic data.
- The `dynamic` decoder functions no longer print the entire value in their
  error messages, to avoid large errors.

## v0.7.0 - 2020-03-03

- The `result` module gains an `Option` type alias.
- The `function` module has been created with `identity`, `compose`, and
  `flip` functions.
- The error type of `list.find_map` is now `Nil`.
- The labels for `list.split` are now `split(list: _, at: _)`.

## v0.6.0 - 2019-12-23

- Syntax has been updated for Gleam v0.6.0.
- The `dynamic` module gains an `element` for decoding tuples.

## v0.5.0 - 2019-12-16

- Syntax has been updated for Gleam v0.5.
- Labels have been added to functions throughout the stdlib.
- `map.fetch` has been renamed to `map.get` and `map.put` to `map.insert`.
- `list.find` has been renamed `list.find_map` and a new `list.find` has been
  introduced.
- The `pair` module gains the `map_first`, and `map_second` functions.
- The `pair.Pair` type has been replaced with a 2 element anonymous struct.
- The `triple` module has been removed.
- The `string` module gains the `compare` function.
- The `float` module gains the `max`, and `min` functions.
- The `int` module gains the `max`, and `min` functions.
- The `Any` type and module have been renamed to `Dynamic`.

## v0.4.0 - 2019-09-19

- Syntax has been updated for Gleam v0.4.
- The `map_dict` module has been renamed to `map`.
- `list:sort` now requires a compare function as comparison operators
  now only work on Ints.
- `list:sort`'s performance has been slightly optimised.
- The `float` module gains a `compare` function.
- `any.tuple` has been renamed `any.pair`.
- The `tuple` module has been renamed to `pair` and has a `Pair` type.
- `pair.fetch` has been replaced with `list.key_find`.
- `triple` module has been created with type `Triple`.
- The error type for `float.parse`, `int.parse`, `list.head`, `list.tail`,
  `list.find`, `list.at`, `map.fetch`, and `map.update` is now `Nil`.

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
