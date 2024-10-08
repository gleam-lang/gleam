-module(gleam@string_builder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([prepend_builder/2, append_builder/2, new/0, from_strings/1, concat/1, from_string/1, prepend/2, append/2, to_string/1, byte_size/1, join/2, lowercase/1, uppercase/1, reverse/1, split/2, replace/3, is_equal/2, is_empty/1]).
-export_type([string_builder/0, direction/0]).

-type string_builder() :: any().

-type direction() :: all.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 51).
-spec prepend_builder(string_builder(), string_builder()) -> string_builder().
prepend_builder(Builder, Prefix) ->
    gleam_stdlib:iodata_append(Prefix, Builder).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 62).
-spec append_builder(string_builder(), string_builder()) -> string_builder().
append_builder(Builder, Suffix) ->
    gleam_stdlib:iodata_append(Builder, Suffix).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 24).
-spec new() -> string_builder().
new() ->
    gleam_stdlib:identity([]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 77).
-spec from_strings(list(binary())) -> string_builder().
from_strings(Strings) ->
    gleam_stdlib:identity(Strings).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 89).
-spec concat(list(string_builder())) -> string_builder().
concat(Builders) ->
    gleam_stdlib:identity(Builders).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 101).
-spec from_string(binary()) -> string_builder().
from_string(String) ->
    gleam_stdlib:identity(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 32).
-spec prepend(string_builder(), binary()) -> string_builder().
prepend(Builder, Prefix) ->
    append_builder(from_string(Prefix), Builder).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 43).
-spec append(string_builder(), binary()) -> string_builder().
append(Builder, Second) ->
    append_builder(Builder, from_string(Second)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 114).
-spec to_string(string_builder()) -> binary().
to_string(Builder) ->
    unicode:characters_to_binary(Builder).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 124).
-spec byte_size(string_builder()) -> integer().
byte_size(Builder) ->
    erlang:iolist_size(Builder).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 134).
-spec join(list(string_builder()), binary()) -> string_builder().
join(Builders, Sep) ->
    _pipe = Builders,
    _pipe@1 = gleam@list:intersperse(_pipe, from_string(Sep)),
    concat(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 143).
-spec lowercase(string_builder()) -> string_builder().
lowercase(Builder) ->
    string:lowercase(Builder).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 154).
-spec uppercase(string_builder()) -> string_builder().
uppercase(Builder) ->
    string:uppercase(Builder).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 164).
-spec reverse(string_builder()) -> string_builder().
reverse(Builder) ->
    string:reverse(Builder).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 191).
-spec do_split(string_builder(), binary()) -> list(string_builder()).
do_split(Iodata, Pattern) ->
    string:split(Iodata, Pattern, all).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 182).
-spec split(string_builder(), binary()) -> list(string_builder()).
split(Iodata, Pattern) ->
    do_split(Iodata, Pattern).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 202).
-spec replace(string_builder(), binary(), binary()) -> string_builder().
replace(Builder, Pattern, Substitute) ->
    gleam_stdlib:string_replace(Builder, Pattern, Substitute).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 227).
-spec is_equal(string_builder(), string_builder()) -> boolean().
is_equal(A, B) ->
    string:equal(A, B).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_builder.gleam", 251).
-spec is_empty(string_builder()) -> boolean().
is_empty(Builder) ->
    string:is_empty(Builder).
