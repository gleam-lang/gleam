-module(gleam@string_builder).
-compile([no_auto_import, nowarn_unused_vars]).

-export([prepend_builder/2, append_builder/2, new/0, from_strings/1, concat/1, from_string/1, prepend/2, append/2, join/2, to_string/1, byte_size/1, lowercase/1, uppercase/1, reverse/1, split/2, replace/3, is_equal/2, is_empty/1]).
-export_type([direction/0, string_builder/0]).

-type direction() :: all.

-type string_builder() :: any().

-spec prepend_builder(string_builder(), string_builder()) -> string_builder().
prepend_builder(Builder, Prefix) ->
    gleam_stdlib:iodata_append(Prefix, Builder).

-spec append_builder(string_builder(), string_builder()) -> string_builder().
append_builder(Builder, Suffix) ->
    gleam_stdlib:iodata_append(Builder, Suffix).

-spec new() -> string_builder().
new() ->
    gleam_stdlib:identity([]).

-spec from_strings(list(binary())) -> string_builder().
from_strings(Strings) ->
    gleam_stdlib:identity(Strings).

-spec concat(list(string_builder())) -> string_builder().
concat(Builders) ->
    gleam_stdlib:identity(Builders).

-spec from_string(binary()) -> string_builder().
from_string(String) ->
    gleam_stdlib:identity(String).

-spec prepend(string_builder(), binary()) -> string_builder().
prepend(Builder, Prefix) ->
    append_builder(from_string(Prefix), Builder).

-spec append(string_builder(), binary()) -> string_builder().
append(Builder, Second) ->
    append_builder(Builder, from_string(Second)).

-spec join(list(string_builder()), binary()) -> string_builder().
join(Builders, Sep) ->
    _pipe = Builders,
    _pipe@1 = gleam@list:intersperse(_pipe, from_string(Sep)),
    concat(_pipe@1).

-spec to_string(string_builder()) -> binary().
to_string(Builder) ->
    unicode:characters_to_binary(Builder).

-spec byte_size(string_builder()) -> integer().
byte_size(Builder) ->
    erlang:iolist_size(Builder).

-spec lowercase(string_builder()) -> string_builder().
lowercase(Builder) ->
    string:lowercase(Builder).

-spec uppercase(string_builder()) -> string_builder().
uppercase(Builder) ->
    string:uppercase(Builder).

-spec reverse(string_builder()) -> string_builder().
reverse(Builder) ->
    string:reverse(Builder).

-spec do_split(string_builder(), binary()) -> list(string_builder()).
do_split(Iodata, Pattern) ->
    string:split(Iodata, Pattern, all).

-spec split(string_builder(), binary()) -> list(string_builder()).
split(Iodata, Pattern) ->
    do_split(Iodata, Pattern).

-spec do_replace(string_builder(), binary(), binary()) -> string_builder().
do_replace(Iodata, Pattern, Substitute) ->
    string:replace(Iodata, Pattern, Substitute, all).

-spec replace(string_builder(), binary(), binary()) -> string_builder().
replace(Builder, Pattern, Substitute) ->
    do_replace(Builder, Pattern, Substitute).

-spec is_equal(string_builder(), string_builder()) -> boolean().
is_equal(A, B) ->
    string:equal(A, B).

-spec is_empty(string_builder()) -> boolean().
is_empty(Builder) ->
    string:is_empty(Builder).
