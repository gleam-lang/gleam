-module(glx@stringx).
-compile([no_auto_import, nowarn_unused_vars]).

-export([lines/1]).

-spec lines(binary()) -> list(binary()).
lines(Value) ->
    _assert_subject = gleam@regex:from_string(<<"\r?\n"/utf8>>),
    {ok, Newline_regex} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"glx/stringx"/utf8>>,
                        function => <<"lines"/utf8>>,
                        line => 24})
    end,
    Is_not_empty = fun(Line) ->
        gleam@bool:negate(gleam@string:is_empty(Line))
    end,
    _pipe = Value,
    _pipe@1 = gleam@regex:split(Newline_regex, _pipe),
    gleam@list:filter(_pipe@1, Is_not_empty).
