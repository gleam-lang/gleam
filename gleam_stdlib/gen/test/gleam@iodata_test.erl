-module(gleam@iodata_test).
-compile(no_auto_import).

-export([iodata_test/0, lowercase_test/0, uppercase_test/0, split_test/0, is_equal_test/0, is_empty_test/0]).

iodata_test() ->
    Data = gleam@iodata:prepend(
        gleam@iodata:append(
            gleam@iodata:append(gleam@iodata:new(<<"ello">>), <<",">>),
            <<" world!">>
        ),
        <<"H">>
    ),
    gleam@expect:equal(gleam@iodata:to_string(Data), <<"Hello, world!">>),
    gleam@expect:equal(gleam@iodata:byte_size(Data), 13),
    Data1 = gleam@iodata:prepend_iodata(
        gleam@iodata:append_iodata(
            gleam@iodata:append_iodata(
                gleam@iodata:new(<<"ello">>),
                gleam@iodata:new(<<",">>)
            ),
            gleam@iodata:concat(
                [gleam@iodata:new(<<" wo">>), gleam@iodata:new(<<"rld!">>)]
            )
        ),
        gleam@iodata:new(<<"H">>)
    ),
    gleam@expect:equal(gleam@iodata:to_string(Data1), <<"Hello, world!">>),
    gleam@expect:equal(gleam@iodata:byte_size(Data1), 13).

lowercase_test() ->
    gleam@expect:equal(
        gleam@iodata:to_string(
            gleam@iodata:lowercase(
                gleam@iodata:from_strings([<<"Gleam">>, <<"Gleam">>])
            )
        ),
        <<"gleamgleam">>
    ).

uppercase_test() ->
    gleam@expect:equal(
        gleam@iodata:to_string(
            gleam@iodata:uppercase(
                gleam@iodata:from_strings([<<"Gleam">>, <<"Gleam">>])
            )
        ),
        <<"GLEAMGLEAM">>
    ).

split_test() ->
    gleam@expect:equal(
        gleam@iodata:split(gleam@iodata:new(<<"Gleam,Erlang,Elixir">>), <<",">>),
        [gleam@iodata:new(<<"Gleam">>),
         gleam@iodata:new(<<"Erlang">>),
         gleam@iodata:new(<<"Elixir">>)]
    ),
    gleam@expect:equal(
        gleam@iodata:split(
            gleam@iodata:from_strings([<<"Gleam, Erl">>, <<"ang,Elixir">>]),
            <<", ">>
        ),
        [gleam@iodata:new(<<"Gleam">>),
         gleam@iodata:from_strings([<<"Erl">>, <<"ang,Elixir">>])]
    ).

is_equal_test() ->
    gleam@expect:true(
        gleam@iodata:is_equal(
            gleam@iodata:new(<<"12">>),
            gleam@iodata:from_strings([<<"1">>, <<"2">>])
        )
    ),
    gleam@expect:true(
        gleam@iodata:is_equal(
            gleam@iodata:new(<<"12">>),
            gleam@iodata:new(<<"12">>)
        )
    ),
    gleam@expect:false(
        gleam@iodata:is_equal(
            gleam@iodata:new(<<"12">>),
            gleam@iodata:new(<<"2">>)
        )
    ).

is_empty_test() ->
    gleam@expect:true(gleam@iodata:is_empty(gleam@iodata:new(<<"">>))),
    gleam@expect:false(gleam@iodata:is_empty(gleam@iodata:new(<<"12">>))),
    gleam@expect:true(gleam@iodata:is_empty(gleam@iodata:from_strings([]))),
    gleam@expect:true(
        gleam@iodata:is_empty(gleam@iodata:from_strings([<<"">>, <<"">>]))
    ).
