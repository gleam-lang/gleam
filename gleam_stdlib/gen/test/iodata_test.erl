-module(iodata_test).
-compile(no_auto_import).

-export([iodata_test/0, lowercase_test/0, uppercase_test/0, split_test/0, is_equal_test/0, is_empty_test/0]).

iodata_test() ->
    Data = iodata:prepend(iodata:append(iodata:append(iodata:new(<<"ello">>),
                                                      <<",">>),
                                        <<" world!">>),
                          <<"H">>),
    expect:equal(iodata:to_string(Data), <<"Hello, world!">>),
    expect:equal(iodata:byte_size(Data), 13),
    Data1 = iodata:prepend_iodata(iodata:append_iodata(iodata:append_iodata(iodata:new(<<"ello">>),
                                                                            iodata:new(<<",">>)),
                                                       iodata:concat([iodata:new(<<" wo">>),
                                                                      iodata:new(<<"rld!">>)])),
                                  iodata:new(<<"H">>)),
    expect:equal(iodata:to_string(Data1), <<"Hello, world!">>),
    expect:equal(iodata:byte_size(Data1), 13).

lowercase_test() ->
    expect:equal(iodata:to_string(iodata:lowercase(iodata:from_strings([<<"Gleam">>,
                                                                        <<"Gleam">>]))),
                 <<"gleamgleam">>).

uppercase_test() ->
    expect:equal(iodata:to_string(iodata:uppercase(iodata:from_strings([<<"Gleam">>,
                                                                        <<"Gleam">>]))),
                 <<"GLEAMGLEAM">>).

split_test() ->
    expect:equal(iodata:split(iodata:new(<<"Gleam,Erlang,Elixir">>), <<",">>),
                 [iodata:new(<<"Gleam">>),
                  iodata:new(<<"Erlang">>),
                  iodata:new(<<"Elixir">>)]),
    expect:equal(iodata:split(iodata:from_strings([<<"Gleam, Erl">>,
                                                   <<"ang,Elixir">>]),
                              <<", ">>),
                 [iodata:new(<<"Gleam">>),
                  iodata:from_strings([<<"Erl">>, <<"ang,Elixir">>])]).

is_equal_test() ->
    expect:true(iodata:is_equal(iodata:new(<<"12">>),
                                iodata:from_strings([<<"1">>, <<"2">>]))),
    expect:true(iodata:is_equal(iodata:new(<<"12">>), iodata:new(<<"12">>))),
    expect:false(iodata:is_equal(iodata:new(<<"12">>), iodata:new(<<"2">>))).

is_empty_test() ->
    expect:true(iodata:is_empty(iodata:new(<<"">>))),
    expect:false(iodata:is_empty(iodata:new(<<"12">>))),
    expect:true(iodata:is_empty(iodata:from_strings([]))),
    expect:true(iodata:is_empty(iodata:from_strings([<<"">>, <<"">>]))).
