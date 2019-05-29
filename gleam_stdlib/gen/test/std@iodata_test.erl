-module(std@iodata_test).
-compile(no_auto_import).

-export([iodata_test/0, lowercase_test/0, uppercase_test/0, split_test/0, is_equal_test/0, is_empty_test/0]).

iodata_test() ->
    Data = std@iodata:prepend(std@iodata:append(std@iodata:append(std@iodata:new(<<"ello">>),
                                                                  <<",">>),
                                                <<" world!">>),
                              <<"H">>),
    std@expect:equal(std@iodata:to_string(Data), <<"Hello, world!">>),
    std@expect:equal(std@iodata:byte_size(Data), 13),
    Data1 = std@iodata:prepend_iodata(std@iodata:append_iodata(std@iodata:append_iodata(std@iodata:new(<<"ello">>),
                                                                                        std@iodata:new(<<",">>)),
                                                               std@iodata:concat([std@iodata:new(<<" wo">>),
                                                                                  std@iodata:new(<<"rld!">>)])),
                                      std@iodata:new(<<"H">>)),
    std@expect:equal(std@iodata:to_string(Data1), <<"Hello, world!">>),
    std@expect:equal(std@iodata:byte_size(Data1), 13).

lowercase_test() ->
    std@expect:equal(std@iodata:to_string(std@iodata:lowercase(std@iodata:from_strings([<<"Gleam">>,
                                                                                        <<"Gleam">>]))),
                     <<"gleamgleam">>).

uppercase_test() ->
    std@expect:equal(std@iodata:to_string(std@iodata:uppercase(std@iodata:from_strings([<<"Gleam">>,
                                                                                        <<"Gleam">>]))),
                     <<"GLEAMGLEAM">>).

split_test() ->
    std@expect:equal(std@iodata:split(std@iodata:new(<<"Gleam,Erlang,Elixir">>),
                                      <<",">>),
                     [std@iodata:new(<<"Gleam">>),
                      std@iodata:new(<<"Erlang">>),
                      std@iodata:new(<<"Elixir">>)]),
    std@expect:equal(std@iodata:split(std@iodata:from_strings([<<"Gleam, Erl">>,
                                                               <<"ang,Elixir">>]),
                                      <<", ">>),
                     [std@iodata:new(<<"Gleam">>),
                      std@iodata:from_strings([<<"Erl">>, <<"ang,Elixir">>])]).

is_equal_test() ->
    std@expect:true(std@iodata:is_equal(std@iodata:new(<<"12">>),
                                        std@iodata:from_strings([<<"1">>,
                                                                 <<"2">>]))),
    std@expect:true(std@iodata:is_equal(std@iodata:new(<<"12">>),
                                        std@iodata:new(<<"12">>))),
    std@expect:false(std@iodata:is_equal(std@iodata:new(<<"12">>),
                                         std@iodata:new(<<"2">>))).

is_empty_test() ->
    std@expect:true(std@iodata:is_empty(std@iodata:new(<<"">>))),
    std@expect:false(std@iodata:is_empty(std@iodata:new(<<"12">>))),
    std@expect:true(std@iodata:is_empty(std@iodata:from_strings([]))),
    std@expect:true(std@iodata:is_empty(std@iodata:from_strings([<<"">>,
                                                                 <<"">>]))).
