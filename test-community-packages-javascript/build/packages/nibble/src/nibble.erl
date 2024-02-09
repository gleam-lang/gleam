-module(nibble).
-compile([no_auto_import, nowarn_unused_vars]).

-export([succeed/1, lazy/1, backtrackable/1, commit/1, then/2, map/2, replace/2, keep/2, drop/2, fail/1, eof/0, take_if/2, any/0, grapheme/1, one_of/1, in/2, inspect/2, string/1, run/2, int/0, float/0, take_while/1, spaces/0, whitespace/0, take_if_and_while/2, take_until/1, loop/2, many/2]).
-export_type([parser/2, state/1, step/2, located/1, backtrackable/0, loop/2, error/0, dead_end/1, bag/1]).

-opaque parser(EVD, EVE) :: {parser, fun((state(EVE)) -> step(EVD, EVE))}.

-type state(EVF) :: {state,
        gleam@map:map_(integer(), binary()),
        integer(),
        list(located(EVF)),
        integer(),
        integer()}.

-type step(EVG, EVH) :: {cont, backtrackable(), EVG, state(EVH)} |
    {fail, backtrackable(), bag(EVH)}.

-type located(EVI) :: {located, integer(), integer(), EVI}.

-type backtrackable() :: commit | backtrack.

-type loop(EVJ, EVK) :: {continue, EVK} | {break, EVJ}.

-type error() :: {bad_parser, binary()} |
    {custom, binary()} |
    end_of_input |
    {expected, binary(), binary()} |
    {unexpected, binary()}.

-type dead_end(EVL) :: {dead_end,
        integer(),
        integer(),
        error(),
        list(located(EVL))}.

-type bag(EVM) :: empty |
    {cons, bag(EVM), dead_end(EVM)} |
    {append, bag(EVM), bag(EVM)}.

-spec runwrap(state(EVV), parser(EVX, EVV)) -> step(EVX, EVV).
runwrap(State, Parser) ->
    {parser, Parse} = Parser,
    Parse(State).

-spec succeed(EWG) -> parser(EWG, any()).
succeed(A) ->
    {parser, fun(State) -> {cont, backtrack, A, State} end}.

-spec lazy(fun(() -> parser(EWO, EWP))) -> parser(EWO, EWP).
lazy(Parser) ->
    {parser, fun(State) -> runwrap(State, Parser()) end}.

-spec backtrackable(parser(EWU, EWV)) -> parser(EWU, EWV).
backtrackable(Parser) ->
    {parser, fun(State) -> case runwrap(State, Parser) of
                {cont, _, A, State@1} ->
                    {cont, backtrack, A, State@1};

                {fail, _, Bag} ->
                    {fail, backtrack, Bag}
            end end}.

-spec commit(EXA) -> parser(EXA, any()).
commit(A) ->
    {parser, fun(State) -> {cont, commit, A, State} end}.

-spec should_commit(backtrackable(), backtrackable()) -> backtrackable().
should_commit(To_x, To_y) ->
    case {To_x, To_y} of
        {commit, _} ->
            commit;

        {_, commit} ->
            commit;

        {_, _} ->
            backtrack
    end.

-spec then(parser(EXE, EXF), fun((EXE) -> parser(EXI, EXF))) -> parser(EXI, EXF).
then(Parser, F) ->
    {parser, fun(State) -> case runwrap(State, Parser) of
                {cont, To_a, A, State@1} ->
                    case runwrap(State@1, F(A)) of
                        {cont, To_b, B, State@2} ->
                            {cont, should_commit(To_a, To_b), B, State@2};

                        {fail, To_b@1, Bag} ->
                            {fail, should_commit(To_a, To_b@1), Bag}
                    end;

                {fail, Can_backtrack, Bag@1} ->
                    {fail, Can_backtrack, Bag@1}
            end end}.

-spec map(parser(EXN, EXO), fun((EXN) -> EXR)) -> parser(EXR, EXO).
map(Parser, F) ->
    then(Parser, fun(A) -> succeed(F(A)) end).

-spec next(state(EWC)) -> {gleam@option:option(binary()), state(EWC)}.
next(State) ->
    case gleam@map:get(erlang:element(2, State), erlang:element(3, State)) of
        {ok, <<"\n"/utf8>>} ->
            {{some, <<"\n"/utf8>>},
                erlang:setelement(
                    5,
                    erlang:setelement(
                        6,
                        erlang:setelement(
                            3,
                            State,
                            erlang:element(3, State) + 1
                        ),
                        1
                    ),
                    erlang:element(5, State) + 1
                )};

        {ok, G} ->
            {{some, G},
                erlang:setelement(
                    6,
                    erlang:setelement(3, State, erlang:element(3, State) + 1),
                    erlang:element(6, State) + 1
                )};

        {error, _} ->
            {none, State}
    end.

-spec map2(parser(EXU, EXV), parser(EXY, EXV), fun((EXU, EXY) -> EYB)) -> parser(EYB, EXV).
map2(Parse_a, Parse_b, F) ->
    then(Parse_a, fun(A) -> map(Parse_b, fun(B) -> F(A, B) end) end).

-spec replace(parser(any(), EYF), EYI) -> parser(EYI, EYF).
replace(Parser, B) ->
    map(Parser, fun(_) -> B end).

-spec keep(parser(fun((EYL) -> EYM), EYN), parser(EYL, EYN)) -> parser(EYM, EYN).
keep(Parse_f, Parse_a) ->
    map2(Parse_f, Parse_a, fun(F, A) -> F(A) end).

-spec drop(parser(EYU, EYV), parser(any(), EYV)) -> parser(EYU, EYV).
drop(Parse_a, Parse_x) ->
    map2(Parse_a, Parse_x, fun(A, _) -> A end).

-spec bag_from_state(state(FCC), error()) -> bag(FCC).
bag_from_state(State, Problem) ->
    {cons,
        empty,
        {dead_end,
            erlang:element(5, State),
            erlang:element(6, State),
            Problem,
            erlang:element(4, State)}}.

-spec fail(binary()) -> parser(any(), any()).
fail(Message) ->
    {parser,
        fun(State) ->
            {fail, backtrack, bag_from_state(State, {custom, Message})}
        end}.

-spec eof() -> parser(nil, any()).
eof() ->
    {parser, fun(State) -> case next(State) of
                {{some, Str}, _} ->
                    {fail, backtrack, bag_from_state(State, {unexpected, Str})};

                {none, _} ->
                    {cont, backtrack, nil, State}
            end end}.

-spec take_if(fun((binary()) -> boolean()), binary()) -> parser(binary(), any()).
take_if(Predicate, Expecting) ->
    {parser,
        fun(State) ->
            {Str, Next_state} = next(State),
            Should_take = begin
                _pipe = Str,
                _pipe@1 = gleam@option:map(_pipe, Predicate),
                gleam@option:unwrap(_pipe@1, false)
            end,
            Str@1 = gleam@option:unwrap(Str, <<""/utf8>>),
            case Should_take of
                true ->
                    {cont, commit, Str@1, Next_state};

                false ->
                    {fail,
                        backtrack,
                        bag_from_state(State, {expected, Expecting, Str@1})}
            end
        end}.

-spec any() -> parser(binary(), any()).
any() ->
    take_if(gleam@function:constant(true), <<"a single grapheme"/utf8>>).

-spec grapheme(binary()) -> parser(nil, any()).
grapheme(Str) ->
    _pipe = take_if(fun(G) -> G =:= Str end, Str),
    map(_pipe, gleam@function:constant(nil)).

-spec add_bag_to_step(step(FCL, FCM), bag(FCM)) -> step(FCL, FCM).
add_bag_to_step(Step, Left) ->
    case Step of
        {cont, Can_backtrack, A, State} ->
            {cont, Can_backtrack, A, State};

        {fail, Can_backtrack@1, Right} ->
            {fail, Can_backtrack@1, {append, Left, Right}}
    end.

-spec one_of(list(parser(FAB, FAC))) -> parser(FAB, FAC).
one_of(Parsers) ->
    {parser,
        fun(State) ->
            Init = {fail, backtrack, empty},
            gleam@list:fold_until(
                Parsers,
                Init,
                fun(Result, Next) -> case Result of
                        {cont, _, _, _} ->
                            {stop, Result};

                        {fail, commit, _} ->
                            {stop, Result};

                        {fail, _, Bag} ->
                            _pipe = runwrap(State, Next),
                            _pipe@1 = add_bag_to_step(_pipe, Bag),
                            {continue, _pipe@1}
                    end end
            )
        end}.

-spec push_context(state(FCY), FCY) -> state(FCY).
push_context(State, Context) ->
    Located = {located,
        erlang:element(5, State),
        erlang:element(6, State),
        Context},
    erlang:setelement(4, State, [Located | erlang:element(4, State)]).

-spec pop_context(state(FDB)) -> state(FDB).
pop_context(State) ->
    case erlang:element(4, State) of
        [] ->
            State;

        [_ | Context] ->
            erlang:setelement(4, State, Context)
    end.

-spec in(parser(FCS, FCT), FCT) -> parser(FCS, FCT).
in(Parser, Context) ->
    {parser, fun(State) -> case runwrap(push_context(State, Context), Parser) of
                {cont, Can_backtrack, A, State@1} ->
                    {cont, Can_backtrack, A, pop_context(State@1)};

                {fail, Can_backtrack@1, Bag} ->
                    {fail, Can_backtrack@1, Bag}
            end end}.

-spec inspect(parser(FDE, FDF), binary()) -> parser(FDE, FDF).
inspect(Parser, Message) ->
    {parser,
        fun(State) ->
            gleam@io:print(Message),
            gleam@io:println(<<": "/utf8>>),
            _pipe = runwrap(State, Parser),
            gleam@io:debug(_pipe)
        end}.

-spec string(binary()) -> parser(nil, any()).
string(Str) ->
    Graphemes = gleam@string:to_graphemes(Str),
    {parser, fun(State) -> case Graphemes of
                [] ->
                    {fail,
                        backtrack,
                        bag_from_state(
                            State,
                            {bad_parser, <<"empty string"/utf8>>}
                        )};

                [Head | Tail] ->
                    Parse_each = gleam@list:fold(
                        Tail,
                        grapheme(Head),
                        fun(Parse, Next) -> _pipe = Parse,
                            drop(_pipe, grapheme(Next)) end
                    ),
                    case runwrap(State, Parse_each) of
                        {cont, _, _, State@1} ->
                            {cont, commit, nil, State@1};

                        {fail, _, Bag} ->
                            {fail, backtrack, Bag}
                    end
            end end}.

-spec to_deadends(bag(FCF), list(dead_end(FCF))) -> list(dead_end(FCF)).
to_deadends(Bag, Acc) ->
    case Bag of
        empty ->
            Acc;

        {cons, empty, Deadend} ->
            [Deadend | Acc];

        {cons, Bag@1, Deadend@1} ->
            to_deadends(Bag@1, [Deadend@1 | Acc]);

        {append, Left, Right} ->
            to_deadends(Left, to_deadends(Right, Acc))
    end.

-spec run(binary(), parser(EVN, EVO)) -> {ok, EVN} |
    {error, list(dead_end(EVO))}.
run(Src, Parser) ->
    Graphemes = begin
        _pipe = gleam@string:to_graphemes(Src),
        _pipe@1 = gleam@list:index_map(
            _pipe,
            fun(I, Grapheme) -> {I, Grapheme} end
        ),
        gleam@map:from_list(_pipe@1)
    end,
    Init = {state, Graphemes, 0, [], 1, 1},
    case runwrap(Init, Parser) of
        {cont, _, A, _} ->
            {ok, A};

        {fail, _, Bag} ->
            {error, to_deadends(Bag, [])}
    end.

-spec int() -> parser(integer(), any()).
int() ->
    _pipe = take_if_and_while(
        fun nibble@predicates:is_digit/1,
        <<"a digit"/utf8>>
    ),
    map(
        _pipe,
        fun(Digits) ->
            _assert_subject = gleam@int:parse(Digits),
            {ok, Int} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"nibble"/utf8>>,
                                function => <<"int"/utf8>>,
                                line => 254})
            end,
            Int
        end
    ).

-spec float() -> parser(float(), any()).
float() ->
    Make_float_string = gleam@function:curry2(
        fun(X, Y) -> gleam@string:concat([X, <<"."/utf8>>, Y]) end
    ),
    _pipe = succeed(Make_float_string),
    _pipe@1 = keep(
        _pipe,
        take_if_and_while(fun nibble@predicates:is_digit/1, <<"a digit"/utf8>>)
    ),
    _pipe@2 = drop(_pipe@1, grapheme(<<"."/utf8>>)),
    _pipe@3 = keep(
        _pipe@2,
        take_if_and_while(fun nibble@predicates:is_digit/1, <<"a digit"/utf8>>)
    ),
    map(
        _pipe@3,
        fun(Digits) ->
            _assert_subject = gleam@float:parse(Digits),
            {ok, Float} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"nibble"/utf8>>,
                                function => <<"float"/utf8>>,
                                line => 271})
            end,
            Float
        end
    ).

-spec take_while(fun((binary()) -> boolean())) -> parser(binary(), any()).
take_while(Predicate) ->
    {parser,
        fun(State) ->
            {Str, Next_state} = next(State),
            Should_take = begin
                _pipe = Str,
                _pipe@1 = gleam@option:map(_pipe, Predicate),
                gleam@option:unwrap(_pipe@1, false)
            end,
            Str@1 = gleam@option:unwrap(Str, <<""/utf8>>),
            case Should_take of
                true ->
                    runwrap(
                        Next_state,
                        map(
                            take_while(Predicate),
                            fun(_capture) ->
                                gleam@string:append(Str@1, _capture)
                            end
                        )
                    );

                false ->
                    {cont, backtrack, <<""/utf8>>, State}
            end
        end}.

-spec spaces() -> parser(nil, any()).
spaces() ->
    _pipe = take_while(fun(G) -> G =:= <<" "/utf8>> end),
    map(_pipe, gleam@function:constant(nil)).

-spec whitespace() -> parser(nil, any()).
whitespace() ->
    _pipe = take_while(fun nibble@predicates:is_whitespace/1),
    map(_pipe, gleam@function:constant(nil)).

-spec take_if_and_while(fun((binary()) -> boolean()), binary()) -> parser(binary(), any()).
take_if_and_while(Predicate, Expecting) ->
    map2(
        take_if(Predicate, Expecting),
        take_while(Predicate),
        fun gleam@string:append/2
    ).

-spec take_until(fun((binary()) -> boolean())) -> parser(binary(), any()).
take_until(Predicate) ->
    take_while(gleam@function:compose(Predicate, fun gleam@bool:negate/1)).

-spec loop_help(
    fun((FBC) -> parser(loop(FBD, FBC), FBG)),
    backtrackable(),
    FBC,
    state(FBG)
) -> step(FBD, FBG).
loop_help(F, Commit, Loop_state, State) ->
    case runwrap(State, F(Loop_state)) of
        {cont, Can_backtrack, {continue, Next_loop_state}, Next_state} ->
            loop_help(
                F,
                should_commit(Commit, Can_backtrack),
                Next_loop_state,
                Next_state
            );

        {cont, Can_backtrack@1, {break, Result}, Next_state@1} ->
            {cont, should_commit(Commit, Can_backtrack@1), Result, Next_state@1};

        {fail, Can_backtrack@2, Bag} ->
            {fail, should_commit(Commit, Can_backtrack@2), Bag}
    end.

-spec loop(FBC, fun((FBC) -> parser(loop(FBD, FBC), FBG))) -> parser(FBD, FBG).
loop(Init, Step) ->
    {parser, fun(State) -> loop_help(Step, backtrack, Init, State) end}.

-spec more(FAS, parser(FAS, FAT), parser(any(), FAT)) -> parser(list(FAS), FAT).
more(X, Parser, Separator) ->
    loop(
        [X],
        fun(Xs) ->
            one_of(
                [begin
                        _pipe = succeed(
                            fun(_capture) ->
                                gleam@list:prepend(Xs, _capture)
                            end
                        ),
                        _pipe@1 = drop(_pipe, Separator),
                        _pipe@2 = keep(_pipe@1, Parser),
                        map(_pipe@2, fun(Field@0) -> {continue, Field@0} end)
                    end,
                    begin
                        _pipe@3 = succeed(Xs),
                        _pipe@4 = drop(_pipe@3, eof()),
                        _pipe@5 = map(_pipe@4, fun gleam@list:reverse/1),
                        map(_pipe@5, fun(Field@0) -> {break, Field@0} end)
                    end,
                    begin
                        _pipe@6 = succeed(Xs),
                        _pipe@7 = map(_pipe@6, fun gleam@list:reverse/1),
                        map(_pipe@7, fun(Field@0) -> {break, Field@0} end)
                    end]
            )
        end
    ).

-spec many(parser(FAI, FAJ), parser(any(), FAJ)) -> parser(list(FAI), FAJ).
many(Parser, Separator) ->
    one_of(
        [begin
                _pipe = Parser,
                then(
                    _pipe,
                    fun(_capture) -> more(_capture, Parser, Separator) end
                )
            end,
            succeed([])]
    ).
