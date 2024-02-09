-module(nibble@pratt).
-compile([no_auto_import, nowarn_unused_vars]).

-export([sub_expression/2, expression/3, prefix/3, postfix/3, infix_left/3, infix_right/3]).
-export_type([config/2, operator/2]).

-opaque config(FVQ, FVR) :: {config,
        list(fun((config(FVQ, FVR)) -> nibble:parser(FVQ, FVR))),
        list(operator(FVQ, FVR)),
        nibble:parser(nil, FVR)}.

-opaque operator(FVS, FVT) :: {operator,
        fun((config(FVS, FVT)) -> {integer(),
            fun((FVS) -> nibble:parser(FVS, FVT))})}.

-spec operation(FWO, config(FWO, FWP), integer()) -> nibble:parser(FWO, FWP).
operation(Expr, Config, Current_precedence) ->
    _pipe = erlang:element(3, Config),
    _pipe@1 = gleam@list:filter_map(
        _pipe,
        fun(Operator) ->
            {operator, Op} = Operator,
            case Op(Config) of
                {Precedence, Parser} when Precedence > Current_precedence ->
                    {ok, Parser(Expr)};

                _ ->
                    {error, nil}
            end
        end
    ),
    nibble:one_of(_pipe@1).

-spec sub_expression(config(FWI, FWJ), integer()) -> nibble:parser(FWI, FWJ).
sub_expression(Config, Precedence) ->
    Expr = nibble:lazy(fun() -> _pipe = erlang:element(2, Config),
            _pipe@1 = gleam@list:map(_pipe, fun(P) -> P(Config) end),
            nibble:one_of(_pipe@1) end),
    Go = fun(Expr@1) -> _pipe@2 = nibble:succeed(fun gleam@function:identity/1),
        _pipe@3 = nibble:drop(_pipe@2, erlang:element(4, Config)),
        nibble:keep(
            _pipe@3,
            nibble:one_of(
                [begin
                        _pipe@4 = nibble:succeed(Expr@1),
                        _pipe@5 = nibble:then(
                            _pipe@4,
                            fun(_capture) ->
                                operation(_capture, Config, Precedence)
                            end
                        ),
                        nibble:map(
                            _pipe@5,
                            fun(Field@0) -> {continue, Field@0} end
                        )
                    end,
                    begin
                        _pipe@6 = nibble:succeed(Expr@1),
                        nibble:map(
                            _pipe@6,
                            fun(Field@0) -> {break, Field@0} end
                        )
                    end]
            )
        ) end,
    _pipe@7 = nibble:succeed(fun gleam@function:identity/1),
    _pipe@8 = nibble:drop(_pipe@7, erlang:element(4, Config)),
    _pipe@9 = nibble:keep(_pipe@8, Expr),
    nibble:then(_pipe@9, fun(_capture@1) -> nibble:loop(_capture@1, Go) end).

-spec expression(
    list(fun((config(FVU, FVV)) -> nibble:parser(FVU, FVV))),
    list(operator(FVU, FVV)),
    nibble:parser(nil, FVV)
) -> nibble:parser(FVU, FVV).
expression(First, Then, Spaces) ->
    Config = {config, First, Then, Spaces},
    sub_expression(Config, 0).

-spec prefix(integer(), nibble:parser(nil, FWU), fun((FWX) -> FWX)) -> fun((config(FWX, FWU)) -> nibble:parser(FWX, FWU)).
prefix(Precedence, Operator, Apply) ->
    fun(Config) -> _pipe = nibble:succeed(Apply),
        _pipe@1 = nibble:drop(_pipe, Operator),
        nibble:keep(_pipe@1, sub_expression(Config, Precedence)) end.

-spec postfix(integer(), nibble:parser(nil, FXO), fun((FXR) -> FXR)) -> operator(FXR, FXO).
postfix(Precedence, Operator, Apply) ->
    {operator,
        fun(_) -> {Precedence, fun(Lhs) -> _pipe = nibble:succeed(Apply(Lhs)),
                    nibble:drop(_pipe, Operator) end} end}.

-spec make_infix(
    {integer(), integer()},
    nibble:parser(nil, FXU),
    fun((FXX, FXX) -> FXX)
) -> operator(FXX, FXU).
make_infix(Precedence, Operator, Apply) ->
    {Left_precedence, Right_precedence} = Precedence,
    {operator,
        fun(Config) ->
            {Left_precedence,
                fun(Lhs) ->
                    _pipe = nibble:succeed(
                        fun(_capture) -> Apply(Lhs, _capture) end
                    ),
                    _pipe@1 = nibble:drop(_pipe, Operator),
                    nibble:keep(
                        _pipe@1,
                        sub_expression(Config, Right_precedence)
                    )
                end}
        end}.

-spec infix_left(integer(), nibble:parser(nil, FXC), fun((FXF, FXF) -> FXF)) -> operator(FXF, FXC).
infix_left(Precedence, Operator, Apply) ->
    make_infix({Precedence, Precedence}, Operator, Apply).

-spec infix_right(integer(), nibble:parser(nil, FXI), fun((FXL, FXL) -> FXL)) -> operator(FXL, FXI).
infix_right(Precedence, Operator, Apply) ->
    make_infix({Precedence, Precedence - 1}, Operator, Apply).
