-module(glove).
-compile([no_auto_import, nowarn_unused_vars]).

-export([display_value/1, into_abi/1, into_base/1, size/1, display_data_item/1, add_inst/2, assign_inst/4, jumps/1, add_block/1, last_block/1, display_linkage/1, private/0, new_datadef/0, new_function/0, private_with_section/1, public/0, public_with_section/1, new_module/0, add_function/2, add_type/2, add_data/2, display_type_def/1, display_type/1, display_inst/1, display_data_def/1, display_statement/1, display_block/1, display_arguments/1, display_blocks/1, display_function/1, display_module/1]).
-export_type([comp/0, inst/0, value/0, type/0, data_def/0, type_def/0, data_item/0, statement/0, block/0, function_/0, linkage/0, module_/0]).

-type comp() :: slt | sle | sgt | sge | eq | ne.

-type inst() :: {add, value(), value()} |
    {sub, value(), value()} |
    {mul, value(), value()} |
    {'div', value(), value()} |
    {'rem', value(), value()} |
    {comp, type(), comp(), value(), value()} |
    {'and', value(), value()} |
    {'or', value(), value()} |
    {copy, value()} |
    {ret, gleam@option:option(value())} |
    {jnz, value(), binary(), binary()} |
    {jmp, binary()} |
    {call, value(), list({type(), value()})} |
    {alloc4, integer()} |
    {alloc8, integer()} |
    {alloc16, integer()} |
    {store, type(), value(), value()} |
    {load, type(), value()} |
    {blit, value(), value(), integer()}.

-type value() :: {temporary, binary()} | {global, binary()} | {const, integer()}.

-type type() :: word |
    long |
    single |
    double |
    byte |
    halfword |
    {aggregate, type_def()}.

-type data_def() :: {data_def,
        linkage(),
        binary(),
        gleam@option:option(integer()),
        list({type(), data_item()})}.

-type type_def() :: {type_def,
        binary(),
        gleam@option:option(integer()),
        list({type(), integer()})}.

-type data_item() :: {symbol, binary(), gleam@option:option(integer())} |
    {str, binary()} |
    {constant, integer()}.

-type statement() :: {assign, value(), type(), inst()} | {volatile, inst()}.

-type block() :: {block, binary(), list(statement())}.

-type function_() :: {function,
        linkage(),
        binary(),
        list({type(), value()}),
        gleam@option:option(type()),
        list(block())}.

-type linkage() :: {linkage,
        boolean(),
        gleam@option:option(binary()),
        gleam@option:option(binary())}.

-type module_() :: {module,
        list(function_()),
        list(type_def()),
        list(data_def())}.

-spec display_value(value()) -> binary().
display_value(Value) ->
    case Value of
        {temporary, Name} ->
            <<"%"/utf8, Name/binary>>;

        {global, Name@1} ->
            <<"$"/utf8, Name@1/binary>>;

        {const, Value@1} ->
            gleam@int:to_string(Value@1)
    end.

-spec into_abi(type()) -> type().
into_abi(Self) ->
    case Self of
        byte ->
            word;

        halfword ->
            word;

        Other ->
            Other
    end.

-spec into_base(type()) -> type().
into_base(Self) ->
    case Self of
        byte ->
            word;

        halfword ->
            word;

        {aggregate, _} ->
            long;

        Other ->
            Other
    end.

-spec size(type()) -> integer().
size(Self) ->
    case Self of
        byte ->
            1;

        halfword ->
            2;

        word ->
            4;

        single ->
            4;

        long ->
            8;

        double ->
            8;

        {aggregate, Td} ->
            case erlang:element(4, Td) of
                [] ->
                    0
            end
    end.

-spec display_data_item(data_item()) -> binary().
display_data_item(Item) ->
    case Item of
        {symbol, Name, Offset} ->
            case Offset of
                {some, Off} ->
                    <<<<<<"$"/utf8, Name/binary>>/binary, " +"/utf8>>/binary,
                        (gleam@int:to_string(Off))/binary>>;

                none ->
                    <<"$"/utf8, Name/binary>>
            end;

        {str, String} ->
            <<<<"\""/utf8, String/binary>>/binary, "\""/utf8>>;

        {constant, Val} ->
            gleam@int:to_string(Val)
    end.

-spec add_inst(block(), inst()) -> block().
add_inst(Block, Inst) ->
    {block,
        erlang:element(2, Block),
        gleam@list:append(erlang:element(3, Block), [{volatile, Inst}])}.

-spec assign_inst(block(), value(), type(), inst()) -> block().
assign_inst(Block, Val, Typ, Inst) ->
    {block,
        erlang:element(2, Block),
        gleam@list:append(erlang:element(3, Block), [{assign, Val, Typ, Inst}])}.

-spec jumps(block()) -> boolean().
jumps(Block) ->
    case gleam@list:last(erlang:element(3, Block)) of
        {ok, Statement} ->
            case Statement of
                {volatile, Instr} ->
                    case Instr of
                        {ret, _} ->
                            true;

                        {jmp, _} ->
                            true;

                        {jnz, _, _, _} ->
                            true;

                        _ ->
                            false
                    end;

                _ ->
                    false
            end;

        {error, _} ->
            false
    end.

-spec add_block(binary()) -> block().
add_block(Label) ->
    {block, Label, []}.

-spec last_block(list(block())) -> gleam@option:option(block()).
last_block(Blocks) ->
    case gleam@list:last(Blocks) of
        {ok, Block} ->
            {some, Block};

        {error, _} ->
            none
    end.

-spec display_linkage(linkage()) -> binary().
display_linkage(Linkage) ->
    Exported_str = case erlang:element(2, Linkage) of
        true ->
            <<"export "/utf8>>;

        false ->
            <<""/utf8>>
    end,
    Section_str = case erlang:element(3, Linkage) of
        {some, Section} ->
            <<<<<<<<"section \""/utf8, Section/binary>>/binary, "\""/utf8>>/binary,
                    (case erlang:element(4, Linkage) of
                        {some, Secflags} ->
                            <<<<" \""/utf8, Secflags/binary>>/binary,
                                "\""/utf8>>;

                        none ->
                            <<""/utf8>>
                    end)/binary>>/binary,
                " "/utf8>>;

        none ->
            <<""/utf8>>
    end,
    <<Exported_str/binary, Section_str/binary>>.

-spec private() -> linkage().
private() ->
    {linkage, false, none, none}.

-spec new_datadef() -> data_def().
new_datadef() ->
    {data_def, private(), <<""/utf8>>, none, []}.

-spec new_function() -> function_().
new_function() ->
    {function, private(), <<""/utf8>>, [], none, []}.

-spec private_with_section(binary()) -> linkage().
private_with_section(Section) ->
    {linkage, false, {some, Section}, none}.

-spec public() -> linkage().
public() ->
    {linkage, true, none, none}.

-spec public_with_section(binary()) -> linkage().
public_with_section(Section) ->
    {linkage, true, {some, Section}, none}.

-spec new_module() -> module_().
new_module() ->
    {module, [], [], []}.

-spec add_function(module_(), function_()) -> module_().
add_function(Module, Function) ->
    {module,
        gleam@list:append(erlang:element(2, Module), [Function]),
        erlang:element(3, Module),
        erlang:element(4, Module)}.

-spec add_type(module_(), type_def()) -> module_().
add_type(Module, Type_def) ->
    {module,
        erlang:element(2, Module),
        gleam@list:append(erlang:element(3, Module), [Type_def]),
        erlang:element(4, Module)}.

-spec add_data(module_(), data_def()) -> module_().
add_data(Module, Data_def) ->
    {module,
        erlang:element(2, Module),
        erlang:element(3, Module),
        gleam@list:append(erlang:element(4, Module), [Data_def])}.

-spec display_type_def(type_def()) -> binary().
display_type_def(Def) ->
    Align_str = case erlang:element(3, Def) of
        {some, Align} ->
            <<<<"align "/utf8, (gleam@int:to_string(Align))/binary>>/binary,
                " "/utf8>>;

        none ->
            <<""/utf8>>
    end,
    Items_str = begin
        _pipe = erlang:element(4, Def),
        _pipe@1 = gleam@list:index_map(_pipe, fun(_, Item) -> case Item of
                    {Ty, Count} ->
                        case Count > 1 of
                            false ->
                                display_type(Ty);

                            true ->
                                <<<<(display_type(Ty))/binary, " "/utf8>>/binary,
                                    (gleam@int:to_string(Count))/binary>>
                        end
                end end),
        gleam@string:join(_pipe@1, <<", "/utf8>>)
    end,
    <<<<<<<<<<<<"type :"/utf8, (erlang:element(2, Def))/binary>>/binary,
                        " = "/utf8>>/binary,
                    Align_str/binary>>/binary,
                "{ "/utf8>>/binary,
            Items_str/binary>>/binary,
        " }"/utf8>>.

-spec display_type(type()) -> binary().
display_type(Ty) ->
    case Ty of
        byte ->
            <<"b"/utf8>>;

        halfword ->
            <<"h"/utf8>>;

        word ->
            <<"w"/utf8>>;

        long ->
            <<"l"/utf8>>;

        single ->
            <<"s"/utf8>>;

        double ->
            <<"d"/utf8>>;

        {aggregate, Ty@1} ->
            display_type_def(Ty@1)
    end.

-spec display_inst(inst()) -> binary().
display_inst(Inst) ->
    case Inst of
        {add, A, B} ->
            <<<<<<"add "/utf8, (display_value(A))/binary>>/binary, ", "/utf8>>/binary,
                (display_value(B))/binary>>;

        {sub, A@1, B@1} ->
            <<<<<<"sub "/utf8, (display_value(A@1))/binary>>/binary, ", "/utf8>>/binary,
                (display_value(B@1))/binary>>;

        {mul, A@2, B@2} ->
            <<<<<<"mul "/utf8, (display_value(A@2))/binary>>/binary, ", "/utf8>>/binary,
                (display_value(B@2))/binary>>;

        {'div', A@3, B@3} ->
            <<<<<<"div "/utf8, (display_value(A@3))/binary>>/binary, ", "/utf8>>/binary,
                (display_value(B@3))/binary>>;

        {'rem', A@4, B@4} ->
            <<<<<<"rem "/utf8, (display_value(A@4))/binary>>/binary, ", "/utf8>>/binary,
                (display_value(B@4))/binary>>;

        {comp, Ty, Cmp, A@5, B@5} ->
            case Ty of
                {aggregate, _} ->
                    <<"Cannot Compare aggregate types"/utf8>>;

                _ ->
                    case Cmp of
                        slt ->
                            <<<<<<<<<<<<<<"c"/utf8, "slt"/utf8>>/binary,
                                                    " "/utf8>>/binary,
                                                (display_type(Ty))/binary>>/binary,
                                            " "/utf8>>/binary,
                                        (display_value(A@5))/binary>>/binary,
                                    " "/utf8>>/binary,
                                (display_value(B@5))/binary>>;

                        sle ->
                            <<<<<<<<<<<<<<"c"/utf8, "sle"/utf8>>/binary,
                                                    " "/utf8>>/binary,
                                                (display_type(Ty))/binary>>/binary,
                                            " "/utf8>>/binary,
                                        (display_value(A@5))/binary>>/binary,
                                    " "/utf8>>/binary,
                                (display_value(B@5))/binary>>;

                        sgt ->
                            <<<<<<<<<<<<<<"c"/utf8, "sgt"/utf8>>/binary,
                                                    " "/utf8>>/binary,
                                                (display_type(Ty))/binary>>/binary,
                                            " "/utf8>>/binary,
                                        (display_value(A@5))/binary>>/binary,
                                    " "/utf8>>/binary,
                                (display_value(B@5))/binary>>;

                        sge ->
                            <<<<<<<<<<<<<<"c"/utf8, "sge"/utf8>>/binary,
                                                    " "/utf8>>/binary,
                                                (display_type(Ty))/binary>>/binary,
                                            " "/utf8>>/binary,
                                        (display_value(A@5))/binary>>/binary,
                                    " "/utf8>>/binary,
                                (display_value(B@5))/binary>>;

                        eq ->
                            <<<<<<<<<<<<<<"c"/utf8, "eq"/utf8>>/binary,
                                                    " "/utf8>>/binary,
                                                (display_type(Ty))/binary>>/binary,
                                            " "/utf8>>/binary,
                                        (display_value(A@5))/binary>>/binary,
                                    " "/utf8>>/binary,
                                (display_value(B@5))/binary>>;

                        ne ->
                            <<<<<<<<<<<<<<"c"/utf8, "ne"/utf8>>/binary,
                                                    " "/utf8>>/binary,
                                                (display_type(Ty))/binary>>/binary,
                                            " "/utf8>>/binary,
                                        (display_value(A@5))/binary>>/binary,
                                    " "/utf8>>/binary,
                                (display_value(B@5))/binary>>
                    end
            end;

        {'and', A@6, B@6} ->
            <<<<<<"and "/utf8, (display_value(A@6))/binary>>/binary, ", "/utf8>>/binary,
                (display_value(B@6))/binary>>;

        {'or', A@7, B@7} ->
            <<<<<<"or "/utf8, (display_value(A@7))/binary>>/binary, ", "/utf8>>/binary,
                (display_value(B@7))/binary>>;

        {copy, Val} ->
            <<"copy "/utf8, (display_value(Val))/binary>>;

        {ret, Val@1} ->
            case Val@1 of
                {some, Val@2} ->
                    <<<<"ret "/utf8, (display_value(Val@2))/binary>>/binary,
                        "\n"/utf8>>;

                none ->
                    <<"ret\n"/utf8>>
            end;

        {jnz, Val@3, If_nonzero, If_zero} ->
            <<<<<<<<<<"jnz "/utf8, (display_value(Val@3))/binary>>/binary,
                            ", @"/utf8>>/binary,
                        If_nonzero/binary>>/binary,
                    ", @"/utf8>>/binary,
                If_zero/binary>>;

        {jmp, Str} ->
            <<"jmp @"/utf8, Str/binary>>;

        {call, Name, Args} ->
            Arg_str = begin
                _pipe = Args,
                _pipe@1 = gleam@list:index_map(_pipe, fun(_, Arg) -> case Arg of
                            {Ty@1, Val@4} ->
                                <<<<(display_type(Ty@1))/binary, " "/utf8>>/binary,
                                    (display_value(Val@4))/binary>>
                        end end),
                gleam@string:join(_pipe@1, <<", "/utf8>>)
            end,
            <<<<<<<<"call "/utf8, (display_value(Name))/binary>>/binary,
                        "("/utf8>>/binary,
                    Arg_str/binary>>/binary,
                ")"/utf8>>;

        {alloc4, Int} ->
            <<"alloc4 "/utf8, (gleam@int:to_string(Int))/binary>>;

        {alloc8, Int@1} ->
            <<"alloc8 "/utf8, (gleam@int:to_string(Int@1))/binary>>;

        {alloc16, Int@2} ->
            <<"alloc16 "/utf8, (gleam@int:to_string(Int@2))/binary>>;

        {store, Typ, Value, Dest} ->
            case Typ of
                {aggregate, _} ->
                    <<"Store to an aggregate type"/utf8>>;

                _ ->
                    <<<<<<<<<<"store"/utf8, (display_type(Typ))/binary>>/binary,
                                    " "/utf8>>/binary,
                                (display_value(Value))/binary>>/binary,
                            " "/utf8>>/binary,
                        (display_value(Dest))/binary>>
            end;

        {load, Typ@1, Val@5} ->
            case Typ@1 of
                {aggregate, _} ->
                    <<"Load aggregate type"/utf8>>;

                _ ->
                    <<<<<<"load"/utf8, (display_type(Typ@1))/binary>>/binary,
                            " "/utf8>>/binary,
                        (display_value(Val@5))/binary>>
            end;

        {blit, Src, Dest@1, N} ->
            <<<<<<<<<<"blit "/utf8, (display_value(Src))/binary>>/binary,
                            ", "/utf8>>/binary,
                        (display_value(Dest@1))/binary>>/binary,
                    ", "/utf8>>/binary,
                (gleam@int:to_string(N))/binary>>
    end.

-spec display_data_def(data_def()) -> binary().
display_data_def(Def) ->
    Linkage_str = display_linkage(erlang:element(2, Def)),
    Align_str = case erlang:element(4, Def) of
        {some, Align} ->
            <<" align "/utf8, (gleam@int:to_string(Align))/binary>>;

        none ->
            <<""/utf8>>
    end,
    Items_str = begin
        _pipe = erlang:element(5, Def),
        _pipe@1 = gleam@list:index_map(_pipe, fun(_, Item) -> case Item of
                    {Ty, Di} ->
                        <<<<(display_type(Ty))/binary, " "/utf8>>/binary,
                            (display_data_item(Di))/binary>>
                end end),
        gleam@string:join(_pipe@1, <<", "/utf8>>)
    end,
    <<<<<<<<<<<<<<Linkage_str/binary, "data $"/utf8>>/binary,
                            (erlang:element(3, Def))/binary>>/binary,
                        " ="/utf8>>/binary,
                    Align_str/binary>>/binary,
                " { "/utf8>>/binary,
            Items_str/binary>>/binary,
        " }"/utf8>>.

-spec display_statement(statement()) -> binary().
display_statement(Stmt) ->
    case Stmt of
        {assign, Val, Typ, Inst} ->
            <<<<<<<<(display_value(Val))/binary, " ="/utf8>>/binary,
                        (display_type(Typ))/binary>>/binary,
                    " "/utf8>>/binary,
                (display_inst(Inst))/binary>>;

        {volatile, Inst@1} ->
            display_inst(Inst@1)
    end.

-spec display_block(block()) -> binary().
display_block(Block) ->
    Label = erlang:element(2, Block),
    Statements = begin
        _pipe = erlang:element(3, Block),
        _pipe@1 = gleam@list:map(_pipe, fun display_statement/1),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    <<<<Label/binary, "\n"/utf8>>/binary, Statements/binary>>.

-spec display_arguments(list({type(), value()})) -> binary().
display_arguments(Arguments) ->
    case Arguments of
        [] ->
            <<""/utf8>>;

        _ ->
            _pipe = Arguments,
            _pipe@1 = gleam@list:index_map(_pipe, fun(_, Arg) -> case Arg of
                        {Ty, Val} ->
                            <<<<(display_type(Ty))/binary, " "/utf8>>/binary,
                                (display_value(Val))/binary>>
                    end end),
            gleam@string:join(_pipe@1, <<", "/utf8>>)
    end.

-spec display_blocks(list(block())) -> binary().
display_blocks(Blocks) ->
    _pipe = Blocks,
    _pipe@1 = gleam@list:map(_pipe, fun(Block) -> display_block(Block) end),
    gleam@string:join(_pipe@1, <<"\n"/utf8>>).

-spec display_function(function_()) -> binary().
display_function(Func) ->
    Linkage_str = display_linkage(erlang:element(2, Func)),
    Name_str = erlang:element(3, Func),
    Return_str = case erlang:element(5, Func) of
        {some, Ty} ->
            <<" "/utf8, (display_type(Ty))/binary>>;

        none ->
            <<""/utf8>>
    end,
    Args_str = display_arguments(erlang:element(4, Func)),
    Blocks_str = display_blocks(erlang:element(6, Func)),
    <<<<<<<<<<<<<<<<<<<<<<Linkage_str/binary, "function"/utf8>>/binary,
                                            Return_str/binary>>/binary,
                                        " "/utf8>>/binary,
                                    "$"/utf8>>/binary,
                                Name_str/binary>>/binary,
                            "("/utf8>>/binary,
                        Args_str/binary>>/binary,
                    ")"/utf8>>/binary,
                " {\n"/utf8>>/binary,
            Blocks_str/binary>>/binary,
        "}"/utf8>>.

-spec display_module(module_()) -> binary().
display_module(Module) ->
    Functions_str = begin
        _pipe = erlang:element(2, Module),
        _pipe@1 = gleam@list:map(_pipe, fun display_function/1),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    Types_str = begin
        _pipe@2 = erlang:element(3, Module),
        _pipe@3 = gleam@list:map(_pipe@2, fun display_type_def/1),
        gleam@string:join(_pipe@3, <<"\n"/utf8>>)
    end,
    Data_str = begin
        _pipe@4 = erlang:element(4, Module),
        _pipe@5 = gleam@list:map(_pipe@4, fun display_data_def/1),
        gleam@string:join(_pipe@5, <<"\n"/utf8>>)
    end,
    <<<<<<Functions_str/binary, Types_str/binary>>/binary, "\n"/utf8>>/binary,
        Data_str/binary>>.
