-module(glerm).
-compile([no_auto_import, nowarn_unused_vars]).

-export([selector/0, clear/0, draw/1, start_listener_spec/1, start_listener/2, print/1, size/0, move_to/2, enable_raw_mode/0, disable_raw_mode/0, enter_alternate_screen/0, leave_alternate_screen/0, enable_mouse_capture/0, disable_mouse_capture/0]).
-export_type([modifier/0, key_code/0, mouse_button/0, mouse_event/0, focus_event/0, event/0, listener_message/1, listener_spec/2]).

-type modifier() :: shift | alt | control.

-type key_code() :: {character, binary()} |
    enter |
    backspace |
    left |
    right |
    down |
    up |
    unsupported.

-type mouse_button() :: mouse_left | mouse_right | mouse_middle.

-type mouse_event() :: {mouse_down,
        mouse_button(),
        gleam@option:option(modifier())} |
    {mouse_up, mouse_button(), gleam@option:option(modifier())} |
    {drag, mouse_button(), gleam@option:option(modifier())} |
    moved |
    scroll_down |
    scroll_up.

-type focus_event() :: lost | gained.

-type event() :: {focus, focus_event()} |
    {key, key_code(), gleam@option:option(modifier())} |
    {mouse, mouse_event()} |
    {resize, integer(), integer()} |
    {unknown, binary(), gleam@dynamic:dynamic()}.

-type listener_message(GUP) :: {term, event()} | {user, GUP}.

-type listener_spec(GUQ, GUR) :: {listener_spec,
        fun(() -> {GUQ, gleam@option:option(gleam@erlang@process:selector(GUR))}),
        fun((listener_message(GUR), GUQ) -> gleam@otp@actor:next(GUQ))}.

-spec decode_atom(binary(), GUW) -> fun((gleam@dynamic:dynamic()) -> {ok, GUW} |
    {error, list(gleam@dynamic:decode_error())}).
decode_atom(Val, Actual) ->
    Real_atom = erlang:binary_to_atom(Val),
    Decode = gleam@function:compose(
        fun gleam@erlang@atom:from_dynamic/1,
        fun(Maybe_atom) -> _pipe = Maybe_atom,
            gleam@result:then(
                _pipe,
                fun(Decoded) -> case Decoded =:= Real_atom of
                        true ->
                            {ok, Real_atom};

                        false ->
                            {error,
                                [{decode_error,
                                        Val,
                                        erlang:atom_to_binary(Decoded),
                                        []}]}
                    end end
            ) end
    ),
    fun(Msg) -> _pipe@1 = Decode(Msg),
        gleam@result:replace(_pipe@1, Actual) end.

-spec modifier_decoder() -> fun((gleam@dynamic:dynamic()) -> {ok,
        gleam@option:option(modifier())} |
    {error, list(gleam@dynamic:decode_error())}).
modifier_decoder() ->
    Decode_some = decode_atom(
        <<"some"/utf8>>,
        fun(Field@0) -> {some, Field@0} end
    ),
    gleam@dynamic:any(
        [decode_atom(<<"none"/utf8>>, none),
            gleam@function:compose(
                gleam@dynamic:tuple2(
                    Decode_some,
                    decode_atom(<<"shift"/utf8>>, shift)
                ),
                fun(_capture) ->
                    gleam@result:replace(_capture, {some, shift})
                end
            ),
            gleam@function:compose(
                gleam@dynamic:tuple2(
                    Decode_some,
                    decode_atom(<<"alt"/utf8>>, alt)
                ),
                fun(_capture@1) ->
                    gleam@result:replace(_capture@1, {some, alt})
                end
            ),
            gleam@function:compose(
                gleam@dynamic:tuple2(
                    Decode_some,
                    decode_atom(<<"control"/utf8>>, control)
                ),
                fun(_capture@2) ->
                    gleam@result:replace(_capture@2, {some, control})
                end
            )]
    ).

-spec keycode_decoder() -> fun((gleam@dynamic:dynamic()) -> {ok, key_code()} |
    {error, list(gleam@dynamic:decode_error())}).
keycode_decoder() ->
    gleam@dynamic:any(
        [begin
                _pipe = gleam@dynamic:tuple2(
                    decode_atom(
                        <<"character"/utf8>>,
                        fun(Field@0) -> {character, Field@0} end
                    ),
                    fun gleam@dynamic:string/1
                ),
                gleam@function:compose(
                    _pipe,
                    fun(Maybe_pair) -> case Maybe_pair of
                            {ok, {_, Value}} ->
                                {ok, {character, Value}};

                            {error, Err} ->
                                {error, Err}
                        end end
                )
            end,
            decode_atom(<<"enter"/utf8>>, enter),
            decode_atom(<<"backspace"/utf8>>, backspace),
            decode_atom(<<"left"/utf8>>, left),
            decode_atom(<<"right"/utf8>>, right),
            decode_atom(<<"down"/utf8>>, down),
            decode_atom(<<"up"/utf8>>, up),
            decode_atom(<<"unsupported"/utf8>>, unsupported)]
    ).

-spec mouse_button_decoder() -> fun((gleam@dynamic:dynamic()) -> {ok,
        mouse_button()} |
    {error, list(gleam@dynamic:decode_error())}).
mouse_button_decoder() ->
    gleam@dynamic:any(
        [decode_atom(<<"mouse_left"/utf8>>, mouse_left),
            decode_atom(<<"mouse_right"/utf8>>, mouse_right),
            decode_atom(<<"mouse_middle"/utf8>>, mouse_middle)]
    ).

-spec mouse_event_decoder() -> fun((gleam@dynamic:dynamic()) -> {ok,
        mouse_event()} |
    {error, list(gleam@dynamic:decode_error())}).
mouse_event_decoder() ->
    gleam@dynamic:any(
        [begin
                _pipe = gleam@dynamic:tuple3(
                    decode_atom(
                        <<"mouse_down"/utf8>>,
                        fun(Field@0, Field@1) -> {mouse_down, Field@0, Field@1} end
                    ),
                    mouse_button_decoder(),
                    modifier_decoder()
                ),
                gleam@function:compose(
                    _pipe,
                    fun(Maybe_triple) -> case Maybe_triple of
                            {ok, {_, Button, Modifier}} ->
                                {ok, {mouse_down, Button, Modifier}};

                            {error, Err} ->
                                {error, Err}
                        end end
                )
            end,
            begin
                _pipe@1 = gleam@dynamic:tuple3(
                    decode_atom(
                        <<"mouse_up"/utf8>>,
                        fun(Field@0, Field@1) -> {mouse_up, Field@0, Field@1} end
                    ),
                    mouse_button_decoder(),
                    modifier_decoder()
                ),
                gleam@function:compose(
                    _pipe@1,
                    fun(Maybe_triple@1) -> case Maybe_triple@1 of
                            {ok, {_, Button@1, Modifier@1}} ->
                                {ok, {mouse_up, Button@1, Modifier@1}};

                            {error, Err@1} ->
                                {error, Err@1}
                        end end
                )
            end,
            begin
                _pipe@2 = gleam@dynamic:tuple3(
                    decode_atom(
                        <<"drag"/utf8>>,
                        fun(Field@0, Field@1) -> {drag, Field@0, Field@1} end
                    ),
                    mouse_button_decoder(),
                    modifier_decoder()
                ),
                gleam@function:compose(
                    _pipe@2,
                    fun(Maybe_triple@2) -> case Maybe_triple@2 of
                            {ok, {_, Button@2, Modifier@2}} ->
                                {ok, {drag, Button@2, Modifier@2}};

                            {error, Err@2} ->
                                {error, Err@2}
                        end end
                )
            end,
            decode_atom(<<"moved"/utf8>>, moved),
            decode_atom(<<"scroll_down"/utf8>>, scroll_down),
            decode_atom(<<"scroll_up"/utf8>>, scroll_up)]
    ).

-spec selector() -> gleam@erlang@process:selector(event()).
selector() ->
    _pipe = gleam_erlang_ffi:new_selector(),
    _pipe@4 = gleam@erlang@process:selecting_record2(
        _pipe,
        erlang:binary_to_atom(<<"focus"/utf8>>),
        fun(Inner) -> _pipe@1 = Inner,
            _pipe@2 = (gleam@dynamic:any(
                [decode_atom(<<"gained"/utf8>>, gained),
                    decode_atom(<<"lost"/utf8>>, lost)]
            ))(_pipe@1),
            _pipe@3 = gleam@result:map(
                _pipe@2,
                fun(Field@0) -> {focus, Field@0} end
            ),
            gleam@result:unwrap(_pipe@3, {unknown, <<"focus"/utf8>>, Inner}) end
    ),
    _pipe@5 = gleam@erlang@process:selecting_record3(
        _pipe@4,
        erlang:binary_to_atom(<<"key"/utf8>>),
        fun(First, Second) ->
            Key_code = (keycode_decoder())(First),
            Modifier = (modifier_decoder())(Second),
            case {Key_code, Modifier} of
                {{ok, Code}, {ok, Mod}} ->
                    {key, Code, Mod};

                {_, _} ->
                    {unknown,
                        <<"key"/utf8>>,
                        gleam@dynamic:from([First, Second])}
            end
        end
    ),
    _pipe@9 = gleam@erlang@process:selecting_record2(
        _pipe@5,
        erlang:binary_to_atom(<<"mouse"/utf8>>),
        fun(Inner@1) -> _pipe@6 = Inner@1,
            _pipe@7 = (mouse_event_decoder())(_pipe@6),
            _pipe@8 = gleam@result:map(
                _pipe@7,
                fun(Field@0) -> {mouse, Field@0} end
            ),
            gleam@result:lazy_unwrap(
                _pipe@8,
                fun() -> {unknown, <<"mouse"/utf8>>, Inner@1} end
            ) end
    ),
    gleam@erlang@process:selecting_record3(
        _pipe@9,
        erlang:binary_to_atom(<<"resize"/utf8>>),
        fun(First@1, Second@1) ->
            Columns = gleam@dynamic:int(First@1),
            Rows = gleam@dynamic:int(Second@1),
            case {Columns, Rows} of
                {{ok, Col}, {ok, Rows@1}} ->
                    {resize, Col, Rows@1};

                {_, _} ->
                    {unknown,
                        <<"resize"/utf8>>,
                        gleam@dynamic:from([First@1, Second@1])}
            end
        end
    ).

-spec clear() -> nil.
clear() ->
    glerm_ffi:clear().

-spec draw(list({integer(), integer(), binary()})) -> {ok, nil} | {error, nil}.
draw(Field@0) ->
    glerm_ffi:draw(Field@0).

-spec start_listener_spec(listener_spec(any(), GVF)) -> {ok,
        gleam@erlang@process:subject(listener_message(GVF))} |
    {error, gleam@otp@actor:start_error()}.
start_listener_spec(Spec) ->
    gleam@otp@actor:start_spec(
        {spec,
            fun() ->
                Pid = erlang:self(),
                _assert_subject = (erlang:element(2, Spec))(),
                {State, User_selector} = case _assert_subject of
                    {_, _} -> _assert_subject;
                    _assert_fail ->
                        erlang:error(#{gleam_error => assert,
                                    message => <<"Assertion pattern match failed"/utf8>>,
                                    value => _assert_fail,
                                    module => <<"glerm"/utf8>>,
                                    function => <<"start_listener_spec"/utf8>>,
                                    line => 314})
                end,
                Term_selector = begin
                    _pipe = selector(),
                    gleam_erlang_ffi:map_selector(
                        _pipe,
                        fun(Field@0) -> {term, Field@0} end
                    )
                end,
                Selector = begin
                    _pipe@1 = User_selector,
                    _pipe@4 = gleam@option:map(
                        _pipe@1,
                        fun(User) -> _pipe@2 = User,
                            _pipe@3 = gleam_erlang_ffi:map_selector(
                                _pipe@2,
                                fun(Field@0) -> {user, Field@0} end
                            ),
                            gleam_erlang_ffi:merge_selector(
                                Term_selector,
                                _pipe@3
                            ) end
                    ),
                    gleam@option:unwrap(_pipe@4, Term_selector)
                end,
                gleam@erlang@process:start(
                    fun() -> glerm_ffi:listen(Pid) end,
                    true
                ),
                {ready, State, Selector}
            end,
            500,
            erlang:element(3, Spec)}
    ).

-spec start_listener(GVL, fun((event(), GVL) -> gleam@otp@actor:next(GVL))) -> {ok,
        gleam@erlang@process:subject(event())} |
    {error, gleam@otp@actor:start_error()}.
start_listener(Initial_state, Loop) ->
    gleam@otp@actor:start_spec(
        {spec,
            fun() ->
                Pid = erlang:self(),
                gleam@erlang@process:start(
                    fun() -> glerm_ffi:listen(Pid) end,
                    true
                ),
                {ready, Initial_state, selector()}
            end,
            500,
            Loop}
    ).

-spec print(bitstring()) -> {ok, nil} | {error, nil}.
print(Field@0) ->
    glerm_ffi:print(Field@0).

-spec size() -> {ok, {integer(), integer()}} | {error, nil}.
size() ->
    glerm_ffi:size().

-spec move_to(integer(), integer()) -> nil.
move_to(Field@0, Field@1) ->
    glerm_ffi:move_to(Field@0, Field@1).

-spec enable_raw_mode() -> {ok, nil} | {error, nil}.
enable_raw_mode() ->
    glerm_ffi:enable_raw_mode().

-spec disable_raw_mode() -> {ok, nil} | {error, nil}.
disable_raw_mode() ->
    glerm_ffi:disable_raw_mode().

-spec enter_alternate_screen() -> {ok, nil} | {error, nil}.
enter_alternate_screen() ->
    glerm_ffi:enter_alternate_screen().

-spec leave_alternate_screen() -> {ok, nil} | {error, nil}.
leave_alternate_screen() ->
    glerm_ffi:leave_alternate_screen().

-spec enable_mouse_capture() -> {ok, nil} | {error, nil}.
enable_mouse_capture() ->
    glerm_ffi:enable_mouse_capture().

-spec disable_mouse_capture() -> {ok, nil} | {error, nil}.
disable_mouse_capture() ->
    glerm_ffi:disable_mouse_capture().
