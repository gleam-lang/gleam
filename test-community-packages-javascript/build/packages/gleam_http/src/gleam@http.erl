-module(gleam@http).
-compile([no_auto_import, nowarn_unused_vars]).

-export([parse_method/1, method_to_string/1, scheme_to_string/1, scheme_from_string/1, method_from_dynamic/1]).
-export_type([method/0, scheme/0]).

-type method() :: get |
    post |
    head |
    put |
    delete |
    trace |
    connect |
    options |
    patch |
    {other, binary()}.

-type scheme() :: http | https.

-spec parse_method(binary()) -> {ok, method()} | {error, nil}.
parse_method(S) ->
    case gleam@string:lowercase(S) of
        <<"connect"/utf8>> ->
            {ok, connect};

        <<"delete"/utf8>> ->
            {ok, delete};

        <<"get"/utf8>> ->
            {ok, get};

        <<"head"/utf8>> ->
            {ok, head};

        <<"options"/utf8>> ->
            {ok, options};

        <<"patch"/utf8>> ->
            {ok, patch};

        <<"post"/utf8>> ->
            {ok, post};

        <<"put"/utf8>> ->
            {ok, put};

        <<"trace"/utf8>> ->
            {ok, trace};

        _ ->
            {error, nil}
    end.

-spec method_to_string(method()) -> binary().
method_to_string(Method) ->
    case Method of
        connect ->
            <<"connect"/utf8>>;

        delete ->
            <<"delete"/utf8>>;

        get ->
            <<"get"/utf8>>;

        head ->
            <<"head"/utf8>>;

        options ->
            <<"options"/utf8>>;

        patch ->
            <<"patch"/utf8>>;

        post ->
            <<"post"/utf8>>;

        put ->
            <<"put"/utf8>>;

        trace ->
            <<"trace"/utf8>>;

        {other, S} ->
            S
    end.

-spec scheme_to_string(scheme()) -> binary().
scheme_to_string(Scheme) ->
    case Scheme of
        http ->
            <<"http"/utf8>>;

        https ->
            <<"https"/utf8>>
    end.

-spec scheme_from_string(binary()) -> {ok, scheme()} | {error, nil}.
scheme_from_string(Scheme) ->
    case gleam@string:lowercase(Scheme) of
        <<"http"/utf8>> ->
            {ok, http};

        <<"https"/utf8>> ->
            {ok, https};

        _ ->
            {error, nil}
    end.

-spec method_from_dynamic(gleam@dynamic:dynamic()) -> {ok, method()} |
    {error, list(gleam@dynamic:decode_error())}.
method_from_dynamic(Value) ->
    case gleam_http_native:decode_method(Value) of
        {ok, Method} ->
            {ok, Method};

        {error, _} ->
            {error,
                [{decode_error,
                        <<"HTTP method"/utf8>>,
                        gleam@dynamic:classify(Value),
                        []}]}
    end.
