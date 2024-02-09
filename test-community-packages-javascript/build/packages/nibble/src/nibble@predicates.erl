-module(nibble@predicates).
-compile([no_auto_import, nowarn_unused_vars]).

-export([is_digit/1, is_whitespace/1, is_upper/1, is_lower/1, is_alphanum/1]).

-spec is_digit(binary()) -> boolean().
is_digit(Grapheme) ->
    case Grapheme of
        <<"0"/utf8>> ->
            true;

        <<"1"/utf8>> ->
            true;

        <<"2"/utf8>> ->
            true;

        <<"3"/utf8>> ->
            true;

        <<"4"/utf8>> ->
            true;

        <<"5"/utf8>> ->
            true;

        <<"6"/utf8>> ->
            true;

        <<"7"/utf8>> ->
            true;

        <<"8"/utf8>> ->
            true;

        <<"9"/utf8>> ->
            true;

        _ ->
            false
    end.

-spec is_whitespace(binary()) -> boolean().
is_whitespace(Grapheme) ->
    case Grapheme of
        <<" "/utf8>> ->
            true;

        <<"\t"/utf8>> ->
            true;

        <<"\n"/utf8>> ->
            true;

        <<"\r"/utf8>> ->
            true;

        _ ->
            false
    end.

-spec is_upper(binary()) -> boolean().
is_upper(Grapheme) ->
    case Grapheme of
        <<"A"/utf8>> ->
            true;

        <<"B"/utf8>> ->
            true;

        <<"C"/utf8>> ->
            true;

        <<"D"/utf8>> ->
            true;

        <<"E"/utf8>> ->
            true;

        <<"F"/utf8>> ->
            true;

        <<"G"/utf8>> ->
            true;

        <<"H"/utf8>> ->
            true;

        <<"I"/utf8>> ->
            true;

        <<"J"/utf8>> ->
            true;

        <<"K"/utf8>> ->
            true;

        <<"L"/utf8>> ->
            true;

        <<"M"/utf8>> ->
            true;

        <<"N"/utf8>> ->
            true;

        <<"O"/utf8>> ->
            true;

        <<"P"/utf8>> ->
            true;

        <<"Q"/utf8>> ->
            true;

        <<"R"/utf8>> ->
            true;

        <<"S"/utf8>> ->
            true;

        <<"T"/utf8>> ->
            true;

        <<"U"/utf8>> ->
            true;

        <<"V"/utf8>> ->
            true;

        <<"W"/utf8>> ->
            true;

        <<"X"/utf8>> ->
            true;

        <<"Y"/utf8>> ->
            true;

        <<"Z"/utf8>> ->
            true;

        _ ->
            false
    end.

-spec is_lower(binary()) -> boolean().
is_lower(Grapheme) ->
    case Grapheme of
        <<"a"/utf8>> ->
            true;

        <<"b"/utf8>> ->
            true;

        <<"c"/utf8>> ->
            true;

        <<"d"/utf8>> ->
            true;

        <<"e"/utf8>> ->
            true;

        <<"f"/utf8>> ->
            true;

        <<"g"/utf8>> ->
            true;

        <<"h"/utf8>> ->
            true;

        <<"i"/utf8>> ->
            true;

        <<"j"/utf8>> ->
            true;

        <<"k"/utf8>> ->
            true;

        <<"l"/utf8>> ->
            true;

        <<"m"/utf8>> ->
            true;

        <<"n"/utf8>> ->
            true;

        <<"o"/utf8>> ->
            true;

        <<"p"/utf8>> ->
            true;

        <<"q"/utf8>> ->
            true;

        <<"r"/utf8>> ->
            true;

        <<"s"/utf8>> ->
            true;

        <<"t"/utf8>> ->
            true;

        <<"u"/utf8>> ->
            true;

        <<"v"/utf8>> ->
            true;

        <<"w"/utf8>> ->
            true;

        <<"x"/utf8>> ->
            true;

        <<"y"/utf8>> ->
            true;

        <<"z"/utf8>> ->
            true;

        _ ->
            false
    end.

-spec is_alphanum(binary()) -> boolean().
is_alphanum(Grapheme) ->
    (is_digit(Grapheme) orelse is_upper(Grapheme)) orelse is_lower(Grapheme).
