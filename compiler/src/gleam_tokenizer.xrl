Definitions.

Int     = [-+]?[0-9]+
Float   = [-+]?[0-9]+\.[0-9]+
WS      = [\n\s;]
Atom    = :[a-zA-Z0-9!\?_]+
Name    = [a-z][a-zA-Z0-9_]*
UpName  = [A-Z][a-zA-Z0-9!\?_]*
String  = "([^\\""]|\\.)*"
Comment = \/\/[^\n]*

Rules.

module     : {token, {kw_module, m(TokenLine)}}.
exposing   : {token, {kw_exposing, m(TokenLine)}}.
raise\(    : {token, {kw_raise, m(TokenLine)}}.
throw\(    : {token, {kw_throw, m(TokenLine)}}.
case       : {token, {kw_case, m(TokenLine)}}.
test       : {token, {kw_test, m(TokenLine)}}.
fn         : {token, {kw_fn, m(TokenLine)}}.
fn\(       : {token, {kw_fn_call, m(TokenLine)}}.
\|>        : {token, {'|>', m(TokenLine)}}.
!=         : {token, {'!=', m(TokenLine)}}.
==         : {token, {'==', m(TokenLine)}}.
=>         : {token, {'=>', m(TokenLine)}}.
=          : {token, {'=', m(TokenLine)}}.
=>         : {token, {'=>', m(TokenLine)}}.
\+         : {token, {'+', m(TokenLine)}}.
-          : {token, {'-', m(TokenLine)}}.
\*         : {token, {'*', m(TokenLine)}}.
/          : {token, {'/', m(TokenLine)}}.
\+\.       : {token, {'+.', m(TokenLine)}}.
-\.        : {token, {'-.', m(TokenLine)}}.
\*\.       : {token, {'*.', m(TokenLine)}}.
/\.        : {token, {'/.', m(TokenLine)}}.
<=         : {token, {'<=', m(TokenLine)}}.
<          : {token, {'<', m(TokenLine)}}.
>=         : {token, {'>', m(TokenLine)}}.
::         : {token, {'::', m(TokenLine)}}.
\>         : {token, {'>=', m(TokenLine)}}.
\.         : {token, {'.', m(TokenLine)}}.
\|         : {token, {'|', m(TokenLine)}}.
\,         : {token, {',', m(TokenLine)}}.
\(         : {token, {'(', m(TokenLine)}}.
\)         : {token, {')', m(TokenLine)}}.
\[         : {token, {'[', m(TokenLine)}}.
\]         : {token, {']', m(TokenLine)}}.
\{         : {token, {'{', m(TokenLine)}}.
\}         : {token, {'}', m(TokenLine)}}.
{Int}      : {token, {int, m(TokenLine), int(TokenChars)}}.
{Float}    : {token, {float, m(TokenLine), flt(TokenChars)}}.
{Atom}     : {token, {atom, m(TokenLine), atom(TokenChars)}}.
:{String}  : {token, {atom, m(TokenLine), atom(TokenChars)}}.
_          : {token, {hole, m(TokenLine)}}.
_{Name}    : {token, {hole, m(TokenLine)}}.
{Name}\(   : {token, {call, m(TokenLine), call(TokenChars)}}.
{Name}     : {token, {name, m(TokenLine), TokenChars}}.
{UpName}\( : {token, {upcall, m(TokenLine), call(TokenChars)}}.
{UpName}   : {token, {upname, m(TokenLine), TokenChars}}.
{String}   : {token, {string, m(TokenLine), str(TokenChars)}}.
{Comment}  : skip_token.
{WS}       : skip_token.


Erlang code.

-include("gleam_records.hrl").

int(S) when is_list(S) ->
  {I, _} = string:to_integer(S),
  I.

flt(S) when is_list(S) ->
  {F, _} = string:to_float(S),
  F.

atom([$:, $" | S]) -> lists:droplast(S);
atom([$: | S])     -> S.

call(S) ->
  lists:droplast(S).

str(S) when is_list(S) ->
  Contents  = tl(lists:droplast(S)),
  Unescaped = deescape(Contents),
  list_to_binary(Unescaped).

deescape(S) when is_list(S) ->
  deescape(S, []).

deescape([$\\, C|Tail], Acc) ->
  deescape(Tail, [C|Acc]);
deescape([C|Tail], Acc) ->
  deescape(Tail, [C|Acc]);
deescape([], Acc) ->
  lists:reverse(Acc).

m(Line) ->
  #meta{line = Line}.
