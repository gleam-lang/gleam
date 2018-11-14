Definitions.

Int     = [-+]?[0-9]+
Float   = [-+]?[0-9]+\.[0-9]+
WS      = [\n\s;]
Name    = [a-z][a-zA-Z0-9_]*
UpName  = [A-Z][a-zA-Z0-9!\?_]*
String  = "([^\\""]|\\.)*"
Atom    = '([^\\'']|\\.)*'
Comment = \/\/[^\n]*

Rules.

external   : {token, {kw_external, m(TokenLine)}}.
import     : {token, {kw_import, m(TokenLine)}}.
case       : {token, {kw_case, m(TokenLine)}}.
test       : {token, {kw_test, m(TokenLine)}}.
type       : {token, {kw_type, m(TokenLine)}}.
enum       : {token, {kw_enum, m(TokenLine)}}.
pub        : {token, {kw_pub, m(TokenLine)}}.
fn         : {token, {kw_fn, m(TokenLine)}}.
\|>        : {token, {'|>', m(TokenLine)}}.
!=         : {token, {'!=', m(TokenLine)}}.
==         : {token, {'==', m(TokenLine)}}.
=>         : {token, {'=>', m(TokenLine)}}.
=          : {token, {'=', m(TokenLine)}}.
=>         : {token, {'=>', m(TokenLine)}}.
->         : {token, {'->', m(TokenLine)}}.
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
\:         : {token, {':', m(TokenLine)}}.
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
_          : {token, {hole, m(TokenLine)}}.
_{Name}    : {token, {hole, m(TokenLine)}}.
{Name}     : {token, {name, m(TokenLine), TokenChars}}.
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

atom(S) when is_list(S) ->
  Contents  = tl(lists:droplast(S)),
  deescape(Contents).

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
