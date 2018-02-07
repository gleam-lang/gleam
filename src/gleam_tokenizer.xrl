Definitions.

Int     = [-+]?[0-9]+
Float   = [-+]?[0-9]+\.[0-9]+
WS      = [\n\s\r\t]
Atom    = :[a-zA-Z0-9!\?_]*
Name    = [a-z_][a-zA-Z0-9!\?_]*
UpName  = [A-Z][a-zA-Z0-9!\?_]*
String  = "([^\\""]|\\.)*"
Comment = \/\/[^\n]*

Rules.

module    : {token, {kw_module, TokenLine}}.
export    : {token, {kw_export, TokenLine}}.
let       : {token, {kw_let, TokenLine}}.
\==       : {token, {'==', TokenLine}}.
\=        : {token, {'=', TokenLine}}.
\+        : {token, {'+', TokenLine}}.
\-        : {token, {'-', TokenLine}}.
\*        : {token, {'*', TokenLine}}.
\/        : {token, {'/', TokenLine}}.
\+\.      : {token, {'+.', TokenLine}}.
\-\.      : {token, {'-.', TokenLine}}.
\*\.      : {token, {'*.', TokenLine}}.
\/\.      : {token, {'/.', TokenLine}}.
\<=       : {token, {'<=', TokenLine}}.
\<        : {token, {'<', TokenLine}}.
\>=       : {token, {'>', TokenLine}}.
\>        : {token, {'>=', TokenLine}}.
\.        : {token, {'.', TokenLine}}.
\|        : {token, {'|', TokenLine}}.
\,        : {token, {',', TokenLine}}.
\(        : {token, {'(', TokenLine}}.
\)        : {token, {')', TokenLine}}.
\[        : {token, {'[', TokenLine}}.
\]        : {token, {']', TokenLine}}.
\{        : {token, {'{', TokenLine}}.
\}        : {token, {'}', TokenLine}}.
{Int}     : {token, {int, TokenLine, int(TokenChars)}}.
{Float}   : {token, {float, TokenLine, flt(TokenChars)}}.
{Atom}    : {token, {atom, TokenLine, atom(TokenChars)}}.
:{String} : {token, {atom, TokenLine, atom(TokenChars)}}.
{Name}    : {token, {name, TokenLine, list_to_atom(TokenChars)}}.
{UpName}  : {token, {upname, TokenLine, list_to_atom(TokenChars)}}.
{String}  : {token, {string, TokenLine, str(TokenChars)}}.
{Comment} : skip_token.
{WS}      : skip_token.


Erlang code.

int(S) when is_list(S) ->
  {I, _} = string:to_integer(S),
  I.

flt(S) when is_list(S) ->
  {F, _} = string:to_float(S),
  F.

atom([$:, $" | S]) -> list_to_atom(lists:droplast(S));
atom([$: | S])     -> list_to_atom(S).

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
