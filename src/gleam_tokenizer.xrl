Definitions.

Int    = [0-9]+
Float  = [0-9]+\.[0-9]+
WS     = [\n\s\r\t]
String = "([^\\""]|\\.)*"
Ident  = [a-z_][a-zA-Z0-9!\?_]*
Atom   = \:[a-zA-Z0-9!\?_-]+

Rules.

module     : {token, {module,     TokenLine}}.
private    : {token, {private,    TokenLine}}.
public     : {token, {public,     TokenLine}}.
\(         : {token, {'(',        TokenLine}}.
\)         : {token, {')',        TokenLine}}.
\{         : {token, {'{',        TokenLine}}.
\}         : {token, {'}',        TokenLine}}.
\[         : {token, {'[',        TokenLine}}.
\]         : {token, {']',        TokenLine}}.
\.         : {token, {'.',        TokenLine}}.
\,         : {token, {',',        TokenLine}}.
\n         : {token, {'nl',       TokenLine}}.
{Int}      : {token, {num,        TokenLine, int(TokenChars)}}.
{Float}    : {token, {num,        TokenLine, flt(TokenChars)}}.
{String}   : {token, {string,     TokenLine, strValue(TokenChars)}}.
{Ident}    : {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.
{Atom}     : {token, {atom,       TokenLine, atomValue(TokenChars)}}.
{WS}       : skip_token.


Erlang code.

int(S) when is_list(S) ->
  {I, _} = string:to_integer(S),
  I.

flt(S) when is_list(S) ->
  {F, _} = string:to_float(S),
  F.

atomValue(S) when is_list(S) ->
  Contents = tl(S),
  list_to_atom(Contents).

strValue(S) when is_list(S) ->
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
