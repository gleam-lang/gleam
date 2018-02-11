-module(gleam_codegen).
-include("gleam_records.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([module/1]).

module(#gleam_ast_module{name = Name, functions = Funs, exports = Exports}) ->
  PrefixedName = prefix_module(Name),
  C_name = cerl:c_atom(PrefixedName),
  C_exports =
    [ cerl:c_fname(module_info, 0)
    , cerl:c_fname(module_info, 1)
    | lists:map(fun export/1, Exports)
    ],
  C_definitions =
    [ module_info(PrefixedName, [])
    , module_info(PrefixedName, [cerl:c_var(item)])
    | lists:map(fun function/1, Funs)
    ],
  Attributes = [],
  Core = cerl:c_module(C_name, C_exports, Attributes, C_definitions),
  {ok, Core}.

export({Name, Arity}) when is_atom(Name), is_integer(Arity) ->
  cerl:c_fname(Name, Arity).

module_info(ModuleName, Params) when is_atom(ModuleName) ->
  Body = cerl:c_call(cerl:c_atom(erlang),
                     cerl:c_atom(get_module_info),
                     [cerl:c_atom(ModuleName) | Params]),
  C_fun = cerl:c_fun(Params, Body),
  C_fname = cerl:c_fname(module_info, length(Params)),
  {C_fname, C_fun}.

function(#gleam_ast_function{name = Name, args = Args, body = Body}) ->
  Arity = length(Args),
  C_fname = cerl:c_fname(Name, Arity),
  C_args = lists:map(fun var/1, Args),
  C_body = expression(Body),
  {C_fname, cerl:c_fun(C_args, C_body)}.

var(Atom) when is_atom(Atom) ->
  cerl:c_var(Atom).

expression(#gleam_ast_string{value = Value}) when is_binary(Value) ->
  Chars = binary_to_list(Value),
  ByteSequence = lists:map(fun binary_string_byte/1, Chars),
  cerl:c_binary(ByteSequence);

expression(#gleam_ast_list{elems = Elems}) ->
  C_elems = lists:map(fun expression/1, Elems),
  c_list(C_elems);

expression(#gleam_ast_tuple{elems = Elems}) ->
  C_elems = lists:map(fun expression/1, Elems),
  cerl:c_tuple(C_elems);

expression(#gleam_ast_atom{value = Value}) when is_atom(Value) ->
  cerl:c_atom(Value);

expression(#gleam_ast_int{value = Value}) when is_integer(Value) ->
  cerl:c_int(Value);

expression(#gleam_ast_float{value = Value}) when is_float(Value) ->
  cerl:c_float(Value);

expression(#gleam_ast_var{name = Name}) when is_atom(Name) ->
  cerl:c_var(Name);

expression(#gleam_ast_local_call{name = '::', args = [H, T]}) ->
  cerl:c_cons(expression(H), expression(T));

expression(#gleam_ast_local_call{name = Name, args = Args}) ->
  C_fname = cerl:c_fname(Name, length(Args)),
  C_args = lists:map(fun expression/1, Args),
  cerl:c_apply(C_fname, C_args);

expression(#gleam_ast_call{module = Mod, name = Name, args = Args}) ->
  C_module = cerl:c_atom(prefix_module(Mod)),
  C_name = call_name(Name),
  C_args = lists:map(fun expression/1, Args),
  cerl:c_call(C_module, C_name, C_args);

expression(#gleam_ast_assignment{name = Name, value = Value, then = Then}) ->
  C_var = cerl:c_var(Name),
  C_value = expression(Value),
  C_then = expression(Then),
  cerl:c_let([C_var], C_value, C_then);

expression(Expressions) when is_list(Expressions) ->
  Rev = lists:reverse(Expressions),
  [Head | Tail] = lists:map(fun expression/1, Rev),
  lists:foldl(fun cerl:c_seq/2, Head, Tail).

c_list(Elems) ->
  Rev = lists:reverse(Elems),
  lists:foldl(fun cerl:c_cons/2, cerl:c_nil(), Rev).

binary_string_byte(Char) ->
  cerl:c_bitstr(cerl:c_int(Char),
                cerl:c_int(8),
                cerl:c_int(1),
                cerl:c_atom(integer),
                c_list([cerl:c_atom(unsigned), cerl:c_atom(big)])).

call_name('/') -> cerl:c_atom('div');
call_name('+.') -> cerl:c_atom('+');
call_name('-.') -> cerl:c_atom('-');
call_name('*.') -> cerl:c_atom('*');
call_name('/.') -> cerl:c_atom('/');
call_name('<=') -> cerl:c_atom('=<');
call_name(Name) when is_atom(Name) -> cerl:c_atom(Name).

prefix_module(erlang) -> erlang;
prefix_module(Name) when is_atom(Name) -> list_to_atom("Gleam." ++ atom_to_list(Name)).
