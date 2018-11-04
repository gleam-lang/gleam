-module(gleam_codegen_test).
-include_lib("eunit/include/eunit.hrl").
-define(refute(Value), ?assert(not Value)).

% Used in tests that pass a module to functions
-export([inc/1, dec/1]).

inc(X) -> X + 1.
dec(X) -> X - 1.

with_module(Name, Source, Fun) ->
  StrName = atom_to_list(Name),
  FileName = StrName ++ ".beam",
  "gleam_" ++ ModName = StrName,
  BeamBinary = gleam_compiler:source_to_binary(Source, ModName, [{gen_tests, true}]),
  {module, Name} = code:load_binary(Name, FileName, BeamBinary),
  ?assertEqual({file, FileName}, code:is_loaded(Name)),
  try Fun() of
    X -> X
  after
    code:purge(Name),
    ?assert(code:delete(Name))
  end.

module_identity_test() ->
  Source =
    "pub fn identity(x) { x }\n"
    "fn hidden(x) { x }\n"
  ,
  with_module(gleam_codegen_module_identity, Source, fun() ->
    ?assert(erlang:function_exported(gleam_codegen_module_identity, identity, 1)),
    ?refute(erlang:function_exported(gleam_codegen_module_identity, hidden, 1)),
    ?assertEqual(ok, gleam_codegen_module_identity:identity(ok)),
    ?assertEqual(42, gleam_codegen_module_identity:identity(42)),
    ?assert(erlang:function_exported(gleam_codegen_module_identity, module_info, 0)),
    ?assert(erlang:function_exported(gleam_codegen_module_identity, module_info, 1)),
    Info = gleam_codegen_module_identity:module_info(),
    ?assertMatch([{module, gleam_codegen_module_identity} | _], Info)
  end).

maths_test() ->
  Source =
    "pub fn int_add(x, y) { x + y }\n"
    "pub fn int_sub(x, y) { x - y }\n"
    "pub fn int_mult(x, y) { x * y }\n"
    "pub fn int_div(x, y) { x / y }\n"
    "pub fn float_add(x, y) { x +. y }\n"
    "pub fn float_sub(x, y) { x -. y }\n"
    "pub fn float_mult(x, y) { x *. y }\n"
    "pub fn float_div(x, y) { x /. y }\n"
  ,
  with_module(gleam_codegen_maths, Source, fun() ->
    ?assertEqual(2, gleam_codegen_maths:int_add(1, 1)),
    ?assertEqual(9, gleam_codegen_maths:int_sub(10, 1)),
    ?assertEqual(8, gleam_codegen_maths:int_mult(4, 2)),
    ?assertEqual(4, gleam_codegen_maths:int_div(9, 2)),
    ?assertEqual(5.1, gleam_codegen_maths:float_add(4.0, 1.1)),
    ?assertEqual(8.4, gleam_codegen_maths:float_sub(10.0, 1.6)),
    ?assertEqual(7.5, gleam_codegen_maths:float_mult(3.0, 2.5)),
    ?assertEqual(5.0, gleam_codegen_maths:float_div(10.0, 2.0))
  end).

comparison_test() ->
  Source =
    "pub fn lt(x, y) { x < y }\n"
    "pub fn lte(x, y) { x <= y }\n"
    "pub fn gt(x, y) { x > y }\n"
    "pub fn gte(x, y) { x >= y }\n"
  ,
  with_module(gleam_codegen_comparison, Source, fun() ->
    ?assert(gleam_codegen_comparison:lt(1, 2)),
    ?refute(gleam_codegen_comparison:lt(1, 1)),
    ?refute(gleam_codegen_comparison:lt(2, 1)),
    ?assert(gleam_codegen_comparison:lte(1, 2)),
    ?assert(gleam_codegen_comparison:lte(1, 1)),
    ?refute(gleam_codegen_comparison:lte(2, 1)),
    ?refute(gleam_codegen_comparison:gt(1, 2)),
    ?assert(gleam_codegen_comparison:gt(1, 1)),
    ?assert(gleam_codegen_comparison:gt(2, 1)),
    ?refute(gleam_codegen_comparison:gte(1, 2)),
    ?refute(gleam_codegen_comparison:gte(1, 1)),
    ?assert(gleam_codegen_comparison:gte(2, 1))
  end).

int_test() ->
  Source =
    "pub fn one() { 1 }\n"
    "pub fn two() { 2 }\n"
    "pub fn inc(x) { x + 1 }\n"
    "pub fn negative() { -1 }\n"
    "pub fn positive() { +10 }\n"
  ,
  with_module(gleam_codegen_int, Source, fun() ->
    ?assertEqual(1, gleam_codegen_int:one()),
    ?assertEqual(2, gleam_codegen_int:two()),
    ?assertEqual(2, gleam_codegen_int:inc(1)),
    ?assertEqual(-1, gleam_codegen_int:negative()),
    ?assertEqual(10, gleam_codegen_int:positive())
  end).

float_test() ->
  Source =
    "pub fn one() { 1.0 }\n"
    "pub fn two() { 2.0 }\n"
    "pub fn inc(x) { x +. 1.0 }\n"
    "pub fn negative() { -1.0 }\n"
    "pub fn positive() { +10.0 }\n"
  ,
  with_module(gleam_codegen_float, Source, fun() ->
    ?assertEqual(1.0, gleam_codegen_float:one()),
    ?assertEqual(2.0, gleam_codegen_float:two()),
    ?assertEqual(2.0, gleam_codegen_float:inc(1.0)),
    ?assertEqual(-1.0, gleam_codegen_float:negative()),
    ?assertEqual(10.0, gleam_codegen_float:positive())
  end).

string_test() ->
  Source =
    "pub fn empty() { \"\" }\n"
    "pub fn name() { \"Louis\" }\n"
  ,
  with_module(gleam_codegen_string, Source, fun() ->
    ?assertEqual(<<>>, gleam_codegen_string:empty()),
    ?assertEqual(<<"Louis">>, gleam_codegen_string:name())
  end).

atom_test() ->
  Source =
    "pub fn one() { 'one' }\n"
    "pub fn caps() { 'CAPS' }\n"
    "pub fn etc() { 'Hello, world!' }\n"
  ,
  with_module(gleam_codegen_atom, Source, fun() ->
    ?assertEqual(one, gleam_codegen_atom:one()),
    ?assertEqual('CAPS', gleam_codegen_atom:caps()),
    ?assertEqual('Hello, world!', gleam_codegen_atom:etc())
  end).

tuple_test() ->
  Source =
    "pub fn ok(x) { {'ok', x} }\n"
    "pub fn threeple() { {1, 2, 3} }\n"
  ,
  with_module(gleam_codegen_tuple, Source, fun() ->
    ?assertEqual({ok, 1}, gleam_codegen_tuple:ok(1)),
    ?assertEqual({1, 2, 3}, gleam_codegen_tuple:threeple())
  end).

list_test() ->
  Source =
    "pub fn empty() { [] }\n"
    "pub fn one() { [1] }\n"
    "pub fn two() { [1, 2] }\n"
    "pub fn cons(head, tail) { head :: tail }\n"
    "pub fn unsugared_list() { 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: [] }\n"
  ,
  with_module(gleam_codegen_list, Source, fun() ->
    ?assertEqual([], gleam_codegen_list:empty()),
    ?assertEqual([1], gleam_codegen_list:one()),
    ?assertEqual([1, 2], gleam_codegen_list:two()),
    ?assertEqual([1, 2, 3], gleam_codegen_list:cons(1, [2, 3])),
    ?assertEqual([1, 2, 3, 4, 5, 6], gleam_codegen_list:unsugared_list())
  end).

call_test() ->
  Source =
    "fn ok(x) { {'ok', x} }\n"
    "fn add(x, y) { x + y }\n"
    "pub fn double(x) { ok(add(x, x)) }\n"
  ,
  Mod = gleam_codegen_call,
  with_module(Mod, Source, fun() ->
    ?assertEqual({ok, 10}, Mod:double(5))
  end).

call_local_test() ->
  Source =
    "pub fn call_this(x) { x() }\n"
    "pub fn call_internal() { x = fn() { 1 } x() }\n"
  ,
  Mod = gleam_codegen_call_local,
  with_module(Mod, Source, fun() ->
    ?assertEqual(1, Mod:call_internal()),
    ?assertEqual(2, Mod:call_this(fun() -> 2 end))
  end).


seq_test() ->
  Source =
    "pub fn main() { 1 2 3 }\n"
  ,
  with_module(gleam_codegen_seq, Source, fun() ->
    ?assertEqual(3, gleam_codegen_seq:main())
  end).

assignment_test() ->
  Source =
    "pub fn go() { x = 100 y = x + 1 'unused' z = x + y 'unused' z }\n"
    "pub fn reassign() { x = 1 x = 2 x }\n"
  ,
  with_module(gleam_codegen_assignment, Source, fun() ->
    ?assertEqual(201, gleam_codegen_assignment:go()),
    ?assertEqual(2, gleam_codegen_assignment:reassign())
  end).

pattern_assignment_test() ->
  Source =
    "pub enum Box(a) = | Box(a)\n"
    "pub fn go(tup) { {a, b} = tup a + b }\n"
    "pub fn unbox(box) { Box(x) = box x }\n"
  ,
  Mod = gleam_codegen_pattern_assignment,
  with_module(Mod, Source, fun() ->
    ?assertEqual(15, Mod:go({10, 5})),
    ?assertEqual(1, Mod:unbox({box, 1}))
  end).

bool_enum_test() ->
  Source =
    "pub fn true() { True }\n"
    "pub fn false() { False }\n"
  ,
  with_module(gleam_codegen_bool_enum, Source, fun() ->
    ?assertEqual(true, gleam_codegen_bool_enum:true()),
    ?assertEqual(false, gleam_codegen_bool_enum:false())
  end).

word_case_enum_test() ->
  Source =
    "pub enum X = | SomeLongName | ADT\n"
    "pub fn one() { SomeLongName }\n"
    "pub fn two() { ADT }\n"
  ,
  with_module(gleam_codegen_word_case_enum, Source, fun() ->
    ?assertEqual(some_long_name, gleam_codegen_word_case_enum:one()),
    ?assertEqual(a_d_t, gleam_codegen_word_case_enum:two())
  end).

product_enum_test() ->
  Source =
    "pub enum Ok(a) = | Ok(a)\n"
    "pub fn ok(x) { Ok(x) }\n"
  ,
  with_module(gleam_codegen_product_enum, Source, fun() ->
    ?assertEqual({ok, "Hi there"}, gleam_codegen_product_enum:ok("Hi there"))
  end).

case_int_test() ->
  Source =
    "pub fn go(x) {\n"
    "  case x {\n"
    "  | 1 -> 'one'\n"
    "  | 2 -> 'two'\n"
    "  | 3 -> 'three'\n"
    "  | _ -> 'other'\n"
    "  }\n"
    "}\n"
  ,
  with_module(gleam_codegen_case_int, Source, fun() ->
    ?assertEqual(one, gleam_codegen_case_int:go(1)),
    ?assertEqual(two, gleam_codegen_case_int:go(2)),
    ?assertEqual(three, gleam_codegen_case_int:go(3)),
    ?assertEqual(other, gleam_codegen_case_int:go(4))
  end).

case_float_test() ->
  Source =
    "pub fn go(x) {\n"
    "  case x {\n"
    "  | 1.0 -> 'one'\n"
    "  | 2.0 -> 'two'\n"
    "  | 3.0 -> 'three'\n"
    "  | _ -> 'other'\n"
    "  }\n"
    "}\n"
  ,
  with_module(gleam_codegen_case_float, Source, fun() ->
    ?assertEqual(one, gleam_codegen_case_float:go(1.0)),
    ?assertEqual(two, gleam_codegen_case_float:go(2.0)),
    ?assertEqual(three, gleam_codegen_case_float:go(3.0)),
    ?assertEqual(other, gleam_codegen_case_float:go(4.0))
  end).

case_string_test() ->
  Source =
    "pub fn go(x) {\n"
    "  case x {\n"
    "  | \"\" -> 'empty'\n"
    "  | _ -> 'non_empty'\n"
    "  }\n"
    "}\n"
  ,
  with_module(gleam_codegen_case_string, Source, fun() ->
    ?assertEqual(empty, gleam_codegen_case_string:go(<<"">>)),
    ?assertEqual(non_empty, gleam_codegen_case_string:go(<<"h">>))
  end).

case_list_test() ->
  Source =
    "pub fn length(x) {\n"
    "  case x {\n"
    "  | [] -> 0\n"
    "  | [[]] -> 1\n"
    "  | [_, _] -> 2\n"
    "  | _ -> -1\n"
    "  }\n"
    "}\n"
  ,
  with_module(gleam_codegen_case_list, Source, fun() ->
    ?assertEqual(0, gleam_codegen_case_list:length([])),
    ?assertEqual(1, gleam_codegen_case_list:length([[]])),
    ?assertEqual(2, gleam_codegen_case_list:length([[], []])),
    ?assertEqual(-1, gleam_codegen_case_list:length([[], [], []]))
  end).

case_cons_test() ->
  Source =
    "pub fn head(x) {\n"
    "  case x {\n"
    "  | x :: _ -> x\n"
    "  | _ -> 0\n"
    "  }\n"
    "}\n"
  ,
  with_module(gleam_codegen_case_cons, Source, fun() ->
    ?assertEqual(0, gleam_codegen_case_cons:head([])),
    ?assertEqual(a, gleam_codegen_case_cons:head([a])),
    ?assertEqual(1, gleam_codegen_case_cons:head([1])),
    ?assertEqual(1, gleam_codegen_case_cons:head([1, 2]))
  end).

case_tuple_test() ->
  Source =
    "pub fn go(x) {\n"
    "  case x {\n"
    "  | {'ok', {1, 1}} -> 'one'\n"
    "  | {'ok', {2, 2}} -> 'two'\n"
    "  | {_, _} -> 'eh'\n"
    "  }\n"
    "}\n"
  ,
  with_module(gleam_codegen_case_tuple, Source, fun() ->
    ?assertEqual(one, gleam_codegen_case_tuple:go({ok, {1, 1}})),
    ?assertEqual(two, gleam_codegen_case_tuple:go({ok, {2, 2}})),
    ?assertEqual(eh, gleam_codegen_case_tuple:go({ok, 3}))
  end).

case_var_test() ->
  Source =
    "pub fn unwrap(x) {\n"
    "  case x {\n"
    "  | {'ok', thing} -> thing\n"
    "  | _ -> 'default'\n"
    "  }\n"
    "}\n"
  ,
  with_module(gleam_codegen_case_var, Source, fun() ->
    ?assertEqual(one, gleam_codegen_case_var:unwrap({ok, one})),
    ?assertEqual(default, gleam_codegen_case_var:unwrap(two))
  end).

case_enum_test() ->
  Source =
    "pub enum Maybe(a) = | Nothing | Just(a)\n"
    "pub fn unwrap(x) {\n"
    "  case x {\n"
    "  | Nothing -> 'default'\n"
    "  | Just(z) -> z\n"
    "  }\n"
    "}\n"
  ,
  with_module(gleam_codegen_case_enum, Source, fun() ->
    ?assertEqual(one, gleam_codegen_case_enum:unwrap({just, one})),
    ?assertEqual(default, gleam_codegen_case_enum:unwrap(nothing))
  end).

record_test() ->
  Source =
    "pub fn zero() { {} }\n"
    "pub fn one(x) { { value = x } }\n"
    "pub fn two(x) { { val1 = x, val2 = x } }\n"
  ,
  with_module(gleam_codegen_record, Source, fun() ->
    ?assertEqual(#{}, gleam_codegen_record:zero()),
    ?assertEqual(#{value => 1}, gleam_codegen_record:one(1)),
    ?assertEqual(#{value => 2}, gleam_codegen_record:one(2)),
    ?assertEqual(#{val1 => ok, val2 => ok}, gleam_codegen_record:two(ok))
  end).

record_extend_test() ->
  Source =
    "pub fn add_name(r) { { r | name = \"Sara\" } }\n"
    "pub fn silly() { { { { {} | a = 1 } | a = 2 } | b = 3 } }\n"
  ,
  Mod = gleam_codegen_record,
  with_module(Mod, Source, fun() ->
    ?assertEqual(#{name => <<"Sara">>}, Mod:add_name(#{})),
    ?assertEqual(#{cool => true, name => <<"Sara">>}, Mod:add_name(#{cool => true})),
    ?assertEqual(#{a => 2, b => 3}, Mod:silly())
  end).

record_access_test() ->
  Source =
    "pub fn name(x) { x.name }\n"
    "pub fn dig(x) { x.one.two.three }\n"
  ,
  Mod = gleam_codegen_record_access,
  with_module(Mod, Source, fun() ->
    ?assertEqual(1, Mod:name(#{name => 1})),
    ?assertEqual(ok, Mod:dig(#{one => #{two => #{three => ok}}}))
  end).

zero_arity_call_test() ->
  Source =
    "fn hidden() { 100 }\n"
    "pub fn one() { hidden() }\n"
  ,
  Mod = gleam_codegen_zero_arity_call,
  with_module(Mod, Source, fun() ->
    ?assertEqual(100, Mod:one())
  end).

fn_test() ->
  Source =
    "pub fn id_fun() { fn(x) { x } }\n"
    "fn double(x) { x + x }\n"
    "pub fn double_fun() { double(_) }\n"
    "pub fn close_over(x) { fn() { x } }\n"
  ,
  Mod = gleam_codegen_closure,
  with_module(Mod, Source, fun() ->
    Identity = Mod:id_fun(),
    ?assertEqual(1, Identity(1)),
    Double = Mod:double_fun(),
    ?assertEqual(8, Double(4)),
    ClosedOver = Mod:close_over(50),
    ?assertEqual(50, ClosedOver())
  end).

fn_call_test() ->
  Source =
    "pub fn call(fun) { fun.() }\n"
  ,
  Mod = gleam_codegen_closure_call,
  with_module(Mod, Source, fun() ->
    ?assertEqual(ok, Mod:call(fun() -> ok end))
  end).

pipe_test() ->
  Source =
    "fn add(x, y) { x + y }\n"
    "fn inc(x) { x + 1 }\n"
    "pub fn incer() { inc(_) }\n"
    "pub fn go(x) { x |> inc(_) |> add(_, 10) |> add(_, 5) }\n"
  ,
  Mod = gleam_codegen_pipe,
  with_module(Mod, Source, fun() ->
    Incer = Mod:incer(),
    ?assertEqual(2, Incer(1)),
    ?assertEqual(17, Mod:go(1))
  end).

curried_test() ->
  Source =
    "fn f(x) { fn(y) { {x, y} } }\n"
    "pub fn anon(f) { f.(1)(2) }\n"
    "pub fn named() { f(1)(2) }\n"
  ,
  Mod = gleam_codegen_curry,
  with_module(Mod, Source, fun() ->
    ?assertEqual(Mod:anon(fun(X) -> fun(Y) -> {X, Y} end end), {1, 2}),
    ?assertEqual(Mod:named(), {1, 2})
  end).

test_test() ->
  Source =
    "test thing { 'ok' }\n"
  ,
  Mod = gleam_codegen_pipe,
  with_module(Mod, Source, fun() ->
    ?assertEqual(ok, Mod:test()),
    ?assertEqual(ok, Mod:thing_test())
  end).

raise_test() ->
  Source =
    "pub fn go(x) { raise(x) }\n"
  ,
  Mod = gleam_codegen_raise,
  with_module(Mod, Source, fun() ->
    ?assertEqual(caught, try Mod:go(1) catch error:1 -> caught end),
    ?assertEqual(caught, try Mod:go(2) catch error:2 -> caught end)
  end).

throw_test() ->
  Source =
    "pub fn go(x) { throw(x) }\n"
  ,
  Mod = gleam_codegen_throw,
  with_module(Mod, Source, fun() ->
    ?assertEqual(caught, try Mod:go(1) catch throw:1 -> caught end),
    ?assertEqual(caught, try Mod:go(2) catch throw:2 -> caught end)
  end).

eq_test() ->
  Source =
    "pub fn eq(x, y) { x == y }\n"
    "pub fn neq(x, y) { x != y }\n"
  ,
  Mod = gleam_codegen_eq,
  with_module(Mod, Source, fun() ->
    ?assertEqual(true, Mod:eq(one, one)),
    ?assertEqual(false, Mod:eq(two, one)),
    ?assertEqual(true, Mod:eq(1, 1)),
    ?assertEqual(false, Mod:eq(2, 1)),
    ?assertEqual(false, Mod:neq(one, one)),
    ?assertEqual(true, Mod:neq(two, one)),
    ?assertEqual(false, Mod:neq(1, 1)),
    ?assertEqual(true, Mod:neq(2, 1))
  end).

external_fn_test() ->
  Source =
    "pub external fn first(String) -> Int = 'binary' 'first'\n"
  ,
  Mod = gleam_codegen_external_fn,
  with_module(Mod, Source, fun() ->
    ?assertEqual(97, Mod:first(<<"abc">>)),
    ?assertEqual(98, Mod:first(<<"bcd">>))
  end).

external_type_test() ->
  Source =
    "pub external type Thing\n"
    "pub fn id(x) { x }\n"
  ,
  Mod = gleam_codegen_external_type,
  with_module(Mod, Source, fun() ->
    ?assertEqual(1, Mod:id(1))
  end).

module_term_call_test() ->
  Source =
    "pub fn call_inc(mod) { mod:inc(1) }\n"
    "pub fn call_dec(mod) { mod:dec(1) }\n"
  ,
  Mod = gleam_module_term_call,
  with_module(Mod, Source, fun() ->
    ?assertEqual(2, Mod:call_inc(gleam_codegen_test)),
    ?assertEqual(0, Mod:call_dec(gleam_codegen_test))
  end).

module_import_test() ->
  % TODO: We cheat and use an Erlang module here atm. Need to actually create
  % it in gleam so it can be typed.
  Source =
    "import codegen_test_module\n"
    "pub fn go() { codegen_test_module:one() }\n"
  ,
  Mod = gleam_module_term_call,
  with_module(Mod, Source, fun() ->
    ?assertEqual(1, Mod:go())
  end).
