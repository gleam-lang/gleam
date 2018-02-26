-module(gleam_codegen_test).
-include_lib("eunit/include/eunit.hrl").
-define(refute(Value), ?assert(not Value)).

with_module(Name, Source, Fun) ->
  FileName = "Gleam." ++ atom_to_list(Name) ++ ".beam",
  BeamBinary = gleam_compiler:source_to_binary(Source),
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
    "module CodegenModuleIdentity\n"
    "export identity/1\n"
    "fn identity(x) = x\n"
    "fn hidden(x) = x\n"
  ,
  with_module('Gleam.CodegenModuleIdentity', Source, fun() ->
    ?assert(erlang:function_exported('Gleam.CodegenModuleIdentity', identity, 1)),
    ?refute(erlang:function_exported('Gleam.CodegenModuleIdentity', hidden, 1)),
    ?assertEqual(ok, 'Gleam.CodegenModuleIdentity':identity(ok)),
    ?assertEqual(42, 'Gleam.CodegenModuleIdentity':identity(42)),
    ?assert(erlang:function_exported('Gleam.CodegenModuleIdentity', module_info, 0)),
    ?assert(erlang:function_exported('Gleam.CodegenModuleIdentity', module_info, 1)),
    Info = 'Gleam.CodegenModuleIdentity':module_info(),
    ?assertMatch([{module, 'Gleam.CodegenModuleIdentity'} | _], Info)
  end).

maths_test() ->
  Source =
    "module CodegenMaths\n"
    "export int_add/2, int_sub/2, int_mult/2, int_div/2, float_add/2,\n"
    "       float_sub/2, float_mult/2, float_div/2\n"
    "fn int_add(x, y) = x + y\n"
    "fn int_sub(x, y) = x - y\n"
    "fn int_mult(x, y) = x * y\n"
    "fn int_div(x, y) = x / y\n"
    "fn float_add(x, y) = x +. y\n"
    "fn float_sub(x, y) = x -. y\n"
    "fn float_mult(x, y) = x *. y\n"
    "fn float_div(x, y) = x /. y\n"
  ,
  with_module('Gleam.CodegenMaths', Source, fun() ->
    ?assertEqual(2, 'Gleam.CodegenMaths':int_add(1, 1)),
    ?assertEqual(9, 'Gleam.CodegenMaths':int_sub(10, 1)),
    ?assertEqual(8, 'Gleam.CodegenMaths':int_mult(4, 2)),
    ?assertEqual(4, 'Gleam.CodegenMaths':int_div(9, 2)),
    ?assertEqual(5.1, 'Gleam.CodegenMaths':float_add(4.0, 1.1)),
    ?assertEqual(8.4, 'Gleam.CodegenMaths':float_sub(10.0, 1.6)),
    ?assertEqual(7.5, 'Gleam.CodegenMaths':float_mult(3.0, 2.5)),
    ?assertEqual(5.0, 'Gleam.CodegenMaths':float_div(10.0, 2.0))
  end).

comparison_test() ->
  Source =
    "module CodegenComparison\n"
    "export lt/2, lte/2, gt/2, gte/2"
    "fn lt(x, y) = x < y\n"
    "fn lte(x, y) = x <= y\n"
    "fn gt(x, y) = x > y\n"
    "fn gte(x, y) = x >= y\n"
  ,
  with_module('Gleam.CodegenComparison', Source, fun() ->
    ?assert('Gleam.CodegenComparison':lt(1, 2)),
    ?refute('Gleam.CodegenComparison':lt(1, 1)),
    ?refute('Gleam.CodegenComparison':lt(2, 1)),
    ?assert('Gleam.CodegenComparison':lte(1, 2)),
    ?assert('Gleam.CodegenComparison':lte(1, 1)),
    ?refute('Gleam.CodegenComparison':lte(2, 1)),
    ?refute('Gleam.CodegenComparison':gt(1, 2)),
    ?assert('Gleam.CodegenComparison':gt(1, 1)),
    ?assert('Gleam.CodegenComparison':gt(2, 1)),
    ?refute('Gleam.CodegenComparison':gte(1, 2)),
    ?refute('Gleam.CodegenComparison':gte(1, 1)),
    ?assert('Gleam.CodegenComparison':gte(2, 1))
  end).

int_test() ->
  Source =
    "module CodegenInt\n"
    "export one/0, two/0, inc/1, negative/0, positive/0"
    "fn one() = 1\n"
    "fn two() = 2\n"
    "fn inc(x) = x + 1\n"
    "fn negative() = -1\n"
    "fn positive() = +10\n"
  ,
  with_module('Gleam.CodegenInt', Source, fun() ->
    ?assertEqual(1, 'Gleam.CodegenInt':one()),
    ?assertEqual(2, 'Gleam.CodegenInt':two()),
    ?assertEqual(2, 'Gleam.CodegenInt':inc(1)),
    ?assertEqual(-1, 'Gleam.CodegenInt':negative()),
    ?assertEqual(10, 'Gleam.CodegenInt':positive())
  end).

float_test() ->
  Source =
    "module CodegenFloat\n"
    "export one/0, two/0, inc/1, negative/0, positive/0"
    "fn one() = 1.0\n"
    "fn two() = 2.0\n"
    "fn inc(x) = x + 1.0\n"
    "fn negative() = -1.0\n"
    "fn positive() = +10.0\n"
  ,
  with_module('Gleam.CodegenFloat', Source, fun() ->
    ?assertEqual(1.0, 'Gleam.CodegenFloat':one()),
    ?assertEqual(2.0, 'Gleam.CodegenFloat':two()),
    ?assertEqual(2.0, 'Gleam.CodegenFloat':inc(1.0)),
    ?assertEqual(-1.0, 'Gleam.CodegenFloat':negative()),
    ?assertEqual(10.0, 'Gleam.CodegenFloat':positive())
  end).

string_test() ->
  Source =
    "module CodegenString\n"
    "export empty/0, name/0"
    "fn empty() = \"\"\n"
    "fn name() = \"Louis\"\n"
  ,
  with_module('Gleam.CodegenString', Source, fun() ->
    ?assertEqual(<<>>, 'Gleam.CodegenString':empty()),
    ?assertEqual(<<"Louis">>, 'Gleam.CodegenString':name())
  end).

atom_test() ->
  Source =
    "module CodegenAtom\n"
    "export one/0, caps/0, etc/0"
    "fn one() = :one\n"
    "fn caps() = :CAPS\n"
    "fn etc() = :\"Hello, world!\"\n"
  ,
  with_module('Gleam.CodegenAtom', Source, fun() ->
    ?assertEqual(one, 'Gleam.CodegenAtom':one()),
    ?assertEqual('CAPS', 'Gleam.CodegenAtom':caps()),
    ?assertEqual('Hello, world!', 'Gleam.CodegenAtom':etc())
  end).

tuple_test() ->
  Source =
    "module CodegenTuple\n"
    "export zero/0, ok/1, threeple/0\n"
    "fn zero() = ()\n"
    "fn ok(x) = (:ok, x)\n"
    "fn threeple() = (1, 2, 3)\n"
  ,
  with_module('Gleam.CodegenTuple', Source, fun() ->
    ?assertEqual({}, 'Gleam.CodegenTuple':zero()),
    ?assertEqual({ok, 1}, 'Gleam.CodegenTuple':ok(1)),
    ?assertEqual({1, 2, 3}, 'Gleam.CodegenTuple':threeple())
  end).

list_test() ->
  Source =
    "module CodegenList\n"
    "export empty/0, one/0, two/0, cons/2, unsugared_list/0\n"
    "fn empty() = []\n"
    "fn one() = [1]\n"
    "fn two() = [1, 2]\n"
    "fn cons(head, tail) = head :: tail\n"
    "fn unsugared_list() = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: []\n"
  ,
  with_module('Gleam.CodegenList', Source, fun() ->
    ?assertEqual([], 'Gleam.CodegenList':empty()),
    ?assertEqual([1], 'Gleam.CodegenList':one()),
    ?assertEqual([1, 2], 'Gleam.CodegenList':two()),
    ?assertEqual([1, 2, 3], 'Gleam.CodegenList':cons(1, [2, 3])),
    ?assertEqual([1, 2, 3, 4, 5, 6], 'Gleam.CodegenList':unsugared_list())
  end).

call_test() ->
  Source =
    "module CodegenCall\n"
    "export double/1\n"
    "fn double(x) = ok(add(x, x))\n"
    "fn add(x, y) = x + y\n"
    "fn ok(x) = (:ok, x)\n"
  ,
  with_module('Gleam.CodegenCall', Source, fun() ->
    ?assertEqual({ok, 10}, 'Gleam.CodegenCall':double(5))
  end).

seq_test() ->
  Source =
    "module CodegenSeq\n"
    "export main/0\n"
    "fn main() = 1 2 3\n"
  ,
  with_module('Gleam.CodegenSeq', Source, fun() ->
    ?assertEqual(3, 'Gleam.CodegenSeq':main())
  end).

assignment_test() ->
  Source =
    "module CodegenAssignment\n"
    "export go/0, reassign/0\n"
    "fn go() = x = 100 y = x + 1 :unused z = x + y :unused z\n"
    "fn reassign() = x = 1 x = 2 x\n"
  ,
  with_module('Gleam.CodegenAssignment', Source, fun() ->
    ?assertEqual(201, 'Gleam.CodegenAssignment':go()),
    ?assertEqual(2, 'Gleam.CodegenAssignment':reassign())
  end).

bool_adt_test() ->
  Source =
    "module CodegenBoolAdt\n"
    "export true/0, false/0\n"
    "fn true() = True\n"
    "fn false() = False\n"
  ,
  with_module('Gleam.CodegenBoolAdt', Source, fun() ->
    ?assertEqual(true, 'Gleam.CodegenBoolAdt':true()),
    ?assertEqual(false, 'Gleam.CodegenBoolAdt':false())
  end).

word_case_adt_test() ->
  Source =
    "module CodegenWordCaseAdt\n"
    "export one/0, two/0\n"
    "fn one() = SomeLongName\n"
    "fn two() = ADT\n"
  ,
  with_module('Gleam.CodegenWordCaseAdt', Source, fun() ->
    ?assertEqual(some_long_name, 'Gleam.CodegenWordCaseAdt':one()),
    ?assertEqual(a_d_t, 'Gleam.CodegenWordCaseAdt':two())
  end).

product_adt_test() ->
  Source =
    "module CodegenProductAdt\n"
    "export ok/1\n"
    "fn ok(x) = Ok(x)\n"
  ,
  with_module('Gleam.CodegenProductAdt', Source, fun() ->
    ?assertEqual({ok, "Hi there"}, 'Gleam.CodegenProductAdt':ok("Hi there"))
  end).

case_int_test() ->
  Source =
    "module CodegenCaseInt\n"
    "export go/1\n"
    "fn go(x) =\n"
    "  case x\n"
    "  | 1 => :one\n"
    "  | 2 => :two\n"
    "  | 3 => :three\n"
    "  | _ => :other\n"
  ,
  with_module('Gleam.CodegenCaseInt', Source, fun() ->
    ?assertEqual(one, 'Gleam.CodegenCaseInt':go(1)),
    ?assertEqual(two, 'Gleam.CodegenCaseInt':go(2)),
    ?assertEqual(three, 'Gleam.CodegenCaseInt':go(3)),
    ?assertEqual(other, 'Gleam.CodegenCaseInt':go(4))
  end).

case_float_test() ->
  Source =
    "module CodegenCaseFloat\n"
    "export go/1\n"
    "fn go(x) =\n"
    "  case x\n"
    "  | 1.0 => :one\n"
    "  | 2.0 => :two\n"
    "  | 3.0 => :three\n"
    "  | _ => :other\n"
  ,
  with_module('Gleam.CodegenCaseFloat', Source, fun() ->
    ?assertEqual(one, 'Gleam.CodegenCaseFloat':go(1.0)),
    ?assertEqual(two, 'Gleam.CodegenCaseFloat':go(2.0)),
    ?assertEqual(three, 'Gleam.CodegenCaseFloat':go(3.0)),
    ?assertEqual(other, 'Gleam.CodegenCaseFloat':go(4.0))
  end).

case_string_test() ->
  Source =
    "module CodegenCaseString\n"
    "export go/1\n"
    "fn go(x) =\n"
    "  case x\n"
    "  | \"\" => :empty\n"
    "  | _ => :non_empty\n"
  ,
  with_module('Gleam.CodegenCaseString', Source, fun() ->
    ?assertEqual(empty, 'Gleam.CodegenCaseString':go(<<"">>)),
    ?assertEqual(non_empty, 'Gleam.CodegenCaseString':go(<<"h">>))
  end).

case_list_test() ->
  Source =
    "module CodegenCaseList\n"
    "export length/1\n"
    "fn length(x) =\n"
    "  case x\n"
    "  | [] => 0\n"
    "  | [[]] => 1\n"
    "  | [_, _] => 2\n"
    "  | _ => -1\n"
  ,
  with_module('Gleam.CodegenCaseList', Source, fun() ->
    ?assertEqual(0, 'Gleam.CodegenCaseList':length([])),
    ?assertEqual(1, 'Gleam.CodegenCaseList':length([[]])),
    ?assertEqual(2, 'Gleam.CodegenCaseList':length([[], []])),
    ?assertEqual(-1, 'Gleam.CodegenCaseList':length([[], [], []]))
  end).

case_cons_test() ->
  Source =
    "module CodegenCaseCons\n"
    "export head/1\n"
    "fn head(x) =\n"
    "  case x\n"
    "  | x :: _ => Just(x)\n"
    "  | _ => Nothing\n"
  ,
  with_module('Gleam.CodegenCaseCons', Source, fun() ->
    ?assertEqual(nothing, 'Gleam.CodegenCaseCons':head([])),
    ?assertEqual({just, 0}, 'Gleam.CodegenCaseCons':head([0])),
    ?assertEqual({just, 1}, 'Gleam.CodegenCaseCons':head([1, 2]))
  end).

case_tuple_test() ->
  Source =
    "module CodegenCaseTuple\n"
    "export go/1\n"
    "fn go(x) =\n"
    "  case x\n"
    "  | (:ok, (1, 1)) => :one\n"
    "  | (:ok, (2, 2)) => :two\n"
    "  | (_, _) => :eh\n"
  ,
  with_module('Gleam.CodegenCaseTuple', Source, fun() ->
    ?assertEqual(one, 'Gleam.CodegenCaseTuple':go({ok, {1, 1}})),
    ?assertEqual(two, 'Gleam.CodegenCaseTuple':go({ok, {2, 2}})),
    ?assertEqual(eh, 'Gleam.CodegenCaseTuple':go({ok, 3}))
  end).

case_var_test() ->
  Source =
    "module CodegenCaseVar\n"
    "export unwrap/1\n"
    "fn unwrap(x) =\n"
    "  case x\n"
    "  | (:ok, thing) => thing\n"
    "  | _ => :default\n"
  ,
  with_module('Gleam.CodegenCaseVar', Source, fun() ->
    ?assertEqual(one, 'Gleam.CodegenCaseVar':unwrap({ok, one})),
    ?assertEqual(default, 'Gleam.CodegenCaseVar':unwrap(two))
  end).

case_adt_test() ->
  Source =
    "module CodegenCaseAdt\n"
    "export unwrap/1\n"
    "fn unwrap(x) =\n"
    "  case x\n"
    "  | Nothing => :default\n"
    "  | Just(z) => z\n"
  ,
  with_module('Gleam.CodegenCaseAdt', Source, fun() ->
    ?assertEqual(one, 'Gleam.CodegenCaseAdt':unwrap({just, one})),
    ?assertEqual(default, 'Gleam.CodegenCaseAdt':unwrap(nothing))
  end).

record_test() ->
  Source =
    "module CodegenRecord\n"
    "export zero/0, one/1, two/1\n"
    "fn zero() = {}\n"
    "fn one(x) = {value = x}\n"
    "fn two(x) = {val1 = x, val2 = x}\n"
  ,
  with_module('Gleam.CodegenRecord', Source, fun() ->
    ?assertEqual(#{}, 'Gleam.CodegenRecord':zero()),
    ?assertEqual(#{value => 1}, 'Gleam.CodegenRecord':one(1)),
    ?assertEqual(#{value => 2}, 'Gleam.CodegenRecord':one(2)),
    ?assertEqual(#{val1 => ok, val2 => ok}, 'Gleam.CodegenRecord':two(ok))
  end).

record_access_test() ->
  Source =
    "module CodegenRecordAccess\n"
    "export name/1, dig/1\n"
    "fn name(x) = x.name\n"
    "fn dig(x) = x.one.two.three\n"
  ,
  Mod = 'Gleam.CodegenRecordAccess',
  with_module(Mod, Source, fun() ->
    ?assertEqual(1, Mod:name(#{name => 1})),
    ?assertEqual(ok, Mod:dig(#{one => #{two => #{three => ok}}}))
  end).

zero_arity_call_test() ->
  Source =
    "module CodegenZeroArityCall\n"
    "export one/0\n"
    "fn one() = hidden()\n"
    "fn hidden() = 100\n"
  ,
  Mod = 'Gleam.CodegenZeroArityCall',
  with_module(Mod, Source, fun() ->
    ?assertEqual(100, Mod:one())
  end).

closure_test() ->
  Source =
    "module CodegenClosure\n"
    "export id_fun/0, double_fun/0, close_over/1\n"
    "fn id_fun() = |x| x\n"
    "fn double_fun() = double(_)\n"
    "fn double(x) = x + x\n"
    "fn close_over(x) = || x\n"
  ,
  Mod = 'Gleam.CodegenClosure',
  with_module(Mod, Source, fun() ->
    Identity = Mod:id_fun(),
    ?assertEqual(1, Identity(1)),
    Double = Mod:double_fun(),
    ?assertEqual(8, Double(4)),
    ClosedOver = Mod:close_over(50),
    ?assertEqual(50, ClosedOver())
  end).

closure_call_test() ->
  Source =
    "module CodegenClosureCall\n"
    "export identity/0, call/1\n"
    "fn call(fun) = fun.()\n"
  ,
  Mod = 'Gleam.CodegenClosureCall',
  with_module(Mod, Source, fun() ->
    ?assertEqual(ok, Mod:call(fun() -> ok end))
  end).

% pipe_test() ->
%   Source =
%     "module CodegenPipe\n"
%     "export go/1, incer/0\n"
%     "fn inc(x) = x + 1\n"
%     "fn incer() = inc(_)\n"
%     "fn go(x) = x |> inc(_) |> inc(_)\n"
%   ,
%   Mod = 'Gleam.CodegenPipe',
%   with_module(Mod, Source, fun() ->
%     Incer = Mod:incer(),
%     ?assertEqual(2, Incer(1)),
%     ?assertEqual(3, Mod:go(1))
%   end).
