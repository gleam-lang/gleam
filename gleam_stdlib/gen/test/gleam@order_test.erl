-module(gleam@order_test).
-compile(no_auto_import).

-export([reverse_test/0, to_int_test/0, compare_test/0, max_test/0, min_test/0]).

reverse_test() ->
    gleam@expect:equal(gleam@order:reverse(lt), gt),
    gleam@expect:equal(gleam@order:reverse(eq), eq),
    gleam@expect:equal(gleam@order:reverse(gt), lt).

to_int_test() ->
    gleam@expect:equal(gleam@order:to_int(lt), -1),
    gleam@expect:equal(gleam@order:to_int(eq), 0),
    gleam@expect:equal(gleam@order:to_int(gt), 1).

compare_test() ->
    gleam@expect:equal(gleam@order:compare(lt, lt), eq),
    gleam@expect:equal(gleam@order:compare(lt, eq), lt),
    gleam@expect:equal(gleam@order:compare(lt, gt), lt),
    gleam@expect:equal(gleam@order:compare(eq, lt), gt),
    gleam@expect:equal(gleam@order:compare(eq, eq), eq),
    gleam@expect:equal(gleam@order:compare(eq, gt), lt),
    gleam@expect:equal(gleam@order:compare(gt, lt), gt),
    gleam@expect:equal(gleam@order:compare(gt, eq), gt),
    gleam@expect:equal(gleam@order:compare(gt, gt), eq).

max_test() ->
    gleam@expect:equal(gleam@order:max(lt, lt), lt),
    gleam@expect:equal(gleam@order:max(lt, eq), eq),
    gleam@expect:equal(gleam@order:max(lt, gt), gt),
    gleam@expect:equal(gleam@order:max(eq, lt), eq),
    gleam@expect:equal(gleam@order:max(eq, eq), eq),
    gleam@expect:equal(gleam@order:max(eq, gt), gt),
    gleam@expect:equal(gleam@order:max(gt, lt), gt),
    gleam@expect:equal(gleam@order:max(gt, eq), gt),
    gleam@expect:equal(gleam@order:max(gt, gt), gt).

min_test() ->
    gleam@expect:equal(gleam@order:min(lt, lt), lt),
    gleam@expect:equal(gleam@order:min(lt, eq), lt),
    gleam@expect:equal(gleam@order:min(lt, gt), lt),
    gleam@expect:equal(gleam@order:min(eq, lt), lt),
    gleam@expect:equal(gleam@order:min(eq, eq), eq),
    gleam@expect:equal(gleam@order:min(eq, gt), eq),
    gleam@expect:equal(gleam@order:min(gt, lt), lt),
    gleam@expect:equal(gleam@order:min(gt, eq), eq),
    gleam@expect:equal(gleam@order:min(gt, gt), gt).
