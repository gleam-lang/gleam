-module(order_test).
-compile(no_auto_import).

-export([reverse_test/0, to_int_test/0, compare_test/0, max_test/0, min_test/0]).

reverse_test() ->
    expect:equal(order:reverse(lt), gt),
    expect:equal(order:reverse(eq), eq),
    expect:equal(order:reverse(gt), lt).

to_int_test() ->
    expect:equal(order:to_int(lt), -1),
    expect:equal(order:to_int(eq), 0),
    expect:equal(order:to_int(gt), 1).

compare_test() ->
    expect:equal(order:compare(lt, lt), eq),
    expect:equal(order:compare(lt, eq), lt),
    expect:equal(order:compare(lt, gt), lt),
    expect:equal(order:compare(eq, lt), gt),
    expect:equal(order:compare(eq, eq), eq),
    expect:equal(order:compare(eq, gt), lt),
    expect:equal(order:compare(gt, lt), gt),
    expect:equal(order:compare(gt, eq), gt),
    expect:equal(order:compare(gt, gt), eq).

max_test() ->
    expect:equal(order:max(lt, lt), lt),
    expect:equal(order:max(lt, eq), eq),
    expect:equal(order:max(lt, gt), gt),
    expect:equal(order:max(eq, lt), eq),
    expect:equal(order:max(eq, eq), eq),
    expect:equal(order:max(eq, gt), gt),
    expect:equal(order:max(gt, lt), gt),
    expect:equal(order:max(gt, eq), gt),
    expect:equal(order:max(gt, gt), gt).

min_test() ->
    expect:equal(order:min(lt, lt), lt),
    expect:equal(order:min(lt, eq), lt),
    expect:equal(order:min(lt, gt), lt),
    expect:equal(order:min(eq, lt), lt),
    expect:equal(order:min(eq, eq), eq),
    expect:equal(order:min(eq, gt), eq),
    expect:equal(order:min(gt, lt), lt),
    expect:equal(order:min(gt, eq), eq),
    expect:equal(order:min(gt, gt), gt).
