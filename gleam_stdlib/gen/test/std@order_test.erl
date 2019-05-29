-module(std@order_test).
-compile(no_auto_import).

-export([reverse_test/0, to_int_test/0, compare_test/0, max_test/0, min_test/0]).

reverse_test() ->
    std@expect:equal(std@order:reverse(lt), gt),
    std@expect:equal(std@order:reverse(eq), eq),
    std@expect:equal(std@order:reverse(gt), lt).

to_int_test() ->
    std@expect:equal(std@order:to_int(lt), -1),
    std@expect:equal(std@order:to_int(eq), 0),
    std@expect:equal(std@order:to_int(gt), 1).

compare_test() ->
    std@expect:equal(std@order:compare(lt, lt), eq),
    std@expect:equal(std@order:compare(lt, eq), lt),
    std@expect:equal(std@order:compare(lt, gt), lt),
    std@expect:equal(std@order:compare(eq, lt), gt),
    std@expect:equal(std@order:compare(eq, eq), eq),
    std@expect:equal(std@order:compare(eq, gt), lt),
    std@expect:equal(std@order:compare(gt, lt), gt),
    std@expect:equal(std@order:compare(gt, eq), gt),
    std@expect:equal(std@order:compare(gt, gt), eq).

max_test() ->
    std@expect:equal(std@order:max(lt, lt), lt),
    std@expect:equal(std@order:max(lt, eq), eq),
    std@expect:equal(std@order:max(lt, gt), gt),
    std@expect:equal(std@order:max(eq, lt), eq),
    std@expect:equal(std@order:max(eq, eq), eq),
    std@expect:equal(std@order:max(eq, gt), gt),
    std@expect:equal(std@order:max(gt, lt), gt),
    std@expect:equal(std@order:max(gt, eq), gt),
    std@expect:equal(std@order:max(gt, gt), gt).

min_test() ->
    std@expect:equal(std@order:min(lt, lt), lt),
    std@expect:equal(std@order:min(lt, eq), lt),
    std@expect:equal(std@order:min(lt, gt), lt),
    std@expect:equal(std@order:min(eq, lt), lt),
    std@expect:equal(std@order:min(eq, eq), eq),
    std@expect:equal(std@order:min(eq, gt), eq),
    std@expect:equal(std@order:min(gt, lt), lt),
    std@expect:equal(std@order:min(gt, eq), eq),
    std@expect:equal(std@order:min(gt, gt), gt).
