-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/function.gleam", 2).
-spec compose(fun((DPQ) -> DPR), fun((DPR) -> DPS)) -> fun((DPQ) -> DPS).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/function.gleam", 7).
-spec curry2(fun((DPT, DPU) -> DPV)) -> fun((DPT) -> fun((DPU) -> DPV)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/function.gleam", 12).
-spec curry3(fun((DPX, DPY, DPZ) -> DQA)) -> fun((DPX) -> fun((DPY) -> fun((DPZ) -> DQA))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/function.gleam", 17).
-spec curry4(fun((DQC, DQD, DQE, DQF) -> DQG)) -> fun((DQC) -> fun((DQD) -> fun((DQE) -> fun((DQF) -> DQG)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/function.gleam", 22).
-spec curry5(fun((DQI, DQJ, DQK, DQL, DQM) -> DQN)) -> fun((DQI) -> fun((DQJ) -> fun((DQK) -> fun((DQL) -> fun((DQM) -> DQN))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/function.gleam", 27).
-spec curry6(fun((DQP, DQQ, DQR, DQS, DQT, DQU) -> DQV)) -> fun((DQP) -> fun((DQQ) -> fun((DQR) -> fun((DQS) -> fun((DQT) -> fun((DQU) -> DQV)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/function.gleam", 36).
-spec flip(fun((DQX, DQY) -> DQZ)) -> fun((DQY, DQX) -> DQZ).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/function.gleam", 42).
-spec identity(DRA) -> DRA.
identity(X) ->
    X.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/function.gleam", 47).
-spec constant(DRB) -> fun((any()) -> DRB).
constant(Value) ->
    fun(_) -> Value end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/function.gleam", 56).
-spec tap(DRD, fun((DRD) -> any())) -> DRD.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/function.gleam", 62).
-spec apply1(fun((DRF) -> DRG), DRF) -> DRG.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/function.gleam", 67).
-spec apply2(fun((DRH, DRI) -> DRJ), DRH, DRI) -> DRJ.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/function.gleam", 72).
-spec apply3(fun((DRK, DRL, DRM) -> DRN), DRK, DRL, DRM) -> DRN.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
