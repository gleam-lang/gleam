-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((ESR) -> ESS), fun((ESS) -> EST)) -> fun((ESR) -> EST).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((ESU, ESV) -> ESW)) -> fun((ESU) -> fun((ESV) -> ESW)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((ESY, ESZ, ETA) -> ETB)) -> fun((ESY) -> fun((ESZ) -> fun((ETA) -> ETB))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((ETD, ETE, ETF, ETG) -> ETH)) -> fun((ETD) -> fun((ETE) -> fun((ETF) -> fun((ETG) -> ETH)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((ETJ, ETK, ETL, ETM, ETN) -> ETO)) -> fun((ETJ) -> fun((ETK) -> fun((ETL) -> fun((ETM) -> fun((ETN) -> ETO))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((ETQ, ETR, ETS, ETT, ETU, ETV) -> ETW)) -> fun((ETQ) -> fun((ETR) -> fun((ETS) -> fun((ETT) -> fun((ETU) -> fun((ETV) -> ETW)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((ETY, ETZ) -> EUA)) -> fun((ETZ, ETY) -> EUA).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(EUB) -> EUB.
identity(X) ->
    X.

-spec constant(EUC) -> fun((any()) -> EUC).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(EUE, fun((EUE) -> any())) -> EUE.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((EUG) -> EUH), EUG) -> EUH.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((EUI, EUJ) -> EUK), EUI, EUJ) -> EUK.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((EUL, EUM, EUN) -> EUO), EUL, EUM, EUN) -> EUO.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
