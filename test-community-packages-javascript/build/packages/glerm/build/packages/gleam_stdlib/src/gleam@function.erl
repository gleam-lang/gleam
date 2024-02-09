-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((I) -> J), fun((J) -> K)) -> fun((I) -> K).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((L, M) -> N)) -> fun((L) -> fun((M) -> N)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((P, Q, R) -> S)) -> fun((P) -> fun((Q) -> fun((R) -> S))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((U, V, W, X) -> Y)) -> fun((U) -> fun((V) -> fun((W) -> fun((X) -> Y)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((AA, AB, AC, AD, AE) -> AF)) -> fun((AA) -> fun((AB) -> fun((AC) -> fun((AD) -> fun((AE) -> AF))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((AH, AI, AJ, AK, AL, AM) -> AN)) -> fun((AH) -> fun((AI) -> fun((AJ) -> fun((AK) -> fun((AL) -> fun((AM) -> AN)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((AP, AQ) -> AR)) -> fun((AQ, AP) -> AR).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(AS) -> AS.
identity(X) ->
    X.

-spec constant(AT) -> fun((any()) -> AT).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(AV, fun((AV) -> any())) -> AV.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((AX) -> AY), AX) -> AY.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((AZ, BA) -> BB), AZ, BA) -> BB.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((BC, BD, BE) -> BF), BC, BD, BE) -> BF.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
