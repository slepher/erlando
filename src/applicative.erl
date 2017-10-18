%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(applicative).

-include("op.hrl").

-export_type([applicative_module/0, f/1, applicative/2]).

%% API
-export(['<*>'/2, ap/2, pure/1]).
-export(['<*'/2, '*>'/2]).

-type applicative_module() :: module().
-type f(_A) :: any().
-type applicative(_F, _A) :: any().

-callback ap(applicative(F, fun((A) -> B)), applicative(F, A)) -> applicative(F, B).
-callback pure(A) -> f(A).

%%%===================================================================
%%% API
%%%===================================================================

-spec ap(applicative(F, fun((A) -> B)), applicative(F, A)) -> applicative(F, B).
ap({undetermined, _} = UF, UA) ->
    undetermined:map(
      fun(Module, AA) ->
              AF = undetermined:run(UF, Module),
              Module:ap(AF, AA)
      end, UA);
ap(UF, UA) ->
    undetermined:map(
      fun(Module, AF) ->
              AA = undetermined:run(UA, Module),
              Module:ap(AF, AA)
      end, UF).

-spec '<*>'(applicative(F, fun((A) -> B)), applicative(F, A)) -> applicative(F, B).
'<*>'(AF, A) ->
    ap(AF, A).

-spec '*>'(applicative(F, _A), applicative(F, B)) -> applicative(F, B).
'*>'(AA, AB) ->
    ConstId = 
        fun(_A) ->
                fun(B) -> B end
        end,
    (ConstId /'<$>'/ AA) /'<*>'/ AB.

-spec '<*'(applicative(F, A), applicative(F, _B)) -> applicative(F, A).
'<*'(AA, AB) ->
    Const = 
        fun(A) -> 
                fun(_B) -> A end
        end,
    (Const /'<$>'/ AA) /'<*>'/ AB.

-spec pure(A) -> applicative:applicative(_F, A).
pure(A) ->
    undetermined:new(fun(M) -> M:pure(A) end).
