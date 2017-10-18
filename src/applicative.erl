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
-export([pure/1, '<*>'/2, lift_a2/3, '<*'/2, '*>'/2]).
-export(['default_<*>'/3, default_lift_a2/4, 'default_<*'/3, 'default_*>'/3]).
-export([ap/2, '<**>'/2, lift_a3/4]).

-type applicative_module() :: {module(), applicative_module()} | module().
-type f(_A) :: any().
-type applicative(_F, _A) :: any().

-callback pure(A) -> f(A).
-callback '<*>'(applicative(F, fun((A) -> B)), applicative(F, A)) -> applicative(F, B).
-callback lift_a2(fun((A, B) -> C), applicative(F, A), applicative(F, B)) -> applicative(F, C).
-callback '*>'(applicative(F, _A), applicative(F, B)) -> applicative(F, B).
-callback '<*'(applicative(F, A), applicative(F, _B)) -> applicative(F, A).

%%%===================================================================
%%% API
%%%===================================================================
-spec pure(A) -> applicative:applicative(_F, A).
pure(A) ->
    undetermined:new(fun(M) -> M:pure(A) end).

-spec '<*>'(applicative(F, fun((A) -> B)), applicative(F, A)) -> applicative(F, B).
'<*>'(UF, UA) ->
    undetermined:map_pair(
      fun(Module, AF, AA) ->
              Module:'<*>'(AF, AA)
      end, UF, UA).

lift_a2(F, UA, UB) ->
    undetermined:map_pair(
      fun(Module, AA, AB) ->
              Module:lift_a2(F, AA, AB)
      end, UA, UB).

-spec '*>'(applicative(F, _A), applicative(F, B)) -> applicative(F, B).
'*>'(UA, UB) ->
    undetermined:map_pair(
      fun(Module, AA, AB) ->
              Module:'*>'(AA, AB)
      end, UA, UB).

-spec '<*'(applicative(F, A), applicative(F, _B)) -> applicative(F, A).
'<*'(UA, UB) ->
    undetermined:map_pair(
      fun(Module, AA, AB) ->
              Module:'<*'(AA, AB)
      end, UA, UB).

'default_<*>'(AF, AA, Module) ->
    FA = 
        fun(F, A) ->
                F(A)
        end,
    Module:lift_a2(FA, AF, AA).

-spec default_lift_a2(fun((A, B) -> C), applicative(F, A), applicative(F, B), module()) -> applicative(F, C).
default_lift_a2(F, AA, AB, Module) ->
    NF = 
        fun(A) ->
                fun(B) ->
                        F(A, B)
                end
        end,
    Module:'<*>'(Module:fmap(NF, AA), AB).

-spec 'default_*>'(applicative(F, _A), applicative(F, B), module()) -> applicative(F, B).
'default_*>'(AA, AB, Module) ->
    ConstId = 
        fun(_A) ->
                fun(B) -> B end
        end,
    Module:'<*>'(Module:fmap(ConstId, AA), AB).

-spec 'default_<*'(applicative(F, A), applicative(F, _B), module()) -> applicative(F, A).
'default_<*'(AA, AB, Module) ->
    Const = 
        fun(A) -> 
                fun(_B) -> A end
        end,
    Module:'<*>'(Module:fmap(Const, AA), AB).

-spec 'ap'(applicative(F, fun((A) -> B)), applicative(F, A)) -> applicative(F, B).
ap(AF, A) ->
    '<*>'(AF, A).

-spec '<**>'(applicative(F, A), applicative(F, fun((A) -> B))) -> applicative(F, B).
'<**>'(AA, AF) ->
    AF /'<*>'/ AA.

-spec lift_a3(fun((A, B, C) -> D), applicative(F, A), applicative(F, B), applicative(F, C)) -> applicative(F, D).
lift_a3(F, AA, AB, AC) ->
    NF = 
        fun(A) ->
                fun(B) ->
                        fun(C) -> F(A, B, C) end
                end
        end,
    (((NF /'<$>'/ AA) /'<*>'/ AB) /'<*>'/ AC).
