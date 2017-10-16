%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(applicative).

-export_type([applicative_module/0, f/1, applicative/2]).

%% API
-export(['<*>'/2, ap/2, pure/1]).

-type applicative_module() :: module().
-type f(_A) :: any().
-type applicative(_F, _A) :: any().

-callback '<*>'(applicative(F, fun((A) -> B)), applicative(F, A)) -> applicative(F, B).
-callback pure(A) -> f(A).

%%%===================================================================
%%% API
%%%===================================================================

-spec '<*>'(applicative(F, fun((A) -> B)), applicative(F, A)) -> applicative(F, B).
'<*>'({undetermined, _} = UF, UA) ->
    undetermined:map_undetermined(
      fun(Module, AA) ->
              AF = undetermined:run(UF, Module),
              Module:'<*>'(AF, AA)
      end, UA);
'<*>'(UF, UA) ->
    undetermined:map_undetermined(
      fun(Module, AF) ->
              AA = undetermined:run(UA, Module),
              Module:'<*>'(AF, AA)
      end, UF).

-spec ap(applicative(F, fun((A) -> B)), applicative(F, A)) -> applicative(F, B).
ap(AF, A) ->
    '<*>'(AF, A).

-spec pure(A) -> applicative:applicative(_F, A).
pure(A) ->
    undetermined:undetermined(fun(M) -> M:pure(A) end).

    
