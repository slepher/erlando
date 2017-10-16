%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(alternative).

%% API
-export([empty/0, '<|>'/2]).

-callback empty() -> applicative:applicative(_F, _A).
-callback '<|>'(applicative:applicative(F, A), 
                applicative:applicative(F, A)) -> applicative:applicative(F, A).

%%%===================================================================
%%% API
%%%===================================================================

empty() ->
    undetermined:undetermined(fun(Module) -> Module:empty() end).

'<|>'({undetermined, _} = UA, UB) ->
    undetermined:map_undetermined(
      fun(Module, AB) ->
              AA = undetermined:run(UA, Module),
              Module:'<|>'(AA, AB)
      end, UB);
'<|>'(UA, UB) ->
    undetermined:map_undetermined(
      fun(Module, AA) ->
              AB = undetermined:run(UB, Module),
              Module:'<|>'(AA, AB)
      end, UA).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
