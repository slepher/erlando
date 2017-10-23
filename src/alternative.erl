%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(alternative).

-compile({parse_transform, cut}).
%% API
-export([empty/0, '<|>'/2]).

-callback empty() -> applicative:applicative(_F, _A).
-callback '<|>'(applicative:applicative(F, A), 
                applicative:applicative(F, A)) -> applicative:applicative(F, A).

%%%===================================================================
%%% API
%%%===================================================================

empty() ->
    undetermined:new(fun(Module) -> Module:empty() end).

'<|>'(UA, UB) ->
    undetermined:map_pair(
      fun(Module, AA, AB) ->
              Module:'<|>'(AA, AB)
      end, UA, UB, ?MODULE).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
