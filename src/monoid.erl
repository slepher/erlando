%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monoid).
-export_type([monoid/1]).

-type monoid(_M) :: any().

-callback mempty() -> monoid(_M).
-callback mappend(monoid(M), monoid(M)) -> monoid(M).

%% API
-export([mempty/0, mappend/2]).

%%%===================================================================
%%% API
%%%===================================================================

mempty() ->
    undetermined:new(fun(Module) -> Module:mempty() end).

mappend({undetermined, _} = UA, UB) ->
    undetermined:map(
      fun(Module, MB) ->
              MA = undetermined:run(UA, Module),
              Module:mappend(MA, MB)
      end, UB);
mappend(UA, UB) ->
    undetermined:map(
      fun(Module, MA) ->
              MB = undetermined:run(UB, Module),
              Module:mappend(MA, MB)
      end, UA).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
