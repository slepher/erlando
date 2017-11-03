%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monoid).

-superclass([]).

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

mappend(UA, UB) ->
    undetermined:map_pair(
      fun(Module, MA, MB) ->
              Module:mappend(MA, MB)
      end, UA, UB, ?MODULE).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
