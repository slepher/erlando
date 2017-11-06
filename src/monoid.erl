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

-compile({parse_transform, monad_t_transform}).

-export_type([monoid/1]).

-type monoid(_M) :: any().

-callback mempty(M) -> monoid(M).
-callback mappend(monoid(M), monoid(M), M) -> monoid(M).

%% API
-export([mempty/1, mappend/3]).

-transform({?MODULE, [?MODULE], [mempty/0, mappend/2]}).
%%%===================================================================
%%% API
%%%===================================================================
mempty(UMonoid) ->
    undetermined:new(
      fun(Monoid) ->
              typeclass_trans:apply(mempty, [], Monoid, ?MODULE)
      end, UMonoid).

mappend(UA, UB, UMonoid) ->
    undetermined:map_pair(
      fun(Monoid, MA, MB) ->
              typeclass_trans:apply(mappend, [MA, MB], Monoid, ?MODULE)
      end, UA, UB, UMonoid).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
