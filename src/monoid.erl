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

-export_type([m/1]).

-type m(_M) :: any().

-callback mempty(M) -> monoid:m(M).
-callback mappend(monoid:m(M), monoid:m(M), M) -> monoid:m(M).

%% API
-export([mempty/1, mappend/3]).

-transform(#{args => [?MODULE], functions => [mempty/0, mappend/2]}).
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
