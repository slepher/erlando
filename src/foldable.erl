%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(foldable).

-superclass([]).

-compile({parse_transform, monad_t_transform}).

-export_type([foldable/2]).

-type foldable(_F, _A) :: any().

-callback fold_map(fun((A) -> monoid:monoid(M)), foldable(F, A), F) -> monoid:monoid(M).
%% API
-export([fold_map/3]).

-transform(#{args => [?MODULE], functions => [fold_map/2]}).

%%%===================================================================
%%% API
%%%===================================================================
fold_map(F, UA, UFoldable) ->
    undetermined:map(
      fun(Foldable, TA) -> 
              typeclass_trans:apply(foldmap, [F, TA], Foldable, ?MODULE)
      end, UA, UFoldable).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
