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

-include("gen_fun.hrl").

-export_type([t/2]).

-type t(_F, _A) :: any().

-callback fold_map(fun((A) -> monoid:m(M)), foldable:t(T, A), T) -> monoid:m(M).
%% API
-export([fold_map/3]).

-gen_fun(#{args => [?MODULE], functions => [fold_map/2]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec fold_map(fun((A) -> monoid:m(M)), foldable:t(T, A), T) -> monoid:m(M).
fold_map(F, UA, UFoldable) ->
    undetermined:map(
      fun(Foldable, TA) -> 
              typeclass_trans:apply(fold_map, [F, TA], Foldable, ?MODULE)
      end, UA, UFoldable).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
