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

-export_type([foldable/2]).

-type foldable(_F, _A) :: any().

-callback fold_map(fun((A) -> monoid:monoid(M)), foldable(_F, A)) -> monoid:monoid(M).
%% API
-export([fold_map/2]).

%%%===================================================================
%%% API
%%%===================================================================
fold_map(F, TA) ->
    undetermined:new(fun(Module) -> Module:foldmap(F, TA) end).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
