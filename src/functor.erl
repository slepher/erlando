%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(functor).

-export_type([functor/2]).
%% API
-export([fmap/2, '<$>'/2]).

-type functor(_F, _A) :: any().

-callback fmap(fun((A) -> B), functor(F, A)) -> functor(F, B).
%%%===================================================================
%%% API
%%%===================================================================

-spec fmap(fun((A) -> B), functor(F, A)) -> functor(F, B).
fmap(F, UA) ->
    undetermined:map(
      fun(Module, FA) ->
              Module:fmap(F, FA)
      end, UA).

-spec '<$>'(fun((A) -> B), functor(F, A)) -> functor(F, B).
'<$>'(F, FA) ->
    fmap(F, FA).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
