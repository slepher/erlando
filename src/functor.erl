%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(functor).

-include("op.hrl").

-export_type([functor/2]).
%% API
-export([fmap/2, '<$>'/2, '<$'/2]).
-export(['default_<$'/3]).

-type functor(_F, _A) :: any().

-callback fmap(fun((A) -> B), functor(F, A)) -> functor(F, B).
-callback '<$'(B, functor(F, _A)) -> functor(F, B).
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

-spec '<$'(B, functor(F, _A)) -> functor(F, B).
'<$'(F, UA) ->
    undetermined:map(
      fun(Module, FA) ->
              Module:'<$'(F, FA)
      end, UA).

'default_<$'(B, FA, Module) ->
    Module:fmap(function_instance:const(B), FA).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
