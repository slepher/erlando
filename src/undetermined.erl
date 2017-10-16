%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(undetermined).

%% API
-export([new/1, run/2, map/2]).
%%%===================================================================
%%% API
%%%===================================================================
new(Inner) ->
    {?MODULE, Inner}.

run({?MODULE, R}, TypeModule) ->
    R(TypeModule);
run(A, _TypeModule) ->
    A.

map(F, {undetermined, FI}) ->
    new(
      fun(Module) ->
              F(Module, FI(Module))
      end);
map(F, M) ->
    Module = typeclass:module(undefined, M),
    F(Module, M).
