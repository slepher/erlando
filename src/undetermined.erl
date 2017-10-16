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
-export([undetermined/1, run/2]).
-export([map_undetermined/2]).
%%%===================================================================
%%% API
%%%===================================================================
undetermined(Inner) ->
    {?MODULE, Inner}.

run({?MODULE, R}, TypeModule) ->
    R(TypeModule);
run(A, _TypeModule) ->
    A.

map_undetermined(F, {undetermined, FI}) ->
    undetermined(
      fun(Module) ->
              F(Module, FI(Module))
      end);
map_undetermined(F, M) ->
    Module = typeclass:module(undefined, M),
    F(Module, M).
