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
-export([new/1, run/2, map/2, map/3, map_pair/3, map_pair/4]).
%%%===================================================================
%%% API
%%%===================================================================
new(Inner) ->
    {?MODULE, Inner}.

run({?MODULE, R}, TypeModule) ->
    R(TypeModule);
run(A, _TypeModule) ->
    A.

map(F, Undetermined) ->
    map(F, Undetermined, undefined).

map(F, {undetermined, FI}, _TypeClass) ->
    new(
      fun(Module) ->
              F(Module, FI(Module))
      end);
map(F, M, Typeclass) ->
    Module = type:module(Typeclass, M),
    F(Module, M).

map_pair(F, UA, UB) ->
    map_pair(F, UA, UB, undefined).

map_pair(F, {undetermined, _} = UA, UB, Typeclass) ->
    undetermined:map(
      fun(Module, B) ->
              A = run(UA, Module),
              F(Module, A, B)
      end, UB, Typeclass);
map_pair(F, UA, UB, Typeclass) ->
    undetermined:map(
      fun(Module, A) ->
              B = run(UB, Module),
              F(Module, A, B)
      end, UA, Typeclass).
