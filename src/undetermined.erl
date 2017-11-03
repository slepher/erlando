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
-export([new/1, new/2, run/2, map/2, map/3, map_pair/3, map_pair/4]).
%%%===================================================================
%%% API
%%%===================================================================
new(Inner) ->
    {?MODULE, Inner}.

new(F, Typeclass) ->
    case typeclass:is_typeclass(Typeclass) of
        true ->
            {?MODULE, F};
        false ->
            F(Typeclass)
    end.

run({?MODULE, R}, Typeclass) ->
    case typeclass:is_typeclass(Typeclass) of
        true ->
            {?MODULE, R};
        false ->
            R(Typeclass)
    end;
run(A, _TypeModule) ->
    A.

map(F, Undetermined) ->
    map(F, Undetermined, undefined).

map(F, {?MODULE, FI}, Typeclass) ->
    case typeclass:is_typeclass(Typeclass) of
        true ->
            new(
              fun(Module) ->
                      F(Module, FI(Module))
              end);
        false ->
            F(Typeclass, FI(Typeclass))
    end;
map(F, M, Typeclass) ->
    case typeclass:is_typeclass(Typeclass) of
        true ->
            Module = type:module(Typeclass, M),
            F(Module, M);
        false ->
            F(Typeclass, M)
    end.

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
