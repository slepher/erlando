%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(undetermined).

-include("erlando.hrl").

%% API
-export([new/2, type/2, run/2, map0/3, map/3, map_pair/3, map_pair/4]).
%%%===================================================================
%%% API
%%%===================================================================
new(F, Typeclass) ->
    case typeclass:is_typeclass(Typeclass) of
        true ->
            #undetermined{typeclass = Typeclass, inner_function = F};
        false ->
            F(Typeclass)
    end.

type(UA, Typeclass) ->
    map0(
      fun(Type, _MA) ->
              Type
      end, UA, Typeclass).

run(UA, Typeclass) ->
    map0(
      fun(_Type, MA) ->
              MA
      end, UA, Typeclass).

map0(F, #undetermined{inner_function = UF} = UA, Typeclass) ->
    case typeclass:is_typeclass(Typeclass) of
        true ->
            F(Typeclass, UA);
        false ->
            F(Typeclass, UF(Typeclass))
    end;
map0(F, A, Typeclass) ->
    map(F, A, Typeclass).

map(F, #undetermined{inner_function = UF}, Typeclass) ->
    case typeclass:is_typeclass(Typeclass) of
        true ->
            new(
              fun(Module) ->
                      F(Module, UF(Module))
              end, Typeclass);
        false ->
            F(Typeclass, UF(Typeclass))
    end;
map(F, M, Typeclass) ->
    case typeclass:is_typeclass(Typeclass) of
        true ->
            Type = type:type(M),
            F(Type, M);
        false ->
            F(Typeclass, M)
    end.

map_pair(F, UA, UB) ->
    map_pair(F, UA, UB, undefined).

map_pair(F, #undetermined{} = UA, UB, Typeclass) ->
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
