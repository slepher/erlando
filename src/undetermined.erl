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
-export([new/1, new/2, type/2, run/2, map0/3, map/3, map_pair/3, map_pair/4]).
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

type({?MODULE, _R}, Typeclass) ->
    Typeclass;
type(A, Typeclass) ->
    case typeclass:is_typeclass(Typeclass) of
        true ->
            type:type(A);
        false ->
            Typeclass
    end.

run(UA, Typeclass) ->
    map0(
      fun(_Type, MA) ->
              MA
      end, UA, Typeclass).

map0(F, {?MODULE, UF} = UA, Typeclass) ->
    case typeclass:is_typeclass(Typeclass) of
        true ->
            F(Typeclass, UA);
        false ->
            F(Typeclass, UF(Typeclass))
    end;
map0(F, A, Typeclass) ->
    map(F, A, Typeclass).

map(F, {?MODULE, UF}, Typeclass) ->
    case typeclass:is_typeclass(Typeclass) of
        true ->
            new(
              fun(Module) ->
                      F(Module, UF(Module))
              end);
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
