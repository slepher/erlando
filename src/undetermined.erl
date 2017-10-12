%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(undetermined).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_reader).
-behaviour(monad_state).

%% API
-export([undetermined/1, run/2]).
-export([wrap/1, unwrap/1]).
-export([fmap/2]).
-export(['<*>'/2, pure/1]).
-export(['>>='/2, return/1]).
-export([lift/1]).

-export([ask/0, reader/1, local/2]).
-export([get/0, put/1, state/1]).

-export([map_undetermined/2]).
%%%===================================================================
%%% API
%%%===================================================================
undetermined(Inner) ->
    {?MODULE, Inner}.

run({?MODULE, {identity, R}}, _TypeModule) ->
    R;
run({?MODULE, R}, TypeModule) ->
    R(TypeModule).

wrap({?MODULE, _} = Undetermined) ->
    Undetermined;
wrap(M) ->
    {?MODULE, {identity, M}}.

unwrap({?MODULE, {identity, M}}) ->
    M;
unwrap(Undetermiend) ->
    Undetermiend.

fmap(F, UTF) ->
    map_undetermined(
      fun(Module, Functor) ->
              Module:fmap(F, Functor)
      end, UTF).

'<*>'({?MODULE, {identity, _}} = UTF, UTA) ->
    map_undetermined(
      fun(Module, AF) ->
              Module:'<*>'(AF, run(UTA, Module))
      end, UTF);
'<*>'(UTF, UTA) ->
    map_undetermined(
      fun(Module, UA) ->
              Module:'<*>'(run(UTF, Module), UA)
      end, UTA).

pure(A) ->
    undetermined(fun(Module) -> Module:pure(A) end).

'>>='(UTA, UTK) ->
    map_undetermined(
      fun(Module, MA) ->
              Module:'>>='(MA, fun(A) -> run(UTK(A), Module) end)
      end, UTA).

return(A) ->
    undetermined(fun(Module) -> Module:return(A) end).

lift(M) ->
    undetermined(fun(Module) -> Module:lift(M) end).

ask() ->
    undetermined(fun(Module) -> Module:ask() end).

reader(F) ->
    undetermined(fun(Module) -> Module:reader(F) end).

local(F, UTM) ->
    map_undetermined(
      fun(Module, M) ->
              Module:local(F, M)
      end, UTM).

get() ->
    undetermined(fun(Module) -> Module:get() end).

put(S) ->
    undetermined(fun(Module) -> Module:put(S) end).

state(F) ->
    undetermined(fun(Module) -> Module:state(F) end).

map_undetermined(F, {undetermined, {identity, M}}) ->
    Module = typeclass:module(undefined, M),
    F(Module, M);
map_undetermined(F, {undetermined, FI}) ->
    undetermined(
      fun(Module) ->
              F(Module, FI(Module))
      end).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
