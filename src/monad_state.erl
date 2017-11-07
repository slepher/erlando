%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_state).

-superclass([monad]).

-callback get(M) -> monad:monadic(M, _S)  when M :: monad:monad().
-callback put(_S, M)  -> monad:monadic(M, ok)  when M :: monad:monad().
-callback state(fun((S) -> {A, S}), M) -> monad:monadic(M, A)  when M :: monad:monad().

-compile({parse_transform, do}).
-compile({parse_transform, monad_t_transform}).
-compile({no_auto_import, [get/0, get/1, put/1, put/2]}).

-include("functor.hrl").
-include("applicative.hrl").
-include("monad.hrl").

-export([get/1, put/2, state/2]).
-export([gets/2, modify/2]).
-export([default_get/1, default_put/2, default_state/2]).

-transform(#{args => [?MODULE], functions => [get/0, put/1, state/1]}).
-transform(#{args => [?MODULE], functions => [gets/1, modify/1]}).

%%%===================================================================
%%% API
%%%===================================================================
get(UMonadState) ->
    undetermined:new(
      fun(MonadState) -> 
              typeclass_trans:apply(get, [], MonadState, ?MODULE)
      end, UMonadState).

put(S, UMonadState) ->
    undetermined:new(
      fun(MonadState) -> 
              typeclass_trans:apply(put, [S], MonadState, ?MODULE)
      end, UMonadState).

state(F, UMonadState) ->
    undetermined:new(
      fun(MonadState) ->
              typeclass_trans:apply(state, [F], MonadState, ?MODULE)
      end, UMonadState).

default_get(MonadState) ->
    state(fun(S) -> {S, S} end, MonadState).

default_put(S, MonadState) ->
    state(fun(_) -> {ok, S} end, MonadState).

default_state(F, MonadState) ->
    do([MonadState ||
           S <- get(MonadState),
           {A, NS} = (F(S)),
           put(NS, MonadState),
           return(A)
       ]).

gets(F, MonadState) ->
    functor:fmap(F, get(MonadState), MonadState).

modify(F, MonadState) ->
    state(fun(S) -> {ok, F(S)} end, MonadState).
%%%===================================================================
%%% Internal functions
%%%===================================================================
