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

-callback get(M) -> monad:m(M, _S) when M :: monad:class().
-callback put(_S, M)  -> monad:m(M, ok) when M :: monad:class().
-callback state(fun((S) -> {A, S}), M) -> monad:m(M, A) when M :: monad:class().

-include("do.hrl").
-include("gen_fun.hrl").
-compile({no_auto_import, [get/0, get/1, put/1, put/2]}).

-include("functor.hrl").
-include("applicative.hrl").
-include("monad.hrl").

-export([get/1, put/2, state/2]).
-export([gets/2, modify/2]).
-export([default_get/1, default_put/2, default_state/2]).

-gen_fun(#{args => [?MODULE], functions => [get/0, put/1, state/1]}).
-gen_fun(#{args => [?MODULE], functions => [gets/1, modify/1]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec get(M) -> monad:m(M, _S) when M :: monad:class().
get(UMonadState) ->
    undetermined:new(
      fun(MonadState) -> 
              typeclass_trans:apply(get, [], MonadState, ?MODULE)
      end, UMonadState).

-spec put(_S, M)  -> monad:m(M, ok)  when M :: monad:class().
put(S, UMonadState) ->
    undetermined:new(
      fun(MonadState) -> 
              typeclass_trans:apply(put, [S], MonadState, ?MODULE)
      end, UMonadState).

-spec state(fun((S) -> {A, S}), M) -> monad:m(M, A) when M :: monad:class().
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

-spec gets(fun((_S) -> T), M) -> monad:m(M, T) when M :: monad:class().
gets(F, MonadState) ->
    functor:fmap(F, get(MonadState), MonadState).

-spec modify(fun((_S) -> _T), M) -> monad:m(M, _A) when M :: monad:class().
modify(F, MonadState) ->
    state(fun(S) -> {ok, F(S)} end, MonadState).
%%%===================================================================
%%% Internal functions
%%%===================================================================
