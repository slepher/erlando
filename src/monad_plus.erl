%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_plus).

-superclass([monad]).

-include("functor.hrl").
-include("applicative.hrl").
-include("monad.hrl").

-callback mzero() -> monad:monadic(_M, _A).
-callback mplus(monad:monadic(M, A), monad:monadic(M, A)) -> monad:monadic(M, A).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-compile({parse_transform, monad_t_transform}).

-export([mzero/1, mplus/3]).
-export([guard/2, msum/2, mfilter/3]).

-transform({?MODULE, [?MODULE], [mzero/0, mplus/2]}).
-transform({?MODULE, [?MODULE], [guard/1, msum/1, mfilter/2]}).

mzero(UMonadPlus) ->
    undetermined:new(
      fun(MonadPlus) ->
              typeclass_trans:apply(mzero, [], MonadPlus)
      end, UMonadPlus).

mplus(UA, UB, UMonadPlus) ->
    undetermined:map_pair(
      fun(MonadPlus, MA, MB) ->
              typeclass_trans:apply(mplus, [MA, MB], MonadPlus)
      end, UA, UB, UMonadPlus).

%% Utility functions
-spec guard(boolean(), M) -> monad:monadic(M, _A).
guard(true, MonadPlus)  -> monad:return(ok, MonadPlus);
guard(false, MonadPlus) -> mzero(MonadPlus).

-spec msum([monad:monadic(M, A)], M) -> monad:monadic(M, A).
msum(List, MonadPlus) ->
    lists:foldr(mplus(_, _, MonadPlus), mzero(MonadPlus), List).

-spec mfilter(fun( (A) -> boolean() ), monad:monadic(M, A), M) -> monad:monadic(M, A).
mfilter(Pred, X, MonadPlus) ->
    do([MonadPlus || A <- X, guard(Pred(A), MonadPlus)]).
