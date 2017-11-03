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

-compile({parse_transform, do}).

-export([mzero/0, mplus/2]).
-export([mzero/1, mplus/3]).
-export([guard/1, msum/1, mfilter/2]).

%% MonadPlus primitives
-callback mzero() -> monad:monadic(_M, _A).
-callback mplus(monad:monadic(M, A), monad:monadic(M, A)) -> monad:monadic(M, A).

mzero() ->
    undetermined:new(fun(MPlus) -> mzero(MPlus) end).

mplus(UA, UB) ->
    undetermined:map_pair(
      fun(Module, MA, MB) ->
              Module:mplus(MA, MB)
      end, UA, UB, ?MODULE).

mzero(MPlus) ->
    typeclass_trans:apply(mzero, [], MPlus).

mplus(MA, MB, MonadPlus) ->
    typeclass_trans:apply(mplus, [MA, MB], MonadPlus).

%% Utility functions
-spec guard(boolean()) -> monad:monadic(_M, _A).
guard(true)  -> monad:return(ok);
guard(false) -> mzero().

-spec msum([monad:monadic(M, A)]) -> monad:monadic(M, A).
msum(List) ->
    lists:foldr(fun mplus/2, mzero(), List).

-spec mfilter(fun( (A) -> boolean() ), monad:monadic(M, A)) -> monad:monadic(M, A).
mfilter(Pred, X) ->
    do([monad || A <- X, guard(Pred(A))]).
