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

-callback mzero(M) -> monad:m(M, _A) when M :: monad:class().
-callback mplus(monad:m(M, A), monad:m(M, A), M) -> monad:m(M, A) when M :: monad:class().

-include("do.hrl").
-include("gen_fun.hrl").
-include("functor.hrl").
-include("applicative.hrl").
-include("monad.hrl").

-compile({parse_transform, cut}).

-export([mzero/1, mplus/3]).
-export([guard/2, msum/2, mfilter/3]).

-gen_fun(#{args => [?MODULE], functions => [mzero/0, mplus/2]}).
-gen_fun(#{args => [?MODULE], functions => [guard/1, msum/1, mfilter/2]}).

-spec mzero(M) -> monad:m(M, _A) when M :: monad:class().
mzero(UMonadPlus) ->
    undetermined:new(
      fun(MonadPlus) ->
              typeclass_trans:apply(mzero, [], MonadPlus, ?MODULE)
      end, UMonadPlus).

-spec mplus(monad:m(M, A), monad:m(M, A), M) -> monad:m(M, A) when M :: monad:class().
mplus(UA, UB, UMonadPlus) ->
    undetermined:map_pair(
      fun(MonadPlus, MA, MB) ->
              typeclass_trans:apply(mplus, [MA, MB], MonadPlus, ?MODULE)
      end, UA, UB, UMonadPlus).

%% Utility functions
-spec guard(boolean(), M) -> monad:m(M, _A) when M :: monad:class().
guard(true, MonadPlus)  -> monad:return(ok, MonadPlus);
guard(false, MonadPlus) -> mzero(MonadPlus).

-spec msum([monad:m(M, A)], M) -> monad:m(M, A) when M :: monad:class().
msum(List, MonadPlus) ->
    lists:foldr(mplus(_, _, MonadPlus), mzero(MonadPlus), List).

-spec mfilter(fun( (A) -> boolean() ), monad:m(M, A), M) -> monad:m(M, A) when M :: monad:class().
mfilter(Pred, X, MonadPlus) ->
    do([MonadPlus || A <- X, guard(Pred(A), MonadPlus)]).
