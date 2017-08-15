%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 11 Aug 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(error_t).
-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-export_type([error_t/3]).

-behaviour(monad_trans).

-export([new/1, error_t/1, run_error_t/1]).
-export([fmap/3]).
-export(['>>='/3, return/2, fail/2, lift/2]).
-export([run_error/2, map_error/3, with_error/3]).
-export([run/2]).

-opaque error_t(E, M, A) :: {error_t, inner_error_t(E, M, A)}.

-type inner_error_t(E, M, A) :: monad:monadic(M, error_m:error_m(E, A)).

-type t(M) :: {error_t, M}.

-spec new(M) -> t(M) when M :: monad:monad().
new(M) ->
    {?MODULE, M}.

error_t(Inner) ->
    {?MODULE, Inner}.

run_error_t({?MODULE, Inner}) ->
    Inner;
run_error_t(Other) ->
    exit({invalid_error_t, Other}).

fmap(F, X, {?MODULE, IM} = ET) ->
    with_error(
      fun(EIM) ->
              monad:fmap(IM, error_m:fmap(F, _), EIM)
      end, X, ET).

-spec '>>='(error_t(E, M, A), fun( (A) -> error_t(E, M, B) ), t(M)) -> error_t(E, M, B).
'>>='(X, Fun, {?MODULE, IM}) ->
    error_t(
      do([IM || R <- run_error_t(X),
              case R of
                  {error, _Err} = Error -> return(Error);
                  {ok,  Result}         -> run_error_t(Fun(Result));
                  ok                    -> run_error_t(Fun(ok))
              end
       ])).

-spec return(A, t(M)) -> error_t(_E, M, A).
return(A, {?MODULE, IM}) -> error_t(monad:return(IM, error_m:return(A))).

%% This is the equivalent of
%%     fail msg = ErrorT $ return (Left (strMsg msg))
%% from the instance (Monad m, Error e) => Monad (ErrorT e m)
%%
%% http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/src/Control-Monad-Error.html#ErrorT
%%
%% I.e. note that calling fail on the outer monad is not a failure of
%% the inner monad: it is success of the inner monad, but the failure
%% is encapsulated.
-spec fail(E, t(M)) -> error_t(E, M, _A).
fail(E, {?MODULE, IM}) ->
    error_t(monad:return(IM, {error, E})).

-spec lift(monad:monadic(M, A), M) -> error_t(_E, M, A).
lift(X, {?MODULE, IM}) ->
    error_t(monad:fmap(IM, error_m:return(_), X)).

-spec run_error(error_t(E, M, A), M) -> monad:monadic(M, error_m:error_m(E, A)).
run_error(EM, {?MODULE, _IM}) -> 
    run_error_t(EM).

run(EM, ET) ->
    run_error(EM, ET).

-spec map_error(fun((monad:monadic(M, error_m:error_m(EA, A))) -> monad:monadic(N, error_m:error_m(EB, B))),
                error_t(EA, M, A), t(M)) -> error_t(EB, N, B).
map_error(F, X, {?MODULE, _IM}) ->
    error_t(F(run_error_t(X))).

-spec with_error(fun((EA) -> EB), error_t(EA, M, A), t(M)) -> error_t(EB, M, A).
with_error(F, X, {?MODULE, IM} = ET) ->
    map_error(
      fun(MA) ->
              do([IM || 
                     Val <- MA,
                     case Val of
                         {error, Reason} ->
                             return({error, F(Reason)});
                         Val ->
                             return(Val)
                     end
                 ])
      end, X, ET).
    
