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

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_reader).
-behaviour(monad_state).

-export([new/1, error_t/1, run_error_t/1]).
-export([fmap/2]).
-export(['<*>'/2, pure/1]).
-export(['>>='/2, return/1]).
-export([lift/1]).
-export([fail/1]).
-export([ask/0, reader/1, local/2]).
-export([get/0, put/1, state/1]).
-export([run_error/1, map_error/2, with_error/2]).

%% ---------------------------------------------------------------------------------------
%%
%% depricated below
%%
%% ---------------------------------------------------------------------------------------
-export([fmap/3]).
-export(['>>='/3, return/2, fail/2, lift/2]).
-export([run_error/2, map_error/3, with_error/3]).
-export([run/2]).

-opaque error_t(E, M, A) :: {error_t, inner_error_t(E, M, A)}.

-type inner_error_t(E, M, A) :: monad:monadic(M, error_m:error_m(E, A)).

-type t(M) :: monad_trans:monad_trans(?MODULE, M).

-spec new(M) -> t(M) when M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec error_t(inner_error_t(E, M, A)) -> error_t(E, M, A).
error_t(Inner) ->
    {?MODULE, Inner}.

-spec run_error_t(error_t(E, M, A)) -> inner_error_t(E, M, A).
run_error_t({?MODULE, Inner}) ->
    Inner;
run_error_t({undetermined, _} = UT) ->
    run_error_t(undetermined:run(UT, ?MODULE));
run_error_t(Other) ->
    exit({invalid_error_t, Other}).

-spec fmap(fun((A) -> B), error_t(E, M, A)) -> error_t(E, M, B).
fmap(F, ETA) ->
    map_error(
      fun(MA) ->
              functor:fmap(fun(A) -> error_instance:fmap(F, A) end, MA)
      end, ETA).

-spec '<*>'(error_t(E, M, fun((A) -> B)), error_t(E, M, A)) -> error_t(E, M, B).
'<*>'(ETF, ETA) ->
    error_t(
      do([monad || 
             EF <- run_error_t(ETF),
             error_instance:'>>='(EF, fun(F) -> functor:fmap(error_instance:fmap(F, _), run_error_t(ETA)) end)
         ])).

-spec pure(A) -> error_t(_E, _M, A).
pure(A) ->
    return(A).

-spec '>>='(error_t(E, M, A), fun( (A) -> error_t(E, M, B) )) -> error_t(E, M, B).
'>>='(X, Fun) ->
    error_t(
      do([monad || R <- run_error_t(X),
              case R of
                  {error, _Err} = Error -> return(Error);
                  {ok,  Result}         -> run_error_t(Fun(Result));
                  ok                    -> run_error_t(Fun(ok))
              end
       ])).

-spec return(A) -> error_t(_E, _M, A).
return(A) -> error_t(monad:return(error_instance:return(A))).

-spec lift(monad:monadic(M, A)) -> error_t(_E, M, A).
lift(X) ->
    error_t(functor:fmap(error_instance:return(_), X)).

-spec fail(E) -> error_t(E, _M, _A).
fail(E) ->
    error_t(monad:return({error, E})).

-spec ask() -> error_t(_E, _M, _A).
ask() ->
    lift(monad_reader:ask()).

-spec reader(fun((_R) -> A)) -> error_t(_E, _M, A).
reader(F) ->
    lift(monad_reader:reader(F)).

-spec local(fun((R) -> R), error_t(E, M, A)) -> error_t(E, M, A).
local(F, ETA) ->
    map_error(
      fun(MA) ->
              monad_reader:local(F, MA)
      end, ETA).

-spec get() -> error_t(_E, _M, _A).
get() ->
    lift(monad_state:get()).

-spec put(_S) -> error_t(_E, _M, ok).
put(S) ->
    lift(monad_state:put(S)).

-spec state(fun((S) -> {A, S})) -> error_t(_E, _M, A).
state(F) ->
    lift(monad_state:state(F)).

-spec run_error(error_t(E, M, A)) -> monad:monadic(M, error_m:error_m(E, A)).
run_error(EM) -> 
    run_error_t(EM).

-spec map_error(fun((monad:monadic(M, error_m:error_m(EA, A))) -> monad:monadic(N, error_m:error_m(EB, B))),
                error_t(EA, M, A)) -> error_t(EB, N, B).
map_error(F, X) ->
    error_t(F(run_error_t(X))).

-spec with_error(fun((EA) -> EB), error_t(EA, M, A)) -> error_t(EB, M, A).
with_error(F, X) ->
    map_error(
      fun(MA) ->
              functor:fmap(fun({error, R}) -> {error, F(R)}; (Val) -> Val end, MA)
      end, X).

%% ---------------------------------------------------------------------------------------
%%
%% old transform funcitons below
%%
%% ---------------------------------------------------------------------------------------
fmap(F, X, {?MODULE, IM}) ->
    map_error(
      fun(EIM) ->
              do([IM ||
                     A <- EIM,
                     return(error_instance:fmap(F, A))
                 ])
      end, X).

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
return(A, {?MODULE, IM}) -> error_t(runtime_do:return(error_instance:return(A), IM)).

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
    error_t(runtime_do:return(error_instance:fail(E), IM)).

-spec lift(monad:monadic(M, A), M) -> error_t(_E, M, A).
lift(X, {?MODULE, IM}) ->
    error_t(
      do([ IM ||
             A <- X,
             return(error_instance:return(A))
         ])).

-spec run_error(error_t(E, M, A), M) -> monad:monadic(M, error_m:error_m(E, A)).
run_error(EM, {?MODULE, _IM}) ->
    run_error(EM).

run(EM, ET) ->
    run_error(EM, ET).

-spec map_error(fun((monad:monadic(M, error_m:error_m(EA, A))) -> monad:monadic(N, error_m:error_m(EB, B))),
                error_t(EA, M, A), t(M)) -> error_t(EB, N, B).
map_error(F, ETA, {?MODULE, _IM}) ->
    map_error(F, ETA).

-spec with_error(fun((EA) -> EB), error_t(EA, M, A), t(M)) -> error_t(EB, M, A).
with_error(F, X, {?MODULE, IM}) ->
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
      end, X).
