%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 14 Aug 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(maybe_t).
-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-define(MAYBE_T_MONAD, {?MODULE, monad}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_fail).
-behaviour(monad_reader).
-behaviour(monad_state).
-behaviour(monad_runnner).

-export_type([maybe_t/2]).

-opaque maybe_t(M, A) :: {maybe_t, inner_maybe_t(M, A)}.
-type inner_maybe_t(M, A) :: monad:monadic(M, maybe:maybe(A)).
-type t(M) :: monad_trans:monad_trans(?MODULE, M).

%% API
-export([maybe_t/1, run_maybe_t/1]).
% impl of functor.
-export([fmap/2]).
% impl of applicative.
-export(['<*>'/2, pure/1]).
% impl of monad.
-export(['>>='/2, return/1]).
% impl of monad_trans.
-export([lift/1]).
% impl of monad_fail.
-export([fail/1]).
% impl of monad_reader.
-export([ask/0, reader/1, local/2]).
% impl of monad_state.
-export([get/0, put/1, state/1]).
% impl of monad_runner.
-export([run_nargs/0, run/2]).
% maybe_t functions.
-export([run_maybe/1, map_maybe/2]).

%% old monad trans functions
-export([new/1]).
-export([fmap/3]).
-export(['>>='/3, return/2, fail/2]).
-export([lift/2]).
-export([mzero/1, mplus/3]).
-export([run_maybe/2, map_maybe/3]).


-spec maybe_t(inner_maybe_t(M, A)) -> maybe_t(M, A).
maybe_t(Inner) ->
    {?MODULE, Inner}.

-spec run_maybe_t(maybe_t(M, A)) -> inner_maybe_t(M, A).
run_maybe_t({?MODULE, Inner}) ->
    Inner;
run_maybe_t({undetermined, _} = U) ->
    run_maybe_t(undetermined:run(U, maybe_t));
run_maybe_t(Other) ->
    exit({invalid_maybe_t, Other}).

-spec fmap(fun((A) -> B), maybe_t(M, A)) -> maybe_t(M, B).
fmap(F, MTA) ->
    map_maybe(
      fun(IA) ->
              functor:fmap(F, IA)
      end, MTA).

-spec '<*>'(maybe_t(M, fun((A) -> B)), maybe_t(M, A)) -> maybe_t(M, B).
'<*>'(MTF, MTA) ->
    maybe_t(
      do([monad ||
             MF <- run_maybe_t(MTF),
             maybe:'>>='(MF, fun(F) -> functor:fmap(maybe:fmap(F, _), run_maybe_t(MTA)) end)
         ])).

-spec pure(A) -> maybe_t(_M, A).
pure(A) ->
    return(A).

-spec '>>='(maybe_t(M, A), fun((A) -> maybe_t(M, B))) -> maybe_t(M, B).
'>>='(MTA, KMTB) ->
    maybe_t(
      do([monad ||
             M <- run_maybe_t(MTA),
             case M of
                 nothing ->
                     return(nothing);
                 {just, V} ->
                     run_maybe_t(KMTB(V))
             end
         ])).

-spec return(A) -> maybe_t(_M, A).
return(A) ->
    maybe_t(monad:return(maybe:return(A))).

-spec fail(_E) -> maybe_t(_M, _A).
fail(E) ->
    maybe_t(monad:return(maybe:fail(E))).

-spec lift(monad:monadic(M, A)) -> maybe_t(M, A).
lift(X) ->
    maybe_t(functor:fmap(maybe:return(_), X)).

-spec ask() -> maybe_t(_M, _A).
ask() ->
    lift(monad_reader:ask()).

-spec reader(fun((_R) -> A)) -> maybe_t(_M, A).
reader(F) ->
    lift(monad_reader:reader(F)).

-spec local(fun((R) -> R), maybe_t(M, A)) -> maybe_t(M, A).
local(F, ETA) ->
    map_maybe(
      fun(MA) ->
              monad_reader:local(F, MA)
      end, ETA).

-spec get() -> maybe_t(_M, _A).
get() ->
    lift(monad_state:get()).

-spec put(_S) -> maybe_t(_M, ok).
put(S) ->
    lift(monad_state:put(S)).

-spec state(fun((S) -> {A, S})) -> maybe_t(_M, A).
state(F) ->
    lift(monad_state:state(F)).

run_nargs() ->
    0.

run(MTA, []) ->
    run_maybe(MTA).

-spec run_maybe(maybe_t(M, A)) -> inner_maybe_t(M, A).
run_maybe(X) ->
    run_maybe_t(X).

-spec map_maybe(fun((monad:monadic(M, A)) -> monad:monadic(N, B)), maybe_t(M, A)) -> maybe_t(N, B).
map_maybe(F, X) ->
    maybe_t(F(run_maybe_t(X))).

%% ---------------------------------------------------------------------------------------
%%
%% old monad trans functions below
%%
%% ---------------------------------------------------------------------------------------
-spec new(M) -> t(M).
new(IM) ->
    {?MODULE, IM}.

-spec fmap(fun((A) -> B), maybe_t(M, A), t(M)) -> maybe_t(M, B).
fmap(F, X, {?MODULE, IM}) ->
    maybe_t(
      do([IM ||
             M <- X,
             maybe:fmap(F, M)
         ])).

-spec '>>='(maybe_t(M, A), fun((A) -> maybe_t(M, B)), t(M)) -> maybe_t(M, B).
'>>='(MTA, KMTB, {?MODULE, IM}) ->
    maybe_t(
      do([IM ||
             MA <- run_maybe_t(MTA),
             case MA of
                 nothing ->
                     return(nothing);
                 {just, A} ->
                     run_maybe_t(KMTB(A))
             end
         ])).

-spec return(A, t(M)) -> maybe_t(M, A).
return(A, {?MODULE, IM}) ->
    maybe_t(monad:return(maybe:return(A), IM)).

-spec fail(_E, t(M)) -> maybe_t(M, _A).
fail(E, {?MODULE, IM}) ->
    maybe_t(monad:return(maybe:fail(E), IM)).

-spec lift(monad:monadic(M, A), t(M)) -> maybe_t(M, A).
lift(MA, {?MODULE, IM}) ->
    maybe_t(
      do([IM ||
             A <- MA,
             return(maybe:return(A))
         ])).

-spec mzero(t(M)) -> maybe_t(M, _A).
mzero({?MODULE, IM}) ->
    maybe_t(monad:return(maybe:mzero(), IM)).

-spec mplus(maybe_t(M, A), maybe_t(M, A), t(M)) -> maybe_t(M, A).
mplus(MTA, MTB, {?MODULE, IM}) ->
    maybe_t(
      do([IM ||
             MA <- run_maybe_t(MTA),
             %% could be
             %% A <- run_maybe_t(MA),
             %% B <- run_maybe_t(MB)
             %% return(maybe_m:mplus(A, B))
             %% but erlang is not lazy as haskell
             %% I have to expand it
             case MA of
                 nothing ->
                     run_maybe_t(MTB);
                 {just, _V} ->
                     return(MA)
             end
       ])).

-spec run_maybe(maybe_t(M, A), t(M)) -> inner_maybe_t(M, A).
run_maybe(X, {?MODULE, _IM}) ->
    run_maybe(X).

-spec map_maybe(fun((monad:monadic(M, A)) -> monad:monadic(N, B)), maybe_t(M, A), t(M)) -> maybe_t(N, B).
map_maybe(F, X, {?MODULE, _IM}) ->
    map_maybe(F, X).
