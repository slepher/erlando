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
-compile({no_auto_import, [get/1, put/2]}).

-include("op.hrl").

-define(MAYBE_T_MONAD, monad).

-behaviour(type).
-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_fail).
-behaviour(monad_reader).
-behaviour(monad_state).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monad_runner).

-export_type([maybe_t/2]).

-opaque maybe_t(M, A) :: {maybe_t, inner_t(M, A)}.
-type inner_t(M, A) :: monad:monadic(M, maybe:maybe(A)).
-type t(M) :: monad_trans:monad_trans(?MODULE, M).

%% API
-export([new/1, maybe_t/1, run_maybe_t/1]).
-export([type/0]).
% impl of functor.
-export([fmap/2, '<$'/2]).
% impl of applicative.
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export([pure/2]).
% impl of monad.
-export(['>>='/2, '>>'/2, return/1]).
-export([return/2]).
% impl of monad_trans.
-export([lift/1]).
% impl of monad_fail.
-export([fail/1]).
% impl of monad_reader.
-export([ask/0, reader/1, local/2]).
-export([ask/1, reader/2]).
% impl of monad_state.
-export([get/0, put/1, state/1]).
-export([get/1, put/2, state/2]).
-export([empty/0, '<|>'/2]).
-export([empty/1]).
-export([mzero/0, mplus/2]).
-export([mzero/1]).

% impl of monad_runner.
-export([run_nargs/0, run_m/2]).
% maybe_t functions.
-export([run/1, map/2]).

type() ->
    type:default_type(?MODULE).

-spec new(M) -> t(M).
new(Inner) ->
    {?MODULE, Inner}.

-spec maybe_t(inner_t(M, A)) -> maybe_t(M, A).
maybe_t(Inner) ->
    {?MODULE, Inner}.

-spec run_maybe_t(maybe_t(M, A)) -> inner_t(M, A).
run_maybe_t({?MODULE, Inner}) ->
    Inner;
run_maybe_t({undetermined, _} = U) ->
    run_maybe_t(undetermined:run(U, maybe_t));
run_maybe_t(Other) ->
    exit({invalid_t, Other}).

-spec fmap(fun((A) -> B), maybe_t(M, A)) -> maybe_t(M, B).
fmap(F, MTA) ->
    map(
      fun(FA) ->
              F /'<$>'/ FA
      end, MTA).

'<$'(B, FA) ->
    functor:'default_<$'(B, FA, ?MODULE).

-spec pure(A) -> maybe_t(_M, A).
pure(A) ->
    pure(A, applicative).

pure(A, IM) ->
    maybe_t(applicative:pure(maybe:pure(A), IM)).

-spec '<*>'(maybe_t(M, fun((A) -> B)), maybe_t(M, A)) -> maybe_t(M, B).
'<*>'(MTF, MTA) ->
    maybe_t(
      do([monad ||
             MF <- run_maybe_t(MTF),
             maybe:'>>='(MF, fun(F) -> maybe:fmap(F, _) /'<$>'/ run_maybe_t(MTA) end)
         ])).

-spec lift_a2(fun((A, B) -> C), maybe_t(M, A), maybe_t(M, B)) -> maybe_t(M, C).
lift_a2(F, MTA, MTB) ->
    applicative:default_lift_a2(F, MTA, MTB, ?MODULE).

-spec '*>'(maybe_t(M, _A), maybe_t(M, B)) -> maybe_t(M, B).
'*>'(MTA, MTB) ->
    applicative:'default_*>'(MTA, MTB, ?MODULE).

-spec '<*'(maybe_t(M, A), maybe_t(M, _B)) -> maybe_t(M, A).
'<*'(MTA, MTB) ->
    applicative:'default_<*'(MTA, MTB, ?MODULE).

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

-spec '>>'(maybe_t(M, _A), maybe_t(M, B)) -> maybe_t(M, B).
'>>'(MTA, MTB) ->
    monad:'default_>>'(MTA, MTB, ?MODULE).

-spec return(A) -> maybe_t(_M, A).
return(A) ->
    return(A, monad).

return(A, IM) ->
    maybe_t(monad:return(maybe:return(A), IM)).

fail(E) ->
    fail(E, monad).

-spec fail(_E, t(M)) -> maybe_t(M, _A).
fail(E, IM) ->
    maybe_t(monad:return(maybe:fail(E), IM)).

-spec lift(monad:monadic(M, A)) -> maybe_t(M, A).
lift(X) ->
    maybe_t(maybe:return(_) /'<$>'/ X).

-spec ask() -> maybe_t(_M, _A).
ask() ->
    ask(monad_reader).

-spec ask(t(M)) -> maybe_t(M, _A).
ask(IM) ->
    lift(monad_reader:ask(IM)).

-spec reader(fun((_R) -> A)) -> maybe_t(_M, A).
reader(F) ->
    reader(F, monad_reader).

-spec reader(fun((_R) -> A), t(M)) -> maybe_t(M, A).
reader(F, IM) ->
    lift(monad_reader:reader(F, IM)).

-spec local(fun((R) -> R), maybe_t(M, A)) -> maybe_t(M, A).
local(F, ETA) ->
    map(
      fun(MA) ->
              monad_reader:local(F, MA)
      end, ETA).

get() ->
    get(monad_state).

-spec get(t(M)) -> maybe_t(M, _A).
get(IM) ->
    lift(monad_state:get(IM)).

-spec put(_S) -> maybe_t(_M, ok).
put(S) ->
    put(S, monad_state).

-spec put(_S, t(M)) -> maybe_t(M, ok).
put(S, IM) ->
    lift(monad_state:put(S, IM)).

-spec state(fun((S) -> {A, S})) -> maybe_t(_M, A).
state(F) ->
    state(F, monad_state).

-spec state(fun((S) -> {A, S}), t(M)) -> maybe_t(M, A).
state(F, IM) ->
    lift(monad_state:state(F, IM)).

empty() ->
    mzero().

empty(IM) ->
    mzero(IM).

'<|>'(MTA, MTB) ->
    mplus(MTA, MTB).

mzero() ->
    mzero(monad).

mzero(IM) ->
    maybe_t(monad:return(nothing, IM)).

mplus(MTA, MTB) ->
    maybe_t(
      do([monad ||
             MA <- run_maybe_t(MTA),
             case MA of
                 nothing ->
                     run_maybe_t(MTB);
                 {just, _} ->
                     return(MA)
             end
         ])).

run_nargs() ->
    0.

run_m(MTA, []) ->
    run(MTA).

-spec run(maybe_t(M, A)) -> inner_t(M, A).
run(X) ->
    run_maybe_t(X).

-spec map(fun((monad:monadic(M, A)) -> monad:monadic(N, B)), maybe_t(M, A)) -> maybe_t(N, B).
map(F, X) ->
    maybe_t(F(run_maybe_t(X))).

%% ---------------------------------------------------------------------------------------
%%
%% old monad trans functions below
%%
%% ---------------------------------------------------------------------------------------
