%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 14 Aug 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(maybe_t).

-erlando_type(?MODULE).

-export_type([maybe_t/2]).

-opaque maybe_t(M, A) :: {maybe_t, inner_t(M, A)}.
-type inner_t(M, A) :: monad:monadic(M, maybe:maybe(A)).
-type t(M) :: monad_trans:monad_trans(?MODULE, M).

-compile({parse_transform, cut}).
-compile({parse_transform, do}).
-compile({parse_transform, monad_t_transform}).
-compile({no_auto_import, [get/0, get/1, put/1, put/2]}).

-include("op.hrl").

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_fail).
-behaviour(monad_fail_trans).
-behaviour(monad_reader).
-behaviour(monad_reader_trans).
-behaviour(monad_state).
-behaviour(monad_state_trans).
-behaviour(monad_runner).

%% API
-export([new/1, maybe_t/1, run_maybe_t/1]).
% impl of functor.
-export([fmap/3, '<$'/3]).
% impl of applicative.
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
% impl of monad.
-export(['>>='/3, '>>'/3, return/2]).
% impl of monad_trans.
-export([lift/2]).
% impl of monad_fail.
-export([fail/2]).
% impl of monad_reader.
-export([ask/1, reader/2, local/3]).
% impl of monad_state.
-export([get/1, put/2, state/2]).
-export([empty/1, '<|>'/3]).
-export([mzero/1, mplus/3]).
% impl of monad_runner.
-export([run_nargs/0, run_m/2]).
% maybe_t functions.
-export([run/1, map/2]).

-transform({?MODULE, functor, [fmap/2, '<$'/2]}).
-transform({?MODULE, monad, [pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]}).
-transform({?MODULE, monad, ['>>='/2, '>>'/2, return/1]}).
-transform({?MODULE, monad, [lift/1]}).
-transform({?MODULE, monad, [fail/1]}).
-transform({?MODULE, monad, [empty/0, '<|>'/2, mzero/0, mplus/2]}).
-transform({?MODULE, monad_reader, [ask/0, reader/1, local/2]}).
-transform({?MODULE, monad_state, [get/0, put/1, state/1]}).

-spec new(M) -> t(M).
new(IM) ->
    {?MODULE, IM}.

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
fmap(F, MTA, {?MODULE, IM}) ->
    map(
      fun(FA) ->
              functor:fmap(F, FA, IM)
      end, MTA).

'<$'(B, FA, {?MODULE, _IM} = MT) ->
    functor:'default_<$'(B, FA, MT).

-spec '<*>'(maybe_t(M, fun((A) -> B)), maybe_t(M, A)) -> maybe_t(M, B).
'<*>'(MTF, MTA, {?MODULE, IM}) ->
    maybe_t(
      do([IM ||
             MF <- run_maybe_t(MTF),
             maybe:'>>='(MF, fun(F) -> maybe:fmap(F, _) /'<$>'/ run_maybe_t(MTA) end)
         ])).

-spec lift_a2(fun((A, B) -> C), maybe_t(M, A), maybe_t(M, B)) -> maybe_t(M, C).
lift_a2(F, MTA, MTB, {?MODULE, _IM} = MT) ->
    applicative:default_lift_a2(F, MTA, MTB, MT).

-spec '*>'(maybe_t(M, _A), maybe_t(M, B)) -> maybe_t(M, B).
'*>'(MTA, MTB, {?MODULE, _IM} = MT) ->
    applicative:'default_*>'(MTA, MTB, MT).

-spec '<*'(maybe_t(M, A), maybe_t(M, _B)) -> maybe_t(M, A).
'<*'(MTA, MTB, {?MODULE, _IM} = MT) ->
    applicative:'default_<*'(MTA, MTB, MT).

pure(A, {?MODULE, _IM} = MT) ->
    return(A, MT).

-spec '>>='(maybe_t(M, A), fun((A) -> maybe_t(M, B)), t(M)) -> maybe_t(M, B).
'>>='(MTA, KMTB, {?MODULE, IM}) ->
    maybe_t(
      do([IM ||
             M <- run_maybe_t(MTA),
             case M of
                 nothing ->
                     return(nothing);
                 {just, V} ->
                     run_maybe_t(KMTB(V))
             end
         ])).

-spec '>>'(maybe_t(M, _A), maybe_t(M, B), t(M)) -> maybe_t(M, B).
'>>'(MTA, MTB, {?MODULE, _IM} = MT) ->
    monad:'default_>>'(MTA, MTB, MT).

return(A, {?MODULE, IM}) ->
    maybe_t(monad:return(maybe:pure(A), IM)).

-spec lift(monad:monadic(M, A)) -> maybe_t(M, A).
lift(MA, {?MODULE, IM}) ->
    maybe_t(monad:lift_m(maybe:return(_), MA, IM)).

fail(E, {?MODULE, IM}) ->
    maybe_t(monad:return(maybe:fail(E), IM)).

-spec local(fun((R) -> R), maybe_t(M, A)) -> maybe_t(M, A).
local(F, ETA, {?MODULE, IM}) ->
    map(
      fun(MA) ->
              monad_reader:local(F, MA, IM)
      end, ETA).

ask({?MODULE, IM} = MT) ->
    lift(monad_reader:ask(IM), MT).

reader(F, {?MODULE, IM} = MT) ->
    lift(monad_reader:reader(F, IM), MT).

get({?MODULE, IM} = MT) ->
    lift(monad_state:get(IM), MT).

put(S, {?MODULE, IM} = MT) ->
    lift(monad_state:put(S, IM), MT).

state(F, {?MODULE, IM} = MT) ->
    lift(monad_state:state(F, IM), MT).

empty({?MODULE, _IM} = MT) ->
    mzero(MT).

'<|>'(MTA, MTB, {?MODULE, _IM} = MT) ->
    mplus(MTA, MTB, MT).

-spec mzero(t(M)) -> maybe_t(M, _A).
mzero({?MODULE, IM}) ->
    maybe_t(monad:return(maybe:mzero(), IM)).

-spec mplus(maybe_t(M, A), maybe_t(M, A)) -> maybe_t(M, A).
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
