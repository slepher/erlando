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
-compile({parse_transform, monad_t_transform}).
-compile({no_auto_import, [get/0, get/1, put/1, put/2]}).

-include("op.hrl").

-define(MAYBE_T_MONAD, {?MODULE, monad}).

-behaviour(type).
-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_fail).
-behaviour(monad_reader).
-behaviour(monad_state).
-behaviour(monad_runner).

-export_type([maybe_t/2]).

-opaque maybe_t(M, A) :: {maybe_t, inner_t(M, A)}.
-type inner_t(M, A) :: monad:monadic(M, maybe:maybe(A)).
-type t(M) :: monad_trans:monad_trans(?MODULE, M).

%% API
-export([new/1, maybe_t/1, run_maybe_t/1]).
% imple of type.
-export([type/0]).
% impl of functor.
-export([fmap/2, '<$'/2]).
% impl of applicative.
-export([pure/2, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
% impl of monad.
-export(['>>='/2, '>>'/2, return/2]).
% impl of monad_trans.
-export([lift/1]).
% impl of monad_fail.
-export([fail/2]).
% impl of monad_reader.
-export([ask/1, reader/2, local/2]).
% impl of monad_state.
-export([get/1, put/2, state/2]).
-export([empty/1, '<|>'/2]).
-export([mzero/1, mplus/2]).
% impl of monad_runner.
-export([run_nargs/0, run_m/2]).
% maybe_t functions.
-export([run/1, map/2]).

-transform({?MODULE, [{?MODULE, monad}], [pure/1, return/1]}).
-transform({?MODULE, [{?MODULE, monad}], [fail/1]}).
-transform({?MODULE, [{?MODULE, monad}], [empty/0, mzero/0]}).
-transform({?MODULE, [{?MODULE, monad_reader}], [ask/0, reader/1]}).
-transform({?MODULE, [{?MODULE, monad_state}], [get/0, put/1, state/1]}).

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

type() ->
    type:default_type(?MODULE).

-spec fmap(fun((A) -> B), maybe_t(M, A)) -> maybe_t(M, B).
fmap(F, MTA) ->
    map(
      fun(FA) ->
              F /'<$>'/ FA
      end, MTA).

'<$'(B, FA) ->
    functor:'default_<$'(B, FA, ?MODULE).

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

pure(A, {?MODULE, _IM} = MT) ->
    return(A, MT).

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

return(A, {?MODULE, IM}) ->
    maybe_t(monad:return(maybe:pure(A), IM)).

-spec lift(monad:monadic(M, A)) -> maybe_t(M, A).
lift(X) ->
    maybe_t(maybe:return(_) /'<$>'/ X).

fail(E, {?MODULE, IM}) ->
    maybe_t(monad:return(maybe:fail(E), IM)).

-spec local(fun((R) -> R), maybe_t(M, A)) -> maybe_t(M, A).
local(F, ETA) ->
    map(
      fun(MA) ->
              monad_reader:local(F, MA)
      end, ETA).

ask({?MODULE, IM}) ->
    lift(monad_reader:ask(IM)).

reader(F, {?MODULE, IM}) ->
    lift(monad_reader:reader(F, IM)).

get({?MODULE, IM}) ->
    lift(monad_state:get(IM)).

put(S, {?MODULE, IM}) ->
    lift(monad_state:put(S, IM)).

state(F, {?MODULE, IM}) ->
    lift(monad_state:state(F, IM)).

'<|>'(MTA, MTB) ->
    mplus(MTA, MTB).

empty({?MODULE, _IM} = MT) ->
    mzero(MT).

-spec mplus(maybe_t(M, A), maybe_t(M, A)) -> maybe_t(M, A).
mplus(MTA, MTB) ->
    maybe_t(
      do([monad ||
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

-spec mzero(t(M)) -> maybe_t(M, _A).
mzero({?MODULE, IM}) ->
    maybe_t(monad:return(maybe:mzero(), IM)).

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
