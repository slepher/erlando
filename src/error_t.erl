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
-compile({parse_transform, monad_t_transform}).
-compile({no_auto_import, [get/0, get/1, put/1, put/2]}).

-include("op.hrl").

-export_type([error_t/3]).

-behaviour(type).
-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_reader).
-behaviour(monad_state).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monad_runner).

-export([new/1, error_t/1, run_error_t/1]).
-export([type/0]).
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
-export([run/1, map/2, with/2]).

-transform({?MODULE, [{?MODULE, functor}], [fmap/2, '<$'/2]}).
-transform({?MODULE, [{?MODULE, monad}], [pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]}).
-transform({?MODULE, [{?MODULE, monad}], ['>>='/2, '>>'/2, return/1]}).
-transform({?MODULE, [{?MODULE, monad}], [lift/1]}).
-transform({?MODULE, [{?MODULE, monad}], [fail/1]}).
-transform({?MODULE, [{?MODULE, monad}], [empty/0, '<|>'/2, mzero/0, mplus/2]}).
-transform({?MODULE, [{?MODULE, monad_reader}], [ask/0, reader/1, local/2]}).
-transform({?MODULE, [{?MODULE, monad_state}], [get/0, put/1, state/1]}).


-opaque error_t(E, M, A) :: {error_t, inner_t(E, M, A)}.

-type inner_t(E, M, A) :: monad:monadic(M, error_m:error_m(E, A)).

-type t(M) :: monad_trans:monad_trans(?MODULE, M).

type() ->
    type:default_type(?MODULE).

-spec new(M) -> t(M) when M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec error_t(inner_t(E, M, A)) -> error_t(E, M, A).
error_t(Inner) ->
    {?MODULE, Inner}.

-spec run_error_t(error_t(E, M, A)) -> inner_t(E, M, A).
run_error_t({?MODULE, Inner}) ->
    Inner;
run_error_t({undetermined, _} = UT) ->
    run_error_t(undetermined:run(UT, ?MODULE));
run_error_t(Other) ->
    exit({invalid_t, Other}).

-spec fmap(fun((A) -> B), error_t(E, M, A)) -> error_t(E, M, B).
fmap(F, ETA, {?MODULE, IM}) ->
    map(
      fun(FA) ->
              functor:fmap(error_instance:fmap(F, _), FA, IM)
      end, ETA).

-spec '<$'(B, error_t(E, M, _A)) -> error_t(E, M, B).
'<$'(B, ETA, {?MODULE, _IM} = ET) ->
    functor:'default_<$'(B, ETA, ET).

-spec '<*>'(error_t(E, M, fun((A) -> B)), error_t(E, M, A)) -> error_t(E, M, B).
'<*>'(ETF, ETA, {?MODULE, IM}) ->
    error_t(
      do([IM || 
             EF <- run_error_t(ETF),
             error_instance:'>>='(
               EF, fun(F) -> error_instance:fmap(F, _) /'<$>'/ run_error_t(ETA) end)
         ])).

-spec lift_a2(fun((A, B) -> C), error_t(E, M, A), error_t(E, M, B)) -> error_t(E, M, C).
lift_a2(F, ETA, ETB, {?MODULE, _IM} = ET) ->
    applicative:default_lift_a2(F, ETA, ETB, ET).

-spec '*>'(error_t(E, M, _A), error_t(E, M, B)) -> error_t(E, M, B).
'*>'(ETA, ETB, {?MODULE, _IM} = ET) ->
    applicative:'default_*>'(ETA, ETB, ET).

-spec '<*'(error_t(E, M, A), error_t(E, M, _B)) -> error_t(E, M, A).
'<*'(ETA, ETB, {?MODULE, _IM} = ET) ->
    applicative:'default_<*'(ETA, ETB, ET).

pure(A, {?MODULE, _IM} = ET) ->
    return(A, ET).

-spec '>>='(error_t(E, M, A), fun( (A) -> error_t(E, M, B) )) -> error_t(E, M, B).
'>>='(ETA, KETB, {?MODULE, IM}) ->
    error_t(
      do([IM || EA <- run_error_t(ETA),
              case EA of
                  {error, _Err}    -> return(EA);
                  {ok,  A}         -> run_error_t(KETB(A));
                  ok               -> run_error_t(KETB(ok))
              end
       ])).

-spec '>>'(error_t(E, M, _A), error_t(E, M, B)) -> error_t(E, M, B).
'>>'(ETA, ETB, {?MODULE, _IM} = ET) ->
    monad:'default_>>'(ETA, ETB, ET).

return(A, {?MODULE, IM}) ->
    error_t(monad:return(error_instance:pure(A), IM)).

-spec lift(monad:monadic(M, A)) -> error_t(_E, M, A).
lift(MA, {?MODULE, IM}) ->
    error_t(functor:fmap(error_instance:return(_), MA, IM)).

fail(E, {?MODULE, IM}) ->
    error_t(monad:return(error_instance:fail(E), IM)).

ask({?MODULE, IM}) ->
    lift(monad_reader:ask(IM), {?MODULE, IM}).

-spec local(fun((R) -> R), error_t(E, M, A)) -> error_t(E, M, A).
local(F, ETA, {?MODULE, IM}) ->
    map(
      fun(MA) ->
              monad_reader:local(F, MA, IM)
      end, ETA).

reader(F, {?MODULE, IM}) ->
    lift(monad_reader:reader(F, IM), {?MODULE, IM}).

get({?MODULE, IM}) ->
    lift(monad_state:get(IM), {?MODULE, IM}).

put(S, {?MODULE, IM}) ->
    lift(monad_state:put(S, IM), {?MODULE, IM}).

state(F, {?MODULE, IM}) ->
    lift(monad_state:state(F, IM), {?MODULE, IM}).

empty({?MODULE, _IM} = ET) ->
    mzero(ET).

'<|>'(ETA, ETB, {?MODULE, _IM} = ET) ->
    mplus(ETA, ETB, ET).

mplus(ETA, ETB, {?MODULE, IM}) ->
    error_t(
      do([IM ||
             EA <- run_error_t(ETA),
             case EA of
                 {error, _} ->
                     run_error_t(ETB);
                 _ ->
                     return(EA)
             end
         ])).

mzero({?MODULE, IM}) ->
    error_t(monad:return({error, error}, IM)).

run_nargs() ->
    0.

run_m(EM, []) ->
    run(EM).

-spec run(error_t(E, M, A)) -> monad:monadic(M, error_m:error_m(E, A)).
run(EM) -> 
    run_error_t(EM).

-spec map(fun((monad:monadic(M, error_m:error_m(EA, A))) -> monad:monadic(N, error_m:error_m(EB, B))),
                error_t(EA, M, A)) -> error_t(EB, N, B).
map(F, X) ->
    error_t(F(run_error_t(X))).

-spec with(fun((EA) -> EB), error_t(EA, M, A)) -> error_t(EB, M, A).
with(F, X) ->
    map(
      fun(MA) ->
              fun({error, R}) -> {error, F(R)}; (Val) -> Val end /'<$>'/ MA
      end, X).
