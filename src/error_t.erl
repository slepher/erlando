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
-export([fmap/2, '<$'/2]).
-export([pure/2, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export(['>>='/2, '>>'/2, return/2]).
-export([lift/1]).
-export([fail/2]).
-export([ask/1, reader/2, local/2]).
-export([get/1, put/2, state/2]).
-export([empty/1, '<|>'/2]).
-export([mzero/1, mplus/2]).
-export([run_nargs/0, run_m/2]).
-export([run/1, map/2, with/2]).

-transform({?MODULE, [{?MODULE, monad}], [pure/1, return/1]}).
-transform({?MODULE, [{?MODULE, monad}], [fail/1]}).
-transform({?MODULE, [{?MODULE, monad}], [empty/0, mzero/0]}).
-transform({?MODULE, [{?MODULE, monad_reader}], [ask/0, reader/1]}).
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
fmap(F, ETA) ->
    map(
      fun(FA) ->
              error_instance:fmap(F, _) /'<$>'/ FA
      end, ETA).

-spec '<$'(B, error_t(E, M, _A)) -> error_t(E, M, B).
'<$'(B, ETA) ->
    functor:'default_<$'(B, ETA, ?MODULE).

-spec '<*>'(error_t(E, M, fun((A) -> B)), error_t(E, M, A)) -> error_t(E, M, B).
'<*>'(ETF, ETA) ->
    error_t(
      do([monad || 
             EF <- run_error_t(ETF),
             error_instance:'>>='(
               EF, fun(F) -> error_instance:fmap(F, _) /'<$>'/ run_error_t(ETA) end)
         ])).

-spec lift_a2(fun((A, B) -> C), error_t(E, M, A), error_t(E, M, B)) -> error_t(E, M, C).
lift_a2(F, ETA, ETB) ->
    applicative:default_lift_a2(F, ETA, ETB, ?MODULE).

-spec '*>'(error_t(E, M, _A), error_t(E, M, B)) -> error_t(E, M, B).
'*>'(ETA, ETB) ->
    applicative:'default_*>'(ETA, ETB, ?MODULE).

-spec '<*'(error_t(E, M, A), error_t(E, M, _B)) -> error_t(E, M, A).
'<*'(ETA, ETB) ->
    applicative:'default_<*'(ETA, ETB, ?MODULE).

pure(A, {?MODULE, _IM} = ET) ->
    return(A, ET).

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

-spec '>>'(error_t(E, M, _A), error_t(E, M, B)) -> error_t(E, M, B).
'>>'(ETA, ETB) ->
    monad:'default_>>'(ETA, ETB, ?MODULE).

return(A, {?MODULE, IM}) ->
    error_t(monad:return(error_instance:pure(A), IM)).

-spec lift(monad:monadic(M, A)) -> error_t(_E, M, A).
lift(X) ->
    error_t(error_instance:return(_) /'<$>'/ X).

fail(E, {?MODULE, IM}) ->
    error_t(monad:return(error_instance:fail(E), IM)).

-spec local(fun((R) -> R), error_t(E, M, A)) -> error_t(E, M, A).
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

'<|>'(ETA, ETB) ->
    mplus(ETA, ETB).

empty({?MODULE, _IM} = ET) ->
    mzero(ET).

mplus(ETA, ETB) ->
    error_t(
      do([monad ||
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
