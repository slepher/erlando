%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 June 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(reader_t).

-erlando_type(?MODULE).

-export_type([reader_t/3]).

-opaque reader_t(R, M, A) :: {reader_t, inner_reader_t(R, M, A)}.
-type inner_reader_t(R, M, A) :: fun( (R) -> monad:monadic(M, A)).

-type t(M) :: {reader_t, M}.

-compile({parse_transform, do}).
-compile({parse_transform, monad_t_transform}).
-compile({no_auto_import, [get/0, get/1, put/1, put/2]}).

-include("op.hrl").

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_reader).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monad_runner).

-define(PG, [[], [?MODULE]]).

-export([new/1, reader_t/1, run_reader_t/1]).
% impl of functor
-export([fmap/3, '<$'/3]).
% impl of applicative
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
% impl of monad
-export(['>>='/3, '>>'/3, return/2]).
% impl of monad_trans
-export([lift/2]).
% impl of monad_reader
-export([ask/1, reader/2, local/3]).
% impl of monad_state
-export([get/1, put/2, state/2]).
% impl of alternative
-export([empty/1, '<|>'/3]).
% impl of monad_plus
-export([mzero/1, mplus/3]).
-export([run_nargs/0, run_m/2]).
% reader related functions
-export([run/2, map/2, with/2]).

-transform(#{patterns_group => ?PG, args => [{?MODULE, functor}], behaviours => [functor]}).
-transform(#{patterns_group => ?PG, args => [{?MODULE, applicative}], behaviours => [applicative]}).
-transform(#{patterns_group => ?PG, args => [{?MODULE, monad}], behaviours => [monad, monad_trans, monad_reader]}).
-transform(#{patterns_group => ?PG, args => [{?MODULE, alternative}], behaviours => [alternative]}).
-transform(#{patterns_group => ?PG, args => [{?MODULE, monad_plus}], behaviours => [monad_plus]}).

-spec new(M) -> t(M) when M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec reader_t(inner_reader_t(R, M, A)) -> reader_t(R, M, A).
reader_t(Inner) ->
    {?MODULE, Inner}.

-spec run_reader_t(reader_t(R, M, A)) -> inner_reader_t(R, M, A).
run_reader_t({?MODULE, Inner}) ->
    Inner;
run_reader_t({undetermined, _} = U) ->
    run_reader_t(undetermined:run(U, ?MODULE));
run_reader_t(Other) ->
    exit({invalid_reader_t, Other}).

-spec fmap(fun((A) -> B), reader_t(R, M, A)) -> reader_t(R, M, B).
fmap(F, RTA, {?MODULE, IM}) ->
    map(
      fun(FA) ->
              functor:fmap(F, FA, IM)
      end, RTA).

'<$'(B, FA, {?MODULE, _IM} = RT) ->
    functor:'default_<$'(B, FA, RT).

-spec '<*>'(reader_t(R, M, fun((A) -> B)), reader_t(R, M, A)) -> reader_t(R, M, B).
'<*>'(RTAB, RTA, {?MODULE, IM}) ->
    reader_t(
      fun(R) ->
              applicative:'<*>'(run(RTAB, R), run(RTA, R), IM)
      end).

-spec lift_a2(fun((A, B) -> C), reader_t(R, M, A), reader_t(R, M, B)) -> reader_t(R, M, C).
lift_a2(F, RTA, RTB, {?MODULE, _IM} = RT) ->
    applicative:default_lift_a2(F, RTA, RTB, RT).

-spec '*>'(reader_t(R, M, _A), reader_t(R, M, B)) -> reader_t(R, M, B).
'*>'(RTA, RTB, {?MODULE, _IM} = RT) ->
    applicative:'default_*>'(RTA, RTB, RT).

-spec '<*'(reader_t(R, M, A), reader_t(R, M, _B)) -> reader_t(R, M, A).
'<*'(RTA, RTB, {?MODULE, _IM} = RT) ->
    applicative:'default_<*'(RTA, RTB, RT).

pure(A, {?MODULE, IM}) ->
    reader_t(fun (_) -> applicative:pure(A, IM) end).

-spec '>>='(reader_t(R, M, A), fun( (A) -> reader_t(R, M, B) )) -> reader_t(R, M, B).
'>>='(RTA, KRTB, {?MODULE, IM}) ->
    reader_t(
      fun(R) ->
              do([IM || 
                     A <- run(RTA, R),
                     run(KRTB(A), R)
               ])
      end).

-spec '>>'(reader_t(R, M, _A), reader_t(R, M, B)) -> reader_t(R, M, B).
'>>'(RTA, RTB, {?MODULE, _IM} = RT) ->
    monad:'default_>>'(RTA, RTB, RT).

return(A, {?MODULE, IM}) ->
    reader_t(fun (_) -> monad:return(A, IM) end).

-spec lift(monad:monadic(M, A)) -> reader_t(_R, M, A).
lift(X, {?MODULE, _IM}) ->
    reader_t(fun(_) -> X end).

-spec local(fun( (R) -> R), reader_t(R, M, A)) -> reader_t(R, M, A).
local(F, RA, {?MODULE, _IM}) ->
    with(F, RA).

ask({?MODULE, IM}) ->
    reader_t(fun(R) -> monad:return(R, IM) end).

-spec reader(fun( (R) -> A)) -> reader_t(R, _M, A).
reader(F, {?MODULE, IM}) ->
    reader_t(fun(R) -> monad:return(F(R), IM) end).

get({?MODULE, IM} = RT) ->
    lift(monad_state:get(IM), RT).

put(S, {?MODULE, IM} = RT) ->
    lift(monad_state:put(S, IM), RT).

state(F, {?MODULE, IM} = RT) ->
    lift(monad_state:state(F, IM), RT).

empty({?MODULE, IM} = RT) ->
    lift(alternative:empty(IM), RT).

'<|>'(RTA, RTB, {?MODULE, IM}) ->
    reader_t(
      fun(R) ->
              alternative:'<|>'(run(RTA, R), run(RTB, R), IM)
      end).

mzero({?MODULE, IM} = RT) ->
    lift(monad_plus:mzero(IM), RT).

-spec mplus(reader_t(R, M, A), reader_t(R, M, A), t(M)) -> reader_t(R, M, A).
mplus(RA, RB, {?MODULE, IM}) ->
    reader_t(
      fun(R) ->
              monad_plus:mplus(run(RA, R), run(RB, R), IM)
      end).

run_nargs() ->
    1.

run_m(MR, [R]) ->
    run(MR, R).

-spec run(reader_t(R, M, A), R) -> monad:monadic(M, A).
run(MR, R) ->
    (run_reader_t(MR))(R).

-spec map(fun((monad:monadic(M, R)) -> monad:monadic(N, R)), reader_t(R, M, A)) -> reader_t(R, N, A).
map(F, MR) ->
    reader_t(fun(R) -> F(run(MR, R)) end).
    
-spec with(fun((NR) -> R), reader_t(NR, M, A)) -> reader_t(R, M, A).
with(F, MR) ->
    reader_t(fun(R) -> run(MR, F(R)) end).
