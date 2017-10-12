%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 June 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(reader_t).
-compile({parse_transform, do}).
-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(monad_reader).
-behaviour(monad_trans).
-behaviour(monad_plus_trans).

-define(READER_T_MONAD, {?MODULE, monad}).

-export_type([reader_t/3]).

-export([new/1, reader_t/1, run_reader_t/1]).
% impl of functor
-export([fmap/2]).
% impl of applicative
-export(['<*>'/2, pure/1]).
% impl of monad
-export(['>>='/2, return/1]).
% impl of monad_trans
-export([lift/1]).
% impl of monad fail
-export([fail/1]).
% impl of monad reader
-export([ask/0, reader/1, local/2]).
-export([run_reader/2]).

% impl of old behaviour modules
-export(['>>='/3, return/2, fail/2]).
% impl of monad trans
-export([lift/2]).
% impl of monad plus
-export([mzero/1, mplus/3]).
% impl of monad reader
-export([ask/1, reader/2, local/3]).
% monad reader functions
-export([run_reader/3, map_reader/3, with_reader/3]).

-opaque reader_t(R, M, A) :: {reader_t, inner_reader_t(R, M, A)}.
-type inner_reader_t(R, M, A) :: fun( (R) -> monad:monadic(M, A)).

-type t(M) :: {reader_t, M}.

-spec new(M) -> t(M) when M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec reader_t(inner_reader_t(R, M, A)) -> reader_t(R, M, A).
reader_t(Inner) ->
    {?MODULE, Inner}.

-spec run_reader_t(reader_t(R, M, A)) -> inner_reader_t(R, M, A).
run_reader_t({?MODULE, Inner}) ->
    Inner;
run_reader_t(Other) ->
    exit({invalid_reader_t, Other}).

-spec fmap(fun((A) -> B), reader_t(R, M, A)) -> reader_t(R, M, B).
fmap(F, X) ->
    map_reader(
      fun(RIM) ->
              functor:fmap(F, RIM)
      end, X).

pure(A) ->
    return(A).

'<*>'(RAB, RA) ->
    reader_t(
      fun(R) ->
              applicative:'<*>'(run_reader(RAB, R), run_reader(RA, R))
      end).

'>>='(X, Fun) ->
    '>>='(X, Fun, ?READER_T_MONAD).

return(A) ->
    return(A, ?READER_T_MONAD).

-spec '>>='(reader_t(R, M, A), fun( (A) -> reader_t(R, M, B) ), t(M)) -> reader_t(R, M, B).
'>>='(X, Fun, {?MODULE, IM} = RT) ->
    reader_t(
      fun(R) ->
              do([IM || 
                     A <- run_reader(X, R, RT),
                     run_reader(Fun(A), R, RT)
               ])
      end).

-spec return(A, t(M)) -> reader_t(_R, M, A).
return(A, {?MODULE, IM}) ->
    reader_t(fun (_) -> monad:return(A, IM) end).

-spec mzero(t(M)) -> reader_t(_R, M, _A).
mzero({?MODULE, IM} = RT) ->
    lift(monad_plus:mzero(IM), RT).

-spec mplus(reader_t(R, M, A), reader_t(R, M, A), t(M)) -> reader_t(R, M, A).
mplus(RA, RB, {?MODULE, IM} = RT) ->
    reader_t(
      fun(R) ->
              monad_plus:mplus(IM, run_reader(RA, R, RT), run_reader(RB, R, RT))
      end).

fail(E) ->
    fail(E, ?READER_T_MONAD).

-spec fail(any(), t(M)) -> reader_t(_R, M, _A).
fail(E, {?MODULE, M}) ->
    reader_t(fun (_) -> M:fail(E) end).

-spec lift(monad:monadic(M, A)) -> reader_t(_R, M, A).
lift(X) ->
    reader_t(fun(_) -> X end).

-spec lift(monad:monadic(M, A), t(M)) -> reader_t(_R, M, A).
lift(X, {?MODULE, _M}) ->
    reader_t(fun(_) -> X end).

-spec ask() -> reader_t(R, _M, R).
ask() ->
    ask(?READER_T_MONAD).

-spec reader(fun( (R) -> A)) -> reader_t(R, _M, A).
reader(F) ->
    reader(F, ?READER_T_MONAD).

-spec local(fun( (R) -> R), reader_t(R, M, A)) -> reader_t(R, M, A).
local(F, RA) ->
    local(F, RA, ?READER_T_MONAD).

-spec ask(M) -> reader_t(R, M, R).
ask({?MODULE, IM}) ->
    reader_t(fun(R) -> monad:return(R, IM) end).

-spec local(fun( (R) -> R), reader_t(R, M, A), t(M)) -> reader_t(R, M, A).
local(F, RA, {?MODULE, _IM} = RT) ->
    with_reader(F, RA, RT).

-spec reader(fun( (R) -> A), t(M)) -> reader_t(R, M, A).
reader(F, {?MODULE, IM}) ->
    reader_t(fun(R) -> monad:return(F(R), IM) end).

-spec run_reader(reader_t(R, M, A), R) -> monad:monadic(M, A).
run_reader(MR, R) ->
    (run_reader_t(MR))(R).

-spec run_reader(reader_t(R, M, A), R, t(M)) -> monad:monadic(M, A).
run_reader(MR, R, {?MODULE, _IM}) ->
    run_reader(MR, R).

-spec map_reader(fun((monad:monadic(M, R)) -> monad:monadic(N, R)), reader_t(R, M, A)) -> reader_t(R, N, A).
map_reader(F, MR) ->
    reader_t(fun(R) -> F(run_reader(MR, R)) end).
    
-spec map_reader(fun((monad:monadic(M, R)) -> monad:monadic(N, R)), reader_t(R, M, A), t(M)) -> reader_t(R, N, A).
map_reader(F, MR, {?MODULE, _IM}) ->
    map_reader(F, MR).

-spec with_reader(fun((NR) -> R), reader_t(NR, M, A), t(M)) -> reader_t(R, M, A).
with_reader(F, MR, {?MODULE, _IM} = RT) ->
    reader_t(fun(R) -> run_reader(MR, F(R), RT) end).
