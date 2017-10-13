%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is Erlando.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2011-2013 VMware, Inc.  All rights reserved.
%%

-module(state_t).
-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_fail).
-behaviour(monad_state).
-behaviour(monad_reader).

-compile({parse_transform, do}).
-export_type([state_t/3]).

-compile({no_auto_import, [get/1, put/2]}).

-define(STATE_T_MONAD, {?MODULE, monad}).

-export([new/1, state_t/1, run_state_t/1]).
% impl of functor.
-export([fmap/2]).
% impl of applcative.
-export(['<*>'/2, pure/1]).
% impl of monad.
-export(['>>='/2, return/1]).
% impl of monad_trans.
-export([lift/1]).
% impl of monad_fail.
-export([fail/1]).
% impl of monad_state.
-export([get/0, put/1, state/1]).
% impl of moand_state.
-export([ask/0, reader/1, local/2]).
%% state related functions
-export([eval_state/2, exec_state/2, run_state/2, map_state/2, with_state/2]).

%% depricated below
% old impl of monad
-export(['>>='/3, return/2, fail/2]).
% impl of monad trans
-export([lift/2]).
% impl of monad plus
-export([mzero/1, mplus/3]).
% impl of monad state
-export([get/1, gets/2, put/2, modify/2]).
-export([state/2, eval_state/3, exec_state/3, run_state/3, map_state/3, with_state/3]).
%% duplicate of eval_state/3, exec_state/3, run_state/3, state/2
-export([eval/3, exec/3, run/3, modify_and_return/2]).

-type state_t(S, M, A) :: {state_t, inner_state_t(S, M, A)}.
-type inner_state_t(S, M, A) :: fun((S) -> monad:monadic(M, {A, S})).

-type t(M) :: {state_t, M}.

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(Inner) ->
    {?MODULE, Inner}.

-spec state_t(inner_state_t(S, M, A)) -> state_t(S, M, A).
state_t(Inner) ->
    {?MODULE, Inner}.

-spec run_state_t(state_t(S, M, A)) -> inner_state_t(S, M, A).
run_state_t({?MODULE, Inner}) ->
    Inner;
run_state_t({undetermined, _} = U) ->
    run_state_t(undetermined:run(U, ?MODULE));
run_state_t(Other) ->
    exit({invalid_state_t, Other}).

-spec fmap(fun((A) -> B), state_t(S, M, A)) -> state_t(S, M, B).
fmap(F, X) ->
    map_state(
      fun(SIM) ->
              functor:fmap(
                fun({A, S}) ->
                        {F(A), S}
                end, SIM)
      end, X).


-spec '<*>'(state_t(S, M, fun((A) -> B)),  state_t(S, M, A)) -> state_t(S, M, B).
'<*>'(STF, STA) ->
    state_t(
      fun(S) ->
              do([monad ||
                     {F, NS} <- run_state(STF, S),
                     {A, NNS} <- run_state(STA, NS),
                     return({F(A), NNS})
                 ])
      end).

-spec pure(A) -> state_t(_S, _M, A).
pure(A) ->
    state(fun (S) -> {A, S} end).

-spec '>>='(state_t(S, M, A), fun( (A) -> state_t(S, M, B))) -> state_t(S, M, B).
'>>='(X, Fun) ->
    state_t(
      fun (S) ->
              do([ monad || 
                     {A, NS} <- run_state(X, S),
                     run_state(Fun(A), NS)
                 ])
        end).

-spec return(A) -> state_t(_S, _M, A).
return(A) ->
    pure(A).

-spec lift(monad:monadic(M, A)) -> state_t(_S, M, A).
lift(ISTM) ->
    state_t(
      fun(S) ->
              functor:fmap(fun(A) -> {A, S} end, ISTM)
      end).

-spec fail(_E) -> state_t(_S, _M, _A).
fail(E) ->
    lift(monad_fail:fail(E)).
    %state_t(fun (_) -> monad_fail:fail(E) end).

-spec get() -> state_t(S, _M, S).
get() ->
    state(fun (S) -> {S, S} end).

-spec put(S) -> state_t(S, _M, ok).
put(S) ->
    state(fun (_) -> {ok, S} end).

-spec state(fun((S) -> {A, S})) -> state_t(S, _M, A).
state(F) ->
    state_t(fun (S) -> monad:return(F(S)) end).

ask() ->
    lift(monad_reader:ask()).

reader(F) ->
    lift(monad_reader:reader(F)).

local(F, STM) ->
    map_state(
      fun(IM) ->
              monad_reader:local(F, IM)
      end, STM).

-spec eval_state(state_t(S, M, A), S) -> monad:monadic(M, A).
eval_state(SM, S) ->    
    functor:fmap(fun({A, _}) -> A end, run_state(SM, S)).

-spec exec_state(state_t(S, M, _A), S) -> monad:monadic(M, S).
exec_state(SM, S) ->
    functor:fmap(fun({_, NS}) -> NS end, run_state(SM, S)).

-spec run_state(state_t(S, M, A), S) -> monad:monadic(M, {A, S}).
run_state(SM, S) -> (run_state_t(SM))(S).

-spec map_state(fun((monad:monadic(M, {A, S})) -> monad:monadic(N, {B, S})), state_t(S, M, A)) -> state_t(S, N, B).
map_state(F, SM) ->
    state_t(fun (S) -> F(run_state(SM, S)) end).

-spec with_state(fun((S) -> S), state_t(S, M, A)) -> state_t(S, M, A).
with_state(F, SM) ->
    state_t(fun (S) -> run_state(SM, F(S)) end).

%% ---------------------------------------------------------------------------------------
%%
%% old monad trans functions below
%%
%% ---------------------------------------------------------------------------------------
-spec '>>='(state_t(S, M, A), fun( (A) -> state_t(S, M, B)), t(M)) -> state_t(S, M, B).
'>>='(X, Fun, {?MODULE, IM} = ST) ->
    state_t(fun (S) ->
                    do([ IM || {A, NS} <- run_state(X, S, ST),
                               run_state(Fun(A), NS, ST)
                       ])
        end).

-spec return(A, t(M)) -> state_t(_S, M, A).
return(A, {?MODULE, _IM} = ST) ->
    state(fun (S) -> {A, S} end, ST).

-spec fail(any(), t(M)) -> state_t(_S, M, _A).
fail(E, {?MODULE, IM}) ->
    state_t(fun (_) -> runtime_do:fail(IM, E) end).

-spec lift(monad:monadic(M, A), t(M)) -> state_t(_S, M, A).
lift(ISTM, {?MODULE, IM}) ->
    state_t(
      fun (S) ->
              do([IM || A <- ISTM,
                        return({A, S})
                 ])
      end).

-spec mzero(t(M)) -> state_t(_S, M, _A).
mzero({?MODULE, IM}) ->
    state_t(fun(_) -> monad_plus:mzero(IM) end).

-spec mplus(state_t(S, M, A), state_t(S, M, A), t(M)) -> state_t(S, M, A).
mplus(SA, SB, {?MODULE, IM} = ST) ->
    state_t(
      fun(S) ->
              monad_plus:mplus(IM, run_state(SA, S, ST), run_state(SB, S, ST))
      end).

-spec get(t(M)) -> state_t(S, M, S).
get({?MODULE, _M} = ST) ->
    state(fun (S) -> {S, S} end, ST).

-spec gets(fun((S) -> A), t(M)) -> state_t(S, M, A).
gets(F, {?MODULE, _IM} = ST) ->
    state(fun (S) -> {F(S), S} end, ST).

-spec put(S, t(M)) -> state_t(S, M, ok).
put(S, {?MODULE, _M} = ST) ->
    state(fun (_) -> {ok, S} end, ST).

-spec modify(fun((S) -> S ), t(M)) -> state_t(S, M, ok).
modify(Fun, {?MODULE, _M} = ST) ->
    state(fun (S) -> {ok, Fun(S)} end, ST).

-spec state(fun((S) -> {A, S}), t(M)) -> state_t(S, M, A).
state(Fun, {?MODULE, IM}) ->
    state_t(fun (S) -> runtime_do:return(Fun(S), IM) end).

-spec eval_state(state_t(S, M, A), S, t(M)) -> monad:monadic(M, A).
eval_state(SM, S, {?MODULE, IM} = ST) ->
     do([ IM || {A, _NS} <- run_state(SM, S, ST),
                return(A)
       ]).

-spec exec_state(state_t(S, M, _A), S, t(M)) -> monad:monadic(M, S).
exec_state(SM, S, {?MODULE, IM}) ->
     do([ IM || {_A, NS} <- run_state(SM, S),
                return(NS)
        ]).

-spec run_state(state_t(S, M, A), S, t(M)) -> monad:monadic(M, {A, S}).
run_state(SM, S, {?MODULE, _IM} = _ST) ->
    run_state(SM, S).

-spec map_state(fun((monad:monadic(M, {A, S})) -> monad:monadic(N, {B, S})), state_t(S, M, A), t(M)) -> state_t(S, N, B).
map_state(F, SM, {?MODULE, _IM}) ->
    map_state(F, SM).

-spec with_state(fun((S) -> S), state_t(S, M, A), t(M)) -> state_t(S, M, A).
with_state(F, STA, {?MODULE, _IM}) ->
    with_state(F, STA).

eval(SM, S, M) ->
    eval_state(SM, S, M).

exec(SM, S, M) ->
    exec_state(SM, S, M).

run(SM, S, M) ->
    run_state(SM, S, M).

modify_and_return(F, M) ->
    state(F, M).
