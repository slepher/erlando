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
-compile({parse_transform, do}).
-behaviour(monad_trans).
-behaviour(monad_state_trans).

-export_type([state_t/3]).

-export([new/1, t/1, run/1]).
% impl of monad
-export(['>>='/3, return/2, fail/2]).
% impl of monad trans
-export([lift/2]).
% impl of monad state
-export([get/1, gets/2, put/2, modify/2]).
-export([state/2, eval_state/3, exec_state/3, run_state/3, map_state/3, with_state/3]).
%% duplicate of eval_state/3, exec_state/3, run_state/3, state/2
-export([eval/3, exec/3, run/3, modify_and_return/2]).


-opaque state_t(S, M, A) :: {state_t, fun( (S) -> monad:monadic(M, {A, S}) )}.

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(Inner) ->
    {?MODULE, Inner}.

-spec t(fun((S) -> monad:monadic(M, {A, S}) )) -> state_t(S, M, A).
t(Inner) ->
    {?MODULE, Inner}.

-spec run(state_t(S, M, A)) -> fun((S) -> monad:monadic(M, {A, S})). 
run({?MODULE, Inner}) ->
    Inner;
run(Other) ->
    exit({invalid_monad, Other}).

-spec '>>='(state_t(S, M, A), fun( (A) -> state_t(S, M, B) ), M) -> state_t(S, M, B).
'>>='(X, Fun, {?MODULE, IM} = ST) ->  
    t(fun (S) ->
                do([IM || 
                       {A, NS} <- ST:run_state(X, S),
                       ST:run_state(Fun(A), NS)
                   ])
        end).

-spec return(A, M) -> state_t(_S, M, A).
return(A, {?MODULE, _IM} = ST) ->
    ST:state(fun (S) -> {A, S} end).

-spec fail(any(), M) -> state_t(_S, M, _A).
fail(E, {?MODULE, IM}) ->
    t(fun (_) -> IM:fail(E) end).

-spec lift(monad:monadic(M, A), M) -> state_t(_S, M, A).
lift(SM, {?MODULE, IM}) ->
    t(fun (S) ->
              do([IM || A <- SM,
                        IM:return({A, S})])
        end).

-spec get(M) -> state_t(S, M, S).
get({?MODULE, _M} = ST) ->
    ST:state(fun (S) -> {S, S} end).

-spec gets(fun((S) -> A), M) -> state_t(S, M, A).
gets(F, {?MODULE, _IM} = ST) ->
    ST:state(fun (S) -> {F(S), S} end).

-spec put(S, M) -> state_t(S, M, ok).
put(S, {?MODULE, _M} = ST) ->
    ST:state(fun (_) -> {ok, S} end).

-spec modify(fun((S) -> S ), M) -> state_t(S, M, ok).
modify(Fun, {?MODULE, _M} = ST) ->
    ST:state(fun (S) -> {ok, Fun(S)} end).

-spec state(fun((S) -> {A, S}), M) -> state_t(S, M, A).
state(Fun, {?MODULE, IM}) ->
    t(fun (S) -> IM:return(Fun(S)) end).

-spec eval_state(state_t(S, M, A), S, M) -> monad:monadic(M, A).
eval_state(SM, S, {?MODULE, IM} = ST) ->
    do([IM || {A, _NS} <- ST:run_state(SM, S),
              IM:return(A)]).

-spec exec_state(state_t(S, M, _A), S, M) -> monad:monadic(M, S).
exec_state(SM, S, {?MODULE, IM} = ST) ->
    do([IM || {_A, NS} <- ST:run_state(SM, S),
              IM:return(NS)]).

-spec run_state(state_t(S, M, A), S, M) -> monad:monadic(M, {A, S}).
run_state(SM, S, {?MODULE, _IM} = _ST) -> (run(SM))(S).

-spec map_state(fun((monad:monadic(M, {A, S})) -> monad:monadic(N, {B, S})), state_t(S, M, A), _TM) -> state_t(S, N, B).
map_state(F, SM, {?MODULE, _IM} = ST) ->
    t(fun (S) -> F(ST:run_state(SM, S)) end).

-spec with_state(fun((S) -> S), state_t(S, M, A), _TM) -> state_t(S, M, A).
with_state(F, SM, {?MODULE, _IM} = ST) ->
    t(fun (S) -> ST:run(SM, F(S)) end).

eval(SM, S, M) ->
    eval_state(SM, S, M).

exec(SM, S, M) ->
    exec_state(SM, S, M).

run(SM, S, M) ->
    run_state(SM, S, M).
            
modify_and_return(F, M) ->
    state(F, M).
