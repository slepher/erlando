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

-module(cont_t).
-compile({parse_transform, do}).
-behaviour(monad_trans).
-behaviour(monad_cont_trans).

-export_type([cont_t/3]).

-export([new/1, t/1, run/1]).
% impl of monad
-export(['>>='/3, return/2, fail/2]).
% impl of monad transformer
-export([lift/2]).
% impl of monad cont
-export([callCC/2, reset/2, shift/2]).
% cont functions
-export([run_cont/3, eval_cont/2, map_cont/3, with_cont/3]).
% lift other monad functions
-export([lift_local/5]).


-opaque cont_t(R, M, A) ::
          {cont_t, fun((fun((A) -> monad:monadic(M, R))) -> monad:monadic(M, R) )}.

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(IM) ->
    {?MODULE, IM}.

t(Inner) ->
    {?MODULE, Inner}.

run({?MODULE, IM}) ->
    IM;
run(Other) ->
    exit({invalid_monad, Other}).

-spec '>>='(cont_t(R, M, A), fun((A) -> cont_t(R, M, B)), M) -> cont_t(R, M, B).
'>>='(M, Fun, {?MODULE, _IM} = CT) ->
    new(fun (K) -> CT:run_cont(M, fun (A) -> CT:run_cont(Fun(A), K) end) end).

-spec return(A, M) -> cont_t(_R, M, A).
return(A, {?MODULE, _IM}) ->
    new(fun (K) -> K(A) end).

-spec fail(any(), M) -> cont_t(_R, M, _A).
fail(E, {?MODULE, IM}) ->
    new(fun (_) -> IM:fail(E) end).

-spec lift(monad:monadic(M, A), M) -> cont_t(_R, M, A).
lift(X, {?MODULE, IM}) ->
    new(fun (F) -> IM:'>>='(X, F) end).

-spec callCC(fun((fun( (A) -> cont_t(R, M, _B) ))-> cont_t(R, M, A)), M) -> cont_t(R, M, A).
callCC(F, {?MODULE, _IM} = CT) ->
    new(fun (CC) -> CT:run_cont(F(fun(A) -> fun(_) -> CC(A) end end), CC) end).

-spec reset(cont_t(R, M, R), M) -> cont_t(_NR, M, R).
reset(X, {?MODULE, _IM} = CT) ->
    CT:lift(CT:eval_cont(X)).

-spec shift(fun((fun((A) -> monad:monadic(M, R))) -> cont_t(R, M, R)), M) -> cont_t(R, M, A).
shift(F, {?MODULE, _IM} = CT) ->
    new(fun(CC) -> CT:eval_cont(F(CC)) end).

-spec run_cont(cont_t(R, M, A), fun((A) -> monad:monadic(M, R)), M) -> monad:monadic(M, R).
run_cont(X, CC, {?MODULE, _IM}) ->
    (run(X))(CC).

-spec eval_cont(cont_t(R, M, R), M) -> monad:monadic(M, R).
eval_cont(X, {?MODULE, IM} = CT) ->
    CT:run_cont(X, fun(A) -> IM:return(A) end).

-spec map_cont(fun((monad:monadic(M, R)) -> monad:monadic(M, R)), cont_t(R, M, A), _MT) -> cont_t(R, M, A).
map_cont(F, X, {?MODULE, _IM} = CT) ->
    new(fun(CC) -> F(CT:run_cont(X, CC)) end).

-spec with_cont(fun((fun((B) -> monad:monadic(M, R))) -> fun((A) -> monad:monadic(M, R))), 
                cont_t(R, M, A), _TM) -> cont_t(R, M, B).
with_cont(F, X, {?MODULE, _IM} = MC) ->
    new(fun(CC) -> MC:run_cont(X, F(CC)) end).

lift_local(Ask, Local, F, X, {?MODULE, IM} = CT) ->    
    new(fun(CC) ->
                do([IM || 
                       R <- Ask(),
                       Local(F, CT:run_cont(X, fun(A) -> Local(fun(_) -> R end, CC(A)) end))
                   ])
        end).
