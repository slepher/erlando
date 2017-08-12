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

-export([new/1, cont_t/1, run_cont_t/1]).
% impl of functor 
-export([fmap/3]).
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


-opaque cont_t(R, M, A) :: {cont_t, inner_cont_t(R, M, A)}.

-type inner_cont_t(R, M, A) :: fun((fun((A) -> monad:monadic(M, R))) -> monad:monadic(M, R)).

-type t(M) :: {cont_t, M}.

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(IM) ->
    {?MODULE, IM}.

-spec cont_t(inner_cont_t(R, M, A)) -> cont_t(R, M, A).
cont_t(Inner) ->
    {?MODULE, Inner}.

-spec run_cont_t(cont_t(R, M, A)) -> inner_cont_t(R, M, A).
run_cont_t({?MODULE, Inner}) ->
    Inner;
run_cont_t(Other) ->
    exit({invalid_monad, Other}).

-spec fmap(fun((A) -> B), cont_t(R, M, A), t(M)) -> cont_t(R, M, B).
fmap(F, X, {?MODULE, _IM} = CT) ->
    cont_t(
      fun(CC) ->
             run_cont(X, fun(A) -> CC(F(A)) end, CT)
      end).

-spec '>>='(cont_t(R, M, A), fun((A) -> cont_t(R, M, B)), t(M)) -> cont_t(R, M, B).
'>>='(M, Fun, {?MODULE, _IM} = CT) ->
    cont_t(fun (K) -> run_cont(M, fun (A) -> run_cont(Fun(A), K, CT) end, CT) end).

-spec return(A, t(M)) -> cont_t(_R, M, A).
return(A, {?MODULE, _IM}) ->
    cont_t(fun (K) -> K(A) end).

-spec fail(any(), t(M)) -> cont_t(_R, M, _A).
fail(E, {?MODULE, IM}) ->
    cont_t(fun (_) -> IM:fail(E) end).

-spec lift(monad:monadic(M, A), t(M)) -> cont_t(_R, M, A).
lift(X, {?MODULE, IM}) ->
    cont_t(fun (F) -> IM:'>>='(X, F) end).

-spec callCC(fun((fun( (A) -> cont_t(R, M, _B) ))-> cont_t(R, M, A)), t(M)) -> cont_t(R, M, A).
callCC(F, {?MODULE, _IM} = CT) ->
    cont_t(fun (CC) -> run_cont(F(fun(A) -> cont_t(fun(_) -> CC(A) end) end), CC, CT) end).

-spec reset(cont_t(R, M, R), t(M)) -> cont_t(_NR, M, R).
reset(X, {?MODULE, _IM} = CT) ->
    lift(eval_cont(X, CT), CT).

-spec shift(fun((fun((A) -> monad:monadic(M, R))) -> cont_t(R, M, R)), t(M)) -> cont_t(R, M, A).
shift(F, {?MODULE, _IM} = CT) ->
    cont_t(fun (CC) -> eval_cont(F(CC), CT) end).

-spec run_cont(cont_t(R, M, A), fun((A) -> monad:monadic(M, R)), t(M)) -> monad:monadic(M, R).
run_cont(X, CC, {?MODULE, _IM}) ->
    (run_cont_t(X))(CC).

-spec eval_cont(cont_t(R, M, R), t(M)) -> monad:monadic(M, R).
eval_cont(X, {?MODULE, IM} = CT) ->
    run_cont(X, fun (A) -> IM:return(A) end, CT).

-spec map_cont(fun((monad:monadic(M, R)) -> monad:monadic(M, R)), cont_t(R, M, A), t(M)) -> cont_t(R, M, A).
map_cont(F, X, {?MODULE, _IM} = CT) ->
    cont_t(fun(CC) -> F(run_cont(X, CC, CT)) end).

-spec with_cont(fun((fun((B) -> monad:monadic(M, R))) -> fun((A) -> monad:monadic(M, R))), 
                cont_t(R, M, A), t(M)) -> cont_t(R, M, B).
with_cont(F, X, {?MODULE, _IM} = CT) ->
    cont_t(fun (CC) -> run_cont(X, F(CC), CT) end).

-spec lift_local(fun(() -> monad:monadic(M, R)), 
                 fun((fun((R) -> R), monad:monadic(M, R)) -> monad:monadic(M, R)),
                 fun((R) -> R),
                 cont_t(R, M, A), t(M)) -> cont_t(R, M, A).
lift_local(Ask, Local, F, X, {?MODULE, IM} = CT) ->    
    cont_t(fun (CC) ->
                   do([IM || 
                          R <- Ask(),
                          Local(F, run_cont(X, fun(A) -> Local(fun(_) -> R end, CC(A)) end, CT))
                      ])
           end).
