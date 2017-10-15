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

-module(monad).
-compile({parse_transform, do}).
-export_type([monad/0, monadic/2]).

-export(['>>='/2, '>>'/2, return/1]).
%% bind is same as >>=, then is same as >> 
-export([bind/2, then/2]).
%% utility function join
-export([join/1]).
-export([as/2, empty/1, run/2, id/1]).

% depricated functions
-export([sequence/2, map_m/3, lift_m/3]).
-export(['>>='/3, return/2, fail/2]).

-type monad()         :: module() | {module(), monad()}.
-type monadic(_M, _A) :: any().

%% Monad primitives
-callback '>>='(monadic(M, A), fun( (A) -> monadic(M, B) )) -> monadic(M, B) when M :: monad().
-callback return(A) -> monadic(M, A) when M :: monad(). 

'>>='(X, F) ->
    undetermined:unwrap(undetermined:'>>='(undetermined:wrap(X), fun(A) -> undetermined:wrap(F(A)) end)).

-spec return(A) -> monad:monadic(M, A) when M :: monad().
return(A) ->
    undetermined:return(A).

bind(X, F) ->
    '>>='(X, F).

-spec '>>'(monad:monadic(M, _A), monad:monadic(M, B)) -> monad:monadic(M, B).
'>>'(Xa, Xb) ->
    '>>='(Xa, fun(_) -> Xb end).

then(X, F) ->
    '>>'(X, F).

join(MMA) ->
    bind(MMA, fun(MA) -> MA end).

as(A, {T, IM}) when is_atom(T) ->
    T:lift(as(A, IM));
as(A, M) when is_atom(M) ->
    M:return(A).

empty(M) ->
    as({}, M).

run(M, Monad) ->
    applicative:ap(id(Monad), M).

id(Monad) ->
    as(fun(A) -> A end, Monad).

%% depricated functions

%% traversable functions
-spec sequence(M, [monadic(M, A)]) -> monadic(M, [A]).
sequence(Monad, Xs) ->
    map_m(Monad, fun(X) -> X end, Xs).

-spec map_m(M, fun((A) -> monad:monadic(M, B)), [A]) -> monad:monadic(M, [B]).
map_m(Monad, F, [X|Xs]) ->
    do([Monad ||
           A <- F(X),
           As <- map_m(Monad, F, Xs),
           return([A|As])
       ]);
map_m(Monad, _F, []) ->
    return(Monad, []).

-spec lift_m(M, fun((A) -> B), monad:monadic(M, A)) -> monad:monadic(M, B) when M :: monad().
lift_m(Monad, F, X) ->
    do([Monad || 
           A <- X,
           return(F(A))
       ]).


%% functions for do transform
-spec '>>='(M, monad:monadic(M, A), fun((A) -> monad:monadic(M, B))) -> monad:monadic(M, B).
'>>='(X, K, monad) ->
    monad:'>>='(X, K);
'>>='({undetermined, _} = UX, K, Monad) ->
    '>>='(undetermined:run(UX, Monad), fun(A) -> undetermined:run(K(A), Monad) end, Monad);
'>>='(X, K, {T, M}) ->
    T:'>>='(X, K, {T, M});
'>>='(X, K, M) ->
    M:'>>='(X, K).

-spec return(M, A) -> monad:monadic(M, A) when M :: monad().
return(A, {T, M}) ->
    T:return(A, {T, M});
return(A, M) ->
    M:return(A).

fail(E, {T, M}) ->
    T:fail(E, {T, M});
fail(E, monad) ->
    monad_fail:fail(E);
fail(E, M) ->
    M:fail(E).
