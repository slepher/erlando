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
-export([join/2, sequence/2, map_m/3, lift_m/3]).
-export([bind/3, then/3]).
-export(['>>='/3, '>>'/3, return/2]).

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

%% Utility functions
-spec join(M, monadic(M, monadic(M, A))) -> monadic(M, A).
join(Monad, X) ->
    bind(Monad, X, fun(Y) -> Y end). 

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
    
-spec '>>='(M, monad:monadic(M, A), fun((A) -> monad:monadic(M, B))) -> monad:monadic(M, B).
'>>='(X, F, {T, _IM} = M) ->
    T:'>>='(undetermined:run(X, T), F, M);
'>>='(X, F, M) ->
    M:'>>='(undetermined:run(X, M), F).

-spec return(M, A) -> monad:monadic(M, A) when M :: monad().
return(A, {T, IM}) when is_atom(T) ->
    T:lift(return(A, IM), {T, IM});
return(A, M) when is_atom(M) ->
    M:return(A).

-spec '>>'(M, monad:monadic(M, _A), monad:monadic(M, B)) -> monad:monadic(M, B).
'>>'(Xa, Xb, Monad) ->
    '>>='(Monad, Xa, fun(_) -> Xb end).

-spec bind(M, monad:monadic(M, A), fun((A) -> monad:monadic(M, B))) -> monad:monadic(M, B).
bind(Monad, X, F) ->
    '>>='(Monad, X, F).

-spec then(M, monad:monadic(M, _A), monad:monadic(M, B)) -> monad:monadic(M, B).
then(Monad, Xa, Xb) ->
    '>>'(Monad, Xa, Xb).
