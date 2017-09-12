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

-export([join/2, sequence/2, map_m/3, lift_m/3]).
-export([fmap/3]).
%% bind is same as >>=, then is same as >> 
-export([bind/3, then/3]).
-export(['>>='/3, '>>'/3, return/2, fail/2]).

-type monad()         :: module() | {module(), monad()}.
-type monadic(_M, _A) :: any().

%% functor primitives
-callback fmap(fun((A) -> B), monad:monadic(M, A)) -> monad:monadic(M, B) when M :: monad:monad().

%% Monad primitives
-callback '>>='(monadic(M, A), fun( (A) -> monadic(M, B) )) -> monadic(M, B) when M :: monad().
-callback return(A) -> monadic(M, A) when M :: monad().
-callback fail(any()) -> monadic(M, _A) when M :: monad().

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
           A <- X,
           As <- map_m(Monad, F, Xs),
           return([F(A)|As])
       ]);
map_m(Monad, _F, []) ->
    return(Monad, []).

-spec lift_m(M, fun((A) -> B), monad:monadic(M, A)) -> monad:monadic(M, B) when M :: monad().
lift_m(Monad, F, X) ->
    do([Monad || 
           A <- X,
           return(F(A))
       ]).

-spec fmap(M, fun((A) -> B), monad:monadic(M, A)) -> monad:monadic(M, B) when M :: monad().
fmap({T, _IM} = M, F, X) ->
    T:fmap(F, X, M);
fmap(M, F, X) ->
    M:fmap(F, X).

-spec bind(M, monad:monadic(M, A), fun((A) -> monad:monadic(M, B))) -> monad:monadic(M, B).
bind(Monad, X, F) ->
    '>>='(Monad, X, F).

-spec then(M, monad:monadic(M, _A), monad:monadic(M, B)) -> monad:monadic(M, B).
then(Monad, Xa, Xb) ->
    '>>'(Monad, Xa, Xb).
    
-spec '>>='(M, monad:monadic(M, A), fun((A) -> monad:monadic(M, B))) -> monad:monadic(M, B).
'>>='({T, _IM} = M, X, F) ->
    T:'>>='(X, F, M);
'>>='(M, X, F) ->
    M:'>>='(X, F).

-spec '>>'(M, monad:monadic(M, _A), monad:monadic(M, B)) -> monad:monadic(M, B).
'>>'(Monad, Xa, Xb) ->
    '>>='(Monad, Xa, fun(_) -> Xb end).

-spec return(M, A) -> monad:monadic(M, A) when M :: monad().
return({T, _IM} = M, A) ->
    T:return(A, M);
return(M, A) ->
    M:return(A).

-spec fail(M, _E) -> monad:monadic(M, _A) when M :: monad().
fail({T, _IM} = M, E) ->
    T:fail(E, M);
fail(M, E) ->
    M:fail(E).
