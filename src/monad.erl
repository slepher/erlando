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

-export([join/2, sequence/2]).
-export([fmap/3]).
-export(['>>='/3, return/2, fail/2]).

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
    do([Monad || Y <- X,
                 Y]).

-spec sequence(M, [monadic(M, A)]) -> monadic(M, [A]).
sequence(Monad, Xs) ->
    sequence(Monad, Xs, []).

sequence(Monad, [], Acc) ->
    do([Monad || return(lists:reverse(Acc))]);
sequence(Monad, [X|Xs], Acc) ->
    do([Monad || E <- X,
                 sequence(Monad, Xs, [E|Acc])]).

-spec fmap(M, fun((A) -> B), monad:monadic(M, A)) -> monad:monadic(M, B) when M :: monad().
fmap({T, _IM} = M, F, X) ->
    T:fmap(F, X, M);
fmap(M, F, X) ->
    M:fmap(F, X).
    
-spec '>>='(M, monad:monadic(M, A), fun((A) -> monad:monadic(M, B))) -> monad:monadic(M, B).
'>>='({T, _IM} = M, X, F) ->
    T:'>>='(X, F, M);
'>>='(M, X, F) ->
    M:'>>='(X, F).

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
