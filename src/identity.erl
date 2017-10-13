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

-module(identity).
-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).

-export_type([identity/1]).

-export([fmap/2]).
-export(['<*>'/2, pure/1]).
-export(['>>='/2, return/1]).
-export([run_identity/1]).

-type identity(A) :: {identity, A}.

-spec fmap(fun((A) -> B), identity(A)) -> identity(B).
fmap(F, X) ->
    two_tuple:fmap(identity, F, X).

-spec '<*>'(identity(fun((A) -> B)), identity(A)) -> identity(B).
'<*>'(F, A) ->
    two_tuple:ap(identity, F, A).

-spec '>>='(identity(A), fun( (A) -> identity(B) )) -> identity(B).
'>>='(X, Fun) -> 
    two_tuple:bind(identity, X, Fun).

-spec pure(A) -> identity(A).
pure(A) -> return(A).

-spec return(A) -> identity(A).
return(A) -> two_tuple:return(identity, A).

run_identity(I) ->
    two_tuple:run(identity, I).
