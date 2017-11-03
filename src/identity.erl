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

-erlando_type(?MODULE).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_runner).

-export_type([identity/1]).

-export([identity/1]).
-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([run_nargs/0, run_m/2]).
-export([run/1]).

-type identity(A) :: {?MODULE, A}.

identity(A) ->
    {?MODULE, A}.

-spec fmap(fun((A) -> B), identity(A)) -> identity(B).
fmap(F, IA) ->
    two_tuple:fmap(identity, F, IA).

-spec '<$'(B, identity(_A)) -> identity(B).
'<$'(B, IA) ->
    functor:'default_<$'(B, IA, ?MODULE).

-spec pure(A) -> identity(A).
pure(A) -> 
    {?MODULE, A}.

-spec '<*>'(identity(fun((A) -> B)), identity(A)) -> identity(B).
'<*>'(IF, IA) ->
    two_tuple:ap(identity, IF, IA).

-spec lift_a2(fun((A, B) -> C), identity(A), identity(B)) -> identity(C).
lift_a2(F, IA, IB) ->
    applicative:default_lift_a2(F, IA, IB, ?MODULE).

-spec '*>'(identity(_A), identity(B)) -> identity(B).
'*>'(IA, IB) ->
    applicative:'default_*>'(IA, IB, ?MODULE).

-spec '<*'(identity(A), identity(_B)) -> identity(A).
'<*'(IA, IB) ->
    applicative:'default_<*'(IA, IB, ?MODULE).

-spec '>>='(identity(A), fun( (A) -> identity(B) )) -> identity(B).
'>>='(IA, KIB) -> 
    two_tuple:bind(identity, IA, KIB).

-spec '>>'(identity(_A), identity(B)) -> identity(B).
'>>'(IA, IB) ->
    monad:'default_>>'(IA, IB, ?MODULE).

-spec return(A) -> identity(A).
return(A) ->
    monad:default_return(A, ?MODULE).

run_nargs() ->
    0.

run_m(I, []) ->
    run(I).

run(I) ->
    two_tuple:run(identity, I).
