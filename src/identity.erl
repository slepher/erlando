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

-compile({parse_transform, monad_t_transform}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_runner).

-export_type([identity/1]).

-export([identity/1]).
-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, '>>'/3, return/2]).
-export([run_nargs/0, run_m/2]).
-export([run/1]).

-transform(#{args => [?MODULE], behaviours => [functor, applicative, monad]}).

-type identity(A) :: {?MODULE, A}.

identity(A) ->
    {?MODULE, A}.

-spec fmap(fun((A) -> B), identity(A)) -> identity(B).
fmap(F, IA, ?MODULE) ->
    two_tuple:fmap(identity, F, IA).

-spec '<$'(B, identity(_A)) -> identity(B).
'<$'(B, IA, ?MODULE) ->
    functor:'default_<$'(B, IA, ?MODULE).

-spec pure(A) -> identity(A).
pure(A, ?MODULE) -> 
    {?MODULE, A}.

-spec '<*>'(identity(fun((A) -> B)), identity(A)) -> identity(B).
'<*>'(IF, IA, ?MODULE) ->
    two_tuple:ap(identity, IF, IA).

-spec lift_a2(fun((A, B) -> C), identity(A), identity(B)) -> identity(C).
lift_a2(F, IA, IB, ?MODULE) ->
    applicative:default_lift_a2(F, IA, IB, ?MODULE).

-spec '*>'(identity(_A), identity(B)) -> identity(B).
'*>'(IA, IB, ?MODULE) ->
    applicative:'default_*>'(IA, IB, ?MODULE).

-spec '<*'(identity(A), identity(_B)) -> identity(A).
'<*'(IA, IB, ?MODULE) ->
    applicative:'default_<*'(IA, IB, ?MODULE).

-spec '>>='(identity(A), fun( (A) -> identity(B) )) -> identity(B).
'>>='(IA, KIB, ?MODULE) -> 
    two_tuple:bind(identity, IA, KIB).

-spec '>>'(identity(_A), identity(B)) -> identity(B).
'>>'(IA, IB, ?MODULE) ->
    monad:'default_>>'(IA, IB, ?MODULE).

-spec return(A) -> identity(A).
return(A, ?MODULE) ->
    monad:default_return(A, ?MODULE).

run_nargs() ->
    0.

run_m(I, []) ->
    run(I).

run(I) ->
    two_tuple:run(identity, I).
