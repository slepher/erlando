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

-superclass([applicative]).

-export_type([monad/0, monadic/2]).

-type monad()         :: module() | {module(), monad()}.
-type monadic(_M, _A) :: any().

%% Monad primitives
-callback return(A) -> monadic(M, A) when M :: monad(). 
-callback '>>='(monadic(M, A), fun( (A) -> monadic(M, B) )) -> monadic(M, B) when M :: monad().
-callback '>>'(monadic(M, _A), monadic(M, B) ) -> monadic(M, B) when M :: monad().

-compile({parse_transform, do}).
-compile({parse_transform, monad_t_transform}).

-include("functor.hrl").
-include("applicative.hrl").

-export(['>>='/3, '>>'/3, return/2]).
-export(['default_>>'/3, default_return/2]).
%% monad utility functions
-export([bind/3, then/3, join/2, lift_m/3]).
%% utility function join
-export([as/2, empty/1, run/2, id/1]).

-transform({?MODULE, [?MODULE], ['>>='/2, '>>'/2, return/1]}).
-transform({?MODULE, [?MODULE], [bind/2, then/2, join/1, lift_m/2]}).

% depricated functions
-export([sequence/2, map_m/3]).

'>>='(UA, KUB, UMonad) ->
    undetermined:map(
      fun(Monad, MA) ->
              KMB = fun(A) -> undetermined:run(KUB(A), Monad) end,
              'do_>>='(MA, KMB, Monad)
      end, UA, UMonad).

'>>'(UA, UB, UMonad) ->
    undetermined:map_pair(
      fun(Monad, MA, MB) ->
              typeclass_trans:apply('>>', [MA, MB], Monad)
      end, UA, UB, UMonad).

-spec return(M, A) -> monad:monadic(M, A) when M :: monad().
return(A, UMonad) ->
    undetermined:new(
      fun(Monad) ->
              typeclass_trans:apply(return, [A], Monad)
      end, UMonad).

-spec 'default_>>'(monadic(M, _A), monadic(M, B), module()) -> monadic(M, B).
'default_>>'(MA, MB, Monad) ->
    'do_>>='(MA, fun(_) -> MB end, Monad).

default_return(A, Monad) ->
    applicative:pure(A, Monad).

-spec bind(monad:monadic(M, A), fun((A) -> monad:monadic(M, B)), M) -> monad:monadic(M, B) when M :: monad:monad().
bind(X, F, Monad) ->
    '>>='(X, F, Monad).

-spec then(monad:monadic(M, _A), monad:monadic(M, B), M) -> monad:monadic(M, B).
then(X, F, Monad) ->
    '>>'(X, F, Monad).

-spec join(monadic(M, monadic(M, A))) -> monadic(M, A).
join(MMA, Monad) ->
    bind(MMA, fun(MA) -> MA end, Monad).

-spec lift_m(M, fun((A) -> B), monad:monadic(M, A)) -> monad:monadic(M, B) when M :: monad().
lift_m(F, MA, Monad) ->
    do([Monad || 
           A <- MA,
           return(F(A))
       ]).

as(A, {T, M}) ->
    T:lift(as(A, M));
as(A, M) ->
    M:return(A).

id(Monad) ->
    as(fun(A) -> A end, Monad).

empty(Monad) ->
    as(ok, Monad).

run(M, Monad) ->
    applicative:ap(id(Monad), M).

%%%===================================================================
%%% Internal functions
%%%===================================================================
'do_>>='(MA, KMB, Monad) ->
    typeclass_trans:apply('>>=', [MA, KMB], Monad).


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

