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

-export_type([class/0, m/2]).
-export_type([monad/0, monadic/2]).

-type monad()         :: class().
-type monadic(M, A) :: m(M, A).

-type class() :: {module(), class()} | module().
-type m(_M, _A) :: any(). 

%% Monad primitives
-callback '>>='(monad:m(M, A), fun( (A) -> monad:m(M, B) ), M) -> monad:m(M, B) when M :: monad:class().
-callback '>>'(monad:m(M, _A), monad:m(M, B), M) -> monad:m(M, B) when M :: monad:class().
-callback return(A, M) -> monad:m(M, A) when M :: monad:class(). 

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

-transform(#{args => [?MODULE], functions => ['>>='/2, '>>'/2, return/1]}).
-transform(#{args => [?MODULE], functions => [bind/2, then/2, join/1, lift_m/2]}).

% depricated functions
-export([sequence/2, map_m/3]).

-spec '>>='(monad:m(M, A), fun((A) -> monad:m(M, B)), M) -> monad:m(M, B) when M :: monad:class().
'>>='(UA, KUB, UMonad) ->
    undetermined:map(
      fun(Monad, MA) ->
              KMB = fun(A) -> undetermined:run(KUB(A), Monad) end,
              'do_>>='(MA, KMB, Monad)
      end, UA, UMonad).

-spec '>>'(monad:m(M, _A), monad:m(M, B), M) -> monad:m(M, B).
'>>'(UA, UB, UMonad) ->
    undetermined:map_pair(
      fun(Monad, MA, MB) ->
              typeclass_trans:apply('>>', [MA, MB], Monad, ?MODULE)
      end, UA, UB, UMonad).

-spec return(A, M) -> monad:m(M, A) when M :: monad:class().
return(A, UMonad) ->
    undetermined:new(
      fun(Monad) ->
              typeclass_trans:apply(return, [A], Monad, ?MODULE)
      end, UMonad).

-spec 'default_>>'(m(M, _A), m(M, B), M) -> m(M, B).
'default_>>'(MA, MB, Monad) ->
    'do_>>='(MA, fun(_) -> MB end, Monad).

-spec default_return(A, M) -> monad:m(M, A) when M :: monad:class().
default_return(A, Monad) ->
    applicative:pure(A, Monad).

-spec bind(monad:m(M, A), fun((A) -> monad:m(M, B)), M) -> monad:m(M, B) when M :: monad:class().
bind(X, F, Monad) ->
    '>>='(X, F, Monad).

-spec then(monad:m(M, _A), monad:m(M, B), M) -> monad:m(M, B).
then(X, F, Monad) ->
    '>>'(X, F, Monad).

-spec join(m(M, m(M, A)), M) -> m(M, A).
join(MMA, Monad) ->
    bind(MMA, fun(MA) -> MA end, Monad).

-spec lift_m(fun((A) -> B), monad:m(M, A), M) -> monad:m(M, B) when M :: monad:class().
lift_m(F, MA, Monad) ->
    do([Monad || 
           A <- MA,
           return(F(A))
       ]).

as(A, {T, M}) ->
    monad_trans:lift(as(A, M), {T, M});
as(A, M) ->
    monad:return(A, M).

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
    typeclass_trans:apply('>>=', [MA, KMB], Monad, ?MODULE).


%% traversable functions
-spec sequence(M, [m(M, A)]) -> m(M, [A]).
sequence(Monad, Xs) ->
    map_m(Monad, fun(X) -> X end, Xs).

-spec map_m(M, fun((A) -> monad:m(M, B)), [A]) -> monad:m(M, [B]).
map_m(Monad, F, [X|Xs]) ->
    do([Monad ||
           A <- F(X),
           As <- map_m(Monad, F, Xs),
           return([A|As])
       ]);
map_m(Monad, _F, []) ->
    return(Monad, []).

