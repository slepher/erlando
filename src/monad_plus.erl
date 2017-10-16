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

-module(monad_plus).
-compile({parse_transform, do}).

-export([mzero/0, mplus/2]).
-export([guard/1, msum/1, mfilter/2]).

%% functions for old monad transform functions
-export([mzero/1, mplus/3]).

%% MonadPlus primitives
-callback mzero() -> monad:monadic(_M, _A).
-callback mplus(monad:monadic(M, A), monad:monadic(M, A)) -> monad:monadic(M, A).

mzero() ->
    undetermined:undetermined(fun(Module) -> Module:mzero() end).


mplus({undetermined, _} = UA, UB) ->
    undetermined:map_undetermined(
      fun(Module, MB) ->
              MA = undetermined:run(UA, Module),
              Module:mplus(MA, MB)
      end, UB);
mplus(UA, UB) ->
    undetermined:map_undetermined(
      fun(Module, MA) ->
              MB = undetermined:run(UB, Module),
              Module:mplus(MA, MB)
      end, UA).


%% Utility functions
-spec guard(boolean()) -> monad:monadic(_M, _A).
guard(true)  -> monad:return(ok);
guard(false) -> mzero().

-spec msum([monad:monadic(M, A)]) -> monad:monadic(M, A).
msum(List) ->
    lists:foldr(fun mplus/2, mzero(), List).

-spec mfilter(fun( (A) -> boolean() ), monad:monadic(M, A)) -> monad:monadic(M, A).
mfilter(Pred, X) ->
    do([monad || A <- X, guard(Pred(A))]).

mzero({T, M}) ->
    T:mzero(M);
mzero(M) ->
    M:mzero().

mplus(MA, MB, {T, M}) ->
    T:mplus(MA, MB, M);
mplus(MA, MB, M) ->
    M:mplus(MA, MB).
