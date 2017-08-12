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
-compile({parse_transform, cut}).

-export_type([monad/0, monadic/2]).

-export([mzero/1, mplus/3]).
-export([guard/2, msum/2, mfilter/3]).

-type monad()         :: module() | {module(), monad()}.
-type monadic(_M, _A) :: any().

%% MonadPlus primitives
-callback mzero() -> monadic(_M, _A).
-callback mplus(monadic(M, A), monadic(M, A)) -> monadic(M, A).

mzero({Trans, _Inner} = M) ->
    Trans:mzero(M);
mzero(M) ->
    M:mzero().

mplus({Trans, _Inner} = M, MA, MB) ->
    Trans:mplus(MA, MB, M);
mplus(M, MA, MB) ->
    M:mplus(MA, MB).

%% Utility functions
-spec guard(M, boolean()) -> monad:monadic(M, _A).
guard(M, true)  -> monad:return(M, ok);
guard(M, false) -> mzero(M).

-spec msum(M, [monadic(M, A)]) -> monadic(M, A).
msum(M, List) ->
    lists:foldr(mplus(M, _, _), mzero(M), List).

-spec mfilter(M, fun( (A) -> boolean() ), monadic(M, A)) -> monadic(M, A).
mfilter(M, Pred, X) ->
    do([M || A <- X, guard(M, Pred(A))]).
