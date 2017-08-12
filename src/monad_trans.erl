%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.

-module(monad_trans).
-compile({parse_transform, do}).
-export_type([monad_trans/2]).

-type monad_trans(T, M) :: {T, M}.

-export([lift/2]).

%% functor primitives
-callback fmap(fun((A) -> B), monad:monadic(TM, A), TM) -> monad:monadic(TM, B) when TM :: monad:monad().

%% Monad primitives
-callback '>>='(monad:monadic(M, A), fun( (A) -> monad:monadic(M, B) ), M) ->
 monad:monadic(M, B) when M :: monad:monad().
-callback return(A, M) -> monad:monadic(M, A) when M :: monad:monad().
-callback fail(any(), M) -> monad:monadic(M, _A) when M :: monad:monad().

%% Lift a computation form the argument monad to the constructed
%% monad.
-callback lift(monad:monadic(M, A), M) ->
    monad:monadic(monad_trans(T, M), A) when T :: module(), M :: monad:monad().

-spec lift(monad_trans(T, M), monad:monadic(M, A)) -> 
                  monad:monadic(monad_trans(T, M), A) 
                      when M :: monad:monad(), T :: module().
lift({T, _IM} = M, X) ->
    T:lift(X, M).
