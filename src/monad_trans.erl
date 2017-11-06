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

-superclass([monad]).

-export_type([monad_trans/2]).

-type monad_trans(T, M) :: {T, M}.

-callback lift(monad:monadic(M, A), M) -> monad:monadic(monad_trans(T, M), A) when T :: module(), M :: monad:monad().

-export([lift/2]).

-transform({?MODULE, [?MODULE], [lift/1]}).

-spec lift(monad_trans(T, M), monad:monadic(M, A)) -> monad:monadic(monad_trans(T, M), A) when M :: monad:monad(), T :: module().
lift(UA, UMonadTrans) ->
    undetermined:new(
      fun(MonadTrans) when is_atom(MonadTrans) ->
              MonadTrans:lift(UA);
         ({MonadTrans, UMonad}) ->
              undetermined:map(
                fun(Monad, MA) ->
                        MonadTrans:lift(MA, {MonadTrans, Monad})
                end, UA, UMonad)
      end, UMonadTrans).
