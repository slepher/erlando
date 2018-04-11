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

-callback lift(monad:m(M, A), M) -> monad:m(monad_trans(T, M), A) when T :: module(), M :: monad:class().

-export([lift/2]).

-transform(#{args => [?MODULE], functions => [lift/1]}).

-spec lift(monad:m(M, A), monad_trans(T, M)) -> monad:m(monad_trans(T, M), A) when M :: monad:class(), T :: module().
lift(UA, UMonadTrans) ->
    undetermined:new(
      fun(MonadTrans) when is_atom(MonadTrans) ->
              do_lift(UA, MonadTrans);
         ({MonadTrans, UMonad}) ->
              undetermined:map0(
                fun(Monad, MA) ->
                        do_lift(MA, {MonadTrans, Monad})
                end, UA, UMonad)
      end, UMonadTrans).

do_lift(UMA, MonadTrans) ->
    typeclass_trans:apply(lift, [UMA], MonadTrans, ?MODULE).
