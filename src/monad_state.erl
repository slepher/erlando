%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.

-module(monad_state).

-compile({parse_transform, do}).
-compile({no_auto_import, [get/0, get/1, put/1, put/2]}).

-callback get() -> monad:monadic(M, _S)  when M :: monad:monad().
-callback put(_S)  -> monad:monadic(M, ok)  when M :: monad:monad().
-callback state(fun((S) -> {A, S})) -> monad:monadic(M, A)  when M :: monad:monad().

-export([get/0, put/1, state/1]).
-export([get/1, put/2, state/2]).
-export([gets/1, modify/1]).
-export([gets/2, modify/2]).
-export([default_get/1, default_put/2, default_state/2]).

get() ->
    undetermined:new(fun(Module) -> monad_state:get(Module) end).

put(S) ->
    undetermined:new(fun(Module) -> monad_state:put(S, Module) end).

state(F) ->
    undetermined:new(fun(Module) -> monad_state:state(F, Module) end).

get(Module) ->
    monad_trans:apply_fun(get, [], Module).

put(S, Module) ->
    monad_trans:apply_fun(put, [S], Module).

state(F, Module) ->
    monad_trans:apply_fun(state, [F], Module).

gets(F) ->
    gets(F, monad_state).

gets(F, MonadState) ->
    functor:fmap(F, get(MonadState)).

modify(F) ->
    modify(F, monad_state).

modify(F, Module) ->
    state(fun(S) -> {ok, F(S)} end, Module).

default_get(MonadState) ->
    state(fun(S) -> {S, S} end, MonadState).

default_put(S, MonadState) ->
    state(fun(_) -> {ok, S} end, MonadState).

default_state(F, MonadState) ->
    do([monad ||
           S <- get(MonadState),
           {A, NS} = (F(S)),
           put(NS, MonadState),
           monad:return(A, MonadState)
       ]).
