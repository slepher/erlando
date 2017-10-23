%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.

-module(monad_reader).

-compile({parse_transform, do}).

-export([ask/0, reader/1, local/2, asks/1]).

-callback ask() -> monad:monadic(M, _R) when M :: monad:monad().
-callback local(fun((R) -> R), monad:monadic(M, R)) -> monad:monadic(M, R) when M :: monad:monad().
-callback reader(fun((_R) -> A)) -> monad:monadic(M, A) when M :: monad:monad().


ask() ->
    undetermined:new(fun(Module) -> Module:ask() end).

reader(F) ->
    undetermined:new(fun(Module) -> Module:reader(F) end).

local(F, UA) ->
    undetermined:map(
      fun(Module, MA) ->
              Module:local(F, MA)
      end, UA, ?MODULE).

asks(F) ->
    do([monad || 
           A <- ask(),
           return(F(A))
       ]).
