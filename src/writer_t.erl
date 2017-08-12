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

-module(writer_t).
-compile({parse_transform, do}).

-export_type([writer_t/3]).

-type writer_t(W, M, A) :: {writer_t, inner_writer_t(W, M, A)}.
-type inner_writer_t(W, M, A) :: monad:monadic(M, {A, [W]}).
-type t(M) :: {writer_t, M}.

-behaviour(monad_trans).
-behaviour(monad_plus_trans).
-export([new/1, writer_t/1, run_writer_t/1]).
-export([fmap/3]).
-export([mzero/1, mplus/3]).
-export(['>>='/3, return/2, fail/2, lift/2]).
-export([tell/2, listen/2, listens/3, pass/2, censor/3]).
-export([exec_writer/2, map_writer/3]).

-spec new(M) -> t(M) when M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec writer_t(inner_writer_t(W, M, A)) -> writer_t(W, M, A).
writer_t(Inner) ->
    {?MODULE, Inner}.

-spec run_writer_t(writer_t(W, M, A)) -> inner_writer_t(W, M, A).
run_writer_t({?MODULE, Inner}) ->
    Inner;
run_writer_t(Other) ->
    exit({invalid_writer_t, Other}).

-spec fmap(fun((A) -> B), writer_t(W, M, A), t(M)) -> writer_t(W, M, B).
fmap(F, X, {?MODULE, IM} = WT) ->
    map_writer(
      fun(WIM) ->
              do([IM ||
                     {A, Ws} <- WIM,
                     return({F(A), Ws})
                 ])
      end, X, WT).

-spec '>>='(writer_t(W, M, A), fun((A) -> writer_t(W, M, B) ), M) -> writer_t(W, M, B).
'>>='(X, Fun, {?MODULE, IM}) -> 
    writer_t(
      do([IM || {A, LogsA} <- run_writer_t(X),
                {B, LogsB} <- run_writer_t(Fun(A)),
                return({B, LogsA ++ LogsB})
       ])).

-spec return(A, M) -> writer_t(_W, M, A).
return(A, {?MODULE, M}) -> 
    writer_t(M:return({A, []})).

-spec fail(any(), M) -> writer_t(_W, M, _A).
fail(X, {?MODULE, M}) ->
    writer_t(M:fail(X)).

-spec lift(monad:monadic(M, A), t(M)) -> writer_t(_W, M, A).
lift(X, {?MODULE, IM}) ->
    do([IM || 
           A <- X,
           return({A, []})
       ]).

-spec mzero(t(M)) -> writer_t(_W, M, _A).
mzero({?MODULE, IM}) ->
    writer_t(monad_plus:mzero(IM)).

-spec mplus(writer_t(W, M, A), writer_t(W, M, A), t(M)) -> writer_t(W, M, A).
mplus(WA, WB, {?MODULE, IM}) ->
    writer_t(
      monad_plus:mplus(IM, run_writer_t(WA), run_writer_t(WB))
     ).
      

-spec tell([W], t(M)) -> writer_t(W, M, ok).
tell(X, {?MODULE, M}) ->
    writer_t(M:return({ok, X})).

-spec listen(writer_t(W, M, A), t(M)) -> writer_t(W, M, {A, [W]}).
listen(X, {?MODULE, _IM} = WT) ->
    listens(fun(Ws) -> Ws end, X, WT).

-spec listens(fun(([W]) -> B), writer_t(W, M, A), t(M)) -> writer_t(W, M, {A, B}).
listens(F, X, {?MODULE, IM} = WT) ->
    map_writer(
      fun(WIM) ->
              do([IM || 
                     {A, Ws} <- WIM,
                     return({{A, F(Ws)}, Ws})
                 ])
      end, X, WT).

-spec pass(writer_t(W, M, {A, fun(([W]) -> [W])}), t(M)) -> writer_t(W, M, A).
pass(X, {?MODULE, IM} = WT) ->
    map_writer(
      fun(WIM) ->
              do([IM ||
                     {{A, F}, Ws} <- WIM,
                     return({A, F(Ws)})
                 ])
      end, X, WT).

-spec censor(fun(([W]) -> [W]), writer_t(W, M, A), t(M)) -> writer_t(W, M, A).
censor(F, X, {?MODULE, IM} = WT) ->
    map_writer(
      fun(WIM) ->
              do([IM ||
                     {A, Ws} <- WIM,
                     return({A, F(Ws)})
                 ])
      end, X, WT).

-spec exec_writer(writer_t(W, M, _A), t(M)) -> monad:monadic(M, [W]).
exec_writer(X, {?MODULE, IM}) ->
    do([IM || 
           {_A, Ws} <- run_writer_t(X),
           return(Ws)
       ]).

-spec map_writer(fun((monad:monadic(M, {A, [WA]})) -> monad:monadic(N, {B, [WB]})),
                   writer_t(WA, M, A), t(M)) -> writer_t(WB, N, B).
map_writer(F, X, {?MODULE, _IM}) ->
    writer_t(F(run_writer_t(X))).
