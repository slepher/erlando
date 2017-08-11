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

-module(reader_t).
-compile({parse_transform, do}).
-behaviour(monad_trans).
-behaviour(monad_reader_trans).

-export_type([reader_t/3]).

-export([new/1, reader_t/1, run_reader_t/1]).
% impl of functor
-export([fmap/3]).
% impl of monad
-export(['>>='/3, return/2, fail/2]).
% impl of monad trans
-export([lift/2]).
% impl of monad reader
-export([ask/1, asks/2, local/3]).
% monad reader functions
-export([reader/2, run_reader/3, map_reader/3, with_reader/3]).

-opaque reader_t(R, M, A) :: {reader_t, inner_reader_t(R, M, A)}.
-type inner_reader_t(R, M, A) :: fun( (R) -> monad:monadic(M, A)).

-type t(M) :: {reader_t, M}.

-spec new(M) -> t(M) when M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec reader_t(inner_reader_t(R, M, A)) -> reader_t(R, M, A).
reader_t(Inner) ->
    {?MODULE, Inner}.

-spec run_reader_t(reader_t(R, M, A)) -> inner_reader_t(R, M, A).
run_reader_t({?MODULE, Inner}) ->
    Inner;
run_reader_t(Other) ->
    exit({invalid_reader_t, Other}).

-spec fmap(fun((A) -> B), reader_t(R, M, A), t(M)) -> reader_t(R, M, B).
fmap(F, X, {?MODULE, IM} = RT) ->
    map_reader(
      fun(RIM) ->
              IM:fmap(F, RIM)
      end, X, RT).

-spec '>>='(reader_t(R, M, A), fun( (A) -> reader_t(R, M, B) ), t(M)) -> reader_t(R, M, B).
'>>='(X, Fun, {?MODULE, IM} = RT) ->
    reader_t(
      fun(R) ->
              do([IM || 
                     A <- run_reader(X, R, RT),
                     run_reader(Fun(A), R, RT)
               ])
      end).

-spec return(A, t(M)) -> reader_t(_R, M, A).
return(A, {?MODULE, IM}) ->
    reader_t(fun (_) -> IM:return(A) end).

-spec fail(any(), t(M)) -> reader_t(_R, M, _A).
fail(E, {?MODULE, M}) ->
    reader_t(fun (_) -> M:fail(E) end).

-spec lift(monad:monadic(M, A), t(M)) -> reader_t(_R, M, A).
lift(X, {?MODULE, _M}) ->
    reader_t(fun(_) -> X end).

-spec ask(M) -> reader_t(R, M, R).
ask({?MODULE, IM}) ->
    reader_t(fun(R) -> IM:return(R) end).

-spec asks(fun((R) -> A), t(M)) -> reader_t(R, M, A).
asks(F, {?MODULE, _IM} = RT) ->
    reader(F, RT).

-spec local(fun( (R) -> R), reader_t(R, M, A), t(M)) -> reader_t(R, M, A).
local(F, RA, {?MODULE, _IM} = RT) ->
    with_reader(F, RA, RT).

-spec reader(fun( (R) -> A), t(M)) -> reader_t(R, M, A).
reader(F, {?MODULE, IM}) ->
    reader_t(fun(R) -> IM:return(F(R)) end).

-spec run_reader(reader_t(R, M, A), R, t(M)) -> monad:monadic(M, A).
run_reader(MR, R, {?MODULE, _IM}) ->
    (run_reader_t(MR))(R).
    
-spec map_reader(fun((monad:monadic(M, R)) -> monad:monadic(N, R)), reader_t(R, M, A), t(M)) -> reader_t(R, N, A).
map_reader(F, MR, {?MODULE, _IM} = RT) ->
    reader_t(fun(R) -> F(run_reader(MR, R, RT)) end).

-spec with_reader(fun((NR) -> R), reader_t(NR, M, A), t(M)) -> reader_t(R, M, A).
with_reader(F, MR, {?MODULE, _IM} = RT) ->
    reader_t(fun(R) -> run_reader(MR, F(R), RT) end).
