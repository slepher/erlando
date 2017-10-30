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

-module(state_t).
-behaviour(type).
-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_fail).
-behaviour(monad_state).
-behaviour(monad_reader).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monad_runner).

-compile({parse_transform, do}).

-include("op.hrl").

-export_type([state_t/3]).

-compile({no_auto_import, [get/1, put/2]}).

-export([type/0]).
-export([new/1, state_t/1, run_state_t/1]).
% impl of functor.
-export([fmap/2, '<$'/2]).
% impl of applcative.
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export([pure/2]).
% impl of monad.
-export(['>>='/2, '>>'/2, return/1]).
-export([return/2]).
% impl of monad_trans.
-export([lift/1]).
% impl of monad_fail.
-export([fail/1]).
-export([fail/2]).
% impl of monad_state.
-export([get/0, put/1, state/1]).
-export([get/1, put/2, state/2]).
% impl of moand_reader.
-export([ask/0, reader/1, local/2]).
-export([ask/1, reader/2]).
% impl of alternative.
-export([empty/0, '<|>'/2]).
-export([empty/1]).
% impl of monad_plus.
-export([mzero/0, mplus/2]).
-export([mzero/1]).
% impl of monad_runner.
-export([run_nargs/0, run_m/2]).
%% state related functions
-export([eval/2, exec/2, run/2, map/2, with/2]).


-type state_t(S, M, A) :: {state_t, inner_t(S, M, A)}.
-type inner_t(S, M, A) :: fun((S) -> monad:monadic(M, {A, S})).

-type t(M) :: {state_t, M}.

type() ->
    type:default_type(?MODULE).

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(Inner) ->
    {?MODULE, Inner}.

-spec state_t(inner_t(S, M, A)) -> state_t(S, M, A).
state_t(Inner) ->
    {?MODULE, Inner}.

-spec run_state_t(state_t(S, M, A)) -> inner_t(S, M, A).
run_state_t({?MODULE, Inner}) ->
    Inner;
run_state_t({undetermined, _} = U) ->
    run_state_t(undetermined:run(U, ?MODULE));
run_state_t(Other) ->
    exit({invalid_state_t, Other}).

-spec fmap(fun((A) -> B), state_t(S, M, A)) -> state_t(S, M, B).
fmap(F, STA) ->
    map(
      fun(FA) ->
              fun({A, S}) -> {F(A), S} end /'<$>'/ FA
      end, STA).

'<$'(B, STA) ->
    functor:'default_<$'(B, STA, ?MODULE).

-spec pure(A) -> state_t(_S, _M, A).
pure(A) ->
    return(A).

-spec pure(A, t(M)) -> state_t(_S, M, A).
pure(A, IM) ->
    return(A, IM).

-spec '<*>'(state_t(S, M, fun((A) -> B)),  state_t(S, M, A)) -> state_t(S, M, B).
'<*>'(STF, STA) ->
    state_t(
      fun(S) ->              
              do([monad ||
                     {F, NS} <- run(STF, S),
                     {A, NNS} <- run(STA, NS),
                     return({F(A), NNS})
                 ])
      end).

-spec lift_a2(fun((A, B) -> C), state_t(S, M, A), state_t(S, M, B)) -> state_t(S, M, C).
lift_a2(F, STA, STB) ->
    applicative:default_lift_a2(F, STA, STB, ?MODULE).

-spec '*>'(state_t(S, M, _A), state_t(S, M, B)) -> state_t(S, M, B).
'*>'(STA, STB) ->
    applicative:'default_*>'(STA, STB, ?MODULE).

-spec '<*'(state_t(S, M, A), state_t(S, M, _B)) -> state_t(S, M, A).
'<*'(STA, STB) ->
    applicative:'default_<*'(STA, STB, ?MODULE).

-spec '>>='(state_t(S, M, A), fun( (A) -> state_t(S, M, B))) -> state_t(S, M, B).
'>>='(STA, KSTB) ->
    state_t(
      fun (S) ->
              do([ monad || 
                     {A, NS} <- run(STA, S),
                     run(KSTB(A), NS)
                 ])
        end).

-spec '>>'(state_t(S, M, _A), state_t(S, M, B)) -> state_t(S, M, B).
'>>'(STA, STB) ->
    monad:'default_>>'(STA, STB, ?MODULE).

-spec return(A) -> state_t(_S, _M, A).
return(A) ->
    return(A, monad).

-spec return(A, t(M)) -> state_t(_S, M, A).
return(A, IM) ->
    state(fun (S) -> {A, S} end, IM).

-spec lift(monad:monadic(M, A)) -> state_t(_S, M, A).
lift(MA) ->
    state_t(
      fun(S) ->
              functor:fmap(fun(A) -> {A, S} end, MA)
      end).

-spec fail(_E) -> state_t(_S, _M, _A).
fail(E) ->
    fail(E, monad_fail).

fail(E, IM) ->
    state_t(fun(_) -> monad_fail:fail(E, IM) end).

-spec get() -> state_t(S, _M, S).
get() ->
    state(fun (S) -> {S, S} end).

get(IM) ->
    state(fun(S) -> {S, S} end, IM).

-spec put(S) -> state_t(S, _M, ok).
put(S) ->
    state(fun (_) -> {ok, S} end).

put(S, IM) ->
    state(fun (_) -> {ok, S} end, IM).

-spec state(fun((S) -> {A, S})) -> state_t(S, _M, A).
state(F) ->
    state_t(fun (S) -> applicative:pure(F(S)) end).

state(F, IM) ->
    state_t(fun (S) -> monad:return(F(S), IM) end).

ask() ->
    ask(monad_reader).

ask(IM) ->
    lift(monad_reader:ask(IM)).

reader(F) ->
    reader(F, monad_reader).

reader(F, IM) ->
    lift(monad_reader:reader(F, IM)).

local(F, STA) ->
    map(
      fun(MA) ->
              monad_reader:local(F, MA)
      end, STA).

empty() ->
    mzero().
    
empty(IM) ->
    mzero(IM).

'<|>'(STA, STB) ->
    mplus(STA, STB).

mzero() ->
    mzero(monad_plus).

mzero(IM) ->
    state_t(fun(_) -> monad_plus:mzero(IM) end).

mplus(STA, STB) ->
    state_t(
      fun(S) ->
              monad_plus:mplus(run(STA, S), run(STB, S))
      end).

run_nargs() ->
    1.

run_m(STA, [S]) ->
    run(STA, S).


-spec eval(state_t(S, M, A), S) -> monad:monadic(M, A).
eval(STA, S) ->    
    fun({A, _}) -> A end /'<$>'/ run(STA, S).

-spec exec(state_t(S, M, _A), S) -> monad:monadic(M, S).
exec(STA, S) ->
    fun({_, NS}) -> NS end /'<$>'/ run(STA, S).

-spec run(state_t(S, M, A), S) -> monad:monadic(M, {A, S}).
run(STA, S) -> (run_state_t(STA))(S).

-spec map(fun((monad:monadic(M, {A, S})) -> monad:monadic(N, {B, S})), state_t(S, M, A)) -> state_t(S, N, B).
map(F, STA) ->
    state_t(fun (S) -> F(run(STA, S)) end).

-spec with(fun((S) -> S), state_t(S, M, A)) -> state_t(S, M, A).
with(F, STA) ->
    state_t(fun (S) -> run(STA, F(S)) end).

