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

-erlando_type({?MODULE, [state_t/3]}).

-export_type([state_t/3]).

-type state_t(S, M, A) :: {state_t, inner_t(S, M, A)}.
-type inner_t(S, M, A) :: fun((S) -> monad:m(M, {A, S})).
-type t(M) :: monad_trans:monad_trans(?MODULE, M).

-compile({parse_transform, do}).
-compile({parse_transform, monad_t_transform}).
-compile({no_auto_import, [get/1, put/2]}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_state).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monad_runner).

-include("op.hrl").
-include("erlando.hrl").

-export([new/1, state_t/1, run_state_t/1]).
% impl of functor.
-export([fmap/3, '<$'/3]).
% impl of applcative.
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
% impl of monad.
-export(['>>='/3, '>>'/3, return/2]).
% impl of monad_trans.
-export([lift/2]).
% impl of monad_state.
-export([get/1, put/2, state/2]).
% impl of alternative.
-export([empty/1, '<|>'/3]).
% impl of monad_plus.
-export([mzero/1, mplus/3]).
% impl of monad_runner.
-export([run_nargs/0, run_m/2]).
%% state related functions
-export([map/3, with/3]).
-export([eval/3, exec/3, run/3]).

-transform(#{inner_type => functor,    behaviours => [functor]}).
-transform(#{inner_type => monad,      behaviours => [applicative]}).
-transform(#{inner_type => monad,      behaviours => [monad, monad_trans, monad_state]}).
-transform(#{inner_type => monad_plus, behaviours => [alternative, monad_plus]}).
-transform(#{args => monad,            functions => [map/2, with/2]}).
-transform(#{args => monad,            functions => [eval/2, exec/2, run/2]}).

-spec new(M) -> TM when TM :: monad:class(), M :: monad:class().
new(Inner) ->
    {?MODULE, Inner}.

-spec state_t(inner_t(S, M, A)) -> state_t(S, M, A).
state_t(Inner) ->
    {?MODULE, Inner}.

-spec run_state_t(state_t(S, M, A)) -> inner_t(S, M, A).
run_state_t({?MODULE, Inner}) ->
    Inner;
run_state_t(#undetermined{} = U) ->
    run_state_t(undetermined:run(U, ?MODULE));
run_state_t(Other) ->
    exit({invalid_t, Other}).

-spec fmap(fun((A) -> B), state_t(S, M, A), t(M)) -> state_t(S, M, B).
fmap(F, STA, {?MODULE, IM}) ->
    map(
      fun(FA) ->
              functor:fmap(fun({A, S}) -> {F(A), S} end, FA, IM)
      end, STA).

'<$'(B, STA, {?MODULE, _IM} = ST) ->
    functor:'default_<$'(B, STA, ST).

-spec pure(A, t(M)) -> state_t(_S, M, A).
pure(A, {?MODULE, _IM} = ST) ->
    return(A, ST).

-spec '<*>'(state_t(S, M, fun((A) -> B)),  state_t(S, M, A)) -> state_t(S, M, B).
'<*>'(STF, STA, {?MODULE, IM}) ->
    state_t(
      fun(S) ->              
              do([IM ||
                     {F, NS} <- run(STF, S),
                     {A, NNS} <- run(STA, NS),
                     return({F(A), NNS})
                 ])
      end).

-spec lift_a2(fun((A, B) -> C), state_t(S, M, A), state_t(S, M, B)) -> state_t(S, M, C).
lift_a2(F, STA, STB, {?MODULE, _IM} = ST) ->
    applicative:default_lift_a2(F, STA, STB, ST).

-spec '*>'(state_t(S, M, _A), state_t(S, M, B)) -> state_t(S, M, B).
'*>'(STA, STB, {?MODULE, _IM} = ST) ->
    applicative:'default_*>'(STA, STB, ST).

-spec '<*'(state_t(S, M, A), state_t(S, M, _B)) -> state_t(S, M, A).
'<*'(STA, STB, {?MODULE, _IM} = ST) ->
    applicative:'default_<*'(STA, STB, ST).

-spec '>>='(state_t(S, M, A), fun( (A) -> state_t(S, M, B)), t(M)) -> state_t(S, M, B).
'>>='(STA, KSTB, {?MODULE, IM}) ->
    state_t(
      fun (S) ->
              do([ IM || 
                     {A, NS} <- run(STA, S),
                     run(KSTB(A), NS)
                 ])
        end).

-spec '>>'(state_t(S, M, _A), state_t(S, M, B), t(M)) -> state_t(S, M, B).
'>>'(STA, STB, {?MODULE, IM}) ->
    monad:'default_>>'(STA, STB, {?MODULE, IM}).

-spec return(A, t(M)) -> state_t(_S, M, A).
return(A, {?MODULE, _IM} = ST) ->
    state(fun (S) -> {A, S} end, ST).

-spec lift(monad:m(M, A)) -> state_t(_S, M, A).
lift(MA, {?MODULE, IM}) ->
    state_t(
      fun(S) ->
              monad:lift_m(fun(A) -> {A, S} end, MA, IM)
      end).

-spec get(t(M)) -> state_t(S, M, S).
get({?MODULE, _IM} = ST) ->
    monad_state:default_get(ST).

-spec put(S, t(M)) -> state_t(S, M, ok).
put(S, {?MODULE, _IM} = ST) ->
    monad_state:default_put(S, ST).

-spec state(fun((S) -> {A, S}), t(M)) -> state_t(S, M, A).
state(F, {?MODULE, IM}) ->
    state_t(fun (S) -> monad:return(F(S), IM) end).

empty({?MODULE, _IM} = ST) ->
    mzero(ST).

'<|>'(STA, STB, {?MODULE, _IM} = ST) ->
    mplus(STA, STB, ST).

-spec mzero(t(M)) -> state_t(_S, M, _A).
mzero({?MODULE, IM}) ->
    state_t(fun(_) -> monad_plus:mzero(IM) end).

-spec mplus(state_t(S, M, A), state_t(S, M, A)) -> state_t(S, M, A).
mplus(STA, STB, {?MODULE, IM}) ->
    state_t(
      fun(S) ->
              monad_plus:mplus(run(STA, S), run(STB, S), IM)
      end).

-spec eval(state_t(S, M, A), S) -> monad:m(M, A).
eval(STA, S, {?MODULE, IM}) ->
    monad:lift_m(fun({A, _}) -> A end, run(STA, S), IM).

-spec exec(state_t(S, M, _A), S) -> monad:m(M, S).
exec(STA, S, {?MODULE, IM}) ->
    monad:lift_m(fun({_, NS}) -> NS end, run(STA, S), IM).

-spec run(state_t(S, M, A), S) -> monad:m(M, {A, S}).
run(STA, S, {?MODULE, _IM}) -> (run_state_t(STA))(S).

-spec map(fun((monad:m(M, {A, S})) -> monad:m(N, {B, S})), state_t(S, M, A)) -> state_t(S, N, B).
map(F, STA, {?MODULE, _IM}) ->
    state_t(fun (S) -> F(run(STA, S)) end).

-spec with(fun((S) -> S), state_t(S, M, A)) -> state_t(S, M, A).
with(F, STA, {?MODULE, _IM}) ->
    state_t(fun (S) -> run(STA, F(S)) end).

run_nargs() ->
    1.

run_m(STA, [S]) ->
    run(STA, S).
