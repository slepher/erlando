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

%% List Monad. Mainly just for fun! As normal, this is depth first.

-module(list_instance).

-erlando_type({list, [list_instance/0]}).

-export_type([list_instance/0]).

-type list_instance() :: [any()].

-compile({parse_transform, cut}).
-include("do.hrl").
-include("gen_fun.hrl").

-include("op.hrl").

-define(TYPE, list).


-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(monad_runner).
-behaviour(foldable).
-behaviour(traversable).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monoid).

-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([fail/1]).

-export([run_nargs/0, run_m/2]).
-export([fold_map/2]).
-export([traverse/2, sequence_a/1, map_m/2, sequence/1]).
-export([empty/0, '<|>'/2]).
-export([mzero/0, mplus/2]).
-export([mempty/0, mappend/2]).

-gen_fun(#{patterns => [?TYPE], tbehaviours => [functor, applicative, monad, monad_fail]}).
-gen_fun(#{patterns => [?TYPE], tbehaviours => [foldable, traversable]}).
-gen_fun(#{patterns => [?TYPE], tbehaviours => [alternative, monad_plus, monoid]}).


fmap(F, Xs) ->
    [F(X) || X <- Xs].

'<$'(B, As) ->
    functor:'default_<$'(B, As, ?TYPE).

pure(A) -> [A].

-spec '<*>'([fun((A) -> B)], [A]) -> [B].
'<*>'(LF, LA) ->
    [F(A) || F <- LF, A <- LA].

-spec lift_a2(fun((A, B) -> C), [A], [B]) -> [C].
lift_a2(F, As, Bs) ->
    applicative:default_lift_a2(F, As, Bs, ?TYPE).

-spec '<*'([_A], [B]) -> [B].
'*>'(As, Bs) ->
    applicative:'default_*>'(As, Bs, ?TYPE).

-spec '*>'([A], [_B]) -> [A].
'<*'(As, Bs) ->
    applicative:'default_<*'(As, Bs, ?TYPE).

%% Note that using a list comprehension is (obviously) cheating, but
%% it's easier to read. The "real" implementation is also included for
%% completeness.

-spec '>>='([A], fun( (A) -> [B] )) -> [B].
'>>='(X, Fun) -> lists:append([Fun(E) || E <- X]).
%%               lists:foldr(fun (E, Acc) -> Fun(E) ++ Acc end, [], X).

-spec '>>'([_A], [B]) -> [B].
'>>'(As, Bs) ->
    monad:'default_>>'(As, Bs, ?TYPE).

-spec return(A) -> [A].
return(A) -> 
    monad:default_return(A, ?TYPE).

-spec fail(any()) -> [_A].
fail(_E) -> [].

run_nargs() ->
    0.

run_m(As, []) ->
    As.

fold_map(F, As) ->
    lists:foldr(
      fun(A, Acc) ->
              monoid:mappend(Acc, F(A))
      end, monoid:mempty(), As).

traverse(A_FB, [H|T]) ->
    applicative:lift_a2([_|_], A_FB(H), traverse(A_FB, T));
traverse(_A_FB, []) ->
    applicative:pure([]).

sequence_a(TFA) ->
    traversable:default_sequence_a(TFA, ?TYPE).

map_m(A_MB, [HA|TA]) ->
    do([monad ||
           HB <- A_MB(HA),
           TB <- map_m(A_MB, TA),
           return([HB|TB])
       ]);
map_m(_A_MB, []) ->
    monad:return([]).

sequence(TMA) ->
    map_m(function_instance:id(), TMA).
           
empty() ->
    mzero(?TYPE).

'<|>'(LA, LB) ->
    mplus(LA, LB).

-spec mzero() -> [_A].
mzero() -> [].

-spec mplus([A], [A]) -> [A].
mplus(X, Y) ->
    lists:append(X, Y).

mempty() -> [].

mappend(X, Y) -> mplus(X, Y).
