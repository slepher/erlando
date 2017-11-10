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

-erlando_type({list, []}).

-compile({parse_transform, cut}).
-compile({parse_transform, do}).
-compile({parse_transform, monad_t_transform}).

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

-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, '>>'/3, return/2]).
-export([fail/2]).
-export([run_nargs/0, run_m/2]).
-export([fold_map/3]).
-export([traverse/3, sequence_a/2, map_m/3, sequence/2]).
-export([empty/1, '<|>'/3]).
-export([mzero/1, mplus/3]).
-export([mempty/1, mappend/3]).

-transform(#{args => [?TYPE], behaviours => [functor, applicative, monad, monad_fail]}).
-transform(#{args => [?TYPE], behaviours => [foldable, traversable]}).
-transform(#{args => [?TYPE], behaviours => [alternative, monad_plus, monoid]}).


fmap(F, Xs, ?TYPE) ->
    [F(X) || X <- Xs].

'<$'(B, As, ?TYPE) ->
    functor:'default_<$'(B, As, ?TYPE).

pure(A, ?TYPE) -> [A].

-spec '<*>'([fun((A) -> B)], [A]) -> [B].
'<*>'(LF, LA, ?TYPE) ->
    [F(A) || F <- LF, A <- LA].

-spec lift_a2(fun((A, B) -> C), [A], [B]) -> [C].
lift_a2(F, As, Bs, ?TYPE) ->
    applicative:default_lift_a2(F, As, Bs, ?MODULE).

-spec '<*'([_A], [B]) -> [B].
'*>'(As, Bs, ?TYPE) ->
    applicative:'default_*>'(As, Bs, ?MODULE).

-spec '*>'([A], [_B]) -> [A].
'<*'(As, Bs, ?TYPE) ->
    applicative:'default_<*'(As, Bs, ?MODULE).

%% Note that using a list comprehension is (obviously) cheating, but
%% it's easier to read. The "real" implementation is also included for
%% completeness.

-spec '>>='([A], fun( (A) -> [B] )) -> [B].
'>>='(X, Fun, ?TYPE) -> lists:append([Fun(E) || E <- X]).
%%               lists:foldr(fun (E, Acc) -> Fun(E) ++ Acc end, [], X).

-spec '>>'([_A], [B]) -> [B].
'>>'(As, Bs, ?TYPE) ->
    monad:'default_>>'(As, Bs, ?MODULE).

-spec return(A) -> [A].
return(A, ?TYPE) -> 
    monad:default_return(A, ?MODULE).

-spec fail(any()) -> [_A].
fail(_E, ?TYPE) -> [].

run_nargs() ->
    0.

run_m(As, []) ->
    As.

fold_map(F, As, ?TYPE) ->
    lists:foldr(
      fun(A, Acc) ->
              monoid:mappend(Acc, F(A))
      end, monoid:mempty(), As).

traverse(A_FB, [H|T], ?TYPE) ->
    applicative:lift_a2([_|_], A_FB(H), traverse(A_FB, T, ?TYPE));
traverse(_A_FB, [], ?TYPE) ->
    applicative:pure([]).

sequence_a(TFA, ?TYPE) ->
    traversable:default_sequence_a(TFA, ?TYPE).

map_m(A_MB, [MH|TM], ?TYPE) ->
    do([monad ||
           H <- MH,
           T <- map_m(A_MB, TM, ?TYPE),
           return([H|T])
       ]);
map_m(_A_MB, [], ?TYPE) ->
    monad:return([]).

sequence(TMA, ?TYPE) ->
    map_m(function_instance:id(), TMA, ?TYPE).
           
empty(?TYPE) ->
    mzero(?TYPE).

'<|>'(LA, LB, ?TYPE) ->
    mplus(LA, LB, ?TYPE).

-spec mzero() -> [_A].
mzero(?TYPE) -> [].

-spec mplus([A], [A]) -> [A].
mplus(X, Y, ?TYPE) ->
    lists:append(X, Y).

mempty(?TYPE) -> [].

mappend(X, Y, ?TYPE) -> mplus(X, Y).
