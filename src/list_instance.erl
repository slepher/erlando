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

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(foldable).
-behaviour(traversable).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monoid).

-export([fmap/2]).
-export(['<*>'/2, pure/1]).
-export(['>>='/2, return/1]).
-export([fail/1]).
-export([fold_map/2]).
-export([traverse/2, sequence_a/1]).

-export([empty/0, '<|>'/2]).
-export([mzero/0, mplus/2]).
-export([mempty/0, mappend/2]).

fmap(F, Xs) ->
    [F(X) || X <- Xs].

-spec '<*>'([fun((A) -> B)], [A]) -> [B].
'<*>'(LF, LA) ->
    [F(A) || F <- LF, A <- LA].

pure(A) ->
    return(A).

%% Note that using a list comprehension is (obviously) cheating, but
%% it's easier to read. The "real" implementation is also included for
%% completeness.


-spec '>>='([A], fun( (A) -> [B] )) -> [B].
'>>='(X, Fun) -> lists:append([Fun(E) || E <- X]).
%%               lists:foldr(fun (E, Acc) -> Fun(E) ++ Acc end, [], X).

-spec return(A) -> [A].
return(X) -> [X].

-spec fail(any()) -> [_A].
fail(_E) -> [].

fold_map(F, As) ->
    lists:foldr(
      fun(A, Acc) ->
              monoid:mappend(Acc, F(A))
      end, monoid:mempty(), As).

traverse(A_FB, [H|T]) ->
    F = fun(A) ->
                fun(B) ->
                        [A|B]
                end
        end,
    applicative:ap(functor:fmap(F, A_FB(H)), traverse(A_FB, T));
traverse(_A_FB, []) ->
    applicative:pure([]).

sequence_a(TFA) ->
    traversable:default_sequence_a(TFA).

empty() ->
    mzero().

'<|>'(LA, LB) ->
    mplus(LA, LB).

-spec mzero() -> [_A].
mzero() -> [].

-spec mplus([A], [A]) -> [A].
mplus(X, Y) ->
    lists:append(X, Y).

mempty() -> [].

mappend(X, Y) -> mplus(X, Y).
