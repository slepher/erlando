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

-module(maybe).

-export_type([maybe/1]).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monad_runner).

%% impl of functor.
-export([fmap/2, '<$'/2]).
%% impl of applicative.
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
%% impl of monad.
-export(['>>='/2, '>>'/2, return/1]).
%% impl of monad fail.
-export([fail/1]).
%% impl of alternative.
-export([empty/0, '<|>'/2]).
%% impl of monad plus.
-export([mzero/0, mplus/2]).
%% impl of monad runner.
-export([run_nargs/0, run/2]).

-type maybe(A) :: {just, A} | nothing.

-spec fmap(fun((A) -> B), maybe(A)) -> maybe(B).
fmap(F, {just, X}) ->
    {just, F(X)};
fmap(_F, nothing) ->
    nothing.

-spec '<$'(B, maybe(_A)) -> maybe(B).
'<$'(B, MA) ->
    functor:'default_<$'(B, MA, ?MODULE).

-spec pure(A) -> maybe(A).
pure(A) ->
    {just, A}.

-spec '<*>'(maybe(fun((A) -> B)), A) -> maybe(B).
'<*>'(nothing, _) ->
    nothing;
'<*>'(_, nothing) ->
    nothing;
'<*>'({just, F}, {just, A}) ->
    {just, F(A)}.

-spec lift_a2(fun((A, B) -> C), maybe(A), maybe(B)) -> maybe(C).
lift_a2(F, RTA, RTB) ->
    applicative:default_lift_a2(F, RTA, RTB, ?MODULE).

-spec '*>'(maybe(_A), maybe(B)) -> maybe(B).
'*>'(RTA, RTB) ->
    applicative:'default_*>'(RTA, RTB, ?MODULE).

-spec '<*'(maybe(A), maybe(_B)) -> maybe(A).
'<*'(RTA, RTB) ->
    applicative:'default_<*'(RTA, RTB, ?MODULE).

-spec '>>='(maybe(A), fun( (A) -> maybe(B) )) -> maybe(B).
'>>='({just, X}, Fun) -> Fun(X);
'>>='(nothing,  _Fun) -> nothing.

-spec '>>'(maybe(_A), maybe(B)) -> maybe(B).
'>>'(MA, MB) ->
    monad:'default_>>'(MA, MB, ?MODULE).

-spec return(A) -> maybe(A).
return(A) -> 
    monad:default_return(A, ?MODULE).

-spec fail(any()) -> maybe(_A).
fail(_E) -> nothing.

empty() ->
    nothing.

-spec '<|>'(maybe(A), maybe(A)) -> maybe(A).
'<|>'(nothing, MB) -> 
    MB;
'<|>'(MA,     _MB) -> 
    MA.

-spec mzero() -> maybe(_A).
mzero() -> 
    empty().

-spec mplus(maybe(A), maybe(A)) -> maybe(A).
mplus(MA, MB) ->
    '<|>'(MA, MB).

-spec run_nargs() -> integer().
run_nargs() ->
    0.

-spec run(maybe(A), [any()]) -> maybe(A).
run(MA, []) ->
    MA.
