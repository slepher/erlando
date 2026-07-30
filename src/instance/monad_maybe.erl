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
-module(monad_maybe).

-erlando_type({?MODULE, [monad_maybe/1]}).

-export_type([monad_maybe/1]).

-type monad_maybe(A) :: {just, A} | nothing.

-include("erlando.hrl").

-include("gen_fun.hrl").

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monad_runner).

%% impl of functor.
-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([fail/1]).
%% impl of alternative.
-export([empty/0, '<|>'/2]).
%% impl of monad plus.
-export([mzero/0, mplus/2]).
%% impl of monad runner.
-export([run_nargs/0, run_m/2]).
-export([run/1]).

-gen_fun(#{patterns => [?MODULE], tbehaviours => [functor, applicative, monad, monad_fail]}).
-gen_fun(#{patterns => [?MODULE], tbehaviours => [alternative, monad_plus]}).

-spec fmap(fun((A) -> B), monad_maybe(A)) -> monad_maybe(B).
fmap(F, {just, X}) ->
    {just, F(X)};
fmap(_F, nothing) ->
    nothing.

-spec '<$'(B, monad_maybe(_A)) -> monad_maybe(B).
'<$'(B, MA) ->
    functor:'default_<$'(B, MA, ?MODULE).

-spec pure(A) -> monad_maybe(A).
pure(A) ->
    {just, A}.

-spec '<*>'(monad_maybe(fun((A) -> B)), A) -> monad_maybe(B).
'<*>'(nothing, _) ->
    nothing;
'<*>'(_, nothing) ->
    nothing;
'<*>'({just, F}, {just, A}) ->
    {just, F(A)}.

-spec lift_a2(fun((A, B) -> C), monad_maybe(A), monad_maybe(B)) -> monad_maybe(C).
lift_a2(F, RTA, RTB) ->
    applicative:default_lift_a2(F, RTA, RTB, ?MODULE).

-spec '*>'(monad_maybe(_A), monad_maybe(B)) -> monad_maybe(B).
'*>'(RTA, RTB) ->
    applicative:'default_*>'(RTA, RTB, ?MODULE).

-spec '<*'(monad_maybe(A), monad_maybe(_B)) -> monad_maybe(A).
'<*'(RTA, RTB) ->
    applicative:'default_<*'(RTA, RTB, ?MODULE).

-spec '>>='(monad_maybe(A), fun( (A) -> monad_maybe(B) )) -> monad_maybe(B).
'>>='({just, X}, Fun) -> Fun(X);
'>>='(nothing,  _Fun) -> nothing.

-spec '>>'(monad_maybe(_A), monad_maybe(B)) -> monad_maybe(B).
'>>'(MA, MB) ->
    monad:'default_>>'(MA, MB, ?MODULE).

-spec return(A) -> monad_maybe(A).
return(A) -> 
    monad:default_return(A, ?MODULE).

-spec fail(any()) -> monad_maybe(_A).
fail(_E) -> nothing.

empty() ->
    nothing.

-spec '<|>'(monad_maybe(A), monad_maybe(A)) -> monad_maybe(A).
'<|>'(nothing, MB) -> 
    MB;
'<|>'(MA,     _MB) -> 
    MA.

-spec mzero() -> monad_maybe(_A).
mzero() -> 
    empty().

-spec mplus(monad_maybe(A), monad_maybe(A)) -> monad_maybe(A).
mplus(MA, MB) ->
    '<|>'(MA, MB).

-spec run_nargs() -> integer().
run_nargs() ->
    0.

-spec run_m(monad_maybe(A), [any()]) -> monad_maybe(A).
run_m(MA, []) ->
    MA.


run(#undetermined{} = UA) ->
    undetermined:run(UA, ?MODULE);
run(Maybe) ->
    Maybe.
