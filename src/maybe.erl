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

-erlando_type({?MODULE, [{just, '_'}, nothing]}).

-export_type([maybe/1]).

-type maybe(A) :: {just, A} | nothing.

-compile({parse_transform, monad_t_transform}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monad_runner).

%% impl of functor.
-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, '>>'/3, return/2]).
-export([fail/2]).
%% impl of alternative.
-export([empty/1, '<|>'/3]).
%% impl of monad plus.
-export([mzero/1, mplus/3]).
%% impl of monad runner.
-export([run_nargs/0, run_m/2]).

-transform(#{args => [?MODULE], behaviours => [functor, applicative, monad, monad_fail]}).
-transform(#{args => [?MODULE], behaviours => [alternative, monad_plus]}).

-spec fmap(fun((A) -> B), maybe(A)) -> maybe(B).
fmap(F, {just, X}, ?MODULE) ->
    {just, F(X)};
fmap(_F, nothing, ?MODULE) ->
    nothing.

-spec '<$'(B, maybe(_A)) -> maybe(B).
'<$'(B, MA, ?MODULE) ->
    functor:'default_<$'(B, MA, ?MODULE).

-spec pure(A) -> maybe(A).
pure(A, ?MODULE) ->
    {just, A}.

-spec '<*>'(maybe(fun((A) -> B)), A) -> maybe(B).
'<*>'(nothing, _, ?MODULE) ->
    nothing;
'<*>'(_, nothing, ?MODULE) ->
    nothing;
'<*>'({just, F}, {just, A}, ?MODULE) ->
    {just, F(A)}.

-spec lift_a2(fun((A, B) -> C), maybe(A), maybe(B)) -> maybe(C).
lift_a2(F, RTA, RTB, ?MODULE) ->
    applicative:default_lift_a2(F, RTA, RTB, ?MODULE).

-spec '*>'(maybe(_A), maybe(B)) -> maybe(B).
'*>'(RTA, RTB, ?MODULE) ->
    applicative:'default_*>'(RTA, RTB, ?MODULE).

-spec '<*'(maybe(A), maybe(_B)) -> maybe(A).
'<*'(RTA, RTB, ?MODULE) ->
    applicative:'default_<*'(RTA, RTB, ?MODULE).

-spec '>>='(maybe(A), fun( (A) -> maybe(B) )) -> maybe(B).
'>>='({just, X}, Fun, ?MODULE) -> Fun(X);
'>>='(nothing,  _Fun, ?MODULE) -> nothing.

-spec '>>'(maybe(_A), maybe(B)) -> maybe(B).
'>>'(MA, MB, ?MODULE) ->
    monad:'default_>>'(MA, MB, ?MODULE).

-spec return(A) -> maybe(A).
return(A, ?MODULE) -> 
    monad:default_return(A, ?MODULE).

-spec fail(any()) -> maybe(_A).
fail(_E, ?MODULE) -> nothing.

empty(?MODULE) ->
    nothing.

-spec '<|>'(maybe(A), maybe(A)) -> maybe(A).
'<|>'(nothing, MB, ?MODULE) -> 
    MB;
'<|>'(MA,     _MB, ?MODULE) -> 
    MA.

-spec mzero() -> maybe(_A).
mzero(?MODULE) -> 
    empty(?MODULE).

-spec mplus(maybe(A), maybe(A)) -> maybe(A).
mplus(MA, MB, ?MODULE) ->
    '<|>'(MA, MB, ?MODULE).

-spec run_nargs() -> integer().
run_nargs() ->
    0.

-spec run_m(maybe(A), [any()]) -> maybe(A).
run_m(MA, []) ->
    MA.
