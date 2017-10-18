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
-behaviour(monad_plus).
-behaviour(monad_runner).

%% impl of functor
-export([fmap/2, '<$'/2]).
-export([ap/2, pure/1]).
%% impl of monad
-export(['>>='/2, return/1, fail/1]).
%% impl of monad plus
-export([mzero/0, mplus/2]).
-export([run_nargs/0, run/2]).

-type maybe(A) :: {just, A} | nothing.

fmap(F, {just, X}) ->
    {just, F(X)};
fmap(_F, nothing) ->
    nothing.

'<$'(B, FA) ->
    functor:'default_<$'(B, FA).

ap(nothing, _) ->
    nothing;
ap(_, nothing) ->
    nothing;
ap({just, F}, {just, A}) ->
    {just, F(A)}.

pure(A) ->
    {just, A}.

-spec '>>='(maybe(A), fun( (A) -> maybe(B) )) -> maybe(B).
'>>='({just, X}, Fun) -> Fun(X);
'>>='(nothing,  _Fun) -> nothing.

-spec return(A) -> maybe(A).
return(X) -> {just, X}.

-spec fail(any()) -> maybe(_A).
fail(_X) -> nothing.

-spec mzero() -> maybe(_A).
mzero() -> nothing.

-spec mplus(maybe(A), maybe(A)) -> maybe(A).
mplus(nothing, Y) -> Y;
mplus(X,      _Y) -> X.

run_nargs() ->
    0.

run(MA, []) ->
    MA.
