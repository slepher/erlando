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

-module(error_m).

-erlando_type({error_m, [{ok, '_'}, {error, '_'}, ok]}).

-export_type([error_m/2, ok/1, error/1]).

%% This is really instance (Error e) => Monad (Either e) with 'error'
%% for Left and 'ok' for Right.
-type error_m(E, A) :: ok(A) | error(E).

-type ok(A) :: ok | {ok, A}.
-type error(E) :: {error, E}.

-compile({parse_transform, monad_t_transform}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(monad_runner).

-include("erlando.hrl").

-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([fail/1]).
-export([run_nargs/0, run_m/2]).
-export([run/1]).

-transform(#{patterns => [?MODULE], gbehaviours => [functor, applicative, monad, monad_fail]}).

-spec fmap(fun((A) -> B), error_m(E, A)) -> error_m(E, B).
fmap(F, EA) ->
    case EA of
        {ok, V} ->
            {ok, F(V)};
        ok ->
            {ok, F(ok)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec '<$'(B, error_m(E, _A)) -> error_m(E, B).
'<$'(B, FA) ->
    functor:'default_<$'(B, FA, ?MODULE).

-spec pure(A) -> error_m(_E, A).
pure(A) ->
    {ok, A}.

-spec '<*>'(error_m(E, fun((A) -> B)), error_m(E, A)) -> error_m(E, B).
'<*>'({ok, F}, {ok, A}) ->
    {ok, F(A)};
'<*>'({ok, F}, ok) ->
    {ok, F(ok)};
'<*>'({ok, _F}, {error, R}) ->
    {error, R};
'<*>'({error, R}, _) ->
    {error, R}.

-spec lift_a2(fun((A, B) -> C), error_m(E, A), error_m(E, B)) -> error_m(E, C).
lift_a2(F, EA, EB) ->
    applicative:default_lift_a2(F, EA, EB, ?MODULE).

-spec '*>'(error_m(E, _A), error_m(E, B)) -> error_m(E, B).
'*>'(EA, EB) ->
    applicative:'default_*>'(EA, EB, ?MODULE).

-spec '<*'(error_m(E, A), error_m(E, _B)) -> error_m(E, A).
'<*'(EA, EB) ->
    applicative:'default_<*'(EA, EB, ?MODULE).

-spec '>>='(error_m(E, A), fun( (A) -> error_m(E, B) )) -> error_m(E, B).
'>>='({error, _Err} = Error, _KEB) -> Error;
'>>='({ok, A},                KEB) -> KEB(A);
'>>='(ok,                     KEB) -> KEB(ok).

-spec '>>'(error_m(E, _A), error_m(E, B)) -> error_m(E, B).
'>>'(EA, EB) ->
    monad:'default_>>'(EA, EB, ?MODULE).

-spec return(A) -> error_m(_E, A).
return(A) ->
    monad:default_return(A, ?MODULE).

-spec fail(E) -> error_m(E, _A).
fail(E) ->
    {error, E}.

run_nargs() ->
    0.

run_m(EA, []) ->
    EA.

run(#undetermined{} = U) ->
    run(undetermined:run(U, ?MODULE));
run(EM) ->
    EM.
