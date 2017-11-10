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

-module(error_instance).

-erlando_type({error, [{ok, '_'}, {error, '_'}, ok]}).

-export_type([error_instance/2, ok/1, error/1]).

%% This is really instance (Error e) => Monad (Either e) with 'error'
%% for Left and 'ok' for Right.
-type error_instance(E, A) :: ok(A) | error(E).

-type ok(A) :: ok | {ok, A}.
-type error(E) :: {error, E}.

-compile({parse_transform, monad_t_transform}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(monad_runner).

-define(TYPE, error).
-include("erlando.hrl").

-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, '>>'/3, return/2]).
-export([fail/2]).
-export([run_nargs/0, run_m/2]).
-export([run/1]).

-transform(#{args => [?TYPE], behaviours => [functor, applicative, monad, monad_fail]}).

-spec fmap(fun((A) -> B), error_instance(E, A)) -> error_instance(E, B).
fmap(F, E, ?TYPE) ->
    case E of
        {ok, V} ->
            {ok, F(V)};
        ok ->
            {ok, F(ok)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec '<$'(B, error_instance(E, _A)) -> error_instance(E, B).
'<$'(B, FA, ?TYPE) ->
    functor:'default_<$'(B, FA, ?TYPE).

-spec pure(A) -> error_instance(_E, A).
pure(A, ?TYPE) ->
    {ok, A}.

-spec '<*>'(error_instance(E, fun((A) -> B)), error_instance(E, A)) -> error_instance(E, B).
'<*>'({ok, F}, {ok, A}, ?TYPE) ->
    {ok, F(A)};
'<*>'({ok, F}, ok, ?TYPE) ->
    {ok, F(ok)};
'<*>'({ok, _F}, {error, R}, ?TYPE) ->
    {error, R};
'<*>'({error, R}, _, ?TYPE) ->
    {error, R}.

-spec lift_a2(fun((A, B) -> C), error_instance(E, A), error_instance(E, B)) -> error_instance(E, C).
lift_a2(F, EA, EB, ?TYPE) ->
    applicative:default_lift_a2(F, EA, EB, ?TYPE).

-spec '*>'(error_instance(E, _A), error_instance(E, B)) -> error_instance(E, B).
'*>'(EA, EB, ?TYPE) ->
    applicative:'default_*>'(EA, EB, ?TYPE).

-spec '<*'(error_instance(E, A), error_instance(E, _B)) -> error_instance(E, A).
'<*'(EA, EB, ?TYPE) ->
    applicative:'default_<*'(EA, EB, ?TYPE).

-spec '>>='(error_instance(E, A), fun( (A) -> error_instance(E, B) )) -> error_instance(E, B).
'>>='({error, _Err} = Error, _KEB, ?TYPE) -> Error;
'>>='({ok, A},                KEB, ?TYPE) -> KEB(A);
'>>='(ok,                     KEB, ?TYPE) -> KEB(ok).

-spec '>>'(error_instance(E, _A), error_instance(E, B)) -> error_instance(E, B).
'>>'(EA, EB, ?TYPE) ->
    monad:'default_>>'(EA, EB, ?TYPE).

-spec return(A) -> error_instance(_E, A).
return(A, ?TYPE) ->
    monad:default_return(A, ?TYPE).

-spec fail(E) -> error_instance(E, _A).
fail(E, ?TYPE) ->
    {error, E}.

run_nargs() ->
    0.

run_m(EA, []) ->
    EA.

run(#undetermined{} = U) ->
    run(undetermined:run(U, ?TYPE));
run(EM) ->
    EM.
