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

-export_type([error_instance/2, ok/1, error/1]).

-behaviour(type).
-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(monad_runner).

-export([type/0]).
-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([fail/1]).
-export([run_nargs/0, run_m/2]).
-export([run/1]).

%% This is really instance (Error e) => Monad (Either e) with 'error'
%% for Left and 'ok' for Right.
-type error_instance(E, A) :: ok(A) | error(E).

-type ok(A) :: ok | {ok, A}.
-type error(E) :: {error, E}.

type() ->
    error.

-spec fmap(fun((A) -> B), error_instance(E, A)) -> error_instance(E, B).
fmap(F, E) ->
    case E of
        {ok, V} ->
            {ok, F(V)};
        ok ->
            {ok, F(ok)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec '<$'(B, error_instance(E, _A)) -> error_instance(E, B).
'<$'(B, FA) ->
    functor:'default_<$'(B, FA, ?MODULE).

-spec pure(A) -> error_instance(_E, A).
pure(A) ->
    {ok, A}.

-spec '<*>'(error_instance(E, fun((A) -> B)), error_instance(E, A)) -> error_instance(E, B).
'<*>'({ok, F}, {ok, A}) ->
    {ok, F(A)};
'<*>'({ok, F}, ok) ->
    {ok, F(ok)};
'<*>'({ok, _F}, {error, R}) ->
    {error, R};
'<*>'({error, R}, _) ->
    {error, R}.

-spec lift_a2(fun((A, B) -> C), error_instance(E, A), error_instance(E, B)) -> error_instance(E, C).
lift_a2(F, EA, EB) ->
    applicative:default_lift_a2(F, EA, EB, ?MODULE).

-spec '*>'(error_instance(E, _A), error_instance(E, B)) -> error_instance(E, B).
'*>'(EA, EB) ->
    applicative:'default_*>'(EA, EB, ?MODULE).

-spec '<*'(error_instance(E, A), error_instance(E, _B)) -> error_instance(E, A).
'<*'(EA, EB) ->
    applicative:'default_<*'(EA, EB, ?MODULE).

-spec '>>='(error_instance(E, A), fun( (A) -> error_instance(E, B) )) -> error_instance(E, B).
'>>='({error, _Err} = Error, _KEB) -> Error;
'>>='({ok, A},                KEB) -> KEB(A);
'>>='(ok,                     KEB) -> KEB(ok).

-spec '>>'(error_instance(E, _A), error_instance(E, B)) -> error_instance(E, B).
'>>'(EA, EB) ->
    monad:'default_>>'(EA, EB, ?MODULE).

-spec return(A) -> error_instance(_E, A).
return(A) ->
    monad:default_return(A, ?MODULE).

-spec fail(E) -> error_instance(E, _A).
fail(E) ->
    {error, E}.

run_nargs() ->
    0.

run_m(EA, []) ->
    EA.

run({undetermined, _} = U) ->
    run(undetermined:run(U, ?MODULE));
run(EM) ->
    EM.
