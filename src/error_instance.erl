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

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).

-export([fmap/2]).
-export(['<*>'/2, pure/1]).
-export(['>>='/2, return/1]).
-export([fail/1]).
-export([run_error/1]).

%% This is really instance (Error e) => Monad (Either e) with 'error'
%% for Left and 'ok' for Right.
-type error_instance(E, A) :: ok(A) | error(E).

-type ok(A) :: ok | {ok, A}.
-type error(E) :: {error, E}.

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

'<*>'({ok, F}, {ok, A}) ->
    {ok, F(A)};
'<*>'({ok, F}, ok) ->
    {ok, F(ok)};
'<*>'({ok, _F}, {error, R}) ->
    {error, R};
'<*>'({error, R}, _) ->
    {error, R}.

-spec pure(A) -> error_instance(_E, A).
pure(A) ->
    return(A).

-spec '>>='(error_instance(E, A), fun( (A) -> error_instance(E, B) )) -> error_instance(E, B).
'>>='({error, _Err} = Error, _Fun) -> Error;
'>>='({ok, Result},           Fun) -> Fun(Result);
'>>='(ok,                     Fun) -> Fun(ok).

-spec return(A) -> error_instance(_E, A).
return(X ) -> {ok, X}.

-spec fail(E) -> error_instance(E, _A).
fail(X) ->
    {error, X}.

run_error({undetermined, _} = U) ->
    run_error(undetermined:run(U, ?MODULE));
run_error(EM) ->
    EM.
