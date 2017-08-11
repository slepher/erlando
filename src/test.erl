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

-module(test).
-export([test/2]).
-compile({parse_transform, do}).

test(Funs, Options) ->
    ErrorT = error_t:new(identity_m),
    Result = error_t:run(test_funs(ErrorT, Funs), ErrorT),
    case proplists:get_bool(report, Options) of
        true ->
            Name = proplists:get_value(name, Options, anonymous),
            case Result of
                {ok, passed} ->
                    io:format("Test suite '~p' passed.~n", [Name]);
                {error, Reason} ->
                    io:format("Test suite '~p' failed with ~p.~n",
                              [Name, Reason])
            end;
        false ->
            ok
    end,
    case Result of
        {ok, passed} -> ok;
        _            -> Result
    end.


test_funs(ErrorT, []) ->
    error_t:return(passed, ErrorT);

test_funs({error_t, IM} = ErrorT, [{Module, {Label, FunName}}|Funs])
  when is_atom(Module) andalso is_atom(FunName) ->
    case erlang:function_exported(Module, FunName, 0) of
        true ->
            do([{error_t, IM} || hoist(ErrorT, Label, fun () -> Module:FunName() end),
                                 test_funs(ErrorT, Funs)]);
        false ->
            error_t:fail({Label, Module, FunName, not_exported}, ErrorT)
    end;
test_funs({error_t, IM} = ErrorT, [{Module, FunName}|Funs])
  when is_atom(Module) andalso is_atom(FunName) ->
    case erlang:function_exported(Module, FunName, 0) of
        true ->
            do([{error_t, IM} || hoist(ErrorT, FunName, fun () -> Module:FunName() end),
                                 test_funs(ErrorT, Funs)]);
        false ->
            error_t:fail({Module, FunName, not_exported}, ErrorT)
    end;
test_funs(ErrorT, [{_Module, []}|Funs]) ->
    test_funs(ErrorT, Funs);

test_funs(ErrorT, [{Module, [FunName|FunNames]}|Funs])
  when is_atom(Module) andalso is_atom(FunName) ->
    test_funs(ErrorT, [{Module, FunName}, {Module, FunNames} | Funs]);

test_funs({error_t, IM} = ErrorT, [{Label, Fun}|Funs]) when is_function(Fun, 0) ->
    do([{error_t, IM} || hoist(ErrorT, Label, Fun),
                         test_funs(ErrorT, Funs)]);

test_funs({error_t, IM}, [Fun|Funs]) when is_function(Fun, 0) ->
    do([{error_t, IM} || hoist({error_t, IM}, anonymous_function, Fun),
                  test_funs({error_t, IM}, Funs)]).

hoist({error_t, IM}, Label, PlainFun) ->
    do([{error_t, IM} ||
           try
               PlainFun(),
               return(passed)
           catch
               Class:Reason ->
                   fail({Label, Class, Reason, erlang:get_stacktrace()})
           end]).
