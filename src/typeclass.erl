%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(typeclass).

%% API
-export([module/2, modules/2, compose/2]).

%%%===================================================================
%%% API
%%%===================================================================
module(_TypeClass, L) when is_list(L) ->
    list_instance;
module(_TypeClass, F) when is_function(F, 1) ->
    function_instance;
module(_TypeClass, {ok, _}) ->
    error_m;
module(_TypeClass, ok) ->
    error_m;
module(_TypeClass, {error, _}) ->
    error_m;
module(_TypeClass, {just, _}) ->
    maybe;
module(_TypeClass, nothing) ->
    maybe;
module(_TypeClass, {Module, _Instance}) ->
    Module.

modules(TypeClass, [H|T]) ->
    compose(module(TypeClass, H), modules(TypeClass, T)).

compose(ModuleA, undetermined) ->
    ModuleA;
compose(undetermined, ModuleA) ->
    ModuleA;
compose(ModuleA, ModuleA) ->
    ModuleA.



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
