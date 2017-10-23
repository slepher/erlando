%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(type).

-callback type() -> atom().

%% API
-export([type/1, module/2, default_type/1]).

%%%===================================================================
%%% API
%%%===================================================================

type(L) when is_list(L) ->
    list;
type(F) when is_function(F, 1) ->
    function;
type({ok, _}) ->
    error;
type(ok) ->
    error;
type({error, _}) ->
    error;
type({just, _}) ->
    maybe;
type(nothing) ->
    maybe;
type({left, _}) ->
    either;
type({right, _}) ->
    either;
type({Module, _Instance}) ->
    Module.

module(Typeclass, Instance) ->
    Type = type(Instance),
    erlando_typeclass:module(Type, Typeclass).

default_type(Module) ->
    Module.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

