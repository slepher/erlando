%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(type).

%% API
-export([type/1, module/2]).

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
    typeclass:module(Type, Typeclass).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

