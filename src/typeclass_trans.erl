%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(typeclass_trans).

-export_type([trans/2]).

-type trans(T, M) :: {T, M}.
%% API
-export([apply/3, apply/4]).

%%%===================================================================
%%% API
%%%===================================================================
apply(F, Args, {T, M}) when is_atom(T) ->
    erlang:apply(T, F, Args ++ [{T, M}]);
apply(F, Args, M) when is_atom(M) ->    
    erlang:apply(M, F, Args).

apply(F, Args, {T, M}, Typeclass) ->
    Module = typeclass:module(T, Typeclass),
    erlang:apply(Module, F, Args ++ [{T, M}]);
apply(F, Args, M, Typeclass) when is_atom(M) ->
    Module = typeclass:module(M, Typeclass),
    erlang:apply(Module, F, Args).

    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
