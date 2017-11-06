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
-export([apply/4]).

%%%===================================================================
%%% API
%%%===================================================================
apply(F, Args, {T, M}, Typeclass) ->
    Module = typeclass:module(T, Typeclass),
    erlang:apply(Module, F, Args ++ [{T, M}]);
apply(F, Args, M, Typeclass) when is_atom(M) ->
    Module = typeclass:module(M, Typeclass),
    erlang:apply(Module, F, Args ++ [M]).

    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
