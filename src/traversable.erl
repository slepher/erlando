%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(traversable).

%% API
-export([sequence_a/1, traverse/2, sequence/1, map_m/2]).

-type traversable(_A) :: any().

-callback traverse(fun((A) -> applicative:applicate(B)), applicative:applicate(A)) -> applicative:applicate(traversable(B)).

%%%===================================================================
%%% API
%%%===================================================================

%% some traversable could not check type by instance
-spec traverse(fun((A) -> applicative:applicate(B)), applicative:applicate(A)) -> applicative:applicate(traversable(B)).
traverse(A_FB, TA) ->
    Module = typeclass:module(traversable, TA),
    Module:traverse(A_FB, TA).

sequence_a(TFA) ->
    traverse(fun(A) -> A end, TFA).

map_m(A_MB, TA) ->
    traverse(A_MB, TA).

sequence(TMA) ->
    sequence_a(TMA).

    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
