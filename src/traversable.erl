%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(traversable).

-superclass([functor, foldable]).

%% API
-export([sequence_a/1, traverse/2, sequence/1, map_m/2]).
-export([default_traverse/2, default_sequence_a/1, default_map_m/2, default_sequence/1]).

-type traversable(_A) :: any().

-callback traverse(fun((A) -> applicative:applicate(B)), applicative:applicate(A)) -> applicative:applicate(traversable(B)).
-callback sequence_a(traversable(applicative:applicate(A))) -> applicative:applicate(traversable(A)).
-callback map_m(fun((A) -> monad:monadic(M, B)), monad:monadic(M, A)) -> monad:monadic(M, traversable(B)).
-callback sequence(traversable(monad:monadic(M, A))) -> monad:monadic(M, traversable(A)).


%%%===================================================================
%%% API
%%%===================================================================

%% some traversable could not check type by instance
-spec traverse(fun((A) -> applicative:applicate(B)), applicative:applicate(A)) ->
                      applicative:applicate(traversable(B)).
traverse(A_FB, TA) ->
    Module = type:module(traversable, TA),
    Module:traverse(A_FB, TA).

-spec sequence_a(traversable(applicative:applicate(A))) -> 
                        applicative:applicate(traversable(A)).
sequence_a(TFA) ->
    Module = type:module(traversable, TFA),
    Module:sequence_a(TFA).

map_m(A_MB, TA) ->
    Module = type:module(traversable, TA),
    Module:map_m(A_MB, TA).

sequence(TMA) ->
    Module = type:module(traversable, TMA),
    Module:sequence(TMA).

default_traverse(AF_B, TA) ->
    sequence_a(functor:fmap(AF_B, TA)).

default_sequence_a(TFA) ->
    traverse(fun(A) -> A end, TFA).

default_map_m(A_MB, TA) ->
    traverse(A_MB, TA).

default_sequence(TMA) ->
    sequence_a(TMA).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
