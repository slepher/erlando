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

-compile({parse_transform, monad_t_transform}).

%% API
-export([sequence_a/2, traverse/3, sequence/2, map_m/3]).
-export([default_traverse/3, default_sequence_a/2, default_map_m/3, default_sequence/2]).

-type traversable(_A) :: any().

-callback traverse(fun((A) -> applicative:applicate(B)), applicative:applicate(A), _F) -> applicative:applicate(traversable(B)).
-callback sequence_a(traversable(applicative:applicate(A)), _F) -> applicative:applicate(traversable(A)).
-callback map_m(fun((A) -> monad:monadic(M, B)), monad:monadic(M, A), M) -> monad:monadic(M, traversable(B)).
-callback sequence(traversable(monad:monadic(M, A)), M) -> monad:monadic(M, traversable(A)).


-transform(#{args => [?MODULE], functions => [traverse/2, sequence_a/1, map_m/2, sequence/1]}).

%%%===================================================================
%%% API
%%%===================================================================

%% some traversable could not check type by instance
-spec traverse(fun((A) -> applicative:applicate(B)), applicative:applicate(A)) ->
                      applicative:applicate(traversable(B)).
traverse(A_FB, UA, UTraversable) ->
    undetermined:map(
      fun(Traversable, TA) ->
              typeclass_trans:apply(traverse, [A_FB, TA], Traversable, ?MODULE)
      end, UA, UTraversable).

-spec sequence_a(traversable(applicative:applicate(A))) -> 
                        applicative:applicate(traversable(A)).
sequence_a(UFA, UTraversable) ->
    undetermined:map(
      fun(Traversable, TFA) ->
              typeclass_trans:apply(sequence_a, [TFA], Traversable, ?MODULE)
      end, UFA, UTraversable).

map_m(A_MB, UA, UTraversable) ->
    undetermined:map(
      fun(Traversable, TA) ->
              typeclass_trans:apply(map_m, [A_MB, TA], Traversable, ?MODULE)
      end, UA, UTraversable).

sequence(UMA, UTraversable) ->
    undetermined:map(
      fun(Traversable, TMA) ->
              typeclass_trans:apply(sequence_a, [TMA], Traversable, ?MODULE)
      end, UMA, UTraversable).

default_traverse(AF_B, TA, Traversable) ->
    sequence_a(functor:fmap(AF_B, TA, Traversable), Traversable).

default_sequence_a(TFA, Traversable) ->
    traverse(fun(A) -> A end, TFA, Traversable).

default_map_m(A_MB, TA, Traversable) ->
    traverse(A_MB, TA, Traversable).

default_sequence(TMA, Traversable) ->
    sequence_a(TMA, Traversable).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
