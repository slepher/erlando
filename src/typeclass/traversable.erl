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

-export_type([class/0, t/2]).

-type class() :: {module(), class()} | module().
-type t(_T, _A) :: any().

-callback traverse(fun((A) -> applicative:f(F, B)), applicative:f(F, A), T) -> applicative:f(F, traversable:t(T, B)) when F :: applicative:class(), T :: class().
-callback sequence_a(traversable:t(T, applicative:f(F, A)), T) -> applicative:f(F, traversable:t(T, A)) when F :: applicative:class(), T :: class().
-callback map_m(fun((A) -> monad:m(M, B)), monad:m(M, A), T) -> monad:m(M, traversable:t(T, B)) when M :: monad:class(), T :: class().
-callback sequence(traversable:t(T, monad:m(M, A)), T) -> monad:m(M, traversable:t(T, A)) when M :: monad:class(), T :: class().

-include("gen_fun.hrl").

%% API
-export([sequence_a/2, traverse/3, sequence/2, map_m/3]).
-export([default_traverse/3, default_sequence_a/2, default_map_m/3, default_sequence/2]).

-gen_fun(#{args => [?MODULE], functions => [traverse/2, sequence_a/1, map_m/2, sequence/1]}).

%%%===================================================================
%%% API
%%%===================================================================

%% some traversable could not check type by instance
-spec traverse(fun((A) -> applicative:f(F, B)), applicative:f(F, A), T) -> applicative:f(F, traversable:t(T, B)) when F :: applicative:class(), T :: class().
traverse(A_FB, UA, UTraversable) ->
    undetermined:map(
      fun(Traversable, TA) ->
              typeclass_trans:apply(traverse, [A_FB, TA], Traversable, ?MODULE)
      end, UA, UTraversable).

-spec sequence_a(traversable:t(T, applicative:f(F, A)), T) -> applicative:f(F, traversable:t(T, A)) when F :: applicative:class(), T :: class().
sequence_a(UFA, UTraversable) ->
    undetermined:map(
      fun(Traversable, TFA) ->
              typeclass_trans:apply(sequence_a, [TFA], Traversable, ?MODULE)
      end, UFA, UTraversable).

-spec map_m(fun((A) -> monad:m(M, B)), monad:m(M, A), T) -> monad:m(M, traversable:t(T, B)) when M :: monad:class(), T :: class().
map_m(A_MB, UA, UTraversable) ->
    undetermined:map(
      fun(Traversable, TA) ->
              typeclass_trans:apply(map_m, [A_MB, TA], Traversable, ?MODULE)
      end, UA, UTraversable).

-spec sequence(traversable:t(T, monad:m(M, A)), T) -> monad:m(M, traversable:t(T, A)) when M :: monad:class(), T :: class().
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
