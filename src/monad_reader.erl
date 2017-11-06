%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_reader).

-superclass([monad]).

-callback ask(M) -> monad:monadic(M, _R) when M :: monad:monad().
-callback local(fun((R) -> R), monad:monadic(M, R), M) -> monad:monadic(M, R) when M :: monad:monad().
-callback reader(fun((_R) -> A), M) -> monad:monadic(M, A) when M :: monad:monad().

-compile({parse_transform, do}).
-compile({parse_transform, monad_t_transform}).

-include("functor.hrl").
-include("applicative.hrl").
-include("monad.hrl").

-export([ask/1, local/3, reader/2]).
-export([default_ask/1, default_reader/2]).
-export([asks/2]).

-transform({?MODULE, [?MODULE], [ask/0, local/2, reader/1]}).
-transform({?MODULE, [monad_reader], [asks/1]}).

%%%===================================================================
%%% API
%%%===================================================================
ask(UMonadReader) ->
    undetermined:new(
      fun(MonadReader) -> 
              typeclass_trans:apply(ask, [], MonadReader)
      end, UMonadReader).

local(F, URA, UMonadReader) ->
    undetermined:map(
      fun(MonadReader, MRA) ->
              typeclass_trans:apply(local, [F, MRA], MonadReader)
      end, URA, UMonadReader).

reader(F, UMonadReader) ->
    undetermined:new(
      fun(MonadReader) ->
              typeclass_trans:apply(reader, [F], MonadReader)
      end, UMonadReader).

default_ask(MonadReader) ->
    reader(function_instance:id(), MonadReader).

default_reader(F, MonadReader) ->
    monad:lift_m(F, MonadReader).

asks(F, MonadReader) ->
    monad:lift_m(F, ask(MonadReader), MonadReader).
%%%===================================================================
%%% Internal functions
%%%===================================================================
