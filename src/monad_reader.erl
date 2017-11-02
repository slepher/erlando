%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_reader).

-callback ask() -> monad:monadic(M, _R) when M :: monad:monad().
-callback local(fun((R) -> R), monad:monadic(M, R)) -> monad:monadic(M, R) when M :: monad:monad().
-callback reader(fun((_R) -> A)) -> monad:monadic(M, A) when M :: monad:monad().

-compile({parse_transform, do}).
-compile({parse_transform, monad_t_transform}).

-include("functor.hrl").
-include("applicative.hrl").
-include("monad.hrl").

-export([ask/0, local/2, reader/1]).
-export([ask/1, local/3, reader/2]).
-export([default_ask/1, default_reader/2]).
-export([asks/2]).

-transform({?MODULE, [monad_reader], [asks/1]}).

%%%===================================================================
%%% API
%%%===================================================================
ask() ->
    undetermined:new(fun(MonadReader) -> ask(MonadReader) end).

local(F, URA) ->
    undetermined:map(
      fun(MonadReader, MRA) ->
              typeclass_trans:apply(local, [F, MRA], MonadReader)
      end, URA, ?MODULE).

reader(F) ->
    undetermined:new(fun(MonadReader) -> reader(F, MonadReader) end).

ask(MonadReader) ->
    typeclass_trans:apply(ask, [], MonadReader).

local(F, URA, MonadReader) ->
    undetermined:run(local(F, URA), MonadReader).

reader(F, MonadReader) ->
    typeclass_trans:apply(reader, [F], MonadReader).

default_ask(MonadReader) ->
    reader(function_instance:id(), MonadReader).

default_reader(F, MonadReader) ->
    monad:lift_m(F, MonadReader).

asks(F, MonadReader) ->
    monad:lift_m(F, ask(MonadReader), MonadReader).
%%%===================================================================
%%% Internal functions
%%%===================================================================
