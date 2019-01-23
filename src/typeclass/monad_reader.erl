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

-callback ask(M) -> monad:m(M, _R) when M :: monad:class().
-callback local(fun((R) -> R), monad:m(M, R), M) -> monad:m(M, R) when M :: monad:class().
-callback reader(fun((_R) -> A), M) -> monad:m(M, A) when M :: monad:class().

-include("do.hrl").
-compile({parse_transform, function_generator}).

-include("functor.hrl").
-include("applicative.hrl").
-include("monad.hrl").

-export([ask/1, local/3, reader/2]).
-export([default_ask/1, default_reader/2]).
-export([asks/2]).

-gen_fun(#{args => [?MODULE], functions => [ask/0, local/2, reader/1]}).
-gen_fun(#{args => [?MODULE], functions => [asks/1]}).

%%%===================================================================
%%% API
%%%===================================================================
ask(UMonadReader) ->
    undetermined:new(
      fun(MonadReader) -> 
              typeclass_trans:apply(ask, [], MonadReader, ?MODULE)
      end, UMonadReader).

local(F, URA, UMonadReader) ->
    undetermined:map(
      fun(MonadReader, MRA) ->
              typeclass_trans:apply(local, [F, MRA], MonadReader, ?MODULE)
      end, URA, UMonadReader).

reader(F, UMonadReader) ->
    undetermined:new(
      fun(MonadReader) ->
              typeclass_trans:apply(reader, [F], MonadReader, ?MODULE)
      end, UMonadReader).

default_ask(MonadReader) ->
    reader(function_instance:id(), MonadReader).

default_reader(F, MonadReader) ->
    monad:lift_m(F, ask(MonadReader), MonadReader).

asks(F, MonadReader) ->
    monad:lift_m(F, ask(MonadReader), MonadReader).
%%%===================================================================
%%% Internal functions
%%%===================================================================
