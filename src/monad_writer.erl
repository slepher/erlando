%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_writer).

-superclass([monad]).

-callback writer({A, [_W]}) -> monad:monadic(_M, A).
-callback tell([_W]) -> monad:monadic(_M, _A).
-callback listen(monad:monadic(M, A)) -> monad:monadic(M, {A, [_W]}).
-callback pass(monad:monadic(M, {A, fun(([W]) -> [W])})) -> monad:monadic(M, A). 

-compile({parse_transform, do}).
-compile({parse_transform, monad_t_transform}).

-include("op.hrl").
-include("functor.hrl").
-include("applicative.hrl").
-include("monad.hrl").

-export([writer/2, tell/2, listen/2, pass/2]).
-export([listens/3, censor/3]).

-transform({?MODULE, [monad_writer], [writer/1, tell/1, listen/1, pass/1]}).
-transform({?MODULE, [monad_writer], [listens/2, censor/2]}).

%%%===================================================================
%%% API
%%%===================================================================
writer({A, Ws}, UMonadWriter) ->
    undetermined:new(
      fun(MonadWriter) ->
              typeclass_trans:apply(writer, [{A, Ws}], MonadWriter)
      end, UMonadWriter).

tell(Ws, UMonadWriter) ->
    undetermined:new(
      fun(MonadWriter) ->
              typeclass_trans:apply(writer, [Ws], MonadWriter)
      end, UMonadWriter).

listen(UA, UMonadWriter) ->
    undetermined:map(
      fun(MonadWriter, MWA) ->
              typeclass_trans:apply(listen, [MWA], MonadWriter)
      end, UA, UMonadWriter).

pass(UA, UMonadWriter) ->
    undetermined:map(
      fun(MonadWriter, MWA) ->
              typeclass_trans:apply(pass, [MWA], MonadWriter)
      end, UA, UMonadWriter).

-spec listens(fun(([_W]) -> B), monad:monadic(M, A), M) -> monad:monadic(M, {A, B}).
listens(F, MWA, MonadWriter) ->
    NF = fun({A, Ws}) -> {A, F(Ws)} end,
    monad:lift_m(NF,listen(MWA), MonadWriter).

censor(F, MWA, MonadWriter) ->
    pass(monad:lift_m(fun(A) -> {A, F} end, MWA, MonadWriter), MonadWriter).
%%%===================================================================
%%% Internal functions
%%%===================================================================
