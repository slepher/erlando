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

-callback writer({A, [_W]}, M) -> monad:m(M, A) when M :: monad:class().
-callback tell([_W], M) -> monad:m(M, _A) when M :: monad:class().
-callback listen(monad:m(M, A), M) -> monad:m(M, {A, [_W]}) when M :: monad:class().
-callback pass(monad:m(M, {A, fun(([W]) -> [W])}), M) -> monad:m(M, A) when M :: monad:class().

-compile({parse_transform, do}).
-compile({parse_transform, monad_t_transform}).

-include("op.hrl").
-include("functor.hrl").
-include("applicative.hrl").
-include("monad.hrl").

-export([writer/2, tell/2, listen/2, pass/2]).
-export([listens/3, censor/3]).

-transform(#{args => [?MODULE], functions => [writer/1, tell/1, listen/1, pass/1]}).
-transform(#{args => [?MODULE], functions => [listens/2, censor/2]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec writer({A, [_W]}, M) -> monad:m(M, A) when M :: monad:class().
writer({A, Ws}, UMonadWriter) ->
    undetermined:new(
      fun(MonadWriter) ->
              typeclass_trans:apply(writer, [{A, Ws}], MonadWriter, ?MODULE)
      end, UMonadWriter).

-spec tell([_W], M) -> monad:m(M, _A) when M :: monad:class().
tell(Ws, UMonadWriter) ->
    undetermined:new(
      fun(MonadWriter) ->
              typeclass_trans:apply(writer, [Ws], MonadWriter, ?MODULE)
      end, UMonadWriter).

-spec listen(monad:m(M, A), M) -> monad:m(M, {A, [_W]}) when M :: monad:class().
listen(UA, UMonadWriter) ->
    undetermined:map(
      fun(MonadWriter, MWA) ->
              typeclass_trans:apply(listen, [MWA], MonadWriter, ?MODULE)
      end, UA, UMonadWriter).

-spec pass(monad:m(M, {A, fun(([W]) -> [W])}), M) -> monad:m(M, A) when M :: monad:class().
pass(UA, UMonadWriter) ->
    undetermined:map(
      fun(MonadWriter, MWA) ->
              typeclass_trans:apply(pass, [MWA], MonadWriter, ?MODULE)
      end, UA, UMonadWriter).

-spec listens(fun(([_W]) -> B), monad:m(M, A), M) -> monad:m(M, {A, B}) when M :: monad:class().
listens(F, MWA, MonadWriter) ->
    NF = fun({A, Ws}) -> {A, F(Ws)} end,
    monad:lift_m(NF,listen(MWA), MonadWriter).

-spec censor(fun(([W]) -> [W]), monad:m(M, A), M) -> monad:m(M, A) when M :: monad:class().
censor(F, MWA, MonadWriter) ->
    pass(monad:lift_m(fun(A) -> {A, F} end, MWA, MonadWriter), MonadWriter).
%%%===================================================================
%%% Internal functions
%%%===================================================================
