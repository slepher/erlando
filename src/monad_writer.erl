%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_writer).

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

-export([writer/1, tell/1, listen/1, pass/1]).
-export([writer/2, tell/2, listen/2, pass/2]).
-export([listens/3, censor/3]).

-transform({?MODULE, [monad_writer], [listens/2, censor/2]}).

%%%===================================================================
%%% API
%%%===================================================================
writer({A, Ws}) ->
    undetermined:new(fun(MonadWriter) -> writer({A, Ws}, MonadWriter) end).

tell(Ws) ->
    undetermined:new(fun(MonadWriter) -> tell(Ws, MonadWriter) end).

listen(UA) ->
    undetermined:map(
      fun(MonadWriter, MWA) ->
              typeclass_trans:apply(listen, [MWA], MonadWriter)
      end, UA, ?MODULE).

pass(UA) ->
    undetermined:map(
      fun(MonadWriter, MWA) ->
              typeclass_trans:apply(pass, [MWA], MonadWriter)
      end, UA, ?MODULE).

writer({A, Ws}, MonadWriter) ->
    typeclass_trans:apply(writer, [{A, Ws}], MonadWriter).

tell(Ws, MonadWriter) ->
    typeclass_trans:apply(tell, [Ws], MonadWriter).

listen(MA, MonadWriter) ->
    undetermined:run(listen(MA), MonadWriter).

pass(MA, MonadWriter) ->
    undetermined:run(pass(MA), MonadWriter).

-spec listens(fun(([_W]) -> B), monad:monadic(M, A), M) -> monad:monadic(M, {A, B}).
listens(F, MWA, MonadWriter) ->
    NF = fun({A, Ws}) -> {A, F(Ws)} end,
    monad:lift_m(NF,listen(MWA), MonadWriter).

censor(F, MWA, MonadWriter) ->
    pass(monad:lift_m(fun(A) -> {A, F} end, MWA, MonadWriter), MonadWriter).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
