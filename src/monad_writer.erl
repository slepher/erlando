%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_writer).

-compile({parse_transform, do}).

-include("op.hrl").

-callback writer({A, [_W]}) -> monad:monadic(_M, A).
-callback tell([_W]) -> monad:monadic(_M, _A).
-callback listen(monad:monadic(M, A)) -> monad:monadic(M, {A, [_W]}).
-callback pass(monad:monadic(M, {A, fun(([W]) -> [W])})) -> monad:monadic(M, A). 

%% API
-export([writer/1, tell/1, listen/1, pass/1]).
-export([writer/2, tell/2]).
-export([listens/2, censor/2]).

%%%===================================================================
%%% API
%%%===================================================================
writer({A, Ws}) ->
    undetermined:new(fun(MonadWriter) -> writer({A, Ws}, MonadWriter) end).

tell(Ws) ->
    undetermined:new(fun(MonadWriter) -> tell(Ws, MonadWriter) end).

listen(UA) ->
    undetermined:map(
      fun(Module, WA) ->
              Module:listen(WA)
      end, UA, ?MODULE).

pass(UA) ->
    undetermined:map(
      fun(Module, WA) ->
              Module:pass(WA)
      end, UA, ?MODULE).

writer({A, Ws}, MonadWriter) ->
    monad_trans:apply_fun(writer, [{A, Ws}], MonadWriter).

tell(Ws, MonadWriter) ->
    monad_trans:apply_fun(tell, [Ws], MonadWriter).

-spec listens(fun(([_W]) -> B), monad:monadic(M, A)) -> monad:monadic(M, {A, B}).
listens(F, MWA) ->
    NF = fun({A, Ws}) -> {A, F(Ws)} end,
    NF /'<$>'/ listen(MWA).

censor(F, MWA) ->
    pass(fun(A) -> {A, F} end /'<$>'/ MWA).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
