%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_reader_instance).

-erlando_type([state_t, cont_t, maybe_t, error_t]).

-compile({parse_transform, cut}).
-include("do.hrl").

-behaviour(monad_reader).

%% API
-export([ask/1, local/3, reader/2]).
-export([lift_local/5]).

%%%===================================================================
%%% API
%%%===================================================================
ask({MonadTrans, MonadReader}) ->
    monad_trans:lift(monad_reader:ask(MonadReader), {MonadTrans, MonadReader});
ask(MonadTrans) ->
    ask({MonadTrans, monad_reader}).

local(F, MRA, {MonadTrans, MonadReader}) ->
    Ask = fun() -> monad_reader:ask(MonadReader) end,
    Local = monad_reader:local(_, _, MonadReader),
    lift_local(Ask, Local, F, MRA, {MonadTrans, MonadReader});
local(F, MRA, MonadTrans) when is_atom(MonadTrans) ->
    local(F, MRA, {MonadTrans, monad_reader}).

reader(F, {MonadTrans, MonadReader}) ->
    monad_trans:lift(monad_reader:reader(F, MonadReader), {MonadTrans, MonadReader});
reader(F, MonadTrans) ->
    reader(F, {MonadTrans, monad_reader}).

lift_local(Ask, Local, F, CTMRA, {cont_t, MonadReader}) ->
    cont_t:cont_t(
      fun (CC) ->
              do([MonadReader || 
                     R <- Ask(),
                     Local(F, cont_t:run(CTMRA, fun(A) -> Local(fun(_) -> R end, CC(A)) end))
                 ])
      end);
lift_local(_Ask, Local, F, MTMRA, {MonadTrans, MonadReader}) ->
    MonadTrans:map(
      fun(MRA) ->
              Local(F, MRA)
      end, MTMRA, {MonadTrans, MonadReader});
lift_local(Ask, Local, F, MRA, MonadTrans) when is_atom(MonadTrans) ->
    lift_local(Ask, Local, F, MRA, {MonadTrans, monad_reader}).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

