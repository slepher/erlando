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

-behaviour(monad_reader).

%% API
-export([ask/1, local/3, reader/2]).

%%%===================================================================
%%% API
%%%===================================================================
ask(MonadTrans) when is_atom(MonadTrans) ->
    monad_trans:lift(monad_reader:ask(), MonadTrans);
    %ask({MonadTrans, monad_reader});
ask({MonadTrans, MonadReader}) ->
    monad_trans:lift(monad_reader:ask(MonadReader), {MonadTrans, MonadReader}).

local(F, MRA, cont_t) ->
    local(F, MRA, {cont, monad_reader});
local(F, MRA, {cont_t, MonadReader}) ->
    Ask = fun() -> monad_reader:ask(MonadReader) end,
    Local = monad_reader:local(_, _, MonadReader),
    cont_t:lift_local(Ask, Local, F, MRA, {cont_t, MonadReader});
local(F, MRA, MonadTrans) when is_atom(MonadTrans)->
    local(F, MRA, {MonadTrans, monad_reader});
local(F, MRA, {MonadTrans, MonadReader}) ->
    MonadTrans:map(
      fun(MA) ->
              monad_reader:local(F, MA, MonadReader)
      end, MRA).

reader(F, MonadTrans) when is_atom(MonadTrans) ->
    reader(F, {MonadTrans, monad_reader});
reader(F, {MonadTrans, MonadReader}) ->
    monad_trans:lift(monad_reader:reader(F, MonadReader), {MonadTrans, MonadReader}).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
